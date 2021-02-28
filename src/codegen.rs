use crate::{env::Environment, typing::*, closure::*};
use std::{convert::TryInto, path::Path, unimplemented, unreachable};
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::TryFrom;
use std::ops::Deref;
use inkwell::*;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::AddressSpace;
use inkwell::values::*;
use inkwell::types::*;
use inkwell::targets::*;

type Env<'a> = Environment<String, BasicValueEnum<'a>>; // key: variable name, value: variable value

const OBJ_INT: u64 = 0;
const OBJ_BOOL: u64 = 1;
const OBJ_FUNC: u64 = 2;
const OBJ_TUPLE: u64 = 3;
const OBJ_CLOSURE: u64 = 4;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module: Module<'ctx>, builder: Builder<'ctx>) -> CodeGen<'ctx> {
        CodeGen {
            context: &context,
            module,
            builder,
        }
    }

    pub fn print_ir(&self) {
        self.module.print_to_stderr();
    }

    pub fn type_to_tag(&self, ty: &Type) -> u64 {
        match *ty {
            Type::Int => OBJ_INT,
            Type::Bool => OBJ_BOOL,
            Type::Func {
                args: _,
                ret: _
            } => OBJ_FUNC,
            Type::Tuple(_) => OBJ_TUPLE,
            _ => unimplemented!(),
        }
    }

    pub fn create_obj(&self, val: BasicValueEnum<'ctx>, ty: u64) -> BasicValueEnum<'ctx> {
        let callsite = self.builder.build_call(self.get_intrinsic("llvm.stacksave").unwrap(), &[], "tmp");
        let saved = callsite.try_as_basic_value().left().unwrap();

        let obj_type = self.module.get_struct_type("obj").unwrap();
        let obj_ptr = self.builder.build_alloca(obj_type, "tmp");
        let field_data = self.builder.build_struct_gep(obj_ptr, 0, "tmp").unwrap();
        let cast_val = match val {
            BasicValueEnum::PointerValue(_) => {
                BasicValueEnum::try_from(self.builder.build_ptr_to_int(val.into_pointer_value(), obj_type.get_field_types()[0].into_int_type(), "tmp")).unwrap()
            },
            _ => self.builder.build_cast(InstructionOpcode::ZExt, val, obj_type.get_field_types()[0], "tmp")
        };
        self.builder.build_store(field_data, cast_val);

        let field_type = self.builder.build_struct_gep(obj_ptr, 1, "tmp").unwrap();
        let ty_num = self.context.i8_type().const_int(ty, false);
        self.builder.build_store(field_type, ty_num);

        let obj = self.builder.build_load(obj_ptr, "tmp");

        self.builder.build_call(self.get_intrinsic("llvm.stackrestore").unwrap(), &[saved], "tmp");

        obj
    }

    pub fn store_obj(&self, obj: BasicValueEnum<'ctx>, val: BasicValueEnum<'ctx>) {
        let callsite = self.builder.build_call(self.get_intrinsic("llvm.stacksave").unwrap(), &[], "tmp");
        let saved = callsite.try_as_basic_value().left().unwrap();

        let obj_type = self.module.get_struct_type("obj").unwrap();
        let struct_val = obj.into_struct_value();
        let struct_ptr = self.builder.build_alloca(struct_val.get_type(), "tmp");
        let field_data = self.builder.build_struct_gep(struct_ptr, 0, "tmp").unwrap();
        let val_cast = self.builder.build_cast(InstructionOpcode::Trunc, val, obj_type.get_field_types()[0], "tmp");
        self.builder.build_store(field_data, val_cast);

        self.builder.build_call(self.get_intrinsic("llvm.stackrestore").unwrap(), &[saved], "tmp");
    }

    pub fn get_obj_data_llvmty(&self, obj: BasicValueEnum<'ctx>, ty: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
        // save stack.
        let callsite = self.builder.build_call(self.get_intrinsic("llvm.stacksave").unwrap(), &[], "tmp");
        let saved = callsite.try_as_basic_value().left().unwrap();

        let struct_val = obj.into_struct_value();
        let struct_ptr = self.builder.build_alloca(struct_val.get_type(), "tmp");
        self.builder.build_store(struct_ptr, struct_val);
        let field_data = self.builder.build_struct_gep(struct_ptr, 0, "tmp").unwrap();
        let data = self.builder.build_load(field_data, "tmp");

        // restore stack.
        self.builder.build_call(self.get_intrinsic("llvm.stackrestore").unwrap(), &[saved], "tmp");

        match ty {
            BasicTypeEnum::PointerType(_) => BasicValueEnum::try_from(self.builder.build_int_to_ptr(data.into_int_value(), ty.into_pointer_type(), "tmp")).unwrap(),
            _ => self.builder.build_cast(InstructionOpcode::Trunc, data, ty, "tmp"),
        }
    }

    pub fn get_obj_data(&self, obj: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
        self.get_obj_data_llvmty(obj, self.type_to_llvmty(ty))
    }

    pub fn create_func_type(&self, ty: &Type) -> FunctionType<'ctx> {
        match *ty {
            Type::Func { ref args, ret: ref _ret} => {
                let mut arg_lltypes = Vec::new();
                arg_lltypes.push(BasicTypeEnum::PointerType(self.context.i8_type().ptr_type(AddressSpace::Generic)));
                for _ in args.iter() {
                    arg_lltypes.push(BasicTypeEnum::StructType(self.module.get_struct_type("obj").unwrap()));
                }
                let ret_lltype = BasicTypeEnum::StructType(self.module.get_struct_type("obj").unwrap());

                let func_lltype = ret_lltype.fn_type(&arg_lltypes, false);
                func_lltype
            },
            _ => unreachable!(),
        }
    }

    pub fn create_tuple_type(&self, ty: &Type) -> StructType<'ctx> {
        match *ty {
            Type::Tuple(ref tys) => {
                let mut lltys = Vec::new();
                for ty in tys.iter() {
                    let llty = self.type_to_llvmty(ty);
                    lltys.push(llty);
                }

                self.context.struct_type(lltys.as_slice(), false)
            },
            _ => unreachable!(),
        }
    }

    pub fn type_to_llvmty(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        use inkwell::types::BasicTypeEnum::*;

        match *ty {
            Type::Int => IntType(self.context.i32_type()),
            Type::Bool => IntType(self.context.bool_type()),
            Type::Func { args: _, ret: _ } => PointerType(self.create_func_type(ty).ptr_type(AddressSpace::Generic)),
            Type::Tuple(ref _tys) => PointerType(self.create_tuple_type(ty).ptr_type(AddressSpace::Generic)),
            _ => unimplemented!(),
        }
    }

    pub fn closure_fv_struct(&mut self, fvs: Vec<BasicTypeEnum<'ctx>>) -> BasicTypeEnum<'ctx> {
        let fv_struct = self.context.struct_type(fvs.as_slice(), false);
        BasicTypeEnum::PointerType(fv_struct.ptr_type(AddressSpace::Generic))
    }

    pub fn make_closure_ty(&mut self, func_ptr: BasicTypeEnum<'ctx>, fvs: Vec<BasicTypeEnum<'ctx>>) -> BasicTypeEnum<'ctx> {
        let mut clos_ty_list = Vec::new();
        clos_ty_list.push(func_ptr);
        clos_ty_list.push(self.closure_fv_struct(fvs));

        let clos_ty = self.context.struct_type(clos_ty_list.as_slice(), false);
        BasicTypeEnum::StructType(clos_ty)
    }

    pub fn make_closure(&mut self, env: Rc<RefCell<Env<'ctx>>>, name: &String, fvs: &Vec<String>) -> BasicValueEnum<'ctx> {
        let func = self.module.get_function(name).unwrap();
        let func_ptr = func.as_global_value().as_pointer_value();
        let mut fv_objs = Vec::new();
        for fv in fvs.iter() {
            let fv_obj_ptr = env.borrow().get(fv).unwrap();
            let fv_obj = self.builder.build_load(fv_obj_ptr.into_pointer_value(), "tmp");
            fv_objs.push(fv_obj);
        }

        let mut fv_types = Vec::new();
        for obj in fv_objs.iter() {
            fv_types.push(obj.get_type());
        }

        let closure_ty = self.make_closure_ty(BasicTypeEnum::PointerType(func_ptr.get_type()), fv_types); // self.context.struct_type(closure_type_list.as_slice(), false);

        let closure_ptr = self.builder.build_malloc(closure_ty, "tmp").unwrap();
        let func_ptr_field = self.builder.build_struct_gep(closure_ptr, 0, "tmp").unwrap();
        self.builder.build_store(func_ptr_field, BasicValueEnum::PointerValue(func_ptr));

        let fv_struct_field = self.builder.build_struct_gep(closure_ptr, 1, "tmp").unwrap();
        let fv_struct_ty = closure_ty.into_struct_type().get_field_type_at_index(1).unwrap().into_pointer_type().get_element_type().into_struct_type();
        let fv_struct_ptr = self.builder.build_malloc(BasicTypeEnum::StructType(fv_struct_ty), "tmp").unwrap();
        self.builder.build_store(fv_struct_field, fv_struct_ptr);
        for (i, obj) in fv_objs.into_iter().enumerate() {
            let fv_field = self.builder.build_struct_gep(fv_struct_ptr, i.try_into().unwrap(), "tmp").unwrap();
            self.builder.build_store(fv_field, obj);
        }

        let closure_basic = BasicValueEnum::PointerValue(closure_ptr);
        let obj = self.create_obj(closure_basic, OBJ_CLOSURE);

        obj
    }

    pub fn gen_objfile(&mut self, file_path: String, target_name: String, target_triple_name: String) {
        let target_triple = if target_triple_name.is_empty() {
            TargetMachine::get_default_triple()
        } else {
            TargetTriple::create(target_triple_name.as_str())
        };
        let opt = OptimizationLevel::Default;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let target = Target::from_triple(&target_triple).unwrap();
        
        let target_machine = target.create_target_machine(
            &target_triple,
            target_name.as_str(),
            "",
            opt,
            reloc,
            model,
        ).unwrap();
        let target_data = target_machine.get_target_data();
        let data_layout = target_data.get_data_layout();
        self.module.set_data_layout(&data_layout);

        let path = Path::new(file_path.as_str());
        assert!(target_machine.write_to_file(&self.module, FileType::Object, &path).is_ok());
    }

    pub fn add_intrinsic(&self, name: &str, func_ty: FunctionType<'ctx>) {
        let _func = self.module.add_function(&name, func_ty, None);
    }

    pub fn get_intrinsic(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    pub fn set_default_intrinsics(&self) {
        let i8_ptr = BasicTypeEnum::PointerType(self.context.i8_type().ptr_type(AddressSpace::Generic));
        let void = self.context.void_type();

        let stacksave = i8_ptr.fn_type(&[], false);
        self.add_intrinsic("llvm.stacksave", stacksave);

        let stackrestore = void.fn_type(&[i8_ptr], false);
        self.add_intrinsic("llvm.stackrestore", stackrestore);
    }

    pub fn set_default_struct(&self) {
        let obj_type = self.context.opaque_struct_type("obj");
        let i64_ty = BasicTypeEnum::IntType(self.context.i64_type());
        let i8_ty = BasicTypeEnum::IntType(self.context.i8_type());
        obj_type.set_body(&[i64_ty, i8_ty], false);
    }

    pub fn codegen_init(&mut self, fundefs: Vec<FunDef>) {
        self.set_default_struct();
        self.set_default_intrinsics();

        self.codegen_toplevel(fundefs);

        let i32_type = self.context.i32_type();
        let main_fn_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
    }

    pub fn codegen_declare(&mut self, fundef: &FunDef) {
        let (ref func_name, ref _func_ty) = fundef.name;
        let mut arg_lltypes = Vec::new();

        let mut fv_types = Vec::new();
        for _ in fundef.formal_fv.iter() {
            fv_types.push(BasicTypeEnum::StructType(self.module.get_struct_type("obj").unwrap()));
        }
        arg_lltypes.push(BasicTypeEnum::PointerType(self.context.i8_type().ptr_type(AddressSpace::Generic)));
        for _ in fundef.args.iter() {
            arg_lltypes.push(BasicTypeEnum::StructType(self.module.get_struct_type("obj").unwrap()));
        }
        let ret_lltype = BasicTypeEnum::StructType(self.module.get_struct_type("obj").unwrap());
        let func_lltype = ret_lltype.fn_type(&arg_lltypes, false);
        let _func = self.module.add_function(func_name, func_lltype, None);
    }

    pub fn codegen_func(&mut self, fundef: &FunDef) {
        let (ref func_name, ref _func_ty) = fundef.name;
        let func = self.module.get_function(func_name).unwrap();
        let basic_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(basic_block);
        let mut env = Env::new();
        let mut args = Vec::new();
        for arg in fundef.args.iter() {
            args.push(arg.0.to_string());
        }
        let mut fv_types = Vec::new();
        for _ in fundef.formal_fv.iter() {
            let obj_type = self.module.get_struct_type("obj").unwrap();
            fv_types.push(BasicTypeEnum::StructType(obj_type));
        }
        let fv_struct_ptr_ty = self.closure_fv_struct(fv_types);
        let fv_i8_ptr = func.get_first_param().unwrap();

        // let fv_struct_ptr_ty = fv_struct_ty.ptr_type(AddressSpace::Generic);
        let fv_ptr = self.builder.build_bitcast(fv_i8_ptr, fv_struct_ptr_ty, "tmp");
        for (i, (name, _ty)) in fundef.formal_fv.iter().enumerate() {
            let obj_ptr = self.builder.build_struct_gep(fv_ptr.into_pointer_value(), i.try_into().unwrap(), name).unwrap();
            env.set(name.to_string(), BasicValueEnum::PointerValue(obj_ptr));
        }
        
        for (arg_name, param) in args.iter().zip(func.get_param_iter().skip(1)) {
            let arg_ptr = self.builder.build_alloca(param.get_type(), arg_name);
            self.builder.build_store(arg_ptr, param);
            env.set(arg_name.to_string(), BasicValueEnum::PointerValue(arg_ptr));
        }
        let func_res = self.codegen(&fundef.body, Rc::new(RefCell::new(env)));
        self.builder.build_return(Some(&func_res));
    }

    pub fn codegen_toplevel(&mut self, fundefs: Vec<FunDef>) {
        for fundef in fundefs.iter() {
            self.codegen_declare(fundef);
        }
        for fundef in fundefs.iter() {
            self.codegen_func(fundef);
        }
    }

    pub fn codegen(&mut self, node: &CNode, env: Rc<RefCell<Env<'ctx>>>) -> BasicValueEnum<'ctx> {
        use inkwell::values::BasicValueEnum::*;
        use crate::closure::CNode::*;

        match *node {
            Int(ref n) => {
                let int_ty = self.type_to_llvmty(&Type::Int);
                let res = int_ty.into_int_type().const_int((*n) as u64, false);
                self.create_obj(IntValue(res), OBJ_INT)
            },
            Bool(ref b) => {
                let bool_ty = self.type_to_llvmty(&Type::Bool);
                let res = bool_ty.into_int_type().const_int(*b as u64, false);
                self.create_obj(IntValue(res), OBJ_BOOL)
            },
            VarExpr(ref name, ref _ty) => {
                let var = env.deref().borrow().get(name).unwrap();
                let res = match var {
                    PointerValue(p) => {
                        let deref_p_ty = p.get_type().get_element_type();
                        match deref_p_ty {
                            AnyTypeEnum::FunctionType(_) => PointerValue(p),
                            _ => self.builder.build_load(p, "tmp"),
                        }
                    },
                    _ =>  {
                        println!("{:?}", var);
                        unimplemented!()
                    },
                };
                res
            },
            Not(ref expr) => {
                let expr_obj = self.codegen(expr, env);
                let expr_val = self.get_obj_data(expr_obj, &Type::Bool);
                let res = self.builder.build_not(expr_val.into_int_value(), "tmp");
                self.create_obj(IntValue(res), OBJ_BOOL)
            },
            Tuple(ref exprs, ref ty) => {
                let mut objs = Vec::new();
                for expr in exprs.iter() {
                    let obj = self.codegen(expr, env.clone());
                    objs.push(obj);
                }
                let tuple_llty = self.create_tuple_type(ty);
                let tuple_obj = self.builder.build_malloc(tuple_llty, "tmp").unwrap();
                for (i, obj) in objs.into_iter().enumerate() {
                    let field = self.builder.build_struct_gep(tuple_obj, i.try_into().unwrap(), "tmp").unwrap();
                    self.builder.build_store(field, obj);
                }

                self.create_obj(BasicValueEnum::PointerValue(tuple_obj), OBJ_TUPLE)
            },
            Expr { ref lhs, ref op, ref rhs, ref ty } => {
                let lhs_val = self.codegen(lhs, env.clone());
                let rhs_val = self.codegen(rhs, env.clone());

                match *ty {
                    Type::Int => {
                        let lval = self.get_obj_data(lhs_val, &Type::Int).into_int_value();
                        let rval = self.get_obj_data(rhs_val, &Type::Int).into_int_value();
                        let res = match op.as_str() {
                            "+" => self.builder.build_int_add(lval, rval, "tmp"),
                            "-" => self.builder.build_int_sub(lval, rval, "tmp"),
                            "*" => self.builder.build_int_mul(lval, rval, "tmp"),
                            "/" => self.builder.build_int_unsigned_div(lval, rval, "tmp"),
                            _ => unreachable!(),
                        };

                        self.create_obj(IntValue(res), OBJ_INT)
                    },
                    Type::Bool => {
                        let lval = self.get_obj_data(lhs_val, &Type::Int).into_int_value();
                        let rval = self.get_obj_data(rhs_val, &Type::Int).into_int_value();
                        let res = match op.as_str() {
                            "<=" => self.builder.build_int_compare(IntPredicate::SLE, lval, rval, "tmp"),
                            "=" => self.builder.build_int_compare(IntPredicate::EQ, lval, rval, "tmp"),
                            _ => unreachable!(),
                        };

                        self.create_obj(IntValue(res), OBJ_BOOL)
                    },
                    _ => unreachable!()
                }
            },
            IfExpr { ref cond, ref then_body, ref else_body, ty: ref _ty } => {
                let func = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let then_bb = self.context.append_basic_block(func, "if_then");
                let else_bb = self.context.append_basic_block(func, "if_else");
                let cont_bb = self.context.append_basic_block(func, "if_cont");
                
                let cond_obj =  self.codegen(cond, env.clone());
                let cond_val = self.get_obj_data(cond_obj, &Type::Bool).into_int_value();
                self.builder.build_conditional_branch(cond_val, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = self.codegen(then_body, env.clone());
                self.builder.build_unconditional_branch(cont_bb);
                
                let then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                let else_val = self.codegen(else_body, env.clone());
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.module.get_struct_type("obj").unwrap(), "iftmp");

                phi.add_incoming(&[
                    (&then_val, then_bb),
                    (&else_val, else_bb),
                ]);

                phi.as_basic_value()
            },
            LetExpr { ref name, ref first_expr, ref second_expr, ty: ref _ty} => {
                let first_val = self.codegen(first_expr, env.clone());
                let (ref id, ref _id_ty) = name;
                let ptr = self.builder.build_alloca(first_val.get_type(), id);
                self.builder.build_store(ptr, first_val);
                let mut env_child = Env::make_child(env);
                env_child.set(id.to_string(), PointerValue(ptr));
                let new_env = Rc::new(RefCell::new(env_child));
                let res = self.codegen(second_expr, new_env);
                res
            },
            LetTupleExpr { ref names, ref first_expr, ref second_expr, ref tuple_ty, ty: ref _ty } => {
                let tuple_obj = self.codegen(first_expr, env.clone());
                let tuple_ptr = self.get_obj_data(tuple_obj, tuple_ty).into_pointer_value();
                let mut env_child = Env::make_child(env);
                for (i, (name, ty)) in names.iter().enumerate() {
                    let field = self.builder.build_struct_gep(tuple_ptr, i.try_into().unwrap(), "tmp").unwrap();
                    let data = self.builder.build_load(field, "tmp");

                    let obj_type = self.module.get_struct_type("obj").unwrap();
                    let ptr = self.builder.build_alloca(obj_type, name);
                    self.builder.build_store(ptr, self.create_obj(data, self.type_to_tag(ty)));
                    env_child.set(name.to_string(), PointerValue(ptr));
                }
                self.builder.build_free(tuple_ptr);
                
                let new_env = Rc::new(RefCell::new(env_child));
                let res = self.codegen(second_expr, new_env);
                res
            },
            MakeCls {
                ref name,
                ref actual_fv,
                ref second_expr,
                ty: ref _ty,
            } => {
                let (ref id, ref _id_ty) = name;
                let closure = self.make_closure(env.clone(), id, actual_fv);
                let closure_ptr = self.builder.build_alloca(closure.get_type(), "tmp");
                self.builder.build_store(closure_ptr, closure);
                let mut new_env = Env::make_child(env.clone());
                new_env.set(id.to_string(), BasicValueEnum::PointerValue(closure_ptr));
                let new_env_rc = Rc::new(RefCell::new(new_env));
                let result = self.codegen(&*second_expr, new_env_rc);
                result
            },
            AppCls {
                ref func,
                ref args,
                ref func_ty,
                ty: ref _ty,
            } => {
                let clos_obj = self.codegen(&*func, env.clone());
                let mut clos_struct_ty_elems = Vec::new();
                clos_struct_ty_elems.push(self.type_to_llvmty(func_ty));
                clos_struct_ty_elems.push(BasicTypeEnum::PointerType(self.context.i8_type().ptr_type(AddressSpace::Generic)));
                let clos_struct_ptr_ty = self.context.struct_type(clos_struct_ty_elems.as_slice(), false).ptr_type(AddressSpace::Generic);
                let clos_struct_ptr = self.get_obj_data_llvmty(clos_obj, BasicTypeEnum::PointerType(clos_struct_ptr_ty));

                let func_ptr_field = self.builder.build_struct_gep(clos_struct_ptr.into_pointer_value(), 0, "tmp").unwrap();
                let func_ptr = self.builder.build_load(func_ptr_field, "tmp").into_pointer_value();

                let mut func_args = Vec::new();
                let fv_field = self.builder.build_struct_gep(clos_struct_ptr.into_pointer_value(), 1, "tmp").unwrap();
                let fv_i8_ptr = self.builder.build_load(fv_field, "tmp");
                func_args.push(fv_i8_ptr);
                for arg in args.iter() {
                    let arg_obj = self.codegen(arg, env.clone());
                    func_args.push(arg_obj);
                }

                let result = self.builder.build_call(func_ptr, func_args.as_slice(), "tmp").try_as_basic_value().unwrap_left();
                result
            },
            AppDir {
                ref func,
                ref args,
                func_ty: ref _func_ty,
                ty: ref _ty,
            } => {
                let func_ll = self.module.get_function(&func).unwrap();
                let mut arg_objs = Vec::new();
                let i8_ptr_null = self.context.i8_type().ptr_type(AddressSpace::Generic).const_null();
                arg_objs.push(BasicValueEnum::PointerValue(i8_ptr_null));
                for arg in args.iter() {
                    arg_objs.push(self.codegen(arg, env.clone()));
                }
                
                let result = self.builder.build_call(func_ll, arg_objs.as_slice(), "tmp").try_as_basic_value().unwrap_left();
                result
            },
        }
    }
}

pub fn codegen(prog: (Vec<FunDef>, CNode), file_name: String, target_name: String, target_triple: String) {
    let config = InitializationConfig::default();
    Target::initialize_all(&config);
    let context = Context::create();
    let module = context.create_module("rvml");
    let builder = context.create_builder();
    let mut c = CodeGen::new(&context, module, builder);
    c.codegen_init(prog.0);
    let env = Rc::new(RefCell::new(Env::new()));
    let res = c.codegen(&prog.1, env);
    let v = c.get_obj_data(res, &Type::Int).into_int_value();
    c.builder.build_return(Some(&v));
    // c.print_ir();
    c.gen_objfile(file_name, target_name, target_triple);
}
