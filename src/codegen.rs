use crate::{env::Environment, typing::*};
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

    pub fn create_obj(&self, val: BasicValueEnum<'ctx>, ty: &Type) -> BasicValueEnum<'ctx> {
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
        let ty_num = match *ty {
            Type::Int => self.context.i8_type().const_int(OBJ_INT, false),
            Type::Bool => self.context.i8_type().const_int(OBJ_BOOL, false),
            Type::Func { args: _, ret: _ } => self.context.i8_type().const_int(OBJ_FUNC, false),
            Type::Tuple(_) => self.context.i8_type().const_int(OBJ_TUPLE, false),
            _ => unimplemented!(),
        };
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
        // let obj_type = self.context.struct_type(&[self.context.i64_type(), self.context.i8_type()], packed);
        // self.module.add_global(obj_type, None, "obj");
        let obj_type = self.context.opaque_struct_type("obj");
        let i64_ty = BasicTypeEnum::IntType(self.context.i64_type());
        let i8_ty = BasicTypeEnum::IntType(self.context.i8_type());
        obj_type.set_body(&[i64_ty, i8_ty], false);
    }

    pub fn codegen_init(&mut self) {
        self.set_default_struct();
        self.set_default_intrinsics();

        let i32_type = self.context.i32_type();
        let main_fn_type = i32_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
    }

    pub fn codegen(&mut self, node: &TypedNode, env: Rc<RefCell<Env<'ctx>>>) -> BasicValueEnum<'ctx> {
        use inkwell::values::BasicValueEnum::*;

        match *node {
            TypedNode::Int(ref n) => {
                let int_ty = self.type_to_llvmty(&Type::Int);
                let res = int_ty.into_int_type().const_int((*n) as u64, false);
                self.create_obj(IntValue(res), &Type::Int)
            },
            TypedNode::Bool(ref b) => {
                let bool_ty = self.type_to_llvmty(&Type::Bool);
                let res = bool_ty.into_int_type().const_int(*b as u64, false);
                self.create_obj(IntValue(res), &Type::Bool)
            },
            TypedNode::VarExpr(ref name, ref _ty) => {
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
            TypedNode::Not(ref expr) => {
                let expr_obj = self.codegen(expr, env);
                let expr_val = self.get_obj_data(expr_obj, &Type::Bool);
                let res = self.builder.build_not(expr_val.into_int_value(), "tmp");
                self.create_obj(IntValue(res), &Type::Bool)
            },
            TypedNode::Tuple(ref exprs, ref ty) => {
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

                self.create_obj(BasicValueEnum::PointerValue(tuple_obj), ty)
            },
            TypedNode::Expr { ref lhs, ref op, ref rhs, ref ty } => {
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

                        self.create_obj(IntValue(res), &Type::Int)
                    },
                    Type::Bool => {
                        let lval = self.get_obj_data(lhs_val, &Type::Int).into_int_value();
                        let rval = self.get_obj_data(rhs_val, &Type::Int).into_int_value();
                        let res = match op.as_str() {
                            "<=" => self.builder.build_int_compare(IntPredicate::SLE, lval, rval, "tmp"),
                            "=" => self.builder.build_int_compare(IntPredicate::EQ, lval, rval, "tmp"),
                            _ => unreachable!(),
                        };

                        self.create_obj(IntValue(res), &Type::Bool)
                    },
                    _ => unreachable!()
                }
            },
            TypedNode::IfExpr { ref cond, ref then_body, ref else_body, ty: ref _ty } => {
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
            TypedNode::LetExpr { ref name, ref first_expr, ref second_expr, ty: ref _ty} => {
                let first_val = self.codegen(first_expr, env.clone());
                let ptr = self.builder.build_alloca(first_val.get_type(), name);
                self.builder.build_store(ptr, first_val);
                let mut env_child = Env::make_child(env);
                env_child.set(name.to_string(), PointerValue(ptr));
                let new_env = Rc::new(RefCell::new(env_child));
                let res = self.codegen(second_expr, new_env);
                res
            },
            TypedNode::LetTupleExpr { ref names, ref first_expr, ref second_expr, ref tuple_ty, ty: ref _ty } => {
                let tuple_obj = self.codegen(first_expr, env.clone());
                let tuple_ptr = self.get_obj_data(tuple_obj, tuple_ty).into_pointer_value();
                let mut env_child = Env::make_child(env);
                for (i, (name, ty)) in names.iter().enumerate() {
                    let field = self.builder.build_struct_gep(tuple_ptr, i.try_into().unwrap(), "tmp").unwrap();
                    let data = self.builder.build_load(field, "tmp");

                    let obj_type = self.module.get_struct_type("obj").unwrap();
                    let ptr = self.builder.build_alloca(obj_type, name);
                    self.builder.build_store(ptr, self.create_obj(data, ty));
                    env_child.set(name.to_string(), PointerValue(ptr));
                }
                self.builder.build_free(tuple_ptr);
                
                let new_env = Rc::new(RefCell::new(env_child));
                let res = self.codegen(second_expr, new_env);
                res
            },
            TypedNode::LetRecExpr { ref name, ref args, ref first_expr, ref second_expr, ref func_ty, ty: ref _ty } => {
                let func_llty = self.create_func_type(func_ty);
                let func = self.module.add_function(name, func_llty, None);

                // create a basicblock of the new function and append it to the builder.
                let before_basic_block = self.builder.get_insert_block().unwrap();
                let basic_block = self.context.append_basic_block(func, "entry");
                self.builder.position_at_end(basic_block);

                // environment for `first_expr`.
                let mut env_child_first = Env::make_child(env.clone());

                // set function pointer to environment because it is rec annotated.
                let func_ptr = func.as_global_value().as_pointer_value();
                let func_obj = self.create_obj(PointerValue(func_ptr), &func_ty);
                let ptr = self.builder.build_alloca(func_obj.get_type(), "tmp");
                self.builder.build_store(ptr, func_obj);
                env_child_first.set(name.to_string(), PointerValue(ptr));

                // set args.
                for ((name, _ty), param) in args.iter().zip(func.get_param_iter()) {
                    let arg_ptr = self.builder.build_alloca(param.get_type(), name);
                    self.builder.build_store(arg_ptr, param);
                    env_child_first.set(name.to_string(), PointerValue(arg_ptr));
                }
                // wrap into Rc<RefCell<>>.
                let new_env_first = Rc::new(RefCell::new(env_child_first));

                let func_res = self.codegen(first_expr, new_env_first);
                self.builder.build_return(Some(&func_res));

                self.builder.position_at_end(before_basic_block);
                let mut env_child_second = Env::make_child(env.clone());

                let func_ptr = func.as_global_value().as_pointer_value();
                let func_obj = self.create_obj(PointerValue(func_ptr), &func_ty);
                let ptr = self.builder.build_alloca(func_obj.get_type(), "tmp");
                self.builder.build_store(ptr, func_obj);
                env_child_second.set(name.to_string(), PointerValue(ptr));
                let new_env_second = Rc::new(RefCell::new(env_child_second));

                let res = self.codegen(second_expr, new_env_second);

                res
            },
            TypedNode::App { ref func, ref args, ref func_ty, ty: ref _ty } => {
                let func_obj = self.codegen(func, env.clone());
                let func_val = self.get_obj_data(func_obj, func_ty).into_pointer_value();
                let mut arg_values = Vec::new();
                for a in args.iter() {
                    arg_values.push(self.codegen(a, env.clone()));
                }
                let res = self.builder.build_call(func_val, arg_values.as_slice(), "tmp").try_as_basic_value().unwrap_left();
                res
            }
            // _ => unimplemented!(),
        }
    }
}

pub fn codegen(node: TypedNode, file_name: String, target_name: String, target_triple: String) {
    let config = InitializationConfig::default();
    Target::initialize_all(&config);
    let context = Context::create();
    let module = context.create_module("rvml");
    let builder = context.create_builder();
    let mut c = CodeGen::new(&context, module, builder);
    c.codegen_init();
    let env = Rc::new(RefCell::new(Env::new()));
    let res = c.codegen(&node, env);
    let v = c.get_obj_data(res, &Type::Int).into_int_value();
    c.builder.build_return(Some(&v));
    c.print_ir();
    c.gen_objfile(file_name, target_name, target_triple);
}