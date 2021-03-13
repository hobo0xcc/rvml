use crate::{closure::*, env::Environment, typing::*};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::*;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::*;
use inkwell::values::*;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::*;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::{convert::TryInto, path::Path, unimplemented, unreachable};

type Env<'a> = Environment<String, BasicValueEnum<'a>>; // key: variable name, value: variable value

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: Module<'ctx>,
        builder: Builder<'ctx>,
    ) -> CodeGen<'ctx> {
        CodeGen {
            context: &context,
            module,
            builder,
        }
    }

    pub fn print_ir(&self) {
        self.module.print_to_stderr();
    }

    pub fn create_func_type(&self, ty: &Type) -> FunctionType<'ctx> {
        match *ty {
            Type::Func { ref args, ref ret } => {
                let mut arg_lltypes = Vec::new();
                arg_lltypes.push(BasicTypeEnum::PointerType(
                    self.context.i8_type().ptr_type(AddressSpace::Generic),
                ));
                for arg_ty in args.iter() {
                    arg_lltypes.push(self.type_to_llvmty(arg_ty));
                }
                let ret_lltype = self.type_to_llvmty(&**ret);

                let func_lltype = ret_lltype.fn_type(&arg_lltypes, false);
                func_lltype
            }
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
            }
            _ => unreachable!(),
        }
    }

    pub fn type_to_llvmty(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        use inkwell::types::BasicTypeEnum::*;

        match *ty {
            Type::Unit => IntType(self.context.i8_type()),
            Type::Int => IntType(self.context.i32_type()),
            Type::Float => FloatType(self.context.f64_type() /* self.context.f64_type() */),
            Type::Bool => IntType(self.context.bool_type()),
            Type::Array(ref ty) => {
                PointerType(self.type_to_llvmty(&**ty).ptr_type(AddressSpace::Generic))
            }
            Type::Tuple(ref _tys) => {
                PointerType(self.create_tuple_type(ty).ptr_type(AddressSpace::Generic))
            }
            Type::Func { args: _, ret: _ } => {
                let func_ptr_ty =
                    PointerType(self.create_func_type(ty).ptr_type(AddressSpace::Generic));
                let i8_fv_ptr_ty = self
                    .context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum();
                let closure_ty = self.context.struct_type(&[func_ptr_ty, i8_fv_ptr_ty], true);
                closure_ty
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum()
            }
            _ => {
                println!("{}", ty);
                unimplemented!()
            }
        }
    }

    pub fn closure_fv_struct(&mut self, fvs: Vec<BasicTypeEnum<'ctx>>) -> BasicTypeEnum<'ctx> {
        let fv_struct = self.context.struct_type(fvs.as_slice(), false);
        BasicTypeEnum::PointerType(fv_struct.ptr_type(AddressSpace::Generic))
    }

    pub fn make_closure_ty(
        &mut self,
        func_ptr: BasicTypeEnum<'ctx>,
        fvs: Vec<BasicTypeEnum<'ctx>>,
    ) -> BasicTypeEnum<'ctx> {
        let mut clos_ty_list = Vec::new();
        clos_ty_list.push(func_ptr);
        clos_ty_list.push(self.closure_fv_struct(fvs));

        let clos_ty = self.context.struct_type(clos_ty_list.as_slice(), true);
        BasicTypeEnum::StructType(clos_ty)
    }

    pub fn make_closure(
        &mut self,
        env: Rc<RefCell<Env<'ctx>>>,
        name: &String,
        ty: &Type,
        fvs: &Vec<String>,
    ) -> BasicValueEnum<'ctx> {
        let func = self.module.get_function(name).unwrap();
        let func_ptr = func.as_global_value().as_pointer_value();
        let mut fv_values = Vec::new();
        for fv in fvs.iter() {
            let fv_val_ptr = env.borrow().get(fv).unwrap();
            let fv_val = self
                .builder
                .build_load(fv_val_ptr.into_pointer_value(), "tmp");
            fv_values.push(fv_val);
        }

        let mut fv_types = Vec::new();
        for val in fv_values.iter() {
            fv_types.push(val.get_type());
        }

        let closure_ty =
            self.make_closure_ty(BasicTypeEnum::PointerType(func_ptr.get_type()), fv_types);

        let closure_ptr = self.builder.build_malloc(closure_ty, "tmp").unwrap();
        let func_ptr_field = self
            .builder
            .build_struct_gep(closure_ptr, 0, "tmp")
            .unwrap();
        self.builder
            .build_store(func_ptr_field, BasicValueEnum::PointerValue(func_ptr));

        let fv_struct_field = self
            .builder
            .build_struct_gep(closure_ptr, 1, "tmp")
            .unwrap();
        let fv_struct_ty = closure_ty
            .into_struct_type()
            .get_field_type_at_index(1)
            .unwrap()
            .into_pointer_type()
            .get_element_type()
            .into_struct_type();
        let fv_struct_ptr = self
            .builder
            .build_malloc(BasicTypeEnum::StructType(fv_struct_ty), "tmp")
            .unwrap();
        self.builder.build_store(fv_struct_field, fv_struct_ptr);
        for (i, val) in fv_values.into_iter().enumerate() {
            let fv_field = self
                .builder
                .build_struct_gep(fv_struct_ptr, i.try_into().unwrap(), "tmp")
                .unwrap();
            self.builder.build_store(fv_field, val);
        }

        let closure_cast_ty = self.type_to_llvmty(ty);
        let closure_cast_ptr = self
            .builder
            .build_bitcast(closure_ptr, closure_cast_ty, "tmp");

        closure_cast_ptr
    }

    pub fn gen_objfile(
        &mut self,
        file_path: String,
        target_name: String,
        target_triple_name: String,
    ) {
        let target_triple = if target_triple_name.is_empty() {
            TargetMachine::get_default_triple()
        } else {
            TargetTriple::create(target_triple_name.as_str())
        };
        let opt = OptimizationLevel::Default;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let target = Target::from_triple(&target_triple).unwrap();

        let target_machine = target
            .create_target_machine(&target_triple, target_name.as_str(), "", opt, reloc, model)
            .unwrap();
        let target_data = target_machine.get_target_data();
        let data_layout = target_data.get_data_layout();
        self.module.set_data_layout(&data_layout);

        let path = Path::new(file_path.as_str());
        assert!(target_machine
            .write_to_file(&self.module, FileType::Object, &path)
            .is_ok());
    }

    pub fn add_intrinsic(&self, name: &str, func_ty: FunctionType<'ctx>) {
        let _func = self.module.add_function(&name, func_ty, None);
    }

    pub fn get_intrinsic(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    pub fn set_default_intrinsics(&self) {
        let i8_ptr =
            BasicTypeEnum::PointerType(self.context.i8_type().ptr_type(AddressSpace::Generic));
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
        obj_type.set_body(&[i64_ty, i8_ty], true);
    }

    pub fn codegen_init(&mut self, fundefs: Vec<FunDef>) {
        self.set_default_struct();
        self.set_default_intrinsics();

        self.codegen_toplevel(fundefs);

        let i8_type = self.context.i8_type();
        let main_fn_type = i8_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
    }

    pub fn codegen_declare(&mut self, fundef: &FunDef) {
        let (ref func_name, ref func_ty) = fundef.name;

        let mut fv_types = Vec::new();
        for (_name, ty) in fundef.formal_fv.iter() {
            fv_types.push(self.type_to_llvmty(ty));
        }

        let mut arg_lltypes = Vec::new();
        arg_lltypes.push(
            self.context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .as_basic_type_enum(),
        );
        for (_name, ty) in fundef.args.iter() {
            arg_lltypes.push(self.type_to_llvmty(ty));
        }

        let ret_lltype = match *func_ty {
            Type::Func {
                args: ref _args,
                ref ret,
            } => self.type_to_llvmty(&**ret),
            _ => unreachable!(),
        };
        let func_lltype = ret_lltype.fn_type(&arg_lltypes, false);
        let _func = self.module.add_function(func_name, func_lltype, None);
    }

    pub fn codegen_func(&mut self, fundef: &FunDef) {
        let (ref func_name, ref func_ty) = fundef.name;
        let func = self.module.get_function(func_name).unwrap();
        let basic_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(basic_block);
        let mut env = Env::new();
        let mut args = Vec::new();
        for arg in fundef.args.iter() {
            args.push(arg.0.to_string());
        }
        let mut fv_types = Vec::new();
        for (_name, ty) in fundef.formal_fv.iter() {
            let val_type = self.type_to_llvmty(ty);
            fv_types.push(val_type);
        }
        let fv_struct_ptr_ty = self.closure_fv_struct(fv_types);
        let fv_i8_ptr = func.get_first_param().unwrap();
        let fv_ptr = self
            .builder
            .build_bitcast(fv_i8_ptr, fv_struct_ptr_ty, "tmp");

        let mut fv_names = Vec::new();
        for (i, (name, _ty)) in fundef.formal_fv.iter().enumerate() {
            fv_names.push(name.to_string());
            let val_ptr = self
                .builder
                .build_struct_gep(fv_ptr.into_pointer_value(), i.try_into().unwrap(), name)
                .unwrap();
            env.set(name.to_string(), BasicValueEnum::PointerValue(val_ptr));
        }

        for (arg_name, param) in args.iter().zip(func.get_param_iter().skip(1)) {
            let arg_ptr = self.builder.build_alloca(param.get_type(), arg_name);
            self.builder.build_store(arg_ptr, param);
            env.set(arg_name.to_string(), BasicValueEnum::PointerValue(arg_ptr));
        }

        let env_rc = Rc::new(RefCell::new(env));

        if fundef.is_recurs {
            let mut fundef_fv_names = Vec::new();
            for (name, _ty) in fundef.formal_fv.iter() {
                fundef_fv_names.push(name.to_string());
            }
            let closure_self =
                self.make_closure(env_rc.clone(), func_name, func_ty, &fundef_fv_names);
            let closure_self_ptr = self.builder.build_alloca(closure_self.get_type(), "tmp");
            self.builder.build_store(closure_self_ptr, closure_self);
            env_rc.borrow_mut().set(
                func_name.to_string(),
                closure_self_ptr.as_basic_value_enum(),
            );
        }

        let func_res = self.codegen(&fundef.body, env_rc.clone());

        self.builder.build_return(Some(&func_res));
    }

    pub fn stack_save(&mut self) -> BasicValueEnum<'ctx> {
        self.builder
            .build_call(self.get_intrinsic("llvm.stacksave").unwrap(), &[], "tmp")
            .try_as_basic_value()
            .unwrap_left()
    }

    pub fn stack_restore(&mut self, stack: BasicValueEnum<'ctx>) {
        self.builder.build_call(
            self.get_intrinsic("llvm.stackrestore").unwrap(),
            &[stack],
            "tmp",
        );
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
        use crate::closure::CNode::*;
        use inkwell::values::BasicValueEnum::*;

        match *node {
            Unit => {
                let unit_val = self.context.i8_type().const_int(0, false);
                IntValue(unit_val)
            }
            Int(ref n) => {
                let int_ty = self.type_to_llvmty(&Type::Int);
                let res = int_ty.into_int_type().const_int((*n) as u64, false);
                IntValue(res)
            }
            Float(ref f) => {
                let float_ty = self.type_to_llvmty(&Type::Float);
                let res = float_ty
                    .into_float_type()
                    .const_float((*f).try_into().unwrap());
                FloatValue(res)
            }
            Bool(ref b) => {
                let bool_ty = self.type_to_llvmty(&Type::Bool);
                let res = bool_ty.into_int_type().const_int(*b as u64, false);
                IntValue(res)
            }
            VarExpr(ref name, ref _ty, ref _subst, ref _is_extern) => {
                let var = env.deref().borrow().get(name).unwrap();
                let res = self.builder.build_load(var.into_pointer_value(), "tmp");
                res
            }
            Not(ref expr) => {
                let expr_val = self.codegen(expr, env);
                let res = self.builder.build_not(expr_val.into_int_value(), "tmp");
                IntValue(res)
            }
            Neg(ref expr) => {
                let expr_val = self.codegen(expr, env);
                let res = self.builder.build_int_neg(expr_val.into_int_value(), "tmp");
                IntValue(res)
            }
            FNeg(ref expr) => {
                let expr_val = self.codegen(expr, env);
                let res = self.builder.build_float_neg(expr_val.into_float_value(), "tmp");
                FloatValue(res)
            }
            Tuple(ref exprs, ref ty) => {
                let mut values = Vec::new();
                for expr in exprs.iter() {
                    let val = self.codegen(expr, env.clone());
                    values.push(val);
                }
                let tuple_llty = self.create_tuple_type(ty);

                let tuple_ptr = self.builder.build_malloc(tuple_llty, "tmp").unwrap();
                for (i, val) in values.into_iter().enumerate() {
                    let field = self
                        .builder
                        .build_struct_gep(tuple_ptr, i.try_into().unwrap(), "tmp")
                        .unwrap();
                    self.builder.build_store(field, val);
                }

                BasicValueEnum::PointerValue(tuple_ptr)
            }
            Array(ref size, ref expr, ref _ty) => {
                let size_val = self.codegen(size, env.clone());
                let expr_val = self.codegen(expr, env.clone());
                let array = self
                    .builder
                    .build_array_malloc(expr_val.get_type(), size_val.into_int_value(), "tmp")
                    .unwrap();

                let stack = self.stack_save();
                let counter = self.builder.build_alloca(self.context.i32_type(), "tmp");
                self.builder
                    .build_store(counter, self.context.i32_type().const_int(0, false));
                let before_bb = self.builder.get_insert_block().unwrap();
                let parent_func = before_bb.get_parent().unwrap();
                let array_bb = self.context.append_basic_block(parent_func, "tmp");
                let cont_bb = self.context.append_basic_block(parent_func, "tmp");

                self.builder.build_unconditional_branch(array_bb);

                self.builder.position_at_end(array_bb);
                let counter_val = self.builder.build_load(counter, "tmp");
                let elem = unsafe {
                    self.builder
                        .build_in_bounds_gep(array, &[counter_val.into_int_value()], "tmp")
                };
                self.builder.build_store(elem, expr_val);
                let next_counter_val = self.builder.build_int_add(
                    counter_val.into_int_value(),
                    self.context.i32_type().const_int(1, false),
                    "tmp",
                );
                self.builder.build_store(counter, next_counter_val);
                let cond = self.builder.build_int_compare(
                    IntPredicate::SLT,
                    next_counter_val,
                    size_val.into_int_value(),
                    "tmp",
                );
                self.builder
                    .build_conditional_branch(cond, array_bb, cont_bb);

                self.builder.position_at_end(cont_bb);
                self.stack_restore(stack);

                array.as_basic_value_enum()
            }
            Get(ref array, ref idx, ref _ty) => {
                let array_val = self.codegen(&**array, env.clone());
                let idx_val = self.codegen(&**idx, env.clone());

                let array_elem_ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        array_val.into_pointer_value(),
                        &[idx_val.into_int_value()],
                        "tmp",
                    )
                };
                let res = self.builder.build_load(array_elem_ptr, "tmp");
                res
            }
            Put(ref array, ref idx, ref expr, ref _ty) => {
                let array_val = self.codegen(&**array, env.clone());
                let idx_val = self.codegen(&**idx, env.clone());
                let expr_val = self.codegen(&**expr, env.clone());

                let array_elem_ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        array_val.into_pointer_value(),
                        &[idx_val.into_int_value()],
                        "tmp",
                    )
                };
                self.builder.build_store(array_elem_ptr, expr_val);
                self.context
                    .i8_type()
                    .const_int(0, false)
                    .as_basic_value_enum()
            }
            Expr {
                ref lhs,
                ref op,
                ref rhs,
                ref ty,
            } => {
                let lhs_val = self.codegen(lhs, env.clone());
                let rhs_val = self.codegen(rhs, env.clone());

                match *ty {
                    Type::Int => {
                        let lval = lhs_val.into_int_value();
                        let rval = rhs_val.into_int_value();
                        let res = match op.as_str() {
                            "+" => self.builder.build_int_add(lval, rval, "tmp"),
                            "-" => self.builder.build_int_sub(lval, rval, "tmp"),
                            "*" => self.builder.build_int_mul(lval, rval, "tmp"),
                            "/" => self.builder.build_int_unsigned_div(lval, rval, "tmp"),
                            _ => unreachable!(),
                        };

                        IntValue(res)
                    }
                    Type::Float => {
                        let lval = lhs_val.into_float_value();
                        let rval = rhs_val.into_float_value();
                        let res = match op.as_str() {
                            "+." => self.builder.build_float_add(lval, rval, "tmp"),
                            "-." => self.builder.build_float_sub(lval, rval, "tmp"),
                            "*." => self.builder.build_float_mul(lval, rval, "tmp"),
                            "/." => self.builder.build_float_div(lval, rval, "tmp"),
                            _ => unreachable!(),
                        };

                        FloatValue(res)
                    }
                    Type::Bool => {
                        let res = match (lhs.get_type(), rhs.get_type()) {
                            (Type::Int, Type::Int) => {
                                let lval = lhs_val.into_int_value();
                                let rval = rhs_val.into_int_value();
                                match op.as_str() {
                                    "<" => self.builder.build_int_compare(
                                        IntPredicate::SLT,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    ">" => self.builder.build_int_compare(
                                        IntPredicate::SGT,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    "<=" => self.builder.build_int_compare(
                                        IntPredicate::SLE,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    ">=" => self.builder.build_int_compare(
                                        IntPredicate::SGE,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    "=" => self.builder.build_int_compare(
                                        IntPredicate::EQ,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    "<>" => self.builder.build_int_compare(
                                        IntPredicate::NE,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    _ => unreachable!(),
                                }
                            }
                            (Type::Float, Type::Float) => {
                                let lval = lhs_val.into_float_value();
                                let rval = rhs_val.into_float_value();
                                match op.as_str() {
                                    "<" => self.builder.build_float_compare(
                                        FloatPredicate::OLT,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    ">" => self.builder.build_float_compare(
                                        FloatPredicate::OGT,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    "<=" => self.builder.build_float_compare(
                                        FloatPredicate::OLE,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    ">=" => self.builder.build_float_compare(
                                        FloatPredicate::OGE,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    "=" => self.builder.build_float_compare(
                                        FloatPredicate::OEQ,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    "<>" => self.builder.build_float_compare(
                                        FloatPredicate::ONE,
                                        lval,
                                        rval,
                                        "tmp",
                                    ),
                                    _ => unreachable!(),
                                }
                            }
                            _ => unreachable!(),
                        };

                        IntValue(res)
                    }
                    _ => unreachable!(),
                }
            }
            IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
                ty: ref _ty,
            } => {
                let func = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let then_bb = self.context.append_basic_block(func, "if_then");
                let else_bb = self.context.append_basic_block(func, "if_else");
                let cont_bb = self.context.append_basic_block(func, "if_cont");

                let cond_obj = self.codegen(cond, env.clone());
                let cond_val = cond_obj.into_int_value();
                self.builder
                    .build_conditional_branch(cond_val, then_bb, else_bb);

                self.builder.position_at_end(then_bb);
                let then_val = self.codegen(then_body, env.clone());
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                let else_val = self.codegen(else_body, env.clone());
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(then_val.get_type(), "iftmp");

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                phi.as_basic_value()
            }
            LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
                ty: ref _ty,
            } => {
                let first_val = self.codegen(first_expr, env.clone());
                let (ref id, ref _id_ty) = name;
                let ptr = self.builder.build_alloca(first_val.get_type(), id);
                self.builder.build_store(ptr, first_val);
                let mut env_child = Env::make_child(env);
                env_child.set(id.to_string(), PointerValue(ptr));
                let new_env = Rc::new(RefCell::new(env_child));
                let res = self.codegen(second_expr, new_env);
                res
            }
            LetTupleExpr {
                ref names,
                ref first_expr,
                ref second_expr,
                tuple_ty: ref _tuple_ty,
                ty: ref _ty,
            } => {
                let tuple_ptr = self.codegen(first_expr, env.clone()).into_pointer_value();
                let mut env_child = Env::make_child(env);
                for (i, (name, _ty)) in names.iter().enumerate() {
                    let field = self
                        .builder
                        .build_struct_gep(tuple_ptr, i.try_into().unwrap(), "tmp")
                        .unwrap();
                    let data = self.builder.build_load(field, "tmp");

                    let data_type = data.get_type();
                    let ptr = self.builder.build_alloca(data_type, name);
                    self.builder.build_store(ptr, data);
                    env_child.set(name.to_string(), PointerValue(ptr));
                }
                // self.builder.build_free(tuple_ptr);

                let new_env = Rc::new(RefCell::new(env_child));
                let res = self.codegen(second_expr, new_env);
                res
            }
            MakeCls {
                ref name,
                ref dups,
                ref actual_fv,
                ref second_expr,
                ty: ref _ty,
            } => {
                let (ref id, ref id_ty) = name;
                let mut new_env = Env::make_child(env.clone());
                if let Some(dups) = dups {
                    for (dup_id, dup_ty) in dups.iter() {
                        println!("{}", dup_id);
                        let closure = self.make_closure(env.clone(), dup_id, dup_ty, actual_fv);
                        let closure_ptr = self.builder.build_alloca(closure.get_type(), "tmp");
                        self.builder.build_store(closure_ptr, closure);
                        new_env.set(
                            dup_id.to_string(),
                            BasicValueEnum::PointerValue(closure_ptr),
                        );
                    }
                } else {
                    let closure = self.make_closure(env.clone(), id, id_ty, actual_fv);
                    let closure_ptr = self.builder.build_alloca(closure.get_type(), "tmp");
                    self.builder.build_store(closure_ptr, closure);
                    new_env.set(id.to_string(), BasicValueEnum::PointerValue(closure_ptr));
                }
                let new_env_rc = Rc::new(RefCell::new(new_env));
                let result = self.codegen(&*second_expr, new_env_rc);
                result
            }
            AppCls {
                ref func,
                ref args,
                func_ty: ref _func_ty,
                ty: ref _ty,
            } => {
                let clos_val = self.codegen(&*func, env.clone());

                let func_ptr_field = self
                    .builder
                    .build_struct_gep(clos_val.into_pointer_value(), 0, "tmp")
                    .unwrap();
                let func_ptr = self
                    .builder
                    .build_load(func_ptr_field, "tmp")
                    .into_pointer_value();

                let fv_field = self
                    .builder
                    .build_struct_gep(clos_val.into_pointer_value(), 1, "tmp")
                    .unwrap();
                let fv_ptr = self.builder.build_load(fv_field, "tmp");

                let mut func_args = Vec::new();
                func_args.push(fv_ptr);
                for arg in args.iter() {
                    let arg_val = self.codegen(arg, env.clone());
                    func_args.push(arg_val);
                }

                let result = self
                    .builder
                    .build_call(func_ptr, func_args.as_slice(), "tmp")
                    .try_as_basic_value()
                    .unwrap_left();
                result
            }
            AppDir {
                ref func,
                ref args,
                ref func_ty,
                ty: ref _ty,
            } => {
                let (name, _ty) = match **func {
                    CNode::VarExpr(ref name, ref ty, _, _) => (name.clone(), ty.clone()),
                    _ => unreachable!(),
                };
                if let Some(func_ll) = self.module.get_function(&name) {
                    let i8_ptr_null = self
                        .context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .const_null();

                    let mut arg_values = Vec::new();
                    arg_values.push(BasicValueEnum::PointerValue(i8_ptr_null));
                    for arg in args.iter() {
                        arg_values.push(self.codegen(arg, env.clone()));
                    }

                    let result = self
                        .builder
                        .build_call(func_ll, arg_values.as_slice(), "tmp")
                        .try_as_basic_value()
                        .unwrap_left();
                    result
                } else {
                    let func_llty = self.create_func_type(func_ty);
                    let func = if is_primitive(&name) {
                        match self.module.get_function(&format!("mincaml_{}", name)) {
                            Some(f) => f,
                            _ => self.module.add_function(&format!("mincaml_{}", name), func_llty, None)
                        }
                    } else {
                        self.module.add_function(&name, func_llty, None)
                    };

                    let mut arg_values = Vec::new();
                    let i8_ptr_null = self
                        .context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .const_null();
                    arg_values.push(BasicValueEnum::PointerValue(i8_ptr_null));
                    for arg in args.iter() {
                        arg_values.push(self.codegen(arg, env.clone()));
                    }

                    let result = self
                        .builder
                        .build_call(func, arg_values.as_slice(), "tmp")
                        .try_as_basic_value()
                        .unwrap_left();
                    result
                }
            }
        }
    }
}

pub fn codegen(
    prog: (Vec<FunDef>, CNode),
    file_name: String,
    target_name: String,
    target_triple: String,
) {
    println!("Codegen");
    let config = InitializationConfig::default();
    Target::initialize_all(&config);
    let context = Context::create();
    let module = context.create_module("program");
    let builder = context.create_builder();
    let mut c = CodeGen::new(&context, module, builder);
    c.codegen_init(prog.0);
    let env = Rc::new(RefCell::new(Env::new()));
    let res = c.codegen(&prog.1, env).into_int_value();
    // let v = c.get_obj_data(res, &Type::Int).into_int_value();
    c.builder.build_return(Some(&res));
    // c.print_ir();
    c.gen_objfile(file_name, target_name, target_triple);
}
