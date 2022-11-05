use std::fmt::{self, Write};

use lang_c::ast::*;
use lang_c::driver::{parse, Config};
use lang_c::span::Span;
use lang_c::visit::*;

pub struct Printer<'a> {
    w: &'a mut dyn fmt::Write,
    offset: usize,
}

impl<'a> Printer<'a> {
    pub fn new(w: &mut dyn fmt::Write) -> Printer {
        Printer { w: w, offset: 0 }
    }

    fn indent(&mut self) -> Printer {
        Printer {
            w: &mut self.w,
            offset: self.offset + 1,
        }
    }

    fn write_indent(&mut self) {
        write!(&mut self.w, "{1:0$}", self.offset * 4, "").unwrap();
    }

    fn write_indented(&mut self, text: &str) {
        self.write_indent();
        self.write(text);
    }

    fn space(&mut self) {
        write!(&mut self.w, " ").unwrap();
    }

    fn write(&mut self, text: &str) {
        write!(&mut self.w, "{}", text).unwrap();
    }

    fn writeln(&mut self, text: &str) {
        writeln!(&mut self.w, "{}", text).unwrap();
    }

    fn write_comma_sep<T, U>(&mut self, vec: &Vec<T>, action: U)
    where
        U: Fn(&mut Self, &T) -> (),
    {
        for (i, parameter) in vec.iter().enumerate() {
            if i > 0 && vec.len() > 1 {
                self.write(", ");
            }
            action(self, parameter);
        }
    }
}

impl<'ast, 'a> Visit<'ast> for Printer<'a> {
    fn visit_identifier(&mut self, n: &'ast Identifier, span: &'ast Span) {
        self.write(&n.name);
        visit_identifier(self, n, span);
    }
    fn visit_constant(&mut self, n: &'ast Constant, span: &'ast Span) {
        visit_constant(self, n, span);
    }
    fn visit_integer(&mut self, n: &'ast Integer, span: &'ast Span) {
        self.write(&n.number);
        visit_integer(self, n, span);
    }
    //fn visit_integer_base(&mut self, n: &'ast IntegerBase, span: &'ast Span) {
    //    self.name("IntegerBase");
    //    self.field(match *n {
    //        IntegerBase::Decimal => "Decimal",
    //        IntegerBase::Octal => "Octal",
    //        IntegerBase::Hexadecimal => "Hexadecimal",
    //        IntegerBase::Binary => "Binary",
    //    });
    //    visit_integer_base(self, n, span);
    //}
    //fn visit_integer_suffix(&mut self, n: &'ast IntegerSuffix, span: &'ast Span) {
    //    self.name("IntegerSuffix");
    //    self.field(n.unsigned);
    //    self.field(n.imaginary);
    //    visit_integer_suffix(self, n, span);
    //}
    //fn visit_integer_size(&mut self, n: &'ast IntegerSize, span: &'ast Span) {
    //    self.name("IntegerSize");
    //    self.field(match *n {
    //        IntegerSize::Int => "Int",
    //        IntegerSize::Long => "Long",
    //        IntegerSize::LongLong => "LongLong",
    //    });
    //    visit_integer_size(self, n, span);
    //}
    //fn visit_float(&mut self, n: &'ast Float, span: &'ast Span) {
    //    self.name("Float");
    //    self.field_str(&n.number);
    //    visit_float(self, n, span);
    //}
    //fn visit_float_base(&mut self, n: &'ast FloatBase, span: &'ast Span) {
    //    self.name("FloatBase");
    //    self.field(match *n {
    //        FloatBase::Decimal => "Decimal",
    //        FloatBase::Hexadecimal => "Hexadecimal",
    //    });
    //    visit_float_base(self, n, span);
    //}
    //fn visit_float_suffix(&mut self, n: &'ast FloatSuffix, span: &'ast Span) {
    //    self.name("FloatSuffix");
    //    self.field(n.imaginary);
    //    visit_float_suffix(self, n, span);
    //}
    //fn visit_float_format(&mut self, n: &'ast FloatFormat, span: &'ast Span) {
    //    self.name("FloatFormat");
    //    print_float_format(self, n);
    //    visit_float_format(self, n, span);
    //}
    fn visit_string_literal(&mut self, n: &'ast StringLiteral, span: &'ast Span) {
        self.write_comma_sep(n, |cont, string| {
            cont.write(string);
        });
    }
    //fn visit_expression(&mut self, n: &'ast Expression, span: &'ast Span) {
    //    self.name("Expression");
    //    visit_expression(self, n, span);
    //}
    //fn visit_member_operator(&mut self, n: &'ast MemberOperator, span: &'ast Span) {
    //    self.name("MemberOperator");
    //    self.field(match *n {
    //        MemberOperator::Direct => "Direct",
    //        MemberOperator::Indirect => "Indirect",
    //    });
    //    visit_member_operator(self, n, span);
    //}
    //fn visit_generic_selection(&mut self, n: &'ast GenericSelection, span: &'ast Span) {
    //    self.name("GenericSelection");
    //    visit_generic_selection(self, n, span);
    //}
    //fn visit_generic_association(&mut self, n: &'ast GenericAssociation, span: &'ast Span) {
    //    self.name("GenericAssociation");
    //    visit_generic_association(self, n, span);
    //}
    //fn visit_generic_association_type(
    //    &mut self,
    //    n: &'ast GenericAssociationType,
    //    span: &'ast Span,
    //) {
    //    self.name("GenericAssociationType");
    //    visit_generic_association_type(self, n, span);
    //}
    //fn visit_member_expression(&mut self, n: &'ast MemberExpression, span: &'ast Span) {
    //    self.name("MemberExpression");
    //    visit_member_expression(self, n, span);
    //}
    fn visit_call_expression(&mut self, n: &'ast CallExpression, _span: &'ast Span) {
        self.visit_expression(&n.callee.node, &n.callee.span);
        self.write("(");
        self.write_comma_sep(&n.arguments, |cont, arg| {
            cont.visit_expression(&arg.node, &arg.span);
        });
        self.write(")");
    }
    //fn visit_compound_literal(&mut self, n: &'ast CompoundLiteral, span: &'ast Span) {
    //    self.name("CompoundLiteral");
    //    visit_compound_literal(self, n, span);
    //}
    //fn visit_sizeofty(&mut self, n: &'ast SizeOfTy, span: &'ast Span) {
    //    self.name("SizeOfTy");
    //    visit_sizeofty(self, n, span);
    //}
    //fn visit_sizeofval(&mut self, n: &'ast SizeOfVal, span: &'ast Span) {
    //    self.name("SizeOfVal");
    //    visit_sizeofval(self, n, span);
    //}
    //fn visit_alignof(&mut self, n: &'ast AlignOf, span: &'ast Span) {
    //    self.name("AlignOf");
    //    visit_alignof(self, n, span);
    //}
    //fn visit_unary_operator(&mut self, n: &'ast UnaryOperator, span: &'ast Span) {
    //    self.name("UnaryOperator");
    //    self.field(match *n {
    //        UnaryOperator::PostIncrement => "PostIncrement",
    //        UnaryOperator::PostDecrement => "PostDecrement",
    //        UnaryOperator::PreIncrement => "PreIncrement",
    //        UnaryOperator::PreDecrement => "PreDecrement",
    //        UnaryOperator::Address => "Address",
    //        UnaryOperator::Indirection => "Indirection",
    //        UnaryOperator::Plus => "Plus",
    //        UnaryOperator::Minus => "Minus",
    //        UnaryOperator::Complement => "Complement",
    //        UnaryOperator::Negate => "Negate",
    //    });
    //    visit_unary_operator(self, n, span);
    //}
    //fn visit_unary_operator_expression(
    //    &mut self,
    //    n: &'ast UnaryOperatorExpression,
    //    span: &'ast Span,
    //) {
    //    self.name("UnaryOperatorExpression");
    //    visit_unary_operator_expression(self, n, span);
    //}
    //fn visit_cast_expression(&mut self, n: &'ast CastExpression, span: &'ast Span) {
    //    self.name("CastExpression");
    //    visit_cast_expression(self, n, span);
    //}
    fn visit_binary_operator(&mut self, n: &'ast BinaryOperator, span: &'ast Span) {
        self.space();
        self.write(match *n {
            BinaryOperator::Index => "TODOIndex",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulo => "%",
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRight => ">>",
            BinaryOperator::Less => "<",
            BinaryOperator::Greater => ">",
            BinaryOperator::LessOrEqual => "<=",
            BinaryOperator::GreaterOrEqual => ">=",
            BinaryOperator::Equals => "==",
            BinaryOperator::NotEquals => "!=",
            BinaryOperator::BitwiseAnd => "&",
            BinaryOperator::BitwiseXor => "^",
            BinaryOperator::BitwiseOr => "|",
            BinaryOperator::LogicalAnd => "&&",
            BinaryOperator::LogicalOr => "||",
            BinaryOperator::Assign => "=",
            BinaryOperator::AssignMultiply => "*=",
            BinaryOperator::AssignDivide => "/=",
            BinaryOperator::AssignModulo => "%=",
            BinaryOperator::AssignPlus => "+=",
            BinaryOperator::AssignMinus => "-=",
            BinaryOperator::AssignShiftLeft => "<<=",
            BinaryOperator::AssignShiftRight => ">>=",
            BinaryOperator::AssignBitwiseAnd => "&=",
            BinaryOperator::AssignBitwiseXor => "^=",
            BinaryOperator::AssignBitwiseOr => "|=",
        });
        self.space();
        visit_binary_operator(self, n, span);
    }
    fn visit_binary_operator_expression(
        &mut self,
        n: &'ast BinaryOperatorExpression,
        _span: &'ast Span,
    ) {
        self.visit_expression(&n.lhs.node, &n.lhs.span);
        self.visit_binary_operator(&n.operator.node, &n.operator.span);
        self.visit_expression(&n.rhs.node, &n.rhs.span);
    }
    //fn visit_conditional_expression(&mut self, n: &'ast ConditionalExpression, span: &'ast Span) {
    //    self.name("ConditionalExpression");
    //    visit_conditional_expression(self, n, span);
    //}
    //fn visit_va_arg_expression(&mut self, n: &'ast VaArgExpression, span: &'ast Span) {
    //    self.name("VaArgExpression");
    //    visit_va_arg_expression(self, n, span);
    //}
    //fn visit_offset_of_expression(&mut self, n: &'ast OffsetOfExpression, span: &'ast Span) {
    //    self.name("OffsetOfExpression");
    //    visit_offset_of_expression(self, n, span);
    //}
    //fn visit_offset_designator(&mut self, n: &'ast OffsetDesignator, span: &'ast Span) {
    //    self.name("OffsetDesignator");
    //    visit_offset_designator(self, n, span);
    //}
    //fn visit_offset_member(&mut self, n: &'ast OffsetMember, span: &'ast Span) {
    //    self.name("OffsetMember");
    //    print_offset_member(self, n);
    //    visit_offset_member(self, n, span);
    //}
    fn visit_declaration(&mut self, n: &'ast Declaration, span: &'ast Span) {
        self.write_indent();
        for specifier in &n.specifiers {
            self.visit_declaration_specifier(&specifier.node, &specifier.span);
        }
        self.write_comma_sep(&n.declarators, |cont, decl| {
            cont.visit_init_declarator(&decl.node, &decl.span);
        });
        self.writeln(";")
    }
    fn visit_declaration_specifier(&mut self, n: &'ast DeclarationSpecifier, span: &'ast Span) {
        visit_declaration_specifier(self, n, span);
    }
    fn visit_init_declarator(&mut self, n: &'ast InitDeclarator, span: &'ast Span) {
        visit_init_declarator(self, n, span);
    }
    //fn visit_storage_class_specifier(&mut self, n: &'ast StorageClassSpecifier, span: &'ast Span) {
    //    self.name("StorageClassSpecifier");
    //    self.field(match *n {
    //        StorageClassSpecifier::Typedef => "Typedef",
    //        StorageClassSpecifier::Extern => "Extern",
    //        StorageClassSpecifier::Static => "Static",
    //        StorageClassSpecifier::ThreadLocal => "ThreadLocal",
    //        StorageClassSpecifier::Auto => "Auto",
    //        StorageClassSpecifier::Register => "Register",
    //    });
    //    visit_storage_class_specifier(self, n, span);
    //}
    fn visit_type_specifier(&mut self, n: &'ast TypeSpecifier, span: &'ast Span) {
        print_type_specifier(self, n);
        visit_type_specifier(self, n, span);
    }
    //fn visit_ts18661_float_type(&mut self, n: &'ast TS18661FloatType, span: &'ast Span) {
    //    self.name("TS18661FloatType");
    //    self.field(n.width);
    //    visit_ts18661_float_type(self, n, span);
    //}
    //fn visit_ts18661_float_format(&mut self, n: &'ast TS18661FloatFormat, span: &'ast Span) {
    //    self.name("TS18661FloatFormat");
    //    self.field(match *n {
    //        TS18661FloatFormat::BinaryInterchange => "BinaryInterchange",
    //        TS18661FloatFormat::BinaryExtended => "BinaryExtended",
    //        TS18661FloatFormat::DecimalInterchange => "DecimalInterchange",
    //        TS18661FloatFormat::DecimalExtended => "DecimalExtended",
    //    });
    //    visit_ts18661_float_format(self, n, span);
    //}
    //fn visit_struct_type(&mut self, n: &'ast StructType, span: &'ast Span) {
    //    self.name("StructType");
    //    visit_struct_type(self, n, span);
    //}
    //fn visit_struct_kind(&mut self, n: &'ast StructKind, span: &'ast Span) {
    //    self.name("StructKind");
    //    self.field(match *n {
    //        StructKind::Struct => "Struct",
    //        StructKind::Union => "Union",
    //    });
    //    visit_struct_kind(self, n, span);
    //}
    //fn visit_struct_declaration(&mut self, n: &'ast StructDeclaration, span: &'ast Span) {
    //    self.name("StructDeclaration");
    //    visit_struct_declaration(self, n, span);
    //}
    //fn visit_struct_field(&mut self, n: &'ast StructField, span: &'ast Span) {
    //    self.name("StructField");
    //    visit_struct_field(self, n, span);
    //}
    //fn visit_specifier_qualifier(&mut self, n: &'ast SpecifierQualifier, span: &'ast Span) {
    //    self.name("SpecifierQualifier");
    //    visit_specifier_qualifier(self, n, span);
    //}
    //fn visit_struct_declarator(&mut self, n: &'ast StructDeclarator, span: &'ast Span) {
    //    self.name("StructDeclarator");
    //    visit_struct_declarator(self, n, span);
    //}
    //fn visit_enum_type(&mut self, n: &'ast EnumType, span: &'ast Span) {
    //    self.name("EnumType");
    //    visit_enum_type(self, n, span);
    //}
    //fn visit_enumerator(&mut self, n: &'ast Enumerator, span: &'ast Span) {
    //    self.name("Enumerator");
    //    visit_enumerator(self, n, span);
    //}
    //fn visit_type_qualifier(&mut self, n: &'ast TypeQualifier, span: &'ast Span) {
    //    self.name("TypeQualifier");
    //    self.field(match *n {
    //        TypeQualifier::Const => "Const",
    //        TypeQualifier::Restrict => "Restrict",
    //        TypeQualifier::Volatile => "Volatile",
    //        TypeQualifier::Nonnull => "Nonnull",
    //        TypeQualifier::NullUnspecified => "NullUnspecified",
    //        TypeQualifier::Nullable => "Nullable",
    //        TypeQualifier::Atomic => "Atomic",
    //    });
    //    visit_type_qualifier(self, n, span);
    //}
    //fn visit_function_specifier(&mut self, n: &'ast FunctionSpecifier, span: &'ast Span) {
    //    self.name("FunctionSpecifier");
    //    self.field(match *n {
    //        FunctionSpecifier::Inline => "Inline",
    //        FunctionSpecifier::Noreturn => "Noreturn",
    //    });
    //    visit_function_specifier(self, n, span);
    //}
    //fn visit_alignment_specifier(&mut self, n: &'ast AlignmentSpecifier, span: &'ast Span) {
    //    self.name("AlignmentSpecifier");
    //    visit_alignment_specifier(self, n, span);
    //}
    fn visit_declarator(&mut self, n: &'ast Declarator, span: &'ast Span) {
        visit_declarator(self, n, span);
    }
    fn visit_declarator_kind(&mut self, n: &'ast DeclaratorKind, span: &'ast Span) {
        print_declarator_kind(self, n);
        visit_declarator_kind(self, n, span);
    }
    fn visit_derived_declarator(&mut self, n: &'ast DerivedDeclarator, span: &'ast Span) {
        print_derived_declarator(self, n);
        visit_derived_declarator(self, n, span);
    }
    //fn visit_array_declarator(&mut self, n: &'ast ArrayDeclarator, span: &'ast Span) {
    //    self.name("ArrayDeclarator");
    //    visit_array_declarator(self, n, span);
    //}
    fn visit_function_declarator(&mut self, n: &'ast FunctionDeclarator, span: &'ast Span) {
        self.write("(");
        self.write_comma_sep(&n.parameters, |cont, param| {
            cont.visit_parameter_declaration(&param.node, &param.span);
        });
        self.visit_ellipsis(&n.ellipsis, span);
        self.write(") ");
    }
    //fn visit_pointer_qualifier(&mut self, n: &'ast PointerQualifier, span: &'ast Span) {
    //    self.name("PointerQualifier");
    //    visit_pointer_qualifier(self, n, span);
    //}
    //fn visit_array_size(&mut self, n: &'ast ArraySize, span: &'ast Span) {
    //    self.name("ArraySize");
    //    print_array_size(self, n);
    //    visit_array_size(self, n, span);
    //}
    //fn visit_parameter_declaration(&mut self, n: &'ast ParameterDeclaration, span: &'ast Span) {
    //    self.name("ParameterDeclaration");
    //    visit_parameter_declaration(self, n, span);
    //}
    //fn visit_ellipsis(&mut self, n: &'ast Ellipsis, span: &'ast Span) {
    //    self.name("Ellipsis");
    //    self.field(match *n {
    //        Ellipsis::Some => "Some",
    //        Ellipsis::None => "None",
    //    });
    //    visit_ellipsis(self, n, span);
    //}
    //fn visit_type_name(&mut self, n: &'ast TypeName, span: &'ast Span) {
    //    self.name("TypeName");
    //    visit_type_name(self, n, span);
    //}
    //fn visit_initializer(&mut self, n: &'ast Initializer, span: &'ast Span) {
    //    self.name("Initializer");
    //    visit_initializer(self, n, span);
    //}
    //fn visit_initializer_list_item(&mut self, n: &'ast InitializerListItem, span: &'ast Span) {
    //    self.name("InitializerListItem");
    //    visit_initializer_list_item(self, n, span);
    //}
    //fn visit_designator(&mut self, n: &'ast Designator, span: &'ast Span) {
    //    self.name("Designator");
    //    visit_designator(self, n, span);
    //}
    //fn visit_range_designator(&mut self, n: &'ast RangeDesignator, span: &'ast Span) {
    //    self.name("RangeDesignator");
    //    visit_range_designator(self, n, span);
    //}
    //fn visit_static_assert(&mut self, n: &'ast StaticAssert, span: &'ast Span) {
    //    self.name("StaticAssert");
    //    visit_static_assert(self, n, span);
    //}
    fn visit_statement(&mut self, n: &'ast Statement, span: &'ast Span) {
        let is_compound = if let Statement::Compound(_) = *n {
            true
        } else {
            false
        };
        if !is_compound {
            self.write_indent();
        }

        match &*n {
            Statement::Compound(_) => self.writeln("{"),
            Statement::Goto(_) => self.write("goto "),
            Statement::Continue => self.write("continue"),
            Statement::Break => self.write("break"),
            Statement::Return(_) => self.write("return "),

            _ => {}
        }
        if is_compound {
            visit_statement(&mut self.indent(), n, span);
            self.writeln("");
            self.writeln("}");
        } else {
            visit_statement(self, n, span);
            self.writeln(";");
        }
    }
    //fn visit_labeled_statement(&mut self, n: &'ast LabeledStatement, span: &'ast Span) {
    //    self.name("LabeledStatement");
    //    visit_labeled_statement(self, n, span);
    //}
    //fn visit_if_statement(&mut self, n: &'ast IfStatement, span: &'ast Span) {
    //    self.name("IfStatement");
    //    visit_if_statement(self, n, span);
    //}
    //fn visit_switch_statement(&mut self, n: &'ast SwitchStatement, span: &'ast Span) {
    //    self.name("SwitchStatement");
    //    visit_switch_statement(self, n, span);
    //}
    //fn visit_while_statement(&mut self, n: &'ast WhileStatement, span: &'ast Span) {
    //    self.name("WhileStatement");
    //    visit_while_statement(self, n, span);
    //}
    //fn visit_do_while_statement(&mut self, n: &'ast DoWhileStatement, span: &'ast Span) {
    //    self.name("DoWhileStatement");
    //    visit_do_while_statement(self, n, span);
    //}
    //fn visit_for_statement(&mut self, n: &'ast ForStatement, span: &'ast Span) {
    //    self.name("ForStatement");
    //    visit_for_statement(self, n, span);
    //}
    //fn visit_label(&mut self, n: &'ast Label, span: &'ast Span) {
    //    self.name("Label");
    //    print_label(self, n);
    //    visit_label(self, n, span);
    //}
    //fn visit_case_range(&mut self, n: &'ast CaseRange, span: &'ast Span) {
    //    self.name("CaseRange");
    //    visit_case_range(self, n, span);
    //}
    //fn visit_for_initializer(&mut self, n: &'ast ForInitializer, span: &'ast Span) {
    //    self.name("ForInitializer");
    //    print_for_initializer(self, n);
    //    visit_for_initializer(self, n, span);
    //}
    fn visit_block_item(&mut self, n: &'ast BlockItem, span: &'ast Span) {
        visit_block_item(self, n, span);
    }
    //fn visit_external_declaration(&mut self, n: &'ast ExternalDeclaration, span: &'ast Span) {
    //    self.name("ExternalDeclaration");
    //    visit_external_declaration(self, n, span);
    //}
    fn visit_function_definition(&mut self, n: &'ast FunctionDefinition, span: &'ast Span) {
        visit_function_definition(self, n, span);
    }
    //fn visit_extension(&mut self, n: &'ast Extension, span: &'ast Span) {
    //    self.name("Extension");
    //    visit_extension(self, n, span);
    //}
    //fn visit_attribute(&mut self, n: &'ast Attribute, span: &'ast Span) {
    //    self.name("Attribute");
    //    self.field_str(&n.name.node);
    //    visit_attribute(self, n, span);
    //}
    //fn visit_asm_statement(&mut self, n: &'ast AsmStatement, span: &'ast Span) {
    //    self.name("AsmStatement");
    //    visit_asm_statement(self, n, span);
    //}
    //fn visit_availability_attribute(&mut self, n: &'ast AvailabilityAttribute, span: &'ast Span) {
    //    self.name("AvailabilityAttribute");
    //    visit_availability_attribute(self, n, span);
    //}
    //fn visit_gnu_extended_asm_statement(
    //    &mut self,
    //    n: &'ast GnuExtendedAsmStatement,
    //    span: &'ast Span,
    //) {
    //    self.name("GnuExtendedAsmStatement");
    //    visit_gnu_extended_asm_statement(self, n, span);
    //}
    //fn visit_gnu_asm_operand(&mut self, n: &'ast GnuAsmOperand, span: &'ast Span) {
    //    self.name("GnuAsmOperand");
    //    visit_gnu_asm_operand(self, n, span);
    //}
    //fn visit_type_of(&mut self, n: &'ast TypeOf, span: &'ast Span) {
    //    self.name("TypeOf");
    //    visit_type_of(self, n, span);
    //}
    //fn visit_translation_unit(&mut self, translation_unit: &'ast TranslationUnit) {
    //    self.name("TranslationUnit");
    //    visit_translation_unit(self, translation_unit);
    //}
}

fn print_float_format<'ast>(p: &mut Printer, n: &'ast FloatFormat) {
    match *n {
        FloatFormat::Float => p.w.write_str(" Float").unwrap(),
        FloatFormat::Double => p.w.write_str(" Double").unwrap(),
        FloatFormat::LongDouble => p.w.write_str(" LongDouble").unwrap(),
        _ => {}
    }
}
fn print_declarator_kind<'ast>(p: &mut Printer, n: &'ast DeclaratorKind) {
    match *n {
        DeclaratorKind::Abstract => p.w.write_str(" abstract").unwrap(),
        _ => {}
    }
}
fn print_derived_declarator<'ast>(p: &mut Printer, n: &'ast DerivedDeclarator) {
    match *n {
        DerivedDeclarator::Pointer(_) => p.w.write_str(" Pointer").unwrap(),
        DerivedDeclarator::KRFunction(_) => p.w.write_str(" KRFunction").unwrap(),
        DerivedDeclarator::Block(_) => p.w.write_str(" Block").unwrap(),
        _ => {}
    }
}
fn print_array_size<'ast>(p: &mut Printer, n: &'ast ArraySize) {
    match *n {
        ArraySize::Unknown => p.w.write_str(" Unknown").unwrap(),
        ArraySize::VariableUnknown => p.w.write_str(" VariableUnknown").unwrap(),
        ArraySize::VariableExpression(_) => p.w.write_str(" VariableExpression").unwrap(),
        ArraySize::StaticExpression(_) => p.w.write_str(" StaticExpression").unwrap(),
    }
}
fn print_offset_member<'ast>(p: &mut Printer, n: &'ast OffsetMember) {
    match *n {
        OffsetMember::Member(_) => p.w.write_str(" Member").unwrap(),
        OffsetMember::IndirectMember(_) => p.w.write_str(" IndirectMember").unwrap(),
        _ => {}
    }
}
fn print_label<'ast>(p: &mut Printer, n: &'ast Label) {
    match *n {
        Label::Default => p.w.write_str(" Default").unwrap(),
        _ => {}
    }
}
fn print_for_initializer<'ast>(p: &mut Printer, n: &'ast ForInitializer) {
    match *n {
        ForInitializer::Empty => p.w.write_str(" Empty").unwrap(),
        _ => {}
    }
}
fn print_type_specifier<'ast>(p: &mut Printer, n: &'ast TypeSpecifier) {
    match *n {
        TypeSpecifier::Void => p.w.write_str("void ").unwrap(),
        TypeSpecifier::Char => p.w.write_str("char ").unwrap(),
        TypeSpecifier::Short => p.w.write_str("short ").unwrap(),
        TypeSpecifier::Int => p.w.write_str("int ").unwrap(),
        TypeSpecifier::Long => p.w.write_str("long ").unwrap(),
        TypeSpecifier::Float => p.w.write_str("float ").unwrap(),
        TypeSpecifier::Double => p.w.write_str("double ").unwrap(),
        TypeSpecifier::Signed => p.w.write_str("signed ").unwrap(),
        TypeSpecifier::Unsigned => p.w.write_str("unsigned ").unwrap(),
        TypeSpecifier::Complex => p.w.write_str("_Complex ").unwrap(),
        TypeSpecifier::Atomic(_) => p.w.write_str("_Atomic ").unwrap(),
        TypeSpecifier::TypedefName(_) => p.w.write_str("TODOTypeDefName ").unwrap(),
        _ => {}
    }
}

fn main() {
    let config = Config::default();
    let parsed = parse(&config, "test.c").expect("Couldn't parse file");

    let buffer = &mut String::new();
    Printer::new(buffer).visit_translation_unit(&parsed.unit);
    //lang_c::print::Printer::new(buffer).visit_translation_unit(&parsed.unit);
    println!("{buffer}");
}
