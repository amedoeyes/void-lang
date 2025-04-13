export module voidlang.print_ast;

import std;
import voidlang.ast;
import voidlang.utility;

namespace voidlang {

void print_indent(std::int32_t indent) {
	std::print("{}", std::string(static_cast<std::size_t>(indent), ' '));
}

void print_literal(const literal& lit, std::int32_t indent) {
	print_indent(indent);
	visit(
		lit,
		[&](const integer_literal& i) { std::println("Integer literal: {}", i.value); },
		[&](const float_literal& f) { std::println("Float literal: {}", f.value); });
}

void print_identifier(const identifier& id, std::int32_t indent) {
	print_indent(indent);
	std::println("Identifier: {}", id.name);
}

void print_type(const type& type, std::int32_t indent) {
	visit(
		type,
		[&](const builtin_type& bt) {
			print_indent(indent);
			std::print("Builtin type: ");
			switch (bt.kind) {
				using enum builtin_type::kind;
				case i8:    std::print("i8"); break;
				case i16:   std::print("i16"); break;
				case i32:   std::print("i32"); break;
				case i64:   std::print("i64"); break;
				case f32:   std::print("f32"); break;
				case f64:   std::print("f64"); break;
				case void_: std::print("void"); break;
			}
			std::println("");
		},
		[&](const function_type& ft) {
			print_indent(indent);
			std::println("Function type:");

			if (!ft.parameters.empty()) {
				print_indent(indent + 2);
				std::println("Params:");
				for (const auto& param : ft.parameters) print_type(param.get(), indent + 4);
			}

			print_indent(indent + 2);
			std::println("Return:");
			print_type(ft.return_type.get(), indent + 4);
		},
		[&](const identifier_type& it) { std::println("Identifier type: {}", it.name); });
}

void print_expression(const expression& expr, std::int32_t indent) {
	visit(
		expr,
		[&](const literal& lit) { print_literal(lit, indent); },

		[&](const identifier& id) { print_identifier(id, indent); },

		[&](const function_expr& fun) {
			print_indent(indent);
			std::println("Function expression:");

			if (!fun.parameters.empty()) {
				print_indent(indent + 2);
				std::println("Params:");
				for (const auto& param : fun.parameters) print_identifier(param, indent + 4);
			}

			print_indent(indent + 2);
			std::println("Expression:");
			print_expression(fun.expression.get(), indent + 4);
		},

		[&](const function_call_expr& fun_call) {
			print_indent(indent);
			std::println("Function call expression:");

			print_expression(fun_call.callee.get(), indent + 2);

			if (!fun_call.arguments.empty()) {
				print_indent(indent + 2);
				std::println("Arguments:");
				for (const auto& arg : fun_call.arguments) print_expression(arg.get(), indent + 4);
			}
		},

		[&](const binary_operation& op) {
			print_indent(indent);
			std::println("Binary operation:");

			print_indent(indent + 2);
			std::print("Kind: ");
			switch (op.kind) {
				using enum binary_operation::kind;
				case add:         std::print("add"); break;
				case sub:         std::print("sub"); break;
				case mult:        std::print("mult"); break;
				case div:         std::print("div"); break;
				case logical_and: std::print("logical_and"); break;
				case logical_or:  std::print("logical_or"); break;
				case equal_equal: std::print("equal_equal"); break;
				case bang_equal:  std::print("bang_equal"); break;
				case bitwise_and: std::print("bitwise_and"); break;
				case bitwise_or:  std::print("bitwise_or"); break;
				case bitwise_xor: std::print("bitwise_xor"); break;
			}
			std::println("");

			print_indent(indent + 2);
			std::println("Left:");
			print_expression(op.lhs.get(), indent + 4);

			print_indent(indent + 2);
			std::println("Right:");
			print_expression(op.rhs.get(), indent + 4);
		},

		[&](const ternary_operation& op) {
			print_indent(indent);
			std::println("Ternary operation:");

			print_indent(indent + 2);
			std::println("Condition:");
			print_expression(op.condition.get(), indent + 4);

			print_indent(indent + 2);
			std::println("True branch:");
			print_expression(op.true_branch.get(), indent + 4);

			print_indent(indent + 2);
			std::println("False branch:");
			print_expression(op.false_branch.get(), indent + 4);
		});
}

void print_variable_declaration(const variable_declaration& var, std::int32_t indent = 0) {
	print_indent(indent);
	std::println("Variable declaration:");

	print_indent(indent + 2);
	std::println("Name:");
	print_identifier(var.name, indent + 4);

	if (var.type) {
		print_indent(indent + 2);
		std::println("Type:");
		print_type(*var.type, indent + 4);
	}

	print_indent(indent + 2);
	std::println("Initializer:");
	print_expression(var.initializer, indent + 4);
}

}  // namespace voidlang

export namespace voidlang {

void print_ast(const top_level& root) {
	std::println("Top level:");

	for (const auto& decl : root.declarations) {
		visit(decl, [&](const variable_declaration& var) { print_variable_declaration(var, 2); });
	}
}

}  // namespace voidlang
