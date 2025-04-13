export module voidlang.ast;

import std;
import voidlang.utility;

export namespace voidlang {

struct builtin_type {
	enum class kind : std::int8_t { i8, i16, i32, i64, f32, f64, void_ } kind;
};

struct function_type {
	std::vector<recursive_wrapper<struct type>> parameters;
	recursive_wrapper<struct type> return_type;
};

struct identifier_type {
	std::string name;
};

struct type : std::variant<builtin_type, identifier_type, function_type> {
	using std::variant<builtin_type, identifier_type, function_type>::variant;
};

struct integer_literal {
	std::int64_t value;
};

struct float_literal {
	double value;
};

struct literal : std::variant<integer_literal, float_literal> {
	using std::variant<integer_literal, float_literal>::variant;
};

struct identifier {
	std::string name;
};

struct function_expr {
	std::vector<identifier> parameters;
	recursive_wrapper<struct expression> expression;
};

struct function_call_expr {
	recursive_wrapper<struct expression> callee;
	std::vector<recursive_wrapper<struct expression>> arguments;
};

struct ternary_operation {
	recursive_wrapper<struct expression> condition;
	recursive_wrapper<struct expression> true_branch;
	recursive_wrapper<struct expression> false_branch;
};

struct binary_operation {
	enum class kind : std::int8_t {
		add,
		sub,
		mult,
		div,

		logical_and,
		logical_or,

		equal_equal,
		bang_equal,

		bitwise_and,
		bitwise_or,
		bitwise_xor
	} kind;
	recursive_wrapper<struct expression> lhs;
	recursive_wrapper<struct expression> rhs;
};

struct expression
	: std::variant<literal, identifier, function_expr, function_call_expr, binary_operation, ternary_operation> {
	using std::variant<literal, identifier, function_expr, function_call_expr, binary_operation, ternary_operation>::
		variant;
};

struct variable_declaration {
	identifier name;
	std::optional<type> type;
	expression initializer;
};

using declaration = std::variant<variable_declaration>;

struct top_level {
	std::vector<declaration> declarations;
};

}  // namespace voidlang
