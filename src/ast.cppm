export module voidlang:ast;

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
	using variant::variant;
};

struct integer_literal {
	std::int64_t value;
};

struct float_literal {
	double value;
};

struct literal : std::variant<integer_literal, float_literal> {
	using variant::variant;
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

enum class prefix_operator : std::int8_t {
	negate,
	logical_not,
	bit_not,
	increment,
	decrement,
};

struct prefix_operation {
	prefix_operator kind;
	recursive_wrapper<struct expression> expression;
};

enum class postfix_operator : std::int8_t {
	increment,
	decrement,
};

struct postfix_operation {
	postfix_operator kind;
	recursive_wrapper<struct expression> expression;
};

using unary_operation = std::variant<prefix_operation, postfix_operation>;

enum class binary_operator : std::int8_t {
	assign,
	assign_add,
	assign_sub,
	assign_mul,
	assign_div,
	assign_mod,
	assign_bit_and,
	assign_bit_or,
	assign_bit_xor,
	assign_bit_lshift,
	assign_bit_rshift,

	add,
	sub,
	mul,
	div,

	logical_and,
	logical_or,

	equal,
	not_equal,
	less_than,
	greater_than,
	less_than_equal,
	greater_than_equal,

	bit_and,
	bit_or,
	bit_xor,
	bit_lshift,
	bit_rshift,
};

struct binary_operation {
	binary_operator kind;
	recursive_wrapper<struct expression> lhs;
	recursive_wrapper<struct expression> rhs;
};

struct ternary_operation {
	recursive_wrapper<struct expression> condition;
	recursive_wrapper<struct expression> true_branch;
	recursive_wrapper<struct expression> false_branch;
};

struct expression : std::variant<literal,
                                 identifier,
                                 function_expr,
                                 function_call_expr,
                                 unary_operation,
                                 ternary_operation,
                                 binary_operation> {
	using variant::variant;
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
