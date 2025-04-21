module;

#include <cassert>

export module voidlang:eval;

import std;
import voidlang.utility;
import :ast;

namespace voidlang {

struct function {
	std::vector<identifier> parameters;
	expression expression;
	symbol_table<struct value> symbol_table;
};

struct value : std::variant<bool, std::int64_t, double, function> {
	using variant::variant;
};

auto eval_expr(symbol_table<value>& st, const expression& expr) noexcept -> value;

auto eval_fun_call(symbol_table<value>& st, const function_call_expr& call) noexcept -> value {
	return std::visit(visitor{
						  [&](const function& fun) -> value {
							  auto new_env = fun.symbol_table;
							  new_env.set_parent(observer_ptr{&st});
							  for (const auto i : std::views::iota(0uz, call.arguments.size())) {
								  new_env.define(fun.parameters[i].name, eval_expr(st, call.arguments[i].get()));
							  }
							  return eval_expr(new_env, fun.expression);
						  },
						  [](const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, call.callee.get()));
}

auto eval_id(symbol_table<value>& st, const identifier& id) noexcept -> value {
	return st.lookup(id.name).value().get();
}

auto eval_lit(const literal& lit) noexcept -> value {
	return std::visit(visitor{[](auto&& l) -> value { return l.value; }}, lit);
}

auto eval_fun(symbol_table<value>& st, const function_expr& fun) noexcept -> value {
	return function{
		.parameters = fun.parameters,
		.expression = fun.expression.get(),
		.symbol_table = st.capture(),
	};
}

template<typename Op>
auto eval_bin_op(symbol_table<value>& st, const binary_operation& op, Op operation) noexcept -> value {
	return std::visit(visitor{
						  [operation](std::int64_t lhs, std::int64_t rhs) -> value { return operation(lhs, rhs); },
						  [operation](double lhs, double rhs) -> value { return operation(lhs, rhs); },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_add(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs + rhs; },
						  [](double lhs, double rhs) -> value { return lhs + rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_sub(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs - rhs; },
						  [](double lhs, double rhs) -> value { return lhs - rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_mul(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs * rhs; },
						  [](double lhs, double rhs) -> value { return lhs * rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_div(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs / rhs; },
						  [](double lhs, double rhs) -> value { return lhs / rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_land(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](bool lhs, bool rhs) -> value { return lhs && rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_lor(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](bool lhs, bool rhs) -> value { return lhs || rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_eq(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](bool lhs, bool rhs) -> value { return lhs == rhs; },
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs == rhs; },
						  [](double lhs, double rhs) -> value { return lhs == rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_neq(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](bool lhs, bool rhs) -> value { return lhs != rhs; },
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs != rhs; },
						  [](double lhs, double rhs) -> value { return lhs != rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_band(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](bool lhs, bool rhs) -> value { return lhs & rhs; },
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs & rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_bor(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](bool lhs, bool rhs) -> value { return lhs | rhs; },
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs | rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin_bxor(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	return std::visit(visitor{
						  [](bool lhs, bool rhs) -> value { return lhs ^ rhs; },
						  [](std::int64_t lhs, std::int64_t rhs) -> value { return lhs ^ rhs; },
						  [](const auto&, const auto&) -> value { std::unreachable(); },
					  },
	                  eval_expr(st, op.lhs.get()),
	                  eval_expr(st, op.rhs.get()));
}

auto eval_bin(symbol_table<value>& st, const binary_operation& op) noexcept -> value {
	switch (op.kind) {
		using enum binary_operator;
		case add:         return eval_bin_add(st, op);
		case sub:         return eval_bin_sub(st, op);
		case mul:         return eval_bin_mul(st, op);
		case div:         return eval_bin_div(st, op);
		case logical_and: return eval_bin_land(st, op);
		case logical_or:  return eval_bin_lor(st, op);
		case equal:       return eval_bin_eq(st, op);
		case not_equal:   return eval_bin_neq(st, op);
		case bit_and:     return eval_bin_band(st, op);
		case bit_or:      return eval_bin_bor(st, op);
		case bit_xor:     return eval_bin_bxor(st, op);
	}
}

auto eval_ter(symbol_table<value>& st, const ternary_operation& op) noexcept -> value {
	auto cond = std::visit(visitor{
							   [&](bool b) -> bool { return b; },
							   [&](const auto&) -> bool { std::unreachable(); },
						   },
	                       eval_expr(st, op.condition.get()));
	if (cond) return eval_expr(st, op.true_branch.get());
	return eval_expr(st, op.false_branch.get());
}

auto eval_expr(symbol_table<value>& st, const expression& expr) noexcept -> value {
	return std::visit(visitor{
						  [&](const literal& lit) { return eval_lit(lit); },
						  [&](const identifier& id) { return eval_id(st, id); },
						  [&](const function_expr& fun) { return eval_fun(st, fun); },
						  [&](const function_call_expr& call) { return eval_fun_call(st, call); },
						  [&](const unary_operation& op) {
							  assert(false);
							  return value{0};
						  },
						  [&](const binary_operation& op) { return eval_bin(st, op); },
						  [&](const ternary_operation& op) { return eval_ter(st, op); },
					  },
	                  expr);
}

}  // namespace voidlang

export namespace voidlang {

auto eval(const top_level& root) noexcept -> void {
	auto st = symbol_table<value>{};

	for (const auto& decl : root.declarations) {
		std::visit(
			visitor{
				[&](const variable_declaration& var) {
					std::visit(visitor{
								   [&](const function_expr&) {
									   st.define(var.name.name, eval_expr(st, var.initializer));
								   },
								   [&](const auto&) {},
							   },
			                   var.initializer);
				},
			},
			decl);
	}

	for (const auto& decl : root.declarations) {
		std::visit(
			visitor{
				[&](const variable_declaration& var) {
					std::visit(visitor{
								   [&](const function_expr&) {},
								   [&](const auto&) { st.define(var.name.name, eval_expr(st, var.initializer)); },
							   },
			                   var.initializer);
				},
			},
			decl);
	}

	st.print([](const value& value) {
		std::visit(visitor{
					   [&](const auto& v) { std::println("{}", v); },
					   [&](const function& fun) {
						   auto out = std::string{"("};
						   for (auto i = 0uz; i < fun.parameters.size(); ++i) {
							   out += fun.parameters[i].name;
							   if (i != fun.parameters.size() - 1) {
								   out += ", ";
							   }
						   }
						   out += ") -> expr";
						   std::println("{}", out);
					   },
				   },
		           value);
	});
}

}  // namespace voidlang
