export module voidlang.eval;

import std;
import voidlang.ast;
import voidlang.utility;

export namespace voidlang {

struct function;

struct function {
	std::vector<identifier> parameters;
	expression expression;
	std::shared_ptr<class environment> environment;
};

using value = std::variant<literal, function>;

class environment {
public:
	environment() = default;

	explicit environment(recursive_wrapper<environment> parent) : parent_{parent} {}

	auto define(const std::string& name, const value& value) noexcept -> void {
		variables_[name] = value;
	}

	[[nodiscard]]
	auto lookup(std::string_view name) const noexcept -> std::optional<std::reference_wrapper<const value>> {
		auto it = variables_.find(std::string{name});
		if (it != variables_.end()) return it->second;
		if (parent_) return (*parent_)->lookup(name);
		return std::nullopt;
	}

public:
	std::optional<recursive_wrapper<environment>> parent_;
	std::unordered_map<std::string, value> variables_;
};

auto eval_expr(const expression& expr, environment& env) -> value;

template<typename Op>
auto eval_bin_op(const binary_operation& op, environment& env, Op operation) -> value {
	return std::visit(visitor{
						  [operation](const literal& lhs_lit, const literal& rhs_lit) -> value {
							  return std::visit(visitor{
													[operation](const integer_literal& lhs,
		                                                        const integer_literal& rhs) -> value {
														return integer_literal{operation(lhs.value, rhs.value)};
													},
													[operation](const float_literal& lhs,
		                                                        const float_literal& rhs) -> value {
														return float_literal{operation(lhs.value, rhs.value)};
													},
													[](auto&&, auto&&) -> value { return integer_literal{0}; },
												},
		                                        lhs_lit,
		                                        rhs_lit);
						  },
						  [](const auto&, const auto&) -> value { return integer_literal{0}; },
					  },
	                  eval_expr(op.lhs.get(), env),
	                  eval_expr(op.rhs.get(), env));
}

auto eval_bin_add(const binary_operation& op, environment& env) -> value {
	return eval_bin_op(op, env, [](auto a, auto b) { return a + b; });
}

auto eval_bin_sub(const binary_operation& op, environment& env) -> value {
	return eval_bin_op(op, env, [](auto a, auto b) { return a - b; });
}

auto eval_bin_mult(const binary_operation& op, environment& env) -> value {
	return eval_bin_op(op, env, [](auto a, auto b) { return a * b; });
}

auto eval_bin_div(const binary_operation& op, environment& env) -> value {
	return eval_bin_op(op, env, [](auto a, auto b) { return a / b; });
}

auto eval_fun_call(const function_call_expr& call, const function_expr& fun, environment& env) -> value {
	if (call.arguments.size() != fun.parameters.size()) return integer_literal{0};
	auto new_env = environment{env};
	for (auto i = 0uz; i < call.arguments.size(); ++i) {
		new_env.define(fun.parameters[i].name, eval_expr(call.arguments[i].get(), env));
	}
	return eval_expr(fun.expression.get(), new_env);
}

auto eval_fun_call(const function_call_expr& call, const function& fun, environment& env) -> value {
	if (call.arguments.size() != fun.parameters.size()) return integer_literal{0};
	auto new_env = environment{*fun.environment};
	for (auto i = 0uz; i < call.arguments.size(); ++i) {
		new_env.define(fun.parameters[i].name, eval_expr(call.arguments[i].get(), env));
	}
	return eval_expr(fun.expression, new_env);
}

auto eval_fun_call(const value& callee, const function_call_expr& call, environment& env) -> value {
	return std::visit(visitor{
						  [&](const function& fun) -> value { return eval_fun_call(call, fun, env); },
						  [](const auto&) -> value { return integer_literal{0}; },
					  },
	                  callee);
}

auto eval_fun_call(const function_call_expr& fun_call, environment& env) -> value {
	return std::visit(visitor{
						  [&](const function_expr& fun) -> value { return eval_fun_call(fun_call, fun, env); },
						  [&](const identifier& id) -> value {
							  auto val = env.lookup(id.name);
							  if (!val) return integer_literal{0};
							  return eval_fun_call(*val, fun_call, env);
						  },
						  [&](const function_call_expr& nest_call) -> value {
							  const auto val = eval_fun_call(nest_call, env);
							  return eval_fun_call(val, fun_call, env);
						  },
						  [](const auto&) -> value { return integer_literal{0}; },
					  },
	                  fun_call.callee.get());
}

auto eval_expr(const expression& expr, environment& env) -> value {
	return visit(
		expr,
		[&](const literal& lit) -> value { return lit; },

		[&](const identifier& id) -> value { return *env.lookup(id.name); },

		[&](const function_expr& fun) -> value {
			return function{
				.parameters = fun.parameters,
				.expression = fun.expression.get(),
				.environment = std::make_shared<environment>(env),
			};
		},

		[&](const function_call_expr& fun_call) -> value { return eval_fun_call(fun_call, env); },

		[&](const binary_operation& op) -> value {
			switch (op.kind) {
				using enum binary_operation::kind;
				case add:         return eval_bin_add(op, env);
				case sub:         return eval_bin_sub(op, env);
				case mult:        return eval_bin_mult(op, env);
				case div:         return eval_bin_div(op, env);
				case logical_and: return integer_literal{0};
				case logical_or:  return integer_literal{0};
				case bitwise_and: return integer_literal{0};
				case bitwise_or:  return integer_literal{0};
				case bitwise_xor: return integer_literal{0};
			}
		});
}

auto print(const value& value) -> void {
	std::visit(visitor{
				   [&](const literal& lit) { std::visit([](auto&& v) { std::println("{}", v.value); }, lit); },
				   [&](const function& fun) {
					   auto out = std::string{"("};
					   for (auto i = 0uz; i < fun.parameters.size(); ++i) {
						   out += fun.parameters[i].name;
						   if (i != fun.parameters.size() - 1) {
							   out += ", ";
						   }
					   }
					   out += ") -> idk";
					   std::println("{}", out);
				   },
			   },
	           value);
}

auto eval(const top_level& root) -> void {
	auto env = environment{};

	for (const auto& decl : root.declarations) {
		visit(decl,
		      [&](const variable_declaration& var) { env.define(var.name.name, eval_expr(var.initializer, env)); });
	}

	for (const auto& [name, value] : env.variables_) {
		std::print("{}: ", name);
		print(value);
	}
}

}  // namespace voidlang
