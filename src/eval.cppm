export module voidlang.eval;

import std;
import voidlang.ast;
import voidlang.utility;

export namespace voidlang {

using value = std::variant<std::int64_t, double>;

class environment {
public:
	environment() = default;

	explicit environment(recursive_wrapper<environment> parent) : parent_{parent} {}

	auto define(const std::string& name, const value& value) noexcept -> void {
		variables_[name] = value;
	}

	[[nodiscard]]
	auto lookup(std::string_view name) const noexcept -> std::optional<value> {
		auto it = variables_.find(std::string{name});
		if (it != variables_.end()) return it->second;
		if (parent_) return (*parent_)->lookup(name);
		return std::nullopt;
	}

public:
	std::optional<recursive_wrapper<environment>> parent_;
	std::unordered_map<std::string, value> variables_;
};

class evaluator {
public:

private:
};

auto eval_expr(const expression& expr, environment& env) -> value {
	return visit(
		expr,
		[&](const literal& lit) -> value {
			return visit(
				lit,
				[&](const integer_literal& i) -> value { return i.value; },
				[&](const float_literal& f) -> value { return f.value; });
		},

		[&](const identifier& id) -> value { return *env.lookup(id.name); },

		[&](const function_expr& fun) -> value { return 0; },

		[&](const function_call_expr& fun_call) -> value { return 0; },

		[&](const binary_operation& op) -> value {
			switch (op.kind) {
				using enum binary_operation::kind;
				case add:
					return std::visit(
						[]<typename T, typename U>(const T& lhs, const U& rhs) -> value {
							if constexpr (std::is_same_v<T, std::int64_t> && std::is_same_v<U, std::int64_t>) {
								return lhs + rhs;
							}
							if constexpr (std::is_same_v<T, double> && std::is_same_v<U, double>) {
								return lhs + rhs;
							}
							return 0;
						},
						eval_expr(op.lhs.get(), env),
						eval_expr(op.rhs.get(), env));

				case sub:
					return std::visit(
						[]<typename T, typename U>(const T& lhs, const U& rhs) -> value {
							if constexpr (std::is_same_v<T, std::int64_t> && std::is_same_v<U, std::int64_t>) {
								return lhs - rhs;
							}
							if constexpr (std::is_same_v<T, double> && std::is_same_v<U, double>) {
								return lhs - rhs;
							}
							return 0;
						},
						eval_expr(op.lhs.get(), env),
						eval_expr(op.rhs.get(), env));

				case mult:
					return std::visit(
						[]<typename T, typename U>(const T& lhs, const U& rhs) -> value {
							if constexpr (std::is_same_v<T, std::int64_t> && std::is_same_v<U, std::int64_t>) {
								return lhs * rhs;
							}
							if constexpr (std::is_same_v<T, double> && std::is_same_v<U, double>) {
								return lhs * rhs;
							}
							return 0;
						},
						eval_expr(op.lhs.get(), env),
						eval_expr(op.rhs.get(), env));

				case div:
					return std::visit(
						[]<typename T, typename U>(const T& lhs, const U& rhs) -> value {
							if constexpr (std::is_same_v<T, std::int64_t> && std::is_same_v<U, std::int64_t>) {
								return lhs / rhs;
							}
							if constexpr (std::is_same_v<T, double> && std::is_same_v<U, double>) {
								return lhs / rhs;
							}
							return 0;
						},
						eval_expr(op.lhs.get(), env),
						eval_expr(op.rhs.get(), env));

				case logical_and: return 0;
				case logical_or:  return 0;
				case bitwise_and: return 0;
				case bitwise_or:  return 0;
				case bitwise_xor: return 0;
			}
		});
}

auto eval(const top_level& root) -> void {
	auto env = environment{};

	for (const auto& decl : root.declarations) {
		visit(decl, [&](const variable_declaration& var) { env.define(var.name.name, eval_expr(var.initializer, env)); });
	}

	for (const auto& [k, v] : env.variables_) {
		std::print("{}: ", k);
		visit(v, [&](auto&& v) { std::println("{}", v); });
	}
}

}  // namespace voidlang
