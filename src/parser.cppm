export module voidlang.parser;

import std;
import voidlang.ast;
import voidlang.token;
import voidlang.utility;

namespace voidlang {

class parser {
public:
	explicit parser(std::span<const token> tokens, std::string_view buffer) : tokens_{tokens}, buffer_{buffer} {}

	auto parse() -> std::expected<top_level, std::string> {
		auto root = top_level{};

		while (!match(token_type::eof)) {
			switch (curr().type) {
				case token_type::kw_let: {
					const auto decl = parse_variable_decl();
					if (!decl) return error(decl.error());
					root.declarations.emplace_back(*decl);
					break;
				}

				default: return error("expected declaration");
			}
		}

		return root;
	}

private:
	std::span<const token> tokens_;
	std::size_t curr_ = 0;
	std::string_view buffer_;

	auto next() -> void {
		if (curr_ < tokens_.size()) ++curr_;
	}

	[[nodiscard]]
	auto curr() const -> const token& {
		return tokens_[curr_];
	}

	[[nodiscard]]
	auto peek(std::size_t n = 1) const -> const token& {
		return curr_ + n < tokens_.size() ? tokens_[curr_ + n] : tokens_[tokens_.size() - 1];
	}

	[[nodiscard]]
	auto match(token_type type) const -> bool {
		return curr().type == type;
	}

	[[nodiscard]]
	auto match(std::string_view sv) const -> bool {
		return curr().lexeme == sv;
	}

	template<typename... TokenTypes>
		requires(std::same_as<TokenTypes, token_type> && ...)
	[[nodiscard]]
	auto match(TokenTypes... types) const -> bool {
		auto offset = 0uz;
		const auto check = [&](token_type type) -> bool {
			const auto tok = offset == 0 ? curr() : peek(offset);
			++offset;
			return tok.type == type;
		};
		return (check(types) && ...);
	}

	[[nodiscard]]
	auto match_and_next(token_type type) -> bool {
		if (match(type)) {
			next();
			return true;
		}
		return false;
	}

	[[nodiscard]]
	auto error(std::string_view message) const -> std::unexpected<std::string> {
		auto msg = std::string{};

		msg += std::format("{}:{}: {}\n", curr().start_line, curr().start_column, message);

		auto iss = std::istringstream{std::string{buffer_}};
		auto line = std::string{};
		for (auto _ : std::views::iota(1uz, curr().start_line + 1)) {
			if (!std::getline(iss, line)) break;
		}
		std::ranges::replace(line, '\t', ' ');

		msg += std::format("{:4} | {}\n", curr().start_line, line);
		msg += std::format("     | {}", std::string(curr().start_column - 1, ' ') + "^");

		return std::unexpected{msg};
	}

	[[nodiscard]]
	auto expected_error(std::string_view expected) const -> std::unexpected<std::string> {
		return std::unexpected{std::format("expected {} but got '{}'", expected, curr().lexeme)};
	}

	auto parse_identifier() -> std::expected<identifier, std::string> {
		if (!match(token_type::identifier)) return expected_error("identifier");
		auto id = identifier{std::string{curr().lexeme}};
		next();
		return id;
	}

	auto parse_integer_lit() -> std::expected<integer_literal, std::string> {
		try {
			auto int_lit = integer_literal{std::stoll(std::string{curr().lexeme})};
			next();
			return int_lit;
		} catch (const std::exception&) {
			return std::unexpected{"invalid integer format"};
		}
	}

	auto parse_float_lit() -> std::expected<float_literal, std::string> {
		try {
			auto float_lit = float_literal{std::stod(std::string{curr().lexeme})};
			next();
			return float_lit;
		} catch (const std::exception&) {
			return std::unexpected{"invalid float format"};
		}
	}

	auto parse_literal() -> std::expected<literal, std::string> {
		switch (curr().type) {
			using enum token_type;

			case lit_integer: {
				const auto int_lit = parse_integer_lit();
				if (!int_lit) return std::unexpected{int_lit.error()};
				return *int_lit;
			}

			case lit_float: {
				const auto float_lit = parse_float_lit();
				if (!float_lit) return std::unexpected{float_lit.error()};
				return *float_lit;
			}

			default: return expected_error("literal");
		}
	}

	auto parse_variable_decl() -> std::expected<declaration, std::string> {
		next();

		auto var = variable_declaration{};

		const auto id = parse_identifier();
		if (!id) return std::unexpected{id.error()};
		var.name = *id;

		if (match_and_next(token_type::colon)) {
			const auto type = parse_type();
			if (!type) return std::unexpected{type.error()};
			var.type = *type;
		}

		if (!match_and_next(token_type::assignment)) return expected_error("'='");

		const auto expr = parse_expression();
		if (!expr) return std::unexpected{expr.error()};
		var.initializer = *expr;

		if (!match_and_next(token_type::semicolon)) return expected_error("';'");

		return var;
	}

	auto parse_type() -> std::expected<type, std::string> {
		switch (curr().type) {
			using enum token_type;

			case bt_i8:      next(); return builtin_type{builtin_type::kind::i8};
			case bt_i16:     next(); return builtin_type{builtin_type::kind::i16};
			case bt_i32:     next(); return builtin_type{builtin_type::kind::i32};
			case bt_i64:     next(); return builtin_type{builtin_type::kind::i64};
			case bt_f32:     next(); return builtin_type{builtin_type::kind::f32};
			case bt_f64:     next(); return builtin_type{builtin_type::kind::f64};
			case bt_void:    next(); return builtin_type{builtin_type::kind::void_};

			case identifier: {
				auto type = identifier_type{std::string{curr().lexeme}};
				next();
				return type;
			}

			case lparen: {
				next();

				auto type = function_type{};

				if (match_and_next(token_type::rparen)) {
					if (!match_and_next(token_type::arrow)) return expected_error("'->'");

					const auto return_type = parse_type();
					if (!return_type) return std::unexpected{return_type.error()};

					type.return_type = recursive_wrapper{*return_type};
					return type;
				}

				while (!match(token_type::eof)) {
					const auto param_type = parse_type();
					if (!param_type) return std::unexpected{param_type.error()};
					type.parameters.emplace_back(*param_type);

					if (match_and_next(token_type::comma)) continue;
					if (!match_and_next(token_type::rparen)) return expected_error("')'");
					if (!match_and_next(token_type::arrow)) return expected_error("'->'");

					const auto return_type = parse_type();
					if (!return_type) return std::unexpected{return_type.error()};
					type.return_type = recursive_wrapper{*return_type};

					return type;
				}
			}

			default: return std::unexpected{std::format("expected a type but got '{}'", curr().lexeme)};
		}
	}

	auto parse_expression() -> std::expected<expression, std::string> {
		return parse_logical_expr();
	}

	auto parse_logical_expr() -> std::expected<expression, std::string> {
		auto lhs = parse_bit_expr();
		if (!lhs) return lhs;

		while (match(token_type::logical_and) || match(token_type::logical_or)) {
			const auto kind = match(token_type::logical_and) ? binary_operation::kind::logical_and
			                                                 : binary_operation::kind::logical_or;
			next();

			auto rhs = parse_bit_expr();
			if (!rhs) return rhs;

			lhs = binary_operation{
				.kind = kind,
				.lhs{std::move(*lhs)},
				.rhs{std::move(*rhs)},
			};
		}

		return *lhs;
	}

	auto parse_bit_expr() -> std::expected<expression, std::string> {
		auto lhs = parse_add_expr();
		if (!lhs) return lhs;

		while (match(token_type::ampersand) || match(token_type::pipe) || match(token_type::caret)) {
			auto kind = binary_operation::kind::bitwise_and;
			switch (curr().type) {
				using enum token_type;
				case ampersand: kind = binary_operation::kind::bitwise_and; break;
				case pipe:      kind = binary_operation::kind::bitwise_or; break;
				case caret:     kind = binary_operation::kind::bitwise_xor; break;
				default:        std::unreachable();
			}
			next();

			auto rhs = parse_add_expr();
			if (!rhs) return rhs;

			lhs = binary_operation{
				.kind = kind,
				.lhs{std::move(*lhs)},
				.rhs{std::move(*rhs)},
			};
		}

		return *lhs;
	}

	auto parse_add_expr() -> std::expected<expression, std::string> {
		auto lhs = parse_mult_expr();
		if (!lhs) return lhs;

		while (match(token_type::plus) || match(token_type::hyphen)) {
			const auto kind = match(token_type::plus) ? binary_operation::kind::add : binary_operation::kind::sub;
			next();

			auto rhs = parse_mult_expr();
			if (!rhs) return rhs;

			lhs = binary_operation{
				.kind = kind,
				.lhs{std::move(*lhs)},
				.rhs{std::move(*rhs)},
			};
		}

		return *lhs;
	}

	auto parse_mult_expr() -> std::expected<expression, std::string> {
		auto lhs = parse_primary_expr();
		if (!lhs) return lhs;

		while (match(token_type::asterisk) || match(token_type::slash)) {
			const auto kind = match(token_type::asterisk) ? binary_operation::kind::mult : binary_operation::kind::div;
			next();

			auto rhs = parse_primary_expr();
			if (!rhs) return rhs;

			lhs = binary_operation{
				.kind = kind,
				.lhs{std::move(*lhs)},
				.rhs{std::move(*rhs)},
			};
		}

		return *lhs;
	}

	auto parse_primary_expr() -> std::expected<expression, std::string> {
		switch (curr().type) {
			case token_type::lit_integer:
			case token_type::lit_float:   {
				const auto lit = parse_literal();
				if (!lit) return std::unexpected{lit.error()};
				return *lit;
			}

			case token_type::identifier: {
				const auto id = parse_identifier();
				if (!id) return std::unexpected{id.error()};

				auto expr = expression{*id};

				while (match_and_next(token_type::lparen)) {
					function_call_expr fun_call{.callee = recursive_wrapper{std::move(expr)}};

					while (!match_and_next(token_type::rparen) && !match(token_type::eof)) {
						const auto arg = parse_expression();
						if (!arg) return std::unexpected{arg.error()};
						fun_call.arguments.emplace_back(*arg);
					}

					expr = std::move(fun_call);
				}

				return expr;
			}

			case token_type::lparen: {
				next();

				if (match(token_type::rparen) || match(token_type::identifier, token_type::rparen)
				    || match(token_type::identifier, token_type::comma)) {
					auto expr = function_expr{};

					if (match_and_next(token_type::rparen)) {
						if (!match_and_next(token_type::arrow)) return expected_error("'->'");

						const auto new_expr = parse_expression();
						if (!new_expr) return std::unexpected{new_expr.error()};
						expr.expression = recursive_wrapper{*new_expr};

						return expr;
					}

					while (!match(token_type::eof)) {
						if (!match(token_type::identifier)) return expected_error("identifier");
						expr.parameters.emplace_back(std::string{curr().lexeme});
						next();

						if (match_and_next(token_type::comma)) continue;
						if (!match_and_next(token_type::rparen)) return expected_error("')'");
						if (!match_and_next(token_type::arrow)) return expected_error("'->'");

						const auto new_expr = parse_expression();
						if (!new_expr) return std::unexpected{new_expr.error()};
						expr.expression = recursive_wrapper{*new_expr};

						return expr;
					}
				}

				const auto expr = parse_expression();
				if (!match_and_next(token_type::rparen)) return expected_error("')'");
				return expr;
			}

			default: return std::unexpected{std::format("expected an expression got '{}'", curr().lexeme)};
		}
	}
};

}  // namespace voidlang

export namespace voidlang {

auto parse(std::span<const token> tokens, std::string_view buffer) -> std::expected<top_level, std::string> {
	return parser{tokens, buffer}.parse();
}

}  // namespace voidlang
