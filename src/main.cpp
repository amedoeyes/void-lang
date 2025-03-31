import std;
import lexer;

template<typename T>
struct recursive_wrapper {
	recursive_wrapper() noexcept = default;

	explicit recursive_wrapper(const T& value) noexcept : ptr{std::make_unique<T>(value)} {}

	recursive_wrapper(const recursive_wrapper& other) noexcept : ptr{std::make_unique<T>(*other.ptr)} {}

	recursive_wrapper(recursive_wrapper&& other) noexcept = default;

	auto operator=(const recursive_wrapper& other) noexcept -> recursive_wrapper& {
		if (this != &other) ptr = std::make_unique<T>(*other.ptr);
		return *this;
	}

	auto operator=(recursive_wrapper&& other) noexcept -> recursive_wrapper& = default;

	~recursive_wrapper() = default;

	[[nodiscard]]
	auto get() noexcept -> T& {
		return *ptr;
	}

	[[nodiscard]]
	auto get() const noexcept -> const T& {
		return *ptr;
	}

	[[nodiscard]]
	auto operator->() noexcept -> T* {
		return ptr.get();
	}

	[[nodiscard]]
	auto operator->() const noexcept -> const T* {
		return ptr.get();
	}

	[[nodiscard]]
	explicit operator T&() noexcept {
		return *ptr;
	}

	[[nodiscard]]
	explicit operator const T&() const noexcept {
		return *ptr;
	}

private:
	std::unique_ptr<T> ptr;
};

enum class token_type : std::uint8_t {
	assignment,
	semicolon,
	colon,
	lparen,
	rparen,
	comma,
	arrow,

	identifier,

	kw_let,

	bt_i8,
	bt_i16,
	bt_i32,
	bt_i64,
	bt_f32,
	bt_f64,
	bt_void,

	lit_integer,
	lit_float,

	unknown,
	eof,
};

auto token_name(token_type token) -> std::string {
	switch (token) {
		using enum token_type;

	case assignment: return "assignment";
	case semicolon: return "semicolon";
	case colon: return "colon";
	case lparen: return "lparen";
	case rparen: return "rparen";
	case comma: return "comma";
	case arrow: return "arrow";

	case identifier: return "identifier";

	case kw_let: return "kw_let";

	case bt_i8: return "bt_i8";
	case bt_i16: return "bt_i16";
	case bt_i32: return "bt_i32";
	case bt_i64: return "bt_i64";
	case bt_f32: return "bt_f32";
	case bt_f64: return "bt_f64";
	case bt_void: return "bt_void";

	case lit_integer: return "lit_integer";
	case lit_float: return "lit_float";

	case unknown: return "unknown";
	case eof: return "eof";

	default: std::unreachable();
	}
}

struct builtin_type {
	enum class kind : std::int8_t { i8, i16, i32, i64, f32, f64, void_ } kind;
};

struct function_type {
	std::vector<recursive_wrapper<struct type>> params;
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

using literal = std::variant<integer_literal, float_literal>;

struct identifier {
	std::string name;
};

struct function_expr {
	std::vector<identifier> paramaters;
	recursive_wrapper<struct expression> expression;
};

struct function_call_expr {
	identifier callee;
	std::vector<recursive_wrapper<struct expression>> arguments;
};

struct expression : std::variant<literal, identifier, function_expr, function_call_expr> {
	using std::variant<literal, identifier, function_expr, function_call_expr>::variant;
};

struct variable_declaration {
	identifier name;
	type type;
	expression initializer;
};

using declaration = std::variant<variable_declaration>;

struct top_level {
	std::vector<declaration> declarations;
};

template<class... Ts>
struct overloads : Ts... {
	using Ts::operator()...;
};

void print_indent(std::int32_t indent) {
	std::print("{}", std::string(indent, ' '));
}

void print_expression(const expression& expr, std::int32_t indent);

void print_literal(const literal& lit, std::int32_t indent) {
	print_indent(indent);
	std::visit(overloads{
							 [&](const integer_literal& i) { std::print("Integer literal: {}\n", i.value); },
							 [&](const float_literal& f) { std::print("Float literal: {}\n", f.value); },
						 },
	           lit);
}

void print_identifier(const identifier& id, std::int32_t indent) {
	print_indent(indent);
	std::print("Identifier: {}\n", id.name);
}

void print_type(const type& type, std::int32_t indent) {
	print_indent(indent);
	std::visit(overloads{
							 [&](const builtin_type& bt) {
								 std::print("Builtin type: ");
								 switch (bt.kind) {
									 using enum builtin_type::kind;
								 case i8: std::print("i8"); break;
								 case i16: std::print("i16"); break;
								 case i32: std::print("i32"); break;
								 case i64: std::print("i64"); break;
								 case f32: std::print("f32"); break;
								 case f64: std::print("f64"); break;
								 case void_: std::print("void"); break;
								 };
								 std::println("");
							 },
							 [&](const function_type& ft) {
								 std::print("Function type:\n");
								 print_indent(indent + 2);
								 std::print("Params:\n");
								 for (const auto& p : ft.params) print_type(p.get(), indent + 4);
								 print_indent(indent + 2);
								 std::print("Return:\n");
								 print_type(ft.return_type.get(), indent + 4);
							 },
							 [&](const identifier_type& it) { std::print("Identifier type: {}\n", it.name); },
						 },
	           type);
}

void print_expression(const expression& expr, std::int32_t indent) {
	print_indent(indent);
	std::visit(overloads{
							 [&](const literal& lit) {
								 std::print("Literal:\n");
								 print_literal(lit, indent + 2);
							 },
							 [&](const identifier& id) {
								 std::print("Identifier:\n");
								 print_identifier(id, indent + 2);
							 },
							 [&](const function_expr& expr) {
								 std::print("Function expression:\n");
								 for (const auto& param : expr.paramaters) print_identifier(param, indent + 2);
								 print_expression(expr.expression.get(), indent + 2);
							 },
							 [&](const function_call_expr& expr) {
								 std::print("Function call expression:\n");
								 print_identifier(expr.callee, indent + 2);
								 print_indent(indent + 2);
								 std::print("Arguments:\n");
								 for (const auto& arg : expr.arguments) print_expression(arg.get(), indent + 4);
							 },
						 },
	           expr);
}

void print_variable_declaration(const variable_declaration& var, std::int32_t indent = 0) {
	print_indent(indent);
	std::print("Variable declaration:\n");

	print_indent(indent + 2);
	std::print("Name:\n");
	print_identifier(var.name, indent + 4);

	print_indent(indent + 2);
	std::print("Type:\n");
	print_type(var.type, indent + 4);

	print_indent(indent + 2);
	std::print("Initializer:\n");
	print_expression(var.initializer, indent + 4);
}

void print_top_level(const top_level& root) {
	std::print("Top level:\n");

	for (const auto& decl : root.declarations) {
		std::visit(
			overloads{
				[&](const variable_declaration& var) { print_variable_declaration(var, 2); },
			},
			decl);
	}
}

class parser {
public:
	explicit parser(std::span<const lexer::token<token_type>> tokens, std::string_view buffer)
		: tokens_{tokens},
			buffer_{buffer} {}

	auto parse() -> std::expected<top_level, std::string> {
		auto root = top_level{};

		while (!match(token_type::eof)) {
			switch (token().type) {
				using enum token_type;

			case kw_let: {
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
	std::span<const lexer::token<token_type>> tokens_;
	std::size_t curr_ = 0;
	std::string_view buffer_;

	auto next() -> void {
		if (curr_ < tokens_.size()) ++curr_;
	}

	[[nodiscard]]
	auto token() const -> lexer::token<token_type> {
		return tokens_[curr_];
	}

	[[nodiscard]]
	auto peek(std::size_t n = 1) const -> lexer::token<token_type> {
		return curr_ + n < tokens_.size() ? tokens_[curr_ + n] : tokens_[tokens_.size() - 1];
	}

	[[nodiscard]]
	auto match(token_type type) const -> bool {
		return token().type == type;
	}

	[[nodiscard]]
	auto match(std::string_view sv) const -> bool {
		return token().lexeme == sv;
	}

	template<typename... TokenTypes>
		requires(std::same_as<TokenTypes, token_type> && ...)
	[[nodiscard]]
	auto match(TokenTypes... types) const -> bool {
		auto offset = 0uz;
		const auto check = [&](token_type type) -> bool {
			const auto tok = offset == 0 ? token() : peek(offset);
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

		msg += std::format("{}:{}: {}\n", token().start_line, token().start_column, message);

		auto iss = std::istringstream{std::string{buffer_}};
		auto line = std::string{};
		for (auto _ : std::views::iota(1uz, token().start_line + 1)) {
			if (!std::getline(iss, line)) break;
		}
		std::ranges::replace(line, '\t', ' ');

		msg += std::format("{:4} | {}\n", token().start_line, line);
		msg += std::format("     | {}", std::string(token().start_column - 1, ' ') + "^");

		return std::unexpected{msg};
	}

	[[nodiscard]]
	auto expected_error(std::string_view expected) const -> std::unexpected<std::string> {
		return std::unexpected{std::format("expected {} but got '{}'", expected, token().lexeme)};
	}

	auto parse_identifier() -> std::expected<identifier, std::string> {
		if (!match(token_type::identifier)) return expected_error("identifier");
		auto id = identifier{std::string{token().lexeme}};
		next();
		return id;
	}

	auto parse_integer_lit() -> std::expected<integer_literal, std::string> {
		try {
			auto int_lit = integer_literal{std::stoll(std::string{token().lexeme})};
			next();
			return int_lit;
		} catch (const std::exception& e) {
			return std::unexpected{"invalid integer format"};
		}
	}

	auto parse_float_lit() -> std::expected<float_literal, std::string> {
		try {
			auto float_lit = float_literal{std::stod(std::string{token().lexeme})};
			next();
			return float_lit;
		} catch (const std::exception& e) {
			return std::unexpected{"invalid float format"};
		}
	}

	auto parse_literal() -> std::expected<literal, std::string> {
		switch (token().type) {
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
		switch (token().type) {
			using enum token_type;

		case bt_i8: {
			auto type = builtin_type{builtin_type::kind::i8};
			next();
			return type;
		}

		case bt_i16: {
			auto type = builtin_type{builtin_type::kind::i16};
			next();
			return type;
		}

		case bt_i32: {
			auto type = builtin_type{builtin_type::kind::i32};
			next();
			return type;
		}

		case bt_i64: {
			auto type = builtin_type{builtin_type::kind::i64};
			next();
			return type;
		}

		case bt_f32: {
			auto type = builtin_type{builtin_type::kind::f32};
			next();
			return type;
		}

		case bt_f64: {
			auto type = builtin_type{builtin_type::kind::f64};
			next();
			return type;
		}

		case bt_void: {
			auto type = builtin_type{builtin_type::kind::void_};
			next();
			return type;
		}

		case identifier: {
			auto type = identifier_type{std::string{token().lexeme}};
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
				type.params.emplace_back(*param_type);

				if (match_and_next(token_type::comma)) continue;
				if (!match_and_next(token_type::rparen)) return expected_error("')'");
				if (!match_and_next(token_type::arrow)) return expected_error("'->'");

				const auto return_type = parse_type();
				if (!return_type) return std::unexpected{return_type.error()};
				type.return_type = recursive_wrapper{*return_type};

				return type;
			}
		};

		default: return std::unexpected{std::format("expected a type but got '{}'", token().lexeme)};
		}
	}

	auto parse_expression() -> std::expected<expression, std::string> {
		switch (token().type) {
		case token_type::lit_integer:
		case token_type::lit_float: {
			const auto lit = parse_literal();
			if (!lit) return std::unexpected{lit.error()};
			return *lit;
		}

		case token_type::identifier: {
			const auto id = parse_identifier();
			if (!id) return std::unexpected{id.error()};

			if (match_and_next(token_type::lparen)) {
				auto expr = function_call_expr{.callee = *id};

				while (!match_and_next(token_type::rparen) || match(token_type::eof)) {
					const auto new_expr = parse_expression();
					if (!new_expr) return std::unexpected{new_expr.error()};
					expr.arguments.emplace_back(*new_expr);
				}

				return expr;
			}

			return *id;
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
					expr.paramaters.emplace_back(std::string{token().lexeme});
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
		}

		default: return std::unexpected{std::format("expected an expression got '{}'", token().lexeme)};
		}
	}
};

auto lex(std::string_view buffer) -> std::expected<std::vector<lexer::token<token_type>>, std::string> {
	auto lexer = lexer::lexer<token_type>{buffer};

	lexer.define([](const auto& ctx) { return ctx.match(std::isspace) || ctx.match("//"); },
	             [](auto& ctx) -> lexer::token_result<token_type> {
								 while (true) {
									 if (ctx.match(std::isspace)) {
										 while (ctx.match(std::isspace)) ctx.next();
									 } else if (ctx.match("//")) {
										 while (!ctx.match('\n') && !ctx.match(lexer::end_of_file)) ctx.next();
									 } else {
										 break;
									 }
								 }
								 return std::nullopt;
							 });

	lexer.define(lexer::definitions::single_char<token_type::assignment, '='>);
	lexer.define(lexer::definitions::single_char<token_type::semicolon, ';'>);
	lexer.define(lexer::definitions::single_char<token_type::colon, ':'>);
	lexer.define(lexer::definitions::single_char<token_type::lparen, '('>);
	lexer.define(lexer::definitions::single_char<token_type::rparen, ')'>);
	lexer.define(lexer::definitions::single_char<token_type::comma, ','>);
	lexer.define(lexer::definitions::multi_char<token_type::arrow, '-', '>'>);

	lexer.define(lexer::definitions::multi_char<token_type::kw_let, 'l', 'e', 't'>);

	lexer.define(lexer::definitions::multi_char<token_type::bt_i8, 'i', '8'>);
	lexer.define(lexer::definitions::multi_char<token_type::bt_i16, 'i', '1', '6'>);
	lexer.define(lexer::definitions::multi_char<token_type::bt_i32, 'i', '3', '2'>);
	lexer.define(lexer::definitions::multi_char<token_type::bt_i64, 'i', '6', '4'>);
	lexer.define(lexer::definitions::multi_char<token_type::bt_f32, 'f', '3', '2'>);
	lexer.define(lexer::definitions::multi_char<token_type::bt_f64, 'f', '6', '4'>);
	lexer.define(lexer::definitions::multi_char<token_type::bt_void, 'v', 'o', 'i', 'd'>);

	lexer.define(lexer::definitions::identifier<token_type::identifier>);

	lexer.define([](const auto& ctx) { return ctx.match(std::isdigit); },
	             [](auto& ctx) -> lexer::token_result<token_type> {
								 const auto start = ctx.index();
								 auto type = token_type::lit_integer;

								 while (ctx.match(std::isdigit)) ctx.next();

								 if (ctx.match('.')) {
									 ctx.next();
									 if (ctx.match(std::isdigit)) {
										 type = token_type::lit_float;
										 while (ctx.match(std::isdigit)) ctx.next();
									 } else {
										 ctx.prev();
									 }
								 }

								 return lexer::token{type, ctx.substr(start, ctx.index() - start)};
							 });

	lexer.define(lexer::definitions::end_of_file<token_type::eof>);
	lexer.define(lexer::definitions::anything<token_type::unknown>);

	auto tokens = std::vector<lexer::token<token_type>>{};

	while (true) {
		const auto token = lexer.next();
		if (!token) {
			auto msg = std::string{};
			auto err = token.error();

			msg += std::format("{}:{}: {}\n", err.line, err.column, err.message);

			auto iss = std::istringstream{std::string{buffer}};
			auto line = std::string{};
			for (auto _ : std::views::iota(1uz, token->start_line + 1)) {
				if (!std::getline(iss, line)) break;
			}
			std::ranges::replace(line, '\t', ' ');

			msg += std::format("{:4} | {}\n", token->start_line, line);
			msg += std::format("     | {}", std::string(token->start_column - 1, ' ') + "^");

			return std::unexpected{msg};
		}
		std::println("{}:{}:{}: '{}'", token_name(token->type), token->start_line, token->start_column, token->lexeme);
		tokens.emplace_back(*token);
		if (token->type == token_type::eof) break;
	}

	return tokens;
}

auto parse(std::span<const lexer::token<token_type>> tokens, std::string_view buffer)
	-> std::expected<top_level, std::string> {
	return parser{tokens, buffer}.parse();
}

auto main() -> int {
	const auto* buffer = R"(
// this is a comment

let x: i32 = 1;
let y: i32 = 2;

// let add: (i32, i32) -> i32 = (a, b) -> a + b;

// let add = (a, b) -> a + b;

let id: (i32) -> i32 = (x) -> x;

let life: () -> i32 = () -> 42;

let nest: () -> i32 = () -> () -> () -> 42;

let test = id(42);
)";

	const auto tokens = lex(buffer);
	if (!tokens) {
		std::println(std::cerr, "{}", tokens.error());
	}

	const auto ast = parse(*tokens, buffer);

	if (!ast) {
		std::println(std::cerr, "{}", ast.error());
		return 1;
	}

	print_top_level(*ast);
}
