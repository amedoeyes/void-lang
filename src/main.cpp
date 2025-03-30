import std;
import lexer;

enum class token_type : std::uint8_t {
	assignment,
	semicolon,
	colon,

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
	lit_string,

	unknown,
	eof,
};

auto token_name(token_type token) -> std::string {
	switch (token) {
		using enum token_type;

	case assignment: return "assignment";
	case semicolon: return "semicolon";
	case colon: return "colon";

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
	case lit_string: return "lit_string";

	case unknown: return "unknown";
	case eof: return "eof";

	default: std::unreachable();
	}
}

struct builtin_type {
	enum class kind : std::int8_t { i8, i16, i32, i64, f32, f64, void_ } kind;
};

struct identifier_type {
	std::string name;
};

using type = std::variant<builtin_type, identifier_type>;

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

using expression = std::variant<literal, identifier>;

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
				auto decl = parse_declaration();
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
	auto match(token_type type) const -> bool {
		return token().type == type;
	}

	[[nodiscard]]
	auto match(std::string_view sv) const -> bool {
		return token().lexeme == sv;
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

	auto parse_declaration() -> std::expected<declaration, std::string> {
		next();
		if (!match(token_type::identifier)) return std::unexpected{"expected identifier after let"};
		const auto name = std::string{token().lexeme};

		next();
		if (!match(token_type::colon)) return std::unexpected{"expected colon after identifier"};

		next();
		const auto type = parse_type();
		if (!type) return std::unexpected{type.error()};

		if (!match(token_type::assignment)) return std::unexpected{std::format("expected '=' got '{}'", token().lexeme)};
		next();

		const auto expr = parse_expression();
		if (!expr) return std::unexpected{expr.error()};

		if (!match(token_type::semicolon)) return std::unexpected{std::format("expected ';' got '{}'", token().lexeme)};
		next();

		return variable_declaration{
			.name{name},
			.type{*type},
			.initializer{*expr},
		};
	}

	auto parse_type() -> std::expected<type, std::string> {
		auto t = type{};

		switch (token().type) {
			using enum token_type;
		case bt_i8: t = builtin_type{builtin_type::kind::i8}; break;
		case bt_i16: t = builtin_type{builtin_type::kind::i16}; break;
		case bt_i32: t = builtin_type{builtin_type::kind::i32}; break;
		case bt_i64: t = builtin_type{builtin_type::kind::i64}; break;
		case bt_f32: t = builtin_type{builtin_type::kind::f32}; break;
		case bt_f64: t = builtin_type{builtin_type::kind::f64}; break;
		case bt_void: t = builtin_type{builtin_type::kind::void_}; break;
		case identifier: t = identifier_type{std::string{token().lexeme}}; break;
		default: return std::unexpected{std::format("expected a type got '{}'", token().lexeme)};
		}

		next();
		return t;
	}

	auto parse_expression() -> std::expected<expression, std::string> {
		auto expr = expression{};

		switch (token().type) {
		case token_type::lit_integer: expr = integer_literal{std::stoll(std::string{token().lexeme})}; break;
		case token_type::lit_float: expr = float_literal{std::stod(std::string{token().lexeme})}; break;
		case token_type::identifier: expr = identifier{std::string{token().lexeme}}; break;
		default: return std::unexpected{std::format("expected an expression got '{}'", token().lexeme)};
		}

		next();
		return expr;
	}
};

auto lex(std::string_view buffer) -> std::expected<std::vector<lexer::token<token_type>>, std::string> {
	auto lexer = lexer::lexer<token_type>{buffer};

	lexer.define(lexer::definitions::skip_whitespace<token_type>);
	lexer.define([](const auto& ctx) { return ctx.match("//"); },
	             [](auto& ctx) -> lexer::token_result<token_type> {
								 while (!ctx.match('\n') && !ctx.match(lexer::end_of_file)) ctx.next();
								 ctx.next();
								 return std::nullopt;
							 });

	lexer.define(lexer::definitions::single_char<token_type::assignment, '='>);
	lexer.define(lexer::definitions::single_char<token_type::semicolon, ';'>);
	lexer.define(lexer::definitions::single_char<token_type::colon, ':'>);

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
		// std::println("{}:{}:{}: '{}'", token_name(token->type), token->start_line, token->start_column, token->lexeme);
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
// let add: (i32, i32) -> i32 = (a, b) => a + b;
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
