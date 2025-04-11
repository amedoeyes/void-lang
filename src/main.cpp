import cli;
import std;
import voidlang;

auto main(int argc, char** argv) -> int {
	auto root_command = cli::command{"void"};
	root_command.add_option("help", {.description = "display this help and exit", .name = "help", .short_name = 'h'});
	root_command.set_action([](const cli::command& cmd) -> std::optional<std::int32_t> {
		if (cmd.got_option("help") || !cmd.got_command()) {
			cmd.print_help();
			return 0;
		}
		return std::nullopt;
	});

	auto& ast_command = root_command.add_command("ast");
	ast_command.set_usage("file");
	ast_command.set_description("dump AST");
	ast_command.add_option("help", {.description = "display this help and exit", .name = "help", .short_name = 'h'});
	ast_command.set_action([](const cli::command& cmd) -> std::optional<std::int32_t> {
		if (cmd.got_option("help")) {
			cmd.print_help();
			return 0;
		}
		const auto args = cmd.arguments();
		if (args.empty()) {
			std::println("void: ast command expects a file");
			return 1;
		}
		if (args.size() > 1) {
			std::println(std::cerr, "void: ast command expects only one file");
			return 1;
		}
		const auto arg = args[0];

		auto file = std::ifstream{std::string{arg}};
		if (!file) {
			std::println(std::cerr, "void: {}: No such file or directory", arg);
			return 1;
		}
		const auto buffer = (std::stringstream{} << file.rdbuf()).str();

		const auto tokens = voidlang::lex(buffer);
		if (!tokens) {
			std::println(std::cerr, "{}", tokens.error());
		}

		const auto ast = voidlang::parse(*tokens, buffer);
		if (!ast) {
			std::println(std::cerr, "{}", ast.error());
			return 1;
		}

		voidlang::print_ast(*ast);

		return 0;
	});

	auto& eval_command = root_command.add_command("eval");
	eval_command.set_usage("file");
	eval_command.set_description("eval");
	eval_command.add_option("help", {.description = "display this help and exit", .name = "help", .short_name = 'h'});
	eval_command.set_action([](const cli::command& cmd) -> std::optional<std::int32_t> {
		if (cmd.got_option("help")) {
			cmd.print_help();
			return 0;
		}
		const auto args = cmd.arguments();
		if (args.empty()) {
			std::println("void: eval command expects a file");
			return 1;
		}
		if (args.size() > 1) {
			std::println(std::cerr, "void: eval command expects only one file");
			return 1;
		}
		const auto arg = args[0];

		auto file = std::ifstream{std::string{arg}};
		if (!file) {
			std::println(std::cerr, "void: {}: No such file or directory", arg);
			return 1;
		}
		const auto buffer = (std::stringstream{} << file.rdbuf()).str();

		const auto tokens = voidlang::lex(buffer);
		if (!tokens) {
			std::println(std::cerr, "{}", tokens.error());
		}

		const auto ast = voidlang::parse(*tokens, buffer);
		if (!ast) {
			std::println(std::cerr, "{}", ast.error());
			return 1;
		}

		voidlang::eval(*ast);

		return 0;
	});

	const auto result = root_command.execute(argc, argv);
	if (!result) {
		std::println(std::cerr, "void: {}", result.error());
		return 1;
	}
	return *result;
}
