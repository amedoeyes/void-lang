import std;
import voidlang;

auto main() -> int {
	const auto* buffer = R"(
// this is a comment

let x: i32 = 1;
let y: i32 = 2;

let add: (i32, i32) -> i32 = (a, b) -> a + b;
let maff = 1 + 2 * 3 / 4 & 2 | 2 && 7 ^ 4 || 1;

let add = (a, b) -> a + b;

let id: (i32) -> i32 = (x) -> x;
let id = (x) -> x;

let life: () -> i32 = () -> 42;
let test = id(42);

let nest: () -> i32 = () -> () -> () -> 42;
let test = nest();
let test = nest()();
let test = nest()()();
)";

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
}
