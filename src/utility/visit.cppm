export module voidlang.utility:visit;

import std;

template<class... Ts>
struct overloads : Ts... {
	using Ts::operator()...;
};

export namespace voidlang {

template<typename Variant, typename... Funcs>
	requires requires(Variant&& variant, Funcs&&... funcs) {
						 std::visit(overloads{std::forward<Funcs>(funcs)...}, std::forward<Variant>(variant));
					 }
auto visit(Variant&& variant, Funcs&&... funcs) {
	return std::visit(overloads{std::forward<Funcs>(funcs)...}, std::forward<Variant>(variant));
}

}  // namespace voidlang
