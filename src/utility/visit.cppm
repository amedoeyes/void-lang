export module voidlang.utility:visit;

import std;

export namespace voidlang {

template<class... Ts>
struct visitor : Ts... {
	using Ts::operator()...;
};

template<typename Variant, typename... Funcs>
	requires requires(Variant&& variant, Funcs&&... funcs) {
						 std::visit(visitor{std::forward<Funcs>(funcs)...}, std::forward<Variant>(variant));
					 }
auto visit(Variant&& variant, Funcs&&... funcs) {
	return std::visit(visitor{std::forward<Funcs>(funcs)...}, std::forward<Variant>(variant));
}

}  // namespace voidlang
