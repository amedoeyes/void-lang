module;

#include <cassert>

export module voidlang.symbol;

import std;

export namespace voidlang {

template<typename T>
struct scope {
	std::unordered_map<std::string, T> symbols;
	std::optional<std::reference_wrapper<scope>> parent;
};

template<typename T>
class symbol_table {
public:
	auto enter_scope() noexcept -> void {
		scope_ = {.parent = std::ref(scope_)};
	}

	auto exit_scope() noexcept -> void {
		assert(!scope_.parent && "cannot exit the global scope");
		scope_ = scope_.parent.value().get();
	}

	auto insert(std::string_view name, const T& value) noexcept -> void {
		scope_.symbols.emplace(std::string(name), value);
	}

	[[nodiscard]]
	auto lookup(std::string_view name) noexcept -> std::optional<std::reference_wrapper<const T>> {
		auto scope = scope_;
		while (true) {
			if (auto it = scope.symbols.find(std::string{name}); it != scope.symbols.end()) {
				return std::cref(it->second);
			}
			if (!scope.parent) break;
			scope = scope.parent.value().get();
		}
		return std::nullopt;
	}

	[[nodiscard]]
	auto capture() const noexcept -> scope<T> {
		return scope_;
	}

private:
	scope<T> scope_;
};

}  // namespace voidlang
