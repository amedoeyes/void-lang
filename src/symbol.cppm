module;

#include <cassert>

export module voidlang.symbol;

import std;

export namespace voidlang {

template<typename T>
class symbol_table {
public:
	symbol_table() noexcept {
		enter_scope();
	}

	auto enter_scope() noexcept -> void {
		scopes_.push_back({});
	}

	auto exit_scope() noexcept -> void {
		assert(scopes_.size() > 1 && "cannot exit the global scope");
		scopes_.pop_back();
	}

	auto insert(std::string_view name, const T& value) noexcept -> void {
		scopes_.back().emplace(std::string(name), value);
	}

	[[nodiscard]]
	auto lookup(std::string_view name) noexcept -> std::optional<std::reference_wrapper<const T>> {
		for (const auto& scope : std::views::reverse(scopes_)) {
			if (auto found = scope.find(name); found != scope.end()) {
				return std::cref(found->second);
			}
		}
		return std::nullopt;
	}

private:
	std::vector<std::unordered_map<std::string, T>> scopes_;
};

}  // namespace voidlang
