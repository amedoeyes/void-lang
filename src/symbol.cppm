module;

#include <cassert>

export module voidlang.symbol;

import std;
import voidlang.utility;

export namespace voidlang {

template<typename T>
class symbol_table {
public:
	symbol_table() = default;

	explicit symbol_table(observer_ptr<symbol_table> parent) : parent_{parent} {}

	auto set_parent(observer_ptr<symbol_table> parent) -> void {
		parent_ = parent;
	}

	auto define(std::string_view name, const T& value) noexcept -> void {
		symbols_[std::string{name}] = value;
	}

	auto assign(std::string_view name, const T& value) noexcept -> void {
		if (auto it = symbols_.find(std::string{name}); it != symbols_.end()) {
			it->second = value;
			return;
		}
		if (parent_) parent_->assign(name, value);
	}

	[[nodiscard]]
	auto lookup(std::string_view name) const noexcept -> std::optional<std::reference_wrapper<const T>> {
		if (auto it = symbols_.find(std::string{name}); it != symbols_.end()) {
			return it->second;
		}
		if (parent_) return parent_->lookup(name);
		return std::nullopt;
	}

	[[nodiscard]]
	auto capture() const noexcept -> symbol_table {
		auto st = *this;
		st.parent_.reset();
		return st;
	}

	void print(const std::function<void(const T&)>& printer, std::size_t depth = 0) const {
		auto indent = std::string(depth * 2, ' ');
		std::println("{}Scope {}:", indent, depth);
		for (const auto& [name, value] : symbols_) {
			std::print("  {}{}: ", indent, name);
			printer(value);
		}
		if (parent_) parent_->print(printer, depth + 1);
	}

private:
	observer_ptr<symbol_table> parent_;
	std::unordered_map<std::string, T> symbols_;
};

}  // namespace voidlang
