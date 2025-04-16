export module voidlang.utility:observer_ptr;

import std;

export namespace voidlang {

template<typename T>

class observer_ptr {
public:
	constexpr observer_ptr() noexcept : ptr_{nullptr} {}

	constexpr explicit observer_ptr(std::nullptr_t) noexcept : ptr_{nullptr} {}

	constexpr explicit observer_ptr(T* p) noexcept : ptr_{p} {}

	constexpr observer_ptr(const observer_ptr&) noexcept = default;

	constexpr observer_ptr(observer_ptr&&) noexcept = default;

	constexpr ~observer_ptr() noexcept = default;

	constexpr auto operator=(const observer_ptr&) -> observer_ptr& = default;

	constexpr auto operator=(observer_ptr&&) -> observer_ptr& = default;

	constexpr auto release() noexcept -> T* {
		auto p = ptr_;
		ptr_ = nullptr;
		return p;
	}

	constexpr auto reset(T* p = nullptr) noexcept -> void {
		ptr_ = p;
	}

	constexpr auto swap(observer_ptr& other) noexcept -> void {
		auto temp = other.ptr_;
		other.ptr_ = this->ptr_;
		this->ptr_ = temp;
	}

	constexpr auto get() const noexcept -> T* {
		return ptr_;
	}

	constexpr explicit operator bool() const noexcept {
		return ptr_ != nullptr;
	}

	constexpr auto operator*() const noexcept -> T& {
		return *ptr_;
	}

	constexpr auto operator->() const noexcept -> T* {
		return ptr_;
	}

	constexpr explicit operator T*() const noexcept {
		return ptr_;
	}

	friend constexpr auto operator==(const observer_ptr& lhs, const observer_ptr& rhs) noexcept -> bool {
		return lhs.ptr_ == rhs.ptr_;
	}

	friend constexpr auto operator!=(const observer_ptr& lhs, const observer_ptr& rhs) noexcept -> bool {
		return !(lhs == rhs);
	}

private:
	T* ptr_;
};

}  // namespace voidlang
