export module voidlang.utility:recursive_wrapper;

import std;

export namespace voidlang {

template<typename T>
struct recursive_wrapper {
	recursive_wrapper() noexcept = default;

	explicit recursive_wrapper(const T& value) noexcept : ptr{std::make_unique<T>(value)} {}

	explicit recursive_wrapper(T&& value) noexcept : ptr{std::make_unique<T>(std::move(value))} {}

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

}  // namespace voidlang
