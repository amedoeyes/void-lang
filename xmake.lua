set_project("void_lang")
set_version("1.0.0")
set_languages("cxx23")

set_toolchains("clang")
set_runtimes("c++_shared")

add_rules("mode.debug", "mode.release", "mode.releasedbg", "mode.check", "mode.profile")
add_rules("plugin.compile_commands.autoupdate", { lsp = "clangd", outputdir = "build" })

set_policy("build.c++.modules", true)

package("lexer", function()
	add_urls("https://github.com/amedoeyes/lexer.git")
	add_versions("2.2.0", "2.2.0")
	on_install(function(package)
		import("package.tools.xmake").install(package)
	end)
end)

package("cli", function()
	add_urls("https://github.com/amedoeyes/cli.git")
	add_versions("1.0.0", "1.0.0")
	on_install(function(package)
		import("package.tools.xmake").install(package)
	end)
end)

add_requires("lexer", "cli")

target("void", function()
	set_kind("binary")
	add_files("src/**.cpp", "src/**.cppm")
	add_packages("lexer", "cli")
	add_cxxflags(
		"-Weverything",
		"-Wno-c++98-compat",
		"-Wno-c++98-compat-pedantic",
		"-Wno-ctad-maybe-unsupported",
		"-Wno-exit-time-destructors",
		"-Wno-float-equal",
		"-Wno-global-constructors",
		"-Wno-missing-designated-field-initializers",
		"-Wno-missing-prototypes",
		"-Wno-missing-variable-declarations", -- dosen't work with modules
		"-Wno-padded",
		"-Wno-reserved-identifier", --doesn't work with modules
		"-Wno-shadow-field-in-constructor",
		"-Wno-switch-default",
		"-Wno-switch-enum",
		"-Wno-unsafe-buffer-usage-in-container",
		"-Wno-weak-vtables"
	)
end)
