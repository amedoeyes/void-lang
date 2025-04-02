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
	add_versions("2.1.0", "2.1.0")
	on_install(function(package)
		import("package.tools.xmake").install(package)
	end)
end)

add_requires("lexer")

target("void", function()
	set_kind("binary")
	add_files("src/**.cpp", "src/**.cppm")
	add_packages("lexer")
end)
