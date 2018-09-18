cmake_minimum_required(VERSION 2.8.12)
project(swipl-yaml)

include("../cmake/PrologPackage.cmake")
find_package(LibYAML)

AC_CHECK_FUNCS(poll)
AC_CHECK_HEADERS(yaml.h)

if(LIBYAML_FOUND)

configure_file(config.h.cmake config.h)

swipl_plugin(
    yaml4pl
    C_SOURCES yaml4pl.c
    C_LIBS ${YAML_LIBRARY}
    PL_LIBS yaml.pl)
target_include_directories(plugin_yaml4pl BEFORE PRIVATE
			   ${LIBYAML_INCLUDE_DIR})

test_libs(
    yaml
    PACKAGES clib plunit)

pkg_doc(yaml
	SECTION
	SOURCE yaml.pl libyaml.tex)

endif(LIBYAML_FOUND)
