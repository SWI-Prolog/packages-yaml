cmake_minimum_required(VERSION 2.8.12)
project(swipl-yaml)

include("../cmake/PrologPackage.cmake")

AC_CHECK_FUNCS(poll)
AC_CHECK_HEADERS(yaml.h)

check_library_exists(yaml yaml_parser_parse "" HAVE_LIBYAML)

if(HAVE_LIBYAML AND HAVE_YAML_H)

configure_file(config.h.cmake config.h)

swipl_plugin(
    yaml4pl
    C_SOURCES yaml4pl.c
    C_LIBS -lyaml
    PL_LIBS yaml.pl)

test_libs(
    yaml
    PACKAGES clib plunit)

pkg_doc(yaml
	SECTION
	SOURCE yaml.pl libyaml.tex)

endif(HAVE_LIBYAML AND HAVE_YAML_H)
