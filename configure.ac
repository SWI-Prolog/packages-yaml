dnl Process this file with autoconf to produce a configure script.

AC_INIT(install-sh)
AC_PREREQ([2.50])
AC_CONFIG_HEADER(config.h)

AC_SUBST(CIFLAGS)
AC_SUBST(TARGETS)
AC_SUBST(PLTARGETS)

m4_include([../ac_swi_c.m4])

AC_CHECK_FUNCS(poll)
AC_CHECK_HEADERS(yaml.h)
AC_CHECK_LIB(yaml, main)

if test "$ac_cv_lib_yaml_main" = yes; then
  TARGETS="yaml4pl.$SO"
  PLTARGETS="yaml.pl"
fi

AC_OUTPUT(Makefile)
