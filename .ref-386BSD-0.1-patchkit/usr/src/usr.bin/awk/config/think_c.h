
/********************************************
Think_C.h

copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* Think C 4.0 for the Macintosh */

#ifndef   CONFIG_H
#define   CONFIG_H      1

#define   HAVE_PROTOS           1
#define   HAVE_STDARG_H         1
#define   HAVE_STDLIB_H		1

#define   HAVE_MATHERR		0
#define   FPE_TRAPS_ON		1
#define   NOINFO_SIGFPE		1

#define   SIZE_T(x)		(size_t)(x)

#include ":config:Idefault.h"

#endif  /* CONFIG_H  */
