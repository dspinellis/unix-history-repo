
/********************************************
apollo.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* 
   compiled and tested on apollo SR10.3
*/

#ifndef   CONFIG_H
#define   CONFIG_H	1

/* don't have IEEE hardware */
#define   FPE_TRAPS_ON		1
#define   FPE_ZERODIVIDE   FPE_FLTDIV_FAULT
#define   FPE_OVERFLOW     FPE_FLTOVF_FAULT

/* compiler sets __STDC__ but really is not ansi */
#undef  __STDC__

#include "config/Idefault.h"


#endif  /* CONFIG_H  */

