
/********************************************
dynix.h

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:
*/

/*
 * should work on any Sequent running Final DYNIX
 */

#ifndef  CONFIG_H
#define  CONFIG_H	1

#define	  HAVE_STRTOD		0
#define	  HAVE_FMOD		0
#define   vfprintf(s,f,a)	_doprnt(f,a,s)
#define   FPE_TRAPS_ON		1
#define	  HAVE_MATHERR		0
#include  <sys/types.h>
#include  <machine/fpu.h>
#define   FPE_ZERODIVIDE   FPE_FLTDIV_TRAP
#define   FPE_OVERFLOW     FPE_FLTOVF_TRAP

extern char *strchr();

#include "config/Idefault.h"

#endif

