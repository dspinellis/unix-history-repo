/********************************************
atarist.h
  ++jrb		bammi@cadence.com

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* 
   This is for atariST/StE/TT series with gcc
   Tested with gcc-1.40 + libs @ Patchlevel 73

*/

/*$Log:	atarist.h,v $
 * Revision 1.1  91/10/29  10:48:52  brennan
 * Initial revision
 * 
*/

#ifndef   CONFIG_H
#define   CONFIG_H	1

#define	  HAVE_STDLIB_H		1

#define	  FPE_TRAPS_ON		1
#define   NOINFO_SIGFPE		1

#include <compiler.h>
#define   SIZE_T(X)		((size_t)(X))

#include "config/Idefault.h"
#endif /* CONFIG_H  */
