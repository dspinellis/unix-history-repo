
/********************************************
mips.h

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	mips.h,v $
 * Revision 1.2  91/11/15  08:13:20  brennan
 * declaration for sprintf
 * 
 * Revision 1.1  91/10/28  09:58:05  brennan
 * Initial revision
 * 
*/

/* tested and works on:

   MIPS M2000 	C 2.20 (4.52)

*/

#ifndef  CONFIG_H
#define  CONFIG_H	1

#define  HAVE_STRTOD    0

/* this is missing and print.c needs it */
char *sprintf() ;

#include "config/Idefault.h"

#endif

