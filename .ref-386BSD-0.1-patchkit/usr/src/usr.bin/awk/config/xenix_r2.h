
/********************************************
xenix_r2.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* 
   This has been compiled and tested on
   SCO XENIX R2.3.1 SysV on a i386

   The floating point support is poor.
   Even with a coprocessor there is no access to IEEE754
   functionality.

   There is no way to tell what caused SIGFPE

*/

/*$Log:	xenix_r2.h,v $
 * Revision 4.1  91/09/25  11:42:13  brennan
 * VERSION 1.0
 * 
 * Revision 1.2  91/08/13  09:04:24  brennan
 * VERSION .9994
 * 
 * Revision 1.1  91/06/19  10:17:08  brennan
 * Initial revision
 * 
*/

#ifndef    CONFIG_H
#define    CONFIG_H		1

#define		FPE_TRAPS_ON		1
#define		NOINFO_SIGFPE		1

#if  0
I would guess that on a 286 you would want to
#define        HAVE_SMALL_MEMORY        1
#endif


#include  "config/Idefault.h"

#endif   /* CONFIG_H */
