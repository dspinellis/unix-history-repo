
/********************************************
msc_dos.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* Microsoft C 6.0A under MSDOS */

/*$Log:	msc_dos.h,v $
 * Revision 4.3  92/01/09  08:54:09  brennan
 * changed SAMESEG macro
 * 
 * Revision 4.2  91/10/29  09:36:59  brennan
 * Large model macro
 * 
*/

#ifndef   CONFIG_H
#define   CONFIG_H      1

#define   MSDOS_MSC		1
#define   MSDOS                 1

#define   HAVE_PROTOS           1
#define   HAVE_STDARG_H         1
#define   HAVE_STDLIB_H		1

#define   FPE_TRAPS_ON		1
#define   NOINFO_SIGFPE		1

#ifndef   HAVE_SMALL_MEMORY   /* allow large model override */
#define   HAVE_SMALL_MEMORY     1
#endif

#if  HAVE_SMALL_MEMORY==0
/* how to test far pointers have the same segment */
#define SAMESEG(p,q) \
  (((unsigned long)(p)^(unsigned long)(q))<0x10000L)
#endif

#include "config/Idefault.h"


#endif  /* CONFIG_H  */
