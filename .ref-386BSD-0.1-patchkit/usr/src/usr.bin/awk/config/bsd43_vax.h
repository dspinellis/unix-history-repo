
/********************************************
bsd43_vax.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	bsd43_vax.h,v $
 * Revision 4.1  91/09/25  11:41:07  brennan
 * VERSION 1.0
 * 
 * Revision 3.2  91/08/13  09:04:09  brennan
 * VERSION .9994
 * 
 * Revision 3.1  91/06/07  10:39:33  brennan
 * VERSION 0.995
 * 
*/

/* BSD UNIX on a vax */

#ifndef   CONFIG_H
#define   CONFIG_H	1

#define   BSD43_VAX

#define   FPE_TRAPS_ON		1
#define   FPE_ZERODIVIDE   FPE_FLTDIV_FAULT
#define   FPE_OVERFLOW     FPE_FLTOVF_FAULT

#define   HAVE_VOID_PTR		0

#define   HAVE_STRTOD		0
#define   HAVE_FMOD		0
#define   HAVE_MATHERR		0

#define   vfprintf(s,f,a)	_doprnt(f,a,s)

#include "config/Idefault.h"


#endif  /* CONFIG_H  */
