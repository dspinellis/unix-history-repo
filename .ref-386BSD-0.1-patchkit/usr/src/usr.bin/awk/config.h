
/********************************************
bsd43_vax.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log: config.h,v $
 * Revision 1.2  1992/06/02  05:07:35  rich
 * Ported to 386bsd.  Changes from vax BSD4.3 include usage of
 * fmod in libm.a, usage of void pointers, and usage of vfprintf
 * in libc.a.  Floating point exceptions are not raised when
 * they should be, which causes the last fpe test to fail.
 *
 * Revision 1.1  1992/06/02  03:45:42  rich
 * Initial revision
 *
 * Revision 4.1  91/09/25  11:41:07  brennan
 * VERSION 1.0
 * 
 * Revision 3.2  91/08/13  09:04:09  brennan
 * VERSION .9994
 * 
 * Revision 3.1  91/06/07  10:39:33  brennan
 * VERSION 0.995
 * 
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         1       00089
 * --------------------         -----   ----------------------
 *
 * 01 Mar 93    Chris Demetriou		Adjust to life with strtod in libc
 *
 */

/* BSD UNIX on 386BSD */

#ifndef   CONFIG_H
#define   CONFIG_H	1

#define   BSD43_VAX

#define   FPE_TRAPS_ON		1
#define   FPE_ZERODIVIDE   FPE_FLTDIV_FAULT
#define   FPE_OVERFLOW     FPE_FLTOVF_FAULT

#define   HAVE_STRTOD		1
#define   HAVE_MATHERR		0

#define   HAVE_FMOD		1
#define   HAVE_VOID_PTR		1

#include "config/Idefault.h"


#endif  /* CONFIG_H  */
