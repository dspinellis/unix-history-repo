
/********************************************
ultrix_vax.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	ultrix_vax.h,v $
 * Revision 4.1  91/09/25  11:42:07  brennan
 * VERSION 1.0
 * 
 * Revision 3.2  91/08/13  09:04:23  brennan
 * VERSION .9994
 * 
 * Revision 3.1  91/06/07  10:38:53  brennan
 * VERSION 0.995
 * 
*/

/* tested on:
	ULTRIX V4.1 (Rev. 200) System #3 on MicroVax 3600
	ULTRIX V3.1  on vax??
*/

#ifndef   CONFIG_H
#define   CONFIG_H	1

/* don't have IEEE hardware */
#define   FPE_TRAPS_ON		1
#define   FPE_ZERODIVIDE   FPE_FLTDIV_FAULT
#define   FPE_OVERFLOW     FPE_FLTOVF_FAULT

/* want SysV style matherr(),
   so set -YSYSTEM_FIVE compiler flag
   in Makefile
*/

#include "config/Idefault.h"


#endif  /* CONFIG_H  */
