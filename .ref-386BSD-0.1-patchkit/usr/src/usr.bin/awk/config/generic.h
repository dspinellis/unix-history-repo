
/********************************************
generic.h
copyright 1991, 1992.  Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	generic.h,v $
 * Revision 4.4  92/03/03  16:40:54  brennan
 * remove HAVE_PRINTF_HD
 * 
 * Revision 4.3  91/10/29  10:48:26  brennan
 * version 1.09
 * 
 * Revision 4.2  91/10/14  09:52:34  brennan
 * added HAVE_PRINTF_HD
 * 
 * Revision 4.1  91/09/25  11:41:23  brennan
 * VERSION 1.0
 * 
 * Revision 3.2  91/08/13  09:04:11  brennan
 * VERSION .9994
 * 
 * Revision 3.1  91/06/07  10:38:48  brennan
 * VERSION 0.995
 * 
*/

/* This is the most common and easiest case ,
   
   if you satisfy the
   following generic conditions 
   or your compiler pre defines __STDC__ != 0
   
   link or copy this file to
   ../config.h and make

generic conditions:
   compiler does not have prototypes
   compiler has void *

   have <string.h>
   do not have  <stdlib.h>
   use <varargs.h> instead of <stdarg.h>
   have <fcntl.h>

   have matherr()
   have strtod()
   have fmod()


   divison by zero, overflow and library domain errors do not
   cause exceptions and this is default behavior.
   For example this is true if you have IEEE754 floating point
   and the traps are off by default as the standard says.
   (The ANSI C committee forgot to read IEEE754 )

   You don't have to be frugal with memory, i.e. you are not
   restricted to 64K of data or something similar
   
   largest integer is 2^31-1 = 0x7fffffff

   Your OS is some flavor of Unix
/*

/* tested and works on:

SunOS4.1  (sun 4.0.3 has a bug in strtod(), use sun_os40.h)

Ultrix4.1  on a MIPS
Ultrix4.2  on a MIPS
SysV3.02beta on a Stardent 3000

*/

#ifndef  CONFIG_H
#define  CONFIG_H	1


#include "config/Idefault.h"

#endif
