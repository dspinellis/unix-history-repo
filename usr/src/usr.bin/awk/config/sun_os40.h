
/********************************************
sun_os40.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	sun_os40.h,v $
 * Revision 4.2  91/11/17  12:34:11  brennan
 * added a declaration for sprintf()
 * 
 * Revision 4.1  91/09/25  11:41:35  brennan
 * VERSION 1.0
 * 
 * Revision 3.4  91/08/26  13:04:38  brennan
 * changed file name
 * 
 * Revision 3.3  91/08/23  08:48:42  brennan
 * discovered strtod() bug can also happen on sun3
 * 
 * Revision 3.2  91/08/13  09:04:13  brennan
 * VERSION .9994
 * 
 * Revision 3.1  91/06/07  10:38:50  brennan
 * VERSION 0.995
 * 
*/

/* On some, not all, sun4's SunOS4.0.3 there is a bug in strtod().
   strtod("0",0) sets errno to ERANGE sometimes! which announces
   underflow which is not true.

   If you get strange error messages:

   mawk: line xx: 0 : decimal underflow

   then your strtod() is broken and use this file as config.h

   Otherwise use generic.h as config.h.

   I've talked to Sun and they tell me its a known bug thats
   fixed in 4.1

   ----------------
   It can happen on sun3's too. (8/23)
*/

#ifndef  CONFIG_H
#define  CONFIG_H


#define  STRTOD_UNDERFLOW_ON_ZERO_BUG  1

extern char *sprintf() ;  /* missing in /usr/include/*.h */


#include "config/Idefault.h"

#endif
