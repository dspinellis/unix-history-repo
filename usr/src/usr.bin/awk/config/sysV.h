
/********************************************
sysV.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* 
   This is for sysVR3+
   without IEEE754 floating point

   Even with IEEE754 hardware e.g. a 80x87 or 486
   you might need this, because the math library doesn't
   support the hardware.

   tested on SCO UNIX VR3.2v2.0

*/

/*$Log:	sysV.h,v $
 * Revision 4.1  91/09/25  11:41:40  brennan
 * VERSION 1.0
 * 
 * Revision 1.2  91/08/13  09:04:15  brennan
 * VERSION .9994
 * 
 * Revision 1.1  91/08/03  05:49:52  brennan
 * Initial revision
 * 
*/

#ifndef    CONFIG_H
#define    CONFIG_H		1

#define		FPE_TRAPS_ON		1
#define		NOINFO_SIGFPE		1


#include  "config/Idefault.h"

#endif   /* CONFIG_H */
