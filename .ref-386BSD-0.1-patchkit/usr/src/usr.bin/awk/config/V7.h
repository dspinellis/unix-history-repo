
/********************************************
V7.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*
The port of mawk to V7 is the work of
Carl Mascott (cmascott@world.std.com)
*/

/*$Log:	V7.h,v $
 * Revision 4.2  91/11/21  13:30:34  brennan
 *    
 * 
 * 11/17/91  C. Mascott		declare fprintf, sprintf on V7
 *
 * Revision 4.1  91/09/25  11:40:41  brennan
 * VERSION 1.0
 * 
 * Revision 1.4  91/08/16  08:22:09  brennan
 * Carl's addition of SW_FP_CHECK for XNX23A
 * 
 * Revision 1.3  91/08/13  09:04:07  brennan
 * VERSION .9994
 * 
 * Revision 1.2  91/06/15  09:28:54  brennan
 * Carl's diffs for V7
 * 
 * 06/11/91  C. Mascott		change NO_FMOD to HAVE_FMOD
 *				change NO_STRTOD to HAVE_STRTOD
 *
 * Revision 1.1  91/06/10  14:20:03  brennan
 * Initial revision
 * 
*/

#ifndef   CONFIG_H
#define   CONFIG_H		1

#define				V7


#define   HAVE_VOID_PTR		0
#define   HAVE_STRTOD           0
#define   HAVE_FMOD             0
#define   HAVE_MATHERR		0

#define   HAVE_STRING_H		0
#define   HAVE_FCNTL_H		0


#define   O_RDONLY		0
#define   O_WRONLY		1
#define   O_RDWR		2

#define   vfprintf(s,f,a)  _doprnt(f,a,s)
#define   strchr	index
#define   strrchr	rindex

#ifdef XNX23A
/* convert double to Boolean.  This is a bug work-around for
   XENIX-68K 2.3A, where logical test of double doesn't work.  This
   macro NG for register double. */
#define   D2BOOL(x)	(*((long *) &(x)))
#define   SW_FP_CHECK	1
#endif


/* these are missing and print.c needs them */
void fprintf() ;
char *sprintf() ;

#include  "config/Idefault.h"
#endif  /* CONFIG_H */
