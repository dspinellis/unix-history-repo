/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)setjmp.h	4.3 (Berkeley) %G%
 */

#ifndef _SETJMP_
#define _SETJMP_

#ifdef vax
#define _JBLEN	10
#endif

#ifdef tahoe
#define _JBLEN	10
#endif

#ifdef hp300
#define _JBLEN	17
#endif

typedef int jmp_buf[_JBLEN];

#ifdef __STDC__
extern int setjmp(jmp_buf), _setjmp(jmp_buf);
extern void longjmp(jmp_buf, int), _longjmp(jmp_buf, int);
#else
extern int setjmp(), _setjmp();
extern int longjmp(), _longjmp();
#endif
#endif
