/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)setjmp.h	5.2 (Berkeley) %G%
 */

#ifndef _SETJMP_
#define _SETJMP_

#ifdef hp300
#define _JBLEN	17
#endif

#ifdef i386
#define _JBLEN	10
#endif

#ifdef tahoe
#define _JBLEN	10
#endif

#ifdef vax
#define _JBLEN	10
#endif

/*
 * sigsetjmp/siglongjmp use the first int to decide if the
 * signal mask was saved or not.
 */
typedef int sigjmp_buf[_JBLEN + 1];

#ifndef _POSIX_SOURCE
typedef int jmp_buf[_JBLEN];
#endif

#if __STDC__ || c_plusplus
int sigsetjmp(sigjmp_buf, int);
void siglongjmp(sigjmp_buf, int);
#ifndef _POSIX_SOURCE
extern int setjmp(jmp_buf);
extern int _setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);
extern void _longjmp(jmp_buf, int);
#endif
#else
int sigsetjmp();
void siglongjmp();
#ifndef _POSIX_SOURCE
extern int setjmp();
extern int _setjmp();
extern void longjmp();
extern void _longjmp();
#endif
#endif
#endif
