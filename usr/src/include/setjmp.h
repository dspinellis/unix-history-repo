/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)setjmp.h	5.4 (Berkeley) %G%
 */

#ifndef _SETJMP_H_
#define _SETJMP_H_

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

#include <sys/cdefs.h>

__BEGIN_DECLS
int sigsetjmp __P((sigjmp_buf, int));
void siglongjmp __P((sigjmp_buf, int));
#ifndef _POSIX_SOURCE
int setjmp __P((jmp_buf));
int _setjmp __P((jmp_buf));
void longjmp __P((jmp_buf, int));
void _longjmp __P((jmp_buf, int));
#endif
__END_DECLS

#endif /* !_SETJMP_H_ */
