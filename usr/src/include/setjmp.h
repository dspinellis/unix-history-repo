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

#ifndef _ANSI_SOURCE
/*
 * WARNING: sigsetjmp() isn't supported yet, this is a placeholder.
 */
typedef int sigjmp_buf[_JBLEN + 1];
#endif /* not ANSI */

typedef int jmp_buf[_JBLEN];

#include <sys/cdefs.h>

__BEGIN_DECLS
int	setjmp __P((jmp_buf));
void	longjmp __P((jmp_buf, int));

#ifndef _ANSI_SOURCE
/*
 * WARNING: sigsetjmp() isn't supported yet, this is a placeholder.
 */
int	sigsetjmp __P((sigjmp_buf, int));
void	siglongjmp __P((sigjmp_buf, int));
#endif /* not ANSI */

#if !defined(_ANSI_SOURCE) && !defined(_POSIX_SOURCE)
int	_setjmp __P((jmp_buf));
void	_longjmp __P((jmp_buf, int));
void	longjmperror __P((void));
#endif /* neither ANSI nor POSIX */
__END_DECLS

#endif /* !_SETJMP_H_ */
