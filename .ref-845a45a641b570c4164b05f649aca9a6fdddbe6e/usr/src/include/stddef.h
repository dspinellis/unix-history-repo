/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stddef.h	8.1 (Berkeley) %G%
 */

#ifndef _STDDEF_H_
#define _STDDEF_H_

#include <machine/ansi.h>

typedef	_BSD_PTRDIFF_T_	ptrdiff_t;

#ifdef	_BSD_SIZE_T_
typedef	_BSD_SIZE_T_	size_t;
#undef	_BSD_SIZE_T_
#endif

#ifdef	_BSD_WCHAR_T_
#ifndef _ANSI_SOURCE
typedef	_BSD_WCHAR_T_	rune_t;
#endif
typedef	_BSD_WCHAR_T_	wchar_t;
#undef	_BSD_WCHAR_T_
#endif

#ifndef	NULL
#define	NULL	0
#endif

#define	offsetof(type, member)	((size_t)(&((type *)0)->member))

#endif /* _STDDEF_H_ */
