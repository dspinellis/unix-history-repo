/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stddef.h	5.7 (Berkeley) %G%
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
typedef	_BSD_WCHAR_T_	wchar_t;
typedef	_BSD_WCHAR_T_	rune_t;
#undef	_BSD_WCHAR_T_
#endif

#ifndef	NULL
#define	NULL	0
#endif

#define	offsetof(type, member)	((size_t)(&((type *)0)->member))

#endif /* _STDDEF_H_ */
