/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stddef.h	5.3 (Berkeley) %G%
 */

#ifndef _STDDEF_H_
#define _STDDEF_H_
#include <machine/types.h>

typedef	_PTRDIFF_T_	ptrdiff_t;

#ifdef	_SIZE_T_
typedef	_SIZE_T_	size_t;
#undef	_SIZE_T_
#endif

#ifdef	_WCHAR_T_
typedef	_WCHAR_T_	wchar_t;
#undef	_WCHAR_T_
#endif

#ifndef	NULL
#define	NULL	0
#endif

#define	offsetof(type, member)	((size_t)(&((type *)0)->member))

#endif /* _STDDEF_H_ */
