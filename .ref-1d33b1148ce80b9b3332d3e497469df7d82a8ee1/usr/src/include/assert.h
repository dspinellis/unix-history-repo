/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)assert.h	8.2 (Berkeley) %G%
 */

/*
 * Unlike other ANSI header files, <assert.h> may usefully be included
 * multiple times, with and without NDEBUG defined.
 */

#undef assert
#undef _assert

#ifdef NDEBUG
#define	assert(e)	((void)0)
#define	_assert(e)	((void)0)
#else
#define	_assert(e)	assert(e)
#ifdef __STDC__
#define	assert(e)	((e) ? (void)0 : __assert(__FILE__, __LINE__, #e))
#else	/* PCC */
#define	assert(e)	((e) ? (void)0 : __assert(__FILE__, __LINE__, "e"))
#endif
#endif

#include <sys/cdefs.h>

__BEGIN_DECLS
void __assert __P((const char *, int, const char *));
__END_DECLS
