/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)assert.h	5.1 (Berkeley) %G%
 */

/*
 * Unlike other ANSI header files, <assert.h> may usefully be included
 * multiple times, with and without NDEBUG defined.
 */

#undef assert

#ifdef NDEBUG
#define	assert(e)	((void)0)
#else
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
