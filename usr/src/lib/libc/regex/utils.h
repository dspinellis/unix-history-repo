/*-
 * Copyright (c) 1992 Henry Spencer.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Henry Spencer of the University of Toronto.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)utils.h	5.1 (Berkeley) %G%
 */

/* utility definitions */
#define	DUPMAX		_POSIX2_RE_DUP_MAX	/* xxx is this right? */
#define	INFINITY	(DUPMAX+1)
#define	NUC		(UCHAR_MAX+1)
typedef unsigned char uchar;

#ifndef STATIC
#define	STATIC	static		/* override if cc no like "static int f();" */
#endif
