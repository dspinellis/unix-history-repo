/*-
 * Copyright (c) 1980, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)mp.h	8.1 (Berkeley) %G%
 */

#ifndef _MP_H_
#define	_MP_H_

#define MINT struct mint
MINT
{	int len;
	short *val;
};
#define FREE(x) {if(x.len!=0) {free((char *)x.val); x.len=0;}}
#ifndef DBG
#define shfree(u) free((char *)u)
#else
#include <stdio.h>
#define shfree(u) { if(dbg) fprintf(stderr, "free %o\n", u); free((char *)u);}
extern int dbg;
#endif
#if !defined(vax) && !defined(i386)
struct half
{	short high;
	short low;
};
#else
struct half
{	short low;
	short high;
};
#endif
extern MINT *itom();
extern short *xalloc();

#ifdef lint
extern xv_oid;
#define VOID xv_oid =
#else
#define VOID
#endif

#endif /* !_MP_H_ */
