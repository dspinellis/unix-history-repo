/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)d_cnjg.c	5.4 (Berkeley) 4/12/91";
#endif /* not lint */

#include "complex"
#ifdef tahoe
#include <tahoe/math/FP.h>
#endif

d_cnjg(r, z)
dcomplex *r, *z;
{
r->dreal = z->dreal;
#ifndef tahoe
r->dimag = - z->dimag;
#else tahoe
r->dimag = z->dimag;
if (z->dimag == 0.0)
	return;
else
	*(unsigned long *)&(z->dimag) ^= SIGN_BIT;
#endif tahoe
}
