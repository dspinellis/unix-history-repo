/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)z_log.c	5.3 (Berkeley) 4/12/91";
#endif /* not lint */

#include "complex"
#ifdef tahoe
#define cabs zabs
#endif tahoe

z_log(r, z)
dcomplex *r, *z;
{
double log(), cabs(), atan2();

r->dimag = atan2(z->dimag, z->dreal);
r->dreal = log( cabs( z->dreal, z->dimag ) );
}
