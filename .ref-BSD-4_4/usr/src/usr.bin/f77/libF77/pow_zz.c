/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)pow_zz.c	5.3 (Berkeley) 4/12/91";
#endif /* not lint */

#include "complex"
#ifdef tahoe
#define cabs zabs
#endif tahoe

pow_zz(r,a,b)
dcomplex *r, *a, *b;
{
double logr, logi, x, y;
double log(), exp(), cos(), sin(), atan2(), cabs();

logr = log( cabs(a->dreal, a->dimag) );
logi = atan2(a->dimag, a->dreal);

x = exp( logr * b->dreal - logi * b->dimag );
y = logr * b->dimag + logi * b->dreal;

r->dreal = x * cos(y);
r->dimag = x * sin(y);
}
