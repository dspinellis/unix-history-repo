/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)z_sqrt.c	5.4 (Berkeley) 4/12/91";
#endif /* not lint */

#include "complex"
#ifdef tahoe
#include <tahoe/math/FP.h>
#define cabs zabs
#endif

z_sqrt(r, z)
dcomplex *r, *z;
{
double mag, sqrt(), cabs();

if( (mag = cabs(z->dreal, z->dimag)) == 0.)
	r->dreal = r->dimag = 0.;
else if(z->dreal > 0)
	{
	r->dreal = sqrt(0.5 * (mag + z->dreal) );
	r->dimag = z->dimag / r->dreal / 2;
	}
else
	{
	r->dimag = sqrt(0.5 * (mag - z->dreal) );
	if(z->dimag < 0)
#ifndef tahoe
		r->dimag = - r->dimag;
#else tahoe
		*((long int *)&r->dimag) ^= SIGN_BIT;
#endif tahoe
	r->dreal = z->dimag / r->dimag / 2;
	}
}
