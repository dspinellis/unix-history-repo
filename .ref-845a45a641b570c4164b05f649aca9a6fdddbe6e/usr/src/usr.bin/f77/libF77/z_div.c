/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)z_div.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include "complex"
#include <stdio.h>
#include <errno.h>
#ifdef tahoe
#include <tahoe/math/FP.h>
#endif

z_div(c, a, b)
dcomplex *a, *b, *c;
{
double ratio, den;
double abr, abi;

#ifndef tahoe
if( (abr = b->dreal) < 0.)
	abr = - abr;
if( (abi = b->dimag) < 0.)
	abi = - abi;
#else tahoe
if( (abr = b->dreal) < 0.)
	*((long int *)&abr) ^= SIGN_BIT;
if( (abi = b->dimag) < 0.)
	*((long int *)&abi) ^= SIGN_BIT;
#endif tahoe
if( abr <= abi )
	{
	if(abi == 0) {
		fprintf( stderr, "Double complex division by zero\n" );
		f77_abort(EDOM);
	}
	ratio = b->dreal / b->dimag ;
	den = b->dimag * (1 + ratio*ratio);
	c->dreal = (a->dreal*ratio + a->dimag) / den;
	c->dimag = (a->dimag*ratio - a->dreal) / den;
	}

else
	{
	ratio = b->dimag / b->dreal ;
	den = b->dreal * (1 + ratio*ratio);
	c->dreal = (a->dreal + a->dimag*ratio) / den;
	c->dimag = (a->dimag - a->dreal*ratio) / den;
	}

}
