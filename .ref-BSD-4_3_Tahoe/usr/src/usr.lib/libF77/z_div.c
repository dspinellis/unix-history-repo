/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)z_div.c	5.2	11/3/86
 */

#include "complex"
#include <stdio.h>
#include <errno.h>
#ifdef tahoe
#include <tahoemath/FP.h>
#endif tahoe

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
