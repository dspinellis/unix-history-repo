/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)z_sqrt.c	5.2	%G%
 */

#include "complex"
#ifdef tahoe
#include <tahoemath/FP.h>
#define cabs zabs
#endif tahoe

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
