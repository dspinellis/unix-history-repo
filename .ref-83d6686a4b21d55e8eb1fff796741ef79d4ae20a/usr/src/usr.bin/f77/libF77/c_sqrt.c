/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)c_sqrt.c	5.2	%G%
 */

#include "complex"
#ifdef tahoe
#include <tahoemath/FP.h>
#endif tahoe

c_sqrt(r, z)
complex *r, *z;
{
double mag, sqrt(), cabs();

if( (mag = cabs(z->real, z->imag)) == 0.)
	r->real = r->imag = 0.;
else if(z->real > 0)
	{
	r->real = sqrt(0.5 * (mag + z->real) );
	r->imag = z->imag / r->real / 2;
	}
else
	{
	r->imag = sqrt(0.5 * (mag - z->real) );
	if(z->imag < 0)
#ifndef tahoe
		r->imag = - r->imag;
#else tahoe
		*(unsigned long*)&(r->imag) ^= SIGN_BIT;
#endif tahoe
	r->real = z->imag / r->imag /2;
	}
}
