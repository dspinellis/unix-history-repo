/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)c_div.c	5.1	%G%
 */

#include "complex"
#include <stdio.h>
#include <errno.h>

c_div(c, a, b)
complex *a, *b, *c;
{
double ratio, den;
double abr, abi;

if( (abr = b->real) < 0.)
	abr = - abr;
if( (abi = b->imag) < 0.)
	abi = - abi;
if( abr <= abi )
	{
	if(abi == 0) {
		fprintf(stderr,"complex division by zero\n");
		f77_abort(EDOM);
	}
	ratio = b->real / b->imag ;
	den = b->imag * (1 + ratio*ratio);
	c->real = (a->real*ratio + a->imag) / den;
	c->imag = (a->imag*ratio - a->real) / den;
	}

else
	{
	ratio = b->imag / b->real ;
	den = b->real * (1 + ratio*ratio);
	c->real = (a->real + a->imag*ratio) / den;
	c->imag = (a->imag - a->real*ratio) / den;
	}

}
