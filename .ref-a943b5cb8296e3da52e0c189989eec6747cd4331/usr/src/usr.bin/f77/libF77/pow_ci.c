/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pow_ci.c	5.2	%G%
 */

#include "complex"

#ifndef tahoe
pow_ci(p, a, b) 	/* p = a**b  */
complex *p, *a;
long int *b;
{
dcomplex p1, a1;

a1.dreal = a->real;
a1.dimag = a->imag;

pow_zi(&p1, &a1, b);

p->real = p1.dreal;
p->imag = p1.dimag;
}

#else tahoe

pow_ci(p, a, b) 	/* p = a**b  */
complex *p, *a;
long int *b;
{
register long int n;
register float t;
complex x;

n = *b;
p->real = 1;
p->imag = 0;

if(n == 0)
	return;
if(n < 0)
	{
	n = -n;
	c_div(&x,p,a);
	}
else
	{
	x.real = a->real;
	x.imag = a->imag;
	}

for( ; ; )
	{
	if(n & 01)
		{
		t = p->real * x.real - p->imag * x.imag;
		p->imag = p->real * x.imag + p->imag * x.real;
		p->real = t;
		}
	if(n >>= 1)
		{
		t = x.real * x.real - x.imag * x.imag;
		x.imag = 2 * x.real * x.imag;
		x.real = t;
		}
	else
		break;
	}
}
#endif tahoe
