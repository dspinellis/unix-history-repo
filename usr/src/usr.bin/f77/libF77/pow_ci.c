/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pow_ci.c	5.1	%G%
 */

#include "complex"

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
