/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)pow_ci.c	5.3	1/24/88
 */

#include "complex"

#ifdef tahoe

#define	C_MULEQ(A,B)	\
	t = (A).real * (B).real - (A).imag * (B).imag,\
	(A).imag = (A).real * (B).imag + (A).imag * (B).real,\
	(A).real = t	/* A *= B */

void
pow_ci(p, a, b) 	/* p = a**b  */
	complex *p, *a;
	long *b;
{
	register long n = *b;
	register float t;
	complex x;

	x = *a;
	p->real = (float)1, p->imag = (float)0;
	if (!n)
		return;
	if (n < 0) {
		c_div(&x, p, a);
		n = -n;
	}
	while (!(n&1)) {
		C_MULEQ(x, x);
		n >>= 1;
	}
	for (*p = x; --n > 0; C_MULEQ(*p, x))
		while (!(n&1)) {
			C_MULEQ(x, x);
			n >>= 1;
		}
}

#else /* !tahoe */

extern void pow_zi();

void
pow_ci(p, a, b) 	/* p = a**b  */
	complex *p, *a;
	long *b;
{
	dcomplex p1, a1;

	a1.dreal = a->real;
	a1.dimag = a->imag;

	pow_zi(&p1, &a1, b);

	p->real = p1.dreal;
	p->imag = p1.dimag;
}

#endif /* tahoe */
