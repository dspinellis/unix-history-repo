/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Extended precision complex arithmetic primitive routines
 */

#include "cmath.h"


COMPLEX _czero_ =		{ &_qzero_, &_qzero_, 1 };
COMPLEX _cone_ =		{ &_qone_, &_qzero_, 1 };
COMPLEX _conei_ =		{ &_qzero_, &_qone_, 1 };

static COMPLEX _cnegone_ =	{ &_qnegone_, &_qzero_, 1 };


/*
 * Free list for complex numbers.
 */
static FREELIST freelist = {
	sizeof(COMPLEX),	/* size of an item */
	100			/* number of free items to keep */
};


/*
 * Add two complex numbers.
 */
COMPLEX *
cadd(c1, c2)
	COMPLEX *c1, *c2;
{
	COMPLEX *r;

	if (ciszero(c1))
		return clink(c2);
	if (ciszero(c2))
		return clink(c1);
	r = comalloc();
	if (!qiszero(c1->real) || !qiszero(c2->real))
		r->real = qadd(c1->real, c2->real);
	if (!qiszero(c1->imag) || !qiszero(c2->imag))
		r->imag = qadd(c1->imag, c2->imag);
	return r;
}


/*
 * Subtract two complex numbers.
 */
COMPLEX *
csub(c1, c2)
	COMPLEX *c1, *c2;
{
	COMPLEX *r;

	if ((c1->real == c2->real) && (c1->imag == c2->imag))
		return clink(&_czero_);
	if (ciszero(c2))
		return clink(c1);
	r = comalloc();
	if (!qiszero(c1->real) || !qiszero(c2->real))
		r->real = qsub(c1->real, c2->real);
	if (!qiszero(c1->imag) || !qiszero(c2->imag))
		r->imag = qsub(c1->imag, c2->imag);
	return r;
}


/*
 * Multiply two complex numbers.
 * This saves one multiplication over the obvious algorithm by
 * trading it for several extra additions, as follows.  Let
 *	q1 = (a + b) * (c + d)
 *	q2 = a * c
 *	q3 = b * d
 * Then (a+bi) * (c+di) = (q2 - q3) + (q1 - q2 - q3)i.
 */
COMPLEX *
cmul(c1, c2)
	COMPLEX *c1, *c2;
{
	COMPLEX *r;
	NUMBER *q1, *q2, *q3, *q4;

	if (ciszero(c1) || ciszero(c2))
		return clink(&_czero_);
	if (cisone(c1))
		return clink(c2);
	if (cisone(c2))
		return clink(c1);
	if (cisreal(c2))
		return cmulq(c1, c2->real);
	if (cisreal(c1))
		return cmulq(c2, c1->real);
	/*
	 * Need to do the full calculation.
	 */
	r = comalloc();
	q2 = qadd(c1->real, c1->imag);
	q3 = qadd(c2->real, c2->imag);
	q1 = qmul(q2, q3);
	qfree(q2);
	qfree(q3);
	q2 = qmul(c1->real, c2->real);
	q3 = qmul(c1->imag, c2->imag);
	q4 = qadd(q2, q3);
	r->real = qsub(q2, q3);
	r->imag = qsub(q1, q4);
	qfree(q1);
	qfree(q2);
	qfree(q3);
	qfree(q4);
	return r;
}


/*
 * Square a complex number.
 */
COMPLEX *
csquare(c)
	COMPLEX *c;
{
	COMPLEX *r;
	NUMBER *q1, *q2;

	if (ciszero(c))
		return clink(&_czero_);
	if (cisrunit(c))
		return clink(&_cone_);
	if (cisiunit(c))
		return clink(&_cnegone_);
	r = comalloc();
	if (cisreal(c)) {
		r->real = qsquare(c->real);
		return r;
	}
	if (cisimag(c)) {
		q1 = qsquare(c->imag);
		r->real = qneg(q1);
		qfree(q1);
		return r;
	}
	q1 = qsquare(c->real);
	q2 = qsquare(c->imag);
	r->real = qsub(q1, q2);
	qfree(q1);
	qfree(q2);
	q1 = qmul(c->real, c->imag);
	r->imag = qscale(q1, 1L);
	qfree(q1);
	return r;
}


/*
 * Divide two complex numbers.
 */
COMPLEX *
cdiv(c1, c2)
	COMPLEX *c1, *c2;
{
	COMPLEX *r;
	NUMBER *q1, *q2, *q3, *den;

	if (ciszero(c2))
		math_error("Division by zero");
	if ((c1->real == c2->real) && (c1->imag == c2->imag))
		return clink(&_cone_);
	r = comalloc();
	if (cisreal(c1) && cisreal(c2)) {
		r->real = qdiv(c1->real, c2->real);
		return r;
	}
	if (cisimag(c1) && cisimag(c2)) {
		r->real = qdiv(c1->imag, c2->imag);
		return r;
	}
	if (cisimag(c1) && cisreal(c2)) {
		r->imag = qdiv(c1->imag, c2->real);
		return r;
	}
	if (cisreal(c1) && cisimag(c2)) {
		q1 = qdiv(c1->real, c2->imag);
		r->imag = qneg(q1);
		qfree(q1);
		return r;
	}
	if (cisreal(c2)) {
		r->real = qdiv(c1->real, c2->real);
		r->imag = qdiv(c1->imag, c2->real);
		return r;
	}
	q1 = qsquare(c2->real);
	q2 = qsquare(c2->imag);
	den = qadd(q1, q2);
	qfree(q1);
	qfree(q2);
	q1 = qmul(c1->real, c2->real);
	q2 = qmul(c1->imag, c2->imag);
	q3 = qadd(q1, q2);
	qfree(q1);
	qfree(q2);
	r->real = qdiv(q3, den);
	qfree(q3);
	q1 = qmul(c1->real, c2->imag);
	q2 = qmul(c1->imag, c2->real);
	q3 = qsub(q2, q1);
	qfree(q1);
	qfree(q2);
	r->imag = qdiv(q3, den);
	qfree(q3);
	qfree(den);
	return r;
}


/*
 * Invert a complex number.
 */
COMPLEX *
cinv(c)
	COMPLEX *c;
{
	COMPLEX *r;
	NUMBER *q1, *q2, *den;

	if (ciszero(c))
		math_error("Inverting zero");
	r = comalloc();
	if (cisreal(c)) {
		r->real = qinv(c->real);
		return r;
	}
	if (cisimag(c)) {
		q1 = qinv(c->imag);
		r->imag = qneg(q1);
		qfree(q1);
		return r;
	}
	q1 = qsquare(c->real);
	q2 = qsquare(c->imag);
	den = qadd(q1, q2);
	qfree(q1);
	qfree(q2);
	r->real = qdiv(c->real, den);
	q1 = qdiv(c->imag, den);
	r->imag = qneg(q1);
	qfree(q1);
	qfree(den);
	return r;
}


/*
 * Negate a complex number.
 */
COMPLEX *
cneg(c)
	COMPLEX *c;
{
	COMPLEX *r;

	if (ciszero(c))
		return clink(&_czero_);
	r = comalloc();
	if (!qiszero(c->real))
		r->real = qneg(c->real);
	if (!qiszero(c->imag))
		r->imag = qneg(c->imag);
	return r;
}


/*
 * Take the integer part of a complex number.
 * This means take the integer part of both components.
 */
COMPLEX *
cint(c)
	COMPLEX *c;
{
	COMPLEX *r;

	if (cisint(c))
		return clink(c);
	r = comalloc();
	r->real = qint(c->real);
	r->imag = qint(c->imag);
	return r;
}


/*
 * Take the fractional part of a complex number.
 * This means take the fractional part of both components.
 */
COMPLEX *
cfrac(c)
	COMPLEX *c;
{
	COMPLEX *r;

	if (cisint(c))
		return clink(&_czero_);
	r = comalloc();
	r->real = qfrac(c->real);
	r->imag = qfrac(c->imag);
	return r;
}


/*
 * Take the conjugate of a complex number.
 * This negates the complex part.
 */
COMPLEX *
cconj(c)
	COMPLEX *c;
{
	COMPLEX *r;

	if (cisreal(c))
		return clink(c);
	r = comalloc();
	if (!qiszero(c->real))
		r->real = qlink(c->real);
	r->imag = qneg(c->imag);
	return r;
}


/*
 * Return the real part of a complex number.
 */
COMPLEX *
creal(c)
	COMPLEX *c;
{
	COMPLEX *r;

	if (cisreal(c))
		return clink(c);
	r = comalloc();
	if (!qiszero(c->real))
		r->real = qlink(c->real);
	return r;
}


/*
 * Return the imaginary part of a complex number as a real.
 */
COMPLEX *
cimag(c)
	COMPLEX *c;
{
	COMPLEX *r;

	if (cisreal(c))
		return clink(&_czero_);
	r = comalloc();
	r->real = qlink(c->imag);
	return r;
}


/*
 * Add a real number to a complex number.
 */
COMPLEX *
caddq(c, q)
	COMPLEX *c;
	NUMBER *q;
{
	COMPLEX *r;

	if (qiszero(q))
		return clink(c);
	r = comalloc();
	r->real = qadd(c->real, q);
	r->imag = qlink(c->imag);
	return r;
}


/*
 * Subtract a real number from a complex number.
 */
COMPLEX *
csubq(c, q)
	COMPLEX *c;
	NUMBER *q;
{
	COMPLEX *r;

	if (qiszero(q))
		return clink(c);
	r = comalloc();
	r->real = qsub(c->real, q);
	r->imag = qlink(c->imag);
	return r;
}


/*
 * Shift the components of a complex number left by the specified
 * number of bits.  Negative values shift to the right.
 */
COMPLEX *
cshift(c, n)
	COMPLEX *c;
	long n;
{
	COMPLEX *r;

	if (ciszero(c) || (n == 0))
		return clink(c);
	r = comalloc();
	r->real = qshift(c->real, n);
	r->imag = qshift(c->imag, n);
	return r;
}


/*
 * Scale a complex number by a power of two.
 */
COMPLEX *
cscale(c, n)
	COMPLEX *c;
	long n;
{
	COMPLEX *r;

	if (ciszero(c) || (n == 0))
		return clink(c);
	r = comalloc();
	r->real = qscale(c->real, n);
	r->imag = qscale(c->imag, n);
	return r;
}


/*
 * Multiply a complex number by a real number.
 */
COMPLEX *
cmulq(c, q)
	COMPLEX *c;
	NUMBER *q;
{
	COMPLEX *r;

	if (qiszero(q))
		return clink(&_czero_);
	if (qisone(q))
		return clink(c);
	if (qisnegone(q))
		return cneg(c);
	r = comalloc();
	r->real = qmul(c->real, q);
	r->imag = qmul(c->imag, q);
	return r;
}


/*
 * Divide a complex number by a real number.
 */
COMPLEX *
cdivq(c, q)
	COMPLEX *c;
	NUMBER *q;
{
	COMPLEX *r;

	if (qiszero(q))
		math_error("Division by zero");
	if (qisone(q))
		return clink(c);
	if (qisnegone(q))
		return cneg(c);
	r = comalloc();
	r->real = qdiv(c->real, q);
	r->imag = qdiv(c->imag, q);
	return r;
}


/*
 * Take the integer quotient of a complex number by a real number.
 * This is defined to be the result of doing the quotient for each component.
 */
COMPLEX *
cquoq(c, q)
	COMPLEX *c;
	NUMBER *q;
{
	COMPLEX *r;

	if (qiszero(q))
		math_error("Division by zero");
	r = comalloc();
	r->real = qquo(c->real, q);
	r->imag = qquo(c->imag, q);
	return r;
}


/*
 * Take the modulus of a complex number by a real number.
 * This is defined to be the result of doing the modulo for each component.
 */
COMPLEX *
cmodq(c, q)
	COMPLEX *c;
	NUMBER *q;
{
	COMPLEX *r;

	if (qiszero(q))
		math_error("Division by zero");
	r = comalloc();
	r->real = qmod(c->real, q);
	r->imag = qmod(c->imag, q);
	return r;
}


/*
 * Construct a complex number given the real and imaginary components.
 */
COMPLEX *
qqtoc(q1, q2)
	NUMBER *q1, *q2;
{
	COMPLEX *r;

	if (qiszero(q1) && qiszero(q2))
		return clink(&_czero_);
	r = comalloc();
	if (!qiszero(q1))
		r->real = qlink(q1);
	if (!qiszero(q2))
		r->imag = qlink(q2);
	return r;
}


/*
 * Compare two complex numbers for equality, returning FALSE if they are equal,
 * and TRUE if they differ.
 */
BOOL
ccmp(c1, c2)
	COMPLEX *c1, *c2;
{
	BOOL i;

	i = qcmp(c1->real, c2->real);
	if (!i)
		i = qcmp(c1->imag, c2->imag);
	return i;
}


/*
 * Allocate a new complex number.
 */
COMPLEX *
comalloc()
{
	COMPLEX *r;

	r = (COMPLEX *) allocitem(&freelist);
	if (r == NULL)
		math_error("Cannot allocate complex number");
	r->links = 1;
	r->real = qlink(&_qzero_);
	r->imag = qlink(&_qzero_);
	return r;
}


/*
 * Free a complex number.
 */
void
comfree(c)
	COMPLEX *c;
{
	if (--(c->links) > 0)
		return;
	qfree(c->real);
	qfree(c->imag);
	freeitem(&freelist, (FREEITEM *) c);
}

/* END CODE */
