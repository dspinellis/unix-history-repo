/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Extended precision complex arithmetic non-primitive routines
 */

#include "cmath.h"


/*
 * Round a complex number to the specified number of decimal places.
 * This simply means to round each of the components of the number.
 * Zero decimal places means round to the nearest complex integer.
 */
COMPLEX *
cround(c, places)
	COMPLEX *c;
	long places;
{
	COMPLEX *res;		/* result */

	res = comalloc();
	res->real = qround(c->real, places);
	res->imag = qround(c->imag, places);
	return res;
}


/*
 * Round a complex number to the specified number of binary decimal places.
 * This simply means to round each of the components of the number.
 * Zero binary places means round to the nearest complex integer.
 */
COMPLEX *
cbround(c, places)
	COMPLEX *c;
	long places;
{
	COMPLEX *res;		/* result */

	res = comalloc();
	res->real = qbround(c->real, places);
	res->imag = qbround(c->imag, places);
	return res;
}


/*
 * Compute the result of raising a complex number to an integer power.
 */
COMPLEX *
cpowi(c, q)
	COMPLEX *c;		/* complex number to be raised */
	NUMBER *q;		/* power to raise it to */
{
	COMPLEX *tmp, *res;	/* temporary values */
	long power;		/* power to raise to */
	unsigned long bit;	/* current bit value */
	int sign;

	if (qisfrac(q))
		math_error("Raising number to non-integral power");
	if (zisbig(q->num))
		math_error("Raising number to very large power");
	power = (zistiny(q->num) ? z1tol(q->num) : z2tol(q->num));
	if (ciszero(c) && (power == 0))
		math_error("Raising zero to zeroth power");
	sign = 1;
	if (qisneg(q))
		sign = -1;
	/*
	 * Handle some low powers specially
	 */
	if (power <= 4) {
		switch ((int) (power * sign)) {
			case 0:
				return clink(&_cone_);
			case 1:
				return clink(c);
			case -1:
				return cinv(c);
			case 2:
				return csquare(c);
			case -2:
				tmp = csquare(c);
				res = cinv(tmp);
				comfree(tmp);
				return res;
			case 3:
				tmp = csquare(c);
				res = cmul(c, tmp);
				comfree(tmp);
				return res;
			case 4:
				tmp = csquare(c);
				res = csquare(tmp);
				comfree(tmp);
				return res;
		}
	}
	/*
	 * Compute the power by squaring and multiplying.
	 * This uses the left to right method of power raising.
	 */
	bit = TOPFULL;
	while ((bit & power) == 0)
		bit >>= 1L;
	bit >>= 1L;
	res = csquare(c);
	if (bit & power) {
		tmp = cmul(res, c);
		comfree(res);
		res = tmp;
	}
	bit >>= 1L;
	while (bit) {
		tmp = csquare(res);
		comfree(res);
		res = tmp;
		if (bit & power) {
			tmp = cmul(res, c);
			comfree(res);
			res = tmp;
		}
		bit >>= 1L;
	}
	if (sign < 0) {
		tmp = cinv(res);
		comfree(res);
		res = tmp;
	}
	return res;
}


/*
 * Calculate the square root of a complex number, with each component
 * within the specified error.  If the number is a square, then the error
 * is zero.  For sqrt(a + bi), this calculates:
 *	R = sqrt(a^2 + b^2)
 *	U = sqrt((R + abs(a))/2)
 *	V = b/(2 * U)
 *	then sqrt(a + bi) = U + Vi if a >= 0,
 *	or abs(V) + sgn(b) * U  if a < 0
 */
COMPLEX *
csqrt(c, epsilon)
	COMPLEX *c;
	NUMBER *epsilon;
{
	COMPLEX *r;
	NUMBER *A, *B, *R, *U, *V, *tmp1, *tmp2, *epsilon2;
	long m, n;

	if (ciszero(c) || cisone(c))
		return clink(c);
	if (cisreal(c)) {
		r = comalloc();
		if (!qisneg(c->real)) {
			r->real = qsqrt(c->real, epsilon);
			return r;
		}
		tmp1 = qneg(c->real);
		r->imag = qsqrt(tmp1, epsilon);
		qfree(tmp1);
		return r;
	}

	A = qlink(c->real);
	B = qlink(c->imag);
	n = zhighbit(B->num) - zhighbit(B->den);
	if (!qiszero(A)) {
		m = zhighbit(A->num) - zhighbit(A->den);
		if (m > n)
			n = m;
	}
	epsilon2 = qscale(epsilon, n/2);
	R = qhypot(A, B, epsilon2);
	qfree(epsilon2);
	if (qisneg(A))
		tmp1 = qsub(R, A);
	else
		tmp1 = qadd(R, A);
	qfree(A);
	tmp2 = qscale(tmp1, -1L);
	qfree(tmp1);
	U = qsqrt(tmp2, epsilon);
	qfree(tmp2);
	qfree(R);
	if (qiszero(U)) {
		qfree(B);
		qfree(U);
		return clink(&_czero_);
	}
	tmp1 = qdiv(B, U);
	V = qscale(tmp1, -1L);
	qfree(tmp1);
	r = comalloc();
	if (qisneg(c->real)) {
		if (qisneg(B)) {	
			tmp1 = qneg(U);
			qfree(U);
			U = tmp1;
			tmp2 = qabs(V);
			qfree(V);
			V = tmp2;
		}
		r->real = V;
		r->imag = U;
	} else {
		r->real = U;
		r->imag = V;
	}
	qfree(B);
	return r;
}


/*
 * Take the Nth root of a complex number, where N is a positive integer.
 * Each component of the result is within the specified error.
 */
COMPLEX *
croot(c, q, epsilon)
	COMPLEX *c;
	NUMBER *q, *epsilon;
{
	COMPLEX *r;
	NUMBER *a2pb2, *root, *tmp1, *tmp2, *epsilon2;

	if (qisneg(q) || qiszero(q) || qisfrac(q))
		math_error("Taking bad root of complex number");
	if (cisone(c) || qisone(q))
		return clink(c);
	if (qistwo(q))
		return csqrt(c, epsilon);
	r = comalloc();
	if (cisreal(c) && !qisneg(c->real)) {
		r->real = qroot(c->real, q, epsilon);
		return r;
	}
	/*
	 * Calculate the root using the formula:
	 *	croot(a + bi, n) =
	 *		cpolar(qroot(a^2 + b^2, 2 * n), qatan2(b, a) / n).
	 */
	epsilon2 = qscale(epsilon, -8L);
	tmp1 = qsquare(c->real);
	tmp2 = qsquare(c->imag);
	a2pb2 = qadd(tmp1, tmp2);
	qfree(tmp1);
	qfree(tmp2);
	tmp1 = qscale(q, 1L);
	root = qroot(a2pb2, tmp1, epsilon2);
	qfree(a2pb2);
	qfree(tmp1);
	tmp1 = qatan2(c->imag, c->real, epsilon2);
	qfree(epsilon2);
	tmp2 = qdiv(tmp1, q);
	qfree(tmp1);
	r = cpolar(root, tmp2, epsilon);
	qfree(root);
	qfree(tmp2);
	return r;
}


/*
 * Calculate the complex exponential function to the desired accuracy.
 * We use the formula:
 *	exp(a + bi) = exp(a) * (cos(b) + i * sin(b)).
 */
COMPLEX *
cexp(c, epsilon)
	COMPLEX *c;
	NUMBER *epsilon;
{
	COMPLEX *r;
	NUMBER *tmp1, *tmp2, *epsilon2;

	if (ciszero(c))
		return clink(&_cone_);
	r = comalloc();
	if (cisreal(c)) {
		r->real = qexp(c->real, epsilon);
		return r;
	}
	epsilon2 = qscale(epsilon, -2L);
	r->real = qcos(c->imag, epsilon2);
	r->imag = qlegtoleg(r->real, epsilon2, _sinisneg_);
	if (qiszero(c->real)) {
		qfree(epsilon2);
		return r;
	}
	tmp1 = qexp(c->real, epsilon2);
	qfree(epsilon2);
	tmp2 = qmul(r->real, tmp1);
	qfree(r->real);
	r->real = tmp2;
	tmp2 = qmul(r->imag, tmp1);
	qfree(r->imag);
	qfree(tmp1);
	r->imag = tmp2;
	return r;
}


/*
 * Calculate the natural logarithm of a complex number within the specified
 * error.  We use the formula:
 *	ln(a + bi) = ln(a^2 + b^2) / 2 + i * atan2(b, a).
 */
COMPLEX *
cln(c, epsilon)
	COMPLEX *c;
	NUMBER *epsilon;
{
	COMPLEX *r;
	NUMBER *a2b2, *tmp1, *tmp2;

	if (ciszero(c))
		math_error("Logarithm of zero");
	if (cisone(c))
		return clink(&_czero_);
	r = comalloc();
	if (cisreal(c) && !qisneg(c->real)) {
		r->real = qln(c->real, epsilon);
		return r;
	}
	tmp1 = qsquare(c->real);
	tmp2 = qsquare(c->imag);
	a2b2 = qadd(tmp1, tmp2);
	qfree(tmp1);
	qfree(tmp2);
	tmp1 = qln(a2b2, epsilon);
	qfree(a2b2);
	r->real = qscale(tmp1, -1L);
	qfree(tmp1);
	r->imag = qatan2(c->imag, c->real, epsilon);
	return r;
}


/*
 * Calculate the complex cosine within the specified accuracy.
 * This uses the formula:
 *	cos(a + bi) = cos(a) * cosh(b) - sin(a) * sinh(b) * i.
 */
COMPLEX *
ccos(c, epsilon)
	COMPLEX *c;
	NUMBER *epsilon;
{
	COMPLEX *r;
	NUMBER *cosval, *coshval, *tmp1, *tmp2, *tmp3, *epsilon2;
	int negimag;

	if (ciszero(c))
		return clink(&_cone_);
	r = comalloc();
	if (cisreal(c)) {
		r->real = qcos(c->real, epsilon);
		return r;
	}
	if (qiszero(c->real)) {
		r->real = qcosh(c->imag, epsilon);
		return r;
	}
	epsilon2 = qscale(epsilon, -2L);
	coshval = qcosh(c->imag, epsilon2);
	cosval = qcos(c->real, epsilon2);
	negimag = !_sinisneg_;
	if (qisneg(c->imag))
		negimag = !negimag;
	r->real = qmul(cosval, coshval);
	/*
	 * Calculate the imaginary part using the formula:
	 *	sin(a) * sinh(b) = sqrt((1 - a^2) * (b^2 - 1)).
	 */
	tmp1 = qsquare(cosval);
	qfree(cosval);
	tmp2 = qdec(tmp1);
	qfree(tmp1);
	tmp1 = qneg(tmp2);
	qfree(tmp2);
	tmp2 = qsquare(coshval);
	qfree(coshval);
	tmp3 = qdec(tmp2);
	qfree(tmp2);
	tmp2 = qmul(tmp1, tmp3);
	qfree(tmp1);
	qfree(tmp3);
	r->imag = qsqrt(tmp2, epsilon2);
	qfree(tmp2);
	qfree(epsilon2);
	if (negimag) {
		tmp1 = qneg(r->imag);
		qfree(r->imag);
		r->imag = tmp1;
	}
	return r;
}


/*
 * Calculate the complex sine within the specified accuracy.
 * This uses the formula:
 *	sin(a + bi) = sin(a) * cosh(b) + cos(a) * sinh(b) * i.
 */
COMPLEX *
csin(c, epsilon)
	COMPLEX *c;
	NUMBER *epsilon;
{
	COMPLEX *r;

	NUMBER *cosval, *coshval, *tmp1, *tmp2, *epsilon2;

	if (ciszero(c))
		return clink(&_czero_);
	r = comalloc();
	if (cisreal(c)) {
		r->real = qsin(c->real, epsilon);
		return r;
	}
	if (qiszero(c->real)) {
		r->imag = qsinh(c->imag, epsilon);
		return r;
	}
	epsilon2 = qscale(epsilon, -2L);
	coshval = qcosh(c->imag, epsilon2);
	cosval = qcos(c->real, epsilon2);
	tmp1 = qlegtoleg(cosval, epsilon2, _sinisneg_);
	r->real = qmul(tmp1, coshval);
	qfree(tmp1);
	tmp1 = qsquare(coshval);
	qfree(coshval);
	tmp2 = qdec(tmp1);
	qfree(tmp1);
	tmp1 = qsqrt(tmp2, epsilon2);
	qfree(tmp2);
	r->imag = qmul(tmp1, cosval);
	qfree(tmp1);
	qfree(cosval);
	if (qisneg(c->imag)) {
		tmp1 = qneg(r->imag);
		qfree(r->imag);
		r->imag = tmp1;
	}
	return r;
}


/*
 * Convert a number from polar coordinates to normal complex number form
 * within the specified accuracy.  This produces the value:
 *	q1 * cos(q2) + q1 * sin(q2) * i.
 */
COMPLEX *
cpolar(q1, q2, epsilon)
	NUMBER *q1, *q2, *epsilon;
{
	COMPLEX *r;
	NUMBER *tmp, *epsilon2;
	long scale;

	r = comalloc();
	if (qiszero(q1) || qiszero(q2)) {
		r->real = qlink(q1);
		return r;
	}
	epsilon2 = epsilon;
	if (!qisunit(q1)) {
		scale = zhighbit(q1->num) - zhighbit(q1->den) + 1;
		if (scale > 0)
			epsilon2 = qscale(epsilon, -scale);
	}
	r->real = qcos(q2, epsilon2);
	r->imag = qlegtoleg(r->real, epsilon2, _sinisneg_);
	if (epsilon2 != epsilon)
		qfree(epsilon2);
	if (qisone(q1))
		return r;
	tmp = qmul(r->real, q1);
	qfree(r->real);
	r->real = tmp;
	tmp = qmul(r->imag, q1);
	qfree(r->imag);
	r->imag = tmp;
	return r;
}


/*
 * Raise one complex number to the power of another one to within the
 * specified error.
 */
COMPLEX *
cpower(c1, c2, epsilon)
	COMPLEX *c1, *c2;
	NUMBER *epsilon;
{
	COMPLEX *tmp1, *tmp2;
	NUMBER *epsilon2;

	if (cisreal(c2) && qisint(c2->real))
		return cpowi(c1, c2->real);
	if (cisone(c1) || ciszero(c1))
		return clink(c1);
	epsilon2 = qscale(epsilon, -4L);
	tmp1 = cln(c1, epsilon2);
	tmp2 = cmul(tmp1, c2);
	comfree(tmp1);
	tmp1 = cexp(tmp2, epsilon);
	comfree(tmp2);
	qfree(epsilon2);
	return tmp1;
}


/*
 * Return a trivial hash value for a complex number.
 */
HASH
chash(c)
	COMPLEX *c;
{
	HASH hash;

	hash = qhash(c->real);
	if (!cisreal(c))
		hash += qhash(c->imag) * 2000029;
	return hash;
}


/*
 * Print a complex number in the current output mode.
 */
void
comprint(c)
	COMPLEX *c;
{
	NUMBER qtmp;

	if (_outmode_ == MODE_FRAC) {
		cprintfr(c);
		return;
	}
	if (!qiszero(c->real) || qiszero(c->imag))
		qprintnum(c->real, MODE_DEFAULT);
	qtmp = c->imag[0];
	if (qiszero(&qtmp))
		return;
	if (!qiszero(c->real) && !qisneg(&qtmp))
		math_chr('+');
	if (qisneg(&qtmp)) {
		math_chr('-');
		qtmp.num.sign = 0;
	}
	qprintnum(&qtmp, MODE_DEFAULT);
	math_chr('i');
}


/*
 * Print a complex number in rational representation.
 * Example:  2/3-4i/5
 */
void
cprintfr(c)
	COMPLEX *c;
{
	NUMBER *r;
	NUMBER *i;

	r = c->real;
	i = c->imag;
	if (!qiszero(r) || qiszero(i))
		qprintfr(r, 0L, FALSE);
	if (qiszero(i))
		return;
	if (!qiszero(r) && !qisneg(i))
		math_chr('+');
	zprintval(i->num, 0L, 0L);
	math_chr('i');
	if (qisfrac(i)) {
		math_chr('/');
		zprintval(i->den, 0L, 0L);
	}
}

/* END CODE */
