/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Transcendental functions for real numbers.
 * These are sin, cos, exp, ln, power, cosh, sinh.
 */

#include "qmath.h"

BOOL _sinisneg_;	/* whether sin(x) < 0 (set by cos(x)) */


/*
 * Calculate the cosine of a number with an accuracy within epsilon.
 * This also saves the sign of the corresponding sin function.
 */
NUMBER *
qcos(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *term, *sum, *qsq, *epsilon2, *tmp;
	FULL n, i;
	long scale, bits, bits2;

	_sinisneg_ = qisneg(q);
	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for cosine");
	if (qiszero(q))
		return qlink(&_qone_);
	bits = qprecision(epsilon) + 1;
	epsilon = qscale(epsilon, -4L);
	/*
	 * If the argument is larger than one, then divide it by a power of two
	 * so that it is one or less.  This will make the series converge quickly.
	 * We will extrapolate the result for the original argument afterwards.
	 */
	scale = zhighbit(q->num) - zhighbit(q->den) + 1;
	if (scale < 0)
		scale = 0;
	if (scale > 0) {
		q = qscale(q, -scale);
		tmp = qscale(epsilon, -scale);
		qfree(epsilon);
		epsilon = tmp;
	}
	epsilon2 = qscale(epsilon, -4L);
	qfree(epsilon);
	bits2 = qprecision(epsilon2) + 10;
	/*
	 * Now use the Taylor series expansion to calculate the cosine.
	 * Keep using approximations so that the fractions don't get too large.
	 */
	qsq = qsquare(q);
	if (scale > 0)
		qfree(q);
	term = qlink(&_qone_);
	sum = qlink(&_qone_);
	n = 0;
	while (qrel(term, epsilon2) > 0) {
		i = ++n;
		i *= ++n;
		tmp = qmul(term, qsq);
		qfree(term);
		term = qdivi(tmp, (long) i);
		qfree(tmp);
		tmp = qbround(term, bits2);
		qfree(term);
		term = tmp;
		if (n & 2)
			tmp = qsub(sum, term);
		else
			tmp = qadd(sum, term);
		qfree(sum);
		sum = qbround(tmp, bits2);
		qfree(tmp);
	}
	qfree(term);
	qfree(qsq);
	qfree(epsilon2);
	/*
	 * Now scale back up to the original value of x by using the formula:
	 *	cos(2 * x) = 2 * (cos(x) ^ 2) - 1.
	 */
	while (--scale >= 0) {
		if (qisneg(sum))
			_sinisneg_ = !_sinisneg_;
		tmp = qsquare(sum);
		qfree(sum);
		sum = qscale(tmp, 1L);
		qfree(tmp);
		tmp = qdec(sum);
		qfree(sum);
		sum = qbround(tmp, bits2);
		qfree(tmp);
	}
	tmp = qbround(sum, bits);
	qfree(sum);
	return tmp;
}


/*
 * Calculate the sine of a number with an accuracy within epsilon.
 * This is calculated using the formula:
 *	sin(x)^2 + cos(x)^2 = 1.
 * The only tricky bit is resolving the sign of the result.
 * Future: Use sin(3*x) = 3*sin(x) - 4*sin(x)^3.
 */
NUMBER *
qsin(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *tmp1, *tmp2, *epsilon2;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for sine");
	if (qiszero(q))
		return qlink(q);
	epsilon2 = qsquare(epsilon);
	tmp1 = qcos(q, epsilon2);
	qfree(epsilon2);
	tmp2 = qlegtoleg(tmp1, epsilon, _sinisneg_);
	qfree(tmp1);
	return tmp2;
}


/*
 * Calculate the tangent function.
 */
NUMBER *
qtan(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *cosval, *sinval, *epsilon2, *tmp, *res;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for tangent");
	if (qiszero(q))
		return qlink(q);
	epsilon2 = qsquare(epsilon);
	cosval = qcos(q, epsilon2);
	sinval = qlegtoleg(cosval, epsilon2, _sinisneg_);
	qfree(epsilon2);
	tmp = qdiv(sinval, cosval);
	qfree(cosval);
	qfree(sinval);
	res = qbround(tmp, qprecision(epsilon) + 1);
	qfree(tmp);
	return res;
}


/*
 * Calculate the arcsine function.
 * The result is in the range -pi/2 to pi/2.
 */
NUMBER *
qasin(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *sum, *term, *epsilon2, *qsq, *tmp;
	FULL n, i;
	long bits, bits2;
	int neg;
	NUMBER mulnum;
	HALF numval[2];
	HALF denval[2];

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for arcsine");
	if (qiszero(q))
		return qlink(&_qzero_);
	if ((qrel(q, &_qone_) > 0) || (qrel(q, &_qnegone_) < 0))
		math_error("Argument too large for asin");
	neg = qisneg(q);
	q = qabs(q);
	epsilon = qscale(epsilon, -4L);
	epsilon2 = qscale(epsilon, -4L);
	mulnum.num.sign = 0;
	mulnum.num.len = 1;
	mulnum.num.v = numval;
	mulnum.den.sign = 0;
	mulnum.den.len = 1;
	mulnum.den.v = denval;
	/*
	 * If the argument is too near one (we use .5) then reduce the
	 * argument to a more accurate range using the formula:
	 *	asin(x) = 2 * asin(sqrt((1 - sqrt(1 - x^2)) / 2)).
	 */
	if (qrel(q, &_qonehalf_) > 0) {
		sum = qlegtoleg(q, epsilon2, FALSE);
		qfree(q);
		tmp = qsub(&_qone_, sum);
		qfree(sum);
		sum = qscale(tmp, -1L);
		qfree(tmp);
		tmp = qsqrt(sum, epsilon2);
		qfree(sum);
		qfree(epsilon2);
		sum = qasin(tmp, epsilon);
		qfree(tmp);
		qfree(epsilon);
		tmp = qscale(sum, 1L);
		qfree(sum);
		if (neg) {
			sum = qneg(tmp);
			qfree(tmp);
			tmp = sum;
		}
		return tmp;
	}
	/*
	 * Argument is between zero and .5, so use the series.
	 */
	epsilon = qscale(epsilon, -4L);
	epsilon2 = qscale(epsilon, -4L);
	bits = qprecision(epsilon) + 1;
	bits2 = bits + 10;
	sum = qlink(q);
	term = qlink(q);
	qsq = qsquare(q);
	qfree(q);
	n = 1;
	while (qrel(term, epsilon2) > 0) {
		i = n * n;
		numval[0] = i & BASE1;
		if (i >= BASE) {
			numval[1] = i / BASE;
			mulnum.den.len = 2;
		}
		i = (n + 1) * (n + 2);
		denval[0] = i & BASE1;
		if (i >= BASE) {
			denval[1] = i / BASE;
			mulnum.den.len = 2;
		}
		tmp = qmul(term, qsq);
		qfree(term);
		term = qmul(tmp, &mulnum);
		qfree(tmp);
		tmp = qbround(term, bits2);
		qfree(term);
		term = tmp;
		tmp = qadd(sum, term);
		qfree(sum);
		sum = qbround(tmp, bits2);
		qfree(tmp);
		n += 2;
	}
	qfree(epsilon);
	qfree(epsilon2);
	qfree(term);
	qfree(qsq);
	tmp = qbround(sum, bits);
	qfree(sum);
	if (neg) {
		term = qneg(tmp);
		qfree(tmp);
		tmp = term;
	}
	return tmp;
}


/*
 * Calculate the acos function.
 * The result is in the range 0 to pi.
 */
NUMBER *
qacos(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *tmp1, *tmp2, *tmp3, *epsilon2;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for arccosine");
	if (qisone(q))
		return qlink(&_qzero_);
	if ((qrel(q, &_qone_) > 0) || (qrel(q, &_qnegone_) < 0))
		math_error("Argument too large for acos");
	/*
	 * Calculate the result using the formula:
	 *	acos(x) = asin(sqrt(1 - x^2)).
	 * The formula is only good for positive x, so we must fix up the
	 * result for negative values.
	 */
	epsilon2 = qscale(epsilon, -8L);
	tmp1 = qlegtoleg(q, epsilon2, FALSE);
	qfree(epsilon2);
	tmp2 = qasin(tmp1, epsilon);
	qfree(tmp1);
	if (!qisneg(q))
		return tmp2;
	/*
	 * For negative values, we need to subtract the asin from pi.
	 */
	tmp1 = qpi(epsilon);
	tmp3 = qsub(tmp1, tmp2);
	qfree(tmp1);
	qfree(tmp2);
	tmp1 = qbround(tmp3, qprecision(epsilon) + 1);
	qfree(tmp3);
	return tmp1;
}


/*
 * Calculate the arctangent function with a accuracy less than epsilon.
 * This uses the formula:
 *	atan(x) = asin(sqrt(x^2 / (x^2 + 1))).
 */
NUMBER *
qatan(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *tmp1, *tmp2, *tmp3, *epsilon2;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for arctangent");
	if (qiszero(q))
		return qlink(&_qzero_);
	tmp1 = qsquare(q);
	tmp2 = qinc(tmp1);
	tmp3 = qdiv(tmp1, tmp2);
	qfree(tmp1);
	qfree(tmp2);
	epsilon2 = qscale(epsilon, -8L);
	tmp1 = qsqrt(tmp3, epsilon2);
	qfree(epsilon2);
	qfree(tmp3);
	tmp2 = qasin(tmp1, epsilon);
	qfree(tmp1);
	if (qisneg(q)) {
		tmp1 = qneg(tmp2);
		qfree(tmp2);
		tmp2 = tmp1;
	}
	return tmp2;
}


/*
 * Calculate the angle which is determined by the point (x,y).
 * This is the same as arctan for non-negative x, but gives the correct
 * value for negative x.  By convention, y is the first argument.
 * For example, qatan2(1, -1) = 3/4 * pi.
 */
NUMBER *
qatan2(qy, qx, epsilon)
	NUMBER *qy, *qx, *epsilon;
{
	NUMBER *tmp1, *tmp2, *epsilon2;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for atan2");
	if (qiszero(qy) && qiszero(qx)) {
		/* conform to 4.3BSD ANSI/IEEE 754-1985 math lib */
		return qlink(&_qzero_);
	}
	/*
	 * If the point is on the negative real axis, then the answer is pi.
	 */
	if (qiszero(qy) && qisneg(qx))
		return qpi(epsilon);
	/*
	 * If the point is in the right half plane, then use the normal atan.
	 */
	if (!qisneg(qx) && !qiszero(qx)) {
		if (qiszero(qy))
			return qlink(&_qzero_);
		tmp1 = qdiv(qy, qx);
		tmp2 = qatan(tmp1, epsilon);
		qfree(tmp1);
		return tmp2;
	}
	/*
	 * The point is in the left half plane.  Calculate the angle by finding
	 * the atan of half the angle using the formula:
	 *	atan2(y,x) = 2 * atan((sqrt(x^2 + y^2) - x) / y).
	 */
	epsilon2 = qscale(epsilon, -4L);
	tmp1 = qhypot(qx, qy, epsilon2);
	tmp2 = qsub(tmp1, qx);
	qfree(tmp1);
	tmp1 = qdiv(tmp2, qy);
	qfree(tmp2);
	tmp2 = qatan(tmp1, epsilon2);
	qfree(tmp1);
	qfree(epsilon2);
	tmp1 = qscale(tmp2, 1L);
	qfree(tmp2);
	return tmp1;
}


/*
 * Calculate the value of pi to within the required epsilon.
 * This uses the following formula which only needs integer calculations
 * except for the final operation:
 *	pi = 1 / SUMOF(comb(2 * N, N) ^ 3 * (42 * N + 5) / 2 ^ (12 * N + 4)),
 * where the summation runs from N=0.  This formula gives about 6 bits of
 * accuracy per term.  Since the denominator for each term is a power of two,
 * we can simply use shifts to sum the terms.  The combinatorial numbers
 * in the formula are calculated recursively using the formula:
 *	comb(2*(N+1), N+1) = 2 * comb(2 * N, N) * (2 * N + 1) / N.
 */
NUMBER *
qpi(epsilon)
	NUMBER *epsilon;
{
	ZVALUE comb;			/* current combinatorial value */
	ZVALUE sum;			/* current sum */
	ZVALUE tmp1, tmp2;
	NUMBER *r, *t1, qtmp;
	long shift;			/* current shift of result */
	long N;				/* current term number */
	long bits;			/* needed number of bits of precision */
	long t;

	if (qiszero(epsilon) || qisneg(epsilon))
		math_error("Bad epsilon value for pi");
	bits = qprecision(epsilon) + 4;
	comb = _one_;
	itoz(5L, &sum);
	N = 0;
	shift = 4;
	do {
		t = 1 + (++N & 0x1);
		(void) zdivi(comb, N / (3 - t), &tmp1);
		zfree(comb);
		zmuli(tmp1, t * (2 * N - 1), &comb);
		zfree(tmp1);
		zsquare(comb, &tmp1);
		zmul(comb, tmp1, &tmp2);
		zfree(tmp1);
		zmuli(tmp2, 42 * N + 5, &tmp1);
		zfree(tmp2);
		zshift(sum, 12L, &tmp2);
		zfree(sum);
		zadd(tmp1, tmp2, &sum);
		t = zhighbit(tmp1);
		zfree(tmp1);
		zfree(tmp2);
		shift += 12;
	} while ((shift - t) < bits);
	qtmp.num = _one_;
	qtmp.den = sum;
	t1 = qscale(&qtmp, shift);
	zfree(sum);
	r = qbround(t1, bits);
	qfree(t1);
	return r;
}


/*
 * Calculate the exponential function with a relative accuracy less than
 * epsilon.
 */
NUMBER *
qexp(q, epsilon)
	NUMBER *q, *epsilon;
{
	long scale;
	FULL n;
	long bits, bits2;
	NUMBER *sum, *term, *qs, *epsilon2, *tmp;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for exp");
	if (qiszero(q))
		return qlink(&_qone_);
	epsilon = qscale(epsilon, -4L);
	/*
	 * If the argument is larger than one, then divide it by a power of two
	 * so that it is one or less.  This will make the series converge quickly.
	 * We will extrapolate the result for the original argument afterwards.
	 * Also make the argument non-negative.
	 */
	qs = qabs(q);
	scale = zhighbit(q->num) - zhighbit(q->den) + 1;
	if (scale < 0)
		scale = 0;
	if (scale > 0) {
		if (scale > 100000)
			math_error("Very large argument for exp");
		tmp = qscale(qs, -scale);
		qfree(qs);
		qs = tmp;
		tmp = qscale(epsilon, -scale);
		qfree(epsilon);
		epsilon = tmp;
	}
	epsilon2 = qscale(epsilon, -4L);
	bits = qprecision(epsilon) + 1;
	bits2 = bits + 10;
	qfree(epsilon);
	/*
	 * Now use the Taylor series expansion to calculate the exponential.
	 * Keep using approximations so that the fractions don't get too large.
	 */
	sum = qlink(&_qone_);
	term = qlink(&_qone_);
	n = 0;
	while (qrel(term, epsilon2) > 0) {
		n++;
		tmp = qmul(term, qs);
		qfree(term);
		term = qdivi(tmp, (long) n);
		qfree(tmp);
		tmp = qbround(term, bits2);
		qfree(term);
		term = tmp;
		tmp = qadd(sum, term);
		qfree(sum);
		sum = qbround(tmp, bits2);
		qfree(tmp);
	}
	qfree(term);
	qfree(qs);
	qfree(epsilon2);
	/*
	 * Now repeatedly square the answer to get the final result.
	 * Then invert it if the original argument was negative.
	 */
	while (--scale >= 0) {
		tmp = qsquare(sum);
		qfree(sum);
		sum = qbround(tmp, bits2);
		qfree(tmp);
	}
	tmp = qbround(sum, bits);
	qfree(sum);
	if (qisneg(q)) {
		sum = qinv(tmp);
		qfree(tmp);
		tmp = sum;
	}
	return tmp;
}


/*
 * Calculate the natural logarithm of a number accurate to the specified
 * epsilon.
 */
NUMBER *
qln(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *term, *term2, *sum, *epsilon2, *tmp1, *tmp2, *maxr;
	long shift, bits, bits2;
	int j, k;
	FULL n;
	BOOL neg;

	if (qisneg(q) || qiszero(q))
		math_error("log of non-positive number");
	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon for ln");
	if (qisone(q))
		return qlink(&_qzero_);
	/*
	 * If the number is less than one, invert it and remember that
	 * the result is to be negative.
	 */
	neg = FALSE;
	if (zrel(q->num, q->den) < 0) {
		neg = TRUE;
		q = qinv(q);
	} else
		q = qlink(q);
	j = 16;
	k = zhighbit(q->num) - zhighbit(q->den) + 1;
	while (k >>= 1)
		j++;
	epsilon2 = qscale(epsilon, -j);
	bits = qprecision(epsilon) + 1;
	bits2 = qprecision(epsilon2) + 5;
	/*
	 * By repeated square-roots scale number down to a value close
	 * to 1 so that Taylor series to be used will converge rapidly.
	 * The effect of scaling will be reversed by a later shift.
	 */
	maxr = iitoq(BASE + 1, BASE);
	shift = 1;
	while (qrel(q, maxr) > 0) {
		tmp1 = qsqrt(q, epsilon2);
		qfree(q);
		q = tmp1;
		shift++;
	}
	qfree(maxr);
	/*
	 * Calculate a value which will always converge using the formula:
	 *	ln((1+x)/(1-x)) = ln(1+x) - ln(1-x).
	 */
	tmp1 = qdec(q);
	tmp2 = qinc(q);
	term = qdiv(tmp1, tmp2);
	qfree(tmp1);
	qfree(tmp2);
	qfree(q);
	/*
	 * Now use the Taylor series expansion to calculate the result.
	 */
	n = 1;
	term2 = qsquare(term);
	sum = qlink(term);
	while (qrel(term, epsilon2) > 0) {
		n += 2;
		tmp1 = qmul(term, term2);
		qfree(term);
		term = qbround(tmp1, bits2);
		qfree(tmp1);
		tmp1 = qdivi(term, (long) n);
		tmp2 = qadd(sum, tmp1);
		qfree(tmp1);
		qfree(sum);
		sum = qbround(tmp2, bits2);
	}
	qfree(epsilon2);
	qfree(term);
	qfree(term2);
	/*
	 * Calculate the final result by multiplying by the proper power
	 * of two to undo the square roots done at the top, and possibly
	 * negating the result.
	 */
	tmp1 = qscale(sum, shift);
	qfree(sum);
	sum = qbround(tmp1, bits);
	qfree(tmp1);
	if (neg) {
		tmp1 = qneg(sum);
		qfree(sum);
		sum = tmp1;
	}
	return sum;
}


/*
 * Calculate the result of raising one number to the power of another.
 * The result is calculated to within the specified relative error.
 */
NUMBER *
qpower(q1, q2, epsilon)
	NUMBER *q1, *q2, *epsilon;
{
	NUMBER *tmp1, *tmp2, *epsilon2;

	if (qisint(q2))
		return qpowi(q1, q2);
	epsilon2 = qscale(epsilon, -4L);
	tmp1 = qln(q1, epsilon2);
	tmp2 = qmul(tmp1, q2);
	qfree(tmp1);
	tmp1 = qexp(tmp2, epsilon);
	qfree(tmp2);
	qfree(epsilon2);
	return tmp1;
}


/*
 * Calculate the Kth root of a number to within the specified accuracy.
 */
NUMBER *
qroot(q1, q2, epsilon)
	NUMBER *q1, *q2, *epsilon;
{
	NUMBER *tmp1, *tmp2, *epsilon2;
	int neg;

	if (qisneg(q2) || qiszero(q2) || qisfrac(q2))
		math_error("Taking bad root of number");
	if (qiszero(q1) || qisone(q1) || qisone(q2))
		return qlink(q1);
	if (qistwo(q2))
		return qsqrt(q1, epsilon);
	neg = qisneg(q1);
	if (neg) {
		if (ziseven(q2->num))
			math_error("Taking even root of negative number");
		q1 = qabs(q1);
	}
	epsilon2 = qscale(epsilon, -4L);
	tmp1 = qln(q1, epsilon2);
	tmp2 = qdiv(tmp1, q2);
	qfree(tmp1);
	tmp1 = qexp(tmp2, epsilon);
	qfree(tmp2);
	qfree(epsilon2);
	if (neg) {
		tmp2 = qneg(tmp1);
		qfree(tmp1);
		tmp1 = tmp2;
	}
	return tmp1;
}


/*
 * Calculate the hyperbolic cosine function with a relative accuracy less
 * than epsilon.  This is defined by:
 *	cosh(x) = (exp(x) + exp(-x)) / 2.
 */
NUMBER *
qcosh(q, epsilon)
	NUMBER *q, *epsilon;
{
	long scale;
	FULL n;
	FULL m;
	long bits, bits2;
	NUMBER *sum, *term, *qs, *epsilon2, *tmp;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for exp");
	if (qiszero(q))
		return qlink(&_qone_);
	epsilon = qscale(epsilon, -4L);
	/*
	 * If the argument is larger than one, then divide it by a power of two
	 * so that it is one or less.  This will make the series converge quickly.
	 * We will extrapolate the result for the original argument afterwards.
	 */
	qs = qabs(q);
	scale = zhighbit(q->num) - zhighbit(q->den) + 1;
	if (scale < 0)
		scale = 0;
	if (scale > 0) {
		if (scale > 100000)
			math_error("Very large argument for exp");
		tmp = qscale(qs, -scale);
		qfree(qs);
		qs = tmp;
		tmp = qscale(epsilon, -scale);
		qfree(epsilon);
		epsilon = tmp;
	}
	epsilon2 = qscale(epsilon, -4L);
	bits = qprecision(epsilon) + 1;
	bits2 = bits + 10;
	qfree(epsilon);
	tmp = qsquare(qs);
	qfree(qs);
	qs = tmp;
	/*
	 * Now use the Taylor series expansion to calculate the exponential.
	 * Keep using approximations so that the fractions don't get too large.
	 */
	sum = qlink(&_qone_);
	term = qlink(&_qone_);
	n = 0;
	while (qrel(term, epsilon2) > 0) {
		m = ++n;
		m *= ++n;
		tmp = qmul(term, qs);
		qfree(term);
		term = qdivi(tmp, (long) m);
		qfree(tmp);
		tmp = qbround(term, bits2);
		qfree(term);
		term = tmp;
		tmp = qadd(sum, term);
		qfree(sum);
		sum = qbround(tmp, bits2);
		qfree(tmp);
	}
	qfree(term);
	qfree(qs);
	qfree(epsilon2);
	/*
	 * Now bring the number back up into range to get the final result.
	 * This uses the formula:
	 *	cosh(2 * x) = 2 * cosh(x)^2 - 1.
	 */
	while (--scale >= 0) {
		tmp = qsquare(sum);
		qfree(sum);
		sum = qscale(tmp, 1L);
		qfree(tmp);
		tmp = qdec(sum);
		qfree(sum);
		sum = qbround(tmp, bits2);
		qfree(tmp);
	}
	tmp = qbround(sum, bits);
	qfree(sum);
	return tmp;
}


/*
 * Calculate the hyperbolic sine with an accurary less than epsilon.
 * This is calculated using the formula:
 *	cosh(x)^2 - sinh(x)^2 = 1.
 */
NUMBER *
qsinh(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *tmp1, *tmp2;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for sinh");
	if (qiszero(q))
		return qlink(q);
	epsilon = qscale(epsilon, -4L);
	tmp1 = qcosh(q, epsilon);
	tmp2 = qsquare(tmp1);
	qfree(tmp1);
	tmp1 = qdec(tmp2);
	qfree(tmp2);
	tmp2 = qsqrt(tmp1, epsilon);
	qfree(tmp1);
	if (qisneg(q)) {
		tmp1 = qneg(tmp2);
		qfree(tmp2);
		tmp2 = tmp1;
	}
	qfree(epsilon);
	return tmp2;
}


/*
 * Calculate the hyperbolic tangent with an accurary less than epsilon.
 * This is calculated using the formula:
 *	tanh(x) = sinh(x) / cosh(x).
 */
NUMBER *
qtanh(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *tmp1, *tmp2, *coshval;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for tanh");
	if (qiszero(q))
		return qlink(q);
	epsilon = qscale(epsilon, -4L);
	coshval = qcosh(q, epsilon);
	tmp2 = qsquare(coshval);
	tmp1 = qdec(tmp2);
	qfree(tmp2);
	tmp2 = qsqrt(tmp1, epsilon);
	qfree(tmp1);
	if (qisneg(q)) {
		tmp1 = qneg(tmp2);
		qfree(tmp2);
		tmp2 = tmp1;
	}
	qfree(epsilon);
	tmp1 = qdiv(tmp2, coshval);
	qfree(tmp2);
	qfree(coshval);
	return tmp1;
}


/*
 * Compute the hyperbolic arccosine within the specified accuracy.
 * This is calculated using the formula:
 *	acosh(x) = ln(x + sqrt(x^2 - 1)).
 */
NUMBER *
qacosh(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *tmp1, *tmp2, *epsilon2;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for acosh");
	if (qisone(q))
		return qlink(&_qzero_);
	if (qreli(q, 1L) < 0)
		math_error("Argument less than one for acosh");
	epsilon2 = qscale(epsilon, -8L);
	tmp1 = qsquare(q);
	tmp2 = qdec(tmp1);
	qfree(tmp1);
	tmp1 = qsqrt(tmp2, epsilon2);
	qfree(tmp2);
	tmp2 = qadd(tmp1, q);
	qfree(tmp1);
	tmp1 = qln(tmp2, epsilon);
	qfree(tmp2);
	qfree(epsilon2);
	return tmp1;
}


/*
 * Compute the hyperbolic arcsine within the specified accuracy.
 * This is calculated using the formula:
 *	asinh(x) = ln(x + sqrt(x^2 + 1)).
 */
NUMBER *
qasinh(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *tmp1, *tmp2, *epsilon2;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for asinh");
	if (qiszero(q))
		return qlink(&_qzero_);
	epsilon2 = qscale(epsilon, -8L);
	tmp1 = qsquare(q);
	tmp2 = qinc(tmp1);
	qfree(tmp1);
	tmp1 = qsqrt(tmp2, epsilon2);
	qfree(tmp2);
	tmp2 = qadd(tmp1, q);
	qfree(tmp1);
	tmp1 = qln(tmp2, epsilon);
	qfree(tmp2);
	qfree(epsilon2);
	return tmp1;
}


/*
 * Compute the hyperbolic arctangent within the specified accuracy.
 * This is calculated using the formula:
 *	atanh(x) = ln((1 + u) / (1 - u)) / 2.
 */
NUMBER *
qatanh(q, epsilon)
	NUMBER *q, *epsilon;
{
	NUMBER *tmp1, *tmp2, *tmp3;

	if (qisneg(epsilon) || qiszero(epsilon))
		math_error("Illegal epsilon value for atanh");
	if (qiszero(q))
		return qlink(&_qzero_);
	if ((qreli(q, 1L) > 0) || (qreli(q, -1L) < 0))
		math_error("Argument not between -1 and 1 for atanh");
	tmp1 = qinc(q);
	tmp2 = qsub(&_qone_, q);
	tmp3 = qdiv(tmp1, tmp2);
	qfree(tmp1);
	qfree(tmp2);
	tmp1 = qln(tmp3, epsilon);
	qfree(tmp3);
	tmp2 = qscale(tmp1, -1L);
	qfree(tmp1);
	return tmp2;
}

/* END CODE */
