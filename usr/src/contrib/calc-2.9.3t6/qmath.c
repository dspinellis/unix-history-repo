/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Extended precision rational arithmetic primitive routines
 */

#include "qmath.h"


NUMBER _qzero_ =	{ { _zeroval_, 1, 0 }, { _oneval_, 1, 0 }, 1 };
NUMBER _qone_ =		{ { _oneval_, 1, 0 }, { _oneval_, 1, 0 }, 1 };
static NUMBER _qtwo_ =	{ { _twoval_, 1, 0 }, { _oneval_, 1, 0 }, 1 };
static NUMBER _qten_ =	{ { _tenval_, 1, 0 }, { _oneval_, 1, 0 }, 1 };
NUMBER _qnegone_ =	{ { _oneval_, 1, 1 }, { _oneval_, 1, 0 }, 1 };
NUMBER _qonehalf_ =	{ { _oneval_, 1, 0 }, { _twoval_, 1, 0 }, 1 };


/*
 * Create another copy of a number.
 *	q2 = qcopy(q1);
 */
NUMBER *
qcopy(q)
	register NUMBER *q;
{
	register NUMBER *r;

	r = qalloc();
	r->num.sign = q->num.sign;
	if (!zisunit(q->num)) {
		r->num.len = q->num.len;
		r->num.v = alloc(r->num.len);
		zcopyval(q->num, r->num);
	}
	if (!zisunit(q->den)) {
		r->den.len = q->den.len;
		r->den.v = alloc(r->den.len);
		zcopyval(q->den, r->den);
	}
	return r;
}


/*
 * Convert a number to a normal integer.
 *	i = qtoi(q);
 */
long
qtoi(q)
	register NUMBER *q;
{
	long i;
	ZVALUE res;

	if (qisint(q))
		return ztoi(q->num);
	zquo(q->num, q->den, &res);
	i = ztoi(res);
	zfree(res);
	return i;
}


/*
 * Convert a normal integer into a number.
 *	q = itoq(i);
 */
NUMBER *
itoq(i)
	long i;
{
	register NUMBER *q;

	if ((i >= -1) && (i <= 10)) {
		switch ((int) i) {
			case 0: q = &_qzero_; break;
			case 1: q = &_qone_; break;
			case 2: q = &_qtwo_; break;
			case 10: q = &_qten_; break;
			case -1: q = &_qnegone_; break;
			default: q = NULL;
		}
		if (q)
			return qlink(q);
	}
	q = qalloc();
	itoz(i, &q->num);
	return q;
}


/*
 * Create a number from the given integral numerator and denominator.
 *	q = iitoq(inum, iden);
 */
NUMBER *
iitoq(inum, iden)
	long inum, iden;
{
	register NUMBER *q;
	long d;
	BOOL sign;

	if (iden == 0)
		math_error("Division by zero");
	if (inum == 0)
		return qlink(&_qzero_);
	sign = 0;
	if (inum < 0) {
		sign = 1;
		inum = -inum;
	}
	if (iden < 0) {
		sign = 1 - sign;
		iden = -iden;
	}
	d = iigcd(inum, iden);
	inum /= d;
	iden /= d;
	if (iden == 1)
		return itoq(sign ? -inum : inum);
	q = qalloc();
	if (inum != 1)
		itoz(inum, &q->num);
	itoz(iden, &q->den);
	q->num.sign = sign;
	return q;
}


/*
 * Add two numbers to each other.
 *	q3 = qadd(q1, q2);
 */
NUMBER *
qadd(q1, q2)
	register NUMBER *q1, *q2;
{
	NUMBER *r;
	ZVALUE t1, t2, temp, d1, d2, vpd1, upd1;

	if (qiszero(q1))
		return qlink(q2);
	if (qiszero(q2))
		return qlink(q1);
	r = qalloc();
	/*
	 * If either number is an integer, then the result is easy.
	 */
	if (qisint(q1) && qisint(q2)) {
		zadd(q1->num, q2->num, &r->num);
		return r;
	}
	if (qisint(q2)) {
		zmul(q1->den, q2->num, &temp);
		zadd(q1->num, temp, &r->num);
		zfree(temp);
		zcopy(q1->den, &r->den);
		return r;
	}
	if (qisint(q1)) {
		zmul(q2->den, q1->num, &temp);
		zadd(q2->num, temp, &r->num);
		zfree(temp);
		zcopy(q2->den, &r->den);
		return r;
	}
	/*
	 * Both arguments are true fractions, so we need more work.
	 * If the denominators are relatively prime, then the answer is the
	 * straightforward cross product result with no need for reduction.
	 */
	zgcd(q1->den, q2->den, &d1);
	if (zisunit(d1)) {
		zfree(d1);
		zmul(q1->num, q2->den, &t1);
		zmul(q1->den, q2->num, &t2);
		zadd(t1, t2, &r->num);
		zfree(t1);
		zfree(t2);
		zmul(q1->den, q2->den, &r->den);
		return r;
	}
	/*
	 * The calculation is now more complicated.
	 * See Knuth Vol 2 for details.
	 */
	zquo(q2->den, d1, &vpd1);
	zquo(q1->den, d1, &upd1);
	zmul(q1->num, vpd1, &t1);
	zmul(q2->num, upd1, &t2);
	zadd(t1, t2, &temp);
	zfree(t1);
	zfree(t2);
	zfree(vpd1);
	zgcd(temp, d1, &d2);
	zfree(d1);
	if (zisunit(d2)) {
		zfree(d2);
		r->num = temp;
		zmul(upd1, q2->den, &r->den);
		zfree(upd1);
		return r;
	}
	zquo(temp, d2, &r->num);
	zfree(temp);
	zquo(q2->den, d2, &temp);
	zfree(d2);
	zmul(temp, upd1, &r->den);
	zfree(temp);
	zfree(upd1);
	return r;
}


/*
 * Subtract one number from another.
 *	q3 = qsub(q1, q2);
 */
NUMBER *
qsub(q1, q2)
	register NUMBER *q1, *q2;
{
	NUMBER *r;

	if (q1 == q2)
		return qlink(&_qzero_);
	if (qiszero(q2))
		return qlink(q1);
	if (qisint(q1) && qisint(q2)) {
		r = qalloc();
		zsub(q1->num, q2->num, &r->num);
		return r;
	}
	q2 = qneg(q2);
	if (qiszero(q1))
		return q2;
	r = qadd(q1, q2);
	qfree(q2);
	return r;
}


/*
 * Increment a number by one.
 */
NUMBER *
qinc(q)
	NUMBER *q;
{
	NUMBER *r;

	r = qalloc();
	if (qisint(q)) {
		zadd(q->num, _one_, &r->num);
		return r;
	}
	zadd(q->num, q->den, &r->num);
	zcopy(q->den, &r->den);
	return r;
}


/*
 * Decrement a number by one.
 */
NUMBER *
qdec(q)
	NUMBER *q;
{
	NUMBER *r;

	r = qalloc();
	if (qisint(q)) {
		zsub(q->num, _one_, &r->num);
		return r;
	}
	zsub(q->num, q->den, &r->num);
	zcopy(q->den, &r->den);
	return r;
}


/*
 * Add a normal small integer value to an arbitrary number.
 */
NUMBER *
qaddi(q1, n)
	NUMBER *q1;
	long n;
{
	NUMBER addnum;		/* temporary number */
	HALF addval[2];		/* value of small number */
	BOOL neg;		/* TRUE if number is neg */

	if (n == 0)
		return qlink(q1);
	if (n == 1)
		return qinc(q1);
	if (n == -1)
		return qdec(q1);
	if (qiszero(q1))
		return itoq(n);
	addnum.num.sign = 0;
	addnum.num.len = 1;
	addnum.num.v = addval;
	addnum.den = _one_;
	neg = (n < 0);
	if (neg)
		n = -n;
	addval[0] = (HALF) n;
	n = (((FULL) n) >> BASEB);
	if (n) {
		addval[1] = (HALF) n;
		addnum.num.len = 2;
	}
	if (neg)
		return qsub(q1, &addnum);
	else
		return qadd(q1, &addnum);
}


/*
 * Multiply two numbers.
 *	q3 = qmul(q1, q2);
 */
NUMBER *
qmul(q1, q2)
	register NUMBER *q1, *q2;
{
	NUMBER *r;			/* returned value */
	ZVALUE n1, n2, d1, d2;		/* numerators and denominators */
	ZVALUE tmp;

	if (qiszero(q1) || qiszero(q2))
		return qlink(&_qzero_);
	if (qisone(q1))
		return qlink(q2);
	if (qisone(q2))
		return qlink(q1);
	if (qisint(q1) && qisint(q2)) {	/* easy results if integers */
		r = qalloc();
		zmul(q1->num, q2->num, &r->num);
		return r;
	}
	n1 = q1->num;
	n2 = q2->num;
	d1 = q1->den;
	d2 = q2->den;
	if (ziszero(d1) || ziszero(d2))
		math_error("Division by zero");
	if (ziszero(n1) || ziszero(n2))
		return qlink(&_qzero_);
	if (!zisunit(n1) && !zisunit(d2)) {	/* possibly reduce */
		zgcd(n1, d2, &tmp);
		if (!zisunit(tmp)) {
			zquo(q1->num, tmp, &n1);
			zquo(q2->den, tmp, &d2);
		}
		zfree(tmp);
	}
	if (!zisunit(n2) && !zisunit(d1)) {	/* again possibly reduce */
		zgcd(n2, d1, &tmp);
		if (!zisunit(tmp)) {
			zquo(q2->num, tmp, &n2);
			zquo(q1->den, tmp, &d1);
		}
		zfree(tmp);
	}
	r = qalloc();
	zmul(n1, n2, &r->num);
	zmul(d1, d2, &r->den);
	if (q1->num.v != n1.v)
		zfree(n1);
	if (q1->den.v != d1.v)
		zfree(d1);
	if (q2->num.v != n2.v)
		zfree(n2);
	if (q2->den.v != d2.v)
		zfree(d2);
	return r;
}


/*
 * Multiply a number by a small integer.
 *	q2 = qmuli(q1, n);
 */
NUMBER *
qmuli(q, n)
	NUMBER *q;
	long n;
{
	NUMBER *r;
	long d;			/* gcd of multiplier and denominator */
	int sign;

	if ((n == 0) || qiszero(q))
		return qlink(&_qzero_);
	if (n == 1)
		return qlink(q);
	r = qalloc();
	if (qisint(q)) {
		zmuli(q->num, n, &r->num);
		return r;
	}
	sign = 1;
	if (n < 0) {
		n = -n;
		sign = -1;
	}
	d = zmodi(q->den, n);
	d = iigcd(d, n);
	zmuli(q->num, (n * sign) / d, &r->num);
	(void) zdivi(q->den, d, &r->den);
	return r;
}


/*
 * Divide two numbers (as fractions).
 *	q3 = qdiv(q1, q2);
 */
NUMBER *
qdiv(q1, q2)
	register NUMBER *q1, *q2;
{
	NUMBER temp;

	if (qiszero(q2))
		math_error("Division by zero");
	if ((q1 == q2) || !qcmp(q1, q2))
		return qlink(&_qone_);
	if (qisone(q1))
		return qinv(q2);
	temp.num = q2->den;
	temp.den = q2->num;
	temp.num.sign = temp.den.sign;
	temp.den.sign = 0;
	temp.links = 1;
	return qmul(q1, &temp);
}


/*
 * Divide a number by a small integer.
 *	q2 = qdivi(q1, n);
 */
NUMBER *
qdivi(q, n)
	NUMBER *q;
	long n;
{
	NUMBER *r;
	long d;			/* gcd of divisor and numerator */
	int sign;

	if (n == 0)
		math_error("Division by zero");
	if ((n == 1) || qiszero(q))
		return qlink(q);
	sign = 1;
	if (n < 0) {
		n = -n;
		sign = -1;
	}
	r = qalloc();
	d = zmodi(q->num, n);
	d = iigcd(d, n);
	(void) zdivi(q->num, d * sign, &r->num);
	zmuli(q->den, n / d, &r->den);
	return r;
}


/*
 * Return the quotient when one number is divided by another.
 * This works for fractional values also, and in all cases:
 *	qquo(q1, q2) = int(q1 / q2).
 */
NUMBER *
qquo(q1, q2)
	register NUMBER *q1, *q2;
{
	ZVALUE num, den, res;
	NUMBER *q;

	if (zisunit(q1->num))
		num = q2->den;
	else if (zisunit(q2->den))
		num = q1->num;
	else
		zmul(q1->num, q2->den, &num);
	if (zisunit(q1->den))
		den = q2->num;
	else if (zisunit(q2->num))
		den = q1->den;
	else
		zmul(q1->den, q2->num, &den);
	zquo(num, den, &res);
	if ((num.v != q2->den.v) && (num.v != q1->num.v))
		zfree(num);
	if ((den.v != q2->num.v) && (den.v != q1->den.v))
		zfree(den);
	if (ziszero(res)) {
		zfree(res);
		return qlink(&_qzero_);
	}
	res.sign = (q1->num.sign != q2->num.sign);
	if (zisunit(res)) {
		q = (res.sign ? &_qnegone_ : &_qone_);
		zfree(res);
		return qlink(q);
	}
	q = qalloc();
	q->num = res;
	return q;
}


/*
 * Return the absolute value of a number.
 *	q2 = qabs(q1);
 */
NUMBER *
qabs(q)
	register NUMBER *q;
{
	register NUMBER *r;

	if (q->num.sign == 0)
		return qlink(q);
	r = qalloc();
	if (!zisunit(q->num))
		zcopy(q->num, &r->num);
	if (!zisunit(q->den))
		zcopy(q->den, &r->den);
	r->num.sign = 0;
	return r;
}


/*
 * Negate a number.
 *	q2 = qneg(q1);
 */
NUMBER *
qneg(q)
	register NUMBER *q;
{
	register NUMBER *r;

	if (qiszero(q))
		return qlink(&_qzero_);
	r = qalloc();
	if (!zisunit(q->num))
		zcopy(q->num, &r->num);
	if (!zisunit(q->den))
		zcopy(q->den, &r->den);
	r->num.sign = !q->num.sign;
	return r;
}


/*
 * Return the sign of a number (-1, 0 or 1)
 */
NUMBER *
qsign(q)
	NUMBER *q;
{
	if (qiszero(q))
		return qlink(&_qzero_);
	if (qisneg(q))
		return qlink(&_qnegone_);
	return qlink(&_qone_);
}


/*
 * Invert a number.
 *	q2 = qinv(q1);
 */
NUMBER *
qinv(q)
	register NUMBER *q;
{
	register NUMBER *r;

	if (qisunit(q)) {
		r = (qisneg(q) ? &_qnegone_ : &_qone_);
		return qlink(r);
	}
	if (qiszero(q))
		math_error("Division by zero");
	r = qalloc();
	if (!zisunit(q->num))
		zcopy(q->num, &r->den);
	if (!zisunit(q->den))
		zcopy(q->den, &r->num);
	r->num.sign = q->num.sign;
	r->den.sign = 0;
	return r;
}


/*
 * Return just the numerator of a number.
 *	q2 = qnum(q1);
 */
NUMBER *
qnum(q)
	register NUMBER *q;
{
	register NUMBER *r;

	if (qisint(q))
		return qlink(q);
	if (zisunit(q->num)) {
		r = (qisneg(q) ? &_qnegone_ : &_qone_);
		return qlink(r);
	}
	r = qalloc();
	zcopy(q->num, &r->num);
	return r;
}


/*
 * Return just the denominator of a number.
 *	q2 = qden(q1);
 */
NUMBER *
qden(q)
	register NUMBER *q;
{
	register NUMBER *r;

	if (qisint(q))
		return qlink(&_qone_);
	r = qalloc();
	zcopy(q->den, &r->num);
	return r;
}


/*
 * Return the fractional part of a number.
 *	q2 = qfrac(q1);
 */
NUMBER *
qfrac(q)
	register NUMBER *q;
{
	register NUMBER *r;
	ZVALUE z;

	if (qisint(q))
		return qlink(&_qzero_);
	if ((q->num.len < q->den.len) || ((q->num.len == q->den.len) &&
		(q->num.v[q->num.len - 1] < q->den.v[q->num.len - 1])))
			return qlink(q);
	r = qalloc();
	if (qisneg(q)) {
		zmod(q->num, q->den, &z);
		zsub(q->den, z, &r->num);
		zfree(z);
	} else {
		zmod(q->num, q->den, &r->num);
	}
	zcopy(q->den, &r->den);
	r->num.sign = q->num.sign;
	return r;
}


/*
 * Return the integral part of a number.
 *	q2 = qint(q1);
 */
NUMBER *
qint(q)
	register NUMBER *q;
{
	register NUMBER *r;

	if (qisint(q))
		return qlink(q);
	if ((q->num.len < q->den.len) || ((q->num.len == q->den.len) &&
		(q->num.v[q->num.len - 1] < q->den.v[q->num.len - 1])))
			return qlink(&_qzero_);
	r = qalloc();
	zquo(q->num, q->den, &r->num);
	return r;
}


/*
 * Compute the square of a number.
 */
NUMBER *
qsquare(q)
	register NUMBER *q;
{
	ZVALUE num, den;

	if (qiszero(q))
		return qlink(&_qzero_);
	if (qisunit(q))
		return qlink(&_qone_);
	num = q->num;
	den = q->den;
	q = qalloc();
	if (!zisunit(num))
		zsquare(num, &q->num);
	if (!zisunit(den))
		zsquare(den, &q->den);
	return q;
}


/*
 * Shift an integer by a given number of bits. This multiplies the number
 * by the appropriate power of two.  Positive numbers shift left, negative
 * ones shift right.  Low bits are truncated when shifting right.
 */
NUMBER *
qshift(q, n)
	NUMBER *q;
	long n;
{
	register NUMBER *r;

	if (qisfrac(q))
		math_error("Shift of non-integer");
	if (qiszero(q) || (n == 0))
		return qlink(q);
	if (n <= -(q->num.len * BASEB))
		return qlink(&_qzero_);
	r = qalloc();
	zshift(q->num, n, &r->num);
	return r;
}


/*
 * Scale a number by a power of two, as in:
 *	ans = q * 2^n.
 * This is similar to shifting, except that fractions work.
 */
NUMBER *
qscale(q, pow)
	NUMBER *q;
	long pow;
{
	long numshift, denshift, tmp;
	NUMBER *r;

	if (qiszero(q) || (pow == 0))
		return qlink(q);
	if ((pow > 1000000L) || (pow < -1000000L))
		math_error("Very large scale value");
	numshift = zisodd(q->num) ? 0 : zlowbit(q->num);
	denshift = zisodd(q->den) ? 0 : zlowbit(q->den);
	if (pow > 0) {
		tmp = pow;
		if (tmp > denshift)
		tmp = denshift;
		denshift = -tmp;
		numshift = (pow - tmp);
	} else {
		pow = -pow;
		tmp = pow;
		if (tmp > numshift)
			tmp = numshift;
		numshift = -tmp;
		denshift = (pow - tmp);
	}
	r = qalloc();
	if (numshift)
		zshift(q->num, numshift, &r->num);
	else
		zcopy(q->num, &r->num);
	if (denshift)
		zshift(q->den, denshift, &r->den);
	else
		zcopy(q->den, &r->den);
	return r;
}


/*
 * Return the minimum of two numbers.
 */
NUMBER *
qmin(q1, q2)
	NUMBER *q1, *q2;
{
	if (q1 == q2)
		return qlink(q1);
	if (qrel(q1, q2) > 0)
		q1 = q2;
	return qlink(q1);
}


/*
 * Return the maximum of two numbers.
 */
NUMBER *
qmax(q1, q2)
	NUMBER *q1, *q2;
{
	if (q1 == q2)
		return qlink(q1);
	if (qrel(q1, q2) < 0)
		q1 = q2;
	return qlink(q1);
}


/*
 * Perform the logical OR of two integers.
 */
NUMBER *
qor(q1, q2)
	NUMBER *q1, *q2;
{
	register NUMBER *r;

	if (qisfrac(q1) || qisfrac(q2))
		math_error("Non-integers for logical or");
	if ((q1 == q2) || qiszero(q2))
		return qlink(q1);
	if (qiszero(q1))
		return qlink(q2);
	r = qalloc();
	zor(q1->num, q2->num, &r->num);
	return r;
}


/*
 * Perform the logical AND of two integers.
 */
NUMBER *
qand(q1, q2)
	NUMBER *q1, *q2;
{
	register NUMBER *r;
	ZVALUE res;

	if (qisfrac(q1) || qisfrac(q2))
		math_error("Non-integers for logical and");
	if (q1 == q2)
		return qlink(q1);
	if (qiszero(q1) || qiszero(q2))
		return qlink(&_qzero_);
	zand(q1->num, q2->num, &res);
	if (ziszero(res)) {
		zfree(res);
		return qlink(&_qzero_);
	}
	r = qalloc();
	r->num = res;
	return r;
}


/*
 * Perform the logical XOR of two integers.
 */
NUMBER *
qxor(q1, q2)
	NUMBER *q1, *q2;
{
	register NUMBER *r;
	ZVALUE res;

	if (qisfrac(q1) || qisfrac(q2))
		math_error("Non-integers for logical xor");
	if (q1 == q2)
		return qlink(&_qzero_);
	if (qiszero(q1))
		return qlink(q2);
	if (qiszero(q2))
		return qlink(q1);
	zxor(q1->num, q2->num, &res);
	if (ziszero(res)) {
		zfree(res);
		return qlink(&_qzero_);
	}
	r = qalloc();
	r->num = res;
	return r;
}


#if 0
/*
 * Return the number whose binary representation only has the specified
 * bit set (counting from zero).  This thus produces a given power of two.
 */
NUMBER *
qbitvalue(n)
	long n;
{
	register NUMBER *r;

	if (n <= 0)
		return qlink(&_qone_);
	r = qalloc();
	zbitvalue(n, &r->num);
	return r;
}


/*
 * Test to see if the specified bit of a number is on (counted from zero).
 * Returns TRUE if the bit is set, or FALSE if it is not.
 *	i = qbittest(q, n);
 */
BOOL
qbittest(q, n)
	register NUMBER *q;
	long n;
{
	int x, y;

	if ((n < 0) || (n >= (q->num.len * BASEB)))
		return FALSE;
	x = q->num.v[n / BASEB];
	y = (1 << (n % BASEB));
	return ((x & y) != 0);
}
#endif


/*
 * Return the precision of a number (usually for examining an epsilon value).
 * This is the largest power of two whose reciprocal is not smaller in absolute
 * value than the specified number.  For example, qbitprec(1/100) = 6.
 * Numbers larger than one have a precision of zero.
 */
long
qprecision(q)
	NUMBER *q;
{
	long r;

	if (qisint(q))
		return 0;
	if (zisunit(q->num))
		return zhighbit(q->den);
	r = zhighbit(q->den) - zhighbit(q->num) - 1;
	if (r < 0)
		r = 0;
	return r;
}


#if 0
/*
 * Return an integer indicating the sign of a number (-1, 0, or 1).
 *	i = qtst(q);
 */
FLAG
qtest(q)
	register NUMBER *q;
{
	if (!ztest(q->num))
		return 0;
	if (q->num.sign)
		return -1;
	return 1;
}
#endif


/*
 * Determine whether or not one number exactly divides another one.
 * Returns TRUE if the first number is an integer multiple of the second one.
 */
BOOL
qdivides(q1, q2)
	NUMBER *q1, *q2;
{
	if (qiszero(q1))
		return TRUE;
	if (qisint(q1) && qisint(q2)) {
		if (qisunit(q2))
			return TRUE;
		return zdivides(q1->num, q2->num);
	}
	return zdivides(q1->num, q2->num) && zdivides(q2->den, q1->den);
}


/*
 * Compare two numbers and return an integer indicating their relative size.
 *	i = qrel(q1, q2);
 */
FLAG
qrel(q1, q2)
	register NUMBER *q1, *q2;
{
	ZVALUE z1, z2;
	long wc1, wc2;
	int sign;
	int z1f = 0, z2f = 0;

	if (q1 == q2)
		return 0;
	sign = q2->num.sign - q1->num.sign;
	if (sign)
		return sign;
	if (qiszero(q2))
		return !qiszero(q1);
	if (qiszero(q1))
		return -1;
	/*
	 * Make a quick comparison by calculating the number of words resulting as
	 * if we multiplied through by the denominators, and then comparing the
	 * word counts.
	 */
	sign = 1;
	if (qisneg(q1))
		sign = -1;
	wc1 = q1->num.len + q2->den.len;
	wc2 = q2->num.len + q1->den.len;
	if (wc1 < wc2 - 1)
		return -sign;
	if (wc2 < wc1 - 1)
		return sign;
	/*
	 * Quick check failed, must actually do the full comparison.
	 */
	if (zisunit(q2->den))
		z1 = q1->num;
	else if (zisone(q1->num))
		z1 = q2->den;
	else {
		z1f = 1;
		zmul(q1->num, q2->den, &z1);
	}
	if (zisunit(q1->den))
		z2 = q2->num;
	else if (zisone(q2->num))
		z2 = q1->den;
	else {
		z2f = 1;
		zmul(q2->num, q1->den, &z2);
	}
	sign = zrel(z1, z2);
	if (z1f)
		zfree(z1);
	if (z2f)
		zfree(z2);
	return sign;
}


/*
 * Compare two numbers to see if they are equal.
 * This differs from qrel in that the numbers are not ordered.
 * Returns TRUE if they differ.
 */
BOOL
qcmp(q1, q2)
	register NUMBER *q1, *q2;
{
	if (q1 == q2)
		return FALSE;
	if ((q1->num.sign != q2->num.sign) || (q1->num.len != q2->num.len) ||
		(q2->den.len != q2->den.len) || (*q1->num.v != *q2->num.v) ||
		(*q1->den.v != *q2->den.v))
			return TRUE;
	if (zcmp(q1->num, q2->num))
		return TRUE;
	if (qisint(q1))
		return FALSE;
	return zcmp(q1->den, q2->den);
}


/*
 * Compare a number against a normal small integer.
 * Returns 1, 0, or -1, according to whether the first number is greater,
 * equal, or less than the second number.
 *	n = qreli(q, n);
 */
FLAG
qreli(q, n)
	NUMBER *q;
	long n;
{
	int sign;
	ZVALUE num;
	HALF h2[2];
	NUMBER q2;

	sign = ztest(q->num);		/* do trivial sign checks */
	if (sign == 0) {
		if (n > 0)
			return -1;
		return (n < 0);
	}
	if ((sign < 0) && (n >= 0))
		return -1;
	if ((sign > 0) && (n <= 0))
		return 1;
	n *= sign;
	if (n == 1) {			/* quick check against 1 or -1 */
		num = q->num;
		num.sign = 0;
		return (sign * zrel(num, q->den));
	}
	num.sign = (sign < 0);
	num.len = 1 + (n >= BASE);
	num.v = h2;
	h2[0] = (n & BASE1);
	h2[1] = (n >> BASEB);
	if (zisunit(q->den))	/* integer compare if no denominator */
		return zrel(q->num, num);
	q2.num = num;
	q2.den = _one_;
	q2.links = 1;
	return qrel(q, &q2);	/* full fractional compare */
}


/*
 * Compare a number against a small integer to see if they are equal.
 * Returns TRUE if they differ.
 */
BOOL
qcmpi(q, n)
	NUMBER *q;
	long n;
{
	long len;

	len = q->num.len;
	if ((len > 2) || qisfrac(q) || (q->num.sign != (n < 0)))
		return TRUE;
	if (n < 0)
		n = -n;
	if (((HALF)(n)) != q->num.v[0])
		return TRUE;
	n = ((FULL) n) >> BASEB;
	return (((n != 0) != (len == 2)) || (n != q->num.v[1]));
}


/*
 * Number node allocation routines
 */

#define	NNALLOC	1000

union allocNode {
	NUMBER	num;
	union allocNode	*link;
};

static union allocNode	*freeNum;


NUMBER *
qalloc()
{
	register union allocNode *temp;

	if (freeNum == NULL) {
		freeNum = (union allocNode *)
		malloc(sizeof (NUMBER) * NNALLOC);
		if (freeNum == NULL)
			math_error("Not enough memory");
		freeNum[NNALLOC-1].link = NULL;
		for (temp=freeNum+NNALLOC-2; temp >= freeNum; --temp) {
			temp->link = temp+1;
		}
	}
	temp = freeNum;
	freeNum = temp->link;
	temp->num.links = 1;
	temp->num.num = _one_;
	temp->num.den = _one_;
	return &temp->num;
}


void
qfreenum(q)
	register NUMBER *q;
{
	union allocNode *a;

	if (q == NULL)
		return;
	zfree(q->num);
	zfree(q->den);
	a = (union allocNode *) q;
	a->link = freeNum;
	freeNum = a;
}

/* END CODE */
