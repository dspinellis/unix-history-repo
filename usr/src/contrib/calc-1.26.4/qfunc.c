/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Extended precision rational arithmetic non-primitive functions
 */

#include "math.h"


NUMBER *_epsilon_;	/* default precision for real functions */
long _epsilonprec_;	/* bits of precision for epsilon */

#if 0
static char *abortmsg = "Calculation aborted";
static char *memmsg = "Not enough memory";
#endif


/*
 * Set the default precision for real calculations.
 * The precision must be between zero and one.
 */
void
setepsilon(q)
	NUMBER *q;		/* number to be set as the new epsilon */
{
	NUMBER *old;

	if (qisneg(q) || qiszero(q) || (qreli(q, 1L) >= 0))
		error("Epsilon value must be between zero and one");
	old = _epsilon_;
	_epsilonprec_ = qprecision(q);
	_epsilon_ = qlink(q);
	if (old)
		qfree(old);
}


/*
 * Return the inverse of one number modulo another.
 * That is, find x such that:
 *	Ax = 1 (mod B)
 * Returns zero if the numbers are not relatively prime (temporary hack).
 */
NUMBER *
qminv(q1, q2)
	NUMBER *q1, *q2;
{
	NUMBER *r;

	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integers for minv");
	r = qalloc();
	if (zmodinv(q1->num, q2->num, &r->num)) {
		qfree(r);
		return qlink(&_qzero_);
	}
	return r;
}


/*
 * Return the modulo of one number raised to another.
 * Here q1 is the number to be raised, q2 is the power to raise it to,
 * and q3 is the number to take the modulo with the result.
 * The second and third numbers are assumed nonnegative.
 * Returned answer is non-negative.
 *	q4 = qpowermod(q1, q2, q3);
 */
NUMBER *
qpowermod(q1, q2, q3)
	NUMBER *q1, *q2, *q3;
{
	NUMBER *r;

	if (qisfrac(q1) || qisfrac(q2) || qisfrac(q3))
		error("Non-integers for powermod");
	r = qalloc();
	zpowermod(q1->num, q2->num, q3->num, &r->num);
	return r;
}


/*
 * Return the power function of one number with another.
 * The power must be integral.
 *	q3 = qpowi(q1, q2);
 */
NUMBER *
qpowi(q1, q2)
	NUMBER *q1, *q2;
{
	register NUMBER *r;
	BOOL invert, sign;
	ZVALUE num, den, z2;

	if (qisfrac(q2))
		error("Raising number to fractional power");
	num = q1->num;
	den = q1->den;
	z2 = q2->num;
	sign = (num.sign && isodd(z2));
	invert = z2.sign;
	num.sign = 0;
	z2.sign = 0;
	/*
	* Check for trivial cases first.
	*/
	if (iszero(num)) {	/* zero raised to a power */
		if (invert || iszero(z2))
			error("Zero raised to non-positive power");
		return qlink(&_qzero_);
	}
	if (isunit(num) && isunit(den)) {	/* 1 or -1 raised to a power */
		r = (sign ? q1 : &_qone_);
		r->links++;
		return r;
	}
	if (iszero(z2))	/* raising to zeroth power */
		return qlink(&_qone_);
	if (isunit(z2)) {	/* raising to power 1 or -1 */
		if (invert)
			return qinv(q1);
		return qlink(q1);
	}
	/*
	 * Not a trivial case.  Do the real work.
	 */
	r = qalloc();
	if (!isunit(num))
		zpowi(num, z2, &r->num);
	if (!isunit(den))
		zpowi(den, z2, &r->den);
	if (invert) {
		z2 = r->num;
		r->num = r->den;
		r->den = z2;
	}
	r->num.sign = sign;
	return r;
}


/*
 * Given the legs of a right triangle, compute its hypothenuse within
 * the specified error.  This is sqrt(a^2 + b^2).
 */
NUMBER *
qhypot(q1, q2, epsilon)
	NUMBER *q1, *q2, *epsilon;
{
	NUMBER *tmp1, *tmp2, *tmp3;

	if (qisneg(epsilon) || qiszero(epsilon))
		error("Bad epsilon value for hypot");
	if (qiszero(q1))
		return qabs(q2);
	if (qiszero(q2))
		return qabs(q1);
	tmp1 = qsquare(q1);
	tmp2 = qsquare(q2);
	tmp3 = qadd(tmp1, tmp2);
	qfree(tmp1);
	qfree(tmp2);
	tmp1 = qsqrt(tmp3, epsilon);
	qfree(tmp3);
	return tmp1;
}


/*
 * Given one leg of a right triangle with unit hypothenuse, calculate
 * the other leg within the specified error.  This is sqrt(1 - a^2).
 * If the wantneg flag is nonzero, then negative square root is returned.
 */
NUMBER *
qlegtoleg(q, epsilon, wantneg)
	NUMBER *q, *epsilon;
	BOOL wantneg;
{
	NUMBER *qt, *res, qtmp;
	ZVALUE num, ztmp1, ztmp2;

	if (qisneg(epsilon) || qiszero(epsilon))
		error("Bad epsilon value for legtoleg");
	if (qisunit(q))
		return qlink(&_qzero_);
	if (qiszero(q)) {
		if (wantneg)
			return qlink(&_qnegone_);
		return qlink(&_qone_);
	}
	num = q->num;
	num.sign = 0;
	if (zrel(num, q->den) >= 0)
		error("Leg too large in legtoleg");
	zsquare(q->den, &ztmp1);
	zsquare(num, &ztmp2);
	zsub(ztmp1, ztmp2, &qtmp.num);
	freeh(ztmp1.v);
	freeh(ztmp2.v);
	qtmp.den = _one_;
	qt = qsqrt(&qtmp, epsilon);
	freeh(qtmp.num.v);
	qtmp.num = q->den;
	res = qdiv(qt, &qtmp);
	qfree(qt);
	qt = qbappr(res, epsilon);
	qfree(res);
	if (wantneg) {
		res = qneg(qt);
		qfree(qt);
		qt = res;
	}
	return qt;
}


/*
 * Compute the square root of a number to within the specified error.
 * If the number is an exact square, the exact result is returned.
 *	q3 = qsqrt(q1, q2);
 */
NUMBER *
qsqrt(q1, epsilon)
	register NUMBER *q1, *epsilon;
{
	long bits, bits2;	/* number of bits of precision */
	int exact;
	NUMBER *r;
	ZVALUE t1, t2;

	if (qisneg(q1))
		error("Square root of negative number");
	if (qisneg(epsilon) || qiszero(epsilon))
		error("Bad epsilon value for sqrt");
	if (qiszero(q1))
		return qlink(&_qzero_);
	if (qisunit(q1))
		return qlink(&_qone_);
	if (qiszero(epsilon) && qisint(q1) && istiny(q1->num) && (*q1->num.v <= 3))
		return qlink(&_qone_);
	bits = zhighbit(epsilon->den) - zhighbit(epsilon->num) + 1;
	if (bits < 0)
		bits = 0;
	bits2 = zhighbit(q1->num) - zhighbit(q1->den) + 1;
	if (bits2 > 0)
		bits += bits2;
	r = qalloc();
	zshift(q1->num, bits * 2, &t2);
	zmul(q1->den, t2, &t1);
	freeh(t2.v);
	exact = zsqrt(t1, &t2);
	freeh(t1.v);
	if (exact) {
		zshift(q1->den, bits, &t1);
		zreduce(t2, t1, &r->num, &r->den);
	} else {
		zquo(t2, q1->den, &t1);
		freeh(t2.v);
		zbitvalue(bits, &t2);
		zreduce(t1, t2, &r->num, &r->den);
	}
	freeh(t1.v);
	freeh(t2.v);
	if (qiszero(r)) {
		qfree(r);
		r = qlink(&_qzero_);
	}
	return r;
}


/*
 * Calculate the integral part of the square root of a number.
 * Example:  qisqrt(13) = 3.
 */
NUMBER *
qisqrt(q)
	NUMBER *q;
{
	NUMBER *r;
	ZVALUE tmp;

	if (qisneg(q))
		error("Square root of negative number");
	if (qiszero(q))
		return qlink(&_qzero_);
	if (qisint(q) && istiny(q->num) && (z1tol(q->num) < 4))
		return qlink(&_qone_);
	r = qalloc();
	if (qisint(q)) {
		(void) zsqrt(q->num, &r->num);
		return r;
	}
	zquo(q->num, q->den, &tmp);
	(void) zsqrt(tmp, &r->num);
	freeh(tmp.v);
	return r;
}


/*
 * Return whether or not a number is an exact square.
 */
BOOL
qissquare(q)
	NUMBER *q;
{
	BOOL flag;

	flag = zissquare(q->num);
	if (qisint(q) || !flag)
		return flag;
	return zissquare(q->den);
}


/*
 * Compute the greatest integer of the Kth root of a number.
 * Example:  qiroot(85, 3) = 4.
 */
NUMBER *
qiroot(q1, q2)
	register NUMBER *q1, *q2;
{
	NUMBER *r;
	ZVALUE tmp;

	if (qisneg(q2) || qiszero(q2) || qisfrac(q2))
		error("Taking number to bad root value");
	if (qiszero(q1))
		return qlink(&_qzero_);
	if (qisone(q1) || qisone(q2))
		return qlink(q1);
	if (qistwo(q2))
		return qisqrt(q1);
	r = qalloc();
	if (qisint(q1)) {
		zroot(q1->num, q2->num, &r->num);
		return r;
	}
	zquo(q1->num, q1->den, &tmp);
	zroot(tmp, q2->num, &r->num);
	freeh(tmp.v);
	return r;
}


/*
 * Return the greatest integer of the base 2 log of a number.
 * This is the number such that  1 <= x / log2(x) < 2.
 * Examples:  qilog2(8) = 3, qilog2(1.3) = 1, qilog2(1/7) = -3.
 */
long
qilog2(q)
	NUMBER *q;		/* number to take log of */
{
	long n;			/* power of two */
	int c;			/* result of comparison */
	ZVALUE tmp;		/* temporary value */

	if (qisneg(q) || qiszero(q))
		error("Non-positive number for log2");
	if (qisint(q))
		return zhighbit(q->num);
	n = zhighbit(q->num) - zhighbit(q->den);
	if (n == 0)
		c = zrel(q->num, q->den);
	else if (n > 0) {
		zshift(q->den, n, &tmp);
		c = zrel(q->num, tmp);
	} else {
		zshift(q->num, n, &tmp);
		c = zrel(tmp, q->den);
	}
	if (n)
		freeh(tmp.v);
	if (c < 0)
		n--;
	return n;
}


/*
 * Return the greatest integer of the base 10 log of a number.
 * This is the number such that  1 <= x / log10(x) < 10.
 * Examples:  qilog10(100) = 2, qilog10(12.3) = 1, qilog10(.023) = -2.
 */
long
qilog10(q)
	NUMBER *q;		/* number to take log of */
{
	long n;			/* log value */
	ZVALUE temp;		/* temporary value */

	if (qisneg(q) || qiszero(q))
		error("Non-positive number for log10");
	if (qisint(q))
		return zlog10(q->num);
	/*
	 * The number is not an integer.
	 * Compute the result if the number is greater than one.
	 */
	if ((q->num.len > q->den.len) ||
		((q->num.len == q->den.len) && (zrel(q->num, q->den) > 0))) {
			zquo(q->num, q->den, &temp);
			n = zlog10(temp);
			freeh(temp.v);
			return n;
	}
	/*
	 * Here if the number is less than one.
	 * If the number is the inverse of a power of ten, then the obvious answer
	 * will be off by one.  Subtracting one if the number is the inverse of an
	 * integer will fix it.
	 */
	if (isunit(q->num))
		zsub(q->den, _one_, &temp);
	else
		zquo(q->den, q->num, &temp);
	n = -zlog10(temp) - 1;
	freeh(temp.v);
	return n;
}


/*
 * Return the number of digits in a number, ignoring the sign.
 * For fractions, this is the number of digits of its greatest integer.
 * Examples: qdigits(3456) = 4, qdigits(-23.45) = 2, qdigits(.0120) = 1.
 */
long
qdigits(q)
	NUMBER *q;		/* number to count digits of */
{
	long n;			/* number of digits */
	ZVALUE temp;		/* temporary value */

	if (qisint(q))
		return zdigits(q->num);
	zquo(q->num, q->den, &temp);
	n = zdigits(temp);
	freeh(temp.v);
	return n;
}


/*
 * Return the digit at the specified decimal place of a number represented
 * in floating point.  The lowest digit of the integral part of a number
 * is the zeroth decimal place.  Negative decimal places indicate digits
 * to the right of the decimal point.  Examples: qdigit(1234.5678, 1) = 3,
 * qdigit(1234.5678, -3) = 7.
 */
FLAG
qdigit(q, n)
	NUMBER *q;
	long n;
{
	ZVALUE tenpow, tmp1, tmp2;
	FLAG res;

	/*
	 * Zero number or negative decimal place of integer is trivial.
	 */
	if (qiszero(q) || (qisint(q) && (n < 0)))
		return 0;
	/*
	 * For non-negative decimal places, answer is easy.
	 */
	if (n >= 0) {
		if (qisint(q))
			return zdigit(q->num, n);
		zquo(q->num, q->den, &tmp1);
		res = zdigit(tmp1, n);
		freeh(tmp1.v);
		return res;
	}
	/*
	 * Fractional value and want negative digit, must work harder.
	 */
	ztenpow(-n, &tenpow);
	zmul(q->num, tenpow, &tmp1);
	freeh(tenpow.v);
	zquo(tmp1, q->den, &tmp2);
	res = zmodi(tmp2, 10L);
	freeh(tmp1.v);
	freeh(tmp2.v);
	return res;
}


/*
 * Return whether or not a bit is set at the specified bit position in a
 * number.  The lowest bit of the integral part of a number is the zeroth
 * bit position.  Negative bit positions indicate bits to the right of the
 * binary decimal point.  Examples: qdigit(17.1, 0) = 1, qdigit(17.1, -1) = 0.
 */
BOOL
qisset(q, n)
	NUMBER *q;
	long n;
{
	NUMBER *qtmp1, *qtmp2;
	ZVALUE ztmp;
	BOOL res;

	/*
	 * Zero number or negative bit position place of integer is trivial.
	 */
	if (qiszero(q) || (qisint(q) && (n < 0)))
		return FALSE;
	/*
	 * For non-negative bit positions, answer is easy.
	 */
	if (n >= 0) {
		if (qisint(q))
			return zisset(q->num, n);
		zquo(q->num, q->den, &ztmp);
		res = zisset(ztmp, n);
		freeh(ztmp.v);
		return res;
	}
	/*
	 * Fractional value and want negative bit position, must work harder.
	 */
	qtmp1 = qscale(q, -n);
	qtmp2 = qint(qtmp1);
	qfree(qtmp1);
	res = ((qtmp2->num.v[0] & 0x01) != 0);
	qfree(qtmp2);
	return res;
}


/*
 * Compute the factorial of an integer.
 *	q2 = qfact(q1);
 */
NUMBER *
qfact(q)
	register NUMBER *q;
{
	register NUMBER *r;

	if (qisfrac(q))
		error("Non-integral factorial");
	if (qiszero(q) || isone(q->num))
		return qlink(&_qone_);
	r = qalloc();
	zfact(q->num, &r->num);
	return r;
}


/*
 * Compute the product of the primes less than or equal to a number.
 *	q2 = qpfact(q1);
 */
NUMBER *
qpfact(q)
	register NUMBER *q;
{
	NUMBER *r;

	if (qisfrac(q))
		error("Non-integral factorial");
	r = qalloc();
	zpfact(q->num, &r->num);
	return r;
}


/*
 * Compute the lcm of all the numbers less than or equal to a number.
 *	q2 = qlcmfact(q1);
 */
NUMBER *
qlcmfact(q)
	register NUMBER *q;
{
	NUMBER *r;

	if (qisfrac(q))
		error("Non-integral lcmfact");
	r = qalloc();
	zlcmfact(q->num, &r->num);
	return r;
}


/*
 * Compute the permutation function  M! / (M - N)!.
 */
NUMBER *
qperm(q1, q2)
	register NUMBER *q1, *q2;
{
	register NUMBER *r;

	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integral arguments for permutation");
	r = qalloc();
	zperm(q1->num, q2->num, &r->num);
	return r;
}


/*
 * Compute the combinatorial function  M! / (N! * (M - N)!).
 */
NUMBER *
qcomb(q1, q2)
	register NUMBER *q1, *q2;
{
	register NUMBER *r;

	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integral arguments for combinatorial");
	r = qalloc();
	zcomb(q1->num, q2->num, &r->num);
	return r;
}


/*
 * Compute the Jacobi function (a / b).
 * -1 => a is not quadratic residue mod b
 * 1 => b is composite, or a is quad residue of b
 * 0 => b is even or b < 0
 */
NUMBER *
qjacobi(q1, q2)
	register NUMBER *q1, *q2;
{
	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integral arguments for jacobi");
	return itoq((long) zjacobi(q1->num, q2->num));
}


/*
 * Compute the Fibonacci number F(n).
 */
NUMBER *
qfib(q)
	register NUMBER *q;
{
	register NUMBER *r;

	if (qisfrac(q))
		error("Non-integral Fibonacci number");
	r = qalloc();
	zfib(q->num, &r->num);
	return r;
}


/*
 * Truncate a number to the specified number of decimal places.
 * Specifying zero places makes the result identical to qint.
 * Example: qtrunc(2/3, 3) = .666
 */
NUMBER *
qtrunc(q1, q2)
	NUMBER *q1, *q2;
{
	long places;
	NUMBER *r;
	ZVALUE tenpow, tmp1, tmp2;

	if (qisfrac(q2) || qisneg(q2) || !istiny(q2->num))
		error("Bad number of places for qtrunc");
	if (qisint(q1))
		return qlink(q1);
	r = qalloc();
	places = z1tol(q2->num);
	/*
	 * Ok, produce the result.
	 * First see if we want no places, in which case just take integer part.
	 */
	if (places == 0) {
		zquo(q1->num, q1->den, &r->num);
		return r;
	}
	ztenpow(places, &tenpow);
	zmul(q1->num, tenpow, &tmp1);
	zquo(tmp1, q1->den, &tmp2);
	freeh(tmp1.v);
	if (iszero(tmp2)) {
		freeh(tmp2.v);
		return qlink(&_qzero_);
	}
	/*
	 * Now reduce the result to the lowest common denominator.
	 */
	zgcd(tmp2, tenpow, &tmp1);
	if (isunit(tmp1)) {
		freeh(tmp1.v);
		r->num = tmp2;
		r->den = tenpow;
		return r;
	}
	zquo(tmp2, tmp1, &r->num);
	zquo(tenpow, tmp1, &r->den);
	freeh(tmp1.v);
	freeh(tmp2.v);
	freeh(tenpow.v);
	return r;
}


/*
 * Round a number to the specified number of decimal places.
 * Zero decimal places means round to the nearest integer.
 * Example: qround(2/3, 3) = .667
 */
NUMBER *
qround(q, places)
	NUMBER *q;		/* number to be rounded */
	long places;		/* number of decimal places to round to */
{
	NUMBER *r;
	ZVALUE tenpow, roundval, tmp1, tmp2;

	if (places < 0)
		error("Negative places for qround");
	if (qisint(q))
		return qlink(q);
	/*
	 * Calculate one half of the denominator, ignoring fractional results.
	 * This is the value we will add in order to cause rounding.
	 */
	zshift(q->den, -1L, &roundval);
	roundval.sign = q->num.sign;
	/*
	 * Ok, now do the actual work to produce the result.
	 */
	r = qalloc();
	ztenpow(places, &tenpow);
	zmul(q->num, tenpow, &tmp2);
	zadd(tmp2, roundval, &tmp1);
	freeh(tmp2.v);
	freeh(roundval.v);
	zquo(tmp1, q->den, &tmp2);
	freeh(tmp1.v);
	if (iszero(tmp2)) {
		freeh(tmp2.v);
		return qlink(&_qzero_);
	}
	/*
	 * Now reduce the result to the lowest common denominator.
	 */
	zgcd(tmp2, tenpow, &tmp1);
	if (isunit(tmp1)) {
		freeh(tmp1.v);
		r->num = tmp2;
		r->den = tenpow;
		return r;
	}
	zquo(tmp2, tmp1, &r->num);
	zquo(tenpow, tmp1, &r->den);
	freeh(tmp1.v);
	freeh(tmp2.v);
	freeh(tenpow.v);
	return r;
}


/*
 * Truncate a number to the specified number of binary places.
 * Specifying zero places makes the result identical to qint.
 */
NUMBER *
qbtrunc(q1, q2)
	NUMBER *q1, *q2;
{
	long places, twopow;
	NUMBER *r;
	ZVALUE tmp1, tmp2;

	if (qisfrac(q2) || qisneg(q2) || !istiny(q2->num))
		error("Bad number of places for qtrunc");
	if (qisint(q1))
		return qlink(q1);
	r = qalloc();
	places = z1tol(q2->num);
	/*
	 * Ok, produce the result.
	 * First see if we want no places, in which case just take integer part.
	 */
	if (places == 0) {
		zquo(q1->num, q1->den, &r->num);
		return r;
	}
	zshift(q1->num, places, &tmp1);
	zquo(tmp1, q1->den, &tmp2);
	freeh(tmp1.v);
	if (iszero(tmp2)) {
		freeh(tmp2.v);
		return qlink(&_qzero_);
	}
	/*
	 * Now reduce the result to the lowest common denominator.
	 */
	if (isodd(tmp2)) {
		r->num = tmp2;
		zbitvalue(places, &r->den);
		return r;
	}
	twopow = zlowbit(tmp2);
	if (twopow > places)
		twopow = places;
	places -= twopow;
	zshift(tmp2, -twopow, &r->num);
	freeh(tmp2.v);
	zbitvalue(places, &r->den);
	return r;
}


/*
 * Round a number to the specified number of binary places.
 * Zero binary places means round to the nearest integer.
 */
NUMBER *
qbround(q, places)
	NUMBER *q;		/* number to be rounded */
	long places;		/* number of binary places to round to */
{
	long twopow;
	NUMBER *r;
	ZVALUE roundval, tmp1, tmp2;

	if (places < 0)
		error("Negative places for qbround");
	if (qisint(q))
		return qlink(q);
	r = qalloc();
	/*
	 * Calculate one half of the denominator, ignoring fractional results.
	 * This is the value we will add in order to cause rounding.
	 */
	zshift(q->den, -1L, &roundval);
	roundval.sign = q->num.sign;
	/*
	 * Ok, produce the result.
	 */
	zshift(q->num, places, &tmp1);
	zadd(tmp1, roundval, &tmp2);
	freeh(roundval.v);
	freeh(tmp1.v);
	zquo(tmp2, q->den, &tmp1);
	freeh(tmp2.v);
	if (iszero(tmp1)) {
		freeh(tmp1.v);
		return qlink(&_qzero_);
	}
	/*
	 * Now reduce the result to the lowest common denominator.
	 */
	if (isodd(tmp1)) {
		r->num = tmp1;
		zbitvalue(places, &r->den);
		return r;
	}
	twopow = zlowbit(tmp1);
	if (twopow > places)
		twopow = places;
	places -= twopow;
	zshift(tmp1, -twopow, &r->num);
	freeh(tmp1.v);
	zbitvalue(places, &r->den);
	return r;
}


/*
 * Approximate a number by using binary rounding with the minimum number
 * of binary places so that the resulting number is within the specified
 * epsilon of the original number.
 */
NUMBER *
qbappr(q, e)
	NUMBER *q, *e;
{
	long bits;

	if (qisneg(e) || qiszero(e))
		error("Bad epsilon value for qbappr");
	if (e == _epsilon_)
		bits = _epsilonprec_ + 1;
	else
		bits = qprecision(e) + 1;
	return qbround(q, bits);
}


/*
 * Approximate a number using continued fractions until the approximation
 * error is less than the specified value.  If a NULL pointer is given
 * for the error value, then the closest simpler fraction is returned.
 */
NUMBER *
qcfappr(q, e)
	NUMBER *q, *e;
{
	NUMBER qtest, *qtmp;
	ZVALUE u1, u2, u3, v1, v2, v3, t1, t2, t3, qq, tt;
	int i;
	BOOL haveeps;

	haveeps = TRUE;
	if (e == NULL) {
		haveeps = FALSE;
		e = &_qzero_;
	}
	if (qisneg(e))
		error("Negative epsilon for cfappr");
	if (qisint(q) || isunit(q->num) || (haveeps && qiszero(e)))
		return qlink(q);
	u1 = _one_;
	u2 = _zero_;
	u3 = q->num;
	u3.sign = 0;
	v1 = _zero_;
	v2 = _one_;
	v3 = q->den;
	while (!iszero(v3)) {
		if (!iszero(u2) && !iszero(u1)) {
			qtest.num = u2;
			qtest.den = u1;
			qtest.den.sign = 0;
			qtest.num.sign = q->num.sign;
			qtmp = qsub(q, &qtest);
			qtest = *qtmp;
			qtest.num.sign = 0;
			i = qrel(&qtest, e);
			qfree(qtmp);
			if (i <= 0)
				break;
		}
		zquo(u3, v3, &qq);
		zmul(qq, v1, &tt); zsub(u1, tt, &t1); freeh(tt.v);
		zmul(qq, v2, &tt); zsub(u2, tt, &t2); freeh(tt.v);
		zmul(qq, v3, &tt); zsub(u3, tt, &t3); freeh(tt.v);
		freeh(qq.v); freeh(u1.v); freeh(u2.v);
		if ((u3.v != q->num.v) && (u3.v != q->den.v))
			freeh(u3.v);
		u1 = v1; u2 = v2; u3 = v3;
		v1 = t1; v2 = t2; v3 = t3;
	}
	if (u3.v != q->den.v)
		freeh(u3.v);
	freeh(v1.v);
	freeh(v2.v);
	i = iszero(v3);
	freeh(v3.v);
	if (i && haveeps) {
		freeh(u1.v);
		freeh(u2.v);
		return qlink(q);
	}
	qtest.num = u2;
	qtest.den = u1;
	qtest.den.sign = 0;
	qtest.num.sign = q->num.sign;
	qtmp = qcopy(&qtest);
	freeh(u1.v);
	freeh(u2.v);
	return qtmp;
}


/*
 * Return an indication on whether or not two fractions are approximately
 * equal within the specified epsilon. Returns negative if the absolute value
 * of the difference between the two numbers is less than epsilon, zero if
 * the difference is equal to epsilon, and positive if the difference is
 * greater than epsilon.
 */
FLAG
qnear(q1, q2, epsilon)
	NUMBER *q1, *q2, *epsilon;
{
	int res;
	NUMBER qtemp, *qq;

	if (qisneg(epsilon))
		error("Negative epsilon for near");
	if (q1 == q2) {
		if (qiszero(epsilon))
			return 0;
		return -1;
	}
	if (qiszero(epsilon))
		return qcmp(q1, q2);
	if (qiszero(q2)) {
		qtemp = *q1;
		qtemp.num.sign = 0;
		return qrel(&qtemp, epsilon);
	}
	if (qiszero(q1)) {
		qtemp = *q2;
		qtemp.num.sign = 0;
		return qrel(&qtemp, epsilon);
	}
	qq = qsub(q1, q2);
	qtemp = *qq;
	qtemp.num.sign = 0;
	res = qrel(&qtemp, epsilon);
	qfree(qq);
	return res;
}


/*
 * Compute the gcd (greatest common divisor) of two numbers.
 *	q3 = qgcd(q1, q2);
 */
NUMBER *
qgcd(q1, q2)
	register NUMBER *q1, *q2;
{
	ZVALUE z;
	NUMBER *q;

	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integers for gcd");
	zgcd(q1->num, q2->num, &z);
	if (isunit(z)) {
		freeh(z.v);
		return qlink(&_qone_);
	}
	q = qalloc();
	q->num = z;
	return q;
}


/*
 * Compute the lcm (least common denominator) of two numbers.
 *	q3 = qlcm(q1, q2);
 */
NUMBER *
qlcm(q1, q2)
	register NUMBER *q1, *q2;
{
	NUMBER *q;

	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integers for lcm");
	if (qisunit(q1))
		return qlink(q2);
	if (qisunit(q2))
		return qlink(q1);
	q = qalloc();
	zlcm(q1->num, q2->num, &q->num);
	return q;
}


/*
 * Remove all occurances of the specified factor from a number.
 * Returned number is always positive.
 */
NUMBER *
qfacrem(q1, q2)
	NUMBER *q1, *q2;
{
	long count;
	ZVALUE tmp;
	NUMBER *r;

	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integers for factor removal");
	count = zfacrem(q1->num, q2->num, &tmp);
	if (isunit(tmp)) {
		freeh(tmp.v);
		return qlink(&_qone_);
	}
	if (count == 0) {
		freeh(tmp.v);
		return qlink(q1);
	}
	r = qalloc();
	r->num = tmp;
	return r;
}


/*
 * Divide one number by the gcd of it with another number repeatedly until
 * the number is relatively prime.
 */
NUMBER *
qgcdrem(q1, q2)
	NUMBER *q1, *q2;
{
	ZVALUE tmp;
	NUMBER *r;

	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integers for gcdrem");
	zgcdrem(q1->num, q2->num, &tmp);
	if (isunit(tmp)) {
		freeh(tmp.v);
		return qlink(&_qone_);
	}
	if (zcmp(q1->num, tmp) == 0) {
		freeh(tmp.v);
		return qlink(q1);
	}
	r = qalloc();
	r->num = tmp;
	return r;
}


/*
 * Return the lowest prime factor of a number.
 * Search is conducted for the specified number of primes.
 * Returns one if no factor was found.
 */
NUMBER *
qlowfactor(q1, q2)
	NUMBER *q1, *q2;
{
	if (qisfrac(q1) || qisfrac(q2))
		error("Non-integers for lowfactor");
	return itoq(zlowfactor(q1->num, ztoi(q2->num)));
}


/*
 * Return the number of places after the decimal point needed to exactly
 * represent the specified number as a real number.  Integers return zero,
 * and non-terminating decimals return minus one.  Examples:
 *	qplaces(1/7)=-1, qplaces(3/10)= 1, qplaces(1/8)=3, qplaces(4)=0.
 */
long
qplaces(q)
	NUMBER *q;
{
	long twopow, fivepow;
	HALF fiveval[2];
	ZVALUE five;
	ZVALUE tmp;

	if (qisint(q))	/* no decimal places if number is integer */
		return 0;
	/*
	 * The number of decimal places of a fraction in lowest terms is finite
	 * if an only if the denominator is of the form 2^A * 5^B, and then the
	 * number of decimal places is equal to MAX(A, B).
	 */
	five.sign = 0;
	five.len = 1;
	five.v = fiveval;
	fiveval[0] = 5;
	fivepow = zfacrem(q->den, five, &tmp);
	if (!zisonebit(tmp)) {
		freeh(tmp.v);
		return -1;
	}
	twopow = zlowbit(tmp);
	freeh(tmp.v);
	if (twopow < fivepow)
		twopow = fivepow;
	return twopow;
}


/*
 * Perform a probabilistic primality test (algorithm P in Knuth).
 * Returns FALSE if definitely not prime, or TRUE if probably prime.
 * Second arg determines how many times to check for primality.
 */
BOOL
qprimetest(q1, q2)
	NUMBER *q1, *q2;
{
	if (qisfrac(q1) || qisfrac(q2) || qisneg(q2))
		error("Bad arguments for qprimetest");
	return zprimetest(q1->num, qtoi(q2));
}

/* END CODE */
