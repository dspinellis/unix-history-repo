/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Extended precision integral arithmetic primitives
 */

#include "zmath.h"


HALF _twoval_[] = { 2 };
HALF _zeroval_[] = { 0 };
HALF _oneval_[] = { 1 };
HALF _tenval_[] = { 10 };

ZVALUE _zero_ = { _zeroval_, 1, 0};
ZVALUE _one_ = { _oneval_, 1, 0 };
ZVALUE _ten_ = { _tenval_, 1, 0 };


/*
 * mask of given bits, rotated thru all bit positions twice
 *
 * bitmask[i] 	 (1 << (i-1)),  for  -BASEB*4<=i<=BASEB*4
 */
static HALF *bmask;		/* actual rotation thru 8 cycles */
static HALF **rmask;		/* actual rotation pointers thru 2 cycles */
HALF *bitmask;			/* bit rotation, norm 0 */

BOOL _math_abort_;		/* nonzero to abort calculations */


static void dadd MATH_PROTO((ZVALUE z1, ZVALUE z2, long y, long n));
static BOOL dsub MATH_PROTO((ZVALUE z1, ZVALUE z2, long y, long n));
static void dmul MATH_PROTO((ZVALUE z, FULL x, ZVALUE *dest));


#ifdef ALLOCTEST
static long nalloc = 0;
static long nfree = 0;
#endif


HALF *
alloc(len)
	long len;
{
	HALF *hp;

	if (_math_abort_)
		math_error("Calculation aborted");
	hp = (HALF *) malloc((len+1) * sizeof(HALF));
	if (hp == 0)
		math_error("Not enough memory");
#ifdef ALLOCTEST
	++nalloc;
#endif
	return hp;
}


#ifdef ALLOCTEST
void
freeh(h)
	HALF *h;
{
	if ((h != _zeroval_) && (h != _oneval_)) {
		free(h);
		++nfree;
	}
}


void
allocStat()
{
	fprintf(stderr, "nalloc: %ld nfree: %ld kept: %ld\n",
		nalloc, nfree, nalloc - nfree);
}
#endif


/*
 * Convert a normal integer to a number.
 */
void
itoz(i, res)
	long i;
	ZVALUE *res;
{
	long diddle, len;

	res->len = 1;
	res->sign = 0;
	diddle = 0;
	if (i == 0) {
		res->v = _zeroval_;
		return;
	}
	if (i < 0) {
		res->sign = 1;
		i = -i;
		if (i < 0) {	/* fix most negative number */
			diddle = 1;
			i--;
		}
	}
	if (i == 1) {
		res->v = _oneval_;
		return;
	}
	len = 1 + (((FULL) i) >= BASE);
	res->len = len;
	res->v = alloc(len);
	res->v[0] = (HALF) (i + diddle);
	if (len == 2)
		res->v[1] = (HALF) (i / BASE);
}


/*
 * Convert a number to a normal integer, as far as possible.
 * If the number is out of range, the largest number is returned.
 */
long
ztoi(z)
	ZVALUE z;
{
	long i;

	if (zisbig(z)) {
		i = MAXFULL;
		return (z.sign ? -i : i);
	}
	i = (zistiny(z) ? z1tol(z) : z2tol(z));
	return (z.sign ? -i : i);
}


/*
 * Make a copy of an integer value
 */
void
zcopy(z, res)
	ZVALUE z, *res;
{
	res->sign = z.sign;
	res->len = z.len;
	if (zisleone(z)) {	/* zero or plus or minus one are easy */
		res->v = (z.v[0] ? _oneval_ : _zeroval_);
		return;
	}
	res->v = alloc(z.len);
	zcopyval(z, *res);
}


/*
 * Add together two integers.
 */
void
zadd(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	ZVALUE dest;
	HALF *p1, *p2, *pd;
	long len;
	FULL carry;
	SIUNION sival;

	if (z1.sign && !z2.sign) {
		z1.sign = 0;
		zsub(z2, z1, res);
		return;
	}
	if (z2.sign && !z1.sign) {
		z2.sign = 0;
		zsub(z1, z2, res);
		return;
	}
	if (z2.len > z1.len) {
		pd = z1.v; z1.v = z2.v; z2.v = pd;
		len = z1.len; z1.len = z2.len; z2.len = len;
	}
	dest.len = z1.len + 1;
	dest.v = alloc(dest.len);
	dest.sign = z1.sign;
	carry = 0;
	pd = dest.v;
	p1 = z1.v;
	p2 = z2.v;
	len = z2.len;
	while (len--) {
		sival.ivalue = ((FULL) *p1++) + ((FULL) *p2++) + carry;
		*pd++ = sival.silow;
		carry = sival.sihigh;
	}
	len = z1.len - z2.len;
	while (len--) {
		sival.ivalue = ((FULL) *p1++) + carry;
		*pd++ = sival.silow;
		carry = sival.sihigh;
	}
	*pd = (HALF)carry;
	zquicktrim(dest);
	*res = dest;
}


/*
 * Subtract two integers.
 */
void
zsub(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	register HALF *h1, *h2, *hd;
	long len1, len2;
	FULL carry;
	SIUNION sival;
	ZVALUE dest;

	if (z1.sign != z2.sign) {
		z2.sign = z1.sign;
		zadd(z1, z2, res);
		return;
	}
	len1 = z1.len;
	len2 = z2.len;
	if (len1 == len2) {
		h1 = z1.v + len1 - 1;
		h2 = z2.v + len2 - 1;
		while ((len1 > 0) && ((FULL)*h1 == (FULL)*h2)) {
			len1--;
			h1--;
			h2--;
		}
		if (len1 == 0) {
			*res = _zero_;
			return;
		}
		len2 = len1;
		carry = ((FULL)*h1 < (FULL)*h2);
	} else {
		carry = (len1 < len2);
	}
	dest.sign = z1.sign;
	h1 = z1.v;
	h2 = z2.v;
	if (carry) {
		carry = len1;
		len1 = len2;
		len2 = carry;
		h1 = z2.v;
		h2 = z1.v;
		dest.sign = !dest.sign;
	}
	hd = alloc(len1);
	dest.v = hd;
	dest.len = len1;
	len1 -= len2;
	carry = 0;
	while (--len2 >= 0) {
		sival.ivalue = (BASE1 - ((FULL) *h1++)) + *h2++ + carry;
		*hd++ = BASE1 - sival.silow;
		carry = sival.sihigh;
	}
	while (--len1 >= 0) {
		sival.ivalue = (BASE1 - ((FULL) *h1++)) + carry;
		*hd++ = BASE1 - sival.silow;
		carry = sival.sihigh;
	}
	if (hd[-1] == 0)
		ztrim(&dest);
	*res = dest;
}


/*
 * Multiply an integer by a small number.
 */
void
zmuli(z, n, res)
	ZVALUE z;
	long n;
	ZVALUE *res;
{
	register HALF *h1, *sd;
	FULL low;
	FULL high;
	FULL carry;
	long len;
	SIUNION sival;
	ZVALUE dest;

	if ((n == 0) || ziszero(z)) {
		*res = _zero_;
		return;
	}
	if (n < 0) {
		n = -n;
		z.sign = !z.sign;
	}
	if (n == 1) {
		zcopy(z, res);
		return;
	}
	low = ((FULL) n) & BASE1;
	high = ((FULL) n) >> BASEB;
	dest.len = z.len + 2;
	dest.v = alloc(dest.len);
	dest.sign = z.sign;
	/*
	 * Multiply by the low digit.
	 */
	h1 = z.v;
	sd = dest.v;
	len = z.len;
	carry = 0;
	while (len--) {
		sival.ivalue = ((FULL) *h1++) * low + carry;
		*sd++ = sival.silow;
		carry = sival.sihigh;
	}
	*sd = (HALF)carry;
	/*
	 * If there was only one digit, then we are all done except
	 * for trimming the number if there was no last carry.
	 */
	if (high == 0) {
		dest.len--;
		if (carry == 0)
			dest.len--;
		*res = dest;
		return;
	}
	/*
	 * Need to multiply by the high digit and add it into the
	 * previous value.  Clear the final word of rubbish first.
	 */
	*(++sd) = 0;
	h1 = z.v;
	sd = dest.v + 1;
	len = z.len;
	carry = 0;
	while (len--) {
		sival.ivalue = ((FULL) *h1++) * high + ((FULL) *sd) + carry;
		*sd++ = sival.silow;
		carry = sival.sihigh;
	}
	*sd = (HALF)carry;
	zquicktrim(dest);
	*res = dest;
}


/*
 * Divide two numbers by their greatest common divisor.
 * This is useful for reducing the numerator and denominator of
 * a fraction to its lowest terms.
 */
void
zreduce(z1, z2, z1res, z2res)
	ZVALUE z1, z2, *z1res, *z2res;
{
	ZVALUE tmp;

	if (zisleone(z1) || zisleone(z2))
		tmp = _one_;
	else
		zgcd(z1, z2, &tmp);
	if (zisunit(tmp)) {
		zcopy(z1, z1res);
		zcopy(z2, z2res);
	} else {
		zquo(z1, tmp, z1res);
		zquo(z2, tmp, z2res);
	}
	zfree(tmp);
}


/*
 * Divide two numbers to obtain a quotient and remainder.
 * This algorithm is taken from
 * Knuth, The Art of Computer Programming, vol 2: Seminumerical Algorithms.
 * Slight modifications were made to speed this mess up.
 */
void
zdiv(z1, z2, res, rem)
	ZVALUE z1, z2, *res, *rem;
{
	long i, j, k;
	register HALF *q, *pp;
	SIUNION pair;		/* pair of halfword values */
	HALF h2, v2;
	long y;
	FULL x;
	ZVALUE ztmp1, ztmp2, ztmp3, quo;

	if (ziszero(z2))
		math_error("Division by zero");
	if (ziszero(z1)) {
		*res = _zero_;
		*rem = _zero_;
		return;
	}
	if (zisone(z2)) {
		zcopy(z1, res);
		*rem = _zero_;
		return;
	}
	i = BASE / 2;
	j = 0;
	k = (long) z2.v[z2.len - 1];
	while (! (k & i)) {
		j ++;
		i >>= 1;
	}
	ztmp1.v = alloc(z1.len + 1);
	ztmp1.len = z1.len + 1;
	zcopyval(z1, ztmp1);
	ztmp1.v[z1.len] = 0;
	ztmp1.sign = 0;
	ztmp2.v = alloc(z2.len);
	ztmp2.len = z2.len;
	ztmp2.sign = 0;
	zcopyval(z2, ztmp2);
	if (zrel(ztmp1, ztmp2) < 0) {
		rem->v = ztmp1.v;
		rem->sign = z1.sign;
		rem->len = z1.len;
		zfree(ztmp2);
		*res = _zero_;
		return;
	}
	quo.len = z1.len - z2.len + 1;
	quo.v = alloc(quo.len);
	quo.sign = z1.sign != z2.sign;
	zclearval(quo);

	ztmp3.v = zalloctemp(z2.len + 1);

	/*
	 * Normalize z1 and z2
	 */
	zshiftl(ztmp1, j);
	zshiftl(ztmp2, j);

	k = ztmp1.len - ztmp2.len;
	q = quo.v + quo.len;
	y = ztmp1.len - 1;
	h2 = ztmp2.v [ztmp2.len - 1];
	v2 = 0;
	if (ztmp2.len >= 2)
		v2 = ztmp2.v [ztmp2.len - 2];
	for (;k--; --y) {
		pp = ztmp1.v + y - 1;
		pair.silow = pp[0];
		pair.sihigh = pp[1];
		if (ztmp1.v[y] == h2)
			x = BASE1;
		else
			x = pair.ivalue / h2;
		if (x) {
			while (pair.ivalue - x * h2 < BASE && y > 1 &&
				v2 * x > (pair.ivalue - x * h2) * BASE + ztmp1.v [y-2]) {
					--x;
			}
			dmul(ztmp2, x, &ztmp3);
#ifdef divblab
			printf(" x: %ld\n", x);
			printf("ztmp1: ");
			printz(ztmp1);
			printf("ztmp2: ");
			printz(ztmp2);
			printf("ztmp3: ");
			printz(ztmp3);
#endif
			if (dsub(ztmp1, ztmp3, y, ztmp2.len)) {
				--x;
				/*
				printf("adding back\n");
				*/
				dadd(ztmp1, ztmp2, y, ztmp2.len);
			}
		}
		ztrim(&ztmp1);
		*--q = (HALF)x;
	}
	zshiftr(ztmp1, j);
	*rem = ztmp1;
	ztrim(rem);
	zfree(ztmp2);
	ztrim(&quo);
	*res = quo;
}


/*
 * Return the quotient and remainder of an integer divided by a small
 * number.  A nonzero remainder is only meaningful when both numbers
 * are positive.
 */
long
zdivi(z, n, res)
	ZVALUE z, *res;
	long n;
{
	register HALF *h1, *sd;
	FULL val;
	HALF divval[2];
	ZVALUE div;
	ZVALUE dest;
	long len;

	if (n == 0)
		math_error("Division by zero");
	if (ziszero(z)) {
		*res = _zero_;
		return 0;
	}
	if (n < 0) {
		n = -n;
		z.sign = !z.sign;
	}
	if (n == 1) {
		zcopy(z, res);
		return 0;
	}
	/*
	 * If the division is by a large number, then call the normal
	 * divide routine.
	 */
	if (n & ~BASE1) {
		div.sign = 0;
		div.len = 2;
		div.v = divval;
		divval[0] = (HALF) n;
		divval[1] = ((FULL) n) >> BASEB;
		zdiv(z, div, res, &dest);
		n = (zistiny(dest) ? z1tol(dest) : z2tol(dest));
		zfree(dest);
		return n;
	}
	/*
	 * Division is by a small number, so we can be quick about it.
	 */
	len = z.len;
	dest.sign = z.sign;
	dest.len = len;
	dest.v = alloc(len);
	h1 = z.v + len - 1;
	sd = dest.v + len - 1;
	val = 0;
	while (len--) {
		val = ((val << BASEB) + ((FULL) *h1--));
		*sd-- = val / n;
		val %= n;
	}
	zquicktrim(dest);
	*res = dest;
	return val;
}


/*
 * Return the quotient of two numbers.
 * This works the same as zdiv, except that the remainer is not returned.
 */
void
zquo(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	long i, j, k;
	register HALF *q, *pp;
	SIUNION pair;			/* pair of halfword values */
	HALF h2, v2;
	long y;
	FULL x;
	ZVALUE ztmp1, ztmp2, ztmp3, quo;

	if (ziszero(z2))
		math_error("Division by zero");
	if (ziszero(z1)) {
		*res = _zero_;
		return;
	}
	if (zisone(z2)) {
		zcopy(z1, res);
		return;
	}
	i = BASE / 2;
	j = 0;
	k = (long) z2.v[z2.len - 1];
	while (! (k & i)) {
		j ++;
		i >>= 1;
	}
	ztmp1.v = alloc(z1.len + 1);
	ztmp1.len = z1.len + 1;
	zcopyval(z1, ztmp1);
	ztmp1.v[z1.len] = 0;
	ztmp1.sign = 0;
	ztmp2.v = alloc(z2.len);
	ztmp2.len = z2.len;
	ztmp2.sign = 0;
	zcopyval(z2, ztmp2);
	if (zrel(ztmp1, ztmp2) < 0) {
		zfree(ztmp1);
		zfree(ztmp2);
		*res = _zero_;
		return;
	}
	quo.len = z1.len - z2.len + 1;
	quo.v = alloc(quo.len);
	quo.sign = z1.sign != z2.sign;
	zclearval(quo);

	ztmp3.v = zalloctemp(z2.len + 1);

	/*
	 * Normalize z1 and z2
	 */
	zshiftl(ztmp1, j);
	zshiftl(ztmp2, j);

	k = ztmp1.len - ztmp2.len;
	q = quo.v + quo.len;
	y = ztmp1.len - 1;
	h2 = ztmp2.v [ztmp2.len - 1];
	v2 = 0;
	if (ztmp2.len >= 2)
		v2 = ztmp2.v [ztmp2.len - 2];
	for (;k--; --y) {
		pp = ztmp1.v + y - 1;
		pair.silow = pp[0];
		pair.sihigh = pp[1];
		if (ztmp1.v[y] == h2)
			x = BASE1;
		else
			x = pair.ivalue / h2;
		if (x) {
			while (pair.ivalue - x * h2 < BASE && y > 1 &&
				v2 * x > (pair.ivalue - x * h2) * BASE + ztmp1.v [y-2]) {
					--x;
			}
			dmul(ztmp2, x, &ztmp3);
			if (dsub(ztmp1, ztmp3, y, ztmp2.len)) {
				--x;
				dadd(ztmp1, ztmp2, y, ztmp2.len);
			}
		}
		ztrim(&ztmp1);
		*--q = (HALF)x;
	}
	zfree(ztmp1);
	zfree(ztmp2);
	ztrim(&quo);
	*res = quo;
}


/*
 * Compute the remainder after dividing one number by another.
 * This is only defined for positive z2 values.
 * The result is normalized to lie in the range 0 to z2-1.
 */
void
zmod(z1, z2, rem)
	ZVALUE z1, z2, *rem;
{
	long i, j, k, neg;
	register HALF *pp;
	SIUNION pair;			/* pair of halfword values */
	HALF h2, v2;
	long y;
	FULL x;
	ZVALUE ztmp1, ztmp2, ztmp3;

	if (ziszero(z2))
		math_error("Division by zero");
	if (zisneg(z2))
		math_error("Non-positive modulus");
	if (ziszero(z1) || zisunit(z2)) {
		*rem = _zero_;
		return;
	}
	if (zistwo(z2)) {
		if (zisodd(z1))
			*rem = _one_;
		else
			*rem = _zero_;
		return;
	}
	neg = z1.sign;
	z1.sign = 0;

	/*
	 * Do a quick check to see if the absolute value of the number
	 * is less than the modulus.  If so, then the result is just a
	 * subtract or a copy.
	 */
	h2 = z1.v[z1.len - 1];
	v2 = z2.v[z2.len - 1];
	if ((z1.len < z2.len) || ((z1.len == z2.len) && (h2 < v2))) {
		if (neg)
			zsub(z2, z1, rem);
		else
			zcopy(z1, rem);
		return;
	}

	/*
	 * Do another quick check to see if the number is positive and
	 * between the size of the modulus and twice the modulus.
	 * If so, then the answer is just another subtract.
	 */
	if (!neg && (z1.len == z2.len) && (h2 > v2) &&
		(((FULL) h2) < 2 * ((FULL) v2)))
	{
		zsub(z1, z2, rem);
		return;
	}

	/*
	 * If the modulus is an exact power of two, then the result
	 * can be obtained by ignoring the high bits of the number.
	 * This truncation assumes that the number of words for the
	 * number is at least as large as the number of words in the
	 * modulus, which is true at this point.
	 */
	if (((v2 & -v2) == v2) && zisonebit(z2)) {	/* ASSUMES 2'S COMP */
		i = zhighbit(z2);
		z1.len = (i + BASEB - 1) / BASEB;
		zcopy(z1, &ztmp1);
		i %= BASEB;
		if (i)
			ztmp1.v[ztmp1.len - 1] &= ((((HALF) 1) << i) - 1);
		ztmp2.len = 0;
		goto gotanswer;
	}

	/*
	 * If the modulus is one less than an exact power of two, then
	 * the result can be simplified similarly to "casting out 9's".
	 * Only do this simplification for large enough modulos.
	 */
	if ((z2.len > 1) && (z2.v[0] == BASE1) && zisallbits(z2)) {
		i = -(zhighbit(z2) + 1);
		zcopy(z1, &ztmp1);
		z1 = ztmp1;
		while ((k = zrel(z1, z2)) > 0) {
			ztmp1 = _zero_;
			while (!ziszero(z1)) {
				zand(z1, z2, &ztmp2);
				zadd(ztmp2, ztmp1, &ztmp3);
				zfree(ztmp1);
				zfree(ztmp2);
				ztmp1 = ztmp3;
				zshift(z1, i, &ztmp2);
				zfree(z1);
				z1 = ztmp2;
			}
			zfree(z1);
			z1 = ztmp1;
		}
		if (k == 0) {
			zfree(ztmp1);
			*rem = _zero_;
			return;
		}
		ztmp2.len = 0;
		goto gotanswer;
	}

	/*
	 * Must actually do the divide.
	 */
	i = BASE / 2;
	j = 0;
	k = (long) z2.v[z2.len - 1];
	while (! (k & i)) {
		j ++;
		i >>= 1;
	}
	ztmp1.v = alloc(z1.len + 1);
	ztmp1.len = z1.len + 1;
	zcopyval(z1, ztmp1);
	ztmp1.v[z1.len] = 0;
	ztmp1.sign = 0;
	ztmp2.v = alloc(z2.len);
	ztmp2.len = z2.len;
	ztmp2.sign = 0;
	zcopyval(z2, ztmp2);
	if (zrel(ztmp1, ztmp2) < 0)
		goto gotanswer;

	ztmp3.v = zalloctemp(z2.len + 1);

	/*
	 * Normalize z1 and z2
	 */
	zshiftl(ztmp1, j);
	zshiftl(ztmp2, j);

	k = ztmp1.len - ztmp2.len;
	y = ztmp1.len - 1;
	h2 = ztmp2.v [ztmp2.len - 1];
	v2 = 0;
	if (ztmp2.len >= 2)
		v2 = ztmp2.v [ztmp2.len - 2];
	for (;k--; --y) {
		pp = ztmp1.v + y - 1;
		pair.silow = pp[0];
		pair.sihigh = pp[1];
		if (ztmp1.v[y] == h2)
			x = BASE1;
		else
			x = pair.ivalue / h2;
		if (x) {
			while (pair.ivalue - x * h2 < BASE && y > 1 &&
				v2 * x > (pair.ivalue - x * h2) * BASE + ztmp1.v [y-2]) {
					--x;
			}
			dmul(ztmp2, x, &ztmp3);
			if (dsub(ztmp1, ztmp3, y, ztmp2.len))
				dadd(ztmp1, ztmp2, y, ztmp2.len);
		}
		ztrim(&ztmp1);
	}
	zshiftr(ztmp1, j);

gotanswer:
	ztrim(&ztmp1);
	if (ztmp2.len)
		zfree(ztmp2);
	if (neg && !ziszero(ztmp1)) {
		zsub(z2, ztmp1, rem);
		zfree(ztmp1);
	} else
		*rem = ztmp1;
}


/*
 * Calculate the mod of an integer by a small number.
 * This is only defined for positive moduli.
 */
long
zmodi(z, n)
	ZVALUE z;
	long n;
{
	register HALF *h1;
	FULL val;
	HALF divval[2];
	ZVALUE div;
	ZVALUE temp;
	long len;

	if (n == 0)
		math_error("Division by zero");
	if (n < 0)
		math_error("Non-positive modulus");
	if (ziszero(z) || (n == 1))
		return 0;
	if (zisone(z))
		return 1;
	/*
	 * If the modulus is by a large number, then call the normal
	 * modulo routine.
	 */
	if (n & ~BASE1) {
		div.sign = 0;
		div.len = 2;
		div.v = divval;
		divval[0] = (HALF) n;
		divval[1] = ((FULL) n) >> BASEB;
		zmod(z, div, &temp);
		n = (zistiny(temp) ? z1tol(temp) : z2tol(temp));
		zfree(temp);
		return n;
	}
	/*
	 * The modulus is by a small number, so we can do this quickly.
	 */
	len = z.len;
	h1 = z.v + len - 1;
	val = 0;
	while (len--)
		val = ((val << BASEB) + ((FULL) *h1--)) % n;
	if (z.sign)
		val = n - val;
	return val;
}


/*
 * Return whether or not one number exactly divides another one.
 * Returns TRUE if division occurs with no remainder.
 * z1 is the number to be divided by z2.
 */
BOOL
zdivides(z1, z2)
	ZVALUE z1, z2;		/* numbers to test division into and by */
{
	ZVALUE temp;
	long cv;

	z1.sign = 0;
	z2.sign = 0;
	/*
	 * Take care of obvious cases first
	 */
	if (zisleone(z2)) {	/* division by zero or one */
		if (*z2.v == 0)
			math_error("Division by zero");
		return TRUE;
	}
	if (ziszero(z1))	/* everything divides zero */
		return TRUE;
	if (z1.len < z2.len)	/* quick size comparison */
		return FALSE;
	if ((z1.len == z2.len) && (z1.v[z1.len-1] < z2.v[z2.len-1]))	/* more */
		return FALSE;
	if (zisodd(z1) && ziseven(z2))	/* can't divide odd by even */
		return FALSE;
	if (zlowbit(z1) < zlowbit(z2))	/* can't have smaller power of two */
		return FALSE;
	cv = zrel(z1, z2);	/* can't divide smaller number */
	if (cv <= 0)
		return (cv == 0);
	/*
	 * Now do the real work.  Divisor divides dividend if the gcd of the
	 * two numbers equals the divisor.
	 */
	zgcd(z1, z2, &temp);
	cv = zcmp(z2, temp);
	zfree(temp);
	return (cv == 0);
}


/*
 * Compute the logical OR of two numbers
 */
void
zor(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	register HALF *sp, *dp;
	long len;
	ZVALUE bz, lz, dest;

	if (z1.len >= z2.len) {
		bz = z1;
		lz = z2;
	} else {
		bz = z2;
		lz = z1;
	}
	dest.len = bz.len;
	dest.v = alloc(dest.len);
	dest.sign = 0;
	zcopyval(bz, dest);
	len = lz.len;
	sp = lz.v;
	dp = dest.v;
	while (len--)
		*dp++ |= *sp++;
	*res = dest;
}


/*
 * Compute the logical AND of two numbers.
 */
void
zand(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	register HALF *h1, *h2, *hd;
	register long len;
	ZVALUE dest;

	len = ((z1.len <= z2.len) ? z1.len : z2.len);
	h1 = &z1.v[len-1];
	h2 = &z2.v[len-1];
	while ((len > 1) && ((*h1 & *h2) == 0)) {
		h1--;
		h2--;
		len--;
	}
	dest.len = len;
	dest.v = alloc(len);
	dest.sign = 0;
	h1 = z1.v;
	h2 = z2.v;
	hd = dest.v;
	while (len--)
		*hd++ = (*h1++ & *h2++);
	*res = dest;
}


/*
 * Compute the logical XOR of two numbers.
 */
void
zxor(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	register HALF *sp, *dp;
	long len;
	ZVALUE bz, lz, dest;

	if (z1.len == z2.len) {
		for (len = z1.len; ((len > 1) && (z1.v[len-1] == z2.v[len-1])); len--) ;
		z1.len = len;
		z2.len = len;
	}
	if (z1.len >= z2.len) {
		bz = z1;
		lz = z2;
	} else {
		bz = z2;
		lz = z1;
	}
	dest.len = bz.len;
	dest.v = alloc(dest.len);
	dest.sign = 0;
	zcopyval(bz, dest);
	len = lz.len;
	sp = lz.v;
	dp = dest.v;
	while (len--)
		*dp++ ^= *sp++;
	*res = dest;
}


/*
 * Shift a number left (or right) by the specified number of bits.
 * Positive shift means to the left.  When shifting right, rightmost
 * bits are lost.  The sign of the number is preserved.
 */
void
zshift(z, n, res)
	ZVALUE z, *res;
	long n;
{
	ZVALUE ans;
	long hc;		/* number of halfwords shift is by */

	if (ziszero(z)) {
		*res = _zero_;
		return;
	}
	if (n == 0) {
		zcopy(z, res);
		return;
	}
	/*
	 * If shift value is negative, then shift right.
	 * Check for large shifts, and handle word-sized shifts quickly.
	 */
	if (n < 0) {
		n = -n;
		if ((n < 0) || (n >= (z.len * BASEB))) {
			*res = _zero_;
			return;
		}
		hc = n / BASEB;
		n %= BASEB;
		z.v += hc;
		z.len -= hc;
		ans.len = z.len;
		ans.v = alloc(ans.len);
		ans.sign = z.sign;
		zcopyval(z, ans);
		if (n > 0) {
			zshiftr(ans, n);
			ztrim(&ans);
		}
		if (ziszero(ans)) {
			zfree(ans);
			ans = _zero_;
		}
		*res = ans;
		return;
	}
	/*
	 * Shift value is positive, so shift leftwards.
	 * Check specially for a shift of the value 1, since this is common.
	 * Also handle word-sized shifts quickly.
	 */
	if (zisunit(z)) {
		zbitvalue(n, res);
		res->sign = z.sign;
		return;
	}
	hc = n / BASEB;
	n %= BASEB;
	ans.len = z.len + hc + 1;
	ans.v = alloc(ans.len);
	ans.sign = z.sign;
	if (hc > 0)
		memset((char *) ans.v, 0, hc * sizeof(HALF));
	memcpy((char *) (ans.v + hc), 
	    (char *) z.v, z.len * sizeof(HALF));
	ans.v[ans.len - 1] = 0;
	if (n > 0) {
		ans.v += hc;
		ans.len -= hc;
		zshiftl(ans, n);
		ans.v -= hc;
		ans.len += hc;
	}
	ztrim(&ans);
	*res = ans;
}


/*
 * Return the position of the lowest bit which is set in the binary
 * representation of a number (counting from zero).  This is the highest
 * power of two which evenly divides the number.
 */
long
zlowbit(z)
	ZVALUE z;
{
	register HALF *zp;
	long n;
	HALF dataval;
	HALF *bitval;

	n = 0;
	for (zp = z.v; *zp == 0; zp++)
		if (++n >= z.len)
			return 0;
	dataval = *zp;
	bitval = bitmask;
	while ((*(bitval++) & dataval) == 0) {
	}
	return (n*BASEB)+(bitval-bitmask-1);
}


/*
 * Return the position of the highest bit which is set in the binary
 * representation of a number (counting from zero).  This is the highest power
 * of two which is less than or equal to the number (which is assumed nonzero).
 */
long
zhighbit(z)
	ZVALUE z;
{
	HALF dataval;
	HALF *bitval;

	dataval = z.v[z.len-1];
	bitval = bitmask+BASEB;
	if (dataval) {
		while ((*(--bitval) & dataval) == 0) {
		}
	}
	return (z.len*BASEB)+(bitval-bitmask-BASEB);
}


#if 0
/*
 * Reverse the bits of a particular range of bits of a number.
 *
 * This function returns an integer with bits a thru b swapped.
 * That is, bit a is swapped with bit b, bit a+1 is swapped with b-1,
 * and so on.
 *
 * As a special case, if the ending bit position is < 0, is it taken to 
 * mean the highest bit set.  Thus zbitrev(0, -1, z, &res) will 
 * perform a complete bit reverse of the number 'z'.
 *
 * As a special case, if the starting bit position is < 0, is it taken to 
 * mean the lowest bit set.  Thus zbitrev(-1, -1, z, &res) is the
 * same as zbitrev(lowbit(z), highbit(z), z, &res).
 *
 * Note that the low order bit number is taken to be 0.  Also, bitrev
 * ignores the sign of the number.
 *
 * Bits beyond the highest bit are taken to be zero.  Thus the calling
 * bitrev(0, 100, _one_, &res) will result in a value of 2^100.
 */
void
zbitrev(low, high, z, res)
	long low;	/* lowest bit to reverse, <0 => lowbit(z) */
	long high;	/* highest bit to reverse, <0 => highbit(z) */
	ZVALUE z;	/* value to bit reverse */
	ZVALUE *res;	/* resulting bit reverse number */
{
}
#endif


/*
 * Return whether or not the specifed bit number is set in a number.
 * Rightmost bit of a number is bit 0.
 */
BOOL
zisset(z, n)
	ZVALUE z;
	long n;
{
	if ((n < 0) || ((n / BASEB) >= z.len))
		return FALSE;
	return ((z.v[n / BASEB] & (((HALF) 1) << (n % BASEB))) != 0);
}


/*
 * Check whether or not a number has exactly one bit set, and
 * thus is an exact power of two.  Returns TRUE if so.
 */
BOOL
zisonebit(z)
	ZVALUE z;
{
	register HALF *hp;
	register LEN len;

	if (ziszero(z) || zisneg(z))
		return FALSE;
	hp = z.v;
	len = z.len;
	while (len > 4) {
		len -= 4;
		if (*hp++ || *hp++ || *hp++ || *hp++)
			return FALSE;
	}
	while (--len > 0) {
		if (*hp++)
			return FALSE;
	}
	return ((*hp & -*hp) == *hp);		/* NEEDS 2'S COMPLEMENT */
}


/*
 * Check whether or not a number has all of its bits set below some
 * bit position, and thus is one less than an exact power of two.
 * Returns TRUE if so.
 */
BOOL
zisallbits(z)
	ZVALUE z;
{
	register HALF *hp;
	register LEN len;
	HALF digit;

	if (ziszero(z) || zisneg(z))
		return FALSE;
	hp = z.v;
	len = z.len;
	while (len > 4) {
		len -= 4;
		if ((*hp++ != BASE1) || (*hp++ != BASE1) ||
			(*hp++ != BASE1) || (*hp++ != BASE1))
				return FALSE;
	}
	while (--len > 0) {
		if (*hp++ != BASE1)
			return FALSE;
	}
	digit = (HALF)(*hp + 1);
	return ((digit & -digit) == digit);	/* NEEDS 2'S COMPLEMENT */
}


/*
 * Return the number whose binary representation contains only one bit which
 * is in the specified position (counting from zero).  This is equivilant
 * to raising two to the given power.
 */
void
zbitvalue(n, res)
	long n;
	ZVALUE *res;
{
	ZVALUE z;

	if (n < 0) n = 0;
	z.sign = 0;
	z.len = (n / BASEB) + 1;
	z.v = alloc(z.len);
	zclearval(z);
	z.v[z.len-1] = (((HALF) 1) << (n % BASEB));
	*res = z;
}


/*
 * Compare a number against zero.
 * Returns the sgn function of the number (-1, 0, or 1).
 */
FLAG
ztest(z)
	ZVALUE z;
{
	register int sign;
	register HALF *h;
	register long len;

	sign = 1;
	if (z.sign)
		sign = -sign;
	h = z.v;
	len = z.len;
	while (len--)
		if (*h++)
			return sign;
	return 0;
}


/*
 * Compare two numbers to see which is larger.
 * Returns -1 if first number is smaller, 0 if they are equal, and 1 if
 * first number is larger.  This is the same result as ztest(z2-z1).
 */
FLAG
zrel(z1, z2)
	ZVALUE z1, z2;
{
	register HALF *h1, *h2;
	register long len1, len2;
	int sign;

	sign = 1;
	if (z1.sign < z2.sign)
		return 1;
	if (z2.sign < z1.sign)
		return -1;
	if (z2.sign)
		sign = -1;
	len1 = z1.len;
	len2 = z2.len;
	h1 = z1.v + z1.len - 1;
	h2 = z2.v + z2.len - 1;
	while (len1 > len2) {
		if (*h1--)
			return sign;
		len1--;
	}
	while (len2 > len1) {
		if (*h2--)
			return -sign;
		len2--;
	}
	while (len1--) {
		if (*h1-- != *h2--)
			break;
	}
	if ((len1 = *++h1) > (len2 = *++h2))
		return sign;
	if (len1 < len2)
		return -sign;
	return 0;
}


/*
 * Compare two numbers to see if they are equal or not.
 * Returns TRUE if they differ.
 */
BOOL
zcmp(z1, z2)
	ZVALUE z1, z2;
{
	register HALF *h1, *h2;
	register long len;

	if ((z1.sign != z2.sign) || (z1.len != z2.len) || (*z1.v != *z2.v))
		return TRUE;
	len = z1.len;
	h1 = z1.v;
	h2 = z2.v;
	while (len-- > 0) {
		if (*h1++ != *h2++)
			return TRUE;
	}
	return FALSE;
}


/*
 * Internal utility subroutines
 */
static void
dadd(z1, z2, y, n)
	ZVALUE z1, z2;
	long y, n;
{
	HALF *s1, *s2;
	short carry;
	long sum;

	s1 = z1.v + y - n;
	s2 = z2.v;
	carry = 0;
	while (n--) {
		sum = (long)*s1 + (long)*s2 + carry;
		carry = 0;
		if (sum >= BASE) {
			sum -= BASE;
			carry = 1;
		}
		*s1 = (HALF)sum;
		++s1;
		++s2;
	}
	sum = (long)*s1 + carry;
	*s1 = (HALF)sum;
}


/*
 * Do subtract for divide, returning TRUE if subtraction went negative.
 */
static BOOL
dsub(z1, z2, y, n)
	ZVALUE z1, z2;
	long y, n;
{
	HALF *s1, *s2, *s3;
	FULL i1;
	BOOL neg;

	neg = FALSE;
	s1 = z1.v + y - n;
	s2 = z2.v;
	if (++n > z2.len)
		n = z2.len;
	while (n--) {
		i1 = (FULL) *s1;
		if (i1 < (FULL) *s2) {
			s3 = s1 + 1;
			while (s3 < z1.v + z1.len && !(*s3)) {
				*s3 = BASE1;
				++s3;
			}
			if (s3 >= z1.v + z1.len)
				neg = TRUE;
			else
				--(*s3);
			i1 += BASE;
		}
		*s1 = i1 - (FULL) *s2;
		++s1;
		++s2;
	}
	return neg;
}


/*
 * Multiply a number by a single 'digit'.
 * This is meant to be used only by the divide routine, and so the
 * destination area must already be allocated and be large enough.
 */
static void
dmul(z, mul, dest)
	ZVALUE z;
	FULL mul;
	ZVALUE *dest;
{
	register HALF *zp, *dp;
	SIUNION sival;
	FULL carry;
	long len;

	dp = dest->v;
	dest->sign = 0;
	if (mul == 0) {
		dest->len = 1;
		*dp = 0;
		return;
	}
	len = z.len;
	zp = z.v + len - 1;
	while ((*zp == 0) && (len > 1)) {
		len--;
		zp--;
	}
	dest->len = len;
	zp = z.v;
	carry = 0;
	while (len >= 4) {
		len -= 4;
		sival.ivalue = (mul * ((FULL) *zp++)) + carry;
		*dp++ = sival.silow;
		sival.ivalue = (mul * ((FULL) *zp++)) + ((FULL) sival.sihigh);
		*dp++ = sival.silow;
		sival.ivalue = (mul * ((FULL) *zp++)) + ((FULL) sival.sihigh);
		*dp++ = sival.silow;
		sival.ivalue = (mul * ((FULL) *zp++)) + ((FULL) sival.sihigh);
		*dp++ = sival.silow;
		carry = sival.sihigh;
	}
	while (--len >= 0) {
		sival.ivalue = (mul * ((FULL) *zp++)) + carry;
		*dp++ = sival.silow;
		carry = sival.sihigh;
	}
	if (carry) {
		*dp = (HALF)carry;
		dest->len++;
	}
}


/*
 * Utility to calculate the gcd of two small integers.
 */
long
iigcd(i1, i2)
	long i1, i2;
{
	FULL f1, f2, temp;

	f1 = (FULL) ((i1 >= 0) ? i1 : -i1);
	f2 = (FULL) ((i2 >= 0) ? i2 : -i2);
	while (f1) {
		temp = f2 % f1;
		f2 = f1;
		f1 = temp;
	}
	return (long) f2;
}


void
ztrim(z)
	ZVALUE *z;
{
	register HALF *h;
	register long len;

	h = z->v + z->len - 1;
	len = z->len;
	while (*h == 0 && len > 1) {
		--h;
		--len;
	}
	z->len = len;
}


/*
 * Utility routine to shift right.
 */
void
zshiftr(z, n)
	ZVALUE z;
	long n;
{
	register HALF *h, *lim;
	FULL mask, maskt;
	long len;

	if (n >= BASEB) {
		len = n / BASEB;
		h = z.v;
		lim = z.v + z.len - len;
		while (h < lim) {
			h[0] = h[len];
			++h;
		}
		n -= BASEB * len;
		lim = z.v + z.len;
		while (h < lim)
			*h++ = 0;
	}
	if (n) {
		len = z.len;
		h = z.v + len - 1;
		mask = 0;
		while (len--) {
			maskt = (((FULL) *h) << (BASEB - n)) & BASE1;
			*h = (*h >> n) | mask;
			mask = maskt;
			--h;
		}
	}
}


/*
 * Utility routine to shift left.
 */
void
zshiftl(z, n)
	ZVALUE z;
	long n;
{
	register HALF *h;
	FULL mask, i;
	long len;

	if (n >= BASEB) {
		len = n / BASEB;
		h = z.v + z.len - 1;
		while (!*h)
			--h;
		while (h >= z.v) {
			h[len] = h[0];
			--h;
		}
		n -= BASEB * len;
		while (len)
			h[len--] = 0;
	}
	if (n > 0) {
		len = z.len;
		h = z.v;
		mask = 0;
		while (len--) {
			i = (((FULL) *h) << n) | mask;
			if (i > BASE1) {
				mask = i >> BASEB;
				i &= BASE1;
			} else
				mask = 0;
			*h = (HALF) i;
			++h;
		}
	}
}

/*
 * initmasks - init the bitmask rotation arrays
 *
 * bitmask[i] 	 (1 << (i-1)),  for  -BASEB*4<=i<=BASEB*4
 *
 * The bmask array contains 8 cycles of rotations of a bit mask.
 */
void
initmasks()
{
	int i;

	/*
	 * setup the bmask array
	 */
	bmask = alloc((long)((8*BASEB)+1));
	for (i=0; i < (8*BASEB)+1; ++i) {
		bmask[i] = 1 << (i%BASEB);
	}

	/*
	 * setup the rmask pointers
	 */
	rmask = (HALF **)malloc(sizeof(HALF *)*((BASEB*4)+2));
	for (i = 0; i <= (4*BASEB)+1; ++i) {
		rmask[i] = &bmask[(2*BASEB)+i];
	}

	/*
	 * setup the bitmask array to allow -4*BASEB thru 4*BASEB indexing
	 */
	bitmask = &bmask[4*BASEB];
	return;
}

/* END CODE */
