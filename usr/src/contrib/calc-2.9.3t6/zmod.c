/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Routines to do modulo arithmetic both normally and also using the REDC
 * algorithm given by Peter L. Montgomery in Mathematics of Computation,
 * volume 44, number 170 (April, 1985).  For multiple multiplies using
 * the same large modulus, the REDC algorithm avoids the usual division
 * by the modulus, instead replacing it with two multiplies or else a
 * special algorithm.  When these two multiplies or the special algorithm
 * are faster then the division, then the REDC algorithm is better.
 */

#include "zmath.h"


#define	POWBITS	4		/* bits for power chunks (must divide BASEB) */
#define	POWNUMS	(1<<POWBITS)	/* number of powers needed in table */


LEN _pow2_ = POW_ALG2;		/* modulo size to use REDC for powers */
LEN _redc2_ = REDC_ALG2;	/* modulo size to use second REDC algorithm */

static REDC *powermodredc = NULL;	/* REDC info for raising to power */

#if 0
extern void zaddmod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE z3, ZVALUE *res));
extern void znegmod MATH_PROTO((ZVALUE z1, ZVALUE z2, ZVALUE *res));

/*
 * Multiply two numbers together and then mod the result with a third number.
 * The two numbers to be multiplied can be negative or out of modulo range.
 * The result will be in the range 0 to the modulus - 1.
 */
void
zmulmod(z1, z2, z3, res)
	ZVALUE z1;		/* first number to be multiplied */
	ZVALUE z2;		/* second number to be multiplied */
	ZVALUE z3;		/* number to take mod with */
	ZVALUE *res;		/* result */
{
	ZVALUE tmp;
	FULL prod;
	FULL digit;
	BOOL neg;

	if (ziszero(z3) || zisneg(z3))
		math_error("Mod of non-positive integer");
	if (ziszero(z1) || ziszero(z2) || zisunit(z3)) {
		*res = _zero_;
		return;
	}

	/*
	 * If the modulus is a single digit number, then do the result
	 * cheaply.  Check especially for a small power of two.
	 */
	if (zistiny(z3)) {
		neg = (z1.sign != z2.sign);
		digit = z3.v[0];
		if ((digit & -digit) == digit) {	/* NEEDS 2'S COMP */
			prod = ((FULL) z1.v[0]) * ((FULL) z2.v[0]);
			prod &= (digit - 1);
		} else {
			z1.sign = 0;
			z2.sign = 0;
			prod = (FULL) zmodi(z1, (long) digit);
			prod *= (FULL) zmodi(z2, (long) digit);
			prod %= digit;
		}
		if (neg && prod)
			prod = digit - prod;
		itoz((long) prod, res);
		return;
	}

	/*
	 * The modulus is more than one digit.
	 * Actually do the multiply and divide if necessary.
	 */
	zmul(z1, z2, &tmp);
	if (zispos(tmp) && ((tmp.len < z3.len) || ((tmp.len == z3.len) &&
		(tmp.v[tmp.len-1] < z2.v[z3.len-1]))))
	{
		*res = tmp;
		return;
	}
	zmod(tmp, z3, res);
	zfree(tmp);
}


/*
 * Square a number and then mod the result with a second number.
 * The number to be squared can be negative or out of modulo range.
 * The result will be in the range 0 to the modulus - 1.
 */
void
zsquaremod(z1, z2, res)
	ZVALUE z1;		/* number to be squared */
	ZVALUE z2;		/* number to take mod with */
	ZVALUE *res;		/* result */
{
	ZVALUE tmp;
	FULL prod;
	FULL digit;

	if (ziszero(z2) || zisneg(z2))
		math_error("Mod of non-positive integer");
	if (ziszero(z1) || zisunit(z2)) {
		*res = _zero_;
		return;
	}

	/*
	 * If the modulus is a single digit number, then do the result
	 * cheaply.  Check especially for a small power of two.
	 */
	if (zistiny(z2)) {
		digit = z2.v[0];
		if ((digit & -digit) == digit) {	/* NEEDS 2'S COMP */
			prod = (FULL) z1.v[0];
			prod = (prod * prod) & (digit - 1);
		} else {
			z1.sign = 0;
			prod = (FULL) zmodi(z1, (long) digit);
			prod = (prod * prod) % digit;
		}
		itoz((long) prod, res);
		return;
	}

	/*
	 * The modulus is more than one digit.
	 * Actually do the square and divide if necessary.
	 */
	zsquare(z1, &tmp);
	if ((tmp.len < z2.len) ||
		((tmp.len == z2.len) && (tmp.v[tmp.len-1] < z2.v[z2.len-1]))) {
			*res = tmp;
			return;
	}
	zmod(tmp, z2, res);
	zfree(tmp);
}


/*
 * Add two numbers together and then mod the result with a third number.
 * The two numbers to be added can be negative or out of modulo range.
 * The result will be in the range 0 to the modulus - 1.
 */
static void
zaddmod(z1, z2, z3, res)
	ZVALUE z1;		/* first number to be added */
	ZVALUE z2;		/* second number to be added */
	ZVALUE z3;		/* number to take mod with */
	ZVALUE *res;		/* result */
{
	ZVALUE tmp;
	FULL sumdigit;
	FULL moddigit;

	if (ziszero(z3) || zisneg(z3))
		math_error("Mod of non-positive integer");
	if ((ziszero(z1) && ziszero(z2)) || zisunit(z3)) {
		*res = _zero_;
		return;
	}
	if (zistwo(z2)) {
		if ((z1.v[0] + z2.v[0]) & 0x1)
			*res = _one_;
		else
			*res = _zero_;
		return;
	}
	zadd(z1, z2, &tmp);
	if (zisneg(tmp) || (tmp.len > z3.len)) {
		zmod(tmp, z3, res);
		zfree(tmp);
		return;
	}
	sumdigit = tmp.v[tmp.len - 1];
	moddigit = z3.v[z3.len - 1];
	if ((tmp.len < z3.len) || (sumdigit < moddigit)) {
		*res = tmp;
		return;
	}
	if (sumdigit < 2 * moddigit) {
		zsub(tmp, z3, res);
		zfree(tmp);
		return;
	}
	zmod(tmp, z2, res);
	zfree(tmp);
}


/*
 * Subtract two numbers together and then mod the result with a third number.
 * The two numbers to be subtract can be negative or out of modulo range.
 * The result will be in the range 0 to the modulus - 1.
 */
void
zsubmod(z1, z2, z3, res)
	ZVALUE z1;		/* number to be subtracted from */
	ZVALUE z2;		/* number to be subtracted */
	ZVALUE z3;		/* number to take mod with */
	ZVALUE *res;		/* result */
{
	if (ziszero(z3) || zisneg(z3))
		math_error("Mod of non-positive integer");
	if (ziszero(z2)) {
		zmod(z1, z3, res);
		return;
	}
	if (ziszero(z1)) {
		znegmod(z2, z3, res);
		return;
	}
	if ((z1.sign == z2.sign) && (z1.len == z2.len) &&
		(z1.v[0] == z2.v[0]) && (zcmp(z1, z2) == 0)) {
			*res = _zero_;
			return;
	}
	z2.sign = !z2.sign;
	zaddmod(z1, z2, z3, res);
}


/*
 * Calculate the negative of a number modulo another number.
 * The number to be negated can be negative or out of modulo range.
 * The result will be in the range 0 to the modulus - 1.
 */
static void
znegmod(z1, z2, res)
	ZVALUE z1;		/* number to take negative of */
	ZVALUE z2;		/* number to take mod with */
	ZVALUE *res;		/* result */
{
	int sign;
	int cv;

	if (ziszero(z2) || zisneg(z2))
		math_error("Mod of non-positive integer");
	if (ziszero(z1) || zisunit(z2)) {
		*res = _zero_;
		return;
	}
	if (zistwo(z2)) {
		if (z1.v[0] & 0x1)
			*res = _one_;
		else
			*res = _zero_;
		return;
	}

	/*
	 * If the absolute value of the number is within the modulo range,
	 * then the result is just a copy or a subtraction.  Otherwise go
	 * ahead and negate and reduce the result.
	 */
	sign = z1.sign;
	z1.sign = 0;
	cv = zrel(z1, z2);
	if (cv == 0) {
		*res = _zero_;
		return;
	}
	if (cv < 0) {
		if (sign)
			zcopy(z1, res);
		else
			zsub(z2, z1, res);
		return;
	}
	z1.sign = !sign;
	zmod(z1, z2, res);
}
#endif


/*
 * Calculate the number congruent to the given number whose absolute
 * value is minimal.  The number to be reduced can be negative or out of
 * modulo range.  The result will be within the range -int((modulus-1)/2)
 * to int(modulus/2) inclusive.  For example, for modulus 7, numbers are
 * reduced to the range [-3, 3], and for modulus 8, numbers are reduced to
 * the range [-3, 4].
 */
void
zminmod(z1, z2, res)
	ZVALUE z1;		/* number to find minimum congruence of */
	ZVALUE z2;		/* number to take mod with */
	ZVALUE *res;		/* result */
{
	ZVALUE tmp1, tmp2;
	int sign;
	int cv;

	if (ziszero(z2) || zisneg(z2))
		math_error("Mod of non-positive integer");
	if (ziszero(z1) || zisunit(z2)) {
		*res = _zero_;
		return;
	}
	if (zistwo(z2)) {
		if (zisodd(z1))
			*res = _one_;
		else
			*res = _zero_;
		return;
	}

	/*
	 * Do a quick check to see if the number is very small compared
	 * to the modulus.  If so, then the result is obvious.
	 */
	if (z1.len < z2.len - 1) {
		zcopy(z1, res);
		return;
	}

	/*
	 * Now make sure the input number is within the modulo range.
	 * If not, then reduce it to be within range and make the
	 * quick check again.
	 */
	sign = z1.sign;
	z1.sign = 0;
	cv = zrel(z1, z2);
	if (cv == 0) {
		*res = _zero_;
		return;
	}
	tmp1 = z1;
	if (cv > 0) {
		z1.sign = (BOOL)sign;
		zmod(z1, z2, &tmp1);
		if (tmp1.len < z2.len - 1) {
			*res = tmp1;
			return;
		}
		sign = 0;
	}

	/*
	 * Now calculate the difference of the modulus and the absolute
	 * value of the original number.  Compare the original number with
	 * the difference, and return the one with the smallest absolute
	 * value, with the correct sign.  If the two values are equal, then
	 * return the positive result.
	 */
	zsub(z2, tmp1, &tmp2);
	cv = zrel(tmp1, tmp2);
	if (cv < 0) {
		zfree(tmp2);
		tmp1.sign = (BOOL)sign;
		if (tmp1.v == z1.v)
			zcopy(tmp1, res);
		else
			*res = tmp1;
	} else {
		if (cv)
			tmp2.sign = !sign;
		if (tmp1.v != z1.v)
			zfree(tmp1);
		*res = tmp2;
	}
}


/*
 * Compare two numbers for equality modulo a third number.
 * The two numbers to be compared can be negative or out of modulo range.
 * Returns TRUE if the numbers are not congruent, and FALSE if they are
 * congruent.
 */
BOOL
zcmpmod(z1, z2, z3)
	ZVALUE z1;		/* first number to be compared */
	ZVALUE z2;		/* second number to be compared */
	ZVALUE z3;		/* modulus */
{
	ZVALUE tmp1, tmp2, tmp3;
	FULL digit;
	LEN len;
	int cv;

	if (zisneg(z3) || ziszero(z3))
		math_error("Non-positive modulus in zcmpmod");
	if (zistwo(z3))
		return (((z1.v[0] + z2.v[0]) & 0x1) != 0);

	/*
	 * If the two numbers are equal, then their mods are equal.
	 */
	if ((z1.sign == z2.sign) && (z1.len == z2.len) &&
		(z1.v[0] == z2.v[0]) && (zcmp(z1, z2) == 0))
			return FALSE;

	/*
	 * If both numbers are negative, then we can make them positive.
	 */
	if (zisneg(z1) && zisneg(z2)) {
		z1.sign = 0;
		z2.sign = 0;
	}

	/*
	 * For small negative numbers, make them positive before comparing.
	 * In any case, the resulting numbers are in tmp1 and tmp2.
	 */
	tmp1 = z1;
	tmp2 = z2;
	len = z3.len;
	digit = z3.v[len - 1];

	if (zisneg(z1) && ((z1.len < len) ||
		((z1.len == len) && (z1.v[z1.len - 1] < digit))))
			zadd(z1, z3, &tmp1);

	if (zisneg(z2) && ((z2.len < len) ||
		((z2.len == len) && (z2.v[z2.len - 1] < digit))))
			zadd(z2, z3, &tmp2);

	/*
	 * Now compare the two numbers for equality.
	 * If they are equal we are all done.
	 */
	if (zcmp(tmp1, tmp2) == 0) {
		if (tmp1.v != z1.v)
			zfree(tmp1);
		if (tmp2.v != z2.v)
			zfree(tmp2);
		return FALSE;
	}

	/*
	 * They are not identical.  Now if both numbers are positive
	 * and less than the modulus, then they are definitely not equal.
	 */
	if ((tmp1.sign == tmp2.sign) &&
		((tmp1.len < len) || (zrel(tmp1, z3) < 0)) &&
		((tmp2.len < len) || (zrel(tmp2, z3) < 0)))
	{
		if (tmp1.v != z1.v)
			zfree(tmp1);
		if (tmp2.v != z2.v)
			zfree(tmp2);
		return TRUE;
	}

	/*
	 * Either one of the numbers is negative or is large.
	 * So do the standard thing and subtract the two numbers.
	 * Then they are equal if the result is 0 (mod z3).
	 */
	zsub(tmp1, tmp2, &tmp3);
	if (tmp1.v != z1.v)
		zfree(tmp1);
	if (tmp2.v != z2.v)
		zfree(tmp2);

	/*
	 * Compare the result with the modulus to see if it is equal to
	 * or less than the modulus.  If so, we know the mod result.
	 */
	tmp3.sign = 0;
	cv = zrel(tmp3, z3);
	if (cv == 0) {
		zfree(tmp3);
		return FALSE;
	}
	if (cv < 0) {
		zfree(tmp3);
		return TRUE;
	}

	/*
	 * We are forced to actually do the division.
	 * The numbers are congruent if the result is zero.
	 */
	zmod(tmp3, z3, &tmp1);
	zfree(tmp3);
	if (ziszero(tmp1)) {
		zfree(tmp1);
		return FALSE;
	} else {
		zfree(tmp1);
		return TRUE;
	}
}


/*
 * Compute the result of raising one number to a power modulo another number.
 * That is, this computes:  a^b (modulo c).
 * This calculates the result by examining the power POWBITS bits at a time,
 * using a small table of POWNUMS low powers to calculate powers for those bits,
 * and repeated squaring and multiplying by the partial powers to generate
 * the complete power.  If the power being raised to is high enough, then
 * this uses the REDC algorithm to avoid doing many divisions.  When using
 * REDC, multiple calls to this routine using the same modulus will be
 * slightly faster.
 */
void
zpowermod(z1, z2, z3, res)
	ZVALUE z1, z2, z3, *res;
{
	HALF *hp;		/* pointer to current word of the power */
	REDC *rp;		/* REDC information to be used */
	ZVALUE *pp;		/* pointer to low power table */
	ZVALUE ans, temp;	/* calculation values */
	ZVALUE modpow;		/* current small power */
	ZVALUE lowpowers[POWNUMS];	/* low powers */
	int sign;		/* original sign of number */
	int curshift;		/* shift value for word of power */
	HALF curhalf;		/* current word of power */
	unsigned int curpow;	/* current low power */
	unsigned int curbit;	/* current bit of low power */
	int i;

	if (zisneg(z3) || ziszero(z3))
		math_error("Non-positive modulus in zpowermod");
	if (zisneg(z2))
		math_error("Negative power in zpowermod");

	sign = z1.sign;
	z1.sign = 0;

	/*
	 * Check easy cases first.
	 */
	if ((ziszero(z1) && !ziszero(z2)) || zisunit(z3)) {
		/* 0^(non_zero) or x^y mod 1 always produces zero */
		*res = _zero_;
		return;
	}
	if (ziszero(z2)) {			/* x^0 == 1 */
		*res = _one_;
		return;
	}
	if (zistwo(z3)) {			/* mod 2 */
		if (zisodd(z1))
			*res = _one_;
		else
			*res = _zero_;
		return;
	}
	if (zisunit(z1) && (!sign || ziseven(z2))) {
		/* 1^x or (-1)^(2x) */
		*res = _one_;
		return;
	}

	/*
	 * Normalize the number being raised to be non-negative and to lie
	 * within the modulo range.  Then check for zero or one specially.
	 */
	zmod(z1, z3, &temp);
	if (ziszero(temp)) {
		zfree(temp);
		*res = _zero_;
		return;
	}
	z1 = temp;
	if (sign) {
		zsub(z3, z1, &temp);
		zfree(z1);
		z1 = temp;
	}
	if (zisunit(z1)) {
		zfree(z1);
		*res = _one_;
		return;
	}

	/*
	 * If the modulus is odd, large enough, is not one less than an
	 * exact power of two, and if the power is large enough, then use
	 * the REDC algorithm.  The size where this is done is configurable.
	 */
	if ((z2.len > 1) && (z3.len >= _pow2_) && zisodd(z3)
		&& !zisallbits(z3))
	{
		if (powermodredc && zcmp(powermodredc->mod, z3)) {
			zredcfree(powermodredc);
			powermodredc = NULL;
		}
		if (powermodredc == NULL)
			powermodredc = zredcalloc(z3);
		rp = powermodredc;
		zredcencode(rp, z1, &temp);
		zredcpower(rp, temp, z2, &z1);
		zfree(temp);
		zredcdecode(rp, z1, res);
		zfree(z1);
		return;
	}

	/*
	 * Modulus or power is small enough to perform the power raising
	 * directly.  Initialize the table of powers.
	 */
	for (pp = &lowpowers[2]; pp < &lowpowers[POWNUMS]; pp++)
		pp->len = 0;
	lowpowers[0] = _one_;
	lowpowers[1] = z1;
	ans = _one_;

	hp = &z2.v[z2.len - 1];
	curhalf = *hp;
	curshift = BASEB - POWBITS;
	while (curshift && ((curhalf >> curshift) == 0))
		curshift -= POWBITS;

	/*
	 * Calculate the result by examining the power POWBITS bits at a time,
	 * and use the table of low powers at each iteration.
	 */
	for (;;) {
		curpow = (curhalf >> curshift) & (POWNUMS - 1);
		pp = &lowpowers[curpow];

		/*
		 * If the small power is not yet saved in the table, then
		 * calculate it and remember it in the table for future use.
		 */
		if (pp->len == 0) {
			if (curpow & 0x1)
				zcopy(z1, &modpow);
			else
				modpow = _one_;

			for (curbit = 0x2; curbit <= curpow; curbit *= 2) {
				pp = &lowpowers[curbit];
				if (pp->len == 0) {
					zsquare(lowpowers[curbit/2], &temp);
					zmod(temp, z3, pp);
					zfree(temp);
				}
				if (curbit & curpow) {
					zmul(*pp, modpow, &temp);
					zfree(modpow);
					zmod(temp, z3, &modpow);
					zfree(temp);
				}
			}
			pp = &lowpowers[curpow];
			*pp = modpow;
		}

		/*
		 * If the power is nonzero, then accumulate the small power
		 * into the result.
		 */
		if (curpow) {
			zmul(ans, *pp, &temp);
			zfree(ans);
			zmod(temp, z3, &ans);
			zfree(temp);
		}

		/*
		 * Select the next POWBITS bits of the power, if there is
		 * any more to generate.
		 */
		curshift -= POWBITS;
		if (curshift < 0) {
			if (hp-- == z2.v)
				break;
			curhalf = *hp;
			curshift = BASEB - POWBITS;
		}

		/*
		 * Square the result POWBITS times to make room for the next
		 * chunk of bits.
		 */
		for (i = 0; i < POWBITS; i++) {
			zsquare(ans, &temp);
			zfree(ans);
			zmod(temp, z3, &ans);
			zfree(temp);
		}
	}

	for (pp = &lowpowers[2]; pp < &lowpowers[POWNUMS]; pp++) {
		if (pp->len)
			freeh(pp->v);
	}
	*res = ans;
}


/*
 * Initialize the REDC algorithm for a particular modulus,
 * returning a pointer to a structure that is used for other
 * REDC calls.  An error is generated if the structure cannot
 * be allocated.  The modulus must be odd and positive.
 */
REDC *
zredcalloc(z1)
	ZVALUE z1;		/* modulus to initialize for */
{
	REDC *rp;		/* REDC information */
	ZVALUE tmp;
	long bit;

	if (ziseven(z1) || zisneg(z1))
		math_error("REDC requires positive odd modulus");

	rp = (REDC *) malloc(sizeof(REDC));
	if (rp == NULL)
		math_error("Cannot allocate REDC structure");

	/*
	 * Round up the binary modulus to the next power of two
	 * which is at a word boundary.  Then the shift and modulo
	 * operations mod the binary modulus can be done very cheaply.
	 * Calculate the REDC format for the number 1 for future use.
	 */
	bit = zhighbit(z1) + 1;
	if (bit % BASEB)
		bit += (BASEB - (bit % BASEB));
	zcopy(z1, &rp->mod);
	zbitvalue(bit, &tmp);
	z1.sign = 1;
	(void) zmodinv(z1, tmp, &rp->inv);
	zmod(tmp, rp->mod, &rp->one);
	zfree(tmp);
	rp->len = bit / BASEB;
	return rp;
}


/*
 * Free any numbers associated with the specified REDC structure,
 * and then the REDC structure itself.
 */
void
zredcfree(rp)
	REDC *rp;		/* REDC information to be cleared */
{
	zfree(rp->mod);
	zfree(rp->inv);
	zfree(rp->one);
	free(rp);
}


/*
 * Convert a normal number into the specified REDC format.
 * The number to be converted can be negative or out of modulo range.
 * The resulting number can be used for multiplying, adding, subtracting,
 * or comparing with any other such converted numbers, as if the numbers
 * were being calculated modulo the number which initialized the REDC
 * information.  When the final value is unconverted, the result is the
 * same as if the usual operations were done with the original numbers.
 */
void
zredcencode(rp, z1, res)
	REDC *rp;		/* REDC information */
	ZVALUE z1;		/* number to be converted */
	ZVALUE *res;		/* returned converted number */
{
	ZVALUE tmp1, tmp2;

	/*
	 * Handle the cases 0, 1, -1, and 2 specially since these are
	 * easy to calculate.  Zero transforms to zero, and the others
	 * can be obtained from the precomputed REDC format for 1 since
	 * addition and subtraction act normally for REDC format numbers.
	 */
	if (ziszero(z1)) {
		*res = _zero_;
		return;
	}
	if (zisone(z1)) {
		zcopy(rp->one, res);
		return;
	}
	if (zisunit(z1)) {
		zsub(rp->mod, rp->one, res);
		return;
	}
	if (zistwo(z1)) {
		zadd(rp->one, rp->one, &tmp1);
		if (zrel(tmp1, rp->mod) < 0) {
			*res = tmp1;
			return;
		}
		zsub(tmp1, rp->mod, res);
		zfree(tmp1);
		return;
	}

	/*
	 * Not a trivial number to convert, so do the full transformation.
	 * Convert negative numbers to positive numbers before converting.
	 */
	tmp1.len = 0;
	if (zisneg(z1)) {
		zmod(z1, rp->mod, &tmp1);
		z1 = tmp1;
	}
	zshift(z1, rp->len * BASEB, &tmp2);
	if (tmp1.len)
		zfree(tmp1);
	zmod(tmp2, rp->mod, res);
	zfree(tmp2);
}


/*
 * The REDC algorithm used to convert numbers out of REDC format and also
 * used after multiplication of two REDC numbers.  Using this routine
 * avoids any divides, replacing the divide by two multiplications.
 * If the numbers are very large, then these two multiplies will be
 * quicker than the divide, since dividing is harder than multiplying.
 */
void
zredcdecode(rp, z1, res)
	REDC *rp;		/* REDC information */
	ZVALUE z1;		/* number to be transformed */
	ZVALUE *res;		/* returned transformed number */
{
	ZVALUE tmp1, tmp2;	/* temporaries */
	HALF *hp;		/* saved pointer to tmp2 value */

	if (zisneg(z1))
		math_error("Negative number for zredc");

	/*
	 * Check first for the special values for 0 and 1 that are easy.
	 */
	if (ziszero(z1)) {
		*res = _zero_;
		return;
	}
	if ((z1.len == rp->one.len) && (z1.v[0] == rp->one.v[0]) &&
		(zcmp(z1, rp->one) == 0)) {
			*res = _one_;
			return;
	}

	/*
	 * First calculate the following:
	 * 	tmp2 = ((z1 % 2^bitnum) * inv) % 2^bitnum.
	 * The mod operations can be done with no work since the bit
	 * number was selected as a multiple of the word size.  Just
	 * reduce the sizes of the numbers as required.
	 */
	tmp1 = z1;
	if (tmp1.len > rp->len)
		tmp1.len = rp->len;
	zmul(tmp1, rp->inv, &tmp2);
	if (tmp2.len > rp->len)
		tmp2.len = rp->len;

	/*
	 * Next calculate the following:
	 *	res = (z1 + tmp2 * modulus) / 2^bitnum
	 * The division by a power of 2 is always exact, and requires no
	 * work.  Just adjust the address and length of the number to do
	 * the divide, but save the original pointer for freeing later.
	 */
	zmul(tmp2, rp->mod, &tmp1);
	zfree(tmp2);
	zadd(z1, tmp1, &tmp2);
	zfree(tmp1);
	hp = tmp2.v;
	if (tmp2.len <= rp->len) {
		freeh(hp);
		*res = _zero_;
		return;
	}
	tmp2.v += rp->len;
	tmp2.len -= rp->len;

	/*
	 * Finally do a final modulo by a simple subtraction if necessary.
	 * This is all that is needed because the previous calculation is
	 * guaranteed to always be less than twice the modulus.
	 */
	if (zrel(tmp2, rp->mod) < 0)
		zcopy(tmp2, res);
	else
		zsub(tmp2, rp->mod, res);
	freeh(hp);
}


/*
 * Multiply two numbers in REDC format together producing a result also
 * in REDC format.  If the result is converted back to a normal number,
 * then the result is the same as the modulo'd multiplication of the
 * original numbers before they were converted to REDC format.  This
 * calculation is done in one of two ways, depending on the size of the
 * modulus.  For large numbers, the REDC definition is used directly
 * which involves three multiplies overall.  For small numbers, a
 * complicated routine is used which does the indicated multiplication
 * and the REDC algorithm at the same time to produce the result.
 */
void
zredcmul(rp, z1, z2, res)
	REDC *rp;		/* REDC information */
	ZVALUE z1;		/* first REDC number to be multiplied */
	ZVALUE z2;		/* second REDC number to be multiplied */
	ZVALUE *res;		/* resulting REDC number */
{
	FULL mulb;
	FULL muln;
	HALF *h1;
	HALF *h2;
	HALF *h3;
	HALF *hd;
	HALF Ninv;
	HALF topdigit = 0;
	LEN modlen;
	LEN len;
	LEN len2;
	SIUNION sival1;
	SIUNION sival2;
	SIUNION sival3;
	SIUNION carry;
	ZVALUE tmp;

	if (zisneg(z1) || (z1.len > rp->mod.len) ||
		zisneg(z2) || (z2.len > rp->mod.len))
			math_error("Negative or too large number in zredcmul");

	/*
	 * Check for special values which we easily know the answer.
	 */
	if (ziszero(z1) || ziszero(z2)) {
		*res = _zero_;
		return;
	}

	if ((z1.len == rp->one.len) && (z1.v[0] == rp->one.v[0]) &&
		(zcmp(z1, rp->one) == 0)) {
			zcopy(z2, res);
			return;
	}

	if ((z2.len == rp->one.len) && (z2.v[0] == rp->one.v[0]) &&
		(zcmp(z2, rp->one) == 0)) {
			zcopy(z1, res);
			return;
	}

	/*
	 * If the size of the modulus is large, then just do the multiply,
	 * followed by the two multiplies contained in the REDC routine.
	 * This will be quicker than directly doing the REDC calculation
	 * because of the O(N^1.585) speed of the multiplies.  The size
	 * of the number which this is done is configurable.
	 */
	if (rp->mod.len >= _redc2_) {
		zmul(z1, z2, &tmp);
		zredcdecode(rp, tmp, res);
		zfree(tmp);
		return;
	}

	/*
	 * The number is small enough to calculate by doing the O(N^2) REDC
	 * algorithm directly.  This algorithm performs the multiplication and
	 * the reduction at the same time.  Notice the obscure facts that
	 * only the lowest word of the inverse value is used, and that
	 * there is no shifting of the partial products as there is in a
	 * normal multiply.
	 */
	modlen = rp->mod.len;
	Ninv = rp->inv.v[0];

	/*
	 * Allocate the result and clear it.
	 * The size of the result will be equal to or smaller than
	 * the modulus size.
	 */
	res->sign = 0;
	res->len = modlen;
	res->v = alloc(modlen);

	hd = res->v;
	len = modlen;
	zclearval(*res);

	/*
	 * Do this outermost loop over all the digits of z1.
	 */
	h1 = z1.v;
	len = z1.len;
	while (len--) {
		/*
		 * Start off with the next digit of z1, the first
		 * digit of z2, and the first digit of the modulus.
		 */
		mulb = (FULL) *h1++;
		h2 = z2.v;
		h3 = rp->mod.v;
		hd = res->v;
		sival1.ivalue = mulb * ((FULL) *h2++) + ((FULL) *hd++);
		muln = ((HALF) (sival1.silow * Ninv));
		sival2.ivalue = muln * ((FULL) *h3++);
		sival3.ivalue = ((FULL) sival1.silow) + ((FULL) sival2.silow);
		carry.ivalue = ((FULL) sival1.sihigh) + ((FULL) sival2.sihigh)
			+ ((FULL) sival3.sihigh);

		/*
		 * Do this innermost loop for each digit of z2, except
		 * for the first digit which was just done above.
		 */
		len2 = z2.len;
		while (--len2 > 0) {
			sival1.ivalue = mulb * ((FULL) *h2++);
			sival2.ivalue = muln * ((FULL) *h3++);
			sival3.ivalue = ((FULL) sival1.silow)
				+ ((FULL) sival2.silow)
				+ ((FULL) *hd)
				+ ((FULL) carry.silow);
			carry.ivalue = ((FULL) sival1.sihigh)
				+ ((FULL) sival2.sihigh)
				+ ((FULL) sival3.sihigh)
				+ ((FULL) carry.sihigh);

			hd[-1] = sival3.silow;
			hd++;
		}

		/*
		 * Now continue the loop as necessary so the total number
		 * of interations is equal to the size of the modulus.
		 * This acts as if the innermost loop was repeated for
		 * high digits of z2 that are zero.
		 */
		len2 = modlen - z2.len;
		while (len2--) {
			sival2.ivalue = muln * ((FULL) *h3++);
			sival3.ivalue = ((FULL) sival2.silow)
				+ ((FULL) *hd)
				+ ((FULL) carry.silow);
			carry.ivalue = ((FULL) sival2.sihigh)
				+ ((FULL) sival3.sihigh)
				+ ((FULL) carry.sihigh);

			hd[-1] = sival3.silow;
			hd++;
		}

		res->v[modlen - 1] = carry.silow;
		topdigit = carry.sihigh;
	}

	/*
	 * Now continue the loop as necessary so the total number
	 * of interations is equal to the size of the modulus.
	 * This acts as if the outermost loop was repeated for high
	 * digits of z1 that are zero.
	 */
	len = modlen - z1.len;
	while (len--) {
		/*
		 * Start off with the first digit of the modulus.
		 */
		h3 = rp->mod.v;
		hd = res->v;
		muln = ((HALF) (*hd * Ninv));
		sival2.ivalue = muln * ((FULL) *h3++);
		sival3.ivalue = ((FULL) *hd++) + ((FULL) sival2.silow);
		carry.ivalue = ((FULL) sival2.sihigh) + ((FULL) sival3.sihigh);

		/*
		 * Do this innermost loop for each digit of the modulus,
		 * except for the first digit which was just done above.
		 */
		len2 = modlen;
		while (--len2 > 0) {
			sival2.ivalue = muln * ((FULL) *h3++);
			sival3.ivalue = ((FULL) sival2.silow)
				+ ((FULL) *hd)
				+ ((FULL) carry.silow);
			carry.ivalue = ((FULL) sival2.sihigh)
				+ ((FULL) sival3.sihigh)
				+ ((FULL) carry.sihigh);

			hd[-1] = sival3.silow;
			hd++;
		}
		res->v[modlen - 1] = carry.silow;
		topdigit = carry.sihigh;
	}

	/*
	 * Determine the true size of the result, taking the top digit of
	 * the current result into account.  The top digit is not stored in
	 * the number because it is temporary and would become zero anyway
	 * after the final subtraction is done.
	 */
	if (topdigit == 0) {
		len = modlen;
		hd = &res->v[len - 1];
		while ((*hd == 0) && (len > 1)) {
			hd--;
			len--;
		}
		res->len = len;
	}

	/*
	 * Compare the result with the modulus.
	 * If it is less than the modulus, then the calculation is complete.
	 */
	if ((topdigit == 0) && ((len < modlen)
		|| (res->v[len - 1] < rp->mod.v[len - 1])
		|| (zrel(*res, rp->mod) < 0)))
			return;

	/*
	 * Do a subtraction to reduce the result to a value less than
	 * the modulus.  The REDC algorithm guarantees that a single subtract
	 * is all that is needed.  Ignore any borrowing from the possible
	 * highest word of the current result because that would affect
	 * only the top digit value that was not stored and would become
	 * zero anyway.
	 */
	carry.ivalue = 0;
	h1 = rp->mod.v;
	hd = res->v;
	len = modlen;
	while (len--) {
		carry.ivalue = BASE1 - ((FULL) *hd) + ((FULL) *h1++)
			+ ((FULL) carry.silow);
		*hd++ = BASE1 - carry.silow;
		carry.silow = carry.sihigh;
	}

	/*
	 * Now finally recompute the size of the result.
	 */
	len = modlen;
	hd = &res->v[len - 1];
	while ((*hd == 0) && (len > 1)) {
		hd--;
		len--;
	}
	res->len = len;
}


/*
 * Square a number in REDC format producing a result also in REDC format.
 */
void
zredcsquare(rp, z1, res)
	REDC *rp;		/* REDC information */
	ZVALUE z1;		/* REDC number to be squared */
	ZVALUE *res;		/* resulting REDC number */
{
	ZVALUE tmp;

	if (zisneg(z1))
		math_error("Negative number in zredcsquare");
	if (ziszero(z1)) {
		*res = _zero_;
		return;
	}
	if ((z1.len == rp->one.len) && (z1.v[0] == rp->one.v[0]) &&
		(zcmp(z1, rp->one) == 0)) {
			zcopy(z1, res);
			return;
	}

	/*
	 * If the modulus is small enough, then call the multiply
	 * routine to produce the result.  Otherwise call the O(N^1.585)
	 * routines to get the answer.
	 */
	if (rp->mod.len < _redc2_) {
		zredcmul(rp, z1, z1, res);
		return;
	}
	zsquare(z1, &tmp);
	zredcdecode(rp, tmp, res);
	zfree(tmp);
}


/*
 * Compute the result of raising a REDC format number to a power.
 * The result is within the range 0 to the modulus - 1.
 * This calculates the result by examining the power POWBITS bits at a time,
 * using a small table of POWNUMS low powers to calculate powers for those bits,
 * and repeated squaring and multiplying by the partial powers to generate
 * the complete power.
 */
void
zredcpower(rp, z1, z2, res)
	REDC *rp;		/* REDC information */
	ZVALUE z1;		/* REDC number to be raised */
	ZVALUE z2;		/* normal number to raise number to */
	ZVALUE *res;		/* result */
{
	HALF *hp;		/* pointer to current word of the power */
	ZVALUE *pp;		/* pointer to low power table */
	ZVALUE ans, temp;	/* calculation values */
	ZVALUE modpow;		/* current small power */
	ZVALUE lowpowers[POWNUMS];	/* low powers */
	int curshift;		/* shift value for word of power */
	HALF curhalf;		/* current word of power */
	unsigned int curpow;	/* current low power */
	unsigned int curbit;	/* current bit of low power */
	int i;

	if (zisneg(z1))
		math_error("Negative number in zredcpower");
	if (zisneg(z2))
		math_error("Negative power in zredcpower");

	/*
	 * Check for zero or the REDC format for one.
	 */
	if (ziszero(z1) || zisunit(rp->mod)) {
		*res = _zero_;
		return;
	}
	if (zcmp(z1, rp->one) == 0) {
		zcopy(rp->one, res);
		return;
	}

	/*
	 * See if the number being raised is the REDC format for -1.
	 * If so, then the answer is the REDC format for one or minus one.
	 * To do this check, calculate the REDC format for -1.
	 */
	if (((HALF)(z1.v[0] + rp->one.v[0])) == rp->mod.v[0]) {
		zsub(rp->mod, rp->one, &temp);
		if (zcmp(z1, temp) == 0) {
			if (zisodd(z2)) {
				*res = temp;
				return;
			}
			zfree(temp);
			zcopy(rp->one, res);
			return;
		}
		zfree(temp);
	}

	for (pp = &lowpowers[2]; pp < &lowpowers[POWNUMS]; pp++)
		pp->len = 0;
	zcopy(rp->one, &lowpowers[0]);
	zcopy(z1, &lowpowers[1]);
	zcopy(rp->one, &ans);

	hp = &z2.v[z2.len - 1];
	curhalf = *hp;
	curshift = BASEB - POWBITS;
	while (curshift && ((curhalf >> curshift) == 0))
		curshift -= POWBITS;

	/*
	 * Calculate the result by examining the power POWBITS bits at a time,
	 * and use the table of low powers at each iteration.
	 */
	for (;;) {
		curpow = (curhalf >> curshift) & (POWNUMS - 1);
		pp = &lowpowers[curpow];

		/*
		 * If the small power is not yet saved in the table, then
		 * calculate it and remember it in the table for future use.
		 */
		if (pp->len == 0) {
			if (curpow & 0x1)
				zcopy(z1, &modpow);
			else
				zcopy(rp->one, &modpow);

			for (curbit = 0x2; curbit <= curpow; curbit *= 2) {
				pp = &lowpowers[curbit];
				if (pp->len == 0)
					zredcsquare(rp, lowpowers[curbit/2],
						pp);
				if (curbit & curpow) {
					zredcmul(rp, *pp, modpow, &temp);
					zfree(modpow);
					modpow = temp;
				}
			}
			pp = &lowpowers[curpow];
			*pp = modpow;
		}

		/*
		 * If the power is nonzero, then accumulate the small power
		 * into the result.
		 */
		if (curpow) {
			zredcmul(rp, ans, *pp, &temp);
			zfree(ans);
			ans = temp;
		}

		/*
		 * Select the next POWBITS bits of the power, if there is
		 * any more to generate.
		 */
		curshift -= POWBITS;
		if (curshift < 0) {
			if (hp-- == z2.v)
				break;
			curhalf = *hp;
			curshift = BASEB - POWBITS;
		}

		/*
		 * Square the result POWBITS times to make room for the next
		 * chunk of bits.
		 */
		for (i = 0; i < POWBITS; i++) {
			zredcsquare(rp, ans, &temp);
			zfree(ans);
			ans = temp;
		}
	}

	for (pp = lowpowers; pp < &lowpowers[POWNUMS]; pp++) {
		if (pp->len)
			freeh(pp->v);
	}
	*res = ans;
}

/* END CODE */
