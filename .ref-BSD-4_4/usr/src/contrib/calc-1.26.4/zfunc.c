/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Extended precision integral arithmetic non-primitive routines
 */

#include "math.h"

static ZVALUE primeprod;		/* product of primes under 100 */
ZVALUE _tenpowers_[32];			/* table of 10^2^n */

#if 0
static char *abortmsg = "Calculation aborted";
static char *memmsg = "Not enough memory";
#endif


/*
 * Compute the factorial of a number.
 */
void
zfact(z, dest)
	ZVALUE z, *dest;
{
	long ptwo;		/* count of powers of two */
	long n;			/* current multiplication value */
	long m;			/* reduced multiplication value */
	long mul;		/* collected value to multiply by */
	ZVALUE res, temp;

	if (isneg(z))
		error("Negative argument for factorial");
	if (isbig(z))
		error("Very large factorial");
	n = (istiny(z) ? z1tol(z) : z2tol(z));
	ptwo = 0;
	mul = 1;
	res = _one_;
	/*
	 * Multiply numbers together, but squeeze out all powers of two.
	 * We will put them back in at the end.  Also collect multiple
	 * numbers together until there is a risk of overflow.
	 */
	for (; n > 1; n--) {
		for (m = n; ((m & 0x1) == 0); m >>= 1)
			ptwo++;
		mul *= m;
		if (mul < BASE1/2)
			continue;
		zmuli(res, mul, &temp);
		freeh(res.v);
		res = temp;
		mul = 1;
	}
	/*
	 * Multiply by the remaining value, then scale result by
	 * the proper power of two.
	 */
	if (mul > 1) {
		zmuli(res, mul, &temp);
		freeh(res.v);
		res = temp;
	}
	zshift(res, ptwo, &temp);
	freeh(res.v);
	*dest = temp;
}


/*
 * Compute the product of the primes up to the specified number.
 */
void
zpfact(z, dest)
	ZVALUE z, *dest;
{
	long n;			/* limiting number to multiply by */
	long p;			/* current prime */
	long i;			/* test value */
	long mul;		/* collected value to multiply by */
	ZVALUE res, temp;

	if (isneg(z))
		error("Negative argument for factorial");
	if (isbig(z))
		error("Very large factorial");
	n = (istiny(z) ? z1tol(z) : z2tol(z));
	/*
	 * Multiply by the primes in order, collecting multiple numbers
	 * together until there is a change of overflow.
	 */
	mul = 1 + (n > 1);
	res = _one_;
	for (p = 3; p <= n; p += 2) {
		for (i = 3; (i * i) <= p; i += 2) {
			if ((p % i) == 0)
				goto next;
		}
		mul *= p;
		if (mul < BASE1/2)
			continue;
		zmuli(res, mul, &temp);
		freeh(res.v);
		res = temp;
		mul = 1;
next: ;
	}
	/*
	 * Multiply by the final value if any.
	 */
	if (mul > 1) {
		zmuli(res, mul, &temp);
		freeh(res.v);
		res = temp;
	}
	*dest = res;
}


/*
 * Compute the least common multiple of all the numbers up to the
 * specified number.
 */
void
zlcmfact(z, dest)
	ZVALUE z, *dest;
{
	long n;			/* limiting number to multiply by */
	long p;			/* current prime */
	long pp;		/* power of prime */
	long i;			/* test value */
	ZVALUE res, temp;

	if (isneg(z) || iszero(z))
		error("Non-positive argument for lcmfact");
	if (isbig(z))
		error("Very large lcmfact");
	n = (istiny(z) ? z1tol(z) : z2tol(z));
	/*
	 * Multiply by powers of the necessary odd primes in order.
	 * The power for each prime is the highest one which is not
	 * more than the specified number.
	 */
	res = _one_;
	for (p = 3; p <= n; p += 2) {
		for (i = 3; (i * i) <= p; i += 2) {
			if ((p % i) == 0)
				goto next;
		}
		i = p;
		while (i <= n) {
			pp = i;
			i *= p;
		}
		zmuli(res, pp, &temp);
		freeh(res.v);
		res = temp;
next: ;
	}
	/*
	 * Finish by scaling by the necessary power of two.
	 */
	zshift(res, zhighbit(z), dest);
	freeh(res.v);
}


/*
 * Compute the permuation function  M! / (M - N)!.
 */
void
zperm(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	long count;
	ZVALUE cur, tmp, ans;

	if (isneg(z1) || isneg(z2))
		error("Negative argument for permutation");
	if (zrel(z1, z2) < 0)
		error("Second arg larger than first in permutation");
	if (isbig(z2))
		error("Very large permutation");
	count = (istiny(z2) ? z1tol(z2) : z2tol(z2));
	zcopy(z1, &ans);
	zsub(z1, _one_, &cur);
	while (--count > 0) {
		zmul(ans, cur, &tmp);
		freeh(ans.v);
		ans = tmp;
		zsub(cur, _one_, &tmp);
		freeh(cur.v);
		cur = tmp;
	}
	freeh(cur.v);
	*res = ans;
}


/*
 * Compute the combinatorial function  M! / ( N! * (M - N)! ).
 */
void
zcomb(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	ZVALUE ans;
	ZVALUE mul, div, temp;
	FULL count, i;
	HALF dh[2];

	if (isneg(z1) || isneg(z2))
		error("Negative argument for combinatorial");
	zsub(z1, z2, &temp);
	if (isneg(temp)) {
		freeh(temp.v);
		error("Second arg larger than first for combinatorial");
	}
	if (isbig(z2) && isbig(temp)) {
		freeh(temp.v);
		error("Very large combinatorial");
	}
	count = (istiny(z2) ? z1tol(z2) : z2tol(z2));
	i = (istiny(temp) ? z1tol(temp) : z2tol(temp));
	if (isbig(z2) || (!isbig(temp) && (i < count)))
		count = i;
	freeh(temp.v);
	mul = z1;
	div.sign = 0;
	div.v = dh;
	ans = _one_;
	for (i = 1; i <= count; i++) {
		dh[0] = i & BASE1;
		dh[1] = i / BASE;
		div.len = 1 + (dh[1] != 0);
		zmul(ans, mul, &temp);
		freeh(ans.v);
		zquo(temp, div, &ans);
		freeh(temp.v);
		zsub(mul, _one_, &temp);
		if (mul.v != z1.v)
			freeh(mul.v);
		mul = temp;
	}
	if (mul.v != z1.v)
		freeh(mul.v);
	*res = ans;
}


/*
 * Perform a probabilistic primality test (algorithm P in Knuth).
 * Returns FALSE if definitely not prime, or TRUE if probably prime.
 * Count determines how many times to check for primality.
 * The chance of a non-prime passing this test is less than (1/4)^count.
 * For example, a count of 100 fails for only 1 in 10^60 numbers.
 */
BOOL
zprimetest(z, count)
	ZVALUE z;		/* number to test for primeness */
	long count;
{
	long ij, ik, ix;
	ZVALUE zm1, z1, z2, z3, ztmp;
	HALF val[2];

	z.sign = 0;
	if (iseven(z))		/* if even, not prime if not 2 */
		return (istwo(z) != 0);
	/*
	 * See if the number is small, and is either a small prime,
	 * or is divisible by a small prime.
	 */
	if (istiny(z) && (*z.v <= (HALF)(101*101-1))) {
		ix = *z.v;
		for (ik = 3; (ik <= 97) && ((ik * ik) <= ix); ik += 2)
			if ((ix % ik) == 0)
				return FALSE;
		return TRUE;
	}
	/*
	 * See if the number is divisible by one of the primes 3, 5,
	 * 7, 11, or 13.  This is a very easy check.
	 */
	ij = zmodi(z, 15015L);
	if (!(ij % 3) || !(ij % 5) || !(ij % 7) || !(ij % 11) || !(ij % 13))
		return FALSE;
	/*
	 * Check the gcd of the number and the product of more of the first
	 * few odd primes.  We must build the prime product on the first call.
	 */
	ztmp.sign = 0;
	ztmp.len = 1;
	ztmp.v = val;
	if (primeprod.len == 0) {
		val[0] = 101;
		zpfact(ztmp, &primeprod);
	}
	zgcd(z, primeprod, &z1);
	if (!isunit(z1)) {
		freeh(z1.v);
		return FALSE;
	}
	freeh(z1.v);
	/*
	 * Not divisible by a small prime, so onward with the real test.
	 * Make sure the count is limited by the number of odd numbers between
	 * three and the number being tested.
	 */
	ix = ((istiny(z) ? z1tol(z) : z2tol(z) - 3) / 2);
	if (count > ix) count = ix;
	zsub(z, _one_, &zm1);
	ik = zlowbit(zm1);
	zshift(zm1, -ik, &z1);
	/*
	 * Loop over various "random" numbers, testing each one.
	 * These numbers are the odd numbers starting from three.
	 */
	for (ix = 0; ix < count; ix++) {
		val[0] = (ix * 2) + 3;
		ij = 0;
		zpowermod(ztmp, z1, z, &z3);
		for (;;) {
			if (isone(z3)) {
				if (ij)	/* number is definitely not prime */
					goto notprime;
				break;
			}
			if (zcmp(z3, zm1) == 0)
				break;
			if (++ij >= ik)
				goto notprime;	/* number is definitely not prime */
			zsquare(z3, &z2);
			freeh(z3.v);
			zmod(z2, z, &z3);
			freeh(z2.v);
		}
		freeh(z3.v);
	}
	freeh(zm1.v);
	freeh(z1.v);
	return TRUE;	/* number might be prime */

notprime:
	freeh(z3.v);
	freeh(zm1.v);
	freeh(z1.v);
	return FALSE;
}


/*
 * Compute the Jacobi function (p / q) for odd q.
 * If q is prime then the result is:
 *	1 if p == x^2 (mod q) for some x.
 *	-1 otherwise.
 * If q is not prime, then the result is not meaningful if it is 1.
 * This function returns 0 if q is even or q < 0.
 */
FLAG
zjacobi(z1, z2)
	ZVALUE z1, z2;
{
	ZVALUE p, q, tmp;
	long lowbit;
	int val;

	if (iseven(z2) || isneg(z2))
		return 0;
	val = 1;
	if (iszero(z1) || isone(z1))
		return val;
	if (isunit(z1)) {
		if ((*z2.v - 1) & 0x2)
			val = -val;
		return val;
	}
	zcopy(z1, &p);
	zcopy(z2, &q);
	for (;;) {
		zmod(p, q, &tmp);
		freeh(p.v);
		p = tmp;
		if (iszero(p)) {
			freeh(p.v);
			p = _one_;
		}
		if (iseven(p)) {
			lowbit = zlowbit(p);
			zshift(p, -lowbit, &tmp);
			freeh(p.v);
			p = tmp;
			if ((lowbit & 1) && (((*q.v & 0x7) == 3) || ((*q.v & 0x7) == 5)))
				val = -val;
		}
		if (isunit(p)) {
			freeh(p.v);
			freeh(q.v);
			return val;
		}
		if ((*p.v & *q.v & 0x3) == 3)
			val = -val;
		tmp = q;
		q = p;
		p = tmp;
	}
}


/*
 * Return the Fibonacci number F(n).
 * This is evaluated by recursively using the formulas:
 *	F(2N+1) = F(N+1)^2 + F(N)^2
 * and
 *	F(2N) = F(N+1)^2 - F(N-1)^2
 */
void
zfib(z, res)
	ZVALUE z, *res;
{
	unsigned long i;
	long n;
	int sign;
	ZVALUE fnm1, fn, fnp1;		/* consecutive fibonacci values */
	ZVALUE t1, t2, t3;

	if (isbig(z))
		error("Very large Fibonacci number");
	n = (istiny(z) ? z1tol(z) : z2tol(z));
	if (n == 0) {
		*res = _zero_;
		return;
	}
	sign = z.sign && ((n & 0x1) == 0);
	if (n <= 2) {
		*res = _one_;
		res->sign = (BOOL)sign;
		return;
	}
	i = TOPFULL;
	while ((i & n) == 0)
		i >>= 1L;
	i >>= 1L;
	fnm1 = _zero_;
	fn = _one_;
	fnp1 = _one_;
	while (i) {
		zsquare(fnm1, &t1);
		zsquare(fn, &t2);
		zsquare(fnp1, &t3);
		freeh(fnm1.v);
		freeh(fn.v);
		freeh(fnp1.v);
		zadd(t2, t3, &fnp1);
		zsub(t3, t1, &fn);
		freeh(t1.v);
		freeh(t2.v);
		freeh(t3.v);
		if (i & n) {
			fnm1 = fn;
			fn = fnp1;
			zadd(fnm1, fn, &fnp1);
		} else
			zsub(fnp1, fn, &fnm1);
		i >>= 1L;
	}
	freeh(fnm1.v);
	freeh(fnp1.v);
	*res = fn;
	res->sign = (BOOL)sign;
}


/*
 * Compute the result of raising one number to the power of another
 * The second number is assumed to be non-negative.
 * It cannot be too large except for trivial cases.
 */
void
zpowi(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	int sign;		/* final sign of number */
	unsigned long power;	/* power to raise to */
	unsigned long bit;	/* current bit value */
	long twos;		/* count of times 2 is in result */
	ZVALUE ans, temp;

	sign = (z1.sign && isodd(z2));
	z1.sign = 0;
	z2.sign = 0;
	if (iszero(z2)) {	/* number raised to power 0 */
		if (iszero(z1))
			error("Zero raised to zero power");
		*res = _one_;
		return;
	}
	if (isleone(z1)) {	/* 0, 1, or -1 raised to a power */
		ans = _one_;
		ans.sign = (BOOL)sign;
		if (*z1.v == 0)
			ans = _zero_;
		*res = ans;
		return;
	}
	if (isbig(z2))
		error("Raising to very large power");
	power = (istiny(z2) ? z1tol(z2) : z2tol(z2));
	if (istwo(z1)) {	/* two raised to a power */
		zbitvalue((long) power, res);
		return;
	}
	/*
	 * See if this is a power of ten
	 */
	if (istiny(z1) && (*z1.v == 10)) {
		ztenpow((long) power, res);
		res->sign = (BOOL)sign;
		return;
	}
	/*
	 * Handle low powers specially
	 */
	if (power <= 4) {
		switch ((int) power) {
			case 1:
				ans.len = z1.len;
				ans.v = alloc(ans.len);
				copyval(z1, ans);
				ans.sign = (BOOL)sign;
				*res = ans;
				return;
			case 2:
				zsquare(z1, res);
				return;
			case 3:
				zsquare(z1, &temp);
				zmul(z1, temp, res);
				freeh(temp.v);
				res->sign = (BOOL)sign;
				return;
			case 4:
				zsquare(z1, &temp);
				zsquare(temp, res);
				freeh(temp.v);
				return;
		}
	}
	/*
	 * Shift out all powers of twos so the multiplies are smaller.
	 * We will shift back the right amount when done.
	 */
	twos = 0;
	if (iseven(z1)) {
		twos = zlowbit(z1);
		ans.v = alloc(z1.len);
		ans.len = z1.len;
		copyval(z1, ans);
		shiftr(ans, twos);
		trim(&ans);
		z1 = ans;
		twos *= power;
	}
	/*
	 * Compute the power by squaring and multiplying.
	 * This uses the left to right method of power raising.
	 */
	bit = TOPFULL;
	while ((bit & power) == 0)
		bit >>= 1L;
	bit >>= 1L;
	zsquare(z1, &ans);
	if (bit & power) {
		zmul(ans, z1, &temp);
		freeh(ans.v);
		ans = temp;
	}
	bit >>= 1L;
	while (bit) {
		zsquare(ans, &temp);
		freeh(ans.v);
		ans = temp;
		if (bit & power) {
			zmul(ans, z1, &temp);
			freeh(ans.v);
			ans = temp;
		}
		bit >>= 1L;
	}
	/*
	 * Scale back up by proper power of two
	 */
	if (twos) {
		zshift(ans, twos, &temp);
		freeh(ans.v);
		ans = temp;
		freeh(z1.v);
	}
	ans.sign = (BOOL)sign;
	*res = ans;
}


/*
 * Compute ten to the specified power
 * This saves some work since the squares of ten are saved.
 */
void
ztenpow(power, res)
	long power;
	ZVALUE *res;
{
	long i;
	ZVALUE ans;
	ZVALUE temp;

	if (power <= 0) {
		*res = _one_;
		return;
	}
	ans = _one_;
	_tenpowers_[0] = _ten_;
	for (i = 0; power; i++) {
		if (_tenpowers_[i].len == 0)
			zsquare(_tenpowers_[i-1], &_tenpowers_[i]);
		if (power & 0x1) {
			zmul(ans, _tenpowers_[i], &temp);
			freeh(ans.v);
			ans = temp;
		}
		power /= 2;
	}
	*res = ans;
}


/*
 * Calculate modular inverse suppressing unnecessary divisions.
 * This is based on the Euclidian algorithm for large numbers.
 * (Algorithm X from Knuth Vol 2, section 4.5.2. and exercise 17)
 * Returns TRUE if there is no solution because the numbers
 * are not relatively prime.
 */
BOOL
zmodinv(u, v, res)
	ZVALUE u, v;
	ZVALUE *res;
{
	FULL	q1, q2, ui3, vi3, uh, vh, A, B, C, D, T;
	ZVALUE	u2, u3, v2, v3, qz, tmp1, tmp2, tmp3;

	if (isneg(u) || isneg(v) || (zrel(u, v) >= 0))
		zmod(u, v, &v3);
	else
		zcopy(u, &v3);
	zcopy(v, &u3);
	u2 = _zero_;
	v2 = _one_;

	/*
	 * Loop here while the size of the numbers remain above
	 * the size of a FULL.  Throughout this loop u3 >= v3.
	 */
	while ((u3.len > 1) && !iszero(v3)) {
		uh = (((FULL) u3.v[u3.len - 1]) << BASEB) + u3.v[u3.len - 2];
		vh = 0;
		if ((v3.len + 1) >= u3.len)
			vh = v3.v[v3.len - 1];
		if (v3.len == u3.len)
			vh = (vh << BASEB) + v3.v[v3.len - 2];
		A = 1;
		B = 0;
		C = 0;
		D = 1;

		/*
		 * Calculate successive quotients of the continued fraction
		 * expansion using only single precision arithmetic until
		 * greater precision is required.
		 */
		while ((vh + C) && (vh + D)) {
			q1 = (uh + A) / (vh + C);
			q2 = (uh + B) / (vh + D);
			if (q1 != q2)
				break;
			T = A - q1 * C;
			A = C;
			C = T;
			T = B - q1 * D;
			B = D;
			D = T;
			T = uh - q1 * vh;
			uh = vh;
			vh = T;
		}
	
		/*
		 * If B is zero, then we made no progress because
		 * the calculation requires a very large quotient.
		 * So we must do this step of the calculation in
		 * full precision
		 */
		if (B == 0) {
			zquo(u3, v3, &qz);
			zmul(qz, v2, &tmp1);
			zsub(u2, tmp1, &tmp2);
			freeh(tmp1.v);
			freeh(u2.v);
			u2 = v2;
			v2 = tmp2;
			zmul(qz, v3, &tmp1);
			zsub(u3, tmp1, &tmp2);
			freeh(tmp1.v);
			freeh(u3.v);
			u3 = v3;
			v3 = tmp2;
			freeh(qz.v);
			continue;
		}
		/*
		 * Apply the calculated A,B,C,D numbers to the current
		 * values to update them as if the full precision
		 * calculations had been carried out.
		 */
		zmuli(u2, (long) A, &tmp1);
		zmuli(v2, (long) B, &tmp2);
		zadd(tmp1, tmp2, &tmp3);
		freeh(tmp1.v);
		freeh(tmp2.v);
		zmuli(u2, (long) C, &tmp1);
		zmuli(v2, (long) D, &tmp2);
		freeh(u2.v);
		freeh(v2.v);
		u2 = tmp3;
		zadd(tmp1, tmp2, &v2);
		freeh(tmp1.v);
		freeh(tmp2.v);
		zmuli(u3, (long) A, &tmp1);
		zmuli(v3, (long) B, &tmp2);
		zadd(tmp1, tmp2, &tmp3);
		freeh(tmp1.v);
		freeh(tmp2.v);
		zmuli(u3, (long) C, &tmp1);
		zmuli(v3, (long) D, &tmp2);
		freeh(u3.v);
		freeh(v3.v);
		u3 = tmp3;
		zadd(tmp1, tmp2, &v3);
		freeh(tmp1.v);
		freeh(tmp2.v);
	}

	/*
	 * Here when the remaining numbers become single precision in size.
	 * Finish the procedure using single precision calculations.
	 */
	if (iszero(v3) && !isone(u3)) {
		freeh(u3.v);
		freeh(v3.v);
		freeh(u2.v);
		freeh(v2.v);
		return TRUE;
	}
	ui3 = (istiny(u3) ? z1tol(u3) : z2tol(u3));
	vi3 = (istiny(v3) ? z1tol(v3) : z2tol(v3));
	freeh(u3.v);
	freeh(v3.v);
	while (vi3) {
		q1 = ui3 / vi3;
		zmuli(v2, (long) q1, &tmp1);
		zsub(u2, tmp1, &tmp2);
		freeh(tmp1.v);
		freeh(u2.v);
		u2 = v2;
		v2 = tmp2;
		q2 = ui3 - q1 * vi3;
		ui3 = vi3;
		vi3 = q2;
	}
	freeh(v2.v);
	if (ui3 != 1) {
		freeh(u2.v);
		return TRUE;
	}
	if (isneg(u2)) {
		zadd(v, u2, res);
		freeh(u2.v);
		return FALSE;
	}
	*res = u2;
	return FALSE;
}


#if 0
/*
 * Approximate the quotient of two integers by another set of smaller
 * integers.  This uses continued fractions to determine the smaller set.
 */
void
zapprox(z1, z2, res1, res2)
	ZVALUE z1, z2, *res1, *res2;
{
	int sign;
	ZVALUE u1, v1, u3, v3, q, t1, t2, t3;

	sign = ((z1.sign != 0) ^ (z2.sign != 0));
	z1.sign = 0;
	z2.sign = 0;
	v3 = z2;
	u3 = z1;
	u1 = _one_;
	v1 = _zero_;
	while (!iszero(v3)) {
		zdiv(u3, v3, &q, &t1);
		zmul(v1, q, &t2);
		zsub(u1, t2, &t3);
		freeh(q.v);
		freeh(t2.v);
		freeh(u1.v);
		if ((u3.v != z1.v) && (u3.v != z2.v))
			freeh(u3.v);
		u1 = v1;
		u3 = v3;
		v1 = t3;
		v3 = t1;
	}
	if (!isunit(u3))
		error("Non-relativly prime numbers for approx");
	if ((u3.v != z1.v) && (u3.v != z2.v))
		freeh(u3.v);
	if ((v3.v != z1.v) && (v3.v != z2.v))
		freeh(v3.v);
	freeh(v1.v);
	zmul(u1, z1, &t1);
	zsub(t1, _one_, &t2);
	freeh(t1.v);
	zquo(t2, z2, &t1);
	freeh(t2.v);
	u1.sign = (BOOL)sign;
	t1.sign = 0;
	*res1 = t1;
	*res2 = u1;
}
#endif


/*
 * Binary gcd algorithm
 * This algorithm taken from Knuth
 */
void
zgcd(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	ZVALUE u, v, t;
	register long j, k, olen, mask;
	register HALF h;
	HALF *oldv1, *oldv2;

	/*
	 * First see if one number is very much larger than the other.
	 * If so, then divide as necessary to get the numbers near each other.
	 */
	z1.sign = 0;
	z2.sign = 0;
	oldv1 = z1.v;
	oldv2 = z2.v;
	if (z1.len < z2.len) {
		t = z1;
		z1 = z2;
		z2 = t;
	}
	while ((z1.len > (z2.len + 5)) && !iszero(z2)) {
		zmod(z1, z2, &t);
		if ((z1.v != oldv1) && (z1.v != oldv2))
			freeh(z1.v);
		z1 = z2;
		z2 = t;
	}
	/*
	 * Ok, now do the binary method proper
	 */
	u.len = z1.len;
	v.len = z2.len;
	u.sign = 0;
	v.sign = 0;
	if (!ztest(z1)) {
		v.v = alloc(v.len);
		copyval(z2, v);
		*res = v;
		goto done;
	}
	if (!ztest(z2)) {
		u.v = alloc(u.len);
		copyval(z1, u);
		*res = u;
		goto done;
	}
	u.v = alloc(u.len);
	v.v = alloc(v.len);
	copyval(z1, u);
	copyval(z2, v);
	k = 0;
	while (u.v[k] == 0 && v.v[k] == 0)
		++k;
	mask = 01;
	h = u.v[k] | v.v[k];
	k *= BASEB;
	while (!(h & mask)) {
		mask <<= 1;
		k++;
	}
	shiftr(u, k);
	shiftr(v, k);
	trim(&u);
	trim(&v);
	if (isodd(u)) {
		t.v = alloc(v.len);
		t.len = v.len;
		copyval(v, t);
		t.sign = !v.sign;
	} else {
		t.v = alloc(u.len);
		t.len = u.len;
		copyval(u, t);
		t.sign = u.sign;
	}
	while (ztest(t)) {
		j = 0;
		while (!t.v[j])
			++j;
		mask = 01;
		h = t.v[j];
		j *= BASEB;
		while (!(h & mask)) {
			mask <<= 1;
			j++;
		}
		shiftr(t, j);
		trim(&t);
		if (ztest(t) > 0) {
			freeh(u.v);
			u = t;
		} else {
			freeh(v.v);
			v = t;
			v.sign = !t.sign;
		}
		zsub(u, v, &t);
	}
	freeh(t.v);
	freeh(v.v);
	if (k) {
		olen = u.len;
		u.len += k / BASEB + 1;
		u.v = (HALF *) realloc(u.v, u.len * sizeof(HALF));
		while (olen != u.len)
			u.v[olen++] = 0;
		shiftl(u, k);
	}
	trim(&u);
	*res = u;

done:
	if ((z1.v != oldv1) && (z1.v != oldv2))
		freeh(z1.v);
	if ((z2.v != oldv1) && (z2.v != oldv2))
		freeh(z2.v);
}


/*
 * Compute the lcm of two integers (least common multiple).
 * This is done using the formula:  gcd(a,b) * lcm(a,b) = a * b.
 */
void
zlcm(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	ZVALUE temp1, temp2;

	zgcd(z1, z2, &temp1);
	zquo(z1, temp1, &temp2);
	freeh(temp1.v);
	zmul(temp2, z2, res);
	freeh(temp2.v);
}


/*
 * Return whether or not two numbers are relatively prime to each other.
 */
BOOL
zrelprime(z1, z2)
	ZVALUE z1, z2;			/* numbers to be tested */
{
	FULL rem1, rem2;		/* remainders */
	ZVALUE rem;
	BOOL result;

	z1.sign = 0;
	z2.sign = 0;
	if (iseven(z1) && iseven(z2))	/* false if both even */
		return FALSE;
	if (isunit(z1) || isunit(z2))	/* true if either is a unit */
		return TRUE;
	if (iszero(z1) || iszero(z2))	/* false if either is zero */
		return FALSE;
	if (istwo(z1) || istwo(z2))	/* true if either is two */
		return TRUE;
	/*
	 * Try reducing each number by the product of the first few odd primes
	 * to see if any of them are a common factor.
	 */
	rem1 = zmodi(z1, 3L * 5 * 7 * 11 * 13);
	rem2 = zmodi(z2, 3L * 5 * 7 * 11 * 13);
	if (((rem1 % 3) == 0) && ((rem2 % 3) == 0))
		return FALSE;
	if (((rem1 % 5) == 0) && ((rem2 % 5) == 0))
		return FALSE;
	if (((rem1 % 7) == 0) && ((rem2 % 7) == 0))
		return FALSE;
	if (((rem1 % 11) == 0) && ((rem2 % 11) == 0))
		return FALSE;
	if (((rem1 % 13) == 0) && ((rem2 % 13) == 0))
		return FALSE;
	/*
	 * Try a new batch of primes now
	 */
	rem1 = zmodi(z1, 17L * 19 * 23);
	rem2 = zmodi(z2, 17L * 19 * 23);
	if (((rem1 % 17) == 0) && ((rem2 % 17) == 0))
		return FALSE;
	if (((rem1 % 19) == 0) && ((rem2 % 19) == 0))
		return FALSE;
	if (((rem1 % 23) == 0) && ((rem2 % 23) == 0))
		return FALSE;
	/*
	 * Yuk, we must actually compute the gcd to know the answer
	 */
	zgcd(z1, z2, &rem);
	result = isunit(rem);
	freeh(rem.v);
	return result;
}


/*
 * Compute the log of one number base another, to the closest integer.
 * This is the largest integer which when the second number is raised to it,
 * the resulting value is less than or equal to the first number.
 * Example:  zlog(123456, 10) = 5.
 */
long
zlog(z1, z2)
	ZVALUE z1, z2;
{
	register ZVALUE *zp;		/* current square */
	long power;			/* current power */
	long worth;			/* worth of current square */
	ZVALUE val;			/* current value of power */
	ZVALUE temp;			/* temporary */
	ZVALUE squares[32];		/* table of squares of base */

	/*
	 * Make sure that the numbers are nonzero and the base is greater than one.
	 */
	if (isneg(z1) || iszero(z1) || isneg(z2) || isleone(z2))
		error("Bad arguments for log");
	/*
	 * Reject trivial cases.
	 */
	if (z1.len < z2.len)
		return 0;
	if ((z1.len == z2.len) && (z1.v[z1.len-1] < z2.v[z2.len-1]))
		return 0;
	power = zrel(z1, z2);
	if (power <= 0)
		return (power + 1);
	/*
	 * Handle any power of two special.
	 */
	if (zisonebit(z2))
		return (zhighbit(z1) / zlowbit(z2));
	/*
	 * Handle base 10 special
	 */
	if ((z2.len == 1) && (*z2.v == 10))
		return zlog10(z1);
	/*
	 * Now loop by squaring the base each time, and see whether or
	 * not each successive square is still smaller than the number.
	 */
	worth = 1;
	zp = &squares[0];
	*zp = z2;
	while (((zp->len * 2) - 1) <= z1.len) {	/* while square not too large */
		zsquare(*zp, zp + 1);
		zp++;
		worth *= 2;
	}
	/*
	 * Now back down the squares, and multiply them together to see
	 * exactly how many times the base can be raised by.
	 */
	val = _one_;
	power = 0;
	for (; zp >= squares; zp--, worth /= 2) {
		if ((val.len + zp->len - 1) <= z1.len) {
			zmul(val, *zp, &temp);
			if (zrel(z1, temp) >= 0) {
				freeh(val.v);
				val = temp;
				power += worth;
			} else
				freeh(temp.v);
		}
		if (zp != squares)
			freeh(zp->v);
	}
	return power;
}


/*
 * Return the integral log base 10 of a number.
 */
long
zlog10(z)
	ZVALUE z;
{
	register ZVALUE *zp;		/* current square */
	long power;			/* current power */
	long worth;			/* worth of current square */
	ZVALUE val;			/* current value of power */
	ZVALUE temp;			/* temporary */

	if (!ispos(z))
		error("Non-positive number for log10");
	/*
	 * Loop by squaring the base each time, and see whether or
	 * not each successive square is still smaller than the number.
	 */
	worth = 1;
	zp = &_tenpowers_[0];
	*zp = _ten_;
	while (((zp->len * 2) - 1) <= z.len) {	/* while square not too large */
		if (zp[1].len == 0)
			zsquare(*zp, zp + 1);
		zp++;
		worth *= 2;
	}
	/*
	 * Now back down the squares, and multiply them together to see
	 * exactly how many times the base can be raised by.
	 */
	val = _one_;
	power = 0;
	for (; zp >= _tenpowers_; zp--, worth /= 2) {
		if ((val.len + zp->len - 1) <= z.len) {
			zmul(val, *zp, &temp);
			if (zrel(z, temp) >= 0) {
				freeh(val.v);
				val = temp;
				power += worth;
			} else
				freeh(temp.v);
		}
	}
	return power;
}


/*
 * Return the number of times that one number will divide another.
 * This works similarly to zlog, except that divisions must be exact.
 * For example, zdivcount(540, 3) = 3, since 3^3 divides 540 but 3^4 won't.
 */
long
zdivcount(z1, z2)
	ZVALUE z1, z2;
{
	long count;		/* number of factors removed */
	ZVALUE tmp;		/* ignored return value */

	count = zfacrem(z1, z2, &tmp);
	freeh(tmp.v);
	return count;
}


/*
 * Remove all occurances of the specified factor from a number.
 * Also returns the number of factors removed as a function return value.
 * Example:  zfacrem(540, 3, &x) returns 3 and sets x to 20.
 */
long
zfacrem(z1, z2, rem)
	ZVALUE z1, z2, *rem;
{
	register ZVALUE *zp;		/* current square */
	long count;			/* total count of divisions */
	long worth;			/* worth of current square */
	ZVALUE temp1, temp2, temp3;	/* temporaries */
	ZVALUE squares[32];		/* table of squares of factor */

	z1.sign = 0;
	z2.sign = 0;
	/*
	 * Make sure factor isn't 0 or 1.
	 */
	if (isleone(z2))
		error("Bad argument for facrem");
	/*
	 * Reject trivial cases.
	 */
	if ((z1.len < z2.len) || (isodd(z1) && iseven(z2)) ||
		((z1.len == z2.len) && (z1.v[z1.len-1] < z2.v[z2.len-1]))) {
		rem->v = alloc(z1.len);
		rem->len = z1.len;
		rem->sign = 0;
		copyval(z1, *rem);
		return 0;
	}
	/*
	 * Handle any power of two special.
	 */
	if (zisonebit(z2)) {
		count = zlowbit(z1) / zlowbit(z2);
		rem->v = alloc(z1.len);
		rem->len = z1.len;
		rem->sign = 0;
		copyval(z1, *rem);
		shiftr(*rem, count);
		trim(rem);
		return count;
	}
	/*
	 * See if the factor goes in even once.
	 */
	zdiv(z1, z2, &temp1, &temp2);
	if (!iszero(temp2)) {
		freeh(temp1.v);
		freeh(temp2.v);
		rem->v = alloc(z1.len);
		rem->len = z1.len;
		rem->sign = 0;
		copyval(z1, *rem);
		return 0;
	}
	freeh(temp2.v);
	z1 = temp1;
	/*
	 * Now loop by squaring the factor each time, and see whether
	 * or not each successive square will still divide the number.
	 */
	count = 1;
	worth = 1;
	zp = &squares[0];
	*zp = z2;
	while (((zp->len * 2) - 1) <= z1.len) {	/* while square not too large */
		zsquare(*zp, &temp1);
		zdiv(z1, temp1, &temp2, &temp3);
		if (!iszero(temp3)) {
			freeh(temp1.v);
			freeh(temp2.v);
			freeh(temp3.v);
			break;
		}
		freeh(temp3.v);
		freeh(z1.v);
		z1 = temp2;
		*++zp = temp1;
		worth *= 2;
		count += worth;
	}
	/*
	 * Now back down the list of squares, and see if the lower powers
	 * will divide any more times.
	 */
	for (; zp >= squares; zp--, worth /= 2) {
		if (zp->len <= z1.len) {
			zdiv(z1, *zp, &temp1, &temp2);
			if (iszero(temp2)) {
				temp3 = z1;
				z1 = temp1;
				temp1 = temp3;
				count += worth;
			}
			freeh(temp1.v);
			freeh(temp2.v);
		}
		if (zp != squares)
			freeh(zp->v);
	}
	*rem = z1;
	return count;
}


/*
 * Keep dividing a number by the gcd of it with another number until the
 * result is relatively prime to the second number.
 */
void
zgcdrem(z1, z2, res)
	ZVALUE z1, z2, *res;
{
	ZVALUE tmp1, tmp2;

	/*
	 * Begin by taking the gcd for the first time.
	 * If the number is already relatively prime, then we are done.
	 */
	zgcd(z1, z2, &tmp1);
	if (isunit(tmp1) || iszero(tmp1)) {
		res->len = z1.len;
		res->v = alloc(z1.len);
		res->sign = z1.sign;
		copyval(z1, *res);
		return;
	}
	zquo(z1, tmp1, &tmp2);
	z1 = tmp2;
	z2 = tmp1;
	/*
	 * Now keep alternately taking the gcd and removing factors until
	 * the gcd becomes one.
	 */
	while (!isunit(z2)) {
		(void) zfacrem(z1, z2, &tmp1);
		freeh(z1.v);
		z1 = tmp1;
		zgcd(z1, z2, &tmp1);
		freeh(z2.v);
		z2 = tmp1;
	}
	*res = z1;
}


/*
 * Find the lowest prime factor of a number if one can be found.
 * Search is conducted for the first count primes.
 * Returns 1 if no factor was found.
 */
long
zlowfactor(z, count)
	ZVALUE z;
	long count;
{
	FULL p, d;
	ZVALUE div, tmp;
	HALF divval[2];

	if ((--count < 0) || iszero(z))
		return 1;
	if (iseven(z))
		return 2;
	div.sign = 0;
	div.v = divval;
	for (p = 3; (count > 0); p += 2) {
		for (d = 3; (d * d) <= p; d += 2)
			if ((p % d) == 0)
				goto next;
		divval[0] = (p & BASE1);
		divval[1] = (p / BASE);
		div.len = 1 + (p >= BASE);
		zmod(z, div, &tmp);
		if (iszero(tmp))
			return p;
		freeh(tmp.v);
		count--;
next:;
	}
	return 1;
}


/*
 * Return the number of digits (base 10) in a number, ignoring the sign.
 */
long
zdigits(z1)
	ZVALUE z1;
{
	long count, val;

	z1.sign = 0;
	if (istiny(z1)) {	/* do small numbers ourself */
		count = 1;
		val = 10;
		while (*z1.v >= (HALF)val) {
			count++;
			val *= 10;
		}
		return count;
	}
	return (zlog10(z1) + 1);
}


/*
 * Return the single digit at the specified decimal place of a number,
 * where 0 means the rightmost digit.  Example:  zdigit(1234, 1) = 3.
 */
FLAG
zdigit(z1, n)
	ZVALUE z1;
	long n;
{
	ZVALUE tmp1, tmp2;
	FLAG res;

	z1.sign = 0;
	if (iszero(z1) || (n < 0) || (n / BASEDIG >= z1.len))
		return 0;
	if (n == 0)
		return zmodi(z1, 10L);
	if (n == 1)
		return zmodi(z1, 100L) / 10;
	if (n == 2)
		return zmodi(z1, 1000L) / 100;
	if (n == 3)
		return zmodi(z1, 10000L) / 1000;
	ztenpow(n, &tmp1);
	zquo(z1, tmp1, &tmp2);
	res = zmodi(tmp2, 10L);
	freeh(tmp1.v);
	freeh(tmp2.v);
	return res;
}


/*
 * Find the square root of a number.  This is the greatest integer whose
 * square is less than or equal to the number. Returns TRUE if the
 * square root is exact.
 */
BOOL
zsqrt(z1, dest)
	ZVALUE z1, *dest;
{
	ZVALUE try, quo, rem, old, temp;
	FULL iquo, val;
	long i,j;

	if (z1.sign)
		error("Square root of negative number");
	if (iszero(z1)) {
		*dest = _zero_;
		return TRUE;
	}
	if ((*z1.v < 4) && istiny(z1)) {
		*dest = _one_;
		return (*z1.v == 1);
	}
	/*
	 * Pick the square root of the leading one or two digits as a first guess.
	 */
	val = z1.v[z1.len-1];
	if ((z1.len & 0x1) == 0)
		val = (val * BASE) + z1.v[z1.len-2];

	/*
	 * Find the largest power of 2 that when squared
	 * is <= val > 0.  Avoid multiply overflow by doing 
	 * a careful check at the BASE boundary.
	 */
	j = 1<<(BASEB+BASEB-2);
	if (val > j) {
		iquo = BASE;
	} else {
		i = BASEB-1;
		while (j > val) {
			--i;
			j >>= 2;
		}
		iquo = bitmask[i];
	}

	for (i = 8; i > 0; i--)
		iquo = (iquo + (val / iquo)) / 2;
	if (iquo > BASE1)
		iquo = BASE1;
	/*
	 * Allocate the numbers to use for the main loop.
	 * The size and high bits of the final result are correctly set here.
	 * Notice that the remainder of the test value is rubbish, but this
	 * is unimportant.
	 */
	try.sign = 0;
	try.len = (z1.len + 1) / 2;
	try.v = alloc(try.len);
	try.v[try.len-1] = (HALF)iquo;
	old.sign = 0;
	old.v = alloc(try.len);
	old.len = 1;
	*old.v = 0;
	/*
	 * Main divide and average loop
	 */
	for (;;) {
		zdiv(z1, try, &quo, &rem);
		i = zrel(try, quo);
		if ((i == 0) && iszero(rem)) {	/* exact square root */
			freeh(rem.v);
			freeh(quo.v);
			freeh(old.v);
			*dest = try;
			return TRUE;
		}
		freeh(rem.v);
		if (i <= 0) {
			/*
			* Current try is less than or equal to the square root since
			* it is less than the quotient.  If the quotient is equal to
			* the try, we are all done.  Also, if the try is equal to the
			* old try value, we are done since no improvement occurred.
			* If not, save the improved value and loop some more.
			*/
			if ((i == 0) || (zcmp(old, try) == 0)) {
				freeh(quo.v);
				freeh(old.v);
				*dest = try;
				return FALSE;
			}
			old.len = try.len;
			copyval(try, old);
		}
		/* average current try and quotent for the new try */
		zadd(try, quo, &temp);
		freeh(quo.v);
		freeh(try.v);
		try = temp;
		shiftr(try, 1L);
		quicktrim(try);
	}
}


/*
 * Take an arbitrary root of a number (to the greatest integer).
 * This uses the following iteration to get the Kth root of N:
 *	x = ((K-1) * x + N / x^(K-1)) / K
 */
void
zroot(z1, z2, dest)
	ZVALUE z1, z2, *dest;
{
	ZVALUE try, quo, old, temp, temp2;
	ZVALUE k1;			/* holds k - 1 */
	int sign;
	long i, k, highbit;
	SIUNION sival;

	sign = z1.sign;
	if (sign && iseven(z2))
		error("Even root of negative number");
	if (iszero(z2) || isneg(z2))
		error("Non-positive root");
	if (iszero(z1)) {	/* root of zero */
		*dest = _zero_;
		return;
	}
	if (isunit(z2)) {	/* first root */
		zcopy(z1, dest);
		return;
	}
	if (isbig(z2)) {	/* humongous root */
		*dest = _one_;
		dest->sign = (HALF)sign;
		return;
	}
	k = (istiny(z2) ? z1tol(z2) : z2tol(z2));
	highbit = zhighbit(z1);
	if (highbit < k) {	/* too high a root */
		*dest = _one_;
		dest->sign = (HALF)sign;
		return;
	}
	sival.ivalue = k - 1;
	k1.v = &sival.silow;
	k1.len = 1 + (sival.sihigh != 0);
	k1.sign = 0;
	z1.sign = 0;
	/*
	 * Allocate the numbers to use for the main loop.
	 * The size and high bits of the final result are correctly set here.
	 * Notice that the remainder of the test value is rubbish, but this
	 * is unimportant.
	 */
	highbit = (highbit + k - 1) / k;
	try.len = (highbit / BASEB) + 1;
	try.v = alloc(try.len);
	try.v[try.len-1] = ((HALF)1 << (highbit % BASEB));
	try.sign = 0;
	old.v = alloc(try.len);
	old.len = 1;
	*old.v = 0;
	old.sign = 0;
	/*
	 * Main divide and average loop
	 */
	for (;;) {
		zpowi(try, k1, &temp);
		zquo(z1, temp, &quo);
		freeh(temp.v);
		i = zrel(try, quo);
		if (i <= 0) {
			/*
			 * Current try is less than or equal to the root since it is
			 * less than the quotient. If the quotient is equal to the try,
			 * we are all done.  Also, if the try is equal to the old value,
			 * we are done since no improvement occurred.
			 * If not, save the improved value and loop some more.
			 */
			if ((i == 0) || (zcmp(old, try) == 0)) {
				freeh(quo.v);
				freeh(old.v);
				try.sign = (HALF)sign;
				quicktrim(try);
				*dest = try;
				return;
			}
			old.len = try.len;
			copyval(try, old);
		}
		/* average current try and quotent for the new try */
		zmul(try, k1, &temp);
		freeh(try.v);
		zadd(quo, temp, &temp2);
		freeh(temp.v);
		freeh(quo.v);
		zquo(temp2, z2, &try);
		freeh(temp2.v);
	}
}


/*
 * Test to see if a number is an exact square or not.
 */
BOOL
zissquare(z)
	ZVALUE z;		/* number to be tested */
{
	long n, i;
	ZVALUE tmp;

	if (z.sign)		/* negative */
		return FALSE;
	while ((z.len > 1) && (*z.v == 0)) {	/* ignore trailing zero words */
		z.len--;
		z.v++;
	}
	if (isleone(z))		/* zero or one */
		return TRUE;
	n = *z.v & 0xf;		/* check mod 16 values */
	if ((n != 0) && (n != 1) && (n != 4) && (n != 9))
		return FALSE;
	n = *z.v & 0xff;	/* check mod 256 values */
	i = 0x80;
	while (((i * i) & 0xff) != n)
		if (--i <= 0)
			return FALSE;
	n = zsqrt(z, &tmp);	/* must do full square root test now */
	freeh(tmp.v);
	return n;
}

/* END CODE */
