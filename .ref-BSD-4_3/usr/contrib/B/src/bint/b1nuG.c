/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1nuG.c,v 1.4 85/08/22 16:50:59 timo Exp $
*/

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b1num.h"


/*
 * Routines for greatest common divisor calculation
 * Cf. Knuth II Sec. 4.5.2. Algorithm B
 * "Binary gcd algorithm"
 * The labels correspond with those in the book
 *
 * Assumptions about built-in arithmetic:
 * x>>1 == x/2  (if x >= 0)
 * 1<<k == 2**k (if it fits in a word)
 */

/* Single-precision gcd for integers > 0 */

Hidden digit dig_gcd(u, v) register digit u, v; {
	register digit t;
	register int k = 0;

	if (u <= 0 || v <= 0) syserr(MESS(900, "dig_gcd of number(s) <= 0"));

 /*B1*/	while (Even(u) && Even(v)) ++k, u >>= 1, v >>= 1;

 /*B2*/	if (Even(u)) t = u;
	else {	t = -v;
		goto B4;
	}

	do {
 /*B3*/		do {
			t /= 2;
 B4:			;
		} while (Even(t));

 /*B5*/		if (t > 0) u = t;
		else v = -t;

 /*B6*/		t = u-v;
	} while (t);

	return u * (1<<k);
}


Visible integer int_half(v) integer v; {
	register int i;
	register long carry;

	if (IsSmallInt(v)) 	return (integer) MkSmallInt(SmallIntVal(v) / 2);

	if (Msd(v) < 0) {
		i = Length(v)-2;
		if (i < 0) {
			release((value) v);
			return int_0;
		}
		carry = BASE;
	}
	else {
		carry = 0;
		i = Length(v)-1;
	}

	if (Refcnt(v) > 1) uniql((value *) &v);

	for (; i >= 0; --i) {
		carry += Digit(v,i);
		Digit(v,i) = carry/2;
		carry = carry&1 ? BASE : 0;
	}

	return int_canon(v);
}


Hidden integer twice(v) integer v; {
	digit carry = 0;
	int i;

	if (IsSmallInt(v)) return mk_int(2.0 * SmallIntVal(v));

	if (Refcnt(v) > 1) uniql((value *) &v);

	for (i = 0; i < Length(v); ++i) {
		carry += Digit(v,i) * 2;
		if (carry >= BASE)
			Digit(v,i) = carry-BASE, carry = 1;
		else
			Digit(v,i) = carry, carry = 0;
	}

	if (carry) {	/* Enlarge the number */
		v = (integer) regrab_num((value) v, Length(v)+1);
		Digit(v, Length(v)-1) = carry;
	}

	return v;
}


Hidden bool even(u) integer u; {
	if (IsSmallInt(u))
		return Even(SmallIntVal(u));
	return Even(Lsd(u));
}


/* Multi-precision gcd of integers > 0 */

Visible integer int_gcd(u1, v1) integer u1, v1; {
	struct integer uu1, vv1;

	if (Msd(u1) <= 0 || Msd(v1) <= 0)
		syserr(MESS(901, "gcd of number(s) <= 0"));

	if (u1==int_1 || v1==int_1) return int_1;
		/* Speed-up for e.g. 1E-100000 */

	if (IsSmallInt(u1) && IsSmallInt(v1))
		return (integer)
			MkSmallInt(dig_gcd(SmallIntVal(u1), SmallIntVal(v1)));

	FreezeSmallInt(u1, uu1);
	FreezeSmallInt(v1, vv1);

	/* Multi-precision binary gcd algorithm */

	{	long k = 0;
		integer t, u, v;

		u = (integer) Copy((value) u1);
		v = (integer) Copy((value) v1);

 /*B1*/		while (even(u) && even(v)) {
			u = int_half(u);
			v = int_half(v);
			if (++k < 0) {
				/*It's a number we can't cope with,
				  with too many common factors 2.
				  Though the user can't help it,
				  the least we can do is to allow
				  continuation of the session.
				*/
				error(MESS(902, "exceptionally large rational number"));
				k = 0;
			}
		}

 /*B2*/		if (even(u)) t = (integer) Copy(u);
		else {
			t = int_neg(v);
			goto B4;
		}

		do {
 /*B3*/			do {
				t = int_half(t);
 B4:				;
			} while (even(t));

 /*B5*/			if (Msd(t) >= 0) {
				release((value) u);
				u = t;
			}
			else {
				release((value) v);
				v = int_neg(t);
				release((value) t);
			}

 /*B6*/			t = int_diff(u, v);
			/* t cannot be int_1 since both u and v are odd! */
		} while (t != int_0);

		release((value) t);
		release((value) v);

		while (--k >= 0) u = twice(u);
		
		return u;
	}
}
