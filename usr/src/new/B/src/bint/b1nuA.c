/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1nuA.c,v 1.4 85/08/22 16:50:25 timo Exp $
*/

/* Approximate arithmetic */

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b1num.h"
#include "b3err.h" /* For still_ok */

/*
For various reasons, on some machines (notably the VAX), the range
of the exponent is too small (ca. 1.7E38), and we cope with this by
adding a second word which holds the exponent.
However, on other machines (notably the IBM PC), the range is sufficient
(ca. 1E300), and here we try to save as much code as possible by not
doing our own exponent handling.  (To be fair, we also don't check
certain error conditions, to save more code.)
The difference is made by #defining EXT_RANGE (in b1num.h), meaning we
have to EXTend the RANGE of the exponent.
*/

#ifdef EXT_RANGE
Hidden struct real app_0_buf = {Num, 1, -1, 0.0, -(double)Maxint};
	/* Exponent must be less than any realistic exponent! */
#else !EXT_RANGE
Hidden struct real app_0_buf = {Num, 1, -1, 0.0};
#endif !EXT_RANGE

Visible real app_0 = &app_0_buf;

/*
 * Build an approximate number.
 */

Visible real mk_approx(frac, expo) double frac, expo; {
	real u;
#ifdef EXT_RANGE
	expint e;
	if (frac != 0) frac = frexp(frac, &e), expo += e;
	if (frac == 0.5) frac = 1, --expo; /* Assert 0.5 < frac <= 1 */
	if (frac == 0 || expo < -BIG) return (real) Copy(app_0);
	if (expo > BIG) {
		error(MESS(700, "approximate number too large"));
		expo = BIG;
	}
#else !EXT_RANGE
	if (frac == 0.0) return (real) Copy(app_0);
	frac= ldexp(frac, (int)expo);
#endif EXT_RANGE
	u = (real) grab_num(-1);
	Frac(u) = frac;
#ifdef EXT_RANGE
	Expo(u) = expo;
#endif EXT_RANGE
	return u;
}

/*
 * Approximate arithmetic.
 */

Visible real app_sum(u, v) real u, v; {
#ifdef EXT_RANGE
	real w;
	if (Expo(u) < Expo(v)) w = u, u = v, v = w;
	if (Expo(v) - Expo(u) < Minexpo) return (real) Copy(u);
	return mk_approx(Frac(u) + ldexp(Frac(v), (int)(Expo(v) - Expo(u))),
		Expo(u));
#else !EXT_RANGE
	return mk_approx(Frac(u) + Frac(v), 0.0);
#endif !EXT_RANGE
}

Visible real app_diff(u, v) real u, v; {
#ifdef EXT_RANGE
	real w;
	int sign = 1;
	if (Expo(u) < Expo(v)) w = u, u = v, v = w, sign = -1;
	if (Expo(v) - Expo(u) < Minexpo)
		return sign < 0 ? app_neg(u) : (real) Copy(u);
	return mk_approx(
		sign * (Frac(u) - ldexp(Frac(v), (int)(Expo(v) - Expo(u)))),
		Expo(u));
#else !EXT_RANGE
	return mk_approx(Frac(u) - Frac(v), 0.0);
#endif !EXT_RANGE
}

Visible real app_neg(u) real u; {
	return mk_approx(-Frac(u), Expo(u));
}

Visible real app_prod(u, v) real u, v; {
	return mk_approx(Frac(u) * Frac(v), Expo(u) + Expo(v));
}

Visible real app_quot(u, v) real u, v; {
	if (Frac(v) == 0.0) {
		error(MESS(701, "in u/v, v is zero"));
		return (real) Copy(u);
	}
	return mk_approx(Frac(u) / Frac(v), Expo(u) - Expo(v));
}

/*
	YIELD log"(frac, expo):
		CHECK frac > 0
		RETURN normalize"(expo*logtwo + log(frac), 0)
*/

Visible real app_log(v) real v; {
	double frac = Frac(v), expo = Expo(v);
	if (frac <= 0) {
		error(MESS(702, "in log x, x is <= 0"));
		return (real) Copy(app_0);
	}
	return mk_approx(expo*logtwo + log(frac), 0.0);
}

/*
	YIELD exp"(frac, expo):
		IF expo < minexpo: RETURN zero"
		WHILE expo < 0: PUT frac/2, expo+1 IN frac, expo
		PUT exp frac IN f
		PUT normalize"(f, 0) IN f, e
		WHILE expo > 0:
			PUT (f, e) prod" (f, e) IN f, e
			PUT expo-1 IN expo
		RETURN f, e
*/

Visible real app_exp(v) real v; {
#ifdef EXT_RANGE
	expint ei;
	double frac = Frac(v), expo = Expo(v), new_expo;
	static double canexp;
	if (!canexp) canexp = floor(log(log(Maxreal)) / logtwo);
	if (expo <= canexp) {
		if (expo < Minexpo) return mk_approx(1.0, 0.0);
		frac = ldexp(frac, (int)expo);
		expo = 0;
	}
	else if (expo >= Maxexpo) {
		/* Definitely too big (the real boundary is much smaller
		   but here we are in danger of overflowing new_expo
		   in the loop below) */
		return mk_approx(1.0, Maxreal); /* Force an error! */
	}
	else {
		frac = ldexp(frac, (int)canexp);
		expo -= canexp;
	}
	frac = exp(frac);
	new_expo = 0;
	while (expo > 0 && frac != 0) {
		frac = frexp(frac, &ei);
		new_expo += ei;
		frac *= frac;
		new_expo += new_expo;
		--expo;
	}
	return mk_approx(frac, new_expo);
#else !EXT_RANGE
	return mk_approx(exp(Frac(v)), 0.0);
#endif !EXT_RANGE
}

/*
	YIELD (frac, expo) power" v":
		\   (f*2**e)**v =
		\ = f**v * 2**(e*v) =
		\ = f**v * 2**((e*v) mod 1) * 2**floor(e*v) .
		PUT exp"(v" prod" normalize"(log frac, 0)) IN temp1" \ = f**v
		PUT expo*numval(v") IN ev \ = e*v
		PUT exp(logtwo * (ev - floor ev))) IN temp2 \ = 2**(ev mod 1)
		PUT temp1" IN f, e
		RETURN normalize"(f*temp2, e + floor ev)
*/

Visible real app_power(u, v) real u, v; {
	double frac = Frac(u);
#ifdef EXT_RANGE
	real logfrac, vlogfrac, result;
	double expo = Expo(u), rest;
#endif !EXT_RANGE
	if (frac <= 0) {
		if (frac < 0) error(MESS(703, "in 0**v, v is negative"));
		if (v == app_0) return mk_approx(1.0, 0.0); /* 0**0 = 1 */
		return (real) Copy(app_0); /* 0**x = 0 */
	}
#ifdef EXT_RANGE
	frac *= 2, expo -= 1; /* Renormalize to 1 < frac <= 2, so log frac > 0 */
	logfrac = mk_approx(log(frac), 0.0);
	vlogfrac = app_prod(v, logfrac);
	result = app_exp(vlogfrac);
	/* But what if result overflows but expo is very negative??? */
	if (still_ok) {
		expo *= numval((value)v);
		rest = expo - floor(expo);
		frac = Frac(result) * exp(logtwo*rest);
		expo = Expo(result) + floor(expo);
	}
	release((value)logfrac), release((value)vlogfrac), release((value)result);
	return mk_approx(frac, expo);
#else !EXT_RANGE
	return mk_approx(exp(log(frac) * Frac(v)), 0.0);
#endif !EXT_RANGE
}

Visible int app_comp(u, v) real u, v; {
	double xu, xv;
#ifdef EXT_RANGE
	double eu, ev;
#endif EXT_RANGE
	if (u == v) return 0;
	xu = Frac(u), xv = Frac(v);
#ifdef EXT_RANGE
	if (xu*xv > 0) {
		eu = Expo(u), ev = Expo(v);
		if (eu < ev) return xu < 0 ? 1 : -1;
		if (eu > ev) return xu < 0 ? -1 : 1;
	}
#endif EXT_RANGE
	if (xu < xv) return -1;
	if (xu > xv) return 1;
	return 0;
}

Visible value app_floor(u) real u; {
#ifdef EXT_RANGE
	integer v, w;
	value twotow, result;
	if (Expo(u) <= Dblbits)
		return (value)
			mk_int(floor(ldexp(Frac(u),
				(int)(Expo(u) < 0 ? -1 : Expo(u)))));
	v = mk_int(ldexp(Frac(u), Dblbits));
	w = mk_int(Expo(u) - Dblbits);
	twotow = power((value)int_2, (value)w);
	result = prod((value)v, twotow);
	release((value) v), release((value) w), release(twotow);
	if (!Integral(result)) syserr(MESS(704, "app_floor: result not integral"));
	return result;
#else !EXT_RANGE
	return (value) mk_int(floor(Frac(u)));
#endif !EXT_RANGE
}
