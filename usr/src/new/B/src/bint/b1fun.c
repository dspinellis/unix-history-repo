/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1fun.c,v 1.4 85/08/22 16:48:24 timo Exp $
*/

/* Functions defined on numeric values. */

#include <errno.h> /* For EDOM and ERANGE */

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b1num.h"

/*
 * The visible routines here implement predefined B arithmetic operators,
 * taking one or two numeric values as operands, and returning a numeric
 * value.
 * No type checking of operands is done: this must be done by the caller.
 */

typedef value (*valfun)();
typedef rational (*ratfun)();
typedef real (*appfun)();
typedef double (*mathfun)();

/*
 * For the arithmetic functions (+, -, *, /) the same action is needed:
 * 1) if both operands are Integral, use function from int_* submodule;
 * 2) if both are Exact, use function from rat_* submodule (after possibly
 *    converting one of them from Integral to Rational);
 * 3) otherwise, make both approximate and use function from app_*
 *    submodule.
 * The functions performing the appropriate action for each of the submodules
 * are passed as parameters.
 * Division is a slight exception, since i/j can be a rational.
 * See `quot' below.
 */

Hidden value dyop(u, v, int_fun, rat_fun, app_fun)
	value u, v;
	valfun int_fun;
	ratfun rat_fun;
	appfun app_fun;
{
	if (Integral(u) && Integral(v))	/* Use integral operation */
		return (*int_fun)(u, v);

	if (Exact(u) && Exact(v)) {
		rational u1, v1, a;

		/* Use rational operation */

		u1 = Integral(u) ? mk_rat((integer)u, int_1, 0) :
				(rational) Copy(u);
		v1 = Integral(v) ? mk_rat((integer)v, int_1, 0) :
				(rational) Copy(v);
		a = (*rat_fun)(u1, v1);
		release((value) u1);
		release((value) v1);

		if (Denominator(a) == int_1 && Roundsize(a) == 0) {
			integer b = (integer) Copy(Numerator(a));
			release((value) a);
			return (value) b;
		}

		return (value) a;
	}

	/* Use approximate operation */

	{
		real u1, v1, a;
		u1 = Approximate(u) ? (real) Copy(u) : (real) approximate(u);
		v1 = Approximate(v) ? (real) Copy(v) : (real) approximate(v);
		a = (*app_fun)(u1, v1);
		release((value) u1);
		release((value) v1);

		return (value) a;
	}
}


Hidden integer isum(u, v) integer u, v; {
	return int_sum(u, v);
}

Visible value sum(u, v) value u, v; {
	if (IsSmallInt(u) && IsSmallInt(v))
		return mk_integer(SmallIntVal(u) + SmallIntVal(v));
	return dyop(u, v, (value (*)())isum, rat_sum, app_sum);
}

Hidden integer idiff(u, v) integer u, v; {
	return int_diff(u, v);
}

Visible value diff(u, v) value u, v; {
	if (IsSmallInt(u) && IsSmallInt(v))
		return mk_integer(SmallIntVal(u) - SmallIntVal(v));
	return dyop(u, v, (value (*)())idiff, rat_diff, app_diff);
}

Visible value prod(u, v) value u, v; {
	if (IsSmallInt(u) && IsSmallInt(v))
		return (value)
			mk_int((double)SmallIntVal(u) * (double)SmallIntVal(v));
	return dyop(u, v, (value (*)())int_prod, rat_prod, app_prod);
}


/*
 * We cannot use int_quot (which performs integer division with truncation).
 * Here is the routine we need.
 */

Hidden value xxx_quot(u, v) integer u, v; {

	if (v == int_0) {
		error(MESS(400, "in i/j, j is zero"));
		return (value) Copy(u);
	}

	return mk_exact(u, v, 0);
}

Visible value quot(u, v) value u, v; {
	return dyop(u, v, xxx_quot, rat_quot, app_quot);
}


/*
 * Unary minus and abs follow the same principle but with only one operand.
 */

Visible value negated(u) value u; {
	if (IsSmallInt(u)) return mk_integer(-SmallIntVal(u));
	if (Integral(u))
		return (value) int_neg((integer)u);
	if (Rational(u))
		return (value) rat_neg((rational)u);
	return (value) app_neg((real)u);
}


Visible value absval(u) value u; {
	if (Integral(u)) {
		if (Msd((integer)u) < 0)
			return (value) int_neg((integer)u);
	} else if (Rational(u)) {
		if (Msd(Numerator((rational)u)) < 0)
			return (value) rat_neg((rational)u);
	} else if (Approximate(u) && Frac((real)u) < 0)
		return (value) app_neg((real)u);

	return Copy(u);
}


/*
 * The remaining operators follow less similar paths and some of
 * them contain quite subtle code.
 */

Visible value mod(u, v) value u, v; {
	value p, q, m, n;

	if (v == (value)int_0 ||
		Rational(v) && Numerator((rational)v) == int_0 ||
		Approximate(v) && Frac((real)v) == 0) {
		error(MESS(401, "in x mod y, y is zero"));
		return Copy(u);
	}

	if (Integral(u) && Integral(v))
		return (value) int_mod((integer)u, (integer)v);

	/* Compute `u-v*floor(u/v)', as in the formal definition of `u mod v'. */

	q = quot(u, v);
	n = floorf(q);
	release(q);
	p = prod(n, v);
	release(n);
	m = diff(u, p);
	release(p);

	return m;
}


/*
 * u**v has the most special cases of all the predefined arithmetic functions.
 */

Visible value power(u, v) value u, v; {
	real ru, rv, rw;
	if (Exact(u) && (Integral(v) ||
			/* Next check catches for integers disguised as rationals: */
			Rational(v) && Denominator((rational)v) == int_1)) {
		rational a;
		integer b = Integral(v) ? (integer)v : Numerator((rational)v);
			/* Now b is really an integer. */

		u = Integral(u) ? (value) mk_rat((integer)u, int_1, 0) :
				Copy(u);
			
		a = rat_power((rational)u, b);
		release(u);
		if (Denominator(a) == int_1) { /* Make integral result */
			b = (integer) Copy(Numerator(a));
			release((value) a);
			return (value)b;
		}
		return (value)a;
	}

	if (Exact(v)) {
		integer vn, vd;
		int s;
		ru = (real) approximate(u);
		s = (Frac(ru) > 0) - (Frac(ru) < 0);

		if (s < 0) rv = app_neg(ru), release((value)ru), ru = rv;
		if (Integral(v)) {
			vn = (integer)v;
			vd = int_1;
		} else {
			vd = Denominator((rational)v);
			if (s < 0 && Even(Lsd(vd)))
				error(MESS(402, "in x**(p/q), x is negative and q is even"));
			vn = Numerator((rational)v);
		}
		if (vn == int_0) {
			release((value)ru);
			return one;
		}
		if (s == 0 && Msd(vn) < 0) {
			error(MESS(403, "in 0**y, y is negative"));
			return (value) ru;
		}
		if (s < 0 && Even(Lsd(vn)))
			s = 1;
		rv = (real) approximate(v);
		rw = app_power(ru, rv);
		release((value)ru), release((value)rv);
		if (s < 0) ru = app_neg(rw), release((value)rw), rw = ru;
		return (value) rw;
	}

	/* Everything else: we now know u or v is approximate */

	ru = (real) approximate(u);
	if (Frac(ru) < 0) {
		error(MESS(404, "in x**y, x is negative and y is not exact"));
		return (value) ru;
	}
	rv = (real) approximate(v);
	if (Frac(ru) == 0 && Frac(rv) < 0) {
		error(MESS(405, "in 0**y, y is negative"));
		release((value)rv);
		return (value) ru;
	}
	rw = app_power(ru, rv);
	release((value)ru), release((value)rv);
	return (value) rw;
}


/*
 * floor: for approximate numbers app_floor() is used;
 * for integers it is a no-op; other exact numbers effectively calculate
 * u - (u mod 1).
 */

Visible value floorf(u) value u; {
	integer quo, rem, v;
	digit div;

	if (Integral(u)) return Copy(u);
	if (Approximate(u)) return app_floor((real)u);

	/* It is a rational number */

	div = int_ldiv(Numerator((rational)u), Denominator((rational)u),
		&quo, &rem);
	if (div < 0 && rem != int_0) { /* Correction for negative noninteger */
		v = int_diff(quo, int_1);
		release((value) quo);
		quo = v;
	}
	release((value) rem);
	return (value) quo;
}


/*
 * ceiling x is defined as -floor(-x);
 * and that's how it's implemented, except for integers.
 */

Visible value ceilf(u) value u; {
	value v;
	if (Integral(u)) return Copy(u);
	u = negated(u);
	v = floorf(u);
	release(u);
	u = negated(v);
	release(v);
	return u;
}


/*
 * round u is defined as floor(u+0.5), which is what is done here,
 * except for integers which are left unchanged.
 */

Visible value round1(u) value u; {
	value v, w;

	if (Integral(u)) return Copy(u);

	v = sum(u, (value)rat_half);
	w = floorf(v);
	release(v);

	return w;
}


/*
 * u round v is defined as 10**-u * round(v*10**u).
 * A complication is that u round v is always printed with exactly u digits
 * after the decimal point, even if this involves trailing zeros,
 * or if v is an integer.
 * Consequently, the result is always kept as a rational, even if it can be
 * simplified to an integer, and the size field of the rational number
 * (which is made negative to distinguish it from integers, and < -1 to
 * distinguish it from approximate numbers) is used to store the number of
 * significant digits.
 * Thus a size of -2 means a normal rational number, and a size < -2
 * means a rounded number to be printed with (-2 - length) digits
 * after the decimal point.  This last expression can be retrieved using
 * the macro Roundsize(v) which should only be applied to Rational
 * numbers.
 */

Visible value round2(u, v) value u, v; {
	value w, tenp;
	int i;

	if (!Integral(u)) {
		error(MESS(406, "in n round x, n is not an integer"));
		i = 0;
	} else
		i = propintlet(intval(u));

	tenp = tento(i);

	v = prod(v, tenp);
	w = round1(v);

	release(v);
	v = quot(w, tenp);
	release(w);
	release(tenp);

	if (i > 0) {	/* Set number of digits to be printed */
		if (propintlet(-2 - i) < -2) {
			if (Rational(v))
				Length(v) = -2 - i;
			else if (Integral(v)) {
				w = v;
				v = mk_exact((integer) w, int_1, i);
				release(w);
			}
		}
	}

	return v;
}


/*
 * sign u inspects the sign of either u, u's numerator or u's fractional part.
 */

Visible value signum(u) value u; {
	int s;

	if (Exact(u)) {
		if (Rational(u))
			u = (value) Numerator((rational)u);
		s = u==(value)int_0 ? 0 : Msd((integer)u) < 0 ? -1 : 1;
	} else
		s = Frac((real)u) > 0 ? 1 : Frac((real)u) < 0 ? -1 : 0;

	return MkSmallInt(s);
}


/*
 * ~u makes an approximate number of any numerical value.
 */

Visible value approximate(u) value u; {
	double x, e, expo;
	register int i;
	if (Approximate(u)) return Copy(u);
	if (IsSmallInt(u))
		x = SmallIntVal(u), expo = 0;
	else if (Rational(u)) {
		rational r = (rational) u;
		integer p = Numerator(r), q;
		double xp, xq;
		int lp, lq;
		if (p == int_0)
			return Copy((value)app_0);
		q = Denominator(r);
		lp = IsSmallInt(p) ? 1 : Length(p);
		lq = IsSmallInt(q) ? 1 : Length(q);
		xp = 0, xq = 0;
		expo = lp - lq;
		if (IsSmallInt(p)) { xp = SmallIntVal(p); --expo; }
		else {
			for (i = Length(p)-1; i >= 0; --i) {
				xp = xp*BASE + Digit(p, i);
				--expo;
				if (xp < -Maxreal/BASE || xp > Maxreal/BASE)
					break;
			}
		}
		if (IsSmallInt(q)) { xq = SmallIntVal(q); ++expo; }
		else {
			for (i = Length(q)-1; i >= 0; --i) {
				xq = xq*BASE + Digit(q, i);
				++expo;
				if (xq > Maxreal/BASE)
					break;
			}
		}
		x = xp/xq;
	}
	else if (Integral(u)) {
		integer p = (integer) u;
		if (Length(p) == 0)
			return Copy((value)app_0);
		x = 0;
		expo = Length(p);
		for (i = Length(p)-1; i >= 0; --i) {
			x = x*BASE + Digit(p, i);
			--expo;
			if (x < -Maxreal/BASE || x > Maxreal/BASE)
				break;
		}
	}
	while (expo < 0 && fabs(x) > BASE/Maxreal) ++expo, x /= BASE;
	while (expo > 0 && fabs(x) < Maxreal/BASE) --expo, x *= BASE;
	if (expo != 0) {
		expo = expo*twologBASE + 1;
		e = floor(expo);
		x *= .5 * exp((expo-e) * logtwo);
	}
	else
		e = 0;
	return (value) mk_approx(x, e);
}


/*
 * numerator v returns the numerator of v, whenever v is an exact number.
 * For integers, that is v itself.
 */

Visible value numerator(v) value v; {
	if (!Exact(v)) {
		error(MESS(407, "*/ on approximate number"));
		return zero;
	}

	if (Integral(v)) return Copy(v);

	return (value) Copy(Numerator((rational)v));
}


/*
 * /*v returns the denominator of v, whenever v is an exact number.
 * For integers, that is 1.
 */

Visible value denominator(v) value v; {
	if (!Exact(v)) {
		error(MESS(408, "/* on approximate number"));
		return zero;
	}

	if (Integral(v)) return one;

	return (value) Copy(Denominator((rational)v));
}


/*
 * u root v is defined as v**(1/u), where u is usually but need not be
 * an integer.
 */

Visible value root2(u, v) value u, v; {
	if (u == (value)int_0 ||
		Rational(u) && Numerator((rational)u) == int_0 ||
		Approximate(u) && Frac((real)u) == 0) {
		error(MESS(409, "in n root x, n is zero"));
		v = Copy(v);
	} else {
		u = quot((value)int_1, u);
		v = power(v, u);
		release(u);
	}

	return v;
}

/* root x is computed more exactly than n root x, by doing
   one iteration step extra.  This ~guarantees root(n**2) = n. */

Visible value root1(v) value v; {
	value r = root2((value)int_2, v);
	value v_over_r, theirsum, result;
	if (Approximate(r) && Frac((real)r) == 0.0) return (value)r;
	v_over_r = quot(v, r);
	theirsum = sum(r, v_over_r), release(r), release(v_over_r);
	result = quot(theirsum, (value)int_2), release(theirsum);
	return result;
}

/* The rest of the mathematical functions */

Visible value pi() { return (value) mk_approx(3.141592653589793238463, 0.0); }
Visible value e() { return (value) mk_approx(2.718281828459045235360, 0.0); }

Hidden value trig(v, fun, zeroflag) value v; mathfun fun; bool zeroflag; {
	real w = (real) approximate(v);
	double expo = Expo(w), frac = Frac(w), x, result;
	extern int errno;
	if (expo <= Minexpo/2) {
		if (zeroflag) return (value) w; /* sin small x = x, etc. */
		frac = 0, expo = 0;
	}
	release((value)w);
	if (expo > Maxexpo) errno = EDOM;
	else {
		x = ldexp(frac, (int)expo);
		if (x >= Maxtrig || x <= -Maxtrig) errno = EDOM;
		else {
			errno = 0;
			result = (*fun)(x);
		}
	}
	if (errno != 0) {
		if (errno == ERANGE)
			error(MESS(410, "the result is too large to be representable"));
		else if (errno == EDOM)
			error(MESS(411, "x is too large to compute a meaningful result"));
		else error(MESS(412, "math library error"));
		return Copy((value)app_0);
	}
	return (value) mk_approx(result, 0.0);
}

Visible value sin1(v) value v; { return trig(v, sin, Yes); }
Visible value cos1(v) value v; { return trig(v, cos, No); }
Visible value tan1(v) value v; { return trig(v, tan, Yes); }

Visible value atn1(v) value v; {
	real w = (real) approximate(v);
	double expo = Expo(w), frac = Frac(w);
	if (expo <= Minexpo + 2) return (value) w; /* atan of small x = x */
	release((value)w);
	if (expo > Maxexpo) expo = Maxexpo;
	return (value) mk_approx(atan(ldexp(frac, (int)expo)), 0.0);
}

Visible value exp1(v) value v; {
	real w = (real) approximate(v);
	real x = app_exp(w);
	release((value)w);
	return (value) x;
}

Visible value log1(v) value v; {
	real w = (real) approximate(v);
	real x = app_log(w);
	release((value)w);
	return (value) x;
}

Visible value log2(u, v) value u, v;{
	value w;
	u = log1(u);
	v = log1(v);
	w = quot(v, u);
	release(u), release(v);
	return w;
}

Visible value atn2(u, v) value u, v; {
	real ru = (real) approximate(u), rv = (real) approximate(v);
	double uexpo = Expo(ru), ufrac = Frac(ru);
	double vexpo = Expo(rv), vfrac = Frac(rv);
	release((value)ru), release((value)rv);
	if (uexpo > Maxexpo) uexpo = Maxexpo;
	if (vexpo > Maxexpo) vexpo = Maxexpo;
	return (value) mk_approx(
		atan2(
			vexpo < Minexpo ? 0.0 : ldexp(vfrac, (int)vexpo),
			uexpo < Minexpo ? 0.0 : ldexp(ufrac, (int)uexpo)),
		0.0);
}
