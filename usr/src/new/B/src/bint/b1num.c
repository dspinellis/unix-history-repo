/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1num.c,v 1.4 85/08/22 16:51:59 timo Exp $
*/

/* B numbers, basic external interface */

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b1num.h"

/*
 * This file contains operations on numbers that are not predefined
 * B functions (the latter are defined in `bfun.c').
 * This includes conversion of numeric B values to C `int's and
 * `double's, (numval() and intval()),
 * but also utilities for comparing numeric values and hashing a numeric
 * value to something usable as initialization for the random generator
 * without chance of overflow (so numval(v) is unusable).
 * It is also possible to build numbers of all types using mk_integer,
 * mk_exact (or mk_rat) and mk_approx.  Note the rather irregular
 * (historical!) argument structure for these: mk_approx has a
 * `double' argument, where mk_exact and mk_rat have two values
 * which must be `integer' (not `int's!) as arguments.
 * The random number generator used by the DRAW and CHOOSE statements
 * is also in this file.
 */

/*
 * ival is used internally by intval() and large().
 * It converts an integer to double precision, yielding
 * a huge value when overflow occurs (but giving no error).
 */

Hidden double ival(u) integer u; {
	double x = 0;
	register int i;

	if (IsSmallInt(u)) return SmallIntVal(u);
	for (i = Length(u)-1; i >= 0; --i) {
		if (x >= Maxreal/BASE)
			return Msd(u) < 0 ? -Maxreal : Maxreal;
		x = x*BASE + Digit(u, i);
	}

	return x;
}


/* Determine if a value may be converted to an int */

Visible bool large(v) value v; {
	double r;
	if (!Is_number(v) || !integral(v)) {
		error(MESS(1300, "number not an integer"));
		return No;
	}
	if (Rational(v)) v= (value) Numerator((rational)v);
	r= ival((integer)v);
	if (r > Maxint) return Yes;
	return No;
}

/* return the C `int' value of a B numeric value, if it exists. */

Visible int intval(v) value v; {
	/* v must be an Integral number or a Rational with Denominator==1
	    which may result from n round x [via mk_exact]!. */
	double i;
	if (IsSmallInt(v)) i= SmallIntVal(v);
	else {
		if (!Is_number(v)) syserr(MESS(1301, "intval on non-number"));
		if (!integral(v)) {
			error(MESS(1302, "number not an integer"));
			return 0;
		}
		if (Rational(v)) v= (value) Numerator((rational)v);
		i= ival((integer)v);
	}
	if (i > Maxint || i < -Maxint) {
		error(MESS(1303, "exceedingly large integer"));
		return 0;
	}
	return (int) i;
}


/* convert an int to an intlet */

Visible intlet propintlet(i) int i; {
	if (i > Maxintlet || i < -Maxintlet) {
		error(MESS(1304, "exceedingly large integer"));
		return 0;
	}
	return i;
}


/*
 * determine if a number is integer
 */

Visible bool integral(v) value v; {
	if (Integral(v) || (Rational(v) && Denominator((rational)v) == int_1))
		return Yes;
	else return No;
}

/*
 * mk_exact makes an exact number out of two integers.
 * The third argument is the desired number of digits after the decimal
 * point when the number is printed (see comments in `bfun.c' for
 * `round' and code in `bnuC.c').
 * This printing size (if positive) is hidden in the otherwise nearly
 * unused * 'Size' field of the value struct
 * (cf. definition of Rational(v) etc.).
 */

Visible value mk_exact(p, q, len) integer p, q; int len; {
	rational r = mk_rat(p, q, len);

	if (Denominator(r) == int_1 && len <= 0) {
		integer i = (integer) Copy(Numerator(r));
		release((value) r);
		return (value) i;
	}

	return (value) r;
}

#define uSMALL 1
#define uINT 2
#define uRAT 4
#define uAPP 8
#define vSMALL 16
#define vINT 32
#define vRAT 64
#define vAPP 128

Visible relation numcomp(u, v) value u, v; {
	int tu, tv; relation s;

	if (IsSmallInt(u)) tu = uSMALL;
	else if (Length(u) >= 0) tu = uINT;
	else if (Length(u) <= -2) tu = uRAT;
	else tu = uAPP;
	if (IsSmallInt(v)) tv = vSMALL;
	else if (Length(v) >= 0) tv = vINT;
	else if (Length(v) <= -2) tv = vRAT;
	else tv = vAPP;

	switch (tu|tv) {

	case uSMALL|vSMALL: return SmallIntVal(u) - SmallIntVal(v);

	case uSMALL|vINT:
	case uINT|vSMALL:
	case uINT|vINT: return int_comp((integer)u, (integer)v);

	case uSMALL|vRAT:
	case uINT|vRAT:
		u = (value) mk_rat((integer)u, int_1, 0);
		s = rat_comp((rational)u, (rational)v);
		release(u);
		return s;

	case uRAT|vRAT:
		return rat_comp((rational)u, (rational)v);

	case uRAT|vSMALL:
	case uRAT|vINT:
		v = (value) mk_rat((integer)v, int_1, 0);
		s = rat_comp((rational)u, (rational)v);
		release(v);
		return s;

	case uSMALL|vAPP:
	case uINT|vAPP:
	case uRAT|vAPP:
		u = approximate(u);
		s = app_comp((real)u, (real)v);
		release(u);
		return s == 0 ? -1 : s; /* u < ~u = v */

	case uAPP|vAPP:
		return app_comp((real)u, (real)v);

	case uAPP|vSMALL:
	case uAPP|vINT:
	case uAPP|vRAT:
		v = approximate(v);
		s = app_comp((real)u, (real)v);
		release(v);
		return s == 0 ? 1 : s; /* u = ~v > v */

	default: syserr(MESS(1305, "num_comp")); /* NOTREACHED */

	}
}


/*
 * Deliver 10**n, where n is a (maybe negative!) C integer.
 * The result is a value (integer or rational, actually).
 */

Visible value tento(n) int n; {
	if (n < 0) {
		integer i= int_tento(-n);
		value v= (value) mk_exact(int_1, i, 0);
		release((value) i);
		return v;
	}
	return (value) int_tento(n);
}


/*
 * numval returns the numerical value of any numeric B value
 * as a C `double'.
 */

Visible double numval(u) value u; {
	double expo, frac;

	if (!Is_number(u)) {
		error(MESS(1306, "value not a number"));
		return 0.0;
	}
	u = approximate(u);
	expo = Expo((real)u), frac = Frac((real)u);
	release(u);
	if (expo > Maxexpo) {
		error(MESS(1307, "approximate number too large to be handled"));
		return 0.0;
	}
	if(expo < Minexpo) return 0.0;
	return ldexp(frac, (int)expo);
}


/*
 * Random numbers
 */


/*
 * numhash produces a `double' number that depends on the value of
 * v, useful for initializing the random generator.
 * Needs rewriting, so that for instance numhash(n) never equals n.
 * The magic numbers here are chosen at random.
 */

Visible double numhash(v) value v; {
	if (Integral(v)) {
		double d = 0;
		register int i;

		if (IsSmallInt(v)) return SmallIntVal(v);
		for (i = Length(v) - 1; i >= 0; --i) {
			d *= 2;
			d += Digit((integer)v, i);
		}

		return d;
	}

	if (Rational(v))
		return .777 * numhash((value) Numerator((rational)v)) +
		       .123 * numhash((value) Denominator((rational)v));

	return numval(v); /* Which fails for HUGE reals.  Never mind. */
}


/* Initialize the random generator */

double lastran;

Hidden Procedure setran (seed) double seed; {
	double x;

	x = seed >= 0 ? seed : -seed;
	while (x >= 1) x /= 10;
	lastran = floor(67108864.0*x);
}

Visible Procedure set_random(v) value v; {
	setran((double) hash(v));
}


/* Return a random number in [0, 1). */

Visible value random() {
	double p;

	p = 26353589.0 * lastran + 1;
	lastran = p - 67108864.0*floor(p/67108864.0);

	return (value) mk_approx(lastran / 67108864.0, 0.0);
}

Visible Procedure initnum() {
	rat_init();
	setran((double) SEED);
}

Visible Procedure endnum() {
	endrat();
}
