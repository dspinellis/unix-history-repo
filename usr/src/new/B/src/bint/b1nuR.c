/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1nuR.c,v 1.4 85/08/22 16:51:49 timo Exp $
*/

/* Rational arithmetic */

#include "b.h"
#include "b0con.h"
#include "b1obj.h"
#include "b1num.h"
#include "b3err.h"

/* Length calculations used for fraction sizes: */
#define Maxlen(u, v) \
	(Roundsize(u) > Roundsize(v) ? Roundsize(u) : Roundsize(v))
#define Sumlen(u, v) (Roundsize(u)+Roundsize(v))
#define Difflen(u, v) (Roundsize(u)-Roundsize(v))

/* To shut off lint and other warnings: */
#undef Copy
#define Copy(x) ((integer)copy((value)(x)))

/* Globally used constants */

rational rat_zero;
rational rat_half;

/* Make a normalized rational from two integers */

Visible rational mk_rat(x, y, len) integer x, y; int len; {
	rational a;
	integer u,v;

	if (y == int_0) {
		if (interrupted)
			return rat_zero;
		syserr(MESS(1200, "mk_rat(x, y) with y=0"));
	}

	if (x == int_0 && len <= 0) return (rational) Copy(rat_zero);

	if (Msd(y) < 0) {	/* interchange signs */
		u = int_neg(x);
		v = int_neg(y);
	} else {
		u = Copy(x);
		v = Copy(y);
	}

	a = (rational) grab_rat();
	if (len > 0 && len+2 <= Maxintlet) Length(a) = -2 - len;

	if (u == int_0 || v == int_1) {
		/* No simplification possible */
		Numerator(a) = Copy(u);
		Denominator(a) = int_1;
	} else {
		integer g, abs_u;

		if (Msd(u) < 0) abs_u = int_neg(u);
		else abs_u = Copy(u);
		g = int_gcd(abs_u, v);
		release((value) abs_u);

		if (g != int_1) {
			Numerator(a) = int_quot(u, g);
			Denominator(a) = int_quot(v, g);
		} else {
			Numerator(a) = Copy(u);
			Denominator(a) = Copy(v);
		}
		release((value) g);
	}

	release((value) u); release((value) v);

	return a;
}


/* Arithmetic on rational numbers */

/* Shorthands: */
#define N(u) Numerator(u)
#define D(u) Denominator(u)

Visible rational rat_sum(u, v) register rational u, v; {
	integer t1, t2, t3, t4;
	rational a;

	t2= int_prod(N(u), D(v));
	t3= int_prod(N(v), D(u));
	t1= int_sum(t2, t3);
	t4= int_prod(D(u), D(v));
	a= mk_rat(t1, t4, Maxlen(u, v));
	release((value) t1); release((value) t2);
	release((value) t3); release((value) t4);

	return a;
}


Visible rational rat_diff(u, v) register rational u, v; {
	integer t1, t2, t3, t4;
	rational a;

	t2= int_prod(N(u), D(v));
	t3= int_prod(N(v), D(u));
	t1= int_diff(t2, t3);
	t4= int_prod(D(u), D(v));
	a= mk_rat(t1, t4, Maxlen(u, v));
	release((value) t1); release((value) t2);
	release((value) t3); release((value) t4);

	return a;
}


Visible rational rat_prod(u, v) register rational u, v; {
	integer t1, t2;
	rational a;

	t1= int_prod(N(u), N(v));
	t2= int_prod(D(u), D(v));
	a= mk_rat(t1, t2, Sumlen(u, v));
	release((value) t1); release((value) t2);

	return a;
}


Visible rational rat_quot(u, v) register rational u, v; {
	integer t1, t2;
	rational a;

	if (Numerator(v) == int_0) {
		error(MESS(1201, "in u/v, v is zero"));
		return (rational) Copy(rat_zero);
	}

	t1= int_prod(N(u), D(v));
	t2= int_prod(D(u), N(v));
	a= mk_rat(t1, t2, Difflen(u, v));
	release((value) t1); release((value) t2);

	return a;
}


Visible rational rat_neg(u) register rational u; {
	register rational a;

	/* Avoid a real subtraction from zero */

	if (Numerator(u) == int_0) return (rational) Copy(u);

	a = (rational) grab_rat();
	N(a) = int_neg(N(u));
	D(a) = Copy(D(u));
	Length(a) = Length(u);

	return a;
}


/* Rational number to the integral power */

Visible rational rat_power(a, n) rational a; integer n; {
	integer u, v, tu, tv, temp;

	if (n == int_0) return mk_rat(int_1, int_1, 0);

	if (Msd(n) < 0) {
		if (Numerator(a) == int_0) {
			error(MESS(1202, "in 0**n, n is negative"));
			return (rational) Copy(a);
		}
		if (Msd(Numerator(a)) < 0) {
			u= int_neg(Denominator(a));
			v = int_neg(Numerator(a));
		}
		else {
			u = Copy(Denominator(a));
			v = Copy(Numerator(a));
		}
		n = int_neg(n);
	} else {
		if (Numerator(a) == int_0) return (rational) Copy(a);
			/* To avoid necessary simplification later on */
		u = Copy(Numerator(a));
		v = Copy(Denominator(a));
		n = Copy(n);
	}

	tu = int_1;
	tv = int_1;

	while (n != int_0 && !interrupted) {
		if (Odd(Lsd(n))) {
			if (u != int_1) {
				temp = tu;
				tu = int_prod(u, tu);
				release((value) temp);
			}
			if (v != int_1) {
				temp = tv;
				tv = int_prod(v, tv);
				release((value) temp);
			}
			if (n == int_1)
				break; /* Avoid useless last squaring */
		}

		/* Square u, v */

		if (u != int_1) {
			temp = u;
			u = int_prod(u, u);
			release((value) temp);
		}
		if (v != int_1) {
			temp = v;
			v = int_prod(v, v);
			release((value) temp);
		}

		n = int_half(n);
	} /* while (n!=0) */

	release((value) n);
	release((value) u);
	release((value) v);
	a = (rational) grab_rat();
	Numerator(a) = tu;
	Denominator(a) = tv;

	return a;
}


/* Compare two rational numbers */

Visible relation rat_comp(u, v) register rational u, v; {
	int sd, su, sv;
	integer nu, nv;

	/* 1. Compare pointers */
	if (u == v || N(u) == N(v) && D(u) == D(v)) return 0;

	/* 2. Either zero? */
	if (N(u) == int_0) return int_comp(int_0, N(v));
	if (N(v) == int_0) return int_comp(N(u), int_0);

	/* 3. Compare signs */
	su = Msd(N(u));
	sv = Msd(N(v));
	su = (su>0) - (su<0);
	sv = (sv>0) - (sv<0);
	if (su != sv) return su > sv ? 1 : -1;

	/* 4. Compute numerator of difference and return sign */
	nu= int_prod(N(u), D(v));
	nv= int_prod(N(v), D(u));
	sd= int_comp(nu, nv);
	release((value) nu); release((value) nv);
	return sd;
}

Visible Procedure rat_init() {
	rat_zero = (rational) grab_rat();
	Numerator(rat_zero) = int_0;
	Denominator(rat_zero) = int_1;

	rat_half = (rational) grab_rat();
	Numerator(rat_half) = int_1;
	Denominator(rat_half) = int_2;
}

Visible Procedure endrat() {
	release((value) rat_zero);
	release((value) rat_half);
}
