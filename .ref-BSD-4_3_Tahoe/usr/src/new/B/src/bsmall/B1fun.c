/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1fun.c,v 1.1 84/06/28 00:48:54 timo Exp $ */

/* B functions */
#include "b.h"
#include "b1obj.h"
#include "b2sem.h"
#include "B1num.h"

#define Maxlen(x, y) (Length(x) > Length(y) ? Length(x) : Length(y))
#define Sumlen(x, y) (Length(x) + Length(y))

value sum(x, y) register value x, y; {
	value r, z;
	Checknum(x); Checknum(y);
	if (!Exact(x) || !Exact(y)) return mk_approx(Numval(x) + Numval(y));
	z= mk_exact(Denominator(x), Denominator(y), 0);
	r= mk_exact(Numerator(x)*Denominator(z)+Numerator(y)*Numerator(z),
			Denominator(x)*Denominator(z), Maxlen(x, y));
	release(z);
	return r;
}

value negated(x) register value x; {
	Checknum(x);
	if (!Exact(x)) return mk_approx(-Numval(x));
	return mk_exact(-Numerator(x), Denominator(x), Length(x));
}

value diff(x, y) register value x, y; {
	value r, my;
	r= sum(x, my= negated(y));
	release(my);
	return r;
}

value inverse(x) value x;{
	Checknum(x);
	if (Numval(x) == 0) error("in x/y, y is zero");
	if (!Exact(x)) return mk_approx(1.0/Numval(x));
	return mk_exact(Denominator(x), Numerator(x), Length(x));
}

value prod(x, y) register value x, y; {
	value a, b, r;
	Checknum(x); Checknum(y);
	if (!Exact(x) || !Exact(y)) return mk_approx(Numval(x) * Numval(y));
	a= mk_exact(Numerator(x), Denominator(y), 0);
	b= mk_exact(Numerator(y), Denominator(x), 0);
	r= mk_exact(Numerator(a)*Numerator(b), Denominator(a)*Denominator(b),
						Sumlen(x, y));
	release(a); release(b);
	return r;
}

value quot(x, y) register value x, y; {
	value r, iy;
	r= prod(x, iy= inverse(y));
	release(iy);
	return r;
}

#define Even(x) ((x) == Two*floor((x)/Two))
value power(x, y) register value x, y; {
	Checknum(x); Checknum(y);
	if (Exact(y)) {
		integer py= Numerator(y), qy= Denominator(y);
		if (Integral(y) && Exact(x)) {
			integer px, qx, ppx, pqx, Ppx, Pqx;
			if (py == Zero) return mk_int(One);
			if (py > Zero) {
				px= Numerator(x);
				qx= Denominator(x);
			} else {
				py= -py;
				px= Denominator(x);
				qx= Numerator(x);
			}
			ppx= pqx= One;
			Ppx= px; Pqx= qx;
			while (py >= Two) {
				if (!Even(py)) {
					ppx*= Ppx; pqx*= Pqx;
				}
				Ppx*= Ppx; Pqx*= Pqx;
				py= floor(py/Two);
			}
			ppx*= Ppx; pqx*= Pqx;
			return mk_exact(ppx, pqx, 0);
		} /* END Integral(y) && Exact(x) */
		else {
			double vx= Numval(x);
			short sx= vx < 0 ? -1 : vx == 0 ? 0 : 1;
			if (sx < 0 && Even(qy))
			    error("in x**(p/q), x is negative and q is even");
			if (sx == 0 && py < Zero)
			    error("0**y with negative y");
			if (sx < 0 && Even(py)) sx= 1;
			return mk_approx(sx * pow(fabs(vx), py/qy));
		}
	} /* END Exact(y) */
	else {
		double vx= Numval(x), vy= Approxval(y);
		if (vy == 0) return mk_approx(1.0);
		if (vx < 0)
			error("in x**y, x is negative and y is not exact");
		if (vx == 0 && vy < 0)
			error("0E0**y with negative y");
		return mk_approx(pow(vx, vy));
	}
}

value root2(n, x) register value n, x; {
	value r, in;
	Checknum(n);
	if (Numval(n) == 0) error("in x root y, x is zero");
	r= power(x, in= inverse(n));
	release(in);
	return r;
}

value absval(x) register value x; {
	Checknum(x);
	if (!Exact(x)) return mk_approx(fabs(Numval(x)));
	return mk_exact((integer) fabs((double) Numerator(x)), Denominator(x), Length(x));
}

value signum(x) register value x; {
	double v= numval(x);
	return mk_int(v < 0 ? -One : v == 0 ? Zero : One);
}

value floorf(x) register value x; {
	return mk_int(floor(numval(x)));
}

value ceilf(x) register value x; {
	return mk_int(ceil(numval(x)));
}

value round1(x) register value x; {
	return mk_int(floor(numval(x) + .5));
}

value round2(n, x) register value n, x; {
	value ten, tenp, xtenp, r0, r;
	Checknum(n);
	if (!Integral(n)) error("in n round x, n is not an integer");
	ten= mk_integer(10);
	tenp= power(ten, n);
	xtenp= prod(x, tenp);
	r0= round1(xtenp);
	r= mk_exact(Numerator(r0), Numerator(tenp), propintlet((int) Numerator(n)));
	release(ten); release(tenp); release(xtenp); release(r0);
	return r;
}

value mod(a, n) register value a, n; {
	value f, p, d;
	Checknum(a); Checknum(n);
	f= mk_int(floor(Numval(a) / Numval(n)));
	p= prod(n, f);
	d= diff(a, p);
	release(f); release(p);
	return d;
}

double lastran;

setran (seed) double seed;
{double x;
 x= seed >= 0 ? seed : -seed;
 while (x >= 1) x/= 10;
 lastran= floor(67108864.0 * x);
}

set_random(v) value v; {
	setran((double) hash(v));
}

value random() /* 0 <= r < 1 */
{double p;
 p= 26353589.0 * lastran + 1;
 lastran= p - 67108864.0 * floor (p / 67108864.0);
 return mk_approx(lastran / 67108864.0);
}

value root1(v) value v; {
	value two= mk_integer(2);
	v= root2(two, v);
	release(two);
	return(v);
}

value pi() { return mk_approx(3.141592653589793238462); }
value e() { return mk_approx(exp(1.0)); }

value sin1(v) value v; { return mk_approx(sin(numval(v))); }
value cos1(v) value v; { return mk_approx(cos(numval(v))); }
value tan1(v) value v; { return mk_approx(tan(numval(v))); }
value atn1(v) value v; { return mk_approx(atan(numval(v))); }
value exp1(v) value v; { return mk_approx(exp(numval(v))); }
value log1(v) value v; { return mk_approx(log(numval(v))); }

value log2(u, v) value u, v;{
	return mk_approx(log(numval(v)) / log(numval(u)));
}

value atn2(u, v) value u, v; {
	return mk_approx(atan2(numval(v), numval(u)));
}
