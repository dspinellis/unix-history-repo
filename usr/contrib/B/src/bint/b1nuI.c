/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1nuI.c,v 1.4 85/08/22 16:51:13 timo Exp $
*/

/* Multi-precision integer arithmetic */

#include "b.h"
#include "b1obj.h"
#include "b1num.h"
#include "b0con.h"
#include "b3err.h"

/*
 * Number representation:
 * ======================
 *
 * (Think of BASE = 10 for ordinary decimal notation.)
 * A number is a sequence of N "digits" b1, b2, ..., bN
 * where each bi is in {0..BASE-1}, except for negative numbers,
 * where bN = -1.
 * The number represented by b1, ..., bN is
 *      b1*BASE**(N-1) + b2*BASE**(N-2) + ... + bN .
 * The base BASE is chosen so that multiplication of two positive
 * integers up to BASE-1 can be multiplied exactly using double
 * precision floating point arithmetic.
 * Also it must be possible to add two long integers between
 * -BASE and +BASE (exclusive), giving a result between -2BASE and
 * +2BASE.
 * BASE must be even (so we can easily decide whether the whole
 * number is even), and positive (to avoid all kinds of other trouble).
 * Presently, it is restricted to a power of 10 by the I/O-conversion
 * routines (file "b1nuC.c").
 *
 * Canonical representation:
 * bN is never zero (for the number zero itself, N is zero).
 * If bN is -1, b[N-1] is never BASE-1 .
 * All operands are assumed te be in canonical representation.
 * Routine "int_canon" brings a number in canonical representation.
 *
 * Mapping to C objects:
 * A "digit" is an integer of type "digit", probably an "int".
 * A number is represented as a "B-integer", i.e. something
 * of type "integer" (which is actually a pointer to some struct).
 * The number of digits N is extracted through the macro Length(v).
 * The i-th digit is extracted through the macro Digit(v,N-i).
 * (So in C, we count in a backwards direction from 0 ... n-1 !)
 * A number is created through a call to grab_num(N), which sets
 * N zero digits (thus not in canonical form!).
 */


/*
 * Bring an integer into canonical form.
 * Make a SmallInt if at all possible.
 * NB: Work done by int_canon is duplicated by mk_integer for optimization;
 *     if the strategy here changes, look at mk_integer, too!
 */

Visible integer int_canon(v) integer v; {
	register int i;

	if (IsSmallInt(v)) return v;

	for (i = Length(v) - 1; i >= 0 && Digit(v,i) == 0; --i)
		;

	if (i < 0) {
		release((value) v);
		return int_0;
	}

	if (i == 0) {
		digit dig = Digit(v,0);
		release((value) v);
		return (integer) MkSmallInt(dig);
	}

	if (i > 0 && Digit(v,i) == -1) {
		while (i > 0 && Digit(v, i-1) == BASE-1) --i;
		if (i == 0) {
			release((value) v);
			return (integer) MkSmallInt(-1);
		}
		if (i == 1) {
			digit dig = Digit(v,0) - BASE;
			release((value) v);
			return (integer) MkSmallInt(dig);
		}
		Digit(v,i) = -1;
	}

	if (i+1 < Length(v)) return (integer) regrab_num((value) v, i+1);

	return v;
}


/* General add/subtract subroutine */

typedef double twodigit; /* Might be long on 16 bit machines */
	/* Should be in b0con.h */

Hidden twodigit fmodulo(x, y) twodigit x, y; {
	return x - y * (twodigit) floor((double)x / (double)y);
}

Visible Procedure dig_gadd(to, nto, from, nfrom, ffactor)
	digit *to, *from; intlet nto, nfrom; digit ffactor; {
	twodigit carry= 0;
	twodigit factor= ffactor;
	digit save;

	nto -= nfrom;
	if (nto < 0)
		syserr(MESS(1000, "dig_gadd: nto < nfrom"));
	for (; nfrom > 0; ++to, ++from, --nfrom) {
		carry += *to + *from * factor;
		*to= save= fmodulo(carry, (twodigit)BASE);
		carry= (carry-save) / BASE;
	}
	for (; nto > 0; ++to, --nto) {
		if (carry == 0)
			return;
		carry += *to;
		*to= save= fmodulo(carry, (twodigit)BASE);
		carry= (carry-save) / BASE;
	}
	if (carry != 0)
		to[-1] += carry*BASE; /* Assume it's -1 */
}


/* Sum or difference of two integers */
/* Should have its own version of dig-gadd without double precision */

Visible integer int_gadd(v, w, factor) integer v, w; intlet factor; {
	struct integer vv, ww;
	integer s;
	int len, lenv, i;

	FreezeSmallInt(v, vv);
	FreezeSmallInt(w, ww);
	lenv= len= Length(v);
	if (Length(w) > len)
		len= Length(w);
	++len;
	s= (integer) grab_num(len);
	for (i= 0; i < lenv; ++i)
		Digit(s, i)= Digit(v, i);
	for (; i < len; ++i)
		Digit(s, i)= 0;
	dig_gadd(&Digit(s, 0), len, &Digit(w, 0), Length(w), (digit)factor);
	return int_canon(s);
}


/* Product of two integers */

Visible integer int_prod(v, w) integer v, w; {
	int i;
	integer a;
	struct integer vv, ww;

	if (v == int_0 || w == int_0) return int_0;
	if (v == int_1) return (integer) Copy(w);
	if (w == int_1) return (integer) Copy(v);

	FreezeSmallInt(v, vv);
	FreezeSmallInt(w, ww);

	a = (integer) grab_num(Length(v) + Length(w));

	for (i= Length(a)-1; i >= 0; --i)
		Digit(a, i)= 0;
	for (i = 0; i < Length(v) && !interrupted; ++i)
		dig_gadd(&Digit(a, i), Length(w)+1, &Digit(w, 0), Length(w), 
			Digit(v, i));

	return int_canon(a);
}


/* Compare two integers */

Visible relation int_comp(v, w) integer v, w; {
	int sv, sw;
	register int i;
	struct integer vv, ww;

	/* 1. Compare pointers and equal SmallInts */
	if (v == w) return 0;

	/* 1a. Handle SmallInts */
	if (IsSmallInt(v) && IsSmallInt(w))
		return SmallIntVal(v) - SmallIntVal(w);
	FreezeSmallInt(v, vv);
	FreezeSmallInt(w, ww);

	/* 2. Extract signs */
	sv = Length(v)==0 ? 0 : Digit(v,Length(v)-1)<0 ? -1 : 1;
	sw = Length(w)==0 ? 0 : Digit(w,Length(w)-1)<0 ? -1 : 1;

	/* 3. Compare signs */
	if (sv != sw) return (sv>sw) - (sv<sw);

	/* 4. Compare sizes */
	if (Length(v) != Length(w))
		return sv * ( (Length(v)>Length(w)) - (Length(v)<Length(w)) );

	/* 5. Compare individual digits */
	for (i = Length(v)-1; i >= 0 && Digit(v,i) == Digit(w,i); --i)
		;

	/* 6. All digits equal? */
	if (i < 0) return 0;  /* Yes */

	/* 7. Compare leftmost different digits */
	if (Digit(v,i) < Digit(w,i)) return -1;

	return 1;
}


/* Construct an integer out of a floating point number */

#define GRAN 8	/* Granularity used when requesting more storage */
		/* MOVE TO MEM! */
Visible integer mk_int(x) double x; {
	register integer a;
	integer b;
	register int i, j;
	int negate;

	if (MinSmallInt <= x && x <= MaxSmallInt)
		return (integer) MkSmallInt((int)x);

	a = (integer) grab_num(1);
	negate = x < 0 ? 1 : 0;
	if (negate) x = -x;

	for (i = 0; x != 0; ++i) {
		double z = floor(x/BASE);
		digit save = Modulo((digit)(x-z*BASE), BASE);
		if (i >= Length(a)) {
			a = (integer) regrab_num((value) a, Length(a)+GRAN);
			for (j = Length(a)-1; j > i; --j)
				Digit(a,j) = 0;	/* clear higher digits */
		}
		Digit(a,i) = save;
		x = floor((x-save)/BASE);
	}

	if (negate) {
		b = int_neg(a);
		release((value) a);
		return b;
	}

	return int_canon(a);
}

/* Construct an integer out of a C int.  Like mk_int, but optimized. */

Visible value mk_integer(x) int x; {
	if (MinSmallInt <= x && x <= MaxSmallInt) return MkSmallInt(x);
	return (value) mk_int((double)x);
}


/* Efficiently compute 10**n as a B integer, where n is a C int >= 0 */

Visible integer int_tento(n) int n; {
	integer i;
	digit msd = 1;
	if (n < 0) syserr(MESS(1001, "int_tento(-n)"));
	if (n < tenlogBASE) {
		while (n != 0) msd *= 10, --n;
		return (integer) MkSmallInt(msd);
	}
	i = (integer) grab_num(1 + (int)(n/tenlogBASE));
	n %= tenlogBASE;
	while (n != 0) msd *= 10, --n;
	Digit(i, Length(i)-1) = msd;
	return i;
}

#ifdef NOT_USED
/* Approximate ceiling(10 log abs(u/v)), as C int.
   It only works for v > 0, u, v both integers.
   The result may be one too large or too small */

Visible int scale(u, v) integer u, v; {
	int s;
	double z;
	struct integer uu, vv;

	if (Msd(v) <= 0) syserr(MESS(1002, "scale(u,v<=0)"));
	if (u == int_0) return 0; /* `Don't care' case */
	FreezeSmallInt(u, uu);
	FreezeSmallInt(v, vv);
	s = (Length(u) - Length(v)) * tenlogBASE;
	if (Digit(u, Length(u)-1) >= 0) z = Digit(u, Length(u)-1);
	else {
		s -= tenlogBASE;
		if (Length(u) == 1) z = 1;
		else z = BASE - Digit(u, Length(u)-2);
	}
	z /= Digit(v, Length(v)-1);
	while (z >= 10) z /= 10, ++s;
	while (z < 1) z *= 10, --s;
	return s;
}
#endif NOT_USED
