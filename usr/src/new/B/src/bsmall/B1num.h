/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: B1num.h,v 1.1 84/06/28 00:48:38 timo Exp $ */

/* Definitions for number internals */

typedef struct{double p, q;} number;
typedef double integer;
#define Zero ((integer) 0)
#define One ((integer) 1)
#define Two ((integer) 2)

#define Checknum(v) if ((v)->type != Num) error("value not a number")
#define Numerator(v) ((integer) ((number *)Ats(v))->p)
#define Denominator(v) ((integer) ((number *)Ats(v))->q)
#define Exact(v) (((number *)Ats(v))->q != Zero)
#define Integral(v) (Exact(v) && (Denominator(v)==One))
#define Approxval(v) (((number *)Ats(v))->p)
#define Numval(v) (Exact(v) ? Numerator(v)/Denominator(v) : Approxval(v))

value mk_int();
value mk_approx();
value mk_exact();

