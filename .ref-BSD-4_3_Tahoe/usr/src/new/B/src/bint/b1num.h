/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b1num.h,v 1.4 85/08/22 16:41:53 timo Exp $
*/

/************************************************************************/
/* Full numeric package: private definitions                            */
/*                                                                      */
/* A number is modelled as one of zero, unbounded integer,              */
/*        unbounded rational or approximate.                            */
/*     Zero has a 'length' field of zero, and nothing else.             */
/*     A length field of +n means the number is an n digit integer,     */
/*        (with digits to some large base).                             */
/*     A length of -1 means there follow two floating point numbers,    */
/*        one the fraction (zero or .5 <= frac <= 1), one the exponent  */
/*        with respect to base 2 (should be an integral value).         */
/*        (This is so when EXT_RANGE is defined.  Otherwise, there is   */
/*        only one field, frac, which is not normalized.  This saves    */
/*        code and data space on e.g. the IBM PC, where the natural     */
/*        range of double's is sufficient (~1E307).)                    */
/*     A length of -2 means there follow two values, pointers to two    */
/*        unbounded integers, ie a rational number.                     */
/*     A length of -n, n>2, means it is a rational with a print width   */
/*        of n-2.                                                       */
/*                                                                      */
/************************************************************************/

/*************** Definitions exported for integers *****************/

typedef int digit;

typedef struct integer {
	HEADER;
	digit	dig[1];
} *integer;

#define FreezeSmallInt(v, vv) \
	(IsSmallInt(v) && (Freeze1(v, vv), Freeze2(v, vv)))
#define Freeze1(v, vv) ((vv).type= Num, (vv).refcnt= Maxrefcnt)
#define Freeze2(v, vv) \
	((vv).len= (v) != 0, (vv).dig[0]= SmallIntVal(v), (v)= &(vv))

integer int_gadd();
integer int_canon();
integer int_prod();
integer int_quot();
integer int_gcd();
integer mk_int();
integer int1mul();
integer int_tento();
integer int_half();
integer int_mod();
digit int_ldiv();

#define int_0 ((integer) MkSmallInt(0))
#define int_1 ((integer) MkSmallInt(1))
#define int_2 ((integer) MkSmallInt(2))
#define int_10 ((integer) MkSmallInt(10))

#define int_sum(v, w) int_gadd(v, w, 1)
#define int_diff(v, w) int_gadd(v, w, -1)
#define int_neg(v) int_gadd(int_0, v, -1)

#define Integral(v) (IsSmallInt(v) || Length(v)>=0)
#define Modulo(a,b) (((a)%(b)+(b))%(b))
#define Digit(v,n) ((v)->dig[n])
#define Msd(v) (IsSmallInt(v) ? SmallIntVal(v) : Digit(v,Length(v)-1))
#define Lsd(v) (IsSmallInt(v) ? SmallIntVal(v) : Digit(v,0))

#define Odd(x) ((x)&1)
#define Even(x) (!Odd(x))

/* Provisional definitions */

value copy();
#define Copy(x) copy((value)(x))

/***************** Definitions exported for rationals *****************/

typedef struct {
	HEADER;
	integer	num, den;
} *rational;


#define Numerator(a) ((a)->num)
#define Denominator(a) ((a)->den)
#define Rational(a) (!IsSmallInt(a) && Length(a)<-1)
#define Roundsize(a) (-2-Length(a))

rational mk_rat();
rational rat_sum();
rational rat_diff();
rational rat_neg();
rational rat_prod();
rational rat_quot();
rational rat_power();

extern rational rat_zero;
extern rational rat_half;

value tento();
value mk_exact();

/***************** Definitions exported for approximate numbers *************/

#ifdef vax
#define EXT_RANGE
#endif

typedef struct real {
	HEADER;
	double	frac;
#ifdef EXT_RANGE
	double	expo;
#endif EXT_RANGE
} *real;

#define Frac(v) ((v)->frac)
#ifdef EXT_RANGE
#define Expo(v) ((v)->expo)
#else
#define Expo(v) 0.0
#endif

#define Approximate(v) (!IsSmallInt(v) && Length(v)==-1)
#define Exact(v) (!Approximate(v))

extern real app_0;

real mk_approx();

real app_sum();
real app_diff();
real app_prod();
real app_quot();
real app_neg();

real app_exp();
real app_log();
real app_power();

value app_floor();


/* Numeric constants. */
/* (Source: Knuth, The Art of Computer Programming, Vol. 1, Appendix B-1.) */

#define logtwo 0.6931471805599453094172321214581765680755
#define invlogtwo 1.4426950408889634073599246810018921374266
#define logten 2.3025850929940456840179914546843642076011
#define logBASE (logten*tenlogBASE)
#define twologBASE (logBASE*invlogtwo)
