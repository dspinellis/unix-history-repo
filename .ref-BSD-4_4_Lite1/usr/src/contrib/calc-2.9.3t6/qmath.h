/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Data structure declarations for extended precision rational arithmetic.
 */

#ifndef	QMATH_H
#define	QMATH_H

#include "zmath.h"


/*
 * Rational arithmetic definitions.
 */
typedef struct {
	ZVALUE num;		/* numerator (containing sign) */
	ZVALUE den;		/* denominator (always positive) */
	long links;		/* number of links to this value */
} NUMBER;


/*
 * Input. output, allocation, and conversion routines.
 */
extern NUMBER *qalloc MATH_PROTO((void));
extern NUMBER *qcopy MATH_PROTO((NUMBER *q));
extern NUMBER *iitoq MATH_PROTO((long i1, long i2));
extern NUMBER *atoq MATH_PROTO((char *str));
extern NUMBER *itoq MATH_PROTO((long i));
extern long qtoi MATH_PROTO((NUMBER *q));
extern long qparse MATH_PROTO((char *str, int flags));
extern void qfreenum MATH_PROTO((NUMBER *q));
extern void qprintnum MATH_PROTO((NUMBER *q, int mode));
extern void qprintff MATH_PROTO((NUMBER *q, long width, long precision));
extern void qprintfe MATH_PROTO((NUMBER *q, long width, long precision));
extern void qprintfr MATH_PROTO((NUMBER *q, long width, BOOL force));
extern void qprintfd MATH_PROTO((NUMBER *q, long width));
extern void qprintfx MATH_PROTO((NUMBER *q, long width));
extern void qprintfb MATH_PROTO((NUMBER *q, long width));
extern void qprintfo MATH_PROTO((NUMBER *q, long width));
extern int tilde_ok;
extern int tab_ok;



/*
 * Basic numeric routines.
 */
extern NUMBER *qaddi MATH_PROTO((NUMBER *q, long i));
extern NUMBER *qmuli MATH_PROTO((NUMBER *q, long i));
extern NUMBER *qdivi MATH_PROTO((NUMBER *q, long i));
extern NUMBER *qadd MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qsub MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qmul MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qdiv MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qquo MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qmod MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qmin MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qmax MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qand MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qor MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qxor MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qpowermod MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *q3));
extern NUMBER *qpowi MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qsquare MATH_PROTO((NUMBER *q));
extern NUMBER *qneg MATH_PROTO((NUMBER *q));
extern NUMBER *qsign MATH_PROTO((NUMBER *q));
extern NUMBER *qint MATH_PROTO((NUMBER *q));
extern NUMBER *qfrac MATH_PROTO((NUMBER *q));
extern NUMBER *qnum MATH_PROTO((NUMBER *q));
extern NUMBER *qden MATH_PROTO((NUMBER *q));
extern NUMBER *qinv MATH_PROTO((NUMBER *q));
extern NUMBER *qabs MATH_PROTO((NUMBER *q));
extern NUMBER *qinc MATH_PROTO((NUMBER *q));
extern NUMBER *qdec MATH_PROTO((NUMBER *q));
extern NUMBER *qshift MATH_PROTO((NUMBER *q, long n));
extern NUMBER *qtrunc MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qround MATH_PROTO((NUMBER *q, long places));
extern NUMBER *qbtrunc MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qbround MATH_PROTO((NUMBER *q, long places));
extern NUMBER *qscale MATH_PROTO((NUMBER *q, long i));
extern BOOL qdivides MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern BOOL qcmp MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern BOOL qcmpi MATH_PROTO((NUMBER *q, long i));
extern FLAG qrel MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern FLAG qreli MATH_PROTO((NUMBER *q, long i));
extern BOOL qisset MATH_PROTO((NUMBER *q, long i));


/*
 * More complicated numeric functions.
 */
extern NUMBER *qcomb MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qgcd MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qlcm MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qfact MATH_PROTO((NUMBER *q));
extern NUMBER *qpfact MATH_PROTO((NUMBER *q));
extern NUMBER *qminv MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qfacrem MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qperm MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qgcdrem MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qlowfactor MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qfib MATH_PROTO((NUMBER *q));
extern NUMBER *qcfappr MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qisqrt MATH_PROTO((NUMBER *q));
extern NUMBER *qjacobi MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qiroot MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qbappr MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qlcmfact MATH_PROTO((NUMBER *q));
extern NUMBER *qminmod MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qredcin MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qredcout MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qredcmul MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *q3));
extern NUMBER *qredcsquare MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qredcpower MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *q3));
extern BOOL qprimetest MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern BOOL qissquare MATH_PROTO((NUMBER *q));
extern long qilog2 MATH_PROTO((NUMBER *q));
extern long qilog10 MATH_PROTO((NUMBER *q));
extern BOOL qcmpmod MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *q3));
extern BOOL qquomod MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER **retdiv,
	NUMBER **retmod));
extern FLAG qnear MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *epsilon));
extern FLAG qdigit MATH_PROTO((NUMBER *q, long i));
extern long qprecision MATH_PROTO((NUMBER *q));
extern long qplaces MATH_PROTO((NUMBER *q));
extern long qdigits MATH_PROTO((NUMBER *q));
extern HASH qhash MATH_PROTO((NUMBER *q));
extern void setepsilon MATH_PROTO((NUMBER *q));

#if 0
extern NUMBER *qbitvalue MATH_PROTO((long i));
extern NUMBER *qmulmod MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *q3));
extern NUMBER *qsquaremod MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern NUMBER *qaddmod MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *q3));
extern NUMBER *qsubmod MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *q3));
extern NUMBER *qreadval MATH_PROTO((FILE *fp));
extern NUMBER *qnegmod MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern BOOL qbittest MATH_PROTO((NUMBER *q, long i));
extern FLAG qtest MATH_PROTO((NUMBER *q));
#endif


/*
 * Transcendental functions.  These all take an epsilon argument to
 * specify the required accuracy of the calculation.
 */
extern NUMBER *qsqrt MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qpower MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *epsilon));
extern NUMBER *qroot MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *epsilon));
extern NUMBER *qcos MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qsin MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qexp MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qln MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qtan MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qacos MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qasin MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qatan MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qatan2 MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *epsilon));
extern NUMBER *qhypot MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *epsilon));
extern NUMBER *qcosh MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qsinh MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qtanh MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qacosh MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qasinh MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qatanh MATH_PROTO((NUMBER *q, NUMBER *epsilon));
extern NUMBER *qlegtoleg MATH_PROTO((NUMBER *q, NUMBER *epsilon, BOOL wantneg));
extern NUMBER *qpi MATH_PROTO((NUMBER *epsilon));


/*
 * macro expansions to speed this thing up
 */
#define qiszero(q)	(ziszero((q)->num))
#define qisneg(q)	(zisneg((q)->num))
#define qispos(q)	(zispos((q)->num))
#define qisint(q)	(zisunit((q)->den))
#define qisfrac(q)	(!zisunit((q)->den))
#define qisunit(q)	(zisunit((q)->num) && zisunit((q)->den))
#define qisone(q)	(zisone((q)->num) && zisunit((q)->den))
#define qisnegone(q)	(zisnegone((q)->num) && zisunit((q)->den))
#define qistwo(q)	(zistwo((q)->num) && zisunit((q)->den))
#define qiseven(q)	(zisunit((q)->den) && ziseven((q)->num))
#define qisodd(q)	(zisunit((q)->den) && zisodd((q)->num))
#define qistwopower(q)	(zisunit((q)->den) && zistwopower((q)->num))

#define qhighbit(q)	(zhighbit((q)->num))
#define qlowbit(q)	(zlowbit((q)->num))
#define qdivcount(q1, q2)	(zdivcount((q1)->num, (q2)->num))
#define qilog(q1, q2)	(zlog((q1)->num, (q2)->num))
#define qlink(q)	((q)->links++, (q))

#define qfree(q)	{if (--((q)->links) <= 0) qfreenum(q);}


/*
 * Flags for qparse calls
 */
#define QPF_SLASH	0x1	/* allow slash for fractional number */
#define QPF_IMAG	0x2	/* allow trailing 'i' for imaginary number */


#ifdef VARARGS
extern void qprintf();
#else
extern void qprintf MATH_PROTO((char *, ...));
#endif


/*
 * constants used often by the arithmetic routines
 */
extern NUMBER _qzero_, _qone_, _qnegone_, _qonehalf_;
extern BOOL _sinisneg_;		/* whether sin(x) < 0 (set by cos(x)) */
extern long _epsilonprec_;	/* binary precision of epsilon */
extern NUMBER *_epsilon_;	/* default error for real functions */
extern long _outdigits_;	/* current output digits for float or exp */
extern int _outmode_;		/* current output mode */

#endif

/* END CODE */
