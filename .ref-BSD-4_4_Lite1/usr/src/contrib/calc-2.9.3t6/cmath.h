/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Data structure declarations for extended precision complex arithmetic.
 */

#ifndef	CMATH_H
#define	CMATH_H

#include "qmath.h"


/*
 * Complex arithmetic definitions.
 */
typedef struct {
	NUMBER *real;		/* real part of number */
	NUMBER *imag;		/* imaginary part of number */
	long links;		/* link count */
} COMPLEX;


/*
 * Input, output, and conversion routines.
 */
extern COMPLEX *comalloc MATH_PROTO((void));
extern COMPLEX *qqtoc MATH_PROTO((NUMBER *q1, NUMBER *q2));
extern void comfree MATH_PROTO((COMPLEX *c));
extern void comprint MATH_PROTO((COMPLEX *c));
extern void cprintfr MATH_PROTO((COMPLEX *c));


/*
 * Basic numeric routines.
 */
extern COMPLEX *cadd MATH_PROTO((COMPLEX *c1, COMPLEX *c2));
extern COMPLEX *csub MATH_PROTO((COMPLEX *c1, COMPLEX *c2));
extern COMPLEX *cmul MATH_PROTO((COMPLEX *c1, COMPLEX *c2));
extern COMPLEX *cdiv MATH_PROTO((COMPLEX *c1, COMPLEX *c2));
extern COMPLEX *caddq MATH_PROTO((COMPLEX *c, NUMBER *q));
extern COMPLEX *csubq MATH_PROTO((COMPLEX *c, NUMBER *q));
extern COMPLEX *cmulq MATH_PROTO((COMPLEX *c, NUMBER *q));
extern COMPLEX *cdivq MATH_PROTO((COMPLEX *c, NUMBER *q));
extern COMPLEX *cmodq MATH_PROTO((COMPLEX *c, NUMBER *q));
extern COMPLEX *cquoq MATH_PROTO((COMPLEX *c, NUMBER *q));
extern COMPLEX *cscale MATH_PROTO((COMPLEX *c, long i));
extern COMPLEX *cshift MATH_PROTO((COMPLEX *c, long i));
extern COMPLEX *cround MATH_PROTO((COMPLEX *c, long i));
extern COMPLEX *cbround MATH_PROTO((COMPLEX *c, long i));
extern COMPLEX *csquare MATH_PROTO((COMPLEX *c));
extern COMPLEX *cconj MATH_PROTO((COMPLEX *c));
extern COMPLEX *creal MATH_PROTO((COMPLEX *c));
extern COMPLEX *cimag MATH_PROTO((COMPLEX *c));
extern COMPLEX *cneg MATH_PROTO((COMPLEX *c));
extern COMPLEX *cinv MATH_PROTO((COMPLEX *c));
extern COMPLEX *cint MATH_PROTO((COMPLEX *c));
extern COMPLEX *cfrac MATH_PROTO((COMPLEX *c));
extern BOOL ccmp MATH_PROTO((COMPLEX *c1, COMPLEX *c2));


/*
 * More complicated functions.
 */
extern COMPLEX *cpowi MATH_PROTO((COMPLEX *c, NUMBER *q));
extern HASH chash MATH_PROTO((COMPLEX *c));


/*
 * Transcendental routines.  These all take an epsilon argument to
 * specify how accurately these are to be calculated.
 */
extern COMPLEX *cpower MATH_PROTO((COMPLEX *c1, COMPLEX *c2, NUMBER *epsilon));
extern COMPLEX *csqrt MATH_PROTO((COMPLEX *c, NUMBER *epsilon));
extern COMPLEX *croot MATH_PROTO((COMPLEX *c, NUMBER *q, NUMBER *epsilon));
extern COMPLEX *cexp MATH_PROTO((COMPLEX *c, NUMBER *epsilon));
extern COMPLEX *cln MATH_PROTO((COMPLEX *c, NUMBER *epsilon));
extern COMPLEX *ccos MATH_PROTO((COMPLEX *c, NUMBER *epsilon));
extern COMPLEX *csin MATH_PROTO((COMPLEX *c, NUMBER *epsilon));
extern COMPLEX *cpolar MATH_PROTO((NUMBER *q1, NUMBER *q2, NUMBER *epsilon));


/*
 * macro expansions to speed this thing up
 */
#define cisreal(c)	(qiszero((c)->imag))
#define cisimag(c)	(qiszero((c)->real) && !cisreal(c))
#define ciszero(c)	(cisreal(c) && qiszero((c)->real))
#define cisone(c)	(cisreal(c) && qisone((c)->real))
#define cisnegone(c)	(cisreal(c) && qisnegone((c)->real))
#define cisrunit(c)	(cisreal(c) && qisunit((c)->real))
#define cisiunit(c)	(qiszero((c)->real) && qisunit((c)->imag))
#define	cisunit(c)	(cisrunit(c) || cisiunit(c))
#define cistwo(c)	(cisreal(c) && qistwo((c)->real))
#define cisint(c)	(qisint((c)->real) && qisint((c)->imag))
#define ciseven(c)	(qiseven((c)->real) && qiseven((c)->imag))
#define cisodd(c)	(qisodd((c)->real) || qisodd((c)->imag))
#define clink(c)	((c)->links++, (c))


/*
 * Pre-defined values.
 */
extern COMPLEX _czero_, _cone_, _conei_;

#endif

/* END CODE */
