/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Modular arithmetic routines for normal numbers, and also using
 * the faster REDC algorithm.
 */

#include "qmath.h"


/*
 * Structure used for caching REDC information.
 */
typedef struct	{
	NUMBER	*num;		/* modulus being cached */
	REDC	*redc;		/* REDC information for modulus */
	long	age;		/* age counter for reallocation */
} REDC_CACHE;


static long redc_age;			/* current age counter */
static REDC_CACHE redc_cache[MAXREDC];	/* cached REDC info */


static REDC *qfindredc MATH_PROTO((NUMBER *q));


/*
 * Return the remainder when one number is divided by another.
 * The second argument cannot be negative.  The result is normalized
 * to lie in the range 0 to q2, and works for fractional values as:
 *	qmod(q1, q2) = q1 - (int(q1 / q2) * q2).
 */
NUMBER *
qmod(q1, q2)
	register NUMBER *q1, *q2;
{
	ZVALUE res;
	NUMBER *q, *tmp;

	if (qisneg(q2) || qiszero(q2))
		math_error("Non-positive modulus");
	if (qisint(q1) && qisint(q2)) {		/* easy case */
		zmod(q1->num, q2->num, &res);
		if (ziszero(res)) {
			zfree(res);
			return qlink(&_qzero_);
		}
		if (zisone(res)) {
			zfree(res);
			return qlink(&_qone_);
		}
		q = qalloc();
		q->num = res;
		return q;
	}
	q = qquo(q1, q2);
	tmp = qmul(q, q2);
	qfree(q);
	q = qsub(q1, tmp);
	qfree(tmp);
	if (qisneg(q)) {
		tmp = qadd(q2, q);
		qfree(q);
		q = tmp;
	}
	return q;
}


/*
 * Given two numbers (a and b), calculate the quotient (c) and remainder (d)
 * when a is divided by b.  This is defined so 0 <= d < b, and c is integral,
 * and a = b * c + d.  The results are returned indirectly through pointers.
 * This works for integers or fractions.  Returns whether or not there is a
 * remainder.  Examples:
 *	qquomod(11, 4, &x, &y) sets x to 2, y to 3, and returns TRUE.
 *	qquomod(-7, 3, &x, &y) sets x to -3, y to 2, and returns TRUE.
 */
BOOL
qquomod(q1, q2, retqdiv, retqmod)
	NUMBER *q1, *q2;		/* numbers to do quotient with */
	NUMBER **retqdiv;		/* returned quotient */
	NUMBER **retqmod;		/* returned modulo */
{
	NUMBER *qq, *qm, *tmp;

	if (qisneg(q2) || qiszero(q2))
		math_error("Non-positive modulus");

	if (qisint(q1) && qisint(q2)) {		/* integer case */
		qq = qalloc();
		qm = qalloc();
		zdiv(q1->num, q2->num, &qq->num, &qm->num);
		if (!qisneg(q1) || qiszero(qm)) {
			*retqdiv = qq;
			*retqmod = qm;
			return !qiszero(qm);
		}

		/*
		 * Need to fix up negative results.
		 */
		tmp = qdec(qq);
		qfree(qq);
		qq = tmp;
		tmp = qsub(q2, qm);
		qfree(qm);
		qm = tmp;
		*retqdiv = qq;
		*retqmod = qm;
		return TRUE;
	}

	/*
	 * Fractional case.
	 */
	qq = qquo(q1, q2);
	tmp = qmul(qq, q2);
	qm = qsub(q1, tmp);
	qfree(tmp);
	if (qisneg(qm)) {
		tmp = qadd(qm, q2);
		qfree(qm);
		qm = tmp;
		tmp = qdec(qq);
		qfree(qq);
		qq = tmp;
	}
	*retqdiv = qq;
	*retqmod = qm;
	return !qiszero(qm);
}


#if 0
/*
 * Return the product of two integers modulo a third integer.
 * The result is in the range 0 to q3 - 1 inclusive.
 *	q4 = (q1 * q2) mod q3.
 */
NUMBER *
qmulmod(q1, q2, q3)
	NUMBER *q1, *q2, *q3;
{
	NUMBER *q;

	if (qisneg(q3) || qiszero(q3))
		math_error("Non-positive modulus");
	if (qisfrac(q1) || qisfrac(q2) || qisfrac(q3))
		math_error("Non-integers for qmulmod");
	if (qiszero(q1) || qiszero(q2) || qisunit(q3))
		return qlink(&_qzero_);
	q = qalloc();
	zmulmod(q1->num, q2->num, q3->num, &q->num);
	return q;
}


/*
 * Return the square of an integer modulo another integer.
 * The result is in the range 0 to q2 - 1 inclusive.
 *	q2 = (q1^2) mod q2.
 */
NUMBER *
qsquaremod(q1, q2)
	NUMBER *q1, *q2;
{
	NUMBER *q;

	if (qisneg(q2) || qiszero(q2))
		math_error("Non-positive modulus");
	if (qisfrac(q1) || qisfrac(q2))
		math_error("Non-integers for qsquaremod");
	if (qiszero(q1) || qisunit(q2))
		return qlink(&_qzero_);
	if (qisunit(q1))
		return qlink(&_qone_);
	q = qalloc();
	zsquaremod(q1->num, q2->num, &q->num);
	return q;
}


/*
 * Return the sum of two integers modulo a third integer.
 * The result is in the range 0 to q3 - 1 inclusive.
 *	q4 = (q1 + q2) mod q3.
 */
NUMBER *
qaddmod(q1, q2, q3)
	NUMBER *q1, *q2, *q3;
{
	NUMBER *q;

	if (qisneg(q3) || qiszero(q3))
		math_error("Non-positive modulus");
	if (qisfrac(q1) || qisfrac(q2) || qisfrac(q3))
		math_error("Non-integers for qaddmod");
	q = qalloc();
	zaddmod(q1->num, q2->num, q3->num, &q->num);
	return q;
}


/*
 * Return the difference of two integers modulo a third integer.
 * The result is in the range 0 to q3 - 1 inclusive.
 *	q4 = (q1 - q2) mod q3.
 */
NUMBER *
qsubmod(q1, q2, q3)
	NUMBER *q1, *q2, *q3;
{
	NUMBER *q;

	if (qisneg(q3) || qiszero(q3))
		math_error("Non-positive modulus");
	if (qisfrac(q1) || qisfrac(q2) || qisfrac(q3))
		math_error("Non-integers for qsubmod");
	if (q1 == q2)
		return qlink(&_qzero_);
	q = qalloc();
	zsubmod(q1->num, q2->num, q3->num, &q->num);
	return q;
}


/*
 * Return the negative of an integer modulo another integer.
 * The result is in the range 0 to q2 - 1 inclusive.
 *	q2 = (-q1) mod q2.
 */
NUMBER *
qnegmod(q1, q2)
	NUMBER *q1, *q2;
{
	NUMBER *q;

	if (qisneg(q2) || qiszero(q2))
		math_error("Non-positive modulus");
	if (qisfrac(q1) || qisfrac(q2))
		math_error("Non-integers for qnegmod");
	if (qiszero(q1) || qisunit(q2))
		return qlink(&_qzero_);
	q = qalloc();
	znegmod(q1->num, q2->num, &q->num);
	return q;
}
#endif


/*
 * Return the integer congruent to an integer whose absolute value is smallest.
 * This is a unique integer in the range int((q2-1)/2 to int(q2/2), inclusive.
 * For example, for a modulus of 7, returned values are [-3, 3], and for a
 * modulus of 8, returned values are [-3, 4].
 */
NUMBER *
qminmod(q1, q2)
	NUMBER *q1, *q2;
{
	NUMBER *q;

	if (qisneg(q2) || qiszero(q2))
		math_error("Non-positive modulus");
	if (qisfrac(q1) || qisfrac(q2))
		math_error("Non-integers for qminmod");
	if (qiszero(q1) || (q1->num.len < q2->num.len - 1))
		return qlink(q1);
	q = qalloc();
	zminmod(q1->num, q2->num, &q->num);
	return q;
}


/*
 * Return whether or not two integers are congruent modulo a third integer.
 * Returns TRUE if the numbers are not congruent, and FALSE if they are.
 */
BOOL
qcmpmod(q1, q2, q3)
	NUMBER *q1, *q2, *q3;
{
	if (qisneg(q3) || qiszero(q3))
		math_error("Non-positive modulus");
	if (qisfrac(q1) || qisfrac(q2) || qisfrac(q3))
		math_error("Non-integers for qcmpmod");
	if (q1 == q2)
		return FALSE;
	return zcmpmod(q1->num, q2->num, q3->num);
}


/*
 * Convert an integer into REDC format for use in faster modular arithmetic.
 * The number can be negative or out of modulus range.
 */
NUMBER *
qredcin(q1, q2)
	NUMBER *q1;		/* number to convert into REDC format */
	NUMBER *q2;		/* modulus */
{
	REDC *rp;		/* REDC information */
	NUMBER *r;		/* result */

	if (qisfrac(q1))
		math_error("Non-integer for qredcin");
	rp = qfindredc(q2);
	if (qiszero(q1))
		return qlink(&_qzero_);
	r = qalloc();
	zredcencode(rp, q1->num, &r->num);
	return r;
}


/*
 * Convert a REDC format number back into a normal integer.
 * The resulting number is in the range 0 to the modulus - 1.
 */
NUMBER *
qredcout(q1, q2)
	NUMBER *q1;		/* number to convert out of REDC format */
	NUMBER *q2;		/* modulus */
{
	REDC *rp;		/* REDC information */
	NUMBER *r;		/* result */

	if (qisfrac(q1) || qisneg(q1))
		math_error("Non-positive integer required for qredcout");
	rp = qfindredc(q2);
	if (qiszero(q1))
		return qlink(&_qzero_);
	r = qalloc();
	zredcdecode(rp, q1->num, &r->num);
	if (zisunit(r->num)) {
		qfree(r);
		r = qlink(&_qone_);
	}
	return r;
}


/*
 * Multiply two REDC format numbers together producing a REDC format result.
 * This multiplication is done modulo the specified modulus.
 */
NUMBER *
qredcmul(q1, q2, q3)
	NUMBER *q1, *q2;	/* REDC numbers to be multiplied */
	NUMBER *q3;		/* modulus */
{
	REDC *rp;		/* REDC information */
	NUMBER *r;		/* result */

	if (qisfrac(q1) || qisneg(q1) || qisfrac(q2) || qisneg(q2))
		math_error("Non-positive integers required for qredcmul");
	rp = qfindredc(q3);
	if (qiszero(q1) || qiszero(q2))
		return qlink(&_qzero_);
	r = qalloc();
	zredcmul(rp, q1->num, q2->num, &r->num);
	return r;
}


/*
 * Square a REDC format number to produce a REDC format result.
 * This squaring is done modulo the specified modulus.
 */
NUMBER *
qredcsquare(q1, q2)
	NUMBER *q1;		/* REDC number to be squared */
	NUMBER *q2;		/* modulus */
{
	REDC *rp;		/* REDC information */
	NUMBER *r;		/* result */

	if (qisfrac(q1) || qisneg(q1))
		math_error("Non-positive integer required for qredcsquare");
	rp = qfindredc(q2);
	if (qiszero(q1))
		return qlink(&_qzero_);
	r = qalloc();
	zredcsquare(rp, q1->num, &r->num);
	return r;
}


/*
 * Raise a REDC format number to the indicated power producing a REDC
 * format result.  This is done modulo the specified modulus.  The
 * power to be raised to is a normal number.
 */
NUMBER *
qredcpower(q1, q2, q3)
	NUMBER *q1;		/* REDC number to be raised */
	NUMBER *q2;		/* power to be raised to */
	NUMBER *q3;		/* modulus */
{
	REDC *rp;		/* REDC information */
	NUMBER *r;		/* result */

	if (qisfrac(q1) || qisneg(q1) || qisfrac(q2) || qisneg(q2))
		math_error("Non-positive integers required for qredcpower");
	rp = qfindredc(q3);
	if (qiszero(q1) || qisunit(q3))
		return qlink(&_qzero_);
	r = qalloc();
	zredcpower(rp, q1->num, q2->num, &r->num);
	return r;
}


/*
 * Search for and return the REDC information for the specified number.
 * The information is cached into a local table so that future calls
 * for this information will be quick.  If the table fills up, then
 * the oldest cached entry is reused.
 */
static REDC *
qfindredc(q)
	NUMBER *q;		/* modulus to find REDC information of */
{
	register REDC_CACHE *rcp;
	REDC_CACHE *bestrcp;

	/*
	 * First try for an exact pointer match in the table.
	 */
	for (rcp = redc_cache; rcp < &redc_cache[MAXREDC]; rcp++) {
		if (q == rcp->num) {
			rcp->age = ++redc_age;
			return rcp->redc;
		}
	}

	/*
	 * Search the table again looking for a value which matches.
	 */
	for (rcp = redc_cache; rcp < &redc_cache[MAXREDC]; rcp++) {
		if (rcp->age && (qcmp(q, rcp->num) == 0)) {
			rcp->age = ++redc_age;
			return rcp->redc;
		}
	}

	/*
	 * Must invalidate an existing entry in the table.
	 * Find the oldest (or first unused) entry.
	 * But first make sure the modulus will be reasonable.
	 */
	if (qisfrac(q) || qiseven(q) || qisneg(q))
		math_error("REDC modulus must be positive odd integer");

	bestrcp = NULL;
	for (rcp = redc_cache; rcp < &redc_cache[MAXREDC]; rcp++) {
		if ((bestrcp == NULL) || (rcp->age < bestrcp->age))
			bestrcp = rcp;
	}

	/*
	 * Found the best entry.
	 * Free the old information for the entry if necessary,
	 * then initialize it.
	 */
	rcp = bestrcp;
	if (rcp->age) {
		rcp->age = 0;
		qfree(rcp->num);
		zredcfree(rcp->redc);
	}

	rcp->redc = zredcalloc(q->num);
	rcp->num = qlink(q);
	rcp->age = ++redc_age;
	return rcp->redc;
}

/* END CODE */
