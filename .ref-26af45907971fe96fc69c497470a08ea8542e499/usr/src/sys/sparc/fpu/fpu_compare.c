/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fpu_compare.c	7.4 (Berkeley) %G%
 *
 * from: $Header: fpu_compare.c,v 1.3 92/11/26 01:39:46 torek Exp $
 */

/*
 * CMP and CMPE instructions.
 *
 * These rely on the fact that our internal wide format is achieved by
 * adding zero bits to the end of narrower mantissas.
 */

#include <sys/types.h>

#include <machine/reg.h>

#include <sparc/fpu/fpu_arith.h>
#include <sparc/fpu/fpu_emu.h>

/*
 * Perform a compare instruction (with or without unordered exception).
 * This updates the fcc field in the fsr.
 *
 * If either operand is NaN, the result is unordered.  For cmpe, this
 * causes an NV exception.  Everything else is ordered:
 *	|Inf| > |numbers| > |0|.
 * We already arranged for fp_class(Inf) > fp_class(numbers) > fp_class(0),
 * so we get this directly.  Note, however, that two zeros compare equal
 * regardless of sign, while everything else depends on sign.
 *
 * Incidentally, two Infs of the same sign compare equal (per the 80387
 * manual---it would be nice if the SPARC documentation were more
 * complete).
 */
void
fpu_compare(struct fpemu *fe, int cmpe)
{
	register struct fpn *a, *b;
	register int cc, r3, r2, r1, r0;
	FPU_DECL_CARRY

	a = &fe->fe_f1;
	b = &fe->fe_f2;

	if (ISNAN(a) || ISNAN(b)) {
		/*
		 * In any case, we already got an exception for signalling
		 * NaNs; here we may replace that one with an identical
		 * exception, but so what?.
		 */
		if (cmpe)
			fe->fe_cx = FSR_NV;
		cc = FSR_CC_UO;
		goto done;
	}

	/*
	 * Must handle both-zero early to avoid sign goofs.  Otherwise,
	 * at most one is 0, and if the signs differ we are done.
	 */
	if (ISZERO(a) && ISZERO(b)) {
		cc = FSR_CC_EQ;
		goto done;
	}
	if (a->fp_sign) {		/* a < 0 (or -0) */
		if (!b->fp_sign) {	/* b >= 0 (or if a = -0, b > 0) */
			cc = FSR_CC_LT;
			goto done;
		}
	} else {			/* a > 0 (or +0) */
		if (b->fp_sign) {	/* b <= -0 (or if a = +0, b < 0) */
			cc = FSR_CC_GT;
			goto done;
		}
	}

	/*
	 * Now the signs are the same (but may both be negative).  All
	 * we have left are these cases:
	 *
	 *	|a| < |b|		[classes or values differ]
	 *	|a| > |b|		[classes or values differ]
	 *	|a| == |b|		[classes and values identical]
	 *
	 * We define `diff' here to expand these as:
	 *
	 *	|a| < |b|, a,b >= 0: a < b => FSR_CC_LT
	 *	|a| < |b|, a,b < 0:  a > b => FSR_CC_GT
	 *	|a| > |b|, a,b >= 0: a > b => FSR_CC_GT
	 *	|a| > |b|, a,b < 0:  a < b => FSR_CC_LT
	 */
#define opposite_cc(cc) ((cc) == FSR_CC_LT ? FSR_CC_GT : FSR_CC_LT)
#define	diff(magnitude) (a->fp_sign ? opposite_cc(magnitude) :  (magnitude))
	if (a->fp_class < b->fp_class) {	/* |a| < |b| */
		cc = diff(FSR_CC_LT);
		goto done;
	}
	if (a->fp_class > b->fp_class) {	/* |a| > |b| */
		cc = diff(FSR_CC_GT);
		goto done;
	}
	/* now none can be 0: only Inf and numbers remain */
	if (ISINF(a)) {				/* |Inf| = |Inf| */
		cc = FSR_CC_EQ;
		goto done;
	}
	/*
	 * Only numbers remain.  To compare two numbers in magnitude, we
	 * simply subtract their mantissas.
	 */
	FPU_SUBS(r3, a->fp_mant[0], b->fp_mant[0]);
	FPU_SUBCS(r2, a->fp_mant[1], b->fp_mant[1]);
	FPU_SUBCS(r1, a->fp_mant[2], b->fp_mant[2]);
	FPU_SUBC(r0, a->fp_mant[3], b->fp_mant[3]);
	if (r0 < 0)				/* underflow: |a| < |b| */
		cc = diff(FSR_CC_LT);
	else if ((r0 | r1 | r2 | r3) != 0)	/* |a| > |b| */
		cc = diff(FSR_CC_GT);
	else
		cc = FSR_CC_EQ;		/* |a| == |b| */
done:
	fe->fe_fsr = (fe->fe_fsr & ~FSR_FCC) | (cc << FSR_FCC_SHIFT);
}
