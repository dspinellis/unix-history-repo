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
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fpu_explode.c	7.3 (Berkeley) %G%
 *
 * from: $Header: fpu_explode.c,v 1.2 92/06/17 05:41:32 torek Exp $
 */

/*
 * FPU subroutines: `explode' the machine's `packed binary' format numbers
 * into our internal format.
 */

#include <sys/types.h>

#include <machine/ieee.h>
#include <machine/instr.h>
#include <machine/reg.h>

#include <sparc/fpu/fpu_arith.h>
#include <sparc/fpu/fpu_emu.h>

/*
 * N.B.: in all of the following, we assume the FP format is
 *
 *	---------------------------
 *	| s | exponent | fraction |
 *	---------------------------
 *
 * (which represents -1**s * 1.fraction * 2**exponent), so that the
 * sign bit is way at the top (bit 31), the exponent is next, and
 * then the remaining bits mark the fraction.  A zero exponent means
 * zero or denormalized (0.fraction rather than 1.fraction), and the
 * maximum possible exponent, 2bias+1, signals inf (fraction==0) or NaN.
 *
 * Since the sign bit is always the topmost bit---this holds even for
 * integers---we set that outside all the *tof functions.  Each function
 * returns the class code for the new number (but note that we use
 * FPC_QNAN for all NaNs; fpu_explode will fix this if appropriate).
 */

/*
 * int -> fpn.
 */
int
fpu_itof(fp, i)
	register struct fpn *fp;
	register u_int i;
{

	if (i == 0)
		return (FPC_ZERO);
	/*
	 * The value FP_1 represents 2^FP_LG, so set the exponent
	 * there and let normalization fix it up.  Convert negative
	 * numbers to sign-and-magnitude.  Note that this relies on
	 * fpu_norm()'s handling of `supernormals'; see fpu_subr.c.
	 */
	fp->fp_exp = FP_LG;
	fp->fp_mant[0] = (int)i < 0 ? -i : i;
	fp->fp_mant[1] = 0;
	fp->fp_mant[2] = 0;
	fp->fp_mant[3] = 0;
	fpu_norm(fp);
	return (FPC_NUM);
}

#define	mask(nbits) ((1 << (nbits)) - 1)

/*
 * All external floating formats convert to internal in the same manner,
 * as defined here.  Note that only normals get an implied 1.0 inserted.
 */
#define	FP_TOF(exp, expbias, allfrac, f0, f1, f2, f3) \
	if (exp == 0) { \
		if (allfrac == 0) \
			return (FPC_ZERO); \
		fp->fp_exp = 1 - expbias; \
		fp->fp_mant[0] = f0; \
		fp->fp_mant[1] = f1; \
		fp->fp_mant[2] = f2; \
		fp->fp_mant[3] = f3; \
		fpu_norm(fp); \
		return (FPC_NUM); \
	} \
	if (exp == (2 * expbias + 1)) { \
		if (allfrac == 0) \
			return (FPC_INF); \
		fp->fp_mant[0] = f0; \
		fp->fp_mant[1] = f1; \
		fp->fp_mant[2] = f2; \
		fp->fp_mant[3] = f3; \
		return (FPC_QNAN); \
	} \
	fp->fp_exp = exp - expbias; \
	fp->fp_mant[0] = FP_1 | f0; \
	fp->fp_mant[1] = f1; \
	fp->fp_mant[2] = f2; \
	fp->fp_mant[3] = f3; \
	return (FPC_NUM)

/*
 * 32-bit single precision -> fpn.
 * We assume a single occupies at most (64-FP_LG) bits in the internal
 * format: i.e., needs at most fp_mant[0] and fp_mant[1].
 */
int
fpu_stof(fp, i)
	register struct fpn *fp;
	register u_int i;
{
	register int exp;
	register u_int frac, f0, f1;
#define SNG_SHIFT (SNG_FRACBITS - FP_LG)

	exp = (i >> (32 - 1 - SNG_EXPBITS)) & mask(SNG_EXPBITS);
	frac = i & mask(SNG_FRACBITS);
	f0 = frac >> SNG_SHIFT;
	f1 = frac << (32 - SNG_SHIFT);
	FP_TOF(exp, SNG_EXP_BIAS, frac, f0, f1, 0, 0);
}

/*
 * 64-bit double -> fpn.
 * We assume this uses at most (96-FP_LG) bits.
 */
int
fpu_dtof(fp, i, j)
	register struct fpn *fp;
	register u_int i, j;
{
	register int exp;
	register u_int frac, f0, f1, f2;
#define DBL_SHIFT (DBL_FRACBITS - 32 - FP_LG)

	exp = (i >> (32 - 1 - DBL_EXPBITS)) & mask(DBL_EXPBITS);
	frac = i & mask(DBL_FRACBITS - 32);
	f0 = frac >> DBL_SHIFT;
	f1 = (frac << (32 - DBL_SHIFT)) | (j >> DBL_SHIFT);
	f2 = j << (32 - DBL_SHIFT);
	frac |= j;
	FP_TOF(exp, DBL_EXP_BIAS, frac, f0, f1, f2, 0);
}

/*
 * 128-bit extended -> fpn.
 */
int
fpu_xtof(fp, i, j, k, l)
	register struct fpn *fp;
	register u_int i, j, k, l;
{
	register int exp;
	register u_int frac, f0, f1, f2, f3;
#define EXT_SHIFT (-(EXT_FRACBITS - 3 * 32 - FP_LG))	/* left shift! */

	/*
	 * Note that ext and fpn `line up', hence no shifting needed.
	 */
	exp = (i >> (32 - 1 - EXT_EXPBITS)) & mask(EXT_EXPBITS);
	frac = i & mask(EXT_FRACBITS - 3 * 32);
	f0 = (frac << EXT_SHIFT) | (j >> (32 - EXT_SHIFT));
	f1 = (j << EXT_SHIFT) | (k >> (32 - EXT_SHIFT));
	f2 = (k << EXT_SHIFT) | (l >> (32 - EXT_SHIFT));
	f3 = l << EXT_SHIFT;
	frac |= j | k | l;
	FP_TOF(exp, EXT_EXP_BIAS, frac, f0, f1, f2, f3);
}

/*
 * Explode the contents of a register / regpair / regquad.
 * If the input is a signalling NaN, an NV (invalid) exception
 * will be set.  (Note that nothing but NV can occur until ALU
 * operations are performed.)
 */
void
fpu_explode(fe, fp, type, reg)
	register struct fpemu *fe;
	register struct fpn *fp;
	int type, reg;
{
	register u_int s, *space;

	space = &fe->fe_fpstate->fs_regs[reg];
	s = space[0];
	fp->fp_sign = s >> 31;
	fp->fp_sticky = 0;
	switch (type) {

	case FTYPE_INT:
		s = fpu_itof(fp, s);
		break;

	case FTYPE_SNG:
		s = fpu_stof(fp, s);
		break;

	case FTYPE_DBL:
		s = fpu_dtof(fp, s, space[1]);
		break;

	case FTYPE_EXT:
		s = fpu_xtof(fp, s, space[1], space[2], space[3]);
		break;

	default:
		panic("fpu_explode");
	}
	if (s == FPC_QNAN && (fp->fp_mant[0] & FP_QUIETBIT) == 0) {
		/*
		 * Input is a signalling NaN.  All operations that return
		 * an input NaN operand put it through a ``NaN conversion'',
		 * which basically just means ``turn on the quiet bit''.
		 * We do this here so that all NaNs internally look quiet
		 * (we can tell signalling ones by their class).
		 */
		fp->fp_mant[0] |= FP_QUIETBIT;
		fe->fe_cx = FSR_NV;	/* assert invalid operand */
		s = FPC_SNAN;
	}
	fp->fp_class = s;
}
