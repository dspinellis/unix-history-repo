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
 *	@(#)fpu_subr.c	7.4 (Berkeley) %G%
 *
 * from: $Header: fpu_subr.c,v 1.4 92/12/01 08:46:52 torek Exp $
 */

/*
 * FPU subroutines.
 */

#include <sys/types.h>

#include <machine/reg.h>

#include <sparc/fpu/fpu_arith.h>
#include <sparc/fpu/fpu_emu.h>

/*
 * Shift the given number right rsh bits.  Any bits that `fall off' will get
 * shoved into the sticky field; we return the resulting sticky.  Note that
 * shifting NaNs is legal (this will never shift all bits out); a NaN's
 * sticky field is ignored anyway.
 */
int
fpu_shr(register struct fpn *fp, register int rsh)
{
	register u_int m0, m1, m2, m3, s;
	register int lsh;

#ifdef DIAGNOSTIC
	if (rsh <= 0 || (fp->fp_class != FPC_NUM && !ISNAN(fp)))
		panic("fpu_rightshift 1");
#endif

	m0 = fp->fp_mant[0];
	m1 = fp->fp_mant[1];
	m2 = fp->fp_mant[2];
	m3 = fp->fp_mant[3];

	/* If shifting all the bits out, take a shortcut. */
	if (rsh >= FP_NMANT) {
#ifdef DIAGNOSTIC
		if ((m0 | m1 | m2 | m3) == 0)
			panic("fpu_rightshift 2");
#endif
		fp->fp_mant[0] = 0;
		fp->fp_mant[1] = 0;
		fp->fp_mant[2] = 0;
		fp->fp_mant[3] = 0;
#ifdef notdef
		if ((m0 | m1 | m2 | m3) == 0)
			fp->fp_class = FPC_ZERO;
		else
#endif
			fp->fp_sticky = 1;
		return (1);
	}

	/* Squish out full words. */
	s = fp->fp_sticky;
	if (rsh >= 32 * 3) {
		s |= m3 | m2 | m1;
		m3 = m0, m2 = 0, m1 = 0, m0 = 0;
	} else if (rsh >= 32 * 2) {
		s |= m3 | m2;
		m3 = m1, m2 = m0, m1 = 0, m0 = 0;
	} else if (rsh >= 32) {
		s |= m3;
		m3 = m2, m2 = m1, m1 = m0, m0 = 0;
	}

	/* Handle any remaining partial word. */
	if ((rsh &= 31) != 0) {
		lsh = 32 - rsh;
		s |= m3 << lsh;
		m3 = (m3 >> rsh) | (m2 << lsh);
		m2 = (m2 >> rsh) | (m1 << lsh);
		m1 = (m1 >> rsh) | (m0 << lsh);
		m0 >>= rsh;
	}
	fp->fp_mant[0] = m0;
	fp->fp_mant[1] = m1;
	fp->fp_mant[2] = m2;
	fp->fp_mant[3] = m3;
	fp->fp_sticky = s;
	return (s);
}

/*
 * Force a number to be normal, i.e., make its fraction have all zero
 * bits before FP_1, then FP_1, then all 1 bits.  This is used for denorms
 * and (sometimes) for intermediate results.
 *
 * Internally, this may use a `supernormal' -- a number whose fp_mant
 * is greater than or equal to 2.0 -- so as a side effect you can hand it
 * a supernormal and it will fix it (provided fp->fp_mant[3] == 0).
 */
void
fpu_norm(register struct fpn *fp)
{
	register u_int m0, m1, m2, m3, top, sup, nrm;
	register int lsh, rsh, exp;

	exp = fp->fp_exp;
	m0 = fp->fp_mant[0];
	m1 = fp->fp_mant[1];
	m2 = fp->fp_mant[2];
	m3 = fp->fp_mant[3];

	/* Handle severe subnormals with 32-bit moves. */
	if (m0 == 0) {
		if (m1)
			m0 = m1, m1 = m2, m2 = m3, m3 = 0, exp -= 32;
		else if (m2)
			m0 = m2, m1 = m3, m2 = 0, m3 = 0, exp -= 2 * 32;
		else if (m3)
			m0 = m3, m1 = 0, m2 = 0, m3 = 0, exp -= 3 * 32;
		else {
			fp->fp_class = FPC_ZERO;
			return;
		}
	}

	/* Now fix any supernormal or remaining subnormal. */
	nrm = FP_1;
	sup = nrm << 1;
	if (m0 >= sup) {
		/*
		 * We have a supernormal number.  We need to shift it right.
		 * We may assume m3==0.
		 */
		for (rsh = 1, top = m0 >> 1; top >= sup; rsh++)	/* XXX slow */
			top >>= 1;
		exp += rsh;
		lsh = 32 - rsh;
		m3 = m2 << lsh;
		m2 = (m2 >> rsh) | (m1 << lsh);
		m1 = (m1 >> rsh) | (m0 << lsh);
		m0 = top;
	} else if (m0 < nrm) {
		/*
		 * We have a regular denorm (a subnormal number), and need
		 * to shift it left.
		 */
		for (lsh = 1, top = m0 << 1; top < nrm; lsh++)	/* XXX slow */
			top <<= 1;
		exp -= lsh;
		rsh = 32 - lsh;
		m0 = top | (m1 >> rsh);
		m1 = (m1 << lsh) | (m2 >> rsh);
		m2 = (m2 << lsh) | (m3 >> rsh);
		m3 <<= lsh;
	}

	fp->fp_exp = exp;
	fp->fp_mant[0] = m0;
	fp->fp_mant[1] = m1;
	fp->fp_mant[2] = m2;
	fp->fp_mant[3] = m3;
}

/*
 * Concoct a `fresh' Quiet NaN per Appendix N.
 * As a side effect, we set NV (invalid) for the current exceptions.
 */
struct fpn *
fpu_newnan(register struct fpemu *fe)
{
	register struct fpn *fp;

	fe->fe_cx = FSR_NV;
	fp = &fe->fe_f3;
	fp->fp_class = FPC_QNAN;
	fp->fp_sign = 0;
	fp->fp_mant[0] = FP_1 - 1;
	fp->fp_mant[1] = fp->fp_mant[2] = fp->fp_mant[3] = ~0;
	return (fp);
}
