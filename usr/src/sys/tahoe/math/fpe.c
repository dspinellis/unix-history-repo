/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)fpe.c	7.1 (Berkeley) 12/6/90
 */

#include "../include/psl.h"
#include "../include/reg.h"
#include "../include/pte.h"
#include "../include/mtpr.h"
#include "../math/Kfp.h"

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/seg.h"
#include "sys/acct.h"
#include "sys/kernel.h"

/*
 * Floating point emulation support.
 */
extern	float Kcvtlf(), Kaddf(), Ksubf(), Kmulf(), Kdivf();
extern	double Kcvtld(), Kaddd(), Ksubd(), Kmuld(), Kdivd();
extern	float Ksinf(), Kcosf(), Katanf(), Klogf(), Ksqrtf(), Kexpf();

#define	OP(dop)		((dop) &~ 01)	/* precision-less version of opcode */
#define	isdouble(op)	((op) & 01)	/* is opcode double or float */

struct	fpetab {
	int	fpe_op;		/* base opcode emulating */
	float	(*fpe_ffunc)();	/* float version of op */
	double	(*fpe_dfunc)();	/* double version of op */
} fpetab[] = {
	{ OP(CVLD),	Kcvtlf,	Kcvtld },
	{ OP(ADDD),	Kaddf,	Kaddd },
	{ OP(SUBD),	Ksubf,	Ksubd },
	{ OP(MULD),	Kmulf,	Kmuld },
	{ OP(DIVD),	Kdivf,	Kdivd },
	{ SINF,		Ksinf,	0 },
	{ COSF,		Kcosf,	0 },
	{ ATANF,	Katanf,	0 },
	{ LOGF,		Klogf,	0 },
	{ SQRTF,	Ksqrtf,	0 },
	{ EXPF,		Kexpf,	0 },
};
#define	NFPETAB	(sizeof (fpetab) / sizeof (fpetab[0]))

/*
 * Emulate the FP opcode. Update psl as necessary.
 * If OK, set opcode to 0, else to the FP exception #.
 * Not all parameter longwords are relevant, depends on opcode.
 *
 * The entry mask is set by locore.s so ALL registers are saved.
 * This enables FP opcodes to change user registers on return.
 */
/* WARNING!!!! THIS CODE MUST NOT PRODUCE ANY FLOATING POINT EXCEPTIONS */
/*ARGSUSED*/
fpemulate(hfsreg, acc_most, acc_least, dbl, op_most, op_least, opcode, pc, psl)
{
	int r0, r1;			/* must reserve space */
	register int *locr0 = ((int *)&psl)-PS;
	register struct fpetab *fp;
	int hfs = 0; 			/* returned data about exceptions */
	int type;			/* opcode type, FLOAT or DOUBLE */
	union { float ff; int fi; } f_res;
	union { double dd; int di[2]; } d_res;
	int error = 0;

#ifdef lint
	r0 = 0; r0 = r0; r1 = 0; r1 = r1;
#endif
	type = isdouble(opcode) ? DOUBLE : FLOAT;
	for (fp = fpetab; fp < &fpetab[NFPETAB]; fp++)
		if ((opcode & 0xfe) == fp->fpe_op)
			break;
	if (type == DOUBLE) {
		if (fp->fpe_dfunc == 0)
			fp = &fpetab[NFPETAB];
		else
			locr0[PS] &= ~PSL_DBL;
	}
	if (fp >= &fpetab[NFPETAB]) {
		opcode = DIV0_EXC;	/* generate SIGILL - XXX */
		return (0);
	}
	switch (type) {

	case DOUBLE:
		d_res.dd = (*fp->fpe_dfunc)(acc_most, acc_least, op_most,
		    op_least, &hfs);
		if (d_res.di[0] == 0 && d_res.di[1] == 0)
			locr0[PS] |= PSL_Z;
		if (d_res.di[0] < 0)
			locr0[PS] |= PSL_N;
		break;

	case FLOAT:
		f_res.ff = (*fp->fpe_ffunc)(acc_most, acc_least, op_most,
		    op_least, &hfs);
		if (f_res.fi == 0)
			locr0[PS] |= PSL_Z;
		if (f_res.fi ==  0)
			locr0[PS] |= PSL_N;
		break;
	}
	if (hfs & HFS_OVF) {
		locr0[PS] |= PSL_V;	/* turn on overflow bit */
#ifdef notdef
		if (locr0[PS] & PSL_IV)   {  /* overflow enabled? */
#endif
			opcode = OVF_EXC;
			return ((hfs & HFS_DOM) ? EDOM : ERANGE);
#ifdef notdef
		}
#endif
	} else if (hfs & HFS_UNDF) {
		if (locr0[PS] &  PSL_FU) {  /* underflow enabled? */
			opcode = UNDF_EXC;
			return ((hfs & HFS_DOM) ? EDOM : ERANGE);
		} 
	} else if (hfs & HFS_DIVZ) {
		opcode = DIV0_EXC;
		return (0);
	} else if (hfs & HFS_DOM)
		error = EDOM;
	else if (hfs & HFS_RANGE)
		error = ERANGE;
	switch (type) {

	case DOUBLE:
		if (hfs & (HFS_OVF|HFS_UNDF)) {
			d_res.dd = 0.0;
			locr0[PS] |= PSL_Z;
		}
		mvtodacc(d_res.di[0], d_res.di[1], &acc_most);
		break;

	case FLOAT:
		if (hfs & (HFS_OVF|HFS_UNDF)) {
			f_res.ff = 0.0;
			locr0[PS] |= PSL_Z;
		}
		mvtofacc(f_res.ff, &acc_most);
		break;
	}
	opcode = 0;
	return (error);
}
