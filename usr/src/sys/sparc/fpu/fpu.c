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
 *	@(#)fpu.c	7.3 (Berkeley) %G%
 *
 * from: $Header: fpu.c,v 1.2 92/06/17 05:41:27 torek Exp $
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/signal.h>
#include <sys/systm.h>
#include <sys/syslog.h>

#include <machine/instr.h>
#include <machine/reg.h>

#include <sparc/fpu/fpu_emu.h>

/*
 * fpu_execute returns the following error numbers (0 = no error):
 */
#define	FPE		1	/* take a floating point exception */
#define	NOTFPU		2	/* not an FPU instruction */

/*
 * Translate current exceptions into `first' exception.  The
 * bits go the wrong way for ffs() (0x10 is most important, etc).
 * There are only 5, so do it the obvious way.
 */
#define	X1(x) x
#define	X2(x) x,x
#define	X4(x) x,x,x,x
#define	X8(x) X4(x),X4(x)
#define	X16(x) X8(x),X8(x)

static char cx_to_trapx[] = {
	X1(FSR_NX),
	X2(FSR_DZ),
	X4(FSR_UF),
	X8(FSR_OF),
	X16(FSR_NV)
};
static u_char fpu_codes[] = {
	X1(FPE_FLTINEX_TRAP),
	X2(FPE_FLTDIV_TRAP),
	X4(FPE_FLTUND_TRAP),
	X8(FPE_FLTOVF_TRAP),
	X16(FPE_FLTOPERR_TRAP)
};

/*
 * The FPU gave us an exception.  Clean up the mess.  Note that the
 * fp queue can only have FPops in it, never load/store FP registers
 * nor FBfcc instructions.  Experiments with `crashme' prove that
 * unknown FPops do enter the queue, however.
 */
fpu_cleanup(p, fs)
	register struct proc *p;
	register struct fpstate *fs;
{
	register int i, fsr = fs->fs_fsr, error;
	union instr instr;
	struct fpemu fe;

	switch ((fsr >> FSR_FTT_SHIFT) & FSR_FTT_MASK) {

	case FSR_TT_NONE:
		panic("fpu_cleanup 1");	/* ??? */
		break;

	case FSR_TT_IEEE:
		/* XXX missing trap address! */
		if ((i = fsr & FSR_CX) == 0)
			panic("fpu ieee trap, but no exception");
		trapsignal(p, SIGFPE, fpu_codes[i - 1]);
		break;		/* XXX should return, but queue remains */

	case FSR_TT_UNFIN:
	case FSR_TT_UNIMP:
		if (fs->fs_qsize == 0)
			panic("fpu_cleanup 2");
		break;

	case FSR_TT_SEQ:
		panic("fpu sequence error");
		/* NOTREACHED */

	case FSR_TT_HWERR:
		log(LOG_ERR, "fpu hardware error (%s[%d])\n",
		    p->p_comm, p->p_pid);
		uprintf("%s[%d]: fpu hardware error\n", p->p_comm, p->p_pid);
		trapsignal(p, SIGFPE, -1);	/* ??? */
		goto out;

	default:
		printf("fsr=%x\n", fsr);
		panic("fpu error");
	}

	/* emulate the instructions left in the queue */
	fe.fe_fpstate = fs;
	for (i = 0; i < fs->fs_qsize; i++) {
		instr.i_int = fs->fs_queue[i].fq_instr;
		if (instr.i_any.i_op != IOP_reg ||
		    (instr.i_op3.i_op3 != IOP3_FPop1 &&
		     instr.i_op3.i_op3 != IOP3_FPop2))
			panic("bogus fpu queue");
		error = fpu_execute(&fe, instr);
		switch (error) {

		case 0:
			continue;

		case FPE:
			trapsignal(p, SIGFPE,
			    fpu_codes[(fs->fs_fsr & FSR_CX) - 1]);
			break;

		case NOTFPU:
			trapsignal(p, SIGILL, 0);	/* ??? code?  */
			break;

		default:
			panic("fpu_cleanup 3");
			/* NOTREACHED */
		}
		/* XXX should stop here, but queue remains */
	}
out:
	fs->fs_qsize = 0;
}

#ifdef notyet
/*
 * If we have no FPU at all (are there any machines like this out
 * there!?) we have to emulate each instruction, and we need a pointer
 * to the trapframe so that we can step over them and do FBfcc's.
 * We know the `queue' is empty, though; we just want to emulate
 * the instruction at tf->tf_pc.
 */
fpu_emulate(p, tf, fs)
	struct proc *p;
	register struct trapframe *tf;
	register struct fpstate *fs;
{

	do {
		fetch instr from pc
		decode
		if (integer instr) {
			/*
			 * We do this here, rather than earlier, to avoid
			 * losing even more badly than usual.
			 */
			if (p->p_addr->u_pcb.pcb_uw) {
				write_user_windows();
				if (rwindow_save(p))
					sigexit(p, SIGILL);
			}
			if (loadstore) {
				do_it;
				pc = npc, npc += 4
			} else if (fbfcc) {
				do_annul_stuff;
			} else
				return;
		} else if (fpu instr) {
			fe.fe_fsr = fs->fs_fsr &= ~FSR_CX;
			error = fpu_execute(&fe, fs, instr);
			switch (error) {
				etc;
			}
		} else
			return;
		if (want to reschedule)
			return;
	} while (error == 0);
}
#endif

/*
 * Execute an FPU instruction (one that runs entirely in the FPU; not
 * FBfcc or STF, for instance).  On return, fe->fe_fs->fs_fsr will be
 * modified to reflect the setting the hardware would have left.
 *
 * Note that we do not catch all illegal opcodes, so you can, for instance,
 * multiply two integers this way.
 */
int
fpu_execute(fe, instr)
	register struct fpemu *fe;
	union instr instr;
{
	register struct fpn *fp;
	register int opf, rs1, rs2, rd, type, mask, fsr, cx;
	register struct fpstate *fs;
	u_int space[4];

	/*
	 * `Decode' and execute instruction.  Start with no exceptions.
	 * The type of any i_opf opcode is in the bottom two bits, so we
	 * squish them out here.
	 */
	opf = instr.i_opf.i_opf;
	type = opf & 3;
	mask = "\0\0\1\3"[type];
	rs1 = instr.i_opf.i_rs1 & ~mask;
	rs2 = instr.i_opf.i_rs2 & ~mask;
	rd = instr.i_opf.i_rd & ~mask;
#ifdef notdef
	if ((rs1 | rs2 | rd) & mask)
		return (BADREG);
#endif
	fs = fe->fe_fpstate;
	fe->fe_fsr = fs->fs_fsr & ~FSR_CX;
	fe->fe_cx = 0;
	switch (opf >>= 2) {

	default:
		return (NOTFPU);

	case FMOV >> 2:		/* these should all be pretty obvious */
		rs1 = fs->fs_regs[rs2];
		goto mov;

	case FNEG >> 2:
		rs1 = fs->fs_regs[rs2] ^ (1 << 31);
		goto mov;

	case FABS >> 2:
		rs1 = fs->fs_regs[rs2] & ~(1 << 31);
	mov:
		fs->fs_regs[rd] = rs1;
		fs->fs_fsr = fe->fe_fsr;
		return (0);	/* success */

	case FSQRT >> 2:
		fpu_explode(fe, &fe->fe_f1, type, rs2);
		fp = fpu_sqrt(fe);
		break;

	case FADD >> 2:
		fpu_explode(fe, &fe->fe_f1, type, rs1);
		fpu_explode(fe, &fe->fe_f2, type, rs2);
		fp = fpu_add(fe);
		break;

	case FSUB >> 2:
		fpu_explode(fe, &fe->fe_f1, type, rs1);
		fpu_explode(fe, &fe->fe_f2, type, rs2);
		fp = fpu_sub(fe);
		break;

	case FMUL >> 2:
		fpu_explode(fe, &fe->fe_f1, type, rs1);
		fpu_explode(fe, &fe->fe_f2, type, rs2);
		fp = fpu_mul(fe);
		break;

	case FDIV >> 2:
		fpu_explode(fe, &fe->fe_f1, type, rs1);
		fpu_explode(fe, &fe->fe_f2, type, rs2);
		fp = fpu_div(fe);
		break;

	case FCMP >> 2:
		fpu_explode(fe, &fe->fe_f1, type, rs1);
		fpu_explode(fe, &fe->fe_f2, type, rs2);
		fpu_compare(fe, 0);
		goto cmpdone;

	case FCMPE >> 2:
		fpu_explode(fe, &fe->fe_f1, type, rs1);
		fpu_explode(fe, &fe->fe_f2, type, rs2);
		fpu_compare(fe, 1);
	cmpdone:
		/*
		 * The only possible exception here is NV; catch it
		 * early and get out, as there is no result register.
		 */
		cx = fe->fe_cx;
		fsr = fe->fe_fsr | (cx << FSR_CX_SHIFT);
		if (cx != 0) {
			if (fsr & (FSR_NV << FSR_TEM_SHIFT)) {
				fs->fs_fsr = (fsr & ~FSR_FTT) |
				    (FSR_TT_IEEE << FSR_FTT_SHIFT);
				return (FPE);
			}
			fsr |= FSR_NV << FSR_AX_SHIFT;
		}
		fs->fs_fsr = fsr;
		return (0);

	case FSMULD >> 2:
	case FDMULX >> 2:
		if (type == FTYPE_EXT)
			return (NOTFPU);
		fpu_explode(fe, &fe->fe_f1, type, rs1);
		fpu_explode(fe, &fe->fe_f2, type, rs2);
		type++;	/* single to double, or double to quad */
		fp = fpu_mul(fe);
		break;

	case FTOS >> 2:
	case FTOD >> 2:
	case FTOX >> 2:
	case FTOI >> 2:
		fpu_explode(fe, fp = &fe->fe_f1, type, rs2);
		type = opf & 3;	/* sneaky; depends on instruction encoding */
		break;
	}

	/*
	 * ALU operation is complete.  Collapse the result and then check
	 * for exceptions.  If we got any, and they are enabled, do not
	 * alter the destination register, just stop with an exception.
	 * Otherwise set new current exceptions and accrue.
	 */
	fpu_implode(fe, fp, type, space);
	cx = fe->fe_cx;
	fsr = fe->fe_fsr;
	if (cx != 0) {
		mask = (fsr >> FSR_TEM_SHIFT) & FSR_TEM_MASK;
		if (cx & mask) {
			/* not accrued??? */
			fs->fs_fsr = (fsr & ~FSR_FTT) |
			    (FSR_TT_IEEE << FSR_FTT_SHIFT) |
			    (cx_to_trapx[(cx & mask) - 1] << FSR_CX_SHIFT);
			return (FPE);
		}
		fsr |= (cx << FSR_CX_SHIFT) | (cx << FSR_AX_SHIFT);
	}
	fs->fs_fsr = fsr;
	fs->fs_regs[rd] = space[0];
	if (type >= FTYPE_DBL) {
		fs->fs_regs[rd + 1] = space[1];
		if (type > FTYPE_DBL) {
			fs->fs_regs[rd + 2] = space[2];
			fs->fs_regs[rd + 3] = space[3];
		}
	}
	return (0);	/* success */
}
