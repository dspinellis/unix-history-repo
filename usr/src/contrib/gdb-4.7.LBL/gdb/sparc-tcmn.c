/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Lawrence Berkeley Laboratory,
 * Berkeley, CA.  The name of the University may not be used to
 * endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char rcsid[] =
    "@(#) $Header: sparc-tcmn.c,v 1.2 93/02/19 15:25:05 mccanne Exp $ (LBL)";
#endif

#include <sys/param.h>
#include <stdio.h>
#include "defs.h"
#include "target.h"
#include "frame.h"
#include "inferior.h"
#include "value.h"

/*
 * Sparc instruction format per Fig 4-1 of the Sun SPARC Architecture 
 * Manual Part No: 800-1399-08 Rev A, Oct 22 1987.
 */
union sparcinsn {
	u_long code;
	struct {
		u_int op:2;
		u_int disp30:30;
	} f1;
	struct insn_f2a {
		u_int op:2;
		u_int rd:5;
		u_int op2:3;
		int imm22:22;
	} f2a;
	struct insn_f2b {
		u_int op:2;
		u_int a:1;
		u_int cond:4;
		u_int op2:3;
		int disp22:22;
	} f2b;
	struct insn_f3a {
		u_int op:2;
		u_int rd:5;
		u_int op3:6;
		u_int rs1:5;
		u_int i:1;
		u_int asi:8;
		u_int rs2:5;
	} f3a;
	struct insn_f3b {
		u_int op:2;
		u_int rd:5;
		u_int op3:6;
		u_int rs1:5;
		u_int i:1;
		int simm13:13;
	} f3b;
};

/*
 * For the instruction INSN at PC, return the address, other than NPC, that
 * could possibly be executed next.  If the only possibility is npc, return 0.
 * (There is only one such "other" possible address.)
 */
CORE_ADDR
annulled_dest(insn, pc, npc)
	union sparcinsn insn;
	CORE_ADDR pc, npc;
{
	register struct insn_f2b i = insn.f2b;
 
	/*
	 * The only time we can branch somewhere other than npc is
	 * on a branch intruction with the anull bit set.  In this
	 * case, the other possible target is the npc + 4 (because
	 * npc was anulled and the branch was not taken) or the
	 * branch destination (because npc was anulled and it was
	 * a branch always).
	 */
	if (i.op == 0 && i.a && (i.op2 == 2 || i.op2 == 6 || i.op2 == 7)) {
		if (i.cond == 8)
			return (pc + 4 * i.disp22);
		else
			return (npc + 4);
	}
	return (0);
}

/*
 * Non-zero if we just simulated a single-step ptrace call.  This is
 * needed because we cannot remove the breakpoints in the inferior
 * process until after the `wait' in `wait_for_inferior'. 
 * Used for sun4.
 */
int one_stepped;

/*
 * single_step() is called just before we want to resume the inferior,
 * if we want to single-step it but there is no hardware or kernel single-step
 * support (as on all SPARCs).  We find all the possible targets of the
 * coming instruction and breakpoint them.
 *
 * single_step is also called just after the inferior stops.  If we had
 * set up a simulated single-step, we undo our damage.
 *
 * Code written by Gary Beihl (beihl@mcc.com); modified by Steven McCanne
 * (mccanne@ee.lbl.gov).
 */
void
single_step(signal)
	int signal;
{
	CORE_ADDR pc;
	union sparcinsn insn;
	/*
	 * Storage for temporary breakpoints.  XXX There should be a uniform
	 * interface for breakpoints, so that we could just set one then 
	 * clear it.  Note that we need only two outstanding breakpoints,
	 * since there can be at most two possiblilities for control flow.
	 */
	static CORE_ADDR target0;
	static CORE_ADDR target1;
	static char shadow0[4];
	static char shadow1[4];

	pc = read_register(PC_REGNUM);
	insn.code = read_memory_integer(pc, 4);

	if (!one_stepped) {
		/*
		 * This is a hack to special case call instructions.
		 * If we are stepping over subroutines, find each call
		 * and trap on return, rather than single step until
		 * wait_for_inferior() discovers that we hit a new routine.
		 * The reason is that stepping over functions in a remote 
		 * kernel can have bad results when the function being 
		 * stepped over is used by the kernel in between traps.
		 * (i.e., a trap instruction gets poked into the function
		 * being stepped over).
		 */
		if (step_over_calls > 0 &&
		    ((insn.code & 0xc0000000) == 0x40000000 || 
		     (insn.code & 0xfff80000) == 0x9fc00000)) {
			target0 = PC_ADJUST(pc);
			target1 = 0;
		} else {
			target0 = read_register(NPC_REGNUM);
			target1 = annulled_dest(insn, pc, target0);
			if (target1 == target0)
				target1 = 0;
		}
		target_insert_breakpoint(target0, shadow0);
		if (target1)
			target_insert_breakpoint(target1, shadow1);
		one_stepped = 1;
	} else {
		/* Remove breakpoints */
		if (target1)
			target_remove_breakpoint(target1, shadow1);
		target_remove_breakpoint(target0, shadow0);
		one_stepped = 0;
	}
}

/* XXX this looks polluted */
#define	FRAME_SAVED_L0	0		/* Byte offset from SP */
#define	FRAME_SAVED_I0	32		/* Byte offset from SP */

/*
 * Find the previous stack frame pointer, given the current frame.
 * On a sparc, this is just the i6 register that was flushed to
 * the kernel save area (pointed to by the previous frame's sp,
 * which is the current frame's fp).
 */
CORE_ADDR
sparc_frame_chain(frame)
	FRAME frame;
{
	CORE_ADDR retval;
	CORE_ADDR addr;

	addr = frame->frame + FRAME_SAVED_I0 +
		  REGISTER_RAW_SIZE(FP_REGNUM) * (FP_REGNUM - I0_REGNUM);
	if (target_read_memory(addr, (char *)&retval, sizeof(retval)))
		return (0);
	SWAP_TARGET_AND_HOST(&retval, sizeof(retval));
	return (retval);
}

/*
 * Read the the first word after the kernel save area of the current
 * frame.  This is where Sun cc puts a pointer to storage for a structure
 * return value.  Return 0 on read errors.
 */
CORE_ADDR
sparc_extract_struct_value_address(regs)
	char *regs;
{
	CORE_ADDR addr, v;

	bcopy(&regs[REGISTER_BYTE(SP_REGNUM)], (char *)&addr, sizeof(addr));
	if (target_read_memory(addr, (char *)&v, sizeof(v)))
		return (0);
	SWAP_TARGET_AND_HOST(&v, sizeof(v));
	return (v);
}

/*
 * Find the pc saved in frame FRAME.  
 */
CORE_ADDR
frame_saved_pc(frame)
     FRAME frame;
{
	return (PC_ADJUST(read_memory_integer(addr_of_pc(frame), 4)));
}

FRAME
setup_arbitrary_frame(fp, sp)
	FRAME_ADDR fp, sp;
{
	FRAME f = create_new_frame(fp, 0);
	f->bottom = sp;
	f->pc = frame_saved_pc(f);

	return (f);
}

static int restore_code[] = {
	0x81e80000,		/* restore */
	0x91d02001,		/* t 1 */
	0x01000000		/* nop */
};

static void
do_restore_insn()
{
	/* From infrun.c */
	extern int stop_after_trap;

	CORE_ADDR sp = read_register(SP_REGNUM);
	CORE_ADDR pc = sp - sizeof(restore_code);
	
	write_memory(pc, (char *)restore_code, sizeof(restore_code));
	clear_proceed_status ();
	stop_after_trap = 1;
	proceed(pc, 0, 0);
}

/*
 * True iff sparc insn INSN is a save instruction.
 */
#define SAVE_INSN_P(i) ((insn).f3b.op == 2 && (insn).f3b.op3 == 60)

/*
 * True iff sparc insn INSN is a store instruction of an input register
 * into the arg overflow area, which starts at [fp + 64] and has a 
 * length of 24 bytes (enough for i0 through i5).
 */
#define ARGSTORE_INSN_P(insn)\
	((insn).f3b.op == 3 && ((insn).f3b.op3 & 0x3c) == 4 && \
	 (insn).f3b.rs1 == FP_REGNUM && (insn).f3b.i == 1 && \
	 (insn).f3b.simm13 >= 64 && (insn).f3b.simm13 < 64 + 24)

CORE_ADDR 
skip_prologue(addr, frameless_p)
	CORE_ADDR addr;
	int frameless_p;
{
	register union sparcinsn insn;
	register CORE_ADDR pc = addr;
	register int n = 3;

	while (--n >= 0) {
		insn.code = read_memory_integer(pc, 4);
		pc += 4;
		if (SAVE_INSN_P(insn)) {
			if (frameless_p)
				return (pc);
			insn.code = read_memory_integer(pc, 4);
			while (ARGSTORE_INSN_P(insn)) {
				pc += 4;
				insn.code = read_memory_integer(pc, 4);
			}
		}
		return (pc);
	}
	/*
	 * Couldn't find a save instruction.
	 */
	return (addr);
}

void
sparc_pop_frame()
{
	register CORE_ADDR pc;

	pc = read_register(I7_REGNUM);
	do_restore_insn();
	/*
	 * Return address in %i7 -- 
	 * adjust it, then restore PC and NPC from it.
	 */
	pc = PC_ADJUST(read_memory_integer(pc, 4));
	write_register(PC_REGNUM, pc);
	write_register(NPC_REGNUM, pc + 4);

	flush_cached_frames();
	set_current_frame(create_new_frame(read_register(FP_REGNUM), pc));
}

/*
 * Sun's cc leaves a hole after a call to function returning a structure
 * (after the delay slot).  Given a function call at PC, return the
 * address to which the called function will return.
 */
CORE_ADDR
sparc_pc_adjust(pc)
	register CORE_ADDR pc;
{
	u_long op;

	if (target_read_memory(pc + 8, (char *)&op, 4) == 0) {
		SWAP_TARGET_AND_HOST(&op, 4);
		if ((op & ~0x1ff) == 0)
			/*
			 * The 20 high bits of this opcode are 0, which
			 * does not conform to a valid instruction.  
			 * It must be the Sun structure length hack.
			 */
			return (pc + 12);
	}
	return (pc + 8);
}

int dummy_code[] = {
	0xd003a044,		/* ld	[%sp + 68], %o0 */
	0xd203a048,		/* ld	[%sp + 72], %o1 */
	0xd403a04c,		/* ld	[%sp + 76], %o2 */
	0xd603a050,		/* ld	[%sp + 80], %o3 */
	0xd803a054,		/* ld	[%sp + 84], %o4 */
#define DUMMY_CALL_INDEX 5
	0x40000000,		/* call .		*/
	0xda03a058,		/* ld	[%sp + 88], %o5 */
#define DUMMY_STRUCTLEN_INDEX 7
	0x01000000,		/* nop - extra insn for Sun cc */
	0x91d02001,		/* ta	1		*/
	0x01000000,		/* nop */
};

/*
 * Leave room on the stack for the kernel save area and the pointer
 * for structure return values.
 */
#define KSA_AND_STRUCT_ADJUST 68

/*
 * Build `dummy' call instructions on inferior's stack to cause
 * it to call a subroutine.
 *
 * N.B. - code in wait_for_inferior requires that sp < pc < fp when
 * we take the trap 2 above so it will recognize that we stopped
 * at a `dummy' call.  So, after the call sp is *not* decremented
 * to clean the arguments, code & other stuff we lay on the stack.
 * Since the regs are restored to saved values at the breakpoint,
 * sp will get reset correctly.  Also, this restore means we don't
 * have to construct frame linkage info to save pc & fp.  The lack
 * of frame linkage means we can't do a backtrace, etc., if the
 * called function gets a fault or hits a breakpoint but code in
 * run_stack_dummy makes this impossible anyway.
 */
CORE_ADDR
setup_dummy(sp, funaddr, nargs, args, struct_return_bytes, gcc_p)
	CORE_ADDR sp;
	CORE_ADDR funaddr;
	int nargs;
	value *args;
	int struct_return_bytes;
	int gcc_p;
{
	int len, padding, i;
	CORE_ADDR top = sp, struct_addr, pc;

	pc = sp - sizeof(dummy_code);
	len = value_arg_bytes(nargs, args) + KSA_AND_STRUCT_ADJUST + 
		sizeof(dummy_code) + struct_return_bytes;
	padding = STACK_ALIGN(len) - len;
	sp = pc - padding - struct_return_bytes;
	struct_addr = sp;
	for (i = 0; i < nargs; ++i) {
		/* pushfn doesn't actually change SP_REGNUM */
		sp = value_arg_push(sp, args[i]);
	}
	sp -= KSA_AND_STRUCT_ADJUST;
	if (struct_return_bytes) {
		write_memory(sp + KSA_AND_STRUCT_ADJUST - 4,
			     (char *)&struct_addr, 4);
		/* Appease Sun's twisted convention for struct returns. */
		if (!gcc_p)
			dummy_code[DUMMY_STRUCTLEN_INDEX] = 
				struct_return_bytes & 0x1fff;
	} else
		/* nop */
		dummy_code[DUMMY_STRUCTLEN_INDEX] = 0x01000000;

	write_register(SP_REGNUM, sp);
	dummy_code[DUMMY_CALL_INDEX] = 0x40000000 | 
		((unsigned)(funaddr - (pc + 4 * DUMMY_CALL_INDEX)) >> 2);

	write_memory(pc, (char *)dummy_code, sizeof(dummy_code));

	return pc;
}

/*
 * Only worry about KSA.
 */
frame_find_saved_regs(fi, srp)
	struct frame_info *fi;
	struct frame_saved_regs *srp;
{
	register int i;
	FRAME_ADDR frame;

	bzero ((char *)srp, sizeof *srp);
	frame = fi->bottom;
	/* XXX There's no excuse for this. */
	if (frame == 0)
		abort();

	/* ins and locals */
	for (i = 16; i < 32; i++)
		srp->regs[i] = frame + (i-16) * 4;

	if (fi->next != 0) {
		frame = fi->next->bottom;
		/* outs */
		for (i = 8; i < 16; i++)
			srp->regs[i] = frame + i * 4;
	}
	srp->regs[PC_REGNUM] = addr_of_pc(fi);
}

static void
fix_leaf_frame(arg, from_tty)
	char* arg;
	int from_tty;
{
	int arg1 = 0, level;
	FRAME fid, nfid;
	struct frame_info *fi, *nfi;

	if (!target_has_stack)
		error ("No inferior or core file.");

	if (arg) {
		while (*arg && *arg == ' ')
			++arg;

		if (*arg)
			arg1 = parse_and_eval_address(arg);
	}
	level = arg1;
	fid = find_relative_frame(get_current_frame(), &level);
	if (level != 0)
		error("Can't find frame %d",  arg1);

	fi = get_frame_info(fid);
	nfid = create_new_frame(fi->frame, fi->pc);
	nfi = get_frame_info(nfid);
	*nfi = *fi;
	nfi->pc = read_register(RP_REGNUM);
	nfi->next = fi;
	fi->prev = nfi;
}

void
_initialize_sparctcmn()
{
	add_com("fix-leaf", class_stack, fix_leaf_frame,
		"fix up the call stack to account for a leaf routine\n\
A new frame is inserted for the routine that called the leaf.");
}
