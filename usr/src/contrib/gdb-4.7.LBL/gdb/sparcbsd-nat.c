/* Host-dependent code for SPARC host systems, for GDB, the GNU debugger.
   Copyright 1986, 1987, 1989, 1990, 1991  Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>

/* This code only compiles when we have the definitions in tm-sparc.h.  */

#define	TM_FILE_OVERRIDE
#include "defs.h"
#include "tm-sparc.h"

#include "inferior.h"
#include "target.h"

#include <sys/param.h>
#include <sys/ptrace.h>
#include <machine/reg.h>

#include "gdbcore.h"

extern char register_valid[];
int deferred_stores = 0;

/*
 * Fetch one or more registers from the inferior.  REGNO == -1 to get
 * them all.  We actually fetch more than requested, when convenient,
 * marking them as valid so we won't fetch them again.
 */
void
fetch_inferior_registers(regno)
     int regno;
{
	register int i;
	register CORE_ADDR sp;
	struct trapframe tf;
	struct fpstate fpstate;

	/*
	 * We should never be called with deferred stores, because a
	 * prerequisitefor writing regs is to have fetched them all
	 * (PREPARE_TO_STORE), sigh.
	 */
	if (deferred_stores)
		abort();

	DO_DEFERRED_STORES;

	ptrace(PT_GETREGS, inferior_pid, (caddr_t)&tf, 0);
	bcopy((caddr_t)tf.tf_global, &registers[REGISTER_BYTE(0)], 16 * 4);
	/*
	 * Get state registers.
	 */
	bcopy((caddr_t)&tf.tf_psr, &registers[REGISTER_BYTE(PS_REGNUM)], 4);
	bcopy((caddr_t)&tf.tf_pc, &registers[REGISTER_BYTE(PC_REGNUM)], 4);
	bcopy((caddr_t)&tf.tf_npc, &registers[REGISTER_BYTE(NPC_REGNUM)], 4);
	bcopy((caddr_t)&tf.tf_y, &registers[REGISTER_BYTE(Y_REGNUM)], 4);
	/*
	 * Read locals and ins from the kernel save area indicated by
	 * the frame pointer (o6).
	 * XXX Don't do this if we don't have to.  (See sparc-xdep.c)
	 * FIXME
	 */
	sp = tf.tf_out[6];
	target_read_memory(sp, &registers[REGISTER_BYTE(16)], 16 * 4);
	for (i = G0_REGNUM; i <= I7_REGNUM; i++)
		register_valid[i] = 1;
	register_valid[Y_REGNUM] = 1;
	register_valid[PS_REGNUM] = 1;
	register_valid[PC_REGNUM] = 1;
	register_valid[NPC_REGNUM] = 1;

	/*
	 * If we don't set these valid, read_register_bytes() rereads
	 * all the regs every time it is called!  FIXME.
	 */
	register_valid[WIM_REGNUM] = 1;
	register_valid[TBR_REGNUM] = 1;
	register_valid[FPS_REGNUM] = 1;
	register_valid[CPS_REGNUM] = 1;

	/*
	 * Get floating point register set, if wanted.
	 */
	if (regno > 0 && regno < FP0_REGNUM && regno > FP0_REGNUM + 31)
		return;
		
	ptrace(PT_GETFPREGS, inferior_pid, (caddr_t)&fpstate, 0);
	bcopy(&fpstate.fs_regs[0], &registers[REGISTER_BYTE(FP0_REGNUM)],
	      sizeof fpstate.fs_regs);
	bcopy((caddr_t)&fpstate.fs_fsr, &registers[REGISTER_BYTE(FPS_REGNUM)],
	      sizeof(fpstate.fs_fsr));

	for (i = FP0_REGNUM; i <= FP0_REGNUM+31; i++)
		register_valid[i] = 1;
	register_valid[FPS_REGNUM] = 1;
}

void
store_inferior_ksaregs(regno)
	int regno;
{
	CORE_ADDR addr;
	/*
	 * Write locals and ins into kernel save area on stack.
	 */
	bcopy(&registers[REGISTER_BYTE(SP_REGNUM)], &addr, 4);
	if (regno > 0) {
		/*
		 * Just write the one register in.
		 */
		addr += (regno - 16) * 4;
		target_write_memory(addr, &registers[REGISTER_BYTE(regno)], 4);
	} else
		target_write_memory(addr, &registers[REGISTER_BYTE(16)], 64);
}

void
store_inferior_cpuregs()
{
	struct trapframe tf;

	bcopy(&registers[REGISTER_BYTE(0)], (caddr_t)tf.tf_global, 16 * 4);

	bcopy(&registers[REGISTER_BYTE(PS_REGNUM)], &tf.tf_psr, 4);
	bcopy(&registers[REGISTER_BYTE(PC_REGNUM)], &tf.tf_pc, 4);
	bcopy(&registers[REGISTER_BYTE(NPC_REGNUM)], &tf.tf_npc, 4);
	bcopy(&registers[REGISTER_BYTE(Y_REGNUM)], &tf.tf_y, 4);
	ptrace(PT_SETREGS, inferior_pid, (caddr_t)&tf, 0);
}

void
store_inferior_fpregs()
{
	struct fpstate fpstate;

	bcopy(&registers[REGISTER_BYTE(FP0_REGNUM)], &fpstate.fs_regs[0],
	      sizeof fpstate.fs_regs);
	fpstate.fs_fsr = *(int *)&registers[REGISTER_BYTE(FPS_REGNUM)];
	fpstate.fs_qsize = 0;	
	ptrace(PT_SETFPREGS, inferior_pid, (caddr_t)&fpstate, 0);
}

#define	RF_KSA	1
#define	RF_CPU	2
#define	RF_FPU	4
#define RF_ALL (RF_KSA|RF_CPU|RF_FPU)

static void
storeregs(rf, regno)
	register int rf;
{
	if (rf & RF_KSA)
		store_inferior_ksaregs(regno);
	if (rf & RF_CPU)
		store_inferior_cpuregs();
	if (rf & RF_FPU)
		store_inferior_fpregs();
}

/*
 * Store our register values back into the inferior.
 * If REGNO is -1, do this for all registers.
 * Otherwise, REGNO specifies which register (so we can save time).
 */
void
store_inferior_registers(regno)
	int regno;
{
	register int rf;

	if (regno < 0) {
		if (regno == -2) {
			rf = deferred_stores;
			deferred_stores = 0;
		} else
			rf = RF_ALL;
		storeregs(rf, regno);
	}
	if (regno >= FP0_REGNUM && regno < FP0_REGNUM + 32)
		deferred_stores |= RF_FPU;
	else if (regno < L0_REGNUM || regno > I7_REGNUM)
		deferred_stores |= RF_CPU;
	else 
		storeregs(RF_KSA, regno);
}

static void
fetch_core_intregs(tf)
	register struct trapframe *tf;
{
	bzero((char *)&registers[REGISTER_BYTE (0)], 4);
	/* The globals and output registers.  */
	bcopy((char *)&tf->tf_global[0], 
	      (char *)&registers[REGISTER_BYTE (G0_REGNUM)],
	      15 * REGISTER_RAW_SIZE (G1_REGNUM));
	bcopy((char *)&tf->tf_psr,
	      (char *)&registers[REGISTER_BYTE(PS_REGNUM)], 4);
	bcopy((char *)&tf->tf_pc,
	      (char *)&registers[REGISTER_BYTE(PC_REGNUM)], 4);
	bcopy((char *)&tf->tf_npc,
	      (char *)&registers[REGISTER_BYTE(NPC_REGNUM)], 4);
	bcopy((char *)&tf->tf_y,
	      (char *)&registers[REGISTER_BYTE(Y_REGNUM)], 4);
	
	/*
	 * Try to read KSA from stack.  This might not
	 * work if the stack pointer got trashed.
	 */
	if (target_read_memory((CORE_ADDR)tf->tf_out[6],
			       &registers[REGISTER_BYTE(L0_REGNUM)], 
			       16 * 4) != 0)
		fprintf(stderr, 
		"Couldn't read input and local registers from core file\n");
}

static void
fetch_core_fpregs(fps, size)
	struct fpstate *fps;
	int size;
{
	/* Floating point registers */
	
	if (size < sizeof(*fps)) {
		fprintf(stderr, "Couldn't read float regs from core file\n");
		return;
	}
	bcopy((char *)fps->fs_regs, 
	      &registers[REGISTER_BYTE(FP0_REGNUM)],
	      sizeof(fps->fs_regs));
	bcopy((char *)&fps->fs_fsr,
	      &registers[REGISTER_BYTE(FPS_REGNUM)],
	      sizeof(fps->fs_fsr));
}

void
fetch_core_registers(core_reg_sect, core_reg_size, which, reg_addr)
	char *core_reg_sect;
	unsigned core_reg_size;
	int which;
	unsigned int reg_addr;
{
	if (which == 0)
		fetch_core_intregs((struct trapframe *)core_reg_sect);
	else if (which == 2)
		fetch_core_fpregs((struct fpstate *)core_reg_sect,
				  core_reg_size);
}

int
get_longjmp_target(pc)
	CORE_ADDR *pc;
{
	error("cannot step over longjmp");
}
