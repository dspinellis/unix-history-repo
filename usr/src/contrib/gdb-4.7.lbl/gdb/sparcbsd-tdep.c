/* Target-dependent code for the SPARC for GDB, the GNU debugger.
   Copyright 1986, 1987, 1989, 1991 Free Software Foundation, Inc.

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

/*
 * This is the target dependent code for a sparcstation running BSD.
 * We cannot share this code with SunOS because many kernel data
 * structures are different.  The shareable code is in sparc-tcmn.c.
 */

#define __sys_stdtypes_h /* XXX defeat sun types file */
#include <sys/param.h>

#include <stdio.h>
#include "defs.h"
#include "frame.h"
#include "inferior.h"
#include "obstack.h"
#include "target.h"
#include "ieee-float.h"

#include <sys/ptrace.h>

#include "gdbcore.h"

#ifdef KERNELDEBUG
#include <kvm.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <fcntl.h>
extern int kernel_debugging;
CORE_ADDR ksym_lookup();
CORE_ADDR addr_of_pc();
/*
 * Read the "thing" at address 'addr' into the space pointed to by P.
 * The length of the "thing" is determined by the type of P.
 * Result is non-zero if transfer fails.
 */
#define READMEM(addr, p) \
    (target_read_memory((CORE_ADDR)(addr), (char *)(p), sizeof(*(p))))
#endif

/*
 * Return the address of the saved pc in frame.
 */
CORE_ADDR
addr_of_pc(frame)
	struct frame_info *frame;
{
	CORE_ADDR addr;
	register struct rwindow *rw;
#ifdef KERNELDEBUG
	/*
	 * If we are kernel debugging, we must special case trap frames. We
	 * can tell if we are a trap frame by looking at the return address
	 * of the frame below us.  If it is in locore, then we are such a
	 * frame and we can find our saved pc in %l1.
	 */
	if (kernel_debugging && frame->next) {
		static CORE_ADDR tstart, tend;

		if (tstart == 0) {
			tstart = ksym_lookup("trapbase");
			tend = ksym_lookup("endtrapcode");
		}
		rw = (struct rwindow *)frame->next->bottom;
		addr = read_memory_integer((CORE_ADDR)&rw->rw_in[7], 4);
		if (addr >= tstart && addr < tend) {
			rw = (struct rwindow *)frame->bottom;
			return (CORE_ADDR)&rw->rw_local[1];
		}
	}
#endif
	rw = (struct rwindow *)frame->bottom;
	return (CORE_ADDR)&rw->rw_in[7];
}

#ifdef KERNELDEBUG
/*
 * The code below implements kernel debugging of crashdumps (or /dev/kmem)
 * or remote systems (via a serial link).  For remote kernels, the remote
 * context does most the work, so there is very little to do -- we just
 * manage the kernel stack boundaries so we know where to stop a backtrace.
 *
 * The crashdump/kmem (kvm) support is a bit more grungy, but thanks to
 * libkvm (see kcore.c) not too bad.  The main work is kvm_fetch_registers
 * which sucks the register state out of the current processes pcb.
 * There is a command that let's you set the current process -- hopefully,
 * to something that's blocked (in the live kernel case).
 */

/* XXX For misc_function_vector. */
#include "symtab.h"

/*
 * Address ranges for the kernel interrupt stack (fixed) and the current
 * process' kernel stack (dynamic).
 */
static CORE_ADDR intstack_top;
static CORE_ADDR intstack_bottom;
static CORE_ADDR kernstack_top;
static CORE_ADDR kernstack_bottom;
static struct pcb *cpcb;
void set_curproc();

/*
 * Return true if ADDR is a valid stack address according to the
 * current boundaries (which are determined by the currently running 
 * user process).
 */
int
inside_kernstack(addr)
        CORE_ADDR addr;
{
        if (cpcb == 0)
		set_curproc();

        return (addr > intstack_bottom && addr < intstack_top) ||
                (addr > kernstack_bottom && addr < kernstack_top);
}

/*
 * (re-)set the variables that make inside_kernstack() work.
 */
void
set_kernel_boundaries(p)
        struct pcb *p;
{
	register CORE_ADDR a = (CORE_ADDR)p;

        if (intstack_top == 0) {
                intstack_top = ksym_lookup("eintstack");
                intstack_bottom = ksym_lookup("intstack");
        }
	kernstack_bottom = a;
	kernstack_top = a + UPAGES * NBPG;
}

/*
 * Return the current proc.  masterprocp points to
 * current proc which points to current u area.
 */
struct pcb *
fetch_cpcb()
{
	struct pcb *p;
	static CORE_ADDR addr;

	if (addr == 0)
		addr = ksym_lookup("cpcb");
	if (READMEM(addr, &p))
		error("cannot read cpcb pointer at 0x%x\n", addr);
	return (p);
}

/*
 * Called from remote_wait, after the remote kernel has stopped.
 * Look up the current proc, and set up boundaries.
 * This is for active kernels only.
 */
void
set_curproc()
{
	cpcb = fetch_cpcb();
	set_kernel_boundaries(cpcb);
}

/*
 * All code below is exclusively for support of kernel core files.
 */

/*
 * Fetch registers from a crashdump or /dev/kmem.
 */
static void
kvm_fetch_registers(p)
	struct pcb *p;
{
	int i;
	u_long reg, sp;
	float freg;
	struct rwindow win;
	struct trapframe tf;
	struct pcb pcb;

	/* find the pcb for the current process */
	if (READMEM(p, &pcb))
		error("cannot read pcb at 0x%x", p);

        /*
         * Invalidate all the registers then fill in the ones we know about.
         */
	registers_changed();

	sp = pcb.pcb_sp;
	supply_register(SP_REGNUM, (char *)&pcb.pcb_sp);
	supply_register(PC_REGNUM, (char *)&pcb.pcb_pc);
	/* PC came from o7. */
	supply_register(15, (char *)&pcb.pcb_pc);
	supply_register(PS_REGNUM, (char *)&pcb.pcb_psr);
#ifdef notyet
	/* XXX There should be a WIM_REGNUM. */
	/* XXX must compute 1 << pcb.pcb_wim */
	supply_register(66, (char *)&pcb.pcb_uwm);
#endif
	/*
	 * Read last register window saved on stack.
	 */
	if (READMEM(sp, &win)) {
		printf("cannot read register window at sp=%x\n", pcb.pcb_sp);
		bzero((char *)&win, sizeof win);
	}
	for (i = 0; i < 8; ++i)
		supply_register(i + 16, &win.rw_local[i]);
	for (i = 0; i < 8; ++i)
		supply_register(i + 24, &win.rw_in[i]);
	/*
	 * read the globals & outs saved on the stack (for a trap frame).
	 */
	sp += 96;	/* XXX sizeof(struct frame) */
	if (READMEM(sp, &tf) == 0) {
		/* XXX 8+6 below knows that o0..o5 appear after g7 */
		for (i = 1; i < 8 + 6; i++)
			supply_register(i, (char *)&tf.tf_global[i]);
	}
}

/*
 * Set the process context to that of the proc structure at
 * system address paddr.  Read in the register state.
 */
int
set_procaddr(paddr)
        CORE_ADDR paddr;
{
	struct pcb *ppcb;

	if (paddr == 0)
		cpcb = fetch_cpcb();
	else {
		struct proc *p = (struct proc *)paddr;

		if ((unsigned)p < KERNBASE)
			return (1);
		if (READMEM(&p->p_addr, &ppcb))
			error("cannot read p_addr at 0x%x", &p->p_addr);
		cpcb = ppcb;
	}
        set_kernel_boundaries(cpcb);
	kvm_fetch_registers(cpcb);
        return (0);
}

/*
 * Get the registers out of a crashdump or /dev/kmem.
 * XXX This somehow belongs in kcore.c.
 *
 * We just get all the registers, so we don't use regno.
 */
/* ARGSUSED */
void
kernel_core_registers (regno)
	int regno;
{
        /*
         * Need to find current u area to get kernel stack and pcb
         * where "panic" saved registers.
         * (libkvm also needs to know current u area to get user
         * address space mapping).
	 */
        (void)set_procaddr(cpcb);
}

#endif

/* Structure of SPARC extended floating point numbers.
   This information is not currently used by GDB, since no current SPARC
   implementations support extended float.  */

const struct ext_format ext_format_sparc = {
/* tot sbyte smask expbyte manbyte */
   16, 0,    0x80, 0,1,	   4,8,		/* sparc */
};
