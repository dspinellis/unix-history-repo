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
 *
 * Target dependent code for a sparcstation running SunOS.
 * This is mostly kernel debugging support.
 * Common code is in sparc-tcmn.c.
 */

#ifndef lint
static char rcsid[] =
    "@(#) $Header: sparc-tdep.c,v 1.2 93/02/19 15:25:07 mccanne Exp $ (LBL)";
#endif

#include <stdio.h>
#include "defs.h"
#include "frame.h"
#include "target.h"
#include <machine/reg.h>

#ifdef KERNELDEBUG
#include "kernel.h"

#include <kvm.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <fcntl.h>

extern int kernel_debugging;
extern CORE_ADDR ksym_lookup();

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
	register CORE_ADDR addr;
	register struct rwindow *rw;
#ifdef KERNELDEBUG
	/*
	 * If we are kernel debugging, we must special case trap frames.
	 * We can tell if we are a trap frame by looking at the return 
	 * address of the frame below us.  If it is in locore, then
	 * we are such a frame and we can find our saved pc in %l1.
	 */
	if (kernel_debugging && frame->next) {
		static CORE_ADDR locore_h, locore_t;
	
		if (locore_h == 0) {
			locore_h = ksym_lookup("sys_trap");
			locore_t = ksym_lookup("kadb_tcode");
		}
		rw = (struct rwindow *)frame->next->bottom;
		addr = read_memory_integer((CORE_ADDR)&rw->rw_in[7], 4);
		if (addr > locore_h && addr < locore_t) {
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
static struct proc *curproc;

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
        if (curproc == 0)
		set_curproc();

        return (addr > intstack_bottom && addr < intstack_top) ||
                (addr > kernstack_bottom && addr < kernstack_top);
}

/*
 * (re-)set the variables that make inside_kernstack() work.
 */
void
set_kernel_boundaries(p)
        struct proc *p;
{
        CORE_ADDR kstack;

        if (intstack_top == 0) {
                intstack_top = ksym_lookup("eintstack");
                intstack_bottom = ksym_lookup("intstack");
        }
	/*
	 * During boot, if masterprocp is still NULL the kernel
	 * stack lives in [ubasic..ubasic+KERNSTACK).
	 */
	if (p == NULL)
		kstack = ksym_lookup("ubasic");
	else {
		if (READMEM(&p->p_segu, &kstack))
			error("cannot read kernel stack pointer at %x\n",
			    &p->p_segu);
	}
        kernstack_bottom = kstack;
        kernstack_top = kstack + KERNSTACK;
}

/*
 * Return the current proc.  masterprocp points to
 * current proc which points to current u area.
 */
struct proc *
fetch_curproc()
{
        struct proc *p;
        static CORE_ADDR addr;

	if (addr == 0)
		addr = ksym_lookup("masterprocp");

        if (READMEM(addr, &p))
                error("cannot read proc pointer at %x\n", addr);
        return p;
}

/*
 * Called from remote_wait, after the remote kernel has stopped.
 * Look up the current proc, and set up boundaries.
 * This is for active kernels only.
 */
void
set_curproc()
{
	curproc = fetch_curproc();
	set_kernel_boundaries(curproc);
}

/*
 * All code below is exclusively for support of kernel core files.
 */

/*
 * Fetch registers from a crashdump or /dev/kmem.
 */
void
kvm_fetch_registers(p)
	struct proc *p;
{
        struct user *uaddr;
        int i;
        u_long cps, reg, sp;
        float freg;
        struct rwindow win;
	struct pcb pcb;

        /* find the pcb for the current process */
        if (READMEM(&p->p_uarea, &uaddr))
                error("cannot u area ptr for proc at 0x%x", p);
        if (READMEM(&uaddr->u_pcb, &pcb))
                error("cannot read pcb at 0x%x", &uaddr->u_pcb);

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
        /* XXX There should be a WIM_REGNUM. */
        supply_register(66, (char *)&pcb.pcb_uwm);
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
        sp += 92 + 12; /* XXX - MINFRAME + R_Y */
        for (i = 1; i < 14; ++i) {
                u_long val;

                if (READMEM(sp + i*4, &val) == 0)
                        supply_register(i, (char *)&val);
        }
        if (READMEM(pcb.pcb_cpctxp, &cps) == 0)
                supply_register(CPS_REGNUM, (char *)&cps);
}

/*
 * Set the process context to that of the proc structure at
 * system address paddr.  Read in the register state.
 */
int
set_procaddr(paddr)
        CORE_ADDR paddr;
{
        struct proc proc;
        struct user *uaddr;

	if (paddr == 0)
		paddr = (CORE_ADDR)fetch_curproc();

	if (paddr < KERNELBASE)
		return (1);
	if (READMEM(paddr, &proc))
		error("cannot read proc struct at 0x%x", paddr);

	/*
	 * This is REALLY STUPID.  The only way to tell libkvm that we want to
	 * change user address maps is with kvm_getu.
	 * If the getu fails, revert to the old address.
	 */
	if (kernel_getu((u_long *)&proc) == 0) {
		(void)READMEM(curproc, &proc);
		(void)kernel_getu((u_long *)&proc);
		error("cannot read uarea for proc at 0x%x", paddr);
		return (1);
	}
        curproc = (struct proc *)paddr;
        set_kernel_boundaries(curproc);
	kvm_fetch_registers(curproc);
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
kernel_core_registers(regno)
	int regno;
{
        /*
         * Need to find current u area to get kernel stack and pcb
         * where "panic" saved registers.
         * (libkvm also needs to know current u area to get user
         * address space mapping).
	 */
        (void)set_procaddr(curproc);
}

/*
 * Building in support for stepping through a longjmp is silly.
 * Couldn't the gdb maintainers spend their time more productively?
 */
int
get_longjmp_target(pc)
	CORE_ADDR *pc;
{
	return (0);
}

#endif
