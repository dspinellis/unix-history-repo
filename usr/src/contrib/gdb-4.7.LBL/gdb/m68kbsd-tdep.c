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
 * This is the target dependent code for an hp300 running BSD.
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

/*
 * Read the "thing" at address 'addr' into the space pointed to by P.
 * The length of the "thing" is determined by the type of P.
 * Result is non-zero if transfer fails.
 */
#define READMEM(addr, p) \
    (target_read_memory((CORE_ADDR)(addr), (char *)(p), sizeof(*(p))))
#endif

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
 * Address ranges for the current process' kernel stack (fixed).
 */
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

        return (addr > kernstack_bottom && addr < kernstack_top);
}

/*
 * (re-)set the variables that make inside_kernstack() work.
 */
void
set_kernel_boundaries(p)
        struct pcb *p;
{
	static CORE_ADDR kstack;

	if (kstack == 0)
		kstack = ksym_lookup("kstack");
	kernstack_bottom = kstack;
	kernstack_top = kstack + UPAGES * NBPG;
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
		addr = ksym_lookup("curpcb");
	if (READMEM(addr, &p))
		error("cannot read curpcb pointer at 0x%x\n", addr);
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
	u_long v;
	struct pcb pcb;

	/* find the pcb for the current process */
	if (READMEM(p, &pcb))
		error("cannot read pcb at 0x%x", p);

        /*
         * Invalidate all the registers then fill in the ones we know about.
         */
	registers_changed();

	for (i = 2; i <= 7; ++i)
		supply_register(i, (char *)&pcb.pcb_regs[i - 2]);

	for (i = 10; i <= 15; ++i)
		supply_register(i, (char *)&pcb.pcb_regs[i - 4]);

	v = pcb.pcb_ps;
	supply_register(PS_REGNUM, (char *)&v);
	
	/* PC is on top of the stack */
	if (READMEM(pcb.pcb_regs[11], &v) == 0)
		supply_register(PC_REGNUM, (char *)&v);
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
        /*
         * Need to find current u area to get kernel stack and pcb
         * where "panic" saved registers.
         * (libkvm also needs to know current u area to get user
         * address space mapping).
	 */

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
	if (cpcb == 0)
		(void)set_procaddr(0);
	else
		kvm_fetch_registers(cpcb);
}

/*
 * XXX We intercept memory transfers since we must translate
 * fixed kernel stack addresses into per-process kernel addresses.
 */
int
Xkernel_xfer_memory(addr, cp, len, write, target)
     CORE_ADDR addr;
     char *cp;
     int len;
     int write;
     struct target_ops *target;
{
	if (cpcb && inside_kernstack(addr))
		addr = addr - kernstack_bottom + (CORE_ADDR)cpcb;
	return kernel_xfer_memory(addr, cp, len, write, target);
}

#endif
