/*	BSDI $Id: i386bsd-tdep.c,v 1.1.1.1 1992/08/27 17:03:50 trent Exp $	*/

/*
 * Machine-dependent kernel debugging support for BSD/386.
 * Mainly taken from sparcbsd-tdep.c from LBL.
 */

#ifdef KERNELDEBUG
#include <sys/param.h>
#include <sys/time.h>
#include <sys/proc.h>
#include <machine/frame.h>
#include <machine/reg.h>
#include <machine/pcb.h>
#ifdef notdef
#include <machine/vmparam.h>
#endif

#define VM_MAXUSER_ADDRESS 0xfdbfe000

extern int kernel_debugging;

/*
 * Read the "thing" at address 'addr' into the space pointed to by P.
 * The length of the "thing" is determined by the type of P.
 * Result is non-zero if transfer fails.
 */
#define READMEM(addr, p) \
    (target_read_memory((CORE_ADDR)(addr), (char *)(p), sizeof(*(p))))
#endif

#include "defs.h"
#include "frame.h"
#include "value.h"
#include "target.h"
#include "gdbcore.h"

/*
 * Return the address of the saved pc in frame.
 */
CORE_ADDR
addr_of_pc(struct frame_info *frame)
{
#ifdef KERNELDEBUG
	static CORE_ADDR tstart, tend, istart, iend;
	CORE_ADDR pc;
	unsigned long addr;

	if (kernel_debugging && frame->next) {
		if (tstart == 0) {
			tstart = ksym_lookup("Xdiv");
			tend = ksym_lookup("Xsyscall");
			istart = ksym_lookup("Vclk");
			iend = ksym_lookup("doreti");
		}
		pc = FRAME_SAVED_PC(frame->next);
		if (tstart <= pc && pc < tend) {
			struct trapframe *tfr = (struct trapframe *)
				(frame->next->frame + 8);
			return ((CORE_ADDR)&tfr->tf_eip);
		}
		if (istart <= pc && pc < iend) {
			struct intrframe *ifr = (struct intrframe *)
				(frame->next->frame + 8);
			return ((CORE_ADDR)&ifr->if_eip);
		}
	}
#endif
	return ((CORE_ADDR)(frame->next->frame + 4));
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
inside_kernstack(CORE_ADDR addr)
{
	if (cpcb == 0)
		set_curproc();

	return (addr > kernstack_bottom && addr < kernstack_top);
}

/*
 * (re-)set the variables that make inside_kernstack() work.
 */
static void
set_kernel_boundaries(struct pcb *p)
{
#if 0	/* fix this when we no longer map PCBs to a fixed address */
	CORE_ADDR a = (CORE_ADDR)p;
#else
	CORE_ADDR a = (CORE_ADDR)VM_MAXUSER_ADDRESS;
#endif

	kernstack_bottom = a;
	kernstack_top = a + UPAGES * NBPG;
}

/*
 * Return the current proc.  masterprocp points to
 * current proc which points to current u area.
 */
static struct pcb *
fetch_cpcb()
{
	struct pcb *p;
	static CORE_ADDR curpcb_addr;

	if (!curpcb_addr)
		curpcb_addr = ksym_lookup("curpcb");
	if (READMEM(curpcb_addr, &p))
		error("cannot read curpcb pointer at 0x%x\n", curpcb_addr);
	return (p);
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
	struct pcb pcb;

	/* find the pcb for the current process */
	if (READMEM(p, &pcb))
		error("cannot read pcb at 0x%x", p);

        /*
         * Invalidate all the registers then fill in the ones we know about.
         */
	registers_changed();

	supply_register(PC_REGNUM, (char *)&pcb.pcb_pc);
	supply_register(FP_REGNUM, (char *)&pcb.pcb_fp);
	supply_register(SP_REGNUM, (char *)&pcb.pcb_ksp);
	supply_register(PS_REGNUM, (char *)&pcb.pcb_psl);

	supply_register(0, (char *)&pcb.pcb_tss.tss_eax);
	supply_register(1, (char *)&pcb.pcb_tss.tss_ecx);
	supply_register(2, (char *)&pcb.pcb_tss.tss_edx);
	supply_register(3, (char *)&pcb.pcb_tss.tss_ebx);
	supply_register(6, (char *)&pcb.pcb_tss.tss_esi);
	supply_register(7, (char *)&pcb.pcb_tss.tss_edi);
	supply_register(10, (char *)&pcb.pcb_tss.tss_cs);
	supply_register(11, (char *)&pcb.pcb_tss.tss_ss);
	supply_register(12, (char *)&pcb.pcb_tss.tss_ds);
	supply_register(13, (char *)&pcb.pcb_tss.tss_es);
	supply_register(14, (char *)&pcb.pcb_tss.tss_fs);
	supply_register(15, (char *)&pcb.pcb_tss.tss_gs);
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
 * Set the process context to that of the proc structure at
 * system address paddr.  Read in the register state.
 */
int
set_procaddr(CORE_ADDR paddr)
{
	struct pcb *ppcb;

	if (paddr == 0)
		cpcb = fetch_cpcb();
	else if (paddr != (CORE_ADDR)cpcb) {
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
void
kernel_core_registers(int regno)
{
        /*
         * Need to find current u area to get kernel stack and pcb
         * where "panic" saved registers.
         * (libkvm also needs to know current u area to get user
         * address space mapping).
	 */
        (void)set_procaddr((CORE_ADDR)cpcb);
}

#endif
