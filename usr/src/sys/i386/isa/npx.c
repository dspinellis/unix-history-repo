/*-
 * Copyright (c) 1990 William Jolitz.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)npx.c	7.5 (Berkeley) %G%
 */
#include "npx.h"
#if NNPX > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/ioctl.h>

#include <machine/cpu.h>
#include <machine/trap.h>
#include <machine/specialreg.h>
#include <i386/isa/isa_device.h>
#include <i386/isa/icu.h>
/*
 * 387 and 287 Numeric Coprocessor Extension (NPX) Driver.
 */

int	npxprobe(), npxattach(), npxintr();
struct	isa_driver npxdriver = {
	npxprobe, npxattach, "npx",
};

struct proc *npxproc;	/* process who owns device, otherwise zero */
struct pcb *npxpcb;	/* owners context structure */
static npxexists;
extern long npx0mask;

/*
 * Probe routine - look device, otherwise set emulator bit
 */
npxprobe(dvp)
	struct isa_device *dvp;
{	static status, control;

#ifdef lint
	npxintr();
#endif

	/* insure EM bit off */
	load_cr0(rcr0() & ~CR0_EM);

	asm("	fninit ");	/* put device in known state */

	/* check for a proper status of zero */
	status = 0xa5a5;	
	asm ("	fnstsw	%0 " : "=m" (status) : "m" (status) );

	if (status == 0) {

		/* good, now check for a proper control word */
		control = 0xa5a5;	
		asm ("	fnstcw %0 " : "=m" (control) : "m" (control));

		if ((control&0x103f) == 0x3f) {
			/* then we have a numeric coprocessor */
		/* XXX should force an exception here to generate an intr */
			return (1);
		}
	}

	/* insure EM bit on */
	load_cr0(rcr0() | CR0_EM);
	return (0);
}

/*
 * Attach routine - announce which it is, and wire into system
 */
npxattach(dvp)
	struct isa_device *dvp;
{

	npxinit(0x262);
	/* check for ET bit to decide 387/287 */
	npxexists++;
	npx0mask = dvp->id_irq;
}

/*
 * Initialize floating point unit, usually after an error
 */
npxinit(control) {

	if (npxexists == 0) return;

	load_cr0(rcr0() & ~CR0_EM);	/* stop emulating */
#ifdef INTEL_COMPAT
	asm ("	finit");
	asm("	fldcw %0" : : "g" (control));
	asm("	fnsave %0 " : : "g" (curpcb->pcb_savefpu) );
#else
	asm("fninit");
	asm("	fnsave %0 " : : "g" (curpcb->pcb_savefpu) );
#endif
	load_cr0(rcr0() | CR0_EM);	/* start emulating */

}

/*
 * Record information needed in processing an exception and clear status word
 */
npxintr(frame)
	struct intrframe frame;
{
	if (npxexists == 0) {
		printf ("stray npxintr\n");
		return;
	}

	outb(0xf0,0);		/* reset coprocessor */

	/*
	 * npxproc may be NULL, if this is a delayed interrupt from
	 * a process that just exited.
	 */
	if (npxproc) {
		/*
		 * sync state in process context structure, in advance 
		 * of debugger/process looking for it.
		 */
		asm("fnsave %0" :: "g" (npxproc->p_addr->u_pcb.pcb_savefpu));
		psignal(npxproc, SIGFPE);

		/* clear the exception so we can catch others like it */
		asm ("	fnclex");
	}
}

/*
 * Implement device not available (DNA) exception
 */
npxdna()
{
	if (npxexists == 0)
		return(0);

	load_cr0(rcr0() & ~CR0_EM);	/* stop emulating */
	if (npxproc != curproc) {
		if (npxproc)
			asm(" fnsave %0 "::"g"
			    (npxproc->p_addr->u_pcb.pcb_savefpu));
		asm("	frstor %0 " : : "g" (curpcb->pcb_savefpu));
		npxproc = curproc;
	}
	return (1);
}
#endif
