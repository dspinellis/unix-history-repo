/*-
 * Copyright (c) 1990 William Jolitz.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)npx.c	7.2 (Berkeley) %G%
 */
#include "npx.h"
#if NNPX > 0

#include "param.h"
#include "systm.h"
#include "conf.h"
#include "file.h"
#include "proc.h"
#include "machine/cpu.h"
#include "machine/pcb.h"
#include "machine/trap.h"
#include "ioctl.h"
#include "machine/specialreg.h"
#include "i386/isa/isa_device.h"
#include "icu.h"
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
	/*outb(0xb1,0);		/* reset processor */
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
 * Load floating point context and record ownership to suite
 */
npxload() {

	if (npxproc) panic ("npxload");
	npxproc = curproc;
	npxpcb = curpcb;
	asm("	frstor %0 " : : "g" (curpcb->pcb_savefpu) );
}

/*
 * Unload floating point context and relinquish ownership
 */
npxunload() {

	if (npxproc == 0) panic ("npxunload");
	asm("	fsave %0 " : : "g" (npxpcb->pcb_savefpu) );
	npxproc = 0 ;
}

/*
 * Record information needed in processing an exception and clear status word
 */
npxintr(frame) struct intrframe frame; {
	struct trapframe tf;

	outb(0xf0,0);		/* reset processor */

	/* sync state in process context structure, in advance of debugger/process looking for it */
	if (npxproc == 0 || npxexists == 0) panic ("npxintr");
	asm ("	fnsave %0 " : : "g" (npxpcb->pcb_savefpu) );

	/*
	 * Prepair a trap frame for our generic exception processing routine, trap()
	 */
	bcopy(&frame.if_es, &tf, sizeof(tf));
	tf.tf_trapno = T_ARITHTRAP;
#ifdef notyet
	/* encode the appropriate code for detailed information on this exception */
	tf.tf_err = ???;
#endif
	trap(tf);

	/*
	 * Restore with any changes to superior frame
	 */
	bcopy(&tf, &frame.if_es, sizeof(tf));

	/* clear the exception so we can catch others like it */
	asm ("	fnclex");
}

/*
 * Implement device not available (DNA) exception
 */
npxdna() {

	if (npxexists == 0) return(0);
	if (!(curpcb->pcb_flags & FP_WASUSED)
	    ||(curpcb->pcb_flags & FP_NEEDSRESTORE)) {
		load_cr0(rcr0() & ~CR0_EM);	/* stop emulating */
		asm("	frstor %0 " : : "g" (curpcb->pcb_savefpu));
		curpcb->pcb_flags |= FP_WASUSED | FP_NEEDSSAVE;
		curpcb->pcb_flags &= ~FP_NEEDSRESTORE;
		npxproc = curproc;
		npxpcb = curpcb;
		
		return(1);
	}
	return (0);
}
#endif
