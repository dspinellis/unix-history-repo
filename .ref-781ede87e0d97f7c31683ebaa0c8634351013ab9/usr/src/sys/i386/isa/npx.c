/*
 * Copyright (c) 1990 W. Jolitz
 * @(#)npx.c	1.2 (Berkeley) %G%
 */
#include "npx.h"
#if	NNPX > 0

#include "param.h"
#include "systm.h"
#include "conf.h"
#include "file.h"
#include "dir.h"
#include "user.h"
#include "ioctl.h"
#include "vm.h"
#include "machine/pte.h"
#include "machine/isa/isa_device.h"
#include "icu.h"
/*
 * 387 and 287 Numeric Coprocessor Extension (NPX) Driver.
 */

int	npxprobe(), npxattach(), npxintr();
struct	isa_driver npxdriver = {
	npxprobe, npxattach, "npx",
};

struct proc	*npxproc;	/* process who owns device, otherwise zero */
extern struct user npxutl;	/* owners user structure */
extern struct pte Npxmap[];	/* kernel ptes mapping owner's user structure */

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
}

/*
 * Initialize floating point unit, usually after an error
 */
npxinit(control) {

	asm ("	fninit");
	asm("	fldcw %0" : : "g" (control));

}

/*
 * Load floating point context and record ownership to suite
 */
npxload() {

	if (npxproc) panic ("npxload");
	npxproc = u.u_procp;
	uaccess(npxproc, Npxmap, &npxutl);
	asm("	frstor %0 " : : "g" (u.u_pcb.pcb_savefpu) );
}

/*
 * Unload floating point context and relinquish ownership
 */
npxunload() {

	if (npxproc == 0) panic ("npxunload");
	asm("	fsave %0 " : : "g" (npxutl.u_pcb.pcb_savefpu) );
	npxproc = 0 ;
}

/*
 * Record information needed in processing an exception and clear status word
 */
npxexcept() {

	/* save state in appropriate user structure */
	if (npxproc == 0) panic ("npxexcept");
	asm ("	fsave %0 " : : "g" (npxutl.u_pcb.pcb_savefpu) );

	/*
	 * encode the appropriate u_code for detailed information
         * on this exception
	 */

	/* signal appropriate process */
	psignal (npxproc, SIGFPE);

	/* clear the exception so we can catch others like it */
	asm ("	fnclex");
}

/*
 * Catch AT/386 interrupt used to signal exception, and simulate trap()
 */
npxintr() {
	outb(0xf0,0);
	pg("npxintr");
}

/*
 * Implement device not available (DNA) exception
 */
npxdna() {
}
#endif
