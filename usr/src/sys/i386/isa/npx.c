/*
 * 387 and 287 Numeric Coprocessor Extension (NPX) Driver.
 * @(#)npx.c	1.1 (Berkeley) %G%
 */

int	npxprobe(), npxattach(), npxintr();
struct	driver npxdriver = {
	npxprobe, npxattach, "npx",
};

struct proc	*npxproc;	/* process who owns device, otherwise zero */
struct user	*npxutl;	/* owners user structure */
struct pte	*Npxmap;	/* kernel ptes mapping owner's user structure */

/*
 * Probe routine - look device, otherwise set emulator bit
 */
npxprobe(dvp)
	struct device *dvp;
{
	register status;

#ifdef lint
	npxintr();
#endif
/* insure EM bit off */
	asm("	fninit");	/* put device in known state */

	/* check for a proper status of zero */
	status = 0xa5a5;	
	asm("	movw	%1,%%ax ; fnstsw %%ax ;  movw %%ax, %0"
		: "=g" (status) : "g" (status) : "ax");

	if (status == 0) {
		register control;

		/* good, now check for a proper control word */
		control = 0xa5a5;	
		asm("	movw	%1,%%ax ; fnstcw %%ax ;  movw %%ax, %0"
			: "=g" (control) : "g" (control) : "ax");

		if (control&0x103f == 0x3f) {
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
	struct device *dvp;
{
	int unit = dvp->unit;

	npxinit();
	/* check for ET bit to decide 387/287 */
	INTREN(IRQ13);
	/*outb(0xb1,0);		/* reset processor */
}

/*
 * Initialize floating point unit, usually after an error
 */
npxinit() {
	register control;

	asm ("	fninit");
	control = XXX;
	asm("	movw	%0,%%ax ; fldcw %%ax "
			: "g" (control) : "ax");

}

/*
 * Load floating point context and record ownership to suite
 */
npxload() {

	if (npxproc) panic ("npxload");
	npxproc = u.u_procp;
	uaccess(npxproc, Npxmap, &npxutl);
	asm("	frstor %0 " : "g" (u.u_pcb.pcb_savefpu) );
}

/*
 * Unload floating point context and relinquish ownership
 */
npxunload() {

	if (npxproc == 0) panic ("npxunload");
	asm("	fsave %0 " : "g" (npxutl.u_pcb.pcb_savefpu) );
	npxproc = 0 ;
}

/*
 * Record information needed in processing an exception and clear status word
 */
npxexcept() {

	/* save state in appropriate user structure */
	if (npxproc == 0) panic ("npxexcept");
	asm ("	fsave %0 " : "g" (npxutl.u_pcb.pcb_savefpu) );

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
