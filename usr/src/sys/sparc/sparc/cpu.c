/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cpu.c	7.4 (Berkeley) %G%
 *
 * from: $Header: cpu.c,v 1.10 93/04/20 11:16:51 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/device.h>

#include <machine/autoconf.h>
#include <machine/cpu.h>
#include <machine/reg.h>

/* the following are used externally (sysctl_hw) */
char	machine[] = "sparc";
char	cpu_model[80];
int	cpuspeed;		/* XXX */

static char *psrtoname();
static char *fsrtoname();

/*
 * Attach the CPU.  Right now we just print stuff like "Sun 4/65 (25 MHz)".
 * Eventually we will need more....
 */
static void
cpu_attach(parent, dev, aux)
	struct device *parent;
	struct device *dev;
	void *aux;
{
	register int node, clk;
	register u_int psr, fver;
	register char *fpuname;
	struct fpstate fpstate;

	/*
	 * Get the FSR and clear any exceptions.  If we do not unload
	 * the queue here and it is left over from a previous crash, we
	 * will panic in the first loadfpstate(), due to a sequence error,
	 * so we need to dump the whole state anyway.
	 *
	 * If there is no FPU, trap.c will advance over all the stores,
	 * so we initialize fs_fsr here.
	 */
	fpstate.fs_fsr = 7 << FSR_VER_SHIFT;	/* 7 is reserved for "none" */
	savefpstate(&fpstate);
	fver = (fpstate.fs_fsr >> FSR_VER_SHIFT) & (FSR_VER >> FSR_VER_SHIFT);
	psr = getpsr();
	if (fver != 7) {
		foundfpu = 1;
		fpuname = fsrtoname(psr, fver);
	} else
		fpuname = "no";

	/* tell them what we have */
	node = ((struct romaux *)aux)->ra_node;
	clk = getpropint(node, "clock-frequency", 0);
	sprintf(cpu_model, "%s (%s @ %s MHz, %s FPU)",
	    getpropstring(node, "name"), psrtoname(psr),
	    clockfreq(clk), fpuname);
	printf(": %s\n", cpu_model);
	cpuspeed = clk / 1000000;	/* XXX */
}

struct cfdriver cpucd =
    { NULL, "cpu", matchbyname, cpu_attach, DV_CPU, sizeof(struct device) };

static char *
psrtoname(psr)
	register u_int psr;
{
	int impl = psr >> 28, vers = (psr >> 24) & 15;

	switch (impl) {

	case 0:
		if (vers == 0)
			return ("MB86900/1A or L64801");
		break;

	case 1:
		if (vers < 2)
			return ("CY7C601 or L64811");
		if (vers == 3)
			return ("CY7C611");
		break;

	case 2:
		if (vers == 0)
			return ("B5010");
		break;

	case 5:
		if (vers == 0)
			return ("MN10501");
		break;
	}
	return ("???");
}

static char *
fsrtoname(psr, fver)
	register u_int psr, fver;
{

	switch (psr >> 28) {

	case 0:
		switch (fver) {
		case 0:
			return ("MB86910 or WTL1164/5");
		case 1:
			return ("MB86911 or WTL1164/5");
		case 2:
			return ("L64802 or ACT8847");
		case 3:
			return ("WTL3170/2");
		case 4:
			return ("L64804");
		}
		break;

	case 1:
		switch (fver) {
		case 0:
			return ("L64812 or ACT8847");
		case 1:
			return ("L64814");
		case 2:
			return ("TMS390C602A");
		case 3:
			return ("WTL3171");
		}
		break;

	case 2:
		if (fver == 0)
			return ("B5010 or B5110/20 or B5210");
		break;

	case 5:
		if (fver == 0)
			return ("MN10501");
	}
	return ("???");
}
