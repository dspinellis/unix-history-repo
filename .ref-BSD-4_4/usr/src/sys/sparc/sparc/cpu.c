/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)cpu.c	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: cpu.c,v 1.12 93/05/03 09:47:57 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/device.h>

#include <machine/autoconf.h>
#include <machine/cpu.h>
#include <machine/reg.h>

#include <sparc/sparc/cache.h>

/* This is declared here so that you must include a CPU for the cache code. */
struct cacheinfo cacheinfo;

/* the following are used externally (sysctl_hw) */
char	machine[] = "sparc";
char	cpu_model[80];

static char *psrtoname();
static char *fsrtoname();

/*
 * Attach the CPU.
 * Discover interesting goop about the virtual address cache.
 */
static void
cpu_attach(parent, dev, aux)
	struct device *parent;
	struct device *dev;
	void *aux;
{
	register int node, clk, i, l;
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

	/*
	 * Fill in the cache info.  Note, vac-hwflush is spelled
	 * with an underscore on 4/75s.
	 */
	cacheinfo.c_totalsize = getpropint(node, "vac-size", 65536);
	cacheinfo.c_hwflush = getpropint(node, "vac_hwflush", 0) |
	    getpropint(node, "vac-hwflush", 0);
	cacheinfo.c_linesize = l = getpropint(node, "vac-linesize", 16);
	for (i = 0; (1 << i) < l; i++)
		/* void */;
	if ((1 << i) != l)
		panic("bad cache line size %d", l);
	cacheinfo.c_l2linesize = i;

	vactype = VAC_WRITETHROUGH;	/* ??? */
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
