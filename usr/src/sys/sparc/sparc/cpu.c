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
 * %sccs.include.redist.c%
 *
 *	@(#)cpu.c	8.5 (Berkeley) %G%
 *
 * from: $Header: cpu.c,v 1.18 93/10/31 05:28:13 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/device.h>

#include <machine/autoconf.h>
#include <machine/cpu.h>
#include <machine/reg.h>
#include <machine/trap.h>

#include <sparc/sparc/cache.h>

/* This is declared here so that you must include a CPU for the cache code. */
struct cacheinfo cacheinfo;

/* The following are used externally (sysctl_hw). */
char	machine[] = "sparc";
char	cpu_model[100];

/* The CPU configuration driver. */
static void cpu_attach __P((struct device *, struct device *, void *));

struct cfdriver cpucd =
    { NULL, "cpu", matchbyname, cpu_attach, DV_CPU, sizeof(struct device) };

static char *psrtoname __P((int, int, int, char *));
static char *fsrtoname __P((int, int, int, char *));

#define	IU_IMPL(psr)	((u_int)(psr) >> 28)
#define	IU_VERS(psr)	(((psr) >> 24) & 0xf)

#ifdef notdef
/*
 * IU implementations are parceled out to vendors (with some slight
 * glitches).  Printing these is cute but takes too much space.
 */
static char *iu_vendor[16] = {
	"Fujitsu",	/* and also LSI Logic */
	"ROSS",		/* ROSS (ex-Cypress) */
	"BIT",
	"LSIL",		/* LSI Logic finally got their own */
	"TI",		/* Texas Instruments */
	"Matsushita",
	"Philips",
	"Harvest",	/* Harvest VLSI Design Center */
	"SPEC",		/* Systems and Processes Engineering Corporation */
	"Weitek",
	"vendor#10",
	"vendor#11",
	"vendor#12",
	"vendor#13",
	"vendor#14",
	"vendor#15"
};
#endif

/*
 * Attach the CPU.
 * Discover interesting goop about the virtual address cache
 * (slightly funny place to do it, but this is where it is to be found).
 */
static void
cpu_attach(parent, dev, aux)
	struct device *parent;
	struct device *dev;
	void *aux;
{
	register int node, clk, i, l;
	register int impl, vers, fver;
	register char *fpuname;
	struct fpstate fpstate;
	char iubuf[40], fpbuf[40];

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
	i = getpsr();
	impl = IU_IMPL(i);
	vers = IU_VERS(i);
	if (fver != 7) {
		foundfpu = 1;
		fpuname = fsrtoname(impl, vers, fver, fpbuf);
	} else
		fpuname = "no";

	/* tell them what we have */
	node = ((struct romaux *)aux)->ra_node;
	clk = getpropint(node, "clock-frequency", 0);
	sprintf(cpu_model, "%s (%s @ %s MHz, %s FPU)",
	    getpropstring(node, "name"),
	    psrtoname(impl, vers, fver, iubuf), clockfreq(clk), fpuname);
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
	vactype = VAC_WRITETHROUGH;

	/*
	 * Machines with "buserr-type" 1 have a bug in the cache
	 * chip that affects traps.  (I wish I knew more about this
	 * mysterious buserr-type variable....)
	 */
	if (getpropint(node, "buserr-type", 0) == 1) {
		kvm_uncache((caddr_t)trapbase, 1);
		printf("%s: cache chip bug; trap page uncached\n",
		    dev->dv_xname);
	}
}

/*
 * The following tables convert <IU impl, IU version, FPU version> triples
 * into names for the CPU and FPU chip.  In most cases we do not need to
 * inspect the FPU version to name the IU chip, but there is one exception
 * (for Tsunami), and this makes the tables the same.
 *
 * The table contents (and much of the structure here) are from Guy Harris.
 *
 * NOTE: we have Sun-4m cpu types here, even though this only runs on the
 * Sun-4c (yet)...
 */
struct info {
	u_char	valid;
	u_char	iu_impl;
	u_char	iu_vers;
	u_char	fpu_vers;
	char	*name;
};

#define	ANY	0xff	/* match any FPU version (or, later, IU version) */

static struct info iu_types[] = {
	{ 1, 0x0, 0x0, ANY, "MB86900/1A or L64801" },
	{ 1, 0x1, 0x0, ANY, "RT601 or L64811 v1" },
	{ 1, 0x1, 0x1, ANY, "RT601 or L64811 v2" },
	{ 1, 0x1, 0x3, ANY, "RT611" },
	{ 1, 0x1, 0xf, ANY, "RT620" },
	{ 1, 0x2, 0x0, ANY, "B5010" },
	{ 1, 0x4, 0x0,   0, "TMS390Z50 v0" },
	{ 1, 0x4, 0x1,   0, "TMS390Z50 v1" },
	{ 1, 0x4, 0x1,   4, "TMS390S10" },
	{ 1, 0x5, 0x0, ANY, "MN10501" },
	{ 1, 0x9, 0x0, ANY, "W8601/8701 or MB86903" },
	{ 0 }
};

static char *
psrtoname(impl, vers, fver, buf)
	register int impl, vers, fver;
	char *buf;
{
	register struct info *p;

	for (p = iu_types; p->valid; p++)
		if (p->iu_impl == impl && p->iu_vers == vers &&
		    (p->fpu_vers == fver || p->fpu_vers == ANY))
			return (p->name);

	/* Not found. */
	sprintf(buf, "IU impl 0x%x vers 0x%x", impl, vers);
	return (buf);
}

/* NB: table order matters here; specific numbers must appear before ANY. */
static struct info fpu_types[] = {
	/*
	 * Vendor 0, IU Fujitsu0.
	 */
	{ 1, 0x0, ANY, 0, "MB86910 or WTL1164/5" },
	{ 1, 0x0, ANY, 1, "MB86911 or WTL1164/5" },
	{ 1, 0x0, ANY, 2, "L64802 or ACT8847" },
	{ 1, 0x0, ANY, 3, "WTL3170/2" },
	{ 1, 0x0, ANY, 4, "L64804" },

	/*
	 * Vendor 1, IU ROSS0/1 or Pinnacle.
	 */
	{ 1, 0x1, 0xf, 0, "on-chip" },		/* Pinnacle */
	{ 1, 0x1, ANY, 0, "L64812 or ACT8847" },
	{ 1, 0x1, ANY, 1, "L64814" },
	{ 1, 0x1, ANY, 2, "TMS390C602A" },
	{ 1, 0x1, ANY, 3, "RT602 or WTL3171" },

	/*
	 * Vendor 2, IU BIT0.
	 */
	{ 1, 0x2, ANY, 0, "B5010 or B5110/20 or B5210" },

	/*
	 * Vendor 4, Texas Instruments.
	 */
	{ 1, 0x4, ANY, 0, "on-chip" },		/* Viking */
	{ 1, 0x4, ANY, 4, "on-chip" },		/* Tsunami */

	/*
	 * Vendor 5, IU Matsushita0.
	 */
	{ 1, 0x5, ANY, 0, "on-chip" },

	/*
	 * Vendor 9, Weitek.
	 */
	{ 1, 0x9, ANY, 3, "on-chip" },

	{ 0 }
};

static char *
fsrtoname(impl, vers, fver, buf)
	register int impl, vers, fver;
	char *buf;
{
	register struct info *p;

	for (p = fpu_types; p->valid; p++)
		if (p->iu_impl == impl &&
		    (p->iu_vers == vers || p->iu_vers == ANY) &
		    p->fpu_vers == fver)
			return (p->name);
	sprintf(buf, "version %x", fver);
	return (buf);
}
