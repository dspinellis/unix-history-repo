/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ka630.c	7.6 (Berkeley) 11/8/88
 */

#ifdef VAX630
#include "param.h"
#include "time.h"
#include "kernel.h"
#include "vmmac.h"

#include "mtpr.h"
#include "cpu.h"
#include "clock.h"
#include "pte.h"
#include "ka630.h"

/*
 * 630-specific routines
 */
extern struct pte Clockmap[];
extern struct pte Ka630map[];
struct ka630clock ka630clock;
struct ka630cpu ka630cpu;

ka630_init()
{

	/*
	 * Map in the clock and the CPU.
	 */
	ioaccess((caddr_t)0x200b8000, &Clockmap[0], sizeof(struct ka630clock));
	ioaccess((caddr_t)0x20080000, &Ka630map[0], sizeof(struct ka630cpu));

	/*
	 * Clear restart and boot in progress flags in the CPMBX.
	 */
	ka630clock.cpmbx = (ka630clock.cpmbx & KA630CLK_LANG) | KA630CLK_REBOOT;

	/*
	 * Enable memory parity error detection.
	 */
	ka630cpu.ka630_mser = KA630MSER_PAREN;
}

/* Start the real-time clock */
ka630_clkstartrt()
{

	mtpr(ICCS, ICCS_IE);
}

/* init system time from tod clock */
/* ARGSUSED */
ka630_clkread(base)
	time_t base;
{
	register struct ka630clock *claddr = &ka630clock;
	struct chiptime c;

	claddr->csr1 = KA630CLK_SET;
	while ((claddr->csr0 & KA630CLK_UIP) != 0)
		;
	/* If the clock is valid, use it. */
	if ((claddr->csr3 & KA630CLK_VRT) != 0 &&
	    (claddr->csr1 & KA630CLK_ENABLE) == KA630CLK_ENABLE) {
		c.sec = claddr->sec;
		c.min = claddr->min;
		c.hour = claddr->hr;
		c.day = claddr->day;
		c.mon = claddr->mon;
		c.year = claddr->yr;
#ifndef lint
		{ int t = claddr->csr2; }	/* ??? */
#endif
		claddr->csr0 = KA630CLK_RATE;
		claddr->csr1 = KA630CLK_ENABLE;

		time.tv_sec = chiptotime(&c);
		return (time.tv_sec ? CLKREAD_OK : CLKREAD_BAD);
	}
	printf("WARNING: TOY clock invalid");
	return (CLKREAD_BAD);
}

/* Set the time of day clock, called via. stime system call.. */
ka630_clkwrite()
{
	register struct ka630clock *claddr = &ka630clock;
	struct chiptime c;
	int s;

	timetochip(&c);
	s = splhigh();
	claddr->csr1 = KA630CLK_SET;
	while ((claddr->csr0 & KA630CLK_UIP) != 0)
		;
	claddr->sec = c.sec;
	claddr->min = c.min;
	claddr->hr = c.hour;
	claddr->day = c.day;
	claddr->mon = c.mon;
	claddr->yr = c.year;
#ifndef lint
	{ int t = claddr->csr2; }	/* ??? */
	{ int t = claddr->csr3; }	/* ??? */
#endif
	claddr->csr0 = KA630CLK_RATE;
	claddr->csr1 = KA630CLK_ENABLE;
	splx(s);
}

ka630_memnop()
{

	/* void */
}

#define NMC630	10
char *mc630[] = {
	0,		"immcr (fsd)",	"immcr (ssd)",	"fpu err 0",
	"fpu err 7",	"mmu st(tb)",	"mmu st(m=0)",	"pte in p0",
	"pte in p1",	"un intr id",
};

struct mc630frame {
	int	mc63_bcnt;		/* byte count == 0xc */
	int	mc63_summary;		/* summary parameter */
	int	mc63_mrvaddr;		/* most recent vad */
	int	mc63_istate;		/* internal state */
	int	mc63_pc;		/* trapped pc */
	int	mc63_psl;		/* trapped psl */
};

ka630_mchk(cmcf)
	caddr_t cmcf;
{
	register struct ka630cpu *ka630addr = &ka630cpu;
	register struct mc630frame *mcf = (struct mc630frame *)cmcf;
	register u_int type = mcf->mc63_summary;

	printf("machine check %x", type);
	if (type < NMC630 && mc630[type])
		printf(": %s", mc630[type]);
	printf("\n\tvap %x istate %x pc %x psl %x\n",
	    mcf->mc63_mrvaddr, mcf->mc63_istate,
	    mcf->mc63_pc, mcf->mc63_psl);
	if (ka630addr->ka630_mser & KA630MSER_MERR) {
		printf("\tmser=0x%x ", ka630addr->ka630_mser);
		if (ka630addr->ka630_mser & KA630MSER_CPUER)
			printf("page=%d", ka630addr->ka630_cear);
		if (ka630addr->ka630_mser & KA630MSER_DQPE)
			printf("page=%d", ka630addr->ka630_dear);
		printf("\n");
	}
	return (MCHK_PANIC);
}
#endif
