/*	machdep.c	2.7	2/10/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/reg.h"
#include "../h/mtpr.h"
#include "../h/clock.h"
#include "../h/pte.h"
#include "../h/vm.h"

char	version[] = "VM/UNIX (Berkeley Version 2.7) 2/10/80 \n";
int	icode[] =
{
	0x9f19af9f,	/* pushab [&"init.vm",0]; pushab */
	0x02dd09af,	/* "/etc/init.vm"; pushl $2 */
	0xbc5c5ed0,	/* movl sp,ap; chmk */
	0x2ffe110b,	/* $exec; brb .; "/ */
	0x2f637465,	/* etc/ */
	0x74696e69,	/* init */
	0x006d762e,	/* .vm";  0 */
	0x00000014,	/* [&"init", */
	0x00000000,	/* 0] */
};
int	szicode = sizeof(icode);
int	memchk();
 
/*
 * Machine-dependent startup code
 */
startup(firstaddr)
{
	register int unixsize;

	/*
	 * Good {morning,afternoon,evening,night}.
	 */

	printf(version);
	printf("real mem  = %d\n", ctob(maxmem));

	/*
	 * Allow for the u. area of process 0.
	 */
	unixsize = (firstaddr+UPAGES);
	if (coresw)
		maxmem = 1024;

	/*
	 * Initialize maps.
	 */
	meminit(unixsize, maxmem);
	maxmem -= unixsize;
	printf("avail mem = %d\n", maxmem*ctob(1));
	mfree(swapmap, nswap - CLSIZE, CLSIZE);
	mfree(kernelmap, USRPTSIZE, 1);
	swplo--;
	mbainit();
	ubainit();
	timeout(memchk, (caddr_t)0, 60);	/* it will pick its own intvl */
}

/*
 * set up a physical address
 * into users virtual address space.
 */
sysphys()
{

	if(!suser())
		return;
	u.u_error = EINVAL;
}

/*
 * Start clock
 */
clkstart()
{

	mtpr(NICR, -16667);	/* 16.667 milli-seconds */
	mtpr(ICCS, ICCS_RUN+ICCS_IE+ICCS_TRANS+ICCS_INT+ICCS_ERR);
}

clkreld()
{

	mtpr(ICCS, ICCS_RUN+ICCS_IE+ICCS_INT+ICCS_ERR);
}

#ifdef ERNIE
/*
 * Return the difference (in microseconds)
 * between the  current time and a previous
 * time as represented  by the arguments.
 * If there is a pending clock interrupt
 * which has not been serviced due to high
 * ipl, return error code.
 */
vmtime(otime, olbolt, oicr)
	register int otime, olbolt, oicr;
{

	if (mfpr(ICCS)&ICCS_INT)
		return(-1);
	else
		return(((time-otime)*60 + lbolt-olbolt)*16667 + mfpr(ICR)-oicr);
}
#endif

/*
 * Send an interrupt to process
 */
sendsig(p, n)
{
	register int *usp, *regs;
	register int mask, r, spa, t;
	int *s;

	regs = u.u_ar0;
	usp = (int *)regs[SP];
	/* WHAT IF WE CAN'T GROW? */
	VOID grow((unsigned)(usp-20));

	/* get register save mask (save r0-r5) */
	mask = (fuword((caddr_t)p) & 0xfff) | 0x3f;

	VOID suword((caddr_t)--usp, n);	/* sig # as param */
	VOID suword((caddr_t)--usp, 1);	/* one parameters */
	s = usp;
	spa = ((int) usp) & 0x3;
	usp = (int *)((int)usp &~ 0x3);
	t = 11;
	for (r=0x800; r; r>>=1) {
		if (mask & r)
			VOID suword((caddr_t) --usp, regs[t]);
		t--;
	}
	VOID suword((caddr_t)--usp, regs[PC]);
	VOID suword((caddr_t)--usp, regs[FP]);
	VOID suword((caddr_t)--usp, regs[AP]);
	VOID suword((caddr_t)--usp, (spa << 30) | (0x2 << 28)
				| (mask << 16) | (regs[PS] & 0xfff1));
	VOID suword((caddr_t)--usp, 0);

	regs[SP] = (int)usp;
	regs[FP] = (int)usp;
	regs[AP] = (int)s;
	regs[PC] = p + 2;
	regs[PS] &=  ~0x1f;
}

/*ARGSUSED*/
/*VARARGS1*/
mtpr(regno, value)
{

	asm("	mtpr	8(ap),4(ap)");
}

/*ARGSUSED*/
mfpr(regno)
{

	asm("	mfpr	4(ap),r0");
#ifdef lint
	return (0);
#endif
}

/*
 * Copy bytes within kernel
 */
/*ARGSUSED*/
bcopy(from, to, count)
	caddr_t from, to;
	unsigned count;
{

	asm("	movc3	12(ap),*4(ap),*8(ap)");
}

/*
 * Check memory controller for memory parity errors
 */
#define	MEMINTVL	(60*60*10)		/* 10 minutes */
int	memintvl = MEMINTVL;

#define	MHIERR	0x20000000
#define	MERLOG	0x10000000

memchk()
{
	register int c = mcr[2];

	if (c & MERLOG) {
		printf("MEMERR: %X\n", c);
		mcr[2] = (MERLOG|MHIERR);
	}
	if (memintvl > 0)
		timeout(memchk, (caddr_t)0, memintvl);
}

/*
 * Invalidate single all pte's in a cluster
 */
tbiscl(v)
	unsigned v;
{
	register caddr_t addr;		/* must be first reg var */
	register int i;

	asm(".set TBIS,58");
	addr = ptob(v);
	for (i = 0; i < CLSIZE; i++) {
#ifdef lint
		mtpr(TBIS, addr);
#else
		asm("mtpr r11,$TBIS");
#endif
		addr += NBPG;
	}
}
