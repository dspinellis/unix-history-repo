/*	machdep.c	3.5	%H%	*/

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
#include "../h/proc.h"
#include "../h/psl.h"

char	version[] = "VM/UNIX (Berkeley Version 3.1) 4/2/80 \n";
int	icode[] =
{
	0x9f19af9f,	/* pushab [&"init.vm",0]; pushab */
	0x02dd09af,	/* "/etc/init.vm"; pushl $2 */
	0xbc5c5ed0,	/* movl sp,ap; chmk */
	0x2ffe110b,	/* $exec; brb .; "/ */
	0x2f637465,	/* etc/ */
	0x74696e69,	/* init */
	0x00000000,	/* \0\0\0";  0 */	/* was .vm" */
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
	unixsize = (firstaddr+UPAGES+1);
	if (coresw)
		maxmem = 4096;

	/*
	 * Initialize maps.
	 */
	meminit(unixsize, maxmem);
	maxmem -= (unixsize+1);
	printf("avail mem = %d\n", ctob(maxmem));
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

#ifdef PGINPROF
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

	regs = u.u_ar0;
	usp = (int *)regs[SP];
#ifdef FASTVAX
	usp -= 5;
	if ((int)usp <= USRSTACK - ctob(u.u_ssize))
		(void) grow((unsigned)usp);
	;			/* Avoid asm() label botch */
	asm("probew $3,$20,(r11)");
	asm("beql bad");
	*usp++ = n;
	*usp++ = n == SIGILL ? u.u_cfcode : 0;
	*usp++ = p;
	*usp++ = regs[PC];
	*usp++ = regs[PS];
	regs[SP] = (int)(usp - 5);
#else
	(void) grow((unsigned)(usp-5));
	if (suword((caddr_t)--usp, regs[PS]))
		goto bad;
	if (suword((caddr_t)--usp, regs[PC]))
		goto bad;
	if (suword((caddr_t)--usp, p))
		goto bad;
	if (suword((caddr_t)--usp, n==SIGILL ? u.u_cfcode : 0))
		goto bad;
	if (suword((caddr_t)--usp, n))
		goto bad;
	regs[SP] = (int)usp;
#endif
	regs[PS] &= ~(PSL_CM|PSL_FPD);
	regs[PC] = (int)u.u_pcb.pcb_sigc;
	return;

#ifdef FASTVAX
asm("bad:");
#endif
bad:
	printf("%d: cant send signal\n", u.u_procp->p_pid);
	psignal(u.u_procp, SIGKILL);
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
