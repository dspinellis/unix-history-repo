/*	machdep.c	4.1	11/10/80	*/

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
#include "../h/uba.h"
#include "../h/cons.h"
#include "../h/reboot.h"

char	version[] = "VM/UNIX (Berkeley Version 4.1) 11/10/80 \n";
int	icode[] =
{
	0x9f19af9f,	/* pushab [&"init.vm",0]; pushab */
	0x02dd09af,	/* "/etc/init.vm"; pushl $2 */
	0xbc5c5ed0,	/* movl sp,ap; chmk */
	0x2ffe110b,	/* $exec; brb .; "/ */
	0x2f637465,	/* etc/ */
	0x74696e69,	/* init" */
	0x00000000,	/* \0\0\0";  0 */
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
	register int i;
	register struct pte *pte;

	/*
	 * Good {morning,afternoon,evening,night}.
	 */

	tocons(TXDB_CWSI);
	printf(version);
	printf("real mem  = %d\n", ctob(maxmem));

	/*
	 * Allow for the u. area of process 0 and its (single)
	 * page of page tables.
	 */
	unixsize = (firstaddr+UPAGES+1);

	/*
	 * Initialze buffers
	 */
	pte = bufmap;
	for (i = 0; i < NBUF * CLSIZE; i++)
		*(int *)pte++ = PG_V | PG_KW | unixsize++;
	mtpr(TBIA, 1);

#ifdef ERNIE
	if (coresw)
		maxmem = 4096;
#endif

	/*
	 * Initialize maps.
	 */
	meminit(unixsize, maxmem);
	maxmem = freemem;
	printf("avail mem = %d\n", ctob(maxmem));
	mfree(kernelmap, USRPTSIZE, 1);
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
 * Initialze the clock, based on the time base which is, e.g.
 * from a filesystem.  Base provides the time to within six months,
 * and the time of year clock provides the rest.
 */
clkinit(base)
	time_t base;
{
	register unsigned todr = mfpr(TODR);
	long deltat;
	int year = YRREF;

	if (base < 5*SECYR) {
		printf("WARNING: preposterous time in file system");
		time = 6*SECYR + 186*SECDAY + SECDAY/2;
		clkset();
		goto check;
	}
	/*
	 * Have been told that VMS keeps time internally with base TODRZERO.
	 * If this is correct, then this routine and VMS should maintain
	 * the same date, and switching shouldn't be painful.
	 */
	if (todr < TODRZERO) {
		printf("WARNING: todr too small (battery backup failed?)");
		time = base;
		/*
		 * Believe the time in the file system for lack of
		 * anything better, resetting the TODR.
		 */
		clkset();
		goto check;
	}
	/*
	 * Sneak to within 6 month of the time in the filesystem,
	 * by starting with the time of the year suggested by the TODR,
	 * and advancing through succesive years.  Adding the number of
	 * seconds in the current year takes us to the end of the current year
	 * and then around into the next year to the same position.
	 */
	for (time = (todr-TODRZERO)/100; time < base-SECYR/2; time += SECYR) {
		if (LEAPYEAR(year))
			time += SECDAY;
		year++;
	}

	/*
	 * See if we gained/lost two or more days;
	 * if so, assume something is amiss.
	 */
	deltat = time - base;
	if (deltat < 0)
		deltat = -deltat;
	if (deltat < 2*SECDAY)
		return;
	printf("WARNING: clock %s %d days",
	    time < base ? "lost" : "gained", deltat / SECDAY);
check:
	printf(" -- CHECK AND RESET THE DATE!\n");
}

/*
 * Reset the TODR based on the time value; used when the TODR
 * has a preposterous value and also when the time is reset
 * by the stime system call.  Also called when the TODR goes past
 * TODRZERO + 100*(SECYEAR+2*SECDAY) (e.g. on Jan 2 just after midnight)
 * to wrap the TODR around.
 */
clkset()
{
	int year = YRREF;
	unsigned secyr;
	unsigned yrtime = time;

	/*
	 * Whittle the time down to an offset in the current year,
	 * by subtracting off whole years as long as possible.
	 */
	for (;;) {
		secyr = SECYR;
		if (LEAPYEAR(year))
			secyr += SECDAY;
		if (yrtime < secyr)
			break;
		yrtime -= secyr;
		year++;
	}
	mtpr(TODR, TODRZERO + yrtime*100);
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

#ifdef TRACE
/*
 * Put the current time into the trace,
 * in fractional seconds (i.e. 12345 means the
 * current time is ``n.12345'' for some n.
 */
ttime()
{

	trace("%d ", (lbolt*16667 + mfpr(ICR)));
}
#endif

/*
 * Send an interrupt to process
 *
 * SHOULD CHANGE THIS TO PASS ONE MORE WORK SO THAT ALL INFORMATION
 * PROVIDED BY HARDWARE IS AVAILABLE TO THE USER PROCESS.
 */
sendsig(p, n)
	int (*p)();
{
	register int *usp, *regs;

	regs = u.u_ar0;
	usp = (int *)regs[SP];
#ifdef FASTVAX
	usp -= 5;
	if ((int)usp <= USRSTACK - ctob(u.u_ssize))
		(void) grow((unsigned)usp);
	;			/* Avoid asm() label botch */
#ifndef lint
	asm("probew $3,$20,(r11)");
	asm("beql bad");
#else
	if (useracc((caddr_t)usp, 0x20, 1))
		goto bad;
#endif
	*usp++ = n;
	*usp++ = n == SIGILL ? u.u_cfcode : 0;
	*usp++ = (int)p;
	*usp++ = regs[PC];
	*usp++ = regs[PS];
	regs[SP] = (int)(usp - 5);
#else
	(void) grow((unsigned)(usp-5));
	if (suword((caddr_t)--usp, regs[PS]))
		goto bad;
	if (suword((caddr_t)--usp, regs[PC]))
		goto bad;
	if (suword((caddr_t)--usp, (int)p))
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

dorti()
{
	struct frame {
		int	handler;
		unsigned int
			psw:16,
			mask:12,
			:1,
			s:1,
			spa:2;
		int	savap;
		int	savfp;
		int	savpc;
	} frame;
	register int sp;
	register int reg, mask;
	extern int ipcreg[];

	(void) copyin((caddr_t)u.u_ar0[FP], (caddr_t)&frame, sizeof (frame));
	sp = u.u_ar0[FP] + sizeof (frame);
	u.u_ar0[PC] = frame.savpc;
	u.u_ar0[FP] = frame.savfp;
	u.u_ar0[AP] = frame.savap;
	mask = frame.mask;
	for (reg = 0; reg <= 11; reg++) {
		if (mask&1) {
			u.u_ar0[ipcreg[reg]] = fuword((caddr_t)sp);
			sp += 4;
		}
		mask >>= 1;
	}
	sp += frame.spa;
	u.u_ar0[PS] = (u.u_ar0[PS] & 0xffff0000) | frame.psw;
	if (frame.s)
		sp += 4 + 4 * (fuword((caddr_t)sp) & 0xff);
	/* phew, now the rei */
	u.u_ar0[PC] = fuword((caddr_t)sp);
	sp += 4;
	u.u_ar0[PS] = fuword((caddr_t)sp);
	sp += 4;
	u.u_ar0[PS] |= PSL_CURMOD|PSL_PRVMOD;
	u.u_ar0[PS] &= ~PSL_USERCLR;
	u.u_ar0[SP] = (int)sp;
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
		printf("MEMERR: mcra %X mcrb %X mcrc %X\n", mcr[0],
		    mcr[1], c);
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
  
int	hangcnt;

unhang()
{
	register struct uba_regs *up = (struct uba_regs *)UBA0;

	if (up->uba_sr == 0)
		return;
	hangcnt++;
	if (hangcnt > 5*HZ) {
		hangcnt = 0;
		printf("HANG ");
		ubareset();
	}
}

int	waittime = -1;

boot(panic, arghowto)
	int panic, arghowto;
{
	register int howto;		/* r11 == how to boot */
	register int devtype;		/* r10 == major of root dev */

	howto = arghowto;
	if ((howto&RB_NOSYNC)==0 && waittime < 0) {
		waittime = 0;
		update();
		printf("syncing disks... ");
		while (++waittime <= 5)
			sleep((caddr_t)&lbolt, PZERO);
		printf("done\n");
	}
	splx(0x1f);			/* extreme priority */
	devtype = major(rootdev);
	if ((howto&RB_HALT))
		tocons(TXDB_WSI);
	else if (panic == RB_PANIC)
		;			/* sent TXDB_CWSI at boot */
	else {
		tocons(TXDB_WSI);
		tocons(TXDB_BOOT);	/* defboo.cmd, not restar.cmd */
	}
	for (;;)
		asm("halt");
#ifdef lint
	printf("howto %d, devtype %d\n", howto, devtype);
#endif
	/*NOTREACHED*/
}

tocons(c)
{

	while ((mfpr(TXCS)&TXCS_RDY) == 0)
		continue;
	mtpr(TXDB, c);
}
