/*	machdep.c	4.48	81/12/09	*/

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
#include "../h/buf.h"
#include "../h/ubavar.h"
#include "../h/ubareg.h"
#include "../h/cons.h"
#include "../h/reboot.h"
#include "../h/conf.h"
#include "../h/mem.h"
#include "../h/cpu.h"
#include "../h/inode.h"
#include "../h/file.h"
#include "../h/text.h"
#include "../h/clist.h"
#include "../h/callout.h"
#include "../h/cmap.h"
#include <frame.h>
#include "../h/mbuf.h"
#include "../h/rpb.h"
#include "../h/msgbuf.h"

int	icode[] =
{
	0x9f19af9f,	/* pushab [&"init",0]; pushab */
	0x02dd09af,	/* "/etc/init"; pushl $2 */
	0xbc5c5ed0,	/* movl sp,ap; chmk */
	0x2ffe110b,	/* $exec; brb .; "/ */
	0x2f637465,	/* etc/ */
	0x74696e69,	/* init" */
	0x00000000,	/* \0\0\0";  0 */
	0x00000014,	/* [&"init", */
	0x00000000,	/* 0] */
};
int	szicode = sizeof(icode);
 
/*
 * Declare these as initialized data so we can patch them.
 */
int	nbuf = 0;
int	nswbuf = 0;

/*
 * Machine-dependent startup code
 */
startup(firstaddr)
	int firstaddr;
{
	register int unixsize;
	register unsigned i;
	register struct pte *pte;
	register caddr_t v;

	/*
	 * Initialize error message buffer (at end of core).
	 */
	maxmem -= btoc(sizeof (struct msgbuf));
	pte = msgbufmap;
	for (i = 0; i < btoc(sizeof (struct msgbuf)); i++)
		*(int *)pte++ = PG_V | PG_KW | (maxmem + i);
	mtpr(TBIA, 1);

	/*
	 * Good {morning,afternoon,evening,night}.
	 */
	printf(version);
	printf("real mem  = %d\n", ctob(maxmem));
	
	/*
	 * First determine how many buffers are reasonable.
	 * Current alg is 32 per megabyte, with min of 32.
	 * We allocate 1/2 as many swap buffer headers as file i/o buffers.
	 */
	if (nbuf == 0) {
		nbuf = (32 * physmem) / btoc(1024*1024);
		if (nbuf < 32)
			nbuf = 32;
	}
	if (nswbuf == 0) {
		nswbuf = (nbuf / 2) &~ 1;	/* force even */
		if (nswbuf > 256)
			nswbuf = 256;		/* sanity */
	}

	/*
	 * Allocate space for system data structures.
	 */
	v = (caddr_t)(0x80000000 | (firstaddr * NBPG));
#define	valloc(name, type, num) \
	    (name) = (type *)(v); (v) = (caddr_t)((name)+(num))
#define	valloclim(name, type, num, lim) \
	    (name) = (type *)(v); (v) = (caddr_t)((lim) = ((name)+(num)))
	valloc(buffers, char, BSIZE*nbuf);
	valloc(buf, struct buf, nbuf);
	valloc(swbuf, struct buf, nswbuf);
	valloc(swsize, short, nswbuf);	/* note: nswbuf is even */
	valloc(swpf, int, nswbuf);
	valloclim(inode, struct inode, ninode, inodeNINODE);
	valloclim(file, struct file, nfile, fileNFILE);
	valloclim(proc, struct proc, nproc, procNPROC);
	valloclim(text, struct text, ntext, textNTEXT);
	valloc(cfree, struct cblock, nclist);
	valloc(callout, struct callout, ncallout);
	valloc(swapmap, struct map, nswapmap = nproc * 2);
	valloc(argmap, struct map, ARGMAPSIZE);
	valloc(kernelmap, struct map, nproc);
	valloc(mbmap, struct map, nmbclusters/4);
	/*
	 * Now allocate space for core map
	 */
	ncmap = (physmem*NBPG - ((int)v &~ 0x80000000)) /
		    (NBPG*CLSIZE + sizeof (struct cmap));
	valloclim(cmap, struct cmap, ncmap, ecmap);
	if ((((int)(ecmap+1))&~0x80000000) > SYSPTSIZE*NBPG)
		panic("sys pt too small");

	/*
	 * Clear allocated space, and make r/w entries
	 * for the space in the kernel map.
	 */
	unixsize = btoc((int)(ecmap+1) &~ 0x80000000);
	if (unixsize >= physmem - 8*UPAGES)
		panic("no memory");
	pte = &Sysmap[firstaddr];
	for (i = firstaddr; i < unixsize; i++) {
		*(int *)(&Sysmap[i]) = PG_V | PG_KW | i;
		clearseg(i);
	}
	mtpr(TBIA, 1);

	/*
	 * Initialize callouts
	 */
	callfree = callout;
	for (i = 1; i < ncallout; i++)
		callout[i-1].c_next = &callout[i];

	/*
	 * Initialize memory allocator and swap
	 * and user page table maps.
	 *
	 * THE USER PAGE TABLE MAP IS CALLED ``kernelmap''
	 * WHICH IS A VERY UNDESCRIPTIVE AND INCONSISTENT NAME.
	 */
	meminit(unixsize, maxmem);
	maxmem = freemem;
	printf("avail mem = %d\n", ctob(maxmem));
	rminit(kernelmap, USRPTSIZE, 1, "usrpt", nproc);
	rminit(mbmap, nmbclusters-CLSIZE, CLSIZE, "mbclusters", nmbclusters/4);

	/*
	 * Configure the system.
	 */
	configure();

	/*
	 * Clear restart inhibit flags.
	 */
	tocons(TXDB_CWSI);
	tocons(TXDB_CCSI);
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
	 * (Unfortunately, VMS keeps local time, so when you run UNIX
	 * and VMS, VMS runs on GMT...).
	 */
	if (todr < TODRZERO) {
		printf("WARNING: todr too small");
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
	if (n == SIGILL || n == SIGFPE) {
		*usp++ = u.u_code;
		u.u_code = 0;
	} else
		*usp++ = 0;
	*usp++ = (int)p;
	*usp++ = regs[PC];
	*usp++ = regs[PS];
	regs[SP] = (int)(usp - 5);
	regs[PS] &= ~(PSL_CM|PSL_FPD);
	regs[PC] = (int)u.u_pcb.pcb_sigc;
	return;

asm("bad:");
bad:
	/*
	 * Process has trashed its stack; give it an illegal
	 * instruction to halt it in its tracks.
	 */
	u.u_signal[SIGILL] = SIG_DFL;
	u.u_procp->p_siga0 &= ~(1<<(SIGILL-1));
	u.u_procp->p_siga1 &= ~(1<<(SIGILL-1));
	psignal(u.u_procp, SIGILL);
}

dorti()
{
	struct frame frame;
	register int sp;
	register int reg, mask;
	extern int ipcreg[];

	(void) copyin((caddr_t)u.u_ar0[FP], (caddr_t)&frame, sizeof (frame));
	sp = u.u_ar0[FP] + sizeof (frame);
	u.u_ar0[PC] = frame.fr_savpc;
	u.u_ar0[FP] = frame.fr_savfp;
	u.u_ar0[AP] = frame.fr_savap;
	mask = frame.fr_mask;
	for (reg = 0; reg <= 11; reg++) {
		if (mask&1) {
			u.u_ar0[ipcreg[reg]] = fuword((caddr_t)sp);
			sp += 4;
		}
		mask >>= 1;
	}
	sp += frame.fr_spa;
	u.u_ar0[PS] = (u.u_ar0[PS] & 0xffff0000) | frame.fr_psw;
	if (frame.fr_s)
		sp += 4 + 4 * (fuword((caddr_t)sp) & 0xff);
	/* phew, now the rei */
	u.u_ar0[PC] = fuword((caddr_t)sp);
	sp += 4;
	u.u_ar0[PS] = fuword((caddr_t)sp);
	sp += 4;
	u.u_ar0[PS] |= PSL_USERSET;
	u.u_ar0[PS] &= ~PSL_USERCLR;
	u.u_ar0[SP] = (int)sp;
}

/*
 * Memenable enables the memory controlle corrected data reporting.
 * This runs at regular intervals, turning on the interrupt.
 * The interrupt is turned off, per memory controller, when error
 * reporting occurs.  Thus we report at most once per memintvl.
 */
int	memintvl = MEMINTVL;

memenable()
{
	register struct mcr *mcr;
	register int m;

	for (m = 0; m < nmcr; m++) {
		mcr = mcraddr[m];
		switch (cpu) {
#if VAX780
		case VAX_780:
			M780_ENA(mcr);
			break;
#endif
#if VAX750
		case VAX_750:
			M750_ENA(mcr);
			break;
#endif
#if VAX7ZZ
		case VAX_7ZZ:
			M7ZZ_ENA(mcr);
			break;
#endif
		}
	}
	if (memintvl > 0)
		timeout(memenable, (caddr_t)0, memintvl);
}

/*
 * Memerr is the interrupt routine for corrected read data
 * interrupts.  It looks to see which memory controllers have
 * unreported errors, reports them, and disables further
 * reporting for a time on those controller.
 */
memerr()
{
	register struct mcr *mcr;
	register int m;

	for (m = 0; m < nmcr; m++) {
		mcr = mcraddr[m];
		switch (cpu) {
#if VAX780
		case VAX_780:
			if (M780_ERR(mcr)) {
				printf("mcr%d: soft ecc addr %x syn %x\n",
				    m, M780_ADDR(mcr), M780_SYN(mcr));
				M780_INH(mcr);
			}
			break;
#endif
#if VAX750
		case VAX_750:
			if (M750_ERR(mcr)) {
				printf("mcr%d: soft ecc addr %x syn %x\n",
				    m, M750_ADDR(mcr), M750_SYN(mcr));
				M750_INH(mcr);
			}
			break;
#endif
#if VAX7ZZ
		case VAX_7ZZ:
			if (M7ZZ_ERR(mcr)) {
				struct mcr amcr;
				amcr.mc_reg[0] = mcr->mc_reg[0];
				printf("mcr%d: soft ecc addr %x syn %x\n",
				    m, M7ZZ_ADDR(&amcr), M7ZZ_SYN(&amcr));
				M7ZZ_INH(mcr);
			}
			break;
#endif
		}
	}
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
  
int	waittime = -1;

boot(paniced, arghowto)
	int paniced, arghowto;
{
	register int howto;		/* r11 == how to boot */
	register int devtype;		/* r10 == major of root dev */

#ifdef lint
	howto = 0; devtype = 0;
	printf("howto %d, devtype %d\n", howto, devtype);
#endif
	howto = arghowto;
	if ((howto&RB_NOSYNC)==0 && waittime < 0 && bfreelist[0].b_forw) {
		waittime = 0;
		update();
		printf("syncing disks... ");
		while (++waittime <= 5)
			sleep((caddr_t)&lbolt, PZERO);
		printf("done\n");
	}
	splx(0x1f);			/* extreme priority */
	devtype = major(rootdev);
	if (howto&RB_HALT) {
		printf("halting (in tight loop); hit\n\t^P\n\tHALT\n\n");
		mtpr(IPL, 0x1f);
		for (;;)
			;
	} else {
		if (paniced == RB_PANIC)
			doadump();
		tocons(TXDB_BOOT);
	}
#if defined(VAX750) || defined(VAX7ZZ)
	if (cpu != VAX_780)
		{ asm("movl r11,r5"); }		/* boot flags go in r5 */
#endif
	for (;;)
		asm("halt");
	/*NOTREACHED*/
}

tocons(c)
{

	while ((mfpr(TXCS)&TXCS_RDY) == 0)
		continue;
	mtpr(TXDB, c);
}

/*
 * Doadump comes here after turning off memory management and
 * getting on the dump stack, either when called above, or by
 * the auto-restart code.
 */
dumpsys()
{

	rpb.rp_flag = 1;
	if ((minor(dumpdev)&07) != 1)
		return;
	printf("\ndumping to dev %x, offset %d\n", dumpdev, dumplo);
	printf("dump ");
	switch ((*bdevsw[major(dumpdev)].d_dump)(dumpdev)) {

	case ENXIO:
		printf("device bad\n");
		break;

	case EFAULT:
		printf("device not ready\n");
		break;

	case EINVAL:
		printf("area improper\n");
		break;

	case EIO:
		printf("i/o error");
		break;

	default:
		printf("succeeded");
		break;
	}
}

/*
 * Machine check error recovery code.
 * Print out the machine check frame and then give up.
 */
#if defined(VAX780) || defined(VAX750)
char *mc780[] = {
	"cp read",	"ctrl str par",	"cp tbuf par",	"cp cache par",
	"cp rdtimo", 	"cp rds",	"ucode lost",	0,
	0,		0,		"ib tbuf par",	0,
	"ib rds",	"ib rd timo",	0,		"ib cache par"
};
#endif
#if VAX7ZZ
#define	NMC7ZZ	12
char *mc7ZZ[] = {
	"tb par",	"bad retry",	"bad intr id",	"cant write ptem",
	"unkn mcr err",	"iib rd err",	"nxm ref",	"cp rds",
	"unalgn ioref",	"nonlw ioref",	"bad ioaddr",	"unalgn ubaddr",
};
#endif

/*
 * Frame for each cpu
 */
struct mc780frame {
	int	mc8_bcnt;		/* byte count == 0x28 */
	int	mc8_summary;		/* summary parameter (as above) */
	int	mc8_cpues;		/* cpu error status */
	int	mc8_upc;		/* micro pc */
	int	mc8_vaviba;		/* va/viba register */
	int	mc8_dreg;		/* d register */
	int	mc8_tber0;		/* tbuf error reg 0 */
	int	mc8_tber1;		/* tbuf error reg 1 */
	int	mc8_timo;		/* timeout address divided by 4 */
	int	mc8_parity;		/* parity */
	int	mc8_sbier;		/* sbi error register */
	int	mc8_pc;			/* trapped pc */
	int	mc8_psl;		/* trapped psl */
};
struct mc750frame {
	int	mc5_bcnt;		/* byte count == 0x28 */
	int	mc5_summary;		/* summary parameter (as above) */
	int	mc5_va;			/* virtual address register */
	int	mc5_errpc;		/* error pc */
	int	mc5_mdr;
	int	mc5_svmode;		/* saved mode register */
	int	mc5_rdtimo;		/* read lock timeout */
	int	mc5_tbgpar;		/* tb group parity error register */
	int	mc5_cacherr;		/* cache error register */
	int	mc5_buserr;		/* bus error register */
	int	mc5_mcesr;		/* machine check status register */
	int	mc5_pc;			/* trapped pc */
	int	mc5_psl;		/* trapped psl */
};
struct mc7ZZframe {
	int	mc3_bcnt;		/* byte count == 0xc */
	int	mc3_summary;		/* summary parameter */
	int	mc3_parm[2];		/* parameter 1 and 2 */
	int	mc3_pc;			/* trapped pc */
	int	mc3_psl;		/* trapped psl */
};

machinecheck(cmcf)
	caddr_t cmcf;
{
	register u_int type = ((struct mc780frame *)cmcf)->mc8_summary;

	printf("machine check %x: ", type);
	switch (cpu) {
#if VAX780
	case VAX_780:
#endif
#if VAX750
	case VAX_750:
#endif
#if defined(VAX780) || defined(VAX750)
		printf("%s%s\n", mc780[type&0xf],
		    (type&0xf0) ? " abort" : " fault"); 
		break;
#endif
#if VAX7ZZ
	case VAX_7ZZ:
		if (type < NMC7ZZ)
			printf("%s", mc7ZZ[type]);
		printf("\n");
		break;
#endif
	}
	switch (cpu) {
#if VAX780
	case VAX_780: {
		register struct mc780frame *mcf = (struct mc780frame *)cmcf;
		register int sbifs;
		printf("\tcpues %x upc %x va/viba %x dreg %x tber %x %x\n",
		   mcf->mc8_cpues, mcf->mc8_upc, mcf->mc8_vaviba,
		   mcf->mc8_dreg, mcf->mc8_tber0, mcf->mc8_tber1);
		sbifs = mfpr(SBIFS);
		printf("\ttimo %x parity %x sbier %x pc %x psl %x sbifs %x\n",
		   mcf->mc8_timo*4, mcf->mc8_parity, mcf->mc8_sbier,
		   mcf->mc8_pc, mcf->mc8_psl, sbifs);
		/* THE FUNNY BITS IN THE FOLLOWING ARE FROM THE ``BLACK */
		/* BOOK AND SHOULD BE PUT IN AN ``sbi.h'' */
		mtpr(SBIFS, sbifs &~ 0x2000000);
		mtpr(SBIER, mfpr(SBIER) | 0x70c0);
		break;
	}
#endif
#if VAX750
	case VAX_750: {
		register struct mc750frame *mcf = (struct mc750frame *)cmcf;
		printf("\tva %x errpc %x mdr %x smr %x rdtimo %x tbgpar %x cacherr %x\n",
		    mcf->mc5_va, mcf->mc5_errpc, mcf->mc5_mdr, mcf->mc5_svmode,
		    mcf->mc5_rdtimo, mcf->mc5_tbgpar, mcf->mc5_cacherr);
		printf("\tbuserr %x mcesr %x pc %x psl %x mcsr %x\n",
		    mcf->mc5_buserr, mcf->mc5_mcesr, mcf->mc5_pc, mcf->mc5_psl,
		    mfpr(MCSR));
		mtpr(MCESR, 0xf);
		break;
		}
#endif
#if VAX7ZZ
	case VAX_7ZZ: {
		register struct mc7ZZframe *mcf = (struct mc7ZZframe *)cmcf;
		printf("params %x,%x pc %x psl %x mcesr %x\n",
		    mcf->mc3_parm[0], mcf->mc3_parm[1],
		    mcf->mc3_pc, mcf->mc3_psl, mfpr(MCESR));
		mtpr(MCESR, 0xf);
		break;
		}
#endif
	}
	panic("mchk");
}
