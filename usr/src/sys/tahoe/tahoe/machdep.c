/*	machdep.c	1.4	85/05/15	*/

#include "../machine/reg.h"
#include "../machine/pte.h"
#include "../machine/psl.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/nami.h"
#include "../h/kernel.h"
#include "../h/map.h"
#include "../h/vm.h"
#include "../h/proc.h"
#include "../h/buf.h"
#include "../h/reboot.h"
#include "../h/conf.h"
#include "../h/inode.h"
#include "../h/file.h"
#include "../h/text.h"
#include "../h/clist.h"
#include "../h/callout.h"
#include "../h/cmap.h"
#include "../h/mbuf.h"
#include "../h/msgbuf.h"
#include "../h/quota.h"
#include "../tahoe/mem.h"
#include "../tahoe/mtpr.h"
#include "../tahoe/cp.h"
#include "../vba/vbavar.h"

int	icode[] = {
	0xf9af19f9,	/*	pushab	argp */
	0xaf09dd02,	/*	pushab	file */
	0xe96e5dcf,	/*	pushl	$2 */
	0x0b11fe2f,	/*	movab	(sp),fp */
	0x6574632f,	/*	kcall	$exec */
	0x696e6974,	/* here:brb	here */
	0x00000000,	/*.file:.ascii	"/etc/" */
	0x00000014,	/* args:.ascii	"init\0" */
	0x00000000,	/*	.align	2 */
			/* argp:.long	args, 0 */
};
int	szicode = sizeof(icode);
 
/*
 * Declare these as initialized data so we can patch them.
 */
int	nbuf = 0;
int	nswbuf = 0;
int	bufpages = 0;

/*
 * Machine-dependent startup code
 */
startup(firstaddr)
	int firstaddr;
{
	register int unixsize;
	register unsigned i;
	register struct pte *pte;
	register int mapaddr, j;
	register caddr_t v;
	register int maxbufs, base, residual;
	extern char etext;

	/*
	 * Initialize error message buffer (at end of core).
	 */
	maxmem -= btoc(sizeof (struct msgbuf));
	pte = msgbufmap;
	for (i = 0; i < btoc(sizeof (struct msgbuf)); i++)
		*(int *)pte++ = PG_V | PG_KW | (maxmem + i);
	mtpr(1,TBIA);

	/*
	 * Good {morning,afternoon,evening,night}.
	 */
	printf(version);
	printf("real mem  = %d\n", ctob(maxmem));
	
	/*
	 * Determine how many buffers to allocate.
	 * Use 10% of memory, with min of 16.
	 * We allocate 1/2 as many swap buffer headers as file i/o buffers.
	 */
	maxbufs = ((SYSPTSIZE * NBPG) - (5 * (int)(&etext - 0xc0000000))) /
	    MAXBSIZE;
	if (bufpages == 0)
		bufpages = (physmem * NBPG) / 10 / CLBYTES;
	if (nbuf == 0) {
		nbuf = bufpages / 2;
		if (nbuf < 16)
			nbuf = 16;
		if (nbuf > maxbufs)
			nbuf = maxbufs;
	}
	if (bufpages > nbuf * (MAXBSIZE / CLBYTES))
		bufpages = nbuf * (MAXBSIZE / CLBYTES);
	if (nswbuf == 0) {
		nswbuf = (nbuf / 2) &~ 1;	/* force even */
		if (nswbuf > 256)
			nswbuf = 256;		/* sanity */
	}

	/*
	 * Allocate space for system data structures.
	 * The first available real memory address is in "firstaddr".
	 * As pages of memory are allocated, "firstaddr" is incremented.
	 * The first available kernel virtual address is in "v".
	 * As pages of kernel virtual memory are allocated, "v" is incremented.
	 * An index into the kernel page table corresponding to the
	 * virtual memory address maintained in "v" is kept in "mapaddr".
	 */
	mapaddr = firstaddr;
	v = (caddr_t)(0xc0000000 | (firstaddr * NBPG));
#define	valloc(name, type, num) \
	    (name) = (type *)(v); (v) = (caddr_t)((name)+(num))
#define	valloclim(name, type, num, lim) \
	    (name) = (type *)(v); (v) = (caddr_t)((lim) = ((name)+(num)))
	valloc(buffers, char, MAXBSIZE * nbuf);
	base = bufpages / nbuf;
	residual = bufpages % nbuf;
	for (i = 0; i < residual; i++) {
		for (j = 0; j < (base + 1) * CLSIZE; j++) {
			*(int *)(&Sysmap[mapaddr+j]) = PG_V | PG_KW | firstaddr;
			clearseg((unsigned)firstaddr);
			firstaddr++;
		}
		mapaddr += MAXBSIZE / NBPG;
	}
	for (i = residual; i < nbuf; i++) {
		for (j = 0; j < base * CLSIZE; j++) {
			*(int *)(&Sysmap[mapaddr+j]) = PG_V | PG_KW | firstaddr;
			clearseg((unsigned)firstaddr);
			firstaddr++;
		}
		mapaddr += MAXBSIZE / NBPG;
	}
	valloc(buf, struct buf, nbuf);
	valloc(swbuf, struct buf, nswbuf);
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
	valloc(nch, struct nch, nchsize);
#ifdef QUOTA
	valloclim(quota, struct quota, nquota, quotaNQUOTA);
	valloclim(dquot, struct dquot, ndquot, dquotNDQUOT);
#endif
	/*
	 * Now allocate space for core map
	 * Allow space for all of phsical memory minus the amount 
	 * dedicated to the system. The amount of physical memory
	 * dedicated to the system is the total virtual memory of
	 * the system minus the space in the buffers which is not
	 * allocated real memory.
	 */
	ncmap = (physmem*NBPG - ((int)v &~ 0xc0000000) +
		(nbuf * MAXBSIZE - bufpages * CLBYTES)) /
		    (NBPG*CLSIZE + sizeof (struct cmap));
	valloclim(cmap, struct cmap, ncmap, ecmap);
	if ((((int)(ecmap+1))&~0xc0000000) > SYSPTSIZE*NBPG)
		panic("sys pt too small");

	/*
	 * Clear allocated space, and make r/w entries
	 * for the space in the kernel map.
	 */
	unixsize = btoc((int)(ecmap+1) &~ 0xc0000000);
	for (i = mapaddr; i < unixsize; i++) {
		*(int *)(&Sysmap[i]) = PG_V | PG_KW | firstaddr;
		clearseg((unsigned)firstaddr);
		firstaddr++;
	}
	if (firstaddr >= physmem - 8*UPAGES)
		panic("no memory");
	mtpr(1,TBIA);

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
	meminit(firstaddr, maxmem);
	maxmem = freemem;
	printf("avail mem = %d\n", ctob(maxmem));
	printf("using %d buffers containing %d bytes of memory\n",
		nbuf, bufpages * CLBYTES);
	rminit(kernelmap, (long)USRPTSIZE, (long)1,
	    "usrpt", nproc);
	rminit(mbmap, (long)((nmbclusters - 1) * CLSIZE), (long)CLSIZE,
	    "mbclusters", nmbclusters/4);
	intenable = 1;		/* Enable interrupts from now on */
	/*
	 * Configure the system.
	 */
	configure();
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
	return(((time.tv_sec-otime)*60 + lbolt-olbolt)*16667);
}
#endif

/*
 * Send an interrupt to process.
 *
 * Stack is set up to allow sigcode stored
 * in u. to call routine, followed by chmk
 * to sigcleanup routine below.  After sigcleanup
 * resets the signal mask and the stack, it
 * returns to user who then unwinds with the
 * rei at the bottom of sigcode.
 */
sendsig(p, sig, mask)
	int (*p)(), sig, mask;
{
	register struct sigcontext *scp;	/* know to be r12 */
	register int *regs;
	register struct sigframe {
		int	sf_signum;
		int	sf_code;
		struct	sigcontext *sf_scp;
		int	(*sf_handler)();
		int	r1;
		int 	r0;
		struct	sigcontext *sf_scpcopy;
	} *fp;					/* known to be r10 */
	int oonstack;

	regs = u.u_ar0;
	oonstack = u.u_onstack;
	scp = (struct sigcontext *)regs[SP] - 1;
	if (!u.u_onstack && (u.u_sigonstack & sigmask(sig))) {
		fp = (struct sigframe *)u.u_sigsp - 1;
		u.u_onstack = 1;
	} else
		fp = (struct sigframe *)scp - 1;
	/*
	 * Must build signal handler context on stack to be returned to
	 * so that rei instruction in sigcode will pop ps and pc
	 * off correct stack.  The remainder of the signal state
	 * used in calling the handler must be placed on the stack
	 * on which the handler is to operate so that the calls
	 * in sigcode will save the registers and such correctly.
	 */
	if (!oonstack && (int)fp <= USRSTACK - ctob(u.u_ssize)) 
		grow((unsigned)fp);
	;
#ifndef lint
	asm("probew $1,(r10),$7*4");
	asm("jeql bad");
#else
	if (useracc((caddr_t)fp, sizeof (struct sigframe), 1))
		goto bad;
#endif
	if (!u.u_onstack && (int)scp <= USRSTACK - ctob(u.u_ssize))
		grow((unsigned)scp);
	;			/* Avoid asm() label botch */
#ifndef lint
	asm("probew $1,(r12),$5*4");
	asm("beql bad");
#else
	if (useracc((caddr_t)scp, sizeof (struct sigcontext), 1))
		goto bad;
#endif
	fp->sf_signum = sig;
	if (sig == SIGILL || sig == SIGFPE) {
		fp->sf_code = u.u_code;
		u.u_code = 0;
	} else
		fp->sf_code = 0;
	fp->sf_scp = scp;
	fp->sf_handler = p;
	fp->r1 = regs[R1];	/* These are not saved by the C compiler */
	fp->r0 = regs[R0];
	/*
	 * Duplicate the pointer to the sigcontext structure.
	 * This one doesn't get popped by the ret, and is used 
	 * by sigcleanup to reset the signal state on inward return.
	 */
	fp->sf_scpcopy = scp;
	/* sigcontext goes on previous stack */
	scp->sc_onstack = oonstack;
	scp->sc_mask = mask;
	/* setup rei */
	scp->sc_sp = (int)&scp->sc_pc;
	scp->sc_pc = regs[PC];
	scp->sc_ps = regs[PS];
	regs[SP] = (int)fp;
	regs[PC] = (int)u.u_pcb.pcb_sigc;
	return;

asm("bad:");
bad:
	/*
	 * Process has trashed its stack; give it an illegal
	 * instruction to halt it in its tracks.
	 */
	u.u_signal[SIGILL] = SIG_DFL;
	sig = sigmask(SIGILL);
	u.u_procp->p_sigignore &= ~sig;
	u.u_procp->p_sigcatch &= ~sig;
	u.u_procp->p_sigmask &= ~sig;
	psignal(u.u_procp, SIGILL);
}

/*
 * Routine to cleanup state after a signal
 * has been taken.  Reset signal mask and
 * stack state from context left by sendsig (above).
 * Pop these values in preparation for rei which
 * follows return from this routine.
 */
sigcleanup()
{
	register struct sigcontext *scp;	/* known as R12 */

	scp = (struct sigcontext *)fuword((caddr_t)u.u_ar0[SP]);
	if ((int)scp == -1)
		return;
#ifndef lint
	;			/* Avoid asm() label botch */
	/* only probe 12 here because that's all we need */
	asm("prober $1,(r12),$12");
	asm("bnequ 1f; ret; 1:");
#else
	if (useracc((caddr_t)scp, sizeof (*scp), 0))
		return;
#endif
	u.u_onstack = scp->sc_onstack & 01;
	u.u_procp->p_sigmask =
	   scp->sc_mask &~ (sigmask(SIGKILL)|sigmask(SIGCONT)|sigmask(SIGSTOP));
	u.u_ar0[SP] = scp->sc_sp;
}

int	waittime = -1;

boot(paniced, arghowto)
	int paniced, arghowto;
{
	register long dummy;
	register int howto;		/* r11 == how to boot */
	register int devtype;		/* r10 == major of root dev */

#ifdef lint
	howto = 0; devtype = 0;
	printf("howto %d, devtype %d\n", arghowto, devtype);
#endif
	(void) spl1();
	howto = arghowto;
	if ((howto&RB_NOSYNC)==0 && waittime < 0 && bfreelist[0].b_forw) {
		waittime = 0;
		update();
		printf("syncing disks... ");
#ifdef notdef
		DELAY(10000000);
#else
		{ register struct buf *bp;
		  int iter, nbusy, oldnbusy;

		  printf ("\tBlocks to sync : ");
		  oldnbusy = 0;
		  for (iter = 0; /* iter < 20 */; iter++) {
			DELAY(1000);
			nbusy = 0;
			for (bp = &buf[nbuf]; --bp >= buf; )
				if ((bp->b_flags & (B_BUSY|B_INVAL)) == B_BUSY)
					nbusy++;
			if (nbusy == 0)
				break;
			if (nbusy != oldnbusy) {
				printf("%d ", nbusy);
				oldnbusy = nbusy;
			}
			if (iter >= 100) {
				printf (" - disk I/O stopped (?), giving up\n");
				DELAY(10000);
				break;
			}
		  }
		}
#endif
		printf("done\n\n");
	}
	splx(0x1f);			/* extreme priority */
	devtype = major(rootdev);
	if (howto&RB_HALT) {
		printf("halting (in tight loop); hit ~h\n\n");
		mtpr(0x1f,IPL);
		for (;;)
			;
	} else {
		if (paniced == RB_PANIC) {
			doadump();		/* TXDB_BOOT's itsself */
			/*NOTREACHED*/
		}
		tocons(CPBOOT);
	}
	for (;;)
		asm("halt");
	/*NOTREACHED*/
}



/*
 * Send the given comand ('c') to the console processor.
 * Assumed to be one of the last things the OS does before
 *  halting or rebooting.
 */

struct cphdr *lasthdr;	/* Available in "dev/cons.c" */

struct	cpdcb_o cpcontrol;

tocons(command)
int command;
{

	int timeout;

	cpcontrol.cp_hdr.cp_unit = CPUNIT;
	cpcontrol.cp_hdr.cp_comm =  (char) command;
	if (command != CPBOOT) 
		cpcontrol.cp_hdr.cp_count = 1;	/* Just for sanity */
	else {
		cpcontrol.cp_hdr.cp_count = 4;
		*(int *)cpcontrol.cp_buf = 0;	/* r11 value for reboot */
	}
	timeout = 100000;				/* Delay loop */
	while (timeout-- && !(lasthdr->cp_unit & CPDONE))
		uncache(&lasthdr->cp_unit);
					/* Give up, force it to listen */
	mtpr ( vtoph(0, &cpcontrol), CPMDCB);
}

/*
 * Invalidate single all pte's in a cluster
 */
tbiscl(v)
	unsigned v;
{
	register caddr_t addr;		/* must be first reg var */
	register int i;

	addr = ptob(v);
	for (i = 0; i < CLSIZE; i++) {
		mtpr(addr, TBIS);
		addr += NBPG;
	}
}

int	dumpmag = 0x8fca0101;	/* magic number for savecore */
int	dumpsize = 0;		/* also for savecore */
/*
 * Doadump comes here after turning off memory management and
 * getting on the dump stack, either when called above, or by
 * the auto-restart code.
 */
dumpsys()
{

#ifdef notdef
	if ((minor(dumpdev)&07) != 1)
		return;
#endif
	dumpsize = physmem;
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
		printf("i/o error\n");
		break;

	default:
		printf("succeeded\n");
		break;
	}
	printf("Rebooting the system ...\n\n");
	tocons(CPBOOT);
}


/*
 * Bus error 'recovery' code.
 * Print out the buss frame and then give up.
 * (More information from special registers can be printed here.)
 * 
 */

/*
 * Frame for bus error
 */
struct buserframe {
	int	which_bus;		/* primary or secondary */
	int	memerreg;		/* memory error register */
	int	trp_pc;			/* trapped pc */
	int	trp_psl;		/* trapped psl */
};

buserror(busef)
	caddr_t busef;
{
	register struct buserframe *frameptr;
	register long 	hardreg;


	frameptr = (struct buserframe *)busef;
	printf("bus error at address %x, psl = %x\n",
		frameptr->trp_pc,frameptr->trp_psl);
	hardreg =  frameptr->memerreg;
	printf("\tMEAR = %x\n",((hardreg&MEAR)>>16)&0xffff);
	switch (hardreg & ERRCD){
		case (APE): 	printf("\tAdress parity error.Should not reach this point!! \n");
				break;
		case (DPE): 	printf("\tData parity error.\n");
				break;
		case (DCE):	printf("\tData check error.\n");
				break;
		case (VTO):	printf("\tVersabus timeout.\n");
				break;
		case (VBE):	printf("\tVersabus error.Should not reach this point!! \n");
				break;
		case (NEM):	printf("\tNon existent memory.\n");
				break;
		default:	printf("\tUnknown error code: %x\n",
					hardreg&ERRCD);
	}
	if (hardreg&AXE) printf("\tAdapter external error\n");
	printf ("\tError master : ");
	if (hardreg&ERM) printf("Versabus\n");
	else printf ("Tahoe\n");
	if (hardreg&IVV)
		printf("\tIllegal interrupt vector, from ipl %d\n",
		    (hardreg >> 2) & 7);

	hardreg = frameptr->which_bus;
	printf("\tMCBR = %x\n", ((hardreg&MCBR)>>16)&0xffff);
	printf("\tVersabus type : %x\n", hardreg&0xffc3);
	if (frameptr->memerreg&IVV) return;
	panic("buserror");
}


physstrat(bp, strat, prio)
	struct buf *bp;
	int (*strat)(), prio;
{
	int s;

	(*strat)(bp);
	/* pageout daemon doesn't wait for pushed pages */
	if (bp->b_flags & B_DIRTY)
		return;
	s = spl8();
	while ((bp->b_flags & B_DONE) == 0)
		sleep((caddr_t)bp, prio);
	splx(s);
}


/*ARGSUSED*/
mtpr (value, regno)
{
	asm("mtpr 4(fp), 8(fp)");
}

/*ARGSUSED*/
int
mfpr (regno)
{
	asm("mfpr 4(fp),r0");
#ifdef lint
	return(0);
#endif
}

