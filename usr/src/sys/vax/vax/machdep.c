/*	machdep.c	4.75	83/02/11	*/

#include "../machine/reg.h"
#include "../machine/pte.h"
#include "../machine/psl.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
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

#include "../vax/frame.h"
#include "../vax/cons.h"
#include "../vax/cpu.h"
#include "../vax/mem.h"
#include "../vax/mtpr.h"
#include "../vax/rpb.h"
#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"

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

#ifdef MUSH
int	mcode[] =
{
	0x9f19af9f,	/* pushab [&"mush",0]; pushab */
	0x02dd09af,	/* "/etc/mush"; pushl $2 */
	0xbc5c5ed0,	/* movl sp,ap; chmk */
	0x2f01bc0b,	/* $exec; chmk $exit; "/ */
	0x2f637465,	/* etc/ */
	0x6873756d,	/* mush" */
	0x00000000,	/* \0\0\0";  0 */
	0x00000014,	/* [&"mush", */
	0x00000000,	/* 0] */
};
int	szmcode = sizeof(mcode);
#endif
 
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
	int mapaddr, j;
	register caddr_t v;
	int maxbufs, base, residual;
	extern char etext;

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
	 * Determine how many buffers to allocate.
	 * Use 10% of memory, with min of 16.
	 * We allocate 1/2 as many swap buffer headers as file i/o buffers.
	 */
	maxbufs = ((SYSPTSIZE * NBPG) - (5 * (int)(&etext - 0x80000000))) /
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
	v = (caddr_t)(0x80000000 | (firstaddr * NBPG));
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
	ncmap = (physmem*NBPG - ((int)v &~ 0x80000000) +
		(nbuf * (MAXBSIZE - 2 * CLBYTES))) /
		    (NBPG*CLSIZE + sizeof (struct cmap));
	valloclim(cmap, struct cmap, ncmap, ecmap);
	if ((((int)(ecmap+1))&~0x80000000) > SYSPTSIZE*NBPG)
		panic("sys pt too small");

	/*
	 * Clear allocated space, and make r/w entries
	 * for the space in the kernel map.
	 */
	unixsize = btoc((int)(ecmap+1) &~ 0x80000000);
	for (i = mapaddr; i < unixsize; i++) {
		*(int *)(&Sysmap[i]) = PG_V | PG_KW | firstaddr;
		clearseg((unsigned)firstaddr);
		firstaddr++;
	}
	if (firstaddr >= physmem - 8*UPAGES)
		panic("no memory");
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
	meminit(firstaddr, maxmem);
	maxmem = freemem;
	printf("avail mem = %d\n", ctob(maxmem));
	printf("using %d buffers containing %d bytes of memory\n",
		nbuf, bufpages * CLBYTES);
	rminit(kernelmap, (long)USRPTSIZE, (long)1,
	    "usrpt", nproc);
	rminit(mbmap, (long)((nmbclusters - 1) * CLSIZE), (long)CLSIZE,
	    "mbclusters", nmbclusters/4);

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
		return(((time.tv_sec-otime)*60 + lbolt-olbolt)*16667 + mfpr(ICR)-oicr);
}
#endif

/*
 * Send an interrupt to process
 *
 * SHOULD CHANGE THIS TO PASS ONE MORE WORD SO THAT ALL INFORMATION
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
#if VAX730
		case VAX_730:
			M730_ENA(mcr);
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
#ifdef TRENDATA
				memlog(m, mcr);
#endif
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
#if VAX730
		case VAX_730:
			if (M730_ERR(mcr)) {
				struct mcr amcr;
				amcr.mc_reg[0] = mcr->mc_reg[0];
				printf("mcr%d: soft ecc addr %x syn %x\n",
				    m, M730_ADDR(&amcr), M730_SYN(&amcr));
				M730_INH(mcr);
			}
			break;
#endif
		}
	}
}

#ifdef TRENDATA
/*
 * Figure out what chip to replace on Trendata boards.
 * Assumes all your memory is Trendata or the non-Trendata
 * memory never fails..
 */
struct {
	u_char	m_syndrome;
	char	m_chip[4];
} memlogtab[] = {
	0x01,	"C00",	0x02,	"C01",	0x04,	"C02",	0x08,	"C03",
	0x10,	"C04",	0x19,	"L01",	0x1A,	"L02",	0x1C,	"L04",
	0x1F,	"L07",	0x20,	"C05",	0x38,	"L00",	0x3B,	"L03",
	0x3D,	"L05",	0x3E,	"L06",	0x40,	"C06",	0x49,	"L09",
	0x4A,	"L10",	0x4c,	"L12",	0x4F,	"L15",	0x51,	"L17",
	0x52,	"L18",	0x54,	"L20",	0x57,	"L23",	0x58,	"L24",
	0x5B,	"L27",	0x5D,	"L29",	0x5E,	"L30",	0x68,	"L08",
	0x6B,	"L11",	0x6D,	"L13",	0x6E,	"L14",	0x70,	"L16",
	0x73,	"L19",	0x75,	"L21",	0x76,	"L22",	0x79,	"L25",
	0x7A,	"L26",	0x7C,	"L28",	0x7F,	"L31",	0x80,	"C07",
	0x89,	"U01",	0x8A,	"U02",	0x8C,	"U04",	0x8F,	"U07",
	0x91,	"U09",	0x92,	"U10",	0x94,	"U12",	0x97, 	"U15",
	0x98,	"U16",	0x9B,	"U19",	0x9D,	"U21",	0x9E, 	"U22",
	0xA8,	"U00",	0xAB,	"U03",	0xAD,	"U05",	0xAE,	"U06",
	0xB0,	"U08",	0xB3,	"U11",	0xB5,	"U13",	0xB6,	"U14",
	0xB9,	"U17",	0xBA,	"U18",	0xBC,	"U20",	0xBF,	"U23",
	0xC1,	"U25",	0xC2,	"U26",	0xC4,	"U28",	0xC7,	"U31",
	0xE0,	"U24",	0xE3,	"U27",	0xE5,	"U29",	0xE6,	"U30"
};

memlog (m, mcr)
	int m;
	struct mcr *mcr;
{
	register i;

	switch (cpu) {

#if VAX780
	case VAX_780:
	for (i = 0; i < (sizeof (memlogtab) / sizeof (memlogtab[0])); i++)
		if ((u_char)(M780_SYN(mcr)) == memlogtab[i].m_syndrome) {
			printf (
	"mcr%d: replace %s chip in %s bank of memory board %d (0-15)\n",
				m,
				memlogtab[i].m_chip,
				(M780_ADDR(mcr) & 0x8000) ? "upper" : "lower",
				(M780_ADDR(mcr) >> 16));
			return;
		}
	printf ("mcr%d: multiple errors, not traceable\n", m);
	break;
#endif
	}
}
#endif

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
	printf("howto %d, devtype %d\n", arghowto, devtype);
#endif
	(void) spl1();
	howto = arghowto;
	if ((howto&RB_NOSYNC)==0 && waittime < 0 && bfreelist[0].b_forw) {
		waittime = 0;
		update();
		printf("syncing disks... ");
#ifdef notdef
		{ register struct buf *bp;
		  int iter, nbusy;

		  for (iter = 0; iter < 10; iter++) {
			nbusy = 0;
			for (bp = &buf[nbuf]; --bp >= buf; )
				if (bp->b_flags & B_BUSY)
					nbusy++;
			if (nbusy == 0)
				break;
			printf("%d ", nbusy);
		  }
		}
#else
		DELAY(10000000);
#endif
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
		if (paniced == RB_PANIC) {
			doadump();		/* TXDB_BOOT's itsself */
			/*NOTREACHED*/
		}
		tocons(TXDB_BOOT);
	}
#if defined(VAX750) || defined(VAX730)
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
#define	MC780_TBPAR	2
#define	MC750_TBPAR	2
#endif
#if VAX730
#define	NMC730	12
char *mc730[] = {
	"tb par",	"bad retry",	"bad intr id",	"cant write ptem",
	"unkn mcr err",	"iib rd err",	"nxm ref",	"cp rds",
	"unalgn ioref",	"nonlw ioref",	"bad ioaddr",	"unalgn ubaddr",
};
#define	MC730_TBPAR	0
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
struct mc730frame {
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
#if VAX730
	case VAX_730:
		if (type < NMC730)
			printf("%s", mc730[type]);
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
		/* BOOK'' AND SHOULD BE PUT IN AN ``sbi.h'' */
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
		if ((type&0xf)==MC750_TBPAR) {
			printf("tbuf par!?!: flushing and returning\n");
			mtpr(TBIA, 0);
			return;
		}
		break;
		}
#endif
#if VAX730
	case VAX_730: {
		register struct mc730frame *mcf = (struct mc730frame *)cmcf;
		printf("params %x,%x pc %x psl %x mcesr %x\n",
		    mcf->mc3_parm[0], mcf->mc3_parm[1],
		    mcf->mc3_pc, mcf->mc3_psl, mfpr(MCESR));
		mtpr(MCESR, 0xf);
		break;
		}
#endif
	}
	memerr();
	panic("mchk");
}

#ifdef notdef
microtime(tvp)
	struct timeval *tvp;
{
	int s = spl7();

	tvp->tv_sec = time.tv_sec;
	tvp->tv_usec = (lbolt+1)*16667 + mfpr(ICR);
	while (tvp->tv_usec > 1000000) {
		tvp->tv_sec++;
		tvp->tv_usec -= 1000000;
	}
	splx(s);
}
#endif

physstrat(bp, strat, prio)
	struct buf *bp;
	int (*strat)(), prio;
{
	int s;

	(*strat)(bp);
	/* pageout daemon doesn't wait for pushed pages */
	if (bp->b_flags & B_DIRTY)
		return;
	s = spl6();
	while ((bp->b_flags & B_DONE) == 0)
		sleep((caddr_t)bp, prio);
	splx(s);
}
