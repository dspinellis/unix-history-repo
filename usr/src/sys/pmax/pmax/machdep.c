/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department, The Mach Operating System project at
 * Carnegie-Mellon University and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)machdep.c	7.13 (Berkeley) %G%
 */

/* from: Utah $Hdr: machdep.c 1.63 91/04/24$ */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/signalvar.h>
#include <sys/kernel.h>
#include <sys/map.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/reboot.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/clist.h>
#include <sys/callout.h>
#include <sys/malloc.h>
#include <sys/mbuf.h>
#include <sys/msgbuf.h>
#include <sys/user.h>
#include <sys/exec.h>
#ifdef SYSVSHM
#include <sys/shm.h>
#endif

#include <vm/vm.h>
#include <vm/vm_kern.h>
#include <vm/vm_page.h>

#include <machine/cpu.h>
#include <machine/reg.h>
#include <machine/psl.h>
#include <machine/pte.h>
#include <machine/dc7085cons.h>

#include <pmax/stand/dec_prom.h>

#include <pmax/dev/device.h>
#include <pmax/dev/sccreg.h>
#include <pmax/dev/ascreg.h>

#include <pmax/pmax/clockreg.h>
#include <pmax/pmax/kn01.h>
#include <pmax/pmax/kn02.h>
#include <pmax/pmax/kmin.h>
#include <pmax/pmax/maxine.h>
#include <pmax/pmax/kn03.h>
#include <pmax/pmax/asic.h>
#include <pmax/pmax/turbochannel.h>
#include <pmax/pmax/pmaxtype.h>
#include <pmax/pmax/cons.h>

#include <pm.h>
#include <cfb.h>
#include <mfb.h>
#include <xcfb.h>
#include <dc.h>
#include <dtop.h>
#include <scc.h>
#include <le.h>
#include <asc.h>

#if NDC > 0
extern int dcGetc(), dcparam();
extern void dcPutc();
#endif
#if NDTOP > 0
extern int dtopKBDGetc();
#endif
#if NSCC > 0
extern int sccGetc(), sccparam();
extern void sccPutc();
#endif
extern int KBDGetc();
extern void fbPutc();
extern struct consdev cn_tab;

/* Will scan from max to min, inclusive */
static int tc_max_slot = KN02_TC_MAX;
static int tc_min_slot = KN02_TC_MIN;
static u_int tc_slot_phys_base [TC_MAX_SLOTS] = {
	/* use 3max for default values */
	KN02_PHYS_TC_0_START, KN02_PHYS_TC_1_START,
	KN02_PHYS_TC_2_START, KN02_PHYS_TC_3_START,
	KN02_PHYS_TC_4_START, KN02_PHYS_TC_5_START,
	KN02_PHYS_TC_6_START, KN02_PHYS_TC_7_START
};

vm_map_t buffer_map;

/*
 * Declare these as initialized data so we can patch them.
 */
int	nswbuf = 0;
#ifdef	NBUF
int	nbuf = NBUF;
#else
int	nbuf = 0;
#endif
#ifdef	BUFPAGES
int	bufpages = BUFPAGES;
#else
int	bufpages = 0;
#endif
int	msgbufmapped = 0;	/* set when safe to use msgbuf */
int	maxmem;			/* max memory per process */
int	physmem;		/* max supported memory, changes to actual */
int	pmax_boardtype;		/* Mother board type */
u_long	le_iomem;		/* 128K for lance chip via. ASIC */
u_long	asc_iomem;		/* and 7 * 8K buffers for the scsi */
u_long	asic_base;		/* Base address of I/O asic */
const	struct callback *callv;	/* pointer to PROM entry points */

void	(*tc_enable_interrupt)();
extern	int (*pmax_hardware_intr)();
void	pmax_slot_hand_fill();
int	kn02_intr(), kmin_intr(), xine_intr(), pmax_intr();
#ifdef DS5000_240
int	kn03_intr();
#endif
extern	int Mach_spl0(), Mach_spl1(), Mach_spl2(), Mach_spl3(), splhigh();
int	(*Mach_splnet)() = splhigh;
int	(*Mach_splbio)() = splhigh;
int	(*Mach_splimp)() = splhigh;
int	(*Mach_spltty)() = splhigh;
int	(*Mach_splclock)() = splhigh;
int	(*Mach_splstatclock)() = splhigh;
void	(*tc_slot_hand_fill)();
extern	volatile struct chiptime *Mach_clock_addr;
u_long	kmin_tc3_imask, xine_tc3_imask;
#ifdef DS5000_240
u_long	kn03_tc3_imask;
#endif
tc_option_t tc_slot_info[TC_MAX_LOGICAL_SLOTS];
static	void asic_init();
extern	void RemconsInit();
#ifdef DS5000
void	kn02_enable_intr(), kn02_slot_hand_fill(),
	kmin_enable_intr(), kmin_slot_hand_fill(),
	xine_enable_intr(), xine_slot_hand_fill(),
	tc_find_all_options();
#ifdef DS5000_240
void	kn03_enable_intr(), kn03_slot_hand_fill();
#endif
#endif /* DS5000 */

/*
 * safepri is a safe priority for sleep to set for a spin-wait
 * during autoconfiguration or after a panic.
 */
int	safepri = PSL_LOWIPL;

struct	user *proc0paddr;
struct	proc nullproc;		/* for use by swtch_exit() */

/*
 * Do all the stuff that locore normally does before calling main().
 * Process arguments passed to us by the prom monitor.
 * Return the first page address following the system.
 */
mach_init(argc, argv, code, cv)
	int argc;
	char *argv[];
	u_int code;
	const struct callback *cv;
{
	register char *cp;
	register int i;
	register unsigned firstaddr;
	register caddr_t v;
	caddr_t start;
	extern char edata[], end[];
	extern char MachUTLBMiss[], MachUTLBMissEnd[];
	extern char MachException[], MachExceptionEnd[];
#ifdef ATTR
	extern char *pmap_attributes;
#endif

	/* clear the BSS segment */
	v = (caddr_t)pmax_round_page(end);
	bzero(edata, v - edata);

	/* check for direct boot from DS5000 PROM */
	if (argc > 0 && strcmp(argv[0], "boot") == 0) {
		argc--;
		argv++;
	}

	/* look at argv[0] and compute bootdev */
	makebootdev(argv[0]);

	/*
	 * Look at arguments passed to us and compute boothowto.
	 */
#ifdef GENERIC
	boothowto = RB_SINGLE | RB_ASKNAME;
#else
	boothowto = RB_SINGLE;
#endif
#ifdef KADB
	boothowto |= RB_KDB;
#endif
	if (argc > 1) {
		for (i = 1; i < argc; i++) {
			for (cp = argv[i]; *cp; cp++) {
				switch (*cp) {
				case 'a': /* autoboot */
					boothowto &= ~RB_SINGLE;
					break;

				case 'd': /* use compiled in default root */
					boothowto |= RB_DFLTROOT;
					break;

				case 'm': /* mini root present in memory */
					boothowto |= RB_MINIROOT;
					break;

				case 'n': /* ask for names */
					boothowto |= RB_ASKNAME;
					break;

				case 'N': /* don't ask for names */
					boothowto &= ~RB_ASKNAME;
				}
			}
		}
	}

#ifdef MFS
	/*
	 * Check to see if a mini-root was loaded into memory. It resides
	 * at the start of the next page just after the end of BSS.
	 */
	if (boothowto & RB_MINIROOT)
		v += mfs_initminiroot(v);
#endif

	/*
	 * Init mapping for u page(s) for proc[0], pm_tlbpid 1.
	 */
	start = v;
	curproc->p_addr = proc0paddr = (struct user *)v;
	curproc->p_md.md_regs = proc0paddr->u_pcb.pcb_regs;
	firstaddr = MACH_CACHED_TO_PHYS(v);
	for (i = 0; i < UPAGES; i++) {
		MachTLBWriteIndexed(i,
			(UADDR + (i << PGSHIFT)) | (1 << VMMACH_TLB_PID_SHIFT),
			curproc->p_md.md_upte[i] = firstaddr | PG_V | PG_M);
		firstaddr += NBPG;
	}
	v += UPAGES * NBPG;
	MachSetPID(1);

	/*
	 * init nullproc for swtch_exit().
	 * init mapping for u page(s), pm_tlbpid 0
	 * This could be used for an idle process.
	 */
	nullproc.p_addr = (struct user *)v;
	nullproc.p_md.md_regs = ((struct user *)v)->u_pcb.pcb_regs;
	for (i = 0; i < UPAGES; i++) {
		nullproc.p_md.md_upte[i] = firstaddr | PG_V | PG_M;
		firstaddr += NBPG;
	}
	v += UPAGES * NBPG;

	/* clear pages for u areas */
	bzero(start, v - start);

	/*
	 * Copy down exception vector code.
	 */
	if (MachUTLBMissEnd - MachUTLBMiss > 0x80)
		panic("startup: UTLB code too large");
	bcopy(MachUTLBMiss, (char *)MACH_UTLB_MISS_EXC_VEC,
		MachUTLBMissEnd - MachUTLBMiss);
	bcopy(MachException, (char *)MACH_GEN_EXC_VEC,
		MachExceptionEnd - MachException);

	/*
	 * Clear out the I and D caches.
	 */
	MachConfigCache();
	MachFlushCache();

	/*
	 * Determine what model of computer we are running on.
	 */
	if (code == DEC_PROM_MAGIC) {
		callv = cv;
		i = (*cv->getsysid)();
		cp = "";
	} else {
		callv = &callvec;
		if (cp = (*callv->getenv)("systype"))
			i = atoi(cp);
		else {
			cp = "";
			i = 0;
		}
	}
	/* check for MIPS based platform */
	if (((i >> 24) & 0xFF) != 0x82) {
		printf("Unknown System type '%s'\n", cp);
		boot(RB_HALT | RB_NOSYNC);
	}

	/* check what model platform we are running on */
	pmax_boardtype = ((i >> 16) & 0xff);
	switch (pmax_boardtype) {
	case DS_PMAX:	/* DS3100 Pmax */
		/*
		 * Set up interrupt handling and I/O addresses.
		 */
		pmax_hardware_intr = pmax_intr;
		Mach_splnet = Mach_spl1;
		Mach_splbio = Mach_spl0;
		Mach_splimp = Mach_spl1;
		Mach_spltty = Mach_spl2;
		Mach_splclock = Mach_spl3;
		Mach_splstatclock = Mach_spl3;
		Mach_clock_addr = (volatile struct chiptime *)
			MACH_PHYS_TO_UNCACHED(KN01_SYS_CLOCK);
		pmax_slot_hand_fill();
		break;

#ifdef DS5000
	case DS_3MAX:	/* DS5000/200 3max */
		{
		volatile int *csr_addr =
			(volatile int *)MACH_PHYS_TO_UNCACHED(KN02_SYS_CSR);

		/* disable all TURBOchannel interrupts */
		i = *csr_addr;
		*csr_addr = i & ~(KN02_CSR_WRESERVED | 0xFF);

		tc_slot_hand_fill = kn02_slot_hand_fill;
		pmax_hardware_intr = kn02_intr;
		tc_enable_interrupt = kn02_enable_intr;
		Mach_splnet = Mach_spl0;
		Mach_splbio = Mach_spl0;
		Mach_splimp = Mach_spl0;
		Mach_spltty = Mach_spl0;
		Mach_splclock = Mach_spl1;
		Mach_splstatclock = Mach_spl1;
		Mach_clock_addr = (volatile struct chiptime *)
			MACH_PHYS_TO_UNCACHED(KN02_SYS_CLOCK);

		/*
		 * Probe the TURBOchannel to see what controllers are present.
		 */
		tc_find_all_options();

		/* clear any memory errors from probes */
		*(unsigned *)MACH_PHYS_TO_UNCACHED(KN02_SYS_ERRADR) = 0;
		}
		break;

	case DS_3MIN:	/* DS5000/1xx 3min */
		tc_max_slot = KMIN_TC_MAX;
		tc_min_slot = KMIN_TC_MIN;
		tc_slot_phys_base[0] = KMIN_PHYS_TC_0_START;
		tc_slot_phys_base[1] = KMIN_PHYS_TC_1_START;
		tc_slot_phys_base[2] = KMIN_PHYS_TC_2_START;
		asic_base = MACH_PHYS_TO_UNCACHED(KMIN_SYS_ASIC);
		tc_slot_hand_fill = kmin_slot_hand_fill;
		pmax_hardware_intr = kmin_intr;
		tc_enable_interrupt = kmin_enable_intr;

		/*
		 * Since all the motherboard interrupts come through the
		 * I/O ASIC, it has to be turned off for all the spls and
		 * since we don't know what kinds of devices are in the
		 * turbochannel option slots, just splhigh().
		 */
		Mach_splnet = splhigh;
		Mach_splbio = splhigh;
		Mach_splimp = splhigh;
		Mach_spltty = splhigh;
		Mach_splclock = splhigh;
		Mach_splstatclock = splhigh;
		Mach_clock_addr = (volatile struct chiptime *)
			MACH_PHYS_TO_UNCACHED(KMIN_SYS_CLOCK);

		/*
		 * Probe the TURBOchannel to see what controllers are present.
		 */
		tc_find_all_options();

		/*
		 * Initialize interrupts.
		 */
		*(u_int *)ASIC_REG_IMSK(asic_base) = KMIN_IM0;
		*(u_int *)ASIC_REG_INTR(asic_base) = 0;
		/* clear any memory errors from probes */
		*(unsigned *)MACH_PHYS_TO_UNCACHED(KMIN_REG_TIMEOUT) = 0;
		break;

	case DS_MAXINE:	/* DS5000/xx maxine */
		tc_max_slot = XINE_TC_MAX;
		tc_min_slot = XINE_TC_MIN;
		tc_slot_phys_base[0] = XINE_PHYS_TC_0_START;
		tc_slot_phys_base[1] = XINE_PHYS_TC_1_START;
		asic_base = MACH_PHYS_TO_UNCACHED(XINE_SYS_ASIC);
		tc_slot_hand_fill = xine_slot_hand_fill;
		pmax_hardware_intr = xine_intr;
		tc_enable_interrupt = xine_enable_intr;
		Mach_splnet = Mach_spl3;
		Mach_splbio = Mach_spl3;
		Mach_splimp = Mach_spl3;
		Mach_spltty = Mach_spl3;
		Mach_splclock = Mach_spl1;
		Mach_splstatclock = Mach_spl1;
		Mach_clock_addr = (volatile struct chiptime *)
			MACH_PHYS_TO_UNCACHED(XINE_SYS_CLOCK);

		/*
		 * Probe the TURBOchannel to see what controllers are present.
		 */
		tc_find_all_options();

		/*
		 * Initialize interrupts.
		 */
		*(u_int *)ASIC_REG_IMSK(asic_base) = XINE_IM0;
		*(u_int *)ASIC_REG_INTR(asic_base) = 0;
		/* clear any memory errors from probes */
		*(unsigned *)MACH_PHYS_TO_UNCACHED(XINE_REG_TIMEOUT) = 0;
		break;

#ifdef DS5000_240
	case DS_3MAXPLUS:	/* DS5000/240 3max+ UNTESTED!! */
		tc_max_slot = KN03_TC_MAX;
		tc_min_slot = KN03_TC_MIN;
		tc_slot_phys_base[0] = KN03_PHYS_TC_0_START;
		tc_slot_phys_base[1] = KN03_PHYS_TC_1_START;
		tc_slot_phys_base[2] = KN03_PHYS_TC_2_START;
		asic_base = MACH_PHYS_TO_UNCACHED(KN03_SYS_ASIC);
		tc_slot_hand_fill = kn03_slot_hand_fill;
		pmax_hardware_intr = kn03_intr;
		tc_enable_interrupt = kn03_enable_intr;

		Mach_splnet = Mach_spl0;
		Mach_splbio = Mach_spl0;
		Mach_splimp = Mach_spl0;
		Mach_spltty = Mach_spl0;
		Mach_splclock = Mach_spl1;
		Mach_splstatclock = Mach_spl1;
		Mach_clock_addr = (volatile struct chiptime *)
			MACH_PHYS_TO_UNCACHED(KN03_SYS_CLOCK);

		/*
		 * Probe the TURBOchannel to see what controllers are present.
		 */
		tc_find_all_options();

		/*
		 * Initialize interrupts.
		 */
		*(u_int *)ASIC_REG_IMSK(asic_base) = KN03_IM0;
		*(u_int *)ASIC_REG_INTR(asic_base) = 0;
		/* clear any memory errors from probes */
		*(unsigned *)MACH_PHYS_TO_UNCACHED(KN03_SYS_ERRADR) = 0;
		break;
#endif /* DS5000_240 */
#endif /* DS5000 */

	default:
		printf("kernel not configured for systype 0x%x\n", i);
		boot(RB_HALT | RB_NOSYNC);
	}

	/*
	 * Find out how much memory is available.
	 */
	physmem = btoc(v - KERNBASE);
	cp = (char *)MACH_PHYS_TO_UNCACHED(physmem << PGSHIFT);
	while (cp < (char *)MACH_MAX_MEM_ADDR) {
		if (badaddr(cp, 4))
			break;
		*(int *)cp = 0xa5a5a5a5;
		/*
		 * Data will persist on the bus if we read it right
		 * away. Have to be tricky here.
		 */
		((int *)cp)[4] = 0x5a5a5a5a;
		MachEmptyWriteBuffer();
		if (*(int *)cp != 0xa5a5a5a5)
			break;
		cp += NBPG;
		physmem++;
	}

	maxmem = physmem;

#if NLE > 0
	/*
	 * Grab 128K at the top of physical memory for the lance chip
	 * on machines where it does dma through the I/O ASIC.
	 * It must be physically contiguous and aligned on a 128K boundary.
	 */
	if (pmax_boardtype == DS_3MIN || pmax_boardtype == DS_MAXINE ||
		pmax_boardtype == DS_3MAXPLUS) {
		maxmem -= btoc(128 * 1024);
		le_iomem = (maxmem << PGSHIFT);
	}
#endif /* NLE */
#if NASC > 0
	/*
	 * Ditto for the scsi chip. There is probably a way to make asc.c
	 * do dma without these buffers, but it would require major
	 * re-engineering of the asc driver.
	 * They must be 8K in size and page aligned.
	 */
	if (pmax_boardtype == DS_3MIN || pmax_boardtype == DS_MAXINE ||
		pmax_boardtype == DS_3MAXPLUS) {
		maxmem -= btoc(ASC_NCMD * 8192);
		asc_iomem = (maxmem << PGSHIFT);
	}
#endif /* NASC */

	/*
	 * Initialize error message buffer (at end of core).
	 */
	maxmem -= btoc(sizeof (struct msgbuf));
	msgbufp = (struct msgbuf *)(MACH_PHYS_TO_CACHED(maxmem << PGSHIFT));
	msgbufmapped = 1;

	/*
	 * Allocate space for system data structures.
	 * The first available kernel virtual address is in "v".
	 * As pages of kernel virtual memory are allocated, "v" is incremented.
	 *
	 * These data structures are allocated here instead of cpu_startup()
	 * because physical memory is directly addressable. We don't have
	 * to map these into virtual address space.
	 */
	start = v;

#define	valloc(name, type, num) \
	    (name) = (type *)v; v = (caddr_t)((name)+(num))
#define	valloclim(name, type, num, lim) \
	    (name) = (type *)v; v = (caddr_t)((lim) = ((name)+(num)))
	valloc(cfree, struct cblock, nclist);
	valloc(callout, struct callout, ncallout);
	valloc(swapmap, struct map, nswapmap = maxproc * 2);
#ifdef SYSVSHM
	valloc(shmsegs, struct shmid_ds, shminfo.shmmni);
#endif
#ifdef ATTR
	/* this is allocated here just to save a few bytes */
	valloc(pmap_attributes, char, physmem);
#endif

	/*
	 * Determine how many buffers to allocate.
	 * We allocate more buffer space than the BSD standard of
	 * using 10% of memory for the first 2 Meg, 5% of remaining.
	 * We just allocate a flat 10%.  Insure a minimum of 16 buffers.
	 * We allocate 1/2 as many swap buffer headers as file i/o buffers.
	 */
	if (bufpages == 0)
		bufpages = physmem / 10 / CLSIZE;
	if (nbuf == 0) {
		nbuf = bufpages;
		if (nbuf < 16)
			nbuf = 16;
	}
	if (nswbuf == 0) {
		nswbuf = (nbuf / 2) &~ 1;	/* force even */
		if (nswbuf > 256)
			nswbuf = 256;		/* sanity */
	}
	valloc(swbuf, struct buf, nswbuf);
	valloc(buf, struct buf, nbuf);

	/*
	 * Clear allocated memory.
	 */
	v = (caddr_t)pmax_round_page(v);
	bzero(start, v - start);

	/*
	 * Initialize the virtual memory system.
	 */
	pmap_bootstrap((vm_offset_t)MACH_CACHED_TO_PHYS(v));
}

/*
 * Console initialization: called early on from main,
 * before vm init or startup.  Do enough configuration
 * to choose and initialize a console.
 */
consinit()
{
	register int kbd, crt;
	register char *oscon;

	/*
	 * First get the "osconsole" environment variable.
	 */
	oscon = (*callv->getenv)("osconsole");
	crt = kbd = -1;
	if (oscon && *oscon >= '0' && *oscon <= '9') {
		kbd = *oscon - '0';
		cn_tab.cn_screen = 0;
		while (*++oscon) {
			if (*oscon == ',')
				cn_tab.cn_screen = 1;
			else if (cn_tab.cn_screen &&
			    *oscon >= '0' && *oscon <= '9') {
				crt = kbd;
				kbd = *oscon - '0';
				break;
			}
		}
	}
	if (pmax_boardtype == DS_PMAX && kbd == 1)
		cn_tab.cn_screen = 1;

	/*
	 * First try the keyboard/crt cases then fall through to the
	 * remote serial lines.
	 */
	if (cn_tab.cn_screen) {
	    switch (pmax_boardtype) {
	    case DS_PMAX:
#if NDC > 0 && NPM > 0
		if (pminit()) {
			cn_tab.cn_dev = makedev(DCDEV, DCKBD_PORT);
			cn_tab.cn_getc = KBDGetc;
			cn_tab.cn_kbdgetc = dcGetc;
			cn_tab.cn_putc = fbPutc;
			cn_tab.cn_disabled = 0;
			return;
		}
#endif /* NDC and NPM */
		goto remcons;

	    case DS_MAXINE:
#if NDTOP > 0
		if (kbd == 3) {
			cn_tab.cn_dev = makedev(DTOPDEV, 0);
			cn_tab.cn_getc = dtopKBDGetc;
			cn_tab.cn_putc = fbPutc;
		} else
#endif /* NDTOP */
			goto remcons;
#if NXCFB > 0
		if (crt == 3 && xcfbinit()) {
			cn_tab.cn_disabled = 0;
			return;
		}
#endif /* XCFB */
		break;

	    case DS_3MAX:
#if NDC > 0
		if (kbd == 7) {
			cn_tab.cn_dev = makedev(DCDEV, DCKBD_PORT);
			cn_tab.cn_getc = KBDGetc;
			cn_tab.cn_kbdgetc = dcGetc;
			cn_tab.cn_putc = fbPutc;
		} else
#endif /* NDC */
			goto remcons;
		break;

	    case DS_3MIN:
	    case DS_3MAXPLUS:
#if NSCC > 0
		if (kbd == 3) {
			cn_tab.cn_dev = makedev(SCCDEV, SCCKBD_PORT);
			cn_tab.cn_getc = KBDGetc;
			cn_tab.cn_kbdgetc = sccGetc;
			cn_tab.cn_putc = fbPutc;
		} else
#endif /* NSCC */
			goto remcons;
		break;

	    default:
		goto remcons;
	    };

	    /*
	     * Check for a suitable turbochannel frame buffer.
	     */
	    if (tc_slot_info[crt].driver_name) {
#if NMFB > 0
		if (strcmp(tc_slot_info[crt].driver_name, "mfb") == 0 &&
		    mfbinit(tc_slot_info[crt].k1seg_address)) {
			cn_tab.cn_disabled = 0;
			return;
		}
#endif /* NMFB */
#if NCFB > 0
		if (strcmp(tc_slot_info[crt].driver_name, "cfb") == 0 &&
		    cfbinit(tc_slot_info[crt].k1seg_address)) {
			cn_tab.cn_disabled = 0;
			return;
		}
#endif /* NCFB */
		printf("crt: %s not supported as console device\n",
			tc_slot_info[crt].driver_name);
	    } else
		printf("No crt console device in slot %d\n", crt);
	}
remcons:
	/*
	 * Configure a serial port as a remote console.
	 */
	cn_tab.cn_screen = 0;
	switch (pmax_boardtype) {
	case DS_PMAX:
#if NDC > 0
		if (kbd == 4)
			cn_tab.cn_dev = makedev(DCDEV, DCCOMM_PORT);
		else
			cn_tab.cn_dev = makedev(DCDEV, DCPRINTER_PORT);
		cn_tab.cn_getc = dcGetc;
		cn_tab.cn_putc = dcPutc;
#endif /* NDC */
		break;

	case DS_3MAX:
#if NDC > 0
		cn_tab.cn_dev = makedev(DCDEV, DCPRINTER_PORT);
		cn_tab.cn_getc = dcGetc;
		cn_tab.cn_putc = dcPutc;
#endif /* NDC */
		break;

	case DS_3MIN:
	case DS_3MAXPLUS:
#if NSCC > 0
		cn_tab.cn_dev = makedev(SCCDEV, SCCCOMM3_PORT);
		cn_tab.cn_getc = sccGetc;
		cn_tab.cn_putc = sccPutc;
#endif /* NSCC */
		break;

	case DS_MAXINE:
#if NSCC > 0
		cn_tab.cn_dev = makedev(SCCDEV, SCCCOMM2_PORT);
		cn_tab.cn_getc = sccGetc;
		cn_tab.cn_putc = sccPutc;
#endif /* NSCC */
		break;
	};
	if (cn_tab.cn_dev == NODEV)
		printf("Can't configure console!\n");
}

/*
 * cpu_startup: allocate memory for variable-sized tables,
 * initialize cpu, and do autoconfiguration.
 */
cpu_startup()
{
	register unsigned i;
	register caddr_t v;
	int base, residual;
	extern long Usrptsize;
	extern struct map *useriomap;
#ifdef DEBUG
	extern int pmapdebug;
	int opmapdebug = pmapdebug;
#endif
	vm_offset_t minaddr, maxaddr;
	vm_size_t size;

#ifdef DEBUG
	pmapdebug = 0;
#endif

	/*
	 * Good {morning,afternoon,evening,night}.
	 */
	printf(version);
	printf("real mem = %d\n", ctob(physmem));

	/*
	 * Allocate virtual address space for file I/O buffers.
	 * Note they are different than the array of headers, 'buf',
	 * and usually occupy more virtual memory than physical.
	 */
	size = MAXBSIZE * nbuf;
	buffer_map = kmem_suballoc(kernel_map, (vm_offset_t *)&buffers,
				   &maxaddr, size, FALSE);
	minaddr = (vm_offset_t)buffers;
	if (vm_map_find(buffer_map, vm_object_allocate(size), (vm_offset_t)0,
			&minaddr, size, FALSE) != KERN_SUCCESS)
		panic("startup: cannot allocate buffers");
	base = bufpages / nbuf;
	residual = bufpages % nbuf;
	for (i = 0; i < nbuf; i++) {
		vm_size_t curbufsize;
		vm_offset_t curbuf;

		/*
		 * First <residual> buffers get (base+1) physical pages
		 * allocated for them.  The rest get (base) physical pages.
		 *
		 * The rest of each buffer occupies virtual space,
		 * but has no physical memory allocated for it.
		 */
		curbuf = (vm_offset_t)buffers + i * MAXBSIZE;
		curbufsize = CLBYTES * (i < residual ? base+1 : base);
		vm_map_pageable(buffer_map, curbuf, curbuf+curbufsize, FALSE);
		vm_map_simplify(buffer_map, curbuf);
	}
	/*
	 * Allocate a submap for exec arguments.  This map effectively
	 * limits the number of processes exec'ing at any time.
	 */
	exec_map = kmem_suballoc(kernel_map, &minaddr, &maxaddr,
				 16*NCARGS, TRUE);
	/*
	 * Allocate a submap for physio
	 */
	phys_map = kmem_suballoc(kernel_map, &minaddr, &maxaddr,
				 VM_PHYS_SIZE, TRUE);

	/*
	 * Finally, allocate mbuf pool.  Since mclrefcnt is an off-size
	 * we use the more space efficient malloc in place of kmem_alloc.
	 */
	mclrefcnt = (char *)malloc(NMBCLUSTERS+CLBYTES/MCLBYTES,
				   M_MBUF, M_NOWAIT);
	bzero(mclrefcnt, NMBCLUSTERS+CLBYTES/MCLBYTES);
	mb_map = kmem_suballoc(kernel_map, (vm_offset_t *)&mbutl, &maxaddr,
			       VM_MBUF_SIZE, FALSE);
	/*
	 * Initialize callouts
	 */
	callfree = callout;
	for (i = 1; i < ncallout; i++)
		callout[i-1].c_next = &callout[i];
	callout[i-1].c_next = NULL;

#ifdef DEBUG
	pmapdebug = opmapdebug;
#endif
	printf("avail mem = %d\n", ptoa(cnt.v_free_count));
	printf("using %d buffers containing %d bytes of memory\n",
		nbuf, bufpages * CLBYTES);
	/*
	 * Set up CPU-specific registers, cache, etc.
	 */
	initcpu();

	/*
	 * Set up buffers, so they can be used to read disk labels.
	 */
	bufinit();

	/*
	 * Configure the system.
	 */
	configure();
}

/*
 * Set registers on exec.
 * Clear all registers except sp, pc.
 */
setregs(p, entry, retval)
	register struct proc *p;
	u_long entry;
	int retval[2];
{
	int sp = p->p_md.md_regs[SP];
	extern struct proc *machFPCurProcPtr;

	bzero((caddr_t)p->p_md.md_regs, (FSR + 1) * sizeof(int));
	p->p_md.md_regs[SP] = sp;
	p->p_md.md_regs[PC] = entry & ~3;
	p->p_md.md_regs[PS] = PSL_USERSET;
	p->p_md.md_flags & ~MDP_FPUSED;
	if (machFPCurProcPtr == p)
		machFPCurProcPtr = (struct proc *)0;
}

/*
 * WARNING: code in locore.s assumes the layout shown for sf_signum
 * thru sf_handler so... don't screw with them!
 */
struct sigframe {
	int	sf_signum;		/* signo for handler */
	int	sf_code;		/* additional info for handler */
	struct	sigcontext *sf_scp;	/* context ptr for handler */
	sig_t	sf_handler;		/* handler addr for u_sigc */
	struct	sigcontext sf_sc;	/* actual context */
};

#ifdef DEBUG
int sigdebug = 0;
int sigpid = 0;
#define SDB_FOLLOW	0x01
#define SDB_KSTACK	0x02
#define SDB_FPSTATE	0x04
#endif

/*
 * Send an interrupt to process.
 */
void
sendsig(catcher, sig, mask, code)
	sig_t catcher;
	int sig, mask;
	unsigned code;
{
	register struct proc *p = curproc;
	register struct sigframe *fp;
	register int *regs;
	register struct sigacts *psp = p->p_sigacts;
	int oonstack, fsize;
	struct sigcontext ksc;
	extern char sigcode[], esigcode[];

	regs = p->p_md.md_regs;
	oonstack = psp->ps_sigstk.ss_flags & SA_ONSTACK;
	/*
	 * Allocate and validate space for the signal handler
	 * context. Note that if the stack is in data space, the
	 * call to grow() is a nop, and the copyout()
	 * will fail if the process has not already allocated
	 * the space with a `brk'.
	 */
	fsize = sizeof(struct sigframe);
	if ((psp->ps_flags & SAS_ALTSTACK) &&
	    (psp->ps_sigstk.ss_flags & SA_ONSTACK) == 0 &&
	    (psp->ps_sigonstack & sigmask(sig))) {
		fp = (struct sigframe *)(psp->ps_sigstk.ss_base +
					 psp->ps_sigstk.ss_size - fsize);
		psp->ps_sigstk.ss_flags |= SA_ONSTACK;
	} else
		fp = (struct sigframe *)(regs[SP] - fsize);
	if ((unsigned)fp <= USRSTACK - ctob(p->p_vmspace->vm_ssize)) 
		(void)grow(p, (unsigned)fp);
#ifdef DEBUG
	if ((sigdebug & SDB_FOLLOW) ||
	    (sigdebug & SDB_KSTACK) && p->p_pid == sigpid)
		printf("sendsig(%d): sig %d ssp %x usp %x scp %x\n",
		       p->p_pid, sig, &oonstack, fp, &fp->sf_sc);
#endif
	/*
	 * Build the signal context to be used by sigreturn.
	 */
	ksc.sc_onstack = oonstack;
	ksc.sc_mask = mask;
	ksc.sc_pc = regs[PC];
	ksc.sc_regs[ZERO] = 0xACEDBADE;		/* magic number */
	bcopy((caddr_t)&regs[1], (caddr_t)&ksc.sc_regs[1],
		sizeof(ksc.sc_regs) - sizeof(int));
	ksc.sc_fpused = p->p_md.md_flags & MDP_FPUSED;
	if (ksc.sc_fpused) {
		extern struct proc *machFPCurProcPtr;

		/* if FPU has current state, save it first */
		if (p == machFPCurProcPtr)
			MachSaveCurFPState(p);
		bcopy((caddr_t)&p->p_md.md_regs[F0], (caddr_t)ksc.sc_fpregs,
			sizeof(ksc.sc_fpregs));
	}
	if (copyout((caddr_t)&ksc, (caddr_t)&fp->sf_sc, sizeof(ksc))) {
		/*
		 * Process has trashed its stack; give it an illegal
		 * instruction to halt it in its tracks.
		 */
		SIGACTION(p, SIGILL) = SIG_DFL;
		sig = sigmask(SIGILL);
		p->p_sigignore &= ~sig;
		p->p_sigcatch &= ~sig;
		p->p_sigmask &= ~sig;
		psignal(p, SIGILL);
		return;
	}
	/* 
	 * Build the argument list for the signal handler.
	 */
	regs[A0] = sig;
	regs[A1] = code;
	regs[A2] = (int)&fp->sf_sc;
	regs[A3] = (int)catcher;

	regs[PC] = (int)catcher;
	regs[SP] = (int)fp;
	/*
	 * Signal trampoline code is at base of user stack.
	 */
	regs[RA] = (int)PS_STRINGS - (esigcode - sigcode);
#ifdef DEBUG
	if ((sigdebug & SDB_FOLLOW) ||
	    (sigdebug & SDB_KSTACK) && p->p_pid == sigpid)
		printf("sendsig(%d): sig %d returns\n",
		       p->p_pid, sig);
#endif
}

/*
 * System call to cleanup state after a signal
 * has been taken.  Reset signal mask and
 * stack state from context left by sendsig (above).
 * Return to previous pc and psl as specified by
 * context left by sendsig. Check carefully to
 * make sure that the user has not modified the
 * psl to gain improper priviledges or to cause
 * a machine fault.
 */
struct sigreturn_args {
	struct sigcontext *sigcntxp;
};
/* ARGSUSED */
sigreturn(p, uap, retval)
	struct proc *p;
	struct sigreturn_args *uap;
	int *retval;
{
	register struct sigcontext *scp;
	register int *regs;
	struct sigcontext ksc;
	int error;

	scp = uap->sigcntxp;
#ifdef DEBUG
	if (sigdebug & SDB_FOLLOW)
		printf("sigreturn: pid %d, scp %x\n", p->p_pid, scp);
#endif
	regs = p->p_md.md_regs;
	/*
	 * Test and fetch the context structure.
	 * We grab it all at once for speed.
	 */
	error = copyin((caddr_t)scp, (caddr_t)&ksc, sizeof(ksc));
	if (error || ksc.sc_regs[ZERO] != 0xACEDBADE) {
#ifdef DEBUG
		if (!(sigdebug & SDB_FOLLOW))
			printf("sigreturn: pid %d, scp %x\n", p->p_pid, scp);
		printf("  old sp %x ra %x pc %x\n",
			regs[SP], regs[RA], regs[PC]);
		printf("  new sp %x ra %x pc %x err %d z %x\n",
			ksc.sc_regs[SP], ksc.sc_regs[RA], ksc.sc_regs[PC],
			error, ksc.sc_regs[ZERO]);
#endif
		return (EINVAL);
	}
	scp = &ksc;
	/*
	 * Restore the user supplied information
	 */
	if (scp->sc_onstack & 01)
		p->p_sigacts->ps_sigstk.ss_flags |= SA_ONSTACK;
	else
		p->p_sigacts->ps_sigstk.ss_flags &= ~SA_ONSTACK;
	p->p_sigmask = scp->sc_mask &~ sigcantmask;
	regs[PC] = scp->sc_pc;
	bcopy((caddr_t)&scp->sc_regs[1], (caddr_t)&regs[1],
		sizeof(scp->sc_regs) - sizeof(int));
	if (scp->sc_fpused)
		bcopy((caddr_t)scp->sc_fpregs, (caddr_t)&p->p_md.md_regs[F0],
			sizeof(scp->sc_fpregs));
	return (EJUSTRETURN);
}

int	waittime = -1;

boot(howto)
	register int howto;
{

	/* take a snap shot before clobbering any registers */
	if (curproc)
		savectx(curproc->p_addr, 0);

	howto |= RB_HALT; /* XXX */
	boothowto = howto;
	if ((howto&RB_NOSYNC) == 0 && waittime < 0) {
		register struct buf *bp;
		int iter, nbusy;

		waittime = 0;
		(void) spl0();
		printf("syncing disks... ");
		/*
		 * Release vnodes held by texts before sync.
		 */
		if (panicstr == 0)
			vnode_pager_umount(NULL);
#ifdef notdef
#include "fd.h"
#if NFD > 0
		fdshutdown();
#endif
#endif
		sync(&proc0, (void *)NULL, (int *)NULL);

		for (iter = 0; iter < 20; iter++) {
			nbusy = 0;
			for (bp = &buf[nbuf]; --bp >= buf; )
				if ((bp->b_flags & (B_BUSY|B_INVAL)) == B_BUSY)
					nbusy++;
			if (nbusy == 0)
				break;
			printf("%d ", nbusy);
			DELAY(40000 * iter);
		}
		if (nbusy)
			printf("giving up\n");
		else
			printf("done\n");
		/*
		 * If we've been adjusting the clock, the todr
		 * will be out of synch; adjust it now.
		 */
		resettodr();
	}
	(void) splhigh();		/* extreme priority */
	if (callv != &callvec) {
		if (howto & RB_HALT)
			(*callv->rex)('h');
		else {
			if (howto & RB_DUMP)
				dumpsys();
			(*callv->rex)('b');
		}
	} else if (howto & RB_HALT) {
		volatile void (*f)() = (volatile void (*)())DEC_PROM_REINIT;

		(*f)();	/* jump back to prom monitor */
	} else {
		volatile void (*f)() = (volatile void (*)())DEC_PROM_AUTOBOOT;

		if (howto & RB_DUMP)
			dumpsys();
		(*f)();	/* jump back to prom monitor and do 'auto' cmd */
	}
	/*NOTREACHED*/
}

int	dumpmag = 0x8fca0101;	/* magic number for savecore */
int	dumpsize = 0;		/* also for savecore */
long	dumplo = 0;

dumpconf()
{
	int nblks;

	dumpsize = physmem;
	if (dumpdev != NODEV && bdevsw[major(dumpdev)].d_psize) {
		nblks = (*bdevsw[major(dumpdev)].d_psize)(dumpdev);
		if (dumpsize > btoc(dbtob(nblks - dumplo)))
			dumpsize = btoc(dbtob(nblks - dumplo));
		else if (dumplo == 0)
			dumplo = nblks - btodb(ctob(physmem));
	}
	/*
	 * Don't dump on the first CLBYTES (why CLBYTES?)
	 * in case the dump device includes a disk label.
	 */
	if (dumplo < btodb(CLBYTES))
		dumplo = btodb(CLBYTES);
}

/*
 * Doadump comes here after turning off memory management and
 * getting on the dump stack, either when called above, or by
 * the auto-restart code.
 */
dumpsys()
{
	int error;

	msgbufmapped = 0;
	if (dumpdev == NODEV)
		return;
	/*
	 * For dumps during autoconfiguration,
	 * if dump device has already configured...
	 */
	if (dumpsize == 0)
		dumpconf();
	if (dumplo < 0)
		return;
	printf("\ndumping to dev %x, offset %d\n", dumpdev, dumplo);
	printf("dump ");
	switch (error = (*bdevsw[major(dumpdev)].d_dump)(dumpdev)) {

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
		printf("error %d\n", error);
		break;

	case 0:
		printf("succeeded\n");
	}
}

/*
 * Return the best possible estimate of the time in the timeval
 * to which tvp points.  Unfortunately, we can't read the hardware registers.
 * We guarantee that the time will be greater than the value obtained by a
 * previous call.
 */
microtime(tvp)
	register struct timeval *tvp;
{
	int s = splclock();
	static struct timeval lasttime;

	*tvp = time;
#ifdef notdef
	tvp->tv_usec += clkread();
	while (tvp->tv_usec > 1000000) {
		tvp->tv_sec++;
		tvp->tv_usec -= 1000000;
	}
#endif
	if (tvp->tv_sec == lasttime.tv_sec &&
	    tvp->tv_usec <= lasttime.tv_usec &&
	    (tvp->tv_usec = lasttime.tv_usec + 1) > 1000000) {
		tvp->tv_sec++;
		tvp->tv_usec -= 1000000;
	}
	lasttime = *tvp;
	splx(s);
}

initcpu()
{
	register volatile struct chiptime *c;
	int i;

	/* disable clock interrupts (until startrtclock()) */
	c = Mach_clock_addr;
	c->regb = REGB_DATA_MODE | REGB_HOURS_FORMAT;
	i = c->regc;
	spl0();		/* safe to turn interrupts on now */
	return (i);
}

/*
 * Convert an ASCII string into an integer.
 */
int
atoi(s)
	char *s;
{
	int c;
	unsigned base = 10, d;
	int neg = 0, val = 0;

	if (s == 0 || (c = *s++) == 0)
		goto out;

	/* skip spaces if any */
	while (c == ' ' || c == '\t')
		c = *s++;

	/* parse sign, allow more than one (compat) */
	while (c == '-') {
		neg = !neg;
		c = *s++;
	}

	/* parse base specification, if any */
	if (c == '0') {
		c = *s++;
		switch (c) {
		case 'X':
		case 'x':
			base = 16;
			break;
		case 'B':
		case 'b':
			base = 2;
			break;
		default:
			base = 8;
		}
	}

	/* parse number proper */
	for (;;) {
		if (c >= '0' && c <= '9')
			d = c - '0';
		else if (c >= 'a' && c <= 'z')
			d = c - 'a' + 10;
		else if (c >= 'A' && c <= 'Z')
			d = c - 'A' + 10;
		else
			break;
		val *= base;
		val += d;
		c = *s++;
	}
	if (neg)
		val = -val;
out:
	return val;	
}

/*
 * Fill in the pmax addresses by hand.
 */
static struct pmax_address {
	char	*pmax_name;
	char	*pmax_addr;
	int	pmax_pri;
} pmax_addresses[] = {
	{ "pm",	(char *)MACH_PHYS_TO_CACHED(KN01_PHYS_FBUF_START),	3 },
	{ "dc",	(char *)MACH_PHYS_TO_UNCACHED(KN01_SYS_DZ),		2 },
	{ "le",	(char *)MACH_PHYS_TO_UNCACHED(KN01_SYS_LANCE),		1 },
	{ "sii",(char *)MACH_PHYS_TO_UNCACHED(KN01_SYS_SII),		0 },
	{ (char *)0, },
};

void
pmax_slot_hand_fill()
{
	register struct pmax_ctlr *cp;
	register struct driver *drp;
	register struct pmax_address *pmap;

	/*
	 * Find the device driver entry and fill in the address.
	 */
	for (cp = pmax_cinit; drp = cp->pmax_driver; cp++) {
		for (pmap = pmax_addresses; pmap->pmax_name; pmap++) {
			if (strcmp(drp->d_name, pmap->pmax_name))
				continue;
			if (cp->pmax_addr == (char *)QUES) {
				cp->pmax_addr = pmap->pmax_addr;
				cp->pmax_pri = pmap->pmax_pri;
				continue;
			}
		}
	}
}

#ifdef DS5000
/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */


/*
 * Driver map: associates a device driver to an option type.
 * Drivers name are (arbitrarily) defined in each driver and
 * used in the various config tables.
 */
struct drivers_map {
	char	module_name[TC_ROM_LLEN];	/* from ROM, literally! */
	char	*driver_name;			/* in bus_??_init[] tables */
} tc_drivers_map[] = {
	{ "KN02    ",	"dc"},		/* (*) 3max system board (with DC) */
	{ "PMAD-AA ",	"le"},		/* Ether */
	{ "PMAZ-AA ",	"asc"},		/* SCSI */
	{ "PMAG-AA ",	"mfb"},		/* Mono Frame Buffer */
	{ "PMAG-BA ",	"cfb"},		/* Color Frame Buffer */
	{ "PMAG-CA ",	"ga"},		/* 2D graphic board */
	{ "PMAG-DA ",	"gq"},		/* 3D graphic board (LM) */
	{ "PMAG-FA ",	"gq"},		/* 3D graphic board (HE) */
	{ "PMAG-DV ",	"xcfb"},	/* (*) maxine Color Frame Buffer */
	{ "Z8530   ",	"scc"},		/* (*) 3min/maxine serial lines */
	{ "ASIC    ",	"asic"},	/* (*) 3min/maxine DMA controller */
	{ "XINE-FDC",	"fdc"},		/* (*) maxine floppy controller */
	{ "DTOP    ",	"dtop"},	/* (*) maxine desktop bus */
	{ "AMD79c30",	"isdn"},	/* (*) maxine ISDN chip */
	{ "XINE-FRC",	"frc"},		/* (*) maxine free-running counter */
	{ "", 0}			/* list end */
};

/*
 * Identify an option on the TC.  Looks at the mandatory
 * info in the option's ROM and checks it.
 */
#ifdef DEBUG
int tc_verbose = 0;
#endif

static int
tc_identify_option( addr, slot, complain)
	tc_rommap_t	*addr;
	tc_option_t	*slot;
	int		complain;
{
	register int	i;
	unsigned char   width;
	char            firmwr[TC_ROM_LLEN+1], vendor[TC_ROM_LLEN+1],
			module[TC_ROM_LLEN+1], host_type[TC_ROM_SLEN+1];

	/*
	 * We do not really use the 'width' info, but take advantage
	 * of the restriction that the spec impose on the portion
	 * of the ROM that maps between +0x3e0 and +0x470, which
	 * is the only piece we need to look at.
	 */
	width = addr->rom_width.value;
	switch (width) {
	case 1:
	case 2:
	case 4:
		break;

	default:
#ifdef DEBUG
		if (tc_verbose && complain)
			printf("%s (x%x) at x%x\n", "Invalid ROM width",
			       width, addr);
#endif
		return (0);
	}

	if (addr->rom_stride.value != 4) {
#ifdef DEBUG
		if (tc_verbose && complain)
			printf("%s (x%x) at x%x\n", "Invalid ROM stride",
			       addr->rom_stride.value, addr);
#endif
		return (0);
	}

	if ((addr->test_data[0] != 0x55) ||
	    (addr->test_data[4] != 0x00) ||
	    (addr->test_data[8] != 0xaa) ||
	    (addr->test_data[12] != 0xff)) {
#ifdef DEBUG
		if (tc_verbose && complain)
			printf("%s x%x\n", "Test pattern failed, option at",
			       addr);
#endif
		return (0);
	}

	for (i = 0; i < TC_ROM_LLEN; i++) {
		firmwr[i] = addr->firmware_rev[i].value;
		vendor[i] = addr->vendor_name[i].value;
		module[i] = addr->module_name[i].value;
		if (i >= TC_ROM_SLEN)
			continue;
		host_type[i] = addr->host_firmware_type[i].value;
	}
	firmwr[TC_ROM_LLEN] = vendor[TC_ROM_LLEN] =
	module[TC_ROM_LLEN] = host_type[TC_ROM_SLEN] = '\0';

#ifdef DEBUG
	if (tc_verbose)
		printf("%s %s '%s' at x%x\n %s %s %s '%s'\n %s %d %s %d %s\n",
		"Found a", vendor, module, addr,
		"Firmware rev.", firmwr,
		"diagnostics for a", host_type,
		"ROM size is", addr->rom_size.value << 3,
		"Kbytes, uses", addr->slot_size.value, "TC slot(s)");
#endif

	bcopy(module, slot->module_name, TC_ROM_LLEN);
	bcopy(vendor,  slot->module_id, TC_ROM_LLEN);
	bcopy(firmwr, &slot->module_id[TC_ROM_LLEN], TC_ROM_LLEN);
	slot->slot_size = addr->slot_size.value;
	slot->rom_width = width;

	return (1);
}

/*
 * TURBOchannel autoconf procedure.  Finds in one sweep what is
 * hanging on the bus and fills in the tc_slot_info array.
 * This is only the first part of the autoconf scheme, at this
 * time we are basically only looking for a graphics board to
 * use as system console (all workstations).
 */


void
tc_find_all_options()
{
	register int i;
	u_long addr;
	int found;
	register tc_option_t *sl;
	struct drivers_map *map;
	register struct pmax_ctlr *cp;
	register struct driver *drp;

	/*
	 * Take a look at the bus
	 */
	bzero(tc_slot_info, sizeof(tc_slot_info));
	for (i = tc_max_slot; i >= tc_min_slot;) {
		addr = MACH_PHYS_TO_UNCACHED(tc_slot_phys_base[i]);
		found = tc_probe_slot(addr, &tc_slot_info[i]);

		if (found) {
			/*
			 * Found a slot, make a note of it 
			 */
			tc_slot_info[i].present = 1;
			tc_slot_info[i].k1seg_address = addr;
		}

		i -= tc_slot_info[i].slot_size;
	}

	/*
	 * Some slots (e.g. the system slot on 3max) might require
	 * hand-filling.  If so, do it now. 
	 */
	if (tc_slot_hand_fill)
		(*tc_slot_hand_fill) (tc_slot_info);

	/*
	 * Now for each alive slot see if we have a device driver that
	 * handles it.  This is done in "priority order", meaning that
	 * always present devices are at higher slot numbers on all
	 * current TC machines, and option slots are at lowest numbers.
	 */
	for (i = TC_MAX_LOGICAL_SLOTS - 1; i >= 0; i--) {
		sl = &tc_slot_info[i];
		if (!sl->present)
			continue;
		found = FALSE;
		for (map = tc_drivers_map; map->driver_name; map++) {
			if (bcmp(sl->module_name, map->module_name, TC_ROM_LLEN))
				continue;
			sl->driver_name = map->driver_name;
			found = TRUE;
			break;
		}
		if (!found) {
			printf("%s %s %s\n", "Cannot associate a device driver to",
			       sl->module_name, ". Will (try to) ignore it.");
			sl->present = 0;
			continue;
		}

		/*
		 * Find the device driver entry and fill in the address.
		 */
		for (cp = pmax_cinit; drp = cp->pmax_driver; cp++) {
			if (strcmp(drp->d_name, map->driver_name))
				continue;
			if (cp->pmax_addr == (char *)QUES) {
				cp->pmax_addr = (char *)sl->k1seg_address;
				cp->pmax_pri = i;
				/*
				 * Only enable interrupts if there is an
				 * interrupt handler for it. (e.g., PMAG-BA 
				 * can't disable the vertical retrace interrupt
				 * and we might want to ignore it).
				 */
				if (drp->d_intr)
					(*tc_enable_interrupt)(i, 1);
				continue;
			}
			if (cp->pmax_addr != (char *)sl->k1seg_address) {
				cp->pmax_addr = (char *)QUES;
				printf("%s: device not at configued address (expected at %x, found at %x)\n",
					drp->d_name,
					cp->pmax_addr, sl->k1seg_address);
			}
		}
	}
}

/*
 * Probe a slot in the TURBOchannel. Return TRUE if a valid option
 * is present, FALSE otherwise. A side-effect is to fill the slot
 * descriptor with the size of the option, whether it is
 * recognized or not.
 */
int
tc_probe_slot(addr, slot)
	caddr_t addr;
	tc_option_t *slot;
{
	int i;
	static unsigned tc_offset_rom[] = {
		TC_OFF_PROTO_ROM, TC_OFF_ROM
	};
#define TC_N_OFFSETS	sizeof(tc_offset_rom)/sizeof(unsigned)

	slot->slot_size = 1;

	for (i = 0; i < TC_N_OFFSETS; i++) {
		if (badaddr(addr + tc_offset_rom[i], 4))
			continue;
		/* complain only on last chance */
		if (tc_identify_option((tc_rommap_t *)(addr + tc_offset_rom[i]),
		    slot, i == (TC_N_OFFSETS-1)))
			return (1);
	}
	return (0);
#undef TC_N_OFFSETS
}

/*
 * Enable/Disable interrupts for a TURBOchannel slot.
 */
void
kn02_enable_intr(slotno, on)
	register int slotno;
	int on;
{
	register volatile int *p_csr =
		(volatile int *)MACH_PHYS_TO_UNCACHED(KN02_SYS_CSR);
	int csr;
	int s;

	slotno = 1 << (slotno + KN02_CSR_IOINTEN_SHIFT);
	s = Mach_spl0();
	csr = *p_csr & ~(KN02_CSR_WRESERVED | 0xFF);
	if (on)
		*p_csr = csr | slotno;
	else
		*p_csr = csr & ~slotno;
	splx(s);
}

/*
 *	Object:
 *		kmin_enable_intr		EXPORTED function
 *
 *	Enable/Disable interrupts from a TURBOchannel slot.
 *
 *	We pretend we actually have 8 slots even if we really have
 *	only 4: TCslots 0-2 maps to slots 0-2, TCslot3 maps to
 *	slots 3-7 (see kmin_slot_hand_fill).
 */
void
kmin_enable_intr(slotno, on)
	register unsigned int slotno;
	int on;
{
	register unsigned mask;

	switch (slotno) {
	case 0:
	case 1:
	case 2:
		return;
	case KMIN_SCSI_SLOT:
		mask = (KMIN_INTR_SCSI | KMIN_INTR_SCSI_PTR_LOAD |
			KMIN_INTR_SCSI_OVRUN | KMIN_INTR_SCSI_READ_E);
		break;
	case KMIN_LANCE_SLOT:
		mask = KMIN_INTR_LANCE;
		break;
	case KMIN_SCC0_SLOT:
		mask = KMIN_INTR_SCC_0;
		break;
	case KMIN_SCC1_SLOT:
		mask = KMIN_INTR_SCC_1;
		break;
	case KMIN_ASIC_SLOT:
		mask = KMIN_INTR_ASIC;
		break;
	default:
		return;
	}
	if (on)
		kmin_tc3_imask |= mask;
	else
		kmin_tc3_imask &= ~mask;
}

/*
 *	Object:
 *		xine_enable_intr		EXPORTED function
 *
 *	Enable/Disable interrupts from a TURBOchannel slot.
 *
 *	We pretend we actually have 11 slots even if we really have
 *	only 3: TCslots 0-1 maps to slots 0-1, TCslot 2 is used for
 *	the system (TCslot3), TCslot3 maps to slots 3-10
 *	(see xine_slot_hand_fill).
 *	Note that all these interrupts come in via the IMR.
 */
void
xine_enable_intr(slotno, on)
	register unsigned int slotno;
	int on;
{
	register unsigned mask;

	switch (slotno) {
	case 0:			/* a real slot, but  */
		mask = XINE_INTR_TC_0;
		break;
	case 1:			/* a real slot, but */
		mask = XINE_INTR_TC_1;
		break;
	case XINE_FLOPPY_SLOT:
		mask = XINE_INTR_FLOPPY;
		break;
	case XINE_SCSI_SLOT:
		mask = (XINE_INTR_SCSI | XINE_INTR_SCSI_PTR_LOAD |
			XINE_INTR_SCSI_OVRUN | XINE_INTR_SCSI_READ_E);
		break;
	case XINE_LANCE_SLOT:
		mask = XINE_INTR_LANCE;
		break;
	case XINE_SCC0_SLOT:
		mask = XINE_INTR_SCC_0;
		break;
	case XINE_DTOP_SLOT:
		mask = XINE_INTR_DTOP_RX;
		break;
	case XINE_ISDN_SLOT:
		mask = XINE_INTR_ISDN;
		break;
	case XINE_ASIC_SLOT:
		mask = XINE_INTR_ASIC;
		break;
	default:
		return;/* ignore */
	}
	if (on)
		xine_tc3_imask |= mask;
	else
		xine_tc3_imask &= ~mask;
}

#ifdef DS5000_240
/*
 * UNTESTED!!
 *	Object:
 *		kn03_enable_intr		EXPORTED function
 *
 *	Enable/Disable interrupts from a TURBOchannel slot.
 *
 *	We pretend we actually have 8 slots even if we really have
 *	only 4: TCslots 0-2 maps to slots 0-2, TCslot3 maps to
 *	slots 3-7 (see kn03_slot_hand_fill).
 */
void
kn03_enable_intr(slotno, on)
	register unsigned int slotno;
	int on;
{
	register unsigned mask;

	switch (slotno) {
	case 0:
	case 1:
	case 2:
		return;
	case KN03_SCSI_SLOT:
		mask = (KN03_INTR_SCSI | KN03_INTR_SCSI_PTR_LOAD |
			KN03_INTR_SCSI_OVRUN | KN03_INTR_SCSI_READ_E);
		break;
	case KN03_LANCE_SLOT:
		mask = KN03_INTR_LANCE;
		break;
	case KN03_SCC0_SLOT:
		mask = KN03_INTR_SCC_0;
		break;
	case KN03_SCC1_SLOT:
		mask = KN03_INTR_SCC_1;
		break;
	case KN03_ASIC_SLOT:
		mask = KN03_INTR_ASIC;
		break;
	default:
		return;
	}
	if (on)
		kn03_tc3_imask |= mask;
	else
		kn03_tc3_imask &= ~mask;
}
#endif /* DS5000_240 */

/*
 *	Object:
 *		kn02_slot_hand_fill		EXPORTED function
 *
 *	Fill in by hand the info for TC slots that are non-standard.
 *	This is basically just the system slot on a 3max, it does not
 *	look to me like it follows the TC rules although some of the
 *	required info is indeed there.
 *
 */
void
kn02_slot_hand_fill(slot)
	tc_option_t *slot;
{
	slot[7].present = 1;
	slot[7].slot_size = 1;
	slot[7].rom_width = 1;
#if unsafe
	bcopy(0xbffc0410, slot[7].module_name, TC_ROM_LLEN+1);
#endif
	bcopy("KN02    ", slot[7].module_name, TC_ROM_LLEN+1);
	bcopy("DEC xxxx", slot[7].module_id, TC_ROM_LLEN+1);
	slot[7].k1seg_address = MACH_PHYS_TO_UNCACHED(KN02_SYS_DZ);
}

/*
 *	Object:
 *		kmin_slot_hand_fill		EXPORTED function
 *
 *	Fill in by hand the info for TC slots that are non-standard.
 *	This is the system slot on a 3min, which we think of as a
 *	set of non-regular size TC slots.
 *
 */
void
kmin_slot_hand_fill(slot)
	tc_option_t *slot;
{
	register int i;

	for (i = KMIN_SCSI_SLOT; i < KMIN_ASIC_SLOT+1; i++) {
		slot[i].present = 1;
		slot[i].slot_size = 1;
		slot[i].rom_width = 1;
		slot[i].unit = 0;
		bcopy("DEC KMIN", slot[i].module_id, TC_ROM_LLEN+1);
	}

	/* scsi */
	bcopy("PMAZ-AA ", slot[KMIN_SCSI_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KMIN_SCSI_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(KMIN_SYS_SCSI);

	/* lance */
	bcopy("PMAD-AA ", slot[KMIN_LANCE_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KMIN_LANCE_SLOT].k1seg_address = 0;

	/* scc */
	bcopy("Z8530   ", slot[KMIN_SCC0_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KMIN_SCC0_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(KMIN_SYS_SCC_0);

	slot[KMIN_SCC1_SLOT].unit = 1;
	bcopy("Z8530   ", slot[KMIN_SCC1_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KMIN_SCC1_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(KMIN_SYS_SCC_1);

	/* asic */
	bcopy("ASIC    ", slot[KMIN_ASIC_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KMIN_ASIC_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(KMIN_SYS_ASIC);
	asic_init(0);
}

/*
 *	Object:
 *		xine_slot_hand_fill		EXPORTED function
 *
 *	Fill in by hand the info for TC slots that are non-standard.
 *	This is the system slot on a 3min, which we think of as a
 *	set of non-regular size TC slots.
 *
 */
void
xine_slot_hand_fill(slot)
	tc_option_t *slot;
{
	register int i;

	for (i = XINE_FLOPPY_SLOT; i < XINE_FRC_SLOT+1; i++) {
		slot[i].present = 1;
		slot[i].slot_size = 1;
		slot[i].rom_width = 1;
		slot[i].unit = 0;
		bcopy("DEC XINE", slot[i].module_id, TC_ROM_LLEN+1);
	}

	/* floppy */
	bcopy("XINE-FDC", slot[XINE_FLOPPY_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_FLOPPY_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(XINE_SYS_FLOPPY);

	/* scsi */
	bcopy("PMAZ-AA ", slot[XINE_SCSI_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_SCSI_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(XINE_SYS_SCSI);

	/* lance */
	bcopy("PMAD-AA ", slot[XINE_LANCE_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_LANCE_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(XINE_SYS_LANCE);

	/* scc */
	bcopy("Z8530   ", slot[XINE_SCC0_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_SCC0_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(XINE_SYS_SCC_0);

	/* Desktop */
	bcopy("DTOP    ", slot[XINE_DTOP_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_DTOP_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(XINE_SYS_DTOP+0x20000); /* why? */

	/* ISDN */
	bcopy("AMD79c30", slot[XINE_ISDN_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_ISDN_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(XINE_SYS_ISDN);

	/* Video */
	bcopy("PMAG-DV ", slot[XINE_CFB_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_CFB_SLOT].k1seg_address =
		MACH_PHYS_TO_CACHED(XINE_PHYS_CFB_START);

	/* asic */
	bcopy("ASIC    ", slot[XINE_ASIC_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_ASIC_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(XINE_SYS_ASIC);

	/* free-running counter (high resolution mapped time) */
	bcopy("XINE-FRC", slot[XINE_FRC_SLOT].module_name, TC_ROM_LLEN+1);
	slot[XINE_FRC_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(XINE_REG_FCTR);
	asic_init(1);
}

#ifdef DS5000_240
/*
 * UNTESTED!!
 *	Object:
 *		kn03_slot_hand_fill		EXPORTED function
 *
 *	Fill in by hand the info for TC slots that are non-standard.
 *	This is the system slot on a 3max+, which we think of as a
 *	set of non-regular size TC slots.
 *
 */
void
kn03_slot_hand_fill(slot)
	tc_option_t *slot;
{
	register int i;

	for (i = KN03_SCSI_SLOT; i < KN03_ASIC_SLOT+1; i++) {
		slot[i].present = 1;
		slot[i].slot_size = 1;
		slot[i].rom_width = 1;
		slot[i].unit = 0;
		bcopy("DEC KN03", slot[i].module_id, TC_ROM_LLEN+1);
	}

	/* scsi */
	bcopy("PMAZ-AA ", slot[KN03_SCSI_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KN03_SCSI_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(KN03_SYS_SCSI);

	/* lance */
	bcopy("PMAD-AA ", slot[KN03_LANCE_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KN03_LANCE_SLOT].k1seg_address = 0;

	/* scc */
	bcopy("Z8530   ", slot[KN03_SCC0_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KN03_SCC0_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(KN03_SYS_SCC_0);

	slot[KN03_SCC1_SLOT].unit = 1;
	bcopy("Z8530   ", slot[KN03_SCC1_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KN03_SCC1_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(KN03_SYS_SCC_1);

	/* asic */
	bcopy("ASIC    ", slot[KN03_ASIC_SLOT].module_name, TC_ROM_LLEN+1);
	slot[KN03_ASIC_SLOT].k1seg_address =
		MACH_PHYS_TO_UNCACHED(KN03_SYS_ASIC);
	asic_init(0);
}
#endif /* DS5000_240 */

/*
 * Initialize the I/O asic
 */
static void
asic_init(isa_maxine)
	int isa_maxine;
{
	volatile u_int *decoder;

	/* These are common between 3min and maxine */
	decoder = (volatile u_int *)ASIC_REG_LANCE_DECODE(asic_base);
	*decoder = KMIN_LANCE_CONFIG;
}
#endif /* DS5000 */
