/*	autoconf.c	4.1	81/02/08	*/

#define	dprintf printf
/*
 * discover whatever we can about the machine we are running on
 */

 /*** NOT DONE YET 
	- unibus map & bdp map setup
	- SCB setup (init mba intr vecs, uba intr vecs, & (750) uba dev vecs)
	  stray interrupt setup in scb
	- ctrlr, & slave routines in uba drivers (+ other fixups)
		(nb: the cntrlr routines must have no reg vars)
	- unibus intr vec setup
	- (probably) lots more wrt UBA's
	- 750 main loop
	- arrange permission to write SCB & give it back
	- set up dk fields in structs
	- make locore.s compatible (incl Scbbase -> _Scbbase)
  ***/

#include "../h/param.h"
#include "../h/ino.h"
#include "../h/inode.h"
#include "../h/map.h"
#include "../h/nexus.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/mba.h"
#include "../bert/uba.h"			/*** TEMPORARY ***/
#include "../h/mtpr.h"
#include "../h/cpu.h"
#include "../h/scb.h"
#include "../h/vmparam.h"
#include "../h/vmmac.h"

int	mbanum;		/* counts MBA's as we see them */
#if	VAX==780 || VAX==ANY
int	ubanum;		/* same for UBA's */
#endif
int	memctl;		/* and memory controllers */
int	nexnum;		/* current nexus number */

extern cpu;
struct	nexus *nxtemp();
extern struct scb Scbbase;
struct uba_regs *curuba;
extern struct pte Nexmap[16][16];
extern struct nexus nexus[16];
int	catcher[129];

extern	mba0int(), mba1int(), mba2int(), mba3int();
extern	ua0int(),  ua1int(),  ua2int(),  ua3int();

int	(*mbaintv[4])() = {
	mba0int, mba1int, mba2int
#if	VAX==780 || VAX==ANY
	    , mba3int
#endif
};

#if	VAX==780 || VAX == ANY
int	(*ubaintv[4])() = {
	ua0int, ua1int, ua2int, ua3int
};

extern	int	(*UNIvec[])();
#endif

/*
 * Determine mass storage and memory configuration for a machine.
 * Get cpu type, and then switch out to machine specific procedures
 * which will probe adaptors to see what is out there.
 */
configure()
{
	union cpusid cpusid;

	cpusid.cpusid = mfpr(SID);
	switch (cpusid.cpuany.cp_type) {

#if	VAX==780 || VAX==ANY
	case VAX_780: cpu = 780; c780(); break;
#endif
#if	VAX==750 || VAX==ANY
	case VAX_750: cpu = 750; c750(); break;
#endif

	default:
		printf("cpu type %d unsupported\n", cpusid.cpuany.cp_type);
		panic("config");
	}
	asm("halt");
}

#if	VAX==750 || VAX==ANY
c750()
{
	printf("not yet, sad to say\n");
	asm("halt");
}
#endif

#if	VAX==780 || VAX==ANY
/*
 * Build configuration table for a 780, by looking
 * at the things (mbas and ubas) in the nexus slots
 * and initialzing each appropriately.
 */
c780()
{
	register struct nexus *nxv;
	struct nexus *nxp = NEXBASE;
	union nexcsr nexcsr;
	int i;
	
	for (nexnum = 0, nxv = nexus; nexnum < NNEXUS; nexnum++, nxp++, nxv++) {
		nxaccess((caddr_t)nxp, Nexmap[nexnum]);
		if (badaddr((caddr_t)nxv, 4))
			continue;
		nexcsr = nxv->nexcsr;
		if (nexcsr.nex_csr&NEX_APD)
			continue;
		dprintf("nexus %d\n", nexnum);
		switch (nexcsr.nex_type) {

		case NEX_MBA:
			mba_hd[mbanum].mh_mba = (struct mba_regs *)nxv;
			mba_hd[mbanum].mh_physmba = (struct mba_regs *)nxp;
			mbafind(nxv);
			setscbnex(nexnum, mbaintv[mbanum]);
			((struct mba_regs *)nxv)->mba_cr = MBAINIT;
			((struct mba_regs *)nxv)->mba_cr = MBAIE;
			mbanum++;
			break;

		case NEX_UBA0:
		case NEX_UBA1:
		case NEX_UBA2:
		case NEX_UBA3:
			uba_hd[ubanum].uh_uba = (struct uba_regs *)nxv;
			uba_hd[ubanum].uh_physuba = (struct uba_regs *)nxp;
			ubafind(nxv, nexcsr.nex_type - NEX_UBA0);
			setscbnex(nexnum, ubaintv[ubanum]);
			if (ubanum == 0)
				uba_hd[0].uh_vec = UNIvec;
#ifdef	notyet
			else {
				uba_hd[ubanum].uh_vec =
				    (int (**)())memall(NBPG);	/*?????*/
		/*** FILL IN uh_vec with something useful !!! */
			}
			mapinit(/* some parameters I suppose*/);
#endif
			ubanum++;
			break;

		case NEX_DR32:
			printf("dr32");
			goto unsupp;

		case NEX_MEM4:
		case NEX_MEM4I:
		case NEX_MEM16:
		case NEX_MEM16I:
				/* What is memfind supposed to do ???? */
#ifdef	notyet
			memfind(memctl++);
#endif
			break;

		case NEX_MPM0:
		case NEX_MPM1:
		case NEX_MPM2:
		case NEX_MPM3:
			printf("mpm");
			goto unsupp;

		default:
			printf("nexus type %x", nexcsr.nex_type);
    unsupp:
			printf(" at tr %d unsupported\n", nexnum);
			continue;
		}
	}
}
#endif

/*
 * Find devices attached to a particular mba
 * and look for each device found in the massbus
 * initialization tables.
 */
mbafind(nxp)
	struct nexus *nxp;
{
	register struct mba_regs *mdp;
	register struct mba_drv *mbd;
	int dn, dt, sn, ds;
	struct mba_info	fnd;

	mdp = (struct mba_regs *)nxp;
	fnd.mi_mba = mdp;
	fnd.mi_mbanum = mbanum;
	for (mbd = mdp->mba_drv, dn = 0; mbd < &mdp->mba_drv[8]; mbd++, dn++) {
		dt = mbd->mbd_dt & 0xffff;
		if (dt == 0)
			continue;
		ds = mbd->mbd_ds;
		if ((dt&MBDT_TYPE) == MBDT_TU78) {
			printf("tm04/tu78 unsupported\n");
			continue;
		}
		if (dt == MBDT_MOH)
			continue;
		fnd.mi_drive = dn;
		if (dt & MBDT_TAP) {
			for (sn = 0; sn < 8; sn++) {
				mbd->mbd_tc = sn;
				dt = mbd->mbd_dt;
				if ((dt & MBDT_SPR) == 0)
					continue;
				dt &= MBDT_TYPE;
				fnd.mi_slave = sn;
				mbaconfig(&fnd, dt);
			}
		} else {
			fnd.mi_slave = -1;
			mbaconfig(&fnd, dt&MBDT_TYPE);
		}
	}
}

/*
 * Have found a massbus device;
 * see if it is in the configuration table.
 * If so, fill in its data.
 */
mbaconfig(ni, type)
	register struct mba_info *ni;
	register int type;
{
	register struct mba_info *mi;
	register short *tp;

	dprintf("mbaconfig %x\n", type);
	for (mi = mbinit; mi->mi_driver; mi++) {
		if (mi->mi_alive)
			continue;
		tp = mi->mi_driver->md_type;
		for (mi->mi_type = 0; *tp; tp++, mi->mi_type++)
			if (*tp == type)
				goto found;
		continue;
found:
#define	match(fld)	(ni->fld == mi->fld || mi->fld == '?')
		if (!match(mi_slave) || !match(mi_drive) || !match(mi_mbanum))
			continue;
		mi->mi_alive = 1;
		mi->mi_hd = &mba_hd[ni->mi_mbanum];
		mba_hd[ni->mi_mbanum].mh_mbip[ni->mi_drive] = mi;
		mi->mi_mba = ni->mi_mba;
		mi->mi_drv = &mi->mi_mba->mba_drv[ni->mi_drive];
		mi->mi_driver->md_info[mi->mi_unit] = mi;
		mi->mi_mbanum = ni->mi_mbanum;
		mi->mi_drive = ni->mi_drive;
		mi->mi_slave = ni->mi_slave;
	}
}

ubafind(nxp, i)
	struct nexus *nxp;
{
	register br, cvec;			/* MUST BE r11, r10 */
	register struct uba_regs *ubp = (struct uba_regs *)nxp;
	register short *uba;
	register struct uba_info *ui;
	register u_short *sp;
	struct uba_driver *udp;
	short *reg;
	int i;

	uba = (short *)(PHYSUDEV0 + i * PHYSUDEVSZ - 0160000);
	return;			/******** ZZZZZZZZZZZ *******/
#if	VAX==ANY || VAX==780
	if (cpu == 780) {
		ubp->uba_sr = ubp->uba_sr;
		curuba = ubp;
	}
#endif
#if	VAX==ANY || VAX==750
	setvecs();
#endif
	for (ui = ubinit; udp = ui->ui_driver; ui++) {
		if (ui->ui_ubanum != ubanum && ui->ui_ubanum != '?')
			continue;
		for (sp = udp->ud_addr; *sp; sp++) {
#define	ubaddr(i)	(short *)((int)uba + (i))
			reg = ubaddr(*sp);
			if (badaddr((caddr_t)reg, 2))
				continue;
#if	VAX==780 || VAX==ANY
			if (cpu == 780) {
				if (ubp->uba_sr) {
					ubp->uba_sr = ubp->uba_sr;
					continue;
				}
				ubatstvec(ubp);
			}
#endif
			cvec = 0x200;
			i = (*udp->ud_cntrlr)(ui, reg);
#if	VAX==780 || VAX==ANY
			if (cpu == 780) {
				ubp->uba_cr = 0;
				if (ubp->uba_sr) {
					ubp->uba_sr = ubp->uba_sr;
					continue;
				}
			}
#endif
			if (i == 0)
				continue;
			printf("\tLocated %c at %o ", ui->ui_name, *sp);
			if (cvec == 0) {
				printf("zero uba vector\n");
				continue;
			}
			if (cvec == 0x200) {
				printf("didn't interrupt\n");
				continue;
			}
			printf("vector %o, ipl %x\n", cvec, br);
			if (ui->ui_slave == -1)
				goto ubdevfnd;
			if (ui->ui_slave != '?') {
				if ((*udp->ud_slave)(ui, reg, ui->ui_slave))
					goto ubdevfnd;
				continue;
			}
			for (i = 0; i < udp->ud_maxslave; i++) {
				if ((*udp->ud_slave)(ui, reg, i)) {
					ui->ui_slave = i;
    ubdevfnd:
					ui->ui_alive = 1;
					ui->ui_ubanum = ubanum;
					ui->ui_hd = &uba_hd[ubanum];
					ui->ui_addr = (caddr_t)reg;
					/* there must be more, surely !!! */
					/* NB: it is drivers responsibility  */
					/* to fill in ui_type if it wants it */
					break;
				}
			}
		}
	}
}

#if	VAX==750 || VAX==ANY
/*
 * For machines which vector unibus interrupts directly,
 * we must spray the unibus vector with pointers to distinct
 * functions.  We use the space normally used to catch stray
 * interrupts (which hasn't been set up) as a subroutine
 * with a number of entry points, with increment register
 * instructions between entry points to tell where we entered.
 */
setvecs()
{
	register int i;

	if (cpu == 780)
		return;
	for (i = 0; i < 128; i++) {
		catcher[i] = 0x015a04c2;	/* subl2 $4,r10; nop */
		Scbbase.scb_ubaint[i] = 
		    scbentry((int (*)())&catcher[i], SCB_ISTACK);
		    /**** WHAT IS scbentry() ???? ****/
	}
	catcher[i] = 0x025b12db;		/* mfpr $IPL,r11; rei */
}
#endif

#if	VAX==780 || VAX==ANY
/*
 * The routine used to catch br 4/5/6/7 interrupts
 * on vaxen with unibus adaptors.  This looks at the
 * resulting vector register to tell where the interrupt
 * occurred.
 */
ubaintr()
{
	register int br, cvec;		/* MUST BE r11, r10 */
	int ubaintr0();

asm(".align 2");
asm(".globl _ubaintr0");
asm("_ubaintr0:");
	br = mfpr(IPL);
	cvec = curuba->uba_brrvr[br-0x14] & 0xffff;
	{ asm("rei"); }
}

/*
 * Init for testing vector addresses on a
 * machine where interrupts are vectored through a uba.
 */
ubatstvec(ubp)
	register struct uba_regs *ubp;
{
	register struct scb *sp = &Scbbase;
	
	sp->scb_ipl14[nexnum] = sp->scb_ipl15[nexnum] =
	    sp->scb_ipl16[nexnum] = sp->scb_ipl17[nexnum] =
		scbentry(ubaintr0, SCB_ISTACK);
	ubp->uba_cr = IFS|BRIE;
}
#endif

setscbnex(nexnum, fn)
	int	(*fn)();
{
	register struct scb *sp = &Scbbase;

	sp->scb_ipl14[nexnum] = sp->scb_ipl15[nexnum] =
	    sp->scb_ipl16[nexnum] = sp->scb_ipl17[nexnum] =
		scbentry(fn, SCB_ISTACK);
}

/*
 * Make a nexus accessible at physical address phys
 * by mapping kernel ptes starting at pte.
 *
 * WE LEAVE ALL NEXI MAPPED; THIS IS PERHAPS UNWISE
 * SINCE MISSING NEXI DONT RESPOND.  BUT THEN AGAIN
 * PRESENT NEXI DONT RESPOND TO ALL OF THEIR ADDRESS SPACE.
 */
nxaccess(phys, pte)
	caddr_t phys;
	register struct pte *pte;
{
	register int cnt = btop(sizeof (struct nexus));
	register unsigned v = btop(phys);
	
	do
		*(int *)pte++ = PG_V|PG_KW|v++;
	while (--cnt > 0);
	mtpr(TBIA, 0);
}
