/*	autoconf.c	4.2	81/02/10	*/

#define	dprintf	printf

/*
 * Configure the system for your own VAX.
 * Mostly used for distribution systems,
 * but parts of this run always.
 *
 *	kre/wnj		Berkeley, February 1981
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
	- arrange permission to write SCB & give it back  (KLUDGED)
	- set up dk fields in structs
	- make locore.s compatible (incl Scbbase -> _Scbbase)
  ***/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/nexus.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/mba.h"
#include "../h/uba.h"
#include "../h/mtpr.h"
#include "../h/cpu.h"
#include "../h/scb.h"
#include "../h/vmparam.h"
#include "../h/vmmac.h"
#include "../h/mem.h"

int	mbanum;		/* counts MBA's as we see them */
int	numuba;		/* same for UBA's */
int	nexnum;		/* current nexus number */

struct	uba_regs *curuba;
int	catcher[129];
extern	caddr_t	umaddr[];
extern	struct pte UMEMmap[4][16];
extern	char	umem[4][16*128*512];

/* I somehow don't think this will work on a 750 */
#define	C (caddr_t)
caddr_t	umaddr[4] = {
	C 0x2013e000, C 0x2017e000, C 0x201be000, C 0x201fe000
};

extern	Xmba0int(), Xmba1int(), Xmba2int(), Xmba3int();
extern	Xua0int(),  Xua1int(),  Xua2int(),  Xua3int();

int	(*mbaintv[4])() = {
	Xmba0int, Xmba1int, Xmba2int,
#if VAX780
				   Xmba3int
#endif
};

#if VAX780
int	(*ubaintv[4])() = {
	Xua0int, Xua1int, Xua2int, Xua3int
};

extern	int	(*UNIvec[])();
#endif

int	c780();
int	c750();
int	c7ZZ();
int	c8ZZ();

struct percpu percpu[] = {
#if VAX780
	c780,	VAX_780,	4,	1,	4,
#endif
#if VAX750
	c750,	VAX_750,	3,	0,	1,
#endif
#if VAX7ZZ
	c7ZZ,	VAX_7ZZ,	0,	0,	1,
#endif
#if VAX8ZZ
	c8ZZ,	VAX_8ZZ,	4,	4,	4,
#endif
};
#define	NCPU	(sizeof(percpu)/sizeof(struct percpu))

/*
 * Determine mass storage and memory configuration for a machine.
 * Get cpu type, and then switch out to machine specific procedures
 * which will probe adaptors to see what is out there.
 */
configure()
{
	union cpusid cpusid;
	register struct percpu *ocp;

	cpusid.cpusid = mfpr(SID);
	for (ocp = percpu; ocp < &percpu[NCPU]; ocp++)
		if (ocp->pc_cputype == cpusid.cpuany.cp_type) {
			cpu = ocp->pc_cputype;
			(*ocp->pc_config)(ocp);
			/*** SET CATCHER FOR STRAY INTERRUPTS HERE ***/
			/*** NB: NOT VECTORS: JUST HANDLER CODE ***/
			panic("config done\n");
			return;
		}
	printf("cpu type %d unsupported\n", cpusid.cpuany.cp_type);
	asm("halt");
}

#if VAX750
c750(ocp)
	register struct percpu *ocp;
{
	printf("not yet, sad to say\n");
	asm("halt");
}
#endif

#if VAX780
/*
 * Build configuration table for a 780, by looking
 * at the things (mbas and ubas) in the nexus slots
 * and initialzing each appropriately.
 */
c780(pcpu)
	register struct percpu *pcpu;
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
		switch (nexcsr.nex_type) {

		case NEX_MBA:
			if (mbanum >= pcpu->pc_maxmba) {
				printf("%d mba's", pcpu->pc_maxmba+1);
				goto unsupp;
			}
			mbafind(nxv, nxp);
			mbanum++;
			break;

		case NEX_UBA0:
		case NEX_UBA1:
		case NEX_UBA2:
		case NEX_UBA3:
			if (numuba >= pcpu->pc_maxuba) {
				printf("%d uba's", pcpu->pc_maxuba+1);
				goto unsupp;
			}
			uba_hd[numuba].uh_bdpfree = 0x7fff;	/* 15 bdp's */
			uba_hd[numuba].uh_uba = (struct uba_regs *)nxv;
			uba_hd[numuba].uh_physuba = (struct uba_regs *)nxp;
			if (numuba == 0)
				uba_hd[0].uh_vec = UNIvec;
			else {
				/** WE JUST KNOW THIS WON'T HAPPEN **/
				uba_hd[numuba].uh_vec = 0;
				/* mapinit() */
			}
			i = nexcsr.nex_type - NEX_UBA0;
			nxaccess(umaddr[i], UMEMmap[numuba]);
			unifind((struct uba_regs *)nxv, umaddr[i], umem[i]);
			setscbnex(nexnum, ubaintv[numuba]);
			numuba++;
			break;

		case NEX_DR32:
			printf("dr32");
			goto unsupp;

		case NEX_MEM4:
		case NEX_MEM4I:
		case NEX_MEM16:
		case NEX_MEM16I:
			if (nmcr >= pcpu->pc_maxmcr) {
				printf("%d mcr's", pcpu->pc_maxmcr+1);
				goto unsupp;
			}
			mcraddr[nmcr++] = (struct mcr *)nxv;
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
			printf(" unsupported (at tr %d)\n", nexnum);
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
mbafind(nxv, nxp)
	struct nexus *nxv;
	struct nexus *nxp;
{
	register struct mba_regs *mdp;
	register struct mba_drv *mbd;
	int dn, dt, sn, ds;
	struct mba_info	fnd;

	mdp = (struct mba_regs *)nxv;
	mba_hd[mbanum].mh_mba = mdp;
	mba_hd[mbanum].mh_physmba = (struct mba_regs *)nxp;
	setscbnex(nexnum, mbaintv[mbanum]);
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
	mdp->mba_cr = MBAINIT;
	mdp->mba_cr = MBAIE;
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

/*
 * Find mass storage devices on a UNIBUS.
 */
unifind(ubp, puba, vuba)
	struct uba_regs *ubp;
	caddr_t puba;
	caddr_t vuba;
{
	register br, cvec;			/* MUST BE r11, r10 */
	register struct uba_dinfo *ui;
	register u_short *uba = (u_short *)vuba, *sp;
	struct uba_driver *udp;
	u_short *reg;
	register i;
	u_short addr;

#if VAX780
	if (ubp) {
		ubp->uba_sr = ubp->uba_sr;
		curuba = ubp;
	}
#endif
#if VAX750
	if (ubp == 0)
		setvecs();
#endif
	for (ui = ubdinit; udp = ui->ui_driver; ui++) {
		if (ui->ui_ubanum != numuba && ui->ui_ubanum != '?')
			continue;
		addr = (u_short)ui->ui_addr;
		sp = udp->ud_addr;
		for (; addr || *sp; addr = 0) {
#define	ubaddr(off)	(u_short *)((int)uba + ((off)&0x1fff))
			if (addr == 0)
				addr = *sp++;
			reg = ubaddr(addr);
			if (badaddr((caddr_t)reg, 2))
				continue;
#if VAX780
			if (ubp) {
				if (ubp->uba_sr) {
					ubp->uba_sr = ubp->uba_sr;
					continue;
				}
				ubatstvec(ubp);
			}
#endif
			cvec = 0x200;
			i = (*udp->ud_cntrlr)(ui, reg);
#if VAX780
			if (ubp) {
				ubp->uba_cr = 0;
				if (ubp->uba_sr) {
					ubp->uba_sr = ubp->uba_sr;
					continue;
				}
			}
#endif
			if (i == 0)
				continue;
			dprintf("\tLocated %c at %o ",
			    ui->ui_name, addr);
			if (cvec == 0) {
				dprintf("zero uba vector\n");
				continue;
			}
			if (cvec == 0x200) {
				dprintf("didn't interrupt\n");
				continue;
			}
			dprintf("vector %o, ipl %x\n", cvec, br);
			if (ui->ui_slave != '?') {
				if ((*udp->ud_slave)(ui, reg, ui->ui_slave))
					goto ubdevfnd;
				continue;
			}
			for (i = 0; i < udp->ud_maxslave; i++) {
				if ((*udp->ud_slave)(ui, reg, i)) {
					int	(**ivec)();

					ui->ui_slave = i;
    ubdevfnd:
					ui->ui_alive = 1;
					ui->ui_ubanum = numuba;
					ui->ui_hd = &uba_hd[numuba];
					ui->ui_addr = (caddr_t)reg;
					ui->ui_physaddr = puba + (addr&0x1fff);
					ui->ui_dk = 0;
					/* ui_type comes from driver */
					udp->ud_info[ui->ui_unit] = ui;
					dprintf("\tslave %d\n", ui->ui_slave);
					for (ivec=udp->ud_intr; *ivec; ivec++) {
						caddr_t cp;
						int fn;

						if ((cp = calloc(12)) == 0)
							panic("nm/iv\n");
						ui->ui_hd->uh_vec[cvec] =
						    scbentry((int (*)()) cp,
							SCB_ISTACK);
						*cp++ = 0xbb; *cp++ = 0xff;
						*cp++ = 0xdd;
						*cp++ = ui->ui_unit&0x3f;
						*cp++ = 1; *cp++ = 0x9f;
						fn = (int)*ivec;
						for (i=0; i<4; i++)
							*cp++ = fn, fn >>= 4;
						*cp = 0x02;
					}
					break;
				}
			}
		}
	}
}

#if VAX750
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

	for (i = 0; i < 128; i++) {
		catcher[i] = 0x015a04c2;	/* subl2 $4,r10; nop */
		Scbbase.scb_ubaint[i] = 
		    scbentry((int (*)())&catcher[i], SCB_ISTACK);
	}
	catcher[i] = 0x025b12db;		/* mfpr $IPL,r11; rei */
}
#endif

#if VAX780
/*
 * Init for testing vector addresses on a
 * machine that has a UNIBUS adaptor to recieve interrupts
 */
ubatstvec(ubp)
	register struct uba_regs *ubp;
{
	register struct scb *sp = &Scbbase;
	extern int Xconfuaint();
	
	sp->scb_ipl14[nexnum] = sp->scb_ipl15[nexnum] =
	    sp->scb_ipl16[nexnum] = sp->scb_ipl17[nexnum] =
		scbentry(Xconfuaint, SCB_ISTACK);
	ubp->uba_cr = UBA_IFS|UBA_BRIE;
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
