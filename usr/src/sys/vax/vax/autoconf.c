/*	autoconf.c	4.3	81/02/15	*/

#define	dprintf	printf

/*
 * Configure the system for the current machine.
 *
 *	kre/wnj		Berkeley, February 1981
 */

 /*** NOT DONE YET 
	- stray interrupt setup in scb and uba vectors
	- 750 main loop
	- write protect scb
	- set up dk fields in structs
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

int	nexnum;		/* current nexus number */

struct	uba_regs *curuba;
extern	int catcher[256];
extern	struct pte UMEMmap[MAXNUBA][16];
extern	char umem[MAXNUBA][16*NBPG];

#define	C (caddr_t)
#if VAX780
caddr_t	umaddr780[4] = {
	C 0x2013e000, C 0x2017e000, C 0x201be000, C 0x201fe000
};
#endif
#if VAX750
caddr_t	umaddr750[1] = {
	C 0xffe000
};
#endif

extern	Xmba0int(),	Xmba1int(),	Xmba2int(),	Xmba3int();
extern	Xua0int();
#if VAX780
extern			Xua1int(),	Xua2int(),	Xua3int();
#endif

int	(*mbaintv[])() = {
	Xmba0int,	Xmba1int,	Xmba2int,	Xmba3int
};

int	(*ubaintv[4])() = {
#if VAX780
	Xua0int,	Xua1int,	Xua2int,	Xua3int
#endif
#if VAX750
	Xua0int
#endif
};

extern	int	(*UNIvec[])();

int	c780();
int	c750();
int	c7ZZ();
int	c980();

struct percpu percpu[] = {
#if VAX780
	c780,	VAX_780,	4,	4,	4,	umaddr780,
#endif
#if VAX750
	c750,	VAX_750,	3,	0,	1,	umaddr750,
#endif
#if VAX7ZZ
	c7ZZ,	VAX_7ZZ,	0,	0,	1,	umaddr7ZZ,
#endif
#if VAX980
	c980,	VAX_980,	4,	4,	4,	umaddr980,
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
	int catcher0 = catcher[0];
	int catcher1 = catcher[1];
	register int i;

	cpusid.cpusid = mfpr(SID);
	for (ocp = percpu; ocp < &percpu[NCPU]; ocp++)
		if (ocp->pc_cputype == cpusid.cpuany.cp_type) {
			cpu = ocp->pc_cputype;
			(*ocp->pc_config)(ocp);
			for (i = 0; i < 256; i += 2) {
				catcher[i] = catcher0;
				catcher[i+1] = catcher1 - i*4;
			}
#if VAXANY
			/*** SET CONFIGURATION ***/
#endif
			asm("halt");
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
	register struct uba_hd *uhp;
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
			if (nummba >= pcpu->pc_maxmba) {
				printf("%d mba's", pcpu->pc_maxmba+1);
				goto unsupp;
			}
			mbafind(nxv, nxp);
			nummba++;
			break;

		case NEX_UBA0:
		case NEX_UBA1:
		case NEX_UBA2:
		case NEX_UBA3:
			if (numuba >= pcpu->pc_maxuba) {
				printf("%d uba's", pcpu->pc_maxuba+1);
				goto unsupp;
			}
			/* THIS BELONGS IN unifind() */
			uhp = &uba_hd[numuba];
			mfree(uhp->uh_map, NUBMREG, 1);
			uhp->uh_bdpfree = 0x7fff;	/* 15 bdp's */
			uhp->uh_uba = (struct uba_regs *)nxv;
			uhp->uh_physuba = (struct uba_regs *)nxp;
			if (numuba == 0)
				uhp->uh_vec = UNIvec;
			else
				uhp->uh_vec = (int(**)())calloc(512);
			for (i = 0; i < 128; i++)
				uhp->uh_vec[i] =
				    scbentry(&catcher[i*2], SCB_ISTACK);
			i = nexcsr.nex_type - NEX_UBA0;
			nxaccess(pcpu->pc_umaddr[i], UMEMmap[numuba]);
			unifind((struct uba_regs *)nxv, pcpu->pc_umaddr[i],
			    umem[i]);
			ubainit((struct uba_regs *)nxv);
			/* END OF CODE WHICH BELONGS IN unifind() */
			setscbnex(nexnum, ubaintv[numuba]);
			numuba++;
			break;

		case NEX_DR32:
		/* there can be more than one... are there other codes??? */
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
	mba_hd[nummba].mh_mba = mdp;
	mba_hd[nummba].mh_physmba = (struct mba_regs *)nxp;
	setscbnex(nexnum, mbaintv[nummba]);
	fnd.mi_mba = mdp;
	fnd.mi_mbanum = nummba;
	for (mbd = mdp->mba_drv, dn = 0; mbd < &mdp->mba_drv[8]; mbd++, dn++) {
		dt = mbd->mbd_dt & 0xffff;
		if (dt == 0)
			continue;
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
			*(int *)(&ubp->uba_map[0]) = UBA_MRV;
			i = (*udp->ud_cntrlr)(ui, reg);
#if VAX780
			if (ubp) {
				ubp->uba_cr = 0;
				if (ubp->uba_sr) {
					dprintf("uba_sr %x\n", ubp->uba_sr);
					ubp->uba_sr = ubp->uba_sr;
					continue;
				}
			}
#endif
			if (i == 0)
				continue;
			dprintf("\tLocated %s at %o ",
			    ui->ui_driver->ud_pname, addr);
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
				dprintf("slave %d refused\n", ui->ui_slave);
				continue;
			}
			for (i = 0; i < udp->ud_maxslave; i++) {
				if ((*udp->ud_slave)(ui, reg, i)) {
					register int	(**ivec)();

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
					for (ivec=ui->ui_intr; *ivec; ivec++) {
						ui->ui_hd->uh_vec[cvec/4] =
						    scbentry(*ivec, SCB_ISTACK);
						cvec += 4;
					}
					break;
				}
				dprintf("slave %d refused\n", i);
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
