/*	autoconf.c	4.6	81/02/17	*/

/*
 * Configure the system for the current machine.
 */

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

/*
 * Definitions for uba's and mba's.
 */

/*
 * Catcher is 1k bytes of memory where we write in stray
 * UNIBUS interrupt decoding code.
 */
extern	int catcher[256];

/*
 * UMEMmap maps the UNIBUS memory for the portion accessible
 * as device registers and umem is the corresponding virtual space.
 */
extern	struct pte UMEMmap[MAXNUBA][16];
extern	char umem[MAXNUBA][16*NBPG];

/*
 * The umaddr* arrays give the physical addresses of the UNIBUS
 * register space.
 */
#if VAX780
caddr_t	umaddr780[4] = {
	(caddr_t) 0x2013e000, (caddr_t) 0x2017e000,
	(caddr_t) 0x201be000, (caddr_t) 0x201fe000
};
#endif
#if VAX750
caddr_t	umaddr750[1] = {
	(caddr_t) 0xffe000
};
#endif

/*
 * The ?baintv arrays give the addresses of the UNIBUS and
 * MASSBUS interrupt handling routines.
 */
extern	Xmba0int(),	Xmba1int(),	Xmba2int(),	Xmba3int();
extern	Xua0int();
#if VAX780
extern			Xua1int(),	Xua2int(),	Xua3int();
#endif

int	(*mbaintv[4])() = {
	Xmba0int,	Xmba1int,	Xmba2int,	Xmba3int
};
int	(*ubaintv[])() = {
#if VAX780
	Xua0int,	Xua1int,	Xua2int,	Xua3int
#endif
#if VAX750
	Xua0int
#endif
};

/*
 * UNIvec is permanently allocated in scb.s.  On an 750, it is
 * where the hardware sends the interrupts; on a 780 we use the
 * space for the first UNIBUS adaptor.
 */
extern	int	(*UNIvec[])();

/*
 * Structure per cpu type giving address of configuration routine
 */
int	c780(), c750();

struct percpu percpu[] = {
#if VAX780
	c780, VAX_780,
#endif
#if VAX750
	c750, VAX_750,
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
	register int i;

	cpusid.cpusid = mfpr(SID);
	for (ocp = percpu; ocp < &percpu[NCPU]; ocp++)
		if (ocp->pc_cputype == cpusid.cpuany.cp_type) {
			cpu = ocp->pc_cputype;
			(*ocp->pc_config)(ocp);
			for (i = 2; i < 256; i += 2) {
				catcher[i] = catcher[0];
				catcher[i+1] = catcher[1] - i*4;
			}
#if GENERIC
			setconf();
#endif
			/* WRITE PROTECT SCB */
			return;
		}
	printf("cpu type %d unsupported\n", cpusid.cpuany.cp_type);
	asm("halt");
}

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
	struct nexus *nxp = NEX780;
	union nexcsr nexcsr;
	int i, ubawatch();
	
	for (nexnum = 0,nxv = nexus; nexnum < NNEX780; nexnum++,nxp++,nxv++) {
		nxaccess((caddr_t)nxp, Nexmap[nexnum]);
		if (badaddr((caddr_t)nxv, 4))
			continue;
		nexcsr = nxv->nexcsr;
		if (nexcsr.nex_csr&NEX_APD)
			continue;
		switch (nexcsr.nex_type) {

		case NEX_MBA:
			if (nummba >= 4) {
				printf("5 mba's");
				goto unsupp;
			}
			mbafind(nxv, nxp);
			nummba++;
			break;

		case NEX_UBA0:
		case NEX_UBA1:
		case NEX_UBA2:
		case NEX_UBA3:
			if (numuba >= 4) {
				printf("5 uba's");
				goto unsupp;
			}
			i = nexcsr.nex_type - NEX_UBA0;
			unifind((struct uba_regs *)nxv, (struct uba_regs *)nxp,
			    umem[i], umaddr780[i]);
			setscbnex(nexnum, ubaintv[numuba]);
			((struct uba_regs *)nxv)->uba_cr =
			    UBA_IFS|UBA_BRIE|UBA_USEFIE|UBA_SUEFIE;
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
			if (nmcr >= 4) {
				printf("%d mcr's", 4);
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
	timeout(ubawatch, 0, HZ);
}
#endif

#if VAX750
/*
 * Configure a 750.  There are four possible mba's,
 * one standard UNIBUS, and a memory controller.
 */
c750(pcpu)
	struct percpu *pcpu;
{
	register struct nexus *nxv = nexus;
	struct nexus *nxp = NEX750;

	for (nexnum = 0; nexnum < NNEX750; nexnum++, nxp++, nxv++) {
		nxaccess((caddr_t)nxp, Nexmap[nexnum]);
		if (badaddr((caddr_t)nxv, 4))
			continue;
		mbafind(nxv, nxp);
		nummba++;
	}
	nxaccess((caddr_t)nxp, Nexmap[nexnum++]);
	unifind((struct uba_regs *)nxv++, (struct uba_regs *)nxp,
	    umem[0], umaddr750[0]);
	numuba = 1;
	nxaccess((caddr_t)MCR_750, Nexmap[nexnum]);
	mcraddr[nmcr++] = nxv;
}
#endif

/*
 * Find devices attached to a particular mba
 * and look for each device found in the massbus
 * initialization tables.
 */
mbafind(nxv, nxp)
	struct nexus *nxv, *nxp;
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
		printf("%c at mba%d drive %d ",
		    mi->mi_name, ni->mi_mbanum, ni->mi_drive);
		if (ni->mi_slave >= 0)
			printf("slave %d ", ni->mi_slave);
		printf("is unit %d\n", mi->mi_unit);
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
 * Fixctlrmask fixes the masks of the driver ctlr routines
 * which otherwise save r10 and r11 where the interrupt and br
 * level are passed through.
 */
fixctlrmask()
{
	register struct uba_minfo *um;
	register struct uba_dinfo *ui;
	register struct uba_driver *ud;
#define	phys(a,b) ((b)(((int)(a))&0x7fffffff))

	for (um = ubminit; ud = phys(um->um_driver, struct uba_driver *); um++)
		*phys(ud->ud_cntrlr, short *) &= ~0xc00;
	for (ui = ubdinit; ud = phys(ui->ui_driver, struct uba_driver *); ui++)
		*phys(ud->ud_cntrlr, short *) &= ~0xc00;
}

/*
 * Find devices on a UNIBUS.
 * Uses per-driver routine to set <br,cvec> into <r11,r10>,
 * and then fills in the tables, with help from a per-driver
 * slave initialization routine.
 */
unifind(vubp, pubp, vumem, pumem)
	struct uba_regs *vubp, *pubp;
	caddr_t vumem, pumem;
{
	register int br, cvec;			/* MUST BE r11, r10 */
	register struct uba_dinfo *ui;
	register struct uba_minfo *um;
	u_short *umem = (u_short *)vumem, *sp, *reg, addr;
	struct uba_hd *uhp;
	struct uba_driver *udp;
	int i, (**ivec)();

	/*
	 * Initialize the UNIBUS, by freeing the map
	 * registers and the buffered data path registers
	 */
	uhp = &uba_hd[numuba];
	uhp->uh_map = (struct map *)calloc(UAMSIZ * sizeof (struct map));
	mfree(uhp->uh_map, NUBMREG, 1);
	switch (cpu) {
#if VAX780
	case VAX_780:
		uhp->uh_bdpfree = (1<<15) - 1;		/* 15 bdp's */
		break;
#endif
#if VAX750
	case VAX_750:
		uhp->uh_bdpfree = (1<<3) - 1;		/* 3 bdp's */
		break;
#endif
	}

	/*
	 * Save virtual and physical addresses
	 * of adaptor, and allocate and initialize
	 * the UNIBUS interrupt vector.
	 */
	uhp->uh_uba = vubp;
	uhp->uh_physuba = pubp;
	if (numuba == 0)
		uhp->uh_vec = UNIvec;
	else
		uhp->uh_vec = (int(**)())calloc(512);
	for (i = 0; i < 128; i++)
		uhp->uh_vec[i] =
		    scbentry(&catcher[i*2], SCB_ISTACK);
	nxaccess((struct nexus *)pumem, UMEMmap[numuba]);
#if VAX780
	if (cpu == VAX_780) {
		vubp->uba_sr = vubp->uba_sr;
		curuba = vubp;
	}
#endif
#if VAX750
	if (cpu != VAX_780)
		setvecs();
#endif
	/*
	 * Map the first page of UNIBUS i/o
	 * space to the first page of memory
	 * for devices which will need to dma
	 * output to produce an interrupt.
	 */
	*(int *)(&vubp->uba_map[0]) = UBA_MRV;

#define	ubaddr(off)	(u_short *)((int)vumem + ((off)&0x1fff))
	/*
	 * Check each unibus mass storage controller.
	 * For each one which is potentially on this uba,
	 * see if it is really there, and if it is record it and
	 * then go looking for slaves.
	 */
	for (um = ubminit; udp = um->um_driver; um++) {
		if (um->um_ubanum != numuba && um->um_ubanum != '?')
			continue;
		addr = (u_short)um->um_addr;
		reg = ubaddr(addr);
		if (badaddr((caddr_t)reg, 2))
			continue;
#if VAX780
		if (vax == VAX_780) {
			if (vubp->uba_sr) {
				vubp->uba_sr = vubp->uba_sr;
				continue;
			}
			ubatstvec(vubp);
		}
#endif
		cvec = 0x200;
		i = (*udp->ud_cntrlr)(um, reg);
#if VAX780
		if (vax == VAX_780) {
			if (vubp->uba_sr) {
				vubp->uba_sr = vubp->uba_sr;
				continue;
			}
		}
#endif
		if (i == 0)
			continue;
		printf("%s %d at uba%d %o ",
		    udp->ud_pname, um->um_ctlr, numuba, addr);
		if (cvec == 0) {
			printf("zero vector\n");
			continue;
		}
		if (cvec == 0x200) {
			printf("didn't interrupt\n");
			continue;
		}
		printf("vector %o, ipl %x\n", cvec, br);
		um->um_alive = 1;
		um->um_ubanum = numuba;
		um->um_hd = &uba_hd[numuba];
		um->um_addr = (caddr_t)reg;
		udp->ud_minfo[um->um_ctlr] = um;
		for (ivec = um->um_intr; *ivec; ivec++) {
			um->um_hd->uh_vec[cvec/4] =
			    scbentry(*ivec, SCB_ISTACK);
			cvec += 4;
		}
		for (ui = ubdinit; ui->ui_driver; ui++) {
			if (ui->ui_driver != udp || ui->ui_alive ||
			    ui->ui_ctlr != um->um_ctlr && ui->ui_ctlr != '?' ||
			    ui->ui_ubanum != numuba && ui->ui_ubanum != '?')
				continue;
			if ((*udp->ud_slave)(ui, reg, ui->ui_slave)) {
				ui->ui_alive = 1;
				ui->ui_ctlr = um->um_ctlr;
				ui->ui_ubanum = numuba;
				ui->ui_hd = &uba_hd[numuba];
				ui->ui_addr = (caddr_t)reg;
				ui->ui_physaddr = pumem + (addr&0x1fff);
				ui->ui_dk = 0;
				ui->ui_mi = um;
				/* ui_type comes from driver */
				udp->ud_dinfo[ui->ui_unit] = ui;
				printf("%s%d slave %d is unit %d\n",
				    udp->ud_pname, um->um_ctlr,
				    ui->ui_slave, ui->ui_unit);
			}
		}
	}
	/*
	 * Now look for non-mass storage peripherals.
	 */
	for (ui = ubdinit; udp = ui->ui_driver; ui++) {
		if (ui->ui_ubanum != numuba && ui->ui_ubanum != '?' ||
		    ui->ui_alive || ui->ui_slave != -1)
			continue;
		addr = (u_short)ui->ui_addr;
		reg = ubaddr(addr);
		if (badaddr((caddr_t)reg, 2))
			continue;
#if VAX780
		if (vax == VAX_780) {
			if (vubp->uba_sr) {
				vubp->uba_sr = vubp->uba_sr;
				continue;
			}
			ubatstvec(vubp);
		}
#endif
		cvec = 0x200;
		i = (*udp->ud_cntrlr)(ui, reg);
#if VAX780
		if (vax == VAX_780) {
			if (vubp->uba_sr) {
				vubp->uba_sr = vubp->uba_sr;
				continue;
			}
		}
#endif
		if (i == 0)
			continue;
		printf("%s at uba%d %o ",
		    ui->ui_driver->ud_pname, numuba, addr);
		if (cvec == 0) {
			printf("zero vector\n");
			continue;
		}
		if (cvec == 0x200) {
			printf("didn't interrupt\n");
			continue;
		}
		printf("vector %o, ipl %x\n", cvec, br);
		ui->ui_hd = &uba_hd[numuba];
		for (ivec = ui->ui_intr; *ivec; ivec++) {
			ui->ui_hd->uh_vec[cvec/4] =
			    scbentry(*ivec, SCB_ISTACK);
			cvec += 4;
		}
		ui->ui_alive = 1;
		ui->ui_ubanum = numuba;
		ui->ui_addr = (caddr_t)reg;
		ui->ui_physaddr = pumem + (addr&0x1fff);
		ui->ui_dk = 0;
		/* ui_type comes from driver */
		udp->ud_dinfo[ui->ui_unit] = ui;
		(*udp->ud_slave)(ui, reg);
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
nxaccess(physa, pte)
	caddr_t physa;
	register struct pte *pte;
{
	register int cnt = btop(sizeof (struct nexus));
	register unsigned v = btop(physa);
	
	do
		*(int *)pte++ = PG_V|PG_KW|v++;
	while (--cnt > 0);
	mtpr(TBIA, 0);
}
