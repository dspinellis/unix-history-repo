/*	autoconf.c	4.34	82/03/31	*/

/*
 * Setup the system to run on the current machine.
 *
 * Configure() is called at boot time and initializes the uba and mba
 * device tables and the memory controller monitoring.  Available
 * devices are determined (from possibilities mentioned in ioconf.c),
 * and the drivers are initialized.
 *
 * N.B.: A lot of the conditionals based on processor type say
 *	#if VAX780
 * and
 *	#if VAX750
 * which may be incorrect after more processors are introduced if they
 * are like either of these machines.
 *
 * TODO:
 *	use pcpu info about whether a ubasr exists
 */

#include "mba.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/nexus.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/mbareg.h"
#include "../h/mbavar.h"
#include "../h/dk.h"
#include "../h/vm.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/mtpr.h"
#include "../h/cpu.h"
#include "../h/scb.h"
#include "../h/mem.h"

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	cold;		/* if 1, still working on cold-start */
int	nexnum;		/* current nexus number */
int	dkn;		/* number of iostat dk numbers assigned so far */

/*
 * Addresses of the (locore) routines which bootstrap us from
 * hardware traps to C code.  Filled into the system control block
 * as necessary.
 */
#if NMBA > 0
int	(*mbaintv[4])() =	{ Xmba0int, Xmba1int, Xmba2int, Xmba3int };
#endif
#if VAX780
int	(*ubaintv[4])() =	{ Xua0int, Xua1int, Xua2int, Xua3int };
#endif

/*
 * This allocates the space for the per-uba information,
 * such as buffered data path usage.
 */
struct	uba_hd uba_hd[MAXNUBA];

/*
 * Determine mass storage and memory configuration for a machine.
 * Get cpu type, and then switch out to machine specific procedures
 * which will probe adaptors to see what is out there.
 */
configure()
{
	union cpusid cpusid;
	register struct percpu *ocp;
	register int *ip;
	extern char Sysbase[];

	cpusid.cpusid = mfpr(SID);
	for (ocp = percpu; ocp->pc_cputype; ocp++)
		if (ocp->pc_cputype == cpusid.cpuany.cp_type) {
			probenexus(ocp);
			/*
			 * Write protect the scb.  It is strange
			 * that this code is here, but this is as soon
			 * as we are done mucking with it, and the
			 * write-enable was done in assembly language
			 * to which we will never return.
			 */
			ip = (int *)Sysmap; *ip &= ~PG_PROT; *ip |= PG_KR;
			mtpr(TBIS, Sysbase);
#if GENERIC
			setconf();
#endif
			cold = 0;
			memenable();
			return;
		}
	printf("cpu type %d not configured\n", cpusid.cpuany.cp_type);
	asm("halt");
}

/*
 * Probe nexus space, finding the interconnects
 * and setting up and probing mba's and uba's for devices.
 */
/*ARGSUSED*/
probenexus(pcpu)
	register struct percpu *pcpu;
{
	register struct nexus *nxv;
	struct nexus *nxp = pcpu->pc_nexbase;
	union nexcsr nexcsr;
	int i;
	
	nexnum = 0, nxv = nexus;
	for (; nexnum < pcpu->pc_nnexus; nexnum++, nxp++, nxv++) {
		nxaccess(nxp, Nexmap[nexnum]);
		if (badaddr((caddr_t)nxv, 4))
			continue;
		if (pcpu->pc_nextype && pcpu->pc_nextype[nexnum] != NEX_ANY)
			nexcsr.nex_csr = pcpu->pc_nextype[nexnum];
		else
			nexcsr = nxv->nexcsr;
		if (nexcsr.nex_csr&NEX_APD)
			continue;
		switch (nexcsr.nex_type) {

		case NEX_MBA:
			printf("mba%d at tr%d\n", nummba, nexnum);
			if (nummba >= NMBA) {
				printf("%d mba's", nummba);
				goto unconfig;
			}
#if NMBA > 0
			mbafind(nxv, nxp);
			nummba++;
#endif
			break;

		case NEX_UBA0:
		case NEX_UBA1:
		case NEX_UBA2:
		case NEX_UBA3:
			printf("uba%d at tr%d\n", numuba, nexnum);
			if (numuba >= 4) {
				printf("5 uba's");
				goto unsupp;
			}
#if VAX780
			if (cpu == VAX_780)
				setscbnex(ubaintv[numuba]);
#endif
			i = nexcsr.nex_type - NEX_UBA0;
			unifind((struct uba_regs *)nxv, (struct uba_regs *)nxp,
			    umem[i], pcpu->pc_umaddr[i], UMEMmap[i]);
#if VAX780
			if (cpu == VAX_780)
				((struct uba_regs *)nxv)->uba_cr =
				    UBACR_IFS|UBACR_BRIE|
				    UBACR_USEFIE|UBACR_SUEFIE;
#endif
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
			printf("mcr%d at tr%d\n", nmcr, nexnum);
			if (nmcr >= 4) {
				printf("5 mcr's");
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
unconfig:
			printf(" not configured\n");
			continue;
		}
	}
#if VAX780
	if (cpu == VAX_780)
		{ int ubawatch(); timeout(ubawatch, (caddr_t)0, hz); }
#endif
}

#if NMBA > 0
struct	mba_device *mbaconfig();
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
	register struct mba_device *mi;
	register struct mba_slave *ms;
	int dn, dt;
	struct mba_device fnd;

	mdp = (struct mba_regs *)nxv;
	mba_hd[nummba].mh_mba = mdp;
	mba_hd[nummba].mh_physmba = (struct mba_regs *)nxp;
	setscbnex(mbaintv[nummba]);
	fnd.mi_mba = mdp;
	fnd.mi_mbanum = nummba;
	for (mbd = mdp->mba_drv, dn = 0; mbd < &mdp->mba_drv[8]; mbd++, dn++) {
		if ((mbd->mbd_ds&MBDS_DPR) == 0)
			continue;
		dt = mbd->mbd_dt & 0xffff;
		if (dt == 0)
			continue;
		if (dt == MBDT_MOH)
			continue;
		fnd.mi_drive = dn;
		if ((mi = mbaconfig(&fnd, dt)) && (dt & MBDT_TAP)) {
			for (ms = mbsinit; ms->ms_driver; ms++)
			if (ms->ms_driver == mi->mi_driver && ms->ms_alive == 0 && 
			    (ms->ms_ctlr == mi->mi_unit || ms->ms_ctlr=='?')) {
				if ((*ms->ms_driver->md_slave)(mi, ms)) {
					printf("%s%d at %s%d slave %d\n",
					    ms->ms_driver->md_sname,
					    ms->ms_unit,
					    mi->mi_driver->md_dname,
					    mi->mi_unit,
					    ms->ms_slave);
					ms->ms_alive = 1;
					ms->ms_ctlr = mi->mi_unit;
				}
			}
		}
	}
	mdp->mba_cr = MBCR_INIT;
	mdp->mba_cr = MBCR_IE;
}

/*
 * Have found a massbus device;
 * see if it is in the configuration table.
 * If so, fill in its data.
 */
struct mba_device *
mbaconfig(ni, type)
	register struct mba_device *ni;
	register int type;
{
	register struct mba_device *mi;
	register short *tp;
	register struct mba_hd *mh;

	for (mi = mbdinit; mi->mi_driver; mi++) {
		if (mi->mi_alive)
			continue;
		tp = mi->mi_driver->md_type;
		for (mi->mi_type = 0; *tp; tp++, mi->mi_type++)
			if (*tp == (type&MBDT_TYPE))
				goto found;
		continue;
found:
#define	match(fld)	(ni->fld == mi->fld || mi->fld == '?')
		if (!match(mi_drive) || !match(mi_mbanum))
			continue;
		printf("%s%d at mba%d drive %d",
		    mi->mi_driver->md_dname, mi->mi_unit,
		    ni->mi_mbanum, ni->mi_drive);
		printf("\n");
		mi->mi_alive = 1;
		mh = &mba_hd[ni->mi_mbanum];
		mi->mi_hd = mh;
		mh->mh_mbip[ni->mi_drive] = mi;
		mh->mh_ndrive++;
		mi->mi_mba = ni->mi_mba;
		mi->mi_drv = &mi->mi_mba->mba_drv[ni->mi_drive];
		mi->mi_driver->md_info[mi->mi_unit] = mi;
		mi->mi_mbanum = ni->mi_mbanum;
		mi->mi_drive = ni->mi_drive;
		if (mi->mi_dk && dkn < DK_NDRIVE)
			mi->mi_dk = dkn++;
		else
			mi->mi_dk = -1;
		(*mi->mi_driver->md_attach)(mi);
		return (mi);
	}
	return (0);
}
#endif

/*
 * Fixctlrmask fixes the masks of the driver ctlr routines
 * which otherwise save r10 and r11 where the interrupt and br
 * level are passed through.
 */
fixctlrmask()
{
	register struct uba_ctlr *um;
	register struct uba_device *ui;
	register struct uba_driver *ud;
#define	phys(a,b) ((b)(((int)(a))&0x7fffffff))

	for (um = ubminit; ud = phys(um->um_driver, struct uba_driver *); um++)
		*phys(ud->ud_probe, short *) &= ~0xc00;
	for (ui = ubdinit; ud = phys(ui->ui_driver, struct uba_driver *); ui++)
		*phys(ud->ud_probe, short *) &= ~0xc00;
}

/*
 * Find devices on a UNIBUS.
 * Uses per-driver routine to set <br,cvec> into <r11,r10>,
 * and then fills in the tables, with help from a per-driver
 * slave initialization routine.
 */
unifind(vubp, pubp, vumem, pumem, memmap)
	struct uba_regs *vubp, *pubp;
	caddr_t vumem, pumem;
	struct pte *memmap;
{
#ifndef lint
	register int br, cvec;			/* MUST BE r11, r10 */
#else
	/*
	 * Lint doesn't realize that these
	 * can be initialized asynchronously
	 * when devices interrupt.
	 */
	register int br = 0, cvec = 0;
#endif
	register struct uba_device *ui;
	register struct uba_ctlr *um;
	u_short *reg, addr;
	struct uba_hd *uhp;
	struct uba_driver *udp;
	int i, (**ivec)(), haveubasr = 0;

	/*
	 * Initialize the UNIBUS, by freeing the map
	 * registers and the buffered data path registers
	 */
	uhp = &uba_hd[numuba];
	uhp->uh_map = (struct map *)calloc(UAMSIZ * sizeof (struct map));
	rminit(uhp->uh_map, NUBMREG, 1, "uba", UAMSIZ);
	switch (cpu) {
#if VAX780
	case VAX_780:
		uhp->uh_bdpfree = (1<<NBDP780) - 1;
		haveubasr = 1;
		break;
#endif
#if VAX750
	case VAX_750:
		uhp->uh_bdpfree = (1<<NBDP750) - 1;
		break;
#endif
#if VAX7ZZ
	case VAX_7ZZ:
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
/* HAVE TO DO SOMETHING SPECIAL FOR SECOND UNIBUS ON COMETS HERE */
	if (numuba == 0)
		uhp->uh_vec = UNIvec;
	else
		uhp->uh_vec = (int(**)())calloc(512);
	for (i = 0; i < 128; i++)
		uhp->uh_vec[i] =
		    scbentry(&catcher[i*2], SCB_ISTACK);
	/*
	 * Set last free interrupt vector for devices with
	 * programmable interrupt vectors.  Use is to decrement
	 * this number and use result as interrupt vector.
	 */
	uhp->uh_lastiv = 0x200;

	/* THIS IS A CHEAT: USING THE FACT THAT UMEM and NEXI ARE SAME SIZE */
	nxaccess((struct nexus *)pumem, memmap);
#if VAX780
	if (haveubasr) {
		vubp->uba_sr = vubp->uba_sr;
		vubp->uba_cr = UBACR_IFS|UBACR_BRIE;
	}
#endif
	/*
	 * Map the first page of UNIBUS i/o
	 * space to the first page of memory
	 * for devices which will need to dma
	 * output to produce an interrupt.
	 */
	*(int *)(&vubp->uba_map[0]) = UBAMR_MRV;

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
		if (haveubasr && vubp->uba_sr) {
			vubp->uba_sr = vubp->uba_sr;
			continue;
		}
#endif
		cvec = 0x200;
		i = (*udp->ud_probe)(reg);
#if VAX780
		if (haveubasr && vubp->uba_sr) {
			vubp->uba_sr = vubp->uba_sr;
			continue;
		}
#endif
		if (i == 0)
			continue;
		printf("%s%d at uba%d csr %o ",
		    udp->ud_mname, um->um_ctlr, numuba, addr);
		if (cvec == 0) {
			printf("zero vector\n");
			continue;
		}
		if (cvec == 0x200) {
			printf("didn't interrupt\n");
			continue;
		}
		printf("vec %o, ipl %x\n", cvec, br);
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
			if ((*udp->ud_slave)(ui, reg)) {
				ui->ui_alive = 1;
				ui->ui_ctlr = um->um_ctlr;
				ui->ui_ubanum = numuba;
				ui->ui_hd = &uba_hd[numuba];
				ui->ui_addr = (caddr_t)reg;
				ui->ui_physaddr = pumem + (addr&0x1fff);
				if (ui->ui_dk && dkn < DK_NDRIVE)
					ui->ui_dk = dkn++;
				else
					ui->ui_dk = -1;
				ui->ui_mi = um;
				/* ui_type comes from driver */
				udp->ud_dinfo[ui->ui_unit] = ui;
				printf("%s%d at %s%d slave %d\n",
				    udp->ud_dname, ui->ui_unit,
				    udp->ud_mname, um->um_ctlr, ui->ui_slave);
				(*udp->ud_attach)(ui);
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
		if (haveubasr && vubp->uba_sr) {
			vubp->uba_sr = vubp->uba_sr;
			continue;
		}
#endif
		cvec = 0x200;
		i = (*udp->ud_probe)(reg, um->um_ctlr);
#if VAX780
		if (haveubasr && vubp->uba_sr) {
			vubp->uba_sr = vubp->uba_sr;
			continue;
		}
#endif
		if (i == 0)
			continue;
		printf("%s%d at uba%d csr %o ",
		    ui->ui_driver->ud_dname, ui->ui_unit, numuba, addr);
		if (cvec == 0) {
			printf("zero vector\n");
			continue;
		}
		if (cvec == 0x200) {
			printf("didn't interrupt\n");
			continue;
		}
		printf("vec %o, ipl %x\n", cvec, br);
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
		ui->ui_dk = -1;
		/* ui_type comes from driver */
		udp->ud_dinfo[ui->ui_unit] = ui;
		(*udp->ud_attach)(ui);
	}
}

setscbnex(fn)
	int (*fn)();
{
	register struct scb *scbp = &scb;

	scbp->scb_ipl14[nexnum] = scbp->scb_ipl15[nexnum] =
	    scbp->scb_ipl16[nexnum] = scbp->scb_ipl17[nexnum] =
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
	struct nexus *physa;
	register struct pte *pte;
{
	register int i = btop(sizeof (struct nexus));
	register unsigned v = btop(physa);
	
	do
		*(int *)pte++ = PG_V|PG_KW|v++;
	while (--i > 0);
	mtpr(TBIA, 0);
}
