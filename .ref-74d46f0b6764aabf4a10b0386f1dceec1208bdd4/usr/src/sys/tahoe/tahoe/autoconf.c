/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)autoconf.c	7.5 (Berkeley) %G%
 */

/*
 * Setup the system to run on the current machine.
 *
 * Configure() is called at boot time and initializes the vba 
 * device tables and the memory controller monitoring.  Available
 * devices are determined (from possibilities mentioned in ioconf.c),
 * and the drivers are initialized.
 */
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/dkstat.h"
#include "sys/vm.h"
#include "sys/conf.h"
#include "sys/dmap.h"
#include "sys/reboot.h"
#include "sys/malloc.h"

#include "../include/pte.h"
#include "mem.h"
#include "../include/mtpr.h"
#include "scb.h"

#include "vba.h"
#ifdef SECSIZE
#include "file.h"
#include "ioctl.h"
#include "disklabel.h"
#endif SECSIZE

#include "pte.h"
#include "mem.h"
#include "mtpr.h"
#include "scb.h"

#include "vba.h"

#include "../vba/vbavar.h"
#include "../vba/vbaparam.h"

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	dkn;		/* number of iostat dk numbers assigned so far */
int	cold;		/* cold start flag initialized in locore.s */

/*
 * This allocates the space for the per-vba information.
 */
struct	vba_hd vba_hd[NVBA];

/*
 * Determine i/o configuration for a machine.
 */
configure()
{
	register int *ip;
	extern caddr_t Sysbase;

	vbafind(numvba, (caddr_t)vmem, VMEMmap);
	numvba++;
	/*
	 * Write protect the scb.  It is strange
	 * that this code is here, but this is as soon
	 * as we are done mucking with it, and the
	 * write-enable was done in assembly language
	 * to which we will never return.
	 */
	ip = (int *)&Sysmap[2]; *ip &= ~PG_PROT; *ip |= PG_KR;
	mtpr(TBIS, Sysbase+2*NBPG);
#if GENERIC
	if ((boothowto & RB_ASKNAME) == 0)
		setroot();
	setconf();
#else
	setroot();
#endif
	/*
	 * Configure swap area and related system
	 * parameter based on device(s) used.
	 */
	swapconf();
	cold = 0;
}

/*
 * Make the controllers accessible at physical address phys
 * by mapping kernel ptes starting at pte.
 */
vbaccess(pte, iobase, n)
	register struct pte *pte;
	caddr_t iobase;
	register int n;
{
	register unsigned v = btop(iobase);
	
	do
		*(int *)pte++ = PG_V|PG_KW|v++;
	while (--n > 0);
	mtpr(TBIA, 0);
}

/*
 * Fixctlrmask fixes the masks of the driver ctlr routines
 * which otherwise save r11 and r12 where the interrupt and br
 * level are passed through.
 */
fixctlrmask()
{
	register struct vba_ctlr *vm;
	register struct vba_device *vi;
	register struct vba_driver *vd;
#define	phys(a,b) ((b)(((int)(a))&~0xc0000000))

	vm = phys(vbminit, struct vba_ctlr *);
	for (; vd = phys(vm->um_driver, struct vba_driver *); vm++)
		*phys(vd->ud_probe, short *) &= ~0x1800;
	vi = phys(vbdinit, struct vba_device *);
	for (; vd = phys(vi->ui_driver, struct vba_driver *); vi++)
		*phys(vd->ud_probe, short *) &= ~0x1800;
}

/*
 * Find devices on the VERSAbus.
 * Uses per-driver routine to see who is on the bus
 * and then fills in the tables, with help from a per-driver
 * slave initialization routine.
 */
vbafind(vban, vumem, memmap)
	int vban;
	caddr_t vumem;
	struct pte memmap[];
{
	register int br, cvec;			/* must be r12, r11 */
	register struct vba_device *ui;
	register struct vba_ctlr *um;
	u_short *reg;
	long addr, *ap;
	struct vba_hd *vhp;
	struct vba_driver *udp;
	int i, octlr, (**ivec)();
	caddr_t valloc;
	extern quad catcher[SCB_LASTIV];

#ifdef lint
	br = 0; cvec = 0;
#endif
	vhp = &vba_hd[vban];
	/*
	 * Make the controllers accessible at physical address phys
	 * by mapping kernel ptes starting at pte.
	 */
	vbaccess(memmap, (caddr_t)VBIOBASE, (int)VBIOSIZE);
	printf("vba%d at %x\n", vban, VBIOBASE);
	/*
	 * Setup scb device entries to point into catcher array.
	 */
	for (i = 0; i < SCB_LASTIV; i++)
		scb.scb_devint[i] = (int (*)())((int)&catcher[i]);
	/*
	 * Set last free interrupt vector for devices with
	 * programmable interrupt vectors.  Use is to decrement
	 * this number and use result as interrupt vector.
	 */
	vhp->vh_lastiv = SCB_LASTIV;
	/*
	 * Grab some memory to record the address space we allocate,
	 * so we can be sure not to place two devices at the same address.
	 * Register I/O space is allocated in 256-byte sections,
	 * and memory I/O space is in 4Kb sections.  We record allocations
	 * in 256-byte sections.
	 *
	 * We could use just 1/8 of this (we only want a 1 bit flag) but
	 * we are going to give it back anyway, and that would make the
	 * code here bigger (which we can't give back), so ...
	 */
#define	VSECT(a)	((a) / 0x100)
#define	VSIZE(s)	(((s) + 0xff) / 0x100)
#define	VALLOC(a)	(valloc[VSECT(vboff(a))])
#define	VMAPSIZE	VSIZE(ctob(VBIOSIZE))
	valloc = (caddr_t)malloc((u_long)(VMAPSIZE), M_TEMP, M_NOWAIT);
	if (valloc == (caddr_t)0)
		panic("no mem for vbafind");
	bzero(valloc, VMAPSIZE);

	/*
	 * Check each VERSAbus mass storage controller.
	 * For each one which is potentially on this vba,
	 * see if it is really there, and if it is record it and
	 * then go looking for slaves.
	 */
#define	vbaddr(off)	(u_short *)(vumem + vboff(off))
	for (um = vbminit; udp = um->um_driver; um++) {
		if (um->um_vbanum != vban && um->um_vbanum != '?')
			continue;
		/*
		 * Use the particular address specified first,
		 * or if it is given as "0", if there is no device
		 * at that address, try all the standard addresses
		 * in the driver until we find it.
		 */
		addr = (long)um->um_addr;
	    for (ap = udp->ud_addr; addr || (addr = *ap++); addr = 0) {
		if (VBIOMAPPED(addr)) {
			if (VALLOC(addr))
				continue;
			reg = vbaddr(addr);
		} else
			reg = (u_short *)addr;
		um->um_hd = vhp;
		cvec = SCB_LASTIV, cold &= ~0x2;
		i = (*udp->ud_probe)(reg, um);
		cold |= 0x2;
		if (i == 0)
			continue;
		printf("%s%d at vba%d csr %x ",
		    udp->ud_mname, um->um_ctlr, vban, addr);
		if (cvec < 0 && vhp->vh_lastiv == cvec) {
			printf("no space for vector(s)\n");
			continue;
		}
		if (cvec == SCB_LASTIV) {
			printf("didn't interrupt\n");
			continue;
		}
		printf("vec %x, ipl %x\n", cvec, br);
		csralloc(valloc, addr, i);
		um->um_alive = 1;
		um->um_vbanum = vban;
		um->um_addr = (caddr_t)reg;
		udp->ud_minfo[um->um_ctlr] = um;
		for (ivec = um->um_intr; *ivec; ivec++)
			((long *)&scb)[cvec++] = (long)*ivec;
		for (ui = vbdinit; ui->ui_driver; ui++) {
			if (ui->ui_driver != udp || ui->ui_alive ||
			    ui->ui_ctlr != um->um_ctlr && ui->ui_ctlr != '?' ||
			    ui->ui_vbanum != vban && ui->ui_vbanum != '?')
				continue;
			octlr = ui->ui_ctlr, ui->ui_ctlr = um->um_ctlr;
			if ((*udp->ud_slave)(ui, reg)) {
				ui->ui_alive = 1;
				ui->ui_ctlr = um->um_ctlr;
				ui->ui_vbanum = vban;
				ui->ui_addr = (caddr_t)reg;
				ui->ui_physaddr = (caddr_t)addr;
				if (ui->ui_dk && dkn < DK_NDRIVE)
					ui->ui_dk = dkn++;
				else
					ui->ui_dk = -1;
				ui->ui_mi = um;
				ui->ui_hd = vhp;
				/* ui_type comes from driver */
				udp->ud_dinfo[ui->ui_unit] = ui;
				printf("%s%d at %s%d slave %d",
				    udp->ud_dname, ui->ui_unit,
				    udp->ud_mname, um->um_ctlr,
				    ui->ui_slave);
				(*udp->ud_attach)(ui);
				printf("\n");
			} else
				ui->ui_ctlr = octlr;
		}
		break;
	    }
	}
	/*
	 * Now look for non-mass storage peripherals.
	 */
	for (ui = vbdinit; udp = ui->ui_driver; ui++) {
		if (ui->ui_vbanum != vban && ui->ui_vbanum != '?' ||
		    ui->ui_alive || ui->ui_slave != -1)
			continue;
		addr = (long)ui->ui_addr;
	    for (ap = udp->ud_addr; addr || (addr = *ap++); addr = 0) {
		if (VBIOMAPPED(addr)) {
			if (VALLOC(addr))
				continue;
			reg = vbaddr(addr);
		} else
			reg = (u_short *)addr;
		ui->ui_hd = vhp;
		cvec = SCB_LASTIV, cold &= ~0x2;
		i = (*udp->ud_probe)(reg, ui);
		cold |= 0x2;
		if (i == 0)
			continue;
		printf("%s%d at vba%d csr %x ",
		    ui->ui_driver->ud_dname, ui->ui_unit, vban, addr);
		if (ui->ui_intr) {
			if (cvec < 0 && vhp->vh_lastiv == cvec) {
				printf("no space for vector(s)\n");
				continue;
			}
			if (cvec == SCB_LASTIV) {
				printf("didn't interrupt\n");
				continue;
			}
			printf("vec %x, ipl %x\n", cvec, br);
			for (ivec = ui->ui_intr; *ivec; ivec++)
				((long *)&scb)[cvec++] = (long)*ivec;
		} else
			printf("no interrupts\n");
		csralloc(valloc, addr, i);
		ui->ui_alive = 1;
		ui->ui_vbanum = vban;
		if (VBIOMAPPED(addr))
			ui->ui_addr = (caddr_t)reg;
		ui->ui_physaddr = (caddr_t)addr;
		ui->ui_dk = -1;
		/* ui_type comes from driver */
		udp->ud_dinfo[ui->ui_unit] = ui;
		(*udp->ud_attach)(ui);
		break;
	    }
	}
	free(valloc, M_TEMP);
}

/*
 * Mark addresses starting at addr and continuing
 * size bytes as allocated in the map.
 * Warn if the new allocation overlaps a previous allocation.
 */
csralloc(valloc, addr, size)
	caddr_t valloc;
	long addr;
	register int size;
{
	register caddr_t p;
	int warned = 0;

	if (!VBIOMAPPED(addr))
		return;
	size = VSIZE(size);
	p = &VALLOC(addr) + size;
	while (--size >= 0) {
		if (*--p && !warned) {
			printf(
	"WARNING: device registers overlap those for a previous device\n");
			warned = 1;
		}
		*p = 1;
	}
}

/*
 * Tahoe VERSAbus adapator support routines.
 */

caddr_t	vbcur = (caddr_t)&vbbase;
int	vbx = 0;
/*
 * Allocate page tables for mapping intermediate i/o buffers.
 * Called by device drivers during autoconfigure.
 */
vbmapalloc(npf, ppte, putl)
	int npf;
	struct pte **ppte;
	caddr_t *putl;
{

	if (vbcur + npf*NBPG > (caddr_t)&vbend)
		return (0);
	*ppte = &VBmap[vbx];
	*putl = vbcur;
	vbx += npf;
	vbcur += npf*NBPG;
	return (1);
}

caddr_t	vbmcur = (caddr_t)&vmem1;
int	vbmx = 0;
/*
 * Allocate page tables and map VERSAbus i/o space.
 * Called by device drivers during autoconfigure.
 */
vbmemalloc(npf, addr, ppte, putl)
	int npf;
	caddr_t addr;
	struct pte **ppte;
	caddr_t *putl;
{

	if (vbmcur + npf*NBPG > (caddr_t)&vmemend)
		return (0);
	*ppte = &VMEMmap1[vbmx];
	*putl = vbmcur;
	vbmx += npf;
	vbmcur += npf*NBPG;
	vbaccess(*ppte, addr, npf);		/* map i/o space */
	return (1);
}

/*
 * Configure swap space and related parameters.
 */
#ifndef SECSIZE
swapconf()
{
	register struct swdevt *swp;
	register int nblks;

	for (swp = swdevt; swp->sw_dev; swp++)
		if (bdevsw[major(swp->sw_dev)].d_psize) {
			nblks =
			  (*bdevsw[major(swp->sw_dev)].d_psize)(swp->sw_dev);
			if (nblks != -1 &&
			    (swp->sw_nblks == 0 || swp->sw_nblks > nblks))
				swp->sw_nblks = nblks;
		}
	dumpconf();
}
#else SECSIZE
swapconf()
{
	register struct swdevt *swp;
	register int nblks;
	register int bsize;
	struct partinfo dpart;

	for (swp = swdevt; swp->sw_dev; swp++)
		if ((nblks = psize(swp->sw_dev, &swp->sw_blksize,
		    &swp->sw_bshift)) != -1 &&
		    (swp->sw_nblks == 0 || swp->sw_nblks > nblks))
			swp->sw_nblks = nblks;

	if (!cold)	/* In case called for addition of another drive */
		return;
	if (dumplo == 0) {
		nblks = psize(dumpdev, (int *)0, (int *)0);
		if (nblks == -1 || nblks < ctod(physmem))
			dumplo = 0;
		else
			dumplo = nblks - ctod(physmem);
	}
}

/*
 * Return size of disk partition in DEV_BSIZE units.
 * If needed, return sector size.
 */
psize(dev, psize, pshift)
	register dev_t dev;
	int *psize, *pshift;
{
	register int nblks, bsize, bshift;
	struct partinfo dpart;

	if ((*bdevsw[major(dev)].d_ioctl)(dev, DIOCGPART,
	    (caddr_t)&dpart, FREAD) == 0)
		bsize = dpart.disklab->d_secsize;
	else
		bsize = DEV_BSIZE;
	if (psize)
		*psize = bsize;
	bshift = 0;
	for (nblks = DEV_BSIZE / bsize; nblks > 1; nblks >>= 1)
		bshift++;
	if (pshift)
		*pshift = bshift;
	nblks = -1;
	if (bdevsw[major(dev)].d_psize) {
		nblks = (*bdevsw[major(dev)].d_psize)(dev);
		if (nblks != -1)
			nblks >>= bshift;
	}
	return (nblks);
}
#endif SECSIZE

#define	DOSWAP			/* change swdevt, argdev, and dumpdev too */
u_long	bootdev;		/* should be dev_t, but not until 32 bits */

static	char devname[][2] = {
	0,0,		/* 0 = ud */
	'd','k',	/* 1 = vd */
	0,0,		/* 2 = xp */
};

#define	PARTITIONMASK	0x7
#define	PARTITIONSHIFT	3

/*
 * Attempt to find the device from which we were booted.
 * If we can do so, and not instructed not to do so,
 * change rootdev to correspond to the load device.
 */
setroot()
{
	int  majdev, mindev, unit, part, controller, adaptor;
	dev_t temp, orootdev;
	struct swdevt *swp;

	if (boothowto & RB_DFLTROOT ||
	    (bootdev & B_MAGICMASK) != (u_long)B_DEVMAGIC)
		return;
	majdev = B_TYPE(bootdev);
	if (majdev >= sizeof(devname) / sizeof(devname[0]))
		return;
	adaptor = B_ADAPTOR(bootdev);
	controller = B_CONTROLLER(bootdev);
	part = B_PARTITION(bootdev);
	unit = B_UNIT(bootdev);
	/*
	 * Search Versabus devices.
	 *
	 * WILL HAVE TO DISTINGUISH VME/VERSABUS SOMETIME
	 */
	{
		register struct vba_device *vbap;

		for (vbap = vbdinit; vbap->ui_driver; vbap++)
			if (vbap->ui_alive && vbap->ui_slave == unit &&
			   vbap->ui_ctlr == controller &&
			   vbap->ui_vbanum == adaptor &&
			   vbap->ui_driver->ud_dname[0] == devname[majdev][0] &&
			   vbap->ui_driver->ud_dname[1] == devname[majdev][1])
				break;
		if (vbap->ui_driver == 0)
			return;
		mindev = vbap->ui_unit;
	}
	mindev = (mindev << PARTITIONSHIFT) + part;
	orootdev = rootdev;
	rootdev = makedev(majdev, mindev);
	/*
	 * If the original rootdev is the same as the one
	 * just calculated, don't need to adjust the swap configuration.
	 */
	if (rootdev == orootdev)
		return;
	printf("changing root device to %c%c%d%c\n",
		devname[majdev][0], devname[majdev][1],
		mindev >> PARTITIONSHIFT, part + 'a');
#ifdef DOSWAP
	mindev &= ~PARTITIONMASK;
	for (swp = swdevt; swp->sw_dev; swp++) {
		if (majdev == major(swp->sw_dev) &&
		    mindev == (minor(swp->sw_dev) & ~PARTITIONMASK)) {
			temp = swdevt[0].sw_dev;
			swdevt[0].sw_dev = swp->sw_dev;
			swp->sw_dev = temp;
			break;
		}
	}
	if (swp->sw_dev == 0)
		return;
	/*
	 * If argdev and dumpdev were the same as the old primary swap
	 * device, move them to the new primary swap device.
	 */
	if (temp == dumpdev)
		dumpdev = swdevt[0].sw_dev;
	if (temp == argdev)
		argdev = swdevt[0].sw_dev;
#endif
}
