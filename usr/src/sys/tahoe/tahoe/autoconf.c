/*	autoconf.c	1.4	86/01/12	*/

/*
 * Setup the system to run on the current machine.
 *
 * Configure() is called at boot time and initializes the vba 
 * device tables and the memory controller monitoring.  Available
 * devices are determined (from possibilities mentioned in ioconf.c),
 * and the drivers are initialized.
 */
#include "../tahoe/pte.h"
#include "../tahoe/mem.h"
#include "../tahoe/mtpr.h"
#include "../tahoe/scb.h"

#include "vba.h"

#include "param.h"
#include "systm.h"
#include "map.h"
#include "buf.h"
#include "dk.h"
#include "vm.h"
#include "conf.h"
#include "dmap.h"

#include "../tahoevba/vbavar.h"

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	dkn;		/* number of iostat dk numbers assigned so far */
int	cold;		/* cold start flag initialized in locore.s */

/*
 * This allocates the space for the per-uba information,
 * such as buffered data path usage.
 */
struct	vba_hd vba_hd[NVBA];

/*
 * Determine mass storage configuration for a machine.
 */
configure()
{
	register int *ip;
	extern caddr_t Sysbase;

	printf("vba%d at %x\n", numvba, IOBASE);
	vbafind(numvba, (char *)vmem,(struct pte *)VMEMmap);
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
	setconf();
#endif
	swapconf();
	cold = 0;
}

/*
 * Make the controllers accessible at physical address phys
 * by mapping kernel ptes starting at pte.
 */
vbaaccess(pte, iobase, n)
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
	char *vumem;
	struct pte *memmap;
{
	register int br, cvec;			/* must be r12, r11 */
	register struct vba_device *ui;
	register struct vba_ctlr *um;
	u_short *reg;
	long addr, *ap;
	struct vba_hd *vhp;
	struct vba_driver *udp;
	int i, (**ivec)();
	extern long cold, catcher[SCB_LASTIV*2];

#ifdef lint
	br = 0; cvec = 0;
#endif
	vhp = &vba_hd[vban];
	/*
	 * Make the controllers accessible at physical address phys
	 * by mapping kernel ptes starting at pte.
	 */
	vbaaccess(memmap, IOBASE, (int)IOSIZE);
	/*
	 * Setup scb device entries to point into catcher array.
	 */
	for (i = 0; i < SCB_LASTIV; i++)
		scb.scb_devint[i] = (int (*)())&catcher[i*2];
	/*
	 * Set last free interrupt vector for devices with
	 * programmable interrupt vectors.  Use is to decrement
	 * this number and use result as interrupt vector.
	 */
	vhp->vh_lastiv = SCB_LASTIV;

	/*
	 * Check each VERSAbus mass storage controller.
	 * For each one which is potentially on this vba,
	 * see if it is really there, and if it is record it and
	 * then go looking for slaves.
	 */
#define	vbaddr(off)	(u_short *)((int)vumem + ((off) & 0x0fffff))
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
#ifdef notdef
		if (vballoc[vbaoff(addr)])
			continue;
#endif
		reg = vbaddr(addr);
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
			if ((*udp->ud_slave)(ui, reg)) {
				ui->ui_alive = 1;
				ui->ui_ctlr = um->um_ctlr;
				ui->ui_vbanum = vban;
				ui->ui_addr = (caddr_t)reg;
				ui->ui_physaddr =
				    (caddr_t)IOBASE + (addr&0x0fffff);
				if (ui->ui_dk && dkn < DK_NDRIVE)
					ui->ui_dk = dkn++;
				else
					ui->ui_dk = -1;
				ui->ui_mi = um;
				ui->ui_hd = vhp;
				/* ui_type comes from driver */
				udp->ud_dinfo[ui->ui_unit] = ui;
				printf("%s%d at %s%d slave %d\n",
				    udp->ud_dname, ui->ui_unit,
				    udp->ud_mname, um->um_ctlr,
				    ui->ui_slave);
				(*udp->ud_attach)(ui);
			}
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
		reg = vbaddr(addr);
		ui->ui_hd = vhp;
		cvec = SCB_LASTIV, cold &= ~0x2;
		i = (*udp->ud_probe)(reg, ui);
		cold |= 0x2;
		if (i == 0)
			continue;
		printf("%s%d at vba%d csr %x ",
		    ui->ui_driver->ud_dname, ui->ui_unit, vban, addr);
		if (cvec < 0 && vhp->vh_lastiv == cvec) {
			printf("no space for vector(s)\n");
			continue;
		}
		if (cvec == SCB_LASTIV) {
			printf("didn't interrupt\n");
			continue;
		}
		printf("vec %x, ipl %x\n", cvec, br);
#ifdef notdef
		while (--i >= 0)
			vballoc[vbaoff(addr+i)] = 1;
#endif
		for (ivec = ui->ui_intr; *ivec; ivec++)
			((long *)&scb)[cvec++] = (long)*ivec;
		ui->ui_alive = 1;
		ui->ui_vbanum = vban;
		ui->ui_addr = (caddr_t)reg;
		ui->ui_physaddr = (caddr_t)IOBASE + (addr&0x0fffff);
		ui->ui_dk = -1;
		/* ui_type comes from driver */
		udp->ud_dinfo[ui->ui_unit] = ui;
		(*udp->ud_attach)(ui);
		break;
	    }
	}
}

/*
 * Configure swap space and related parameters.
 */
swapconf()
{
	register struct swdevt *swp;
	register int nblks;

	for (swp = swdevt; swp->sw_dev; swp++)
		if (bdevsw[major(swp->sw_dev)].d_psize) {
			nblks =
			  (*bdevsw[major(swp->sw_dev)].d_psize)(swp->sw_dev);
			if (swp->sw_nblks == 0 || swp->sw_nblks > nblks)
				swp->sw_nblks = nblks;
		}
	if (dumplo == 0 && bdevsw[major(dumpdev)].d_psize)
		dumplo = (*bdevsw[major(dumpdev)].d_psize)(dumpdev) - physmem;
	if (dumplo < 0)
		dumplo = 0;
}
