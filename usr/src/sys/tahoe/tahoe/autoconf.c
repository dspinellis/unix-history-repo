/*	autoconf.c	1.1	85/07/21	*/

/*
 * Setup the system to run on the current machine.
 *
 * Configure() is called at boot time and initializes the vba 
 * device tables and the memory controller monitoring.  Available
 * devices are determined (from possibilities mentioned in ioconf.c),
 * and the drivers are initialized.
 *
 */

#include "../machine/pte.h"
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/buf.h"
#include "../h/dk.h"
#include "../h/vm.h"
#include "../h/conf.h"
#include "../h/dmap.h"
#include "../machine/mem.h"
#include "../machine/mtpr.h"
#include "../vba/vbavar.h"

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	dkn;		/* number of iostat dk numbers assigned so far */

/*
 * Determine mass storage configuration for a machine.
 */
configure()
{
	register int *ip;
	extern char Sysbase[];

	printf("vba%d at 0x%x\n", numvba, IOBASE);
	vbafind((char *)vmem,(struct pte *)VMEMmap);
	/*
	 * Write protect the scb.  It is strange
	 * that this code is here, but this is as soon
	 * as we are done mucking with it, and the
	 * write-enable was done in assembly language
	 * to which we will never return.
	 */
	ip = (int *)&Sysmap[2]; *ip &= ~PG_PROT; *ip |= PG_KR;
	mtpr(Sysbase+0x800, TBIS);
#if GENERIC
	setconf();
#endif
	swapconf();
}


/*
 * Make the controllers accessible at physical address phys
 * by mapping kernel ptes starting at pte.
 */

ioaccess(pte,iobase,iosize)
	register struct pte *pte;
	register caddr_t iobase;
	register int	iosize;
{
	register int i = iosize;	/* number of ptes to map */
	register unsigned v = btop(iobase);
	
	do
		*(int *)pte++ = PG_V|PG_KW|v++;
	while (--i > 0);
	mtpr(0, TBIA);
}


/*
 * Find devices on the BUS.
 * Uses per-driver routine to see who is on the bus
 * and then fills in the tables, with help from a per-driver
 * slave initialization routine.
 */

int	iospace_mapped = 0;

vbafind(vumem, memmap)
	char *vumem;
	struct pte *memmap;
{
	register struct vba_device *ui;
	register struct vba_ctlr *um;
	u_short *reg;
	long  addr;
	struct vba_driver *udp;
	int i;

	/*
	 * Make the controllers accessible at physical address phys
	 * by mapping kernel ptes starting at pte.
	 */
	ioaccess(memmap,IOBASE,IOSIZE);
	iospace_mapped = 1;
#define	vbaddr(off)	(u_short *)((int)vumem + ((off) & 0x0fffff))


	/*
	 * Check each VERSAbus mass storage controller.
	 * For each one which is potentially on this vba,
	 * see if it is really there, and if it is record it and
	 * then go looking for slaves.
	 */
	for (um = vbminit; udp = um->um_driver; um++) {
		if (um->um_vbanum != numvba && um->um_vbanum != '?')
			continue;
		addr = (long)um->um_addr;
		reg = vbaddr(addr);
		i = (*udp->ud_probe)(reg);
		if (i == 0)
			continue;
		printf("%s%d at csr 0x%x\n",
		    udp->ud_mname, um->um_ctlr, addr);
		um->um_alive = 1;
		um->um_vbanum = numvba;
		um->um_addr = (caddr_t)reg;
		udp->ud_minfo[um->um_ctlr] = um;
		for (ui = vbdinit; ui->ui_driver; ui++) {
			if (ui->ui_driver != udp || ui->ui_alive ||
			    ui->ui_ctlr != um->um_ctlr && ui->ui_ctlr != '?' ||
			    ui->ui_vbanum != numvba && ui->ui_vbanum != '?')
				continue;
			if ((*udp->ud_slave)(ui, reg)) {
				ui->ui_alive = 1;
				ui->ui_ctlr = um->um_ctlr;
				ui->ui_vbanum = numvba;
				ui->ui_addr = (caddr_t)reg;
				ui->ui_physaddr = (caddr_t)IOBASE + (addr&0x0fffff);
				if (ui->ui_dk && dkn < DK_NDRIVE)
					ui->ui_dk = dkn++;
				else
					ui->ui_dk = -1;
				ui->ui_mi = um;
				/* ui_type comes from driver */
				udp->ud_dinfo[ui->ui_unit] = ui;
				printf("%s%d at %s%d slave %d\n",
				    udp->ud_dname, ui->ui_unit,
				    udp->ud_mname, um->um_ctlr,
				    ui->ui_slave);
				(*udp->ud_attach)(ui);
			}
		}
	}
	/*
	 * Now look for non-mass storage peripherals.
	 */
	for (ui = vbdinit; udp = ui->ui_driver; ui++) {
		if (ui->ui_vbanum != numvba && ui->ui_vbanum != '?' ||
		    ui->ui_alive || ui->ui_slave != -1)
			continue;
		addr = (long)ui->ui_addr;
		reg = vbaddr(addr);
		if (badaddr((caddr_t)reg, 2))
			continue;
		i = (*udp->ud_probe)(reg);
		if (i == 0)
			continue;
		printf("%s%d at csr 0x%x\n",
		    ui->ui_driver->ud_dname, ui->ui_unit, addr);
		ui->ui_alive = 1;
		ui->ui_vbanum = numvba;
		ui->ui_addr = (caddr_t)reg;
		ui->ui_physaddr = (caddr_t)IOBASE + (addr&0x0fffff);
		ui->ui_dk = -1;
		/* ui_type comes from driver */
		udp->ud_dinfo[ui->ui_unit] = ui;
		(*udp->ud_attach)(ui);
	}
}


#define	DMMIN	32
#define	DMMAX	1024
#define	DMTEXT	1024
#define	MAXDUMP	(10*2048)
/*
 * Configure swap space and related parameters.
 */
swapconf()
{
	register struct swdevt *swp;
	register int nblks;

	for (swp = swdevt; swp->sw_dev; swp++) {
		if (bdevsw[major(swp->sw_dev)].d_psize)
			nblks =
			  (*bdevsw[major(swp->sw_dev)].d_psize)(swp->sw_dev);
		if (swp->sw_nblks == 0 || swp->sw_nblks > nblks)
			swp->sw_nblks = nblks;
	}
	if (dumplo == 0 && bdevsw[major(dumpdev)].d_psize)
		dumplo = (*bdevsw[major(dumpdev)].d_psize)(dumpdev) - MAXDUMP;
	if (dumplo < 0)
		dumplo = 0;
	if (dmmin == 0)
		dmmin = DMMIN;
	if (dmmax == 0)
		dmmax = DMMAX;
	if (dmtext == 0)
		dmtext = DMTEXT;
	if (dmtext > dmmax)
		dmtext = dmmax;
}
