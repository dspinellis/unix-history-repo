/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: Utah $Hdr: autoconf.c 1.31 91/01/21$
 *
 *	@(#)autoconf.c	8.1 (Berkeley) 6/10/93
 */

/*
 * Setup the system to run on the current machine.
 *
 * Configure() is called at boot time.  Available
 * devices are determined (from possibilities mentioned in ioconf.c),
 * and the drivers are initialized.
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/dkstat.h>
#include <sys/conf.h>
#include <sys/dmap.h>
#include <sys/reboot.h>

#include <machine/cpu.h>
#include <pmax/dev/device.h>
#include <pmax/pmax/pmaxtype.h>
#include <pmax/pmax/turbochannel.h>

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	cold = 1;	/* if 1, still working on cold-start */
int	dkn;		/* number of iostat dk numbers assigned so far */
int	cpuspeed = 30;	/* approx # instr per usec. */
extern	int pmax_boardtype;
extern	tc_option_t tc_slot_info[TC_MAX_LOGICAL_SLOTS];

/*
 * Determine mass storage and memory configuration for a machine.
 * Get cpu type, and then switch out to machine specific procedures
 * which will probe adaptors to see what is out there.
 */
configure()
{
	register struct pmax_ctlr *cp;
	register struct scsi_device *dp;
	register struct driver *drp;
	register int i;

	/* print what type of CPU and FPU we have */

	/*
	 * for some reason the Pmax has an R2000 cpu with an implementation
	 * level of 2 and DEC's R3000s are level 2 as well?
	 */
	if (pmax_boardtype == DS_PMAX) {
		cpu.cpu.cp_imp = MIPS_R2000;
		fpu.cpu.cp_imp = MIPS_R2010;
	}

	switch (cpu.cpu.cp_imp) {
	case MIPS_R2000:
		printf("cpu0 (MIPS R2000 revision %d.%d)\n",
			cpu.cpu.cp_majrev, cpu.cpu.cp_minrev);
		break;

	case MIPS_R3000:
		printf("cpu0 (MIPS R3000 revision %d.%d)\n",
			cpu.cpu.cp_majrev, cpu.cpu.cp_minrev);
		break;

	case MIPS_R4000:
		printf("cpu0 (MIPS R4000 revision %d.%d)\n",
			cpu.cpu.cp_majrev, cpu.cpu.cp_minrev);
		break;

	default:
		printf("cpu0 (implementation %d revision %d.%d)\n",
			cpu.cpu.cp_imp, cpu.cpu.cp_majrev, cpu.cpu.cp_minrev);
	}
	switch (fpu.cpu.cp_imp) {
	case MIPS_R2010:
		printf("fpu0 (MIPS R2010 revision %d.%d)\n",
			fpu.cpu.cp_majrev, fpu.cpu.cp_minrev);
		break;

	case MIPS_R3010:
		printf("fpu0 (MIPS R3010 revision %d.%d)\n",
			fpu.cpu.cp_majrev, fpu.cpu.cp_minrev);
		break;

	case MIPS_R4010:
		printf("fpu0 (MIPS R4010 revision %d.%d)\n",
			fpu.cpu.cp_majrev, fpu.cpu.cp_minrev);
		break;

	default:
		printf("fpu0 (implementation %d revision %d.%d)\n",
			fpu.cpu.cp_imp, fpu.cpu.cp_majrev, fpu.cpu.cp_minrev);
	}
	printf("data cache size %dK inst cache size %dK\n",
		machDataCacheSize >> 10, machInstCacheSize >> 10);

	/* probe and initialize controllers */
	for (cp = pmax_cinit; drp = cp->pmax_driver; cp++) {
		switch (pmax_boardtype) {
		case DS_PMAX:
			if (cp->pmax_addr == (char *)QUES)
				continue;
			if (!(*drp->d_init)(cp))
				continue;
			break;
#ifdef DS5000
		case DS_3MAX:
		case DS_3MIN:
		case DS_MAXINE:
		case DS_3MAXPLUS:
			/*
			 * If the device is still in an unknown slot,
			 * then it was not found by tc_find_all_options().
			 */
			if (cp->pmax_addr == (char *)QUES)
				continue;
			if (!(*drp->d_init)(cp)) {
				cp->pmax_alive = 0;
				continue;
			}
			if (drp->d_intr && (i = cp->pmax_pri) >= 0) {
				if (tc_slot_info[i].intr)
					printf("%s: slot %d already in use\n",
						drp->d_name, i);
				tc_slot_info[i].intr = drp->d_intr;
				tc_slot_info[i].unit = cp->pmax_unit;
			}
			break;
#endif /* DS5000 */
		};

		cp->pmax_alive = 1;

		/* probe and initialize devices connected to controller */
		for (dp = scsi_dinit; drp = dp->sd_driver; dp++) {
			/* might want to get fancier later */
			if (dp->sd_cdriver != cp->pmax_driver ||
			    dp->sd_ctlr != cp->pmax_unit)
				continue;	/* not connected */
			if (!(*drp->d_init)(dp))
				continue;
			dp->sd_alive = 1;
			/* if device is a disk, assign number for statistics */
			if (dp->sd_dk && dkn < DK_NDRIVE)
				dp->sd_dk = dkn++;
			else
				dp->sd_dk = -1;
		}
	}

#ifdef GENERIC
	if ((boothowto & RB_ASKNAME) == 0)
		setroot();
	setconf();
#else
	setroot();
#endif
	swapconf();
	cold = 0;
}

/*
 * Configure swap space and related parameters.
 */
swapconf()
{
	register struct swdevt *swp;
	register int nblks;

	for (swp = swdevt; swp->sw_dev != NODEV; swp++)
		if (bdevsw[major(swp->sw_dev)].d_psize) {
			nblks =
			  (*bdevsw[major(swp->sw_dev)].d_psize)(swp->sw_dev);
			if (nblks != -1 &&
			    (swp->sw_nblks == 0 || swp->sw_nblks > nblks))
				swp->sw_nblks = nblks;
		}
	dumpconf();
}

#define	DOSWAP			/* Change swdevt and dumpdev too */
u_long	bootdev;		/* should be dev_t, but not until 32 bits */

static	char devname[][2] = {
	'r','z',	/* 0 = rz */
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
	register struct scsi_device *dp;
	int  majdev, mindev, unit, part, controller;
	dev_t temp, orootdev;
	struct swdevt *swp;

	if (boothowto & RB_DFLTROOT ||
	    (bootdev & B_MAGICMASK) != B_DEVMAGIC)
		return;
	majdev = B_TYPE(bootdev);
	if (majdev >= sizeof(devname) / sizeof(devname[0]))
		return;
	controller = B_CONTROLLER(bootdev);
	part = B_PARTITION(bootdev);
	unit = B_UNIT(bootdev);

	for (dp = scsi_dinit; ; dp++) {
		if (dp->sd_driver == 0)
			return;
		if (dp->sd_alive && dp->sd_drive == unit &&
		    dp->sd_ctlr == controller &&
		    dp->sd_driver->d_name[0] == devname[majdev][0] &&
		    dp->sd_driver->d_name[1] == devname[majdev][1]) {
			mindev = dp->sd_unit;
		    	break;
		}
	}
	/*
	 * Form a new rootdev
	 */
	mindev = (mindev << PARTITIONSHIFT) + part;
	orootdev = rootdev;
	rootdev = makedev(majdev, mindev);
	/*
	 * If the original rootdev is the same as the one
	 * just calculated, don't need to adjust the swap configuration.
	 */
	if (rootdev == orootdev)
		return;

	printf("Changing root device to %c%c%d%c\n",
		devname[majdev][0], devname[majdev][1],
		mindev >> PARTITIONSHIFT, part + 'a');

#ifdef DOSWAP
	mindev &= ~PARTITIONMASK;
	for (swp = swdevt; swp->sw_dev != NODEV; swp++) {
		if (majdev == major(swp->sw_dev) &&
		    mindev == (minor(swp->sw_dev) & ~PARTITIONMASK)) {
			temp = swdevt[0].sw_dev;
			swdevt[0].sw_dev = swp->sw_dev;
			swp->sw_dev = temp;
			break;
		}
	}
	if (swp->sw_dev == NODEV)
		return;

	/*
	 * If dumpdev was the same as the old primary swap
	 * device, move it to the new primary swap device.
	 */
	if (temp == dumpdev)
		dumpdev = swdevt[0].sw_dev;
#endif
}

/*
 * Look at the string 'cp' and decode the boot device.
 * Boot names can be something like 'rz(0,0,0)vmunix' or '5/rz0/vmunix'.
 */
void
makebootdev(cp)
	register char *cp;
{
	int majdev, unit, part, ctrl;

	if (*cp >= '0' && *cp <= '9') {
		/* XXX should be able to specify controller */
		if (cp[1] != '/' || cp[4] < '0' || cp[4] > '9')
			goto defdev;
		unit = cp[4] - '0';
		if (cp[5] >= 'a' && cp[5] <= 'h')
			part = cp[5] - 'a';
		else
			part = 0;
		cp += 2;
		for (majdev = 0; majdev < sizeof(devname)/sizeof(devname[0]);
		    majdev++) {
			if (cp[0] == devname[majdev][0] &&
			    cp[1] == devname[majdev][1]) {
				bootdev = MAKEBOOTDEV(majdev, 0, 0, unit, part);
				return;
			}
		}
		goto defdev;
	}
	for (majdev = 0; majdev < sizeof(devname)/sizeof(devname[0]); majdev++)
		if (cp[0] == devname[majdev][0] &&
		    cp[1] == devname[majdev][1] &&
		    cp[2] == '(')
			goto fndmaj;
defdev:
	bootdev = B_DEVMAGIC;
	return;

fndmaj:
	for (ctrl = 0, cp += 3; *cp >= '0' && *cp <= '9'; )
		ctrl = ctrl * 10 + *cp++ - '0';
	if (*cp == ',')
		cp++;
	for (unit = 0; *cp >= '0' && *cp <= '9'; )
		unit = unit * 10 + *cp++ - '0';
	if (*cp == ',')
		cp++;
	for (part = 0; *cp >= '0' && *cp <= '9'; )
		part = part * 10 + *cp++ - '0';
	if (*cp != ')')
		goto defdev;
	bootdev = MAKEBOOTDEV(majdev, 0, ctrl, unit, part);
}
