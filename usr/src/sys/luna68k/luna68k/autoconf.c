/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)autoconf.c	7.2 (Berkeley) %G%
 */

/*
 * autoconf.c --  for auto configration
 * remade by A.Fujita, MAR-24-1992
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
#include <sys/dkstat.h>
#include <sys/conf.h>
#include <sys/reboot.h>

#include <machine/cpu.h>
#include <luna68k/dev/device.h>

/*
 * The following several variables are related to
 * the configuration process, and are used in initializing
 * the machine.
 */
int	cold;		    /* if 1, still working on cold-start */
int	dkn;		    /* number of iostat dk numbers assigned so far */
struct	hp_hw sc_table[MAXCTLRS];

u_long	bootdev;		/* should be dev_t, but not until 32 bits */

#ifdef DEBUG
int	acdebug = 0;
#endif

/*
 * Determine mass storage and memory configuration for a machine.
 */
configure()
{
	register struct hp_hw *hw;
	int found;

	/*
	 * Look over each hardware device actually found and attempt
	 * to match it with an ioconf.c table entry.
	 */
	for (hw = sc_table; hw->hw_type; hw++) {
		if (HW_ISCTLR(hw))
			found = find_controller(hw);
		else
			found = find_device(hw);
#ifdef DEBUG
		if (!found) {
			printf("unconfigured card id %x ", hw->hw_id);
			printf("at 0x%x\n", hw->hw_pa);
		}
#endif
	}

#if GENERIC
	if ((boothowto & RB_ASKNAME) == 0)
		setroot();
	setconf();
#else
	setroot();
#endif
	swapconf();
	cold = 0;
}

#define dr_type(d, s)	\
	(strcmp((d)->d_name, (s)) == 0)

#define same_hw_ctlr(hw, hc) \
	 HW_ISSCSI(hw) && dr_type((hc)->hp_driver, "sc")

find_controller(hw)
	register struct hp_hw *hw;
{
	register struct hp_ctlr *hc;
	struct hp_ctlr *match_c;
	caddr_t oaddr;
	int sc;

#ifdef DEBUG
	if (acdebug)
		printf("find_controller: hw: id%x at sc%d (%x), type %x...",
		       hw->hw_id, hw->hw_sc, hw->hw_kva, hw->hw_type);
#endif
	sc = hw->hw_sc;
	match_c = NULL;
	for (hc = hp_cinit; hc->hp_driver; hc++) {
		if (hc->hp_alive)
			continue;
		/*
		 * Make sure we are looking at the right
		 * controller type.
		 */
		if (!same_hw_ctlr(hw, hc))
			continue;
		/*
		 * Exact match; all done
		 */
		if ((int)hc->hp_addr == sc) {
			match_c = hc;
			break;
		}
		/*
		 * Wildcard; possible match so remember first instance
		 * but continue looking for exact match.
		 */
		if (hc->hp_addr == NULL && match_c == NULL)
			match_c = hc;
	}
#ifdef DEBUG
	if (acdebug) {
		if (match_c)
			printf("found %s%d\n",
			       match_c->hp_driver->d_name,
			       match_c->hp_unit);
		else
			printf("not found\n");
	}
#endif
	/*
	 * Didn't find an ioconf entry for this piece of hardware,
	 * just ignore it.
	 */
	if (match_c == NULL)
		return(0);
	/*
	 * Found a match, attempt to initialize and configure all attached
	 * slaves.  Note, we can still fail if HW won't initialize.
	 */
	hc = match_c;
	oaddr = hc->hp_addr;
	hc->hp_addr = hw->hw_kva;
	if ((*hc->hp_driver->d_init)(hc)) {
		hc->hp_alive = 1;
		printf("%s%d", hc->hp_driver->d_name, hc->hp_unit);
		printf(" at 0x%x,", hw->hw_pa);
		printf(" ipl %d", hc->hp_ipl);
		if (hc->hp_flags)
			printf(" flags 0x%x", hc->hp_flags);
		printf("\n");
		find_slaves(hc);
	} else
		hc->hp_addr = oaddr;
	return(1);
}

find_device(hw)
	register struct hp_hw *hw;
{
	register struct hp_device *hd;
	struct hp_device *match_d;
	caddr_t oaddr;
	int sc;

#ifdef DEBUG
	if (acdebug)
		printf("find_device: hw: id%x at sc%d (%x), type %x...",
		       hw->hw_id, hw->hw_sc, hw->hw_kva, hw->hw_type);
#endif
	match_d = NULL;
	for (hd = hp_dinit; hd->hp_driver; hd++) {
		if (hd->hp_alive)
			continue;
		/* Must not be a slave */
		if (hd->hp_cdriver)
			continue;
		sc = (int) hd->hp_addr;
		/*
		 * Exact match; all done.
		 */
		if (sc > 0 && sc == hw->hw_sc) {
			match_d = hd;
			break;
		}
		/*
		 * Wildcard; possible match so remember first instance
		 * but continue looking for exact match.
		 */
		if (sc == 0 && same_hw_device(hw, hd) && match_d == NULL)
			match_d = hd;
	}
#ifdef DEBUG
	if (acdebug) {
		if (match_d)
			printf("found %s%d\n",
			       match_d->hp_driver->d_name,
			       match_d->hp_unit);
		else
			printf("not found\n");
	}
#endif
	/*
	 * Didn't find an ioconf entry for this piece
	 * of hardware, just ignore it.
	 */
	if (match_d == NULL)
		return(0);
	/*
	 * Found a match, attempt to initialize.
	 * Note, we can still fail if HW won't initialize.
	 */
	hd = match_d;
	oaddr = hd->hp_addr;
	hd->hp_addr = hw->hw_kva;
	if ((*hd->hp_driver->d_init)(hd)) {
		hd->hp_alive = 1;
		printf("%s%d", hd->hp_driver->d_name, hd->hp_unit);
		printf(" at 0x%x", hw->hw_pa);
		if (hd->hp_ipl)
			printf(", ipl %d", hd->hp_ipl);
		if (hd->hp_flags)
			printf(", flags 0x%x", hd->hp_flags);
		printf("\n");
	} else
		hd->hp_addr = oaddr;
	return(1);
}

/*
 * Search each BUS controller found for slaves attached to it.
 * The bad news is that we don't know how to uniquely identify all slaves
 * (e.g. PPI devices on HP-IB).  The good news is that we can at least
 * differentiate those from slaves we can identify.  At worst (a totally
 * wildcarded entry) this will cause us to locate such a slave at the first
 * unused position instead of where it really is.  To save grief, non-
 * identifing devices should always be fully qualified.
 */
find_slaves(hc)
	register struct hp_ctlr *hc;
{
	register int s;
	register struct hp_device *hd;
	struct hp_device *match_s;
	int maxslaves = MAXSLAVES - 1;
	int new_s, new_c, old_s, old_c;
	int rescan;
	
#ifdef DEBUG
	if (acdebug)
		printf("find_busslaves: for %s%d\n",
		       hc->hp_driver->d_name, hc->hp_unit);
#endif
	for (s = 0; s < maxslaves; s++) {
		rescan = 1;
		match_s = NULL;
		for (hd = hp_dinit; hd->hp_driver; hd++) {
			/*
			 * Rule out the easy ones:
			 * 1. slave already assigned or not a slave
			 * 2. not of the proper type
			 * 3. controller specified but not this one
			 * 4. slave specified but not this one
			 */
			if (hd->hp_alive || hd->hp_cdriver == NULL)
				continue;
			if (!dr_type(hc->hp_driver, hd->hp_cdriver->d_name))
				continue;
			if (hd->hp_ctlr >= 0 && hd->hp_ctlr != hc->hp_unit)
				continue;
			if (hd->hp_slave >= 0 && hd->hp_slave != s)
				continue;
			/*
			 * Case 0: first possible match.
			 * Remember it and keep looking for better.
			 */
			if (match_s == NULL) {
				match_s = hd;
				new_c = hc->hp_unit;
				new_s = s;
				continue;
			}
			/*
			 * Case 1: exact match.
			 * All done.  Note that we do not attempt any other
			 * matches if this one fails.  This allows us to
			 * "reserve" locations for dynamic addition of
			 * disk/tape drives by fully qualifing the location.
			 */
			if (hd->hp_slave == s && hd->hp_ctlr == hc->hp_unit) {
				match_s = hd;
				rescan = 0;
				break;
			}
			/*
			 * Case 2: right controller, wildcarded slave.
			 * Remember first and keep looking for an exact match.
			 */
			if (hd->hp_ctlr == hc->hp_unit &&
			    match_s->hp_ctlr < 0) {
				match_s = hd;
				new_s = s;
				continue;
			}
			/*
			 * Case 3: right slave, wildcarded controller.
			 * Remember and keep looking for a better match.
			 */
			if (hd->hp_slave == s &&
			    match_s->hp_ctlr < 0 && match_s->hp_slave < 0) {
				match_s = hd;
				new_c = hc->hp_unit;
				continue;
			}
			/*
			 * OW: we had a totally wildcarded spec.
			 * If we got this far, we have found a possible
			 * match already (match_s != NULL) so there is no
			 * reason to remember this one.
			 */
			continue;
		}
		/*
		 * Found a match.  We need to set hp_ctlr/hp_slave properly
		 * for the init routines but we also need to remember all
		 * the old values in case this doesn't pan out.
		 */
		if (match_s) {
			hd = match_s;
			old_c = hd->hp_ctlr;
			old_s = hd->hp_slave;
			if (hd->hp_ctlr < 0)
				hd->hp_ctlr = new_c;
			if (hd->hp_slave < 0)
				hd->hp_slave = new_s;
#ifdef DEBUG
			if (acdebug)
				printf("looking for %s%d at slave %d...",
				       hd->hp_driver->d_name,
				       hd->hp_unit, hd->hp_slave);
#endif

			if ((*hd->hp_driver->d_init)(hd)) {
#ifdef DEBUG
				if (acdebug)
					printf("found\n");
#endif
				printf("%s%d at %s%d, slave %d",
				       hd->hp_driver->d_name, hd->hp_unit,
				       hc->hp_driver->d_name, hd->hp_ctlr,
				       hd->hp_slave);
				if (hd->hp_flags)
					printf(" flags 0x%x", hd->hp_flags);
				printf("\n");
				hd->hp_alive = 1;
				if (hd->hp_dk && dkn < DK_NDRIVE)
					hd->hp_dk = dkn++;
				else
					hd->hp_dk = -1;
				rescan = 1;
			} else {
#ifdef DEBUG
				if (acdebug)
					printf("not found\n");
#endif
				hd->hp_ctlr = old_c;
				hd->hp_slave = old_s;
			}
			/*
			 * XXX: This should be handled better.
			 * Re-scan a slave.  There are two reasons to do this.
			 * 1. It is possible to have both a tape and disk
			 *    (e.g. 7946) or two disks (e.g. 9122) at the
			 *    same slave address.  Here we need to rescan
			 *    looking only at entries with a different
			 *    physical unit number (hp_flags).
			 * 2. It is possible that an init failed because the
			 *    slave was there but of the wrong type.  In this
			 *    case it may still be possible to match the slave
			 *    to another ioconf entry of a different type.
			 *    Here we need to rescan looking only at entries
			 *    of different types.
			 * In both cases we avoid looking at undesirable
			 * ioconf entries of the same type by setting their
			 * alive fields to -1.
			 */
			if (rescan) {
				for (hd = hp_dinit; hd->hp_driver; hd++) {
					if (hd->hp_alive)
						continue;
					if (match_s->hp_alive == 1) {	/* 1 */
						if (hd->hp_flags == match_s->hp_flags)
							hd->hp_alive = -1;
					} else {			/* 2 */
						if (hd->hp_driver == match_s->hp_driver)
							hd->hp_alive = -1;
					}
				}
				s--;
				continue;
			}
		}
		/*
		 * Reset bogon alive fields prior to attempting next slave
		 */
		for (hd = hp_dinit; hd->hp_driver; hd++)
			if (hd->hp_alive == -1)
				hd->hp_alive = 0;
	}
}

same_hw_device(hw, hd)
	struct hp_hw *hw;
	struct hp_device *hd;
{
	int found = 0;

	switch (hw->hw_type & ~B_MASK) {
	case C_SCSI:
		found = dr_type(hd->hp_driver, "sc");
		break;
	case D_LAN:
		found = dr_type(hd->hp_driver, "le");
		break;
	case D_SIO:
		found = dr_type(hd->hp_driver, "sio");
		break;
	default:
		break;
	}
	return(found);
}

/*
 * Scan the IO space looking for devices.
 */

#define setup_hw(hw, addr, sc, type, id) \
	(hw)->hw_pa   = addr; \
	(hw)->hw_kva  = addr; \
	(hw)->hw_id   = id; \
	(hw)->hw_type = type; \
	(hw)->hw_sc   = sc

find_devs()
{
	register struct hp_hw *hw = sc_table;
	
	setup_hw(hw, (char *) 0x51000000, 0x5, D_SIO,      0x5);
	hw->hw_secid = 0; hw++;
	
	setup_hw(hw, (char *) 0x51000004, 0x5, D_KEYBOARD, 0x5);
	hw->hw_secid = 1; hw++;
	
	setup_hw(hw, (char *) 0xe1000000, 0xe, C_SCSI,     0xe);
	hw++;

	if (!badaddr((caddr_t) 0xf1000000)) {
		setup_hw(hw, (char *) 0xf1000000, 0xf, D_LAN,      0xf);
		hw++;
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
			if (nblks != -1 &&
			    (swp->sw_nblks == 0 || swp->sw_nblks > nblks))
				swp->sw_nblks = nblks;
		}
	dumpconf();
}

#define	DOSWAP			/* Change swdevt and dumpdev too */

static	char devname[][2] = {
	0,0,		/* 0 = ct */
	0,0,		/* 1 = xx */
	'r','d',	/* 2 = rd */
	0,0,		/* 3 = sw */
	's','d',	/* 4 = rd */
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
	register struct hp_ctlr *hc;
	register struct hp_device *hd;
	int  majdev, mindev, unit, part, adaptor;
	dev_t temp, orootdev;
	struct swdevt *swp;

	if (boothowto & RB_DFLTROOT)
		goto defdev;

	if ((bootdev & B_MAGICMASK) != (u_long)B_DEVMAGIC) {
		printf("Wrong B_DEVMAGIC\n");
		goto defdev;
	}

	majdev = (bootdev >> B_TYPESHIFT) & B_TYPEMASK;
	if (majdev > sizeof(devname) / sizeof(devname[0])) {
		printf("Wrong Major Number: %d", majdev);
		goto defdev;
	}

	adaptor = (bootdev >> B_ADAPTORSHIFT) & B_ADAPTORMASK;
	part = (bootdev >> B_PARTITIONSHIFT) & B_PARTITIONMASK;
	unit = (bootdev >> B_UNITSHIFT) & B_UNITMASK;
	/*
	 * First, find the device type which support this device.
	 */
	for (hd = hp_dinit; hd->hp_driver; hd++)
		if (hd->hp_driver->d_name[0] == devname[majdev][0] &&
		    hd->hp_driver->d_name[1] == devname[majdev][1])
			break;
	if (hd->hp_driver == 0) {
		printf("Device type mismatch: %c%c\n",
		       devname[majdev][0], devname[majdev][1]);
		goto defdev;
	}
	/*
	 * Next, find the controller of that type corresponding to
	 * the adaptor number.
	 */
	for (hc = hp_cinit; hc->hp_driver; hc++)
		if (hc->hp_alive && hc->hp_unit == adaptor &&
		    hc->hp_driver == hd->hp_cdriver)
			break;
	if (hc->hp_driver == 0) {
		printf("Controller is not available\n");
		goto defdev;
	}
	/*
	 * Finally, find the device in question attached to that controller.
	 */
	for (hd = hp_dinit; hd->hp_driver; hd++)
		if (hd->hp_alive && hd->hp_slave == unit &&
		    hd->hp_cdriver == hc->hp_driver &&
		    hd->hp_ctlr == hc->hp_unit)
			break;
	if (hd->hp_driver == 0) {
		printf("Device not found\n");
		goto defdev;
	}

	mindev = hd->hp_unit;
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
	if (rootdev == orootdev) {
		printf("root on %c%c%d%c\n",
		       devname[majdev][0], devname[majdev][1],
		       mindev >> PARTITIONSHIFT, part + 'a');
		return;
	}

	printf("Changing root device to %c%c%d%c\n",
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
	 * If dumpdev was the same as the old primary swap
	 * device, move it to the new primary swap device.
	 */
	if (temp == dumpdev)
		dumpdev = swdevt[0].sw_dev;
#endif
	return;

 defdev:
	majdev = major(rootdev);
	mindev = minor(rootdev);
	unit   = mindev >> PARTITIONSHIFT;
	part   = mindev &  PARTITIONMASK;
	printf("Use default root device: %c%c%d%c\n",
	       devname[majdev][0], devname[majdev][1],
	       unit, part);
	return;
}

strcmp(s1, s2)
	register char *s1, *s2;
{
	while (*s1 == *s2++)
		if (*s1++=='\0')
			return (0);
	return (*s1 - *--s2);
}

strncmp(s1, s2, n)
	register char *s1, *s2;
	register unsigned int n;
{

	if (n == 0)
		return (0);
	do {
		if (*s1 != *s2++)
			return (*(unsigned char *)s1 - *(unsigned char *)--s2);
		if (*s1++ == 0)
			break;
	} while (--n != 0);
	return (0);
}
