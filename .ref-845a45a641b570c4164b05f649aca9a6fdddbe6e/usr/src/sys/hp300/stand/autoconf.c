/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: autoconf.c 1.16 92/05/29$
 *
 *	@(#)autoconf.c	7.8 (Berkeley) %G%
 */

#include <hp300/stand/samachdep.h>
#include <hp300/stand/rominfo.h>
#include <sys/param.h>
#include <sys/reboot.h>

#include <hp/dev/device.h>
#include <hp/dev/grfreg.h>

/*
 * Mapping of ROM MSUS types to BSD major device numbers
 * WARNING: major numbers must match bdevsw indices in hp300/conf.c.
 */
char rom2mdev[] = {
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0-13: none */
	4,	/* 14: SCSI disk */
	0,	/* 15: none */
	2,	/* 16: CS/80 device on HPIB */
	2,	/* 17: CS/80 device on HPIB */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 18-31: none */
};

struct hp_hw sc_table[MAXCTLRS];
int cpuspeed;

extern int internalhpib;

#if 0
printrominfo()
{
	struct rominfo *rp = (struct rominfo *)ROMADDR;

	printf("boottype %x, name %s, lowram %x, sysflag %x\n",
	       rp->boottype, rp->name, rp->lowram, rp->sysflag&0xff);
	printf("rambase %x, ndrives %x, sysflag2 %x, msus %x\n",
	       rp->rambase, rp->ndrives, rp->sysflag2&0xff, rp->msus);
}
#endif

configure()
{
	u_long msustobdev();

	switch (machineid) {
	case HP_320:
	case HP_330:
	case HP_340:
		cpuspeed = MHZ_16;
		break;
	case HP_350:
	case HP_360:
		cpuspeed = MHZ_25;
		break;
	case HP_370:
		cpuspeed = MHZ_33;
		break;
	case HP_375:
		cpuspeed = MHZ_50;
		break;
	case HP_380:
		cpuspeed = MHZ_25 * 2;	/* XXX */
		break;
	case HP_433:
		cpuspeed = MHZ_33 * 2;	/* XXX */
		break;
	default:	/* assume the fastest (largest delay value) */
		cpuspeed = MHZ_50;
		break;
	}
	find_devs();
	cninit();
#if 0
	printrominfo();
#endif
	hpibinit();
	scsiinit();
	if ((bootdev & B_MAGICMASK) != B_DEVMAGIC)
		bootdev = msustobdev();
}

/*
 * Convert HP MSUS to a valid bootdev layout:
 *	TYPE comes from MSUS device type as mapped by rom2mdev
 *	PARTITION is set to 0 ('a')
 *	UNIT comes from MSUS unit (almost always 0)
 *	CONTROLLER comes from MSUS primary address
 *	ADAPTOR comes from SCSI/HPIB driver logical unit number
 *		(passed back via unused hw_pa field)
 */
u_long
msustobdev()
{
	struct rominfo *rp = (struct rominfo *) ROMADDR;
	u_long bdev = 0;
	register struct hp_hw *hw;
	int sc;

	sc = (rp->msus >> 8) & 0xFF;
	for (hw = sc_table; hw < &sc_table[MAXCTLRS]; hw++)
		if (hw->hw_sc == sc)
			break;
	bdev |= rom2mdev[(rp->msus >> 24) & 0x1F] << B_TYPESHIFT;
	bdev |= 0 << B_PARTITIONSHIFT;
	bdev |= ((rp->msus >> 16) & 0xFF) << B_UNITSHIFT;
	bdev |= (rp->msus & 0xFF) << B_CONTROLLERSHIFT;
	bdev |= (int)hw->hw_pa << B_ADAPTORSHIFT;
	bdev |= B_DEVMAGIC;
#if 0
	printf("msus %x -> bdev %x\n", rp->msus, bdev);
#endif
	return (bdev);
}

sctoaddr(sc)
	int sc;
{
	if (sc == -1)
		return(GRFIADDR);
	if (sc == 7 && internalhpib)
		return(internalhpib);
	if (sc < 32)
		return(DIOBASE + sc * DIOCSIZE);
	if (sc >= 132)
		return(DIOIIBASE + (sc - 132) * DIOIICSIZE);
	return(sc);
}

/*
 * Probe all DIO select codes (0 - 32), the internal display address,
 * and DIO-II select codes (132 - 256).
 *
 * Note that we only care about displays, SCSIs and HP-IBs.
 */
find_devs()
{
	short sc, sctop;
	u_char *id_reg;
	register caddr_t addr;
	register struct hp_hw *hw;

	hw = sc_table;
	sctop = machineid == HP_320 ? 32 : 256;
	for (sc = -1; sc < sctop; sc++) {
		if (sc >= 32 && sc < 132)
			continue;
		addr = (caddr_t) sctoaddr(sc);
		if (badaddr(addr))
			continue;

		id_reg = (u_char *) addr;
		hw->hw_pa = 0;	/* XXX used to pass back LUN from driver */
		if (sc >= 132)
			hw->hw_size = (id_reg[0x101] + 1) * 0x100000;
		else
			hw->hw_size = DIOCSIZE;
		hw->hw_kva = addr;
		hw->hw_id = id_reg[1];
		hw->hw_sc = sc;

		/*
		 * Not all internal HP-IBs respond rationally to id requests
		 * so we just go by the "internal HPIB" indicator in SYSFLAG.
		 */
		if (sc == 7 && internalhpib) {
			hw->hw_type = C_HPIB;
			hw++;
			continue;
		}

		switch (hw->hw_id) {
		case 5:		/* 98642A */
		case 5+128:	/* 98642A remote */
			hw->hw_type = D_COMMDCM;
			break;
		case 8:		/* 98625B */
		case 128:	/* 98624A */
			hw->hw_type = C_HPIB;
			break;
		case 57:	/* Displays */
			hw->hw_type = D_BITMAP;
			hw->hw_secid = id_reg[0x15];
			switch (hw->hw_secid) {
			case 4:	/* renaissance */
			case 8: /* davinci */
				sc++;		/* occupy 2 select codes */
				break;
			}
			break;
		case 9:
			hw->hw_type = D_KEYBOARD;
			break;
		case 7:
		case 7+32:
		case 7+64:
		case 7+96:
			hw->hw_type = C_SCSI;
			break;
		default:	/* who cares */
			hw->hw_type = D_MISC;
			break;
		}
		hw++;
	}
}
