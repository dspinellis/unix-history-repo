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
 * from: Utah $Hdr: autoconf.c 1.13 91/01/21$
 *
 *	@(#)autoconf.c	7.5 (Berkeley) %G%
 */

#include "samachdep.h"
#include "sys/param.h"

#include "../dev/device.h"
#include "../dev/grfvar.h"

struct hp_hw sc_table[MAXCTLRS];

extern int internalhpib;

#if 0
#include "rominfo.h"
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
	find_devs();
	cninit();
#if 0
	printrominfo();
#endif
	hpibinit();
	scsiinit();
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
	extern int machineid;

	hw = sc_table;
	sctop = machineid == HP_320 ? 32 : 256;
	for (sc = -1; sc < sctop; sc++) {
		if (sc >= 32 && sc < 132)
			continue;
		addr = (caddr_t) sctoaddr(sc);
		if (badaddr(addr))
			continue;

		id_reg = (u_char *) addr;
		hw->hw_pa = addr;
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
