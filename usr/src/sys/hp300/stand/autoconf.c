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
 * from: Utah $Hdr: autoconf.c 1.9 89/10/07$
 *
 *	@(#)autoconf.c	7.1 (Berkeley) %G%
 */

#include "samachdep.h"
#include "param.h"

#include "../hpdev/device.h"
#include "../hpdev/grfvar.h"

struct hp_hw sc_table[MAX_CTLR];

configure()
{
	find_devs();
	cninit();
	hpibinit();
	scsiinit();
}

sctoaddr(sc)
	int sc;
{
	extern int internalhpib;

	if (sc == -2)
		return(0x1000000);
	if (sc == -1)
		return(GRFIADDR);
	if (sc == 7)
		return(internalhpib);
	if (sc < 32)
		return(0x600000+(0x10000*sc));
	return(sc);
}

/*
 * Probe all select codes (0 - 32) and internal display address.
 * Note that we only care about displays, SCSIs and HP-IBs.
 */
find_devs()
{
	u_char *id_reg;
	register short sc;
	register int addr;
	register struct hp_hw *hw;

	hw = sc_table;
	for (sc = -2; sc < 32; sc++) {
		addr = sctoaddr(sc);
		if (badaddr(addr))
			continue;

		id_reg = (u_char *) addr;
		hw->hw_addr = (char *) addr;
		hw->hw_id = id_reg[1] & 0xff;
		hw->hw_sc = sc;

		switch (hw->hw_id) {
		case 8:		/* 98625B */
		case 128:	/* 98624A */
			hw->hw_type = HPIB;
			break;
		case 57:	/* Displays */
			hw->hw_type = BITMAP;
			hw->hw_id2 = id_reg[0x15];
			switch (hw->hw_id2) {
			case 4:	/* renaissance */
			case 8: /* davinci */
				sc++;		/* occupy 2 select codes */
				break;
			}
			break;
		case 9:
			hw->hw_type = KEYBOARD;
			break;
		case 7:
		case 39:
		case 71:
		case 103:
			hw->hw_type = SCSI;
			break;
		default:	/* who cares */
			hw->hw_type = MISC;
			break;
		}
		hw++;
	}
}
