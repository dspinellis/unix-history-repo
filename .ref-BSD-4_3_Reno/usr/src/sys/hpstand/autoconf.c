/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: autoconf.c 1.9 89/10/07$
 *
 *	@(#)autoconf.c	7.2 (Berkeley) 5/25/90
 */

#include "samachdep.h"
#include "param.h"

#include "../hpdev/device.h"
#include "../hpdev/grfvar.h"

struct hp_hw sc_table[MAX_CTLR];

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
	if (sc == -2)
		return(0x1000000);
	if (sc == -1)
		return(GRFIADDR);
	if (sc == 7 && internalhpib)
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

		/*
		 * Not all internal HP-IBs respond rationally to id requests
		 * so we just go by the "internal HPIB" indicator in SYSFLAG.
		 */
		if (sc == 7 && internalhpib) {
			hw->hw_type = HPIB;
			hw++;
			continue;
		}

		switch (hw->hw_id) {
		case 5:		/* 98642A */
		case 128+5:	/* 98642A remote */
			hw->hw_type = COMMDCM;
			break;
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
