/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)hpib.c	7.2 (Berkeley) 6/24/90
 */

/*
 * HPIB driver
 */
#include "reboot.h"
#include "../hpdev/device.h"
#include "hpibvar.h"

#include "saio.h"
#include "samachdep.h"

int	internalhpib = 0x478000;
int	fhpibppoll(), nhpibppoll();

struct	hpib_softc hpib_softc[NHPIB];

#define	hpibunit(x)	((x) >> 3)
#define	hpibslave(x)	((x) & 7)

hpibinit()
{
	extern struct hp_hw sc_table[];
	register struct hp_hw *hw;
	register struct hpib_softc *hs;
	register int i, addr;
	static int first = 1;
	
	i = 0;
	for (hw = sc_table; i < NHPIB && hw < &sc_table[MAX_CTLR]; hw++) {
		if (hw->hw_type != HPIB)
			continue;
		hs = &hpib_softc[i];
		hs->sc_addr = hw->hw_addr;
		if (nhpibinit(i) == 0)
			if (fhpibinit(i) == 0)
				continue;
		if (howto & RB_ASKNAME)
			printf("hpib%d at sc%d\n", i, hw->hw_sc);
		/*
		 * Adjust devtype on first call.  This routine assumes that
		 * adaptor is in the high byte of devtype.
		 */
		if (first && ((devtype >> 24) & 0xff) == hw->hw_sc) {
			devtype = (devtype & 0x00ffffff) | (i << 24);
			first = 0;
		}
		hs->sc_alive = 1;
		i++;
	}
}

hpibalive(unit)
	register int unit;
{
	unit = hpibunit(unit);
	if (unit >= NHPIB || hpib_softc[unit].sc_alive == 0)
		return (0);
	return (1);
}

hpibid(unit)
	register int unit;
{
	register struct hpib_softc *hs = &hpib_softc[hpibunit(unit)];
	register int slave;
	short id;

	slave = hpibslave(unit);
	unit = hpibunit(unit);
	if (hs->sc_type == HPIBC)
		slave = fhpibrecv(unit, 31, slave, &id, 2);
	else
		slave = nhpibrecv(unit, 31, slave, &id, 2);
	if (slave != 2)
		return (0);
	return (id);
}

hpibsend(unit, sec, buf, cnt)
	register char *buf;
	register int cnt;
{
	register struct hpib_softc *hs = &hpib_softc[hpibunit(unit)];
	register int slave;

	slave = hpibslave(unit);
	unit = hpibunit(unit);
	if (hs->sc_type == HPIBC)
		return (fhpibsend(unit, slave, sec, buf, cnt));
	else
		return (nhpibsend(unit, slave, sec, buf, cnt));
}

hpibrecv(unit, sec, buf, cnt)
	register char *buf;
	register int cnt;
{
	register struct hpib_softc *hs = &hpib_softc[hpibunit(unit)];
	register int slave;

	slave = hpibslave(unit);
	unit = hpibunit(unit);
	if (hs->sc_type == HPIBC)
		return (fhpibrecv(unit, slave, sec, buf, cnt));
	else
		return (nhpibrecv(unit, slave, sec, buf, cnt));
}

hpibswait(unit)
	register int unit;
{
	register int timo = 1000000;
	register int slave = 0x80 >> hpibslave(unit);
	register int (*poll)();

	unit = hpibunit(unit);
	if (hpib_softc[unit].sc_type == HPIBC)
		poll = fhpibppoll;
	else
		poll = nhpibppoll;
	while (((*poll)(unit) & slave) == 0)
		if (--timo == 0)
			break;
	if (timo == 0)
		return (-1);
	return (0);
}

hpibgo(unit, sec, addr, count, flag)
	register int unit;
	char *addr;
{
	register int slave;

	slave = hpibslave(unit);
	unit = hpibunit(unit);
	if (hpib_softc[unit].sc_type == HPIBC)
		if (flag == READ)
			fhpibrecv(unit, slave, sec, addr, count);
		else
			fhpibsend(unit, slave, sec, addr, count);
	else
		if (flag == READ)
			nhpibrecv(unit, slave, sec, addr, count);
		else
			nhpibsend(unit, slave, sec, addr, count);
}
