/*
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)hpib.c	8.1 (Berkeley) 6/10/93
 */

/*
 * HPIB driver
 */
#include <sys/param.h>
#include <sys/reboot.h>
#include <hp/dev/device.h>
#include <hp300/stand/hpibvar.h>

#include <stand.att/saio.h>
#include <hp300/stand/samachdep.h>

int	internalhpib = IIOV(0x478000);
int	fhpibppoll(), nhpibppoll();

struct	hpib_softc hpib_softc[NHPIB];

hpibinit()
{
	extern struct hp_hw sc_table[];
	register struct hp_hw *hw;
	register struct hpib_softc *hs;
	register int i, addr;

	i = 0;
	for (hw = sc_table; i < NHPIB && hw < &sc_table[MAXCTLRS]; hw++) {
		if (!HW_ISHPIB(hw))
			continue;
		hs = &hpib_softc[i];
		hs->sc_addr = hw->hw_kva;
		if (nhpibinit(i) == 0)
			if (fhpibinit(i) == 0)
				continue;
		if (howto & RB_ASKNAME)
			printf("hpib%d at sc%d\n", i, hw->hw_sc);
		hw->hw_pa = (caddr_t) i;	/* XXX for autoconfig */
		hs->sc_alive = 1;
		i++;
	}
}

hpibalive(unit)
	register int unit;
{
	if (unit >= NHPIB || hpib_softc[unit].sc_alive == 0)
		return (0);
	return (1);
}

hpibid(unit, slave)
	int unit, slave;
{
	short id;
	int rv;

	if (hpib_softc[unit].sc_type == HPIBC)
		rv = fhpibrecv(unit, 31, slave, &id, 2);
	else
		rv = nhpibrecv(unit, 31, slave, &id, 2);
	if (rv != 2)
		return (0);
	return (id);
}

hpibsend(unit, slave, sec, buf, cnt)
	int unit, slave;
	char *buf;
	int cnt;
{
	if (hpib_softc[unit].sc_type == HPIBC)
		return (fhpibsend(unit, slave, sec, buf, cnt));
	return (nhpibsend(unit, slave, sec, buf, cnt));
}

hpibrecv(unit, slave, sec, buf, cnt)
	int unit, slave;
	char *buf;
	int cnt;
{
	if (hpib_softc[unit].sc_type == HPIBC)
		return (fhpibrecv(unit, slave, sec, buf, cnt));
	return (nhpibrecv(unit, slave, sec, buf, cnt));
}

hpibswait(unit, slave)
	register int unit, slave;
{
	register int timo = 1000000;
	register int (*poll)();

	slave = 0x80 >> slave;
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

hpibgo(unit, slave, sec, addr, count, flag)
	int unit, slave;
	char *addr;
{
	if (hpib_softc[unit].sc_type == HPIBC)
		if (flag == F_READ)
			fhpibrecv(unit, slave, sec, addr, count);
		else
			fhpibsend(unit, slave, sec, addr, count);
	else
		if (flag == F_READ)
			nhpibrecv(unit, slave, sec, addr, count);
		else
			nhpibsend(unit, slave, sec, addr, count);
}
