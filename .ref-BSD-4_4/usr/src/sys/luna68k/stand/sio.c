/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
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
 *	@(#)sio.c	8.1 (Berkeley) 6/10/93
 */

/* sio.c   NOV-25-1991 */

#define NSIO 2

#include <sys/param.h>
#include <luna68k/stand/sioreg.h>
#include <luna68k/stand/rcvbuf.h>
#include <luna68k/stand/kbdreg.h>

struct rcvbuf	rcvbuf[NSIO];

int	sioconsole = -1;
struct	siodevice *sio_addr[2];
int	cur_unit;


#define	siounit(x)	( x & 0xffff )
#define isprint(c)      ((c >= 0x20) && (c < 0x7F) ? 1 : 0)


void
_siointr()
{
	register int unit;

	for (unit = 0; unit < NSIO; unit++)
		siointr(unit);
}

siointr(unit)
	register int unit;
{
	register struct	siodevice *sio = sio_addr[unit];
	register int rr0 = sioreg(REG(unit, RR0), 0);
	register int rr1 = sioreg(REG(unit, RR1), 0);

	if (rr0 & RR0_RXAVAIL) {
		if (rr1 & RR1_FRAMING)
			return;

		if (rr1 & (RR1_PARITY | RR1_OVERRUN))
		    sioreg(REG(unit, WR0), WR0_ERRRST); /* Channel-A Error Reset */

		if (unit == 1) {
			register int c = kbd_decode(sio_addr[unit]->sio_data);

			if ((c & KC_TYPE) == KC_CODE)
				PUSH_RBUF(unit, c);
		} else {
			PUSH_RBUF(unit, sio_addr[unit]->sio_data);
		}
	}
}

/*
 * Following are all routines needed for SIO to act as console
 */
#include <luna68k/luna68k/cons.h>
#include "romvec.h"

siocnprobe(cp)
	struct consdev *cp;
{
	sio_addr[0] = (struct siodevice *) 0x51000000;
	sio_addr[1] = (struct siodevice *) 0x51000004;

	/* make sure hardware exists */
	if (badaddr((short *)sio_addr[0])) {
		cp->cn_pri = CN_DEAD;
		return;
	}

	/* locate the major number */

	/* initialize required fields */
	cp->cn_dev = cur_unit = 0;
	cp->cn_tp  = 0;
	cp->cn_pri = CN_NORMAL;
}

siocninit(cp)
	struct consdev *cp;
{
	int unit = siounit(cp->cn_dev);

	sioinit();
	sioconsole = unit;
}

siocngetc(dev)
	dev_t dev;
{
	register int c, unit = siounit(dev);

	while (RBUF_EMPTY(unit)) {
		DELAY(10);
	}

	POP_RBUF(unit, c);

	return(c);
}

siocnputc(dev, c)
	dev_t dev;
	int c;
{
	int unit = siounit(dev);
	int s;

	if (sioconsole == -1) {
		(void) sioinit();
		sioconsole = unit;
	}

	s = splsio();

	/* wait for any pending transmission to finish */
	while ((sioreg(REG(unit, RR0), 0) & RR0_TXEMPTY) == 0);

	sio_addr[unit]->sio_data = (c & 0xFF);

	/* wait for any pending transmission to finish */
	while ((sioreg(REG(unit, RR0), 0) & RR0_TXEMPTY) == 0);

	splx(s);
}

/* SIO misc routines */

sioinit()
{
	int s;

	RBUF_INIT(0);
	RBUF_INIT(1);

	s = splsio();

	sioreg(REG(0, WR0), WR0_CHANRST);		/* Channel-A Reset */

	sioreg(WR2A, WR2_VEC86  | WR2_INTR_1);		/* Set CPU BUS Interface Mode */
	sioreg(WR2B, 0);				/* Set Interrupt Vector */

	sioreg(REG(0, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(0, WR4), WR4_BAUD96 | WR4_STOP1 | WR4_NPARITY);	/* Tx/Rx */
	sioreg(REG(0, WR3), WR3_RX8BIT | WR3_RXENBL);		/* Rx */
	sioreg(REG(0, WR5), WR5_TX8BIT | WR5_TXENBL);		/* Tx */
	sioreg(REG(0, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(0, WR1), WR1_RXALLS);		/* Interrupted All Char. */

	sioreg(REG(1, WR0), WR0_CHANRST);		/* Channel-A Reset */

	sioreg(REG(1, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(1, WR4), WR4_BAUD96 | WR4_STOP1 | WR4_NPARITY);	/* Tx/Rx */
	sioreg(REG(1, WR3), WR3_RX8BIT | WR3_RXENBL);		/* Rx */
	sioreg(REG(1, WR5), WR5_TX8BIT | WR5_TXENBL);		/* Tx */
	sioreg(REG(1, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(1, WR1), WR1_RXALLS);		/* Interrupted All Char. */

	splx(s);
}

int
sioreg(reg, val)
	register int reg, val;
{
	register int chan;

	chan = CHANNEL(reg);

	if (isStatusReg(reg)) {
		if (REGNO(reg) != 0)
		    sio_addr[chan]->sio_cmd = REGNO(reg);
		return(sio_addr[chan]->sio_stat);
	} else {
		if (REGNO(reg) != 0)
		    sio_addr[chan]->sio_cmd = REGNO(reg);
		sio_addr[chan]->sio_cmd = val;
		return(val);
	}
}
