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
 *	@(#)sio.c	7.1 (Berkeley) %G%
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

			if ((c & KC_TYPE) == 0) {
				if (isprint(c))
					PUSH_RBUF(unit, c);
				else
					PUSH_RBUF(unit, ' ');
			}
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
