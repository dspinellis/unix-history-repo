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
 *	@(#)dca.c	7.1 (Berkeley) 5/8/90
 */

#ifdef DCACONSOLE
#include "../sys/param.h"
#include "../hpdev/dcareg.h"
#include "machine/cpu.h"
#include "machine/cons.h"

#define CONSDEV	(0)
#define CONSOLE ((struct dcadevice *)(EXTIOBASE + (9 * IOCARDSIZE)))

dcaprobe(cp)
	struct consdev *cp;
{
	register struct dcadevice *dca = CONSOLE;

	if (badaddr((char *)CONSOLE)) {
		cp->cn_pri = CN_DEAD;
		return;
	}
	switch (dca->dca_irid) {
	case DCAID0:
	case DCAID1:
		cp->cn_pri = CN_NORMAL;
		break;
	case DCAREMID0:
	case DCAREMID1:
		cp->cn_pri = CN_REMOTE;
		break;
	default:
		cp->cn_pri = CN_DEAD;
		break;
	}
}

dcainit(cp)
	struct consdev *cp;
{
	register struct dcadevice *dca = CONSOLE;

	dca->dca_irid = 0xFF;
	DELAY(100);
	dca->dca_ic = 0;
	dca->dca_cfcr = CFCR_DLAB;
	dca->dca_data = DCABRD(9600) & 0xFF;
	dca->dca_ier = DCABRD(9600) >> 8;
	dca->dca_cfcr = CFCR_8BITS;
}

#ifndef SMALL
dcagetchar()
{
	register struct dcadevice *dca = CONSOLE;
	short stat;
	int c;

	if (((stat = dca->dca_lsr) & LSR_RXRDY) == 0)
		return(0);
	c = dca->dca_data;
	return(c);
}
#else
dcagetchar()
{
	return(0);
}
#endif

dcaputchar(c)
	register int c;
{
	register struct dcadevice *dca = CONSOLE;
	register int timo;
	short stat;

	/* wait a reasonable time for the transmitter to come ready */
	timo = 50000;
	while (((stat = dca->dca_lsr) & LSR_TXRDY) == 0 && --timo)
		;
	dca->dca_data = c;
	/* wait for this transmission to complete */
	timo = 1000000;
	while (((stat = dca->dca_lsr) & LSR_TXRDY) == 0 && --timo)
		;
}
#endif
