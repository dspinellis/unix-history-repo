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
 *	@(#)dca.c	7.2 (Berkeley) %G%
 */

#ifdef DCACONSOLE
#include "sys/param.h"
#include "../dev/dcareg.h"
#include "../include/cpu.h"
#include "../hp300/cons.h"

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
