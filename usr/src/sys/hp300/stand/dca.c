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
 *	@(#)dca.c	7.5 (Berkeley) %G%
 */

#ifdef DCACONSOLE
#include <sys/param.h>
#include <hp/dev/dcareg.h>
#include <machine/cpu.h>
#include <hp/dev/cons.h>

struct dcadevice *dcacnaddr = 0;

dcaprobe(cp)
	struct consdev *cp;
{
	register struct dcadevice *dca;

	dcacnaddr = (struct dcadevice *) sctoaddr(CONSCODE);
	if (badaddr((char *)dcacnaddr)) {
		cp->cn_pri = CN_DEAD;
		return;
	}
#ifdef FORCEDCACONSOLE
	cp->cn_pri = CN_REMOTE;
#else
	dca = dcacnaddr;
	switch (dca->dca_id) {
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
#endif
}

dcainit(cp)
	struct consdev *cp;
{
	register struct dcadevice *dca = dcacnaddr;

	dca->dca_reset = 0xFF;
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
	register struct dcadevice *dca = dcacnaddr;
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
	register struct dcadevice *dca = dcacnaddr;
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
