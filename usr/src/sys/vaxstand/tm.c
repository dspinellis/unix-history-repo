/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tm.c	7.6 (Berkeley) 4/4/90
 */

/*
 * TM11/TE??
 */

#include "param.h"

#include "../vax/pte.h"

#include "../vaxuba/ubareg.h"
#include "../vaxuba/tmreg.h"

#include "saio.h"
#include "savax.h"

#define	MAXCTLR		1		/* all addresses must be specified */
u_short	tmstd[MAXCTLR] = { 0172520 };

tmopen(io)
	register struct iob *io;
{
	register int skip;

	if ((u_int)io->i_adapt >= nuba)
		return (EADAPT);
	if ((u_int)io->i_ctlr >= MAXCTLR)
		return (ECTLR);
	if (badaddr((char *)ubamem(io->i_adapt, tmstd[io->i_ctlr]), sizeof(short)))
		return (ENXIO);
	tmstrategy(io, TM_REW);
	for (skip = io->i_part; skip--;) {
		io->i_cc = 0;
		tmstrategy(io, TM_SFORW);
	}
	return (0);
}

tmclose(io)
	register struct iob *io;
{
	tmstrategy(io, TM_REW);
}

tmstrategy(io, func)
	register struct iob *io;
{
	register int com, errcnt;
	register struct tmdevice *tmaddr;
	int word, info;

	tmaddr = (struct tmdevice *)ubamem(io->i_adapt, tmstd[io->i_ctlr]);
	errcnt = 0;
retry:
	tmquiet(tmaddr);
	info = ubasetup(io, 1);
	tmaddr->tmbc = -io->i_cc;
	tmaddr->tmba = info;
	com = (io->i_unit<<8) | TM_GO;
	if (func == READ)
		tmaddr->tmcs = com | TM_RCOM;
	else if (func == WRITE)
		tmaddr->tmcs = com | TM_WCOM;
	else if (func == TM_SREV) {
		tmaddr->tmbc = -1;
		tmaddr->tmcs = com | TM_SREV;
		return (0);
	} else
		tmaddr->tmcs = com | func;
	for (;;) {
		word = tmaddr->tmcs;
		DELAY(100);
		if (word & TM_CUR)
			break;
	}
	ubafree(io, info);
	word = tmaddr->tmer;
	if (word & TMER_EOT)
		return (0);
	if (word & TM_ERR) {
		if (word & TMER_EOF)
			return (0);
		printf("tm tape error: er=%b\n", word, TMER_BITS);
		if (errcnt++ == 10) {
			printf("tm: unrecovered error\n");
			return (-1);
		}
		tmstrategy(io, TM_SREV);
		goto retry;
	}
	if (errcnt)
		printf("tm: recovered by retry\n");
	if (word & TMER_EOF)
		return (0);
	return (io->i_cc + tmaddr->tmbc);
}

tmquiet(tmaddr)
	register struct tmdevice *tmaddr;
{
	register word;
	for (;;) {
		word = tmaddr->tmcs;
		DELAY(100);
		if (word&TM_CUR)
			break;
	}
	for (;;) {
		word = tmaddr->tmer;
		DELAY(100);
		if ((word&TMER_TUR) && (word&TMER_SDWN)==0)
			break;
	}
}
