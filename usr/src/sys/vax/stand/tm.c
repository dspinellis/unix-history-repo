/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tm.c	7.2 (Berkeley) %G%
 */

/*
 * TM11/TE??
 */
#include "../machine/pte.h"

#include "param.h"
#include "inode.h"
#include "fs.h"

#include "../vaxuba/ubareg.h"
#include "../vaxuba/tmreg.h"

#include "saio.h"
#include "savax.h"


u_short	tmstd[] = { 0172520 };

tmopen(io)
	register struct iob *io;
{
	register skip;

	if (badaddr((char *)ubamem(io->i_unit, tmstd[0]), sizeof (short))) {
		printf("nonexistent device\n");
		return (ENXIO);
	}
	tmstrategy(io, TM_REW);
	skip = io->i_boff;
	while (skip--) {
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
	register int com, unit, errcnt;
	register struct tmdevice *tmaddr =
	    (struct tmdevice *)ubamem(io->i_unit, tmstd[0]);
	int word, info;

	unit = io->i_unit;
	errcnt = 0;
retry:
	tmquiet(tmaddr);
	com = (unit<<8);
	info = ubasetup(io, 1);
	tmaddr->tmbc = -io->i_cc;
	tmaddr->tmba = info;
	if (func == READ)
		tmaddr->tmcs = com | TM_RCOM | TM_GO;
	else if (func == WRITE)
		tmaddr->tmcs = com | TM_WCOM | TM_GO;
	else if (func == TM_SREV) {
		tmaddr->tmbc = -1;
		tmaddr->tmcs = com | TM_SREV | TM_GO;
		return (0);
	} else
		tmaddr->tmcs = com | func | TM_GO;
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
		if (errcnt == 0)
			printf("te error: er=%b", word, TMER_BITS);
		if (errcnt == 10) {
			printf("\n");
			return (-1);
		}
		errcnt++;
		tmstrategy(io, TM_SREV);
		goto retry;
	}
	if (errcnt)
		printf(" recovered by retry\n");
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
