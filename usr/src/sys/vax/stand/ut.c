/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ut.c	7.8 (Berkeley) %G%
 */

/*
 * SI Model 9700 -- emulates TU45 on the UNIBUS
 */

#include "sys/param.h"

#include "../include/pte.h"

#include "../uba/ubareg.h"
#include "../uba/utreg.h"

#include "stand/saio.h"
#include "savax.h"

#define	MASKREG(reg)	((reg)&0xffff)

#define	MAXCTLR		1		/* all addresses must be specified */
u_short	utstd[MAXCTLR] = { 0172440 };	/* non-standard */

utopen(io)
	register struct iob *io;
{
	register int skip;

	if ((u_int)io->i_adapt >= nuba)
		return (EADAPT);
	if ((u_int)io->i_ctlr >= MAXCTLR)
		return (ECTLR);
	if (badaddr((char *)ubamem(io->i_unit, utstd[io->i_ctlr]), sizeof(short)))
		return (ENXIO);
	utstrategy(io, UT_REW);
	for (skip = io->i_part; skip--;)
		utstrategy(io, UT_SFORWF);
	return (0);
}

utclose(io)
	register struct iob *io;
{
	utstrategy(io, UT_REW);
}

#define	UTWAIT(addr) { \
	do \
		word = addr->utcs1; \
	while((word&UT_RDY) == 0); \
}

utstrategy(io, func)
	register struct iob *io;
{
	register struct utdevice *addr;
	register u_short word;
	register int errcnt;
	int info, resid;
	u_short dens;

	addr = (struct utdevice *)ubamem(io->i_unit, utstd[io->i_ctlr]);
	dens = io->i_unit | PDP11FMT | UT_PE;
	errcnt = 0;
retry:
	utquiet(addr);
	addr->uttc = dens;
	info = ubasetup(io, 1);
	addr->utwc = -((io->i_cc+1) >> 1);
	addr->utfc = -io->i_cc;
	if (func == F_READ) {
		addr->utba = info;
		addr->utcs1 = UT_RCOM | ((info>>8) & 0x30) | UT_GO;
	} else if (func == F_WRITE) {
		addr->utba = info;
		addr->utcs1 = UT_WCOM | ((info>>8) & 0x30) | UT_GO;
	} else if (func == UT_SREV) {
		addr->utcs1 = UT_SREV | UT_GO;
		return (0);
	} else
		addr->utcs1 = func | UT_GO;
	UTWAIT(addr);
	ubafree(io, info);
	word = addr->utds;
	if (word&(UTDS_EOT|UTDS_TM)) {
		addr->utcs1 = UT_CLEAR | UT_GO;
		goto done;
	}
	if ((word&UTDS_ERR) || (addr->utcs1&UT_TRE)) {
		printf("ut error: cs1=%b er=%b cs2=%b ds=%b",
		  addr->utcs1, UT_BITS, addr->uter, UTER_BITS,
		  addr->utcs2, UTCS2_BITS, word, UTDS_BITS);
		if (errcnt++ == 10) {
			printf("ut: unrecovered error\n");
			return (-1);
		}
		if (addr->utcs1&UT_TRE)
			addr->utcs2 |= UTCS2_CLR;
		addr->utcs1 = UT_CLEAR | UT_GO;
		utstrategy(io, UT_SREV);
		utquiet(addr);
		if (func == F_WRITE) {
			addr->utcs1 = UT_ERASE | UT_GO;
			UTWAIT(addr);
		}
		goto retry;
	}
	if (errcnt)
		printf("ut: recovered by retry\n");
done:
	if (func == F_READ) {
		resid = 0;
		if (io->i_cc > MASKREG(addr->utfc))
			resid = io->i_cc - MASKREG(addr->utfc);
	} else
		resid = MASKREG(-addr->utfc);
	return (io->i_cc - resid);
}

static
utquiet(addr)
	register struct utdevice *addr;
{
	register u_short word;

	UTWAIT(addr);
	do
		word = addr->utds;
	while ((word&UTDS_DRY) == 0 && (word&UTDS_PIP));
}
