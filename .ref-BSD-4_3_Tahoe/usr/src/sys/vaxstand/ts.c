/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ts.c	7.5 (Berkeley) 7/9/88
 */

/*
 * TS11 tape driver
 */

#include "param.h"
#include "inode.h"
#include "fs.h"

#include "../vax/pte.h"

#include "../vaxuba/tsreg.h"
#include "../vaxuba/ubareg.h"

#include "saio.h"
#include "savax.h"

#define	MAXCTLR		1		/* all addresses must be specified */
u_short	tsstd[MAXCTLR] = { 0772520 };

struct	iob	ctsbuf;

u_short	ts_uba;			/* Unibus address of ts structure */

struct ts {
	struct ts_cmd ts_cmd;
	struct ts_char ts_char;
	struct ts_sts ts_sts;
} ts;

tsopen(io)
	register struct iob *io;
{
	static struct ts *ts_ubaddr;
	register struct tsdevice *tsaddr;
	long i = 0;

	if ((u_int)io->i_adapt >= nuba)
		return (EADAPT);
	if ((u_int)io->i_ctlr >= MAXCTLR)
		return (ECTLR);
	/* TS11 only supports one transport per formatter */
	if (io->i_unit)
		return(EUNIT);
	tsaddr = (struct tsdevice *)ubamem(io->i_adapt, tsstd[io->i_ctlr]);
	if (badaddr((char *)tsaddr, sizeof (short)))
		return (ENXIO);
	tsaddr->tssr = 0;
	while ((tsaddr->tssr & TS_SSR)==0) {
		DELAY(10);
		if (++i > 1000000) {
			printf("ts: not ready\n");
			return (ENXIO);
		}
	}
	if (tsaddr->tssr&TS_OFL) {
		printf("ts: offline\n");
		return (ENXIO);
	}
	if (tsaddr->tssr&TS_NBA) {
		int i;

		ctsbuf.i_ma = (caddr_t) &ts;
		ctsbuf.i_cc = sizeof(ts);
		if (ts_ubaddr == 0)
			ts_ubaddr = (struct ts *)ubasetup(&ctsbuf, 2);
		ts_uba = (u_short)((long)ts_ubaddr + (((long)ts_ubaddr>>16)&03));
		ts.ts_char.char_addr = (int)&ts_ubaddr->ts_sts;
		ts.ts_char.char_size = sizeof(ts.ts_sts);
		ts.ts_char.char_mode = TS_ESS;
		ts.ts_cmd.c_cmd = TS_ACK|TS_SETCHR;
		i = (int)&ts_ubaddr->ts_char;
		ts.ts_cmd.c_loba = i;
		ts.ts_cmd.c_hiba = (i>>16)&3;
		ts.ts_cmd.c_size = sizeof(ts.ts_char);
		tsaddr->tsdb = ts_uba;
	}
	tsstrategy(io, TS_REW);
	if (io->i_cc = io->i_part)
		tsstrategy(io, TS_SFORWF);
	return (0);
}

tsclose(io)
	register struct iob *io;
{
	tsstrategy(io, TS_REW);
}

tsstrategy(io, func)
	register struct iob *io;
{
	register struct tsdevice *tsaddr;
	register int errcnt, info;

	tsaddr = (struct tsdevice *)ubamem(io->i_adapt, tsstd[io->i_ctlr]);
	errcnt = info = 0;
retry:
	while ((tsaddr->tssr & TS_SSR) == 0)
		DELAY(100);
	if (func == TS_REW || func == TS_SFORWF)
		ts.ts_cmd.c_repcnt = io->i_cc;
	else {
		info = ubasetup(io, 1);
		ts.ts_cmd.c_size = io->i_cc;
		ts.ts_cmd.c_loba = info;
		ts.ts_cmd.c_hiba = (info>>16)&3;
		if (func == READ)
			func = TS_RCOM;
		else if (func == WRITE)
			func = TS_WCOM;
	}
	ts.ts_cmd.c_cmd = TS_ACK|TS_CVC|func;
	tsaddr->tsdb = ts_uba;
	do
		DELAY(100)
	while ((tsaddr->tssr & TS_SSR) == 0);
	if (info)
		ubafree(io, info);
	if (ts.ts_sts.s_xs0 & TS_TMK)
		return (0);
	if (tsaddr->tssr & TS_SC) {
		printf("ts tape error: er=%b, xs0=%b\n",
		    tsaddr->tssr, TSSR_BITS,
		    ts.ts_sts.s_xs0, TSXS0_BITS);
		if (errcnt++ == 10) {
			printf("ts: unrecovered error\n");
			return (-1);
		}
		if (func == TS_RCOM || func == TS_WCOM)
			func |= TS_RETRY;
		goto retry;
	}
	if (errcnt)
		printf("ts: recovered by retry\n");
	return (io->i_cc - ts.ts_sts.s_rbpcr);
}
