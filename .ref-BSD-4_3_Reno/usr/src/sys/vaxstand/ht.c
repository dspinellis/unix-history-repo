/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ht.c	7.5 (Berkeley) 4/4/90
 */

/*
 * TM03/TU?? tape driver
 */
#include "machine/pte.h"

#include "param.h"

#include "../vaxmba/htreg.h"
#include "../vaxmba/mbareg.h"

#include "saio.h"
#include "savax.h"

short	httypes[] =
	{ MBDT_TM03, MBDT_TE16, MBDT_TU45, MBDT_TU77, 0 };

#define	MASKREG(reg)	((reg)&0xffff)

htopen(io)
	register struct iob *io;
{
	register struct htdevice *htaddr;
	register int i, skip;

	htaddr = (struct htdevice *)mbadrv(io->i_adapt, io->i_ctlr);
	if (mbainit(io->i_adapt) == 0)
		return (EADAPT);
	for (i = 0;; i++) {
		if (!httypes[i]) {
			printf("ht: not a tape\n");
			return (ENXIO);
		}
		if (httypes[i] == (htaddr->htdt&MBDT_TYPE))
			break;
	}
	htaddr->htcs1 = HT_DCLR|HT_GO;
	htstrategy(io, HT_REW);
	for (skip = io->i_part; skip--;) {
		io->i_cc = -1;
		while (htstrategy(io, HT_SFORW));
		DELAY(65536);
		htstrategy(io, HT_SENSE);
	}
	return (0);
}

htclose(io)
	register struct iob *io;
{
	htstrategy(io, HT_REW);
}

htstrategy(io, func)
	register struct iob *io;
	int func;
{
	register struct htdevice *htaddr;
	register int den, errcnt, ds;
	int er;
	short fc;

	errcnt = 0;
	htaddr = (struct htdevice *)mbadrv(io->i_adapt, io->i_ctlr);
retry:
	den = HTTC_1600BPI | HTTC_PDP11 | io->i_unit;
	htquiet(htaddr);
	htaddr->htcs1 = HT_DCLR|HT_GO;
	htaddr->httc = den;
	htaddr->htfc = -io->i_cc;
	if (func == HT_SREV) {
		htaddr->htfc = -1;
		htaddr->htcs1 = HT_SREV|HT_GO;
		return (0);
	}
	if (func == READ || func == WRITE)
		mbastart(io, io->i_ctlr, func);
	else
		htaddr->htcs1 = func|HT_GO;
	htquiet(htaddr);
	ds = htaddr->htds;
	er = htaddr->hter;
	if (ds & HTDS_TM) {
		htaddr->htcs1 = HT_DCLR|HT_GO;
		return (0);
	}
	if (ds & HTDS_ERR) {
		htaddr->htcs1 = HT_DCLR|HT_GO;
		if ((er & HTER_CORCRC) == 0) {
			printf("ht error: ds=%b, er=%b\n",
			    MASKREG(ds), HTDS_BITS,
			    MASKREG(er), HTER_BITS);
			if (errcnt++ == 10) {
				printf("ht: unrecovered error\n");
				return (-1);
			}
			htstrategy(io, HT_SREV);
			goto retry;
		}
	}
	if (errcnt)
		printf("ht: recovered by retry\n");
	fc = htaddr->htfc;
	return (io->i_cc+fc);
}

static
htquiet(htaddr)
	register struct htdevice *htaddr;
{
	register int s;

	do
		s = htaddr->htds;
	while ((s & HTDS_DRY) == 0);
}
