/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mt.c	7.2 (Berkeley) %G%
 */

/*
 * TM78/TU78 tape driver
 * Made to work reliably by by Jeffrey R. Schwab (Purdue)
 */
#include "../machine/pte.h"

#include "param.h"
#include "inode.h"
#include "fs.h"

#include "../vaxmba/mtreg.h"
#include "../vaxmba/mbareg.h"

#include "saio.h"
#include "savax.h"

short	mttypes[] =
	{ MBDT_TU78, 0 };

#define	MASKREG(reg)	((reg)&0xffff)

mtopen(io)
	register struct iob *io;
{
	register int skip;
	register struct mtdevice *mtaddr =
		(struct mtdevice *)mbadrv(io->i_unit);
	register int i;

	if (mbainit(UNITTOMBA(io->i_unit)) == 0)
		return (ENXIO);
	for (i = 0; mttypes[i]; i++)
		if (mttypes[i] == (mtaddr->mtdt&MBDT_TYPE))
			goto found;
	printf("not a tape\n");
	return (ENXIO);
found:
	mtaddr->mtid = MTID_CLR;
	DELAY(250);
	while ((mtaddr->mtid & MTID_RDY) == 0)
		;

	/* clear any attention bits present on open */
	i = mtaddr->mtner;
	mtaddr->mtas = mtaddr->mtas;

	mtstrategy(io, MT_REW);
	skip = io->i_boff;
	while (skip--) {
		io->i_cc = -1;
		mtstrategy(io, MT_SFORWF);
	}
	return (0);
}

mtclose(io)
	register struct iob *io;
{

	mtstrategy(io, MT_REW);
}

mtstrategy(io, func)
	register struct iob *io;
	int func;
{
	register int errcnt, s, ic;
	register struct mtdevice *mtaddr =
	    (struct mtdevice *)mbadrv(io->i_unit);
	struct mba_regs *mba = mbamba(io->i_unit);

	errcnt = 0;
retry:
	/* code to trap for attention up prior to start of command */
	if ((mtaddr->mtas & 0xffff) != 0) {
		printf("mt unexpected attention er=%x - continuing\n",
			MASKREG(mtaddr->mtner));
		mtaddr->mtas = mtaddr->mtas;
	}

	if (func == READ || func == WRITE) {
		mtaddr->mtca = 1<<2;	/* 1 record */
		mtaddr->mtbc = io->i_cc;
		mbastart(io, func);
		/* wait for mba to go idle and read result status */
		while((mba->mba_sr & MBSR_DTBUSY) != 0)
			;
		ic = mtaddr->mter & MTER_INTCODE;
	} else {
		mtaddr->mtncs[0] = (-io->i_cc << 8)|func|MT_GO;
	rwait:
		do
			s = mtaddr->mtas&0xffff;
		while (s == 0);
		ic = mtaddr->mtner & MTER_INTCODE;
		mtaddr->mtas = mtaddr->mtas;	/* clear attention */
	}
	switch (ic) {
	case MTER_TM:
	case MTER_EOT:
	case MTER_LEOT:
		return (0);

	case MTER_DONE:
		/* make sure a record was read */
		if ((mtaddr->mtca & (1 << 2)) != 0) {
			printf("mt record count not decremented - retrying\n");
			goto retry;
		}
		break;

	case MTER_RWDING:
		goto rwait;
	default:
		printf("mt hard error: er=%x\n",
		    MASKREG(mtaddr->mter));
		mtaddr->mtid = MTID_CLR;
		DELAY(250);
		while ((mtaddr->mtid & MTID_RDY) == 0)
			;
		return (-1);

	case MTER_RETRY:
		printf("mt error: er=%x\n", MASKREG(mtaddr->mter));
		if (errcnt == 10) {
			printf("mt: unrecovered error\n");
			return (-1);
		}
		errcnt++;
		goto retry;
	}
	if (errcnt)
		printf("mt: recovered by retry\n");
	return (io->i_cc);	/* NO PARTIAL RECORD READS!!! */
}
