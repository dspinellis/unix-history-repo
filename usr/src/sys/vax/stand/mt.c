/*	mt.c	4.1	81/12/01	*/

/*
 * TM78/TU78 tape driver
 */

#include "../h/mtreg.h"
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/mbareg.h"
#include "saio.h"
#include "savax.h"

short	mttypes[] =
	{ MBDT_TU78, 0 };

#define	MASKREG(reg)	((reg)&0xffff)

mtopen(io)
	register struct iob *io;
{
	register int skip;
	register struct mtdevice *mtaddr = (struct mtdevice *)mbadrv(io->i_unit);
	int i;

	for (i = 0; mttypes[i]; i++)
		if (mttypes[i] == (mtaddr->mtdt&MBDT_TYPE))
			goto found;
	_stop("not a tape\n");
found:
	mbainit(UNITTOMBA(io->i_unit));
	mtaddr->mtid = MTID_CLR;
	DELAY(250);
	while ((mtaddr->mtid & MTID_RDY) == 0)
		;
	mtstrategy(io, MT_REW);
	skip = io->i_boff;
	while (skip--) {
		io->i_cc = -1;
		mtstrategy(io, MT_SFORWF);
	}
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

	errcnt = 0;
retry:
	if (func == READ || func == WRITE) {
		mtaddr->mtca = 1<<2;	/* 1 record */
		mtaddr->mtbc = io->i_cc;
		mtaddr->mter = 0;
		mbastart(io, func);
		do
			s = mtaddr->mter & MTER_INTCODE;
		while (s == 0);
		ic = s;
		DELAY(2000);
	} else {
		mtaddr->mtas = -1;
		mtaddr->mtncs[0] = (-io->i_cc << 8)|func|MT_GO;
	rwait:
		do
			s = mtaddr->mtas&0xffff;
		while (s == 0);
		mtaddr->mtas = mtaddr->mtas;	/* clear attention */
		ic = mtaddr->mtner & MTER_INTCODE;
	}
	switch (ic) {
	case MTER_TM:
	case MTER_EOT:
	case MTER_LEOT:
		return (0);

	case MTER_DONE:
		break;

	case MTER_RWDING:
		goto rwait;
	default:
		printf("mt hard error: er=%b\n",
		    MASKREG(mtaddr->mter));
		mtaddr->mtid = MTID_CLR;
		DELAY(250);
		while ((mtaddr->mtid & MTID_RDY) == 0)
			;
		return (-1);

	case MTER_RETRY:
		printf("mt error: er=%b\n",
		    MASKREG(mtaddr->mter));
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
