/*	mt.c	4.1	81/12/01	*/

/*
 * TM78/TU78 tape driver
 * Original obtained from 4.1a bsd tape
 * Local changes by Jeffrey R. Schwab	June, 1982
 *			Purdue University Computing Center
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

	/* clear any attention bits present on open */

	i = mtaddr->mtner;
	mtaddr->mtas = mtaddr->mtas;

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
		printf("mt error: er=%x\n",
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
