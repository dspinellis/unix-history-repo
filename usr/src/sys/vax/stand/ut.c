/*	ut.c	4.1	81/12/01	*/

/*
 * SI Model 9700 -- emulates TU45 on the UNIBUS
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/ubareg.h"
#include "saio.h"
#include "savax.h"

#include "../h/utreg.h"

u_short	utstd[] = { 0172440 };		/* non-standard */

utopen(io)
	register struct iob *io;
{
	register skip;

	utstrategy(io, UT_REW);
	skip = io->i_boff;
	while (skip--) {
		io->i_cc = 0;
		utstrategy(io, UT_SFORW);
	}
}

utclose(io)
	register struct iob *io;
{
	utstrategy(io, UT_REW);
}

#define utwait(addr)	{do word=addr->utcs1; while((word&UT_RDY)==0);}

utstrategy(io, func)
	register struct iob *io;
{
	register u_short word;
	register int errcnt;
	register struct utdevice *addr =
	    (struct utdevice *)ubamem(io->i_unit, utstd[0]);
	int info;
	u_short dens;

	dens = (io->i_unit&07) | PDP11FMT | UT_PE;
	errcnt = 0;
retry:
	utquiet(addr);
	addr->uttc = dens;
	info = ubasetup(io, 1);
	addr->utwc = -((io->i_cc+1) >> 1);
	addr->utfc = -io->i_cc;
	if (func == READ) {
		addr->utba = info;
		addr->utcs1 = UT_RCOM | ((info>>8) & 0x30) | UT_GO;
	} else if (func == WRITE) {
		addr->utba = info;
		addr->utcs1 = UT_WCOM | ((info>>8) & 0x30) | UT_GO;
	} else if (func == UT_SREV) {
		addr->utcs1 = UT_SREV | UT_GO;
		return (0);
	} else
		addr->utcs1 = func | UT_GO;
	utwait(addr);
	ubafree(io, info);
	word = addr->utds;
	if (word&(UTDS_EOT|UTDS_TM)) {
		addr->utcs1 = UT_CLEAR | UT_GO;
		return(0);
	}
	if ((word&UTDS_ERR) || (addr->utcs1&UT_TRE)) {
		if (errcnt == 0)
			printf("tj error: cs1=%b er=%b cs2=%b ds=%b",
				addr->utcs1, UT_BITS, addr->uter, UTER_BITS,
				addr->utcs2, UTCS2_BITS, word, UTDS_BITS);
		if (errcnt == 10) {
			printf("\n");
			return(-1);
		}
		errcnt++;
		if (addr->utcs1&UT_TRE)
			addr->utcs2 |= UTCS2_CLR;
		addr->utcs1 = UT_CLEAR | UT_GO;
		utstrategy(io, UT_SREV);
		utquiet(addr);
		if (func == WRITE) {
			addr->utcs1 = UT_ERASE | UT_GO;
			utwait(addr);
		}
		goto retry;
	}
	if (errcnt)
		printf(" recovered by retry\n");
	return ((-addr->utfc) & 0xffff);
}

utquiet(addr)
	register struct utdevice *addr;
{
	register u_short word;

	utwait(addr);
	do
		word = addr->utds;
	while ((word&UTDS_DRY) == 0 && (word&UTDS_PIP));
}
