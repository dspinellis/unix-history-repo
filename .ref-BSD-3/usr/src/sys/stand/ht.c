/*
 * TJU16 tape driver
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/mba.h"
#include "saio.h"

struct	device
{
	int	htcs1;
	int	htds;
	int	hter;
	int	htmr;
	int	htas;
	int	htfc;
	int	htdt;
	int	htck;
	int	htsn;
	int	httc;
};

#define	HTADDR	((struct device *)(PHYSMBA1 + MBA_ERB))

#define	GO	01
#define	WCOM	060
#define	RCOM	070
#define	NOP	0
#define	WEOF	026
#define	SFORW	030
#define	SREV	032
#define	ERASE	024
#define	REW	06
#define	DCLR	010
#define P800	01700		/* 800 + pdp11 mode */
#define	P1600	02300		/* 1600 + pdp11 mode */
#define	IENABLE	0100
#define	RDY	0200
#define	TM	04
#define	DRY	0200
#define EOT	02000
#define CS	02000
#define COR	0100000
#define PES	040
#define WRL	04000
#define MOL	010000
#define ERR	040000
#define FCE	01000
#define	TRE	040000
#define HARD	064023	/* UNS|OPI|NEF|FMT|RMR|ILR|ILF */

#define	SIO	1
#define	SSFOR	2
#define	SSREV	3
#define SRETRY	4
#define SCOM	5
#define SOK	6

htopen(io)
register struct iob *io;
{
	register skip;
	int i;

	htinit();
	htstrategy(io, REW);
	skip = io->i_boff;
	while (skip--) {
		io->i_cc = -1;
		while (htstrategy(io, SFORW))
			;
		i = 65536;
		while (--i)
			;
		htstrategy(io, NOP);
	}
}

htclose(io)
register struct iob *io;
{
	htstrategy(io, REW);
}

htstrategy(io, func)
register struct iob *io;
{
	register int unit, den, errcnt, ds;
	short fc;

	unit = io->i_unit;
	errcnt = 0;
retry:
	if(unit & 1)
		den = P1600;
	else
		den = P800;
	htquiet();
	if((HTADDR->httc&03777) != den)
		HTADDR->httc = den;
	HTADDR->htfc = -io->i_cc;
	if (func == SREV) {
		HTADDR->htfc = -1;
		HTADDR->htcs1 = SREV | GO;
		return(0);
	}
	if (func == READ || func == WRITE)
		mbastart(io, HTADDR, func);
	else
		HTADDR->htcs1 = func | GO;
	htquiet();
	ds = HTADDR->htds & TM;
	if (ds&TM) {
		htinit();
		return(0);
	}
	if (ds&ERR) {
		if (errcnt == 0)
			printf("tape error: ds=%x, er=%x, mbasr=%x",
			    HTADDR->htds, HTADDR->hter, PHYSMBA1->mba_sr);
		htinit();
		if (errcnt == 10) {
			printf("\n");
			return(-1);
		}
		errcnt++;
		htstrategy(io, SREV);
		goto retry;
	}
	if (errcnt)
		printf(" recovered by retry\n");
	fc = HTADDR->htfc;
	return(io->i_cc+fc);
}

htinit()
{

	HTADDR->htcs1 = DCLR|GO;
}

htquiet()
{
	register int s;

	do
		s = HTADDR->htds;
	while ((s & RDY) == 0);
}
