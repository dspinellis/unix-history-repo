/*
 * TJU16 tape driver
 */

#include <sys/param.h>
#include <sys/inode.h>
#include "saio.h"

struct	device
{
	int	htcs1;
	int	htwc;
	caddr_t	htba;
	int	htfc;
	int	htcs2;
	int	htds;
	int	hter;
	int	htas;
	int	htck;
	int	htdb;
	int	htmr;
	int	htdt;
	int	htsn;
	int	httc;
	int	htbae;	/* 11/70 bus extension */
	int	htcs3;
};



#define	HTADDR	((struct device *)0172440)

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
#define CLR	040
#define P800	01300		/* 800 + pdp11 mode */
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
#define PIP	020000
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

	htstrategy(io, REW);
	skip = io->i_boff;
	while (skip--) {
		io->i_cc = -1;
		while (htstrategy(io, SFORW))
			;
		i = 0;
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
	register unit, den, errcnt;

	unit = io->i_unit;
	errcnt = 0;
retry:
	HTADDR->htcs2 = unit&03;
	if(unit > 3)
		den = P1600;
	else
		den = P800;
	htquiet();
	if((HTADDR->httc&03777) != den)
		HTADDR->httc = den;
	HTADDR->htba = io->i_ma;
	HTADDR->htfc = -io->i_cc;
	HTADDR->htwc = -(io->i_cc>>1);
	den = ((segflag) << 8) | GO;
	if (func == READ)
		den =| RCOM;
	else if (func == WRITE)
		den =| WCOM;
	else if (func == SREV) {
		HTADDR->htfc = -1;
		HTADDR->htcs1 = den | SREV;
		return(0);
	} else
		den |= func;
	HTADDR->htcs1 = den;
	while ((HTADDR->htcs1&RDY) == 0)
		;
	if (HTADDR->htds&TM) {
		htinit();
		return(0);
	}
	if (HTADDR->htcs1&TRE) {
		if (errcnt == 0)
			printf("tape error: cs2=%o, er=%o",
			    HTADDR->htcs2, HTADDR->hter);
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
	return(io->i_cc+HTADDR->htfc);
}

htinit()
{
	int omt, ocs2;

	omt = HTADDR->httc & 03777;
	ocs2 = HTADDR->htcs2 & 07;

	HTADDR->htcs2 = CLR;
	HTADDR->htcs2 = ocs2;
	HTADDR->httc = omt;
	HTADDR->htcs1 = DCLR|GO;
}

htquiet()
{
	while ((HTADDR->htcs1&RDY) == 0)
		;
	while (HTADDR->htds&PIP)
		;
}
