/*	rk.c	4.1	%G%	*/

/*
 * RK disk driver, standalone version
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "saio.h"

#define	RKADDR	((struct rk_regs *)(PHYSUMEM - 0160000 + 0177440))
#define FORMAT_22 0
#define	RESET	0102000
#define	WCOM	022
#define	RCOM	020
#define RK07	02000
#define	GO	01
#define RELEASE	010
#define	CTLRDY	0200
#define	PACKAK	000003	/* Pack Acknowledge */

struct	rk_regs
{
	short	rkcs1;
	short	rkwc;
	u_short	rkba;
	short	rkda;
	short	rkcs2;
	short	rkds;
	short	rker;
	short	rkasof;
	short	rkdc;
	short	rknull;
	short	rkdb;
	short	rkmr1;
	short	rkecps;
	short	rkecpt;
	short	rkmr2;
	short	rkmr3;
};

struct	devsize {
	daddr_t	cyloff;
} rk_sizes[] = {
	0, 146, 246, -1, -1, -1, -1, -1,
};

rkopen(io)
register struct iob *io;
{

	if (rk_sizes[io->i_boff].cyloff == -1 ||
	    io->i_boff < 0 || io->i_boff > 7)
		_stop("rk bad unit");
	io->i_boff = rk_sizes[io->i_boff].cyloff * 66;
}

rkstrategy(io, func)
register struct iob *io;
{
	register short com;
	daddr_t bn;
	short dn, cn, sn, tn;
	int ubinfo;

	ubinfo = ubasetup(io, 1);
	bn = io->i_bn;
	dn = io->i_unit;
	cn = bn/66;
	sn = bn%22;
	tn = (bn / 22) % 3;

	RKADDR->rkcs2 = dn;
	RKADDR->rkcs1 = PACKAK | RK07;
	while ((com = RKADDR->rkcs1) & 01)
		;
	RKADDR->rkda = sn | (tn << 8);
	RKADDR->rkdc = cn;
	RKADDR->rkba = ubinfo;
	RKADDR->rkwc = -(io->i_cc >> 1);
	com = ((ubinfo & 0x30000) >> 8) | RK07 | GO | (FORMAT_22 << 12);
	if(func == READ)
		com |= RCOM; else
		com |= WCOM;
	RKADDR->rkcs1 = com;
	while (((com = RKADDR->rkcs1) & CTLRDY) == 0)
		;
	while(RKADDR->rkds >= 0)
		;
	ubafree(ubinfo);
	if (RKADDR->rkcs1 < 0) {		/* error bit */
		printf("RK07 error: unit %d, cyl %d, trk %d, sect %d, ",
			io->i_unit, cn, tn, sn);
		printf("cs1 %X, cs2 %X, err %X\n",
			RKADDR->rkcs1, RKADDR->rkcs2, RKADDR->rker);
		RKADDR->rkcs1 = RESET|GO;
		while(((com = RKADDR->rkcs1)&CTLRDY) == 0)
			;
		return (-1);
	}
	return (io->i_cc);
}
