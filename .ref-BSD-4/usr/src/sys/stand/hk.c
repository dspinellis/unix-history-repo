/*	hk.c	4.1	11/9/80	*/

/*
 * RK7 disk driver, standalone version
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "saio.h"

#define	RK7ADDR	((struct rk7_regs *)(PHYSUMEM - 0160000 + 0177440))
#define FORMAT_22 0
#define	RESET	0102000
#define	WCOM	022
#define	RCOM	020
#define RK07	02000
#define	GO	01
#define RELEASE	010
#define	CTLRDY	0200
#define	PACKAK	000003	/* Pack Acknowledge */


struct	rk7_regs
{
	short rk7cs1;
	short rk7wc;
	unsigned short rk7ba;
	short rk7da;
	short rk7cs2;
	short rk7ds;
	short	rk7er;
	short rk7asof;
	short rk7dc;
	short rk7null;
	short rk7db;
	short rk7mr1;
	short rk7ecps;
	short rk7ecpt;
	short rk7mr2;
	short rk7mr3;
};

rk7strategy(io, func)
register struct iob *io;
{
	register short com;
	daddr_t bn;
	short dn, cn, sn, tn;
	int ubinfo;

	ubinfo = ubasetup( io, 1 );
	bn = io->i_bn;
	dn = io->i_unit;
	cn = bn/66;
	sn = bn%22;
	tn = (bn / 22) % 3;

	RK7ADDR -> rk7cs2 = dn;
	RK7ADDR -> rk7cs1 = PACKAK | RK07;
	while((com = RK7ADDR->rk7cs1) & 01);

	RK7ADDR -> rk7da = sn | (tn << 8);
	RK7ADDR -> rk7dc = cn;
	RK7ADDR->rk7ba = ubinfo;
	RK7ADDR->rk7wc = -(io->i_cc >> 1);

	com = ((ubinfo & 0x30000) >> 8 ) | RK07 | GO | ( FORMAT_22<< 12);
	if(func == READ)
		com |= RCOM; else
		com |= WCOM;
	RK7ADDR->rk7cs1 = com;

	while (((com = RK7ADDR->rk7cs1) & CTLRDY) == 0);
	while(RK7ADDR->rk7ds >= 0);

	ubafree (ubinfo);
	if (RK7ADDR->rk7cs1 < 0) {		/* error bit */
		printf ("RK07 error: unit %d, cyl %d, trk %d, sect %d, ",
			io->i_unit, cn, tn, sn);
		printf ("cs1 %X, cs2 %X, err %X\n",
			RK7ADDR->rk7cs1, RK7ADDR->rk7cs2, RK7ADDR->rk7er);

		RK7ADDR->rk7cs1 = RESET|GO;
		while(((com = RK7ADDR->rk7cs1)&CTLRDY) == 0);
		return (-1);
	}
	return (io->i_cc);
}
