/*	rk.c	4.3	81/03/22	*/

/*
 * RK611/RK07
 */

#include "../h/param.h"
#include "../h/rkreg.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/ubareg.h"
#include "saio.h"
#include "savax.h"

u_short	rkstd[] = { 0777440 };
short	rk_off[] = { 0, 241, 0, -1, -1, -1, 393, -1 };

rkopen(io)
	register struct iob *io;
{
	register struct rkdevice *rkaddr = (struct rkdevice *)ubamem(io->i_unit, rkstd[0]);

	if (rk_off[io->i_boff] == -1 ||
	    io->i_boff < 0 || io->i_boff > 7)
		_stop("rk bad unit");
	io->i_boff = rk_off[io->i_boff] * NRKSECT*NRKTRK;
	rkaddr->rkcs2 = RKCS2_SCLR;
	rkwait(rkaddr);
}

rkstrategy(io, func)
	register struct iob *io;
{
	register struct rkdevice *rkaddr = (struct rkdevice *)ubamem(io->i_unit, rkstd[0]);
	int com;
	daddr_t bn;
	short dn, cn, sn, tn;
	int ubinfo, errcnt = 0;

retry:
	ubinfo = ubasetup(io, 1);
	bn = io->i_bn;
	dn = io->i_unit;
	cn = bn/(NRKSECT*NRKTRK);
	sn = bn%NRKSECT;
	tn = (bn / NRKSECT) % NRKTRK;
	rkaddr->rkcs2 = dn;
	rkaddr->rkcs1 = RK_CDT|RK_PACK|RK_GO;
	rkwait(rkaddr);
	rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
	rkwait(rkaddr);
	rkaddr->rkda = sn | (tn << 8);
	rkaddr->rkcyl = cn;
	rkaddr->rkba = ubinfo;
	rkaddr->rkwc = -(io->i_cc >> 1);
	com = RK_CDT|((ubinfo>>16)&0x30)|RK_GO;
	if (func == READ)
		com |= RK_READ;
	else
		com |= RK_WRITE;
	rkaddr->rkcs1 = com;
	rkwait(rkaddr);
	while ((rkaddr->rkds & RKDS_SVAL) == 0)
		;
	ubafree(io, ubinfo);
	if (rkaddr->rkcs1 & RK_CERR) {
		printf("rk error: (cyl,trk,sec)=(%d,%d,%d) cs2=%b er=%b\n",
		    cn, tn, sn, rkaddr->rkcs2, RKCS2_BITS,
		    rkaddr->rker, RKER_BITS);
		rkaddr->rkcs1 = RK_CDT|RK_DCLR|RK_GO;
		rkwait(rkaddr);
		if (errcnt == 10) {
			printf("rk: unrecovered error\n");
			return (-1);
		}
		errcnt++;
		goto retry;
	}
	if (errcnt)
		printf("rk: recovered by retry\n");
	return (io->i_cc);
}

rkwait(rkaddr)
	register struct rkdevice *rkaddr;
{

	while ((rkaddr->rkcs1 & RK_CRDY) == 0)
		;
}
