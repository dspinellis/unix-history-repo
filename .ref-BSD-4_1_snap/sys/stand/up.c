/*	up.c	4.9	81/04/18	*/

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/upreg.h"
#include "../h/pte.h"
#include "../h/ubareg.h"
#include "saio.h"
#include "savax.h"

u_short	ubastd[] = { 0776700 };
char	up_gottype[MAXNUBA*8] = { 0 };
char	up_type[MAXNUBA*8] = { 0 };
short	up_off[] = { 0, 27, 68, -1, -1, -1, -1, 82 };
short	fj_off[] = { 0, 50, 0, -1, -1, -1, -1, 155 };
struct upst {
	short nsect;
	short ntrak;
	short nspc;
	short ncyl;
	short *off;
} upst[] = {
	32,	19,	32*19,	823,	up_off,
	32,	10,	32*10,	823,	fj_off,
};

upopen(io)
	register struct iob *io;
{
	register struct updevice *upaddr =
	    (struct updevice *)ubamem(io->i_unit, ubastd[0]);
	register struct upst *st;

	while ((upaddr->upcs1 & UP_DVA) == 0)
		;
	if (up_gottype[io->i_unit] == 0) {
		upaddr->uphr = UPHR_MAXTRAK;
		if (upaddr->uphr == 9)
			up_type[io->i_unit] = 1;	/* fuji kludge */
		upaddr->upcs2 = UPCS2_CLR;
		up_gottype[io->i_unit] = 1;
	}
	st = &upst[up_type[io->i_unit]];
	if (io->i_boff < 0 || io->i_boff > 7 || st->off[io->i_boff] == -1)
		_stop("up bad unit");
	io->i_boff = st->off[io->i_boff] * st->nspc;
}

upstrategy(io, func)
	register struct iob *io;
{
	int unit, nspc, ns, cn, tn, sn;
	daddr_t bn;
	int info;
	register short *rp;
	register struct updevice *upaddr =
	    (struct updevice *)ubamem(io->i_unit, ubastd[0]);
	register struct upst *st = &upst[up_type[io->i_unit]];

	unit = io->i_unit;
	bn = io->i_bn;
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect;
	upaddr->upcs2 = unit;
	if ((upaddr->upds & UPDS_VV) == 0) {
		upaddr->upcs1 = UP_DCLR|UP_GO;
		upaddr->upcs1 = UP_PRESET|UP_GO;
		upaddr->upof = UPOF_FMT22;
	}
	if ((upaddr->upds & UPDS_DREADY) != UPDS_DREADY)
		_stop("up not ready");
	info = ubasetup(io, 1);
	rp = (short *) &upaddr->upda;
	upaddr->updc = cn;
	*rp = (tn << 8) + sn;
	*--rp = info;
	*--rp = -io->i_cc / sizeof (short);
	if (func == READ)
		*--rp = UP_RCOM|UP_GO;
	else
		*--rp = UP_WCOM|UP_GO;
	do {
		DELAY(25);
	} while ((upaddr->upcs1 & UP_RDY) == 0);
	if (upaddr->upds & UPDS_ERR) {
		printf("up error: (cyl,trk,sec)=(%d,%d,%d) cs2=%b er1=%b er2=%b\n",
		    cn, tn, sn,
		    upaddr->upcs2, UPCS2_BITS, upaddr->uper1, UPER1_BITS,
		    upaddr->uper2, UPER2_BITS);
		return (-1);
	}
	ubafree(io, info);
	return (io->i_cc);
}
