/*	up.old.c	7.1	86/06/05	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"

#include "../vaxuba/upreg.h"
#include "../vaxuba/ubareg.h"

#include "saio.h"
#include "savax.h"

u_short	ubastd[] = { 0776700 };
char	up_gottype[MAXNUBA*8];
char	up_type[MAXNUBA*8];
extern	struct st upst[];

upopen(io)
	register struct iob *io;
{
	register struct updevice *upaddr =
	    (struct updevice *)ubamem(io->i_unit, ubastd[0]);
	register struct st *st;

	while ((upaddr->upcs1 & UP_DVA) == 0)
		;
	if (up_gottype[io->i_unit] == 0) {
		up_type[io->i_unit] = upmaptype(io->i_unit, upaddr);
		if (up_type[io->i_unit] < 0)
			_stop("unknown drive type");
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
	register struct updevice *upaddr =
	    (struct updevice *)ubamem(io->i_unit, ubastd[0]);
	register struct st *st = &upst[up_type[io->i_unit]];

	unit = io->i_unit;
	bn = io->i_bn;
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect;
	upaddr->upcs2 = unit % 8;
	if ((upaddr->upds & UPDS_VV) == 0) {
		upaddr->upcs1 = UP_DCLR|UP_GO;
		upaddr->upcs1 = UP_PRESET|UP_GO;
		upaddr->upof = UPOF_FMT22;
	}
	if ((upaddr->upds & UPDS_DREADY) != UPDS_DREADY)
		_stop("up not ready");
	info = ubasetup(io, 1);
	upaddr->updc = cn;
	upaddr->upda = (tn << 8) + sn;
	upaddr->upba = info;
	upaddr->upwc = -io->i_cc / sizeof (short);
	if (func == READ)
		upaddr->upcs1 = UP_RCOM|UP_GO;
	else
		upaddr->upcs1 = UP_WCOM|UP_GO;
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
	return (io->i_cc + (upaddr->upwc * sizeof (short)));
}

/*ARGSUSED*/
upioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{

	return (ECMD);
}
