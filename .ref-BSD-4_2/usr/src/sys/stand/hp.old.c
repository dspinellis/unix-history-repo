/*	hp.old.c	6.1	83/07/29	*/

/*
 * RP??/RM?? disk driver
 */
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"

#include "../vaxmba/hpreg.h"
#include "../vaxmba/mbareg.h"

#include "saio.h"
#include "savax.h"

#define	MASKREG(reg)	((reg)&0xffff)

short	hptypes[] = {
	MBDT_RM03,
	MBDT_RM05,
	MBDT_RP06,
	MBDT_RM80,
	MBDT_RP05,
	MBDT_RP07,
	MBDT_ML11A,
	MBDT_ML11B,
	-1,		/* 9755 */
	-1,		/* 9730 */
	-1,		/* Capricorn */
	-1,		/* Eagle */
	MBDT_RM02,	/* actually something else */
	-1,		/* 9300 */
	0
};

#define RP06 (hptypes[hp_type[unit]] <= MBDT_RP06)
#define ML11 (hptypes[hp_type[unit]] == MBDT_ML11A)
#define RM80 (hptypes[hp_type[unit]] == MBDT_RM80)

char	hp_type[MAXNMBA*8] = { 0 };
extern	struct st hpst[];

hpopen(io)
	register struct iob *io;
{
	register unit = io->i_unit;
	struct hpdevice *hpaddr = (struct hpdevice *)mbadrv(unit);
	register struct st *st;

	mbainit(UNITTOMBA(io->i_unit));
	if (hp_type[unit] == 0) {
		register i, type = hpaddr->hpdt & MBDT_TYPE;

		for (i = 0; hptypes[i]; i++)
			if (hptypes[i] == type)
				goto found;
		_stop("unknown drive type");
found:
		hpaddr->hpcs1 = HP_DCLR|HP_GO;		/* init drive */
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		if (type != MBDT_ML11A && type != MBDT_ML11B)
			hpaddr->hpof = HPOF_FMT22;
		hp_type[unit] = hpmaptype(hpaddr, i, unit);
	}
	st = &hpst[hp_type[unit]];
	if (io->i_boff < 0 || io->i_boff > 7 ||
	    st->off[io->i_boff]== -1)
		_stop("hp bad minor");
	io->i_boff = st->off[io->i_boff] * st->nspc;
}

hpstrategy(io, func)
	register struct iob *io;
{
	int unit = io->i_unit;
	daddr_t bn = io->i_bn;
	struct hpdevice *hpaddr = (struct hpdevice *)mbadrv(unit);
	struct st *st = &hpst[hp_type[unit]];
	int cn, tn, sn;

	if ((hpaddr->hpds & HPDS_VV) == 0) {
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		if (!ML11)
			hpaddr->hpof = HPOF_FMT22;
	}
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect;
	if (ML11)
		hpaddr->hpda = bn;
	else {
		hpaddr->hpdc = cn;
		hpaddr->hpda = (tn << 8) + sn;
	}
	mbastart(io, func);
	while ((hpaddr->hpds & HPDS_DRY) == 0)
		;
	if (hpaddr->hpds&HPDS_ERR) {
		printf("hp error: (cyl,trk,sec)=(%d,%d,%d) ds=%b er1=%b\n",
		    cn, tn, sn, MASKREG(hpaddr->hpds), HPDS_BITS,
		    MASKREG(hpaddr->hper1), HPER1_BITS);
		return (-1);
	}
	return (io->i_cc);
}

/*ARGSUSED*/
hpioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{

	return (ECMD);
}
