/*	hp.old.c	7.1	86/06/05	*/

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

#define	SECTSIZ	512
#define	MASKREG(reg)	((reg)&0xffff)

extern	short hptypes[];

char	hp_type[MAXNMBA*8];
extern	struct st hpst[];

hpopen(io)
	register struct iob *io;
{
	register unit = io->i_unit;
	struct hpdevice *hpaddr = (struct hpdevice *)mbadrv(unit);
	register struct st *st;
	register i, type = hpaddr->hpdt & MBDT_TYPE;

	mbainit(UNITTOMBA(io->i_unit));
	for (i = 0; hptypes[i]; i++)
		if (hptypes[i] == type)
			goto found;
	_stop("unknown drive type");
found:
	hpaddr->hpcs1 = HP_DCLR|HP_GO;		/* init drive */
	hpaddr->hpcs1 = HP_PRESET|HP_GO;
	hpaddr->hpof = HPOF_FMT22;
	hp_type[unit] = hpmaptype(hpaddr, i, unit);
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
		hpaddr->hpof = HPOF_FMT22;
	}
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect;
	hpaddr->hpdc = cn;
	hpaddr->hpda = (tn << 8) + sn;
	mbastart(io, func);
	while ((hpaddr->hpds & HPDS_DRY) == 0)
		;
	if (hpaddr->hpds&HPDS_ERR) {
		printf("hp error: sn [%d-%d) ds=%b er1=%b\n",
		    bn, bn + io->i_cc/SECTSIZ, MASKREG(hpaddr->hpds), HPDS_BITS,
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
