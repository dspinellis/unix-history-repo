/*	hp.c	4.6	81/05/10	*/

/*
 * RP??/RM?? disk driver
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/hpreg.h"
#include "../h/pte.h"
#include "../h/mbareg.h"
#include "saio.h"
#include "savax.h"

#define	MASKREG(reg)	((reg)&0xffff)

char	hp_type[MAXNMBA*8] = { 0 };

/* THIS SHOULD BE READ IN OFF THE PACK, PER DRIVE */
short	hp6_off[8] =	{ 0, 38, 0, -1, -1, -1, 118, -1 };
short	rm3_off[8] =	{ 0, 100, 0, -1, -1, -1, 310, -1 };
short	rm5_off[8] =	{ 0, 27, 0, 562, 589, 681, 562, 82 };
short	rm80_off[8] =	{ 0, 37, 0, -1, -1, -1, 115, 305 };
short	hp7_off[8] = 	{ 0, 10, 0, 330, 340, 500, 330, 50 };
/* END SHOULD BE READ IN */

short	hptypes[] =
    { MBDT_RM03, MBDT_RM05, MBDT_RP06, MBDT_RM80, MBDT_RP05, MBDT_RP07, 0 };

struct hpst {
	short	nsect;
	short	ntrak;
	short	nspc;
	short	ncyl;
	short	*off;
} hpst[] = {
	32,	5,	32*5,	823,	rm3_off,	/* RM03 */
	32,	19,	32*19,	823,	rm5_off,	/* RM05 */
	22,	19,	22*19,	815,	hp6_off,	/* RP06 */
	31,	14, 	31*14,	559,	rm80_off,	/* RM80 */
	22,	19,	22*19,	411,	hp6_off,	/* RP06 */
	50,	32,	50*32,	630,	hp7_off,	/* RP07 */
};

hpopen(io)
	register struct iob *io;
{
	register unit = io->i_unit;
	struct hpdevice *hpaddr = (struct hpdevice *)mbadrv(unit);
	register struct hpst *st;

	mbainit(UNITTOMBA(io->i_unit));
	if (hp_type[unit] == 0) {
		register type = hpaddr->hpdt & MBDT_TYPE;
		register int i;

		for (i = 0; hptypes[i]; i++)
			if (hptypes[i] == type)
				goto found;
		_stop("unknown drive type");
found:
		hp_type[unit] = i;
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
	struct hpst *st = &hpst[hp_type[unit]];
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
		printf("hp error: (cyl,trk,sec)=(%d,%d,%d) ds=%b er1=%b\n",
		    cn, tn, sn, MASKREG(hpaddr->hpds), HPDS_BITS,
		    MASKREG(hpaddr->hper1), HPER1_BITS);
		return (-1);
	}
	return (io->i_cc);
}
