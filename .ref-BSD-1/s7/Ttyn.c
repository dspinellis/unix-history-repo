#
#
/*
 * Ttyn - a fast version of ttyn which hashes major minor device numbers
 *
 * Bill Joy UCB August 17, 1977
 *
 * This routine can be called instead of calling 'ttyn' and stats
 * the specified unit, then looking in a table to determine
 * the unit number.  If the given unit is not in the table the standard
 * 'ttyn' is called to get the answer.
 *
 * Generated:	Fri Nov  4 15:08:27 1977
 * No. ttys:	79
 */

static	struct T {
	char	Tmajor;
	char	Tfirst;
	char 	*Tttys;
	char	Tlast;
} Tttyinfo[] {
	0,	0,	"845=x_%!#0123679.",	16,
	14,	0,	"ABCDEFGHIJKLxNOPQRSTUVWXYZ\024\025\026\027\030\031abcdefghijklmnopqrstuvwyz+-,\001\002\003\005",	63,
};

#define	NMAJDEV	2

struct	Stat {
	char	xminor, xmajor;
	int	inumber, flags;
	char	nlinks, uid, gid, size0;
	int	size1;
	char	dminor, dmajor;
	int	addr1[7];
	long	actime, modtime;
};

Ttyn(unit)
	int unit;
{
	register struct T *tp;
	struct Stat stbuf;

	if (fstat(unit, &stbuf))
		return ('x');
	for (tp = &Tttyinfo; tp < &Tttyinfo[NMAJDEV]; tp++)
		if (stbuf.dmajor == tp->Tmajor && stbuf.dminor >= tp->Tfirst && stbuf.dminor <= tp->Tlast)
			return (tp->Tttys[stbuf.dminor - tp->Tfirst]);
	return (ttyn(unit));
}
