/* Copyright (c) 1979 Regents of the University of California */
#include "ex.h"
#include "ex_tty.h"

/*
 * Terminal type initialization routines,
 * and calculation of flags at entry or after
 * a shell escape which may change them.
 */
short	ospeed = -1;

gettmode()
{

	if (gtty(1, &tty) < 0)
		return;
	if (ospeed != tty.sg_ospeed)
		value(SLOWOPEN) = tty.sg_ospeed < B1200;
	ospeed = tty.sg_ospeed;
	normf = tty.sg_flags;
	UPPERCASE = (tty.sg_flags & LCASE) != 0;
	GT = (tty.sg_flags & XTABS) != XTABS;
	NONL = (tty.sg_flags & CRMOD) == 0;
}

char *xPC;
char **sstrs[] = {
	&AL, &BC, &CD, &CE, &CL, &CM, &DC, &DL, &DM, &DO, &ED, &EI, &HO,
	&IC, &IM, &IP, &LL, &MA, &ND, &xPC, &SE, &SF, &SO, &SR, &TA, &UP,
	&VB, &VS, &VE
};
bool *sflags[] = {
	&AM, &BS, &DA, &DB, &EO, &HZ, &IN, &MI, &NC, &OS, &UL, &XN
};
setterm(type)
	char *type;
{
	char *cgoto();
	register int unknown, i;
	register int l;

	if (type[0] == 0)
		type = "xx";
	unknown = 0;
	if (tgetent(genbuf, type) != 1) {
		unknown++;
		CP(genbuf, "xx|dumb:");
	}
	i = LINES = tgetnum("li");
	if (LINES <= 5)
		LINES = 24;
	if (LINES > 48)
		LINES = 48;
	l = LINES;
	if (ospeed < B1200)
		l /= 2;
	else if (ospeed < B2400)
		l = (l * 2) / 3;
	options[WINDOW].ovalue = options[WINDOW].odefault = l - 1;
	options[SCROLL].ovalue = options[SCROLL].odefault = l / 2;
	COLUMNS = tgetnum("co");
	if (COLUMNS <= 20)
		COLUMNS = 1000;
	aoftspace = tspace;
	zap();
	if (cgoto()[0] == 'O')
		CA = 0, CM = 0;
	else
		CA = 1;
	PC = xPC ? xPC[0] : 0;
	aoftspace = tspace;
	CP(ttytype, longname(genbuf, type));
	if (i <= 0)
		LINES = 2;
	termreset();
	value(REDRAW) = AL && DL;
	value(OPTIMIZE) = !CA && !GT;
	if (unknown)
		serror("%s: Unknown terminal type", type);
}

zap()
{
	register char *namp;
	register bool **fp;
	register char ***sp;

 	namp = "ambsdadbeohzinmincosulxn";
	fp = sflags;
	do {
		*(*fp++) = tgetflag(namp);
		namp += 2;
	} while (*namp);
	namp = "albccdceclcmdcdldmdoedeihoicimipllmandpcsesfsosrtaupvbvsve";
	sp = sstrs;
	do {
		*(*sp++) = tgetstr(namp, &aoftspace);
		namp += 2;
	} while (*namp);
}

char *
longname(bp, def)
	register char *bp;
	char *def;
{
	register char *cp;

	while (*bp && *bp != ':' && *bp != '|')
		bp++;
	if (*bp == '|') {
		bp++;
		cp = bp;
		while (*cp && *cp != ':' && *cp != '|')
			cp++;
		*cp = 0;
		return (bp);
	}
	return (def);
}
