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
	GT = (tty.sg_flags & XTABS) != XTABS && !XT;
	NONL = (tty.sg_flags & CRMOD) == 0;
}

char *xPC;
char **sstrs[] = {
	&AL, &BC, &BT, &CD, &CE, &CL, &CM, &DC, &DL, &DM, &DO, &ED, &EI,
	&F0, &F1, &F2, &F3, &F4, &F5, &F6, &F7, &F8, &F9,
	&HO, &IC, &IM, &IP, &KD, &KE, &KH, &KL, &KR, &KS, &KU, &LL,
	&ND, &xPC, &SE, &SF, &SO, &SR, &TA, &TE, &TI, &UP, &VB, &VS, &VE
};
bool *sflags[] = {
	&AM, &BS, &DA, &DB, &EO, &HC, &HZ, &IN, &MI, &NC, &OS, &UL, &XN, &XT
};
char **fkeys[10] = {
	&F0, &F1, &F2, &F3, &F4, &F5, &F6, &F7, &F8, &F9
};
setterm(type)
	char *type;
{
	char *cgoto();
	register int unknown, i;
	register int l;
	char ltcbuf[TCBUFSIZE];

	if (type[0] == 0)
		type = "xx";
	unknown = 0;
	putpad(TE);
	if (tgetent(ltcbuf, type) != 1) {
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
	aoftspace = tspace;
	zap();
	/*
	 * Initialize keypad arrow keys.
	 */
	arrows[0].cap = KU; arrows[0].mapto = "k"; arrows[0].descr = "up";
	arrows[1].cap = KD; arrows[1].mapto = "j"; arrows[1].descr = "down";
	arrows[2].cap = KL; arrows[2].mapto = "h"; arrows[2].descr = "left";
	arrows[3].cap = KR; arrows[3].mapto = "l"; arrows[3].descr = "right";
	arrows[4].cap = KH; arrows[4].mapto = "H"; arrows[4].descr = "home";

	options[WINDOW].ovalue = options[WINDOW].odefault = l - 1;
	if (defwind) options[WINDOW].ovalue = defwind;
	options[SCROLL].ovalue = options[SCROLL].odefault = HC ? 11 : ((l-1) / 2);
	COLUMNS = tgetnum("co");
	if (COLUMNS <= 20)
		COLUMNS = 1000;
	if (cgoto()[0] == 'O')	/* OOPS */
		CA = 0, CM = 0;
	else
		CA = 1, costCM = strlen(tgoto(CM, 8, 10));
	PC = xPC ? xPC[0] : 0;
	aoftspace = tspace;
	CP(ttytype, longname(genbuf, type));
	if (i <= 0)
		LINES = 2;
	/* proper strings to change tty type */
#ifdef notdef
	/* Taken out because we don't allow it. See ex_set.c for reasons. */
	if (inopen)
		putpad(VE);
#endif
	termreset();
	gettmode();
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

 	namp = "ambsdadbeohchzinmincosulxnxt";
	fp = sflags;
	do {
		*(*fp++) = tgetflag(namp);
		namp += 2;
	} while (*namp);
	namp = "albcbtcdceclcmdcdldmdoedeik0k1k2k3k4k5k6k7k8k9hoicimipkdkekhklkrkskullndpcsesfsosrtatetiupvbvsve";
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

char *
fkey(i)
	int i;
{
	if (0 <= i && i <= 9)
		return(*fkeys[i]);
	else
		return(NOSTR);
}
