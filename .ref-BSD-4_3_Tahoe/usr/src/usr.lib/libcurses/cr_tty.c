/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)cr_tty.c	5.4 (Berkeley) 6/30/88";
#endif /* not lint */

/*
 * Terminal initialization routines.
 *
 */

# include	"curses.ext"

static bool	*sflags[] = {
			&AM, &BS, &DA, &DB, &EO, &HC, &HZ, &IN, &MI,
			&MS, &NC, &NS, &OS, &UL, &XB, &XN, &XT, &XS,
			&XX
		};

static char	*_PC,
		**sstrs[] = {
			&AL, &BC, &BT, &CD, &CE, &CL, &CM, &CR, &CS,
			&DC, &DL, &DM, &DO, &ED, &EI, &K0, &K1, &K2,
			&K3, &K4, &K5, &K6, &K7, &K8, &K9, &HO, &IC,
			&IM, &IP, &KD, &KE, &KH, &KL, &KR, &KS, &KU,
			&LL, &MA, &ND, &NL, &_PC, &RC, &SC, &SE, &SF,
			&SO, &SR, &TA, &TE, &TI, &UC, &UE, &UP, &US,
			&VB, &VS, &VE, &AL_PARM, &DL_PARM, &UP_PARM,
			&DOWN_PARM, &LEFT_PARM, &RIGHT_PARM,
		},
		*tgoto();

char		_tspace[2048];		/* Space for capability strings */

static char	*aoftspace;		/* Address of _tspace for relocation */

static int	destcol, destline;

/*
 *	This routine does terminal type initialization routines, and
 * calculation of flags at entry.  It is almost entirely stolen from
 * Bill Joy's ex version 2.6.
 */
short	ospeed = -1;

gettmode() {

	if (gtty(_tty_ch, &_tty) < 0)
		return;
	savetty();
	if (stty(_tty_ch, &_tty) < 0)
		_tty.sg_flags = _res_flg;
	ospeed = _tty.sg_ospeed;
	_res_flg = _tty.sg_flags;
	UPPERCASE = (_tty.sg_flags & LCASE) != 0;
	GT = ((_tty.sg_flags & XTABS) == 0);
	NONL = ((_tty.sg_flags & CRMOD) == 0);
	_tty.sg_flags &= ~XTABS;
	stty(_tty_ch, &_tty);
# ifdef DEBUG
	fprintf(outf, "GETTMODE: UPPERCASE = %s\n", UPPERCASE ? "TRUE":"FALSE");
	fprintf(outf, "GETTMODE: GT = %s\n", GT ? "TRUE" : "FALSE");
	fprintf(outf, "GETTMODE: NONL = %s\n", NONL ? "TRUE" : "FALSE");
	fprintf(outf, "GETTMODE: ospeed = %d\n", ospeed);
# endif
}

setterm(type)
reg char	*type; {

	reg int		unknown;
	static char	genbuf[1024];
# ifdef TIOCGWINSZ
	struct winsize win;
# endif

# ifdef DEBUG
	fprintf(outf, "SETTERM(\"%s\")\n", type);
	fprintf(outf, "SETTERM: LINES = %d, COLS = %d\n", LINES, COLS);
# endif
	if (type[0] == '\0')
		type = "xx";
	unknown = FALSE;
	if (tgetent(genbuf, type) != 1) {
		unknown++;
		strcpy(genbuf, "xx|dumb:");
	}
# ifdef DEBUG
	fprintf(outf, "SETTERM: tty = %s\n", type);
# endif
# ifdef TIOCGWINSZ
	if (ioctl(_tty_ch, TIOCGWINSZ, &win) >= 0) {
		if (LINES == 0)
			LINES = win.ws_row;
		if (COLS == 0)
			COLS = win.ws_col;
	}
# endif

	if (LINES == 0)
		LINES = tgetnum("li");
	if (LINES <= 5)
		LINES = 24;

	if (COLS == 0)
		COLS = tgetnum("co");
	if (COLS <= 4)
		COLS = 80;

# ifdef DEBUG
	fprintf(outf, "SETTERM: LINES = %d, COLS = %d\n", LINES, COLS);
# endif
	aoftspace = _tspace;
	zap();			/* get terminal description		*/

	/*
	 * Handle funny termcap capabilities
	 */
	if (CS && SC && RC) AL=DL="";
	if (AL_PARM && AL==NULL) AL="";
	if (DL_PARM && DL==NULL) DL="";
	if (IC && IM==NULL) IM="";
	if (IC && EI==NULL) EI="";
	if (!GT) BT=NULL;	/* If we can't tab, we can't backtab either */

	if (tgoto(CM, destcol, destline)[0] == 'O')
		CA = FALSE, CM = 0;
	else
		CA = TRUE;

	PC = _PC ? _PC[0] : FALSE;
	aoftspace = _tspace;
	strncpy(ttytype, longname(genbuf, type), sizeof(ttytype) - 1);
	ttytype[sizeof(ttytype) - 1] = '\0';
	if (unknown)
		return ERR;
	return OK;
}

/*
 *	This routine gets all the terminal flags from the termcap database
 */

zap()
{
	register char	*namp;
	register bool	**fp;
	register char	***sp;
#ifdef	DEBUG
	register char	*cp;
#endif
	extern char	*tgetstr();

	namp = "ambsdadbeohchzinmimsncnsosulxbxnxtxsxx";
	fp = sflags;
	do {
		*(*fp++) = tgetflag(namp);
#ifdef DEBUG
		fprintf(outf, "%2.2s = %s\n", namp, *fp[-1] ? "TRUE" : "FALSE");
#endif
		namp += 2;
	} while (*namp);
	namp = "albcbtcdceclcmcrcsdcdldmdoedeik0k1k2k3k4k5k6k7k8k9hoicimipkdkekhklkrkskullmandnlpcrcscsesfsosrtatetiucueupusvbvsveALDLUPDOLERI";
	sp = sstrs;
	do {
		*(*sp++) = tgetstr(namp, &aoftspace);
#ifdef DEBUG
		fprintf(outf, "%2.2s = %s", namp, *sp[-1] == NULL ? "NULL\n" : "\"");
		if (*sp[-1] != NULL) {
			for (cp = *sp[-1]; *cp; cp++)
				fprintf(outf, "%s", unctrl(*cp));
			fprintf(outf, "\"\n");
		}
#endif
		namp += 2;
	} while (*namp);
	if (XS)
		SO = SE = NULL;
	else {
		if (tgetnum("sg") > 0)
			SO = NULL;
		if (tgetnum("ug") > 0)
			US = NULL;
		if (!SO && US) {
			SO = US;
			SE = UE;
		}
	}
}

/*
 * return a capability from termcap
 */
char *
getcap(name)
char *name;
{
	char *tgetstr();

	return tgetstr(name, &aoftspace);
}
