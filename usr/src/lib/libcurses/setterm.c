/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)setterm.c	5.12 (Berkeley) %G%";
#endif /* not lint */

#include <sys/ioctl.h>

#include <curses.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void zap __P((void));

static char	*sflags[] = {
			&AM, &BS, &DA, &EO, &HC, &HZ, &IN, &MI, &MS,
			&NC, &NS, &OS, &UL, &XB, &XN, &XT, &XS, &XX
		};

static char	*_PC,
		**sstrs[] = {
			&AL, &BC, &BT, &CD, &CE, &CL, &CM, &CR, &CS,
			&DC, &DL, &DM, &DO, &ED, &EI, &K0, &K1, &K2,
			&K3, &K4, &K5, &K6, &K7, &K8, &K9, &HO, &IC,
			&IM, &IP, &KD, &KE, &KH, &KL, &KR, &KS, &KU,
			&LL, &MA, &ND, &NL, &_PC, &RC, &SC, &SE, &SF,
			&SO, &SR, &TA, &TE, &TI, &UC, &UE, &UP, &US,
			&VB, &VS, &VE, &al, &dl, &sf, &sr, &AL_PARM, 
			&DL_PARM, &UP_PARM, &DOWN_PARM, &LEFT_PARM, 
			&RIGHT_PARM,
		};

static char	*aoftspace;		/* Address of _tspace for relocation */
static char	tspace[2048];		/* Space for capability strings */

char *ttytype;

int
setterm(type)
	register char *type;
{
	static char genbuf[1024];
	static char __ttytype[1024];
	register int unknown;
	int destcol, destline;
	struct winsize win;
	char *p;

#ifdef DEBUG
	__TRACE("setterm: (\"%s\")\nLINES = %d, COLS = %d\n",
	    type, LINES, COLS);
#endif
	if (type[0] == '\0')
		type = "xx";
	unknown = 0;
	if (tgetent(genbuf, type) != 1) {
		unknown++;
		strcpy(genbuf, "xx|dumb:");
	}
#ifdef DEBUG
	__TRACE("setterm: tty = %s\n", type);
#endif

	/* Try TIOCGWINSZ, and, if it fails, the termcap entry. */
	if (ioctl(STDERR_FILENO, TIOCGWINSZ, &win) != -1 &&
	    win.ws_row != 0 && win.ws_col != 0) {
		LINES = win.ws_row;
		COLS = win.ws_col;
	}  else {
		LINES = tgetnum("li");
		COLS = tgetnum("co");
	}

	/* POSIX 1003.2 requires that the environment override. */
	if ((p = getenv("ROWS")) != NULL)
		LINES = strtol(p, NULL, 10);
	if ((p = getenv("COLUMNS")) != NULL)
		COLS = strtol(p, NULL, 10);

	/*
	 * XXX
	 * Historically, curses fails if rows <= 5, cols <= 4.
	 */
	if (LINES <= 5 || COLS <= 4)
		return (ERR);

#ifdef DEBUG
	__TRACE("setterm: LINES = %d, COLS = %d\n", LINES, COLS);
#endif
	aoftspace = tspace;
	zap();			/* Get terminal description. */

	/* Handle funny termcap capabilities. */
	if (CS && SC && RC)
		AL = DL = "";
	if (AL_PARM && AL == NULL)
		AL = "";
	if (DL_PARM && DL == NULL)
		DL = "";
	if (IC) {
		if (IM == NULL)
			IM = "";
		if (EI == NULL)
			EI = "";
	}
	if (!GT)		/* If we can't tab, we can't backtab either. */
		BT = NULL;

	if (tgoto(CM, destcol, destline)[0] == 'O') {
		CA = 0;
		CM = 0;
	} else
		CA = 1;

	PC = _PC ? _PC[0] : 0;
	aoftspace = tspace;
	ttytype = longname(genbuf, __ttytype);

	if ((!AL && !al) || (!DL && !dl))
		__noqch = 1;

	return (unknown ? ERR : OK);
}

/*
 * zap --
 *	Gets all the terminal flags from the termcap database.
 */
static void
zap()
{
	register char *namp, ***sp;
	register char **fp;
	char tmp[3];
#ifdef DEBUG
	register char	*cp;
#endif
	tmp[2] = '\0';

	namp = "ambsdadbeohchzinmimsncnsosulxbxnxtxsxx";
	fp = sflags;
	do {
		*tmp = *namp;
		*(tmp + 1) = *(namp + 1);
		*(*fp++) = tgetflag(tmp);
#ifdef DEBUG
		__TRACE("2.2s = %s\n", namp, *fp[-1] ? "TRUE" : "FALSE");
#endif
		namp += 2;
		
	} while (*namp);
	namp = "ALbcbtcdceclcmcrcsdcDLdmdoedeik0k1k2k3k4k5k6k7k8k9hoicimipkdkekhklkrkskullmandnlpcrcscseSFsoSRtatetiucueupusvbvsvealdlsfsrALDLUPDOLERI";
	sp = sstrs;
	do {
		*tmp = *namp;
		*(tmp + 1) = *(namp + 1);
		*(*sp++) = tgetstr(tmp, &aoftspace);
#ifdef DEBUG
		__TRACE("2.2s = %s", namp, *sp[-1] == NULL ? "NULL\n" : "\"");
		if (*sp[-1] != NULL) {
			for (cp = *sp[-1]; *cp; cp++)
				__TRACE("%s", unctrl(*cp));
			__TRACE("\"\n");
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
 * getcap --
 *	Return a capability from termcap.
 */
char *
getcap(name)
	char *name;
{
	return (tgetstr(name, &aoftspace));
}
