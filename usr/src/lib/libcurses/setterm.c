/*
 * Terminal initialization routines.
 *
 * %G% (Berkeley) @(#)setterm.c	1.6
 */

# undef	DEBUG

# include	"curses.ext"
# include	"cr_ex.h"

static bool	*sflags[]	= {
			&AM, &BS, &EO, &HZ, &IN, &MI, &MS, &NC, &OS, &UL, &XN
		};

static char	*xPC,
		**sstrs[]	= {
			&AL, &BC,  &BT, &CD, &CE, &CL, &CM, &DC, &DL,
			&DM, &DO,  &ED, &EI, &HO, &IC, &IM, &IP, &LL,
			&MA, &ND, &xPC, &SE, &SF, &SO, &SR, &TA, &TE,
			&TI, &UC,  &UE, &UP, &US, &VB, &VS, &VE
		},
		*tgoto();

static char	tspace[128],		/* Space for capability strings */
		*aoftspace;		/* Address of tspace for relocation */

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

	reg int	unknown;
	char	genbuf[1024];

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
	if (LINES == 0)
		LINES = tgetnum("li");
	if (LINES <= 5)
		LINES = 24;
	else if (LINES > 48)
		LINES = 48;

	if (COLS == 0)
		COLS = tgetnum("co");
	if (COLS <= 4)
		COLS = 80;
	else if (COLS > 1000)
		COLS = 1000;
# ifdef DEBUG
	fprintf(outf, "SETTERM: LINES = %d, COLS = %d\n", LINES, COLS);
# endif
	aoftspace = tspace;
	zap();			/* get terminal description		*/
	if (tgoto(CM, destcol, destline)[0] == 'O')
		CA = FALSE, CM = 0;
	else
		CA = TRUE;
	PC = xPC ? xPC[0] : FALSE;
	aoftspace = tspace;
	strcpy(ttytype, longname(genbuf, type));
	if (unknown)
		return ERR;
	return OK;
}
/*
 *	This routine gets all the terminal falgs from the termcap database
 */
zap() {

	reg bool	**fp;
	reg char	*namp, ***sp;
	extern char	*tgetstr();

	/*
	 * get boolean flags
	 */
 	namp = "ambseohzinmimsncosulxn\0\0";
# ifdef FULLDEBUG
	fprintf(outf, "ZAP: namp = \"%s\"\n", namp);
# endif
	fp = sflags;
	do {
		*(*fp++) = tgetflag(namp);
# ifdef FULLDEBUG
		fprintf(outf, "ZAP: %.2s = %d", namp, *(*(fp - 1)));
# endif
		namp += 2;
	} while (*namp);

	/*
	 * get string values
	 */
	namp = "albcbtcdceclcmdcdldmdoedeihoicimipllmandpcsesfsosrtatetiucueupusvbvsve";
# ifdef FULLDEBUG
	fprintf(outf, "ZAP: namp = \"%s\"\n", namp);
# endif
	sp = sstrs;
	do {
		*(*sp++) = tgetstr(namp, &aoftspace);
# ifdef FULLDEBUG
		fprintf(outf, "ZAP: %.2s = \"%s\"\n", namp, *(*(sp-1)));
# endif
		namp += 2;
	} while (*namp);
	if (!SO && US) {
		SO = US;
		SE = UE;
	}
}

/*
 * return a capability from termcap
 */
char *
getcap(name)
char *name;
{
	char *tgetent();

	return tgetent(name, &aoftspace);
}
