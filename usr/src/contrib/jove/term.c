/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "fp.h"
#include "disp.h"
#include <ctype.h>
#include <errno.h>

#ifndef MAC	/* most of the file... */

# ifdef	STDARGS
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif

#ifndef MSDOS
# ifdef SYSV
#   include <termio.h>
# else
#   include <sgtty.h>
# endif /* SYSV */
#endif /* MSDOS */

#ifdef IPROCS
# include <signal.h>
#endif

#include "termcap.h"

/* Termcap definitions */

#ifndef IBMPC
char	*CS,
	*SO,
	*SE,
	*CM,
	*CL,
	*CE,
	*HO,
	*AL,
	*DL,
	*VS,
	*VE,
	*KS,
	*KE,
	*TI,
	*TE,
	*IC,
	*DC,
	*IM,
	*EI,
	*LL,
	*M_IC,	/* Insert char with arg */
	*M_DC,	/* Delete char with arg */
	*M_AL,	/* Insert line with arg */
	*M_DL,	/* Delete line with arg */
	*SF,	/* Scroll forward */
	*SR,
	*SP,	/* Send Cursor Position */
	*VB,
	*BL,
	*IP,	/* insert pad after character inserted */
	*lPC,
	*NL,
	*DO;
#endif

int	LI,
	ILI,	/* Internal lines, i.e., 23 of LI is 24. */
	CO,

	UL,
	MI,
	SG,	/* number of magic cookies left by SO and SE */
	XS,	/* whether standout is braindamaged */
	HZ,	/* Hazeltine tilde kludge */

	TABS,
	UPlen,
	HOlen,
	LLlen;

#ifdef notdef
	/*
	 * Are you sure about this one Jon?  On the SYSV system I tried this
	 * on I got a multiple definition of PC because it was already
	 * defined in -ltermcap.  Similarly for BC and UP ...
	 */
# ifdef SYSVR2 /* release 2, at least */
char	PC;
# endif /* SYSVR2 */
#endif

#ifndef IBMPC
private char	tspace[256];

/* The ordering of ts and meas must agree !! */
private const char	ts[] =
	"vsvealdlspcssosecmclcehoupbcicimdceillsfsrvbksketiteALDLICDCpcipblnldo";
private char	**const meas[] = {
	&VS, &VE, &AL, &DL, &SP, &CS, &SO, &SE,
	&CM, &CL, &CE, &HO, &UP, &BC, &IC, &IM,
	&DC, &EI, &LL, &SF, &SR, &VB, &KS, &KE,
	&TI, &TE, &M_AL, &M_DL, &M_IC, &M_DC,
	&lPC, &IP, &BL, &NL, &DO, 0
};

private void
TermError()
{
	flusho();
	_exit(1);
}

void
getTERM()
{
	extern char	*getenv(), *tgetstr() ;
	char	termbuf[13],
		*termname = NULL,
		*termp = tspace,
		tbuff[2048];	/* Good grief! */
	const char	*tsp = ts;
	int	i;

	termname = getenv("TERM");
	if ((termname == NULL) || (*termname == '\0') ||
	    (strcmp(termname, "dumb") == 0) ||
	    (strcmp(termname, "unknown") == 0) ||
	    (strcmp(termname, "network") == 0)) {
		putstr("Enter terminal type (e.g, vt100): ");
		flusho();
		termbuf[read(0, termbuf, sizeof(termbuf)) - 1] = '\0';
		if (termbuf[0] == 0)
			TermError();

		termname = termbuf;
	}

	if (tgetent(tbuff, termname) < 1) {
		writef("[\"%s\" unknown terminal type?]", termname);
		TermError();
	}
	if ((CO = tgetnum("co")) == -1) {
wimperr:
		writef("You can't run JOVE on a %s terminal.\n", termname);
		TermError();
		/*NOTREACHED*/
	}

	else if (CO > MAXCOLS)
		CO = MAXCOLS;

	if ((LI = tgetnum("li")) == -1)
		goto wimperr;

	if ((SG = tgetnum("sg")) == -1)
		SG = 0;			/* Used for mode line only */

	if ((XS = tgetflag("xs")) == -1)
		XS = 0;			/* Used for mode line only */

	if ((HZ = tgetflag("hz")) == -1)
		HZ = 0;			/* Hazeltine tilde kludge */

	for (i = 0; meas[i]; i++) {
		static char	nm[3] = "xx";

		nm[0] = *tsp++;
		nm[1] = *tsp++;
		*(meas[i]) = (char *) tgetstr(nm, &termp);
		if (termp > tspace + sizeof(tspace))
			goto wimperr;
	}
	if (lPC)
		PC = *lPC;
	if (XS)
		SO = SE = 0;

	if (CS && !SR)
		CS = SR = SF = 0;

	if (CS && !SF)
		SF = "\n";

	if (IM && (*IM == 0))
		IM = 0;
	else
		MI = tgetflag("mi");

	UL = tgetflag("ul");

	if (NL == 0)
		NL = "\n";
	else {			/* strip stupid padding information */
		while (isdigit(*NL))
			NL += 1;
		if (*NL == '*')
			NL += 1;
	}
	if (!DO)
		DO = NL;

	if (BL == 0)
		BL = "\007";

	if (tgetflag("km") > 0)		/* has meta-key */
		MetaKey = YES;

#ifdef ID_CHAR
	disp_opt_init();
#endif
	if ((CanScroll = ((AL && DL) || CS)) != 0)
		IDline_setup(termname);
}

#else

void
InitCM()
{
}

int EGA;

void
getTERM()
{
	char	*getenv(), *tgetstr() ;
	char	*termname;
	void	init_43(), init_term();
	unsigned char lpp(), chpl();

	if (getenv("EGA") || (!stricmp(getenv("TERM"), "EGA"))) {
	   termname = "ega";
	   init_43();
	   EGA = 1;
	}
	else {
	   termname = "ibmpc";
	   init_term();
	   EGA = 0;
	}

	CO = chpl();
	LI = lpp();

	SG = 0;			/* Used for mode line only */
	XS = 0;			/* Used for mode line only */

	CanScroll = 1;
}

#endif /* IBMPC */

#else /* MAC */
int	LI,
	ILI,	/* Internal lines, i.e., 23 of LI is 24. */
	CO,
	TABS,
	SG;

void getTERM()
{
	SG = 0;
	CanScroll = 1;
}

#endif /* MAC */

/* put a string with padding */

#ifndef IBMPC
private void
tputc(c)
int	c;
{
	jputchar(c);
}
#endif /* IBMPC */

#ifndef MAC
void
putpad(str, lines)
char	*str;
int	lines;
{
#ifndef IBMPC
	if (str)
		tputs(str, lines, tputc);
#else /* IBMPC */
	write_emif(str);
#endif /* IBMPC */
}

void
putargpad(str, arg, lines)
char	*str;
int	arg,
	lines;
{
#ifndef	IBMPC
	if (str) {
		tputs(
#ifdef	TERMINFO
			tparm(str, arg),
#else	/* TERMINFO */
			tgoto(str, 0, arg),	/* fudge */
#endif	/* TERMINFO */
			lines, tputc);
	}
#else	/* IBMPC */
	/* This code is only a guess: I don't know if any M_* termcap
	 * attributes are defined for the PC.  If they are not used,
	 * this routine is not called.  Perhaps this routine should
	 * simply abort.
	 */
	if (str) {
		char	buf[16];	/* hope that this is long enough */

		swritef(buf, str, arg);	/* hope only %d appears in str */
		write_em(buf);
	}
#endif	/* IBMPC */
}

#endif /* MAC */

/* Determine the number of characters to buffer at each baud rate.  The
   lower the number, the quicker the response when new input arrives.  Of
   course the lower the number, the more prone the program is to stop in
   output.  Decide what matters most to you. This sets BufSize to the right
   number or chars, and initializes `stdout'.  */

void
settout(ttbuf)
char	*ttbuf;
{
	int	speed_chars;
	static const int speeds[] = {
		1,	/* 0	*/
		1,	/* 50	*/
		1,	/* 75	*/
		1,	/* 110	*/
		1,	/* 134	*/
		1,	/* 150	*/
		1,	/* 200	*/
		2,	/* 300	*/
		4,	/* 600	*/
		8,	/* 1200 */
		16,	/* 1800	*/
		32,	/* 2400	*/
		128,	/* 4800	*/
		256,	/* 9600	*/
		512,	/* EXTA	*/
		1024	/* EXT	*/
	};

#if (defined(MSDOS) || defined(MAC))
	speed_chars = 256;
#else
	speed_chars = speeds[ospeed];
#endif
	flusho();		/* flush the one character buffer */
	BufSize = min(MAXTTYBUF, speed_chars * max(LI / 24, 1));
	stdout = fd_open("/dev/tty", F_WRITE|F_LOCKED, 1, ttbuf, BufSize);
}
