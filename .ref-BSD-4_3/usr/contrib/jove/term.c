/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include <errno.h>
#ifdef SYSV
#	include <termio.h>
#else
#	include <sgtty.h>
#endif SYSV

#ifdef IPROCS
#	include <signal.h>
#endif

/* Termcap definitions */

char	*UP,
	*CS,
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
	*BC,
	*M_IC,	/* Insert char with arg */
	*M_DC,	/* Delete char with arg */
	*M_AL,	/* Insert line with arg */
	*M_DL,	/* Delete line with arg */
	*SF,	/* Scroll forward */
	*SR,
	*SP,	/* Send Cursor Position */
#ifdef LSRHS
	*RS,	/* Reverse video start */
	*RE,	/* Reverse end */
#endif
	*VB,
	*IP,	/* insert pad after character inserted */
	*lPC;

int	LI,
	ILI,	/* Internal lines, i.e., 23 of LI is 24. */
	CO,

	UL,
	MI,
	SG,	/* number of magic cookies left by SO and SE */
	XS,	/* whether standout is braindamaged */

	TABS,
	UPlen,
	HOlen,
	LLlen;

int	ospeed;
extern char	PC;

static char	tspace[256];

/* The ordering of ts and meas must agree !! */
#ifdef LSRHS
static char	*ts="vsvealdlspcssosecmclcehoupbcicimdceillsfsrvbksketiteALDLICDCrsrepcip";
static char	**meas[] = {
	&VS, &VE, &AL, &DL, &SP, &CS, &SO, &SE,
	&CM, &CL, &CE, &HO, &UP, &BC, &IC, &IM,
	&DC, &EI, &LL, &SF, &SR, &VB, &KS, &KE,
	&TI, &TE, &M_AL, &M_DL, &M_IC, &M_DC,
	&RS, &RE, &lPC, &IP, 0
};
#else
static char	*ts="vsvealdlspcssosecmclcehoupbcicimdceillsfsrvbksketiteALDLICDCpcip";
static char	**meas[] = {
	&VS, &VE, &AL, &DL, &SP, &CS, &SO, &SE,
	&CM, &CL, &CE, &HO, &UP, &BC, &IC, &IM,
	&DC, &EI, &LL, &SF, &SR, &VB, &KS, &KE,
	&TI, &TE, &M_AL, &M_DL, &M_IC, &M_DC,
	&lPC, &IP, 0
};
#endif

static
gets(buf)
char	*buf;
{
	buf[read(0, buf, 12) - 1] = 0;
}	

/* VARARGS1 */

static
TermError(fmt, a)
char	*fmt;
{
	printf(fmt, a);
	_exit(1);
}

getTERM()
{
	char	*getenv();
	char	termbuf[13],
		*termname = 0,
		*termp = tspace,
		tbuff[2048];	/* Good grief! */
	int	i;

	termname = getenv("TERM");
	if (termname == 0) {
		putstr("Enter terminal name: ");
		gets(termbuf);
		if (termbuf[0] == 0)
			TermError(NullStr);

		termname = termbuf;
	}

	if (tgetent(tbuff, termname) < 1)
		TermError("[\"%s\" unknown terminal type?]", termname);

	if ((CO = tgetnum("co")) == -1)
		TermError("columns?");

	if ((LI = tgetnum("li")) == -1)
		TermError("lines?");

	if ((SG = tgetnum("sg")) == -1)
		SG = 0;			/* Used for mode line only */

	if ((XS = tgetflag("xs")) == -1)
		XS = 0;			/* Used for mode line only */

	for (i = 0; meas[i]; i++) {
		*(meas[i]) = (char *) tgetstr(ts, &termp);
		ts += 2;
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

#ifdef LSRHS		/* We, at the high school, are the only ones who
			   do SO right in termcap, but unfortunately the
			   right SO doesn't look as good with modelines. */
	if (RS)
		SO = RS;
	if (RE)
		SE = RE;
			/* I only ever use SO for the modeline anyway. */

/* SO is really BOLDFACE!  Why is LS always right and the rest of the
   world wrong? */
#endif
#ifdef ID_CHAR
	disp_opt_init();
#endif
	if (CanScroll = ((AL && DL) || CS))
		IDline_setup(termname);
}

