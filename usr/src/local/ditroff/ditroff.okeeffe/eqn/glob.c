#ifndef lint
static char *sccsid = "glob.c	(CWI)	1.1	85/03/01";
#endif
#include "e.h"

int	dbg;	/* debugging print if non-zero */
int	lp[80];	/* stack for things like piles and matrices */
int	ct;	/* pointer to lp */
int	used[100];	/* available registers */
int	ps;	/* default init point size */
int	deltaps	= 3;	/* default change in ps */
int	gsize	= 10;	/* default initial point size */
int	gfont	= ITAL;	/* italic */
int	ft;	/* default font */
#ifdef	APS
	int	ttype	= DEVAPS;	/* type of typesetter today */
	int	res	= 723;	/* resolution of typesetter; dflt = 202 */
	int	minsize	= 5;	/* min size it can handle; ditto */
#endif
#ifdef
		/*
		 * with just two possible typesetters, this would be the
		 * approach
		 */
	int	ttype	= DEV202;	/* type of typesetter today */
	int	res	= 972;	/* resolution of typesetter; dflt = 202 */
	int	minsize	= 6;	/* min size it can handle; ditto */
#else
	int	ttype	= DEVHAR;	/* type of typesetter today */
	int	res	= 1445;	/* resolution of typesetter; dflt = har */
	int	minsize	= 4;	/* min size it can handle; ditto */
#endif
FILE	*curfile;	/* current input file */
int	ifile;
int	linect;	/* line number in file */
int	eqline;	/* line where eqn started */
int	svargc;
char	**svargv;
int	eht[100];
int	ebase[100];
int	lfont[100];
int	rfont[100];
int	eqnreg;	/* register where final string appears */
int	eqnht;	/* inal height of equation */
int	lefteq	= '\0';	/* left in-line delimiter */
int	righteq	= '\0';	/* right in-line delimiter */
int	lastchar;	/* last character read by lex */
int	markline	= 0;	/* 1 if this EQ/EN contains mark; 2 if lineup */
