#ifndef lint
static char sccsid[] = "@(#)glob.c	4.2 %G%";
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
int	markline	= 0;	/* 1 if this EQ/EN contains mark or lineup */
