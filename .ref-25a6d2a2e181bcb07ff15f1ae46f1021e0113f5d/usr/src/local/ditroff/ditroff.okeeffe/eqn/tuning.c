#ifndef lint
static char sccsid[] = "@(#)tuning.c	1.1 (CWI) 87/04/01";
#endif lint
/*

This file contains parameter values for many of the
tuning parameters in eqn.  Ideally, these should be
settable dynamically, but getting them into a single
file is at least a step in the right direction.

Strings are plugged in verbatim.
Floats are usually in ems.

*/

/* In main.c: */

float	BeforeSub = 1.2;	/* line space before a subscript */
float	AfterSub  = 0.2;	/* line space after a subscript */

/* diacrit.c: */

float	Dvshift	= 0.25;		/* vertical shift for diacriticals on tall letters */
float	Dhshift = 0.025;	/* horizontal shift for tall letters */
float	Dh2shift = 0.05;	/* horizontal shift for small letters */
float	Dheight	= 0.25;		/* increment to height for diacriticals */
float	Barv	= 0.68;		/* vertical shift for bar */
float	Barh	= 0.05;		/* 1/2 horizontal shrink for bar */
char	*Vec	= "\\v'-.45m'\\s-1\\(->\\s0\\v'.45m'";
char	*Dyad	= "\\v'-.45m'\\s-1\\z\\(<-\\|\\(->\\s0\\v'.45m'";
char	*Hat	= "\\v'-.1m'\\s+1^\\s0\\v'.1m'";
char	*Tilde	= "\\v'-.1m'\\s+1~\\s0\\v'.1m'";
char	*Dot	= "\\v'-.67m'.\\v'.67m'";
char	*Dotdot	= "\\v'-.67m'..\\v'.67m'";
char	*Utilde	= "\\v'1.0m'\\s+2~\\s-2\\v'-1.0m'";

/* eqnbox.c: */

char	*IRspace = "\\^";	/* space between italic & roman boxes */

/* fat.c: */

float	Fatshift = 0.05;	/* fattening shifts by Fatshift ems */

/* funny.c: */

char	*Sum		= "\\v'.3m'\\s+5\\(*S\\s-5\\v'-.3m'";
char	*Union		= "\\v'.3m'\\s+5\\(cu\\s-5\\v'-.3m'";
char	*Inter		= "\\v'.3m'\\s+5\\(ca\\s-5\\v'-.3m'";
char	*Prod		= "\\v'.3m'\\s+5\\(*P\\s-5\\v'-.3m'";
int	Funnyps	= 5;		/* point size change (== 5 above) */
float	Funnyht = 0.2;		/* height correction */
float	Funnybase = 0.3;	/* base correction */

/* integral.c: */

char	*Integral	= "\\v'.1m'\\s+4\\(is\\s-4\\v'-.1m'";
int	Intps	= 4;		/* point size change for integral (== 4 above) */
float	Intht	= 1.15;		/* ht of integral in ems */
float	Intbase	= 0.3;		/* base in ems */
float	Int1h	= 0.4;		/* lower limit left */
float	Int1v	= 0.2;		/* lower limit down */
float	Int2h	= 0.08;		/* upper limit right */
float	Int2v	= 0.1;		/* upper limit up */

/* matrix.c: */

char	*Matspace = "\\ \\ ";	/* space between matrix columns */

/* over.c: */

float	Overgap	= 0.3;		/* gap between num and denom */
float	Overwid	= 0.5;		/* extra width of box */
float	Overline = 0.1;		/* extra length of fraction bar */

/* paren.c* */

float	Parenbase = 0.4;	/* shift of base for even count */

/* pile.c: */

float	Pilegap	= 0.4;		/* gap between pile elems */
float	Pilebase = 0.5;		/* shift base of even # of piled elems */

/* shift.c: */

float	Subbase	= 0.2;		/* subscript base belowe main base */
float	Supshift = 0.4;		/* superscript .4 up main box */
char	*Sub1space = "\\|";	/* italic sub roman space */
char	*Sup1space = "\\|";	/* italic sup roman space */
char	*Sub2space = "\\^";	/* space after subscripted thing */
char	*SS1space = "\\^";	/* space before sub in x sub i sup j */
char	*SS2space = "\\^";	/* space before sup */

/* sqrt.c: */
	/* sqrt is hard!  punt for now. */
	/* part of the problem is that every typesetter does it differently */
	/* and we have several typesetters to run. */

/* text.c: */
	/* ought to be done by a table */
