#ifndef lint
/*
static char sccsid[] = "@(#)ni.c	2.3 (CWI) 89/08/14";
*/
static char sccsid[] = "@(#)ni.c	2.5 (Berkeley) %G%";
#endif lint
#include "tdef.h"
#include "pathnames.h"

/* You may want to change these names */

#ifdef NROFF

char	termtab[NS] = _PATH_TERMTAB;
					/* term type added in ptinit() */
char	fontfile[NS] = "";	/* not used */
char	devname[20] = "37";

#else

char	termtab[NS] = _PATH_FONTS;	/* rest added in ptinit() */
char	fontfile[NS] = _PATH_FONTS;	/* rest added in casefp() */
char	devname[20]	 = "psc";	/* default typesetter */

#endif
struct numtab numtab[NN] = {
	{ PAIR('%', 0) },
	{ PAIR('n', 'l') },
	{ PAIR('y', 'r') },
	{ PAIR('h', 'p') },
	{ PAIR('c', 't') },
	{ PAIR('d', 'n') },
	{ PAIR('m', 'o') },
	{ PAIR('d', 'y') },
	{ PAIR('d', 'w') },
	{ PAIR('l', 'n') },
	{ PAIR('d', 'l') },
	{ PAIR('s', 't') },
	{ PAIR('s', 'b') },
	{ PAIR('c', '.') },
	{ PAIR('$', '$') },
};


int	pto = 10000;
int	pfrom = 1;
int	print = 1;
char	nextf[NS] = _PATH_TMAC;
#ifndef NROFF
int	oldbits = -1;
#endif
int	init = 1;
int	fc = IMP;	/* field character */
int	eschar = '\\';
#ifdef	NROFF
int	pl = 11*INCH;
int	po = PO;
#else
int	pl;
int	po;
#endif
int	dfact = 1;
int	dfactd = 1;
int	res = 1;
int	smnt = 0;	/* beginning of special fonts */
int	ascii = ASCII;
FILE	*ptid = stdout;
int	lg = LG;
int	pnlist[NPN] = { -1 };


int	*pnp = pnlist;
int	npn = 1;
int	npnflg = 1;
int	dpn = -1;
int	totout = 1;
int	ulfont = ULFONT;
int	tabch = TAB;
int	ldrch = LEADER;

extern    caseds(), caseas(), casesp(), caseft(), caseps(), casevs(),
casenr(), caseif(), casepo(), casetl(), casetm(), casebp(), casech(),
casepn(), tbreak(), caseti(), casene(), casenf(), casece(), casefi(),
casein(), casell(), casens(), casemk(), casert(), caseam(), casest(),
casede(), casedi(), caseda(), casewh(), casedt(), caseit(), caserm(),
casern(), casead(), casers(), casena(), casepl(), caseta(), casetr(),
caseul(), caselt(), casenx(), caseso(), caseig(), casetc(), casefc(),
caseec(), caseeo(), caselc(), caseev(), caserd(), caseab(), casefl(),
done(),   casess(), casefp(), casecs(), casebd(), caselg(), casehc(),
casehy(), casenh(), casenm(), casenn(), casesv(), caseos(), casels(),
casecc(), casec2(), caseem(), caseaf(), casehw(), casemc(), casepm(),
casecu(), casepi(), caserr(), caseuf(), caseie(), caseel(), casepc(),
caseht(), casecf(), casesy(), caself(), caseha();

#define	C(a,b)	{a, 0, b, 0}
struct contab contab[NM] = {
	C(PAIR('d', 's'), caseds),
	C(PAIR('a', 's'), caseas),
	C(PAIR('s', 'p'), casesp),
	C(PAIR('f', 't'), caseft),
	C(PAIR('p', 's'), caseps),
	C(PAIR('v', 's'), casevs),
	C(PAIR('n', 'r'), casenr),
	C(PAIR('i', 'f'), caseif),
	C(PAIR('i', 'e'), caseie),
	C(PAIR('e', 'l'), caseel),
	C(PAIR('p', 'o'), casepo),
	C(PAIR('t', 'l'), casetl),
	C(PAIR('t', 'm'), casetm),
	C(PAIR('b', 'p'), casebp),
	C(PAIR('c', 'h'), casech),
	C(PAIR('p', 'n'), casepn),
	C(PAIR('b', 'r'), tbreak),
	C(PAIR('t', 'i'), caseti),
	C(PAIR('n', 'e'), casene),
	C(PAIR('n', 'f'), casenf),
	C(PAIR('c', 'e'), casece),
	C(PAIR('f', 'i'), casefi),
	C(PAIR('i', 'n'), casein),
	C(PAIR('l', 'l'), casell),
	C(PAIR('n', 's'), casens),
	C(PAIR('m', 'k'), casemk),
	C(PAIR('r', 't'), casert),
	C(PAIR('a', 'm'), caseam),
	C(PAIR('s', 't'), casest),
	C(PAIR('d', 'e'), casede),
	C(PAIR('d', 'i'), casedi),
	C(PAIR('d', 'a'), caseda),
	C(PAIR('w', 'h'), casewh),
	C(PAIR('d', 't'), casedt),
	C(PAIR('i', 't'), caseit),
	C(PAIR('r', 'm'), caserm),
	C(PAIR('r', 'r'), caserr),
	C(PAIR('r', 'n'), casern),
	C(PAIR('a', 'd'), casead),
	C(PAIR('r', 's'), casers),
	C(PAIR('n', 'a'), casena),
	C(PAIR('p', 'l'), casepl),
	C(PAIR('t', 'a'), caseta),
	C(PAIR('t', 'r'), casetr),
	C(PAIR('u', 'l'), caseul),
	C(PAIR('c', 'u'), casecu),
	C(PAIR('l', 't'), caselt),
	C(PAIR('n', 'x'), casenx),
	C(PAIR('s', 'o'), caseso),
	C(PAIR('i', 'g'), caseig),
	C(PAIR('t', 'c'), casetc),
	C(PAIR('f', 'c'), casefc),
	C(PAIR('e', 'c'), caseec),
	C(PAIR('e', 'o'), caseeo),
	C(PAIR('l', 'c'), caselc),
	C(PAIR('e', 'v'), caseev),
	C(PAIR('r', 'd'), caserd),
	C(PAIR('a', 'b'), caseab),
	C(PAIR('f', 'l'), casefl),
	C(PAIR('e', 'x'), done),
	C(PAIR('s', 's'), casess),
	C(PAIR('f', 'p'), casefp),
	C(PAIR('c', 's'), casecs),
	C(PAIR('b', 'd'), casebd),
	C(PAIR('l', 'g'), caselg),
	C(PAIR('h', 'c'), casehc),
	C(PAIR('h', 'y'), casehy),
	C(PAIR('n', 'h'), casenh),
	C(PAIR('n', 'm'), casenm),
	C(PAIR('n', 'n'), casenn),
	C(PAIR('s', 'v'), casesv),
	C(PAIR('o', 's'), caseos),
	C(PAIR('l', 's'), casels),
	C(PAIR('c', 'c'), casecc),
	C(PAIR('c', '2'), casec2),
	C(PAIR('e', 'm'), caseem),
	C(PAIR('a', 'f'), caseaf),
	C(PAIR('h', 'w'), casehw),
	C(PAIR('m', 'c'), casemc),
	C(PAIR('p', 'm'), casepm),
	C(PAIR('p', 'i'), casepi),
	C(PAIR('u', 'f'), caseuf),
	C(PAIR('p', 'c'), casepc),
	C(PAIR('h', 't'), caseht),
	C(PAIR('c', 'f'), casecf),
	C(PAIR('s', 'y'), casesy),
	C(PAIR('l', 'f'), caself),
	C(PAIR('!',  0 ), casesy),	/* synonym for .sy */
	C(PAIR('h', 'a'), caseha),	/* select hyphenation, (jaap) */
};


tchar oline[LNSIZE+1];

/*
 * troff environment block
 *
 * If you change this, don't forget to update tdef.h (jaap)
 */

struct	env env_array[NEV] = {
{
/* int	ics	 */	0,
/* int	sps	 */	0,
/* int	spacesz	 */	0,
/* int	lss	 */	0,
/* int	lss1	 */	0,
/* int	ll	 */	0,
/* int	ll1	 */	0,
/* int	lt	 */	0,
/* int	lt1	 */	0,
/* tchar	i*/	0,	/* insertion character (/* = margin character) */
/* int	icf	 */	0,
/* tchar	chbits	 */	0,	/* size+font bits for current character */
/* tchar	spbits	 */	0,
/* tchar	nmbits	 */	0,
/* int	apts	 */	PS,	/* actual point size -- as requested by user */
/* int	apts1	 */	PS,	/* need not match an existent size */
/* int	pts	 */	PS,	/* hence, this is the size that really exists */
/* int	pts1	 */	PS,
/* int	font	 */	FT,
/* int	font1	 */	FT,
/* int	ls	 */	1,
/* int	ls1	 */	1,
/* int	ad	 */	1,
/* int	nms	 */	1,
/* int	ndf	 */	1,
/* int	fi	 */	1,
/* int	cc	 */	'.',
/* int	c2	 */	'\'',
/* int	ohc	 */	OHC,
/* int	tdelim	 */	IMP,
#ifdef NROFF
/* int	hyf	 */	0,
#else
/* int	hyf	 */	1,
#endif
/* int	hyoff	 */	0,
/* int	hyalg	 */	ORIGINAL,	/* Default hyphenation style (jaap)*/
/* int	hyalg1	 */	ORIGINAL,
/* int	thresh	 */	THRESH,		/* now part of environment (jaap) */
/* int	un1	 */	-1,
/* int	tabc	 */	0,
/* int	dotc	 */	'.',
/* int	adsp	 */	0,
/* int	adrem	 */	0,
/* int	lastl	 */	0,
/* int	nel	 */	0,
/* int	admod	 */	0,
/* tchar	*wordp	 */	0,
/* int	spflg	 */	0,	/* probably to indicate space after punctuation needed */
/* tchar	*linep	 */	0,
/* tchar	*wdend	 */	0,
/* tchar	*wdstart	 */	0,
/* int	wne	 */	0,
/* int	ne	 */	0,
/* int	nc	 */	0,
/* int	nb	 */	0,
/* int	lnmod	 */	0,
/* int	nwd	 */	0,
/* int	nn	 */	0,
/* int	ni	 */	0,
/* int	ul	 */	0,
/* int	cu	 */	0,
/* int	ce	 */	0,
/* int	in	 */	0,
/* int	in1	 */	0,
/* int	un	 */	0,
/* int	wch	 */	0,
/* int	pendt	 */	0,
/* tchar	*pendw	 */	(tchar *)0,
/* int	pendnf	 */	0,
/* int	spread	 */	0,
/* int	it	 */	0,
/* int	itmac	 */	0,
/* int	lnsize	 */	LNSIZE,
/* int	stip	 */	STIP,
/* int	stip1	 */	STIP,
},
};

struct env *env = &env_array[0];
