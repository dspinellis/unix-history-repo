#include "tdef.h"

/* You may want to change these names */
/* these SHOULD be defined in the makefile */

#ifndef FONTDIR
#	define FONTDIR "/usr/lib/font"
#endif
#ifndef MACROLIB
			/* MACROLIB is the prefix of the macro filenames */
#	define MACROLIB "/usr/lib/tmac/tmac."
#endif

#ifndef NROFF
char	termtab[NS] = FONTDIR;		/* rest added in ptinit() */
char	fontfile[NS] = FONTDIR;		/* rest added in casefp() */
char	devname[20]	 = "va";	/* default typesetter */
#endif
char	obuf[OBUFSZ];	/* characters collected here for actual typesetter output */
char	*obufp = obuf;
int	r[NN] = {	/* read-only number registers at beginning */
	PAIR('%', 0),
	PAIR('n', 'l'),
	PAIR('y', 'r'),
	PAIR('h', 'p'),
	PAIR('c', 't'),
	PAIR('d', 'n'),
	PAIR('m', 'o'),
	PAIR('d', 'y'),
	PAIR('d', 'w'),
	PAIR('l', 'n'),
	PAIR('d', 'l'),
	PAIR('s', 't'),
	PAIR('s', 'b'),
	PAIR('c', '.'),
	PAIR('$', '$'),
};


int	pto = 10000;
int	pfrom = 1;
int	print = 1;
char	nextf[NS] = MACROLIB;
int	nfi;
#ifdef NROFF
char	termtab[NS] = "/usr/lib/term/tab37";
#endif
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
int	ascii = ASCII;
int	ptid = PTID;
int	lg = LG;
int	pnlist[NPN] = {
	-1};


int	*pnp = pnlist;
int	npn = 1;
int	npnflg = 1;
int	xflg = 1;
int	dpn = -1;
int	totout = 1;
int	ulfont = FT + 1;
int	ulbit = 1 << 9;
int	tabch = TAB;
int	ldrch = LEADER;
int	xxx;
extern caseds(), caseas(), casesp(), caseft(), caseps(), casevs(),
casenr(), caseif(), casepo(), casetl(), casetm(), casebp(), casech(),
casepn(), tbreak(), caseti(), casene(), casenf(), casece(), casefi(),
casein(), casell(), casens(), casemk(), casert(), caseam(), casest(),
casede(), casedi(), caseda(), casewh(), casedt(), caseit(), caserm(),
casern(), casead(), casers(), casena(), casepl(), caseta(), casetr(),
caseul(), caselt(), casenx(), caseso(), caseig(), casetc(), casefc(),
caseec(), caseeo(), caselc(), caseev(), caserd(), caseab(), casefl(),
done(), casess(), casefp(), casecs(), casebd(), caselg(), casehc(),
casehy(), casenh(), casenm(), casenn(), casesv(), caseos(), casels(),
casecc(), casec2(), caseem(), caseaf(), casehw(), casemc(), casepm(),
casecu(), casepi(), caserr(), caseuf(), caseie(), caseel(), casepc(),
caseht(), casecf(), casesy();

struct contab {
	int	rq;
	/*
	union {
 */
	int	(*f)();
	/*
		unsigned mx;
	}x;
 */
} contab[NM] = {
	PAIR('d', 's'), caseds,
	PAIR('a', 's'), caseas,
	PAIR('s', 'p'), casesp,
	PAIR('f', 't'), caseft,
	PAIR('p', 's'), caseps,
	PAIR('v', 's'), casevs,
	PAIR('n', 'r'), casenr,
	PAIR('i', 'f'), caseif,
	PAIR('i', 'e'), caseie,
	PAIR('e', 'l'), caseel,
	PAIR('p', 'o'), casepo,
	PAIR('t', 'l'), casetl,
	PAIR('t', 'm'), casetm,
	PAIR('b', 'p'), casebp,
	PAIR('c', 'h'), casech,
	PAIR('p', 'n'), casepn,
	PAIR('b', 'r'), tbreak,
	PAIR('t', 'i'), caseti,
	PAIR('n', 'e'), casene,
	PAIR('n', 'f'), casenf,
	PAIR('c', 'e'), casece,
	PAIR('f', 'i'), casefi,
	PAIR('i', 'n'), casein,
	PAIR('l', 'l'), casell,
	PAIR('n', 's'), casens,
	PAIR('m', 'k'), casemk,
	PAIR('r', 't'), casert,
	PAIR('a', 'm'), caseam,
	PAIR('s', 't'), casest,
	PAIR('d', 'e'), casede,
	PAIR('d', 'i'), casedi,
	PAIR('d', 'a'), caseda,
	PAIR('w', 'h'), casewh,
	PAIR('d', 't'), casedt,
	PAIR('i', 't'), caseit,
	PAIR('r', 'm'), caserm,
	PAIR('r', 'r'), caserr,
	PAIR('r', 'n'), casern,
	PAIR('a', 'd'), casead,
	PAIR('r', 's'), casers,
	PAIR('n', 'a'), casena,
	PAIR('p', 'l'), casepl,
	PAIR('t', 'a'), caseta,
	PAIR('t', 'r'), casetr,
	PAIR('u', 'l'), caseul,
	PAIR('c', 'u'), casecu,
	PAIR('l', 't'), caselt,
	PAIR('n', 'x'), casenx,
	PAIR('s', 'o'), caseso,
	PAIR('i', 'g'), caseig,
	PAIR('t', 'c'), casetc,
	PAIR('f', 'c'), casefc,
	PAIR('e', 'c'), caseec,
	PAIR('e', 'o'), caseeo,
	PAIR('l', 'c'), caselc,
	PAIR('e', 'v'), caseev,
	PAIR('r', 'd'), caserd,
	PAIR('a', 'b'), caseab,
	PAIR('f', 'l'), casefl,
	PAIR('e', 'x'), done,
	PAIR('s', 's'), casess,
	PAIR('f', 'p'), casefp,
	PAIR('c', 's'), casecs,
	PAIR('b', 'd'), casebd,
	PAIR('l', 'g'), caselg,
	PAIR('h', 'c'), casehc,
	PAIR('h', 'y'), casehy,
	PAIR('n', 'h'), casenh,
	PAIR('n', 'm'), casenm,
	PAIR('n', 'n'), casenn,
	PAIR('s', 'v'), casesv,
	PAIR('o', 's'), caseos,
	PAIR('l', 's'), casels,
	PAIR('c', 'c'), casecc,
	PAIR('c', '2'), casec2,
	PAIR('e', 'm'), caseem,
	PAIR('a', 'f'), caseaf,
	PAIR('h', 'w'), casehw,
	PAIR('m', 'c'), casemc,
	PAIR('p', 'm'), casepm,
	PAIR('p', 'i'), casepi,
	PAIR('u', 'f'), caseuf,
	PAIR('p', 'c'), casepc,
	PAIR('h', 't'), caseht,
	PAIR('c', 'f'), casecf,
	PAIR('s', 'y'), casesy,
	PAIR('!', 0), casesy,	/* synonym for .sy */
};


tchar oline[LNSIZE+1];

/*
troff environment block
*/

int	block	 = 0;
#ifdef	NROFF
/* these are initialized statically in nroff (so far)
	/* and dynamically in ptinit() in troff beacuse INCH
	/* is a variable
	*/
int	ics	 = ICS;	/* space for insertion character */
int	sps	 = SPS;
int	spacesz	 = SS;
int	lss	 = VS;
int	lss1	 = VS;
int	ll	 = LL;
int	ll1	 = LL;
int	lt	 = LL;
int	lt1	 = LL;
#else
int	ics	 = 0;
int	sps	 = 0;
int	spacesz	 = 0;
int	lss	 = 0;
int	lss1	 = 0;
int	ll	 = 0;
int	ll1	 = 0;
int	lt	 = 0;
int	lt1	 = 0;
#endif
tchar	ic	 = 0;	/* insertion character (= margin character) */
int	icf	 = 0;
tchar	chbits	 = 0;	/* size+font bits for current character */
tchar	spbits	 = 0;	/* ditto for special font */
tchar	nmbits	 = 0;
int	apts	 = PS;	/* actual point size -- as requested by user */
int	apts1	 = PS;	/* need not match an existent size */
int	pts	 = PS;	/* hence, this is the size that really exists */
int	pts1	 = PS;
int	font	 = FT;
int	font1	 = FT;
int	stip	 = ST;
int	stip1	 = ST;
int	ls	 = 1;
int	ls1	 = 1;
int	ad	 = 1;
int	nms	 = 1;
int	ndf	 = 1;
int	fi	 = 1;
int	cc	 = '.';
int	c2	 = '\'';
int	ohc	 = OHC;
int	tdelim	 = IMP;
int	hyf	 = 1;
int	hyoff	 = 0;
int	un1	 = -1;
int	tabc	 = 0;
int	dotc	 = '.';
int	adsp	 = 0;
int	adrem	 = 0;
int	lastl	 = 0;
int	nel	 = 0;
int	admod	 = 0;
tchar	*wordp	 = 0;
int	spflg	 = 0;	/* probably to indicate space after punctuation needed */
tchar	*linep	 = 0;
tchar	*wdend	 = 0;
tchar	*wdstart	 = 0;
int	wne	 = 0;
int	ne	 = 0;
int	nc	 = 0;
int	nb	 = 0;
int	lnmod	 = 0;
int	nwd	 = 0;
int	nn	 = 0;
int	ni	 = 0;
int	ul	 = 0;
int	cu	 = 0;
int	ce	 = 0;
int	in	 = 0;
int	in1	 = 0;
int	un	 = 0;
int	wch	 = 0;
int	pendt	 = 0;
tchar	*pendw	 = 0;
int	pendnf	 = 0;
int	spread	 = 0;
int	it	 = 0;
int	itmac	 = 0;
int	lnsize	 = LNSIZE;
tchar	*hyptr[NHYP]	 = {
	0};


int	tabtab[NTAB]	 = {
	0};


tchar	line[LNSIZE]	 = {
	0};


tchar	word[WDSIZE]	 = {
	0};


char	blockxxx[EVSPARE]	 = {
	0};


