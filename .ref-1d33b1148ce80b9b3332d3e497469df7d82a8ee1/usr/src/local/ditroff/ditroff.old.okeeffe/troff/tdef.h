#include <signal.h>

#define	MAXPTR	(char *)-1	/* max value of any pointer variable */
				/* likely to be machine-dependent */

/* starting values for typesetting parameters: */

#define	PS	10	/* default point size */
#define	FT	1	/* default font position */
#define ST	1	/* default stipple */
#define	LL	(unsigned) 65*INCH/10	/* line length; 39picas=6.5in */
#define	VS	((12*INCH)/72)	/* initial vert space */

#ifdef	NROFF
#	define	EM	t.Em
#	define	HOR	t.Hor
#	define	VERT	t.Vert
#	define	INCH	240	/* increments per inch */
#	define	SPS	INCH/10	/* space size */
#	define	SS	INCH/10	/* " */
#	define	TRAILER	0
#	define	UNPAD	0227
#	define	PO	0 /* page offset */
#	define	ASCII	1
#	define	PTID	1
#	define	LG	0
#	define	DTAB	0	/* set at 8 Ems at init time */
#	define	ICS	2*SPS
#endif
#ifndef NROFF	/* TROFF */
	/* Inch is set by ptinit() when troff started.
	/* all derived values set then too
	*/
#	define	INCH	Inch	/* troff resolution -- number of goobies/inch  */
#	define	POINT	(INCH/72)	/* goobies per point (1/72 inch) */
#	define	HOR	Hor	/* horizontal resolution in goobies */
#	define	VERT	Vert	/* vertical resolution in goobies */
#	define	SPS	(EM/3)	/* space size  */
#	define	SS	12	/* space size in 36ths of an em */
#	define	UNPAD	027
#	define	PO	(INCH - INCH/27) /* page offset 26/27ths inch */
/* #	define	EM	(POINT * pts) */
#define	EM	(((long) INCH * pts + 36) / 72)	/* don't lose significance */
#define	EMPTS(pts)	(((long) INCH * (pts) + 36) / 72)
#	define	ASCII	0
#	define	PTID	1
#	define	LG	1
#	define	DTAB	(INCH/2)
#	define	ICS	3*SPS
#endif

/* These "characters" are used to encode various internal functions
/* Some make use of the fact that most ascii characters between
/* 0 and 040 don't have any graphic or other function.
/* The few that do have a purpose (e.g., \n, \b, \t, ...
/* are avoided by the ad hoc choices here.
/* See ifilt[] in n1.c for others -- 1, 2, 3, 5, 6, 7, 010, 011, 012 
*/

#define	LEADER	001
#define	IMP	004	/* impossible char; glues things together */
#define	TAB	011
#define	RPT	014	/* next character is to be repeated many times */
#define	CHARHT	015	/* size field sets character height */
#define	SLANT	016	/* size field sets amount of slant */
#define	DRAWFCN	017	/* next several chars describe arb drawing fcn */
			/* style: 's' m */
			/* thickness: 't' w */
			/* line: 'l' dx dy char */
			/* circle: 'c' r */
			/* ellipse: 'e' rx ry */
			/* arc: 'a' dx dy r */
			/* wiggly line '~' x y x y ... */
			/*	or     'g' x y x y ... */
#define DRAWTHICK	't'
#define DRAWSTYLE	's'
#define	DRAWLINE	'l'
#define	DRAWCIRCLE	'c'	/* circle */
#define	DRAWELLIPSE	'e'
#define	DRAWARC		'a'	/* arbitrary arc */
#define DRAWCURVE	'g'	/* gremlin spline */
#define	DRAWWIG		'~'	/* wiggly line with spline */
#define	DRAWPOLY	'p'	/* polygon with stipples */
#define	DRAWUBPOLY	'P'	/* polygon, stipples, no border */

#define	LEFT	020	/* \{ */
#define	RIGHT	021	/* \} */
#define	FILLER	022	/* \& and similar purposes */
#define	OHC	024	/* optional hyphenation character \% */
#define	CONT	025	/* \c character */
#define	PRESC	026	/* printable escape */
#define	XPAR	030	/* transparent mode indicator */
#define	FLSS	031
#define	WORDSP	032	/* paddable word space */
#define	ESC	033

	/* there are several tests (using iscontrol())
	/* that rely on these having 034 true.
	*/
#define	iscontrol(n)	(n>=034 && n<=037)	/* used to test the next two */
#define	HX	035	/* next character is value of \x'...' */
#define	FONTPOS	036	/* position of font \f(XX encoded in top */
#define	FONTNAME 037	/* name of font in \f(XX in top */
			/* have to appear in this order */

#define	HYPHEN	c_hyphen
#define	EMDASH	c_emdash	/* \(em */
#define	RULE	c_rule	/* \(ru */
#define	MINUS	c_minus	/* minus sign on current font */
#define	NARSP	c_narsp	/* narrow space \|: fake character */
#define	HNARSP	c_hnarsp	/* half narrow space \^: fake character */
#define	LIG_FI	c_fi	/* \(ff */
#define	LIG_FL	c_fl	/* \(fl */
#define	LIG_FF	c_ff	/* \(ff */
#define	LIG_FFI	c_ffi	/* \(Fi */
#define	LIG_FFL	c_ffl	/* \(Fl */
#define	ACUTE	c_acute	/* acute accent \(aa */
#define	GRAVE	c_grave	/* grave accent \(ga */
#define	UNDERLINE	c_under	/* \(ul */
#define	ROOTEN	c_rooten	/* root en \(rn */
#define	BOXRULE	c_boxrule	/* box rule \(br */
#define	LEFTHAND	c_lefthand

/* array sizes, and similar limits: */

#define	NFONT	60	/* maximum number of fonts (including specials) */
#define	EXTRAFONT	500	/* extra space for swapping a font */
#define	NN	300	/* number registers */
#define	NNAMES	15	 /* predefined reg names */
#define	NIF	15	/* if-else nesting */
#define	NS	64	/* name buffer */
#define	NTM	256	/* tm buffer */
#define	NEV	3	/* environments */
#define	EVLSZ	10	/* size of ev stack */
#define	DSIZE	512	/* disk sector size in chars */

/* the following defines the size of the infamous environment block.
   this macro is guaranteed to blow older C preprocessors out of the
   water.  Any additions to ni.c between "block" and the end of the
   data space affects this macro.
*/

#define	EVUSED	\
		(61 * sizeof(int)	/* integers in env block */	\
		+ 4 * sizeof(tchar)	/* tchars in env block */	\
		+ 5 * sizeof(tchar *)	/* tchar pointers in env block */ \
		+ NHYP * sizeof(tchar *)/* hytab */	\
		+ NTAB * sizeof(int)	/* tabtab */	\
		+ (LNSIZE+WDSIZE) * sizeof(tchar))	/* line+word */

#define	EVSPARE	DSIZE - EVUSED % DSIZE	/* number of leftover chars */
#define	EVS	(EVUSED + EVSPARE)	/* should be a multiple of DSIZE */
#define	NM	500	/* requests + macros */
#define	DELTA	1024	/* delta core bytes */
#define	NHYP	10	/* max hyphens per word */
#define	NHEX	128	/* byte size of exception word list */
#define	NTAB	35	/* tab stops */
#define	NSO	5	/* "so" depth */
#define	WDSIZE	270	/* word buffer size */
#define	LNSIZE	680	/* line buffer size */
#define	NDI	5	/* number of diversions */
#define	NTRTAB	350	/* number of items in trtab[] */
#define	NTRAP	20	/* number of traps */
#define	NPN	20	/* numbers in "-o" */
#define	FBUFSZ	256	/* field buf size words */
#define	OBUFSZ	512	/* bytes */
#define	IBUFSZ	512	/* bytes */
#define	NC	256	/* cbuf size words */
#define	NOV	10	/* number of overstrike chars */
#define	NPP	10	/* pads per field */

/*
	Internal character representation:
	Internally, every character is carried around as
	a 32 bit cookie, called a "tchar" (typedef long).
	Bits are numbered 31..0 from left to right.
	If bit 15 is 1, the character is motion, with
		if bit 16 it's vertical motion
		if bit 17 it's negative motion
	If bit 15 is 0, the character is a real character.
		if bit 31	zero motion
		bits 30..24	size
		bits 23..16	font
*/

/* in the following, "L" should really be a tchar, but ... */

#define	MOT	(unsigned short)(01<<15)	/* motion character indicator */
#define	MOTV	(07L<<15)	/* clear for motion part */
#define	VMOT	(01L<<16)	/* vert motion bit */
#define	NMOT	(01L<<17)	/* negative motion indicator*/
#define	MAXMOT	32767	/* bad way to write this!!! */
#define	ismot(n)	((unsigned short)(n) & MOT)	/* (short) is a cheap mask */
#define	isvmot(n)	((n) & VMOT)	/* must have tested MOT previously */
#define	isnmot(n)	((n) & NMOT)	/* ditto */
#define	absmot(n)	((unsigned short)(n) & ~MOT)	/* (short) is cheap mask */

#define	ZBIT	(01L << 31)	/* zero width char */
#define	iszbit(n)	((n) & ZBIT)

#define	SMASK		(0177L << 24)
#define	FMASK		(0377L << 16)
#define	SFMASK		(SMASK|FMASK)	/* size and font in a tchar */
#define	CMASK		~MOT		/* clears MOT */
#define	CMASKL		077777L
#define	sbits(n)	((unsigned short)((n) >> 24) & 0177)
#define	fbits(n)	((unsigned short)((n) >> 16) & 0377)
#define	sfbits(n)	((unsigned short)(((n) & SFMASK) >> 16))
#define	cbits(n)	(unsigned short)(n)	/* isolate bottom 16 bits  */
#define	setsbits(n,s)	n = (n & ~SMASK) | (tchar)(s) << 24
#define	setfbits(n,f)	n = (n & ~FMASK) | (tchar)(f) << 16
#define	setsfbits(n,sf)	n = (n & ~SFMASK) | (tchar)(sf) << 16
#define	setcbits(n,c)	n = (n & ~CMASKL | (c))	/* set character bits */

#define	MMASK	0100000	/* macro mask indicator */
#define	BMASK	0377
#define	BYTE	8

#define	ZONE	5	/* 5 hrs for EST */
#define	TMASK	 037777
#define	RTAB	(unsigned) 0100000
#define	CTAB	040000

#define	PAIR(A,B)	(A|(B<<BYTE))

typedef unsigned filep;
#define	BLK	128	/* alloc block tchars */
#define	NBLIST	1024	/* allocation list */
/* previous values were BLK 256 NBLIST 512 */
/* it seems good to keep the product constant */
		/* BLK*NBLIST<=65536 words, if filep=unsigned */

typedef	long	tchar;	/* as an experiment */

extern	tchar	getch(), getch0();
extern	tchar	rbf(), rbf0();
extern	tchar	mot(), hmot(), vmot();
extern	tchar	makem(), sethl();
extern	tchar	popi();
extern	tchar	getlg();
extern	tchar	xlss();
extern	tchar	setfield();
extern	tchar	setz();
extern	tchar	setch(), absch();
extern	tchar	setht(), setslant();

#define	atoi(i)		((int) atoi0(i))
extern	long	atoi0();

extern	int	Inch, Hor, Vert, Unitwidth;

/* these characters are used as various signals or values
/* in miscellaneous places.
/* values are set in specnames in t10.c
*/

extern int	c_hyphen;
extern int	c_emdash;
extern int	c_rule;
extern int	c_minus;
extern int	c_narsp;
extern int	c_hnarsp;
extern int	c_fi;
extern int	c_fl;
extern int	c_ff;
extern int	c_ffi;
extern int	c_ffl;
extern int	c_acute;
extern int	c_grave;
extern int	c_under;
extern int	c_rooten;
extern int	c_boxrule;
extern int	c_lefthand;

extern int	stderr;	/* this is NOT the stdio value! */
