/*	pass2.h	4.2	86/10/29	*/

#ifndef _PASS2_
#define	_PASS2_

#include "macdefs.h"
#include "mac2defs.h"
#include "manifest.h"

/* cookies, used as arguments to codgen */
#define FOREFF	01		/* compute for effects only */
#define INAREG	02		/* compute into a register */
#define INTAREG	04		/* compute into a scratch register */
#define INBREG	010		/* compute into a lvalue register */
#define INTBREG 020		/* compute into a scratch lvalue register */
#define FORCC	040		/* compute for condition codes only */
#define INTEMP	010000		/* compute into a temporary location */
#define FORARG	020000		/* compute for an argument of a function */
#define FORREW	040000		/* search the table for a rewrite rule */

/*
 * OP descriptors,
 * the ASG operator may be used on some of these
 */
#define OPSIMP	010000		/* +, -, &, |, ^ */
#define OPCOMM	010002		/* +, &, |, ^ */
#define OPMUL	010004		/* *, / */
#define OPDIV	010006		/* /, % */
#define OPUNARY	010010		/* unary ops */
#define OPLEAF	010012		/* leaves */
#define OPANY	010014		/* any op... */
#define OPLOG	010016		/* logical ops */
#define OPFLOAT	010020		/* +, -, *, or / (for floats) */
#define OPSHFT	010022		/* <<, >> */
#define OPLTYPE	010024		/* leaf type nodes (e.g, NAME, ICON, etc.) */

/* match returns */
#define MNOPE	010000		/* no match generated */
#define MDONE	010001		/* done evalution */

/* shapes */
#define SANY	01		/* same as FOREFF */
#define SAREG	02		/* same as INAREG */
#define STAREG	04		/* same as INTAREG */
#define SBREG	010		/* same as INBREG */
#define STBREG	020		/* same as INTBREG */
#define SCC	040		/* same as FORCC */
#define SNAME	0100		/* name */
#define SCON	0200		/* constant */
#define SFLD	0400		/* field */
#define SOREG	01000		/* offset from register */
/* indirection or wild card shapes */
#ifndef WCARD1
#define STARNM	02000		/* indirect through name */
#endif
#ifndef WCARD2
#define STARREG	04000		/* indirect through register */
#endif
#define SWADD	040000		/* word address */
#define SPECIAL	0100000		/* special stuff (follows) */
#define SZERO	SPECIAL		/* constant zero */
#define SONE	(SPECIAL|1)	/* constant +1 */
#define SMONE	(SPECIAL|2)	/* constant -1 */
#define SCCON	(SPECIAL|3)	/* -256 <= constant < 256 */
#define SSCON	(SPECIAL|4)	/* -32768 <= constant < 32768 */
#define SSOREG	(SPECIAL|5)	/* non-indexed OREG */
/* FORARG and INTEMP are carefully not conflicting with shapes */

/* types */
#define TCHAR		01	/* char */
#define TSHORT		02	/* short */
#define TINT		04	/* int */
#define TLONG		010	/* long */
#define TFLOAT		020	/* float */
#define TDOUBLE		040	/* double */
#define TPOINT		0100	/* pointer to something */
#define TUCHAR		0200	/* unsigned char */
#define TUSHORT		0400	/* unsigned short */
#define TUNSIGNED	01000	/* unsigned int */
#define TULONG		02000	/* unsigned long */
#define TPTRTO		04000	/* pointer to one of the above */
#define TANY		010000	/* matches anything within reason */
#define TSTRUCT		020000	/* structure or union */

/* reclamation cookies */
#define RNULL		0	/* clobber result */
#define RLEFT		01	/* reclaim left resource */
#define RRIGHT		02	/* reclaim right resource */
#define RESC1		04	/* reclaim resource allocated #1 */
#define RESC2		010	/* reclaim resource allocated #2 */
#define RESC3		020	/* reclaim resource allocated #3 */
#define RESCC		04000	/* reclaim condition codes */
#define RNOP		010000	/* DANGER: can cause loops.. */

/* needs */
#define NAREG		01	/* need an A register */
#define NACOUNT		03	/* count mask of A registers */
#define NAMASK		017	/* A register need field mask */
#define NASL		04	/* need A register shared with left resource */
#define NASR		010	/* need A register shared with right resource */
#define NBREG		020	/* need a B register */
#define NBCOUNT		060	/* count mask of B register */
#define NBMASK		0360	/* B register need field mask */
#define NBSL		0100	/* need B register shared with left resource */
#define NBSR		0200	/* need B register shared with right resource */
#define NTEMP		0400	/* need temporary storage location */
#define NTMASK		07400	/* count mask of temporary storage locations */
#define REWRITE		010000	/* need rewrite */
#define EITHER		040000	/* allocate all resources or nothing */

#define MUSTDO		010000	/* force register requirements */
#ifndef NOPREF
/* also defined in onepass.h */
#define NOPREF		020000	/* no preference for register assignment */
#endif
#define	NEVEN		0100000	/* even register required */

/* register allocation */
extern	int rstatus[];		/* register status info */
extern	int busy[];		/* register use info */
extern	struct respref {
	int	cform;
	int	mform;
} respref[];			/* resource preference rules */

#define isbreg(r)	(rstatus[r]&SBREG)
#define istreg(r)	(rstatus[r]&(STBREG|STAREG))
#define istnode(p)	(p->in.op==REG && istreg(p->tn.rval))

#define TBUSY		01000	/* register temporarily busy (during alloc) */
#define REGLOOP(i)	for (i = 0; i < REGSZ; ++i)

extern	NODE *deltrees[DELAYS];	/* trees held for delayed evaluation */
extern	int deli;		/* mmmmm */

#define SETSTO(x,y)	(stotree = (x), stocook = (y))
extern	int stocook;
extern	NODE *stotree;
extern	int callflag;

extern	int fregs;

#ifndef ONEPASS
#include "ndu.h"
#endif

extern	NODE node[];

/* code tables */
extern	struct optab {
	int	op;			/* operator to match */
	int	visit;			/* goal to match */
	int	lshape;			/* left shape to match */
	int	ltype;			/* left type to match */
	int	rshape;			/* right shape to match */
	int	rtype;			/* right type to match */
	int	needs;			/* resource required */
	int	rewrite;		/* how to rewrite afterwards */
	char	*cstring;		/* code generation template */
} table[];

extern	NODE resc[];

extern	OFFSZ tmpoff;
extern	OFFSZ maxoff;
extern	OFFSZ baseoff;
extern	OFFSZ maxtemp;
extern	int maxtreg;
extern	int ftnno;
extern	int rtyflg;
extern	int nrecur;		/* flag to keep track of recursions */

extern	NODE
	*talloc(),
	*eread(),
	*tcopy(),
	*getlr();

extern	CONSZ rdin();
extern	int eprint();
extern	char *rnames[];

extern	int lineno;
extern	char filename[];
extern	int fldshf, fldsz;
extern	int lflag, xdebug, udebug, edebug, odebug;
extern	int rdebug, radebug, tdebug, sdebug;
#ifdef FORT
extern	int Oflag;
#endif

#ifndef callchk
#define callchk(x) allchk()
#endif

#ifndef PUTCHAR
#define PUTCHAR(x) putchar(x)
#endif

/* macros for doing double indexing */
#define R2PACK(x,y,z)	(0200*((x)+1)+y+040000*z)	/* pack 3 regs */
#define R2UPK1(x)	((((x)>>7)-1)&0177)		/* unpack reg 1 */
#define R2UPK2(x)	((x)&0177)			/* unpack reg 2 */
#define R2UPK3(x)	(x>>14)				/* unpack reg 3 */
#define R2TEST(x)	((x)>=0200)			/* test if packed */

#ifdef MULTILEVEL
union	mltemplate {
	struct ml_head {
		int	tag;			/* tree class */
		int	subtag;			/* subclass of tree */
		union	mltemplate *nexthead;	/* linked by mlinit() */
	} mlhead;
	struct ml_node {
		int	op;			/* operator or op description */
		int	nshape;			/* node shape */
		/*
		 * Both op and nshape must match the node.
		 * where the work is to be done entirely by
		 * op, nshape can be SANY, visa versa, op can
		 * be OPANY.
		 */
		int	ntype;			/* type descriptor */
	} mlnode;
};
extern	union mltemplate mltree[];
#endif
#endif
