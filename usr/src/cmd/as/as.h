#
/* %Z%%M% %R%.%L% %D% %T% */
#define readonly
#define	NINST	300
#define	NSYM	3000
#define	NHASH	(NSYM+1)
#define	NLOC	4		/* number of location ctrs */
#define	NCPS	8

/*
 * Symbol types
 */
#define	XUNDEF	0x0
#define	XABS	0x2
#define	XTEXT	0x4
#define	XDATA	0x6
#define	XBSS	0x8
#define	XDATAO	0xA
#define	XBSSO	0xC
#define	XTEXTO	0xE
#define	XABSO	0x10
#define	XUNDEFO	0x12

#define	XTXRN	0xA
#define	XXTRN	0x1
#define	XTYPE	0x1E

#define	XFORW	0x20	/* Was forward-referenced when undefined */

#define	ERR	(-1)
#define	NBPW	32

#define	AMASK	017

/*
 * Actual argument syntax types
 */
#define AREG	1	/* %r */
#define ABASE	2	/* (%r) */
#define ADECR	3	/* -(%r) */
#define AINCR	4	/* (%r)+ */
#define ADISP	5	/* expr(%r) */
#define AEXP	6	/* expr */
#define AIMM	7	/* $ expr */
#define ASTAR	8	/* * */
#define AINDX	16	/* [%r] */

/*
 * Argument access types
 */
#define ACCA	(8<<3)	/* address only */
#define ACCR	(1<<3)	/* read */
#define ACCW	(2<<3)	/* write */
#define ACCM	(3<<3)	/* modify */
#define ACCB	(4<<3)	/* branch displacement */
#define ACCI	(5<<3)	/* XFC code */

/*
 * Argument data types
 */
#define TYPB	0	/* byte */
#define TYPW	1	/* word */
#define TYPL	2	/* long */
#define TYPQ	3	/* quad */
#define TYPF	4	/* floating */
#define TYPD	5	/* double floating */

#define TYPMASK 7

/* reference types for loader */
#define PCREL 1
#define LEN1 2
#define LEN2 4
#define LEN4 6
#define LEN8 8

#define	TMPC	7
#define	HW	01
#define	FW	03
#define	DW	07

#define	PAGRND	0x1FFL

#define	round(x,y)	(((x)+(y)) & ~(y))

#define	STABTYPS	0340

struct	symtab {
	char	name[NCPS];
	char	type;
	char	tag;
	short	index;
	long	value;
	char	ptype;
	char	other;
	short	desc;
};

struct	instab {
	char	name[NCPS];
	char	opcode; /* must have same offset as symtab.type */
	char	tag;	/* yacc-coded nargs (INST0, ...) or token # (IALIGN, ...) */
	char	nargs;
	char	argtyp[6];
};

struct	arg {
	char	atype;
	char	areg1;
	char	areg2;
	struct	exp *xp;
};

struct	exp {
	char	xtype;
	char	xloc;
	long	xvalue;
	long	yvalue;		/* 2nd half of double floating */
	struct	symtab *xname;
};

struct	hdr {
	long	magic;
	long	tsize;
	long	dsize;
	long	bsize;
	long	ssize;
	long	entry;
	long	trsize;
	long	drsize;
};

struct	symtab	*symtab;
struct	symtab	*hshtab[NHASH];
struct	arg	arglist[6];
struct	exp	explist[20];
extern	int	lineno;
extern	int	hshused;
extern	int	usrname;
extern	int lclname;
extern	struct	symtab *nextsym;
extern	struct	exp	usedot[NLOC+NLOC];
extern	FILE	*usefile[NLOC+NLOC];
extern	FILE	*rusefile[NLOC+NLOC];
extern	char	*tmpn2;
extern	char	*tmpn3;
extern	struct	exp	*dotp;
extern	int	loctr;
extern	long	tsize;
extern	long	dsize;
extern	long	datbase;
extern	int	bitoff;
extern	long	bitfield;
extern	char	yytext[NCPS+2];
extern	FILE	*txtfil;
extern	FILE	*tmpfil;
extern	FILE	*relfil;
extern	int	passno;
extern	int	argcnt;
extern	int	anyerrs;
extern	int	*brptr;
struct	instab	*itab[NINST];
struct	instab	instab[];
int	curlen;
int	gindex;
int	orgwarn;
struct	symtab	**lookup();
struct	exp	*combine();
