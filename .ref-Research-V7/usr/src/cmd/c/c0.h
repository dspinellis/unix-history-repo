#
/*
* 	C compiler-- first pass header
*/

#include <stdio.h>

/*
 * parameters
 */

#define	LTYPE	long	/* change to int if no long consts */
#define	MAXINT	077777	/* Largest positive short integer */
#define	MAXUINT	0177777	/* largest unsigned integer */
#define	NCPS	8	/* # chars per symbol */
#define	HSHSIZ	400	/* # entries in hash table for names */
#define	CMSIZ	40	/* size of expression stack */
#define	SSIZE	20	/* size of other expression stack */
#define	SWSIZ	230	/* size of switch table */
#define	NMEMS	128	/* Number of members in a structure */
#define	NBPW	16	/* bits per word, object machine */
#define	NBPC	8	/* bits per character, object machine */
#define	NCPW	2	/* chars per word, object machine */
#define	LNCPW	2	/* chars per word, compiler's machine */
#define	STAUTO	(-6)	/* offset of first auto variable */
#define	STARG	4	/* offset of first argument */


/*
 * # bytes in primitive types
 */
#define	SZCHAR	1
#define	SZINT	2
#define	SZPTR	2
#define	SZFLOAT	4
#define	SZLONG	4
#define	SZDOUB	8

/*
 * format of a structure description
 */
struct str {
	int	ssize;			/* structure size */
	struct hshtab 	**memlist;	/* member list */
};

/*
 * For fields, strp points here instead.
 */
struct field {
	int	flen;		/* field width in bits */
	int	bitoffs;	/* shift count */
};

/*
 * Structure of tree nodes for operators
 */
struct tnode {
	int	op;		/* operator */
	int	type;		/* data type */
	int	*subsp;		/* subscript list (for arrays) */
	struct	str *strp;	/* structure description for structs */
	struct	tnode *tr1;	/* left operand */
	struct	tnode *tr2;	/* right operand */
};

/*
 * Tree node for constants
 */
struct	cnode {
	int	op;
	int	type;
	int	*subsp;
	struct	str *strp;
	int	value;
};

/*
 * Tree node for long constants
 */
struct lnode {
	int	op;
	int	type;
	int	*subsp;
	struct	str *strp;
	LTYPE	lvalue;
};

/*
 * tree node for floating
 * constants
 */
struct	fnode {
	int	op;
	int	type;
	int	*subsp;
	struct	str *strp;
	char	*cstr;
};

/*
 * Structure of namelist
 */
/*
 * Pushed-down entry for block structure
 */
struct	phshtab {
	char	hclass;
	char	hflag;
	int	htype;
	int	*hsubsp;
	struct	str *hstrp;
	int	hoffset;
	struct	phshtab *hpdown;
	char	hblklev;
};

/*
 * Top-level namelist
 */
struct hshtab {
	char	hclass;		/* storage class */
	char	hflag;		/* various flags */
	int	htype;		/* type */
	int	*hsubsp;	/* subscript list */
	struct	str *hstrp;	/* structure description */
	int	hoffset;	/* post-allocation location */
	struct	phshtab *hpdown;	/* Pushed-down name in outer block */
	char	hblklev;	/* Block level of definition */
	char	name[NCPS];	/* ASCII name */
};

/*
 * Place used to keep dimensions
 * during declarations
 */
struct	tdim {
	int	rank;
	int	dimens[5];
};

/*
 * Table for recording switches.
 */
struct swtab {
	int	swlab;
	int	swval;
};

char	cvtab[4][4];
char	filename[64];
int	opdope[];
char	ctab[];
char	symbuf[NCPS+2];
int	hshused;
struct	hshtab	hshtab[HSHSIZ];
struct	tnode **cp;
int	isn;
struct	swtab	swtab[SWSIZ];
struct	swtab	*swp;
int	contlab;
int	brklab;
int	retlab;
int	deflab;
unsigned autolen;		/* make these int if necessary */
unsigned maxauto;		/* ... will only cause trouble rarely */
int	peeksym;
int	peekc;
int	eof;
int	line;
char	*funcbase;
char	*curbase;
char	*coremax;
char	*maxdecl;
struct	hshtab	*defsym;
struct	hshtab	*funcsym;
int	proflg;
struct	hshtab	*csym;
int	cval;
LTYPE	lcval;
int	nchstr;
int	nerror;
struct	hshtab	**paraml;
struct	hshtab	**parame;
int	strflg;
int	mosflg;
int	initflg;
int	inhdr;
char	sbuf[BUFSIZ];
FILE	*sbufp;
int	regvar;
int	bitoffs;
struct	tnode	funcblk;
char	cvntab[];
char	numbuf[64];
struct	hshtab **memlist;
int	nmems;
struct	hshtab	structhole;
int	blklev;
int	mossym;

/*
  operators
*/
#define	EOFC	0
#define	NULLOP	218
#define	SEMI	1
#define	LBRACE	2
#define	RBRACE	3
#define	LBRACK	4
#define	RBRACK	5
#define	LPARN	6
#define	RPARN	7
#define	COLON	8
#define	COMMA	9
#define	FSEL	10
#define	CAST	11
#define	ETYPE	12

#define	KEYW	19
#define	NAME	20
#define	CON	21
#define	STRING	22
#define	FCON	23
#define	SFCON	24
#define	LCON	25
#define	SLCON	26

#define	SIZEOF	91
#define	INCBEF	30
#define	DECBEF	31
#define	INCAFT	32
#define	DECAFT	33
#define	EXCLA	34
#define	AMPER	35
#define	STAR	36
#define	NEG	37
#define	COMPL	38

#define	DOT	39
#define	PLUS	40
#define	MINUS	41
#define	TIMES	42
#define	DIVIDE	43
#define	MOD	44
#define	RSHIFT	45
#define	LSHIFT	46
#define	AND	47
#define	OR	48
#define	EXOR	49
#define	ARROW	50
#define	ITOF	51
#define	FTOI	52
#define	LOGAND	53
#define	LOGOR	54
#define	FTOL	56
#define	LTOF	57
#define	ITOL	58
#define	LTOI	59
#define	ITOP	13
#define	PTOI	14
#define	LTOP	15

#define	EQUAL	60
#define	NEQUAL	61
#define	LESSEQ	62
#define	LESS	63
#define	GREATEQ	64
#define	GREAT	65
#define	LESSEQP	66
#define	LESSP	67
#define	GREATQP	68
#define	GREATP	69

#define	ASPLUS	70
#define	ASMINUS	71
#define	ASTIMES	72
#define	ASDIV	73
#define	ASMOD	74
#define	ASRSH	75
#define	ASLSH	76
#define	ASSAND	77
#define	ASOR	78
#define	ASXOR	79
#define	ASSIGN	80

#define	QUEST	90
#define	MAX	93
#define	MAXP	94
#define	MIN	95
#define	MINP	96
#define	SEQNC	97
#define	CALL	100
#define	MCALL	101
#define	JUMP	102
#define	CBRANCH	103
#define	INIT	104
#define	SETREG	105
#define	RFORCE	110
#define	BRANCH	111
#define	LABEL	112
#define	NLABEL	113
#define	RLABEL	114
#define	STRASG	115
#define	ITOC	109
#define	SEOF	200	/* stack EOF marker in expr compilation */

/*
  types
*/
#define	INT	0
#define	CHAR	1
#define	FLOAT	2
#define	DOUBLE	3
#define	STRUCT	4
#define	LONG	6
#define	UNSIGN	7
#define	UNION	8		/* adjusted later to struct */

#define	ALIGN	01
#define	TYPE	07
#define	BIGTYPE	060000
#define	TYLEN	2
#define	XTYPE	(03<<3)
#define	PTR	010
#define	FUNC	020
#define	ARRAY	030

/*
  storage classes
*/
#define	KEYWC	1
#define	DEFXTRN	20
#define	TYPEDEF	9
#define	MOS	10
#define	AUTO	11
#define	EXTERN	12
#define	STATIC	13
#define	REG	14
#define	STRTAG	15
#define ARG	16
#define	ARG1	17
#define	AREG	18
#define	MOU	21
#define	ENUMTAG	22
#define	ENUMCON	24

/*
  keywords
*/
#define	GOTO	20
#define	RETURN	21
#define	IF	22
#define	WHILE	23
#define	ELSE	24
#define	SWITCH	25
#define	CASE	26
#define	BREAK	27
#define	CONTIN	28
#define	DO	29
#define	DEFAULT	30
#define	FOR	31
#define	ENUM	32

/*
  characters
*/
#define	BSLASH	117
#define	SHARP	118
#define	INSERT	119
#define	PERIOD	120
#define	SQUOTE	121
#define	DQUOTE	122
#define	LETTER	123
#define	DIGIT	124
#define	NEWLN	125
#define	SPACE	126
#define	UNKN	127

/*
 * Special operators in intermediate code
 */
#define	BDATA	200
#define	WDATA	201
#define	PROG	202
#define	DATA	203
#define	BSS	204
#define	CSPACE	205
#define	SSPACE	206
#define	SYMDEF	207
#define	SAVE	208
#define	RETRN	209
#define	EVEN	210
#define	PROFIL	212
#define	SWIT	213
#define	EXPR	214
#define	SNAME	215
#define	RNAME	216
#define	ANAME	217
#define	SETSTK	219
#define	SINIT	220

/*
  Flag bits
*/

#define	BINARY	01
#define	LVALUE	02
#define	RELAT	04
#define	ASSGOP	010
#define	LWORD	020
#define	RWORD	040
#define	COMMUTE	0100
#define	RASSOC	0200
#define	LEAF	0400

/*
 * Conversion codes
 */
#define	ITF	1
#define	ITL	2
#define	LTF	3
#define	ITP	4
#define	PTI	5
#define	FTI	6
#define	LTI	7
#define	FTL	8
#define	LTP	9
#define	ITC	10
#define	XX	15

/*
 * symbol table flags
 */

#define	FMOS	01
#define	FKEYW	04
#define	FFIELD	020
#define	FINIT	040
#define	FLABL	0100

/*
 * functions
 */
char	*sbrk();
struct	tnode *tree();
char	*copnum();
struct	tnode *convert();
struct	tnode *chkfun();
struct	tnode *disarray();
struct	tnode *block();
struct	cnode *cblock();
struct	fnode *fblock();
char	*gblock();
struct	tnode *pexpr();
struct	str *strdec();
struct	hshtab *xprtype();
struct	tnode *nblock();
