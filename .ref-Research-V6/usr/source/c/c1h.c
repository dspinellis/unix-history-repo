/*
 *	C pass 2 header
 */

#define	swsiz	200
#define	ossiz	500

struct	tnode {
	int	op;
	int	type;
	int	degree;
	struct	tnode *tr1, *tr2;
};

struct	bnode {
	int	bop;
	struct	tnode *btree;
	int	lbl;
	int	cond;
};

struct	tname {
	int	nop;
	int	ntype;
	int	elsize;
	char	class;
	char	regno;
	int	offset;
	int	nloc;
};

/*
 * for field selections
 */
struct tsel {
	int	op;
	int	type;
	int	degree;
	struct	tnode *tr1;
	char	flen;
	char	bitoffs;
};

struct	tconst {
	int	cop;
	int	ctype;
	int	cdeg;
	int	value;
};

struct	optab {
	char	tabdeg1;
	char	tabtyp1;
	char	tabdeg2;
	char	tabtyp2;
	char	*tabstring;
};

struct	table {
	int	tabop;
	struct	optab *tabp;
};

struct	instab {
	int	iop;
	char	*str1;
	char	*str2;
};

struct	swtab {
	int	swlab;
	int	swval;
};

char	maprel[];
char	notrel[];
int	nreg;
int	isn;
int	namsiz;
int	line;
char	ascbuf[518];
int	nerror;
struct	table	cctab[];
struct	table	efftab[];
struct	table	regtab[];
struct	table	sptab[];
struct	table	lsptab[];
struct	instab	instab[];
struct	instab	branchtab[];
int	opdope[];
char	*opntab[];
int	nstack;
int	nfloat;
int	*spacep;
int	treespace[ossiz];
int	eolflg;
struct tconst czero, cone, fczero;

/*
	operators
*/
#define	EOF	0
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

#define	KEYW	19
#define	NAME	20
#define	CON	21
#define	STRING	22
#define	FCON	23
#define	SFCON	24

#define	AUTOI	27
#define	AUTOD	28
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
#define	NAND	55
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
#define	TAND	81
#define	LTIMES	82
#define	LDIV	83
#define	LMOD	84
#define	ASSNAND	85
#define	LASTIMES 86
#define	LASDIV	87
#define	LASMOD	88

#define	QUEST	90
#define	LLSHIFT	91
#define	ASLSHL	92
#define	CALL1	98
#define	CALL2	99
#define	CALL	100
#define	MCALL	101
#define	JUMP	102
#define	CBRANCH	103
#define	INIT	104
#define	SETREG	105
#define	LOAD	106
#define	RFORCE	110

/*
 * Intermediate code operators
 */
#define	BRANCH	111
#define	LABEL	112
#define	NLABEL	113
#define	RLABEL	114
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
#define	NULL	218

/*
 *	types
 */
#define	INT	0
#define	CHAR	1
#define	FLOAT	2
#define	DOUBLE	3
#define	STRUCT	4
#define	RSTRUCT	5
#define	LONG	6

#define	TYLEN	2
#define	TYPE	07
#define	XTYPE	(03<<3)
#define	PTR	010
#define	FUNC	020
#define	ARRAY	030

/*
	storage	classes
*/
#define	KEYWC	1
#define	MOS	10
#define	AUTO	11
#define	EXTERN	12
#define	STATIC	13
#define	REG	14
#define	STRTAG	15
#define	ARG	16
#define	OFFS	20
#define	XOFFS	21
#define	SOFFS	22

/*
	Flag	bits
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
#define	CNVRT	01000
