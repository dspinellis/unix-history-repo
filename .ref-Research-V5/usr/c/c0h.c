#
/*

	C compiler-- pass 1 header

	Copyright 1973 Bell Telephone Laboratories, Inc.

*/

/*
  parameters
*/

#define	ncps	8
#define	hshsiz	200
#define	cmsiz	40
#define	swsiz	200
#define	ncpw	2
#define	ossiz	500
#define	dimsiz	100

#define	regtab	0
#define	efftab	1
#define	cctab	2
#define	sptab	3

struct tnode {
	int	op;
	int	type;
	int	dimp;
	struct tnode *tr1, *tr2;
};

struct {
	int	op;
	int	type;
	char	ssp;		/* subscript list */
	char	lenp;		/* structure length */
};

struct tname {
	int	op;
	int	type;
	int	dimp;
	int	class;
	int	offset;
	int	nloc;
};


struct tconst {
	int	op;
	int	type;
	int	dimp;
	int	value;
};

struct hshtab {
	char	hclass;
	char	hflag;
	int	htype;
	int	hdimp;
	int	hoffset;
	char	name[ncps];
};

struct swtab {
	int	swlab;
	int	swval;
};

char	cvtab[];
int	opdope[];
char	ctab[];
char	symbuf[ncps];
int	hshused;
struct	hshtab	hshtab[hshsiz];
int	*space;
int	*cp;
int	cmst[cmsiz];
int	isn;
struct	swtab	swtab[swsiz];
struct	swtab	*swp;
int	contlab;
int	brklab;
int	retlab;
int	deflab;
int	nauto;
int	autolen;
int	peeksym;
int	peekc;
int	eof;
int	line;
int	*treebase;
int	debug;
struct	hshtab	*defsym;
struct	hshtab	*funcsym;
int	xdflg;
int	proflg;
struct	hshtab	*csym;
int	cval;
double	fcval;
int	nchstr;
int	nerror;
struct	hshtab	*paraml;
struct	hshtab	*parame;
int	strflg;
int	osleft;
int	mosflg;
int	initflg;
int	inhdr;
int	dimtab[dimsiz];
char	binbuf[518];
int	dimp;
int	regvar;

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

#define	KEYW	19
#define	NAME	20
#define	CON	21
#define	STRING	22
#define	FCON	23
#define	SFCON	24

#define	SIZEOF	29
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

#define	EQUAL	60
#define	NEQUAL	61
#define	LESSEQ	62
#define	LESS	63
#define	GREATEQ	64
#define	GREAT	65
#define	LESSP	66
#define	LESSEQP	67
#define	GREATP	68
#define	GREATQP	69

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
#define	CALL	100
#define	MCALL	101
#define	JUMP	102
#define	CBRANCH	103
#define	INIT	104
#define	SETREG	105
#define	RFORCE	110
#define	BRANCH	111
#define	LABEL	112

/*
  types
*/
#define	INT	0
#define	CHAR	1
#define	FLOAT	2
#define	DOUBLE	3
#define	STRUCT	4
#define	RSTRUCT	5
#define	PTR	010
#define	FUNC	020
#define	ARRAY	030

/*
  storage classes
*/
#define	KEYWC	1
#define	MOS	4
#define	AUTO	5
#define	EXTERN	6
#define	STATIC	7
#define	REG	8
#define	STRTAG	9
#define ARG	10
#define	ARG1	11

/*
  keywords
*/
#define	GOTO	10
#define	RETURN	11
#define	IF	12
#define	WHILE	13
#define	ELSE	14
#define	SWITCH	15
#define	CASE	16
#define	BREAK	17
#define	CONTIN	18
#define	DO	19
#define	DEFAULT	20
#define	FOR	21

/*
  characters
*/
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
 * symbol table flags
 */

#define	FNDEL	1
