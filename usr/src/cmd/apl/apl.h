#include <stdio.h>

double *test1;
double gamma();
/*
 *  A P L
 */

/*
 *  Magic numbers
 */

#define OPERBOXSIZE 76

#define MRANK	8			/* Size of dimension buffer	*/
#define CANBS	300			/* Size of input buffer		*/
#define STKS	50			/* Size of stack increment	*/
#define NLS	100			/* Size of namelist		*/
#define NAMS	24			/* Size of name buffer		*/
#define OBJS	500			/* Size of compiled expr buffer	*/
#define MAGIC	0100554			/* Magic word for WS file	*/

/*
 *  Magic words
 */

#define	EDIT_ED	"/usr/bin/ed"
#define	EDIT_EX	"/usr/ucb/ex"
#define EDIT_VI "/usr/ucb/vi"

/*
 *  Debug modes
 */
#define FULLD 1

#ifdef FULLD
#define SOMED SOMED
#endif

#ifdef SHORTD
#define SOMED SOMED
#endif

/*
 *  Derived constants
 */

#define	SDAT	sizeof datum
#define	SINT	sizeof integ

/*
 *  Interpreter Op Codes
 */

#define EOF	(-1)
#define EOL	0

#define ADD	1
#define PLUS	2
#define SUB	3
#define MINUS	4
#define MUL	5
#define SGN	6
#define DIV	7
#define RECIP	8
#define MOD	9
#define ABS	10
#define MIN	11
#define FLOOR	12
#define MAX	13
#define CEIL	14
#define PWR	15
#define EXP	16
#define LOG	17
#define LOGE	18
#define CIR	19
#define PI	20
#define COMB	21
#define FAC	22

#define DEAL	23
#define RAND	24
#define DRHO	25
#define MRHO	26
#define DIOT	27
#define MIOT	28
#define ROT0	29
#define REV0	30
#define DTRN	31
#define MTRN	32
#define DIBM	33
#define MIBM	34

#define GDU	35
#define GDUK	36
#define GDD	37
#define GDDK	38
#define EXD	39
#define SCAN	40
#define EXDK	41
#define SCANK	42
#define IPROD	43
#define OPROD	44
#define QUAD	45
#define QQUAD	46
#define BRAN0	47
#define BRAN	48
#define DDOM	49
#define MDOM	50

#define COM	51
#define RED	52
#define COMK	53
#define REDK	54
#define ROT	55
#define REV	56
#define ROTK	57
#define REVK	58
#define CAT	59
#define RAV	60
#define CATK	61
#define RAVK	62

#define PRINT	63
#define QUOT	64
#define ELID	65
#define CQUAD	66
#define COMNT	67
#define INDEX	68
#define HPRINT	69

#define LT	71
#define LE	72
#define GT	73
#define GE	74
#define EQ	75
#define NE	76
#define AND	77
#define OR	78
#define NAND	79
#define NOR	80
#define NOT	81
#define EPS	82
#define MEPS	83
#define REP	84
#define TAKE	85
#define DROP	86
#define ASGN	88
#define IMMED	89


#define NAME	90
#define CONST	91
#define FUN	92
#define ARG1	93
#define ARG2	94
#define AUTO	95
#define REST	96

#define COM0	97
#define RED0	98
#define EXD0	99
#define SCAN0	100
#define BASE	101
#define MENC	102	/*	monadic	encode	*/

/*
 *  Immediate sub-op codes
 */

#define	CLEAR	1
#define	DIGITS	2
#define	ED_IT	3
#define	ERASE	4
#define	FNS	5
#define	FUZZ	6
#define	READ	7
#define	ORIGIN	8
#define	VARS	9
#define	WIDTH	10
#define	DEBUG	11
#define OFF	12
#define LOAD	13
#define SAVE	14
#define COPY	15
#define CONTIN	16
#define LIB	17
#define DROPC	18
#ifdef SOMED
#define	SYMBOLS	19
#endif
#define	EX_IT	20
#define EX_VI	21
#define ASCII	22
#define APL	23
#define CSH 	24

/*
 *  Types
 */

#define	DA	1	/* Numeric data (?) */
#define	CH	2	/* Character data (?) */
#define	LV	3	/* Used for most data items */
#define	QD	4	/* Quad (assignment to (?)) */
#define	QQ	5	/* Quote-quad */
#define	IN	6	/* ??? */
#define	EL	7	/* Literal data (??) */
#define	NF	8	/* Name of function with no args */
#define	MF	9	/* Monadic function */
#define	DF	10	/* Dyadic function */
#define QC	11

/*
 *  Idiosyncracies
 */

#define data	double
#define unsignd	unsigned

/*
 *  Structures
 */

struct {
	char	c[8];
};

struct env {
	double	fuzz;
	int	iorg;
	int	digits;
	int	width;
} thread;

struct item {
	char	rank;
	char	type;
	int	size;
	int	index;
	data	*datap;
	int	dim[MRANK];
};

struct nlist {
	char	use;
	char	type;	/* == LV */
	int	*itemp;
	char	*namep;
	int	label;
} nlist[NLS];

struct	lablist {
	char	*lname;
	int	lno;
	struct	lablist *nextll;
} labldefs;

struct {
	char	rank;
	char	type;
	int	size;
	int	dimk;
	int	delk;
	int	dim[MRANK];
	int	del[MRANK];
	int	idx[MRANK];
} idx;

struct	{
	char	*name;
	int	line;
} now_xeq;

struct	item **sp, **stack, **staktop;	/* Internal run-time stack */

/*
 *  Externals
 */

data	zero;
data	one;
data	pi;
data	maxexp;
data	datum;
data	getdat();

int	cs_size;			/* Current stack size	   */

int	(*exop[])();

double	floor();
double	ceil();
double	log();
double	sin();
double	cos();
double	atan();
double	atan2();
double	sqrt();
double	exp();
double	gamma();
double	ltod();

int	integ;
int	signgam;
int	column;
int	intflg;
int	echoflg;
int	ifile;
int	wfile;
int	ofile;
int	funlc;
int	debug;
int	ttystat[3];
int	stime[2];
char	*pcp;
int     rowsz;
int	mencflg;
char    *mencptr;
char	*memstart;
int pt;
int syze;
int pas1;

char	*continu;

struct charbox {
	char a1,a2;
};

struct asoperbox {
	unsigned char letters[2];
	int	returnchar
};
