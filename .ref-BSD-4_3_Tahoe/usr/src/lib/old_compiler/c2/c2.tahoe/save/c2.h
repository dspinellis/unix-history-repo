/*	%M%	%I%	%E%	*/

/*
 * Header for object code improver
 */

/* tokens */
typedef	enum {
	NIL,
	JBR, CBR, JMP, LABEL, DLABEL, EROU, JSW,
	MOV, CLR, INC, DEC, TST, PUSH, CVT, MOVZ,
	CMP, ADD, SUB, BIT, AND, OR, XOR, COM,
	NEG, EMUL, MUL, DIV, EDIV, SHAL, SHAR,
	SHL, SHR, CALLF, CALLS, CASE, ADDA, SUBA,
	AOBLEQ, AOBLSS, MOVA, PUSHA, LDF, LNF, STF,
	CMPF, CMPF2, TSTF, PUSHD, CVLF, CVFL, LDFD,
	CVDF, NEGF, ADDF, SUBF, MULF, DIVF, SINF,
	COSF, ATANF, LOGF, SQRTF, EXPF, MOVBLK,
	MFPR, MTPR, PROBE, MOVO, TEXT, DATA, BSS,
	ALIGN, END, LGEN, WGEN, SET, LCOMM, COMM
} OpCode;

#define	ord(e)	((int)(e))

#define	JEQ	0
#define	JNE	1
#define	JLE	2
#define	JGE	3
#define	JLT	4
#define	JGT	5
/* rearranged for unsigned branches so that jxxu = jxx + 6 */
#define	JLOS	8
#define	JHIS	9
#define	JLO	10
#define	JHI	11

#define	JBC	12
#define	JBS	13
#define	RET	14

#define	BYTE	1
#define	WORD	2
#define LONG	3
#define QUAD	4
#define FLOAT	5
#define DOUBLE	6
#define OP2	7
#define OP3	8
#define OPB	9
#define OPX	10

#define	has2ops(p)	(((p)->subop>>4) == OP2)
#define	has3ops(p)	(((p)->subop>>4) == OP3)

/* #define T(a,b) (a|((b)<<8)) NUXI problems */
#define U(a,b) (a|((b)<<4))

#define C2_ASIZE 128

struct optab {
	char	opstring[7];
	OpCode	opcod;
	unsigned char	subopcod;
} optab[];

struct node {
	OpCode	op;
	unsigned char	subop;
	short	refc;
	struct	node	*forw;
	struct	node	*back;
	struct	node	*ref;
	char	*code;
	struct	optab	*pop;
	long	labno;
	short	seq;
};

struct intleavetab  {
	OpCode		op;
	unsigned char	subop;
	int		intleavect;
} intltab[];

/* struct { NUXI problems
	short	combop;
}; */

char	line[512];
struct	node	first;
char	*curlp;
int	nbrbr;
int	nsaddr;
int	redunm;
int	iaftbr;
int	njp1;
int	nrlab;
int	nxjump;
int	ncmot;
int	nrevbr;
int	loopiv;
int	nredunj;
int	nskip;
int	ncomj;
int	naob;
int	nrtst;
int	nbj;
int	nst;
int	nld;

int	nchange;
int	isn;
int	debug;
int	fortflg;
int 	aobflag;
char	*lasta;
char	*lastr;
char	*firstr;
char	revbr[];
#define	NREG	13
/* 0-12, f.p. accumulator, 4 for operands, 1 for running off end */
char	*regs[NREG+6];
char	conloc[C2_ASIZE];
char	conval[C2_ASIZE];
char	ccloc[C2_ASIZE];

#define	ACC	NREG
#define	RT1	NREG+1
#define	RT2	NREG+2
#define RT3	NREG+3
#define RT4	NREG+4
#define	LABHS	127

#define MAXAOBDISP	5000

#define NUSE 6
#define	tempreg(s,r)	((unsigned)((r)=isreg(s)) < NUSE)
#define	isused(r,p)	(uses[(r)] && uses[(r)] == (p)))
#define	lastuse(r)	(uses[(r)])
#define	setuse(r,p)	(uses[(r)] = (p))

long usemask;		/* registers used within a block */
struct node *uses[NUSE + 7]; /* for backwards flow analysis */
struct node *useacc; /* same for acc */
char *lastrand; /* last operand of instruction */
struct node *bflow();
char *copy();
long getnum();
struct node *codemove();
struct node *insertl();
struct node *nonlab();
struct node *alloc();
char *findcon();
char *byondrd();
#define equstr !strcmp
#define COPYCODE
