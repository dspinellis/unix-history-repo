/*	c2.h	1.2	86/07/27	*/

/*
 * Header for object code improver
 */

/* tokens */
#define JBR	1
#define CBR	2
#define JMP	3
#define LABEL	4
#define DLABEL	5
#define EROU	6
#define JSW	7
#define MOV	8
#define CLR	9
#define INC	10
#define DEC	11
#define TST	12
#define PUSH	13
#define CVT	14
#define MOVZ	15
#define CMP	16
#define ADD	17
#define SUB	18
#define BIT	19
#define AND	20
#define OR	21
#define XOR	22
#define COM	23
#define NEG	24
#define EMUL	25
#define MUL	26
#define DIV	27
#define EDIV	28
#define SHAL	29
#define SHAR	30
#define SHL	31
#define SHR	32
#define CALLF	33
#define CALLS	34
#define CASE	35
#define ADDA	36
#define SUBA	37
#define AOBLEQ	38
#define AOBLSS	39
#define MOVA	40
#define PUSHA	41
#define LDF	42
#define LNF	43
#define STF	44
#define CMPF	45
#define CMPF2	46
#define TSTF	47
#define PUSHD	48
#define CVLF	49
#define CVFL	50
#define LDFD	51
#define CVDF	52
#define NEGF	53
#define ADDF	54
#define SUBF	55
#define MULF	56
#define DIVF	57
#define SINF	58
#define COSF	59
#define ATANF	60
#define LOGF	61
#define SQRTF	62
#define EXPF	63
#define MOVBLK	64
#define MFPR	65
#define MTPR	66
#define PROBE	67
#define MOVO	68
#define TEXT	69
#define DATA	70
#define BSS	71
#define ALIGN	72
#define END	73
#define LGEN	74
#define WGEN	75
#define SET	76
#define LCOMM	77
#define COMM	78

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

#define JBC 12
#define JBS 13
#define RET 14

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

/* #define T(a,b) (a|((b)<<8)) NUXI problems */
#define U(a,b) (a|((b)<<4))

#define C2_ASIZE 128

struct optab {
	char	opstring[7];
	char	opcod;
	unsigned char	subopcod;
} optab[];

struct node {
	char	op;
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
	char		op;
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
