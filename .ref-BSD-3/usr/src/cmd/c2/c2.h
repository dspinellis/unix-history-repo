/* @(#)c2.h 1.13 78/09/23 16:37:28 */
/*
 * Header for object code improver
 */

#define	JBR	1
#define	CBR	2
#define	JMP	3
#define	LABEL	4
#define	DLABEL	5
#define	EROU	7
#define	JSW	9
#define	MOV	10
#define	CLR	11
#define	INC	12
#define	DEC	13
#define	TST	14
#define	PUSH	15
#define CVT 16
#define	CMP	17
#define	ADD	18
#define	SUB	19
#define	BIT	20
#define	BIC	21
#define	BIS	22
#define	XOR	23
#define	COM	24
#define	NEG	25
#define	MUL	26
#define	DIV	27
#define	ASH	28
#define EXTV	29
#define EXTZV	30
#define INSV	31
#define	CALLS	32
#define RET	33
#define	CASE	34
#define	SOB	35
#define	TEXT	36
#define	DATA	37
#define	BSS	38
#define	ALIGN	39
#define	END	40
#define MOVZ 41
#define WGEN 42
#define SOBGEQ 43
#define SOBGTR 44
#define AOBLEQ 45
#define AOBLSS 46
#define ACB 47
#define MOVA 48
#define PUSHA 49
#define LGEN 50
#define SET 51

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
#define JLBC 14
#define JLBS 15
#define JBCC 16
#define JBSC 17
#define JBCS 18
#define JBSS 19

#define	BYTE	1
#define	WORD	2
#define LONG	3
#define	FLOAT	4
#define	DOUBLE	5
#define QUAD	6
#define OP2	7
#define OP3	8
#define OPB 9
#define OPX 10

#define T(a,b) (a|((b)<<8))
#define U(a,b) (a|((b)<<4))

struct optab {
	char	opstring[7];
	short	opcode;
} optab[];

struct node {
	char	op;
	char	subop;
	short	refc;
	struct	node	*forw;
	struct	node	*back;
	struct	node	*ref;
	char	*code;
	struct	optab	*pop;
	short	labno;
	short	seq;
};

struct {
	short	combop;
};

char	line[BUFSIZ];
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
int	nsob;
int	nrtst;
int nbj;
int nfield;

int	nchange;
int	isn;
int	debug;
char	*lasta;
char	*lastr;
char	*firstr;
char	revbr[];
#define	NREG	12
char	*regs[NREG+5]; /* 0-11, 4 for operands, 1 for running off end */
char	conloc[20];
char	conval[20];
char	ccloc[20];

#define	RT1	12
#define	RT2	13
#define RT3 14
#define RT4 15
#define	LABHS	127

struct { char lbyte; };

char *copy();
long getnum();
struct node *codemove();
struct node *insertl();
struct node *nonlab();
