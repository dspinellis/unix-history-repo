/*	c2.h	4.10	85/08/22	*/

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
#define MOVC3 52
#define RSB 53
#define JSB 54
#define MFPR 55
#define MTPR 56
#define PROBER 57
#define PROBEW 58
#define	LCOMM 59
#define	COMM 60

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

#define	JCC 20
#define	JCS 21
#define	JVC 22
#define	JVS 23

/*
 *	When the new opcodes were added, the relative
 *	ordering of the first 3 (those that are not float)
 *	had to be retained, so that other parts of the program
 *	were not broken.
 *
 *	In addition, the distance between OP3 and OP2 must be preserved.
 *	The order of definitions above OP2 must not be changed.
 *
 *	Note that these definitions DO NOT correspond to
 *	those definitions used in as, adb and sdb.
 */
#define	BYTE	1
#define	WORD	2
#define LONG	3
#define	FFLOAT	4
#define	DFLOAT	5
#define QUAD	6
#define OP2	7
#define OP3	8
#define OPB	9
#define OPX	10
#define	GFLOAT	11
#define	HFLOAT	12
#define OCTA	13

#define T(a,b) (a|((b)<<8))
#define U(a,b) (a|((b)<<4))

#define C2_ASIZE 128

struct optab {
	char	opstring[7];
	short	opcode;
} optab[];

struct node {
	union {
		struct {
			char op_op;
			char op_subop;
		} un_op;
		short	un_combop;
	} op_un;
	short	refc;
	struct	node	*forw;
	struct	node	*back;
	struct	node	*ref;
	char	*code;
	struct	optab	*pop;
	long	labno;
	short	seq;
};

#define op op_un.un_op.op_op
#define subop op_un.un_op.op_subop
#define combop op_un.un_combop

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
int	nsob;
int	nrtst;
int nbj;
int nfield;

int	nchange;
long	isn;
int	debug;
char	revbr[];
#define	NREG	12
char	*regs[NREG+5]; /* 0-11, 4 for operands, 1 for running off end */
char	conloc[C2_ASIZE];
char	conval[C2_ASIZE];
char	ccloc[C2_ASIZE];

#define	RT1	12
#define	RT2	13
#define RT3 14
#define RT4 15
#define	LABHS	127

char *copy();
long getnum();
struct node *codemove();
struct node *insertl();
struct node *nonlab();

#ifdef notdef
#define decref(p) \
	((p) && --(p)->refc <= 0 ? nrlab++, delnode(p) : 0)
#define delnode(p) \
	((p)->back->forw = (p)->forw, (p)->forw->back = (p)->back)
#endif notdef

char *xalloc();
extern char *newa;
extern char *lasta;
extern char *lastr;
#define	XALIGN(n) \
		(((n)+(sizeof (char *) - 1)) & ~(sizeof (char *) - 1))
#define	alloc(n) \
		((struct node *) \
		 ((newa = lasta) + (n) > lastr ? \
			xalloc(n) : \
			(lasta += XALIGN(n), newa)))
