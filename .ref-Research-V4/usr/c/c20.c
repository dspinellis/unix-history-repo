#

#define	JBR	1
#define	CBR	2
#define	JMP	3
#define	LABEL	4
#define	DLABEL	5
#define	ESW	6
#define	EROU	7
#define	FOR	8
#define	JSW	9
#define	MOV	10
#define	CLR	11
#define	COM	12
#define	INC	13
#define	DEC	14
#define	NEG	15
#define	TST	16
#define	ASR	17
#define	ASL	18
#define	SXT	19
#define	CMP	20
#define	ADD	21
#define	SUB	22
#define	BIT	23
#define	BIC	24
#define	BIS	25
#define	MUL	26
#define	DIV	27
#define	ASH	28
#define	XOR	29
#define	TEXT	30
#define	DATA	31
#define	BSS	32
#define	EVEN	33
#define	MOVF	34
#define	MOVOF	35
#define	MOVFO	36
#define	ADDF	37
#define	SUBF	38
#define	DIVF	39
#define	MULF	40
#define	CLRF	41
#define	CMPF	42
#define	NEGF	43
#define	TSTF	44
#define	CFCC	45

#define	JEQ	0
#define	JNE	1
#define	JLE	2
#define	JGE	3
#define	JLT	4
#define	JGT	5
#define	JLO	6
#define	JHI	7
#define	JLOS	8
#define	JHIS	9

#define	BYTE	100

struct node {
	char	op;
	char	subop;
	struct	node	*forw;
	struct	node	*back;
	struct	node	*ref;
	int	labno;
	char	*code;
	int	refc;
};

struct {
	int	combop;
};

struct optab {
	char	*opstring;
	int	opcode;
} optab[] {
	"jbr",	JBR,
	"jeq",	CBR | JEQ<<8,
	"jne",	CBR | JNE<<8,
	"jle",	CBR | JLE<<8,
	"jge",	CBR | JGE<<8,
	"jlt",	CBR | JLT<<8,
	"jgt",	CBR | JGT<<8,
	"jlo",	CBR | JLO<<8,
	"jhi",	CBR | JHI<<8,
	"jlos",	CBR | JLOS<<8,
	"jhis",	CBR | JHIS<<8,
	"jmp",	JMP,
	"/esw",	ESW,
	"/for", FOR,
	".globl",EROU,
	"mov",	MOV,
	"clr",	CLR,
	"com",	COM,
	"inc",	INC,
	"dec",	DEC,
	"neg",	NEG,
	"tst",	TST,
	"asr",	ASR,
	"asl",	ASL,
	"sxt",	SXT,
	"cmp",	CMP,
	"add",	ADD,
	"sub",	SUB,
	"bit",	BIT,
	"bic",	BIC,
	"bis",	BIS,
	"mul",	MUL,
	"ash",	ASH,
	"xor",	XOR,
	".text",TEXT,
	".data",DATA,
	".bss",	BSS,
	".even",EVEN,
	"movf",	MOVF,
	"movof",MOVOF,
	"movfo",MOVFO,
	"addf",	ADDF,
	"subf",	SUBF,
	"divf",	DIVF,
	"mulf",	MULF,
	"clrf",	CLRF,
	"cmpf",	CMPF,
	"negf",	NEGF,
	"tstf",	TSTF,
	"cfcc",	CFCC,
	0,	0
};

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

int	nchange;
int	isn	20000;

int	debug;
char	*lasta;
char	*lastr;
char	*firstr;

char	revbr[] { JNE, JEQ, JGT, JLT, JGE, JLE, JHIS, JLOS, JHI, JLO };

char	regs[12][20];
#define	RT1	10
#define	RT2	11
#define	FREG	5
#define	NREG	5
#define	LABHS	127
#define	OPHS	57

struct optab *ophash[OPHS];
struct { int int;};
struct { char lbyte; };

main(argc, argv)
char **argv;
{
	register int niter, maxiter, isend;
	extern end;
	extern fin, fout;
	int nflag;

	if (argc>1 && argv[1][0]=='+') {
		argc--;
		argv++;
		debug++;
	}
	if (argc>1 && argv[1][0]=='-') {
		argc--;
		argv++;
		nflag++;
	}
	if (argc>1) {
		if ((fin = open(argv[1], 0)) < 0) {
			printf("C2: can't find %s\n", argv[1]);
			exit(1);
		}
	} else
		fin = dup(0);
	if (argc>2) {
		if ((fout = creat(argv[2], 0666)) < 0) {
			fout = 1;
			printf("C2: can't create %s\n", argv[2]);
			exit(1);
		}
	} else
		fout = dup(1);
	lasta = firstr = lastr = sbrk(2);
	maxiter = 0;
	opsetup();
	do {
		isend = input();
		movedat();
		niter = 0;
		do {
			refcount();
			do {
				iterate();
				clearreg();
				niter++;
			} while (nchange);
			rmove();
		} while (jumpsw());
		output();
		if (niter > maxiter)
			maxiter = niter;
		lasta = firstr;
	} while (isend);
	flush();
	fout = 2;
	if (nflag) {
		printf("%d iterations\n", maxiter);
		printf("%d jumps to jumps\n", nbrbr);
		printf("%d inst. after jumps\n", iaftbr);
		printf("%d jumps to .+2\n", njp1);
		printf("%d redundant labels\n", nrlab);
		printf("%d cross-jumps\n", nxjump);
		printf("%d code motions\n", ncmot);
		printf("%d branches reversed\n", nrevbr);
		printf("%d redundant moves\n", redunm);
		printf("%d simplified addresses\n", nsaddr);
		printf("%dK core\n", ((lastr+01777)>>10)&077);
		flush();
	}
}

input()
{
	register struct node *p, *lastp;
	register int op;

	lastp = &first;
	while ((op = getline()) >= 0) {
		switch (op.op) {
	
		case LABEL:
			p = alloc(sizeof first);
			if (line[0] == 'L') {
				p->combop = LABEL;
				p->labno = getnum(line+1);
				p->code = 0;
			} else {
				p->combop = DLABEL;
				p->labno = 0;
				p->code = copy(line);
			}
			break;
	
		case JBR:
		case CBR:
		case JMP:
		case JSW:
			p = alloc(sizeof first);
			p->combop = op;
			if (*curlp=='L' && (p->labno = getnum(curlp+1)))
				p->code = 0;
			else {
				p->labno = 0;
				p->code = copy(curlp);
			}
			break;

		default:
			p = alloc(sizeof first);
			p->combop = op;
			p->labno = 0;
			p->code = copy(curlp);
			break;

		}
		p->forw = 0;
		p->back = lastp;
		lastp->forw = p;
		lastp = p;
		p->ref = 0;
		if (op==EROU)
			return(1);
	}
	lastp->forw = 0;
	return(0);
}

getline()
{
	register char *lp;
	register c;

	lp = line;
	while (c = getchar()) {
		if (c==':') {
			*lp++ = 0;
			return(LABEL);
		}
		if (c=='\n') {
			*lp++ = 0;
			return(oplook());
		}
		*lp++ = c;
	}
	return(-1);
}

getnum(ap)
char *ap;
{
	register char *p;
	register n, c;

	p = ap;
	n = 0;
	while ((c = *p++) >= '0' && c <= '9')
		n = n*10 + c - '0';
	if (*--p != 0)
		return(0);
	return(n);
}

output()
{
	register struct node *t;
	register struct optab *op;
	register int byte;

	t = &first;
	while (t = t->forw) switch (t->op) {

	case LABEL:
		printf("L%d:", t->labno);
		continue;

	case DLABEL:
		printf("%s:", t->code);
		continue;

	default:
		if ((byte = t->subop) == BYTE)
			t->subop = 0;
		for (op = optab; op->opstring!=0; op++) 
			if (op->opcode == t->combop) {
				printf("%s", op->opstring);
				if (byte==BYTE)
					printf("b");
				break;
			}
		if (t->code)
			printf("\t%s\n", t->code);
		else if (t->op==JBR || t->op==CBR)
			printf("\tL%d\n", t->labno);
		else
			printf("\n");
		continue;

	case JSW:
		printf("L%d\n", t->labno);
		continue;

	case 0:
		if (t->code)
			printf("%s", t->code);
		printf("\n");
		continue;
	}
}

copy(ap)
char *ap;
{
	register char *p, *np;
	char *onp;
	register n;
	int na;

	na = nargs();
	p = ap;
	n = 0;
	if (*p==0)
		return(0);
	do
		n++;
	while (*p++);
	if (na>1) {
		p = (&ap)[1];
		while (*p++)
			n++;
	}
	onp = np = alloc(n);
	p = ap;
	while (*np++ = *p++);
	if (na>1) {
		p = (&ap)[1];
		np--;
		while (*np++ = *p++);
	}
	return(onp);
}

opsetup()
{
	register struct optab *optp, **ophp;
	register int *p;

	for (optp = optab; p = optp->opstring; optp++) {
		ophp = &ophash[((p[0]+p[1].lbyte)&077777) % OPHS];
		while (*ophp++)
			if (ophp > &ophash[OPHS])
				ophp = ophash;
		*--ophp = optp;
	}
}

oplook()
{
	register struct optab *optp;
	register char *lp, *op;
	static char tmpop[32];
	struct optab **ophp;

	op = tmpop;
	for (lp = line; *lp && *lp!=' ' && *lp!='\t';)
		*op++ = *lp++;
	*op++ = 0;
	while (*lp=='\t' || *lp==' ')
		lp++;
	curlp = lp;
	ophp = &ophash[((tmpop[0].int+tmpop[2])&077777) % OPHS];
	while (optp = *ophp) {
		op = optp->opstring;
		lp = tmpop;
		while (*lp == *op++)
			if (*lp++ == 0)
				return(optp->opcode);
		if (*lp++=='b' && *lp++==0 && *--op==0)
			return(optp->opcode + (BYTE<<8));
		ophp++;
		if (ophp >= &ophash[OPHS])
			ophp = ophash;
	}
	if (line[0]=='L') {
		lp = &line[1];
		while (*lp)
			if (*lp<'0' || *lp++>'9')
				return(0);
		curlp = line;
		return(JSW);
	}
	curlp = line;
	return(0);
}

refcount()
{
	register struct node *p, *lp;
	static struct node *labhash[LABHS];
	register struct node **hp;

	for (hp = labhash; hp < &labhash[LABHS];)
		*hp++ = 0;
	for (p = first.forw; p!=0; p = p->forw)
		if (p->op==LABEL) {
			labhash[p->labno % LABHS] = p;
			p->refc = 0;
		}
	for (p = first.forw; p!=0; p = p->forw) {
		if (p->op==JBR || p->op==CBR || p->op==JSW) {
			p->ref = 0;
			lp = labhash[p->labno % LABHS];
			if (lp && p->labno == lp->labno) {
				p->ref = lp;
				lp->refc++;
				continue;
			}
			for (lp=first.forw; lp; lp = lp->forw) {
				if (lp->op==LABEL && p->labno==lp->labno) {
					p->ref = lp;
					lp->refc++;
					break;
				}
			}
		}
	}
	for (p = first.forw; p!=0; p = p->forw)
		if (p->refc==0)
			p->refc++;
}

iterate()
{
	register struct node *p, *rp;

	nchange = 0;
	for (p = first.forw; p!=0; p = p->forw) {
		if ((p->op==JBR||p->op==CBR||p->op==JSW) && p->ref) {
			rp = nonlab(p->ref);
			if (rp->op==JBR && rp->labno) {
				nbrbr++;
				p->labno = rp->labno;
				decref(p->ref);
				rp->ref->refc++;
				p->ref = rp->ref;
				nchange++;
			}
		}
		if (p->op==JBR) {
			while (p->forw && p->forw->op!=LABEL) {
				nchange++;
				iaftbr++;
				if (p->forw->ref)
					decref(p->forw->ref);
				p->forw = p->forw->forw;
				p->forw->back = p;
			}
			rp = p->forw;
			while (rp && rp->op==LABEL) {
				if (p->ref == rp) {
					p->back->forw = p->forw;
					p->forw->back = p->back;
					p = p->back;
					decref(rp);
					nchange++;
					njp1++;
					break;
				}
				rp = rp->forw;
			}
			xjump(p);
			p = codemove(p);
		}
	}
}

xjump(ap)
{
	register int *p1, *p2, *p3;
	int nxj;

	nxj = 0;
	p1 = ap;
	if ((p2 = p1->ref) == 0)
		return(0);
	for (;;) {
		while ((p1 = p1->back) && p1->op==LABEL);
		while ((p2 = p2->back) && p2->op==LABEL);
		if (!equop(p1, p2))
			return(nxj);
		p3 = alloc(sizeof first);
		p3->combop = LABEL;
		p3->labno = isn;
		p3->ref = 0;
		p3->code = 0;
		p3->refc = 1;
		p3->back = p2->back;
		p3->forw = p2;
		p2->back->forw = p3;
		p2->back = p3;
		p1->combop = JBR;
		p1->ref = p3;
		p1->labno = isn++;
		p1->code = 0;
		nxj++;
		nxjump++;
		nchange++;
	}
}

codemove(ap)
struct node *ap;
{
	register struct node *p1, *p2, *p3;

	p1 = ap;
	if (p1->op!=JBR || (p2 = p1->ref)==0)
		return(p1);
	while (p2->op == LABEL)
		if ((p2 = p2->back) == 0)
			return(p1);
	if (p2->op!=JBR && p2->op!=ESW && p2->op!=FOR)
		return(p1);
	p2 = p2->forw;
	p3 = p1->ref;
	while (p3) {
		if (p3->op==JBR || p3->op==ESW) {
			if (p1 == p3)
				return(p1);
			ncmot++;
			nchange++;
			if (p2->back->op==FOR) {
				p2->back->op = JBR;
				p2->back->labno = p2->labno;
				p2->back->ref = p2;
				p2->refc++;
				p2->back->code = 0;
			}
			p1->back->forw = p2;
			p1->forw->back = p3;
			p2->back->forw = p3->forw;
			p3->forw->back = p2->back;
			p2->back = p1->back;
			p3->forw = p1->forw;
			decref(p1->ref);
			return(p2);
		} else
			p3 = p3->forw;
	}
	return(p1);
}

rmove()
{
	register struct node *p;
	register char *cp;
	register int r;
	int r1, flt;

	for (p=first.forw; p!=0; p = p->forw) {
	if (debug) {
		for (r=0; r<2*NREG; r++)
			if (regs[r][0])
				printf("%d: %s\n", r, regs[r]);
		printf("-\n");
	}
	flt = 0;
	switch (p->op) {

	case MOVF:
	case MOVFO:
	case MOVOF:
		flt = NREG;

	case MOV:
		dualop(p);
		if ((r = findrand(regs[RT1], flt)) >= 0) {
			if (r == flt+isreg(regs[RT2]) && p->forw->op!=CBR) {
				p->forw->back = p->back;
				p->back->forw = p->forw;
				redunm++;
				continue;
			}
		}
		repladdr(p, 0, flt);
		r = isreg(regs[RT1]);
		r1 = isreg(regs[RT2]);
		dest(regs[RT2], flt);
		if (r >= 0)
			if (r1 >= 0)
				savereg(r1+flt, regs[r+flt]);
			else
				savereg(r+flt, regs[RT2]);
		else
			if (r1 >= 0)
				savereg(r1+flt, regs[RT1]);
		source(regs[RT1]);
		continue;

	case ADDF:
	case SUBF:
	case DIVF:
	case MULF:
		flt = NREG;

	case ADD:
	case SUB:
	case BIC:
	case BIS:
	case MUL:
	case DIV:
	case ASH:
		dualop(p);
		repladdr(p, 0, flt);
		source(regs[RT1]);
		dest(regs[RT2], flt);
		if (p->op==DIV && (r = isreg(regs[RT2])>=0))
			regs[r+1][0] = 0;
		continue;

	case CLRF:
	case NEGF:
		flt = NREG;

	case CLR:
	case COM:
	case INC:
	case DEC:
	case NEG:
	case ASR:
	case ASL:
	case SXT:
		singop(p);
		dest(regs[RT1], flt);
		continue;

	case TSTF:
		flt = NREG;

	case TST:
		singop(p);
		repladdr(p, 0, flt);
		source(regs[RT1]);
		continue;

	case CMPF:
		flt = NREG;

	case CMP:
	case BIT:
		dualop(p);
		source(regs[RT1]);
		source(regs[RT2]);
		repladdr(p, 1, flt);
		continue;

	case CBR:
	case CFCC:
		continue;

	default:
		clearreg();
	}
	}
}

jumpsw()
{
	register struct node *p, *p1;
	register t;
	int nj;

	t = 0;
	nj = 0;
	for (p=first.forw; p!=0; p = p->forw)
		p->refc = ++t;
	for (p=first.forw; p!=0; p = p1) {
		p1 = p->forw;
		if (p->op == CBR && p1->op==JBR && p->ref && p1->ref
		 && abs(p->refc - p->ref->refc) > abs(p1->refc - p1->ref->refc)) {
			p->subop = revbr[p->subop];
			t = p1->ref;
			p1->ref = p->ref;
			p->ref = t;
			t = p1->labno;
			p1->labno = p->labno;
			p->labno = t;
			nrevbr++;
			nj++;
		}
	}
	return(nj);
}

abs(x)
{
	return(x<0? -x: x);
}

equop(ap1, p2)
struct node *ap1, *p2;
{
	register char *cp1, *cp2;
	register struct node *p1;

	p1 = ap1;
	if (p1->combop != p2->combop)
		return(0);
	if (p1->op>0 && p1->op<MOV)
		return(0);
	cp1 = p1->code;
	cp2 = p2->code;
	if (cp1==0 && cp2==0)
		return(1);
	if (cp1==0 || cp2==0)
		return(0);
	while (*cp1 == *cp2++)
		if (*cp1++ == 0)
			return(1);
	return(0);
}

decref(ap)
{
	register struct node *p;

	p = ap;
	if (--p->refc <= 0) {
		nrlab++;
		p->back->forw = p->forw;
		p->forw->back = p->back;
	}
}

nonlab(ap)
struct node *ap;
{
	register struct node *p;

	p = ap;
	while (p && p->op==LABEL)
		p = p->forw;
	return(p);
}

alloc(an)
{
	register int n;
	register char *p;

	n = an;
	n++;
	n =& ~01;
	if (lasta+n >= lastr) {
		if (sbrk(2000) == -1) {
			write(2, "Out of space\n", 14);
			exit(1);
		}
		lastr =+ 2000;
	}
	p = lasta;
	lasta =+ n;
	return(p);
}

clearreg()
{
	register int i;

	for (i=0; i<2*NREG; i++)
		regs[i][0] = '\0';
}

savereg(ai, as)
char *as;
{
	register char *p, *s;

	p = regs[ai];
	s = as;
	if (source(s))
		return;
	while (*p++ = *s)
		if (*s++ == ',')
			break;
	*--p = '\0';
}

dest(as, flt)
char *as;
{
	register char *s;
	register int i;

	s = as;
	if ((i = isreg(s)) >= 0)
		regs[i+flt][0] = 0;
	while ((i = findrand(s, flt)) >= 0)
		regs[i][0] = 0;
	while (*s) {
		if ((*s=='(' && (*(s+1)!='r' || *(s+2)!='5')) || *s++=='*') {
			for (i=flt; i<flt+NREG; i++)
				if (regs[i][0] != '$')
					regs[i][0] = 0;
			return;
		}
	}
}

singop(ap)
struct node *ap;
{
	register char *p1, *p2;

	p1 = ap->code;
	p2 = regs[RT1];
	while (*p2++ = *p1++);
	regs[RT2][0] = 0;
}


dualop(ap)
struct node *ap;
{
	register char *p1, *p2;
	register struct node *p;

	p = ap;
	p1 = p->code;
	p2 = regs[RT1];
	while (*p1 && *p1!=',')
		*p2++ = *p1++;
	*p2++ = 0;
	p2 = regs[RT2];
	*p2 = 0;
	if (*p1++ !=',')
		return;
	while (*p2++ = *p1++);
}

findrand(as, flt)
char *as;
{
	register char *s;
	register int i;
	register char *p;

	for (i = flt; i<NREG+flt; i++) {
		p = regs[i];
		s = as;
		while (*s == *p++)
			if (*s++ == 0)
				return(i);
	}
	return(-1);
}

isreg(as)
char *as;
{
	register char *s;

	s = as;
	if (s[0]=='r' && s[1]>='0' && s[1]<='4' && s[2]==0)
		return(s[1]-'0');
	return(-1);
}

check()
{
	register struct node *p, *lp;

	lp = &first;
	for (p=first.forw; p!=0; p = p->forw) {
		if (p->back != lp)
			abort();
		lp = p;
	}
}

source(ap)
char *ap;
{
	register char *p1, *p2;

	p1 = ap;
	p2 = p1;
	if (*p1==0)
		return(0);
	while (*p2++);
	if (*p1=='-' && *(p1+1)=='('
	 || *p1=='*' && *(p1+1)=='-' && *(p1+2)=='('
	 || *(p2-2)=='+') {
		while (*p1 && *p1++!='r');
		if (*p1>='0' && *p1<='4')
			regs[*p1 - '0'][0] = 0;
		return(1);
	}
	return(0);
}

repladdr(p, f, flt)
struct node *p;
{
	register r;
	int r1;
	register char *p1, *p2;
	static char rt1[50], rt2[50];

	if (f)
		r1 = findrand(regs[RT2], flt);
	else
		r1 = -1;
	r = findrand(regs[RT1], flt);
	if (r1 >= NREG)
		r1 =- NREG;
	if (r >= NREG)
		r =- NREG;
	if (r>=0 || r1>=0) {
		p2 = regs[RT1];
		for (p1 = rt1; *p1++ = *p2++;);
		if (regs[RT2][0]) {
			p1 = rt2;
			*p1++ = ',';
			for (p2 = regs[RT2]; *p1++ = *p2++;);
		} else
			rt2[0] = 0;
		if (r>=0) {
			rt1[0] = 'r';
			rt1[1] = r + '0';
			rt1[2] = 0;
			nsaddr++;
		}
		if (r1>=0) {
			rt2[1] = 'r';
			rt2[2] = r1 + '0';
			rt2[3] = 0;
			nsaddr++;
		}
		p->code = copy(rt1, rt2);
	}
}

movedat()
{
	register struct node *p1, *p2;
	struct node *p3;
	register seg;
	struct node data;
	struct node *datp;

	if (first.forw == 0)
		return;
	datp = &data;
	for (p1 = first.forw; p1!=0; p1 = p1->forw) {
		if (p1->op == DATA) {
			p2 = p1->forw;
			while (p2 && p2->op!=TEXT)
				p2 = p2->forw;
			if (p2==0)
				break;
			p3 = p1->back;
			p1->back->forw = p2->forw;
			p2->forw->back = p3;
			p2->forw = 0;
			datp->forw = p1;
			p1->back = datp;
			p1 = p3;
			datp = p2;
		}
	}
	if (data.forw) {
		datp->forw = first.forw;
		first.forw->back = datp;
		data.forw->back = &first;
		first.forw = data.forw;
	}
	seg = -1;
	for (p1 = first.forw; p1!=0; p1 = p1->forw) {
		if (p1->op==TEXT|p1->op==DATA||p1->op==BSS) {
			if (p1->op == seg || p1->forw&&p1->forw->op==seg) {
				p1->back->forw = p1->forw;
				p1->forw->back = p1->back;
				p1 = p1->back;
				continue;
			}
			seg = p1->op;
		}
	}
}
