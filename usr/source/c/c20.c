#
/*
 *	 C object code improver
 */

#include "c2h.c"

struct optab optab[] {
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
	"sob",	SOB,
	"jsr",	JSR,
	".end",	END,
	0,	0};

char	revbr[] { JNE, JEQ, JGT, JLT, JGE, JLE, JHIS, JLOS, JHI, JLO };
int	isn	20000;

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
			comjump();
			rmove();
		} while (nchange || jumpsw());
		addsob();
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
		printf("%d loops inverted\n", loopiv);
		printf("%d redundant jumps\n", nredunj);
		printf("%d common seqs before jmp's\n", ncomj);
		printf("%d skips over jumps\n", nskip);
		printf("%d sob's added\n", nsob);
		printf("%d redundant tst's\n", nrtst);
		printf("%d literals eliminated\n", nlit);
		printf("%dK core\n", ((lastr+01777)>>10)&077);
		flush();
	}
	exit(0);
}

input()
{
	register struct node *p, *lastp;
	register int op;

	lastp = &first;
	for (;;) {
		op = getline();
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
		if (op==END)
			return(0);
	}
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
	*lp++ = 0;
	return(END);
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

	case END:
		return;

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
		if (t->code) {
			reducelit(t);
			printf("\t%s\n", t->code);
		} else if (t->op==JBR || t->op==CBR)
			printf("\tL%d\n", t->labno);
		else
			printf("\n");
		continue;

	case JSW:
		printf("L%d\n", t->labno);
		continue;

	case SOB:
		printf("sob	%s,L%d\n", t->code, t->labno);
		continue;

	case 0:
		if (t->code)
			printf("%s", t->code);
		printf("\n");
		continue;
	}
}

/*
 * Notice addresses of the form
 * $xx,xx(r)
 * and replace them with (pc),xx(r)
 *     -- Thanx and a tip of the Hatlo hat to Bliss-11.
 */
reducelit(at)
struct node *at;
{
	register char *c1, *c2;
	char *c2s;
	register struct node *t;

	t = at;
	if (*t->code != '$')
		return;
	c1 = t->code;
	while (*c1 != ',')
		if (*c1++ == '\0')
			return;
	c2s = c1;
	c1++;
	if (*c1=='*')
		c1++;
	c2 = t->code+1;
	while (*c1++ == *c2++);
	if (*--c1!='(' || *--c2!=',')
		return;
	t->code = copy("(pc)", c2s);
	nlit++;
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
	register char *p;

	for (optp = optab; p = optp->opstring; optp++) {
		ophp = &ophash[(((p[0]<<3)+(p[1]<<1)+p[2])&077777) % OPHS];
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
	ophp = &ophash[(((tmpop[0]<<3)+(tmpop[1]<<1)+tmpop[2])&077777) % OPHS];
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
			if (lp==0 || p->labno!=lp->labno)
			for (lp = first.forw; lp!=0; lp = lp->forw) {
				if (lp->op==LABEL && p->labno==lp->labno)
					break;
			}
			if (lp) {
				hp = nonlab(lp)->back;
				if (hp!=lp) {
					p->labno = hp->labno;
					lp = hp;
				}
				p->ref = lp;
				lp->refc++;
			}
		}
	}
	for (p = first.forw; p!=0; p = p->forw)
		if (p->op==LABEL && p->refc==0
		 && (lp = nonlab(p))->op && lp->op!=JSW)
			decref(p);
}

iterate()
{
	register struct node *p, *rp, *p1;

	nchange = 0;
	for (p = first.forw; p!=0; p = p->forw) {
		if ((p->op==JBR||p->op==CBR||p->op==JSW) && p->ref) {
			rp = nonlab(p->ref);
			if (rp->op==JBR && rp->labno && p!=rp) {
				nbrbr++;
				p->labno = rp->labno;
				decref(p->ref);
				rp->ref->refc++;
				p->ref = rp->ref;
				nchange++;
			}
		}
		if (p->op==CBR && (p1 = p->forw)->op==JBR) {
			rp = p->ref;
			do
				rp = rp->back;
			while (rp->op==LABEL);
			if (rp==p1) {
				decref(p->ref);
				p->ref = p1->ref;
				p->labno = p1->labno;
				p1->forw->back = p;
				p->forw = p1->forw;
				p->subop = revbr[p->subop];
				nchange++;
				nskip++;
			}
		}
		if (p->op==JBR || p->op==JMP) {
			while (p->forw && p->forw->op!=LABEL
				&& p->forw->op!=EROU && p->forw->op!=END) {
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
	if ((p2 = p1->ref)==0)
		return(0);
	for (;;) {
		while ((p1 = p1->back) && p1->op==LABEL);
		while ((p2 = p2->back) && p2->op==LABEL);
		if (!equop(p1, p2) || p1==p2)
			return(nxj);
		p3 = insertl(p2);
		p1->combop = JBR;
		p1->ref = p3;
		p1->labno = p3->labno;
		p1->code = 0;
		nxj++;
		nxjump++;
		nchange++;
	}
}

insertl(ap)
struct node *ap;
{
	register struct node *lp, *op;

	op = ap;
	if (op->op == LABEL) {
		op->refc++;
		return(op);
	}
	if (op->back->op == LABEL) {
		op = op->back;
		op->refc++;
		return(op);
	}
	lp = alloc(sizeof first);
	lp->combop = LABEL;
	lp->labno = isn++;
	lp->ref = 0;
	lp->code = 0;
	lp->refc = 1;
	lp->back = op->back;
	lp->forw = op;
	op->back->forw = lp;
	op->back = lp;
	return(lp);
}

codemove(ap)
struct node *ap;
{
	register struct node *p1, *p2, *p3;
	struct node *t, *tl;
	int n;

	p1 = ap;
	if (p1->op!=JBR || (p2 = p1->ref)==0)
		return(p1);
	while (p2->op == LABEL)
		if ((p2 = p2->back) == 0)
			return(p1);
	if (p2->op!=JBR && p2->op!=JMP)
		goto ivloop;
	p2 = p2->forw;
	p3 = p1->ref;
	while (p3) {
		if (p3->op==JBR || p3->op==JMP) {
			if (p1==p3)
				return(p1);
			ncmot++;
			nchange++;
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
ivloop:
	if (p1->forw->op!=LABEL)
		return(p1);
	p3 = p2 = p2->forw;
	n = 16;
	do {
		if ((p3 = p3->forw) == 0 || p3==p1 || --n==0)
			return(p1);
	} while (p3->op!=CBR || p3->labno!=p1->forw->labno);
	do 
		if ((p1 = p1->back) == 0)
			return(ap);
	while (p1!=p3);
	p1 = ap;
	tl = insertl(p1);
	p3->subop = revbr[p3->subop];
	decref(p3->ref);
	p2->back->forw = p1;
	p3->forw->back = p1;
	p1->back->forw = p2;
	p1->forw->back = p3;
	t = p1->back;
	p1->back = p2->back;
	p2->back = t;
	t = p1->forw;
	p1->forw = p3->forw;
	p3->forw = t;
	p2 = insertl(p1->forw);
	p3->labno = p2->labno;
	p3->ref = p2;
	decref(tl);
	if (tl->refc<=0)
		nrlab--;
	loopiv++;
	nchange++;
	return(p3);
}

comjump()
{
	register struct node *p1, *p2, *p3;

	for (p1 = first.forw; p1!=0; p1 = p1->forw)
		if (p1->op==JBR && (p2 = p1->ref) && p2->refc > 1)
			for (p3 = p1->forw; p3!=0; p3 = p3->forw)
				if (p3->op==JBR && p3->ref == p2)
					backjmp(p1, p3);
}

backjmp(ap1, ap2)
struct node *ap1, *ap2;
{
	register struct node *p1, *p2, *p3;

	p1 = ap1;
	p2 = ap2;
	for(;;) {
		while ((p1 = p1->back) && p1->op==LABEL);
		p2 = p2->back;
		if (equop(p1, p2)) {
			p3 = insertl(p1);
			p2->back->forw = p2->forw;
			p2->forw->back = p2->back;
			p2 = p2->forw;
			decref(p2->ref);
			p2->labno = p3->labno;
			p2->ref = p3;
			nchange++;
			ncomj++;
		} else
			return;
	}
}
