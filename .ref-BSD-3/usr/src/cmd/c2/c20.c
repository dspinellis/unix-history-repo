#
char C20[] = {"@(#)c20.c 1.27 78/10/23 14:06:38"}; /* sccs ident */
/*
 *	 C object code improver
 */

#include <stdio.h>
#include "c2.h"
#include <ctype.h>

char _sibuf[BUFSIZ], _sobuf[BUFSIZ];
int ioflag;
int	isn	= 20000;
struct optab *oplook();
struct optab *getline();

struct node *
alloc(an)
{
	register int n;
	register char *p;

	n = an;
	n++;
	n &= ~01;
	if (lasta+n >= lastr) {
		if (sbrk(2000) == -1) {
			fprintf(stderr, "Optimizer: out of space\n");
			exit(1);
		}
		lastr += 2000;
	}
	p = lasta;
	lasta += n;
	return(p);
}

main(argc, argv)
char **argv;
{
	register int niter, maxiter, isend;
	int nflag,infound;

	nflag = 0; infound=0; argc--; argv++;
	while (argc>0) {/* get flags */
		if (**argv=='+') debug++;
		else if (**argv=='-') {
			if ((*argv)[1]=='i') ioflag++; else nflag++;
		} else if (infound==0) {
			if (freopen(*argv, "r", stdin) ==NULL) {
				fprintf(stderr,"C2: can't find %s\n", *argv);
				exit(1);
			}
			setbuf(stdin,_sibuf); ++infound;
		} else if (freopen(*argv, "w", stdout) ==NULL) {
			fprintf(stderr,"C2: can't create %s\n", *argv);
			exit(1);
		}
		setbuf(stdout,_sobuf);
		argc--; argv++;
	}
	lasta = lastr = sbrk(2);
	opsetup();
	lasta = firstr = lastr = alloc(0);
	maxiter = 0;
	do {
		isend = input();
		niter = 0;
		bmove();
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
	if (nflag) {
		fprintf(stderr,"%d iterations\n", maxiter);
		fprintf(stderr,"%d jumps to jumps\n", nbrbr);
		fprintf(stderr,"%d inst. after jumps\n", iaftbr);
		fprintf(stderr,"%d jumps to .+1\n", njp1);
		fprintf(stderr,"%d redundant labels\n", nrlab);
		fprintf(stderr,"%d cross-jumps\n", nxjump);
		fprintf(stderr,"%d code motions\n", ncmot);
		fprintf(stderr,"%d branches reversed\n", nrevbr);
		fprintf(stderr,"%d redundant moves\n", redunm);
		fprintf(stderr,"%d simplified addresses\n", nsaddr);
		fprintf(stderr,"%d loops inverted\n", loopiv);
		fprintf(stderr,"%d redundant jumps\n", nredunj);
		fprintf(stderr,"%d common seqs before jmp's\n", ncomj);
		fprintf(stderr,"%d skips over jumps\n", nskip);
		fprintf(stderr,"%d sob's added\n", nsob);
		fprintf(stderr,"%d redundant tst's\n", nrtst);
		fprintf(stderr,"%d jump on bit\n", nbj);
		fprintf(stderr,"%d field operations\n", nfield);
		fprintf(stderr,"%dK core\n", ((unsigned)lastr+01777) >> 10);
	}
	fflush(stdout); exit(0);
}

input()
{
	register struct node *p, *lastp;
	struct optab *op; register char *cp1;
	static struct optab F77JSW = {".long", T(JSW,1)};

	lastp = &first;
	for (;;) {
	  top:
		op = getline();
		if (debug && op==0) fprintf(stderr,"? %s\n",line);
		switch (op->opcode&0377) {
	
		case LABEL:
			p = alloc(sizeof first);
			if (line[0] == 'L') {
				p->combop = LABEL;
				p->labno = getnum(line+1);
				if (isn<=p->labno) isn=1+p->labno;
				p->code = 0;
			} else {
				p->combop = DLABEL;
				p->labno = 0;
				p->code = copy(line);
			}
			break;
	
		case LGEN:
			if (*curlp!='L') goto std;
			op= &F77JSW;
		case JBR:
			if (op->opcode==T(JBR,RET)) goto std;
		case CBR:
		case JMP:
		case JSW:
		case SOBGEQ: case SOBGTR: case AOBLEQ: case AOBLSS: case ACB:
			p = alloc(sizeof first);
			p->combop = op->opcode; p->code=0; cp1=curlp;
			if (*cp1!='L' || 0==(p->labno = getnum(cp1+1))) {/* jbs, etc.? */
				while (*cp1++); while (*--cp1!=',' && cp1!=curlp);
				if (cp1==curlp || *++cp1!='L' || 0==(p->labno=getnum(cp1+1)))
					p->labno = 0;
				else *--cp1=0;
				p->code = copy(curlp);
			}
			if (isn<=p->labno) isn=1+p->labno;
			break;

		case MOVA:
			p=alloc(sizeof first);
			p->combop=op->opcode; p->code=0; cp1=curlp;
			if (*cp1++=='L') {
				while (*cp1++!=','); *--cp1=0;
				if (0!=(p->labno=getnum(curlp+1))) p->code=copy(cp1+1);
				else {*cp1=','; p->code=copy(curlp);}
/* new */		} else {p->code=copy(--cp1); p->labno=0;}
			break;

		case SET:
			printf("%s\n",line); goto top;

		case BSS:
		case DATA:
			for (;;) {
				printf("%s%c",line,(op->opcode==LABEL ? ':' : '\n'));
				if (op->opcode==TEXT) goto top;
				if (END==(op=getline())->opcode) {/* dangling .data is bad for you */
					printf(".text\n");
					break;
				}
			}

		std:
		default:
			p = alloc(sizeof first);
			p->combop = op->opcode;
			p->labno = 0;
			p->code = copy(curlp);
			break;

		}
		p->forw = 0;
		p->back = lastp;
		p->pop = op;
		lastp->forw = p;
		lastp = p;
		p->ref = 0;
		if (p->op==CASE) {
			char *lp; int ncase;
			lp=curlp; while (*lp++); while (*--lp!='$'); ncase=getnum(lp+1);
			if (LABEL!=(getline())->opcode) abort(-2);
			do {
				if (WGEN!=(getline())->opcode) abort(-3);
				p = alloc(sizeof first); p->combop = JSW; p->code = 0;
				lp=curlp; while(*lp++!='-'); *--lp=0; p->labno=getnum(curlp+1);
				if (isn<=p->labno) isn=1+p->labno;
				p->forw = 0; p->back = lastp; lastp->forw = p; lastp = p;
				p->ref = 0; p->pop=0;
			} while (--ncase>=0);
		}
		if (op->opcode==EROU)
			return(1);
		if (op->opcode==END)
			return(0);
	}
}

struct optab *
getline()
{
	register char *lp;
	register c;
	static struct optab OPLABEL={"",LABEL};
	static struct optab OPEND={"",END};

	lp = line;
	while (EOF!=(c=getchar()) && isspace(c));
	while (EOF!=c) {
		if (c==':') {
			*lp++ = 0;
			return(&OPLABEL);
		}
		if (c=='\n') {
			*lp++ = 0;
			return(oplook());
		}
		*lp++ = c;
		c = getchar();
	}
	*lp++ = 0;
	return(&OPEND);
}

long
getnum(p)
register char *p;
{
	register c; int neg; register long n;

	n = 0; neg=0; if (*p=='-') {++neg; ++p;}
	while (isdigit(c = *p++)) {
		 c -= '0'; n *= 10; if (neg) n -= c; else n += c;
	}
	if (*--p != 0)
		return(0);
	return(n);
}

output()
{
	register struct node *t;
	int casebas;

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

	case CASE:
		casebas=0;

	default: std:
		if (t->pop==0) {/* must find it */
			register struct optab *p;
			for (p=optab; p->opstring[0]; ++p)
				if (p->opcode==t->combop) {t->pop=p; break;}
		}
		printf("%s", t->pop->opstring);
		if (t->code) printf("\t%s", t->code);
		if (t->labno!=0) printf("%cL%d\n",
							(t->code ? ',' : '\t'),
							t->labno);
		else printf("\n");
		continue;

	case MOVA:
		if (t->labno==0) goto std;
		printf("mova%c\tL%d,%s\n","bwlq"[t->subop-BYTE],t->labno,t->code);
		continue;

	case JSW:
		if (t->subop!=0) {/* F77JSW */
			printf(".long\tL%d\n",t->labno); continue;
		}
		if (casebas==0) printf("L%d:\n",casebas=isn++);
		printf(".word	L%d-L%d\n", t->labno, casebas);
		continue;

	}
}

char *
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

#define	OPHS	560
struct optab *ophash[OPHS];

opsetup()
{
	register struct optab *optp, **ophp;
	register int i,t;

	for(i=NREG+5;--i>=0;) regs[i]=alloc(20);
	for (optp = optab; optp->opstring[0]; optp++) {
		t=7; i=0; while (--t>=0) i+= i+optp->opstring[t];
		ophp = &ophash[i % OPHS];
		while (*ophp++) {
/*			fprintf(stderr,"\ncollision: %d %s %s",
/*				ophp-1-ophash,optp->opstring,(*(ophp-1))->opstring);
*/
			if (ophp > &ophash[OPHS])
				ophp = ophash;
		}
		*--ophp = optp;
	}
}

struct optab *
oplook()
{
	register struct optab *optp,**ophp;
	register char *p,*p2;
	register int t;
	char tempop[20];
	static struct optab OPNULL={"",0};

	for (p=line, p2=tempop; *p && !isspace(*p); *p2++= *p++); *p2=0; p2=p;
	while (isspace(*p2)) ++p2; curlp=p2;
	t=0; while(--p>=line) t += t+*p; ophp = &ophash[t % OPHS];
	while (optp = *ophp) {
		if (equstr(tempop,optp->opstring)) return(optp);
		if ((++ophp) >= &ophash[OPHS]) ophp = ophash;
	}
	curlp = line;
	return(&OPNULL);
}

refcount()
{
	register struct node *p, *lp;
	struct node *labhash[LABHS];
	register struct node **hp;

	for (hp = labhash; hp < &labhash[LABHS];)
		*hp++ = 0;
	for (p = first.forw; p!=0; p = p->forw)
		if (p->op==LABEL) {
			labhash[p->labno % LABHS] = p;
			p->refc = 0;
		}
	for (p = first.forw; p!=0; p = p->forw) {
		if (p->op==JBR || p->op==CBR || p->op==JSW || p->op==JMP
		  || p->op==SOBGEQ || p->op==SOBGTR || p->op==AOBLEQ || p->op==AOBLSS
		  || p->op==ACB || (p->op==MOVA && p->labno!=0)) {
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
			if (rp->op==JBR && rp->labno && p->labno!=rp->labno) {
				nbrbr++;
				p->labno = rp->labno;
				decref(p->ref);
				rp->ref->refc++;
				p->ref = rp->ref;
				nchange++;
			}
		}
		if (p->op==CBR && (p1 = p->forw)->combop==JBR) {/* combop: RET problems */
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
				p->pop=0;
				nchange++;
				nskip++;
			}
		}
		if (p->op==JBR || p->op==JMP) {
			while (p->forw && p->forw->op!=LABEL && p->forw->op!=DLABEL
				&& p->forw->op!=EROU && p->forw->op!=END
				&& p->forw->op!=ALIGN
				&& p->forw->op!=0 && p->forw->op!=DATA) {
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

xjump(p1)
register struct node *p1;
{
	register struct node *p2, *p3;
	int nxj;

	nxj = 0;
	if ((p2 = p1->ref)==0)
		return(0);
	for (;;) {
		while ((p1 = p1->back) && p1->op==LABEL);
		while ((p2 = p2->back) && p2->op==LABEL);
		if (!equop(p1, p2) || p1==p2)
			return(nxj);
		p3 = insertl(p2);
		p1->combop = JBR;
		p1->pop=0;
		p1->ref = p3;
		p1->labno = p3->labno;
		p1->code = 0;
		nxj++;
		nxjump++;
		nchange++;
	}
}

struct node *
insertl(op)
register struct node *op;
{
	register struct node *lp;

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

struct node *
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
	p3->pop=0;
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
		if (p1->op==JBR && ((p2 = p1->ref) && p2->refc > 1 || p1->subop==RET))
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
			p2->combop = JBR; /* to handle RET */
			p2->pop=0;
			p2->labno = p3->labno;
			p2->ref = p3;
			nchange++;
			ncomj++;
		} else
			return;
	}
}
