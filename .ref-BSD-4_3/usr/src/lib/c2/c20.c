#ifndef lint
static	char sccsid[] = "@(#)c20.c	4.10 (Berkeley) 8/22/85";
#endif

/*
 *	 C object code improver
 */

#include "c2.h"
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>

char *malloc();
char firstr[sizeof (char *)];
char *currentb;
char *newa;
char *lasta;
char *lastr;
int ncore;

int ioflag;
int fflag;

long	isn	= 2000000;

struct optab *oplook();
struct optab *getline();

long lgensym[10] =
  {100000L,200000L,300000L,400000L,500000L,600000L,700000L,800000L,900000L,1000000L};

#define ALLOCSIZE	4096

/*
 * Cheapo space allocator.  Works much like the old one but uses malloc()
 * and doesn't clash with stdio.  Assumes that no one requests more than
 * ALLOCSIZE bytes at a time.
 */
char *
xalloc(n)
int n;
{
	register char *nextb = * (char **) currentb;

	if (n == 0) {	/* Free everything */
		currentb = firstr;
		nextb = * (char **) currentb;
	}
	if (nextb == NULL) {
		if ((nextb = malloc(ALLOCSIZE)) == NULL) {
			fprintf(stderr, "Optimizer: out of space\n");
			exit(1);
		}
		ncore += (ALLOCSIZE/1024);
		* (char **) currentb = nextb;
		* (char **) nextb = NULL;
	}
	lasta = nextb + sizeof nextb;
	lastr = nextb + ALLOCSIZE;
	currentb = nextb;
	newa = lasta;
	lasta += XALIGN(n);
	return(newa);
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
			if ((*argv)[1]=='i') ioflag++;
			else if ((*argv)[1]=='f') fflag++;
			else nflag++;
		} else if (infound==0) {
			if (freopen(*argv, "r", stdin) ==NULL) {
				fprintf(stderr,"C2: can't find %s\n", *argv);
				exit(1);
			}
			++infound;
		} else if (freopen(*argv, "w", stdout) ==NULL) {
			fprintf(stderr,"C2: can't create %s\n", *argv);
			exit(1);
		}
		argc--; argv++;
	}
	opsetup();
	maxiter = 0;
	do {
		(void) xalloc(0);
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
		fprintf(stderr,"%dK core\n", ncore);
	}
	putc('\n',stdout);
	fflush(stdout); exit(0);
}

input()
{
	register struct node *p, *lastp;
	struct optab *opp; register char *cp1;
	static struct optab F77JSW = {".long", T(JSW,1)};

	lastp = &first;
	for (;;) {
	  top:
		opp = getline();
		if (debug && opp==0) fprintf(stderr,"? %s\n",line);
		switch (opp->opcode&0377) {
	
		case LABEL:
			p = alloc(sizeof first);
			if (isdigit(line[0]) && (p->labno=locdef(line)) ||
			  (line[0] == 'L') && (p->labno=getnum(line+1))) {
				p->combop = LABEL;
				if (p->labno<100000L && isn<=p->labno) isn=1+p->labno;
				p->code = 0;
			} else {
				p->combop = DLABEL;
				p->labno = 0;
				p->code = copy(line);
			}
			break;
	
		case LGEN:
			if (*curlp!='L' && !locuse(curlp)) goto std;
			opp= &F77JSW;
		case JBR:
			if (opp->opcode==T(JBR,RET) || opp->opcode==T(JBR,RSB)) goto std;
		case CBR:
		case JMP:
		case JSW:
		case SOBGEQ: case SOBGTR: case AOBLEQ: case AOBLSS: case ACB:
			p = alloc(sizeof first);
			p->combop = opp->opcode; p->code=0; cp1=curlp;
			if ((!isdigit(*cp1) || 0==(p->labno=locuse(cp1))) &&
			  (*cp1!='L' || 0==(p->labno = getnum(cp1+1)))) {/* jbs, etc.? */
				while (*cp1++); while (*--cp1!=',' && cp1!=curlp);
				if (cp1==curlp ||
				  (!isdigit(*++cp1) || 0==(p->labno=locuse(cp1))) &&
				  (*cp1!='L' || 0==(p->labno=getnum(cp1+1))))
					p->labno = 0;
				else *--cp1=0;
				p->code = copy(curlp);
			}
			if (isn<=p->labno) isn=1+p->labno;
			break;

		case MOVA:
			p=alloc(sizeof first);
			p->combop=opp->opcode; p->code=0; cp1=curlp+1;
			if (cp1[-1]=='L' || isdigit(cp1[-1])) {
				while (*cp1++!=','); *--cp1=0;
				if (0!=(p->labno=locuse(curlp)) ||
					0!=(p->labno=getnum(curlp+1))) p->code=copy(cp1+1);
				else {*cp1=','; p->code=copy(curlp);}
			} else {p->code=copy(--cp1); p->labno=0;}
			break;

		case SET:
		case COMM:
		case LCOMM:
			printf("%s\n",line); goto top;

		case BSS:
		case DATA:
			for (;;) {
				printf("%s%c",line,(opp->opcode==LABEL ? ':' : '\n'));
				if (opp->opcode==TEXT) goto top;
				if (END==(opp=getline())->opcode) {/* dangling .data is bad for you */
					printf(".text\n");
					break;
				}
			}

		std:
		default:
			p = alloc(sizeof first);
			p->combop = opp->opcode;
			p->labno = 0;
			p->code = copy(curlp);
			break;

		}
		p->forw = 0;
		p->back = lastp;
		p->pop = opp;
		lastp->forw = p;
		lastp = p;
		p->ref = 0;
		if (p->op==CASE) {
			char *lp; int ncase;
			lp=curlp; while (*lp++); while (*--lp!='$'); ncase=getnum(lp+1);
			if (LABEL!=(getline())->opcode) {
				fprintf(stderr, "c2: garbled 'case' instruction\n");
				exit(-2);
			}
			do {
				if (WGEN!=(getline())->opcode) {
					fprintf(stderr, "c2: garbled 'case' instruction\n");
					exit(-3);
				}
				p = alloc(sizeof first); p->combop = JSW; p->code = 0;
				lp=curlp; while(*lp++!='-'); *--lp=0; p->labno=getnum(curlp+1);
				if (isn<=p->labno) isn=1+p->labno;
				p->forw = 0; p->back = lastp; lastp->forw = p; lastp = p;
				p->ref = 0; p->pop=0;
			} while (--ncase>=0);
		}
		if (opp->opcode==EROU)
			return(1);
		if (opp->opcode==END)
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

locuse(p)
register char *p;
{
	if (!isdigit(p[0]) || p[1] != 'f' && p[1] != 'b' || p[2]) return(0);
	return (lgensym[p[0] - '0'] - (p[1] == 'b'));
}

locdef(p)
register char *p;
{

	if (!isdigit(p[0]) || p[1]) return(0);
	return (lgensym[p[0] - '0']++);
}

output()
{
	register struct node *t;
	int casebas;

	t = &first;
	while (t = t->forw) switch (t->op) {

	case END:
		fflush(stdout);
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
	case MOV:
		if (!fflag) goto std;
		if (t->forw) if(t->forw->op == CBR) goto std;
		if (*t->code == '$') goto std;
		if (t->subop == FFLOAT)
			{
			printf("movl\t%s\n", t->code);
			continue;
			}
		if (t->subop == DFLOAT || t->subop == GFLOAT)
			{
			printf("movq\t%s\n", t->code);
			continue;
			}
		if (t->subop == HFLOAT)
			{
			printf("movo\t%s\n", t->code);
			continue;
			}
		goto std;

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
	onp = np = (char *) alloc(n);
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

	for(i=NREG+5;--i>=0;) regs[i] = malloc(C2_ASIZE);
	for (optp = optab; optp->opstring[0]; optp++) {
		t=7; i=0; while (--t>=0) i+= i+optp->opstring[t];
		ophp = &ophash[i % OPHS];
		while (*ophp++) {
/*			fprintf(stderr,"\ncollision: %d %s %s",
/*				ophp-1-ophash,optp->opstring,(*(ophp-1))->opstring);
*/
			if (ophp >= &ophash[OPHS])
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
	register struct node *p, *lp, *tp;
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
		if (p->combop==JBR || p->op==CBR || p->op==JSW || p->op==JMP
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
				tp = nonlab(lp)->back;
				if (tp!=lp) {
					p->labno = tp->labno;
					lp = tp;
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
#ifndef COPYCODE
		if (p->op==CBR && (p1 = p->forw)->combop==JBR) {/* combop: RET problems */
#else
		if (p->op==CBR && (p1 = p->forw)->combop==JBR &&
		    p->ref) {/* combop: RET problems */
#endif
			rp = p->ref;
			do
				rp = rp->back;
			while (rp->op==LABEL);
			if (rp==p1) {
				decref(p->ref);
				p->ref = p1->ref;
				p->labno = p1->labno;
#ifdef COPYCODE
				if (p->labno == 0)
					p->code = p1->code;
#endif
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

	if ((p2 = p1->ref)==0)
		return;
	for (;;) {
		while ((p1 = p1->back) && p1->op==LABEL);
		while ((p2 = p2->back) && p2->op==LABEL);
		if (!equop(p1, p2) || p1==p2)
			return;
		p3 = insertl(p2);
		p1->combop = JBR;
		p1->pop=0;
		p1->ref = p3;
		p1->labno = p3->labno;
		p1->code = 0;
		nxjump++;
		nchange++;
	}
}

struct node *
insertl(np)
register struct node *np;
{
	register struct node *lp;

	if (np->op == LABEL) {
		np->refc++;
		return(np);
	}
	if (np->back->op == LABEL) {
		np = np->back;
		np->refc++;
		return(np);
	}
	lp = alloc(sizeof first);
	lp->combop = LABEL;
	lp->labno = isn++;
	lp->ref = 0;
	lp->code = 0;
	lp->refc = 1;
	lp->back = np->back;
	lp->forw = np;
	np->back->forw = lp;
	np->back = lp;
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
/* last clause to avoid infinite loop on partial compiler droppings:
	L183:	jbr L179
	L191:	jbr L179
			casel r0,$0,$1
	L193:	.word L183-L193
			.word L191-L193
	L179:	ret
*/
	if (p1->op!=JBR || (p2 = p1->ref)==0 || p2==p1->forw)
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
#ifdef COPYCODE
	if (p3->labno == 0)
		p3->code = p2->code;
#endif
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
		if (p1->op==JBR && ((p2 = p1->ref) && p2->refc > 1
				|| p1->subop==RET || p1->subop==RSB))
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
#ifdef COPYCODE
			p2->code = 0;
#endif
			p2->ref = p3;
			nchange++;
			ncomj++;
		} else
			return;
	}
}
