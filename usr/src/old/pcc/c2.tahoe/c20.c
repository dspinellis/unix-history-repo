#ifndef lint
static char sccsid[] = "@(#)c20.c	1.5 (Berkeley/CCI) %G%";
#endif

/*
 *	 C object code improver
 */

#include "c2.h"
#include <stdio.h>
#include <ctype.h>

char _sibuf[BUFSIZ], _sobuf[BUFSIZ];
int ioflag;
int isn	= 2000000;
struct optab *oplook();
struct optab *getline();
long lgensym[10] =
  {100000L,200000L,300000L,400000L,500000L,600000L,700000L,800000L,900000L,1000000L};

/* VARARGS1 */
error(s1, s2)
	char *s1, *s2;
{
	fprintf(stderr, s1, s2);
	exit(1);
}

struct node *
alloc(an)
{
	register int n;
	register char *p;

	n = an;
	n+=sizeof(char *)-1;
	n &= ~(sizeof(char *)-1);
	if (lasta+n >= lastr) {
		if (sbrk(2000) == -1)
			error("Optimizer: out of space\n");
		lastr += 2000;
	}
	p = lasta;
	lasta += n;
	return((struct node *)p);
}

main(argc, argv)
char **argv;
{
	register int niter, maxiter, isend;
	int nflag,infound;

	nflag = 0; infound=0; argc--; argv++;
	while (argc>0) {/* get flags */
		if (**argv=='-') {
			switch ((*argv)[1]) {
			case 'a': aobflag++; break;
			case 'n': nflag++; break;
			case 'd': debug++; break;
			case 'f': fortflg++; break;
			}
		} else if (infound==0) {
			if (freopen(*argv, "r", stdin) ==NULL)
				error("C2: can't find %s\n", *argv);
			setbuf(stdin,_sibuf); ++infound;
		} else if (freopen(*argv, "w", stdout) ==NULL)
			error("C2: can't create %s\n", *argv);
		setbuf(stdout,_sobuf);
		argc--; argv++;
	}
	lasta = lastr = (char *)sbrk(2);
	opsetup();
	lasta = firstr = lastr = (char *)alloc(0);
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
		addaob();
		interleave();
		output();
		if (niter > maxiter)
			maxiter = niter;
		lasta = firstr;
	} while (isend);
	if (nflag) {
		score("iterations", maxiter);
		score("jumps to jumps", nbrbr);
		score("inst. after jumps", iaftbr);
		score("jumps to .+1", njp1);
		score("redundant labels", nrlab);
		score("cross-jumps", nxjump);
		score("code motions", ncmot);
		score("branches reversed", nrevbr);
		score("redundant moves", redunm);
		score("simplified addresses", nsaddr);
		score("loops inverted", loopiv);
		score("redundant jumps", nredunj);
		score("common seqs before jmp's", ncomj);
		score("skips over jumps", nskip);
		score("aob's added", naob);
		score("redundant tst's", nrtst);
		score("jump on bit", nbj);
		score("redundant accumulator stores", nst);
		score("redundant accumulator loads", nld);
		score("K core", ((unsigned)lastr+01777) >> 10);
	}
	putc('\n',stdout);
	fflush(stdout); exit(0);
}

score(s, n)
	char *s;
{
	if(n > 0)
		fprintf(stderr, "%d %s\n", n, s);
}

input()
{
	register struct node *p, *lastp;
	register struct optab *op; register char *cp1;
	static struct optab F77JSW = {".long", JSW, 1};

	lastp = &first;
	for (;;) {
	  top:
		op = getline();
		if (debug && op==0) fprintf(stderr,"? %s\n",line);
		switch (op->opcod) {

		case LABEL:
			p = alloc(sizeof first);
			if (isdigit(line[0]) && (p->labno=locdef(line)) ||
			  (line[0] == 'L') && (p->labno=getnum(line+1))) {
				p->op = LABEL; p->subop = 0;
				if (p->labno<100000L && isn<=p->labno) isn=1+p->labno;
				p->code = 0;
			} else {
				p->op = DLABEL; p->subop = 0;
				p->labno = 0;
				p->code = copy(line);
			}
			break;
	
		case LGEN:
			if (*curlp!='L' && !locuse(curlp)) goto std;
			op= &F77JSW;
		case JBR:
			if (op->opcod==JBR && (op->subopcod&0xF)==RET) goto std;
		case CBR:
		case JMP:
		case JSW:
		case AOBLEQ: case AOBLSS:
			p = alloc(sizeof first);
			p->op = op->opcod; p->subop = op->subopcod;
			p->code=0; cp1=curlp;
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
			p->op = op->opcod; p->subop = op->subopcod;
			p->code=0; cp1=curlp+1;
			if (cp1[-1]=='L' || isdigit(cp1[-1])) {
				while (*cp1++!=','); *--cp1=0;
				if (0!=(p->labno=locuse(curlp)) ||
					0!=(p->labno=getnum(curlp+1))) p->code=copy(cp1+1);
				else {*cp1=','; p->code=copy(curlp);}
			} else {p->code=copy(--cp1); p->labno=0;}
			break;

		case MOV:
			p=alloc(sizeof first);
			p->op = op->opcod; p->subop = op->subopcod;
			p->code = copy(curlp);
			p->labno = 0;
			cp1=curlp+1;
			if ((cp1[-1]=='$') && (cp1[0] == 'L')) {
				while (*cp1++!=','); *--cp1 =0; 
				p->labno = getnum(curlp+2);
				}
			break;
		case MOVBLK:	/* used implicitly */
			curlp = "(r0),(r1),(r2)";
			goto std;

		case SET:
		case COMM:
		case LCOMM:
			printf("%s\n",line); goto top;

		case BSS:
		case DATA:
			for (;;) {
				printf("%s%c",line,(op->opcod==LABEL ? ':' : '\n'));
				if (op->opcod==TEXT) goto top;
				if (END==(op=getline())->opcod) {/* dangling .data is bad for you */
					printf(".text\n");
					break;
				}
			}

		std:
		default:
			p = alloc(sizeof first);
			p->op = op->opcod; p->subop = op->subopcod;
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
			if (ALIGN!=(getline())->opcod || LABEL!=(getline())->opcod) caserr();
			do {
				if (WGEN!=(getline())->opcod) caserr();
				p = alloc(sizeof first);
				p->op = JSW; p->subop = 0;
				p->code = 0;
				lp=curlp; while(*lp++!='-'); *--lp=0; p->labno=getnum(curlp+1);
				if (isn<=p->labno) isn=1+p->labno;
				p->forw = 0; p->back = lastp; lastp->forw = p; lastp = p;
				p->ref = 0; p->pop=0;
			} while (--ncase>=0);
		}
		if (op->opcod==EROU)
			return(1);
		if (op->opcod==END)
			return(0);
	}
}

caserr()
{
	error("C2: improper casel instruction\n");
}

struct optab *
getline()
{
	register char *lp;
	register int c;
	static struct optab OPLABEL={"",LABEL,0};
	static struct optab OPEND={"",END,0};

	lp = line;
again:
	while (EOF!=(c=getchar()) && isspace(c));
	if (c=='#') {
		while((c=getchar()) != '\n');
		goto again;
	}
	while (EOF!=c) {
		if (c==':') {
			*lp++ = 0;
			return(&OPLABEL);
		}
		if (c=='\n') {
			*lp++ = 0;
			return(oplook());
		}
		if (c=='"')
			do {
				if (c=='\\') {
					*lp++ = c;
					c = getchar();
				}
				*lp++ = c;
				c = getchar();
			} while(c!='"');
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
	register int c, neg, n;

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
				if (p->opcod==t->op && p->subopcod==t->subop) {
					t->pop=p; break;}
		}
		printf("%s", t->pop->opstring);
		if (t->code) printf("\t%s", t->code);
		if (t->op != MOV ) {
		if (t->labno!=0) printf("%cL%d\n",
							(t->code ? ',' : '\t'),
							t->labno);

		else printf("\n");
		}
		else printf("\n");
		continue;

	case MOVA:
		if (t->labno==0) goto std;
		printf("mova%c\tL%d,%s\n","bwlq"[t->subop-BYTE],t->labno,t->code);
		continue;

	case MOVBLK:
		t->code = 0;
		goto std;

	case JSW:
		if (t->subop!=0) {/* F77JSW */
			printf(".long\tL%d\n",t->labno); continue;
		}
		if (casebas==0) printf(".align 1\nL%d:\n",casebas=isn++);
		printf(".word	L%d-L%d\n", t->labno, casebas);
		continue;

	}
}

char *
copy(ap)
char *ap;
{
	register char *p, *np, *onp;
	register int n;

	p = ap;
	n = 0;
	if (*p==0)
		return(0);
	do
		n++;
	while (*p++);
	onp = np = (char *)alloc(n);
	p = ap;
	while (*np++ = *p++);
	return(onp);
}

#define	OPHS	560
struct optab *ophash[OPHS];

opsetup()
{
	register struct optab *optp, **ophp;
	register int i,t;

	for(i=RT1+5;--i>=0;) regs[i]=(char *)alloc(C2_ASIZE);
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
	static struct optab OPNULL={"",NIL,0};

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
	register struct node *p, *lp, *np;
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
		if (p->op==JBR && p->subop==0 || p->op==CBR || p->op==JSW || p->op==JMP
		  || p->op==AOBLEQ || p->op==AOBLSS
		  || (p->op==MOVA && p->labno!=0) 
	          || (p->op==MOV && p->labno!=0)){
			p->ref = 0;
			lp = labhash[p->labno % LABHS];
			if (lp==0 || p->labno!=lp->labno)
			for (lp = first.forw; lp!=0; lp = lp->forw) {
				if (lp->op==LABEL && p->labno==lp->labno)
					break;
			}
			if (lp) {
				np = nonlab(lp)->back;
				if (np!=lp) {
					p->labno = np->labno;
					lp = np;
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
		if (p->op==CBR && (p1 = p->forw)->op==JBR && p1->subop==0
#ifdef COPYCODE
		 && p->ref
#endif
		    ) {/* RET problems */
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
			while ((p1=p->forw)!=0 && p1->op!=LABEL && p1->op!=DLABEL
				&& p1->op!=EROU && p1->op!=END
				&& p1->op!=ALIGN
				&& p1->op!=NIL && p1->op!=DATA) {
				nchange++;
				iaftbr++;
				if (p1->ref)
					decref(p1->ref);
				p->forw = p1->forw;
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
		p1->op = JBR; p1->subop = 0;
		p1->pop=0;
		p1->ref = p3;
		p1->labno = p3->labno;
		p1->code = 0;
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
	lp->op = LABEL; lp->subop = 0;
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
	register struct node *t, *tl;
	register int n;

	p1 = ap;
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
				|| (p1->subop&0xF)==RET))
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
			p2->op = JBR; p2->subop = 0; /* to handle RET */
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
