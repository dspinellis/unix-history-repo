#ifndef lint
static char sccsid[] = "%W% (Berkeley/CCI) %G%";
#endif

/*
 * C object code improver-- third part
 */

#include "c2.h"
#include <stdio.h>
#include <ctype.h>

rmove()
{
	register struct node *p;
	register int r, r1;

	clearreg();
	for (p=first.forw; p!=0; p = p->forw) {
	if (debug) {
		printf("Regs: ");
		for (r=0; r<=NREG; r++)
			if (regs[r][0]) {
				r1=regs[r][0];
				printf("%d: %d%d %s\n", r, r1&0xF, r1>>4, regs[r]+1);
			}
		printf("-\n");
	}
	switch (p->op) {

	case CVT:
	case MOVZ:
		splitrand(p);
		repladdr(p);
		r = isreg(regs[RT1]);
		r1 = isreg(regs[RT2]);
		dest(regs[RT2],p->subop, 1);
		if (r>=0 && r1>=0) {
			p->op = MOV; p->subop = LONG;
			p->pop = 0;
			nchange++;
			goto case_mov;
		}
		if(p->op == CVT) {
			if (r1>=0) savereg(r1, regs[RT1], p->subop);
		} else
			ccloc[0] = 0;
		break;

	case MOV:
	case_mov:
		splitrand(p);
		if ((r = findrand(regs[RT1],p->subop)) >= 0) {
			if (r == isreg(regs[RT2]))
				if(p->forw->op!=CBR) {
					delnode(p); redunm++; nchange++; break;
				} else {
					p->op=TST; p->pop=0;
					while(*p->code++ != ',');
					redunm++; nchange++;
					goto case_tst;
				}
		}
		repladdr(p);
		r = isreg(regs[RT1]);
		r1 = isreg(regs[RT2]);
		dest(regs[RT2],p->subop, 1);
		if ((regs[ACC][0]) && equstr(regs[RT2],regs[ACC]+1))
			*(short *)(regs[ACC]) = 0;
		if (r>=0) {
			if (r1>=0) {
				if (r == r1 && p->forw->op!=CBR) {
					delnode(p); redunm++; nchange++;
					break;
				}
				if(regs[r][0])
					savereg(r1, regs[r]+1, p->subop);
			} else
				savereg(r, regs[RT2], p->subop);
		} else if (r1>=0)
			savereg(r1, regs[RT1], p->subop);
		else
			setcon(regs[RT1], regs[RT2], p->subop);
		break;

/* .rx,.wx or .rx,.rx,.wx */
	case ADD:
	case SUB:
	case AND:
	case OR:
	case XOR:
	case MUL:
	case DIV:
#ifdef EMOD
	case EDIV:
	case EMOD:
#endif EMOD
	case SHAL:
	case SHAR:
	case SHL:
	case SHR:
	case ADDA:
	case SUBA:
/* .rx,.wx */
	case MFPR:
	case COM:
	case NEG:
		splitrand(p);
		repladdr(p);
		dest(lastrand,p->subop, p->op!=ADDA && p->op!=SUBA);
		break;

/* .mx or .wx */
	case STF:
		if(equstr(p->code, regs[ACC]+1) && p->subop==regs[ACC][0]) {
			delnode(p);
			nst++; nchange++; break;
		}
		savereg(ACC, p->code, p->subop);
	case INC:
	case DEC:
	case CVFL:
		dest(p->code,p->subop, 1);
		break;

	case CLR:
		dest(p->code,p->subop, 1);
		if ((regs[ACC][0]) && equstr(p->code,regs[ACC]+1))
			*(short *)(regs[ACC]) = 0;
		if ((r = isreg(p->code)) < 0)
			setcon("$0", p->code, p->subop);
		else
			savereg(r, "$0", p->subop);
		break;

/* .rx */
	case LDF:
		if(equstr(p->code, regs[ACC]+1) && p->subop==regs[ACC][0]) {
			delnode(p);
			nld++; nchange++; break;
		}
		savereg(ACC, p->code, p->subop);
		goto case_tst;
	case LNF:
		if(equstr(p->code, regs[ACC]+1) && p->subop==regs[ACC][0]) {
			p->op = NEGF; p->pop = 0; p->code = 0;
			regs[ACC][0] = 0;
			break;
		}
	case CVLF:
	case LDFD:
	case ADDF:
	case SUBF:
	case MULF:
	case DIVF:
		regs[ACC][0] = 0;
	case TST:
	case_tst:
	case PUSH:
		splitrand(p);
		lastrand=regs[RT1+1]; /* fool repladdr into doing 1 operand */
		repladdr(p);
		lastrand=regs[RT1];
		if (p->op==TST && equstr(lastrand, ccloc+1)
		  && ((0xf&(ccloc[0]>>4))==p->subop || equtype(ccloc[0],p->subop))) {
			delnode(p); nrtst++; nchange++; break;
		}
		if (p->op==PUSH && p->subop!=LONG &&
		 (isreg(lastrand)>=0 || *lastrand=='$')) {
			p->subop = LONG;
			p->pop = 0;
			nchange++;
		}
		if (p->op==TST || p->op==PUSH)
			setcc(lastrand,p->subop);
		break;

/* .rx,.rx,.rx */
	case PROBE:
	case CASE:
/* .rx,.rx */
	case MTPR:
	case CALLS:
	case CALLF:
	case CMP:
	case BIT:
	case CMPF:
	case CMPF2:
		splitrand(p);
		/* fool repladdr into doing right number of operands */
		lastrand=byondrd(p);
		if (p->op==CALLF || p->op==CALLS) clearreg();
		else repladdr(p);
	case TSTF:
		ccloc[0]=0;
	case PUSHD:
		break;

/* acc only */
	case CVDF:
	case NEGF:
	case SINF:
	case COSF:
	case ATANF:
	case LOGF:
	case SQRTF:
	case EXPF:
		regs[ACC][0] = 0;
		break;

#ifndef EMOD
/* .rx,.rx,.wx,.wx */
	case EDIV:
		splitrand(p);
		lastrand = regs[RT3];
		repladdr(p);
		dest(regs[RT3], p->subop, 1);
		dest(regs[RT4], p->subop, 0);
		break;
#endif EMOD

/* .rx,.rx,.rx,wx */
	case EMUL:
		splitrand(p);
		lastrand = regs[RT4];
		repladdr(p);
		dest(regs[RT4],QUAD, 1); /* fourth operand is a quad */
		break;
	case CBR:
		if (p->subop>=JBC) {
			splitrand(p);
			lastrand=regs[RT3]; /* 2 operands can be optimized */
			repladdr(p);
			ccloc[0] = 0;
		} else
			reduncbr(p);
		break;

	case JBR:
		redunbr(p);

	default:
		clearreg();
	}
	}
}

jumpsw()
{
	register struct node *p, *p1, *pt;
	register int t, nj;

	t = 0;
	nj = 0;
	for (p=first.forw; p!=0; p = p->forw)
		p->seq = ++t;
	for (p=first.forw; p!=0; p = p1) {
		p1 = p->forw;
		if (p->op == CBR && p1->op==JBR && p->ref && p1->ref
		 && abs(p->seq - p->ref->seq) > abs(p1->seq - p1->ref->seq)) {
			if (p->ref==p1->ref)
				continue;
			p->subop = revbr[p->subop];
			p->pop=0;
			pt = p1->ref;
			p1->ref = p->ref;
			p->ref = pt;
			t = p1->labno;
			p1->labno = p->labno;
			p->labno = t;
#ifdef COPYCODE
			if (p->labno == 0) {
				pt = (struct node *)p1->code; p1->code = p->code; p->code = (char *)pt;
			}
#endif
			nrevbr++;
			nj++;
		}
	}
	return(nj);
}

addaob()
{
	register struct node *p, *p1, *p2, *p3;

	for (p = &first; (p1 = p->forw)!=0; p = p1) {
	if (p->op==INC && p->subop==LONG) {
		if (p1->op==LABEL && p1->refc==1 && p1->forw->op==CMP && p1->forw->subop==LONG
		  && (p2=p1->forw->forw)->op==CBR && p2->subop==JLE
		  && (p3=p2->ref->back)->op==JBR && p3->subop==0 && p3->ref==p1
		  && p3->forw->op==LABEL && p3->forw==p2->ref) {
			/* change	INC LAB: CMP	to	LAB: INC CMP */
			p->back->forw=p1; p1->back=p->back;
			p->forw=p1->forw; p1->forw->back=p;
			p->back=p1; p1->forw=p;
			p1=p->forw;
			/* adjust beginning value by 1 */
			p2=alloc(sizeof first); p2->op = DEC; p2->subop = LONG;
			p2->pop=0;
			p2->forw=p3; p2->back=p3->back; p3->back->forw=p2;
			p3->back=p2; p2->code=p->code; p2->labno=0;
		}
		if (p1->op==CMP && p1->subop==LONG &&
		  (p2=p1->forw)->op==CBR && p2->forw->op!=CBR) {
			register char *cp1,*cp2;
			splitrand(p1); if (!equstr(p->code,regs[RT1])) continue;
			if ((p2->subop==JLE || p2->subop==JLT) &&
			    checkaobdisp(p2)){
				if (p2->subop==JLE) p->op = AOBLEQ; else p->op = AOBLSS; p->subop = 0;
				cp2=regs[RT1]; cp1=regs[RT2]; while (*cp2++= *cp1++); /* limit */
				cp2=regs[RT2]; cp1=p->code; while (*cp2++= *cp1++); /* index */
				p->pop=0; newcode(p);
				p->labno = p2->labno; delnode(p2); delnode(p1); naob++;
			}
		}
	}
	}
}

ispow2(n) register long n; {/* -1 -> no; else -> log to base 2 */
	register int log;
	if (n==0 || n&(n-1)) return(-1); log=0;
	for (;;) {n >>= 1; if (n==0) return(log); ++log; if (n== -1) return(log);}
}

equop(p1, p2)
register struct node *p1, *p2;
{
	register char *cp1, *cp2;

	if (p1->op != p2->op || p1->subop != p2->subop)
		return(0);
	if (p1->op != NIL && ord(p1->op) < ord(MOV))
		return(0);
	if (p1->op==MOVA && p1->labno!=p2->labno) return(0);
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

delnode(p) register struct node *p; {
	p->back->forw = p->forw;
	p->forw->back = p->back;
}

decref(p)
register struct node *p;
{
	if (p && --p->refc <= 0) {
		nrlab++; nchange++;
		delnode(p);
	}
}

struct node *
nonlab(ap)
struct node *ap;
{
	register struct node *p;

	p = ap;
	while (p && p->op==LABEL)
		p = p->forw;
	return(p);
}

clearuse() {
	register struct node **i;
	register short *p;
	for (i=uses+NREG; i>uses;) *--i=0;
	useacc = 0;
	for (p=usecnt+NUSE; p>usecnt;) *--p=0;
}

clearreg() {
	register char **i;
	for (i=regs+NREG+1; i>regs;){ **--i=0; **i=0; }
	conloc[0] = 0; ccloc[0] = 0;
}

savereg(ai, s, type)
register char *s;
{
	register char *p, *sp;

	sp = p = regs[ai];
	/* if any indexing, must be parameter or local */
	/* indirection (as in "*-4(fp)") is ok, however */
	*p++ = type;
	if (*s=='*' || *s=='$')
		*p++ = *s++;
	if (natural(s))
		strcpy(p, s);
	else {*sp = 0; return;}
}

dest(s,type, ccflg)
register char *s;
{
	register int i;

	if ((i = isreg(s)) >= 0) {
		*(short *)(regs[i]) = 0; /* if register destination, that reg is a goner */
		if (DOUBLE==(type&0xF) || DOUBLE==((type>>4)&0xF) || type==QUAD)
			*(short *)(regs[i+1]) = 0;
	}
	for (i=NREG; --i>=0;)
		if (regs[i][1]=='*' && equstr(s, regs[i]+2))
			*(short *)(regs[i]) = 0; /* previous indirection through destination is invalid */
	while ((i = findrand(s,0)) >= 0) /* previous values of destination are invalid */
		*(short *)(regs[i]) = 0;

	if (!natural(s)) {/* wild store, everything except constants vanishes */
		for (i=NREG; --i>=0;) if (regs[i][1] != '$') *(short *)(regs[i]) = 0;
		conloc[0] = 0; ccloc[0] = 0;
	} else {
		if(ccflg)setcc(s,type); /* natural destinations set condition codes */
		if (equstr(s, conloc))
			conloc[0] = 0;
	}
}

splitrand(p) struct node *p; {
/* separate operands at commas, set up 'regs' and 'lastrand' */
register char *p1, *p2; register char **preg;

	preg=regs+RT1;
	if (p1=p->code) while (*p1) {
		lastrand=p2= *preg++;
		while (*p1) if (','==(*p2++= *p1++)) {--p2; break;}
		*p2=0;
	}
	while (preg<(regs+RT1+5)) *(*preg++)=0;
}

compat(have, want)
register int have, want;
{
	register int hsrc, hdst;
	extern int bitsize[];

	if (0==(want &= 0xF)) return(1); /* anything satisfies a wildcard want */
	hsrc=have&0xF; if (0==(hdst=((have>>4)&0xF)) || hdst>=OP2) hdst=hsrc;
	if (want>=QUAD)
		return(bitsize[hdst]==bitsize[want] && bitsize[hsrc]==bitsize[want]);
	return(hsrc==want && hdst>=want && hdst<QUAD);
}

equtype(t1,t2) {return(compat(t1,t2) && compat(t2,t1));}

findrand(as, type)
char *as;
{
	register char **i;
	for (i = regs+NREG; --i>=regs;) {
		if (**i && equstr(*i+1, as) && compat(**i,type))
			return(i-regs);
	}
	return(-1);
}

isreg(s)
register char *s;
{
	if (*s++!='r' || !isdigit(*s++)) return(-1);
	if (*s==0) return(*--s-'0');
	if (*(s-1)=='1' && isdigit(*s++) && *s==0) return(10+*--s-'0');
	return(-1);
}

/*
check()
{
	register struct node *p, *lp;

	lp = &first;
	for (p=first.forw; p!=0; p = p->forw) {
		if (p->back != lp)
			abort(-1);
		lp = p;
	}
}
*/

newcode(p) struct node *p; {
	register char *p1,*p2,**preg;

	preg=regs+RT1; p2=line;
	while (*(p1= *preg++)) {while (*p2++= *p1++); *(p2-1)=',';}
	*--p2=0;
	p->code=copy(line);
}

repladdr(p)
struct node *p;
{
	register int r;
	register char *p1;
	register char **preg;
	register int nrepl;

	preg=regs+RT1; nrepl=0;
	while (lastrand!=(p1= *preg++))
		if (0<=(r=findrand(p1,p->subop))) {
			*p1++='r'; if (r>9) {*p1++='1'; r -= 10;} *p1++=r+'0'; *p1=0;
			nchange++; nrepl++; nsaddr++;
		}
	if (nrepl) newcode(p);
}

/* conditional branches which are never/always taken */
reduncbr(p)
register struct node *p;
{
	register struct node *p1;
	register char *ap1, *ap2;

	p1 = p->back;
	if (p1->op==CMP) {
		splitrand(p1);
		ap1 = findcon(regs[RT1], p1->subop);
		ap2 = findcon(regs[RT2], p1->subop);
	} else {
		if(!ccloc[0])
			return;
		ap1 = findcon(ccloc+1, ccloc[0]);
		ap2 = "$0";
	}
	switch (compare(p->subop, ap1, ap2)) {
	case 0:		/* branch never taken */
		delnode(p);
		nredunj++;
		nchange++;
		decref(p->ref);
		if(p->forw->op!=CBR && (p1->op==TST || p1->op==CMP)) {
			delnode(p1);
			nrtst++;
		}
		break;
	case 1:		/* branch always taken */
		p->op = JBR;
		p->subop = 0;
		p->pop = 0;
		nchange++;
		if(nonlab(p->ref)->op!=CBR && (p1->op==TST || p1->op==CMP)) {
			delnode(p1);
			nrtst++;
		}
	}
}

/* a jump to a redundant compare (start of a 'for') */
redunbr(p)
register struct node *p;
{
	register struct node *p1;
	register char *ap1, *ap2;

	if ((p1 = p->ref) == 0)
		return;
	p1 = nonlab(p1);
	if (p1->op==TST || p1->op==CMP)
		splitrand(p1);
	else
		return;
	if (p1->forw->op==CBR) {
		ap1 = findcon(regs[RT1], p1->subop);
		if (p1->op==TST)
			ap2 = "$0";
		else
			ap2 = findcon(regs[RT2], p1->subop);
		p1 = p1->forw;
		if (compare(p1->subop, ap1, ap2) > 0) {
			nredunj++;
			nchange++;
			decref(p->ref);
			p->ref = p1->ref;
			p->labno = p1->labno;
#ifdef COPYCODE
			if (p->labno == 0)
				p->code = p1->code;
			if (p->ref)
#endif
				p->ref->refc++;
		}
	} else if (p1->op==TST && equstr(regs[RT1],ccloc+1) &&
			equtype(ccloc[0],p1->subop)) {
		p1=insertl(p1->forw); decref(p->ref); p->ref=p1; 
		nrtst++; nchange++;
	}
}

char *
findcon(p, type)
	register char *p;
{
	register int r;

	if (*p=='$')
		return(p);
	if ((r = isreg(p)) >= 0 && compat(regs[r][0],type))
		return(regs[r]+1);
	if (equstr(p, conloc) && equtype(conloc[0], type))
		return(conval+1);
	return(p);
}

/* compare constants: 0 - branch taken; 1 - not taken; -1 - don't know */
compare(op, acp1, acp2)
char *acp1, *acp2;
{
	register char *cp1, *cp2;
	register int n1, n2, sign;

	cp1 = acp1;
	cp2 = acp2;
	if (*cp1++ != '$' || *cp2++ != '$')
		return(-1);
	n1 = 0; sign=1; if (*cp1=='-') {++cp1; sign= -1;}
	while (isdigit(*cp1)) {n1 *= 10; n1 += *cp1++ - '0';}
	n1 *= sign;
	n2 = 0; sign=1; if (*cp2=='-') {++cp2; sign= -1;}
	while (isdigit(*cp2)) {n2 *= 10; n2 += *cp2++ - '0';}
	n2 *= sign;
	if (*cp1=='+')
		cp1++;
	if (*cp2=='+')
		cp2++;
	do {
		if (*cp1++ != *cp2)
			return(-1);
	} while (*cp2++);
	switch(op) {

	case JEQ:
		return(n1 == n2);
	case JNE:
		return(n1 != n2);
	case JLE:
		return(n1 <= n2);
	case JGE:
		return(n1 >= n2);
	case JLT:
		return(n1 < n2);
	case JGT:
		return(n1 > n2);
	case JLO:
		return((unsigned)n1 < (unsigned)n2);
	case JHI:
		return((unsigned)n1 > (unsigned)n2);
	case JLOS:
		return((unsigned)n1 <= (unsigned)n2);
	case JHIS:
		return((unsigned)n1 >= (unsigned)n2);
	}
	return(-1);
}

setcon(cv, cl, type)
register char *cv, *cl;
{
	register char *p;

	if (*cv != '$')
		return;
	if (!natural(cl))
		return;
	p = conloc;
	while (*p++ = *cl++);
	p = conval;
	*p++ = type;
	while (*p++ = *cv++);
}

setcc(ap,type)
char *ap;
{
	register char *p, *p1;

	p = ap;
	if (!natural(p)) {
		ccloc[0] = 0;
		return;
	}
	p1 = ccloc;
	*p1++ = type;
	while (*p1++ = *p++);
}

indexa(p) register char *p; {/* 1-> uses [r] addressing mode; 0->doesn't */
	while (*p) if (*p++=='[') return(1);
	return(0);
}

natural(p)
register char *p;
{/* 1->simple local, parameter, global, or register; 0->otherwise */

	if (*p=='*' || *p=='(' || *p=='$')
		return(0);
	while (*p++);
	p--;
	if (*--p==']' || *p==')' &&
	 !(*(p-2)=='f' || fortflg && (*--p=='1' || *p=='2') && *--p=='1'))
		return(0);
	return(1);
}

/*
** Tell if an argument is most likely static.
*/

isstatic(cp)
register char	*cp;
{
	if (*cp == '_' || *cp == 'L')
		return (1);
	return (0);
}


checkaobdisp(p)
register struct node *p;
{
register struct node *q;
register int i;
	

if (!aobflag) return(1);
/*  backward search */
	i = 0;
	q = p; 
	while (i++ < MAXAOBDISP && ((q= q->back) !=&first))
	{
		if (p->ref == q) 
		   return(1);
	}

/*  forward search */
	i = 0;
	q = p; 
	while (i++ < MAXAOBDISP && ((q= q->forw) !=0))
	{
		if (p->ref == q) 
		   return(1);
	}
	return(0);
}


struct intleavetab intltab[] = {
	ADDF,	FLOAT,		1,
	ADDF,	DOUBLE,		1,
	SUBF,	FLOAT,		1,
	SUBF,	DOUBLE,		1,
	MULF,	FLOAT,		1,
	MULF,	DOUBLE,		1,
	DIVF,	FLOAT,		1,
	DIVF,	DOUBLE,		1,
	SINF,	FLOAT,		1,
	COSF,	FLOAT,		1,
	ATANF,	FLOAT,		1,
	LOGF,	FLOAT,		1,
	SQRTF,	FLOAT,		1,
	EXPF,	FLOAT,		1,
	LDF,	FLOAT,		0,
	LDF,	DOUBLE,		0,
	LNF,	FLOAT,		0,
	LNF,	DOUBLE,		0,
	STF,	FLOAT,		0,
	CMPF,	FLOAT,		0,
	CMPF,	DOUBLE,		0,
	CMPF2,	FLOAT,		0,
	TSTF,	FLOAT,		0,
	TSTF,	DOUBLE,		0,
	PUSHD,	DOUBLE,		0,
	CVLF,	U(LONG,FLOAT),	0,
	CVFL,	U(FLOAT,LONG),	0, 
	LDFD,	U(FLOAT,DOUBLE),0, 
	CVDF,	U(DOUBLE,FLOAT),0,
	NEGF,	FLOAT,		0,
	NIL,	0,		0};

interleave()
{
	register struct node *p, *p1;

	register struct intleavetab *t;
	register int r;
	int count;
	for (p= first.forw; p!=0; p = p->forw){
		count = 0;
		for  (t =intltab; t->op != NIL; t++){
			if (t->op == p->op && t->subop == p->subop){
			count = t->intleavect;
			break;
			}
		}
		if (count < 1) continue;
		p1 = p->forw;
		clearuse();
		clearreg();
		while ((p1 != 0) && (p1->op != CBR) &&
		      (p1->subop == FLOAT || p1->subop == DOUBLE ||
	              ((p1->subop&0xF0)==DOUBLE<<4) || ((p1->subop&0xF)==DOUBLE )||
	              ((p1->subop&0xF0)==FLOAT<<4) || (p1->subop&0xF)==FLOAT))
		{
			if (((r = isreg(p1->code)) >= 0)){
			uses[r] = p1;
			if ((p1->subop == DOUBLE) || ((p->subop&0xF0)==DOUBLE<<4) || 
		           ((p->subop&0xF)==DOUBLE)) 
				uses[r+1] = p1;
			}
			else checkreg(p1,p1->code);
			p1 = p1->forw;

		}
		if (p1 == 0) return;
		if (!(sideeffect(p, p1)))
			insertblk(p,p1);
	}
		
}


insertblk(p, p1)
struct node *p, *p1;
{
	p1->back->forw = p1->forw;
	p1->forw->back = p1->back;
	p1->forw = p->forw;
	p->forw->back = p1;
	p->forw = p1;
	p1->back = p;
}

OpCode termop[] = {
	JBR, CBR, JMP, LABEL, DLABEL, EROU, JSW, TST, CMP, BIT,
	CALLF, CALLS, CASE, AOBLEQ, AOBLSS, CMPF, CMPF2, TSTF, MOVBLK, MFPR,
	MTPR, PROBE, MOVO, TEXT, DATA, BSS, ALIGN, END, LGEN, SET,
	LCOMM, COMM, NIL
}; 

sideeffect(p,p1)
struct node *p, *p1;
{
	register struct node *q;
	register int r;
	register OpCode *t;
	register char *cp;
	int i;

	if (p1->op == NIL) return(1);  /*  special instructions */

	for (t = termop; *t!=NIL; t++){
		if (*t == p1->op) return(1);
	}
	if ((p1->forw != NULL) && (p1->forw->op == CBR))
		return(1);
	splitrand(p1);
	r = isreg(lastrand);
	if (uses[r] &&  r >= 0 ) return(1);
	if ((p1->op == EDIV) && (r = isreg(regs[RT3]) >= 0) &&
	   (uses[r]))  return(1);

	for (q = p1->back ; q!=p; q=q->back)
	{
		if ((p1->op == PUSH || p1->op == PUSHA) &&
		    (q->op == PUSHD || q->op == PUSH || q->op == PUSHA))  
		     return(1); 		     /* keep args in order */
		if (((i = strlen(q->code)) >= 5 &&    /* cvdl -(sp); pushl r0*/
		    (strcmp(q->code+i-5,"-(sp)") == 0 )) || 
		    (strcmp(lastrand,"-(sp)") == 0)) return(1);
		if (equstr(q->code, lastrand))
		    return(1);
		if (q->op == STF || q->op == CVFL || q->op == CVLF) 
		   {
		    if (equstr(q->code, regs[RT1])) return(1);
		if (has3ops(p1) || p1->op == EMUL || p1->op == EDIV)
		    if (equstr(q->code, regs[RT2]))
		       return(1);
		/*  handle the case  std -56(fp) pushl -60(fp) pushl
		    -56(fp);
		*/
		if ((p1->forw != NULL) &&  (q->op == STF) &&
		(q->subop == DOUBLE)){ 
		if (!strncmp(q->code,p1->forw->code,strlen(q->code)))
			return(1);
		}
		}
	}
	return(0);
}
checkreg(p,s)
struct node *p;
char *s;
{
char *cp2;  
register int r;
	/* check for (r),[r] */
	do if (*s=='(' || *s=='[') {/* get register number */
		char t;
		cp2= ++s; while (*++s!=')' && *s!=']'); t= *s; *s=0;
		if ((r=isreg(cp2)) >= 0)  {
			uses[r]=p; 
		}
		*s=t;
	} while (*++s);
}
