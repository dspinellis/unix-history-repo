#
/*
 *  C compiler
 */

#include "c1h.c"

max(a, b)
{
	if (a>b)
		return(a);
	return(b);
}

degree(at)
struct tnode *at;
{
	register struct tnode *t;

	if ((t=at)==0 || t->op==0)
		return(0);
	if (t->op == CON)
		return(-3);
	if (t->op == AMPER)
		return(-2);
	if (t->op == AMPERA)
		return(-1);
	if ((opdope[t->op] & LEAF) != 0) {
		if (t->type==CHAR)
			return(1);
		return(0);
	}
	return(t->degree);
}

pname(ap, flag)
struct tnode *ap;
{
	register i;
	register struct tnode *p;

	p = ap;
loop:
	switch(p->op) {

	case SFCON:
	case CON:
		printf("$0%o", p->value);
		return;

	case FCON:
		printf("L%d", p->value);
		return;

	casename:
	case NAME:
		if (i = p->offset)
			printf("%d.+", i);
		switch(p->class) {

		case AUTO:
			printf("%d.(r5)", p->nloc);
			return;

		case EXTERN:
			printf("_%.8s", &(p->nloc));
			return;

		case STRUCT:
			error("Illegal structure reference");
			printf("$0");
			return;

		case REG:
			if (i)
				error("Bad reg. reference");
			printf("r%d", p->nloc);
			return;

		}
		/* presumably, it's static */
		printf("L%d", p->nloc);
		return;

	case AMPER:
		putchar('$');
		p = p->tr1;
		goto loop;

	case STAR:
		p = p->tr1;
		/* reg[const] or statchar[reg] */
		if (p->op==PLUS) {
			if (p->tr2->op==AMPER) {
				pname(p->tr2->tr1, 0);
				printf("(r%d)", p->tr1->nloc);
			return;
		}
			printf("%d.(r%d)", p->tr2->value, p->tr1->nloc);
			return;
		} else if (p->op==INCAFT) {
			printf("(r%d)%c", p->tr1->nloc, flag?0:'+');
			return;
		} else if (p->op==DECBEF) {
			printf("%c(r%d)", flag?0:'-', p->tr1->nloc);
			return;
		} else if (p->op==NAME && p->class==REG) {
			printf("(r%d)", p->nloc);
			return;
		}
		putchar('*');
		goto loop;

	}
	error("pname called illegally");
}

xdcalc(ap, nrleft)
struct tnode *ap;
{
	register struct tnode *p;
	register d;

	p = ap;
	d = dcalc(p, nrleft);
	if (d<20 && p->type==CHAR) {
		if (nrleft>=1)
			d = 20;
		else
			d = 24;
	}
	return(d);
}

dcalc(ap, nrleft)
struct tnode *ap;
{
	register struct tnode *p, *p1;

	if ((p=ap)==0)
		return(0);
	switch (p->op) {

	case NAME:
	case AMPER:
		return(12);

	case CON:
	case SFCON:
		return(p->value==0? 4:(p->value==1?5:8));

	case FCON:
		return(12);

	case STAR:
		p1 = p->tr1;
		if (p1->op==NAME || p1->op==CON)
			return(12);
		if (p1->op==STAR)
			p1 = p1->tr1;
		if (p1->op==PLUS && p1->tr1->op==NAME
		 && p1->tr1->class==REG && p1->tr2->op==CON)
			return(12);
		if (p1->op==PLUS && p1->tr2->op==AMPER && p1->tr1->op==NAME
		 && p1->tr2->type==(PTR+CHAR) && p1->tr1->class==REG)
			return(12);
		if ((p1->op==INCAFT || p1->op==DECBEF) &&
		     p1->tr1->type!=(STRUCT+PTR))
			p1 = p1->tr1;
		 if (p1->op==NAME && p1->class==REG)
			return(12);
	}
	return(p->degree<=nrleft? 20: 24);
}

notcompat(ap, ast)
struct tnode *ap;
{
	register at, st;
	register struct tnode *p;

	p = ap;
	at = p->type;
	st = ast;
	if ((at&07)==STRUCT)
		at =& 077770;	/* map to int */
	if (st==0)		/* word, byte */
		return(at>1 & at<=07);
	if (st==1)		/* word */
		return(at>0 & at<=07);
	st =- 2;
	if ((at&077740) != 0)
		at = 020;
	if ((at&077770) != 0)
		at = at&07 | 020;
	if (st==2 && at==3)
		at = 2;
	if (p->op==NAME && p->class==REG && at==INT && st==CHAR)
		return(0);
	return(st != at);
}

prins(op, c) {
	register struct instab *insp;
	register char *ip;

	for (insp=instab; insp->op != 0; insp++) {
		if (insp->op == op) {
			ip = c? insp->str2: insp->str1;
			if (ip==0)
				break;
			printf("%s", ip);
			return;
		}
	}
	error("No match' for op %d", op);
}

collcon(ap)
struct tnode *ap;
{
	register op;
	register struct tnode *p;

	p = ap;
	if(p->op==PLUS) {
		op = p->tr2->op;
		if (op==CON || op==AMPER || op==AMPERA)
			return(1);
	}
	return(0);
}

isfloat(at)
struct tnode *at;
{
	register struct tnode *t;

	t = at;
	if ((opdope[t->op]&RELAT)!=0)
		t = t->tr1;
	if (t->type==FLOAT || t->type==DOUBLE) {
		nfloat = 1;
		return('f');
	}
	return(0);
}

oddreg(t, areg)
struct tnode *t;
{
	register reg;

	reg = areg;
	if (!isfloat(t))
		switch(t->op) {
		case DIVIDE:
		case MOD:
		case ASDIV:
		case ASMOD:
			reg++;

		case TIMES:
		case ASTIMES:
			return(reg|1);
		}
	return(reg);
}

arlength(t)
{
	if (t>=PTR)
		return(2);
	switch(t) {

	case INT:
	case CHAR:
		return(2);

	case FLOAT:
	case DOUBLE:
		return(8);
	}
	return(1024);
}

pswitch(afp, alp, deflab)
struct swtab *afp, *alp;
{
	int tlab, ncase, i, j, tabs, worst, best, range;
	register struct swtab *swp, *fp, *lp;
	int poctab[swsiz];

	fp = afp;
	lp = alp;
	if (fp==lp) {
		printf("jbr	L%d\n", deflab);
		return;
	}
	tlab = isn++;
	if (sort(fp, lp))
		return;
	ncase = lp-fp;
	lp--;
	range = lp->swval - fp->swval;
	/* direct switch */
	if (range>0 && range <= 3*ncase) {
		if (fp->swval)
			printf("sub	$0%o,r0\n", fp->swval);
		printf("cmp	r0,$0%o\n", range);
		printf("jhi	L%d\n", deflab);
		printf("asl	r0\n");
		printf("jmp	*.+4(r0)\n");
		for (i=fp->swval; i<=lp->swval; i++) {
			if (i==fp->swval) {
				printf("L%d\n", fp->swlab);
				fp++;
			} else
				printf("L%d\n", deflab);
		}
		goto esw;
	}
	/* simple switch */
	if (ncase<8) {
		i = isn++;
		j = isn++;
		printf("mov	$L%d,r1\n", i);
		printf("mov	r0,L%d\n", j);
		printf("L%d:cmp	r0,(r1)+\n", isn);
		printf("jne	L%d\n", isn++);
		printf("jmp	*L%d-L%d(r1)\n", j, i);
		printf(".data\nL%d:", i);
		for (; fp<=lp; fp++)
			printf("%o\n", fp->swval);
		printf("L%d:..\n", j);
		for (fp = afp; fp<=lp; fp++)
			printf("L%d\n", fp->swlab);
		printf("L%d\n.text\n", deflab);
		goto esw;
	}
	/* hash switch */
	best = 077777;
	for (i=ncase/4; i<=ncase/2; i++) {
		for (j=0; j<i; j++)
			poctab[j] = 0;
		for (swp=fp; swp<=lp; swp++)
			poctab[lrem(0, swp->swval, i)]++;
		worst = 0;
		for (j=0; j<i; j++)
			if (poctab[j]>worst)
				worst = poctab[j];
		if (i*worst < best) {
			tabs = i;
			best = i*worst;
		}
	}
	printf("jsr	r2,hsw; %o; L%d\n", tabs, isn);
	printf("jmp	*L%d-L%d(r1)\n", isn+tabs+1, isn+1);
	printf(".data\nL%d:", isn++);
	for (i=0; i<=tabs; i++)
		printf("L%d\n", isn+i);
	for (i=0; i<tabs; i++) {
		printf("L%d:..\n", isn++);
		for (swp=fp; swp<=lp; swp++)
			if (lrem(0, swp->swval, tabs) == i)
				printf("%o\n", ldiv(0, swp->swval, tabs));
	}
	printf("L%d:", isn++);
	for (i=0; i<tabs; i++) {
		printf("L%d\n", deflab);
		for (swp=fp; swp<=lp; swp++)
			if (lrem(0, swp->swval, tabs) == i)
				printf("L%d\n", swp->swlab);
	}
	printf(".text\n");
esw:
	printf("/esw\n");
}

sort(afp, alp)
struct swtab *afp, *alp;
{
	register struct swtab *cp, *fp, *lp;
	int intch, t;

	fp = afp;
	lp = alp;
	while (fp < --lp) {
		intch = 0;
		for (cp=fp; cp<lp; cp++) {
			if (cp->swval == cp[1].swval) {
				error("Duplicate case (%d)", cp->swval);
				return(1);
			}
			if (cp->swval > cp[1].swval) {
				intch++;
				t = cp->swval;
				cp->swval = cp[1].swval;
				cp[1].swval = t;
				t = cp->swlab;
				cp->swlab = cp[1].swlab;
				cp[1].swlab = t;
			}
		}
		if (intch==0)
			break;
	}
	return(0);
}

ispow2(atree)
{
	register int d;
	register struct tnode *tree;

	tree = atree;
	if (!isfloat(tree) && tree->tr2->op==CON) {
		d = tree->tr2->value;
		if (d>0 && (d&(d-1))==0)
			return(d);
	}
	return(0);
}

pow2(atree)
struct tnode *atree;
{
	register int d, i;
	register struct tnode *tree;

	tree = atree;
	if (d = ispow2(tree)) {
		for (i=0; (d=>>1)!=0; i++);
		tree->tr2->value = i;
		d = tree->op;
		tree->op = d==TIMES? LSHIFT:
			  (d==DIVIDE? RSHIFT:
			  (d==ASTIMES? ASLSH: ASRSH));
	}
}

cbranch(atree, albl, cond, areg)
struct tnode *atree;
{
	int l1, opfix;
	register lbl, reg;
	register struct tnode *tree;
	struct tnode lbuf;

	lbl = albl;
	reg = areg;
	if ((tree=atree)==0)
		return;
	switch(tree->op) {

	case LOGAND:
		if (cond) {
			cbranch(tree->tr1, l1=isn++, 0, reg);
			cbranch(tree->tr2, lbl, 1, reg);
			label(l1);
		} else {
			cbranch(tree->tr1, lbl, 0, reg);
			cbranch(tree->tr2, lbl, 0, reg);
		}
		return;

	case LOGOR:
		if (cond) {
			cbranch(tree->tr1, lbl, 1, reg);
			cbranch(tree->tr2, lbl, 1, reg);
		} else {
			cbranch(tree->tr1, l1=isn++, 1, reg);
			cbranch(tree->tr2, lbl, 0, reg);
			label(l1);
		}
		return;

	case EXCLA:
		cbranch(tree->tr1, lbl, !cond, reg);
		return;

	case COMMA:
		rcexpr(tree->tr1, efftab, reg);
		tree = tree->tr2;
		break;
	}
	opfix = 0;
	if ((opdope[tree->op]&RELAT)==0) {
		lbuf.op = NEQUAL;
		lbuf.type = tree->type;
		lbuf.degree = tree->degree;
		lbuf.tr1 = tree;
		lbuf.tr2 = &czero;
		if (isfloat(tree))
			lbuf.tr2 = &fczero;
		tree = &lbuf;
	}
 	if (tree->tr2->op==CON && tree->tr2->value==0)
		opfix = 200;
	rcexpr(tree, cctab, reg);
	if (isfloat(tree))
		printf("cfcc\n");
	branch(lbl, tree->op+opfix, !cond);
}

branch(lbl, aop, c)
{
	register op;

	if(op=aop)
		prins(op,c);
	else
		printf("jbr");
	printf("\tL%d\n", lbl);
}

label(l)
{
	printf("L%d:", l);
}

popstk(a)
{
	switch(a) {

	case 0:
		return;

	case 2:
		printf("tst	(sp)+\n");
		return;

	case 4:
		printf("cmp	(sp)+,(sp)+\n");
		return;
	}
	printf("add	$0%o,sp\n", a);
}

error(s, p1, p2, p3, p4, p5, p6)
{
	register f;

	nerror++;
	fflush(outbuf);
	f = outbuf[0];
	outbuf[0] = 1;
	printf("%d: ", line);
	printf(s, p1, p2, p3, p4, p5, p6);
	putchar('\n');
	fflush(outbuf);
	outbuf[0] = f;
}

