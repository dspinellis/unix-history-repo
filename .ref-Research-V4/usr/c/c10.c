#
/*

	    	C compiler, part 2

	Copyright 1972 Bell Telephone Laboratories, Inc.

*/

#include "c1h.c"

char	maprel[] {	EQUAL, NEQUAL, GREATEQ, GREAT, LESSEQ,
			LESS, GREATP, GREATQP, LESSP, LESSEQP
};

char	notrel[] {	NEQUAL, EQUAL, GREAT, GREATEQ, LESS,
			LESSEQ, GREATQP, GREATP, LESSEQP, LESSP
};

struct tconst czero { CON, INT, 0, 0};
struct tconst cone  { CON, INT, 0, 1};
struct tconst fczero { SFCON, DOUBLE, 0, 0 };

struct table *tabtab[]
{
	regtab,
	efftab,
	cctab,
	sptab,
	0
};

int	nreg	3;
int	isn	10000;
int	namsiz	8;
int	*treebase;

main(argc, argv)
char *argv[];
{
	int treespace[ossiz];
	struct table *table;
	register *sp, c, *tree;

	if (argc<4) {
		error("Arg count");
		exit(1);
	}
	if(fopen(argv[1], ascbuf)<0 || fopen(argv[2], binbuf)<0){
		error("Missing temp file");
		exit(1);
	}
	if (fcreat(argv[3], outbuf) < 0) {
		error("Can't create %s", argv[3]);
		exit(1);
	}
	treebase = getw(binbuf);
	if (treebase < treespace) {
		error("Tree space botch");
		exit(1);
	}
	while ((c=getc(ascbuf)) > 0) {
		if(c=='#') {
			sp = treebase;
			c = getw(binbuf);
			tree = getw(binbuf);
			table = tabtab[getw(binbuf)];
			line = getw(binbuf);
			while(--c >= 0)
				*sp++ = getw(binbuf);
			if (table==0)		/* is switch */
				pswitch(treebase, sp, tree);
			else {
				tree = optim(tree);
				nstack = 0;
				rcexpr(tree, table, 0);
			}
		} else
			putchar(c);
	}
	if (nfloat)
		printf(".globl	fltused\n");
	fflush(outbuf);
	exit(nerror!=0);
}

char *match(atree, table, nrleft)
struct tnode *atree;
struct table *table;
{
	int op, d1, d2, t1, t2, dope;
	struct tnode *p2;
	register struct tnode *p1, *tree;
	register struct optab *opt;

	if ((tree=atree)==0)
		return(0);
	if (table==lsptab)
		table = sptab;
	op = tree->op;
	dope = opdope[op];
	if ((dope&LEAF) == 0)
		p1 = tree->tr1;
	else
		p1 = tree;
	t1 = p1->type;
	d1 = dcalc(p1, nrleft);
	if ((dope&BINARY)!=0) {
		p2 = tree->tr2;
		t2 = p2->type;
		d2 = dcalc(p2, nrleft);
	}
	for (; table->op!=op; table++)
		if (table->op==0)
			return(0);
	for (opt = table->tabp; opt->tabdeg1!=0; opt++) {
		if (d1 > (opt->tabdeg1&077)
		 || (opt->tabdeg1 >= 0100 && (p1->op != STAR)))
			continue;
		if (notcompat(p1, opt->tabtyp1)) {
			continue;
		}
		if ((opdope[op]&BINARY)!=0 && p2!=0) {
			if (d2 > (opt->tabdeg2&077)
			 || (opt->tabdeg2 >= 0100) && (p2->op != STAR) )
				continue;
			if (notcompat(p2,opt->tabtyp2))
				continue;
		}
		return(opt);
	}
	return(0);
}

rcexpr(atree, atable, reg)
struct tnode *atree;
struct table *atable;
{
	register r;
	int modf;
	register struct tnode *tree;
	register struct table *table;

	table = atable;
	if((tree=atree)==0)
		return(0);
	switch (tree->op)  {

	case SETREG:
		nreg = tree->type-1;
		return;

	case CBRANCH:
		cbranch(tree->btree, tree->lbl, tree->cond, 0);
		return(0);

	case INIT:
		if (tree->tr1->op == AMPER)
			tree->tr1 = tree->tr1->tr1;
		if (tree->tr1->op!=NAME && tree->tr1->op!=CON)
			error("Illegal initialization");
		else
			cexpr(tree, regtab, nreg);
		return(0);

	case EXCLA:
		if ((opdope[tree->tr1->op] & RELAT) != 0) {
			tree = tree->tr1;
			tree->op = notrel[tree->op - EQUAL];
		}
		break;

	case RFORCE:
		if((r=rcexpr(tree->tr1, table, reg)) != 0)
			printf("mov%c	r%d,r0\n", isfloat(tree->tr1), r);
		return(0);

	case COMMA:
		rcexpr(tree->tr1, efftab, reg);
		tree = tree->tr2;
		break;

	case TIMES:
	case DIVIDE:
	case ASTIMES:
	case ASDIV:
		pow2(tree);
	}
	if ((r=cexpr(tree, table, reg))>=0) {
		return(r);
}
	if (table!=regtab) 
		if((r=cexpr(tree, regtab, reg))>=0) {
			modf = isfloat(tree);
			if (table==sptab || table==lsptab) {
				printf("mov%c	r%d,%c(sp)\n", modf, r,
					table==sptab? '-':0);
				nstack++;
			}
			if (table==cctab) {
				printf("tst%c	r%d\n", modf, r);
			}
			return(0);
		}
	error("No match for op %d", tree->op);
}

cexpr(atree, table, areg)
struct tnode *atree;
struct table *table;
{
	int c, r;
	register struct tnode *p, *p1, *tree;
	struct table *ctable;
	struct tnode *p2;
	char *string;
	int reg, reg1, rreg, flag, nargs, fflag;
	char *opt;

	tree = atree;
	reg = areg;
	p1 = tree->tr2;
	if ((c = tree->op)==CALL) {
		r = 0;
		nargs = 0;
		fflag = 0;
		if (tree->tr1->op!=NAME) {	/* get nargs right */
			nargs++;
			nstack++;
		}
		if(p1->op) {
			while (p1->op==COMMA) {
				r =+ comarg(p1->tr2, &fflag);
				p1 = p1->tr1;
				nargs++;
			}
			r =+ comarg(p1, &fflag);
			nargs++;
		}
		tree->op = CALL+1;
		tree->degree = r;	/* save arg length */
	}
	if ((opdope[c]&RELAT||c==LOGAND||c==LOGOR) && table!=cctab) {
		cbranch(tree, c=isn++, 1, reg);
		rcexpr(&czero, table, reg);
		branch(isn, 0);
		label(c);
		rcexpr(&cone, table, reg);
		label(isn++);
		return(reg);
	}
	if(c==QUEST) {
		if (table==cctab)
			return(-1);
		cbranch(tree->tr1, c=isn++, 0, reg);
		flag = nstack;
		rreg = rcexpr(p1->tr1, table, reg);
		nstack = flag;
		branch(r=isn++, 0);
		label(c);
		reg = rcexpr(p1->tr2, table, rreg);
		if (rreg!=reg)
			printf("mov%c	r%d,r%d\n",
			    isfloat(tree),reg,rreg);
		reg = rreg;
		label(r);
		goto retrn;
	}
	if (c==AMPER && tree->tr1->op==NAME && tree->tr1->class==REG)
		error("Illegal use of register");
	reg = oddreg(tree, reg);
	reg1 = reg+1;
	if (chkleaf(tree, table, reg) >= 0)
		goto retrn;
	if ((opt=match(tree, table, nreg-reg))==0) 
		return(-1);
	string = opt->tabstring;
	p1 = tree->tr1;
	p2 = 0;
	if (opdope[tree->op] & BINARY)
		p2 = tree->tr2;
loop:
	switch(c = *string++) {

	case '\0':
		if (tree->op==CALL+1) {
			popstk(tree->degree);
			reg = 0;
			nstack =- nargs;
		}
retrn:
		if (!isfloat(tree))
			if (tree->op==DIVIDE || tree->op==ASDIV)
				reg--;
		return(reg);

	/* A1 */
	case 'A':
		p = p1;
		goto adr;

	/* A2 */
	case 'B':
		p = p2;
		goto adr;

	/* A */
	case 'O':
		p = tree;
	adr:
		c = 0;
		if (*string=='\'') {
			c++;
			string++;
		}
		pname(p, c);
		goto loop;

	/* I */
	case 'M':
		if ((c = *string)=='\'')
			string++;
		else
			c = 0;
		prins(tree->op, c);
		goto loop;

	/* B1 */
	case 'C':
		if ((opdope[tree->op]&LEAF) != 0)
			p = tree;
		else
			p = p1;
		goto pbyte;

	/* BF */
	case 'P':
		p = tree;
		goto pb1;

	/* B2 */
	case 'D':
		p = p2;
	pbyte:
		if (p->type==CHAR)
			putchar('b');
	pb1:
		if (isfloat(p))
			putchar('f');
		goto loop;

	/* BE */
	case 'L':
		if (p1->type==CHAR || p2->type==CHAR)
			putchar('b');
		p = tree;
		goto pb1;

	/* C1 */
	case 'E':
		p = p1->tr1;
		goto const;

	/* C2 */
	case 'F':
		p = p2->tr1;
	const:
		printf("0%o", p);
		goto loop;

	/* F */
	case 'G':
		p = p1;
		flag = 01;
		goto subtre;

	/* S */
	case 'K':
		p = p2;
		flag = 02;
		goto subtre;

	/* H */
	case 'H':
		p = tree;
		flag = 04;

	subtre:
		ctable = regtab;
		c = *string++ - 'A';
		if ((c&02)!=0)
			ctable = sptab;
		if ((c&04)!=0)
			if (p->op!=INCAFT && p->op!=DECAFT
			 && match(p, efftab, nreg-reg))
				ctable = efftab;
			else
				ctable = cctab;
		if ((c&01)!=0) {
			p = p->tr1;
			if(collcon(p) && ctable!=sptab) {
				if (p->tr2->op==AMPERA)
					flag =| 010;
				p = p->tr1;
			}
		}
		if (table==lsptab && ctable==sptab)
			ctable = lsptab;
		if (c&010)
			r = reg1;
		else
			if (opdope[p->op]&LEAF || p->degree < 2)
				r = reg;
			else
				r = areg;
		rreg = rcexpr(p, ctable, r);
		if (flag&010)
			printf("add	r5,r%d\n", rreg);
		if (c&010)
			reg1 = rreg;
		else if (rreg!=reg && ctable==regtab)
			if (oddreg(tree, 0)==0 && (flag&04 ||
			      flag&01
			  && xdcalc(p2, nreg-rreg-1) <= (opt->tabdeg2&077)
			 ||   flag&02
			  && xdcalc(p1,nreg-rreg-1) <= (opt->tabdeg1&077))) {
				reg = rreg;
				reg1 = rreg+1;
			} else
				printf("mov%c\tr%d,r%d\n",
				    isfloat(tree), rreg, reg);
		goto loop;

	/* R */
	case 'I':
		r = reg;
		if (*string=='-') {
			string++;
			r--;
		}
		goto preg;

	/* R1 */
	case 'J':
		r = reg1;
	preg:
		if (r>nreg)
			error("Register overflow: simplify expression");
		printf("r%d", r);
		goto loop;

	case '-':		/* check -(sp) */
		if (*string=='(') {
			nstack++;
			if (table!=lsptab)
				putchar('-');
			goto loop;
		}
		break;

	case ')':		/* check (sp)+ */
		putchar(')');
		if (*string=='+')
			nstack--;
		goto loop;

	case '#':
		p = p1->tr1;
		goto nmbr;

	case '"':
		p = p2->tr1;
		goto nmbr;

	case '~':
		p = p1;

	nmbr:
		if(collcon(p)) {
			switch((p = p->tr2)->op) {

			case CON:
				if (p->value)
					printf("%d.", p->value);
				break;

			case AMPER:
				pname(p->tr1, 0);
				break;

			case AMPERA:
				p = p->tr1;
				printf("%d.", p->nloc+p->offset);
				break;
			}
		}
		goto loop;

	/* V */
	case 'V':
		tree->op = maprel[tree->op - EQUAL];
		goto loop;

	/* Z */
	case 'Z':
		printf("$0%o", p1->offset+p1->nloc);
		goto loop;

	case '^':		/* for ++ --, tr2 is length */
		printf("0%o", tree->tr2);
		goto loop;

	case 'T':		/* "tst R" if 1st op not in cctab */
		if (dcalc(p1, 5)>12 && !match(p1, cctab, 10))
			printf("tst	r%d\n", reg);
		goto loop;

	case '`':		/* for jsr pc,*$F on compression */
		if (fflag)
			printf("*$");
		goto loop;
	}
	putchar(c);
	goto loop;
}

chkleaf(atree, table, reg)
struct tnode *atree;
{
	struct tnode lbuf;
	register struct tnode *tree;

	tree = atree;
	if (dcalc(tree, nreg-reg) > 12)
		return(-1);
	lbuf.op = LOAD;
	lbuf.type = tree->type;
	lbuf.degree = tree->degree;
	lbuf.tr1 = tree;
	return(cexpr(&lbuf, table, reg));
}

comarg(atree, flagp)
int *flagp;
{
	register struct tnode *tree;

	tree = atree;
	if (tree->type==STRUCT)
		error("Illegal structure");
	if (nstack || isfloat(tree)) {
		rcexpr(tree, sptab, 0);
		return(arlength(tree->type));
	}
	(*flagp)++;
	rcexpr(tree, lsptab, 0);
	return(0);
}

optim(atree)
struct tnode *atree;
{
	register op, dope;
	int d1, d2;
	struct tnode *t;
	register struct tnode *tree;

	if ((tree=atree)==0)
		return(0);
	op = tree->op;
	if (op==0)
		return(tree);
	dope = opdope[op];
	if ((dope&LEAF) != 0)
		return(tree);
	if ((dope&BINARY) == 0)
		return(unoptim(tree));
	/* is known to be binary */
	if ((dope&COMMUTE)!=0) {
	acomm:	d1 = tree->type;
		tree = acommute(tree);
		tree->type = d1;
		return(tree);
	}
	tree->tr1 = optim(tree->tr1);
	tree->tr2 = optim(tree->tr2);
	if ((dope&RELAT) != 0) {
		if (degree(tree->tr1) < degree(tree->tr2)) {
			t = tree->tr1;
			tree->tr1 = tree->tr2;
			tree->tr2 = t;
			tree->op = maprel[op-EQUAL];
		}
		if (tree->tr1->type==CHAR && tree->tr2->op==CON
		 && dcalc(tree->tr1) <= 12
		 && tree->tr2->value <= 127 && tree->tr2->value >= 0)
			tree->tr2->type = CHAR;
	}
	d1 = max(degree(tree->tr1), 1);
	d2 = max(degree(tree->tr2), 0);
	switch (op) {

	case CALL:
		tree->degree = 10;
		break;

	case QUEST:
	case COLON:
		tree->degree = max(d1, d2);
		break;

	case MINUS:
		if (tree->tr2->op==CON) {	/* const */
			tree->op = PLUS;
			tree->tr2->value = -tree->tr2->value;
			goto acomm;
		}
		goto def;

	case DIVIDE:
	case ASDIV:
	case ASTIMES:
		if (ispow2(tree) == 0) {

		case MOD:
		case ASMOD:
			d1 =+ 2;
			d2 =+ 2;
		}

	case LSHIFT:
	case RSHIFT:
		if (tree->tr1->op==CON && tree->tr2->op==CON) {
			const(op, &tree->tr1->value, tree->tr2->value);
			return(tree->tr1);
		}

	def:
	default:
		tree->degree = d1==d2? ++d1: max(d1, d2);
		break;
	}
	return(tree);
}

unoptim(atree)
struct tnode *atree;
{
	register struct tnode *subtre, *tree;
	register int *p;
	double static fv;
	struct { int integer; };

	if ((tree=atree)==0)
		return(0);
	if (tree->op==CBRANCH) {
		tree->btree = optim(tree->btree);
		return(tree);
	}
	subtre = tree->tr1 = optim(tree->tr1);
	/* try to reduce * & */
	if (tree->op==STAR && (subtre->op==AMPER || subtre->op==AMPERA))
		return(subtre->tr1);
	if (tree->op == ITOF && subtre->op == CON) {
		fv = subtre->value;
		p = &fv;
		p++;
		if (*p++==0 && *p++==0 && *p++==0) {
			subtre->type = DOUBLE;
			subtre->value = fv.integer;
			subtre->op = SFCON;
			return(subtre);
		}
	}
	if (subtre->op == CON) switch(tree->op) {

	case NEG:
		subtre->value = -subtre->value;
		return(subtre);

	case COMPL:
		subtre->value = ~subtre->value;
		return(subtre);
	}
	tree->degree = max(1, degree(subtre));
	return(tree);
}

struct acl {
	int nextl;
	int nextn;
	struct tnode *nlist[20];
	struct tnode *llist[21];
};

acommute(atree)
{
	struct acl acl;
	int d, i, op, flt;
	register struct tnode *t1, **t2, *tree;
	struct tnode *t;

	acl.nextl = 0;
	acl.nextn = 0;
	tree = atree;
	op = tree->op;
	flt = isfloat(tree);
	insert(op, tree, &acl);
	acl.nextl--;
	if (!flt) {
		/* put constants together */
		t2 = &acl.llist[acl.nextl];
		for (i=acl.nextl;i>0&&t2[0]->op==CON&&t2[-1]->op==CON;i--) {
			acl.nextl--;
			t2--;
			const(op, &t2[0]->value, t2[1]->value);
		}
	}
	if (op==PLUS && !flt) {
		/* toss out "+0" */
		if (acl.nextl>0 && (*t2)->op==CON && (*t2)->value==0) {
			acl.nextl--;
			t2--;
		}
		if (acl.nextl <= 0)
			return(*t2);
		/* subsume constant in "&x+c" */
		if (t2[0]->op==CON &&
		   (t2[-1]->op==AMPER || t2[-1]->op==AMPERA)) {
			t2--;
			t2[0]->tr1->offset =+ t2[1]->value;
			acl.nextl--;
		}
	} else if (op==TIMES) {
		t1 = acl.llist[acl.nextl];
		if (t1->op==CON && t1->value==0)
			return(t1);
	}
	if (op==PLUS && !flt)
		distrib(&acl);
	tree = *(t2 = &acl.llist[0]);
	d = max(degree(tree), 1);
	if (op==TIMES && !flt)
		d++;
	for (i=0; i<acl.nextl; i++) {
		t1 = acl.nlist[i];
		t1->tr2 = t = *++t2;
		t1->degree = d = degree(t)>=d? d+1:d;
		t1->tr1 = tree;
		tree = t1;
	}
	if (tree->op==TIMES && ispow2(tree))
		tree->degree = max(degree(tree->tr1), 1);
	return(tree);
}

distrib(list)
struct acl *list;
{
/*
 * Find a list member of the form c1c2*x such
 * that c1c2 divides no other such constant, is divided by
 * at least one other (say in the form c1*y), and which has
 * fewest divisors. Reduce this pair to c1*(y+c2*x)
 * and iterate until no reductions occur.
 */
	register struct tnode **p1, **p2;
	struct tnode *t;
	int ndmaj, ndmin;
	struct tnode **dividend, **divisor;
	struct tnode **maxnod, **mindiv;

    loop:
	maxnod = &list->llist[list->nextl];
	ndmaj = 1000;
	dividend = 0;
	for (p1 = list->llist; p1 <= maxnod; p1++) {
		if ((*p1)->op!=TIMES || (*p1)->tr2->op!=CON)
			continue;
		ndmin = 0;
		for (p2 = list->llist; p2 <= maxnod; p2++) {
			if (p1==p2 || (*p2)->op!=TIMES || (*p2)->tr2->op!=CON)
				continue;
			if ((*p1)->tr2->value == (*p2)->tr2->value) {
				(*p2)->tr2 = (*p1)->tr1;
				(*p2)->op = PLUS;
				(*p1)->tr1 = (*p2);
				*p1 = optim(*p1);
				squash(p2, maxnod);
				list->nextl--;
				goto loop;
			}
			if (((*p2)->tr2->value % (*p1)->tr2->value) == 0)
				goto contmaj;
			if (((*p1)->tr2->value % (*p2)->tr2->value) == 0) {
				ndmin++;
				mindiv = p2;
			}
		}
		if (ndmin > 0 && ndmin < ndmaj) {
			ndmaj = ndmin;
			dividend = p1;
			divisor = mindiv;
		}
    contmaj:;
	}
	if (dividend==0)
		return;
	t = list->nlist[--list->nextn];
	p1 = dividend;
	p2 = divisor;
	t->op = PLUS;
	t->type = (*p1)->type;
	t->tr1 = (*p1);
	t->tr2 = (*p2)->tr1;
	(*p1)->tr2->value =/ (*p2)->tr2->value;
	(*p2)->tr1 = t;
	t = optim(*p2);
	if (p1 < p2) {
		*p1 = t;
		squash(p2, maxnod);
		list->nextl--;
		goto loop;
	}
	*p2 = t;
	squash(p1, maxnod);
	list->nextl--;
	goto loop;
}

squash(p, maxp)
struct tnode **p, **maxp;
{
	register struct tnode **np;

	for (np = p; np < maxp; np++)
		*np = *(np+1);
}

const(op, vp, av)
int *vp;
{
	register int v;

	v = av;
	switch (op) {

	case PLUS:
		*vp =+ v;
		return;

	case TIMES:
		*vp =* v;
		return;

	case AND:
		*vp =& v;
		return;

	case OR:
		*vp =| v;
		return;

	case EXOR:
		*vp =^ v;
		return;

	case DIVIDE:
	case MOD:
		if (v==0)
			error("Divide check");
		else
			if (op==DIVIDE)
				*vp =/ v;
			else
				*vp =% v;
		return;

	case RSHIFT:
		*vp =>> v;
		return;

	case LSHIFT:
		*vp =<< v;
		return;
	}
	error("C error: const");
}

insert(op, atree, alist)
struct acl *alist;
{
	register d;
	register struct acl *list;
	register struct tnode *tree;
	int d1, i;
	struct tnode *t;

	tree = atree;
	list = alist;
	if (tree->op == op) {
	ins:	list->nlist[list->nextn++] = tree;
		insert(op, tree->tr1, list);
		insert(op, tree->tr2, list);
		return;
	}
	tree = optim(tree);
	if (tree->op == op)
		goto ins;
	if (!isfloat(tree)) {
		/* c1*(x+c2) -> c1*x+c1*c2 */
		if ((tree->op==TIMES||tree->op==LSHIFT) && tree->tr2->op==CON
		  && tree->tr1->op==PLUS && tree->tr1->tr2->op==CON) {
			d = tree->tr2->value;
			if (tree->op==TIMES)
				tree->tr2->value =* tree->tr1->tr2->value;
			else
				tree->tr2->value = tree->tr1->tr2->value << d;
			tree->tr1->tr2->value = d;
			tree->tr1->op = tree->op;
			tree->op = PLUS;
			if (op==PLUS)
				goto ins;
		}
	}
	d = degree(tree);
	for (i=0; i<list->nextl; i++) {
		if ((d1=degree(list->llist[i]))<d) {
			t = list->llist[i];
			list->llist[i] = tree;
			tree = t;
			d = d1;
		}
	}
	list->llist[list->nextl++] = tree;
}

putchar(c)
{
	putc(c&0177, outbuf);
}
