#
/*

	    	C compiler, part 2

	Copyright 1972 Bell Telephone Laboratories, Inc.

*/

#include "c1h.c"

char	maprel[] {	EQUAL, NEQUAL, GREATEQ, GREAT, LESSEQ,
			LESS, GREATQP, GREATP, LESSEQP, LESSP
};

char	notrel[] {	NEQUAL, EQUAL, GREAT, GREATEQ, LESS,
			LESSEQ, GREATP, GREATQP, LESSP, LESSEQP
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
struct tnode	*xdel[2];

main(argc, argv)
char *argv[];
{
	int treespace[ossiz];
	struct table *table;
	register *sp, c, *tree;
	extern fout;

	if (argc<4) {
		error("Arg count");
		exit(1);
	}
	if(fopen(argv[1], ascbuf)<0 || fopen(argv[2], binbuf)<0){
		error("Missing temp file");
		exit(1);
	}
	if ((fout = creat(argv[3], 0666)) < 0) {
		error("Can't create %s", argv[3]);
		exit(1);
	}
	treebase = getw(binbuf);
	if (treebase < treespace) {
		error("Tree space botch");
		exit(1);
	}
	spacemax = &treespace[ossiz];
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
				spacep = sp;
				tree = optim(tree);
				nstack = 0;
				rcexpr(tree, table, 0);
			}
		} else
			putchar(c);
	}
	if (nfloat)
		printf(".globl	fltused\n");
	flush();
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
		if (notcompat(p1, opt->tabtyp1, op)) {
			continue;
		}
		if ((opdope[op]&BINARY)!=0 && p2!=0) {
			if (d2 > (opt->tabdeg2&077)
			 || (opt->tabdeg2 >= 0100) && (p2->op != STAR) )
				continue;
			if (notcompat(p2,opt->tabtyp2, 0))
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
	int modf, nargs;
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

	case CALL:
		r = 0;
		nargs = 0;
		modf = 0;
		if (tree->tr1->op!=NAME) {	/* get nargs right */
			nargs++;
			nstack++;
		}
		tree = tree->tr2;
		if(tree->op) {
			while (tree->op==COMMA) {
				r =+ comarg(tree->tr2, &modf);
				tree = tree->tr1;
				nargs++;
			}
			r =+ comarg(tree, &modf);
			nargs++;
		}
		tree = atree;
		tree->op = CALL2;
		if (modf && tree->tr1->op==NAME && tree->tr1->class==EXTERN)
			tree->op = CALL1;
		cexpr(tree, regtab, reg);
		popstk(r);
		nstack =- nargs;
		if (table==efftab || table==regtab)
			return(0);
		r = 0;
		xdel[0] = 0;
		xdel[1] = 0;
		goto fixup;

	case TIMES:
	case DIVIDE:
	case ASTIMES:
	case ASDIV:
		pow2(tree);
	}
	modf = 100;
	tree = reorder(tree, reg, &modf, 0);
	if (modf!=100)
		tree = optim(tree);
	if (table==efftab && tree->op==NAME)
		return(reg);
	if ((r=cexpr(tree, table, reg))>=0)
		return(r);
	if (table!=regtab)  {
		if((r=cexpr(tree, regtab, reg))>=0) {
	fixup:
			modf = isfloat(tree);
			if (table==sptab || table==lsptab) {
				printf("mov%c	r%d,%c(sp)\n", modf, r,
					table==sptab? '-':0);
				nstack++;
			}
			atree = xdel[1];
			xdel[1] = 0;
			if (xdel[0]) {
				tree = xdel[0];
				xdel[0] = 0;
				rcexpr(tree, efftab, 0);
			}
			if (atree)
				rcexpr(atree, efftab, 0);
			if (table==cctab)
				printf("tst%c	r%d\n", modf, r);
			return(r);
		}
	}
	error("No match for op %d", tree->op);
	return(reg);
}
struct table *cregtab;

cexpr(atree, table, areg)
struct tnode *atree;
struct table *table;
{
	int c, r;
	register struct tnode *p, *p1, *tree;
	struct table *ctable;
	struct tnode *p2;
	char *string;
	int reg, reg1, rreg, flag, opd;
	char *opt;
	struct tnode *del[2];

	tree = atree;
	del[0] = 0;
	del[1] = 0;
	reg = areg;
	p1 = tree->tr2;
	c = tree->op;
	opd = opdope[c];
	if ((opd&RELAT||c==LOGAND||c==LOGOR||c==EXCLA) && table!=cctab) {
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
	reg = oddreg(tree, reg);
	reg1 = reg+1;
	if ((r = chkleaf(tree, table, reg)) >= 0)
		return(r);
	r = 0;
	if (table==cctab || table==cregtab)
		r++;
	for (;;) {
		flag = 0;
		if ((opd & LEAF) == 0)
			p1 = tree->tr1 = reorder(tree->tr1, areg, &flag,
			    r?0: &del[0]);
		p2 = 0;
		if (opd&BINARY)
			p2 = tree->tr2 = reorder(tree->tr2, areg, &flag,
			    r?0: &del[1]);
		if (flag==0)
			break;
		if (flag > 1 && (opd&RELAT) && p2->op==CON
		 && p2->value==0 && r && opdope[p1->op]&LEAF
		 && del[0]==0 && del[1]==0)
			goto retrn;
		tree = optim(tree);
	}
	if ((tree->op==PLUS||tree->op==ASPLUS) &&
	    p2->op == CON && p2->value == -1) {
		p2->value = 1;
		tree->op++;		/* +, =+ to -, =- */
	}
	if (table==cregtab)
		table = regtab;
	if (table!=cctab || c==INCAFT || c==DECAFT
	 || (opt = match(tree, efftab, nreg-reg)) == 0)
		if ((opt=match(tree, table, nreg-reg))==0) {
			xdel[0] = del[0];
			xdel[1] = del[1];
			return(-1);
		}
	string = opt->tabstring;
loop:
	if ((c = *string++) & 0200) {
		c =& 0177;
		putchar('\t');
	}
	switch (c) {

	case '\0':
	retrn:
		if (del[0])
			rcexpr(del[0], efftab, 0);
		if (del[1])
			rcexpr(del[1], efftab, 0);
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
		if ((opd&LEAF) != 0)
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
		printf("%o", p);
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
			ctable = cctab;
		if ((flag&01) && ctable==regtab && (c&01)==0
		  && (tree->op==DIVIDE||tree->op==MOD
		   || tree->op==ASDIV||tree->op==ASMOD))
			ctable = cregtab;
		if ((c&01)!=0) {
			p = p->tr1;
			if(collcon(p) && ctable!=sptab) {
				if (p->op==STAR)
					p = p->tr1;
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
		if (ctable!=regtab && ctable!=cregtab)
			goto loop;
		if (c&010)
			reg1 = rreg;
		else if (rreg!=reg)
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

	/* #1 */
	case '#':
		p = p1->tr1;
		goto nmbr;

	/* #2 */
	case '"':
		p = p2->tr1;

	nmbr:
		if(collcon(p)) {
			if (p->op==STAR) {
				printf("*");
				p = p->tr1;
			}
			if ((p = p->tr2)->op == CON) {
				if (p->value)
					psoct(p->value);
			} else if (p->op==AMPER)
				pname(p->tr1, 0);
		}
		goto loop;

	/* V */
	case 'V':
		tree->op = maprel[tree->op - EQUAL];
		goto loop;

	case '^':		/* for ++ --, tr2 is length */
		printf("%o", tree->tr2);
		goto loop;

	case 'T':		/* "tst R" if 1st op not in cctab */
		if (dcalc(p1, 5)>12 && !match(p1, cctab, 10))
			printf("tst	r%d\n", reg);
		goto loop;
	}
	putchar(c);
	goto loop;
}

reorder(ap, reg, afp, delp)
struct tnode *ap;
int *afp, *delp;
{
	register struct tnode *p, *p1;
	register int *fp;

	p = ap;
	if (opdope[p->op]&LEAF)
		return(p);
	fp = afp;
	p1 = p->tr1;
	if (p->op==STAR || p->op==PLUS) {
		p->tr1 = reorder(p1, reg, fp, delp);
		if (p->op==PLUS)
			p->tr2 = reorder(p->tr2, reg, fp, delp);
		if (*fp)
			*fp = 1;
		return(p);
	}
	if (p1->op==NAME) switch(p->op) {
		case ASLSH:
		case ASRSH:
		case ASSIGN:
			if (p1->class != REG || isfloat(p->tr2))
				break;
			if (p->op==ASSIGN) switch (p->tr2->op) {
			case TIMES:
			case DIVIDE:
				if (!ispow2(p->tr2))
					break;
				pow2(p->tr2);
			case PLUS:
			case MINUS:
			case AND:
			case OR:
			case EXOR:
			case LSHIFT:
			case RSHIFT:
				p1 = p->tr2->tr2;
				if (xdcalc(p1) > 12
				 || p1->op==NAME
				 &&(p1->nloc==p->tr1->nloc
				  || p1->regno==p->tr1->nloc))
					return(p);
				p1 = p->tr2;
				p->tr2 = p1->tr1;
				if (p1->tr1->op!=NAME
				 || p1->tr1->class!=REG
				 || p1->tr1->nloc!=p->tr1->nloc)
					rcexpr(p, efftab, reg);
				p->tr2 = p1->tr2;
				p->op = p1->op + ASPLUS - PLUS;
				(*fp) = 2;
				return(p);
			}
			goto OK;

		case INCAFT:
		case DECAFT:
			if (delp && *fp < 100) {
				if (p1->op==NAME && p1->class==REG)
					p1 = block(3, p1->op,p1->type,p1->elsize,
					  p1->tr1,p1->offset,p1->nloc);
				*delp = p;
				*fp = 1;
				return(p1);
			}
			break;

		case ASTIMES:
		case ASDIV:
			if (!ispow2(p))
				break;
		case ASPLUS:
		case ASMINUS:
		case ASSAND:
		case ASOR:
		case ASXOR:
		case DECBEF:
		case INCBEF:
		OK:
			if (*fp >= 100)
				break;
			rcexpr(p, delp?efftab:cctab, reg);
			(*fp) = 2;
			return(p1);
	}
	return(p);
}

chkleaf(atree, table, reg)
struct tnode *atree;
{
	struct tnode lbuf;
	register struct tnode *tree;

	tree = atree;
	if (tree->op!=STAR && dcalc(tree, nreg-reg) > 12)
		return(-1);
	lbuf.op = LOAD;
	lbuf.type = tree->type;
	lbuf.degree = tree->degree;
	lbuf.tr1 = tree;
	return(rcexpr(&lbuf, table, reg));
}

comarg(atree, flagp)
int *flagp;
{
	static rathole;
	register struct tnode *tree;
	struct tnode *pmp;
	register retval;

	pmp = 0;
	tree = reorder(atree, 0, &rathole, &pmp);
	if (tree->type==STRUCT)
		error("Illegal structure");
	if (nstack || isfloat(tree)) {
		rcexpr(tree, sptab, 0);
		retval = arlength(tree->type);
	} else {
		(*flagp)++;
		rcexpr(tree, lsptab, 0);
		retval = 0;
	}
	if (pmp)
		rcexpr(pmp, efftab, 0);
	return(retval);
}
