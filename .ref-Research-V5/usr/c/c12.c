#
/*
 *		C compiler part 2 -- expression optimizer
 *
 *	Copyright 1972, 1973, 1974  Bell Telephone Laboratories
 */

#include "c1h.c"

optim(atree)
struct tnode *atree;
{
	register op, dope;
	int d1, d2;
	struct tnode *t;
	register struct tnode *tree;

	if ((tree=atree)==0)
		return(0);
	if ((op = tree->op)==0)
		return(tree);
	if (op==NAME && tree->class==AUTO) {
		tree->class = OFFS;
		tree->regno = 5;
		tree->offset = tree->nloc;
	}
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
		if ((d1=degree(tree->tr1)) < (d2=degree(tree->tr2))
		 || d1==d2 && tree->tr1->op==NAME && tree->tr2->op!=NAME) {
			t = tree->tr1;
			tree->tr1 = tree->tr2;
			tree->tr2 = t;
			tree->op = maprel[op-EQUAL];
		}
		if (tree->tr1->type==CHAR && tree->tr2->op==CON
		 && (dcalc(tree->tr1) <= 12 || tree->tr1->op==STAR)
		 && tree->tr2->value <= 127 && tree->tr2->value >= 0)
			tree->tr2->type = CHAR;
	}
	d1 = max(degree(tree->tr1), 1);
	d2 = max(degree(tree->tr2), 0);
	switch (op) {

	case ASSAND:
		if (tree->tr2->op == COMPL) {
			tree->tr2 = tree->tr2->tr1;
			d2 = max(degree(tree->tr2), 0);
			tree->op = ASSNAND;
		}
		break;

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
		if (tree->tr2->op==CON && tree->tr2->value==1)
			return(tree->tr1);
		if (ispow2(tree) == 0) {

		case MOD:
		case ASMOD:
			d1 =+ 2;
			d2 =+ 2;
		}
		goto constant;

	case LSHIFT:
	case RSHIFT:
	case ASRSH:
	case ASLSH:
		if (tree->tr2->op==CON && tree->tr2->value==0)
			return(tree->tr1);

	constant:
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
	/* reduce & * */
	if (tree->op==AMPER) {
		if (subtre->op==STAR)
			return(subtre->tr1);
		if (subtre->op==NAME && subtre->class == OFFS) {
			p = block(2, PLUS, tree->type, 1, subtre, tree);
			subtre->type = tree->type;
			tree->op = CON;
			tree->type = INT;
			tree->degree = 0;
			tree->value = subtre->offset;
			subtre->class = REG;
			subtre->nloc = subtre->regno;
			subtre->offset = 0;
			return(p);
		}
	}
	/* try to reduce * & */
	if (tree->op==STAR) {
		if (subtre->op==AMPER)
			return(subtre->tr1);
		if (subtre->op==NAME && subtre->class==REG) {
			subtre->type = tree->type;
			subtre->class = OFFS;
			subtre->regno = subtre->nloc;
			return(subtre);
		}
		p = subtre->tr1;
		if ((subtre->op==INCAFT || subtre->op==DECBEF)
		 && p->op==NAME && p->class==REG && p->type==subtre->type) {
			p->type = tree->type;
			p->op = subtre->op==INCAFT? AUTOI: AUTOD;
			return(p);
		}
		if (subtre->op==PLUS && p->op==NAME && p->class==REG) {
			if (subtre->tr2->op==CON) {
				p->offset =+ subtre->tr2->value;
				p->class = OFFS;
				p->type = tree->type;
				p->regno = p->nloc;
				return(p);
			}
			if (subtre->tr2->op==AMPER) {
				subtre = subtre->tr2->tr1;
				subtre->class =+ XOFFS-EXTERN;
				subtre->regno = p->nloc;
				subtre->type = tree->type;
				return(subtre);
			}
		}
	}
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
	t2 = &acl.llist[acl.nextl];
	if (!flt) {
		/* put constants together */
		for (i=acl.nextl;i>0&&t2[0]->op==CON&&t2[-1]->op==CON;i--) {
			acl.nextl--;
			t2--;
			const(op, &t2[0]->value, t2[1]->value);
		}
	}
	if (op==PLUS) {
		/* toss out "+0" */
		if (acl.nextl>0 && ((*t2)->op==CON || (*t2)->op==SFCON)
		 && (*t2)->value==0) {
			acl.nextl--;
			t2--;
		}
		if (acl.nextl <= 0)
			return(*t2);
		/* subsume constant in "&x+c" */
		if (t2[0]->op==CON && t2[-1]->op==AMPER) {
			t2--;
			t2[0]->tr1->offset =+ t2[1]->value;
			acl.nextl--;
		}
	} else if (op==TIMES) {
		t1 = acl.llist[acl.nextl];
		if (t1->op==CON) {
			if (t1->value==0)
				return(t1);
			if (t1->value==1 && acl.nextl>0)
				if (--acl.nextl <= 0)
					return(acl.llist[0]);
		}
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
	} else {
		*p2 = t;
		squash(p1, maxnod);
	}
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

block(an, args)
{
	register int *p;
	int *oldp;
	register *argp, n;

	oldp = p = spacep;
	n = an+3;
	argp = &args;
	do
		*p++ = *argp++;
	while (--n);
	if (p >= spacemax) {
		error("Exp. ov. pass 2");
		exit(1);
	}
	spacep = p;
	return(oldp);
}
