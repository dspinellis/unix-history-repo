#
/*
 * C compiler
 *
 *
 */

#include "c0h.c"

/*
 * Called from tree, this routine takes the top 1, 2, or 3
 * operands on the expression stack, makes a new node with
 * the operator op, and puts it on the stack.
 * Essentially all the work is in inserting
 * appropriate conversions.
 */
build(op) {
	register int t1;
	int t2, t3, t;
	struct tnode *p3, *disarray();
	register struct tnode *p1, *p2;
	int d, dope, leftc, cvn, pcvn;

	/*
	 * a[i] => *(a+i)
	 */
	if (op==LBRACK) {
		build(PLUS);
		op = STAR;
	}
	dope = opdope[op];
	if ((dope&BINARY)!=0) {
		p2 = chkfun(disarray(*--cp));
		t2 = p2->type;
	}
	p1 = *--cp;
	/*
	 * sizeof gets turned into a number here.
	 * Bug: sizeof(structure-member-array) is 2 because
	 * the array has been turned into a ptr already.
	 */
	if (op==SIZEOF) {
		t1 = length(p1);
		p1->op = CON;
		p1->type = INT;
		p1->dimp = 0;
		p1->value = t1;
		*cp++ = p1;
		return;
	}
	if (op!=AMPER) {
		p1 = disarray(p1);
		if (op!=CALL)
			p1 = chkfun(p1);
	}
	t1 = p1->type;
	pcvn = 0;
	t = INT;
	switch (op) {

	/* end of expression */
	case 0:
		*cp++ = p1;
		return;

	/* no-conversion operators */
	case QUEST:
		if (p2->op!=COLON)
			error("Illegal conditional");
		t = t2;

	case COMMA:
	case LOGAND:
	case LOGOR:
		*cp++ = block(2, op, t, 0, p1, p2);
		return;

	case CALL:
		if ((t1&XTYPE) != FUNC)
			error("Call of non-function");
		*cp++ = block(2,CALL,decref(t1),p1->dimp,p1,p2);
		return;

	case STAR:
		if (p1->op==AMPER ) {
			*cp++ = p1->tr1;
			return;
		}
		if ((t1&XTYPE) == FUNC)
			error("Illegal indirection");
		*cp++ = block(1,STAR,decref(t1),p1->dimp,p1);
		return;

	case AMPER:
		if (p1->op==STAR) {
			p1->tr1->dimp = p1->dimp;
			p1->tr1->type = incref(t1);
			*cp++ = p1->tr1;
			return;
		}
		if (p1->op==NAME) {
			*cp++ = block(1,op,incref(t1),p1->dimp,p1);
			return;
		}
		error("Illegal lvalue");
		break;

	/*
	 * a->b goes to (*a).b
	 */
	case ARROW:
		*cp++ = p1;
		chkw(p1, -1);
		p1->type = PTR+STRUCT;
		build(STAR);
		p1 = *--cp;

	/*
	 * In a.b, a fairly complicated process has to
	 * be used to make the left operand look
	 * as if it had the type of the second.
	 * Also, the offset in the structure has to be
	 * given a special type to prevent conversion.
	 */
	case DOT:
		if (p2->op!=NAME || (p2->class!=MOS && p2->class!=FMOS))
			error("Illegal structure ref");
		*cp++ = p1;
		t = t2;
		if ((t&XTYPE) == ARRAY) {
			t = decref(t);
			p2->ssp++;
		}
		setype(p1, t, p2->dimp);
		build(AMPER);
		*cp++ = block(1,CON,NOTYPE,0,p2->nloc);
		build(PLUS);
		if ((t2&XTYPE) != ARRAY)
			build(STAR);
		if (p2->class == FMOS)
			*cp++ = block(2, FSEL, t, 0, *--cp, p2->dimp);
		return;
	}
	if ((dope&LVALUE)!=0)
		chklval(p1);
	if ((dope&LWORD)!=0)
		chkw(p1, LONG);
	if ((dope&RWORD)!=0)
		chkw(p2, LONG);
	if ((dope&BINARY)==0) {
		if (op==ITOF)
			t1 = DOUBLE;
		else if (op==FTOI)
			t1 = INT;
		if (!fold(op, p1, 0))
			*cp++ = block(1,op,t1,p1->dimp,p1);
		return;
	}
	cvn = 0;
	if (t1==STRUCT || t2==STRUCT) {
		error("Unimplemented structure operation");
		t1 = t2 = INT;
	}
	if (t2==NOTYPE) {
		t = t1;
		p2->type = INT;	/* no int cv for struct */
		t2 = INT;
	} else
		cvn = cvtab[lintyp(t1)][lintyp(t2)];
	leftc = (cvn>>4)&017;
	cvn =& 017;
	t = leftc? t2:t1;
	if (dope&ASSGOP) {
		t = t1;
		if (op==ASSIGN && (cvn==ITP||cvn==PTI))
			cvn = leftc = 0;
		if (leftc)
			cvn = leftc;
		leftc = 0;
	} else if (op==COLON && t1>=PTR && t1==t2)
		cvn = 0;
	else if (dope&RELAT) {
		if (op>=LESSEQ && (t1>=PTR || t2>=PTR))
			op =+ LESSEQP-LESSEQ;
		if (cvn==PTI)
			cvn = 0;
	}
	if (cvn==PTI) {
		cvn = 0;
		if (op==MINUS) {
			t = INT;
			pcvn++;
		} else {
			if (t1!=t2 || t1!=(PTR+CHAR))
				cvn = XX;
		}
	}
	if (cvn) {
		t1 = plength(p1);
		t2 = plength(p2);
		if (cvn==XX || (cvn==PTI&&t1!=t2))
			error("Illegal conversion");
		else if (leftc)
			p1 = convert(p1, t, cvn, t2);
		else
			p2 = convert(p2, t, cvn, t1);
	}
	if (dope&RELAT)
		t = INT;
	if (fold(op, p1, p2)==0)
		*cp++ = block(2,op,t,(p1->dimp==0? p2:p1)->dimp,p1,p2);
	if (pcvn && t1!=(PTR+CHAR)) {
		p1 = *--cp;
		*cp++ = convert(p1, 0, PTI, plength(p1->tr1));
	}
}

/*
 * Generate the appropirate conversion operator.
 * For pointer <=> integer this is a multiplication
 * or division, otherwise a special operator.
 */
convert(p, t, cvn, len)
struct tnode *p;
{
	register int n;

	switch(cvn) {

	case PTI:
	case ITP:
		if (len==1)
			return(p);
		return(block(2, (cvn==PTI?DIVIDE:TIMES), t, 0, p,
			block(1, CON, 0, 0, len)));

	case ITF:
		n = ITOF;
		break;
	case FTI:
		n = FTOI;
		break;
	case ITL:
		n = ITOL;
		break;
	case LTI:
		n = LTOI;
		break;
	case FTL:
		n = FTOL;
		break;
	case LTF:
		n = LTOF;
		break;
	}
	return(block(1, n, t, 0, p));
}

/*
 * Traverse an expression tree, adjust things
 * so the types of things in it are consistent
 * with the view that its top node has
 * type at.
 * Used with structure references.
 */
setype(ap, at, adimptr)
struct tnode *ap;
{
	register struct tnode *p;
	register t, dimptr;

	p = ap;
	t = at;
	dimptr = adimptr;
	p->type = t;
	if (dimptr != -1)
		p->dimp = dimptr;
	switch(p->op) {

	case AMPER:
		setype(p->tr1, decref(t), dimptr);
		return;

	case STAR:
		setype(p->tr1, incref(t), dimptr);
		return;

	case PLUS:
	case MINUS:
		setype(p->tr1, t, dimptr);
	}
}

/*
 * A mention of a function name is turned into
 * a pointer to that function.
 */
chkfun(ap)
struct tnode *ap;
{
	register struct tnode *p;
	register int t;

	p = ap;
	if (((t = p->type)&XTYPE)==FUNC)
		return(block(1,AMPER,incref(t),p->dimp,p));
	return(p);
}

/*
 * A mention of an array is turned into
 * a pointer to the base of the array.
 */
struct tnode *disarray(ap)
struct tnode *ap;
{
	register int t;
	register struct tnode *p;

	p = ap;
	/* check array & not MOS */
	if (((t = p->type)&XTYPE)!=ARRAY || p->op==NAME&&p->class==MOS)
		return(p);
	p->ssp++;
	*cp++ = p;
	setype(p, decref(t), -1);
	build(AMPER);
	return(*--cp);
}

/*
 * make sure that p is a ptr to a node
 * with type int or char or 'okt.'
 * okt might be nonexistent or 'long'
 * (e.g. for <<).
 */
chkw(p, okt)
struct tnode *p;
{
	register int t;

	if ((t=p->type)>CHAR && t<PTR && t!=okt)
		error("Integer operand required");
	return;
}

/*
 *'linearize' a type for looking up in the
 * conversion table
 */
lintyp(t)
{
	switch(t) {

	case INT:
	case CHAR:
		return(0);

	case FLOAT:
	case DOUBLE:
		return(1);

	case LONG:
		return(2);

	default:
		return(3);
	}
}

/*
 * Report an error.
 */
error(s, p1, p2, p3, p4, p5, p6)
{
	nerror++;
	printf("%d: ", line);
	printf(s, p1, p2, p3, p4, p5, p6);
	printf("\n");
}

/*
 * Generate a node in an expression tree,
 * setting the operator, type, degree (unused in this pass)
 * and the operands.
 */
block(an, op, t, d, p1,p2,p3)
int *p1, *p2, *p3;
{
	register int *ap, *p, n;
	int *oldp;

	n = an+3;
	p = gblock(n);
	oldp = p;
	ap = &op;
	do {
		*p++ = *ap++;
	} while (--n);
	return(oldp);
}

/*
 * Assign an unitialized block for use in the
 * expression tree.
 */
gblock(n)
{
	register int *p;

	p = space;
	if ((space =+ n) >= &osspace[OSSIZ]) {
		error("Expression overflow");
		exit(1);
	}
	return(p);
}

/*
 * Check that a tree can be used as an lvalue.
 */
chklval(ap)
struct tnode *ap;
{
	register struct tnode *p;

	p = ap;
	if (p->op!=NAME && p->op!=STAR)
		error("Lvalue required");
}

/*
 * reduce some forms of `constant op constant'
 * to a constant.  More of this is done in the next pass
 * but this is used to allow constant expressions
 * to be used in switches and array bounds.
 */
fold(op, ap1, ap2)
struct tnode *ap1, *ap2;
{
	register struct tnode *p1;
	register int v1, v2;

	p1 = ap1;
	if (p1->op!=CON || (ap2!=0 && ap2->op!=CON))
		return(0);
	v1 = p1->value;
	v2 = ap2->value;
	switch (op) {

	case PLUS:
		v1 =+ v2;
		break;

	case MINUS:
		v1 =- v2;
		break;

	case TIMES:
		v1 =* v2;
		break;

	case DIVIDE:
		v1 =/ v2;
		break;

	case MOD:
		v1 =% v2;
		break;

	case AND:
		v1 =& v2;
		break;

	case OR:
		v1 =| v2;
		break;

	case EXOR:
		v1 =^ v2;
		break;

	case NEG:
		v1 = - v1;
		break;

	case COMPL:
		v1 = ~ v1;
		break;

	case LSHIFT:
		v1 =<< v2;
		break;

	case RSHIFT:
		v1 =>> v2;
		break;

	default:
		return(0);
	}
	p1->value = v1;
	*cp++ = p1;
	return(1);
}

/*
 * Compile an expression expected to have constant value,
 * for example an array bound or a case value.
 */
conexp()
{
	register struct tnode *t;

	initflg++;
	if (t = tree())
		if (t->op != CON)
			error("Constant required");
	initflg--;
	return(t->value);
}
