#
/*
 * C compiler
 *
 *
 */

#include "c0.h"

/*
 * Reduce the degree-of-reference by one.
 * e.g. turn "ptr-to-int" into "int".
 */
decref(at)
{
	register t;

	t = at;
	if ((t & ~TYPE) == 0) {
		error("Illegal indirection");
		return(t);
	}
	return((t>>TYLEN) & ~TYPE | t&TYPE);
}

/*
 * Increase the degree of reference by
 * one; e.g. turn "int" to "ptr-to-int".
 */
incref(t)
{
	return(((t&~TYPE)<<TYLEN) | (t&TYPE) | PTR);
}

/*
 * Make a tree that causes a branch to lbl
 * if the tree's value is non-zero together with the cond.
 */
cbranch(t, lbl, cond)
struct tnode *t;
{
	treeout(t, 0);
	outcode("BNNN", CBRANCH, lbl, cond, line);
}

/*
 * Write out a tree.
 */
rcexpr(atp)
struct tnode *atp;
{
	register struct tnode *tp;

	/*
	 * Special optimization
	 */
	if ((tp=atp)->op==INIT && tp->tr1->op==CON) {
		if (tp->type==CHAR) {
			outcode("B1N0", BDATA, tp->tr1->value);
			return;
		} else if (tp->type==INT || tp->type==UNSIGN) {
			outcode("BN", SINIT, tp->tr1->value);
			return;
		}
	}
	treeout(tp, 0);
	outcode("BN", EXPR, line);
}

treeout(atp, isstruct)
struct tnode *atp;
{
	register struct tnode *tp;
	register struct hshtab *hp;
	register nextisstruct;

	if ((tp = atp) == 0) {
		outcode("B", NULLOP);
		return;
	}
	nextisstruct = tp->type==STRUCT;
	switch(tp->op) {

	case NAME:
		hp = tp->tr1;
		if (hp->hclass==TYPEDEF)
			error("Illegal use of type name");
		outcode("BNN", NAME, hp->hclass==0?STATIC:hp->hclass, tp->type);
		if (hp->hclass==EXTERN)
			outcode("S", hp->name);
		else
			outcode("N", hp->hoffset);
		break;

	case LCON:
		outcode("BNNN", tp->op, tp->type, tp->lvalue);
		break;

	case CON:
		outcode("BNN", tp->op, tp->type, tp->value);
		break;

	case FCON:
		outcode("BNF", tp->op, tp->type, tp->cstr);
		break;

	case STRING:
		outcode("BNNN", NAME, STATIC, tp->type, tp->tr1);
		break;

	case FSEL:
		treeout(tp->tr1, nextisstruct);
		outcode("BNNN",tp->op,tp->type,tp->tr2->bitoffs,tp->tr2->flen);
		break;

	case ETYPE:
		error("Illegal use of type");
		break;

	case AMPER:
		treeout(tp->tr1, 1);
		outcode("BN", tp->op, tp->type);
		break;


	case CALL:
		treeout(tp->tr1, 1);
		treeout(tp->tr2, 0);
		outcode("BN", CALL, tp->type);
		break;

	default:
		treeout(tp->tr1, nextisstruct);
		if (opdope[tp->op]&BINARY)
			treeout(tp->tr2, nextisstruct);
		outcode("BN", tp->op, tp->type);
		break;
	}
	if (nextisstruct && isstruct==0)
		outcode("BNN", STRASG, STRUCT, tp->strp->ssize);
}

/*
 * Generate a branch
 */
branch(lab)
{
	outcode("BN", BRANCH, lab);
}

/*
 * Generate a label
 */
label(l)
{
	outcode("BN", LABEL, l);
}

/*
 * ap is a tree node whose type
 * is some kind of pointer; return the size of the object
 * to which the pointer points.
 */
plength(ap)
struct tname *ap;
{
	register t, l;
	register struct tnode *p;

	p = ap;
	if (p==0 || ((t=p->type)&~TYPE) == 0)		/* not a reference */
		return(1);
	p->type = decref(t);
	l = length(p);
	p->type = t;
	return(l);
}

/*
 * return the number of bytes in the object
 * whose tree node is acs.
 */
length(acs)
struct tnode *acs;
{
	register t, elsz;
	long n;
	register struct tnode *cs;
	int nd;

	cs = acs;
	t = cs->type;
	n = 1;
	nd = 0;
	while ((t&XTYPE) == ARRAY) {
		t = decref(t);
		n =* cs->subsp[nd++];
	}
	if ((t&~TYPE)==FUNC)
		return(0);
	if (t>=PTR)
		elsz = SZPTR;
	else switch(t&TYPE) {

	case INT:
	case UNSIGN:
		elsz = SZINT;
		break;

	case CHAR:
		elsz = 1;
		break;

	case FLOAT:
		elsz = SZFLOAT;
		break;

	case LONG:
		elsz = SZLONG;
		break;

	case DOUBLE:
		elsz = SZDOUB;
		break;

	case STRUCT:
		if ((elsz = cs->strp->ssize) == 0)
			error("Undefined structure");
		break;
	default:
		error("Compiler error (length)");
		return(0);
	}
	n *= elsz;
	if (n >= (unsigned)50000) {
		error("Warning: very large data structure");
		nerror--;
	}
	return(n);
}

/*
 * The number of bytes in an object, rounded up to a word.
 */
rlength(cs)
struct tnode *cs;
{
	return((length(cs)+ALIGN) & ~ALIGN);
}

/*
 * After an "if (...) goto", look to see if the transfer
 * is to a simple label.
 */
simplegoto()
{
	register struct hshtab *csp;

	if ((peeksym=symbol())==NAME && nextchar()==';') {
		csp = csym;
		if (csp->hblklev == 0)
			pushdecl(csp);
		if (csp->hclass==0 && csp->htype==0) {
			csp->htype = ARRAY;
			csp->hflag =| FLABL;
			if (csp->hoffset==0)
				csp->hoffset = isn++;
		}
		if ((csp->hclass==0||csp->hclass==STATIC)
		 &&  csp->htype==ARRAY) {
			peeksym = -1;
			return(csp->hoffset);
		}
	}
	return(0);
}

/*
 * Return the next non-white-space character
 */
nextchar()
{
	while (spnextchar()==' ')
		peekc = 0;
	return(peekc);
}

/*
 * Return the next character, translating all white space
 * to blank and handling line-ends.
 */
spnextchar()
{
	register c;

	if ((c = peekc)==0)
		c = getchar();
	if (c=='\t' || c=='\014')	/* FF */
		c = ' ';
	else if (c=='\n') {
		c = ' ';
		if (inhdr==0)
			line++;
		inhdr = 0;
	} else if (c=='\001') {	/* SOH, insert marker */
		inhdr++;
		c = ' ';
	}
	peekc = c;
	return(c);
}

/*
 * is a break or continue legal?
 */
chconbrk(l)
{
	if (l==0)
		error("Break/continue error");
}

/*
 * The goto statement.
 */
dogoto()
{
	register struct tnode *np;

	*cp++ = tree();
	build(STAR);
	chkw(np = *--cp, -1);
	rcexpr(block(JUMP,0,NULL,NULL,np));
}

/*
 * The return statement, which has to convert
 * the returned object to the function's type.
 */
doret()
{
	register struct tnode *t;

	if (nextchar() != ';') {
		t = tree();
		*cp++ = &funcblk;
		*cp++ = t;
		build(ASSIGN);
		cp[-1] = cp[-1]->tr2;
		if (funcblk.type==CHAR)
			cp[-1] = block(ITOC, INT, NULL, NULL, cp[-1]);
		build(RFORCE);
		rcexpr(*--cp);
	}
	branch(retlab);
}

/*
 * Write a character on the error output.
 */
/*
 * Coded output:
 *   B: beginning of line; an operator
 *   N: a number
 *   S: a symbol (external)
 *   1: number 1
 *   0: number 0
 */
outcode(s, a)
char *s;
{
	register *ap;
	register FILE *bufp;
	int n;
	register char *np;

	bufp = stdout;
	if (strflg)
		bufp = sbufp;
	ap = &a;
	for (;;) switch(*s++) {
	case 'B':
		putc(*ap++, bufp);
		putc(0376, bufp);
		continue;

	case 'N':
		putc(*ap, bufp);
		putc(*ap++>>8, bufp);
		continue;

	case 'F':
		n = 1000;
		np = *ap++;
		goto str;

	case 'S':
		n = NCPS;
		np = *ap++;
		if (*np)
			putc('_', bufp);
	str:
		while (n-- && *np) {
			putc(*np++&0177, bufp);
		}
		putc(0, bufp);
		continue;

	case '1':
		putc(1, bufp);
		putc(0, bufp);
		continue;

	case '0':
		putc(0, bufp);
		putc(0, bufp);
		continue;

	case '\0':
		if (ferror(bufp)) {
			error("Write error on temp");
			exit(1);
		}
		return;

	default:
		error("Botch in outcode");
	}
}
