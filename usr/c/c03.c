#
/* C compiler

Copyright 1972 Bell Telephone Laboratories, Inc. 

*/

#include "c0h.c"

decref(at)
{
	register t;

	t = at;
	if ((t & ~07) == 0) {
		error("Illegal indirection");
		return(t);
	}
	return((t>>2) & ~07 | t&07);
}

incref(t)
{
	return((t<<2)&~034 | (t&07) | PTR);
}

cbranch(tree, lbl, cond)
struct tnode *tree;
{
	rcexpr(block(1,CBRANCH,tree,lbl,cond),cctab);
}

rcexpr(tree, table)
int table;
struct tnode *tree;
{
	register c, *sp;

	if (tree == 0)
		return;
	putchar('#');
	c = space-treebase;
	sp = treebase;
	putw(c, binbuf);
	putw(tree, binbuf);
	putw(table, binbuf);
	putw(line, binbuf);
	while(c--)
		putw(*sp++, binbuf);
}

branch(lab) {
	printf("jbr\tL%d\n", lab);
}

label(l) {
	printf("L%d:", l);
}

plength(ap)
struct tname *ap;
{
	register t, l;
	register struct tname *p;

	p = ap;
	if (((t=p->type)&~07) == 0)		/* not a reference */
		return(1);
	p->type = decref(t);
	l = length(p);
	p->type = t;
	return(l);
}

length(acs)
struct tnode *acs;
{
	register t, n;
	register struct tnode *cs;

	cs = acs;
	t = cs->type;
	n = 1;
	while ((t&030) == ARRAY) {
		t = decref(t);
		n = dimtab[cs->ssp&0377];
	}
	if ((t&~07)==FUNC)
		return(0);
	if (t>=PTR)
		return(2*n);
	switch(t&07) {

	case INT:
		return(2*n);

	case CHAR:
		return(n);

	case FLOAT:
		return(4*n);

	case DOUBLE:
		return(8*n);

	case STRUCT:
		return(n * dimtab[cs->lenp&0377]);

	case RSTRUCT:
		error("Bad structure");
		return(0);
	}
	error("Compiler error (length)");
}

rlength(cs)
struct tnode *cs;
{
	register int l;

	if (((l=length(cs))&01) != 0)
		l++;
	return(l);
}

simplegoto()
{
	register struct hshtab *csp;

	if ((peeksym=symbol())==NAME && nextchar()==';') {
		csp = csym;
		if (csp->hclass==0 && csp->htype==0) {
			csp->htype = ARRAY;
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

nextchar()
{
	while (ctab[peekc]==SPACE)
		peekc = getchar();
	return(peekc);
}

chconbrk(l)
{
	if (l==0)
		error("Break/continue error");
}

dogoto()
{
	register struct tnode *np;

	*cp++ = tree();
	build(STAR);
	chkw(np = *--cp);
	rcexpr(block(1,JUMP,0,0,np), regtab);
}

doret()
{
	if (nextchar() != ';')
		rcexpr(block(1, RFORCE, 0, 0, tree()), regtab);
	branch(retlab);
}
