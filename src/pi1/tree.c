/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 */

#include "tree.h"
#include "0.h"

/*
 * TREE SPACE DECLARATIONS
 */
struct tr {
	int	*tr_low;
	int	*tr_high;
} ttab[MAXTREE], *tract;

static	int *ltsnt;

/*
 * The variable space is the
 * absolute base of the tree segments.
 * (exactly the same as ttab[0].tr_low)
 * Spacep is maintained to point at the
 * beginning of the next tree slot to
 * be allocated for use by the grammar.
 * Spacep is used "extern" by the semantic
 * actions in pas.y.
 * The variable tract is maintained to point
 * at the tree segment out of which we are
 * allocating (the active segment).
 */
int	*space, *spacep;

/*
 * TREENMAX is the maximum width
 * in words that any tree node 
 * due to the way in which the parser uses
 * the pointer spacep.
 */
#define	TREENMAX	6

#ifndef PI0
int	trspace[ITREE];
int	*space	trspace;
int	*spacep	trspace;
#endif
struct	tr *tract	ttab;

int	treemax;

/*
 * Inittree allocates the first tree slot
 * and sets up the first segment descriptor.
 * A lot of this work is actually done statically
 * above.
 */
#ifndef PI0
inittree()
#else
inittree(trspace)
	int *trspace;
#endif
{

#ifdef PI0
	space = spacep = trspace;
#endif
	ttab[0].tr_low = space;
	ttab[0].tr_high = &space[ITREE - 1];
#ifndef PI1
	ltsnt = space;
#endif
	treemax = ITREE;
	*spacep = 0;
}

#ifndef PI1
/*
 * Tree builds the nodes in the
 * parse tree. It is rarely called
 * directly, rather calls are made
 * to tree[12345] which supplies the
 * first argument to save space in
 * the code. Tree also guarantees
 * that spacep points to the beginning
 * of the next slot it will return,
 * a property required by the parser
 * which was always true before we
 * segmented the tree space.
 */
int *
tree(cnt, a)
	int cnt;
{
	register int *p, *q;
	register int i;

	i = cnt;
	p = spacep;
	q = &a;
	do
		*p++ = *q++;
	while (--i);
	*p = 0;
	q = spacep;
	spacep = p;
	if (p+TREENMAX >= tract->tr_high)
		/*
		 * this peek-ahead should
		 * save a great number of calls
		 * to tralloc.
		 */
		tralloc(TREENMAX);
	return (q);
}
#else
treev(i, q)
	register int i, *q;
{
	register int *p;

	p = spacep;
	do
		*p++ = *q++;
	while (--i);
	*p = 0;
	q = spacep;
	spacep = p;
	if (p+TREENMAX >= tract->tr_high)
		tralloc(TREENMAX);
	return (q);
}
#endif
/*
 * Tralloc preallocates enough
 * space in the tree to allow
 * the grammar to use the variable
 * spacep, as it did before the
 * tree was segmented.
 */
tralloc(howmuch)
{
	register char *cp;
	register i;

	if (spacep + howmuch >= tract->tr_high) {
		talloc(++tract);
		spacep = tract->tr_low;
		*spacep = 0;
	}
}

talloc(tp)
	register struct tr *tp;
{
	register char *cp;
	register int i;

	if (tp >= &ttab[MAXTREE]) {
		yerror("Ran out of tree tables");
		pexit(DIED);
	}
	if (tp->tr_low != NIL)
		return;
	cp = alloc(TRINC * 2);
	if (cp == -1) {
		yerror("Ran out of memory (talloc)");
		pexit(DIED);
	}
	tp->tr_low = cp;
	tp->tr_high = tp->tr_low + (TRINC - 1);
	i = (tp - ttab + 1) * TRINC;
	if (i > treemax)
		treemax = i;
}
#ifndef PI1
extern	int yylacnt;
extern	bottled;
#endif
/*
 * Free up the tree segments
 * at the end of a block.
 * If there is scanner lookahead,
 * i.e. if yylacnt != 0 or there is bottled output, then we
 * cannot free the tree space.
 * This happens only when errors
 * occur and the forward move extends
 * across "units".
 */
trfree()
{

#ifndef PI1
	if (yylacnt != 0 || bottled != NIL)
		return;
#endif
#ifndef PI1
	send(RTRFREE);
	ltsnt = space;
#endif
	spacep = space;
	while (tract->tr_low > spacep || tract->tr_high <= spacep) {
		free(tract->tr_low);
		tract->tr_low = NIL;
		tract->tr_high = NIL;
		tract--;
		if (tract < ttab)
			panic("ttab");
	}
	treemax = ITREE;
}

/*
 * Copystr copies a token from
 * the "token" buffer into the
 * tree space.
 */
copystr(token)
	register char *token;
{
	register char *cp;
	register int i;

	i = (strlen(token) + 4) & ~1;
	tralloc(i >> 1);
	*spacep++ = T_COPSTR;
	i =- 2;
	strcpy(spacep, token);
	cp = spacep;
	spacep = cp + i;
	*spacep = 0;
	tralloc(TREENMAX);
	return (cp);
}

/* actually needed in PI1 only if DEBUG... */
toffset(ap)
	register int *ap;
{
	register struct tr *tp;
	register int i;

	if (ap == 0)
		return (0);
	i = TRINC;
	for (tp = ttab; tp->tr_low != NIL && tp < &ttab[MAXTREE]; tp++) {
		if (ap >= tp->tr_low && ap < tp->tr_high)
			return (i + (ap - tp->tr_low));
		i =+ TRINC;
	}
	return (-soffset(ap));
}

#ifndef PI1
tsend()
{
	register struct tr *trp;
	register int *ap;

	ap = ltsnt;
	for (trp = &ttab[(toffset(ltsnt) / TRINC) - 1]; trp <= tract; trp++) {
		while (ap < trp->tr_high && *ap)
			ap = send(RTREE, ap);
		ltsnt = ap;
		ap = trp[1].tr_low;
	}
#ifdef DEBUG
	send(RTRCHK, toffset(ltsnt));
#endif
}
#endif
#ifdef PI1
treloc(i)
	register int i;
{

	if (i == 0)
		return (0);
	if (i < TRINC)
		return (sreloc(-i));
	i =- TRINC;
	if (i >= treemax)
		trmax(i);
	return (ttab[i / TRINC].tr_low + i % TRINC);
}

trmax(i)
	register int i;
{
	register struct tr *tp;

	i = (i + TRINC) / TRINC;
	for (tp = ttab; i > 0; tp++, i--)
		talloc(tp);
}
#endif
