/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tree.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "whoami.h"
#include "0.h"

/*
 * TREE SPACE DECLARATIONS
 */
struct tr {
	int	*tr_low;
	int	*tr_high;
} ttab[MAXTREE], *tract;

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

int	trspace[ITREE];
int	*space	= trspace;
int	*spacep	= trspace;
struct	tr *tract	= ttab;

/*
 * Inittree allocates the first tree slot
 * and sets up the first segment descriptor.
 * A lot of this work is actually done statically
 * above.
 */
inittree()
{

	ttab[0].tr_low = space;
	ttab[0].tr_high = &space[ITREE];
}

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
/*VARARGS1*/
struct tnode *
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
	q = spacep;
	spacep = p;
	if (p+TREENMAX >= tract->tr_high)
		/*
		 * this peek-ahead should
		 * save a great number of calls
		 * to tralloc.
		 */
		tralloc(TREENMAX);
	return ((struct tnode *) q);
}

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
		i = TRINC;
		cp = malloc((unsigned) (i * sizeof ( int )));
		if (cp == 0) {
			yerror("Ran out of memory (tralloc)");
			pexit(DIED);
		}
		spacep = (int *) cp;
		tract++;
		if (tract >= &ttab[MAXTREE]) {
			yerror("Ran out of tree tables");
			pexit(DIED);
		}
		tract->tr_low = (int *) cp;
		tract->tr_high = tract->tr_low+i;
	}
}

extern	int yylacnt;
extern	struct B *bottled;
#ifdef PXP
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

	if (yylacnt != 0 || bottled != NIL)
		return;
#ifdef PXP
	if (needtree())
		return;
#endif
	spacep = space;
	while (tract->tr_low > spacep || tract->tr_high <= spacep) {
		free((char *) tract->tr_low);
		tract->tr_low = NIL;
		tract->tr_high = NIL;
		tract--;
		if (tract < ttab)
			panic("ttab");
	}
#ifdef PXP
	packtree();
#endif
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

	i = (strlen(token) + sizeof ( int )) & ~( ( sizeof ( int ) ) - 1 );
	tralloc(i / sizeof ( int ));
	(void) pstrcpy((char *) spacep, token);
	cp = (char *) spacep;
	spacep = ((int *) cp + i);
	tralloc(TREENMAX);
	return ((int) cp);
}
