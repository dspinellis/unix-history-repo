/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nodes.c.pat	8.2 (Berkeley) %G%
 */

#include <stdlib.h>
/*
 * Routine for dealing with parsed shell commands.
 */

#include "shell.h"
#include "nodes.h"
#include "memalloc.h"
#include "machdep.h"
#include "mystring.h"


int     funcblocksize;		/* size of structures in function */
int     funcstringsize;		/* size of strings in node */
pointer funcblock;		/* block to allocate function from */
char   *funcstring;		/* block to allocate strings from */

%SIZES


STATIC void calcsize __P((union node *));
STATIC void sizenodelist __P((struct nodelist *));
STATIC union node *copynode __P((union node *));
STATIC struct nodelist *copynodelist __P((struct nodelist *));
STATIC char *nodesavestr __P((char *));



/*
 * Make a copy of a parse tree.
 */

union node *
copyfunc(n)
	union node *n;
{
	if (n == NULL)
		return NULL;
	funcblocksize = 0;
	funcstringsize = 0;
	calcsize(n);
	funcblock = ckmalloc(funcblocksize + funcstringsize);
	funcstring = funcblock + funcblocksize;
	return copynode(n);
}



STATIC void
calcsize(n)
	union node *n;
{
	%CALCSIZE
}



STATIC void
sizenodelist(lp)
	struct nodelist *lp;
{
	while (lp) {
		funcblocksize += ALIGN(sizeof(struct nodelist));
		calcsize(lp->n);
		lp = lp->next;
	}
}



STATIC union node *
copynode(n)
	union node *n;
{
	union node *new;

	%COPY
	return new;
}


STATIC struct nodelist *
copynodelist(lp)
	struct nodelist *lp;
{
	struct nodelist *start;
	struct nodelist **lpp;

	lpp = &start;
	while (lp) {
		*lpp = funcblock;
		funcblock += ALIGN(sizeof(struct nodelist));
		(*lpp)->n = copynode(lp->n);
		lp = lp->next;
		lpp = &(*lpp)->next;
	}
	*lpp = NULL;
	return start;
}



STATIC char *
nodesavestr(s)
	char   *s;
{
	register char *p = s;
	register char *q = funcstring;
	char   *rtn = funcstring;

	while ((*q++ = *p++) != '\0')
		continue;
	funcstring = q;
	return rtn;
}



/*
 * Free a parse tree.
 */

void
freefunc(n)
	union node *n;
{
	if (n)
		ckfree(n);
}
