/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nodes.c.pat	8.1 (Berkeley) %G%
 */

/*
 * Routine for dealing with parsed shell commands.
 */

#include "shell.h"
#include "nodes.h"
#include "memalloc.h"
#include "machdep.h"
#include "mystring.h"


int funcblocksize;		/* size of structures in function */
int funcstringsize;		/* size of strings in node */
#ifdef __STDC__
pointer funcblock;		/* block to allocate function from */
#else
char *funcblock;		/* block to allocate function from */
#endif
char *funcstring;		/* block to allocate strings from */

%SIZES


#ifdef __STDC__
STATIC void calcsize(union node *);
STATIC void sizenodelist(struct nodelist *);
STATIC union node *copynode(union node *);
STATIC struct nodelist *copynodelist(struct nodelist *);
STATIC char *nodesavestr(char *);
#else
STATIC void calcsize();
STATIC void sizenodelist();
STATIC union node *copynode();
STATIC struct nodelist *copynodelist();
STATIC char *nodesavestr();
#endif



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
	    funcblocksize += ALIGN(sizeof (struct nodelist));
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
	    funcblock += ALIGN(sizeof (struct nodelist));
	    (*lpp)->n = copynode(lp->n);
	    lp = lp->next;
	    lpp = &(*lpp)->next;
      }
      *lpp = NULL;
      return start;
}



STATIC char *
nodesavestr(s)
      char *s;
      {
      register char *p = s;
      register char *q = funcstring;
      char *rtn = funcstring;

      while (*q++ = *p++);
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
