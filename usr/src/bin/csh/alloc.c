/*-
 * Copyright (c) 1983, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)alloc.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#if __STDC__
# include <stdarg.h>
#else
# include <varargs.h>
#endif

#include "csh.h"
#include "extern.h"

char   *memtop = NULL;		/* PWP: top of current memory */
char   *membot = NULL;		/* PWP: bottom of allocatable memory */

ptr_t
Malloc(n)
    size_t  n;
{
    ptr_t   ptr;

    if (membot == NULL)
	memtop = membot = sbrk(0);
    if ((ptr = malloc(n)) == (ptr_t) 0) {
	child++;
	stderror(ERR_NOMEM);
    }
    return (ptr);
}

ptr_t
Realloc(p, n)
    ptr_t   p;
    size_t  n;
{
    ptr_t   ptr;

    if (membot == NULL)
	memtop = membot = sbrk(0);
    if ((ptr = realloc(p, n)) == (ptr_t) 0) {
	child++;
	stderror(ERR_NOMEM);
    }
    return (ptr);
}

ptr_t
Calloc(s, n)
    size_t  s, n;
{
    ptr_t   ptr;

    if (membot == NULL)
	memtop = membot = sbrk(0);
    if ((ptr = calloc(s, n)) == (ptr_t) 0) {
	child++;
	stderror(ERR_NOMEM);
    }

    return (ptr);
}

void
Free(p)
    ptr_t   p;
{
    if (p)
	free(p);
}

/*
 * mstats - print out statistics about malloc
 *
 * Prints two lines of numbers, one showing the length of the free list
 * for each size category, the second showing the number of mallocs -
 * frees for each size category.
 */
void
/*ARGSUSED*/
showall(v, t)
    Char **v;
    struct command *t;
{
    memtop = (char *) sbrk(0);
    (void) fprintf(cshout, "Allocated memory from 0x%lx to 0x%lx (%ld).\n",
	    (unsigned long) membot, (unsigned long) memtop, memtop - membot);
}
