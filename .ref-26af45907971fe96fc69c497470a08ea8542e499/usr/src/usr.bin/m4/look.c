/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit at York University.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)look.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * look.c
 * Facility: m4 macro processor
 * by: oz
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mdef.h"
#include "stdd.h"
#include "extern.h"

int
hash(name)
register char *name;
{
	register unsigned long h = 0;
	while (*name)
		h = (h << 5) + h + *name++;
	return (h % HASHSIZE);
}

/*
 * find name in the hash table
 */
ndptr 
lookup(name)
char *name;
{
	register ndptr p;

	for (p = hashtab[hash(name)]; p != nil; p = p->nxtptr)
		if (STREQ(name, p->name))
			break;
	return (p);
}

/*
 * hash and create an entry in the hash table.
 * The new entry is added in front of a hash bucket.
 */
ndptr 
addent(name)
char *name;
{
	register int h;
	ndptr p;

	h = hash(name);
	p = (ndptr) xalloc(sizeof(struct ndblock));
	p->nxtptr = hashtab[h];
	hashtab[h] = p;
	p->name = xstrdup(name);
	return p;
}

static void
freent(p)
ndptr p;
{
	if (!(p->type & STATIC)) {
		free((char *) p->name);
		if (p->defn != null)
			free((char *) p->defn);
	}
	free((char *) p);
}

/*
 * remove an entry from the hashtable
 */
void
remhash(name, all)
char *name;
int all;
{
	register int h;
	register ndptr xp, tp, mp;

	h = hash(name);
	mp = hashtab[h];
	tp = nil;
	while (mp != nil) {
		if (STREQ(mp->name, name)) {
			mp = mp->nxtptr;
			if (tp == nil) {
				freent(hashtab[h]);
				hashtab[h] = mp;
			}
			else {
				xp = tp->nxtptr;
				tp->nxtptr = mp;
				freent(xp);
			}
			if (!all)
				break;
		}
		else {
			tp = mp;
			mp = mp->nxtptr;
		}
	}
}
