#ifndef lint
static char sccsid[] = "@(#)db_lookup.c	4.3 (Berkeley) 6/4/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

/*
 * Table lookup routines.
 */

#include <sys/types.h>
#include <stdio.h>
#include <arpa/nameser.h>
#include "db.h"

struct hashbuf *hashtab;	/* root hash table */

#ifdef DEBUG
extern int debug;
extern FILE *ddt;
#endif

/* 
 * Lookup 'name' and return a pointer to the namebuf;
 * NULL otherwise. If 'insert', insert name into tables.
 * Wildcard lookups are handled.
 */
struct namebuf *
nlookup(name, htpp, fname, insert)
	char *name;
	struct hashbuf **htpp;
	char **fname;
	int insert;
{
	register struct namebuf *np;
	register char *cp;
	register int c;
	register unsigned hval;
	register struct hashbuf *htp;
	struct namebuf *parent = NULL;


	htp = *htpp;
	hval = 0;
	for (cp = name; c = *cp++; ) {
		if (c == '.') {
			parent = np = nlookup(cp, htpp, fname, insert);
			if (np == NULL)
				return (NULL);
			if (*fname != cp)
				return (np);
			if ((htp = np->n_hash) == NULL) {
				if (!insert) {
					if (np->n_dname[0] == '*' && 
					    np->n_dname[1] == '\0')
						*fname = name;
					return (np);
				}
				htp = savehash((struct hashbuf *)NULL);
				np->n_hash = htp;
			}
			*htpp = htp;
			break;
		}
		hval <<= HASHSHIFT;
		hval += c & HASHMASK;
	}
	c = *--cp;
	*cp = '\0';
	hval %= htp->h_size;
	/*
	 * Lookup this label in current hash table
	 */
	for (np = htp->h_tab[hval]; np != NULL; np = np->n_next) {
		if (cistrcmp(name, np->n_dname) == 0) {
			*cp = c;
			*fname = name;
			return (np);
		}
	}
	if (!insert) {
		/*
		 * look for wildcard in this hash table
		 */
		hval = ('*' & HASHMASK)  % htp->h_size;
		for (np = htp->h_tab[hval]; np != NULL; np = np->n_next) {
			if (np->n_dname[0] == '*'  && np->n_dname[1] == '\0') {
				*cp = c;
				*fname = name;
				return (np);
			}
		}
		*cp = c;
		return (parent);
	}
	np = savename(name);
	np->n_parent = parent;
	np->n_next = htp->h_tab[hval];
	htp->h_tab[hval] = np;
	/* increase hash table size */
	if (++htp->h_cnt > htp->h_size * 2) {
		*htpp = htp = savehash(htp);
		if (parent == NULL)
			hashtab = htp;
		else
			parent->n_hash = htp;
	}
	*cp = c;
	*fname = name;
	return (np);
}

/*
 * Does the data record match the class and type?
 */
match(dp, class, type)
	struct databuf *dp;
	int class, type;
{

#ifdef DEBUG
	if (debug >= 5)
		fprintf(ddt,"match(%x, %d, %d) %d, %d\n", dp, class, type,
			dp->d_class, dp->d_type);
#endif

	if (dp->d_class != class && class != C_ANY)
		return (0);
	if (dp->d_type != type && type != T_ANY)
		return (0);
	return (1);
}
