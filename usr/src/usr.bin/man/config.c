/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)config.c	8.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>

#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "pathnames.h"

struct _head head;

/*
 * config --
 *
 * Read the configuration file and build a doubly linked
 * list that looks like:
 *
 *	tag1 <-> record <-> record <-> record
 *	|
 *	tag2 <-> record <-> record <-> record
 */
void
config(fname)
	char *fname;
{
	TAG *tp;
	ENTRY *ep;
	FILE *cfp;
	size_t len;
	int lcnt;
	char *p, *t;

	if (fname == NULL)
		fname = _PATH_MANCONF;
	if ((cfp = fopen(fname, "r")) == NULL)
		err(1, "%s", fname);
	TAILQ_INIT(&head);
	for (lcnt = 1; (p = fgetline(cfp, &len)) != NULL; ++lcnt) {
		if (!len)			/* Skip empty lines. */
			continue;
		if (p[len - 1] != '\n') {	/* Skip corrupted lines. */
			warnx("%s: line %d corrupted", fname, lcnt);
			continue;
		}
		p[len - 1] = '\0';		/* Terminate the line. */

						/* Skip leading space. */
		for (; *p != '\0' && isspace(*p); ++p);
						/* Skip empty/comment lines. */
		if (*p == '\0' || *p == '#')
			continue;
						/* Find first token. */
		for (t = p; *t && !isspace(*t); ++t);
		if (*t == '\0')			/* Need more than one token.*/
			continue;
		*t = '\0';

		for (tp = head.tqh_first;	/* Find any matching tag. */
		    tp != NULL && strcmp(p, tp->s); tp = tp->q.tqe_next);

		if (tp == NULL)		/* Create a new tag. */
			tp = addlist(p);

		/*
		 * Attach new records.  The keyword _build takes the rest of
		 * the line as a single entity, everything else is white
		 * space separated.  The reason we're not just using strtok(3)
		 * for all of the parsing is so we don't get caught if a line
		 * has only a single token on it.
		 */
		if (!strcmp(p, "_build")) {
			while (*++t && isspace(*t));
			if ((ep = malloc(sizeof(ENTRY))) == NULL ||
			    (ep->s = strdup(t)) == NULL)
				err(1, NULL);
			TAILQ_INSERT_TAIL(&tp->list, ep, q);
		} else for (++t; (p = strtok(t, " \t\n")) != NULL; t = NULL) {
			if ((ep = malloc(sizeof(ENTRY))) == NULL ||
			    (ep->s = strdup(p)) == NULL)
				err(1, NULL);
			TAILQ_INSERT_TAIL(&tp->list, ep, q);
		}
	}
}

/*
 * addlist --
 *	Add a tag to the list.
 */
TAG *
addlist(name)
	char *name;
{
	TAG *tp;

	if ((tp = calloc(1, sizeof(TAG))) == NULL ||
	    (tp->s = strdup(name)) == NULL)
		err(1, NULL);
	TAILQ_INIT(&tp->list);
	TAILQ_INSERT_TAIL(&head, tp, q);
	return (tp);
}

/*
 * getlist --
 *	Return the linked list of entries for a tag if it exists.
 */
TAG *
getlist(name)
	char *name;
{
	TAG *tp;

	for (tp = head.tqh_first; tp != NULL; tp = tp->q.tqe_next)
		if (!strcmp(name, tp->s))
			return (tp);
	return (NULL);
}

void
debug(l)
	char *l;
{
	TAG *tp;
	ENTRY *ep;

	(void)printf("%s ===============\n", l);
	for (tp = head.tqh_first; tp != NULL; tp = tp->q.tqe_next) {
		printf("%s\n", tp->s);
		for (ep = tp->list.tqh_first; ep != NULL; ep = ep->q.tqe_next)
			printf("\t%s\n", ep->s);
	}
}
