/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)config.c	8.2 (Berkeley) %G%";
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

struct queue_entry head;

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
config()
{
	ENTRY *ep, *qp;
	FILE *cfp;
	size_t len;
	int lcnt;
	char *p, *t;

	if ((cfp = fopen(_PATH_MANCONF, "r")) == NULL)
		err(1, "%s", _PATH_MANCONF);
	queue_init(&head);
	for (lcnt = 1; (p = fgetline(cfp, &len)) != NULL; ++lcnt) {
		if (!len)			/* Skip empty lines. */
			continue;
		if (p[len - 1] != '\n') {	/* Skip corrupted lines. */
			warnx("%s: line %d corrupted", _PATH_MANCONF, lcnt);
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

		for (qp = head.qe_next;		/* Find any matching tag. */
		    qp != NULL && strcmp(p, qp->s); qp = qp->tags.qe_next);

		if (qp == NULL)			/* Create a new tag. */
			qp = addlist(p);

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
			queue_enter_tail(&qp->list, ep, ENTRY *, list);
		} else while ((p = strtok(t + 1, " \t\n")) != NULL) {
			if ((ep = malloc(sizeof(ENTRY))) == NULL ||
			    (ep->s = strdup(p)) == NULL)
				err(1, NULL);
			queue_enter_tail(&qp->list, ep, ENTRY *, list);
			t = NULL;
		}
	}
}

/*
 * addlist --
 *	Add a tag to the list.
 */
ENTRY *
addlist(name)
	char *name;
{
	ENTRY *ep;

	if ((ep = malloc(sizeof(ENTRY))) == NULL ||
	    (ep->s = strdup(name)) == NULL)
		err(1, NULL);
	queue_init(&ep->list);
	queue_enter_head(&head, ep, ENTRY *, tags);
	return (head.qe_next);
}

/*
 * getlist --
 *	Return the linked list for a tag if it exists.
 */
ENTRY *
getlist(name)
	char *name;
{
	ENTRY *qp;

	for (qp = head.qe_next; qp != NULL; qp = qp->tags.qe_next)
		if (!strcmp(name, qp->s))
			return (qp);
	return (NULL);
}

void
debug(l)
	char *l;
{
	ENTRY *ep, *qp;

	(void)printf("%s ===============\n", l);
	for (qp = head.qe_next; qp != NULL; qp = qp->tags.qe_next) {
		printf("%s\n", qp->s);
		for (ep = qp->list.qe_next; ep != NULL; ep = ep->list.qe_next)
			printf("\t%s\n", ep->s);
	}
}
