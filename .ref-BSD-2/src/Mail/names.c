/* Copyright (c) 1979 Regents of the University of California */
#

/*
 * Mail -- a mail program
 *
 * Handle name lists.
 */

#include "rcv.h"

/*
 * Allocate a single element of a name list,
 * initialize its name field to the passed
 * name and return it.
 */

struct name *
nalloc(str)
	char str[];
{
	register struct name *np;

	np = (struct name *) salloc(sizeof *np);
	np->n_flink = NIL;
	np->n_blink = NIL;
	np->n_name = savestr(str);
	return(np);
}

/*
 * Find the tail of a list and return it.
 */

struct name *
tailof(name)
	struct name *name;
{
	register struct name *np;

	np = name;
	if (np == NIL)
		return(NIL);
	while (np->n_flink != NIL)
		np = np->n_flink;
	return(np);
}

/*
 * Extract a list of names from a line,
 * and make a list of names from it.
 * Return the list or NIL if none found.
 */

struct name *
extract(line)
	char line[];
{
	register char *cp;
	register struct name *top, *np, *t;
	char nbuf[BUFSIZ];

	if (line == NOSTR)
		return(NIL);
	top = NIL;
	np = NIL;
	cp = line;
	while ((cp = yankword(cp, nbuf)) != NOSTR) {
		t = nalloc(nbuf);
		if (top == NIL)
			top = t;
		else
			np->n_flink = t;
		t->n_blink = np;
		np = t;
	}
	return(top);
}

/*
 * Grab a single word (liberal word)
 * Throw away things between ()'s.
 */

char *
yankword(ap, wbuf)
	char *ap, wbuf[];
{
	register char *cp, *cp2;

	do {
		for (cp = ap; *cp && any(*cp, " \t"); cp++)
			;
		if (*cp == '(') {
			while (*cp && *cp != ')')
				cp++;
			if (*cp)
				cp++;
		}
		if (*cp == '\0')
			return(NOSTR);
	} while (any(*cp, " \t("));
	for (cp2 = wbuf; *cp && !any(*cp, " \t("); *cp2++ = *cp++)
		;
	*cp2 = '\0';
	return(cp);
}

/*
 * Verify that all the users in the list of names are
 * legitimate.  Bitch about and delink those who aren't.
 */

struct name *
verify(names)
	struct name *names;
{
	register struct name *np, *top, *t, *x;

	top = names;
	np = names;
	while (np != NIL) {
		if (any(':', np->n_name) || getuserid(np->n_name) != -1
		    || strcmp(np->n_name, "msgs") == 0) {
			np = np->n_flink;
			continue;
		}
		fprintf(stderr, "Can't send to %s\n", np->n_name);
		senderr++;
		if (np == top) {
			top = np->n_flink;
			if (top != NIL)
				top->n_blink = NIL;
			np = top;
			continue;
		}
		x = np->n_blink;
		t = np->n_flink;
		x->n_flink = t;
		if (t != NIL)
			t->n_blink = x;
		np = t;
	}
	return(top);
}

/*
 * For each recipient in the passed name list with a /
 * in the name, append the message to the end of the named file
 * and remove him from the recipient list.
 */

struct name *
outof(names, fo, hp)
	struct name *names;
	FILE *fo;
	struct header *hp;
{
	register int c;
	register struct name *np, *top, *t, *x;
	long now;
	char *date, *ctime();
	FILE *fout;

	top = names;
	np = names;
	time(&now);
	date = ctime(&now);
	while (np != NIL) {
		if (!any('/', np->n_name)) {
			np = np->n_flink;
			continue;
		}
		if ((fout = fopen(np->n_name, "a")) == NULL) {
			perror(np->n_name);
			senderr++;
		}
		else {
			rewind(fo);
			fprintf(fout, "From %s %s", myname, date);
			puthead(hp, fout);
			while ((c = getc(fo)) != EOF)
				putc(c, fout);
			fflush(fout);
			if (ferror(fout))
				perror(np->n_name);
			fclose(fout);
		}
		if (np == top) {
			top = np->n_flink;
			if (top != NIL)
				top->n_blink = NIL;
			np = top;
			continue;
		}
		x = np->n_blink;
		t = np->n_flink;
		x->n_flink = t;
		if (t != NIL)
			t->n_blink = x;
		np = t;
	}
	return(top);
}

/*
 * Map all of the aliased users in the invoker's sendrc
 * file and insert them into the list.
 */

struct name *
usermap(names)
	struct name *names;
{
	register struct name *new, *np, *cp;
	struct grouphead *gh;
	struct group *gp;
	register int metoo;

	new = NIL;
	np = names;
	metoo = (value("metoo") != NOSTR);
	while (np != NIL) {
		if (np->n_name[0] == '\\') {
			while (*np->n_name == '\\')
				(np->n_name)++;
			cp = np->n_flink;
			new = put(new, np);
			np = cp;
			continue;
		}
		if ((gh = findgroup(np->n_name)) != NOGRP) {
			for (gp = gh->g_list; gp != NOGE; gp = gp->ge_link) {
				if (!metoo && equal(gp->ge_name, myname))
					continue;
				cp = nalloc(gp->ge_name);
				new = put(new, cp);
			}
			np = np->n_flink;
			continue;
		}
		else {
			cp = np->n_flink;
			new = put(new, np);
			np = cp;
		}
	}
	return(new);
}

/*
 * Compute the length of the passed name list and
 * return it.
 */

lengthof(name)
	struct name *name;
{
	register struct name *np;
	register int c;

	for (c = 0, np = name; np != NIL; c++, np = np->n_flink)
		;
	return(c);
}

/*
 * Concatenate the two passed name lists, return the result.
 */

struct name *
cat(n1, n2)
	struct name *n1, *n2;
{
	register struct name *tail;

	if (n1 == NIL)
		return(n2);
	if (n2 == NIL)
		return(n1);
	tail = tailof(n1);
	tail->n_flink = n2;
	n2->n_blink = tail;
	return(n1);
}

/*
 * Unpack the name list onto a vector of strings.
 * Return an error if the name list won't fit.
 */

char **
unpack(np)
	struct name *np;
{
	register char **ap, **top;
	register struct name *n;
	char *cp;
	int t;

	n = np;
	if ((t = lengthof(n)) == 0)
		panic("No names to unpack");
	top = (char **) salloc((t+2) * sizeof cp);
	ap = top;
	*ap++ = "mail";
	while (n != NIL) {
		*ap++ = n->n_name;
		n = n->n_flink;
	}
	*ap = NOSTR;
	return(top);
}

/*
 * See if the user named himself as a destination
 * for outgoing mail.  If so, set the global flag
 * selfsent so that we avoid removing his mailbox.
 */

mechk(names)
	struct name *names;
{
	register struct name *np;
	char myname[9];

	if (getname(uid, myname) < 0)
		return;
	for (np = names; np != NIL; np = np->n_flink)
		if (equal(myname, np->n_name)) {
			selfsent++;
			return;
		}
}

/*
 * Remove all of the duplicates from the passed name list by
 * insertion sorting them, then checking for dups.
 * Return the head of the new list.
 */

struct name *
elide(names)
	struct name *names;
{
	register struct name *np, *t, *new;
	struct name *x;

	new = names;
	np = names;
	np = np->n_flink;
	if (np != NIL)
		np->n_blink = NIL;
	new->n_flink = NIL;
	while (np != NIL) {
		t = new;
		while (strcmp(t->n_name, np->n_name) > 0) {
			if (t->n_flink == NIL)
				break;
			t = t->n_flink;
		}

		/*
		 * If we ran out of t's, put the new entry after
		 * the current value of t.
		 */

		if (strcmp(t->n_name, np->n_name) > 0) {
			t->n_flink = np;
			np->n_blink = t;
			t = np;
			np = np->n_flink;
			t->n_flink = NIL;
			continue;
		}

		/*
		 * Otherwise, put the new entry in front of the
		 * current t.  If at the front of the list,
		 * the new guy becomes the new head of the list.
		 */

		if (t == new) {
			t = np;
			np = np->n_flink;
			t->n_flink = new;
			new->n_blink = t;
			t->n_blink = NIL;
			new = t;
			continue;
		}

		/*
		 * The normal case -- we are inserting into the
		 * middle of the list.
		 */

		x = np;
		np = np->n_flink;
		x->n_flink = t;
		x->n_blink = t->n_blink;
		t->n_blink->n_flink = x;
		t->n_blink = x;
	}

	/*
	 * Now the list headed up by new is sorted.
	 * Go through it and remove duplicates.
	 */

	np = new;
	while (np != NIL) {
		t = np;
		while (t->n_flink!=NIL && equal(np->n_name,t->n_flink->n_name))
			t = t->n_flink;
		if (t == np || t == NIL) {
			np = np->n_flink;
			continue;
		}
		
		/*
		 * Now t points to the last entry with the same name
		 * as np.  Make np point beyond t.
		 */

		np->n_flink = t->n_flink;
		if (t->n_flink != NIL)
			t->n_flink->n_blink = np;
		np = np->n_flink;
	}
	return(new);
}

/*
 * Put another node onto a list of names and return
 * the list.
 */

struct name *
put(list, node)
	struct name *list, *node;
{
	node->n_flink = list;
	node->n_blink = NIL;
	if (list != NIL)
		list->n_blink = node;
	return(node);
}

/*
 * Determine the number of elements in
 * a name list and return it.
 */

count(np)
	register struct name *np;
{
	register int c = 0;

	while (np != NIL) {
		c++;
		np = np->n_flink;
	}
	return(c);
}

/*
 * Pretty print a name list
 * Uncomment it if you need it.
 *
 * prettyprint(name)
 * 	struct name *name;
 * {
 * 	register struct name *np;
 * 
 * 	np = name;
 * 	while (np != NIL) {
 * 		fprintf(stderr, "%s ", np->n_name);
 * 		np = np->n_flink;
 * 	}
 * 	fprintf(stderr, "\n");
 * }
 */
