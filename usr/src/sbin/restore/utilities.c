/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)utilities.c	5.9 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <ufs/ufs/dinode.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "restore.h"
#include "extern.h"

/*
 * Insure that all the components of a pathname exist.
 */
void
pathcheck(name)
	char *name;
{
	register char *cp;
	struct entry *ep;
	char *start;

	start = index(name, '/');
	if (start == 0)
		return;
	for (cp = start; *cp != '\0'; cp++) {
		if (*cp != '/')
			continue;
		*cp = '\0';
		ep = lookupname(name);
		if (ep == NULL) {
			ep = addentry(name, pathsearch(name), NODE);
			newnode(ep);
		}
		ep->e_flags |= NEW|KEEP;
		*cp = '/';
	}
}

/*
 * Change a name to a unique temporary name.
 */
void
mktempname(ep)
	register struct entry *ep;
{
	char oldname[MAXPATHLEN];

	if (ep->e_flags & TMPNAME)
		badentry(ep, "mktempname: called with TMPNAME");
	ep->e_flags |= TMPNAME;
	(void) strcpy(oldname, myname(ep));
	freename(ep->e_name);
	ep->e_name = savename(gentempname(ep));
	ep->e_namlen = strlen(ep->e_name);
	renameit(oldname, myname(ep));
}

/*
 * Generate a temporary name for an entry.
 */
char *
gentempname(ep)
	struct entry *ep;
{
	static char name[MAXPATHLEN];
	struct entry *np;
	long i = 0;

	for (np = lookupino(ep->e_ino);
	    np != NULL && np != ep; np = np->e_links)
		i++;
	if (np == NULL)
		badentry(ep, "not on ino list");
	(void) sprintf(name, "%s%d%d", TMPHDR, i, ep->e_ino);
	return (name);
}

/*
 * Rename a file or directory.
 */
void
renameit(from, to)
	char *from, *to;
{
	if (!Nflag && rename(from, to) < 0) {
		fprintf(stderr, "warning: cannot rename %s to %s: %s\n",
		    from, to, strerror(errno));
		return;
	}
	vprintf(stdout, "rename %s to %s\n", from, to);
}

/*
 * Create a new node (directory).
 */
void
newnode(np)
	struct entry *np;
{
	char *cp;

	if (np->e_type != NODE)
		badentry(np, "newnode: not a node");
	cp = myname(np);
	if (!Nflag && mkdir(cp, 0777) < 0) {
		np->e_flags |= EXISTED;
		fprintf(stderr, "warning: %s: %s\n", cp, strerror(errno));
		return;
	}
	vprintf(stdout, "Make node %s\n", cp);
}

/*
 * Remove an old node (directory).
 */
void
removenode(ep)
	register struct entry *ep;
{
	char *cp;

	if (ep->e_type != NODE)
		badentry(ep, "removenode: not a node");
	if (ep->e_entries != NULL)
		badentry(ep, "removenode: non-empty directory");
	ep->e_flags |= REMOVED;
	ep->e_flags &= ~TMPNAME;
	cp = myname(ep);
	if (!Nflag && rmdir(cp) < 0) {
		fprintf(stderr, "warning: %s: %s\n", cp, strerror(errno));
		return;
	}
	vprintf(stdout, "Remove node %s\n", cp);
}

/*
 * Remove a leaf.
 */
void
removeleaf(ep)
	register struct entry *ep;
{
	char *cp;

	if (ep->e_type != LEAF)
		badentry(ep, "removeleaf: not a leaf");
	ep->e_flags |= REMOVED;
	ep->e_flags &= ~TMPNAME;
	cp = myname(ep);
	if (!Nflag && unlink(cp) < 0) {
		fprintf(stderr, "warning: %s: %s\n", cp, strerror(errno));
		return;
	}
	vprintf(stdout, "Remove leaf %s\n", cp);
}

/*
 * Create a link.
 */
int
linkit(existing, new, type)
	char *existing, *new;
	int type;
{

	if (type == SYMLINK) {
		if (!Nflag && symlink(existing, new) < 0) {
			fprintf(stderr,
			    "warning: cannot create symbolic link %s->%s: %s\n",
			    new, existing, strerror(errno));
			return (FAIL);
		}
	} else if (type == HARDLINK) {
		if (!Nflag && link(existing, new) < 0) {
			fprintf(stderr,
			    "warning: cannot create hard link %s->%s: %s\n",
			    new, existing, strerror(errno));
			return (FAIL);
		}
	} else {
		panic("linkit: unknown type %d\n", type);
		return (FAIL);
	}
	vprintf(stdout, "Create %s link %s->%s\n",
		type == SYMLINK ? "symbolic" : "hard", new, existing);
	return (GOOD);
}

/*
 * find lowest number file (above "start") that needs to be extracted
 */
ino_t
lowerbnd(start)
	ino_t start;
{
	register struct entry *ep;

	for ( ; start < maxino; start++) {
		ep = lookupino(start);
		if (ep == NULL || ep->e_type == NODE)
			continue;
		if (ep->e_flags & (NEW|EXTRACT))
			return (start);
	}
	return (start);
}

/*
 * find highest number file (below "start") that needs to be extracted
 */
ino_t
upperbnd(start)
	ino_t start;
{
	register struct entry *ep;

	for ( ; start > ROOTINO; start--) {
		ep = lookupino(start);
		if (ep == NULL || ep->e_type == NODE)
			continue;
		if (ep->e_flags & (NEW|EXTRACT))
			return (start);
	}
	return (start);
}

/*
 * report on a badly formed entry
 */
void
badentry(ep, msg)
	register struct entry *ep;
	char *msg;
{

	fprintf(stderr, "bad entry: %s\n", msg);
	fprintf(stderr, "name: %s\n", myname(ep));
	fprintf(stderr, "parent name %s\n", myname(ep->e_parent));
	if (ep->e_sibling != NULL)
		fprintf(stderr, "sibling name: %s\n", myname(ep->e_sibling));
	if (ep->e_entries != NULL)
		fprintf(stderr, "next entry name: %s\n", myname(ep->e_entries));
	if (ep->e_links != NULL)
		fprintf(stderr, "next link name: %s\n", myname(ep->e_links));
	if (ep->e_next != NULL)
		fprintf(stderr,
		    "next hashchain name: %s\n", myname(ep->e_next));
	fprintf(stderr, "entry type: %s\n",
		ep->e_type == NODE ? "NODE" : "LEAF");
	fprintf(stderr, "inode number: %ld\n", ep->e_ino);
	panic("flags: %s\n", flagvalues(ep));
}

/*
 * Construct a string indicating the active flag bits of an entry.
 */
char *
flagvalues(ep)
	register struct entry *ep;
{
	static char flagbuf[BUFSIZ];

	(void) strcpy(flagbuf, "|NIL");
	flagbuf[0] = '\0';
	if (ep->e_flags & REMOVED)
		(void) strcat(flagbuf, "|REMOVED");
	if (ep->e_flags & TMPNAME)
		(void) strcat(flagbuf, "|TMPNAME");
	if (ep->e_flags & EXTRACT)
		(void) strcat(flagbuf, "|EXTRACT");
	if (ep->e_flags & NEW)
		(void) strcat(flagbuf, "|NEW");
	if (ep->e_flags & KEEP)
		(void) strcat(flagbuf, "|KEEP");
	if (ep->e_flags & EXISTED)
		(void) strcat(flagbuf, "|EXISTED");
	return (&flagbuf[1]);
}

/*
 * Check to see if a name is on a dump tape.
 */
ino_t
dirlookup(name)
	char *name;
{
	ino_t ino;

	ino = pathsearch(name);
	if (ino == 0 || TSTINO(ino, dumpmap) == 0)
		fprintf(stderr, "%s is not on tape\n", name);
	return (ino);
}

/*
 * Elicit a reply.
 */
int
reply(question)
	char *question;
{
	char c;

	do	{
		fprintf(stderr, "%s? [yn] ", question);
		(void) fflush(stderr);
		c = getc(terminal);
		while (c != '\n' && getc(terminal) != '\n')
			if (feof(terminal))
				return (FAIL);
	} while (c != 'y' && c != 'n');
	if (c == 'y')
		return (GOOD);
	return (FAIL);
}

/*
 * handle unexpected inconsistencies
 */
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
panic(const char *fmt, ...)
#else
panic(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif

	vfprintf(stderr, fmt, ap);
	if (yflag)
		return;
	if (reply("abort") == GOOD) {
		if (reply("dump core") == GOOD)
			abort();
		done(1);
	}
}
