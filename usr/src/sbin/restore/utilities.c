/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)utilities.c	3.5	(Berkeley)	83/01/16";
#endif

#include "restore.h"

/*
 * Insure that all the components of a pathname exist.
 */
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
		if (ep == NIL) {
			ep = addentry(name, ep->e_ino, NODE);
			ep->e_flags |= KEEP;
			newnode(ep);
		}
		*cp = '/';
	}
}

/*
 * Change a name to a unique temporary name.
 */
mktempname(ep)
	register struct entry *ep;
{
	char oldname[BUFSIZ];

	if (ep->e_flags & TMPNAME)
		badentry(ep, "mktempname: called with TMPNAME");
	ep->e_flags |= TMPNAME;
	strcpy(oldname, myname(ep));
	ep->e_name[ep->e_namlen++] = TMPCHAR;
	ep->e_name[ep->e_namlen] = '\0';
	renameit(oldname, myname(ep));
}

/*
 * Rename a file or directory.
 */
renameit(from, to)
	char *from, *to;
{
	if (rename(from, to) < 0) {
		perror("renameit");
		panic("Cannot rename %s to %s\n", from, to);
	}
	vprintf(stdout, "rename %s to %s\n", from, to);
}

/*
 * Create a new node (directory).
 */
newnode(np)
	struct entry *np;
{
	char *cp;

	if (np->e_type != NODE)
		badentry(np, "newnode: not a node");
	cp = myname(np);
	if (mkdir(cp, 0777) < 0) {
		if (command == 'x') {
			perror(cp);
			return;
		}
		perror("newnode");
		panic("Cannot make node %s\n", cp);
	}
	vprintf(stdout, "Make node %s\n", cp);
}

/*
 * Remove an old node (directory).
 */
removenode(ep)
	register struct entry *ep;
{
	char *cp;

	if (ep->e_type != NODE)
		badentry(ep, "removenode: not a node");
	if (ep->e_entries != NIL)
		badentry(ep, "removenode: non-empty directory");
	cp = myname(ep);
	if (rmdir(cp) < 0) {
		perror("removenode");
		panic("Cannot remove node %s\n", cp);
	}
	ep->e_flags |= REMOVED;
	ep->e_flags &= ~TMPNAME;
	vprintf(stdout, "Remove node %s\n", cp);
}

/*
 * Remove a leaf.
 */
removeleaf(ep)
	register struct entry *ep;
{
	char *cp;

	if (ep->e_type != LEAF)
		badentry(ep, "removeleaf: not a leaf");
	cp = myname(ep);
	if (unlink(cp) < 0) {
		perror("removeleaf");
		panic("Cannot remove leaf %s\n", cp);
	}
	ep->e_flags |= REMOVED;
	ep->e_flags &= ~TMPNAME;
	vprintf(stdout, "Remove leaf %s\n", cp);
}

/*
 * Create a link.
 */
linkit(existing, new, type)
	char *existing, *new;
	int type;
{

	if (type == SYMLINK) {
		if (symlink(existing, new) < 0) {
			perror("linkit");
			panic("Cannot create symbolic link %s->%s\n",
				new, existing);
		}
	} else if (type == HARDLINK) {
		if (link(existing, new) < 0) {
			perror("linkit");
			panic("Cannot create hard link %s->%s\n",
				new, existing);
		}
	} else {
		panic("linkit: unknown type %d\n", type);
	}
	vprintf(stdout, "Create %s link %s->%s\n",
		type == SYMLINK ? "symbolic" : "hard", new, existing);
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
		if (ep == NIL)
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
		if (ep == NIL)
			continue;
		if (ep->e_flags & (NEW|EXTRACT))
			return (start);
	}
	return (start);
}

/*
 * report on a badly formed entry
 */
badentry(ep, msg)
	register struct entry *ep;
	char *msg;
{
	char flagbuf[BUFSIZ];

	fprintf(stderr, "bad entry: %s\n", msg);
	fprintf(stderr, "name: %s\n", myname(ep));
	fprintf(stderr, "parent name %s\n", myname(ep->e_parent));
	if (ep->e_sibling != NIL)
		fprintf(stderr, "sibling name: %s\n", myname(ep->e_sibling));
	if (ep->e_entries != NIL)
		fprintf(stderr, "next entry name: %s\n", myname(ep->e_entries));
	if (ep->e_links != NIL)
		fprintf(stderr, "next link name: %s\n", myname(ep->e_links));
	if (ep->e_next != NIL)
		fprintf(stderr, "next hashchain name: %s\n", myname(ep->e_next));
	fprintf(stderr, "entry type: %s\n",
		ep->e_type == NODE ? "NODE" : "LEAF");
	fprintf(stderr, "inode number: %ld\n", ep->e_ino);
	strcpy(flagbuf, "|NIL");
	flagbuf[0] = '\0';
	if (ep->e_flags & REMOVED)
		strcat(flagbuf, "|REMOVED");
	if (ep->e_flags & TMPNAME)
		strcat(flagbuf, "|TMPNAME");
	if (ep->e_flags & EXTRACT)
		strcat(flagbuf, "|EXTRACT");
	if (ep->e_flags & NEW)
		strcat(flagbuf, "|NEW");
	if (ep->e_flags & KEEP)
		strcat(flagbuf, "|KEEP");
	panic("flags: %s\n", &flagbuf[1]);
}

/*
 * canonicalize file names to always start with ``./''
 */
canon(rawname, canonname)
	char *rawname, *canonname;
{
	int len;

	if (strcmp(rawname, ".") == 0 || strncmp(rawname, "./", 2) == 0)
		(void) strcpy(canonname, "");
	else if (rawname[0] == '/')
		(void) strcpy(canonname, ".");
	else
		(void) strcpy(canonname, "./");
	(void) strcat(canonname, rawname);
	len = strlen(canonname) - 1;
	if (canonname[len] == '/')
		canonname[len] = '\0';
}

/*
 * elicit a reply
 */
reply(question)
	char *question;
{
	char c;

	fprintf(stderr, "%s? ", question);
	do	{
		fprintf(stderr, "[yn] ");
		c = getchar();
		while (c != '\n' && getchar() != '\n')
			/* void */;
	} while (c != 'y' && c != 'n');
	if (c == 'y')
		return (GOOD);
	return (FAIL);
}
