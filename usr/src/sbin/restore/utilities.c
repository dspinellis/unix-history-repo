/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)utilities.c	3.1	(Berkeley)	83/02/18";
#endif

#include "restore.h"

/*
 * Move the contents of a directory to a new directory.
 */
movecontents(from, to)
	struct entry *from, *to;
{
	register struct entry *ep;
	struct entry *np;
	register char *targetp;
	char target[BUFSIZ];

	strcpy(target, myname(to));
	targetp = &target[strlen(target)];
	*targetp++ = '/';
	for (ep = from->e_entries; ep != NIL; ) {
		strcpy(targetp, ep->e_name);
		if (ep->e_flags & TMPNAME)
			badentry(ep, "movecontents: found TMPNAME");
		np = lookupname(target);
		if (np != NIL)
			mktempname(np);
		renameit(myname(ep), target);
		np = ep->e_sibling;
		moveentry(ep, target);
		ep = np;
	}
}

/*
 * Insure that all the components of a pathname exist.
 */
struct entry *
pathcheck(name, type)
	char *name;
	char type;
{
	register char *cp;
	struct entry *ep;
	char *start, *last;

	start = index(name, '/');
	last = rindex(name, '/');
	if (last == 0)
		panic("bad name %s to pathcheck\n", name);
	if (start == last)
		return (lookupino(ROOTINO));
	for (cp = start; *cp != '\0'; cp++) {
		if (*cp != '/')
			continue;
		*cp = '\0';
		ep = lookupname(name);
		if (ep == NIL) {
			ep = addentry(name, (ino_t)0, NODE);
			ep->e_flags |= type;
			newnode(ep);
		}
		*cp = '/';
	}
	return (ep);
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
	if (mkdir(cp, 0666) < 0) {
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
	ep->e_flags &= ~(TMPNAME|TMPNODE);
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
		if (ep->e_flags & (NEW|EXTRACT|CHANGE))
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
		if (ep->e_flags & (NEW|EXTRACT|CHANGE))
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
	if (ep->e_newname != NULL)
		fprintf(stderr, "new name: %s\n", ep->e_newname);
	fprintf(stderr, "parent name %s\n", myname(ep->e_parent));
	if (ep->e_sibling != NIL)
		fprintf(stderr, "sibling name: %s\n", myname(ep->e_sibling));
	if (ep->e_entries != NIL)
		fprintf(stderr, "next entry name: %s\n", myname(ep->e_entries));
	if (ep->e_links != NIL)
		fprintf(stderr, "next link name: %s\n", myname(ep->e_links));
	fprintf(stderr, "entry type: %s\n",
		ep->e_type == NODE ? "NODE" : "LEAF");
	fprintf(stderr, "inode number: %ld\n", ep->e_ino);
	strcpy(flagbuf, "|NIL");
	flagbuf[0] = '\0';
	if (ep->e_flags & REMOVE)
		strcat(flagbuf, "|REMOVE");
	if (ep->e_flags & REMOVED)
		strcat(flagbuf, "|REMOVED");
	if (ep->e_flags & RENAME)
		strcat(flagbuf, "|RENAME");
	if (ep->e_flags & TMPNAME)
		strcat(flagbuf, "|TMPNAME");
	if (ep->e_flags & TMPNODE)
		strcat(flagbuf, "|TMPNODE");
	if (ep->e_flags & EXTRACT)
		strcat(flagbuf, "|EXTRACT");
	if (ep->e_flags & RENUMBER)
		strcat(flagbuf, "|RENUMBER");
	if (ep->e_flags & CHANGE)
		strcat(flagbuf, "|CHANGE");
	if (ep->e_flags & NEW)
		strcat(flagbuf, "|NEW");
	if (ep->e_flags & KEEP)
		strcat(flagbuf, "|KEEP");
	panic("flags: %s\n", &flagbuf[1]);
}

/*
 * respond to interrupts
 */
onintr()
{
	if (reply("restore interrupted, continue"))
		return;
	done(1);
}

/*
 * handle unexpected inconsistencies
 */
/* VARARGS1 */
panic(msg, d1, d2)
	char *msg;
	long d1, d2;
{

	fprintf(stderr, msg, d1, d2);
	if (reply("abort"))
		abort();
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
