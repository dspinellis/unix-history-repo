/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)symtab.c	3.5	(Berkeley)	83/01/16";
#endif

#include "restore.h"
#include <sys/stat.h>

struct symtableheader {
	long	volno;
	long	stringsize;
	long	entrytblsize;
	time_t	dumptime;
	time_t	dumpdate;
	ino_t	maxino;
};

static struct entry *freelist = NIL;
static struct entry **entry;
static long entrytblsize;
/* used to scale maxino to get inode hash table size */
#define HASHFACTOR 5

/*
 * Look up an entry by inode number
 */
struct entry *
lookupino(inum)
	ino_t inum;
{
	register struct entry *ep;

	if (inum < ROOTINO || inum >= maxino)
		return (NIL);
	for (ep = entry[inum % entrytblsize]; ep != NIL; ep = ep->e_next)
		if (ep->e_ino == inum)
			return (ep);
	return (NIL);
}

/*
 * Add an entry into the entry table
 */
addino(inum, np)
	ino_t inum;
	struct entry *np;
{
	struct entry **epp;

	if (inum < ROOTINO || inum >= maxino)
		panic("addino: out of range %d\n", inum);
	epp = &entry[inum % entrytblsize];
	np->e_next = *epp;
	*epp = np;
	if (dflag)
		for (np = np->e_next; np != NIL; np = np->e_next)
			if (np->e_ino == inum)
				badentry(np, "duplicate inum");
}

/*
 * Delete an entry from the entry table
 */
deleteino(inum)
	ino_t inum;
{
	register struct entry *next;
	struct entry **prev;

	if (inum < ROOTINO || inum >= maxino)
		panic("deleteino: out of range %d\n", inum);
	prev = &entry[inum % entrytblsize];
	for (next = *prev; next != NIL; next = next->e_next) {
		if (next->e_ino == inum) {
			*prev = next->e_next;
			return;
		}
		prev = &next->e_next;
	}
	panic("deleteino: %d not found\n", inum);
}

/*
 * Look up an entry by name
 */
struct entry *
lookupname(name)
	char *name;
{
	register struct entry *ep;
	register char *np, *cp;
	char buf[BUFSIZ];

	cp = name;
	for (ep = lookupino(ROOTINO); ep != NIL; ep = ep->e_entries) {
		for (np = buf; *cp != '/' && *cp != '\0'; )
			*np++ = *cp++;
		*np = '\0';
		for ( ; ep != NIL; ep = ep->e_sibling)
			if (strcmp(ep->e_name, buf) == 0)
				break;
		if (ep == NIL)
			break;
		if (*cp++ == '\0')
			return (ep);
	}
	return (NIL);
}

/*
 * Look up the parent of a pathname
 */
struct entry *
lookupparent(name)
	char *name;
{
	struct entry *ep;
	char *tailindex;

	tailindex = rindex(name, '/');
	if (tailindex == 0)
		return (NIL);
	*tailindex = '\0';
	ep = lookupname(name);
	*tailindex = '/';
	if (ep == NIL)
		return (NIL);
	if (ep->e_type != NODE)
		panic("%s is not a directory\n", name);
	return (ep);
}

/*
 * Determine the current pathname of a node or leaf
 */
char *
myname(ep)
	register struct entry *ep;
{
	register char *cp;
	static char namebuf[BUFSIZ];

	for (cp = &namebuf[BUFSIZ - 2]; cp > &namebuf[ep->e_namlen]; ) {
		cp -= ep->e_namlen;
		bcopy(ep->e_name, cp, (long)ep->e_namlen);
		if (ep == lookupino(ROOTINO))
			return (cp);
		*(--cp) = '/';
		ep = ep->e_parent;
	}
	panic("%s: pathname too long\n", cp);
	return(cp);
}

/*
 * add an entry to the symbol table
 */
struct entry *
addentry(name, inum, type)
	char *name;
	ino_t inum;
	int type;
{
	register struct entry *np, *ep;

	if (freelist != NIL) {
		np = freelist;
		freelist = np->e_sibling;
		bzero((char *)np, (long)sizeof(struct entry));
	} else {
		np = (struct entry *)calloc(1, sizeof(struct entry));
	}
	np->e_ino = inum;
	np->e_type = type & ~LINK;
	ep = lookupparent(name);
	if (ep == NIL) {
		if (inum != ROOTINO || lookupino(ROOTINO) != NIL)
			panic("bad name to addentry %s\n", name);
		np->e_name = savename(name);
		np->e_namlen = strlen(name);
		np->e_parent = np;
		addino(ROOTINO, np);
		return (np);
	}
	np->e_name = savename(rindex(name, '/') + 1);
	np->e_namlen = strlen(np->e_name);
	np->e_parent = ep;
	np->e_sibling = ep->e_entries;
	ep->e_entries = np;
	if (type & LINK) {
		ep = lookupino(inum);
		if (ep == NIL)
			panic("link to non-existant name\n");
		np->e_links = ep->e_links;
		ep->e_links = np;
	} else if (inum != 0) {
		if (lookupino(inum) != NIL)
			panic("duplicate entry\n");
		addino(inum, np);
	}
	return (np);
}

/*
 * delete an entry from the symbol table
 */
freeentry(ep)
	register struct entry *ep;
{
	register struct entry *np;

	np = lookupino(ep->e_ino);
	if (np == NIL)
		badentry(ep, "lookupino failed");
	if (ep->e_flags != REMOVED)
		badentry(ep, "not marked REMOVED");
	if (np->e_type == NODE) {
		if (np == ep && np->e_links != NIL)
			badentry(ep, "freeing referenced directory");
		if (ep->e_entries != NIL)
			badentry(ep, "freeing non-empty directory");
	}
	if (np == ep) {
		deleteino(ep->e_ino);
		addino(ep->e_ino, ep->e_links);
	} else {
		for (; np != NIL; np = np->e_links) {
			if (np->e_links == ep) {
				np->e_links = ep->e_links;
				break;
			}
		}
		if (np == NIL)
			badentry(ep, "link not found");
	}
	removeentry(ep);
	free(ep->e_name);
	ep->e_sibling = freelist;
	freelist = ep;
}

/*
 * Relocate an entry in the tree structure
 */
moveentry(ep, newname)
	register struct entry *ep;
	char *newname;
{
	struct entry *np;
	char *cp;
	long len;

	np = lookupparent(newname);
	if (np == NIL)
		badentry(ep, "cannot move ROOT");
	if (np != ep->e_parent) {
		removeentry(ep);
		ep->e_parent = np;
		ep->e_sibling = np->e_entries;
		np->e_entries = ep;
	}
	cp = rindex(newname, '/') + 1;
	len = strlen(cp);
	if (ep->e_flags & TMPNAME)
		ep->e_namlen--;
	if (ep->e_namlen >= len) {
		strcpy(ep->e_name, cp);
	} else {
		free(ep->e_name);
		ep->e_name = savename(cp);
	}
	ep->e_namlen = len;
	if (cp[len - 1] == TMPCHAR)
		ep->e_flags |= TMPNAME;
	else
		ep->e_flags &= ~TMPNAME;
}

/*
 * Remove an entry in the tree structure
 */
removeentry(ep)
	register struct entry *ep;
{
	register struct entry *np;

	np = ep->e_parent;
	if (np->e_entries == ep) {
		np->e_entries = ep->e_sibling;
	} else {
		for (np = np->e_entries; np != NIL; np = np->e_sibling) {
			if (np->e_sibling == ep) {
				np->e_sibling = ep->e_sibling;
				break;
			}
		}
		if (np == NIL)
			badentry(ep, "cannot find entry in parent list");
	}
}

/*
 * allocate space for a name
 */
char *
savename(name)
	char *name;
{
	long len;
	char *cp;

	if (name == NULL)
		panic("bad name\n");
	len = strlen(name) + 2;
	len = (len + 3) & ~3;
	cp = malloc((unsigned)len);
	(void) strcpy(cp, name);
	return (cp);
}

/*
 * dump a snapshot of the symbol table
 */
dumpsymtable(filename, checkpt)
	char *filename;
	long checkpt;
{
	register struct entry *ep;
	register ino_t i;
	struct entry *next;
	long mynum = 0, stroff = 0;
	FILE *fd;
	struct symtableheader hdr;

	vprintf(stdout, "Check pointing the restore\n");
	if ((fd = fopen(filename, "w")) == NULL) {
		perror("fopen");
		panic("cannot create save file %s for symbol table\n",
			filename);
	}
	clearerr(fd);
	/*
	 * Assign indicies to each entry
	 * Write out the string entries
	 */
	for (i = ROOTINO; i < maxino; i++) {
		for (ep = lookupino(i); ep != NIL; ep = ep->e_links) {
			ep->e_index = mynum++;
			fwrite(ep->e_name, sizeof(char), (int)ep->e_namlen, fd);
			ep->e_name = (char *)stroff;
			stroff += ep->e_namlen;
		}
	}
	/*
	 * Convert entry pointers to indexes, and output
	 */
	for (i = 0; i < entrytblsize; i++) {
		if (entry[i] == NIL)
			continue;
		entry[i] = (struct entry *)entry[i]->e_index;
	}
	fwrite((char *)entry, sizeof(struct entry *), (int)entrytblsize, fd);
	/*
	 * Convert pointers to indexes, and output
	 */
	for (i = ROOTINO; i < maxino; i++) {
		for (ep = lookupino(i); ep != NIL; ep = next) {
			next = ep->e_links;
			ep->e_parent = (struct entry *)ep->e_parent->e_index;
			if (ep->e_links != NIL)
				ep->e_links =
					(struct entry *)ep->e_links->e_index;
			if (ep->e_sibling != NIL)
				ep->e_sibling =
					(struct entry *)ep->e_sibling->e_index;
			if (ep->e_entries != NIL)
				ep->e_entries =
					(struct entry *)ep->e_entries->e_index;
			if (ep->e_next != NIL)
				ep->e_next =
					(struct entry *)ep->e_next->e_index;
			fwrite((char *)ep, sizeof(struct entry), 1, fd);
		}
	}
	hdr.volno = checkpt;
	hdr.maxino = maxino;
	hdr.entrytblsize = entrytblsize;
	hdr.stringsize = stroff;
	hdr.dumptime = dumptime;
	hdr.dumpdate = dumpdate;
	fwrite((char *)&hdr, sizeof(struct symtableheader), 1, fd);
	if (ferror(fd)) {
		perror("fwrite");
		panic("output error to file %s writing symbol table\n",
			filename);
	}
	fclose(fd);
}

/*
 * Initialize a symbol table from a file
 */
initsymtable(filename)
	char *filename;
{
	char *base;
	long tblsize;
	register struct entry *ep;
	struct entry *baseep, *lep;
	struct symtableheader hdr;
	struct stat stbuf;
	register long i;
	int fd;

	vprintf(stdout, "Initialize symbol table.\n");
	if (filename == NULL) {
		entrytblsize = maxino / HASHFACTOR;
		entry = (struct entry **)
			calloc((unsigned)entrytblsize, sizeof(struct entry *));
		if (entry == (struct entry **)NIL)
			panic("no memory for entry table\n");
		(void)addentry(".", ROOTINO, NODE);
		return;
	}
	if ((fd = open(filename, 0)) < 0) {
		perror("open");
		panic("cannot open symbol table file %s\n", filename);
	}
	if (fstat(fd, &stbuf) < 0) {
		perror("stat");
		panic("cannot stat symbol table file %s\n", filename);
	}
	tblsize = stbuf.st_size - sizeof(struct symtableheader);
	base = malloc((unsigned)tblsize);
	if (base == NULL)
		panic("cannot allocate space for symbol table\n");
	if (read(fd, base, (int)tblsize) < 0 ||
	    read(fd, (char *)&hdr, sizeof(struct symtableheader)) < 0) {
		perror("read");
		panic("cannot read symbol table file %s\n", filename);
	}
	switch (command) {
	case 'r':
		/*
		 * For normal continuation, insure that we are using
		 * the next incremental tape
		 */
		if (hdr.dumptime != dumpdate) {
			if (hdr.dumptime < dumpdate)
				fprintf(stderr, "Incremental tape too low\n");
			else
				fprintf(stderr, "Incremental tape too high\n");
			done(1);
		}
		break;
	case 'R':
		/*
		 * For restart, insure that we are using the same tape
		 */
		curfile.action = SKIP;
		dumptime = hdr.dumptime;
		dumpdate = hdr.dumpdate;
		getvol(hdr.volno);
		break;
	default:
		panic("initsymtable called from command %c\n", command);
		break;
	}
	maxino = hdr.maxino;
	entrytblsize = hdr.entrytblsize;
	entry = (struct entry **)(base + hdr.stringsize);
	baseep = (struct entry *)(&entry[entrytblsize]);
	lep = (struct entry *)(base + tblsize);
	for (i = 0; i < entrytblsize; i++) {
		if (entry[i] == NIL)
			continue;
		entry[i] = &baseep[(long)entry[i]];
	}
	for (ep = baseep; ep < lep; ep++) {
		ep->e_name = base + (long)ep->e_name;
		ep->e_parent = &baseep[(long)ep->e_parent];
		if (ep->e_sibling != NIL)
			ep->e_sibling = &baseep[(long)ep->e_sibling];
		if (ep->e_links != NIL)
			ep->e_links = &baseep[(long)ep->e_links];
		if (ep->e_entries != NIL)
			ep->e_entries = &baseep[(long)ep->e_entries];
		if (ep->e_next != NIL)
			ep->e_next = &baseep[(long)ep->e_next];
	}
}
