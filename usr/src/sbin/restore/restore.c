/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)restore.c	3.6	(Berkeley)	83/01/16";
#endif

#include "restore.h"

static struct entry *removelist;	/* list of nodes to be deleted */

/*
 *	Mark entries to be removed.
 */
removeoldleaves()
{
	register struct entry *ep;
	register ino_t i;

	vprintf(stdout, "Mark entries to be removed.\n");
	for (i = ROOTINO; i < maxino; i++) {
		ep = lookupino(i);
		if (ep == NIL)
			continue;
		if (BIT(i, clrimap)) {
			for ( ; ep != NIL; ep = ep->e_links) {
				dprintf(stdout, "%s: REMOVE\n", myname(ep));
				if (ep->e_type == LEAF) {
					removeleaf(ep);
					freeentry(ep);
				} else {
					mktempname(ep);
					deleteino(ep->e_ino);
					ep->e_next = removelist;
					removelist = ep;
				}
			}
		}
	}
}

/*
 *	List entries on the tape.
 */
void
listfile(name, ino, type)
	char *name;
	ino_t ino;
	int type;
{

	if (BIT(ino, dumpmap) == 0) {
		return;
	}
	vprintf(stdout, "%s\t", type == LEAF ? "leaf" : "dir");
	fprintf(stdout, "%10d\t%s\n", ino, name);
}

/*
 *	Request that new entries be extracted.
 */
void
addfile(name, ino, type)
	char *name;
	ino_t ino;
	int type;
{
	register struct entry *ep;
	char buf[100];

	if (BIT(ino, dumpmap) == 0) {
		vprintf(stdout, "%s: not on the tape\n", name);
		return;
	}
	if (!mflag) {
		sprintf(buf, "./%u", ino);
		name = buf;
		if (type == NODE) {
			(void) genliteraldir(name, ino);
			return;
		}
	}
	if (ino == ROOTINO)
		return;
	ep = lookupino(ino);
	if (ep != NIL) {
		if (strcmp(name, myname(ep)) == 0)
			return;
		type |= LINK;
	}
	ep = addentry(name, ino, type);
	if (type == NODE) {
		newnode(ep);
		return;
	}
	ep->e_flags |= NEW;
	return;
}

/*
 *	For each directory entry on the incremental tape, determine which
 *	category it falls into as follows:
 *	KEEP - entries that are to be left alone.
 *	NEW - new entries to be added.
 *	EXTRACT - files that must be updated with new contents.
 *	LINK - new links to be added.
 *	Renames are done at the same time.
 */
void
nodeupdates(name, ino, type)
	char *name;
	ino_t ino;
	int type;
{
	register struct entry *ep, *np, *ip;
	int key = 0;
		/* key values */
#		define ONTAPE	0x1
#		define INOFND	0x2
#		define NAMEFND	0x4
#		define MODECHG	0x8
	char keybuf[32], *keyval = &keybuf[1];

	/*
	 * This routine is called once for each element in the 
	 * directory hierarchy, with a full path name.
	 * The "type" value is incorrectly specified as LEAF for
	 * directories that are not on the dump tape.
	 */
	strcpy(keybuf, "|NIL");
	keybuf[0] = '\0';
	if (BIT(ino, dumpmap)) {
		key |= ONTAPE;
		strcat(keybuf, "|ONTAPE");
	}
	np = lookupname(name);
	if (np != NIL)
		key |= NAMEFND;
	ip = lookupino(ino);
	if (ip != NIL) {
		key |= INOFND;
		strcat(keybuf, "|INOFND");
		for (ep = ip; ep != NIL; ep = ep->e_links)
			if (ep == np) {
				ip = ep;
				break;
			}
	}
	if ((key & (INOFND|NAMEFND) == (INOFND|NAMEFND)) && ip != np) {
		mktempname(np);
		dprintf(stdout, "[%s] %s: mktempname\n", keyval, name);
		np = NIL;
		key &= ~NAMEFND;
	}
	if (key & NAMEFND)
		strcat(keybuf, "|NAMEFND");
	if ((key & ONTAPE) &&
	  (((key & INOFND) && ip->e_type != type) ||
	   ((key & NAMEFND) && np->e_type != type))) {
		key |= MODECHG;
		(void) strcat(keybuf, "|MODECHG");
	}
	switch (key) {

	case INOFND|NAMEFND:
		ip->e_flags |= KEEP;
		dprintf(stdout, "[%s] %s: KEEP\n", keyval, name);
		break;

	case ONTAPE:
		ep = addentry(name, ino, type);
		if (type == NODE)
			newnode(ep);
		else
			ep->e_flags |= NEW;
		ep->e_flags |= KEEP;
		dprintf(stdout, "[%s] %s: KEEP|NEW\n", keyval, name);
		break;

	case ONTAPE|INOFND:
		if (type == LEAF)
			ip->e_flags |= EXTRACT;
		dprintf(stdout, "[%s] %s: EXTRACT\n", keyval, name);
		/* fall through */
	case INOFND:
		if (ip->e_flags & KEEP) {
			if (ip->e_type == NODE)
				panic("%s linked to directory %s\n",
				    name, myname(ip));
			ep = addentry(name, ino, type|LINK);
			ep->e_flags |= NEW;
			dprintf(stdout, "[%s] %s: NEW\n", keyval, name);
			break;
		}
		renameit(myname(ip), name);
		moveentry(ip, name);
		ip->e_flags |= KEEP;
		dprintf(stdout, "[%s] %s: RENAME\n", keyval, name);
		break;

	case ONTAPE|NAMEFND:
	case ONTAPE|NAMEFND|MODECHG:
		mktempname(np);
		np = addentry(name, ino, type);
		if (type == NODE)
			newnode(np);
		/* fall through */
	case ONTAPE|INOFND|NAMEFND:
		if (type == LEAF)
			np->e_flags |= EXTRACT;
		np->e_flags |= KEEP;
		dprintf(stdout, "[%s] %s: KEEP|EXTRACT\n", keyval, name);
		break;

	case ONTAPE|INOFND|MODECHG:
	case ONTAPE|INOFND|NAMEFND|MODECHG:
		badentry(ip, "not removed");
		break;

	case INOFND|NAMEFND|MODECHG:
	case NAMEFND|MODECHG:
	case INOFND|MODECHG:
	case NAMEFND:
	case NIL:
		panic("[%s] %s: inconsistent state\n", keyval, name);
		break;

	case ONTAPE|MODECHG:
	case MODECHG:
	default:
		panic("[%s] %s: impossible state\n", keyval, name);
		break;
	}	
}

/*
 *	Find unreferenced link names.
 */
findunreflinks()
{
	register struct entry *ep, *np;
	register ino_t i;

	vprintf(stdout, "Find unreferenced names.\n");
	for (i = ROOTINO; i < maxino; i++) {
		ep = lookupino(i);
		if (ep == NIL || ep->e_type == LEAF || !BIT(i, dumpmap))
			continue;
		for (np = ep->e_entries; np != NIL; np = np->e_sibling) {
			if (np->e_flags == 0) {
				dprintf(stdout,
				    "%s: remove unreferenced name\n",
				    myname(np));
				removeleaf(np);
				freeentry(np);
			}
		}
	}
}

/*
 *	Remove old nodes (directories).
 */
removeoldnodes()
{
	register struct entry *ep;
	long change;

	vprintf(stdout, "Remove old nodes (directories).\n");
	do	{
		change = 0;
		for (ep = removelist; ep != NIL; ep = ep->e_next) {
			if (ep->e_entries == NIL) {
				dprintf(stdout, "%s: removed\n", myname(ep));
				removenode(ep);
				freeentry(ep);
				change++;
			}
		}
	} while (change);
	for (ep = removelist; ep != NIL; ep = ep->e_next)
		badentry(ep, "cannot remove, non-empty");
}

/*
 *	Extract new leaves.
 */
createleaves(symtabfile)
	char *symtabfile;
{
	register struct entry *ep;
	ino_t first;
	long curvol;

	if (command == 'R')
		vprintf(stdout, "Continue extraction of new leaves\n");
	else
		vprintf(stdout, "Extract new leaves.\n");
	dumpsymtable(symtabfile, volno);
	first = lowerbnd(ROOTINO);
	curvol = volno;
	while (curfile.ino < maxino) {
		while (first < curfile.ino) {
			ep = lookupino(first);
			if (ep == NIL)
				panic("%d: bad first\n", first);
			fprintf(stderr, "%s: not found on tape\n", myname(ep));
			ep->e_flags &= ~(NEW|EXTRACT);
			first = lowerbnd(first);
		}
		if (first != curfile.ino)
			panic("expected next file %d, got %d\n",
				first, curfile.ino);
		ep = lookupino(curfile.ino);
		if (ep == NIL)
			panic("unknown file on tape\n");
		if ((ep->e_flags & (NEW|EXTRACT)) == 0)
			badentry(ep, "unexpected file on tape");
		if ((ep->e_flags & EXTRACT) != 0) {
			removeleaf(ep);
			ep->e_flags &= ~REMOVED;
		}
		extractfile(myname(ep));
		ep->e_flags &= ~(NEW|EXTRACT);
		if (curvol != volno) {
			dumpsymtable(symtabfile, volno);
			skipmaps();
			curvol = volno;
		}
	}
}

/*
 *	Efficiently extract a subset of the files on a tape
 */
createfiles()
{
	register ino_t first, next, last;
	register struct entry *ep;
	long curvol;

	vprintf(stdout, "Extract requested files\n");
	curfile.action = SKIP;
	getvol((long)1);
	skipmaps();
	skipdirs();
	first = lowerbnd(ROOTINO);
	last = upperbnd(maxino - 1);
	for (;;) {
		first = lowerbnd(first);
		last = upperbnd(last);
		/*
		 * Check to see if any files remain to be extracted
		 */
		if (first > last)
			return;
		/*
		 * Reject any volumes with inodes greater
		 * than the last one needed
		 */
		while (curfile.ino > last) {
			curfile.action = SKIP;
			getvol((long)0);
			skipmaps();
			skipdirs();
		}
		/*
		 * Decide on the next inode needed.
		 * Skip across the inodes until it is found
		 * or an out of order volume change is encountered
		 */
		next = lowerbnd(curfile.ino);
		do	{
			curvol = volno;
			while (next > curfile.ino && volno == curvol)
				skipfile();
			skipmaps();
			skipdirs();
		} while (volno == curvol + 1);
		/*
		 * If volume change out of order occurred the
		 * current state must be re calculated
		 */
		if (volno != curvol)
			continue;
		/*
		 * If the current inode is greater than the one we were
		 * looking for then we missed the one we were looking for.
		 * Since we only attempt to extract files listed in the
		 * dump map, the file must have been lost due to a tape
		 * read error. Thus we report all requested files between
		 * the one we were looking for, and the one we found as
		 * missing, and delete their request flags.
		 */
		while (next < curfile.ino) {
			ep = lookupino(next);
			if (ep == NIL)
				panic("corrupted symbol table\n");
			fprintf(stderr, "%s: not found on tape\n", myname(ep));
			ep->e_flags &= ~NEW;
			next = lowerbnd(next);
		}
		/*
		 * The current inode is the one that we are looking for,
		 * so extract it per its requested name.
		 */
		if (next == curfile.ino && next <= last) {
			ep = lookupino(next);
			if (ep == NIL)
				panic("corrupted symbol table\n");
			extractfile(myname(ep));
			ep->e_flags &= ~NEW;
		}
	}
}

/*
 *	Add links.
 */
createlinks()
{
	register struct entry *np, *ep;
	register ino_t i;
	char name[BUFSIZ];

	vprintf(stdout, "Add links\n");
	for (i = ROOTINO; i < maxino; i++) {
		ep = lookupino(i);
		if (ep == NIL)
			continue;
		for (np = ep->e_links; np != NIL; np = np->e_links) {
			(void) strcpy(name, myname(ep));
			if (ep->e_type == NODE) {
				vprintf(stdout, "changing hard link to directory to a symbolic link:");
				linkit(name, myname(np), SYMLINK);
			} else {
				linkit(name, myname(np), HARDLINK);
			}
			np->e_flags &= ~NEW;
		}
	}
}

/*
 *	Check the symbol table.
 */
checkrestore()
{
	register struct entry *ep;
	register ino_t i;

	vprintf(stdout, "Check the symbol table.\n");
	for (i = ROOTINO; i < maxino; i++) {
		for (ep= lookupino(i); ep != NIL; ep = ep->e_links) {
			ep->e_flags &= ~KEEP;
			if (ep->e_flags != NULL)
				badentry(ep, "incomplete operations");
		}
	}
}

/*
 *	Compare with the directory structure on the tape
 */
void
verifyfile(name, ino, type)
	char *name;
	ino_t ino;
	int type;
{
	struct entry *np, *ep;

	ep = lookupname(name);
	if (ep == NIL)
		panic("missing name %s\n", name);
	for (np = lookupino(ino); np != NIL; np = np->e_links)
		if (np == ep)
			break;
	if (np == NIL)
		panic("missing inumber %d\n", ino);
	if (ep->e_type == LEAF && type != LEAF)
		badentry(ep, "type should be LEAF");
}
