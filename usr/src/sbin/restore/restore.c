/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)restore.c	3.1	(Berkeley)	83/02/18";
#endif

#include "restore.h"

/*
 *	Mark entries to be removed.
 */
markremove()
{
	register struct entry *ep;
	register int i;

	vprintf(stdout, "Mark entries to be removed.\n");
	for (i = ROOTINO; i < maxino; i++) {
		ep = lookupino(i);
		if (ep == NIL)
			continue;
		if (BIT(i, clrimap)) {
			for ( ; ep != NIL; ep = ep->e_links) {
				ep->e_flags |= REMOVE;
				dprintf(stdout, "%s: REMOVE\n", myname(ep));
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
	if (mflag) {
		ep = addentry(name, ino, type);
	} else {
		(void) sprintf(buf, "./%u", ino);
		ep = addentry(buf, ino, type);
	}
	ep->e_flags |= NEW;
}

/*
 *	For each directory entry, determine which catagory it falls
 *	into as follows:
 *	KEEP - entries that are to be left alone.
 *	NEW - new entries to be added.
 *	RENAME - entries whose name is to be changed.
 *	EXTRACT - files that must be updated with new contents.
 *	CHANGE - REMOVE followed by EXTRACT.
 *	RENUMBER - same as CHANGE, but not reusing same inode number.
 */
void
markfile(name, ino, type)
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
	 * directory heirarchy, with a full path name.
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
		if ((np->e_flags & REMOVE) == 0)
			panic("%s: not on remove list\n", name);
		np = NIL;
		key &= ~NAMEFND;
	}
	if (key & NAMEFND)
		strcat(keybuf, "|NAMEFND");
	if ((key & ONTAPE) &&
	  (((key & INOFND) && ip->e_type != type) ||
	   ((key & NAMEFND) && np->e_type != type))) {
		key |= MODECHG;
		strcat(keybuf, "|MODECHG");
	}
	switch (key) {

	case INOFND|NAMEFND:
		if (ip->e_flags & RENAME) {
			ip->e_flags &= ~RENAME;
			ep = addentry(ip->e_newname, ip->e_ino,
			    ip->e_type|LINK);
			dprintf(stdout, "[%s] %s: RENAME become LINK\n", keyval,
			    ip->e_newname);
			np->e_flags |= NEW;
			free(ip->e_newname);
			ip->e_newname = NULL;
		}
		ip->e_flags |= KEEP;
		dprintf(stdout, "[%s] %s: KEEP\n", keyval, name);
		break;

	case ONTAPE:
		ep = addentry(name, ino, type);
		ep->e_flags |= NEW;
		dprintf(stdout, "[%s] %s: NEW\n", keyval, name);
		break;

	case ONTAPE|INOFND:
		ip->e_flags |= EXTRACT;
		dprintf(stdout, "[%s] %s: EXTRACT\n", keyval, name);
		/* fall through */
	case INOFND:
		if (ip->e_flags & (KEEP|NEW|EXTRACT)) {
			ep = addentry(name, ip->e_ino, ip->e_type|LINK);
			ep->e_flags |= NEW;
			dprintf(stdout, "[%s] %s: NEW\n", keyval, name);
			break;
		}
		ip->e_newname = savename(name);
		ip->e_flags |= RENAME;
		dprintf(stdout, "[%s] %s: RENAME\n", keyval, name);
		break;

	case ONTAPE|NAMEFND:
		if ((np->e_flags & REMOVE) == 0)
			panic("[%s] %s: not on remove list\n", keyval, name);
		renumber(np, ino);
		/* fall through */
	case ONTAPE|INOFND|NAMEFND:
		np->e_flags |= EXTRACT;
		dprintf(stdout, "[%s] %s: EXTRACT\n", keyval, name);
		break;

	case ONTAPE|NAMEFND|MODECHG:
		renumber(np, ino);
		ip = np;
		/* fall through */
	case ONTAPE|INOFND|MODECHG:
	case ONTAPE|INOFND|NAMEFND|MODECHG:
		if ((ip->e_flags & REMOVE) == 0)
			panic("[%s] %s: not on remove list\n", keyval, name);
		ip->e_flags |= CHANGE;
		dprintf(stdout, "[%s] %s: CHANGE\n", keyval, name);
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
 *	Find unreferenced names.
 */
findunref()
{
	register struct entry *ep, *np;
	register int i;

	vprintf(stdout, "Find unreferenced names.\n");
	for (i = ROOTINO; i < maxino; i++) {
		ep = lookupino(i);
		if (ep == NIL || ep->e_type == LEAF || !BIT(i, dumpmap))
			continue;
		for (np = ep->e_entries; np != NIL; np = np->e_sibling) {
			if (np->e_flags == 0) {
				np->e_flags |= REMOVE;
				dprintf(stdout, "%s: REMOVE\n", myname(np));
			}
		}
	}
}

/*
 *	Remove old leaves.
 */
removeleaves()
{
	register struct entry *ep;
	register int i;

	vprintf(stdout, "Remove old leaves.\n");
	for (i = ROOTINO; i < maxino; i++) {
		for (ep = lookupino(i); ep != NIL; ep = ep->e_links) {
			if (ep->e_type != LEAF || (ep->e_flags & REMOVE) == 0)
				continue;
			removeleaf(ep);
			ep->e_flags &= ~REMOVE;
			if ((ep->e_flags & (CHANGE|EXTRACT)) == 0)
				freeentry(ep);
		}
	}
}
			
/*
 *	Rename nodes (directories).
 */
renamenodes()
{
	register struct entry *ep, *np, *pp;
	register int i;
	char oldname[MAXPATHLEN];

	vprintf(stdout, "Rename nodes (directories).\n");
	for (i = ROOTINO; i < maxino; i++) {
		np = lookupino(i);
		if (np == NIL || np->e_type != NODE ||
		    (np->e_flags & RENAME) == 0)
			continue;
		strcpy(oldname, myname(np));
		if (np->e_newname == NULL) {
			badentry(np, "no new name");
			continue;
		}
		np->e_flags &= ~RENAME;
		if (strcmp(oldname, np->e_newname) == 0)
			continue;
		ep = lookupname(np->e_newname);
		if (ep != NIL) {
			if (ep->e_flags & TMPNODE) {
				movecontents(ep, np);
				removenode(ep);
				freeentry(ep);
			} else {
				mktempname(ep);
			}
		} else {
			pp = pathcheck(np->e_newname, TMPNODE);
			if (pp == np) {
				mktempname(np);
				strcpy(oldname, myname(np));
				pp = pathcheck(np->e_newname, TMPNODE);
				if (pp == np)
					badentry(np, "mktempname failed");
			}
		}
		renameit(oldname, np->e_newname);
		moveentry(np, np->e_newname);
	}
}

/*
 *	Create new nodes (directories).
 */
createnodes()
{
	register struct entry *ep;
	register int i;

	vprintf(stdout, "Create new nodes (directories).\n");
	for (i = ROOTINO; i < maxino; i++) {
		ep = lookupino(i);
		if (ep == NIL || ep->e_type == LEAF ||
		    ((ep->e_flags & (EXTRACT|CHANGE|NEW)) == 0))
			continue;
		if (ep->e_flags & (CHANGE|NEW)) {
			if (ep->e_flags & REMOVE)
				badentry(ep, "bad REMOVE flag");
			newnode(ep);
			ep->e_flags &= ~(CHANGE|NEW);
			continue;
		}
		if (ep->e_flags & REMOVE) {
			if (ep->e_flags & RENUMBER)
				ep->e_flags &= ~REMOVE;
			else
				badentry(ep, "REMOVE without RENUMBER flag");
			if (ep->e_flags & (TMPNODE|TMPNAME))
				badentry(ep, "creating TMPNODE or TMPNAME");
			ep->e_flags &= ~EXTRACT;
		}
	}
}

/*
 *	Rename leaves.
 */
renameleaves()
{
	register struct entry *ep, *np;
	register int i;

	vprintf(stdout, "Rename leaves.\n");
	for (i = ROOTINO; i < maxino; i++) {
		for (ep = lookupino(i); ep != NIL; ep = ep->e_links) {
			if ((ep->e_flags & RENAME) == 0)
				continue;
			if (ep->e_type == NODE)
				badentry(ep, "too late to RENAME directory");
			ep->e_flags &= ~RENAME;
			np = lookupname(ep->e_newname);
			if (np != NIL)
				mktempname(np);
			renameit(myname(ep), ep->e_newname);
			moveentry(ep, ep->e_newname);
		}
	}
}

/*
 *	Remove old nodes (directories).
 */
removenodes()
{
	register struct entry *ep;
	register int i;

	vprintf(stdout, "Remove old nodes (directories).\n");
	for (i = ROOTINO; i < maxino; i++) {
		ep = lookupino(i);
		if (ep == NIL || (ep->e_flags & REMOVE) == 0)
			continue;
		removenode(ep);
		ep->e_flags &= ~REMOVE;
		if ((ep->e_flags & (CHANGE|EXTRACT)) == 0)
			freeentry(ep);
	}
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
			ep->e_flags &= ~(NEW|EXTRACT|CHANGE);
			first = lowerbnd(first + 1);
		}
		if (first != curfile.ino)
			panic("expected next file %d, got %d\n",
				first, curfile.ino);
		ep = lookupino(curfile.ino);
		if (ep == NIL)
			panic("unknown file on tape\n");
		if ((ep->e_flags & (NEW|EXTRACT|CHANGE)) == 0)
			badentry(ep, "unexpected file on tape");
		ep->e_flags &= ~(NEW|EXTRACT|CHANGE);
		extractfile(myname(ep));
		if (curvol != volno) {
			dumpsymtable(symtabfile, volno);
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
	first = lowerbnd(ROOTINO);
	last = upperbnd(maxino);
	for (;;) {
		first = lowerbnd(first);
		last = upperbnd(last);
		if (first > last)
			return;
		while (curfile.ino > last) {
			curfile.action = SKIP;
			getvol((long)0);
		}
		next = lowerbnd(curfile.ino);
		do	{
			curvol = volno;
			while (next > curfile.ino && volno == curvol)
				skipfile();
		} while (volno == curvol + 1);
		if (volno != curvol)
			continue;
		while (next < curfile.ino) {
			ep = lookupino(next);
			if (ep == NIL)
				panic("corrupted symbol table\n");
			fprintf(stderr, "%s: not found on tape\n", myname(ep));
			ep->e_flags &= ~NEW;
			next = lowerbnd(next + 1);
		}
		if (next == curfile.ino) {
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
	register long i;
	char name[BUFSIZ];

	vprintf(stdout, "Add links\n");
	for (i = ROOTINO; i < maxino; i++) {
		ep = lookupino(i);
		if (ep == NIL)
			continue;
		for (np = ep->e_links; np != NIL; np = np->e_links) {
			strcpy(name, myname(ep));
			if (ep->e_type == NODE) {
				vprintf(stdout, "changing hard link to directory to a symbolic link:");
				linkit(name, myname(np), SYMLINK);
			} else {
				linkit(name, myname(np), HARDLINK);
			}
		}
	}
}

/*
 *	Check the symbol table.
 */
checkrestore()
{
	register struct entry *ep;
	register long i;

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
