/*
 * Copyright (c) 1980, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)pass2.c	5.16 (Berkeley) 9/18/90";
#endif /* not lint */

#include <sys/param.h>
#include <ufs/dinode.h>
#include <ufs/fs.h>
#define KERNEL
#include <ufs/dir.h>
#undef KERNEL
#include <stdlib.h>
#include <string.h>
#include "fsck.h"

#define MINDIRSIZE	(sizeof (struct dirtemplate))

int	pass2check(), blksort();

pass2()
{
	register struct dinode *dp;
	register struct inoinfo **inpp, *inp;
	struct inoinfo **inpend;
	struct inodesc curino;
	struct dinode dino;
	char pathbuf[MAXPATHLEN + 1];

	switch (statemap[ROOTINO]) {

	case USTATE:
		pfatal("ROOT INODE UNALLOCATED");
		if (reply("ALLOCATE") == 0)
			errexit("");
		if (allocdir(ROOTINO, ROOTINO, 0755) != ROOTINO)
			errexit("CANNOT ALLOCATE ROOT INODE\n");
		break;

	case DCLEAR:
		pfatal("DUPS/BAD IN ROOT INODE");
		if (reply("REALLOCATE")) {
			freeino(ROOTINO);
			if (allocdir(ROOTINO, ROOTINO, 0755) != ROOTINO)
				errexit("CANNOT ALLOCATE ROOT INODE\n");
			break;
		}
		if (reply("CONTINUE") == 0)
			errexit("");
		break;

	case FSTATE:
	case FCLEAR:
		pfatal("ROOT INODE NOT DIRECTORY");
		if (reply("REALLOCATE")) {
			freeino(ROOTINO);
			if (allocdir(ROOTINO, ROOTINO, 0755) != ROOTINO)
				errexit("CANNOT ALLOCATE ROOT INODE\n");
			break;
		}
		if (reply("FIX") == 0)
			errexit("");
		dp = ginode(ROOTINO);
		dp->di_mode &= ~IFMT;
		dp->di_mode |= IFDIR;
		inodirty();
		break;

	case DSTATE:
		break;

	default:
		errexit("BAD STATE %d FOR ROOT INODE", statemap[ROOTINO]);
	}
	statemap[ROOTINO] = DFOUND;
	/*
	 * Sort the directory list into disk block order.
	 */
	qsort((char *)inpsort, (size_t)inplast, sizeof *inpsort, blksort);
	/*
	 * Check the integrity of each directory.
	 */
	bzero((char *)&curino, sizeof(struct inodesc));
	curino.id_type = DATA;
	curino.id_func = pass2check;
	dino.di_mode = IFDIR;
	dp = &dino;
	inpend = &inpsort[inplast];
	for (inpp = inpsort; inpp < inpend; inpp++) {
		inp = *inpp;
		if (inp->i_isize == 0)
			continue;
		if (inp->i_isize < MINDIRSIZE) {
			direrror(inp->i_number, "DIRECTORY TOO SHORT");
			inp->i_isize = roundup(MINDIRSIZE, DIRBLKSIZ);
			if (reply("FIX") == 1) {
				dp = ginode(inp->i_number);
				dp->di_size = inp->i_isize;
				inodirty();
				dp = &dino;
			}
		} else if ((inp->i_isize & (DIRBLKSIZ - 1)) != 0) {
			getpathname(pathbuf, inp->i_number, inp->i_number);
			pwarn("DIRECTORY %s: LENGTH %d NOT MULTIPLE OF %d",
				pathbuf, inp->i_isize, DIRBLKSIZ);
			if (preen)
				printf(" (ADJUSTED)\n");
			inp->i_isize = roundup(inp->i_isize, DIRBLKSIZ);
			if (preen || reply("ADJUST") == 1) {
				dp = ginode(inp->i_number);
				dp->di_size = roundup(inp->i_isize, DIRBLKSIZ);
				inodirty();
				dp = &dino;
			}
		}
		dp->di_size = inp->i_isize;
		bcopy((char *)&inp->i_blks[0], (char *)&dp->di_db[0],
			(size_t)inp->i_numblks);
		curino.id_number = inp->i_number;
		curino.id_parent = inp->i_parent;
		(void)ckinode(dp, &curino);
	}
	/*
	 * Now that the parents of all directories have been found,
	 * make another pass to verify the value of `..'
	 */
	for (inpp = inpsort; inpp < inpend; inpp++) {
		inp = *inpp;
		if (inp->i_parent == 0 || inp->i_isize == 0)
			continue;
		if (statemap[inp->i_parent] == DFOUND &&
		    statemap[inp->i_number] == DSTATE)
			statemap[inp->i_number] = DFOUND;
		if (inp->i_dotdot == inp->i_parent ||
		    inp->i_dotdot == (ino_t)-1)
			continue;
		if (inp->i_dotdot == 0) {
			inp->i_dotdot = inp->i_parent;
			fileerror(inp->i_parent, inp->i_number, "MISSING '..'");
			if (reply("FIX") == 0)
				continue;
			(void)makeentry(inp->i_number, inp->i_parent, "..");
			lncntp[inp->i_parent]--;
			continue;
		}
		fileerror(inp->i_parent, inp->i_number,
			"BAD INODE NUMBER FOR '..'");
		if (reply("FIX") == 0)
			continue;
		lncntp[inp->i_dotdot]++;
		lncntp[inp->i_parent]--;
		inp->i_dotdot = inp->i_parent;
		(void)changeino(inp->i_number, "..", inp->i_parent);
	}
	/*
	 * Mark all the directories that can be found from the root.
	 */
	propagate();
}

pass2check(idesc)
	struct inodesc *idesc;
{
	register struct direct *dirp = idesc->id_dirp;
	register struct inoinfo *inp;
	int n, entrysize, ret = 0;
	struct dinode *dp;
	char *errmsg;
	struct direct proto;
	char namebuf[MAXPATHLEN + 1];
	char pathbuf[MAXPATHLEN + 1];

	/* 
	 * check for "."
	 */
	if (idesc->id_entryno != 0)
		goto chk1;
	if (dirp->d_ino != 0 && strcmp(dirp->d_name, ".") == 0) {
		if (dirp->d_ino != idesc->id_number) {
			direrror(idesc->id_number, "BAD INODE NUMBER FOR '.'");
			dirp->d_ino = idesc->id_number;
			if (reply("FIX") == 1)
				ret |= ALTERED;
		}
		goto chk1;
	}
	direrror(idesc->id_number, "MISSING '.'");
	proto.d_ino = idesc->id_number;
	proto.d_namlen = 1;
	(void)strcpy(proto.d_name, ".");
	entrysize = DIRSIZ(&proto);
	if (dirp->d_ino != 0 && strcmp(dirp->d_name, "..") != 0) {
		pfatal("CANNOT FIX, FIRST ENTRY IN DIRECTORY CONTAINS %s\n",
			dirp->d_name);
	} else if (dirp->d_reclen < entrysize) {
		pfatal("CANNOT FIX, INSUFFICIENT SPACE TO ADD '.'\n");
	} else if (dirp->d_reclen < 2 * entrysize) {
		proto.d_reclen = dirp->d_reclen;
		bcopy((char *)&proto, (char *)dirp, (size_t)entrysize);
		if (reply("FIX") == 1)
			ret |= ALTERED;
	} else {
		n = dirp->d_reclen - entrysize;
		proto.d_reclen = entrysize;
		bcopy((char *)&proto, (char *)dirp, (size_t)entrysize);
		idesc->id_entryno++;
		lncntp[dirp->d_ino]--;
		dirp = (struct direct *)((char *)(dirp) + entrysize);
		bzero((char *)dirp, (size_t)n);
		dirp->d_reclen = n;
		if (reply("FIX") == 1)
			ret |= ALTERED;
	}
chk1:
	if (idesc->id_entryno > 1)
		goto chk2;
	inp = getinoinfo(idesc->id_number);
	proto.d_ino = inp->i_parent;
	proto.d_namlen = 2;
	(void)strcpy(proto.d_name, "..");
	entrysize = DIRSIZ(&proto);
	if (idesc->id_entryno == 0) {
		n = DIRSIZ(dirp);
		if (dirp->d_reclen < n + entrysize)
			goto chk2;
		proto.d_reclen = dirp->d_reclen - n;
		dirp->d_reclen = n;
		idesc->id_entryno++;
		lncntp[dirp->d_ino]--;
		dirp = (struct direct *)((char *)(dirp) + n);
		bzero((char *)dirp, (size_t)proto.d_reclen);
		dirp->d_reclen = proto.d_reclen;
	}
	if (dirp->d_ino != 0 && strcmp(dirp->d_name, "..") == 0) {
		inp->i_dotdot = dirp->d_ino;
		goto chk2;
	}
	if (dirp->d_ino != 0 && strcmp(dirp->d_name, ".") != 0) {
		fileerror(inp->i_parent, idesc->id_number, "MISSING '..'");
		pfatal("CANNOT FIX, SECOND ENTRY IN DIRECTORY CONTAINS %s\n",
			dirp->d_name);
		inp->i_dotdot = (ino_t)-1;
	} else if (dirp->d_reclen < entrysize) {
		fileerror(inp->i_parent, idesc->id_number, "MISSING '..'");
		pfatal("CANNOT FIX, INSUFFICIENT SPACE TO ADD '..'\n");
		inp->i_dotdot = (ino_t)-1;
	} else if (inp->i_parent != 0) {
		/*
		 * We know the parent, so fix now.
		 */
		inp->i_dotdot = inp->i_parent;
		fileerror(inp->i_parent, idesc->id_number, "MISSING '..'");
		proto.d_reclen = dirp->d_reclen;
		bcopy((char *)&proto, (char *)dirp, (size_t)entrysize);
		if (reply("FIX") == 1)
			ret |= ALTERED;
	}
	idesc->id_entryno++;
	if (dirp->d_ino != 0)
		lncntp[dirp->d_ino]--;
	return (ret|KEEPON);
chk2:
	if (dirp->d_ino == 0)
		return (ret|KEEPON);
	if (dirp->d_namlen <= 2 &&
	    dirp->d_name[0] == '.' &&
	    idesc->id_entryno >= 2) {
		if (dirp->d_namlen == 1) {
			direrror(idesc->id_number, "EXTRA '.' ENTRY");
			dirp->d_ino = 0;
			if (reply("FIX") == 1)
				ret |= ALTERED;
			return (KEEPON | ret);
		}
		if (dirp->d_name[1] == '.') {
			direrror(idesc->id_number, "EXTRA '..' ENTRY");
			dirp->d_ino = 0;
			if (reply("FIX") == 1)
				ret |= ALTERED;
			return (KEEPON | ret);
		}
	}
	idesc->id_entryno++;
	n = 0;
	if (dirp->d_ino > maxino || dirp->d_ino <= 0) {
		fileerror(idesc->id_number, dirp->d_ino, "I OUT OF RANGE");
		n = reply("REMOVE");
	} else {
again:
		switch (statemap[dirp->d_ino]) {
		case USTATE:
			if (idesc->id_entryno <= 2)
				break;
			fileerror(idesc->id_number, dirp->d_ino, "UNALLOCATED");
			n = reply("REMOVE");
			break;

		case DCLEAR:
		case FCLEAR:
			if (idesc->id_entryno <= 2)
				break;
			if (statemap[dirp->d_ino] == DCLEAR)
				errmsg = "ZERO LENGTH DIRECTORY";
			else
				errmsg = "DUP/BAD";
			fileerror(idesc->id_number, dirp->d_ino, errmsg);
			if ((n = reply("REMOVE")) == 1)
				break;
			dp = ginode(dirp->d_ino);
			statemap[dirp->d_ino] =
			    (dp->di_mode & IFMT) == IFDIR ? DSTATE : FSTATE;
			lncntp[dirp->d_ino] = dp->di_nlink;
			goto again;

		case DSTATE:
			if (statemap[idesc->id_number] == DFOUND)
				statemap[dirp->d_ino] = DFOUND;
			/* fall through */

		case DFOUND:
			inp = getinoinfo(dirp->d_ino);
			if (inp->i_parent != 0 && idesc->id_entryno > 2) {
				getpathname(pathbuf, idesc->id_number,
				    idesc->id_number);
				getpathname(namebuf, dirp->d_ino, dirp->d_ino);
				pwarn("%s %s %s\n", pathbuf,
				    "IS AN EXTRANEOUS HARD LINK TO DIRECTORY",
				    namebuf);
				if (preen)
					printf(" (IGNORED)\n");
				else if ((n = reply("REMOVE")) == 1)
					break;
			}
			if (idesc->id_entryno > 2)
				inp->i_parent = idesc->id_number;
			/* fall through */

		case FSTATE:
			lncntp[dirp->d_ino]--;
			break;

		default:
			errexit("BAD STATE %d FOR INODE I=%d",
			    statemap[dirp->d_ino], dirp->d_ino);
		}
	}
	if (n == 0)
		return (ret|KEEPON);
	dirp->d_ino = 0;
	return (ret|KEEPON|ALTERED);
}

/*
 * Routine to sort disk blocks.
 */
blksort(inpp1, inpp2)
	struct inoinfo **inpp1, **inpp2;
{

	return ((*inpp1)->i_blks[0] - (*inpp2)->i_blks[0]);
}
