#ifndef lint
static char version[] = "@(#)dir.c	3.4 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#define KERNEL
#include <sys/dir.h>
#undef KERNEL
#include "fsck.h"

#define MINDIRSIZE	(sizeof (struct dirtemplate))

char	*endpathname = &pathname[BUFSIZ - 2];
char	*lfname = "lost+found";

DIRECT	*fsck_readdir();

descend(parentino, inumber)
	struct inodesc *parentino;
	ino_t inumber;
{
	register DINODE *dp;
	struct inodesc curino;

	bzero((char *)&curino, sizeof(struct inodesc));
	if (statemap[inumber] != DSTATE)
		errexit("BAD INODE %d TO DESCEND", statemap[inumber]);
	statemap[inumber] = DFOUND;
	dp = ginode(inumber);
	if (dp->di_size == 0) {
		direrr(inumber, "ZERO LENGTH DIRECTORY");
		if (reply("REMOVE") == 1)
			statemap[inumber] = DCLEAR;
		return;
	}
	if (dp->di_size < MINDIRSIZE) {
		direrr(inumber, "DIRECTORY TOO SHORT");
		dp->di_size = MINDIRSIZE;
		if (reply("FIX") == 1)
			inodirty();
	}
	curino.id_type = DATA;
	curino.id_func = parentino->id_func;
	curino.id_parent = parentino->id_number;
	curino.id_number = inumber;
	curino.id_filesize = dp->di_size;
	(void)ckinode(dp, &curino);
}

dirscan(idesc)
	register struct inodesc *idesc;
{
	register DIRECT *dp;
	int dsize, n;
	long blksiz;
	char dbuf[DIRBLKSIZ];

	if (idesc->id_type != DATA)
		errexit("wrong type to dirscan %d\n", idesc->id_type);
	blksiz = idesc->id_numfrags * sblock.fs_fsize;
	if (outrange(idesc->id_blkno, idesc->id_numfrags)) {
		idesc->id_filesize -= blksiz;
		return (SKIP);
	}
	idesc->id_loc = 0;
	for (dp = fsck_readdir(idesc); dp != NULL; dp = fsck_readdir(idesc)) {
		dsize = dp->d_reclen;
		bcopy((char *)dp, dbuf, dsize);
		idesc->id_dirp = (DIRECT *)dbuf;
		if ((n = (*idesc->id_func)(idesc)) & ALTERED) {
			if (getblk(&fileblk, idesc->id_blkno, blksiz) != NULL) {
				bcopy(dbuf, (char *)dp, dsize);
				dirty(&fileblk);
				sbdirty();
			} else
				n &= ~ALTERED;
		}
		if (n & STOP) 
			return (n);
	}
	return (idesc->id_filesize > 0 ? KEEPON : STOP);
}

/*
 * get next entry in a directory.
 */
DIRECT *
fsck_readdir(idesc)
	register struct inodesc *idesc;
{
	register DIRECT *dp, *ndp;
	long size, blksiz;

	blksiz = idesc->id_numfrags * sblock.fs_fsize;
	if (getblk(&fileblk, idesc->id_blkno, blksiz) == NULL) {
		idesc->id_filesize -= blksiz - idesc->id_loc;
		return NULL;
	}
	if (idesc->id_loc % DIRBLKSIZ == 0 && idesc->id_filesize > 0 &&
	    idesc->id_loc < blksiz) {
		dp = (DIRECT *)(dirblk.b_buf + idesc->id_loc);
		if (dircheck(idesc, dp))
			goto dpok;
		idesc->id_loc += DIRBLKSIZ;
		idesc->id_filesize -= DIRBLKSIZ;
		dp->d_reclen = DIRBLKSIZ;
		dp->d_ino = 0;
		dp->d_namlen = 0;
		dp->d_name[0] = '\0';
		if (dofix(idesc, "DIRECTORY CORRUPTED"))
			dirty(&fileblk);
		return (dp);
	}
dpok:
	if (idesc->id_filesize <= 0 || idesc->id_loc >= blksiz)
		return NULL;
	dp = (DIRECT *)(dirblk.b_buf + idesc->id_loc);
	idesc->id_loc += dp->d_reclen;
	idesc->id_filesize -= dp->d_reclen;
	if ((idesc->id_loc % DIRBLKSIZ) == 0)
		return (dp);
	ndp = (DIRECT *)(dirblk.b_buf + idesc->id_loc);
	if (idesc->id_loc < blksiz && idesc->id_filesize > 0 &&
	    dircheck(idesc, ndp) == 0) {
		size = DIRBLKSIZ - (idesc->id_loc % DIRBLKSIZ);
		dp->d_reclen += size;
		idesc->id_loc += size;
		idesc->id_filesize -= size;
		if (dofix(idesc, "DIRECTORY CORRUPTED"))
			dirty(&fileblk);
	}
	return (dp);
}

/*
 * Verify that a directory entry is valid.
 * This is a superset of the checks made in the kernel.
 */
dircheck(idesc, dp)
	struct inodesc *idesc;
	register DIRECT *dp;
{
	register int size;
	register char *cp;
	int spaceleft;

	size = DIRSIZ(dp);
	spaceleft = DIRBLKSIZ - (idesc->id_loc % DIRBLKSIZ);
	if (dp->d_ino < imax &&
	    dp->d_reclen != 0 &&
	    dp->d_reclen <= spaceleft &&
	    (dp->d_reclen & 0x3) == 0 &&
	    dp->d_reclen >= size &&
	    idesc->id_filesize >= size &&
	    dp->d_namlen <= MAXNAMLEN) {
		if (dp->d_ino == 0)
			return (1);
		for (cp = dp->d_name, size = 0; size < dp->d_namlen; size++)
			if (*cp == 0 || (*cp++ & 0200))
				return (0);
		if (*cp == 0)
			return (1);
	}
	return (0);
}

direrr(ino, s)
	ino_t ino;
	char *s;
{
	register DINODE *dp;

	pwarn("%s ", s);
	pinode(ino);
	printf("\n");
	if (ino < ROOTINO || ino > imax) {
		pfatal("NAME=%s\n", pathname);
		return;
	}
	dp = ginode(ino);
	if (ftypeok(dp))
		pfatal("%s=%s\n", DIRCT(dp) ? "DIR" : "FILE", pathname);
	else
		pfatal("NAME=%s\n", pathname);
}

adjust(idesc, lcnt)
	register struct inodesc *idesc;
	short lcnt;
{
	register DINODE *dp;

	dp = ginode(idesc->id_number);
	if (dp->di_nlink == lcnt) {
		if (linkup(idesc->id_number, (ino_t)0) == 0)
			clri(idesc, "UNREF", 0);
	} else {
		pwarn("LINK COUNT %s", (lfdir == idesc->id_number) ? lfname :
			(DIRCT(dp) ? "DIR" : "FILE"));
		pinode(idesc->id_number);
		printf(" COUNT %d SHOULD BE %d",
			dp->di_nlink, dp->di_nlink-lcnt);
		if (preen) {
			if (lcnt < 0) {
				printf("\n");
				pfatal("LINK COUNT INCREASING");
			}
			printf(" (ADJUSTED)\n");
		}
		if (preen || reply("ADJUST") == 1) {
			dp->di_nlink -= lcnt;
			inodirty();
		}
	}
}

mkentry(idesc)
	struct inodesc *idesc;
{
	register DIRECT *dirp = idesc->id_dirp;
	DIRECT newent;
	int newlen, oldlen;

	newent.d_namlen = 11;
	newlen = DIRSIZ(&newent);
	if (dirp->d_ino != 0)
		oldlen = DIRSIZ(dirp);
	else
		oldlen = 0;
	if (dirp->d_reclen - oldlen < newlen)
		return (KEEPON);
	newent.d_reclen = dirp->d_reclen - oldlen;
	dirp->d_reclen = oldlen;
	dirp = (struct direct *)(((char *)dirp) + oldlen);
	dirp->d_ino = idesc->id_parent;	/* ino to be entered is in id_parent */
	dirp->d_reclen = newent.d_reclen;
	dirp->d_namlen = lftempname(dirp->d_name, idesc->id_parent);
	return (ALTERED|STOP);
}

chgdd(idesc)
	struct inodesc *idesc;
{
	register DIRECT *dirp = idesc->id_dirp;

	if (dirp->d_name[0] == '.' && dirp->d_name[1] == '.' &&
	dirp->d_name[2] == 0) {
		dirp->d_ino = lfdir;
		return (ALTERED|STOP);
	}
	return (KEEPON);
}

linkup(orphan, pdir)
	ino_t orphan;
	ino_t pdir;
{
	register DINODE *dp;
	int lostdir, len;
	struct inodesc idesc;

	bzero((char *)&idesc, sizeof(struct inodesc));
	dp = ginode(orphan);
	lostdir = DIRCT(dp);
	pwarn("UNREF %s ", lostdir ? "DIR" : "FILE");
	pinode(orphan);
	if (preen && dp->di_size == 0)
		return (0);
	if (preen)
		printf(" (RECONNECTED)\n");
	else
		if (reply("RECONNECT") == 0)
			return (0);
	pathp = pathname;
	*pathp++ = '/';
	*pathp = '\0';
	if (lfdir == 0) {
		dp = ginode(ROOTINO);
		idesc.id_name = lfname;
		idesc.id_type = DATA;
		idesc.id_func = findino;
		idesc.id_number = ROOTINO;
		idesc.id_filesize = dp->di_size;
		(void)ckinode(dp, &idesc);
		lfdir = idesc.id_parent;
		if (lfdir < ROOTINO || lfdir > imax)
			lfdir = 0;
		if (lfdir == 0) {
			pfatal("SORRY. NO lost+found DIRECTORY");
			printf("\n\n");
			return (0);
		}
	}
	dp = ginode(lfdir);
	if (!DIRCT(dp) || statemap[lfdir] != DFOUND) {
		pfatal("SORRY. NO lost+found DIRECTORY");
		printf("\n\n");
		return (0);
	}
	if (fragoff(&sblock, dp->di_size)) {
		dp->di_size = fragroundup(&sblock, dp->di_size);
		inodirty();
	}
	len = strlen(lfname);
	bcopy(lfname, pathp, len + 1);
	pathp += len;
	idesc.id_type = DATA;
	idesc.id_func = mkentry;
	idesc.id_number = lfdir;
	idesc.id_filesize = dp->di_size;
	idesc.id_parent = orphan;	/* this is the inode to enter */
	idesc.id_fix = DONTKNOW;
	if ((ckinode(dp, &idesc) & ALTERED) == 0) {
		pfatal("SORRY. NO SPACE IN lost+found DIRECTORY");
		printf("\n\n");
		return (0);
	}
	lncntp[orphan]--;
	*pathp++ = '/';
	pathp += lftempname(pathp, orphan);
	if (lostdir) {
		dp = ginode(orphan);
		idesc.id_type = DATA;
		idesc.id_func = chgdd;
		idesc.id_number = orphan;
		idesc.id_filesize = dp->di_size;
		idesc.id_fix = DONTKNOW;
		(void)ckinode(dp, &idesc);
		dp = ginode(lfdir);
		dp->di_nlink++;
		inodirty();
		lncntp[lfdir]++;
		pwarn("DIR I=%u CONNECTED. ", orphan);
		printf("PARENT WAS I=%u\n", pdir);
		if (preen == 0)
			printf("\n");
	}
	return (1);
}

/*
 * generate a temporary name for the lost+found directory.
 */
lftempname(bufp, ino)
	char *bufp;
	ino_t ino;
{
	register ino_t in;
	register char *cp;
	int namlen;

	cp = bufp + 2;
	for (in = imax; in > 0; in /= 10)
		cp++;
	*--cp = 0;
	namlen = cp - bufp;
	in = ino;
	while (cp > bufp) {
		*--cp = (in % 10) + '0';
		in /= 10;
	}
	*cp = '#';
	return (namlen);
}
