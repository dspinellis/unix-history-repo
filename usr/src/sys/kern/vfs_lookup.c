/*	vfs_lookup.c	4.33	82/12/21	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/mount.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/uio.h"
#include "../h/nami.h"

struct	buf *blkatoff();
int	dirchk = 0;
/*
 * Convert a pathname into a pointer to a locked inode,
 * with side effects usable in creating and removing files.
 * This is a very central and rather complicated routine.
 *
 * The func argument gives the routine which returns successive
 * characters of the name to be translated. 
 *
 * The flag argument is (LOOKUP, CREATE, DELETE) depending on whether
 * the name is to be (looked up, created, deleted).  If flag has
 * LOCKPARENT or'ed into it and the target of the pathname exists,
 * namei returns both the target and its parent directory locked. 
 * If the file system is not maintained in a strict tree hierarchy,
 * this can result in a deadlock situation.  When creating and
 * LOCKPARENT is specified, the target may not be ".".  When deleting
 * and LOCKPARENT is specified, the target may be ".", but the caller
 * must check to insure it does an irele and iput instead of two iputs.
 *
 * The follow argument is 1 when symbolic links are to be followed
 * when they occur at the end of the name translation process.
 *
 * Overall outline:
 *
 *	copy in name
 *	get starting directory
 * dirloop:
 *	check accessibility of directory
 * dirloop2:
 *	copy next component of name to u.u_dent
 *	handle degenerate case where name is null string
 *	search for name in directory, to found or notfound
 * notfound:
 *	if creating, return locked directory, leaving info on avail. slots
 *	else return error
 * found:
 *	if at end of path and deleting, return information to allow delete
 *	if at end of path and rewriting (create and LOCKPARENT), lock targe
 *	  inode and return info to allow rewrite
 *	if .. and on mounted filesys, look in mount table for parent
 *	if symbolic link, massage name in buffer and continue at dirloop
 *	if more components of name, do next level at dirloop
 *	return the answer as locked inode
 *
 * NOTE: (LOOKUP | LOCKPARENT) currently returns the parent inode,
 *	 but unlocked.
 */
struct inode *
namei(func, flag, follow)
	int (*func)(), flag, follow;
{
	register char *cp;		/* pointer into pathname argument */
/* these variables refer to things which must be freed or unlocked */
	register struct inode *dp = 0;	/* the directory we are searching */
	register struct fs *fs;		/* file system that directory is in */
	register struct buf *bp = 0;	/* a buffer of directory entries */
	register struct direct *ep;	/* the current directory entry */
	int entryoffsetinblock;		/* offset of ep in bp's buffer */
	register struct buf *nbp;	/* buffer storing path name argument */
/* these variables hold information about the search for a slot */
	enum {NONE, COMPACT, FOUND} slotstatus;
	int slotoffset = -1;		/* offset of area with free space */
	int slotsize;			/* size of area at slotoffset */
	int slotfreespace;		/* amount of space free in slot */
	int slotneeded;			/* size of the entry we're seeking */
/* */
	int dirsize;
	int prevoff;			/* u.u_offset of previous entry */
	int nlink = 0;			/* number of symbolic links taken */
	struct inode *pdp;		/* saved dp during symlink work */
	int i;
	int lockparent;

	lockparent = flag & LOCKPARENT;
	flag &= ~LOCKPARENT;
	/*
	 * Get a buffer for the name to be translated, and copy the
	 * name into the buffer.
	 */
	nbp = geteblk(MAXPATHLEN);
	for (cp = nbp->b_un.b_addr; *cp = (*func)(); ) {
		if ((*cp&0377) == ('/'|0200) || (*cp&0200) && flag != 2) {
			u.u_error = EPERM;
			goto bad;
		}
		cp++;
		if (cp >= nbp->b_un.b_addr + MAXPATHLEN) {
			u.u_error = ENOENT;
			goto bad;
		}
	}
	if (u.u_error)
		goto bad;

	/*
	 * Get starting directory.
	 */
	cp = nbp->b_un.b_addr;
	if (*cp == '/') {
		while (*cp == '/')
			cp++;
		if ((dp = u.u_rdir) == NULL)
			dp = rootdir;
	} else
		dp = u.u_cdir;
	fs = dp->i_fs;
	ilock(dp);
	dp->i_count++;
	u.u_pdir = (struct inode *)0xc0000000;		/* illegal */

	/*
	 * We come to dirloop to search a new directory.
	 * The directory must be locked so that it can be
	 * iput, and fs must be already set to dp->i_fs.
	 */
dirloop:
	/*
	 * Check accessiblity of directory.
	 */
	if ((dp->i_mode&IFMT) != IFDIR) {
		u.u_error = ENOTDIR;
		goto bad;
	}
	if (access(dp, IEXEC))
		goto bad;

dirloop2:
	/*
	 * Copy next component of name to u.u_dent.
	 */
	for (i = 0; *cp != 0 && *cp != '/'; cp++) {
		if (i >= MAXNAMLEN) {
			u.u_error = ENOENT;
			goto bad;
		}
		u.u_dent.d_name[i++] = *cp;
	}
	u.u_dent.d_namlen = i;
	u.u_dent.d_name[i] = 0;

	/*
	 * Check for degenerate name (e.g. / or "")
	 * which is a way of talking about a directory,
	 * e.g. like "/." or ".".
	 */
	if (u.u_dent.d_name[0] == 0) {
		if (flag) {
			u.u_error = ENOENT;
			goto bad;
		}
		brelse(nbp);
		return (dp);
	}

	/*
	 * Suppress search for slots unless creating
	 * file and at end of pathname, in which case
	 * we watch for a place to put the new file in
	 * case it doesn't already exist.
	 */
	slotstatus = FOUND;
	if (flag == CREATE && *cp == 0) {
		slotstatus = NONE;
		slotfreespace = 0;
		slotneeded = DIRSIZ(&u.u_dent);
	}

	dirsize = roundup(dp->i_size, DIRBLKSIZ);
	u.u_offset = 0;
	while (u.u_offset < dirsize) {
		/*
		 * If offset is on a block boundary,
		 * read the next directory block.
		 * Release previous if it exists.
		 */
		if (blkoff(fs, u.u_offset) == 0) {
			if (bp != NULL)
				brelse(bp);
			bp = blkatoff(dp, u.u_offset, (char **)0);
			if (bp == 0)
				goto bad;
			entryoffsetinblock = 0;
		}

		/*
		 * If still looking for a slot, and at a DIRBLKSIZE
		 * boundary, have to start looking for free space
		 * again.
		 */
		if (slotstatus == NONE &&
		    (entryoffsetinblock&(DIRBLKSIZ-1)) == 0) {
			slotoffset = -1;
			slotfreespace = 0;
		}

		/*
		 * Get pointer to next entry, and do consistency checking:
		 *	record length must be multiple of 4
		 *	record length must not be zero
		 *	entry must fit in rest of this DIRBLKSIZ block
		 *	record must be large enough to contain name
		 * When dirchk is set we also check:
		 *	name is not longer than MAXNAMLEN
		 *	name must be as long as advertised, and null terminated
		 * Checking last two conditions is done only when dirchk is
		 * set, to save time.
		 */
		ep = (struct direct *)(bp->b_un.b_addr + entryoffsetinblock);
		i = DIRBLKSIZ - (entryoffsetinblock & (DIRBLKSIZ - 1));
		if ((ep->d_reclen & 0x3) || ep->d_reclen == 0 ||
		    ep->d_reclen > i || DIRSIZ(ep) > ep->d_reclen ||
		    dirchk && (ep->d_namlen > MAXNAMLEN || dirbadname(ep))) {
			dirbad(dp, "mangled entry");
			u.u_offset += i;
			entryoffsetinblock += i;
			continue;
		}

		/*
		 * If an appropriate sized slot has not yet been found,
		 * check to see if one is available. Also accumulate space
		 * in the current block so that we can determine if
		 * compaction is viable.
		 */
		if (slotstatus != FOUND) {
			int size = ep->d_reclen;

			if (ep->d_ino != 0)
				size -= DIRSIZ(ep);
			if (size > 0) {
				if (size >= slotneeded) {
					slotstatus = FOUND;
					slotoffset = u.u_offset;
					slotsize = ep->d_reclen;
				} else if (slotstatus == NONE) {
					slotfreespace += size;
					if (slotoffset == -1)
						slotoffset = u.u_offset;
					if (slotfreespace >= slotneeded) {
						slotstatus = COMPACT;
						slotsize =
						    u.u_offset+ep->d_reclen -
						      slotoffset;
					}
				}
			}
		}

		/*
		 * Check for a name match.
		 */
		if (ep->d_ino) {
			if (ep->d_namlen == u.u_dent.d_namlen &&
			    !bcmp(u.u_dent.d_name, ep->d_name, ep->d_namlen))
				goto found;
		}
		prevoff = u.u_offset;
		u.u_offset += ep->d_reclen;
		entryoffsetinblock += ep->d_reclen;
	}
/* notfound: */
	/*
	 * If creating, and at end of pathname and current
	 * directory has not been removed, then can consider
	 * allowing file to be created.
	 */
	if (flag == CREATE && *cp == 0 && dp->i_nlink != 0) {
		/*
		 * Access for write is interpreted as allowing
		 * creation of files in the directory.
		 */
		if (access(dp, IWRITE))
			goto bad;
		/*
		 * Return an indication of where the new directory
		 * entry should be put.  If we didn't find a slot,
		 * then set u.u_count to 0 indicating that the
		 * new slot belongs at the end of the directory.
		 * If we found a slot, then the new entry can be
		 * put in the range [u.u_offset..u.u_offset+u.u_count)
		 */
		if (slotstatus == NONE)
			u.u_count = 0;
		else {
			u.u_offset = slotoffset;
			u.u_count = slotsize;
		}
		dp->i_flag |= IUPD|ICHG;
		if (bp)
			brelse(bp);
		brelse(nbp);
		/*
		 * We return with the directory locked, so that
		 * the parameters we set up above will still be
		 * valid if we actually decide to do a direnter().
		 * We return NULL to indicate that the entry doesn't
		 * currently exist, leaving a pointer to the (locked)
		 * directory inode in u.u_pdir.
		 */
		u.u_pdir = dp;
		return (NULL);
	}
	u.u_error = ENOENT;
	goto bad;
found:
	/*
	 * Check that directory length properly reflects presence
	 * of this entry.
	 */
	if (entryoffsetinblock + DIRSIZ(ep) > dp->i_size) {
		dirbad(dp, "i_size too small");
		dp->i_size = entryoffsetinblock + DIRSIZ(ep);
		dp->i_flag |= IUPD|ICHG;
	}

	/*
	 * Found component in pathname; save directory
	 * entry in u.u_dent, and release directory buffer.
	 */
	bcopy((caddr_t)ep, (caddr_t)&u.u_dent, (u_int)DIRSIZ(ep));
	brelse(bp);
	bp = NULL;

	/*
	 * If deleting, and at end of pathname, return
	 * parameters which can be used to remove file.
	 * If the lockparent flag isn't set, we return only
	 * the directory (in u.u_pdir), otherwise we go
	 * on and lock the inode, being careful with ".".
	 */
	if (flag == DELETE && *cp == 0) {
		/*
		 * Write access to directory required to delete files.
		 */
		if (access(dp, IWRITE))
			goto bad;
		u.u_pdir = dp;		/* for dirremove() */
		/*
		 * Return pointer to current entry in u.u_offset,
		 * and distance past previous entry (if there
		 * is a previous entry in this block) in u.u_count.
		 * Save directory inode pointer in u.u_pdir for dirremove().
		 */
		if ((u.u_offset&(DIRBLKSIZ-1)) == 0)
			u.u_count = 0;
		else
			u.u_count = u.u_offset - prevoff;
		if (lockparent) {
			if (dp->i_number == u.u_dent.d_ino)
				dp->i_count++;
			else {
				dp = iget(dp->i_dev, fs, u.u_dent.d_ino);
				if (dp == NULL) {
					iput(u.u_pdir);
					goto bad;
				}
			}
		}
		brelse(nbp);
		return (dp);
	}

	/*
	 * Special handling for ".." allowing chdir out of mounted
	 * file system: indirect .. in root inode to reevaluate
	 * in directory file system was mounted on.
	 */
	if (u.u_dent.d_name[0] == '.' && u.u_dent.d_name[1] == '.' &&
	    u.u_dent.d_name[2] == '\0') {
		if (dp == u.u_rdir)
			u.u_dent.d_ino = dp->i_number;
		else if (u.u_dent.d_ino == ROOTINO &&
		   dp->i_number == ROOTINO) {
			for (i = 1; i < NMOUNT; i++)
			if (mount[i].m_bufp != NULL &&
			   mount[i].m_dev == dp->i_dev) {
				iput(dp);
				dp = mount[i].m_inodp;
				ilock(dp);
				dp->i_count++;
				fs = dp->i_fs;
				cp -= 2;     /* back over .. */
				goto dirloop2;
			}
		}
	}

	/*
	 * If rewriting (rename), return the inode and the
	 * information required to rewrite the present directory
	 * Must get inode of directory entry to verify it's a
	 * regular file, or empty directory.  
	 */
	if ((flag == CREATE && lockparent) && *cp == 0) {
		if (access(dp, IWRITE))
			goto bad;
		u.u_pdir = dp;		/* for dirrewrite() */
		/*
		 * Careful about locking second inode. 
		 * This can only occur if the target is ".". 
		 */
		if (dp->i_number == u.u_dent.d_ino) {
			u.u_error = EISDIR;		/* XXX */
			goto bad;
		}
		dp = iget(dp->i_dev, fs, u.u_dent.d_ino);
		if (dp == NULL) {
			iput(u.u_pdir);
			goto bad;
		}
		brelse(nbp);
		return (dp);
	}

	/*
	 * Check for symbolic link, which may require us
	 * to massage the name before we continue translation.
	 * To avoid deadlock have to unlock the current directory,
	 * but don't iput it because we may need it again (if
	 * the symbolic link is relative to .).  Instead save
	 * it (unlocked) as pdp.
	 */
	pdp = dp;
	iunlock(pdp);
	dp = iget(dp->i_dev, fs, u.u_dent.d_ino);
	if (dp == NULL)
		goto bad2;
	fs = dp->i_fs;

	/*
	 * Check for symbolic link
	 */
	if ((dp->i_mode & IFMT) == IFLNK && (follow || *cp == '/')) {
		u_int pathlen = strlen(cp) + 1;

		if (dp->i_size + pathlen >= MAXPATHLEN - 1 ||
		    ++nlink > MAXSYMLINKS) {
			u.u_error = ELOOP;
			goto bad2;
		}
		ovbcopy(cp, nbp->b_un.b_addr + dp->i_size, pathlen);
		u.u_error =
		    rdwri(UIO_READ, dp, nbp->b_un.b_addr, (int)dp->i_size,
			0, 1, (int *)0);
		if (u.u_error)
			goto bad2;
		cp = nbp->b_un.b_addr;
		iput(dp);
		if (*cp == '/') {
			irele(pdp);
			while (*cp == '/')
				cp++;
			if ((dp = u.u_rdir) == NULL)
				dp = rootdir;
			ilock(dp);
			dp->i_count++;
		} else {
			dp = pdp;
			ilock(dp);
		}
		fs = dp->i_fs;
		goto dirloop;
	}

	/*
	 * Not a symbolic link.  If more pathname,
	 * continue at next component, else return.
	 */
	if (*cp == '/') {
		while (*cp == '/')
			cp++;
		irele(pdp);
		goto dirloop;
	}
	brelse(nbp);
	if (lockparent)
		u.u_pdir = pdp;
	else
		irele(pdp);
	return (dp);
bad2:
	irele(pdp);
bad:
	if (bp)
		brelse(bp);
	if (dp)
		iput(dp);
	brelse(nbp);
	return (NULL);
}

dirbad(ip, how)
	struct inode *ip;
	char *how;
{

	printf("%s: bad dir ino %d at offset %d: %s\n",
	    ip->i_fs->fs_fsmnt, ip->i_number, u.u_offset, how);
}

dirbadname(ep)
	register struct direct *ep;
{
	register int i;

	for (i = 0; i < ep->d_namlen; i++)
		if (ep->d_name[i] == 0)
			return (1);
	return (ep->d_name[i]);
}

/*
 * Write a directory entry after a call to namei, using the parameters
 * which it left in the u. area.  The argument ip is the inode which
 * the new directory entry will refer to.  The u. area field u.u_pdir is
 * a pointer to the directory to be written, which was left locked by
 * namei.  Remaining parameters (u.u_offset, u.u_count) indicate
 * how the space for the new entry is to be gotten.
 */
direnter(ip)
	struct inode *ip;
{
	register struct direct *ep, *nep;
	struct buf *bp;
	int loc, freespace;
	u_int dsize;
	int newentrysize;
	char *dirbuf;

	u.u_dent.d_ino = ip->i_number;
	u.u_segflg = 1;
	newentrysize = DIRSIZ(&u.u_dent);
	if (u.u_count == 0) {
		/*
		 * If u.u_count is 0, then namei could find no space in the
		 * directory.  In this case u.u_offset will be on a directory
		 * block boundary and we will write the new entry into a fresh
		 * block.
		 */
		if (u.u_offset&(DIRBLKSIZ-1))
			panic("wdir: newblk");
		u.u_dent.d_reclen = DIRBLKSIZ;
		(void) rdwri(UIO_WRITE, u.u_pdir, (caddr_t)&u.u_dent,
		    newentrysize, u.u_offset, 1, (int *)0);
		iput(u.u_pdir);
		return;
	}

	/*
	 * If u.u_count is non-zero, then namei found space for the
	 * new entry in the range u.u_offset to u.u_offset+u.u_count.
	 * in the directory.  To use this space, we may have to compact
	 * the entries located there, by copying them together towards
	 * the beginning of the block, leaving the free space in
	 * one usable chunk at the end.
	 */

	/*
	 * Increase size of directory if entry eats into new space.
	 * This should never push the size past a new multiple of
	 * DIRBLKSIZE.
	 */
	if (u.u_offset + u.u_count > u.u_pdir->i_size)
		u.u_pdir->i_size = u.u_offset + u.u_count;

	/*
	 * Get the block containing the space for the new directory
	 * entry.
	 */
	bp = blkatoff(u.u_pdir, u.u_offset, (char **)&dirbuf);
	if (bp == 0) {
		iput(u.u_pdir);
		return;
	}

	/*
	 * Find space for the new entry.  In the simple case, the
	 * entry at offset base will have the space.  If it does
	 * not, then namei arranged that compacting the region
	 * u.u_offset to u.u_offset+u.u_count would yield the space.
	 */
	ep = (struct direct *)dirbuf;
	dsize = DIRSIZ(ep);
	freespace = ep->d_reclen - dsize;
	for (loc = ep->d_reclen; loc < u.u_count; ) {
		nep = (struct direct *)(dirbuf + loc);
		if (ep->d_ino) {
			/* trim the existing slot */
			ep->d_reclen = dsize;
			ep = (struct direct *)((char *)ep + dsize);
		} else {
			/* overwrite; nothing there; header is ours */
			freespace += dsize;	
		}
		dsize = DIRSIZ(nep);
		freespace += nep->d_reclen - dsize;
		loc += nep->d_reclen;
		bcopy((caddr_t)nep, (caddr_t)ep, dsize);
	}
	/*
	 * Update the pointer fields in the previous entry (if any),
	 * copy in the new entry, and write out the block.
	 */
	if (ep->d_ino == 0) {
		if (freespace + dsize < newentrysize)
			panic("wdir: compact1");
		u.u_dent.d_reclen = freespace + dsize;
	} else {
		if (freespace < newentrysize)
			panic("wdir: compact2");
		u.u_dent.d_reclen = freespace;
		ep->d_reclen = dsize;
		ep = (struct direct *)((char *)ep + dsize);
	}
	bcopy((caddr_t)&u.u_dent, (caddr_t)ep, (u_int)newentrysize);
	bwrite(bp);
	u.u_pdir->i_flag |= IUPD|ICHG;
	iput(u.u_pdir);
}

/*
 * Remove a directory entry after a call to namei, using the
 * parameters which it left in the u. area.  The u. entry
 * u_offset contains the offset into the directory of the
 * entry to be eliminated.  The u_count field contains the
 * size of the previous record in the directory.  If this
 * is 0, the first entry is being deleted, so we need only
 * zero the inode number to mark the entry as free.  If the
 * entry isn't the first in the directory, we must reclaim
 * the space of the now empty record by adding the record size
 * to the size of the previous entry.
 */
dirremove()
{
	register struct inode *dp = u.u_pdir;
	register struct buf *bp;
	struct direct *ep;

	if (u.u_count == 0) {
		/*
		 * First entry in block: set d_ino to zero.
		 */
		u.u_dent.d_ino = 0;
		(void) rdwri(UIO_WRITE, dp, (caddr_t)&u.u_dent,
		    (int)DIRSIZ(&u.u_dent), u.u_offset, 1, (int *)0);
	} else {
		/*
		 * Collapse new free space into previous entry.
		 */
		bp = blkatoff(dp, (int)(u.u_offset - u.u_count), (char **)&ep);
		if (bp == 0)
			return (0);
		ep->d_reclen += u.u_dent.d_reclen;
		bwrite(bp);
		dp->i_flag |= IUPD|ICHG;
	}
	return (1);
}

/*
 * Rewrite an existing directory entry to point at the inode
 * supplied.  The parameters describing the directory entry are
 * set up by a call to namei.
 */
dirrewrite(dp, ip)
	struct inode *dp, *ip;
{

	u.u_dent.d_ino = ip->i_number;
	u.u_error = rdwri(UIO_WRITE, dp, (caddr_t)&u.u_dent,
		(int)DIRSIZ(&u.u_dent), u.u_offset, 1, (int *)0);
	iput(dp);
}

/*
 * Return buffer with contents of block "offset"
 * from the beginning of directory "ip".  If "res"
 * is non-zero, fill it in with a pointer to the
 * remaining space in the directory.
 */
struct buf *
blkatoff(ip, offset, res)
	struct inode *ip;
	off_t offset;
	char **res;
{
	register struct fs *fs = ip->i_fs;
	daddr_t lbn = lblkno(fs, offset);
	int base = blkoff(fs, offset);
	int bsize = blksize(fs, ip, lbn);
	daddr_t bn = fsbtodb(fs, bmap(ip, lbn, B_WRITE, base, bsize));
	register struct buf *bp;

	if (u.u_error)
		return (0);
	bp = bread(ip->i_dev, bn, bsize);
	if (bp->b_flags & B_ERROR) {
		brelse(bp);
		return (0);
	}
	if (res)
		*res = bp->b_un.b_addr + base;
	return (bp);
}

/*
 * Check if a directory is empty or not.
 * Inode supplied must be locked.
 */
dirempty(ip)
	register struct inode *ip;
{
	register off_t off;
	struct direct dbuf;
	register struct direct *dp = &dbuf;
	int error, count;

	for (off = 0; off < ip->i_size; off += dp->d_reclen) {
		error = rdwri(UIO_READ, ip, (caddr_t)dp,
			sizeof (struct direct), off, 1, &count);
		count = sizeof (struct direct) - count;
#define	MINDIRSIZ (sizeof (struct direct) - (MAXNAMLEN + 1))
		if (error || count < MINDIRSIZ || count < DIRSIZ(dp))
			return (0);
		if (dp->d_ino == 0)
			continue;
		if (dp->d_name[0] != '.')
			return (0);
		if (dp->d_namlen == 1 ||
		    (dp->d_namlen == 2 && dp->d_name[1] == '.'))
			continue;
		return (0);
	}
	return (1);
}
