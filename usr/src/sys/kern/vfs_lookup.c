/*	vfs_lookup.c	4.16	82/06/07	*/

/* merged into kernel:	@(#)nami.c 2.3 4/8/82 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/mount.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/conf.h"

/*
 * Convert a pathname into a pointer to
 * a locked inode.
 *
 * func = function called to get next char of name
 *	&uchar if name is in user space
 *	&schar if name is in system space
 * flag = 0 if name is sought
 *	1 if name is to be created
 *	2 if name is to be deleted
 * follow = 1 if links are to be followed at the end of the name
 */
struct inode *
namei(func, flag, follow)
	int (*func)(), flag, follow;
{
	register struct inode *dp;
	register char *cp;
	register struct buf *bp, *nbp;
	register struct direct *ep;
	register struct fs *fs;
	struct inode *pdp;
	enum {NONE, COMPACT, FOUND} slot;
	int entryfree, entrysize;
	int spccnt, size, newsize;
	int loc, prevoff, curoff;
	int i, nlink, bsize;
	unsigned pathlen;
	daddr_t lbn, bn;
	dev_t d;

	/*
	 * allocate name buffer; copy name
	 */
	nbp = geteblk(MAXPATHLEN);
	nlink = 0;
	for (i = 0, cp = nbp->b_un.b_addr; *cp = (*func)(); i++) {
		if ((*cp & 0377) == ('/'|0200)) {
			u.u_error = EPERM;
			break;
		}
#ifdef notdef
		if (*cp++ & 0200 && flag == 1 ||
		    cp >= nbp->b_un.b_addr + MAXPATHLEN) {
#else
		cp++;
		if (cp >= nbp->b_un.b_addr + MAXPATHLEN) {
#endif
			u.u_error = ENOENT;
			break;
		}
	}
	if (u.u_error) {
		brelse(nbp);
		return (NULL);
	}
	cp = nbp->b_un.b_addr;
	/*
	 * If name starts with '/' start from
	 * root; otherwise start from current dir.
	 */
	dp = u.u_cdir;
	if (*cp == '/') {
		while (*cp == '/')
			cp++;
		if ((dp = u.u_rdir) == NULL)
			dp = rootdir;
	}
	ilock(dp);
	dp->i_count++;
	fs = dp->i_fs;
	newsize = 0;
dirloop:
	/*
	 * dp must be a directory and
	 * must have X permission.
	 * cp is a path name relative to that directory.
	 */
	if ((dp->i_mode&IFMT) != IFDIR)
		u.u_error = ENOTDIR;
	(void) access(dp, IEXEC);
dirloop2:
	for (i = 0; *cp != '\0' && *cp != '/'; cp++) {
#ifdef notdef
		if (i >= MAXNAMLEN) {
			u.u_error = ENOENT;
			break;
		}
		u.u_dent.d_name[i] = *cp;
#else
		if (i < MAXNAMLEN) {
			u.u_dent.d_name[i] = *cp;
			i++;
		}
#endif
	}
	if (u.u_error) {
		iput(dp);
		brelse(nbp);
		return (NULL);
	}
	u.u_dent.d_namlen = i;
	u.u_dent.d_name[i] = '\0';
	newsize = DIRSIZ(&u.u_dent);
	u.u_pdir = dp;
	if (u.u_dent.d_name[0] == '\0') {	/* null name, e.g. "/" or "" */
		if (flag != 0) {
			u.u_error = ENOENT;
			iput(dp);
			dp = NULL;
		}
		u.u_offset = 0;
		u.u_count = newsize;
		brelse(nbp);
		return (dp);
	}
	/*
	 * set up to search a directory
	 */
	if (flag == 1)
		slot = NONE;
	else
		slot = FOUND;
	u.u_offset = 0;
	u.u_segflg = 1;
	bp = NULL;
	spccnt = 0;
	loc = 0;
	while (u.u_offset < dp->i_size) {
		/*
		 * check to see if enough space has been accumulated to make
		 * an entry by compaction. Reset the free space counter each
		 * time a directory block is crossed.
		 */
		if (slot == NONE) {
			if (spccnt >= newsize) {
				slot = COMPACT;
				entrysize = u.u_offset - entryfree;
			} else if (loc % DIRBLKSIZ == 0) {
				entryfree = NULL;
				spccnt = 0;
			}
		}
		/*
		 * If offset is on a block boundary,
		 * read the next directory block.
		 * Release previous if it exists.
		 */
		if (blkoff(fs, u.u_offset) == 0) {
			if (bp != NULL)
				brelse(bp);
			lbn = (daddr_t)lblkno(fs, u.u_offset);
			bsize = blksize(fs, dp, lbn);
			if ((bn = bmap(dp, lbn, B_READ)) < 0) {
				printf("hole in dir: %s i = %d\n",
				    fs->fs_fsmnt, dp->i_number);
				if (fs->fs_ronly != 0 ||
				    (bn = bmap(dp, lbn, B_WRITE, bsize)) < 0) {
					u.u_offset += bsize;
					bp = NULL;
					continue;
				}
			}
			bp = bread(dp->i_dev, fsbtodb(fs, bn), bsize);
			if (bp->b_flags & B_ERROR) {
				brelse(bp);
				iput(dp);
				brelse(nbp);
				return (NULL);
			}
			loc = 0;
		} else {
			loc += ep->d_reclen;
		}
		/*
		 * calculate the next directory entry and run
		 * some rudimentary bounds checks to make sure
		 * that it is reasonable. If the check fails
		 * resync at the beginning of the next directory
		 * block.
		 */
		ep = (struct direct *)(bp->b_un.b_addr + loc);
		i = DIRBLKSIZ - (loc & (DIRBLKSIZ - 1));
		if (ep->d_reclen <= 0 || ep->d_reclen > i) {
			loc += i;
			u.u_offset += i;
			continue;
		}
		/*
		 * If an appropriate sized hole has not yet been found,
		 * check to see if one is available. Also accumulate space
		 * in the current block so that we can determine if
		 * compaction is viable.
		 */
		if (slot != FOUND) {
			size = ep->d_reclen;
			if (ep->d_ino != 0)
				size -= DIRSIZ(ep);
			if (size > 0) {
				if (size >= newsize) {
					slot = FOUND;
					entryfree = u.u_offset;
					entrysize = DIRSIZ(ep) + newsize;
				}
				if (entryfree == NULL)
					entryfree = u.u_offset;
				spccnt += size;
			}
		}
		/*
		 * String compare the directory entry
		 * and the current component.
		 * If they do not match, continue to the next entry.
		 */
		prevoff = curoff;
		curoff = u.u_offset;
		u.u_offset += ep->d_reclen;
		if (ep->d_ino == 0)
			continue;
		if (ep->d_namlen != u.u_dent.d_namlen)
			continue;
		if (bcmp(u.u_dent.d_name, ep->d_name, ep->d_namlen))
			continue;
		/*
		 * Here a component matched in a directory.
		 * If there is more pathname, go back to
		 * dirloop, otherwise return.
		 */
		bcopy((caddr_t)ep, (caddr_t)&u.u_dent, DIRSIZ(ep));
		brelse(bp);
		if (flag == 2 && *cp == '\0') {
			brelse(nbp);
			if (access(dp, IWRITE)) {
				iput(dp);
				return (NULL);
			}
			if (curoff % DIRBLKSIZ == 0) {
				u.u_offset = curoff;
				u.u_count = 0;
				return (dp);
			}
			u.u_offset = prevoff;
			u.u_count = DIRSIZ((struct direct *)
			    (bp->b_un.b_addr + blkoff(fs, prevoff)));
			return (dp);
		}
		/*
		 * Special handling for ".."
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
		d = dp->i_dev;
		pdp = dp;
		iunlock(pdp);
		dp = iget(d, fs, u.u_dent.d_ino);
		if (dp == NULL)  {
			irele(pdp);
			brelse(nbp);
			return (NULL);
		}
		fs = dp->i_fs;
		/*
		 * Check for symbolic link
		 */
		if ((dp->i_mode & IFMT) == IFLNK && (follow || *cp == '/')) {
			pathlen = strlen(cp) + 1;
			if (dp->i_size + pathlen >= MAXPATHLEN - 1 ||
			    ++nlink > MAXSYMLINKS) {
				u.u_error = ELOOP;
				irele(pdp);
				iput(dp);
				brelse(nbp);
				return (NULL);
			}
			bcopy(cp, nbp->b_un.b_addr + dp->i_size, pathlen);
			bn =  bmap(dp, (daddr_t)0, B_READ);
			if (bn < 0) {
				printf("hole in symlink: %s i = %d\n",
				    fs->fs_fsmnt, dp->i_number);
				irele(pdp);
				iput(dp);
				brelse(nbp);
				return (NULL);
			}
			bp = bread(dp->i_dev, fsbtodb(fs, bn),
				   (int)blksize(fs, dp, (daddr_t)0));
			if (bp->b_flags & B_ERROR) {
				brelse(bp);
				irele(pdp);
				iput(dp);
				brelse(nbp);
				return (NULL);
			}
			bcopy(bp->b_un.b_addr, nbp->b_un.b_addr,
			  (unsigned)dp->i_size);
			brelse(bp);
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
		irele(pdp);
		if (*cp == '/') {
			while (*cp == '/')
				cp++;
			goto dirloop;
		}
		/*
		 * End of path, so return name matched.
		 */
		u.u_offset -= ep->d_reclen;
		u.u_count = newsize;
		brelse(nbp);
		return (dp);
	}
	/*
	 * Search failed.
	 * Report what is appropriate as per flag.
	 */
	if (bp != NULL)
		brelse(bp);
	if (flag == 1 && *cp == '\0' && dp->i_nlink != 0) {
		brelse(nbp);
		if (access(dp, IWRITE)) {
			iput(dp);
			return (NULL);
		}
		if (slot == NONE) {
			u.u_count = 0;
		} else {
			u.u_offset = entryfree;
			u.u_count = entrysize;
		}
		dp->i_flag |= IUPD|ICHG;
		return (NULL);
	}
	u.u_error = ENOENT;
	iput(dp);
	brelse(nbp);
	return (NULL);
}

/*
 * Return the next character from the
 * kernel string pointed at by dirp.
 */
schar()
{

	return (*u.u_dirp++ & 0377);
}

/*
 * Return the next character from the
 * user string pointed at by dirp.
 */
uchar()
{
	register c;

	c = fubyte(u.u_dirp++);
	if (c == -1) {
		u.u_error = EFAULT;
		c = 0;
	}
	return (c);
}

#ifndef vax
bcmp(s1, s2, len)
	register char *s1, *s2;
	register int len;
{

	while (--len)
		if (*s1++ != *s2++)
			return (1);
	return (0);
}

strlen(s1)
	register char *s1;
{
	register int len;

	for (len = 0; *s1++ != '\0'; len++)
		/* void */;
	return (len);
}
#endif
