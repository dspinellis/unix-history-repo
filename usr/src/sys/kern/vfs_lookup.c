/*	vfs_lookup.c	4.9	82/02/26	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/inode.h"
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
	int i, nlink;
	dev_t d;
	ino_t ino;
	off_t eo;

	/*
	 * allocate name buffer; copy name
	 */
	nbp = geteblk();
	nlink = 0;
	for (i=0, cp = nbp->b_un.b_addr; *cp = (*func)(); i++) {
		if (*cp++&0200 && flag==1 || cp >= nbp->b_un.b_addr+BSIZE) {
			u.u_error = ENOENT;
			break;
		}
	}
	if (u.u_error) {
		dp = NULL;
		goto out1;
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

	/*
	 * dp must be a directory and
	 * must have X permission.
	 * cp is a path name relative to that directory.
	 */

dirloop:
	if ((dp->i_mode&IFMT) != IFDIR)
		u.u_error = ENOTDIR;
	(void) access(dp, IEXEC);
	for (i=0; *cp!='\0' && *cp!='/'; i++) {
		if (i >= DIRSIZ) {
			u.u_error = ENOENT;
			break;
		}
		u.u_dbuf[i] = *cp++;
	}
	if (u.u_error)
		goto out;
	while (i < DIRSIZ)
		u.u_dbuf[i++] = '\0';
	if (u.u_dbuf[0] == '\0') {		/* null name, e.g. "/" or "" */
		if (flag) {
			u.u_error = ENOENT;
			goto out;
		}
		goto out1;
	}
	u.u_segflg = 1;
	eo = -1;
	bp = NULL;

	for (u.u_offset=0; u.u_offset < dp->i_size;
	   u.u_offset += sizeof(struct direct), ep++) {
		/*
		 * If offset is on a block boundary,
		 * read the next directory block.
		 * Release previous if it exists.
		 */
		if ((u.u_offset&BMASK) == 0) {
			if (bp != NULL)
				brelse(bp);
			bp = bread(dp->i_dev,
				bmap(dp,(daddr_t)(u.u_offset>>BSHIFT), B_READ));
			if (bp->b_flags & B_ERROR) {
				brelse(bp);
				goto out;
			}
			ep = (struct direct *)bp->b_un.b_addr;
		}
		/*
		 * Note first empty directory slot
		 * in eo for possible creat.
		 * String compare the directory entry
		 * and the current component.
		 */
		if (ep->d_ino == 0) {
			if (eo < 0)
				eo = u.u_offset;
			continue;
		}
		if (strncmp(u.u_dbuf, ep->d_name, DIRSIZ) != 0)
			continue;
		/*
		 * Here a component matched in a directory.
		 * If there is more pathname, go back to
		 * dirloop, otherwise return.
		 */
		bcopy((caddr_t)ep, (caddr_t)&u.u_dent, sizeof(struct direct));
		brelse(bp);
		if (flag==2 && *cp=='\0') {
			if (access(dp, IWRITE))
				goto out;
			/* should fix unlink */
			u.u_offset += sizeof(struct direct);
			goto out1;
		}
		/*
		 * Special handling for ".."
		 */
		if (u.u_dent.d_name[0]=='.' && u.u_dent.d_name[1]=='.' &&
		    u.u_dent.d_name[2]=='\0') {
			if (dp == u.u_rdir)
				u.u_dent.d_ino = dp->i_number;
			else if (u.u_dent.d_ino==ROOTINO &&
			   dp->i_number == ROOTINO) {
				for(i=1; i<NMOUNT; i++)
					if (mount[i].m_bufp != NULL &&
					   mount[i].m_dev == dp->i_dev) {
						iput(dp);
						dp = mount[i].m_inodp;
						ilock(dp);
						dp->i_count++;
						cp -= 2;     /* back over .. */
						goto dirloop;
					}
			}
		}
		d = dp->i_dev;
		ino = dp->i_number;
		iput(dp);
		dp = iget(d, u.u_dent.d_ino);
		if (dp == NULL)
			goto out1;
		/*
		 * Check for symbolic link
		 */
		if ((dp->i_mode&IFMT)==IFLNK && (follow || *cp=='/')) {
			char *ocp;

			ocp = cp;
			while (*cp++)
				;
			if (dp->i_size + (cp-ocp) >= BSIZE-1 || ++nlink>8) {
				u.u_error = ELOOP;
				goto out;
			}
			bcopy(ocp, nbp->b_un.b_addr+dp->i_size, cp-ocp);
			bp = bread(dp->i_dev, bmap(dp, (daddr_t)0, B_READ));
			if (bp->b_flags & B_ERROR) {
				brelse(bp);
				goto out;
			}
			bcopy(bp->b_un.b_addr, nbp->b_un.b_addr, dp->i_size);
			brelse(bp);
			cp = nbp->b_un.b_addr;
			iput(dp);
			if (*cp == '/') {
				while (*cp == '/')
					cp++;
				if ((dp = u.u_rdir) == NULL)
					dp = rootdir;
				ilock(dp);
				dp->i_count++;
			} else {
				dp = iget(d, ino);	/* retrieve directory */
				if (dp == NULL)
					goto out1;
			}
			goto dirloop;
		}
		if (*cp == '/') {
			while (*cp == '/')
				cp++;
			goto dirloop;
		}
		goto out1;
	}
	/*
	 * Search failed.
	 */
	if (bp != NULL)
		brelse(bp);
	if (flag==1 && *cp=='\0' && dp->i_nlink) {
		if (access(dp, IWRITE))
			goto out;
		u.u_pdir = dp;
		if (eo>=0)
			u.u_offset = eo;
		dp->i_flag |= IUPD|ICHG;
		dp = NULL;
		goto out1;
	}
	u.u_error = ENOENT;
out:
	iput(dp);
	dp = NULL;
out1:
	brelse(nbp);
	return (dp);
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
strncmp(s1, s2, len)
	register char *s1, *s2;
	register len;
{

	do {
		if (*s1 != *s2++)
			return (1);
		if (*s1++ == '\0')
			return (0);
	} while (--len);
	return (0);
}
#endif
