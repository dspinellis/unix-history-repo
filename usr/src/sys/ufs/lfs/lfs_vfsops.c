/*	lfs_vfsops.c	6.1	83/07/29	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/fs.h"
#include "../h/buf.h"
#include "../h/mount.h"
#include "../h/file.h"
#include "../h/nami.h"
#include "../h/conf.h"

smount()
{
	register struct a {
		char	*fspec;
		char	*freg;
		int	ronly;
	} *uap;
	dev_t dev;
	register struct inode *ip;
	register struct fs *fs;
	register char *cp;

	uap = (struct a *)u.u_ap;
	u.u_error = getmdev(&dev);
	if (u.u_error)
		return;
	u.u_dirp = (caddr_t)uap->freg;
	ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return;
	if (ip->i_count!=1 || (ip->i_mode&IFMT) != IFDIR) {
		iput(ip);
		u.u_error = EBUSY;
		return;
	}
	fs = mountfs(dev, uap->ronly, ip);
	if (fs == 0)
		return;
	u.u_dirp = uap->freg;
	for (cp = fs->fs_fsmnt; cp < &fs->fs_fsmnt[sizeof(fs->fs_fsmnt) - 2]; )
		if ((*cp++ = uchar()) == 0)
			u.u_dirp--;		/* get 0 again */
	*cp = 0;
}

/* this routine has lousy error codes */
/* this routine has races if running twice */
struct fs *
mountfs(dev, ronly, ip)
	dev_t dev;
	int ronly;
	struct inode *ip;
{
	register struct mount *mp = 0;
	struct buf *tp = 0;
	register struct buf *bp = 0;
	register struct fs *fs;
	int blks;
	caddr_t space;
	int i, size;

	u.u_error =
	    (*bdevsw[major(dev)].d_open)(dev, ronly ? FREAD : FREAD|FWRITE);
	if (u.u_error) {
		u.u_error = EIO;
		goto out;
	}
	tp = bread(dev, SBLOCK, SBSIZE);
	if (tp->b_flags & B_ERROR)
		goto out;
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp != 0 && dev == mp->m_dev) {
			mp = 0;
			goto out;
		}
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp == 0)
			goto found;
	mp = 0;
	goto out;
found:
	mp->m_bufp = tp;	/* just to reserve this slot */
	mp->m_dev = NODEV;
	fs = tp->b_un.b_fs;
	bp = geteblk((int)fs->fs_sbsize);
	mp->m_bufp = bp;
	bcopy((caddr_t)tp->b_un.b_addr, (caddr_t)bp->b_un.b_addr,
	   (u_int)fs->fs_sbsize);
	brelse(tp);
	tp = 0;
	fs = bp->b_un.b_fs;
	if (fs->fs_magic != FS_MAGIC || fs->fs_bsize > MAXBSIZE)
		goto out;
	fs->fs_ronly = (ronly != 0);
	if (ronly == 0)
		fs->fs_fmod = 1;
	blks = howmany(fs->fs_cssize, fs->fs_fsize);
	space = wmemall(vmemall, (int)fs->fs_cssize);
	if (space == 0)
		goto out;
	for (i = 0; i < blks; i += fs->fs_frag) {
		size = fs->fs_bsize;
		if (i + fs->fs_frag > blks)
			size = (blks - i) * fs->fs_fsize;
		tp = bread(dev, fsbtodb(fs, fs->fs_csaddr + i), size);
		if (tp->b_flags&B_ERROR) {
			wmemfree(space, (int)fs->fs_cssize);
			goto out;
		}
		bcopy((caddr_t)tp->b_un.b_addr, space, (u_int)size);
		fs->fs_csp[i / fs->fs_frag] = (struct csum *)space;
		space += size;
		brelse(tp);
		tp = 0;
	}
	mp->m_inodp = ip;
	mp->m_dev = dev;
	if (ip) {
		ip->i_flag |= IMOUNT;
		iunlock(ip);
	}
	return (fs);
out:
	u.u_error = EBUSY;
	if (ip)
		iput(ip);
	if (mp)
		mp->m_bufp = 0;
	if (bp)
		brelse(bp);
	if (tp)
		brelse(tp);
	return (0);
}

umount()
{
	struct a {
		char	*fspec;
	};

	u.u_error = unmount1(0);
}

unmount1(forcibly)
	int forcibly;
{
	dev_t dev;
	register struct mount *mp;
	int stillopen, flag, error;
	register struct inode *ip;
	register struct fs *fs;

	error = getmdev(&dev);
	if (error)
		return (error);
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp != NULL && dev == mp->m_dev)
			goto found;
	return (EINVAL);
found:
	xumount(dev);	/* remove unused sticky files from text table */
	update();
#ifdef QUOTA
	if ((stillopen = iflush(dev, mp->m_qinod)) < 0 && !forcibly)
#else
	if ((stillopen = iflush(dev)) < 0 && !forcibly)
#endif
		return (EBUSY);
	if (stillopen < 0)
		return (EBUSY);			/* XXX */
#ifdef QUOTA
	closedq(mp);
	/*
	 * Here we have to iflush again to get rid of the quota inode.
	 * A drag, but it would be ugly to cheat, & this doesn't happen often
	 */
	(void)iflush(dev, (struct inode *)NULL);
#endif
	ip = mp->m_inodp;
	ip->i_flag &= ~IMOUNT;
	irele(ip);
	fs = mp->m_bufp->b_un.b_fs;
	wmemfree((caddr_t)fs->fs_csp[0], (int)fs->fs_cssize);
	flag = !fs->fs_ronly;
	brelse(mp->m_bufp);
	mp->m_bufp = 0;
	mp->m_dev = 0;
	mpurge(mp - &mount[0]);
	if (!stillopen) {
		(*bdevsw[major(dev)].d_close)(dev, flag);
		binval(dev);
	}
	return (0);
}

sbupdate(mp)
	struct mount *mp;
{
	register struct fs *fs = mp->m_bufp->b_un.b_fs;
	register struct buf *bp;
	int blks;
	caddr_t space;
	int i, size;

	bp = getblk(mp->m_dev, SBLOCK, (int)fs->fs_sbsize);
	bcopy((caddr_t)fs, bp->b_un.b_addr, (u_int)fs->fs_sbsize);
	bwrite(bp);
	blks = howmany(fs->fs_cssize, fs->fs_fsize);
	space = (caddr_t)fs->fs_csp[0];
	for (i = 0; i < blks; i += fs->fs_frag) {
		size = fs->fs_bsize;
		if (i + fs->fs_frag > blks)
			size = (blks - i) * fs->fs_fsize;
		bp = getblk(mp->m_dev, fsbtodb(fs, fs->fs_csaddr + i), size);
		bcopy(space, bp->b_un.b_addr, (u_int)size);
		space += size;
		bwrite(bp);
	}
}

/*
 * Common code for mount and umount.
 * Check that the user's argument is a reasonable
 * thing on which to mount, and return the device number if so.
 */
getmdev(pdev)
	dev_t *pdev;
{
	dev_t dev;
	register struct inode *ip;

	if (!suser())
		return (u.u_error);
	ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return (u.u_error);
	if ((ip->i_mode&IFMT) != IFBLK)
		return (ENOTBLK);
	dev = (dev_t)ip->i_rdev;
	if (major(dev) >= nblkdev)
		return (ENXIO);
	iput(ip);
	*pdev = dev;
	return (0);
}
