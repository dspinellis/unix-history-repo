/*	ufs_vfsops.c	6.10	85/05/22	*/

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "inode.h"
#include "proc.h"
#include "fs.h"
#include "buf.h"
#include "mount.h"
#include "file.h"
#include "conf.h"

smount()
{
	register struct a {
		char	*fspec;
		char	*freg;
		int	ronly;
	} *uap = (struct a *)u.u_ap;
	dev_t dev;
	register struct inode *ip;
	register struct fs *fs;
	register char *cp;
	register struct nameidata *ndp = &u.u_nd;
	u_int len;

	u.u_error = getmdev(&dev, uap->fspec);
	if (u.u_error)
		return;
	ndp->ni_nameiop = LOOKUP | NOCACHE | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = (caddr_t)uap->freg;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	if (ip->i_count != 1) {
		iput(ip);
		u.u_error = EBUSY;
		return;
	}
	if ((ip->i_mode&IFMT) != IFDIR) {
		iput(ip);
		u.u_error = ENOTDIR;
		return;
	}
	fs = mountfs(dev, uap->ronly, ip);
	if (fs == 0)
		return;
	(void) copyinstr(uap->freg, fs->fs_fsmnt, sizeof(fs->fs_fsmnt)-1, &len);
	bzero(fs->fs_fsmnt + len, sizeof (fs->fs_fsmnt) - len);
}

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
	register error;
	int needclose = 0;

	error =
	    (*bdevsw[major(dev)].d_open)(dev, ronly ? FREAD : FREAD|FWRITE);
	if (error)
		goto out;
	needclose = 1;
	tp = bread(dev, SBLOCK, SBSIZE);
	if (tp->b_flags & B_ERROR)
		goto out;
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp != 0 && dev == mp->m_dev) {
			mp = 0;
			error = EBUSY;
			goto out;
		}
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp == 0)
			goto found;
	mp = 0;
	error = EMFILE;		/* needs translation */
	goto out;
found:
	mp->m_bufp = tp;	/* just to reserve this slot */
	mp->m_dev = NODEV;
	fs = tp->b_un.b_fs;
	if (fs->fs_magic != FS_MAGIC || fs->fs_bsize > MAXBSIZE
	    || fs->fs_bsize < sizeof(struct fs)) {
		error = EINVAL;		/* also needs translation */
		goto out;
	}
	bp = geteblk((int)fs->fs_sbsize);
	mp->m_bufp = bp;
	bcopy((caddr_t)tp->b_un.b_addr, (caddr_t)bp->b_un.b_addr,
	   (u_int)fs->fs_sbsize);
	brelse(tp);
	tp = 0;
	fs = bp->b_un.b_fs;
	fs->fs_ronly = (ronly != 0);
	if (ronly == 0)
		fs->fs_fmod = 1;
	blks = howmany(fs->fs_cssize, fs->fs_fsize);
	space = wmemall(vmemall, (int)fs->fs_cssize);
	if (space == 0) {
		error = ENOMEM;
		goto out;
	}
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
		fs->fs_csp[fragstoblks(fs, i)] = (struct csum *)space;
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
	if (error == 0)
		error = EIO;
	if (ip)
		iput(ip);
	if (mp)
		mp->m_bufp = 0;
	if (bp)
		brelse(bp);
	if (tp)
		brelse(tp);
	if (needclose)
		(*bdevsw[major(dev)].d_close)(dev, ronly ? FREAD : FREAD|FWRITE);
	binval(dev);
	u.u_error = error;
	return (0);
}

umount()
{
	struct a {
		char	*fspec;
	} *uap = (struct a *)u.u_ap;

	u.u_error = unmount1(uap->fspec, 0);
}

unmount1(fname, forcibly)
	caddr_t fname;
	int forcibly;
{
	dev_t dev;
	register struct mount *mp;
	int stillopen, flag, error;
	register struct inode *ip;
	register struct fs *fs;

	error = getmdev(&dev, fname);
	if (error)
		return (error);
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp != NULL && dev == mp->m_dev)
			goto found;
	return (EINVAL);
found:
	xumount(dev);	/* remove unused sticky files from text table */
	nchinval(dev);	/* flush the name cache */
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
getmdev(pdev, fname)
	caddr_t fname;
	dev_t *pdev;
{
	dev_t dev;
	register struct inode *ip;
	register struct nameidata *ndp = &u.u_nd;

	if (!suser())
		return (u.u_error);
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = fname;
	ip = namei(ndp);
	if (ip == NULL) {
		if (u.u_error == ENOENT)
			return (ENODEV); /* needs translation */
		return (u.u_error);
	}
	if ((ip->i_mode&IFMT) != IFBLK) {
		iput(ip);
		return (ENOTBLK);
	}
	dev = (dev_t)ip->i_rdev;
	iput(ip);
	if (major(dev) >= nblkdev)
		return (ENXIO);
	*pdev = dev;
	return (0);
}
