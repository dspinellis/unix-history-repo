/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ufs_vnops.c	6.19 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "inode.h"
#include "fs.h"
#include "buf.h"
#include "proc.h"
#include "quota.h"
#include "uio.h"
#include "socket.h"
#include "socketvar.h"
#include "mount.h"

extern	struct fileops inodeops;
struct	file *getinode();

/*
 * Change current working directory (``.'').
 */
chdir()
{

	chdirec(&u.u_cdir);
}

/*
 * Change notion of root (``/'') directory.
 */
chroot()
{

	if (suser())
		chdirec(&u.u_rdir);
}

/*
 * Common routine for chroot and chdir.
 */
chdirec(ipp)
	register struct inode **ipp;
{
	register struct inode *ip;
	struct a {
		char	*fname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;

	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT) != IFDIR) {
		u.u_error = ENOTDIR;
		goto bad;
	}
	if (access(ip, IEXEC))
		goto bad;
	IUNLOCK(ip);
	if (*ipp)
		irele(*ipp);
	*ipp = ip;
	return;

bad:
	iput(ip);
}

/*
 * Open system call.
 */
open()
{
	struct a {
		char	*fname;
		int	mode;
		int	crtmode;
	} *uap = (struct a *) u.u_ap;

	copen(uap->mode-FOPEN, uap->crtmode, uap->fname);
}

/*
 * Creat system call.
 */
creat()
{
	struct a {
		char	*fname;
		int	fmode;
	} *uap = (struct a *)u.u_ap;

	copen(FWRITE|FCREAT|FTRUNC, uap->fmode, uap->fname);
}

/*
 * Common code for open and creat.
 * Check permissions, allocate an open file structure,
 * and call the device open routine if any.
 */
copen(mode, arg, fname)
	register int mode;
	int arg;
	caddr_t fname;
{
	register struct inode *ip;
	register struct file *fp;
	register struct nameidata *ndp = &u.u_nd;
	int i;

#ifdef notdef
	if ((mode&(FREAD|FWRITE)) == 0) {
		u.u_error = EINVAL;
		return;
	}
#endif
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = fname;
	if (mode&FCREAT) {
		if (mode & FEXCL)
			ndp->ni_nameiop = CREATE;
		else
			ndp->ni_nameiop = CREATE | FOLLOW;
		ip = namei(ndp);
		if (ip == NULL) {
			if (u.u_error)
				return;
			ip = maknode(arg&07777&(~ISVTX), ndp);
			if (ip == NULL)
				return;
			mode &= ~FTRUNC;
		} else {
			if (mode&FEXCL) {
				u.u_error = EEXIST;
				iput(ip);
				return;
			}
			mode &= ~FCREAT;
		}
	} else {
		ndp->ni_nameiop = LOOKUP | FOLLOW;
		ip = namei(ndp);
		if (ip == NULL)
			return;
	}
	if ((ip->i_mode & IFMT) == IFSOCK) {
		u.u_error = EOPNOTSUPP;
		goto bad;
	}
	if ((mode&FCREAT) == 0) {
		if (mode&FREAD)
			if (access(ip, IREAD))
				goto bad;
		if (mode&(FWRITE|FTRUNC)) {
			if (access(ip, IWRITE))
				goto bad;
			if ((ip->i_mode&IFMT) == IFDIR) {
				u.u_error = EISDIR;
				goto bad;
			}
		}
	}
	fp = falloc();
	if (fp == NULL)
		goto bad;
	if (mode&FTRUNC)
		itrunc(ip, (u_long)0);
	IUNLOCK(ip);
	fp->f_flag = mode&FMASK;
	fp->f_type = DTYPE_INODE;
	fp->f_ops = &inodeops;
	fp->f_data = (caddr_t)ip;
	i = u.u_r.r_val1;
	if (setjmp(&u.u_qsave)) {
		if (u.u_error == 0)
			u.u_error = EINTR;
		u.u_ofile[i] = NULL;
		closef(fp);
		return;
	}
	u.u_error = openi(ip, mode);
	if (u.u_error == 0)
		return;
	u.u_ofile[i] = NULL;
	fp->f_count--;
	irele(ip);
	return;
bad:
	iput(ip);
}

/*
 * Mknod system call
 */
mknod()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		int	fmode;
		int	dev;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;

	if (!suser())
		return;
	ndp->ni_nameiop = CREATE;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	ip = namei(ndp);
	if (ip != NULL) {
		u.u_error = EEXIST;
		goto out;
	}
	if (u.u_error)
		return;
	ip = maknode(uap->fmode, ndp);
	if (ip == NULL)
		return;
	switch (ip->i_mode & IFMT) {

	case IFMT:	/* used by badsect to flag bad sectors */
	case IFCHR:
	case IFBLK:
		if (uap->dev) {
			/*
			 * Want to be able to use this to make badblock
			 * inodes, so don't truncate the dev number.
			 */
			ip->i_rdev = uap->dev;
			ip->i_flag |= IACC|IUPD|ICHG;
		}
	}

out:
	iput(ip);
}

/*
 * link system call
 */
link()
{
	register struct inode *ip, *xp;
	register struct a {
		char	*target;
		char	*linkname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;

	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->target;
	ip = namei(ndp);	/* well, this routine is doomed anyhow */
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT) == IFDIR && !suser()) {
		iput(ip);
		return;
	}
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	iupdat(ip, &time, &time, 1);
	IUNLOCK(ip);
	ndp->ni_nameiop = CREATE;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = (caddr_t)uap->linkname;
	xp = namei(ndp);
	if (xp != NULL) {
		u.u_error = EEXIST;
		iput(xp);
		goto out;
	}
	if (u.u_error)
		goto out;
	if (ndp->ni_pdir->i_dev != ip->i_dev) {
		iput(ndp->ni_pdir);
		u.u_error = EXDEV;
		goto out;
	}
	u.u_error = direnter(ip, ndp);
out:
	if (u.u_error) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
	irele(ip);
}

/*
 * symlink -- make a symbolic link
 */
symlink()
{
	register struct a {
		char	*target;
		char	*linkname;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip;
	register char *tp;
	register c, nc;
	register struct nameidata *ndp = &u.u_nd;

	tp = uap->target;
	nc = 0;
	while (c = fubyte(tp)) {
		if (c < 0) {
			u.u_error = EFAULT;
			return;
		}
		tp++;
		nc++;
	}
	ndp->ni_nameiop = CREATE;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->linkname;
	ip = namei(ndp);
	if (ip) {
		iput(ip);
		u.u_error = EEXIST;
		return;
	}
	if (u.u_error)
		return;
	ip = maknode(IFLNK | 0777, ndp);
	if (ip == NULL)
		return;
	u.u_error = rdwri(UIO_WRITE, ip, uap->target, nc, 0, 0, (int *)0);
	/* handle u.u_error != 0 */
	iput(ip);
}

/*
 * Unlink system call.
 * Hard to avoid races here, especially
 * in unlinking directories.
 */
unlink()
{
	struct a {
		char	*fname;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip, *dp;
	register struct nameidata *ndp = &u.u_nd;

	ndp->ni_nameiop = DELETE | LOCKPARENT;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	dp = ndp->ni_pdir;
	if ((ip->i_mode&IFMT) == IFDIR && !suser())
		goto out;
	/*
	 * Don't unlink a mounted file.
	 */
	if (ip->i_dev != dp->i_dev) {
		u.u_error = EBUSY;
		goto out;
	}
	if (ip->i_flag&ITEXT)
		xrele(ip);	/* try once to free text */
	if (dirremove(ndp)) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
out:
	if (dp == ip)
		irele(ip);
	else
		iput(ip);
	iput(dp);
}

/*
 * Seek system call
 */
lseek()
{
	register struct file *fp;
	register struct a {
		int	fd;
		off_t	off;
		int	sbase;
	} *uap = (struct a *)u.u_ap;

	GETF(fp, uap->fd);
	if (fp->f_type != DTYPE_INODE) {
		u.u_error = ESPIPE;
		return;
	}
	switch (uap->sbase) {

	case L_INCR:
		fp->f_offset += uap->off;
		break;

	case L_XTND:
		fp->f_offset = uap->off + ((struct inode *)fp->f_data)->i_size;
		break;

	case L_SET:
		fp->f_offset = uap->off;
		break;

	default:
		u.u_error = EINVAL;
		return;
	}
	u.u_r.r_off = fp->f_offset;
}

/*
 * Access system call
 */
saccess()
{
	register svuid, svgid;
	register struct inode *ip;
	register struct a {
		char	*fname;
		int	fmode;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;

	svuid = u.u_uid;
	svgid = u.u_gid;
	u.u_uid = u.u_ruid;
	u.u_gid = u.u_rgid;
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	ip = namei(ndp);
	if (ip != NULL) {
		if ((uap->fmode&R_OK) && access(ip, IREAD))
			goto done;
		if ((uap->fmode&W_OK) && access(ip, IWRITE))
			goto done;
		if ((uap->fmode&X_OK) && access(ip, IEXEC))
			goto done;
done:
		iput(ip);
	}
	u.u_uid = svuid;
	u.u_gid = svgid;
}

/*
 * Stat system call.  This version follows links.
 */
stat()
{

	stat1(FOLLOW);
}

/*
 * Lstat system call.  This version does not follow links.
 */
lstat()
{

	stat1(NOFOLLOW);
}

stat1(follow)
	int follow;
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		struct stat *ub;
	} *uap = (struct a *)u.u_ap;
	struct stat sb;
	register struct nameidata *ndp = &u.u_nd;

	ndp->ni_nameiop = LOOKUP | follow;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	(void) ino_stat(ip, &sb);
	iput(ip);
	u.u_error = copyout((caddr_t)&sb, (caddr_t)uap->ub, sizeof (sb));
}

/*
 * Return target name of a symbolic link
 */
readlink()
{
	register struct inode *ip;
	register struct a {
		char	*name;
		char	*buf;
		int	count;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	int resid;

	ndp->ni_nameiop = LOOKUP;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->name;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT) != IFLNK) {
		u.u_error = EINVAL;
		goto out;
	}
	u.u_error = rdwri(UIO_READ, ip, uap->buf, uap->count, 0, 0, &resid);
out:
	iput(ip);
	u.u_r.r_val1 = uap->count - resid;
}

/*
 * Change mode of a file given path name.
 */
chmod()
{
	struct inode *ip;
	struct a {
		char	*fname;
		int	fmode;
	} *uap = (struct a *)u.u_ap;

	if ((ip = owner(uap->fname, FOLLOW)) == NULL)
		return;
	u.u_error = chmod1(ip, uap->fmode);
	iput(ip);
}

/*
 * Change mode of a file given a file descriptor.
 */
fchmod()
{
	struct a {
		int	fd;
		int	fmode;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip;
	register struct file *fp;

	fp = getinode(uap->fd);
	if (fp == NULL)
		return;
	ip = (struct inode *)fp->f_data;
	if (u.u_uid != ip->i_uid && !suser())
		return;
	ILOCK(ip);
	u.u_error = chmod1(ip, uap->fmode);
	IUNLOCK(ip);
}

/*
 * Change the mode on a file.
 * Inode must be locked before calling.
 */
chmod1(ip, mode)
	register struct inode *ip;
	register int mode;
{

	if (ip->i_fs->fs_ronly)
		return (EROFS);
	ip->i_mode &= ~07777;
	if (u.u_uid) {
		if ((ip->i_mode & IFMT) != IFDIR)
			mode &= ~ISVTX;
		if (!groupmember(ip->i_gid))
			mode &= ~ISGID;
	}
	ip->i_mode |= mode&07777;
	ip->i_flag |= ICHG;
	if (ip->i_flag&ITEXT && (ip->i_mode&ISVTX)==0)
		xrele(ip);
	return (0);
}

/*
 * Set ownership given a path name.
 */
chown()
{
	struct inode *ip;
	struct a {
		char	*fname;
		int	uid;
		int	gid;
	} *uap = (struct a *)u.u_ap;

	if (!suser() || (ip = owner(uap->fname, NOFOLLOW)) == NULL)
		return;
	u.u_error = chown1(ip, uap->uid, uap->gid);
	iput(ip);
}

/*
 * Set ownership given a file descriptor.
 */
fchown()
{
	struct a {
		int	fd;
		int	uid;
		int	gid;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip;
	register struct file *fp;

	fp = getinode(uap->fd);
	if (fp == NULL)
		return;
	ip = (struct inode *)fp->f_data;
	if (!suser())
		return;
	ILOCK(ip);
	u.u_error = chown1(ip, uap->uid, uap->gid);
	IUNLOCK(ip);
}

/*
 * Perform chown operation on inode ip;
 * inode must be locked prior to call.
 */
chown1(ip, uid, gid)
	register struct inode *ip;
	int uid, gid;
{
#ifdef QUOTA
	register long change;
#endif

	if (ip->i_fs->fs_ronly)
		return (EROFS);
	if (uid == -1)
		uid = ip->i_uid;
	if (gid == -1)
		gid = ip->i_gid;
#ifdef QUOTA
	if (ip->i_uid == uid)		/* this just speeds things a little */
		change = 0;
	else
		change = ip->i_blocks;
	(void) chkdq(ip, -change, 1);
	(void) chkiq(ip->i_dev, ip, ip->i_uid, 1);
	dqrele(ip->i_dquot);
#endif
	ip->i_uid = uid;
	ip->i_gid = gid;
	ip->i_flag |= ICHG;
	if (u.u_ruid != 0)
		ip->i_mode &= ~(ISUID|ISGID);
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
	(void) chkdq(ip, change, 1);
	(void) chkiq(ip->i_dev, (struct inode *)NULL, uid, 1);
	return (u.u_error);		/* should == 0 ALWAYS !! */
#else
	return (0);
#endif
}

utimes()
{
	register struct a {
		char	*fname;
		struct	timeval *tptr;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip;
	struct timeval tv[2];

	if ((ip = owner(uap->fname, FOLLOW)) == NULL)
		return;
	if (ip->i_fs->fs_ronly) {
		u.u_error = EROFS;
		iput(ip);
		return;
	}
	u.u_error = copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof (tv));
	if (u.u_error == 0) {
		ip->i_flag |= IACC|IUPD|ICHG;
		iupdat(ip, &tv[0], &tv[1], 0);
	}
	iput(ip);
}

/*
 * Flush any pending I/O.
 */
sync()
{

	update();
}

/*
 * Truncate a file given its path name.
 */
truncate()
{
	struct a {
		char	*fname;
		u_long	length;
	} *uap = (struct a *)u.u_ap;
	struct inode *ip;
	register struct nameidata *ndp = &u.u_nd;

	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	if (access(ip, IWRITE))
		goto bad;
	if ((ip->i_mode&IFMT) == IFDIR) {
		u.u_error = EISDIR;
		goto bad;
	}
	itrunc(ip, uap->length);
bad:
	iput(ip);
}

/*
 * Truncate a file given a file descriptor.
 */
ftruncate()
{
	struct a {
		int	fd;
		u_long	length;
	} *uap = (struct a *)u.u_ap;
	struct inode *ip;
	struct file *fp;

	fp = getinode(uap->fd);
	if (fp == NULL)
		return;
	if ((fp->f_flag&FWRITE) == 0) {
		u.u_error = EINVAL;
		return;
	}
	ip = (struct inode *)fp->f_data;
	ILOCK(ip);
	itrunc(ip, uap->length);
	IUNLOCK(ip);
}

/*
 * Synch an open file.
 */
fsync()
{
	struct a {
		int	fd;
	} *uap = (struct a *)u.u_ap;
	struct inode *ip;
	struct file *fp;

	fp = getinode(uap->fd);
	if (fp == NULL)
		return;
	ip = (struct inode *)fp->f_data;
	ILOCK(ip);
	syncip(ip);
	IUNLOCK(ip);
}

/*
 * Rename system call.
 * 	rename("foo", "bar");
 * is essentially
 *	unlink("bar");
 *	link("foo", "bar");
 *	unlink("foo");
 * but ``atomically''.  Can't do full commit without saving state in the
 * inode on disk which isn't feasible at this time.  Best we can do is
 * always guarantee the target exists.
 *
 * Basic algorithm is:
 *
 * 1) Bump link count on source while we're linking it to the
 *    target.  This also insure the inode won't be deleted out
 *    from underneath us while we work (it may be truncated by
 *    a concurrent `trunc' or `open' for creation).
 * 2) Link source to destination.  If destination already exists,
 *    delete it first.
 * 3) Unlink source reference to inode if still around. If a
 *    directory was moved and the parent of the destination
 *    is different from the source, patch the ".." entry in the
 *    directory.
 *
 * Source and destination must either both be directories, or both
 * not be directories.  If target is a directory, it must be empty.
 */
rename()
{
	struct a {
		char	*from;
		char	*to;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip, *xp, *dp;
	struct dirtemplate dirbuf;
	int doingdirectory = 0, oldparent = 0, newparent = 0;
	register struct nameidata *ndp = &u.u_nd;
	int error = 0;

	ndp->ni_nameiop = DELETE | LOCKPARENT;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->from;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	dp = ndp->ni_pdir;
	if ((ip->i_mode&IFMT) == IFDIR) {
		register struct direct *d;

		d = &ndp->ni_dent;
		/*
		 * Avoid ".", "..", and aliases of "." for obvious reasons.
		 */
		if ((d->d_namlen == 1 && d->d_name[0] == '.') ||
		    (d->d_namlen == 2 && bcmp(d->d_name, "..", 2) == 0) ||
		    (dp == ip) || (ip->i_flag & IRENAME)) {
			iput(dp);
			if (dp == ip)
				irele(ip);
			else
				iput(ip);
			u.u_error = EINVAL;
			return;
		}
		ip->i_flag |= IRENAME;
		oldparent = dp->i_number;
		doingdirectory++;
	}
	iput(dp);

	/*
	 * 1) Bump link count while we're moving stuff
	 *    around.  If we crash somewhere before
	 *    completing our work, the link count
	 *    may be wrong, but correctable.
	 */
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	iupdat(ip, &time, &time, 1);
	IUNLOCK(ip);

	/*
	 * When the target exists, both the directory
	 * and target inodes are returned locked.
	 */
	ndp->ni_nameiop = CREATE | LOCKPARENT | NOCACHE;
	ndp->ni_dirp = (caddr_t)uap->to;
	xp = namei(ndp);
	if (u.u_error) {
		error = u.u_error;
		goto out;
	}
	dp = ndp->ni_pdir;
	/*
	 * If ".." must be changed (ie the directory gets a new
	 * parent) then the source directory must not be in the
	 * directory heirarchy above the target, as this would
	 * orphan everything below the source directory. Also
	 * the user must have write permission in the source so
	 * as to be able to change "..". We must repeat the call 
	 * to namei, as the parent directory is unlocked by the
	 * call to checkpath().
	 */
	if (oldparent != dp->i_number)
		newparent = dp->i_number;
	if (doingdirectory && newparent) {
		if (access(ip, IWRITE))
			goto bad;
		do {
			dp = ndp->ni_pdir;
			if (xp != NULL)
				iput(xp);
			u.u_error = checkpath(ip, dp);
			if (u.u_error)
				goto out;
			xp = namei(ndp);
			if (u.u_error) {
				error = u.u_error;
				goto out;
			}
		} while (dp != ndp->ni_pdir);
	}
	/*
	 * 2) If target doesn't exist, link the target
	 *    to the source and unlink the source. 
	 *    Otherwise, rewrite the target directory
	 *    entry to reference the source inode and
	 *    expunge the original entry's existence.
	 */
	if (xp == NULL) {
		if (dp->i_dev != ip->i_dev) {
			error = EXDEV;
			goto bad;
		}
		/*
		 * Account for ".." in new directory.
		 * When source and destination have the same
		 * parent we don't fool with the link count.
		 */
		if (doingdirectory && newparent) {
			dp->i_nlink++;
			dp->i_flag |= ICHG;
			iupdat(dp, &time, &time, 1);
		}
		error = direnter(ip, ndp);
		if (error)
			goto out;
	} else {
		if (xp->i_dev != dp->i_dev || xp->i_dev != ip->i_dev) {
			error = EXDEV;
			goto bad;
		}
		/*
		 * Short circuit rename(foo, foo).
		 */
		if (xp->i_number == ip->i_number)
			goto bad;
		/*
		 * Target must be empty if a directory
		 * and have no links to it.
		 * Also, insure source and target are
		 * compatible (both directories, or both
		 * not directories).
		 */
		if ((xp->i_mode&IFMT) == IFDIR) {
			if (!dirempty(xp, dp->i_number) || xp->i_nlink > 2) {
				error = ENOTEMPTY;
				goto bad;
			}
			if (!doingdirectory) {
				error = ENOTDIR;
				goto bad;
			}
			cacheinval(dp);
		} else if (doingdirectory) {
			error = EISDIR;
			goto bad;
		}
		dirrewrite(dp, ip, ndp);
		if (u.u_error) {
			error = u.u_error;
			goto bad1;
		}
		/*
		 * Adjust the link count of the target to
		 * reflect the dirrewrite above.  If this is
		 * a directory it is empty and there are
		 * no links to it, so we can squash the inode and
		 * any space associated with it.  We disallowed
		 * renaming over top of a directory with links to
		 * it above, as the remaining link would point to
		 * a directory without "." or ".." entries.
		 */
		xp->i_nlink--;
		if (doingdirectory) {
			if (--xp->i_nlink != 0)
				panic("rename: linked directory");
			itrunc(xp, (u_long)0);
		}
		xp->i_flag |= ICHG;
		iput(xp);
		xp = NULL;
	}

	/*
	 * 3) Unlink the source.
	 */
	ndp->ni_nameiop = DELETE | LOCKPARENT;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->from;
	xp = namei(ndp);
	if (xp != NULL)
		dp = ndp->ni_pdir;
	else
		dp = NULL;
	/*
	 * Insure that the directory entry still exists and has not
	 * changed while the new name has been entered. If the source is
	 * a file then the entry may have been unlinked or renamed. In
	 * either case there is no further work to be done. If the source
	 * is a directory then it cannot have been rmdir'ed; its link
	 * count of three would cause a rmdir to fail with ENOTEMPTY.
	 * The IRENAME flag insures that it cannot be moved by another
	 * rename.
	 */
	if (xp != ip) {
		if (doingdirectory)
			panic("rename: lost dir entry");
	} else {
		/*
		 * If the source is a directory with a
		 * new parent, the link count of the old
		 * parent directory must be decremented
		 * and ".." set to point to the new parent.
		 */
		if (doingdirectory && newparent) {
			dp->i_nlink--;
			dp->i_flag |= ICHG;
			error = rdwri(UIO_READ, xp, (caddr_t)&dirbuf,
				sizeof (struct dirtemplate), (off_t)0, 1,
				(int *)0);
			if (error == 0) {
				if (dirbuf.dotdot_namlen != 2 ||
				    dirbuf.dotdot_name[0] != '.' ||
				    dirbuf.dotdot_name[1] != '.') {
					printf("rename: mangled dir\n");
				} else {
					dirbuf.dotdot_ino = newparent;
					(void) rdwri(UIO_WRITE, xp,
					    (caddr_t)&dirbuf,
					    sizeof (struct dirtemplate),
					    (off_t)0, 1, (int *)0);
					cacheinval(dp);
				}
			}
		}
		if (dirremove(ndp)) {
			xp->i_nlink--;
			xp->i_flag |= ICHG;
		}
		xp->i_flag &= ~IRENAME;
		if (error == 0)		/* XXX conservative */
			error = u.u_error;
	}
	if (dp)
		iput(dp);
	if (xp)
		iput(xp);
	irele(ip);
	if (error)
		u.u_error = error;
	return;

bad:
	iput(dp);
bad1:
	if (xp)
		iput(xp);
out:
	ip->i_nlink--;
	ip->i_flag |= ICHG;
	irele(ip);
	if (error)
		u.u_error = error;
}

/*
 * Make a new file.
 */
struct inode *
maknode(mode, ndp)
	int mode;
	register struct nameidata *ndp;
{
	register struct inode *ip;
	register struct inode *pdir = ndp->ni_pdir;
	ino_t ipref;

	if ((mode & IFMT) == IFDIR)
		ipref = dirpref(pdir->i_fs);
	else
		ipref = pdir->i_number;
	ip = ialloc(pdir, ipref, mode);
	if (ip == NULL) {
		iput(pdir);
		return (NULL);
	}
#ifdef QUOTA
	if (ip->i_dquot != NODQUOT)
		panic("maknode: dquot");
#endif
	ip->i_flag |= IACC|IUPD|ICHG;
	if ((mode & IFMT) == 0)
		mode |= IFREG;
	ip->i_mode = mode & ~u.u_cmask;
	ip->i_nlink = 1;
	ip->i_uid = u.u_uid;
	ip->i_gid = pdir->i_gid;
	if (ip->i_mode & ISGID && !groupmember(ip->i_gid))
		ip->i_mode &= ~ISGID;
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
#endif

	/*
	 * Make sure inode goes to disk before directory entry.
	 */
	iupdat(ip, &time, &time, 1);
	u.u_error = direnter(ip, ndp);
	if (u.u_error) {
		/*
		 * Write error occurred trying to update directory
		 * so must deallocate the inode.
		 */
		ip->i_nlink = 0;
		ip->i_flag |= ICHG;
		iput(ip);
		return (NULL);
	}
	return (ip);
}

/*
 * A virgin directory (no blushing please).
 */
struct dirtemplate mastertemplate = {
	0, 12, 1, ".",
	0, DIRBLKSIZ - 12, 2, ".."
};

/*
 * Mkdir system call
 */
mkdir()
{
	struct a {
		char	*name;
		int	dmode;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip, *dp;
	struct dirtemplate dirtemplate;
	register struct nameidata *ndp = &u.u_nd;

	ndp->ni_nameiop = CREATE;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->name;
	ip = namei(ndp);
	if (u.u_error)
		return;
	if (ip != NULL) {
		iput(ip);
		u.u_error = EEXIST;
		return;
	}
	dp = ndp->ni_pdir;
	uap->dmode &= 0777;
	uap->dmode |= IFDIR;
	/*
	 * Must simulate part of maknode here
	 * in order to acquire the inode, but
	 * not have it entered in the parent
	 * directory.  The entry is made later
	 * after writing "." and ".." entries out.
	 */
	ip = ialloc(dp, dirpref(dp->i_fs), uap->dmode);
	if (ip == NULL) {
		iput(dp);
		return;
	}
#ifdef QUOTA
	if (ip->i_dquot != NODQUOT)
		panic("mkdir: dquot");
#endif
	ip->i_flag |= IACC|IUPD|ICHG;
	ip->i_mode = uap->dmode & ~u.u_cmask;
	ip->i_nlink = 2;
	ip->i_uid = u.u_uid;
	ip->i_gid = dp->i_gid;
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
#endif
	iupdat(ip, &time, &time, 1);

	/*
	 * Bump link count in parent directory
	 * to reflect work done below.  Should
	 * be done before reference is created
	 * so reparation is possible if we crash.
	 */
	dp->i_nlink++;
	dp->i_flag |= ICHG;
	iupdat(dp, &time, &time, 1);

	/*
	 * Initialize directory with "."
	 * and ".." from static template.
	 */
	dirtemplate = mastertemplate;
	dirtemplate.dot_ino = ip->i_number;
	dirtemplate.dotdot_ino = dp->i_number;
	u.u_error = rdwri(UIO_WRITE, ip, (caddr_t)&dirtemplate,
		sizeof (dirtemplate), (off_t)0, 1, (int *)0);
	if (u.u_error) {
		dp->i_nlink--;
		dp->i_flag |= ICHG;
		goto bad;
	}
	if (DIRBLKSIZ > ip->i_fs->fs_fsize)
		panic("mkdir: blksize");     /* XXX - should grow with bmap() */
	else
		ip->i_size = DIRBLKSIZ;
	/*
	 * Directory all set up, now
	 * install the entry for it in
	 * the parent directory.
	 */
	u.u_error = direnter(ip, ndp);
	dp = NULL;
	if (u.u_error) {
		ndp->ni_nameiop = LOOKUP | NOCACHE;
		ndp->ni_segflg = UIO_USERSPACE;
		ndp->ni_dirp = uap->name;
		dp = namei(ndp);
		if (dp) {
			dp->i_nlink--;
			dp->i_flag |= ICHG;
		}
	}
bad:
	/*
	 * No need to do an explicit itrunc here,
	 * irele will do this for us because we set
	 * the link count to 0.
	 */
	if (u.u_error) {
		ip->i_nlink = 0;
		ip->i_flag |= ICHG;
	}
	if (dp)
		iput(dp);
	iput(ip);
}

/*
 * Rmdir system call.
 */
rmdir()
{
	struct a {
		char	*name;
	} *uap = (struct a *)u.u_ap;
	register struct inode *ip, *dp;
	register struct nameidata *ndp = &u.u_nd;

	ndp->ni_nameiop = DELETE | LOCKPARENT;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->name;
	ip = namei(ndp);
	if (ip == NULL)
		return;
	dp = ndp->ni_pdir;
	/*
	 * No rmdir "." please.
	 */
	if (dp == ip) {
		irele(dp);
		iput(ip);
		u.u_error = EINVAL;
		return;
	}
	if ((ip->i_mode&IFMT) != IFDIR) {
		u.u_error = ENOTDIR;
		goto out;
	}
	/*
	 * Don't remove a mounted on directory.
	 */
	if (ip->i_dev != dp->i_dev) {
		u.u_error = EBUSY;
		goto out;
	}
	/*
	 * Verify the directory is empty (and valid).
	 * (Rmdir ".." won't be valid since
	 *  ".." will contain a reference to
	 *  the current directory and thus be
	 *  non-empty.)
	 */
	if (ip->i_nlink != 2 || !dirempty(ip, dp->i_number)) {
		u.u_error = ENOTEMPTY;
		goto out;
	}
	/*
	 * Delete reference to directory before purging
	 * inode.  If we crash in between, the directory
	 * will be reattached to lost+found,
	 */
	if (dirremove(ndp) == 0)
		goto out;
	dp->i_nlink--;
	dp->i_flag |= ICHG;
	cacheinval(dp);
	iput(dp);
	dp = NULL;
	/*
	 * Truncate inode.  The only stuff left
	 * in the directory is "." and "..".  The
	 * "." reference is inconsequential since
	 * we're quashing it.  The ".." reference
	 * has already been adjusted above.  We've
	 * removed the "." reference and the reference
	 * in the parent directory, but there may be
	 * other hard links so decrement by 2 and
	 * worry about them later.
	 */
	ip->i_nlink -= 2;
	itrunc(ip, (u_long)0);
	cacheinval(ip);
out:
	if (dp)
		iput(dp);
	iput(ip);
}

struct file *
getinode(fdes)
	int fdes;
{
	struct file *fp;

	if ((unsigned)fdes >= NOFILE || (fp = u.u_ofile[fdes]) == NULL) {
		u.u_error = EBADF;
		return ((struct file *)0);
	}
	if (fp->f_type != DTYPE_INODE) {
		u.u_error = EINVAL;
		return ((struct file *)0);
	}
	return (fp);
}

/*
 * mode mask for creation of files
 */
umask()
{
	register struct a {
		int	mask;
	} *uap = (struct a *)u.u_ap;

	u.u_r.r_val1 = u.u_cmask;
	u.u_cmask = uap->mask & 07777;
}
