/*	lfs_vnops.c	6.2	83/09/25	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/file.h"
#include "../h/stat.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/buf.h"
#include "../h/proc.h"
#include "../h/quota.h"
#include "../h/uio.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/nami.h"
#include "../h/mount.h"

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
	};

	ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT) != IFDIR) {
		u.u_error = ENOTDIR;
		goto bad;
	}
	if (access(ip, IEXEC))
		goto bad;
	iunlock(ip);
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

	copen(uap->mode-FOPEN, uap->crtmode);
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

	copen(FWRITE|FCREAT|FTRUNC, uap->fmode);
}

/*
 * Common code for open and creat.
 * Check permissions, allocate an open file structure,
 * and call the device open routine if any.
 */
copen(mode, arg)
	register int mode;
	int arg;
{
	register struct inode *ip;
	register struct file *fp;
	int i;

#ifdef notdef
	if ((mode&(FREAD|FWRITE)) == 0) {
		u.u_error = EINVAL;
		return;
	}
#endif
	if (mode&FCREAT) {
		ip = namei(uchar, CREATE, 1);
		if (ip == NULL) {
			if (u.u_error)
				return;
			ip = maknode(arg&07777&(~ISVTX));
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
		ip = namei(uchar, LOOKUP, 1);
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
		if (mode&FWRITE) {
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
	iunlock(ip);
	fp->f_flag = mode&FMASK;
	fp->f_type = DTYPE_INODE;
	fp->f_ops = &inodeops;
	fp->f_data = (caddr_t)ip;
	i = u.u_r.r_val1;
#ifdef notdef
	if (setjmp(&u.u_qsave)) {
		if (u.u_error == 0)
			u.u_error = EINTR;
		u.u_ofile[i] = NULL;
		closef(fp);
		return;
	}
#endif
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
	} *uap;

	uap = (struct a *)u.u_ap;
	if (!suser())
		return;
	ip = namei(uchar, CREATE, 0);
	if (ip != NULL) {
		u.u_error = EEXIST;
		goto out;
	}
	if (u.u_error)
		return;
	ip = maknode(uap->fmode);
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
	} *uap;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, LOOKUP, 1); /* well, this routine is doomed anyhow */
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT) == IFDIR && !suser()) {
		iput(ip);
		return;
	}
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	iupdat(ip, &time, &time, 1);
	iunlock(ip);
	u.u_dirp = (caddr_t)uap->linkname;
	xp = namei(uchar, CREATE, 0);
	if (xp != NULL) {
		u.u_error = EEXIST;
		iput(xp);
		goto out;
	}
	if (u.u_error)
		goto out;
	if (u.u_pdir->i_dev != ip->i_dev) {
		iput(u.u_pdir);
		u.u_error = EXDEV;
		goto out;
	}
	u.u_error = direnter(ip);
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
	} *uap;
	register struct inode *ip;
	register char *tp;
	register c, nc;

	uap = (struct a *)u.u_ap;
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
	u.u_dirp = uap->linkname;
	ip = namei(uchar, CREATE, 0);
	if (ip) {
		iput(ip);
		u.u_error = EEXIST;
		return;
	}
	if (u.u_error)
		return;
	ip = maknode(IFLNK | 0777);
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
	};
	register struct inode *ip, *dp;

	ip = namei(uchar, DELETE | LOCKPARENT, 0);
	if (ip == NULL)
		return;
	dp = u.u_pdir;
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
	if (dirremove()) {
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
	} *uap;

	uap = (struct a *)u.u_ap;
	fp = getinode(uap->fd);
	if (fp == NULL)
		return;
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
	} *uap;

	uap = (struct a *)u.u_ap;
	svuid = u.u_uid;
	svgid = u.u_gid;
	u.u_uid = u.u_ruid;
	u.u_gid = u.u_rgid;
	ip = namei(uchar, LOOKUP, 1);
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

	stat1(1);
}

/*
 * Lstat system call.  This version does not follow links.
 */
lstat()
{

	stat1(0);
}

stat1(follow)
	int follow;
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		struct stat *ub;
	} *uap;
	struct stat sb;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, LOOKUP, follow);
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
	int resid;

	ip = namei(uchar, LOOKUP, 0);
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT) != IFLNK) {
		u.u_error = ENXIO;
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
	} *uap;

	uap = (struct a *)u.u_ap;
	if ((ip = owner(1)) == NULL)
		return;
	chmod1(ip, uap->fmode);
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
	} *uap;
	register struct inode *ip;
	register struct file *fp;

	uap = (struct a *)u.u_ap;
	fp = getinode(uap->fd);
	if (fp == NULL)
		return;
	ip = (struct inode *)fp->f_data;
	if (u.u_uid != ip->i_uid && !suser())
		return;
	ilock(ip);
	chmod1(ip, uap->fmode);
	iunlock(ip);
}

/*
 * Change the mode on a file.
 * Inode must be locked before calling.
 */
chmod1(ip, mode)
	register struct inode *ip;
	register int mode;
{

	ip->i_mode &= ~07777;
	if (u.u_uid) {
		mode &= ~ISVTX;
		if (!groupmember(ip->i_gid))
			mode &= ~ISGID;
	}
	ip->i_mode |= mode&07777;
	ip->i_flag |= ICHG;
	if (ip->i_flag&ITEXT && (ip->i_mode&ISVTX)==0)
		xrele(ip);
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
	} *uap;

	uap = (struct a *)u.u_ap;
	if (!suser() || (ip = owner(0)) == NULL)
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
	} *uap;
	register struct inode *ip;
	register struct file *fp;

	uap = (struct a *)u.u_ap;
	fp = getinode(uap->fd);
	if (fp == NULL)
		return;
	ip = (struct inode *)fp->f_data;
	if (!suser())
		return;
	ilock(ip);
	u.u_error = chown1(ip, uap->uid, uap->gid);
	iunlock(ip);
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

	if ((ip = owner(1)) == NULL)
		return;
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

	ip = namei(uchar, LOOKUP, 1);
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
	ilock(ip);
	itrunc(ip, uap->length);
	iunlock(ip);
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
	ilock(ip);
	syncip(ip);
	iunlock(ip);
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
 *    from underneath us while we work.
 * 2) Link source to destination.  If destination already exists,
 *    delete it first.
 * 3) Unlink source reference to inode if still around.
 * 4) If a directory was moved and the parent of the destination
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
	} *uap;
	register struct inode *ip, *xp, *dp;
	int oldparent, parentdifferent, doingdirectory;
	int error = 0;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, DELETE | LOCKPARENT, 0);
	if (ip == NULL)
		return;
	dp = u.u_pdir;
	oldparent = 0, doingdirectory = 0;
	if ((ip->i_mode&IFMT) == IFDIR) {
		register struct direct *d;

		d = &u.u_dent;
		/*
		 * Avoid ".", "..", and aliases of "." for obvious reasons.
		 */
		if ((d->d_namlen == 1 && d->d_name[0] == '.') ||
		    (d->d_namlen == 2 && bcmp(d->d_name, "..", 2) == 0) ||
		    (dp == ip)) {
			iput(dp);
			if (dp == ip)
				irele(ip);
			else
				iput(ip);
			u.u_error = EINVAL;
			return;
		}
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
	iunlock(ip);

	/*
	 * When the target exists, both the directory
	 * and target inodes are returned locked.
	 */
	u.u_dirp = (caddr_t)uap->to;
	xp = namei(uchar, CREATE | LOCKPARENT, 0);
	if (u.u_error) {
		error = u.u_error;
		goto out;
	}
	dp = u.u_pdir;
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
	parentdifferent = oldparent != dp->i_number;
	if (doingdirectory && parentdifferent) {
		if (access(ip, IWRITE))
			goto bad;
		do {
			dp = u.u_pdir;
			if (xp != NULL)
				iput(xp);
			u.u_error = checkpath(ip, dp);
			if (u.u_error)
				goto out;
			u.u_dirp = (caddr_t)uap->to;
			xp = namei(uchar, CREATE | LOCKPARENT, 0);
			if (u.u_error) {
				error = u.u_error;
				goto out;
			}
		} while (dp != u.u_pdir);
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
		 * Account for ".." in directory.
		 * When source and destination have the
		 * same parent we don't fool with the
		 * link count -- this isn't required
		 * because we do a similar check below.
		 */
		if (doingdirectory && parentdifferent) {
			dp->i_nlink++;
			dp->i_flag |= ICHG;
			iupdat(dp, &time, &time, 1);
		}
		error = direnter(ip);
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
			if (!dirempty(xp) || xp->i_nlink > 2) {
				error = ENOTEMPTY;
				goto bad;
			}
			if (!doingdirectory) {
				error = ENOTDIR;
				goto bad;
			}
		} else if (doingdirectory) {
			error = EISDIR;
			goto bad;
		}
		dirrewrite(dp, ip);
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
		 * it above, as we've no way to determine if
		 * we've got a link or the directory itself, and
		 * if we get a link, then ".." will be screwed up.
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
	u.u_dirp = uap->from;
	dp = namei(uchar, DELETE, 0);
	/*
	 * Insure directory entry still exists and
	 * has not changed since the start of all
	 * this.  If either has occured, forget about
	 * about deleting the original entry and just
	 * adjust the link count in the inode.
	 */
	if (dp == NULL || u.u_dent.d_ino != ip->i_number) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	} else {
		/*
		 * If source is a directory, must adjust
		 * link count of parent directory also.
		 * If target didn't exist and source and
		 * target have the same parent, then we
		 * needn't touch the link count, it all
		 * balances out in the end.  Otherwise, we
		 * must do so to reflect deletion of ".."
		 * done above.
		 */
		if (doingdirectory && (xp != NULL || parentdifferent)) {
			dp->i_nlink--;
			dp->i_flag |= ICHG;
		}
		if (dirremove()) {
			ip->i_nlink--;
			ip->i_flag |= ICHG;
		}
		if (error == 0)		/* conservative */
			error = u.u_error;
	}
	irele(ip);
	if (dp)
		iput(dp);

	/*
	 * 4) Renaming a directory with the parent
	 *    different requires ".." to be rewritten.
	 *    The window is still there for ".." to
	 *    be inconsistent, but this is unavoidable,
	 *    and a lot shorter than when it was done
	 *    in a user process.
	 */
	if (doingdirectory && parentdifferent && error == 0) {
		struct dirtemplate dirbuf;

		u.u_dirp = uap->to;
		ip = namei(uchar, LOOKUP | LOCKPARENT, 0);
		if (ip == NULL) {
			printf("rename: .. went away\n");
			return;
		}
		dp = u.u_pdir;
		if ((ip->i_mode&IFMT) != IFDIR) {
			printf("rename: .. not a directory\n");
			goto stuck;
		}
		error = rdwri(UIO_READ, ip, (caddr_t)&dirbuf,
			sizeof (struct dirtemplate), (off_t)0, 1, (int *)0);
		if (error == 0) {
			dirbuf.dotdot_ino = dp->i_number;
			(void) rdwri(UIO_WRITE, ip, (caddr_t)&dirbuf,
			  sizeof (struct dirtemplate), (off_t)0, 1, (int *)0);
		}
stuck:
		irele(dp);
		iput(ip);
	}
	goto done;

bad:
	iput(dp);
bad1:
	if (xp)
		iput(xp);
out:
	ip->i_nlink--;
	ip->i_flag |= ICHG;
	irele(ip);
done:
	if (error)
		u.u_error = error;
}

/*
 * Make a new file.
 */
struct inode *
maknode(mode)
	int mode;
{
	register struct inode *ip;
	ino_t ipref;

	if ((mode & IFMT) == IFDIR)
		ipref = dirpref(u.u_pdir->i_fs);
	else
		ipref = u.u_pdir->i_number;
	ip = ialloc(u.u_pdir, ipref, mode);
	if (ip == NULL) {
		iput(u.u_pdir);
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
	ip->i_gid = u.u_pdir->i_gid;
	if (ip->i_mode & ISGID && !groupmember(ip->i_gid))
		ip->i_mode &= ~ISGID;
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
#endif

	/*
	 * Make sure inode goes to disk before directory entry.
	 */
	iupdat(ip, &time, &time, 1);
	u.u_error = direnter(ip);
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
	} *uap;
	register struct inode *ip, *dp;
	struct dirtemplate dirtemplate;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, CREATE, 0);
	if (u.u_error)
		return;
	if (ip != NULL) {
		iput(ip);
		u.u_error = EEXIST;
		return;
	}
	dp = u.u_pdir;
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
	/*
	 * Directory all set up, now
	 * install the entry for it in
	 * the parent directory.
	 */
	u.u_error = direnter(ip);
	dp = NULL;
	if (u.u_error) {
		u.u_dirp = uap->name;
		dp = namei(uchar, LOOKUP, 0);
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
	};
	register struct inode *ip, *dp;

	ip = namei(uchar, DELETE | LOCKPARENT, 0);
	if (ip == NULL)
		return;
	dp = u.u_pdir;
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
	if (ip->i_nlink != 2 || !dirempty(ip)) {
		u.u_error = ENOTEMPTY;
		goto out;
	}
	/*
	 * Delete reference to directory before purging
	 * inode.  If we crash in between, the directory
	 * will be reattached to lost+found,
	 */
	if (dirremove() == 0)
		goto out;
	dp->i_nlink--;
	dp->i_flag |= ICHG;
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
out:
	if (dp)
		iput(dp);
	iput(ip);
}

struct file *
getinode(fdes)
	int fdes;
{
	register struct file *fp;

	fp = getf(fdes);
	if (fp == 0)
		return (0);
	if (fp->f_type != DTYPE_INODE) {
		u.u_error = EINVAL;
		return (0);
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
	} *uap;

	uap = (struct a *)u.u_ap;
	u.u_r.r_val1 = u.u_cmask;
	u.u_cmask = uap->mask & 07777;
}
