/*	ffs_vnops.c	4.48	83/01/11	*/

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
#include "../h/descrip.h"
#include "../h/uio.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/nami.h"

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
	register struct inode *ip;
	register struct a {
		char	*fname;
		int	flags;
		int	mode;
	} *uap;
	int checkpermissions = 1, flags;

	uap = (struct a *)u.u_ap;
	flags = uap->flags + 1;
	if ((flags&FTRUNCATE) && (flags&FWRITE) == 0) {
		u.u_error = EINVAL;
		return;
	}
	if (flags&FCREATE) {
		ip = namei(uchar, CREATE, 1);
		if (ip == NULL) {
			if (u.u_error)
				return;
			ip = maknode(uap->mode&07777&(~ISVTX));
			checkpermissions = 0;
			flags &= ~FTRUNCATE;
		}
	} else
		ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return;
	open1(ip, flags, checkpermissions);
}

#ifndef NOCOMPAT
/*
 * Creat system call.
 */
ocreat()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		int	fmode;
	} *uap;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, CREATE, 1);
	if (ip == NULL) {
		if (u.u_error)
			return;
		ip = maknode(uap->fmode&07777&(~ISVTX));
		if (ip == NULL)
			return;
		open1(ip, FWRITE, 0);
	} else
		open1(ip, FWRITE|FTRUNCATE, 1);
}
#endif

/*
 * Common code for open and creat.
 * Check permissions (if we haven't done so already),
 * allocate an open file structure, and call
 * the device open routine, if any.
 */
open1(ip, mode, checkpermissions)
	register struct inode *ip;
	register mode;
{
	register struct file *fp;
	int i, flags;

	if (checkpermissions) {
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

	/*
	 * Check locking on inode.  Release "inode lock"
	 * while doing so in case we block inside flocki.
	 */
	flags = 0;
	if (mode&(FSHLOCK|FEXLOCK)) {
		iunlock(ip);
		flags = flocki(ip, 0, mode);
		ilock(ip);
		if (u.u_error)
			goto bad;
	}
	if (mode&FTRUNCATE)
		itrunc(ip, (u_long)0);
	iunlock(ip);
	if ((fp = falloc()) == NULL)
		goto out;
	fp->f_flag = mode & FMODES;
	fp->f_type = DTYPE_FILE;
	i = u.u_r.r_val1;
	fp->f_inode = ip;
	u.u_error = openi(ip, mode);
	if (u.u_error == 0) {
		u.u_pofile[i] = flags;
		return;
	}
	u.u_ofile[i] = NULL;
	fp->f_count--;
out:
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
	if (suser()) {
		ip = namei(uchar, CREATE, 0);
		if (ip != NULL) {
			u.u_error = EEXIST;
			goto out;
		}
	}
	if (u.u_error)
		return;
	ip = maknode(uap->fmode);
	if (ip == NULL)
		return;
	if (uap->dev) {
		/*
		 * Want to be able to use this to make badblock
		 * inodes, so don't truncate the dev number.
		 */
		ip->i_rdev = uap->dev;
		ip->i_flag |= IACC|IUPD|ICHG;
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
	direnter(ip);
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
	fp = getf(uap->fd);
	if (fp == NULL)
		return;
	if (fp->f_type == DTYPE_SOCKET) {
		u.u_error = ESPIPE;
		return;
	}
	if (uap->sbase == FSEEK_RELATIVE)
		uap->off += fp->f_offset;
	else if (uap->sbase == FSEEK_EOF)
		uap->off += fp->f_inode->i_size;
	fp->f_offset = uap->off;
	u.u_r.r_off = uap->off;
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
		if ((uap->fmode&FACCESS_READ) && access(ip, IREAD))
			goto done;
		if ((uap->fmode&FACCESS_WRITE) && access(ip, IWRITE))
			goto done;
		if ((uap->fmode&FACCESS_EXECUTE) && access(ip, IEXEC))
			goto done;
done:
		iput(ip);
	}
	u.u_uid = svuid;
	u.u_gid = svgid;
}

/*
 * the fstat system call.
 */
fstat()
{
	register struct file *fp;
	register struct a {
		int	fd;
		struct stat *sb;
	} *uap;

	uap = (struct a *)u.u_ap;
	fp = getf(uap->fd);
	if (fp == NULL)
		return;
	if (fp->f_type == DTYPE_SOCKET)
		u.u_error = sostat(fp->f_socket, uap->sb);
	else
		stat1(fp->f_inode, uap->sb);
}

/*
 * Stat system call.  This version follows links.
 */
stat()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		struct stat *sb;
	} *uap;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return;
	stat1(ip, uap->sb);
	iput(ip);
}

/*
 * Lstat system call.  This version does not follow links.
 */
lstat()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		struct stat *sb;
	} *uap;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, LOOKUP, 0);
	if (ip == NULL)
		return;
	stat1(ip, uap->sb);
	iput(ip);
}

/*
 * The basic routine for fstat and stat:
 * get the inode and pass appropriate parts back.
 */
stat1(ip, ub)
	register struct inode *ip;
	struct stat *ub;
{
	struct stat ds;

	IUPDAT(ip, &time, &time, 0);
	/*
	 * Copy from inode table
	 */
	ds.st_dev = ip->i_dev;
	ds.st_ino = ip->i_number;
	ds.st_mode = ip->i_mode;
	ds.st_nlink = ip->i_nlink;
	ds.st_uid = ip->i_uid;
	ds.st_gid = ip->i_gid;
	ds.st_rdev = (dev_t)ip->i_rdev;
	ds.st_size = ip->i_size;
	ds.st_atime = ip->i_atime;
	ds.st_spare1 = 0;
	ds.st_mtime = ip->i_mtime;
	ds.st_spare2 = 0;
	ds.st_ctime = ip->i_ctime;
	ds.st_spare3 = 0;
	/* this doesn't belong here */
	if ((ip->i_mode&IFMT) == IFBLK)
		ds.st_blksize = BLKDEV_IOSIZE;
	else if ((ip->i_mode&IFMT) == IFCHR)
		ds.st_blksize = MAXBSIZE;
	else
		ds.st_blksize = ip->i_fs->fs_bsize;
	ds.st_spare4[0] = ds.st_spare4[1] = ds.st_spare4[2] = 0;
	u.u_error = copyout((caddr_t)&ds, (caddr_t)ub, sizeof(ds));
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
	fp = getf(uap->fd);
	if (fp == NULL)
		return;
	if (fp->f_type == DTYPE_SOCKET) {
		u.u_error = EINVAL;
		return;
	}
	ip = fp->f_inode;
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
	register int *gp;

	ip->i_mode &= ~07777;
	if (u.u_uid) {
		mode &= ~ISVTX;
		for (gp = u.u_groups; gp < &u.u_groups[NGROUPS]; gp++)
			if (*gp == ip->i_gid)
				goto ok;
		mode &= ~ISGID;
ok:
		;
#ifdef MUSH
		if (u.u_quota->q_syflags & QF_UMASK && u.u_uid != 0 &&
		    (ip->i_mode & IFMT) != IFCHR)
			mode &= ~u.u_cmask;
#endif
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
	chown1(ip, uap->uid, uap->gid);
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
	fp = getf(uap->fd);
	if (fp == NULL)
		return;
	if (fp->f_type == DTYPE_SOCKET) {
		u.u_error = EINVAL;
		return;
	}
	ip = fp->f_inode;
	if (!suser())
		return;
	ilock(ip);
	chown1(ip, uap->uid, uap->gid);
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

	/*
	 * This doesn't allow for holes in files (which hopefully don't
	 * happen often in files that we chown), and is not accurate anyway
	 * (eg: it totally ignores 3 level indir blk files - but hopefully
	 * noone who can make a file that big will have a quota)
	 */
	if (ip->i_uid == uid)
		change = 0;
	else {
		register struct fs *fs = ip->i_fs;

		if (ip->i_size > (change = NDADDR * fs->fs_bsize)) {
			register off_t size;

			size = blkroundup(fs, ip->i_size) - change;
			change += size;
			change += fs->fs_bsize;
			/* this assumes NIADDR <= 2 */
			if (size > NINDIR(fs) * fs->fs_bsize)
				change += fs->fs_bsize;
		} else
			change = fragroundup(fs, ip->i_size);
		change /= DEV_BSIZE;
	}
	(void)chkdq(ip, -change, 1);
	(void)chkiq(ip->i_dev, ip, ip->i_uid, 1);
	dqrele(ip->i_dquot);
#endif
	/*
	 * keep uid/gid's in sane range -- no err,
	 * so chown(file, uid, -1) will do something useful
	 */
	if (uid >= 0 && uid <= 32767)	/* should have a constant */
		ip->i_uid = uid;
	if (gid >= 0 && gid <= 32767)	/* same here */
		ip->i_gid = gid;
	ip->i_flag |= ICHG;
	if (u.u_ruid != 0)
		ip->i_mode &= ~(ISUID|ISGID);
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
	(void)chkdq(ip, change, 1);
	(void)chkiq(ip->i_dev, (struct inode *)NULL, uid, 1);
#endif
}

/*
 * Set IUPD and IACC times on file.
 * Can't set ICHG.
 */
outime()
{
	register struct a {
		char	*fname;
		time_t	*tptr;
	} *uap;
	register struct inode *ip;
	time_t tv[2];
	struct timeval tv0, tv1;

	uap = (struct a *)u.u_ap;
	if ((ip = owner(1)) == NULL)
		return;
	u.u_error = copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof(tv));
	if (u.u_error == 0) {
		ip->i_flag |= IACC|IUPD|ICHG;
		tv0.tv_sec = tv[0]; tv0.tv_usec = 0;
		tv1.tv_sec = tv[1]; tv1.tv_usec = 0;
		iupdat(ip, &tv0, &tv1, 0);
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
 * Apply an advisory lock on a file descriptor.
 */
flock()
{
	struct a {
		int	fd;
		int	how;
	} *uap;
	register struct file *fp;
	register int cmd, flags;

	uap = (struct a *)u.u_ap;
	fp = getf(uap->fd);
	if (fp == NULL)
		return;
	if (fp->f_type == DTYPE_SOCKET) {		/* XXX */
		u.u_error = EINVAL;
		return;
	}
	cmd = uap->how;
	flags = u.u_pofile[uap->fd] & (UF_SHLOCK|UF_EXLOCK);
	if (cmd&FUNLOCK) {
		if (flags == 0) {
			u.u_error = EINVAL;
			return;
		}
		funlocki(fp->f_inode, flags);
		u.u_pofile[uap->fd] &= ~(UF_SHLOCK|UF_EXLOCK);
		return;
	}
	/*
	 * No reason to write lock a file we've already
	 * write locked, similarly with a read lock.
	 */
	if ((flags&UF_EXLOCK) && (cmd&FEXLOCK) ||
	    (flags&UF_SHLOCK) && (cmd&FSHLOCK))
		return;
	u.u_pofile[uap->fd] = flocki(fp->f_inode, u.u_pofile[uap->fd], cmd);
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

	fp = getf(uap->fd);
	if (fp == NULL)
		return;
	if (fp->f_type == DTYPE_SOCKET) {
		u.u_error = EINVAL;
		return;
	}
	if ((fp->f_flag&FWRITE) == 0) {
		u.u_error = EINVAL;
		return;
	}
	ip = fp->f_inode;
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

	fp = getf(uap->fd);
	if (fp == NULL)
		return;
	if (fp->f_type == DTYPE_SOCKET) {
		u.u_error = EINVAL;
		return;
	}
	ip = fp->f_inode;
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
	ip = namei(uchar, LOOKUP | LOCKPARENT, 0);
	if (ip == NULL)
		return;
	dp = u.u_pdir;
	oldparent = 0, doingdirectory = 0;
	if ((ip->i_mode&IFMT) == IFDIR) {
		register struct direct *d;

		d = &u.u_dent;
		/*
		 * Avoid "." and ".." for obvious reasons.
		 */
		if (d->d_name[0] == '.') {
			if (d->d_namlen == 1 ||
			    (d->d_namlen == 2 && d->d_name[1] == '.')) {
				iput(ip);
				u.u_error = EINVAL;
				return;
			}
		}
		oldparent = dp->i_number;
		doingdirectory++;
	}
	irele(dp);

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
	 * 2) If target doesn't exist, link the target
	 *    to the source and unlink the source. 
	 *    Otherwise, rewrite the target directory
	 *    entry to reference the source inode and
	 *    expunge the original entry's existence.
	 */
	parentdifferent = oldparent != dp->i_number;
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
		direnter(ip);
		if (u.u_error) {
			error = u.u_error;
			goto out;
		}
	} else {
		if (xp->i_dev != dp->i_dev || xp->i_dev != ip->i_dev) {
			error = EXDEV;
			goto bad;
		}
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
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
#endif

	/*
	 * Make sure inode goes to disk before directory entry.
	 */
	iupdat(ip, &time, &time, 1);
	direnter(ip);
	if (u.u_error) {
		/*
		 * write error occurred trying to update directory
		 * so must deallocate the inode
		 */
		ip->i_nlink = 0;
		ip->i_flag |= ICHG;
		iput(ip);
		return (NULL);
	}
	return (ip);
}
