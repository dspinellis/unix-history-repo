/*	vfs_syscalls.c	4.37	82/09/06	*/

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
#include "../h/inline.h"
/* no reason to inline expand these guys here */
#undef ilock
#undef iunlock
#include "../h/quota.h"
#include "../h/descrip.h"
#include "../h/uio.h"
#include "../h/socket.h"

chdir()
{

	chdirec(&u.u_cdir);
}

chroot()
{

	if (suser())
		chdirec(&u.u_rdir);
}

chdirec(ipp)
	register struct inode **ipp;
{
	register struct inode *ip;
	struct a {
		char	*fname;
	};

	ip = namei(uchar, 0, 1);
	if(ip == NULL)
		return;
	if((ip->i_mode&IFMT) != IFDIR) {
		u.u_error = ENOTDIR;
		goto bad;
	}
	if(access(ip, IEXEC))
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
	int checkpermissions = 1;

	uap = (struct a *)u.u_ap;
	if (uap->flags&FCREATE) {
		ip = namei(uchar, 1, 1);
		if (ip == NULL) {
			if (u.u_error)
				return;
			ip = maknode(uap->mode&07777&(~ISVTX));
			checkpermissions = 0;
			uap->flags &= ~FTRUNCATE;
		}
	} else
		ip = namei(uchar, 0, 1);
	if (ip == NULL)
		return;
	open1(ip, ++uap->flags, checkpermissions);
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
	ip = namei(uchar, 1, 1);
	if (ip == NULL) {
		if (u.u_error)
			return;
		ip = maknode(uap->fmode&07777&(~ISVTX));
		if (ip == NULL)
			return;
		open1(ip, FWRITE, 0);
	} else
		open1(ip, FWRITE|FTRUNCATE, 0);
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
	if (mode&(FRDLOCK|FWRLOCK)) {
		iunlock(ip);
		flags = flocki(ip, 0, mode);
		ilock(ip);
		if (u.u_error)
			goto bad;
	}
	if (mode&FTRUNCATE)
		itrunc(ip, 0);
	iunlock(ip);
	if ((fp = falloc()) == NULL)
		goto out;
	fp->f_flag = mode & FMODES;
	fp->f_type = DTYPE_FILE;
	i = u.u_r.r_val1;
	fp->f_inode = ip;
	openi(ip, mode);
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
		ip = namei(uchar, 1, 0);
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
	ip = namei(uchar, 0, 1);    /* well, this routine is doomed anyhow */
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT)==IFDIR && !suser()) {
		iput(ip);
		return;
	}
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	iupdat(ip, &time.tv_sec, &time.tv_sec, 1);
	iunlock(ip);
	u.u_dirp = (caddr_t)uap->linkname;
	xp = namei(uchar, 1, 0);
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
	ip = namei(uchar, 1, 0);
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
	iput(ip);
}

/*
 * Unlink system call.
 * Hard to avoid races here, especially
 * in unlinking directories.
 */
unlink()
{
	register struct inode *ip, *pp;
	struct a {
		char	*fname;
	};
	int unlinkingdot = 0;

	pp = namei(uchar, 2, 0);
	if (pp == NULL)
		return;

	/*
	 * Check for unlink(".")
	 * to avoid hanging on the iget
	 */
	if (pp->i_number == u.u_dent.d_ino) {
		ip = pp;
		ip->i_count++;
		unlinkingdot++;
	} else
		ip = iget(pp->i_dev, pp->i_fs, u.u_dent.d_ino);
	if(ip == NULL)
		goto out1;
	if((ip->i_mode&IFMT)==IFDIR && !suser())
		goto out;
	/*
	 * Don't unlink a mounted file.
	 */
	if (ip->i_dev != pp->i_dev) {
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
	if (unlinkingdot)
		irele(ip);
	else
		iput(ip);
out1:
	iput(pp);
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
	ip = namei(uchar, 0, 1);
	if (ip != NULL) {
		if (uap->fmode&FACCESS_READ && access(ip, IREAD))
			goto done;
		if (uap->fmode&FACCESS_WRITE && access(ip, IWRITE))
			goto done;
		if (uap->fmode&FACCESS_EXECUTE && access(ip, IEXEC))
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
	ip = namei(uchar, 0, 1);
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
	ip = namei(uchar, 0, 0);
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

	IUPDAT(ip, &time.tv_sec, &time.tv_sec, 0);
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
	ds.st_mtime = ip->i_mtime;
	ds.st_ctime = ip->i_ctime;
	/* this doesn't belong here */
	if ((ip->i_mode&IFMT) == IFBLK)
		ds.st_blksize = BLKDEV_IOSIZE;
	else if ((ip->i_mode&IFMT) == IFCHR)
		ds.st_blksize = MAXBSIZE;
	else
		ds.st_blksize = ip->i_fs->fs_bsize;
	if (copyout((caddr_t)&ds, (caddr_t)ub, sizeof(ds)) < 0)
		u.u_error = EFAULT;
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

	ip = namei(uchar, 0, 0);
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
}

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
	ilock(ip);
	if (u.u_uid != ip->i_uid && !suser()) {
		iunlock(ip);
		return;
	}
	chmod1(ip, uap->fmode);
}

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
	iput(ip);
}

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
}

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
	ilock(ip);
	if (!suser()) {
		iunlock(ip);
		return;
	}
	chown1(ip, uap->uid, uap->gid);
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
	chkdq(ip, -change, 1);
	chkiq(ip->i_dev, ip, ip->i_uid, 1);
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
	chkdq(ip, change, 1);
	chkiq(ip->i_dev, NULL, uid, 1);
#endif
	iput(ip);
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

	uap = (struct a *)u.u_ap;
	if ((ip = owner(1)) == NULL)
		return;
	if (copyin((caddr_t)uap->tptr, (caddr_t)tv, sizeof(tv))) {
		u.u_error = EFAULT;
	} else {
		ip->i_flag |= IACC|IUPD|ICHG;
		iupdat(ip, &tv[0], &tv[1], 0);
	}
	iput(ip);
}

sync()
{

	update(0);
}

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
	flags = u.u_pofile[uap->fd] & (RDLOCK|WRLOCK);
	if (cmd&FUNLOCK) {
		if (flags == 0) {
			u.u_error = EINVAL;
			return;
		}
		funlocki(fp->f_inode, flags);
		u.u_pofile[uap->fd] &= ~(RDLOCK|WRLOCK);
		return;
	}
	/*
	 * No reason to write lock a file we've already
	 * write locked, similarly with a read lock.
	 */
	if ((flags&WRLOCK) && (cmd&FWRLOCK) ||
	    (flags&RDLOCK) && (cmd&FRDLOCK))
		return;
	u.u_pofile[uap->fd] = flocki(fp->f_inode, u.u_pofile[uap->fd], cmd);
}

truncate()
{
	struct a {
		char	*fname;
		int	length;
	} *uap = (struct a *)u.u_ap;
	struct inode *ip;

	ip = namei(uchar, 0, 1);
	if (ip == NULL)
		return;
	if (access(ip, IWRITE))
		goto bad;
	if ((ip->i_mode&IFMT) == IFDIR) {
		u.u_error = EISDIR;
		goto bad;
	}
	itrunc(ip, uap->length);
	return;
bad:
	iput(ip);
}

ftruncate()
{
	struct a {
		int	fd;
		int	length;
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
}

rename()
{
#ifdef notdef
	struct a {
		char	*from;
		char	*to;
	} *uap;
#endif

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
	iupdat(ip, &time.tv_sec, &time.tv_sec, 1);
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
