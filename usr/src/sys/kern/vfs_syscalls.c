/*	vfs_syscalls.c	4.24	82/06/04	*/

#ifdef SIMFS
#include "../h/sysrenam.h"
#endif
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/file.h"
#include "../h/stat.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/buf.h"
#include "../h/proc.h"
#include "../h/inline.h"

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
	irele(ip);
	if (*ipp) {
		ilock(*ipp);
		iput(*ipp);
	}
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
		int	rwmode;
	} *uap;

	uap = (struct a *)u.u_ap;
	ip = namei(uchar, 0, 1);
	if (ip == NULL)
		return;
	open1(ip, ++uap->rwmode, 0);
}

/*
 * Creat system call.
 */
creat()
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
		if (ip==NULL)
			return;
		open1(ip, FWRITE, 2);
	} else
		open1(ip, FWRITE, 1);
}

/*
 * Common code for open and creat.
 * Check permissions, allocate an open file structure,
 * and call the device open routine if any.
 */
open1(ip, mode, trf)
	register struct inode *ip;
	register mode;
{
	register struct file *fp;
	int i;

	if (trf != 2) {
		if (mode&FREAD)
			(void) access(ip, IREAD);
		if (mode&FWRITE) {
			(void) access(ip, IWRITE);
			if ((ip->i_mode&IFMT) == IFDIR)
				u.u_error = EISDIR;
		}
	}
	if (u.u_error)
		goto out;
	if (trf == 1)
		itrunc(ip);
	irele(ip);
	if ((fp = falloc()) == NULL)
		goto out;
	fp->f_flag = mode&(FREAD|FWRITE);
	i = u.u_r.r_val1;
	fp->f_inode = ip;
	openi(ip, mode&(FREAD|FWRITE));
	if (u.u_error == 0)
		return;
	u.u_ofile[i] = NULL;
	fp->f_count--;
out:
	if (ip != NULL)
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
	if ((ip->i_mode&IFMT)==IFDIR && !suser())
		goto out1;
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	iupdat(ip, &time, &time, 1);
	irele(ip);
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
	wdir(ip);
out:
	if (u.u_error) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
out1:
	iput(ip);
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
	u.u_base = uap->target;
	u.u_count = nc;
	u.u_offset = 0;
	u.u_segflg = 0;
	writei(ip);
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
	struct fs *fs;
	struct buf *bp;
	int lbn, bn, base;

	pp = namei(uchar, 2, 0);
	if(pp == NULL)
		return;
	/*
	 * Check for unlink(".")
	 * to avoid hanging on the iget
	 */
	if (pp->i_number == u.u_dent.d_ino) {
		ip = pp;
		ip->i_count++;
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
/*
	if ((ip->i_flag&ITEXT) && ip->i_nlink==1) {
 		u.u_error = ETXTBSY;
		goto out;
	}
*/
	if (u.u_count == 0) {
		/*
		 * first entry in block, so set d_ino to zero.
		 */
		u.u_base = (caddr_t)&u.u_dent;
		u.u_count = DIRSIZ(&u.u_dent);
		u.u_dent.d_ino = 0;
		writei(pp);
	} else {
		/*
		 * updating preceeding entry to skip over current entry.
		 */
		fs = pp->i_fs;
		lbn = lblkno(fs, u.u_offset);
		base = blkoff(fs, u.u_offset);
		bn = fsbtodb(fs, bmap(pp, lbn, B_WRITE, base + u.u_count));
		bp = bread(pp->i_dev, bn, blksize(fs, pp, lbn));
		if (bp->b_flags & B_ERROR) {
			brelse(bp);
			goto out;
		}
		((struct direct *)(bp->b_un.b_addr + base))->d_reclen +=
		    u.u_dent.d_reclen;
		bwrite(bp);
		pp->i_flag |= IUPD|ICHG;
	}
	ip->i_nlink--;
	ip->i_flag |= ICHG;

out:
	iput(ip);
out1:
	iput(pp);
}

/*
 * Seek system call
 */
seek()
{
	register struct file *fp;
	register struct a {
		int	fdes;
		off_t	off;
		int	sbase;
	} *uap;

	uap = (struct a *)u.u_ap;
	fp = getf(uap->fdes);
	if (fp == NULL)
		return;
	if (fp->f_flag&FSOCKET) {
		u.u_error = ESPIPE;
		return;
	}
	if (uap->sbase == 1)
		uap->off += fp->f_offset;
	else if (uap->sbase == 2)
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
		if (uap->fmode&(IREAD>>6))
			(void) access(ip, IREAD);
		if (uap->fmode&(IWRITE>>6))
			(void) access(ip, IWRITE);
		if (uap->fmode&(IEXEC>>6))
			(void) access(ip, IEXEC);
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
		int	fdes;
		struct stat *sb;
	} *uap;

	uap = (struct a *)u.u_ap;
	fp = getf(uap->fdes);
	if (fp == NULL)
		return;
	if (fp->f_flag & FSOCKET)
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
	ds.st_mtime = ip->i_mtime;
	ds.st_ctime = ip->i_ctime;
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
	} *uap;

	ip = namei(uchar, 0, 0);
	if (ip == NULL)
		return;
	if ((ip->i_mode&IFMT) != IFLNK) {
		u.u_error = ENXIO;
		goto out;
	}
	uap = (struct a *)u.u_ap;
	u.u_offset = 0;
	u.u_base = uap->buf;
	u.u_count = uap->count;
	u.u_segflg = 0;
	readi(ip);
out:
	iput(ip);
	u.u_r.r_val1 = uap->count - u.u_count;
}

chmod()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		int	fmode;
	} *uap;

	uap = (struct a *)u.u_ap;
	if ((ip = owner(1)) == NULL)
		return;
	ip->i_mode &= ~07777;
	if (u.u_uid)
		uap->fmode &= ~ISVTX;
	ip->i_mode |= uap->fmode&07777;
	ip->i_flag |= ICHG;
	if (ip->i_flag&ITEXT && (ip->i_mode&ISVTX)==0)
		xrele(ip);
	iput(ip);
}

chown()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
		int	uid;
		int	gid;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (!suser() || (ip = owner(0)) == NULL)
		return;
	ip->i_uid = uap->uid;
	ip->i_gid = uap->gid;
	ip->i_flag |= ICHG;
	if (u.u_ruid != 0)
		ip->i_mode &= ~(ISUID|ISGID);
	iput(ip);
}

/*
 * Set IUPD and IACC times on file.
 * Can't set ICHG.
 */
utime()
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
