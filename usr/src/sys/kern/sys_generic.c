/*	sys_generic.c	5.10	82/08/14	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/proc.h"
#include "../h/inline.h"
#include "../h/conf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/cmap.h"
#include "../h/vlimit.h"
#include "../h/fs.h"
#ifdef MUSH
#include "../h/quota.h"
#include "../h/share.h"
#else
#define	CHARGE(nothing)
#endif
#include "../h/descrip.h"
#include "../h/uio.h"

/*
 * Read system call.
 */
read()
{
	register struct file *fp;
	register struct inode *ip;
	register struct a {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap;
	struct uio auio;
	struct iovec aiov;

	uap = (struct a *)u.u_ap;
	if ((int)uap->count < 0) {
		u.u_error = EINVAL;
		return;
	}
	GETF(fp, uap->fdes);
	if ((fp->f_flag&FREAD) == 0) {
		u.u_error = EBADF;
		return;
	}
	aiov.iov_base = (caddr_t)uap->cbuf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	auio.uio_segflg = 0;
	auio.uio_resid = uap->count;
	u.u_base = (caddr_t)0xc0000000;
	u.u_count = 0x40000000;
	if ((u.u_procp->p_flag&SNUSIG) && setjmp(u.u_qsav)) {
		if (auio.uio_resid == uap->count)
			u.u_eosys = RESTARTSYS;
	} else if (fp->f_type == DTYPE_SOCKET)
		u.u_error = soreceive(fp->f_socket, (struct sockaddr *)0, &auio);
	else {
		ip = fp->f_inode;
		auio.uio_offset = fp->f_offset;
		if ((ip->i_mode&IFMT) == IFREG) {
			ilock(ip);
			u.u_error = readip(ip, &auio);
			iunlock(ip);
		} else
			u.u_error = readip(ip, &auio);
		fp->f_offset += uap->count - auio.uio_resid;
	}
	u.u_r.r_val1 = uap->count - auio.uio_resid;
}

/*
 * Write system call
 */
write()
{
	register struct file *fp;
	register struct inode *ip;
	register struct a {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap;

	uap = (struct a *)u.u_ap;
	if ((int)uap->count < 0) {
		u.u_error = EINVAL;
		return;
	}
	GETF(fp, uap->fdes);
	if ((fp->f_flag&FWRITE) == 0) {
		u.u_error = EBADF;
		return;
	}
	u.u_base = (caddr_t)uap->cbuf;
	u.u_count = uap->count;
	u.u_segflg = 0;
	if ((u.u_procp->p_flag&SNUSIG) && setjmp(u.u_qsav)) {
		if (u.u_count == uap->count)
			u.u_eosys = RESTARTSYS;
	} else if (fp->f_type == DTYPE_SOCKET)
		u.u_error = sosend(fp->f_socket, (struct sockaddr *)0);
	else {
		ip = fp->f_inode;
		if (fp->f_flag&FAPPEND)
			fp->f_offset = ip->i_size;
		u.u_offset = fp->f_offset;
		if ((ip->i_mode&IFMT) == IFREG) {
			ilock(ip);
			writei(ip);
			iunlock(ip);
		} else
			writei(ip);
		fp->f_offset += uap->count - u.u_count;
	}
	u.u_r.r_val1 = uap->count - u.u_count;
}

readv()
{

}

writev()
{

}

/*
 * Ioctl system call
 * Check legality, execute common code,
 * and switch out to individual device routine.
 */
ioctl()
{
	register struct file *fp;
	struct a {
		int	fdes;
		int	cmd;
		caddr_t	cmarg;
	} *uap;
	register int com, size;
	char data[IOCPARM_MASK+1];

	uap = (struct a *)u.u_ap;
	if ((fp = getf(uap->fdes)) == NULL)
		return;
	if ((fp->f_flag & (FREAD|FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	com = uap->cmd;

#ifndef NOCOMPAT
	/*
	 * Map old style ioctl's into new for the
	 * sake of backwards compatibility (sigh).
	 */
	if ((com&~0xffff) == 0) {
		com = mapioctl(com);
		if (com == 0) {
			u.u_error = EINVAL;
			return;
		}
	}
#endif
	if (com == FIOCLEX) {
		u.u_pofile[uap->fdes] |= EXCLOSE;
		return;
	}
	if (com == FIONCLEX) {
		u.u_pofile[uap->fdes] &= ~EXCLOSE;
		return;
	}

	/*
	 * Interpret high order word to find
	 * amount of data to be copied to/from the
	 * user's address space.
	 * (this'll have to change if we have in+out ioctls)
	 */
	size = (com &~ (IOC_INOUT|IOC_VOID)) >> 16;
	if (size > sizeof (data)) {
		u.u_error = EFAULT;
		return;
	}
	if (com&IOC_IN && size) {
		if (copyin(uap->cmarg, (caddr_t)data, size)) {
			u.u_error = EFAULT;
			return;
		}
	} else
		*(caddr_t *)data = uap->cmarg;
	/*
	 * Zero the buffer on the stack so the user
	 * always gets back something deterministic.
	 */
	if ((com&IOC_OUT) && size)
		bzero((caddr_t)data, size);

	if (fp->f_type == DTYPE_SOCKET)
		soioctl(fp->f_socket, com, data);
	else {
		register struct inode *ip = fp->f_inode;
		int fmt = ip->i_mode & IFMT;
		dev_t dev;

		if (fmt != IFCHR) {
			if (com == FIONREAD && (fmt == IFREG || fmt == IFDIR)) {
				*(off_t *)data = ip->i_size - fp->f_offset;
				goto returndata;
			}
			if (com != FIONBIO && com != FIOASYNC)
				u.u_error = ENOTTY;
			return;
		}
		dev = ip->i_rdev;
		u.u_r.r_val1 = 0;
		if ((u.u_procp->p_flag&SNUSIG) && setjmp(u.u_qsav)) {
			u.u_eosys = RESTARTSYS;
			return;
		}
		(*cdevsw[major(dev)].d_ioctl)(dev, com, data, 0);
	}

returndata:
	/*
	 * Copy any data to user, size was
	 * already set and checked above.
	 */
	if (u.u_error == 0 && com&IOC_OUT)
		if (size && copyout(data, uap->cmarg, size))
			u.u_error = EFAULT;
}

/*
 * Do nothing specific version of line
 * discipline specific ioctl command.
 */
/*ARGSUSED*/
nullioctl(tp, cmd, data, flags)
	struct tty *tp;
	char *data;
	int flags;
{

#ifdef lint
	tp = tp; data = data; flags = flags;
#endif
	return (cmd);
}

/*
 * Write the file corresponding to
 * the inode pointed at by the argument.
 * The actual write arguments are found
 * in the variables:
 *	u_base		core address for source
 *	u_offset	byte offset in file
 *	u_count		number of bytes to write
 *	u_segflg	write to kernel/user/user I
 */
writei(ip)
	register struct inode *ip;
{
	struct buf *bp;
	register struct fs *fs;
	dev_t dev;
	daddr_t lbn, bn;
	register int on, type;
	register unsigned n;
	long bsize;
	int size, i, count;
	extern int mem_no;

	dev = (dev_t)ip->i_rdev;
	if (u.u_offset < 0 && ((ip->i_mode&IFMT) != IFCHR ||
	    mem_no != major(dev)) ) {
		u.u_error = EINVAL;
		return;
	}
	type = ip->i_mode & IFMT;
	if (type == IFCHR) {
		ip->i_flag |= IUPD|ICHG;
		CHARGE(sc_tio * u.u_count);
		(*cdevsw[major(dev)].d_write)(dev);
		return;
	}
	if (u.u_count == 0)
		return;
	if ((ip->i_mode & IFMT) == IFREG &&
	    u.u_offset + u.u_count > u.u_limit[LIM_FSIZE]) {
		psignal(u.u_procp, SIGXFSZ);
		u.u_error = EMFILE;
		return;
	}
	if (type!=IFBLK) {
		dev = ip->i_dev;
		fs = ip->i_fs;
		bsize = fs->fs_bsize;
	} else
		bsize = BLKDEV_IOSIZE;
	do {
		lbn = u.u_offset / bsize;
		on = u.u_offset % bsize;
		n = MIN((unsigned)(bsize - on), u.u_count);
		if (type != IFBLK) {
			bn = fsbtodb(fs, bmap(ip, lbn, B_WRITE, (int)(on + n)));
			if (u.u_error || (long)bn<0)
				return;
			if(u.u_offset + n > ip->i_size &&
			   (type == IFDIR || type == IFREG || type == IFLNK))
				ip->i_size = u.u_offset + n;
			size = blksize(fs, ip, lbn);
		} else {
			size = bsize;
			bn = lbn * (BLKDEV_IOSIZE/DEV_BSIZE);
		}
		count = howmany(size, DEV_BSIZE);
		for (i = 0; i < count; i += CLSIZE)
			if (mfind(dev, bn + i))
				munhash(dev, bn + i);
		if (n == bsize) 
			bp = getblk(dev, bn, size);
		else
			bp = bread(dev, bn, size);
		if (u.u_segflg != 1) {
			if (copyin(u.u_base, bp->b_un.b_addr + on, n)) {
				u.u_error = EFAULT;
				goto bad;
			}
		} else
			bcopy(u.u_base, bp->b_un.b_addr + on, n);
		u.u_base += n;
		u.u_offset += n;
		u.u_count -= n;
bad:
		;
		if (u.u_error != 0)
			brelse(bp);
		else {
			if ((ip->i_mode&IFMT) == IFDIR)
				/*
				 * Writing to clear a directory entry.
				 * Must insure the write occurs before
				 * the inode is freed, or may end up
				 * pointing at a new (different) file
				 * if inode is quickly allocated again
				 * and system crashes.
				 */
				bwrite(bp);
			else if (n + on == bsize) {
				bp->b_flags |= B_AGE;
				bawrite(bp);
			} else
				bdwrite(bp);
		}
		ip->i_flag |= IUPD|ICHG;
		if (u.u_ruid != 0)
			ip->i_mode &= ~(ISUID|ISGID);
	} while (u.u_error == 0 && u.u_count != 0);
}

/*
 * Move n bytes at byte location
 * &bp->b_un.b_addr[o] to/from (flag) the
 * user/kernel (u.segflg) area starting at u.base.
 * Update all the arguments by the number
 * of bytes moved.
 */
iomove(cp, n, flag)
	register caddr_t cp;
	register unsigned n;
{
	register int t;

	if (n==0)
		return;
	if (u.u_segflg != 1) {
		if (flag==B_WRITE)
			t = copyin(u.u_base, (caddr_t)cp, n);
		else
			t = copyout((caddr_t)cp, u.u_base, n);
		if (t) {
			u.u_error = EFAULT;
			return;
		}
	} else
		if (flag == B_WRITE)
			bcopy(u.u_base, (caddr_t)cp, n);
		else
			bcopy((caddr_t)cp, u.u_base, n);
	u.u_base += n;
	u.u_offset += n;
	u.u_count -= n;
}

readip1(ip, base, len, offset, segflg, aresid)
	struct inode *ip;
	caddr_t base;
	int len, offset, segflg;
	int *aresid;
{
	struct uio auio;
	struct iovec aiov;
	int error;

	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = base;
	aiov.iov_len = len;
	auio.uio_resid = len;
	auio.uio_offset = offset;
	auio.uio_segflg = segflg;
	error = readip(ip, &auio);
	if (aresid)
		*aresid = auio.uio_resid;
	else
		if (auio.uio_resid)
			error = EIO;
	return (error);
}

readip(ip, uio)
	register struct inode *ip;
	register struct uio *uio;
{
	register struct iovec *iov;
	struct buf *bp;
	struct fs *fs;
	dev_t dev;
	daddr_t lbn, bn;
	off_t diff;
	register int on, type;
	register unsigned n;
	int size;
	long bsize;
	extern int mem_no;
	int error = 0;

	dev = (dev_t)ip->i_rdev;
	if (uio->uio_offset < 0 &&
	    ((ip->i_mode&IFMT) != IFCHR || mem_no != major(dev)))
		return (EINVAL);
	ip->i_flag |= IACC;
	type = ip->i_mode&IFMT;
	if (type == IFCHR) {
		register c = u.u_count;
		(*cdevsw[major(dev)].d_read)(dev, uio);
		CHARGE(sc_tio * (c - uio->uio_resid));
		return (u.u_error);
	}
	if (type != IFBLK) {
		dev = ip->i_dev;
		fs = ip->i_fs;
		bsize = fs->fs_bsize;
	} else
		bsize = BLKDEV_IOSIZE;
nextiov:
	if (uio->uio_iovcnt == 0)
		return (0);
	iov = uio->uio_iov;
	if (iov->iov_len > 0)
	do {
		lbn = uio->uio_offset / bsize;
		on = uio->uio_offset % bsize;
		n = MIN((unsigned)(bsize - on), iov->iov_len);
		if (type != IFBLK) {
			diff = ip->i_size - uio->uio_offset;
			if (diff <= 0)
				return;
			if (diff < n)
				n = diff;
			bn = fsbtodb(fs, bmap(ip, lbn, B_READ));
			if (u.u_error)
				return (u.u_error);
			size = blksize(fs, ip, lbn);
		} else {
			size = bsize;
			bn = lbn * (BLKDEV_IOSIZE/DEV_BSIZE);
			rablock = bn + (BLKDEV_IOSIZE/DEV_BSIZE);
			rasize = bsize;
		}
		if ((long)bn<0) {
			bp = geteblk(size);
			clrbuf(bp);
		} else if (ip->i_lastr + 1 == lbn)
			bp = breada(dev, bn, size, rablock, rasize);
		else
			bp = bread(dev, bn, size);
		ip->i_lastr = lbn;
		n = MIN(n, size - bp->b_resid);
		if (n != 0) {
			if (uio->uio_segflg != 1) {
				if (copyout(bp->b_un.b_addr+on, iov->iov_base, n)) {
					brelse(bp);
					return (EFAULT);
				}
			} else
				bcopy(bp->b_un.b_addr + on, iov->iov_base, n);
			iov->iov_base += n;
			uio->uio_offset += n;
			iov->iov_len -= n;
			uio->uio_resid -= n;
		}
		if (n + on == bsize || uio->uio_offset == ip->i_size)
			bp->b_flags |= B_AGE;
		brelse(bp);
	} while (u.u_error == 0 && iov->iov_len != 0 && n != 0);
	if (u.u_error == 0 && iov->iov_len == 0) {
		uio->uio_iov++;
		uio->uio_iovcnt--;
		goto nextiov;
	}
	return (error);
}

uiomove(cp, n, dir, uio)
	register caddr_t cp;
	register unsigned n;
	enum uio_direction dir;
	struct uio *uio;
{
	register struct iovec *iov;
	int bad, cnt;

	if (n == 0)
		return (0);
	if (uio->uio_segflg != 1) {
		if (dir == UIO_READFROM)
#ifdef notdef
			bad = copyuin(uio, (caddr_t)cp, n);
#else
			panic("uiomove");
#endif
		else
			bad = copyuout((caddr_t)cp, n, uio);
		if (bad)
			return (EFAULT);
	} else {
		while (n > 0 && uio->uio_iovcnt) {
			iov = uio->uio_iov;
			cnt = iov->iov_len;
			if (cnt > n)
				cnt = n;
			if (dir == UIO_READFROM)
				bcopy(iov->iov_base, (caddr_t)cp, cnt);
			else
				bcopy((caddr_t)cp, iov->iov_base, cnt);
			iov->iov_base += cnt;
			iov->iov_len -= cnt;
			uio->uio_resid -= cnt;
			n -= cnt;
		}
	}
	return (0);
}
