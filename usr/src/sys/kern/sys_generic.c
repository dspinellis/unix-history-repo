/*	sys_generic.c	5.29	82/12/28	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/ioctl.h"
#include "../h/tty.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/proc.h"
#include "../h/conf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/fs.h"
#ifdef MUSH
#include "../h/quota.h"
#include "../h/share.h"
#else
#define	CHARGE(nothing)
#endif
#include "../h/descrip.h"
#include "../h/uio.h"
#include "../h/cmap.h"

/*
 * Read system call.
 */
read()
{
	register struct a {
		int	fdes;
		char	*cbuf;
		unsigned count;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov;

	aiov.iov_base = (caddr_t)uap->cbuf;
	aiov.iov_len = uap->count;
	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	rwuio(&auio, UIO_READ);
}

readv()
{
	register struct a {
		int	fdes;
		struct	iovec *iovp;
		int	iovcnt;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov[16];		/* XXX */

	if (uap->iovcnt <= 0 || uap->iovcnt > sizeof(aiov)/sizeof(aiov[0])) {
		u.u_error = EINVAL;
		return;
	}
	auio.uio_iov = aiov;
	auio.uio_iovcnt = uap->iovcnt;
	u.u_error = copyin((caddr_t)uap->iovp, (caddr_t)aiov,
	    (unsigned)(uap->iovcnt * sizeof (struct iovec)));
	if (u.u_error)
		return;
	rwuio(&auio, UIO_READ);
}

/*
 * Write system call
 */
write()
{
	register struct a {
		int	fdes;
		char	*cbuf;
		int	count;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov;

	auio.uio_iov = &aiov;
	auio.uio_iovcnt = 1;
	aiov.iov_base = uap->cbuf;
	aiov.iov_len = uap->count;
	rwuio(&auio, UIO_WRITE);
}

writev()
{
	register struct a {
		int	fdes;
		struct	iovec *iovp;
		int	iovcnt;
	} *uap = (struct a *)u.u_ap;
	struct uio auio;
	struct iovec aiov[16];		/* XXX */

	if (uap->iovcnt <= 0 || uap->iovcnt > sizeof(aiov)/sizeof(aiov[0])) {
		u.u_error = EINVAL;
		return;
	}
	auio.uio_iov = aiov;
	auio.uio_iovcnt = uap->iovcnt;
	u.u_error = copyin((caddr_t)uap->iovp, (caddr_t)aiov,
	    (unsigned)(uap->iovcnt * sizeof (struct iovec)));
	if (u.u_error)
		return;
	rwuio(&auio, UIO_WRITE);
}

rwuio(uio, rw)
	register struct uio *uio;
	enum uio_rw rw;
{
	struct a {
		int	fdes;
	};
	register struct file *fp;
	register struct iovec *iov;
	register struct inode *ip;
	int i, count;

	GETF(fp, ((struct a *)u.u_ap)->fdes);
	if ((fp->f_flag&(rw==UIO_READ ? FREAD : FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	uio->uio_resid = 0;
	uio->uio_segflg = 0;
	iov = uio->uio_iov;
	for (i = 0; i < uio->uio_iovcnt; i++) {
		if (iov->iov_len < 0) {
			u.u_error = EINVAL;
			return;
		}
		uio->uio_resid += iov->iov_len;
		if (uio->uio_resid < 0) {
			u.u_error = EINVAL;
			return;
		}
	}
	count = uio->uio_resid;
	if ((u.u_procp->p_flag&SNUSIG) && setjmp(&u.u_qsave)) {
		if (uio->uio_resid == count)
			u.u_eosys = RESTARTSYS;
	} else if (fp->f_type == DTYPE_SOCKET) {
		int sosend(), soreceive();
		u.u_error = 
		    (*(rw==UIO_READ?soreceive:sosend))
		      (fp->f_socket, (struct sockaddr *)0, uio, 0);
	} else {
		ip = fp->f_inode;
		uio->uio_offset = fp->f_offset;
		if ((ip->i_mode&IFMT) == IFREG) {
			ILOCK(ip);
			u.u_error = rwip(ip, uio, rw);
			IUNLOCK(ip);
		} else
			u.u_error = rwip(ip, uio, rw);
		fp->f_offset += count - uio->uio_resid;
	}
	u.u_r.r_val1 = count - uio->uio_resid;
}

rdwri(rw, ip, base, len, offset, segflg, aresid)
	struct inode *ip;
	caddr_t base;
	int len, offset, segflg;
	int *aresid;
	enum uio_rw rw;
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
	error = rwip(ip, &auio, rw);
	if (aresid)
		*aresid = auio.uio_resid;
	else
		if (auio.uio_resid)
			error = EIO;
	return (error);
}

rwip(ip, uio, rw)
	register struct inode *ip;
	register struct uio *uio;
	enum uio_rw rw;
{
	dev_t dev = (dev_t)ip->i_rdev;
	struct buf *bp;
	struct fs *fs;
	daddr_t lbn, bn;
	register int n, on, type;
	int size;
	long bsize;
	extern int mem_no;
	int error = 0;

	if (rw != UIO_READ && rw != UIO_WRITE)
		panic("rwip");
	if (rw == UIO_READ && uio->uio_resid == 0)
		return (0);
	if (uio->uio_offset < 0 &&
	    ((ip->i_mode&IFMT) != IFCHR || mem_no != major(dev)))
		return (EINVAL);
	if (rw == UIO_READ)
		ip->i_flag |= IACC;
	type = ip->i_mode&IFMT;
	if (type == IFCHR) {
#ifdef QUOTA
		register c = uio->uio_resid;
#endif
		if (rw == UIO_READ)
			u.u_error = (*cdevsw[major(dev)].d_read)(dev, uio);
		else {
			ip->i_flag |= IUPD|ICHG;
			u.u_error = (*cdevsw[major(dev)].d_write)(dev, uio);
		}
		CHARGE(sc_tio * (c - uio->uio_resid));
		return (u.u_error);
	}
	if (uio->uio_resid == 0)
		return (0);
	if (rw == UIO_WRITE && type == IFREG &&
	    uio->uio_offset + uio->uio_resid >
	      u.u_rlimit[RLIMIT_FSIZE].rlim_cur) {
		psignal(u.u_procp, SIGXFSZ);
		return (EMFILE);
	}
	if (type != IFBLK) {
		dev = ip->i_dev;
		fs = ip->i_fs;
		bsize = fs->fs_bsize;
	} else
		bsize = BLKDEV_IOSIZE;
	do {
		lbn = uio->uio_offset / bsize;
		on = uio->uio_offset % bsize;
		n = MIN((unsigned)(bsize - on), uio->uio_resid);
		if (type != IFBLK) {
			if (rw == UIO_READ) {
				int diff = ip->i_size - uio->uio_offset;
				if (diff <= 0)
					return (0);
				if (diff < n)
					n = diff;
			}
			bn = fsbtodb(fs,
			    bmap(ip, lbn, rw == UIO_WRITE ? B_WRITE: B_READ, (int)(on+n)));
			if (u.u_error || rw == UIO_WRITE && (long)bn<0)
				return (u.u_error);
			if (rw == UIO_WRITE && uio->uio_offset + n > ip->i_size &&
			   (type == IFDIR || type == IFREG || type == IFLNK))
				ip->i_size = uio->uio_offset + n;
			size = blksize(fs, ip, lbn);
		} else {
			bn = lbn * (BLKDEV_IOSIZE/DEV_BSIZE);
			rablock = bn + (BLKDEV_IOSIZE/DEV_BSIZE);
			rasize = size = bsize;
		}
		if (rw == UIO_READ) {
			if ((long)bn<0) {
				bp = geteblk(size);
				clrbuf(bp);
			} else if (ip->i_lastr + 1 == lbn)
				bp = breada(dev, bn, size, rablock, rasize);
			else
				bp = bread(dev, bn, size);
			ip->i_lastr = lbn;
		} else {
			int i, count;
			extern struct cmap *mfind();

			count = howmany(size, DEV_BSIZE);
			for (i = 0; i < count; i += CLSIZE)
				if (mfind(dev, bn + i))
					munhash(dev, bn + i);
			if (n == bsize) 
				bp = getblk(dev, bn, size);
			else
				bp = bread(dev, bn, size);
		}
		n = MIN(n, size - bp->b_resid);
		if (bp->b_flags & B_ERROR) {
			error = EIO;
			brelse(bp);
			goto bad;
		}
		u.u_error =
		    uiomove(bp->b_un.b_addr+on, n, rw, uio);
		if (rw == UIO_READ) {
			if (n + on == bsize || uio->uio_offset == ip->i_size)
				bp->b_flags |= B_AGE;
			brelse(bp);
		} else {
			if ((ip->i_mode&IFMT) == IFDIR)
				bwrite(bp);
			else if (n + on == bsize) {
				bp->b_flags |= B_AGE;
				bawrite(bp);
			} else
				bdwrite(bp);
			ip->i_flag |= IUPD|ICHG;
			if (u.u_ruid != 0)
				ip->i_mode &= ~(ISUID|ISGID);
		}
	} while (u.u_error == 0 && uio->uio_resid > 0 && n != 0);
bad:
	return (error);
}

uiomove(cp, n, rw, uio)
	register caddr_t cp;
	register int n;
	enum uio_rw rw;
	register struct uio *uio;
{
	register struct iovec *iov;
	u_int cnt;
	int error = 0;

	while (n > 0 && uio->uio_resid) {
		iov = uio->uio_iov;
		cnt = iov->iov_len;
		if (cnt == 0) {
			uio->uio_iov++;
			uio->uio_iovcnt--;
			continue;
		}
		if (cnt > n)
			cnt = n;
		switch (uio->uio_segflg) {

		case 0:
		case 2:
			if (rw == UIO_READ)
				error = copyout(cp, iov->iov_base, cnt);
			else
				error = copyin(iov->iov_base, cp, cnt);
			if (error)
				return (error);
			break;

		case 1:
			if (rw == UIO_READ)
				bcopy((caddr_t)cp, iov->iov_base, cnt);
			else
				bcopy(iov->iov_base, (caddr_t)cp, cnt);
			break;
		}
		iov->iov_base += cnt;
		iov->iov_len -= cnt;
		uio->uio_resid -= cnt;
		uio->uio_offset += cnt;
		cp += cnt;
		n -= cnt;
	}
	return (error);
}

/*
 * Give next character to user as result of read.
 */
ureadc(c, uio)
	register int c;
	register struct uio *uio;
{
	register struct iovec *iov;

again:
	if (uio->uio_iovcnt == 0)
		panic("ureadc");
	iov = uio->uio_iov;
	if (iov->iov_len <= 0 || uio->uio_resid <= 0) {
		uio->uio_iovcnt--;
		uio->uio_iov++;
		goto again;
	}
	switch (uio->uio_segflg) {

	case 0:
		if (subyte(iov->iov_base, c) < 0)
			return (EFAULT);
		break;

	case 1:
		*iov->iov_base = c;
		break;

	case 2:
		if (suibyte(iov->iov_base, c) < 0)
			return (EFAULT);
		break;
	}
	iov->iov_base++;
	iov->iov_len--;
	uio->uio_resid--;
	uio->uio_offset++;
	return (0);
}

#ifdef notdef
/*
 * Get next character written in by user from uio.
 */
uwritec(uio)
	struct uio *uio;
{
	register struct iovec *iov;
	register int c;

again:
	if (uio->uio_iovcnt <= 0 || uio->uio_resid <= 0)
		panic("uwritec");
	iov = uio->uio_iov;
	if (iov->iov_len == 0) {
		uio->uio_iovcnt--;
		uio->uio_iov++;
		goto again;
	}
	switch (uio->uio_segflg) {

	case 0:
		c = fubyte(iov->iov_base);
		break;

	case 1:
		c = *iov->iov_base & 0377;
		break;

	case 2:
		c = fuibyte(iov->iov_base);
		break;
	}
	if (c < 0)
		return (-1);
	iov->iov_base++;
	iov->iov_len--;
	uio->uio_resid--;
	uio->uio_offset++;
	return (c & 0377);
}
#endif

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
	register int com;
	register u_int size;
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
		u.u_pofile[uap->fdes] |= UF_EXCLOSE;
		return;
	}
	if (com == FIONCLEX) {
		u.u_pofile[uap->fdes] &= ~UF_EXCLOSE;
		return;
	}

	/*
	 * Interpret high order word to find
	 * amount of data to be copied to/from the
	 * user's address space.
	 */
	size = (com &~ (IOC_INOUT|IOC_VOID)) >> 16;
	if (size > sizeof (data)) {
		u.u_error = EFAULT;
		return;
	}
	if (com&IOC_IN && size) {
		u.u_error = copyin(uap->cmarg, (caddr_t)data, (u_int)size);
		if (u.u_error)
			return;
	} else
		*(caddr_t *)data = uap->cmarg;
	/*
	 * Zero the buffer on the stack so the user
	 * always gets back something deterministic.
	 */
	if ((com&IOC_OUT) && size)
		bzero((caddr_t)data, size);

	if (fp->f_type == DTYPE_SOCKET)
		u.u_error = soioctl(fp->f_socket, com, data);
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
		if ((u.u_procp->p_flag&SNUSIG) && setjmp(&u.u_qsave)) {
			u.u_eosys = RESTARTSYS;
			return;
		}
		u.u_error = (*cdevsw[major(dev)].d_ioctl)(dev, com, data, 0);
	}

returndata:
	/*
	 * Copy any data to user, size was
	 * already set and checked above.
	 */
	if (u.u_error == 0 && (com&IOC_OUT) && size)
		u.u_error = copyout(data, uap->cmarg, (u_int)size);
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
	return (-1);
}

ostty()
{

}

ogtty()
{

}
