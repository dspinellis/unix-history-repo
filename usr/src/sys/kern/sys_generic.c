/*	sys_generic.c	5.2	82/07/22	*/

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
	u.u_base = (caddr_t)uap->cbuf;
	u.u_count = uap->count;
	u.u_segflg = 0;
	if ((u.u_procp->p_flag&SNUSIG) && setjmp(u.u_qsav)) {
		if (u.u_count == uap->count)
			u.u_eosys = RESTARTSYS;
	} else if (fp->f_flag & FSOCKET)
		u.u_error = soreceive(fp->f_socket, (struct sockaddr *)0);
	else {
		ip = fp->f_inode;
		u.u_offset = fp->f_offset;
		if ((ip->i_mode&IFMT) == IFREG) {
			ilock(ip);
			readi(ip);
			iunlock(ip);
		} else
			readi(ip);
		fp->f_offset += uap->count - u.u_count;
	}
	u.u_r.r_val1 = uap->count - u.u_count;
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
	} else if (fp->f_flag & FSOCKET)
		u.u_error = sosend(fp->f_socket, (struct sockaddr *)0);
	else {
		ip = fp->f_inode;
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


/*
 * Ioctl system call
 * Check legality, execute common code, and switch out to individual
 * device routine.
 */
ioctl()
{
	register struct file *fp;
	register struct inode *ip;
	register struct a {
		int	fdes;
		int	cmd;
		caddr_t	cmarg;
	} *uap;
	register dev_t dev;
	register fmt;

	uap = (struct a *)u.u_ap;
	if ((fp = getf(uap->fdes)) == NULL)
		return;
	if ((fp->f_flag & (FREAD|FWRITE)) == 0) {
		u.u_error = EBADF;
		return;
	}
	if (uap->cmd==FIOCLEX) {
		u.u_pofile[uap->fdes] |= EXCLOSE;
		return;
	}
	if (uap->cmd==FIONCLEX) {
		u.u_pofile[uap->fdes] &= ~EXCLOSE;
		return;
	}
	if (fp->f_flag & FSOCKET) {
		soioctl(fp->f_socket, uap->cmd, uap->cmarg);
		return;
	}
	ip = fp->f_inode;
	fmt = ip->i_mode & IFMT;
	if (fmt != IFCHR) {
		if (uap->cmd==FIONREAD && (fmt == IFREG || fmt == IFDIR)) {
			off_t nread = ip->i_size - fp->f_offset;

			if (copyout((caddr_t)&nread, uap->cmarg, sizeof(off_t)))
				u.u_error = EFAULT;
		} else if (uap->cmd == FIONBIO || uap->cmd == FIOASYNC)
			return;
		else
			u.u_error = ENOTTY;
		return;
	}
	dev = ip->i_rdev;
	u.u_r.r_val1 = 0;
	if ((u.u_procp->p_flag&SNUSIG) && setjmp(u.u_qsav)) {
		u.u_eosys = RESTARTSYS;
		return;
	}
	(*cdevsw[major(dev)].d_ioctl)(dev, uap->cmd, uap->cmarg, 0);
}

/*
 * Do nothing specific version of line
 * discipline specific ioctl command.
 */
/*ARGSUSED*/
nullioctl(tp, cmd, addr)
	struct tty *tp;
	caddr_t addr;
{

	return (cmd);
}

/*
 * Read the file corresponding to
 * the inode pointed at by the argument.
 * The actual read arguments are found
 * in the variables:
 *	u_base		core address for destination
 *	u_offset	byte offset in file
 *	u_count		number of bytes to read
 *	u_segflg	read to kernel/user/user I
 */
readi(ip)
	register struct inode *ip;
{
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

	if (u.u_count == 0)
		return;
	dev = (dev_t)ip->i_rdev;
	if (u.u_offset < 0 && ((ip->i_mode&IFMT) != IFCHR ||
	    mem_no != major(dev))) {
		u.u_error = EINVAL;
		return;
	}
	ip->i_flag |= IACC;
	type = ip->i_mode&IFMT;
	if (type == IFCHR) {
		register c = u.u_count;
		(*cdevsw[major(dev)].d_read)(dev);
		CHARGE(sc_tio * (c - u.u_count));
		return;
	}
	if (type != IFBLK) {
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
			diff = ip->i_size - u.u_offset;
			if (diff <= 0)
				return;
			if (diff < n)
				n = diff;
			bn = fsbtodb(fs, bmap(ip, lbn, B_READ));
			if (u.u_error)
				return;
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
#ifdef UNFAST
			iomove(bp->b_un.b_addr + on, n, B_READ);
#else
			if (u.u_segflg != 1) {
				if (copyout(bp->b_un.b_addr+on, u.u_base, n)) {
					u.u_error = EFAULT;
					goto bad;
				}
			} else
				bcopy(bp->b_un.b_addr + on, u.u_base, n);
			u.u_base += n;
			u.u_offset += n;
			u.u_count -= n;
bad:
			;
#endif
		}
		if (n + on == bsize || u.u_offset == ip->i_size)
			bp->b_flags |= B_AGE;
		brelse(bp);
	} while (u.u_error == 0 && u.u_count != 0 && n != 0);
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
	} else {
		bsize = BLKDEV_IOSIZE;
	}
	do {
		lbn = u.u_offset / bsize;
		on = u.u_offset % bsize;
		n = MIN((unsigned)(bsize - on), u.u_count);
		if (type != IFBLK) {
			bn = fsbtodb(fs, bmap(ip, lbn, B_WRITE, (int)(on + n)));
			if ((long)bn<0)
				return;
			if(u.u_offset + n > ip->i_size &&
			   (type == IFDIR || type == IFREG || type == IFLNK))
				ip->i_size = u.u_offset + n;
			size = blksize(fs, ip, lbn);
		} else {
			size = bsize;
			bn = lbn * (BLKDEV_IOSIZE/DEV_BSIZE);
		}
		if (bn) {
			count = howmany(size, DEV_BSIZE);
			for (i = 0; i < count; i += CLSIZE) {
				if (mfind(dev, bn + i))
					munhash(dev, bn + i);
			}
		}
		if(n == bsize) 
			bp = getblk(dev, bn, size);
		else
			bp = bread(dev, bn, size);
#ifdef UNFAST
		iomove(bp->b_un.b_addr + on, n, B_WRITE);
#else
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
#endif
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
