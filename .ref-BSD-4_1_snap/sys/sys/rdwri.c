/*	rdwri.c	4.6	81/03/08	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/cmap.h"
#include "../h/vlimit.h"
#include "../h/proc.h"

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
	dev_t dev;
	daddr_t lbn, bn;
	off_t diff;
	register int on, type;
	register unsigned n;
	extern int mem_no;

	if(u.u_count == 0)
		return;
	dev = (dev_t)ip->i_un.i_rdev;
	if (u.u_offset < 0 && ((ip->i_mode&IFMT) != IFCHR || mem_no != major(dev))) {
		u.u_error = EINVAL;
		return;
	}
	ip->i_flag |= IACC;
	type = ip->i_mode&IFMT;
	if (type==IFCHR || type==IFMPC) {
		(*cdevsw[major(dev)].d_read)(dev);
		return;
	}

	do {
		lbn = bn = u.u_offset >> BSHIFT;
		on = u.u_offset & BMASK;
		n = MIN((unsigned)(BSIZE-on), u.u_count);
		if (type!=IFBLK && type!=IFMPB) {
			diff = ip->i_size - u.u_offset;
			if (diff <= 0)
				return;
			if (diff < n)
				n = diff;
			bn = bmap(ip, bn, B_READ);
			if (u.u_error)
				return;
			dev = ip->i_dev;
		} else
			rablock = bn+1;
		if ((long)bn<0) {
			bp = geteblk();
			clrbuf(bp);
		} else if (ip->i_un.i_lastr+1==lbn)
			bp = breada(dev, bn, rablock);
		else
			bp = bread(dev, bn);
		ip->i_un.i_lastr = lbn;
		n = MIN(n, BSIZE-bp->b_resid);
		if (n!=0) {
#ifdef UNFAST
			iomove(bp->b_un.b_addr+on, n, B_READ);
#else
			if (u.u_segflg != 1) {
				if (copyout(bp->b_un.b_addr+on, u.u_base, n)) {
					u.u_error = EFAULT;
					goto bad;
				}
			} else
				bcopy(bp->b_un.b_addr+on, u.u_base, n);
			u.u_base += n;
			u.u_offset += n;
			u.u_count -= n;
bad:
			;
#endif
		}
		if (n+on==BSIZE || u.u_offset==ip->i_size) {
			if (ip->i_flag&IPIPE)
				bp->b_flags &= ~B_DELWRI;	/* cancel i/o */
			bp->b_flags |= B_AGE;
		}
		brelse(bp);
	} while(u.u_error==0 && u.u_count!=0 && n!=0);
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
	dev_t dev;
	daddr_t bn;
	register int on, type;
	register unsigned n;
	extern int mem_no;

	dev = (dev_t)ip->i_un.i_rdev;
	if(u.u_offset < 0 && ((ip->i_mode&IFMT) != IFCHR || mem_no != major(dev)) ) {
		u.u_error = EINVAL;
		return;
	}
	type = ip->i_mode&IFMT;
	if (type==IFCHR || type==IFMPC) {
		ip->i_flag |= IUPD|ICHG;
		(*cdevsw[major(dev)].d_write)(dev);
		return;
	}
	if (u.u_count == 0)
		return;
	if ((ip->i_mode&IFMT)==IFREG &&
	    u.u_offset + u.u_count > u.u_limit[LIM_FSIZE]) {
		psignal(u.u_procp, SIGXFSZ);
		u.u_error = EMFILE;
		return;
	}
	do {
		bn = u.u_offset >> BSHIFT;
		on = u.u_offset & BMASK;
		n = MIN((unsigned)(BSIZE-on), u.u_count);
		if (type!=IFBLK && type!=IFMPB) {
			bn = bmap(ip, bn, B_WRITE);
			if((long)bn<0)
				return;
			dev = ip->i_dev;
		}
		if (bn && mfind(dev, bn))
			munhash(dev, bn);
		if(n == BSIZE) 
			bp = getblk(dev, bn);
		else
			bp = bread(dev, bn);
#ifdef UNFAST
		iomove(bp->b_un.b_addr+on, n, B_WRITE);
#else
		if (u.u_segflg != 1) {
			if (copyin(u.u_base, bp->b_un.b_addr+on, n)) {
				u.u_error = EFAULT;
				goto bad;
			}
		} else
			bcopy(u.u_base, bp->b_un.b_addr+on, n);
		u.u_base += n;
		u.u_offset += n;
		u.u_count -= n;
bad:
		;
#endif
		if(u.u_error != 0)
			brelse(bp);
		else {
			if ((ip->i_mode&IFMT) == IFDIR &&
			    ((struct direct *)(bp->b_un.b_addr+on))->d_ino == 0)
				/*
				 * Writing to clear a directory entry.
				 * Must insure the write occurs before
				 * the inode is freed, or may end up
				 * pointing at a new (different) file
				 * if inode is quickly allocated again
				 * and system crashes.
				 */
				bwrite(bp);
			else if (n+on==BSIZE && (ip->i_flag&IPIPE) == 0) {
				bp->b_flags |= B_AGE;
				bawrite(bp);
			} else
				bdwrite(bp);
		}
		if(u.u_offset > ip->i_size &&
		   (type==IFDIR || type==IFREG))
			ip->i_size = u.u_offset;
		ip->i_flag |= IUPD|ICHG;
#ifndef INSECURE
		ip->i_mode &= ~(ISUID|ISGID);
#endif
	} while(u.u_error==0 && u.u_count!=0);
}

/*
 * Return the logical maximum
 * of the 2 arguments.
 */
unsigned
max(a, b)
unsigned a, b;
{

	if(a > b)
		return(a);
	return(b);
}

/*
 * Return the logical minimum
 * of the 2 arguments.
 */
unsigned
min(a, b)
unsigned a, b;
{

	if(a < b)
		return(a);
	return(b);
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
