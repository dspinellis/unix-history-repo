/*	subr_xxx.c	4.10	82/04/19	*/

/* merged into kernel:	@(#)subr.c 2.2 4/8/82 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/conf.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/proc.h"
#include "../h/fs.h"

/*
 * Bmap defines the structure of file system storage
 * by returning the physical block number on a device given the
 * inode and the logical block number in a file.
 * When convenient, it also leaves the physical
 * block number of the next block of the file in rablock
 * for use in read-ahead.
 */
/*VARARGS3*/
daddr_t
bmap(ip, bn, rwflg, size)
	register struct inode *ip;
	daddr_t bn;
	int rwflg;
	int size;	/* supplied only when rwflg == B_WRITE */
{
	register int i;
	int osize, nsize;
	struct buf *bp, *nbp;
	struct fs *fs;
	int j, sh;
	daddr_t nb, *bap, pref, blkpref();

	if (bn < 0) {
		u.u_error = EFBIG;
		return ((daddr_t)0);
	}
	fs = ip->i_fs;
	rablock = 0;

	/*
	 * If the next write will extend the file into a new block,
	 * and the file is currently composed of a fragment
	 * this fragment has to be extended to be a full block.
	 */
	nb = lblkno(fs, ip->i_size);
	if (rwflg == B_WRITE && nb < NDADDR && nb < bn) {
		osize = blksize(fs, ip, nb);
		if (osize < fs->fs_bsize && osize > 0) {
			bp = realloccg(ip, ip->i_db[nb],
				nb == 0 ? 0 : ip->i_db[nb - 1] + fs->fs_frag,
				osize, fs->fs_bsize);
			ip->i_size = (nb + 1) * fs->fs_bsize;
			ip->i_db[nb] = dbtofsb(fs, bp->b_blkno);
			ip->i_flag |= IUPD|ICHG;
			bdwrite(bp);
		}
	}
	/*
	 * The first NDADDR blocks are direct blocks
	 */
	if (bn < NDADDR) {
		i = bn;
		nb = ip->i_db[i];
		if (rwflg == B_READ) {
			if (nb == 0)
				return ((daddr_t)-1);
			goto gotit;
		}
		if (nb == 0 || ip->i_size < (i + 1) * fs->fs_bsize) {
			if (nb != 0) {
				/* consider need to reallocate a frag */
				osize = fragroundup(fs, blkoff(fs, ip->i_size));
				nsize = fragroundup(fs, size);
				if (nsize <= osize)
					goto gotit;
				bp = realloccg(ip, nb, i == 0 ?
					0 : ip->i_db[i - 1] + fs->fs_frag,
					osize, nsize);
			} else {
				if (ip->i_size < (i + 1) * fs->fs_bsize)
					nsize = fragroundup(fs, size);
				else
					nsize = fs->fs_bsize;
				bp = alloc(ip, i > 0 ?
					ip->i_db[i - 1] + fs->fs_frag : 0,
					nsize);
			}
			if (bp == NULL)
				return ((daddr_t)-1);
			nb = dbtofsb(fs, bp->b_blkno);
			if ((ip->i_mode&IFMT) == IFDIR)
				/*
				 * Write directory blocks synchronously
				 * so they never appear with garbage in
				 * them on the disk.
				 */
				bwrite(bp);
			else
				bdwrite(bp);
			ip->i_db[i] = nb;
			ip->i_flag |= IUPD|ICHG;
		}
gotit:
		if (i < NDADDR - 1)
			rablock = ip->i_db[i+1];
		return (nb);
	}

	/*
	 * Determine how many levels of indirection.
	 */
	sh = 1;
	bn -= NDADDR;
	for (j = NIADDR; j>0; j--) {
		sh *= NINDIR(fs);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0) {
		u.u_error = EFBIG;
		return ((daddr_t)0);
	}

	/*
	 * fetch the first indirect block
	 */
	nb = ip->i_ib[NIADDR - j];
	if (nb == 0) {
		if (rwflg==B_READ ||
		    (bp = alloc(ip, (daddr_t)0, fs->fs_bsize)) == NULL)
			return ((daddr_t)-1);
		nb = dbtofsb(fs, bp->b_blkno);
		/*
		 * Write synchronously so that indirect blocks
		 * never point at garbage.
		 */
		bwrite(bp);
		ip->i_ib[NIADDR - j] = nb;
		ip->i_flag |= IUPD|ICHG;
	}

	/*
	 * fetch through the indirect blocks
	 */
	for (; j <= NIADDR; j++) {
		bp = bread(ip->i_dev, fsbtodb(fs, nb), fs->fs_bsize);
		if (bp->b_flags & B_ERROR) {
			brelse(bp);
			return ((daddr_t)0);
		}
		bap = bp->b_un.b_daddr;
		sh /= NINDIR(fs);
		i = (bn / sh) % NINDIR(fs);
		nb = bap[i];
		if (nb == 0) {
			if (rwflg==B_READ) {
				brelse(bp);
				return ((daddr_t)-1);
			}
			if (i % (fs->fs_fsize / sizeof(daddr_t)) == 0 ||
			    bap[i - 1] == 0)
				pref = blkpref(ip->i_fs);
			else
				pref = bap[i - 1] + fs->fs_frag;
		        nbp = alloc(ip, pref, fs->fs_bsize);
			if (nbp == NULL) {
				brelse(bp);
				return ((daddr_t)-1);
			}
			nb = dbtofsb(fs, nbp->b_blkno);
			if (j < NIADDR || (ip->i_mode&IFMT) == IFDIR)
				/*
				 * Write synchronously so indirect blocks
				 * never point at garbage and blocks
				 * in directories never contain garbage.
				 */
				bwrite(nbp);
			else
				bdwrite(nbp);
			bap[i] = nb;
			bdwrite(bp);
		} else
			brelse(bp);
	}

	/*
	 * calculate read-ahead.
	 */
	if (i < NINDIR(fs) - 1)
		rablock = bap[i+1];
	return (nb);
}

/*
 * Pass back  c  to the user at his location u_base;
 * update u_base, u_count, and u_offset.  Return -1
 * on the last character of the user's read.
 * u_base is in the user address space unless u_segflg is set.
 */
passc(c)
register c;
{
	register id;

	if ((id = u.u_segflg) == 1)
		*u.u_base = c;
	else
		if (id?suibyte(u.u_base, c):subyte(u.u_base, c) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
	u.u_count--;
	u.u_offset++;
	u.u_base++;
	return (u.u_count == 0? -1: 0);
}

#include "ct.h"
#if NCT > 0
/*
 * Pick up and return the next character from the user's
 * write call at location u_base;
 * update u_base, u_count, and u_offset.  Return -1
 * when u_count is exhausted.  u_base is in the user's
 * address space unless u_segflg is set.
 */
cpass()
{
	register c, id;

	if (u.u_count == 0)
		return (-1);
	if ((id = u.u_segflg) == 1)
		c = *u.u_base;
	else
		if ((c = id==0?fubyte(u.u_base):fuibyte(u.u_base)) < 0) {
			u.u_error = EFAULT;
			return (-1);
		}
	u.u_count--;
	u.u_offset++;
	u.u_base++;
	return (c&0377);
}
#endif

/*
 * Routine which sets a user error; placed in
 * illegal entries in the bdevsw and cdevsw tables.
 */
nodev()
{

	u.u_error = ENODEV;
}

/*
 * Null routine; placed in insignificant entries
 * in the bdevsw and cdevsw tables.
 */
nulldev()
{

}

imin(a, b)
{

	return (a < b ? a : b);
}

imax(a, b)
{

	return (a > b ? a : b);
}

unsigned
min(a, b)
	unsigned int a, b;
{

	return (a < b ? a : b);
}

unsigned
max(a, b)
	unsigned int a, b;
{

	return (a > b ? a : b);
}

struct proc *
pfind(pid)
	int pid;
{
	register struct proc *p;

	for (p = &proc[pidhash[PIDHASH(pid)]]; p != &proc[0]; p = &proc[p->p_idhash])
		if (p->p_pid == pid)
			return (p);
	return ((struct proc *)0);
}
