/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)nfs_bio.c	7.2 (Berkeley) %G%
 */

#include "param.h"
#include "user.h"
#include "buf.h"
#include "vnode.h"
#include "trace.h"
#include "mount.h"
#include "nfsnode.h"
#include "nfsiom.h"

/* True and false, how exciting */
#define	TRUE	1
#define	FALSE	0

/*
 * Vnode op for read using bio
 * Any similarity to readip() is purely coincidental
 */
nfs_read(vp, uio, offp, ioflag, cred)
	register struct vnode *vp;
	struct uio *uio;
	off_t *offp;
	int ioflag;
	struct ucred *cred;
{
	register struct nfsnode *np = VTONFS(vp);
	struct buf *bp;
	struct vattr vattr;
	daddr_t lbn, bn, rablock;
	int error = 0;
	int diff;
	long n, on;
	int count;

	if (!(ioflag & IO_NODELOCKED))
		nfs_lock(vp);
	/*
	 * Avoid caching directories. Once everything is using getdirentries()
	 * this will never happen anyhow.
	 */
	if (vp->v_type == VDIR) {
		error = nfs_readrpc(vp, uio, offp, cred);
		if (!(ioflag & IO_NODELOCKED))
			nfs_unlock(vp);
		return (error);
	}
	uio->uio_offset = *offp;
	count = uio->uio_resid;
	if (uio->uio_rw != UIO_READ)
		panic("nfs_read mode");
	if (vp->v_type != VREG)
		panic("nfs_read type");
	if (uio->uio_resid == 0)
		goto out;
	if (uio->uio_offset < 0) {
		error = EINVAL;
		goto out;
	}
	/*
	 * If the file's modify time on the server has changed since the
	 * last read rpc or you have written to the file,
	 * you may have lost data cache consistency with the
	 * server, so flush all of the file's data out of the cache.
	 * This will implicitly bring the modify time up to date, since
	 * up to date attributes are returned in the reply to any write rpc's
	 * NB: This implies that cache data can be read when up to
	 * NFS_ATTRTIMEO seconds out of date. If you find that you need current
	 * attributes this could be forced by setting n_attrstamp to 0 before
	 * the nfs_getattr() call.
	 */
	if (np->n_flag & NMODIFIED) {
		np->n_flag &= ~NMODIFIED;
		if (error = nfs_blkflush(vp, (daddr_t)0, np->n_size, TRUE))
			goto out;
		if (error = nfs_getattr(vp, &vattr, cred))
			goto out;
		np->n_size = vattr.va_size;
		np->n_mtime = vattr.va_mtime.tv_sec;
	} else {
		if (error = nfs_getattr(vp, &vattr, cred))
			goto out;
		if (np->n_mtime != vattr.va_mtime.tv_sec) {
			if (error = nfs_blkflush(vp, (daddr_t)0, np->n_size, TRUE))
				goto out;
			np->n_size = vattr.va_size;
			np->n_mtime = vattr.va_mtime.tv_sec;
		}
	}
	np->n_flag |= NBUFFERED;
	do {
		lbn = uio->uio_offset >> NFS_BIOSHIFT;
		on = uio->uio_offset & (NFS_BIOSIZE-1);
		n = MIN((unsigned)(NFS_BIOSIZE - on), uio->uio_resid);
		diff = np->n_size - uio->uio_offset;
		if (diff <= 0)
			goto out;
		if (diff < n)
			n = diff;
		bn = lbn*(NFS_BIOSIZE/DEV_BSIZE);
		rablock = (lbn+1)*(NFS_BIOSIZE/DEV_BSIZE);
		if (np->n_lastr+1 == lbn)
			error = breada(vp, bn, NFS_BIOSIZE, rablock, NFS_BIOSIZE,
				cred, &bp);
		else
			error = bread(vp, bn, NFS_BIOSIZE, cred, &bp);
		np->n_lastr = lbn;
		if (bp->b_resid) {
			diff = (on >= (NFS_BIOSIZE-bp->b_resid)) ? 0 :
				(NFS_BIOSIZE-bp->b_resid-on);
			n = MIN(n, diff);
		}
		if (error) {
			brelse(bp);
			goto out;
		}
		if (n > 0)
			error = uiomove(bp->b_un.b_addr + on, (int)n, uio);
		if (n+on == NFS_BIOSIZE || uio->uio_offset == np->n_size)
			bp->b_flags |= B_AGE;
		brelse(bp);
	} while (error == 0 && uio->uio_resid > 0 && n != 0);
out:
	*offp = uio->uio_offset;
	if (!(ioflag & IO_NODELOCKED))
		nfs_unlock(vp);
	return (error);
}

/*
 * Vnode op for write using bio
 */
nfs_write(vp, uio, offp, ioflag, cred)
	register struct vnode *vp;
	register struct uio *uio;
	off_t *offp;
	int ioflag;
	struct ucred *cred;
{
	struct buf *bp;
	struct nfsnode *np = VTONFS(vp);
	daddr_t lbn, bn;
	int i, n, on;
	int flags, count, size;
	int error = 0;
	int cnt;
	u_long osize;

	if ((ioflag & IO_NODELOCKED) == 0)
		nfs_lock(vp);
	/* Should we try and do this ?? */
	if (vp->v_type == VREG && (ioflag & IO_APPEND))
		*offp = np->n_size;
	uio->uio_offset = *offp;
	cnt = uio->uio_resid;
#ifdef notdef
	osize = np->n_size;
#endif
	if (uio->uio_rw != UIO_WRITE)
		panic("nfs_write mode");
	if (vp->v_type != VREG)
		panic("nfs_write type");
	if (uio->uio_offset < 0) {
		error = EINVAL;
		goto out;
	}
	if (uio->uio_resid == 0)
		goto out;
	/*
	 * Maybe this should be above the vnode op call, but so long as
	 * file servers have no limits, i don't think it matters
	 */
	if (vp->v_type == VREG &&
	    uio->uio_offset + uio->uio_resid >
	      u.u_rlimit[RLIMIT_FSIZE].rlim_cur) {
		psignal(u.u_procp, SIGXFSZ);
		error = EFBIG;
		goto out;
	}
	np->n_flag |= (NMODIFIED|NBUFFERED);
	do {
		lbn = uio->uio_offset >> NFS_BIOSHIFT;
		on = uio->uio_offset & (NFS_BIOSIZE-1);
		n = MIN((unsigned)(NFS_BIOSIZE - on), uio->uio_resid);
		if (uio->uio_offset+n > np->n_size)
			np->n_size = uio->uio_offset+n;
		bn = lbn*(NFS_BIOSIZE/DEV_BSIZE);
		count = howmany(NFS_BIOSIZE, CLBYTES);
		for (i = 0; i < count; i++)
			munhash(vp, bn + i * CLBYTES / DEV_BSIZE);
		bp = getblk(vp, bn, NFS_BIOSIZE);
		if (bp->b_wcred == NOCRED) {
			crhold(cred);
			bp->b_wcred = cred;
		}
		if (bp->b_dirtyend > 0) {
			/*
			 * Iff the new write will leave a contiguous
			 * dirty area, just update the b_dirtyoff and
			 * b_dirtyend
			 * otherwise force a write rpc of the old dirty
			 * area
			 */
			if (on <= bp->b_dirtyend && (on+n) >= bp->b_dirtyoff) {
				bp->b_dirtyoff = MIN(on, bp->b_dirtyoff);
				bp->b_dirtyend = MAX((on+n), bp->b_dirtyend);
			} else {
				/*
				 * Like bwrite() but without the brelse
				 */
				bp->b_flags &= ~(B_READ | B_DONE |
				    B_ERROR | B_DELWRI | B_ASYNC);
				u.u_ru.ru_oublock++;
				VOP_STRATEGY(bp);
				error = biowait(bp);
				if (bp->b_flags & B_ERROR) {
					brelse(bp);
					if (bp->b_error)
						error = bp->b_error;
					else
						error = EIO;
					goto out;
				}
				bp->b_dirtyoff = on;
				bp->b_dirtyend = on+n;
			}
		} else {
			bp->b_dirtyoff = on;
			bp->b_dirtyend = on+n;
		}
		if (error = uiomove(bp->b_un.b_addr + on, n, uio))
			goto out;
		if ((n+on) == NFS_BIOSIZE) {
			bp->b_flags |= B_AGE;
			bawrite(bp);
		} else {
			bdwrite(bp);
		}
	} while (error == 0 && uio->uio_resid > 0 && n != 0);
#ifdef notdef
	/* Should we try and do this for nfs ?? */
	if (error && (ioflag & IO_UNIT))
		np->n_size = osize;
	else
#endif
		*offp += cnt - uio->uio_resid;
out:
	if ((ioflag & IO_NODELOCKED) == 0)
		nfs_unlock(vp);
	return (error);
}

/*
 * Flush and invalidate all of the buffers associated with the blocks of vp
 */
nfs_blkflush(vp, blkno, size, invalidate)
	struct vnode *vp;
	daddr_t blkno;
	long size;
	int invalidate;
{
	register struct buf *ep;
	struct buf *dp;
	daddr_t curblkno, last;
	int s, error, allerrors = 0;

	last = blkno + btodb(size);
	for (curblkno = blkno; curblkno <= last;
	     curblkno += (NFS_BIOSIZE / DEV_BSIZE)) {
		dp = BUFHASH(vp, curblkno);
loop:
		for (ep = dp->b_forw; ep != dp; ep = ep->b_forw) {
			if (ep->b_vp != vp || (ep->b_flags & B_INVAL))
				continue;
			if (curblkno != ep->b_blkno)
				continue;
			s = splbio();
			if (ep->b_flags & B_BUSY) {
				ep->b_flags |= B_WANTED;
				sleep((caddr_t)ep, PRIBIO+1);
				splx(s);
				goto loop;
			}
			splx(s);
			notavail(ep);
			if (ep->b_flags & B_DELWRI) {
				ep->b_flags &= ~B_ASYNC;
				if (error = bwrite(ep))
					allerrors = error;
				goto loop;
			}
			if (invalidate) {
				ep->b_flags |= B_INVAL;
				brelvp(ep);
			}
			brelse(ep);
		}
	}
	return (allerrors);
}
