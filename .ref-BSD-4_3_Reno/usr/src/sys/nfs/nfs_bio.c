/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)nfs_bio.c	7.16 (Berkeley) 6/28/90
 */

#include "param.h"
#include "user.h"
#include "buf.h"
#include "vnode.h"
#include "trace.h"
#include "mount.h"
#include "nfsnode.h"
#include "nfsv2.h"
#include "nfs.h"
#include "nfsiom.h"
#include "nfsmount.h"

/* True and false, how exciting */
#define	TRUE	1
#define	FALSE	0

/*
 * Vnode op for read using bio
 * Any similarity to readip() is purely coincidental
 */
nfs_bioread(vp, uio, ioflag, cred)
	register struct vnode *vp;
	register struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	register struct nfsnode *np = VTONFS(vp);
	register int biosize;
	struct buf *bp;
	struct vattr vattr;
	daddr_t lbn, bn, rablock;
	int diff, error = 0;
	long n, on;

#ifdef lint
	ioflag = ioflag;
#endif /* lint */
	if (uio->uio_rw != UIO_READ)
		panic("nfs_read mode");
	if (uio->uio_resid == 0)
		return (0);
	if (uio->uio_offset < 0 && vp->v_type != VDIR)
		return (EINVAL);
	biosize = VFSTONFS(vp->v_mount)->nm_rsize;
	/*
	 * If the file's modify time on the server has changed since the
	 * last read rpc or you have written to the file,
	 * you may have lost data cache consistency with the
	 * server, so flush all of the file's data out of the cache.
	 * Then force a getattr rpc to ensure that you have up to date
	 * attributes.
	 * NB: This implies that cache data can be read when up to
	 * NFS_ATTRTIMEO seconds out of date. If you find that you need current
	 * attributes this could be forced by setting n_attrstamp to 0 before
	 * the nfs_dogetattr() call.
	 */
	if (vp->v_type != VLNK) {
		if (np->n_flag & NMODIFIED) {
			np->n_flag &= ~NMODIFIED;
			vinvalbuf(vp, TRUE);
			np->n_attrstamp = 0;
			np->n_direofoffset = 0;
			if (error = nfs_dogetattr(vp, &vattr, cred, 1))
				return (error);
			np->n_mtime = vattr.va_mtime.tv_sec;
		} else {
			if (error = nfs_dogetattr(vp, &vattr, cred, 1))
				return (error);
			if (np->n_mtime != vattr.va_mtime.tv_sec) {
				np->n_direofoffset = 0;
				vinvalbuf(vp, TRUE);
				np->n_mtime = vattr.va_mtime.tv_sec;
			}
		}
	}
	do {
	    switch (vp->v_type) {
	    case VREG:
		nfsstats.biocache_reads++;
		lbn = uio->uio_offset / biosize;
		on = uio->uio_offset & (biosize-1);
		n = MIN((unsigned)(biosize - on), uio->uio_resid);
		diff = np->n_size - uio->uio_offset;
		if (diff <= 0)
			return (error);
		if (diff < n)
			n = diff;
		bn = lbn*(biosize/DEV_BSIZE);
		rablock = (lbn+1)*(biosize/DEV_BSIZE);
		if (vp->v_lastr + 1 == lbn &&
		    np->n_size > (rablock * DEV_BSIZE))
			error = breada(vp, bn, biosize, rablock, biosize,
				cred, &bp);
		else
			error = bread(vp, bn, biosize, cred, &bp);
		vp->v_lastr = lbn;
		if (bp->b_resid) {
		   diff = (on >= (biosize-bp->b_resid)) ? 0 :
			(biosize-bp->b_resid-on);
		   n = MIN(n, diff);
		}
		break;
	    case VLNK:
		nfsstats.biocache_readlinks++;
		on = 0;
		error = bread(vp, (daddr_t)0, NFS_MAXPATHLEN, cred, &bp);
		n = MIN(uio->uio_resid, NFS_MAXPATHLEN - bp->b_resid);
		break;
	    case VDIR:
		nfsstats.biocache_readdirs++;
		on = 0;
		error = bread(vp, uio->uio_offset, DIRBLKSIZ, cred, &bp);
		n = MIN(uio->uio_resid, DIRBLKSIZ - bp->b_resid);
		break;
	    };
	    if (error) {
		brelse(bp);
		return (error);
	    }
	    if (n > 0)
		error = uiomove(bp->b_un.b_addr + on, (int)n, uio);
	    switch (vp->v_type) {
	    case VREG:
		if (n+on == biosize || uio->uio_offset == np->n_size)
			bp->b_flags |= B_AGE;
		break;
	    case VLNK:
		n = 0;
		break;
	    case VDIR:
		uio->uio_offset = bp->b_blkno;
		break;
	    };
	    brelse(bp);
	} while (error == 0 && uio->uio_resid > 0 && n != 0);
	return (error);
}

/*
 * Vnode op for write using bio
 */
nfs_write(vp, uio, ioflag, cred)
	register struct vnode *vp;
	register struct uio *uio;
	int ioflag;
	struct ucred *cred;
{
	register int biosize;
	struct buf *bp;
	struct nfsnode *np = VTONFS(vp);
	struct vattr vattr;
	daddr_t lbn, bn;
	int n, on, error = 0;

	if (uio->uio_rw != UIO_WRITE)
		panic("nfs_write mode");
	if (vp->v_type != VREG)
		return (EIO);
	/* Should we try and do this ?? */
	if (ioflag & (IO_APPEND | IO_SYNC)) {
		if (np->n_flag & NMODIFIED) {
			np->n_flag &= ~NMODIFIED;
			vinvalbuf(vp, TRUE);
		}
		if (ioflag & IO_APPEND) {
			np->n_attrstamp = 0;
			if (error = nfs_dogetattr(vp, &vattr, cred, 1))
				return (error);
			uio->uio_offset = np->n_size;
		}
		return (nfs_writerpc(vp, uio, cred, u.u_procp));
	}
#ifdef notdef
	cnt = uio->uio_resid;
	osize = np->n_size;
#endif
	if (uio->uio_offset < 0)
		return (EINVAL);
	if (uio->uio_resid == 0)
		return (0);
	/*
	 * Maybe this should be above the vnode op call, but so long as
	 * file servers have no limits, i don't think it matters
	 */
	if (uio->uio_offset + uio->uio_resid >
	      u.u_rlimit[RLIMIT_FSIZE].rlim_cur) {
		psignal(u.u_procp, SIGXFSZ);
		return (EFBIG);
	}
	/*
	 * I use nm_rsize, not nm_wsize so that all buffer cache blocks
	 * will be the same size within a filesystem. nfs_writerpc will
	 * still use nm_wsize when sizing the rpc's.
	 */
	biosize = VFSTONFS(vp->v_mount)->nm_rsize;
	np->n_flag |= NMODIFIED;
	do {
		nfsstats.biocache_writes++;
		lbn = uio->uio_offset / biosize;
		on = uio->uio_offset & (biosize-1);
		n = MIN((unsigned)(biosize - on), uio->uio_resid);
		if (uio->uio_offset+n > np->n_size)
			np->n_size = uio->uio_offset+n;
		bn = lbn*(biosize/DEV_BSIZE);
again:
		bp = getblk(vp, bn, biosize);
		if (bp->b_wcred == NOCRED) {
			crhold(cred);
			bp->b_wcred = cred;
		}
		if (bp->b_dirtyend > 0) {
			/*
			 * If the new write will leave a contiguous dirty
			 * area, just update the b_dirtyoff and b_dirtyend,
			 * otherwise force a write rpc of the old dirty area.
			 */
			if (on <= bp->b_dirtyend && (on+n) >= bp->b_dirtyoff) {
				bp->b_dirtyoff = MIN(on, bp->b_dirtyoff);
				bp->b_dirtyend = MAX((on+n), bp->b_dirtyend);
			} else {
				bp->b_proc = u.u_procp;
				if (error = bwrite(bp))
					return (error);
				goto again;
			}
		} else {
			bp->b_dirtyoff = on;
			bp->b_dirtyend = on+n;
		}
		if (error = uiomove(bp->b_un.b_addr + on, n, uio)) {
			brelse(bp);
			return (error);
		}
		if ((n+on) == biosize) {
			bp->b_flags |= B_AGE;
			bp->b_proc = (struct proc *)0;
			bawrite(bp);
		} else {
			bp->b_proc = (struct proc *)0;
			bdwrite(bp);
		}
	} while (error == 0 && uio->uio_resid > 0 && n != 0);
#ifdef notdef
	/* Should we try and do this for nfs ?? */
	if (error && (ioflag & IO_UNIT)) {
		np->n_size = osize;
		uio->uio_offset -= cnt - uio->uio_resid;
		uio->uio_resid = cnt;
	}
#endif
	return (error);
}
