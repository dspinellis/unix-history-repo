/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfs_bio.c	7.33 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/resourcevar.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/trace.h>
#include <sys/mount.h>
#include <sys/kernel.h>

#include <vm/vm.h>

#include <nfs/nfsnode.h>
#include <nfs/rpcv2.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#include <nfs/nfsmount.h>
#include <nfs/nqnfs.h>

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
	struct nfsmount *nmp;
	daddr_t lbn, bn, rablock[NFS_MAXRAHEAD];
	int rasize[NFS_MAXRAHEAD], nra, diff, error = 0;
	int n, on;

#ifdef lint
	ioflag = ioflag;
#endif /* lint */
#ifdef DIAGNOSTIC
	if (uio->uio_rw != UIO_READ)
		panic("nfs_read mode");
#endif
	if (uio->uio_resid == 0)
		return (0);
	if (uio->uio_offset < 0 && vp->v_type != VDIR)
		return (EINVAL);
	nmp = VFSTONFS(vp->v_mount);
	biosize = nmp->nm_rsize;
	/*
	 * For nfs, cache consistency can only be maintained approximately.
	 * Although RFC1094 does not specify the criteria, the following is
	 * believed to be compatible with the reference port.
	 * For nqnfs, full cache consistency is maintained within the loop.
	 * For nfs:
	 * If the file's modify time on the server has changed since the
	 * last read rpc or you have written to the file,
	 * you may have lost data cache consistency with the
	 * server, so flush all of the file's data out of the cache.
	 * Then force a getattr rpc to ensure that you have up to date
	 * attributes.
	 * The mount flag NFSMNT_MYWRITE says "Assume that my writes are
	 * the ones changing the modify time.
	 * NB: This implies that cache data can be read when up to
	 * NFS_ATTRTIMEO seconds out of date. If you find that you need current
	 * attributes this could be forced by setting n_attrstamp to 0 before
	 * the VOP_GETATTR() call.
	 */
	if ((nmp->nm_flag & NFSMNT_NQNFS) == 0 && vp->v_type != VLNK) {
		if (np->n_flag & NMODIFIED) {
			np->n_flag &= ~NMODIFIED;
			if ((nmp->nm_flag & NFSMNT_MYWRITE) == 0 ||
			     vp->v_type != VREG)
				vinvalbuf(vp, TRUE, cred, uio->uio_procp);
			np->n_attrstamp = 0;
			np->n_direofoffset = 0;
			if (error = VOP_GETATTR(vp, &vattr, cred, uio->uio_procp))
				return (error);
			np->n_mtime = vattr.va_mtime.ts_sec;
		} else {
			if (error = VOP_GETATTR(vp, &vattr, cred, uio->uio_procp))
				return (error);
			if (np->n_mtime != vattr.va_mtime.ts_sec) {
				np->n_direofoffset = 0;
				vinvalbuf(vp, TRUE, cred, uio->uio_procp);
				np->n_mtime = vattr.va_mtime.ts_sec;
			}
		}
	}
	do {

	    /*
	     * Get a valid lease. If cached data is stale, flush it.
	     */
	    if ((nmp->nm_flag & NFSMNT_NQNFS) &&
		NQNFS_CKINVALID(vp, np, NQL_READ)) {
		do {
			error = nqnfs_getlease(vp, NQL_READ, cred, uio->uio_procp);
		} while (error == NQNFS_EXPIRED);
		if (error)
			return (error);
		if (np->n_lrev != np->n_brev ||
		    ((np->n_flag & NMODIFIED) && vp->v_type == VDIR)) {
			if (vp->v_type == VDIR) {
				np->n_direofoffset = 0;
				cache_purge(vp);
			}
			np->n_flag &= ~NMODIFIED;
			vinvalbuf(vp, TRUE, cred, uio->uio_procp);
			np->n_brev = np->n_lrev;
		}
	    }
	    if (np->n_flag & NQNFSNONCACHE) {
		switch (vp->v_type) {
		case VREG:
			error = nfs_readrpc(vp, uio, cred);
			break;
		case VLNK:
			error = nfs_readlinkrpc(vp, uio, cred);
			break;
		case VDIR:
			error = nfs_readdirrpc(vp, uio, cred);
			break;
		};
		return (error);
	    }
	    switch (vp->v_type) {
	    case VREG:
		nfsstats.biocache_reads++;
		lbn = uio->uio_offset / biosize;
		on = uio->uio_offset & (biosize-1);
		n = min((unsigned)(biosize - on), uio->uio_resid);
		diff = np->n_size - uio->uio_offset;
		if (diff <= 0)
			return (error);
		if (diff < n)
			n = diff;
		bn = lbn*(biosize/DEV_BSIZE);
		for (nra = 0; nra < nmp->nm_readahead &&
			(lbn + 1 + nra) * biosize < np->n_size; nra++) {
			rablock[nra] = (lbn + 1 + nra) * (biosize / DEV_BSIZE);
			rasize[nra] = biosize;
		}
again:
		if (nra > 0 && lbn >= vp->v_lastr)
			error = breadn(vp, bn, biosize, rablock, rasize, nra,
				cred, &bp);
		else
			error = bread(vp, bn, biosize, cred, &bp);
		if (bp->b_validend > 0) {
			if (on < bp->b_validoff || (on+n) > bp->b_validend) {
				bp->b_flags |= B_INVAL;
				if (bp->b_dirtyend > 0) {
					if ((bp->b_flags & B_DELWRI) == 0)
						panic("nfsbioread");
					(void) bwrite(bp);
				} else
					brelse(bp);
				goto again;
			}
		} else {
			bp->b_validoff = 0;
			bp->b_validend = biosize - bp->b_resid;
		}
		vp->v_lastr = lbn;
		if (bp->b_resid) {
		   diff = (on >= (biosize-bp->b_resid)) ? 0 :
			(biosize-bp->b_resid-on);
		   n = min(n, diff);
		}
		break;
	    case VLNK:
		nfsstats.biocache_readlinks++;
		on = 0;
		error = bread(vp, (daddr_t)0, NFS_MAXPATHLEN, cred, &bp);
		n = min(uio->uio_resid, NFS_MAXPATHLEN - bp->b_resid);
		break;
	    case VDIR:
		nfsstats.biocache_readdirs++;
		on = 0;
		error = bread(vp, uio->uio_offset, NFS_DIRBLKSIZ, cred, &bp);
		n = min(uio->uio_resid, NFS_DIRBLKSIZ - bp->b_resid);
		break;
	    };
	    if (error) {
		brelse(bp);
		return (error);
	    }

	    /*
	     * For nqnfs:
	     * Must check for valid lease, since it may have expired while in
	     * bread(). If expired, get a lease.
	     * If data is stale, flush and try again.
	     * nb: If a read rpc is done by bread() or breada() and there is
	     *     no valid lease, a get_lease request will be piggy backed.
	     */
	    if (nmp->nm_flag & NFSMNT_NQNFS) {
		if (NQNFS_CKINVALID(vp, np, NQL_READ)) {
			do {
				error = nqnfs_getlease(vp, NQL_READ, cred, uio->uio_procp);
			} while (error == NQNFS_EXPIRED);
			if (error) {
				brelse(bp);
				return (error);
			}
			if ((np->n_flag & NQNFSNONCACHE) ||
			    np->n_lrev != np->n_brev ||
			    ((np->n_flag & NMODIFIED) && vp->v_type == VDIR)) {
				if (vp->v_type == VDIR) {
					np->n_direofoffset = 0;
					cache_purge(vp);
				}
				brelse(bp);
				np->n_flag &= ~NMODIFIED;
				vinvalbuf(vp, TRUE, cred, uio->uio_procp);
				np->n_brev = np->n_lrev;
				continue;
			}
		} else if ((np->n_flag & NQNFSNONCACHE) ||
		    ((np->n_flag & NMODIFIED) && vp->v_type == VDIR)) {
			np->n_direofoffset = 0;
			brelse(bp);
			np->n_flag &= ~NMODIFIED;
			vinvalbuf(vp, TRUE, cred, uio->uio_procp);
			np->n_brev = np->n_lrev;
			continue;
		}
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
nfs_write(ap)
	struct vop_write_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		int  a_ioflag;
		struct ucred *a_cred;
	} */ *ap;
{
	register int biosize;
	register struct uio *uio = ap->a_uio;
	struct proc *p = uio->uio_procp;
	register struct vnode *vp = ap->a_vp;
	struct nfsnode *np = VTONFS(vp);
	register struct ucred *cred = ap->a_cred;
	int ioflag = ap->a_ioflag;
	struct buf *bp;
	struct vattr vattr;
	struct nfsmount *nmp;
	daddr_t lbn, bn;
	int n, on, error = 0;

#ifdef DIAGNOSTIC
	if (uio->uio_rw != UIO_WRITE)
		panic("nfs_write mode");
	if (uio->uio_segflg == UIO_USERSPACE && uio->uio_procp != curproc)
		panic("nfs_write proc");
#endif
	if (vp->v_type != VREG)
		return (EIO);
	if (np->n_flag & NWRITEERR) {
		np->n_flag &= ~NWRITEERR;
		return (np->n_error);
	}
	if (ioflag & (IO_APPEND | IO_SYNC)) {
		if (np->n_flag & NMODIFIED) {
			np->n_flag &= ~NMODIFIED;
			np->n_attrstamp = 0;
			vinvalbuf(vp, TRUE, cred, p);
		}
		if (ioflag & IO_APPEND) {
			np->n_attrstamp = 0;
			if (error = VOP_GETATTR(vp, &vattr, cred, p))
				return (error);
			uio->uio_offset = np->n_size;
		}
	}
	nmp = VFSTONFS(vp->v_mount);
	if (uio->uio_offset < 0)
		return (EINVAL);
	if (uio->uio_resid == 0)
		return (0);
	/*
	 * Maybe this should be above the vnode op call, but so long as
	 * file servers have no limits, i don't think it matters
	 */
	if (p && uio->uio_offset + uio->uio_resid >
	      p->p_rlimit[RLIMIT_FSIZE].rlim_cur) {
		psignal(p, SIGXFSZ);
		return (EFBIG);
	}
	/*
	 * I use nm_rsize, not nm_wsize so that all buffer cache blocks
	 * will be the same size within a filesystem. nfs_writerpc will
	 * still use nm_wsize when sizing the rpc's.
	 */
	biosize = nmp->nm_rsize;
	np->n_flag |= NMODIFIED;
	do {

		/*
		 * Check for a valid write lease.
		 * If non-cachable, just do the rpc
		 */
		if ((nmp->nm_flag & NFSMNT_NQNFS) &&
		    NQNFS_CKINVALID(vp, np, NQL_WRITE)) {
			do {
				error = nqnfs_getlease(vp, NQL_WRITE, cred, p);
			} while (error == NQNFS_EXPIRED);
			if (error)
				return (error);
			if (np->n_lrev != np->n_brev ||
			    (np->n_flag & NQNFSNONCACHE)) {
				vinvalbuf(vp, TRUE, cred, p);
				np->n_brev = np->n_lrev;
			}
		}
		if (np->n_flag & NQNFSNONCACHE)
			return (nfs_writerpc(vp, uio, cred, 0));
		nfsstats.biocache_writes++;
		lbn = uio->uio_offset / biosize;
		on = uio->uio_offset & (biosize-1);
		n = min((unsigned)(biosize - on), uio->uio_resid);
		if (uio->uio_offset + n > np->n_size) {
			np->n_size = uio->uio_offset + n;
			vnode_pager_setsize(vp, (u_long)np->n_size);
		}
		bn = lbn * (biosize / DEV_BSIZE);
again:
		bp = getblk(vp, bn, biosize);
		if (bp->b_wcred == NOCRED) {
			crhold(cred);
			bp->b_wcred = cred;
		}

		/*
		 * If the new write will leave a contiguous dirty
		 * area, just update the b_dirtyoff and b_dirtyend,
		 * otherwise force a write rpc of the old dirty area.
		 */
		if (bp->b_dirtyend > 0 &&
		    (on > bp->b_dirtyend || (on + n) < bp->b_dirtyoff)) {
			bp->b_proc = p;
			if (error = bwrite(bp))
				return (error);
			goto again;
		}

		/*
		 * Check for valid write lease and get one as required.
		 * In case getblk() and/or bwrite() delayed us.
		 */
		if ((nmp->nm_flag & NFSMNT_NQNFS) &&
		    NQNFS_CKINVALID(vp, np, NQL_WRITE)) {
			do {
				error = nqnfs_getlease(vp, NQL_WRITE, cred, p);
			} while (error == NQNFS_EXPIRED);
			if (error) {
				brelse(bp);
				return (error);
			}
			if (np->n_lrev != np->n_brev ||
			    (np->n_flag & NQNFSNONCACHE)) {
				brelse(bp);
				vinvalbuf(vp, TRUE, cred, p);
				np->n_brev = np->n_lrev;
				goto again;
			}
		}
		if (error = uiomove(bp->b_un.b_addr + on, n, uio)) {
			brelse(bp);
			return (error);
		}
		if (bp->b_dirtyend > 0) {
			bp->b_dirtyoff = min(on, bp->b_dirtyoff);
			bp->b_dirtyend = max((on+n), bp->b_dirtyend);
		} else {
			bp->b_dirtyoff = on;
			bp->b_dirtyend = on+n;
		}
		if (bp->b_validend == 0 || bp->b_validend < bp->b_dirtyoff ||
		    bp->b_validoff > bp->b_dirtyend) {
			bp->b_validoff = bp->b_dirtyoff;
			bp->b_validend = bp->b_dirtyend;
		} else {
			bp->b_validoff = min(bp->b_validoff, bp->b_dirtyoff);
			bp->b_validend = max(bp->b_validend, bp->b_dirtyend);
		}

		/*
		 * If the lease is non-cachable or IO_SYNC do bwrite().
		 */
		if ((np->n_flag & NQNFSNONCACHE) || (ioflag & IO_SYNC)) {
			bp->b_proc = p;
			bwrite(bp);
		} else if ((n+on) == biosize &&
			 (nmp->nm_flag & NFSMNT_NQNFS) == 0) {
			bp->b_flags |= B_AGE;
			bp->b_proc = (struct proc *)0;
			bawrite(bp);
		} else {
			bp->b_proc = (struct proc *)0;
			bdwrite(bp);
		}
	} while (error == 0 && uio->uio_resid > 0 && n != 0);
	return (error);
}
