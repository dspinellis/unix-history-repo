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
 *	@(#)nfs_vnops.c	7.6 (Berkeley) %G%
 */

/*
 * vnode op calls for sun nfs version 2
 */

#include "machine/pte.h"
#include "machine/mtpr.h"
#include "strings.h"
#include "param.h"
#include "user.h"
#include "proc.h"
#include "mount.h"
#include "buf.h"
#include "vm.h"
#include "../ufs/dir.h"
#include "malloc.h"
#include "mbuf.h"
#include "uio.h"
#include "ucred.h"
#include "namei.h"
#include "errno.h"
#include "file.h"
#include "conf.h"
#include "vnode.h"
#include "../ufs/inode.h"
#include "map.h"
#include "nfsv2.h"
#include "nfs.h"
#include "nfsnode.h"
#include "nfsmount.h"
#include "xdr_subs.h"
#include "nfsm_subs.h"
#include "nfsiom.h"

/* Defs */
#define	TRUE	1
#define	FALSE	0

/* Global vars */
int	nfs_lookup(),
	nfs_create(),
	nfs_open(),
	nfs_close(),
	nfs_access(),
	nfs_getattr(),
	nfs_setattr(),
	nfs_read(),
	nfs_write(),
	vfs_noop(),
	vfs_nullop(),
	nfs_remove(),
	nfs_link(),
	nfs_rename(),
	nfs_mkdir(),
	nfs_rmdir(),
	nfs_symlink(),
	nfs_readdir(),
	nfs_readlink(),
	nfs_abortop(),
	nfs_lock(),
	nfs_unlock(),
	nfs_bmap(),
	nfs_strategy(),
	nfs_fsync(),
	nfs_inactive();

struct vnodeops nfsv2_vnodeops = {
	nfs_lookup,
	nfs_create,
	vfs_noop,
	nfs_open,
	nfs_close,
	nfs_access,
	nfs_getattr,
	nfs_setattr,
	nfs_read,
	nfs_write,
	vfs_noop,
	vfs_noop,
	vfs_noop,
	nfs_fsync,
	vfs_noop,
	nfs_remove,
	nfs_link,
	nfs_rename,
	nfs_mkdir,
	nfs_rmdir,
	nfs_symlink,
	nfs_readdir,
	nfs_readlink,
	nfs_abortop,
	nfs_inactive,
	nfs_lock,
	nfs_unlock,
	nfs_bmap,
	nfs_strategy,
};

/* Special device vnode ops */
int	blk_open(),
	blk_read(),
	blk_write(),
	blk_ioctl(),
	blk_select(),
	ufs_bmap(),
	blk_strategy();

struct vnodeops nfsv2chr_vnodeops = {
	vfs_noop,
	vfs_noop,
	vfs_noop,
	blk_open,
	nfs_close,
	nfs_access,
	nfs_getattr,
	nfs_setattr,
	blk_read,
	blk_write,
	blk_ioctl,
	blk_select,
	vfs_noop,
	vfs_nullop,
	vfs_noop,
	nfs_remove,
	nfs_link,
	nfs_rename,
	vfs_noop,
	vfs_noop,
	nfs_symlink,
	vfs_noop,
	vfs_noop,
	nfs_abortop,
	nfs_inactive,
	nfs_lock,
	nfs_unlock,
	ufs_bmap,
	blk_strategy,
};

extern u_long nfs_procids[NFS_NPROCS];
extern u_long nfs_prog, nfs_vers;
extern char nfsiobuf[MAXPHYS+NBPG];
struct map nfsmap[NFS_MSIZ];
enum vtype v_type[NFLNK+1];
struct buf nfs_bqueue;		/* Queue head for nfsiod's */
int nfs_asyncdaemons = 0;
struct proc *nfs_iodwant[MAX_ASYNCDAEMON];
static int nfsmap_want = 0;

/*
 * nfs null call from vfs.
 */
nfs_null(vp, cred)
	struct vnode *vp;
	struct ucred *cred;
{
	nfsm_vars;
	
	nfsm_reqhead(nfs_procids[NFSPROC_NULL], cred, 0);
	nfsm_request(vp);
	nfsm_reqdone;
	return (error);
}

/*
 * nfs access vnode op.
 * Essentially just get vattr and then imitate iaccess()
 */
nfs_access(vp, mode, cred)
	struct vnode *vp;
	int mode;
	register struct ucred *cred;
{
	register struct vattr *vap;
	register gid_t *gp;
	struct vattr vattr;
	register int i;
	int error;

	/*
	 * If you're the super-user,
	 * you always get access.
	 */
	if (cred->cr_uid == 0)
		return (0);
	vap = &vattr;
	if (error = nfs_getattr(vp, vap, cred))
		return (error);
	/*
	 * Access check is based on only one of owner, group, public.
	 * If not owner, then check group. If not a member of the
	 * group, then check public access.
	 */
	if (cred->cr_uid != vap->va_uid) {
		mode >>= 3;
		gp = cred->cr_groups;
		for (i = 0; i < cred->cr_ngroups; i++, gp++)
			if (vap->va_gid == *gp)
				goto found;
		mode >>= 3;
found:
		;
	}
	if ((vap->va_mode & mode) != 0)
		return (0);
	return (EACCES);
}

/*
 * nfs open vnode op
 * Just check to see if the type is ok
 */
nfs_open(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{
	register enum vtype vtyp;

	vtyp = vp->v_type;
	if (vtyp == VREG || vtyp == VDIR || vtyp == VLNK)
		return (0);
	else
		return (EACCES);
}

/*
 * nfs close vnode op
 * For reg files, invalidate any buffer cache entries.
 * For VCHR, do the device close
 */
nfs_close(vp, fflags, cred)
	register struct vnode *vp;
	int fflags;
	struct ucred *cred;
{
	struct nfsnode *np = VTONFS(vp);
	dev_t dev;
	int error = 0;

	nfs_lock(vp);
	if (vp->v_type == VREG && ((np->n_flag & NMODIFIED) ||
	   ((np->n_flag & NBUFFERED) && np->n_sillyrename))) {
		np->n_flag &= ~(NMODIFIED|NBUFFERED);
		error = nfs_blkflush(vp, (daddr_t)0, np->n_size, TRUE);
		if (np->n_flag & NWRITEERR) {
			np->n_flag &= ~NWRITEERR;
			if (!error)
				error = np->n_error ? np->n_error : EIO;
		}
	}
	nfs_unlock(vp);
	if (vp->v_type != VCHR || vp->v_count > 1)
		return (error);
	dev = vp->v_rdev;
	/* XXX what is this doing below the vnode op call */
	if (setjmp(&u.u_qsave)) {
		/*
		 * If device close routine is interrupted,
		 * must return so closef can clean up.
		 */
		error = EINTR;
	} else
		error = (*cdevsw[major(dev)].d_close)(dev, fflags, IFCHR);
	/*
	 * Most device close routines don't return errors,
	 * and dup2() doesn't work right on error.
	 */
	error = 0;		/* XXX */
	return (error);
}

/*
 * nfs getattr call from vfs.
 */
nfs_getattr(vp, vap, cred)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
{
	nfsm_vars;
	
	/* First look in the cache.. */
	if (nfs_getattrcache(vp, vap) == 0)
		return (0);
	nfsstats.rpccnt[NFSPROC_GETATTR]++;
	nfsm_reqhead(nfs_procids[NFSPROC_GETATTR], cred, NFSX_FH);
	nfsm_fhtom(vp);
	nfsm_request(vp);
	nfsm_loadattr(vp, vap);
	nfsm_reqdone;
	return (error);
}

/*
 * nfs setattr call.
 */
nfs_setattr(vp, vap, cred)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
{
	register struct nfsv2_sattr *sp;
	nfsm_vars;

	nfsstats.rpccnt[NFSPROC_SETATTR]++;
	nfsm_reqhead(nfs_procids[NFSPROC_SETATTR], cred, NFSX_FH+NFSX_SATTR);
	nfsm_fhtom(vp);
	nfsm_build(sp, struct nfsv2_sattr *, NFSX_SATTR);
	if (vap->va_mode == 0xffff)
		sp->sa_mode = VNOVAL;
	else
		sp->sa_mode = vtonfs_mode(vp->v_type, vap->va_mode);
	if (vap->va_uid == 0xffff)
		sp->sa_uid = VNOVAL;
	else
		sp->sa_uid = txdr_unsigned(vap->va_uid);
	if (vap->va_gid == 0xffff)
		sp->sa_gid = VNOVAL;
	else
		sp->sa_gid = txdr_unsigned(vap->va_gid);
	sp->sa_size = txdr_unsigned(vap->va_size);
	if (vap->va_size != VNOVAL)
		VTONFS(vp)->n_size = vap->va_size;
	txdr_time(&vap->va_atime, &sp->sa_atime);
	txdr_time(&vap->va_mtime, &sp->sa_mtime);
	nfsm_request(vp);
	nfsm_loadattr(vp, (struct vattr *)0);
	/* should we fill in any vap fields ?? */
	nfsm_reqdone;
	return (error);
}

/*
 * nfs lookup call, one step at a time...
 * First look in cache
 * If not found, unlock the directory nfsnode and do the rpc
 */
nfs_lookup(vp, ndp)
	register struct vnode *vp;
	register struct nameidata *ndp;
{
	register struct vnode *vdp;
	nfsm_vars;
	struct vnode *newvp;
	long len;
	nfsv2fh_t *fhp;
	struct nfsnode *np;
	int lockparent, wantparent, flag;
	dev_t rdev;

	ndp->ni_dvp = vp;
	ndp->ni_vp = NULL;
	if (vp->v_type != VDIR)
		return (ENOTDIR);
	lockparent = ndp->ni_nameiop & LOCKPARENT;
	flag = ndp->ni_nameiop & OPFLAG;
	wantparent = ndp->ni_nameiop & (LOCKPARENT|WANTPARENT);
#ifndef notyet
	if ((error = cache_lookup(ndp)) && error != ENOENT) {
		struct vattr vattr;
		int vpid;

		if (ndp->ni_vp == ndp->ni_rdir && ndp->ni_isdotdot)
			vdp = vp;
		else
			vdp = ndp->ni_vp;
		vpid = vdp->v_id;
		/*
		 * See the comment starting `Step through' in ufs/ufs_lookup.c
		 * for an explanation of the locking protocol
		 */
		if (vp == vdp) {
			VREF(vdp);
		} else if (ndp->ni_isdotdot) {
			nfs_unlock(vp);
			nfs_ngrab(VTONFS(vdp));
		} else {
			nfs_ngrab(VTONFS(vdp));
			nfs_unlock(vp);
		}
		if (vpid == vdp->v_id &&
		   !nfs_getattr(vdp, &vattr, ndp->ni_cred)) {
			nfsstats.lookupcache_hits++;
			if (vdp->v_type == VNON) {
				vdp->v_type = vattr.va_type;
				VTONFS(vdp)->n_mtime = vattr.va_mtime.tv_sec;
			}
			return (0);
		}
		nfs_nput(vdp);
		nfs_lock(vp);
		ndp->ni_vp = (struct vnode *)0;
	}
	error = 0;
	nfsstats.lookupcache_misses++;
#endif
	nfsstats.rpccnt[NFSPROC_LOOKUP]++;
	len = ndp->ni_namelen;
	nfsm_reqhead(nfs_procids[NFSPROC_LOOKUP], ndp->ni_cred, NFSX_FH+NFSX_UNSIGNED+nfsm_rndup(len));
	nfsm_fhtom(vp);
	nfsm_strtom(ndp->ni_ptr, len, NFS_MAXNAMLEN);
	nfsm_request(vp);
nfsmout:
	if (error) {
		if ((flag == CREATE || flag == RENAME) &&
			*ndp->ni_next == 0) {
			if (!lockparent)
				nfs_unlock(vp);
		}
		return (ENOENT);
	}
	nfsm_disect(fhp,nfsv2fh_t *,NFSX_FH);

	/*
	 * Handle DELETE and RENAME cases...
	 */
	if (flag == DELETE && *ndp->ni_next == 0) {
		if (!bcmp(VTONFS(vp)->n_fh.fh_bytes, (caddr_t)fhp, NFSX_FH)) {
			VREF(vp);
			newvp = vp;
			np = VTONFS(vp);
		} else {
			if (error = nfs_nget(vp->v_mount, fhp, &np)) {
				m_freem(mrep);
				return (error);
			}
			newvp = NFSTOV(np);
		}
		if (error = nfs_loadattrcache(newvp, &md, &dpos, (struct vattr *)0)) {
			if (newvp != vp)
				nfs_nput(newvp);
			else
				vrele(vp);
			m_freem(mrep);
			return (error);
		}
		if (newvp->v_type == VNON) {
			newvp->v_type = np->n_vattr.va_type;
			np->n_mtime = np->n_vattr.va_mtime.tv_sec;
		}
		ndp->ni_vp = newvp;
		if (!lockparent)
			nfs_unlock(vp);
		m_freem(mrep);
		return (0);
	}

	if (flag == RENAME && wantparent && *ndp->ni_next == 0) {
		if (!bcmp(VTONFS(vp)->n_fh.fh_bytes, (caddr_t)fhp, NFSX_FH)) {
			m_freem(mrep);
			return (EISDIR);
		}
		if (error = nfs_nget(vp->v_mount, fhp, &np)) {
			m_freem(mrep);
			return (error);
		}
		newvp = NFSTOV(np);
		if (error = nfs_loadattrcache(newvp, &md, &dpos, (struct vattr *)0)) {
			nfs_nput(newvp);
			m_freem(mrep);
			return (error);
		}
		ndp->ni_vp = newvp;
		if (!lockparent)
			nfs_unlock(vp);
		return (0);
	}

	if (!bcmp(VTONFS(vp)->n_fh.fh_bytes, (caddr_t)fhp, NFSX_FH)) {
		VREF(vp);
		newvp = vp;
		np = VTONFS(vp);
	} else if (ndp->ni_isdotdot) {
		nfs_unlock(vp);
		if (error = nfs_nget(vp->v_mount, fhp, &np)) {
			nfs_lock(vp);
			m_freem(mrep);
			return (error);
		}
		nfs_lock(vp);
		newvp = NFSTOV(np);
	} else {
		if (error = nfs_nget(vp->v_mount, fhp, &np)) {
			m_freem(mrep);
			return (error);
		}
		newvp = NFSTOV(np);
	}
	if (error = nfs_loadattrcache(newvp, &md, &dpos, (struct vattr *)0)) {
		if (newvp != vp)
			nfs_nput(newvp);
		else
			vrele(vp);
		m_freem(mrep);
		return (error);
	}
	m_freem(mrep);
	if (newvp->v_type == VNON) {
		newvp->v_type = np->n_vattr.va_type;
		np->n_mtime = np->n_vattr.va_mtime.tv_sec;
	}

	/*
	 * Handling special files...
	 * For VCHR, use the nfs_node, but with the nfsv2chr_vnodeops
	 * that are a mix of nfs and blk vnode ops.
	 * Also, returns right away to avoid loading the name cache
	 */
	if (newvp->v_type == VCHR) {
		newvp->v_rdev = np->n_vattr.va_rdev;
		newvp->v_op = &nfsv2chr_vnodeops;
	}
	if (vp != newvp && (!lockparent || *ndp->ni_next != '\0'))
		nfs_unlock(vp);
	ndp->ni_vp = newvp;
#ifndef notyet
	if (error == 0 && ndp->ni_makeentry)
		cache_enter(ndp);
#endif
	return (error);
}

/*
 * nfs readlink call
 */
nfs_readlink(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{
	nfsm_vars;
	long len;

	nfsstats.rpccnt[NFSPROC_READLINK]++;
	nfsm_reqhead(nfs_procids[NFSPROC_READLINK], cred, NFSX_FH);
	nfsm_fhtom(vp);
	nfsm_request(vp);
	nfsm_strsiz(len, NFS_MAXPATHLEN);
	nfsm_mtouio(uiop, len);
	nfsm_reqdone;
	return (error);
}

/*
 * nfs read call
 */
nfs_readrpc(vp, uiop, offp, cred)
	struct vnode *vp;
	struct uio *uiop;
	off_t *offp;
	struct ucred *cred;
{
	nfsm_vars;
	struct nfsmount *nmp;
	long len, retlen, tsiz;

	nmp = vfs_to_nfs(vp->v_mount);
	tsiz = uiop->uio_resid;
	while (tsiz > 0) {
		nfsstats.rpccnt[NFSPROC_READ]++;
		len = (tsiz > nmp->nm_rsize) ? nmp->nm_rsize : tsiz;
		nfsm_reqhead(nfs_procids[NFSPROC_READ], cred, NFSX_FH+NFSX_UNSIGNED*3);
		nfsm_fhtom(vp);
		nfsm_build(p, u_long *, NFSX_UNSIGNED*3);
		*p++ = txdr_unsigned(*offp);
		*p++ = txdr_unsigned(len);
		*p = 0;
		nfsm_request(vp);
		nfsm_loadattr(vp, (struct vattr *)0);
		nfsm_strsiz(retlen, nmp->nm_rsize);
		nfsm_mtouio(uiop, retlen);
		m_freem(mrep);
		*offp += retlen;
		if (retlen < len)
			tsiz = 0;
		else
			tsiz -= len;
	}
nfsmout:
	return (error);
}

/*
 * nfs write call
 */
nfs_writerpc(vp, uiop, offp, cred)
	struct vnode *vp;
	struct uio *uiop;
	off_t *offp;
	struct ucred *cred;
{
	nfsm_vars;
	struct nfsmount *nmp;
	long len, tsiz;

	nmp = vfs_to_nfs(vp->v_mount);
	tsiz = uiop->uio_resid;
	while (tsiz > 0) {
		nfsstats.rpccnt[NFSPROC_WRITE]++;
		len = (tsiz > nmp->nm_wsize) ? nmp->nm_wsize : tsiz;
		nfsm_reqhead(nfs_procids[NFSPROC_WRITE], cred,
			NFSX_FH+NFSX_UNSIGNED*4);
		nfsm_fhtom(vp);
		nfsm_build(p, u_long *, NFSX_UNSIGNED*4);
		*(p+1) = txdr_unsigned(*offp);
		*(p+3) = txdr_unsigned(len);
		nfsm_uiotom(uiop, len);
		nfsm_request(vp);
		nfsm_loadattr(vp, (struct vattr *)0);
		m_freem(mrep);
		tsiz -= len;
		*offp += len;
	}
nfsmout:
	return (error);
}

/*
 * nfs file create call
 */
nfs_create(ndp, vap)
	register struct nameidata *ndp;
	register struct vattr *vap;
{
	register struct nfsv2_sattr *sp;
	nfsm_vars;

	nfsstats.rpccnt[NFSPROC_CREATE]++;
	nfsm_reqhead(nfs_procids[NFSPROC_CREATE], ndp->ni_cred,
	  NFSX_FH+NFSX_UNSIGNED+nfsm_rndup(ndp->ni_dent.d_namlen)+NFSX_SATTR);
	nfsm_fhtom(ndp->ni_dvp);
	nfsm_strtom(ndp->ni_dent.d_name, ndp->ni_dent.d_namlen, NFS_MAXNAMLEN);
	nfsm_build(sp, struct nfsv2_sattr *, NFSX_SATTR);
	sp->sa_mode = vtonfs_mode(VREG, vap->va_mode);
	sp->sa_uid = txdr_unsigned(ndp->ni_cred->cr_uid);
	sp->sa_gid = txdr_unsigned(ndp->ni_cred->cr_gid);
	sp->sa_size = txdr_unsigned(0);
	/* or should these be VNOVAL ?? */
	txdr_time(&vap->va_atime, &sp->sa_atime);
	txdr_time(&vap->va_mtime, &sp->sa_mtime);
	nfsm_request(ndp->ni_dvp);
	nfsm_mtofh(ndp->ni_dvp, ndp->ni_vp);
	nfsm_reqdone;
	nfs_nput(ndp->ni_dvp);
	return (error);
}

/*
 * nfs file remove call
 * To try and make nfs semantics closer to vfs semantics, a file that has
 * other references to the vnode is renamed instead of removed and then
 * removed later on the last close.
 * Unfortunately you must flush the buffer cache and cmap to get rid of
 * all extraneous vnode references before you check the reference cnt.
 * 1 - If the file could have blocks in the buffer cache
 *	  flush them out and invalidate them
 *	  mpurge the vnode to flush out cmap references
 *	  (This is necessary to update the vnode ref cnt as well as sensible
 *	   for actual removes, to free up the buffers)
 * 2 - If v_count > 1
 *	  If a rename is not already in the works
 *	     call nfs_sillyrename() to set it up
 *     else
 *	  do the remove rpc
 */
nfs_remove(ndp)
	register struct nameidata *ndp;
{
	register struct vnode *vp = ndp->ni_vp;
	register struct nfsnode *np = VTONFS(ndp->ni_vp);
	nfsm_vars;

	if (vp->v_type == VREG) {
		if (np->n_flag & (NMODIFIED|NBUFFERED)) {
			np->n_flag &= ~(NMODIFIED|NBUFFERED);
			nfs_blkflush(vp, (daddr_t)0, np->n_size, TRUE);
		}
		if (np->n_flag & NPAGEDON)
			mpurge(vp);	/* In case cmap entries still ref it */
	}
	if (vp->v_count > 1) {
		if (!np->n_sillyrename)
			error = nfs_sillyrename(ndp, REMOVE);
	} else {
		nfsstats.rpccnt[NFSPROC_REMOVE]++;
		nfsm_reqhead(nfs_procids[NFSPROC_REMOVE], ndp->ni_cred,
			NFSX_FH+NFSX_UNSIGNED+nfsm_rndup(ndp->ni_dent.d_namlen));
		nfsm_fhtom(ndp->ni_dvp);
		nfsm_strtom(ndp->ni_dent.d_name, ndp->ni_dent.d_namlen, NFS_MAXNAMLEN);
		nfsm_request(ndp->ni_dvp);
		nfsm_reqdone;
	}
	if (ndp->ni_dvp == ndp->ni_vp)
		vrele(ndp->ni_vp);
	else
		nfs_nput(ndp->ni_vp);
	nfs_nput(ndp->ni_dvp);
	return (error);
}

/*
 * nfs file remove rpc called from nfs_inactive
 */
nfs_removeit(ndp)
	register struct nameidata *ndp;
{
	nfsm_vars;

	nfsstats.rpccnt[NFSPROC_REMOVE]++;
	nfsm_reqhead(nfs_procids[NFSPROC_REMOVE], ndp->ni_cred,
		NFSX_FH+NFSX_UNSIGNED+nfsm_rndup(ndp->ni_dent.d_namlen));
	nfsm_fhtom(ndp->ni_dvp);
	nfsm_strtom(ndp->ni_dent.d_name, ndp->ni_dent.d_namlen, NFS_MAXNAMLEN);
	nfsm_request(ndp->ni_dvp);
	nfsm_reqdone;
	return (error);
}

/*
 * nfs file rename call
 */
nfs_rename(sndp, tndp)
	register struct nameidata *sndp, *tndp;
{
	nfsm_vars;

	nfsstats.rpccnt[NFSPROC_RENAME]++;
	nfsm_reqhead(nfs_procids[NFSPROC_RENAME], tndp->ni_cred,
		(NFSX_FH+NFSX_UNSIGNED)*2+nfsm_rndup(sndp->ni_dent.d_namlen)+
		nfsm_rndup(tndp->ni_dent.d_namlen)); /* or sndp->ni_cred?*/
	nfsm_fhtom(sndp->ni_dvp);
	nfsm_strtom(sndp->ni_dent.d_name,sndp->ni_dent.d_namlen,NFS_MAXNAMLEN);
	nfsm_fhtom(tndp->ni_dvp);
	nfsm_strtom(tndp->ni_dent.d_name,tndp->ni_dent.d_namlen,NFS_MAXNAMLEN);
	nfsm_request(sndp->ni_dvp);
	nfsm_reqdone;
#ifndef notyet
	if (sndp->ni_vp->v_type == VDIR) {
		if (tndp->ni_vp != NULL && tndp->ni_vp->v_type == VDIR)
			cache_purge(tndp->ni_dvp);
		cache_purge(sndp->ni_dvp);
	}
#endif
	nfs_abortop(sndp);
	nfs_abortop(tndp);
	return (error);
}

/*
 * nfs file rename rpc called from above
 */
nfs_renameit(sndp, tndp)
	register struct nameidata *sndp, *tndp;
{
	nfsm_vars;

	nfsstats.rpccnt[NFSPROC_RENAME]++;
	nfsm_reqhead(nfs_procids[NFSPROC_RENAME], tndp->ni_cred,
		(NFSX_FH+NFSX_UNSIGNED)*2+nfsm_rndup(sndp->ni_dent.d_namlen)+
		nfsm_rndup(tndp->ni_dent.d_namlen)); /* or sndp->ni_cred?*/
	nfsm_fhtom(sndp->ni_dvp);
	nfsm_strtom(sndp->ni_dent.d_name,sndp->ni_dent.d_namlen,NFS_MAXNAMLEN);
	nfsm_fhtom(tndp->ni_dvp);
	nfsm_strtom(tndp->ni_dent.d_name,tndp->ni_dent.d_namlen,NFS_MAXNAMLEN);
	nfsm_request(sndp->ni_dvp);
	nfsm_reqdone;
	return (error);
}

/*
 * nfs hard link create call
 */
nfs_link(vp, ndp)
	struct vnode *vp;
	register struct nameidata *ndp;
{
	nfsm_vars;

	if (ndp->ni_dvp != vp)
		nfs_lock(vp);
	nfsstats.rpccnt[NFSPROC_LINK]++;
	nfsm_reqhead(nfs_procids[NFSPROC_LINK], ndp->ni_cred,
		NFSX_FH*2+NFSX_UNSIGNED+nfsm_rndup(ndp->ni_dent.d_namlen));
	nfsm_fhtom(vp);
	nfsm_fhtom(ndp->ni_dvp);
	nfsm_strtom(ndp->ni_dent.d_name, ndp->ni_dent.d_namlen, NFS_MAXNAMLEN);
	nfsm_request(vp);
	nfsm_reqdone;
	if (ndp->ni_dvp != vp)
		nfs_unlock(vp);
	nfs_nput(ndp->ni_dvp);
	return (error);
}

/*
 * nfs symbolic link create call
 */
nfs_symlink(ndp, vap, nm)
	struct nameidata *ndp;
	struct vattr *vap;
	char *nm;		/* is this the path ?? */
{
	register struct nfsv2_sattr *sp;
	nfsm_vars;

	nfsstats.rpccnt[NFSPROC_SYMLINK]++;
	nfsm_reqhead(nfs_procids[NFSPROC_SYMLINK], ndp->ni_cred,
	NFSX_FH+NFSX_UNSIGNED+nfsm_rndup(ndp->ni_dent.d_namlen)+NFSX_UNSIGNED);
	nfsm_fhtom(ndp->ni_dvp);
	nfsm_strtom(ndp->ni_dent.d_name, ndp->ni_dent.d_namlen, NFS_MAXNAMLEN);
	nfsm_strtom(nm, strlen(nm), NFS_MAXPATHLEN);
	nfsm_build(sp, struct nfsv2_sattr *, NFSX_SATTR);
	sp->sa_mode = vtonfs_mode(VLNK, vap->va_mode);
	sp->sa_uid = txdr_unsigned(ndp->ni_cred->cr_uid);
	sp->sa_gid = txdr_unsigned(ndp->ni_cred->cr_gid);
	sp->sa_size = txdr_unsigned(VNOVAL);
	txdr_time(&vap->va_atime, &sp->sa_atime);		/* or VNOVAL ?? */
	txdr_time(&vap->va_mtime, &sp->sa_mtime);	/* or VNOVAL ?? */
	nfsm_request(ndp->ni_dvp);
	nfsm_reqdone;
	nfs_nput(ndp->ni_dvp);
	return (error);
}

/*
 * nfs make dir call
 */
nfs_mkdir(ndp, vap)
	struct nameidata *ndp;
	struct vattr *vap;
{
	register struct nfsv2_sattr *sp;
	nfsm_vars;

	nfsstats.rpccnt[NFSPROC_MKDIR]++;
	nfsm_reqhead(nfs_procids[NFSPROC_MKDIR], ndp->ni_cred,
	  NFSX_FH+NFSX_UNSIGNED+nfsm_rndup(ndp->ni_dent.d_namlen)+NFSX_SATTR);
	nfsm_fhtom(ndp->ni_dvp);
	nfsm_strtom(ndp->ni_dent.d_name, ndp->ni_dent.d_namlen, NFS_MAXNAMLEN);
	nfsm_build(sp, struct nfsv2_sattr *, NFSX_SATTR);
	sp->sa_mode = vtonfs_mode(VDIR, vap->va_mode);
	sp->sa_uid = txdr_unsigned(ndp->ni_cred->cr_uid);
	sp->sa_gid = txdr_unsigned(ndp->ni_cred->cr_gid);
	sp->sa_size = txdr_unsigned(VNOVAL);
	txdr_time(&vap->va_atime, &sp->sa_atime);		/* or VNOVAL ?? */
	txdr_time(&vap->va_mtime, &sp->sa_mtime);	/* or VNOVAL ?? */
	nfsm_request(ndp->ni_dvp);
	nfsm_mtofh(ndp->ni_dvp, ndp->ni_vp);
	nfsm_reqdone;
	nfs_nput(ndp->ni_dvp);
	return (error);
}

/*
 * nfs remove directory call
 */
nfs_rmdir(ndp)
	register struct nameidata *ndp;
{
	nfsm_vars;

	if (ndp->ni_dvp == ndp->ni_vp) {
		vrele(ndp->ni_dvp);
		nfs_nput(ndp->ni_dvp);
		return (EINVAL);
	}
	nfsstats.rpccnt[NFSPROC_RMDIR]++;
	nfsm_reqhead(nfs_procids[NFSPROC_RMDIR], ndp->ni_cred,
		NFSX_FH+NFSX_UNSIGNED+nfsm_rndup(ndp->ni_dent.d_namlen));
	nfsm_fhtom(ndp->ni_dvp);
	nfsm_strtom(ndp->ni_dent.d_name, ndp->ni_dent.d_namlen, NFS_MAXNAMLEN);
	nfsm_request(ndp->ni_dvp);
	nfsm_reqdone;
#ifndef notyet
	cache_purge(ndp->ni_dvp);
	cache_purge(ndp->ni_vp);
#endif
	nfs_nput(ndp->ni_vp);
	nfs_nput(ndp->ni_dvp);
	return (error);
}

/*
 * nfs readdir call
 * Although cookie is defined as opaque, I translate it to/from net byte
 * order so that it looks more sensible. This appears consistent with the
 * Ultrix implementation of NFS.
 */
nfs_readdir(vp, uiop, offp, cred)
	struct vnode *vp;
	struct uio *uiop;
	off_t *offp;
	struct ucred *cred;
{
	register long len;
	register struct direct *dp;
	nfsm_vars;
	struct mbuf *md2;
	caddr_t dpos2;
	int siz;
	int more_dirs, eofflg;
	off_t off, savoff;
	struct direct *savdp;

	nfs_lock(vp);
	nfsstats.rpccnt[NFSPROC_READDIR]++;
	nfsm_reqhead(nfs_procids[NFSPROC_READDIR], cred, xid);
	nfsm_fhtom(vp);
	nfsm_build(p, u_long *, 2*NFSX_UNSIGNED);
	off = *offp;
	*p++ = txdr_unsigned(off);
	*p = txdr_unsigned(uiop->uio_resid);
	nfsm_request(vp);
	siz = 0;
	nfsm_disect(p, u_long *, NFSX_UNSIGNED);
	more_dirs = fxdr_unsigned(int, *p);

	/* Save the position so that we can do nfsm_mtouio() later */
	dpos2 = dpos;
	md2 = md;

	/* loop thru the dir entries, doctoring them to 4bsd form */
	while (more_dirs && siz < uiop->uio_resid) {
		savoff = off;		/* Hold onto offset and dp */
		savdp = dp;
		nfsm_disecton(p, u_long *, 2*NFSX_UNSIGNED);
		dp = (struct direct *)p;
		dp->d_ino = fxdr_unsigned(u_long, *p++);
		len = fxdr_unsigned(int, *p);
		if (len <= 0 || len > NFS_MAXNAMLEN) {
			error = EBADRPC;
			m_freem(mrep);
			goto nfsmout;
		}
		dp->d_namlen = (u_short)len;
		len = nfsm_rndup(len);
		nfsm_adv(len);
		nfsm_disecton(p, u_long *, 2*NFSX_UNSIGNED);
		off = fxdr_unsigned(off_t, *p);
		*p++ = 0;		/* Ensures null termination of name */
		more_dirs = fxdr_unsigned(int, *p);
		dp->d_reclen = len+4*NFSX_UNSIGNED;
		siz += dp->d_reclen;
	}
	/*
	 * If at end of rpc data, get the eof boolean
	 */
	if (!more_dirs) {
		nfsm_disecton(p, u_long *, NFSX_UNSIGNED);
		eofflg = fxdr_unsigned(long, *p);
	}
	/*
	 * If there is too much to fit in the data buffer, use savoff and
	 * savdp to trim off the last record.
	 * --> we are not at eof
	 */
	if (siz > uiop->uio_resid) {
		eofflg = FALSE;
		off = savoff;
		siz -= dp->d_reclen;
		dp = savdp;
	}
	if (siz > 0) {
#ifdef notdef
		if (!eofflg)
			dp->d_reclen += (uiop->uio_resid-siz);
#endif
		md = md2;
		dpos = dpos2;
		nfsm_mtouio(uiop, siz);
#ifdef notdef
		if (!eofflg)
			uiop->uio_resid = 0;
#endif
		*offp = off;
	}
	nfsm_reqdone;
	nfs_unlock(vp);
	return (error);
}

/*
 * nfs statfs call
 * (Actually a vfsop, not a vnode op)
 */
nfs_statfs(mp, sbp)
	struct mount *mp;
	register struct statfs *sbp;
{
	register struct nfsv2_statfs *sfp;
	nfsm_vars;
	struct nfsmount *nmp;
	struct ucred *cred;
	struct nfsnode *np;
	struct vnode *vp;

	nmp = vfs_to_nfs(mp);
	if (error = nfs_nget(mp, &nmp->nm_fh, &np))
		return (error);
	vp = NFSTOV(np);
	nfsstats.rpccnt[NFSPROC_STATFS]++;
	cred = crget();
	cred->cr_ngroups = 1;
	nfsm_reqhead(nfs_procids[NFSPROC_STATFS], cred, NFSX_FH);
	nfsm_fhtom(vp);
	nfsm_request(vp);
	nfsm_disect(sfp, struct nfsv2_statfs *, NFSX_STATFS);
	sbp->f_type = MOUNT_NFS;
	sbp->f_flags = nmp->nm_flag;
	sbp->f_bsize = fxdr_unsigned(long, sfp->sf_tsize);
	sbp->f_fsize = fxdr_unsigned(long, sfp->sf_bsize);
	sbp->f_blocks = fxdr_unsigned(long, sfp->sf_blocks);
	sbp->f_bfree = fxdr_unsigned(long, sfp->sf_bfree);
	sbp->f_bavail = fxdr_unsigned(long, sfp->sf_bavail);
	sbp->f_files = 0;
	sbp->f_ffree = 0;
	sbp->f_fsid.val[0] = mp->m_fsid.val[0];
	sbp->f_fsid.val[1] = mp->m_fsid.val[1];
	bcopy(nmp->nm_path, sbp->f_mntonname, MNAMELEN);
	bcopy(nmp->nm_host, sbp->f_mntfromname, MNAMELEN);
	nfsm_reqdone;
	nfs_nput(vp);
	crfree(cred);
	return (error);
}

#define	HEXTOASC(x)	"0123456789abcdef"[x]

/*
 * Silly rename. To make the NFS filesystem that is stateless look a little
 * more like the "ufs" a remove of an active vnode is translated to a rename
 * to a funny looking filename that is removed by nfs_inactive on the
 * nfsnode. There is the potential for another process on a different client
 * to create the same funny name between the nfs_lookitup() fails and the
 * nfs_rename() completes, but...
 */
nfs_sillyrename(ndp, flag)
	struct nameidata *ndp;
	int flag;
{
	register struct nfsnode *np;
	register struct sillyrename *sp;
	register struct nameidata *tndp;
	int error;
	short pid;

	np = VTONFS(ndp->ni_dvp);
	cache_purge(ndp->ni_dvp);
	MALLOC(sp, struct sillyrename *, sizeof (struct sillyrename),
		M_TEMP, M_WAITOK);
	sp->s_flag = flag;
	bcopy((caddr_t)&np->n_fh, (caddr_t)&sp->s_fh, NFSX_FH);
	np = VTONFS(ndp->ni_vp);
	tndp = &sp->s_namei;
	tndp->ni_cred = crdup(ndp->ni_cred);

	/* Fudge together a funny name */
	pid = u.u_procp->p_pid;
	bcopy(".nfsAxxxx4.4", tndp->ni_dent.d_name, 13);
	tndp->ni_dent.d_namlen = 12;
	tndp->ni_dent.d_name[8] = HEXTOASC(pid & 0xf);
	tndp->ni_dent.d_name[7] = HEXTOASC((pid >> 4) & 0xf);
	tndp->ni_dent.d_name[6] = HEXTOASC((pid >> 8) & 0xf);
	tndp->ni_dent.d_name[5] = HEXTOASC((pid >> 12) & 0xf);

	/* Try lookitups until we get one that isn't there */
	while (nfs_lookitup(ndp->ni_dvp, tndp, (nfsv2fh_t *)0) == 0) {
		tndp->ni_dent.d_name[4]++;
		if (tndp->ni_dent.d_name[4] > 'z') {
			error = EINVAL;
			goto bad;
		}
	}
	if (error = nfs_renameit(ndp, tndp))
		goto bad;
	nfs_lookitup(ndp->ni_dvp, tndp, &np->n_fh);
	np->n_sillyrename = sp;
	return (0);
bad:
	crfree(tndp->ni_cred);
	free((caddr_t)sp, M_TEMP);
	return (error);
}

/*
 * Look up a file name for silly rename stuff.
 * Just like nfs_lookup() except that it doesn't load returned values
 * into the nfsnode table.
 * If fhp != NULL it copies the returned file handle out
 */
nfs_lookitup(vp, ndp, fhp)
	register struct vnode *vp;
	register struct nameidata *ndp;
	nfsv2fh_t *fhp;
{
	nfsm_vars;
	long len;

	nfsstats.rpccnt[NFSPROC_LOOKUP]++;
	ndp->ni_dvp = vp;
	ndp->ni_vp = NULL;
	len = ndp->ni_dent.d_namlen;
	nfsm_reqhead(nfs_procids[NFSPROC_LOOKUP], ndp->ni_cred, NFSX_FH+NFSX_UNSIGNED+nfsm_rndup(len));
	nfsm_fhtom(vp);
	nfsm_strtom(ndp->ni_dent.d_name, len, NFS_MAXNAMLEN);
	nfsm_request(vp);
	if (fhp != NULL) {
		nfsm_disect(cp, caddr_t, NFSX_FH);
		bcopy(cp, (caddr_t)fhp, NFSX_FH);
	}
	nfsm_reqdone;
	return (error);
}

/*
 * Kludge City..
 * - make nfs_bmap() essentially a no-op that does no translation
 * - do nfs_strategy() by faking physical I/O with nfs_readit/nfs_writeit
 *   after mapping the physical addresses into Kernel Virtual space in the
 *   nfsiobuf area.
 *   (Maybe I could use the process's page mapping, but I was concerned that
 *    Kernel Write might not be enabled and also figured copyout() would do
 *    a lot more work than bcopy() and also it currently happens in the
 *    context of the swapper process (2).
 */
nfs_bmap(vp, bn, vpp, bnp)
	struct vnode *vp;
	daddr_t bn;
	struct vnode **vpp;
	daddr_t *bnp;
{
	if (vpp != NULL)
		*vpp = vp;
	if (bnp != NULL)
		*bnp = bn * btodb(vp->v_mount->m_bsize);
	return (0);
}

/*
 * Strategy routine for phys. i/o
 * If the biod's are running, queue a request
 * otherwise just call nfs_doio() to get it done
 */
nfs_strategy(bp)
	register struct buf *bp;
{
	register struct buf *dp;
	register int i;
	struct proc *rp;
	int error = 0;
	int fnd = 0;

	/*
	 * If an i/o daemon is waiting
	 * queue the request, wake it up and wait for completion
	 * otherwise just do it ourselves
	 */
	for (i = 0; i < nfs_asyncdaemons; i++) {
		if (rp = nfs_iodwant[i]) {
			/*
			 * Ensure that the async_daemon is still waiting here
			 */
			if (rp->p_stat != SSLEEP ||
			    rp->p_wchan != ((caddr_t)&nfs_iodwant[i])) {
				nfs_iodwant[i] = (struct proc *)0;
				continue;
			}
			dp = &nfs_bqueue;
			if (dp->b_actf == NULL) {
				dp->b_actl = bp;
				bp->b_actf = dp;
			} else {
				dp->b_actf->b_actl = bp;
				bp->b_actf = dp->b_actf;
			}
			dp->b_actf = bp;
			bp->b_actl = dp;
			fnd++;
			nfs_iodwant[i] = (struct proc *)0;
			wakeup((caddr_t)&nfs_iodwant[i]);
			break;
		}
	}
	if (!fnd)
		error = nfs_doio(bp);
	return (error);
}

/*
 * Fun and games with i/o
 * Essentially play ubasetup() and disk interrupt service routine by
 * mapping the data buffer into kernel virtual space and doing the
 * nfs read or write rpc's from it.
 * If the biod's are not running, this is just called from nfs_strategy(),
 * otherwise it is called by the biod's to do what would normally be
 * partially disk interrupt driven.
 */
nfs_doio(bp)
	register struct buf *bp;
{
	register struct pte *pte, *ppte;
	register caddr_t vaddr;
	register struct uio *uiop;
	register struct vnode *vp;
	struct nfsnode *np;
	struct ucred *cr;
	int npf, npf2;
	int reg;
	caddr_t vbase;
	caddr_t addr;
	unsigned v;
	struct proc *rp;
	int o, error;
	int bcnt;
	off_t off;
	struct uio uio;
	struct iovec io;

	vp = bp->b_vp;
	uiop = &uio;
	uiop->uio_iov = &io;
	uiop->uio_iovcnt = 1;
	uiop->uio_segflg = UIO_SYSSPACE;
	if (bp->b_flags & B_READ) {
		io.iov_len = uiop->uio_resid = bp->b_bcount;
		uiop->uio_offset = off = bp->b_blkno*DEV_BSIZE;
		addr = bp->b_un.b_addr;
		bcnt = bp->b_bcount;
	} else {
		io.iov_len = uiop->uio_resid = bp->b_dirtyend-bp->b_dirtyoff;
		uiop->uio_offset = off = (bp->b_blkno*DEV_BSIZE)+bp->b_dirtyoff;
		addr = bp->b_un.b_addr+bp->b_dirtyoff;
		bcnt = bp->b_dirtyend-bp->b_dirtyoff;
	}
	/*
	 * For phys i/o, map the b_addr into kernel virtual space using
	 * the Nfsiomap pte's
	 * Also, add a temporary b_rcred for reading using the process's uid
	 * and a guess at a group
	 */
	if (bp->b_flags & B_PHYS) {
		VTONFS(vp)->n_flag |= NPAGEDON;
		bp->b_rcred = cr = crget();
		rp = (bp->b_flags & B_DIRTY) ? &proc[2] : bp->b_proc;
		cr->cr_uid = rp->p_uid;
		cr->cr_gid = 0;		/* Anything ?? */
		cr->cr_ngroups = 1;
		o = (int)addr & PGOFSET;
		npf2 = npf = btoc(bcnt + o);
		/*
		 * Get some mapping page table entries
		 */
		while ((reg = rmalloc(nfsmap, (long)npf)) == 0) {
			nfsmap_want++;
			sleep((caddr_t)&nfsmap_want, PZERO-1);
		}
		reg--;
		/* I know it is always the else, but that may change someday */
		if ((bp->b_flags & B_PHYS) == 0)
			pte = kvtopte(bp->b_un.b_addr);
		else if (bp->b_flags & B_PAGET)
			pte = &Usrptmap[btokmx((struct pte *)bp->b_un.b_addr)];
		else {
			v = btop(bp->b_un.b_addr);
			if (bp->b_flags & B_UAREA)
				pte = &rp->p_addr[v];
			else
				pte = vtopte(rp, v);
		}
		/*
		 * Play vmaccess() but with the Nfsiomap page table
		 */
		ppte = &Nfsiomap[reg];
		vbase = vaddr = &nfsiobuf[reg*NBPG];
		while (npf != 0) {
			mapin(ppte, (u_int)vaddr, pte->pg_pfnum, (int)(PG_V|PG_KW));
#if defined(tahoe)
			mtpr(P1DC, vaddr);
#endif
			ppte++;
			pte++;
			vaddr += NBPG;
			--npf;
		}
		io.iov_base = vbase+o;
	} else {
		io.iov_base = addr;
	}
	if (bp->b_flags & B_READ) {
		uiop->uio_rw = UIO_READ;
		bp->b_error = error = nfs_readrpc(vp, uiop, &off, bp->b_rcred);
	} else {
		uiop->uio_rw = UIO_WRITE;
		bp->b_error = error = nfs_writerpc(vp, uiop, &off, bp->b_wcred);
		if (error) {
			np = VTONFS(vp);
			np->n_error = error;
			np->n_flag |= NWRITEERR;
		}
		bp->b_dirtyoff = bp->b_dirtyend = 0;
	}
	if (error)
		bp->b_flags |= B_ERROR;
	bp->b_resid = uiop->uio_resid;
	/*
	 * Release pte's used by physical i/o
	 */
	if (bp->b_flags & B_PHYS) {
		crfree(cr);
		rmfree(nfsmap, (long)npf2, (long)++reg);
		if (nfsmap_want) {
			nfsmap_want = 0;
			wakeup((caddr_t)&nfsmap_want);
		}
	}
	biodone(bp);
	return (error);
}

/*
 * Flush all the blocks associated with a vnode.
 * 	Walk through the buffer pool and push any dirty pages
 *	associated with the vnode.
 */
nfs_fsync(vp, fflags, cred)
	register struct vnode *vp;
	int fflags;
	struct ucred *cred;
{
	register struct nfsnode *np = VTONFS(vp);
	int error;

	nfs_lock(vp);
	if (np->n_flag & NMODIFIED) {
		np->n_flag &= ~NMODIFIED;
		error = nfs_blkflush(vp, (daddr_t)0, np->n_size, FALSE);
	}
	nfs_unlock(vp);
	return (error);
}
