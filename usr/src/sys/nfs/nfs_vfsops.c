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
 *	@(#)nfs_vfsops.c	7.7 (Berkeley) %G%
 */

#include "param.h"
#include "signal.h"
#include "user.h"
#include "proc.h"
#include "uio.h"
#include "ucred.h"
#include "../ufs/dir.h"
#include "namei.h"
#include "vnode.h"
#include "mount.h"
#include "errno.h"
#include "malloc.h"
#include "mbuf.h"
#undef	m_data
#include "socket.h"
#include "socketvar.h"
#include "../netinet/in.h"
#include "nfsv2.h"
#include "nfsnode.h"
#include "nfsmount.h"
#include "nfs.h"

#ifndef shouldbe
#include "conf.h"
#endif

/*
 * nfs vfs operations.
 */
int nfs_mount();
int nfs_start();
int nfs_unmount();
int nfs_root();
int nfs_statfs();
int nfs_sync();
int nfs_fhtovp();
int nfs_vptofh();
int nfs_init();

struct vfsops nfs_vfsops = {
	nfs_mount,
	nfs_start,
	nfs_unmount,
	nfs_root,
	nfs_statfs,
	nfs_sync,
	nfs_fhtovp,
	nfs_vptofh,
	nfs_init,
};

extern struct nfsreq nfsreqh;
static long nfs_mntid;

/*
 * Called by vfs_mountroot when nfs is going to be mounted as root
 * Not Yet (By a LONG shot)
 */
nfs_mountroot()
{
	return (ENODEV);
}

/*
 * VFS Operations.
 *
 * mount system call
 * It seems a bit dumb to copyinstr() the host and path here and then
 * bcopy() them in mountnfs(), but I wanted to detect errors before
 * doing the sockargs() call because sockargs() allocates an mbuf and
 * an error after that means that I have to release the mbuf.
 */
nfs_mount(mp, path, data, ndp)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
{
	int error;
	struct nfs_args args;
	struct mbuf *saddr;
	char pth[MNAMELEN], hst[MNAMELEN];
	int len;
	nfsv2fh_t nfh;

	if (mp->m_flag & M_UPDATE)
		return (0);
	if (error = copyin(data, (caddr_t)&args, sizeof (struct nfs_args)))
		return (error);
	if (error=copyin((caddr_t)args.fh, (caddr_t)&nfh, sizeof (nfsv2fh_t)))
		return (error);
	if (error = copyinstr(path, pth, MNAMELEN-1, &len))
		return (error);
	bzero(&pth[len], MNAMELEN-len);
	if (error = copyinstr(args.hostname, hst, MNAMELEN-1, &len))
		return (error);
	bzero(&hst[len], MNAMELEN-len);
	/* sockargs() call must be after above copyin() calls */
	if (error = sockargs(&saddr, (caddr_t)args.addr,
		sizeof (struct sockaddr_in), MT_SONAME))
		return (error);
	args.fh = &nfh;
	error = mountnfs(&args, mp, saddr, pth, hst);
	return (error);
}

/*
 * Common code for mount and mountroot
 */
mountnfs(argp, mp, saddr, pth, hst)
	register struct nfs_args *argp;
	register struct mount *mp;
	register struct mbuf *saddr;
	char *pth, *hst;
{
	register struct nfsmount *nmp;
	struct nfsnode *np;
#ifdef notdef
	struct statfs statf, *sbp;
#endif
	int error;

	nmp = (struct nfsmount *)malloc(sizeof (struct nfsmount), M_NFSMNT,
	    M_WAITOK);
	mp->m_data = (qaddr_t)nmp;
	mp->m_fsid.val[0] = ++nfs_mntid;
	mp->m_fsid.val[1] = MOUNT_NFS;
	nmp->nm_mountp = mp;
	nmp->nm_flag = argp->flags;
	nmp->nm_sockaddr = saddr;
	/* Set up the sockets */
	if (error = socreate(AF_INET, &nmp->nm_so, SOCK_DGRAM, 0))
		goto bad;
	if (error = soconnect(nmp->nm_so, saddr))
		goto bad;
	if ((argp->flags & NFSMNT_TIMEO) && argp->timeo >= 1)
		nmp->nm_timeo = argp->timeo;
	else
		nmp->nm_timeo = NFS_TIMEO;
	if ((argp->flags & NFSMNT_RETRANS) && argp->retrans > 0)
		nmp->nm_retrans = argp->retrans;
	else
		nmp->nm_retrans = NFS_RETRANS;
	if ((argp->flags & NFSMNT_WSIZE) &&
	    argp->wsize <= NFS_MAXDATA && argp->wsize > 0 &&
	   (argp->wsize & 0x1ff) == 0)
		nmp->nm_wsize = argp->wsize;
	else
		nmp->nm_wsize = NFS_WSIZE;
	if ((argp->flags & NFSMNT_RSIZE) &&
	    argp->rsize <= NFS_MAXDATA && argp->rsize > 0 &&
	   (argp->rsize & 0x1ff) == 0)
		nmp->nm_rsize = argp->rsize;
	else
		nmp->nm_rsize = NFS_RSIZE;
	bcopy((caddr_t)argp->fh, (caddr_t)&nmp->nm_fh, sizeof(nfsv2fh_t));
	bcopy(pth, nmp->nm_path, MNAMELEN);
	bcopy(hst, nmp->nm_host, MNAMELEN);
#ifdef notdef
	sbp = &statf;
	/*
	 * Kludge City...
	 * To do an interruptable hard mount, turn it into a soft mount
	 * with a retry limit of one and then repeat it so long as it
	 * times out and there is no pending signal for the process.
	 * It is tempting to just let nfs_statfs() sleep at positive
	 * prio, but then you would long jump out without taking the
	 * mount structure back out of the list.
	 * NB: NFSMNT_INT must NEVER be set for nfs_mountroot(), since
	 * the process context is not yet built!!
	 */
	if ((argp->flags && NFSMNT_INT) && (argp->flags & NFSMNT_SOFT) == 0) {
		int savretrans;

		nmp->nm_flag |= NFSMNT_SOFT;
		savretrans = nmp->nm_retrans;
		nmp->nm_retrans = 1;
		do {
			error = nfs_statfs(mp, sbp);
		} while (error == ETIMEDOUT && (u.u_procp->p_sig &
			(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGTERM)|
			 sigmask(SIGKILL))) == 0);
		nmp->nm_retrans = savretrans;
		nmp->nm_flag &= ~NFSMNT_SOFT;
		if (error)
			goto bad;
	} else if (error = nfs_statfs(mp, sbp))
		goto bad;
	mp->m_fsize = sbp->f_fsize;

	/*
	 * If the block size is not an exact multiple of CLBYTES
	 * use CLBYTES so that paging in ZMAGIC executables doesn't
	 * get sick. (It is used by vinitfod())
	 */
	if (sbp->f_bsize >= CLBYTES && claligned(sbp->f_bsize))
		mp->m_bsize = sbp->f_bsize;
	else
		mp->m_bsize = CLBYTES;
#else
	/*
	 * Set to CLBYTES so that vinifod() doesn't get confused.
	 * Actually any exact multiple of CLBYTES will do
	 */
	mp->m_bsize = mp->m_fsize = CLBYTES;
#endif
	return (0);
bad:
	m_freem(saddr);
	free((caddr_t)nmp, M_NFSMNT);
	return (error);
}

/*
 * unmount system call
 */
nfs_unmount(mp, flags)
	struct mount *mp;
	int flags;
{
	register struct nfsmount *nmp;
	register struct nfsreq *rep;
	struct nfsreq *rep2;
	int error;
	int s;

	if (flags & MNT_FORCE)
		return (EINVAL);
	nmp = vfs_to_nfs(mp);
	/*
	 * Clear out the buffer cache
	 */
	bflush(mp);
	if (binval(mp))
		return (EBUSY);
	/*
	 * Goes something like this..
	 * - Call nfs_nflush() to clear out the nfsnode table
	 * - Flush out lookup cache
	 * - Close the socket
	 * - Free up the data structures
	 */
	if (error = nfs_nflush(mp)) {
		return (error);
	}
	/*
	 * Scan the request list for any requests left hanging about
	 */
	s = splnet();
	rep = nfsreqh.r_next;
	while (rep && rep != &nfsreqh) {
		if (rep->r_mntp == nmp) {
			rep->r_prev->r_next = rep2 = rep->r_next;
			rep->r_next->r_prev = rep->r_prev;
			m_freem(rep->r_mreq);
			if (rep->r_mrep != NULL)
				m_freem(rep->r_mrep);
			free((caddr_t)rep, M_NFSREQ);
			rep = rep2;
		} else
			rep = rep->r_next;
	}
	splx(s);
	soclose(nmp->nm_so);
	m_freem(nmp->nm_sockaddr);
	free((caddr_t)nmp, M_NFSMNT);
	return (0);
}

/*
 * Return root of a filesystem
 */
nfs_root(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{
	register struct vnode *vp;
	struct nfsmount *nmp;
	struct nfsnode *np;
	int error;

	nmp = vfs_to_nfs(mp);
	if (error = nfs_nget(mp, &nmp->nm_fh, &np))
		return (error);
	vp = NFSTOV(np);
	vp->v_type = VDIR;
	vp->v_flag = VROOT;
	*vpp = vp;
	return (0);
}

extern int syncprt;

/*
 * Flush out the buffer cache
 */
nfs_sync(mp, waitfor)
	struct mount *mp;
	int waitfor;
{
	if (syncprt)
		bufstats();
	/*
	 * Force stale buffer cache information to be flushed.
	 */
	bflush(mp);
	return (0);
}

/*
 * At this point, this should never happen
 */
nfs_fhtovp(mp, fhp, vpp)
	struct mount *mp;
	struct fid *fhp;
	struct vnode **vpp;
{
	return (EINVAL);
}

/*
 * Vnode pointer to File handle, should never happen either
 */
nfs_vptofh(mp, fhp, vpp)
	struct mount *mp;
	struct fid *fhp;
	struct vnode **vpp;
{
	return (EINVAL);
}

/*
 * Vfs start routine, a no-op.
 */
nfs_start(mp, flags)
	struct mount *mp;
	int flags;
{
	return (0);
}
