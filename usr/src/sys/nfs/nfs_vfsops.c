/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfs_vfsops.c	7.28 (Berkeley) %G%
 */

#include "param.h"
#include "conf.h"
#include "ioctl.h"
#include "signal.h"
#include "proc.h"
#include "namei.h"
#include "vnode.h"
#include "mount.h"
#include "buf.h"
#include "mbuf.h"
#include "socket.h"
#include "systm.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../netinet/in.h"

#include "nfsv2.h"
#include "nfsnode.h"
#include "nfsmount.h"
#include "nfs.h"
#include "xdr_subs.h"
#include "nfsm_subs.h"
#include "nfsdiskless.h"

/*
 * nfs vfs operations.
 */
struct vfsops nfs_vfsops = {
	nfs_mount,
	nfs_start,
	nfs_unmount,
	nfs_root,
	nfs_quotactl,
	nfs_statfs,
	nfs_sync,
	nfs_fhtovp,
	nfs_vptofh,
	nfs_init,
};

static u_char nfs_mntid;
extern u_long nfs_procids[NFS_NPROCS];
extern u_long nfs_prog, nfs_vers;
struct nfs_diskless nfs_diskless;
void nfs_disconnect();

#define TRUE	1
#define	FALSE	0

/*
 * nfs statfs call
 */
nfs_statfs(mp, sbp, p)
	struct mount *mp;
	register struct statfs *sbp;
	struct proc *p;
{
	register struct vnode *vp;
	register struct nfsv2_statfs *sfp;
	register caddr_t cp;
	register long t1;
	caddr_t bpos, dpos, cp2;
	u_long xid;
	int error = 0;
	struct mbuf *mreq, *mrep, *md, *mb, *mb2;
	struct nfsmount *nmp;
	struct ucred *cred;
	struct nfsnode *np;

	nmp = VFSTONFS(mp);
	if (error = nfs_nget(mp, &nmp->nm_fh, &np))
		return (error);
	vp = NFSTOV(np);
	nfsstats.rpccnt[NFSPROC_STATFS]++;
	cred = crget();
	cred->cr_ngroups = 1;
	nfsm_reqhead(nfs_procids[NFSPROC_STATFS], cred, NFSX_FH);
	nfsm_fhtom(vp);
	nfsm_request(vp, NFSPROC_STATFS, p, 0);
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
	if (sbp != &mp->mnt_stat) {
		bcopy(mp->mnt_stat.f_mntonname, sbp->f_mntonname, MNAMELEN);
		bcopy(mp->mnt_stat.f_mntfromname, sbp->f_mntfromname, MNAMELEN);
	}
	nfsm_reqdone;
	nfs_nput(vp);
	crfree(cred);
	return (error);
}

/*
 * Mount a remote root fs via. nfs. This depends on the info in the
 * nfs_diskless structure that has been filled in properly by some primary
 * bootstrap.
 * It goes something like this:
 * - do enough of "ifconfig" by calling ifioctl() so that the system
 *   can talk to the server
 * - If nfs_diskless.mygateway is filled in, use that address as
 *   a default gateway.
 *   (This is done the 4.3 way with rtioctl() and should be changed)
 * - hand craft the swap nfs vnode hanging off a fake mount point
 * - build the rootfs mount point and call mountnfs() to do the rest.
 */
nfs_mountroot()
{
	register struct mount *mp;
	register struct mbuf *m;
	struct socket *so;
	struct vnode *vp;
	int error;

	/*
	 * Do enough of ifconfig(8) so that critical net interface can
	 * talk to the server.
	 */
	if (socreate(nfs_diskless.myif.ifra_addr.sa_family, &so, SOCK_DGRAM, 0))
		panic("nfs ifconf");
	if (ifioctl(so, SIOCAIFADDR, &nfs_diskless.myif))
		panic("nfs ifconf2");
	soclose(so);

	/*
	 * If the gateway field is filled in, set it as the default route.
	 */
#ifdef COMPAT_43
	if (nfs_diskless.mygateway.sa_family == AF_INET) {
		struct ortentry rt;
		struct sockaddr_in *sin;

		sin = (struct sockaddr_in *) &rt.rt_dst;
		sin->sin_len = sizeof (struct sockaddr_in);
		sin->sin_family = AF_INET;
		sin->sin_addr.s_addr = 0;	/* default */
		bcopy((caddr_t)&nfs_diskless.mygateway, (caddr_t)&rt.rt_gateway,
			sizeof (struct sockaddr_in));
		rt.rt_flags = (RTF_UP | RTF_GATEWAY);
		if (rtioctl(SIOCADDRT, (caddr_t)&rt))
			panic("nfs root route");
	}
#endif	/* COMPAT_43 */

	/*
	 * If swapping to an nfs node (indicated by swdevt[0].sw_dev == NODEV):
	 * Create a fake mount point just for the swap vnode so that the
	 * swap file can be on a different server from the rootfs.
	 */
	if (swdevt[0].sw_dev == NODEV) {
		mp = (struct mount *)malloc((u_long)sizeof(struct mount),
			M_MOUNT, M_NOWAIT);
		if (mp == NULL)
			panic("nfs root mount");
		mp->mnt_op = &nfs_vfsops;
		mp->mnt_flag = 0;
		mp->mnt_exroot = 0;
		mp->mnt_mounth = NULLVP;
	
		/*
		 * Set up the diskless nfs_args for the swap mount point
		 * and then call mountnfs() to mount it.
		 * Since the swap file is not the root dir of a file system,
		 * hack it to a regular file.
		 */
		nfs_diskless.swap_args.fh = (nfsv2fh_t *)nfs_diskless.swap_fh;
		MGET(m, MT_SONAME, M_DONTWAIT);
		if (m == NULL)
			panic("nfs root mbuf");
		bcopy((caddr_t)&nfs_diskless.swap_saddr, mtod(m, caddr_t),
			nfs_diskless.swap_saddr.sa_len);
		m->m_len = nfs_diskless.swap_saddr.sa_len;
		if (mountnfs(&nfs_diskless.swap_args, mp, m, "/swap",
			nfs_diskless.swap_hostnam, &vp))
			panic("nfs swap");
		vp->v_type = VREG;
		vp->v_flag = 0;
		swapdev_vp = vp;
		VREF(vp);
		swdevt[0].sw_vp = vp;
		VREF(vp);
		argdev_vp = vp;
	}

	/*
	 * Create the rootfs mount point.
	 */
	mp = (struct mount *)malloc((u_long)sizeof(struct mount),
		M_MOUNT, M_NOWAIT);
	if (mp == NULL)
		panic("nfs root mount2");
	mp->mnt_op = &nfs_vfsops;
	mp->mnt_flag = MNT_RDONLY;
	mp->mnt_exroot = 0;
	mp->mnt_mounth = NULLVP;

	/*
	 * Set up the root fs args and call mountnfs() to do the rest.
	 */
	nfs_diskless.root_args.fh = (nfsv2fh_t *)nfs_diskless.root_fh;
	MGET(m, MT_SONAME, M_DONTWAIT);
	if (m == NULL)
		panic("nfs root mbuf2");
	bcopy((caddr_t)&nfs_diskless.root_saddr, mtod(m, caddr_t),
		nfs_diskless.root_saddr.sa_len);
	m->m_len = nfs_diskless.root_saddr.sa_len;
	if (mountnfs(&nfs_diskless.root_args, mp, m, "/",
		nfs_diskless.root_hostnam, &vp))
		panic("nfs root");
	if (vfs_lock(mp))
		panic("nfs root2");
	rootfs = mp;
	mp->mnt_next = mp;
	mp->mnt_prev = mp;
	mp->mnt_vnodecovered = NULLVP;
	vfs_unlock(mp);
	rootvp = vp;
	inittodr((time_t)0);	/* There is no time in the nfs fsstat so ?? */
	return (0);
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
/* ARGSUSED */
nfs_mount(mp, path, data, ndp, p)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
	struct proc *p;
{
	int error;
	struct nfs_args args;
	struct mbuf *nam;
	struct vnode *vp;
	char pth[MNAMELEN], hst[MNAMELEN];
	int len;
	nfsv2fh_t nfh;

	if (mp->mnt_flag & MNT_UPDATE)
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
	if (error = sockargs(&nam, (caddr_t)args.addr,
		sizeof (struct sockaddr), MT_SONAME))
		return (error);
	args.fh = &nfh;
	error = mountnfs(&args, mp, nam, pth, hst, &vp);
	return (error);
}

/*
 * Common code for mount and mountroot
 */
mountnfs(argp, mp, nam, pth, hst, vpp)
	register struct nfs_args *argp;
	register struct mount *mp;
	struct mbuf *nam;
	char *pth, *hst;
	struct vnode **vpp;
{
	register struct nfsmount *nmp;
	struct proc *p = curproc;		/* XXX */
	struct nfsnode *np;
	int error;
	fsid_t tfsid;

	MALLOC(nmp, struct nfsmount *, sizeof *nmp, M_NFSMNT, M_WAITOK);
	bzero((caddr_t)nmp, sizeof *nmp);
	mp->mnt_data = (qaddr_t)nmp;
	/*
	 * Generate a unique nfs mount id. The problem is that a dev number
	 * is not unique across multiple systems. The techique is as follows:
	 * 1) Set to nblkdev,0 which will never be used otherwise
	 * 2) Generate a first guess as nblkdev,nfs_mntid where nfs_mntid is
	 *	NOT 0
	 * 3) Loop searching the mount list for another one with same id
	 *	If a match, increment val[0] and try again
	 * NB: I increment val[0] { a long } instead of nfs_mntid { a u_char }
	 *	so that nfs is not limited to 255 mount points
	 *     Incrementing the high order bits does no real harm, since it
	 *     simply makes the major dev number tick up. The upper bound is
	 *     set to major dev 127 to avoid any sign extention problems
	 */
	mp->mnt_stat.f_fsid.val[0] = makedev(nblkdev, 0);
	mp->mnt_stat.f_fsid.val[1] = MOUNT_NFS;
	if (++nfs_mntid == 0)
		++nfs_mntid;
	tfsid.val[0] = makedev(nblkdev, nfs_mntid);
	tfsid.val[1] = MOUNT_NFS;
	while (rootfs && getvfs(&tfsid)) {
		tfsid.val[0]++;
		nfs_mntid++;
	}
	if (major(tfsid.val[0]) > 127) {
		error = ENOENT;
		goto bad;
	}
	mp->mnt_stat.f_fsid.val[0] = tfsid.val[0];
	nmp->nm_mountp = mp;
	nmp->nm_flag = argp->flags;
	nmp->nm_rto = NFS_TIMEO;
	nmp->nm_rtt = -1;
	nmp->nm_rttvar = nmp->nm_rto << 1;
	nmp->nm_retry = NFS_RETRANS;
	nmp->nm_wsize = NFS_WSIZE;
	nmp->nm_rsize = NFS_RSIZE;
	bcopy((caddr_t)argp->fh, (caddr_t)&nmp->nm_fh, sizeof(nfsv2fh_t));
	mp->mnt_stat.f_type = MOUNT_NFS;
	bcopy(hst, mp->mnt_stat.f_mntfromname, MNAMELEN);
	bcopy(pth, mp->mnt_stat.f_mntonname, MNAMELEN);
	nmp->nm_nam = nam;

	if ((argp->flags & NFSMNT_TIMEO) && argp->timeo > 0) {
		nmp->nm_rto = argp->timeo;
		/* NFS timeouts are specified in 1/10 sec. */
		nmp->nm_rto = (nmp->nm_rto * 10) / NFS_HZ;
		if (nmp->nm_rto < NFS_MINTIMEO)
			nmp->nm_rto = NFS_MINTIMEO;
		else if (nmp->nm_rto > NFS_MAXTIMEO)
			nmp->nm_rto = NFS_MAXTIMEO;
		nmp->nm_rttvar = nmp->nm_rto << 1;
	}

	if ((argp->flags & NFSMNT_RETRANS) && argp->retrans > 1) {
		nmp->nm_retry = argp->retrans;
		if (nmp->nm_retry > NFS_MAXREXMIT)
			nmp->nm_retry = NFS_MAXREXMIT;
	}

	if ((argp->flags & NFSMNT_WSIZE) && argp->wsize > 0) {
		nmp->nm_wsize = argp->wsize;
		/* Round down to multiple of blocksize */
		nmp->nm_wsize &= ~0x1ff;
		if (nmp->nm_wsize <= 0)
			nmp->nm_wsize = 512;
		else if (nmp->nm_wsize > NFS_MAXDATA)
			nmp->nm_wsize = NFS_MAXDATA;
	}
	if (nmp->nm_wsize > MAXBSIZE)
		nmp->nm_wsize = MAXBSIZE;

	if ((argp->flags & NFSMNT_RSIZE) && argp->rsize > 0) {
		nmp->nm_rsize = argp->rsize;
		/* Round down to multiple of blocksize */
		nmp->nm_rsize &= ~0x1ff;
		if (nmp->nm_rsize <= 0)
			nmp->nm_rsize = 512;
		else if (nmp->nm_rsize > NFS_MAXDATA)
			nmp->nm_rsize = NFS_MAXDATA;
	}
	if (nmp->nm_rsize > MAXBSIZE)
		nmp->nm_rsize = MAXBSIZE;
	/* Set up the sockets and per-host congestion */
	nmp->nm_sotype = argp->sotype;
	nmp->nm_soproto = argp->proto;
	if (error = nfs_connect(nmp))
		goto bad;

	if (error = nfs_statfs(mp, &mp->mnt_stat, p))
		goto bad;
	/*
	 * A reference count is needed on the nfsnode representing the
	 * remote root.  If this object is not persistent, then backward
	 * traversals of the mount point (i.e. "..") will not work if
	 * the nfsnode gets flushed out of the cache. Ufs does not have
	 * this problem, because one can identify root inodes by their
	 * number == ROOTINO (2).
	 */
	if (error = nfs_nget(mp, &nmp->nm_fh, &np))
		goto bad;
	/*
	 * Unlock it, but keep the reference count.
	 */
	nfs_unlock(NFSTOV(np));
	*vpp = NFSTOV(np);

	return (0);
bad:
	nfs_disconnect(nmp);
	FREE(nmp, M_NFSMNT);
	m_freem(nam);
	return (error);
}

/*
 * unmount system call
 */
nfs_unmount(mp, mntflags, p)
	struct mount *mp;
	int mntflags;
	struct proc *p;
{
	register struct nfsmount *nmp;
	struct nfsnode *np;
	struct vnode *vp;
	int flags = 0;
	int error;

	if (mntflags & MNT_FORCE) {
		if (mp == rootfs)
			return (EINVAL);
		flags |= FORCECLOSE;
	}
	nmp = VFSTONFS(mp);
	/*
	 * Clear out the buffer cache
	 */
	mntflushbuf(mp, 0);
	if (mntinvalbuf(mp))
		return (EBUSY);
	/*
	 * Goes something like this..
	 * - Check for activity on the root vnode (other than ourselves).
	 * - Call vflush() to clear out vnodes for this file system,
	 *   except for the root vnode.
	 * - Decrement reference on the vnode representing remote root.
	 * - Close the socket
	 * - Free up the data structures
	 */
	/*
	 * We need to decrement the ref. count on the nfsnode representing
	 * the remote root.  See comment in mountnfs().  The VFS unmount()
	 * has done vput on this vnode, otherwise we would get deadlock!
	 */
	if (error = nfs_nget(mp, &nmp->nm_fh, &np))
		return(error);
	vp = NFSTOV(np);
	if (vp->v_usecount > 2) {
		vput(vp);
		return (EBUSY);
	}
	if (error = vflush(mp, vp, flags)) {
		vput(vp);
		return (error);
	}
	/*
	 * Get rid of two reference counts, and unlock it on the second.
	 */
	vrele(vp);
	vput(vp);
	nfs_disconnect(nmp);
	m_freem(nmp->nm_nam);
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

	nmp = VFSTONFS(mp);
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
/* ARGSUSED */
nfs_sync(mp, waitfor)
	struct mount *mp;
	int waitfor;
{
	if (syncprt)
		bufstats();
	/*
	 * Force stale buffer cache information to be flushed.
	 */
	mntflushbuf(mp, waitfor == MNT_WAIT ? B_SYNC : 0);
	return (0);
}

/*
 * At this point, this should never happen
 */
/* ARGSUSED */
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
/* ARGSUSED */
nfs_vptofh(vp, fhp)
	struct vnode *vp;
	struct fid *fhp;
{

	return (EINVAL);
}

/*
 * Vfs start routine, a no-op.
 */
/* ARGSUSED */
nfs_start(mp, flags, p)
	struct mount *mp;
	int flags;
	struct proc *p;
{

	return (0);
}

/*
 * Do operations associated with quotas, not supported
 */
nfs_quotactl(mp, cmd, uid, arg, p)
	struct mount *mp;
	int cmd;
	uid_t uid;
	caddr_t arg;
	struct proc *p;
{
#ifdef lint
	mp = mp; cmd = cmd; uid = uid; arg = arg;
#endif /* lint */
	return (EOPNOTSUPP);
}
