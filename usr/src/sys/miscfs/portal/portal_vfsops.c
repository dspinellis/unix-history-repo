/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)portal_vfsops.c	8.2 (Berkeley) %G%
 *
 * $Id: portal_vfsops.c,v 1.5 1992/05/30 10:25:27 jsp Exp jsp $
 */

/*
 * Portal Filesystem
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/proc.h>
#include <sys/filedesc.h>
#include <sys/file.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/protosw.h>
#include <sys/domain.h>
#include <sys/un.h>
#include <miscfs/portal/portal.h>

static u_short portal_mntid;

int portal_init()
{

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_init\n");		/* printed during system boot */
#endif
}

/*
 * Mount the per-process file descriptors (/dev/fd)
 */
portal_mount(mp, path, data, ndp, p)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
	struct proc *p;
{
	int error = 0;
	struct portal_args args;
	u_int size;
	struct portalmount *fmp;
	struct vnode *rvp;
	struct sockaddr_un *unp;
	struct file *fp;
	struct socket *so;
	char cfile[MAXPATHLEN];

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_mount(mp = %x)\n", mp);
#endif

	/*
	 * Update is a no-op
	 */
	if (mp->mnt_flag & MNT_UPDATE)
		return (EOPNOTSUPP);

	if (error = copyin(data, (caddr_t) &args, sizeof(struct portal_args)))
		return (error);

	if (error = getsock(p->p_fd, args.pa_socket, &fp))
		return (error);
	so = (struct socket *) fp->f_data;
	if (so->so_proto->pr_domain->dom_family != AF_UNIX)
		return (ESOCKTNOSUPPORT);

	error = getnewvnode(VT_PORTAL, mp, portal_vnodeop_p, &rvp); /* XXX */
	if (error)
		return (error);
	MALLOC(rvp->v_data, void *, sizeof(struct portalnode),
		M_TEMP, M_WAITOK);

	fmp = (struct portalmount *) malloc(sizeof(struct portalmount),
				 M_UFSMNT, M_WAITOK);	/* XXX */
	rvp->v_type = VDIR;
	rvp->v_flag |= VROOT;
	VTOPORTAL(rvp)->pt_arg = 0;
	VTOPORTAL(rvp)->pt_size = 0;
	VTOPORTAL(rvp)->pt_fileid = PORTAL_ROOTFILEID;
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_mount: root vp = %x\n", rvp);
#endif
	fmp->pm_root = rvp;
	fmp->pm_server = fp; fp->f_count++;

	mp->mnt_flag |= MNT_LOCAL;
	mp->mnt_data = (qaddr_t) fmp;
	getnewfsid(mp, MOUNT_PORTAL);

	(void) copyinstr(path, mp->mnt_stat.f_mntonname, MNAMELEN - 1, &size);
	bzero(mp->mnt_stat.f_mntonname + size, MNAMELEN - size);
	(void) copyinstr(args.pa_config, mp->mnt_stat.f_mntfromname, MNAMELEN - 1,
		&size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);

#ifdef notdef
	bzero(mp->mnt_stat.f_mntfromname, MNAMELEN);
	bcopy("portal", mp->mnt_stat.f_mntfromname, sizeof("portal"));
#endif

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_mount: config %s at %s\n",
			mp->mnt_stat.f_mntfromname, mp->mnt_stat.f_mntonname);
#endif
	return (0);
}

portal_start(mp, flags, p)
	struct mount *mp;
	int flags;
	struct proc *p;
{

	return (0);
}

portal_unmount(mp, mntflags, p)
	struct mount *mp;
	int mntflags;
	struct proc *p;
{
	int error;
	int flags = 0;
	extern int doforce;
	struct vnode *rootvp = VFSTOPORTAL(mp)->pm_root;

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_unmount(mp = %x)\n", mp);
#endif

	if (mntflags & MNT_FORCE) {
		/* portal can never be rootfs so don't check for it */
		if (!doforce)
			return (EINVAL);
		flags |= FORCECLOSE;
	}

	/*
	 * Clear out buffer cache.  I don't think we
	 * ever get anything cached at this level at the
	 * moment, but who knows...
	 */
#ifdef notyet
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_unmount: calling mntflushbuf\n");
#endif
	mntflushbuf(mp, 0); 
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_unmount: calling mntinvalbuf\n");
#endif
	if (mntinvalbuf(mp, 1))
		return (EBUSY);
#endif
	if (rootvp->v_usecount > 1)
		return (EBUSY);
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_unmount: calling vflush\n");
#endif
	if (error = vflush(mp, rootvp, flags))
		return (error);

#ifdef PORTAL_DIAGNOSTIC
	vprint("portal root", rootvp);
#endif	 
	/*
	 * Release reference on underlying root vnode
	 */
	vrele(rootvp);
	/*
	 * And blow it away for future re-use
	 */
	vgone(rootvp);
	/*
	 * Shutdown the socket.  This will cause the select in the
	 * daemon to wake up, and then the accept will get ECONNABORTED
	 * which it interprets as a request to go and bury itself.
	 */
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_unmount: shutdown socket\n");
#endif	 
	soshutdown((struct socket *) VFSTOPORTAL(mp)->pm_server->f_data, 2);
	/*
	 * Discard reference to underlying file.  Must call closef because
	 * this may be the last reference.
	 */
#ifdef PORTAL_DIAGNOSTIC
	printf("portal_unmount: closef(%x)\n", VFSTOPORTAL(mp)->pm_server);
#endif	 
	closef(VFSTOPORTAL(mp)->pm_server, (struct proc *) 0);
	/*
	 * Finally, throw away the portalmount structure
	 */
	free(mp->mnt_data, M_UFSMNT);	/* XXX */
	mp->mnt_data = 0;
	return (0);
}

portal_root(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{
	struct vnode *vp;
	int error;

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_root(mp = %x)\n", mp);
#endif

	/*
	 * Return locked reference to root.
	 */
	vp = VFSTOPORTAL(mp)->pm_root;
	VREF(vp);
	VOP_LOCK(vp);
	*vpp = vp;
	return (0);
}

portal_quotactl(mp, cmd, uid, arg, p)
	struct mount *mp;
	int cmd;
	uid_t uid;
	caddr_t arg;
	struct proc *p;
{

	return (EOPNOTSUPP);
}

portal_statfs(mp, sbp, p)
	struct mount *mp;
	struct statfs *sbp;
	struct proc *p;
{
	struct filedesc *fdp;
	int lim;
	int i;
	int last;
	int freefd;

#ifdef PORTAL_DIAGNOSTIC
	printf("portal_statfs(mp = %x)\n", mp);
#endif

	sbp->f_type = MOUNT_PORTAL;
	sbp->f_flags = 0;
	sbp->f_bsize = DEV_BSIZE;
	sbp->f_iosize = DEV_BSIZE;
	sbp->f_blocks = 2;		/* 1K to keep df happy */
	sbp->f_bfree = 0;
	sbp->f_bavail = 0;
	sbp->f_files = 1;		/* Allow for "." */
	sbp->f_ffree = 0;		/* See comments above */
	if (sbp != &mp->mnt_stat) {
		bcopy(&mp->mnt_stat.f_fsid, &sbp->f_fsid, sizeof(sbp->f_fsid));
		bcopy(mp->mnt_stat.f_mntonname, sbp->f_mntonname, MNAMELEN);
		bcopy(mp->mnt_stat.f_mntfromname, sbp->f_mntfromname, MNAMELEN);
	}
	return (0);
}

portal_sync(mp, waitfor)
	struct mount *mp;
	int waitfor;
{

	return (0);
}

portal_vget(mp, ino, vpp)
	struct mount *mp;
	ino_t ino;
	struct vnode **vpp;
{

	return (EOPNOTSUPP);
}

portal_fhtovp(mp, fhp, vpp)
	struct mount *mp;
	struct fid *fhp;
	struct vnode **vpp;
{

	return (EOPNOTSUPP);
}

portal_vptofh(vp, fhp)
	struct vnode *vp;
	struct fid *fhp;
{

	return (EOPNOTSUPP);
}

struct vfsops portal_vfsops = {
	portal_mount,
	portal_start,
	portal_unmount,
	portal_root,
	portal_quotactl,
	portal_statfs,
	portal_sync,
	portal_vget,
	portal_fhtovp,
	portal_vptofh,
	portal_init,
};
