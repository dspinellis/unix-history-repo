/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lofs_vfsops.c	8.3 (Berkeley) %G%
 *
 * $Id: lofs_vfsops.c,v 1.9 1992/05/30 10:26:24 jsp Exp jsp $
 */

/*
 * Loopback Filesystem
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <miscfs/lofs/lofs.h>

/*
 * Mount loopback copy of existing name space
 */
int
lofs_mount(mp, path, data, ndp, p)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
	struct proc *p;
{
	int error = 0;
	struct lofs_args args;
	struct vnode *vp;
	struct vnode *rootvp;
	struct lofsmount *amp;
	u_int size;

	/*
	 * Update is a no-op
	 */
	if (mp->mnt_flag & MNT_UPDATE)
		return (EOPNOTSUPP);

	/*
	 * Get argument
	 */
	if (error = copyin(data, (caddr_t)&args, sizeof(struct lofs_args)))
		return (error);

	/*
	 * Find target node
	 */
	NDINIT(ndp, LOOKUP, FOLLOW|WANTPARENT|LOCKLEAF,
		UIO_USERSPACE, args.target, p);
	if (error = namei(ndp))
		return (error);

	/*
	 * Sanity check on target vnode
	 */
	vp = ndp->ni_vp;
	vrele(ndp->ni_dvp);
	ndp->ni_dvp = 0;

	if (vp->v_type != VDIR) {
		vput(vp);
		return (EINVAL);
	}

	amp = (struct lofsmount *) malloc(sizeof(struct lofsmount),
				M_UFSMNT, M_WAITOK);	/* XXX */

	/*
	 * Save reference to underlying target FS
	 */
	amp->looped_vfs = vp->v_mount;

	/*
	 * Save reference.  Each mount also holds
	 * a reference on the root vnode.
	 */
	error = make_lofs(mp, &vp);
	/*
	 * Unlock the node (either the target or the alias)
	 */
	VOP_UNLOCK(vp);
	/*
	 * Make sure the node alias worked
	 */
	if (error) {
		vrele(vp);
		free(amp, M_UFSMNT);	/* XXX */
		return (error);
	}

	/*
	 * Keep a held reference to the root vnode.
	 * It is vrele'd in lofs_unmount.
	 */
	rootvp = vp;
	rootvp->v_flag |= VROOT;
	amp->rootvp = rootvp;
	if (LOFSVP(rootvp)->v_mount->mnt_flag & MNT_LOCAL)
		mp->mnt_flag |= MNT_LOCAL;
	mp->mnt_data = (qaddr_t) amp;
	getnewfsid(mp, MOUNT_LOFS);

	(void) copyinstr(path, mp->mnt_stat.f_mntonname, MNAMELEN - 1, &size);
	bzero(mp->mnt_stat.f_mntonname + size, MNAMELEN - size);
	(void) copyinstr(args.target, mp->mnt_stat.f_mntfromname, MNAMELEN - 1, 
	    &size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);
	return (0);
}

/*
 * VFS start.  Nothing needed here - the start routine
 * on the underlying filesystem will have been called
 * when that filesystem was mounted.
 */
int
lofs_start(mp, flags, p)
	struct mount *mp;
	int flags;
	struct proc *p;
{

	return (0);
}

/*
 * Free reference to looped FS
 */
int
lofs_unmount(mp, mntflags, p)
	struct mount *mp;
	int mntflags;
	struct proc *p;
{
	struct vnode *rootvp = VFSTOLOFS(mp)->rootvp;
	int error;
	int flags = 0;
	extern int doforce;

	if (mntflags & MNT_FORCE) {
		/* lofs can never be rootfs so don't check for it */
		if (!doforce)
			return (EINVAL);
		flags |= FORCECLOSE;
	}

	/*
	 * Clear out buffer cache.  I don't think we
	 * ever get anything cached at this level at the
	 * moment, but who knows...
	 */
	if (rootvp->v_usecount > 1)
		return (EBUSY);
	if (error = vflush(mp, rootvp, flags))
		return (error);

	/*
	 * Release reference on underlying root vnode
	 */
	vrele(rootvp);
	/*
	 * And blow it away for future re-use
	 */
	vgone(rootvp);
	/*
	 * Finally, throw away the lofsmount structure
	 */
	free(mp->mnt_data, M_UFSMNT);	/* XXX */
	mp->mnt_data = 0;
	return (0);
}

int
lofs_root(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{
	struct vnode *vp;

	/*
	 * Return locked reference to root.
	 */
	vp = VFSTOLOFS(mp)->rootvp;
	VREF(vp);
	VOP_LOCK(vp);
	*vpp = vp;
	return (0);
}

int
lofs_quotactl(mp, cmd, uid, arg, p)
	struct mount *mp;
	int cmd;
	uid_t uid;
	caddr_t arg;
	struct proc *p;
{
	return (VFS_QUOTACTL(VFSTOLOFS(mp)->looped_vfs, cmd, uid, arg, p));
}

int
lofs_statfs(mp, sbp, p)
	struct mount *mp;
	struct statfs *sbp;
	struct proc *p;
{
	int error;
	struct statfs mstat;

	bzero(&mstat, sizeof(mstat));

	error = VFS_STATFS(VFSTOLOFS(mp)->looped_vfs, &mstat, p);
	if (error)
		return (error);

	/* now copy across the "interesting" information and fake the rest */
	sbp->f_type = mstat.f_type;
	sbp->f_flags = mstat.f_flags;
	sbp->f_bsize = mstat.f_bsize;
	sbp->f_iosize = mstat.f_iosize;
	sbp->f_blocks = mstat.f_blocks;
	sbp->f_bfree = mstat.f_bfree;
	sbp->f_bavail = mstat.f_bavail;
	sbp->f_files = mstat.f_files;
	sbp->f_ffree = mstat.f_ffree;
	if (sbp != &mp->mnt_stat) {
		bcopy(&mp->mnt_stat.f_fsid, &sbp->f_fsid, sizeof(sbp->f_fsid));
		bcopy(mp->mnt_stat.f_mntonname, sbp->f_mntonname, MNAMELEN);
		bcopy(mp->mnt_stat.f_mntfromname, sbp->f_mntfromname, MNAMELEN);
	}
	return (0);
}

int
lofs_sync(mp, waitfor)
struct mount *mp;
int waitfor;
{
	return (0);
}

/*
 * LOFS flat namespace lookup.
 * Currently unsupported.
 */
int
lofs_vget(mp, ino, vpp)
	struct mount *mp;
	ino_t ino;
	struct vnode **vpp;
{

	return (EOPNOTSUPP);
}

int
lofs_fhtovp(mp, fhp, nam, vpp, exflagsp, credanonp)
	register struct mount *mp;
	struct fid *fhp;
	struct mbuf *nam;
	struct vnode **vpp;
	int *exflagsp;
	struct ucred **credanonp;
{
	return (VFS_FHTOVP(VFSTOLOFS(mp)->looped_vfs, fhp, nam, vpp, exflagsp, credanonp));
}

int
lofs_vptofh(vp, fhp)
	struct vnode *vp;
	struct fid *fhp;
{
	return (VFS_VPTOFH(LOFSVP(vp), fhp));
}

struct vfsops lofs_vfsops = {
	lofs_mount,
	lofs_start,
	lofs_unmount,
	lofs_root,
	lofs_quotactl,
	lofs_statfs,
	lofs_sync,
	lofs_vget,
	lofs_fhtovp,
	lofs_vptofh,
	lofs_init,
};
