/*
 * Copyright (c) 1992 The Regents of the University of California
 * Copyright (c) 1990, 1992 Jan-Simon Pendry
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)null_vfsops.c	1.3 (Berkeley) %G%
 *
 * @(#)lofs_vfsops.c	1.2 (Berkeley) 6/18/92
 * $Id: lofs_vfsops.c,v 1.9 1992/05/30 10:26:24 jsp Exp jsp $
 */

/*
 * Null Layer
 * (See null_vnops.c for a description of what this does.)
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <lofs/lofs.h>

/*
 * Mount null layer
 */
nullfs_mount(mp, path, data, ndp, p)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
	struct proc *p;
{
	USES_VOP_UNLOCK;
	int error = 0;
	struct null_args args;
	struct vnode *lowerrootvp, *vp;
	struct vnode *nullm_rootvp;
	struct null_mount *amp;
	u_int size;

#ifdef NULLFS_DIAGNOSTIC
	printf("nullfs_mount(mp = %x)\n", mp);
#endif

	/*
	 * Update is a no-op
	 */
	if (mp->mnt_flag & MNT_UPDATE) {
		return (EOPNOTSUPP);
		/* return VFS_MOUNT(MOUNTTONULLMOUNT(mp)->nullm_vfs, path, data, ndp, p);*/
	}

	/*
	 * Get argument
	 */
	if (error = copyin(data, (caddr_t)&args, sizeof(struct null_args)))
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
	lowerrootvp = ndp->ni_vp;
#ifdef NULLFS_DIAGNOSTIC
	printf("vp = %x, check for VDIR...\n", lowerrootvp);
#endif
	vrele(ndp->ni_dvp);
	ndp->ni_dvp = 0;

	/*
	 * NEEDSWORK: Is this really bad, or just not
	 * the way it's been?
	 */
	if (lowerrootvp->v_type != VDIR) {
		vput(lowerrootvp);
		return (EINVAL);
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("mp = %x\n", mp);
#endif

	amp = (struct null_mount *) malloc(sizeof(struct null_mount),
				M_UFSMNT, M_WAITOK);	/* XXX */

	/*
	 * Save reference to underlying target FS
	 */
	amp->nullm_vfs = lowerrootvp->v_mount;

	/*
	 * Save reference.  Each mount also holds
	 * a reference on the root vnode.
	 */
	error = make_null_node(mp, lowerrootvp, &vp);
	/*
	 * Unlock the node (either the target or the alias)
	 */
	VOP_UNLOCK(vp);
	/*
	 * Make sure the node alias worked
	 */
	if (error) {
		vrele(lowerrootvp);
		free(amp, M_UFSMNT);	/* XXX */
		return (error);
	}

	/*
	 * Keep a held reference to the root vnode.
	 * It is vrele'd in nullfs_unmount.
	 */
	nullm_rootvp = vp;
	nullm_rootvp->v_flag |= VROOT;
	amp->nullm_rootvp = nullm_rootvp;
	if (NULLTOLOWERVP(nullm_rootvp)->v_mount->mnt_flag & MNT_LOCAL)
		mp->mnt_flag |= MNT_LOCAL;
	mp->mnt_data = (qaddr_t) amp;
	getnewfsid(mp, MOUNT_LOFS);

	(void) copyinstr(path, mp->mnt_stat.f_mntonname, MNAMELEN - 1, &size);
	bzero(mp->mnt_stat.f_mntonname + size, MNAMELEN - size);
	(void) copyinstr(args.target, mp->mnt_stat.f_mntfromname, MNAMELEN - 1, 
	    &size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);
#ifdef NULLFS_DIAGNOSTIC
	printf("nullfs_mount: target %s, alias at %s\n",
		mp->mnt_stat.f_mntfromname, mp->mnt_stat.f_mntonname);
#endif
	return (0);
}

/*
 * VFS start.  Nothing needed here - the start routine
 * on the underlying filesystem will have been called
 * when that filesystem was mounted.
 */
nullfs_start(mp, flags, p)
	struct mount *mp;
	int flags;
	struct proc *p;
{
	return (0);
	/* return VFS_START(MOUNTTONULLMOUNT(mp)->nullm_vfs, flags, p); */
}

/*
 * Free reference to null layer
 */
nullfs_unmount(mp, mntflags, p)
	struct mount *mp;
	int mntflags;
	struct proc *p;
{
	struct vnode *nullm_rootvp = MOUNTTONULLMOUNT(mp)->nullm_rootvp;
	int error;
	int flags = 0;
	extern int doforce;

#ifdef NULLFS_DIAGNOSTIC
	printf("nullfs_unmount(mp = %x)\n", mp);
#endif

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
	mntflushbuf(mp, 0); 
	if (mntinvalbuf(mp, 1))
		return (EBUSY);
	if (nullm_rootvp->v_usecount > 1)
		return (EBUSY);
	if (error = vflush(mp, nullm_rootvp, flags))
		return (error);

#ifdef NULLFS_DIAGNOSTIC
	/*
	 * Flush any remaining vnode references
	 */
	null_node_flushmp (mp);
#endif

#ifdef NULLFS_DIAGNOSTIC
	vprint("alias root of target", nullm_rootvp);
#endif	 
	/*
	 * Release reference on underlying root vnode
	 */
	vrele(nullm_rootvp);
	/*
	 * And blow it away for future re-use
	 */
	vgone(nullm_rootvp);
	/*
	 * Finally, throw away the null_mount structure
	 */
	free(mp->mnt_data, M_UFSMNT);	/* XXX */
	mp->mnt_data = 0;
	return 0;
}

nullfs_root(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{
	USES_VOP_LOCK;
	struct vnode *vp;

#ifdef NULLFS_DIAGNOSTIC
	printf("nullfs_root(mp = %x, vp = %x->%x)\n", mp,
			MOUNTTONULLMOUNT(mp)->nullm_rootvp,
			NULLTOLOWERVP(MOUNTTONULLMOUNT(mp)->nullm_rootvp)
			);
#endif

	/*
	 * Return locked reference to root.
	 */
	vp = MOUNTTONULLMOUNT(mp)->nullm_rootvp;
	VREF(vp);
	VOP_LOCK(vp);
	*vpp = vp;
	return 0;
}

nullfs_quotactl(mp, cmd, uid, arg, p)
	struct mount *mp;
	int cmd;
	uid_t uid;
	caddr_t arg;
	struct proc *p;
{
	return VFS_QUOTACTL(MOUNTTONULLMOUNT(mp)->nullm_vfs, cmd, uid, arg, p);
}

nullfs_statfs(mp, sbp, p)
	struct mount *mp;
	struct statfs *sbp;
	struct proc *p;
{
	int error;
	struct statfs mstat;

#ifdef NULLFS_DIAGNOSTIC
	printf("nullfs_statfs(mp = %x, vp = %x->%x)\n", mp,
			MOUNTTONULLMOUNT(mp)->nullm_rootvp,
			NULLTOLOWERVP(MOUNTTONULLMOUNT(mp)->nullm_rootvp)
			);
#endif

	bzero(&mstat, sizeof(mstat));

	error = VFS_STATFS(MOUNTTONULLMOUNT(mp)->nullm_vfs, &mstat, p);
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

nullfs_sync(mp, waitfor)
struct mount *mp;
int waitfor;
{
	/*
	 * NEEDSWORK: Assumes no data cached at null layer.
	 * Is this true?
	 */
	return (0);
}

nullfs_fhtovp(mp, fhp, setgen, vpp)
	struct mount *mp;
	struct fid *fhp;
	int setgen;
	struct vnode **vpp;
{
	return VFS_FHTOVP(MOUNTTONULLMOUNT(mp)->nullm_vfs, fhp, setgen, vpp);
}

nullfs_vptofh(vp, fhp)
	struct vnode *vp;
	struct fid *fhp;
{
	return VFS_VPTOFH(NULLTOLOWERVP(vp), fhp);
}

int nullfs_init __P((void));

struct vfsops null_vfsops = {
	nullfs_mount,
	nullfs_start,
	nullfs_unmount,
	nullfs_root,
	nullfs_quotactl,
	nullfs_statfs,
	nullfs_sync,
	nullfs_fhtovp,
	nullfs_vptofh,
	nullfs_init,
};
