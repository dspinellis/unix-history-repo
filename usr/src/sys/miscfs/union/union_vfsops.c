/*
 * Copyright (c) 1994 The Regents of the University of California.
 * Copyright (c) 1994 Jan-Simon Pendry.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)union_vfsops.c	1.1 (Berkeley) %G%
 */

/*
 * Null Layer
 * (See union_vnops.c for a description of what this does.)
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include "union.h"

/*
 * Mount union filesystem
 */
int
union_mount(mp, path, data, ndp, p)
	struct mount *mp;
	char *path;
	caddr_t data;
	struct nameidata *ndp;
	struct proc *p;
{
	int error = 0;
	struct union_args args;
	struct vnode *lowerrootvp;
	struct vnode *upperrootvp;
	struct union_mount *um;
	u_int size;

#ifdef UNION_DIAGNOSTIC
	printf("union_mount(mp = %x)\n", mp);
#endif

	/*
	 * Update is a no-op
	 */
	if (mp->mnt_flag & MNT_UPDATE)
		return (EOPNOTSUPP);

	/*
	 * Get argument
	 */
	if (error = copyin(data, (caddr_t)&args, sizeof(struct union_args)))
		return (error);

	lowerrootvp = mp->mnt_vnodecovered;
	VREF(lowerrootvp);

	/*
	 * Find upper node
	 */
	NDINIT(ndp, LOOKUP, FOLLOW|WANTPARENT,
	       UIO_USERSPACE, args.target, p);
	if (error = namei(ndp)) {
		vrele(lowerrootvp);
		return (error);
	}
	upperrootvp = ndp->ni_vp;
	vrele(ndp->ni_dvp);
	ndp->ni_dvp = NULL;

	if (upperrootvp->v_type != VDIR) {
		vrele(upperrootvp);
		return (EINVAL);
	}
	
	um = (struct union_mount *) malloc(sizeof(struct union_mount),
				M_UFSMNT, M_WAITOK);	/* XXX */

	/*
	 * Keep a held reference to the target vnodes.
	 * They are vrele'd in union_unmount.
	 */
	um->um_lowervp = lowerrootvp;
	um->um_uppervp = upperrootvp;
	/*
	 * Take a copy of the process's credentials.  This isn't
	 * quite right since the euid will always be zero and we
	 * want to get the "real" users credentials.  So fix up
	 * the uid field after taking the copy.
	 */
	um->um_cred = crdup(p->p_ucred);
	um->um_cred->cr_uid = p->p_cred->p_ruid;

	if ((lowerrootvp->v_mount->mnt_flag & MNT_LOCAL) ||
	    (upperrootvp->v_mount->mnt_flag & MNT_LOCAL))
		mp->mnt_flag |= MNT_LOCAL;
	mp->mnt_data = (qaddr_t) um;
	getnewfsid(mp, MOUNT_UNION);

	(void) copyinstr(path, mp->mnt_stat.f_mntonname, MNAMELEN - 1, &size);
	bzero(mp->mnt_stat.f_mntonname + size, MNAMELEN - size);
	(void) copyinstr(args.target, mp->mnt_stat.f_mntfromname, MNAMELEN - 1, 
	    &size);
	bzero(mp->mnt_stat.f_mntfromname + size, MNAMELEN - size);
#ifdef UNION_DIAGNOSTIC
	printf("union_mount: upper %s, lower at %s\n",
		mp->mnt_stat.f_mntfromname, mp->mnt_stat.f_mntonname);
#endif
	return (0);
}

/*
 * VFS start.  Nothing needed here - the start routine
 * on the underlying filesystem(s) will have been called
 * when that filesystem was mounted.
 */
int
union_start(mp, flags, p)
	struct mount *mp;
	int flags;
	struct proc *p;
{

	return (0);
}

/*
 * Free reference to union layer
 */
int
union_unmount(mp, mntflags, p)
	struct mount *mp;
	int mntflags;
	struct proc *p;
{
	struct union_mount *um = MOUNTTOUNIONMOUNT(mp);
	struct vnode *um_rootvp;
	int error;
	int flags = 0;
	extern int doforce;

#ifdef UNION_DIAGNOSTIC
	printf("union_unmount(mp = %x)\n", mp);
#endif

	if (mntflags & MNT_FORCE) {
		/* union can never be rootfs so don't check for it */
		if (!doforce)
			return (EINVAL);
		flags |= FORCECLOSE;
	}

	if (error = union_root(mp, &um_rootvp))
		return (error);
	if (um_rootvp->v_usecount > 1)
		return (EBUSY);
	if (error = vflush(mp, um_rootvp, flags))
		return (error);

#ifdef UNION_DIAGNOSTIC
	vprint("alias root of lower", um_rootvp);
#endif	 
	/*
	 * Discard references to upper and lower target vnodes.
	 */
	vrele(um->um_lowervp);
	vrele(um->um_uppervp);
	crfree(um->um_cred);
	/*
	 * Release reference on underlying root vnode
	 */
	vrele(um_rootvp);
	/*
	 * And blow it away for future re-use
	 */
	vgone(um_rootvp);
	/*
	 * Finally, throw away the union_mount structure
	 */
	free(mp->mnt_data, M_UFSMNT);	/* XXX */
	mp->mnt_data = 0;
	return 0;
}

int
union_root(mp, vpp)
	struct mount *mp;
	struct vnode **vpp;
{
	struct union_mount *um = MOUNTTOUNIONMOUNT(mp);
	int error;

#ifdef UNION_DIAGNOSTIC
	printf("union_root(mp = %x, lvp = %x, uvp = %x)\n", mp,
			um->um_lowervp,
			um->um_uppervp);
#endif

	/*
	 * Return locked reference to root.
	 */
	error = union_allocvp(vpp, mp, (struct vnode *) 0,
			      (struct componentname *) 0,
			      um->um_uppervp,
			      um->um_lowervp);
	if (error == 0)
		(*vpp)->v_flag |= VROOT;

	return (error);
}

int
union_quotactl(mp, cmd, uid, arg, p)
	struct mount *mp;
	int cmd;
	uid_t uid;
	caddr_t arg;
	struct proc *p;
{

	return (EOPNOTSUPP);
}

int
union_statfs(mp, sbp, p)
	struct mount *mp;
	struct statfs *sbp;
	struct proc *p;
{
	int error;
	struct union_mount *um = MOUNTTOUNIONMOUNT(mp);
	struct statfs mstat;
	int lbsize;

#ifdef UNION_DIAGNOSTIC
	printf("union_statfs(mp = %x, lvp = %x, uvp = %x)\n", mp,
			um->um_lowervp,
	       		um->um_uppervp);
#endif

	bzero(&mstat, sizeof(mstat));

	error = VFS_STATFS(um->um_lowervp->v_mount, &mstat, p);
	if (error)
		return (error);

	/* now copy across the "interesting" information and fake the rest */
#if 0
	sbp->f_type = mstat.f_type;
	sbp->f_flags = mstat.f_flags;
	sbp->f_bsize = mstat.f_bsize;
	sbp->f_iosize = mstat.f_iosize;
#endif
	lbsize = mstat.f_bsize;
	sbp->f_blocks = mstat.f_blocks;
	sbp->f_bfree = mstat.f_bfree;
	sbp->f_bavail = mstat.f_bavail;
	sbp->f_files = mstat.f_files;
	sbp->f_ffree = mstat.f_ffree;

	error = VFS_STATFS(um->um_uppervp->v_mount, &mstat, p);
	if (error)
		return (error);

	sbp->f_type = mstat.f_type;
	sbp->f_flags = mstat.f_flags;
	sbp->f_bsize = mstat.f_bsize;
	sbp->f_iosize = mstat.f_iosize;

	/*
	 * if the lower and upper blocksizes differ, then frig the
	 * block counts so that the sizes reported by df make some
	 * kind of sense.  none of this makes sense though.
	 */

	if (mstat.f_bsize != lbsize) {
		sbp->f_blocks = sbp->f_blocks * lbsize / mstat.f_bsize;
		sbp->f_bfree = sbp->f_bfree * lbsize / mstat.f_bsize;
		sbp->f_bavail = sbp->f_bavail * lbsize / mstat.f_bsize;
	}
	sbp->f_blocks += mstat.f_blocks;
	sbp->f_bfree += mstat.f_bfree;
	sbp->f_bavail += mstat.f_bavail;
	sbp->f_files += mstat.f_files;
	sbp->f_ffree += mstat.f_ffree;

	if (sbp != &mp->mnt_stat) {
		bcopy(&mp->mnt_stat.f_fsid, &sbp->f_fsid, sizeof(sbp->f_fsid));
		bcopy(mp->mnt_stat.f_mntonname, sbp->f_mntonname, MNAMELEN);
		bcopy(mp->mnt_stat.f_mntfromname, sbp->f_mntfromname, MNAMELEN);
	}
	return (0);
}

int
union_sync(mp, waitfor, cred, p)
	struct mount *mp;
	int waitfor;
	struct ucred *cred;
	struct proc *p;
{

	/*
	 * XXX - Assumes no data cached at union layer.
	 */
	return (0);
}

int
union_vget(mp, ino, vpp)
	struct mount *mp;
	ino_t ino;
	struct vnode **vpp;
{
	
	return (EOPNOTSUPP);
}

int
union_fhtovp(mp, fidp, nam, vpp, exflagsp, credanonp)
	struct mount *mp;
	struct fid *fidp;
	struct mbuf *nam;
	struct vnode **vpp;
	int *exflagsp;
	struct ucred **credanonp;
{

	return (EOPNOTSUPP);
}

int
union_vptofh(vp, fhp)
	struct vnode *vp;
	struct fid *fhp;
{

	return (EOPNOTSUPP);
}

int union_init __P((void));

struct vfsops union_vfsops = {
	union_mount,
	union_start,
	union_unmount,
	union_root,
	union_quotactl,
	union_statfs,
	union_sync,
	union_vget,
	union_fhtovp,
	union_vptofh,
	union_init,
};
