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
 *	@(#)union_vfsops.c	2.2 (Berkeley) %G%
 */

/*
 * Union Layer
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
#include <sys/filedesc.h>
#include <sys/queue.h>
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
	struct vnode *lowerrootvp = NULLVP;
	struct vnode *upperrootvp = NULLVP;
	struct union_mount *um;
	struct ucred *cred = 0;
	char *cp;
	int len;
	u_int size;

#ifdef UNION_DIAGNOSTIC
	printf("union_mount(mp = %x)\n", mp);
#endif

	/*
	 * Update is a no-op
	 */
	if (mp->mnt_flag & MNT_UPDATE) {
		/*
		 * Need to provide.
		 * 1. a way to convert between rdonly and rdwr mounts.
		 * 2. support for nfs exports.
		 */
		error = EOPNOTSUPP;
		goto bad;
	}

	/*
	 * Take a copy of the process's credentials.  This isn't
	 * quite right since the euid will always be zero and we
	 * want to get the "real" users credentials.  So fix up
	 * the uid field after taking the copy.
	 */
	cred = crdup(p->p_ucred);
	cred->cr_uid = p->p_cred->p_ruid;

	/*
	 * Ensure the *real* user has write permission on the
	 * mounted-on directory.  This allows the mount_union
	 * command to be made setuid root so allowing anyone
	 * to do union mounts onto any directory on which they
	 * have write permission.
	 */
	error = VOP_ACCESS(mp->mnt_vnodecovered, VWRITE, cred, p);
	if (error)
		goto bad;

	/*
	 * Get argument
	 */
	if (error = copyin(data, (caddr_t)&args, sizeof(struct union_args)))
		goto bad;

	lowerrootvp = mp->mnt_vnodecovered;
	VREF(lowerrootvp);

	/*
	 * Find upper node
	 */
	NDINIT(ndp, LOOKUP, FOLLOW|WANTPARENT,
	       UIO_USERSPACE, args.target, p);
	if (error = namei(ndp))
		goto bad;

	upperrootvp = ndp->ni_vp;
	vrele(ndp->ni_dvp);
	ndp->ni_dvp = NULL;

	if (upperrootvp->v_type != VDIR) {
		error = EINVAL;
		goto bad;
	}
	
	um = (struct union_mount *) malloc(sizeof(struct union_mount),
				M_UFSMNT, M_WAITOK);	/* XXX */

	/*
	 * Keep a held reference to the target vnodes.
	 * They are vrele'd in union_unmount.
	 *
	 * Depending on the _BELOW flag, the filesystems are
	 * viewed in a different order.  In effect, this is the
	 * same as providing a mount under option to the mount syscall.
	 */

	switch (args.mntflags & UNMNT_OPMASK) {
	case UNMNT_ABOVE:
		um->um_lowervp = lowerrootvp;
		um->um_uppervp = upperrootvp;
		break;

	case UNMNT_BELOW:
		um->um_lowervp = upperrootvp;
		um->um_uppervp = lowerrootvp;
		break;

	case UNMNT_REPLACE:
		vrele(lowerrootvp);
		lowerrootvp = NULLVP;
		um->um_uppervp = upperrootvp;
		um->um_lowervp = lowerrootvp;
		break;

	default:
		error = EINVAL;
		goto bad;
	}

	um->um_cred = cred;
	um->um_cmode = UN_DIRMODE &~ p->p_fd->fd_cmask;

	/*
	 * Depending on what you think the MNT_LOCAL flag might mean,
	 * you may want the && to be || on the conditional below.
	 * At the moment it has been defined that the filesystem is
	 * only local if it is all local, ie the MNT_LOCAL flag implies
	 * that the entire namespace is local.  If you think the MNT_LOCAL
	 * flag implies that some of the files might be stored locally
	 * then you will want to change the conditional.
	 */
	if (((um->um_lowervp == NULLVP) ||
	     (um->um_lowervp->v_mount->mnt_flag & MNT_LOCAL)) &&
	    (um->um_uppervp->v_mount->mnt_flag & MNT_LOCAL))
		mp->mnt_flag |= MNT_LOCAL;

	/*
	 * Copy in the upper layer's RDONLY flag.  This is for the benefit
	 * of lookup() which explicitly checks the flag, rather than asking
	 * the filesystem for it's own opinion.  This means, that an update
	 * mount of the underlying filesystem to go from rdonly to rdwr
	 * will leave the unioned view as read-only.
	 */
	mp->mnt_flag |= (um->um_uppervp->v_mount->mnt_flag & MNT_RDONLY);
	mp->mnt_data = (qaddr_t) um;
	getnewfsid(mp, MOUNT_UNION);

	(void) copyinstr(path, mp->mnt_stat.f_mntonname, MNAMELEN - 1, &size);
	bzero(mp->mnt_stat.f_mntonname + size, MNAMELEN - size);

	switch (args.mntflags & UNMNT_OPMASK) {
	case UNMNT_ABOVE:
		cp = "un-above:";
		break;
	case UNMNT_BELOW:
		cp = "un-below:";
		break;
	case UNMNT_REPLACE:
		cp = "replace:";
		break;
	}
	len = strlen(cp);
	bcopy(cp, mp->mnt_stat.f_mntfromname, len);

	cp = mp->mnt_stat.f_mntfromname + len;
	len = MNAMELEN - len;

	(void) copyinstr(args.target, cp, len - 1, &size);
	bzero(cp + size, len - size);

#ifdef UNION_DIAGNOSTIC
	printf("union_mount: from %s, on %s\n",
		mp->mnt_stat.f_mntfromname, mp->mnt_stat.f_mntonname);
#endif
	return (0);

bad:
	if (cred)
		crfree(cred);
	if (upperrootvp)
		vrele(upperrootvp);
	if (lowerrootvp)
		vrele(lowerrootvp);
	return (error);
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
	if (um_rootvp->v_usecount > 1) {
		vput(um_rootvp);
		return (EBUSY);
	}
	if (error = vflush(mp, um_rootvp, flags)) {
		vput(um_rootvp);
		return (error);
	}

#ifdef UNION_DIAGNOSTIC
	vprint("alias root of lower", um_rootvp);
#endif	 
	/*
	 * Discard references to upper and lower target vnodes.
	 */
	if (um->um_lowervp)
		vrele(um->um_lowervp);
	vrele(um->um_uppervp);
	crfree(um->um_cred);
	/*
	 * Release reference on underlying root vnode
	 */
	vput(um_rootvp);
	/*
	 * And blow it away for future re-use
	 */
	vgone(um_rootvp);
	/*
	 * Finally, throw away the union_mount structure
	 */
	free(mp->mnt_data, M_UFSMNT);	/* XXX */
	mp->mnt_data = 0;
	return (0);
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
	VREF(um->um_uppervp);
	VOP_LOCK(um->um_uppervp);
	if (um->um_lowervp)
		VREF(um->um_lowervp);
	error = union_allocvp(vpp, mp,
			      (struct vnode *) 0,
			      (struct vnode *) 0,
			      (struct componentname *) 0,
			      um->um_uppervp,
			      um->um_lowervp);

	if (error) {
		vrele(um->um_uppervp);
		if (um->um_lowervp)
			vrele(um->um_lowervp);
	} else {
		(*vpp)->v_flag |= VROOT;
	}

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

	if (um->um_lowervp) {
		error = VFS_STATFS(um->um_lowervp->v_mount, &mstat, p);
		if (error)
			return (error);
	}

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
