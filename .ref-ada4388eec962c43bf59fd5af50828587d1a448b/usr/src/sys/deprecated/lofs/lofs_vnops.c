/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lofs_vnops.c	8.4 (Berkeley) %G%
 *
 * $Id: lofs_vnops.c,v 1.11 1992/05/30 10:05:43 jsp Exp jsp $
 */

/*
 * Loopback Filesystem
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <sys/buf.h>
#include <miscfs/lofs/lofs.h>

/*
 * Basic strategy: as usual, do as little work as possible.
 * Nothing is ever locked in the lofs'ed filesystem, all
 * locks are held in the underlying filesystems.
 */

/*
 * Save a vnode and replace with
 * the lofs'ed one
 */
#define PUSHREF(v, nd) \
{ \
	struct { struct vnode *vnp; } v; \
	v.vnp = (nd); \
	(nd) = LOFSVP(v.vnp)

/*
 * Undo the PUSHREF
 */
#define POP(v, nd) \
	\
	(nd) = v.vnp; \
}

/*
 * vp is the current namei directory
 * ndp is the name to locate in that directory...
 */
int
lofs_lookup(ap)
	struct vop_lookup_args /* {
		struct vnode * a_dvp;
		struct vnode ** a_vpp;
		struct componentname * a_cnp;
	} */ *ap;
{
	struct vnode *dvp = ap->a_dvp;
	struct vnode *newvp;
	struct vnode *targetdvp;
	int error;
	int flag = ap->a_cnp->cn_nameiop /*& OPMASK*/;

	/*
	 * (ap->a_dvp) was locked when passed in, and it will be replaced
	 * with the target vnode, BUT that will already have been
	 * locked when (ap->a_dvp) was locked [see lofs_lock].  all that
	 * must be done here is to keep track of reference counts.
	 */
	targetdvp = LOFSVP(dvp);
	/*VREF(targetdvp);*/

	/*
	 * Call lookup on the looped vnode
	 */
	error = VOP_LOOKUP(targetdvp, &newvp, ap->a_cnp);
	/*vrele(targetdvp);*/

	if (error) {
		*ap->a_vpp = NULLVP;
		return (error);
	}

	*ap->a_vpp = newvp;

	/*
	 * If we just found a directory then make
	 * a loopback node for it and return the loopback
	 * instead of the real vnode.  Otherwise simply
	 * return the aliased directory and vnode.
	 */
	if (newvp && newvp->v_type == VDIR && flag == LOOKUP) {
		/*
		 * At this point, newvp is the vnode to be looped.
		 * Activate a loopback and return the looped vnode.
		 */
		return (make_lofs(dvp->v_mount, ap->a_vpp));
	}

	return (0);
}

/*
 * this = ni_dvp
 * ni_dvp references the locked directory.
 * ni_vp is NULL.
 */
int
lofs_mknod(ap)
	struct vop_mknod_args /* {
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
		struct vattr *a_vap;
	} */ *ap;
{
	int error;

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);

	error = VOP_MKNOD(ap->a_dvp, ap->a_vpp, ap->a_cnp, ap->a_vap);

	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

	return (error);
}

/*
 * this = ni_dvp;
 * ni_dvp references the locked directory
 * ni_vp is NULL.
 */
int
lofs_create(ap)
	struct vop_create_args /* {
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
		struct vattr *a_vap;
	} */ *ap;
{
	int error;

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);

	error = VOP_CREATE(ap->a_dvp, ap->a_vpp, ap->a_cnp, ap->a_vap);

	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

	return (error);
}

int
lofs_open(ap)
	struct vop_open_args /* {
		struct vnode *a_vp;
		int  a_mode;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_OPEN(LOFSVP(ap->a_vp), ap->a_mode, ap->a_cred, ap->a_p));
}

int
lofs_close(ap)
	struct vop_close_args /* {
		struct vnode *a_vp;
		int  a_fflag;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_CLOSE(LOFSVP(ap->a_vp), ap->a_fflag, ap->a_cred, ap->a_p));
}

int
lofs_access(ap)
	struct vop_access_args /* {
		struct vnode *a_vp;
		int  a_mode;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_ACCESS(LOFSVP(ap->a_vp), ap->a_mode, ap->a_cred, ap->a_p));
}

int
lofs_getattr(ap)
	struct vop_getattr_args /* {
		struct vnode *a_vp;
		struct vattr *a_vap;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{
	int error;

	/*
	 * Get the stats from the underlying filesystem
	 */
	error = VOP_GETATTR(LOFSVP(ap->a_vp), ap->a_vap, ap->a_cred, ap->a_p);
	if (error)
		return (error);
	/*
	 * and replace the fsid field with the loopback number
	 * to preserve the namespace.
	 */
	ap->a_vap->va_fsid = ap->a_vp->v_mount->mnt_stat.f_fsid.val[0];
	return (0);
}

int
lofs_setattr(ap)
	struct vop_setattr_args /* {
		struct vnode *a_vp;
		struct vattr *a_vap;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_SETATTR(LOFSVP(ap->a_vp), ap->a_vap, ap->a_cred, ap->a_p));
}

int
lofs_read(ap)
	struct vop_read_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		int  a_ioflag;
		struct ucred *a_cred;
	} */ *ap;
{

	return (VOP_READ(LOFSVP(ap->a_vp), ap->a_uio, ap->a_ioflag, ap->a_cred));
}

int
lofs_write(ap)
	struct vop_write_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		int  a_ioflag;
		struct ucred *a_cred;
	} */ *ap;
{

	return (VOP_WRITE(LOFSVP(ap->a_vp), ap->a_uio, ap->a_ioflag, ap->a_cred));
}

int
lofs_ioctl(ap)
	struct vop_ioctl_args /* {
		struct vnode *a_vp;
		int  a_command;
		caddr_t  a_data;
		int  a_fflag;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_IOCTL(LOFSVP(ap->a_vp), ap->a_command, ap->a_data, ap->a_fflag, ap->a_cred, ap->a_p));
}

int
lofs_select(ap)
	struct vop_select_args /* {
		struct vnode *a_vp;
		int  a_which;
		int  a_fflags;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_SELECT(LOFSVP(ap->a_vp), ap->a_which, ap->a_fflags, ap->a_cred, ap->a_p));
}

int
lofs_mmap(ap)
	struct vop_mmap_args /* {
		struct vnode *a_vp;
		int  a_fflags;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_MMAP(LOFSVP(ap->a_vp), ap->a_fflags, ap->a_cred, ap->a_p));
}

int
lofs_fsync(ap)
	struct vop_fsync_args /* {
		struct vnode *a_vp;
		struct ucred *a_cred;
		int  a_waitfor;
		struct proc *a_p;
	} */ *ap;
{
	struct vnode *targetvp = LOFSVP(ap->a_vp);

	if (targetvp)
		return (VOP_FSYNC(targetvp, ap->a_cred, ap->a_waitfor, ap->a_p));
	return (0);
}

int
lofs_seek(ap)
	struct vop_seek_args /* {
		struct vnode *a_vp;
		off_t  a_oldoff;
		off_t  a_newoff;
		struct ucred *a_cred;
	} */ *ap;
{

	return (VOP_SEEK(LOFSVP(ap->a_vp), ap->a_oldoff, ap->a_newoff, ap->a_cred));
}

int
lofs_remove(ap)
	struct vop_remove_args /* {
		struct vnode *a_dvp;
		struct vnode *a_vp;
		struct componentname *a_cnp;
	} */ *ap;
{
	int error;

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);
	PUSHREF(xvp, ap->a_vp);
	VREF(ap->a_vp);

	error = VOP_REMOVE(ap->a_dvp, ap->a_vp, ap->a_cnp);

	POP(xvp, ap->a_vp);
	vrele(ap->a_vp);
	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

	return (error);
}

/*
 * vp is this.
 * ni_dvp is the locked parent of the target.
 * ni_vp is NULL.
 */
int
lofs_link(ap)
	struct vop_link_args /* {
		struct vnode *a_vp;
		struct vnode *a_tdvp;
		struct componentname *a_cnp;
	} */ *ap;
{
	int error;

	PUSHREF(xdvp, ap->a_vp);
	VREF(ap->a_vp);

	error = VOP_LINK(ap->a_vp, LOFSVP(ap->a_tdvp), ap->a_cnp);

	POP(xdvp, ap->a_vp);
	vrele(ap->a_vp);

	return (error);
}

int
lofs_rename(ap)
	struct vop_rename_args  /* {
		struct vnode *a_fdvp;
		struct vnode *a_fvp;
		struct componentname *a_fcnp;
		struct vnode *a_tdvp;
		struct vnode *a_tvp;
		struct componentname *a_tcnp;
	} */ *ap;
{
	struct vnode *fvp, *tvp;
	struct vnode *tdvp;
#ifdef notdef
	struct vnode *fsvp, *tsvp;
#endif
	int error;

	/*
	 * Switch source directory to point to lofsed vnode
	 */
	PUSHREF(fdvp, ap->a_fdvp);
	VREF(ap->a_fdvp);

	/*
	 * And source object if it is lofsed...
	 */
	fvp = ap->a_fvp;
	if (fvp && fvp->v_op == lofs_vnodeop_p) {
		ap->a_fvp = LOFSVP(fvp);
		VREF(ap->a_fvp);
	} else {
		fvp = 0;
	}

#ifdef notdef
	/*
	 * And source startdir object if it is lofsed...
	 */
	fsvp = fndp->ni_startdir;
	if (fsvp && fsvp->v_op == lofs_vnodeop_p) {
		fndp->ni_startdir = LOFSVP(fsvp);
		VREF(fndp->ni_startdir);
	} else {
		fsvp = 0;
	}
#endif

	/*
 	 * Switch target directory to point to lofsed vnode
	 */
	tdvp = ap->a_tdvp;
	if (tdvp && tdvp->v_op == lofs_vnodeop_p) {
		ap->a_tdvp = LOFSVP(tdvp);
		VREF(ap->a_tdvp);
	} else {
		tdvp = 0;
	}

	/*
	 * And target object if it is lofsed...
	 */
	tvp = ap->a_tvp;
	if (tvp && tvp->v_op == lofs_vnodeop_p) {
		ap->a_tvp = LOFSVP(tvp);
		VREF(ap->a_tvp);
	} else {
		tvp = 0;
	}

#ifdef notdef
	/*
	 * And target startdir object if it is lofsed...
	 */
	tsvp = tndp->ni_startdir;
	if (tsvp && tsvp->v_op == lofs_vnodeop_p) {
		tndp->ni_startdir = LOFSVP(fsvp);
		VREF(tndp->ni_startdir);
	} else {
		tsvp = 0;
	}
#endif

	error = VOP_RENAME(ap->a_fdvp, ap->a_fvp, ap->a_fcnp, ap->a_tdvp, ap->a_tvp, ap->a_tcnp);

	/*
	 * Put everything back...
	 */
 
#ifdef notdef

	if (tsvp) {
		if (tndp->ni_startdir)
			vrele(tndp->ni_startdir);
		tndp->ni_startdir = tsvp;
	}
#endif

	if (tvp) {
		ap->a_tvp = tvp;
		vrele(ap->a_tvp);
	}

	if (tdvp) {
		ap->a_tdvp = tdvp;
		vrele(ap->a_tdvp);
	}

#ifdef notdef

	if (fsvp) {
		if (fndp->ni_startdir)
			vrele(fndp->ni_startdir);
		fndp->ni_startdir = fsvp;
	}
#endif

	if (fvp) {
		ap->a_fvp = fvp;
		vrele(ap->a_fvp);
	}

	POP(fdvp, ap->a_fdvp);
	vrele(ap->a_fdvp);

	return (error);
}

/*
 * ni_dvp is the locked (alias) parent.
 * ni_vp is NULL.
 */
int
lofs_mkdir(ap)
	struct vop_mkdir_args /* {
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
		struct vattr *a_vap;
	} */ *ap;
{
	int error;
	struct vnode *dvp = ap->a_dvp;
	struct vnode *xdvp;
	struct vnode *newvp;

	xdvp = dvp;
	dvp = LOFSVP(xdvp);
	VREF(dvp);

	error = VOP_MKDIR(dvp, &newvp, ap->a_cnp, ap->a_vap);

	if (error) {
		*ap->a_vpp = NULLVP;
		vrele(xdvp);
		return (error);
	}

	/*
	 * Make a new lofs node
	 */
	/*VREF(dvp);*/

	error = make_lofs(dvp->v_mount, &newvp);

	*ap->a_vpp = newvp;

	return (error);
}

/*
 * ni_dvp is the locked parent.
 * ni_vp is the entry to be removed.
 */
int
lofs_rmdir(ap)
	struct vop_rmdir_args /* {
		struct vnode *a_dvp;
		struct vnode *a_vp;
		struct componentname *a_cnp;
	} */ *ap;
{
	struct vnode *vp = ap->a_vp;
	struct vnode *dvp = ap->a_dvp;
	int error;

	PUSHREF(xdvp, dvp);
	VREF(dvp);
	PUSHREF(xvp, vp);
	VREF(vp);

	error = VOP_RMDIR(dvp, vp, ap->a_cnp);

	POP(xvp, vp);
	vrele(vp);
	POP(xdvp, dvp);
	vrele(dvp);

	return (error);
}

/*
 * ni_dvp is the locked parent.
 * ni_vp is NULL.
 */
int
lofs_symlink(ap)
	struct vop_symlink_args /* {
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
		struct vattr *a_vap;
		char *a_target;
	} */ *ap;
{
	int error;

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);

	error = VOP_SYMLINK(ap->a_dvp, ap->a_vpp, ap->a_cnp, ap->a_vap, ap->a_target);

	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

	return (error);
}

int
lofs_readdir(ap)
	struct vop_readdir_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		struct ucred *a_cred;
	} */ *ap;
{

	return (VOP_READDIR(LOFSVP(ap->a_vp), ap->a_uio, ap->a_cred));
}

int
lofs_readlink(ap)
	struct vop_readlink_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		struct ucred *a_cred;
	} */ *ap;
{

	return (VOP_READLINK(LOFSVP(ap->a_vp), ap->a_uio, ap->a_cred));
}

/*
 * Anyone's guess...
 */
int
lofs_abortop(ap)
	struct vop_abortop_args /* {
		struct vnode *a_dvp;
		struct componentname *a_cnp;
	} */ *ap;
{
	int error;

	PUSHREF(xdvp, ap->a_dvp);

	error = VOP_ABORTOP(ap->a_dvp, ap->a_cnp);

	POP(xdvp, ap->a_dvp);

	return (error);
}

int
lofs_inactive(ap)
	struct vop_inactive_args /* {
		struct vnode *a_vp;
	} */ *ap;
{
	struct vnode *targetvp = LOFSVP(ap->a_vp);

#ifdef DIAGNOSTIC
	{ extern int prtactive;
	if (prtactive && ap->a_vp->v_usecount != 0)
		vprint("lofs_inactive: pushing active", ap->a_vp);
	}
#endif

	if (targetvp) {
		vrele(targetvp);
		LOFSP(ap->a_vp)->a_lofsvp = 0;
	}
}

int
lofs_reclaim(ap)
	struct vop_reclaim_args /* {
		struct vnode *a_vp;
	} */ *ap;
{
	struct vnode *targetvp;

	remque(LOFSP(ap->a_vp));
	targetvp = LOFSVP(ap->a_vp);
	if (targetvp) {
		printf("lofs: delayed vrele of %x\n", targetvp);
		vrele(targetvp);	/* XXX should never happen */
	}
	FREE(ap->a_vp->v_data, M_TEMP);
	ap->a_vp->v_data = 0;
	return (0);
}

int
lofs_lock(ap)
	struct vop_lock_args /* {
		struct vnode *a_vp;
	} */ *ap;
{
	int error;
	register struct vnode *vp = ap->a_vp;
	struct vnode *targetvp;

	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
	}
	if (vp->v_tag == VT_NON)
		return (ENOENT);
	targetvp = LOFSVP(ap->a_vp);

	if (targetvp && (error = VOP_LOCK(targetvp)))
		return (error);
	return (0);
}

int
lofs_unlock(ap)
	struct vop_unlock_args /* {
		struct vnode *a_vp;
	} */ *ap;
{
	struct vnode *targetvp = LOFSVP(ap->a_vp);

	if (targetvp)
		return (VOP_UNLOCK(targetvp));
	return (0);
}

int
lofs_bmap(ap)
	struct vop_bmap_args /* {
		struct vnode *a_vp;
		daddr_t  a_bn;
		struct vnode **a_vpp;
		daddr_t *a_bnp;
		int *a_runp;
	} */ *ap;
{

	return (VOP_BMAP(LOFSVP(ap->a_vp), ap->a_bn, ap->a_vpp, ap->a_bnp, ap->a_runp));
}

int
lofs_strategy(ap)
	struct vop_strategy_args /* {
		struct buf *a_bp;
	} */ *ap;
{
	int error;

	PUSHREF(vp, ap->a_bp->b_vp);

	error = VOP_STRATEGY(ap->a_bp);

	POP(vp, ap->a_bp->b_vp);

	return (error);
}

int
lofs_print(ap)
	struct vop_print_args /* {
		struct vnode *a_vp;
	} */ *ap;
{

	struct vnode *targetvp = LOFSVP(ap->a_vp);
	printf("tag VT_LOFS ref ");
	if (targetvp)
		return (VOP_PRINT(targetvp));
	printf("NULLVP\n");
	return (0);
}

int
lofs_islocked(ap)
	struct vop_islocked_args /* {
		struct vnode *a_vp;
	} */ *ap;
{

	struct vnode *targetvp = LOFSVP(ap->a_vp);
	if (targetvp)
		return (VOP_ISLOCKED(targetvp));
	return (0);
}

int
lofs_pathconf(ap)
	struct vop_pathconf_args *ap;
{

	return (VOP_PATHCONF(LOFSVP(ap->a_vp), ap->a_name, ap->a_retval));
}

int
lofs_advlock(ap)
	struct vop_advlock_args /* {
		struct vnode *a_vp;
		caddr_t  a_id;
		int  a_op;
		struct flock *a_fl;
		int  a_flags;
	} */ *ap;
{

	return (VOP_ADVLOCK(LOFSVP(ap->a_vp), ap->a_id, ap->a_op, ap->a_fl, ap->a_flags));
}

/*
 * LOFS directory offset lookup.
 * Currently unsupported.
 */
int
lofs_blkatoff(ap)
	struct vop_blkatoff_args /* {
		struct vnode *a_vp;
		off_t a_offset;
		char **a_res;
		struct buf **a_bpp;
	} */ *ap;
{

	return (EOPNOTSUPP);
}

/*
 * LOFS flat namespace allocation.
 * Currently unsupported.
 */
int
lofs_valloc(ap)
	struct vop_valloc_args /* {
		struct vnode *a_pvp;
		int a_mode;
		struct ucred *a_cred;
		struct vnode **a_vpp;
	} */ *ap;
{

	return (EOPNOTSUPP);
}

/*
 * LOFS flat namespace free.
 * Currently unsupported.
 */
/*void*/
int
lofs_vfree(ap)
	struct vop_vfree_args /* {
		struct vnode *a_pvp;
		ino_t a_ino;
		int a_mode;
	} */ *ap;
{

	return (0);
}

/*
 * LOFS file truncation.
 */
int
lofs_truncate(ap)
	struct vop_truncate_args /* {
		struct vnode *a_vp;
		off_t a_length;
		int a_flags;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	/* Use lofs_setattr */
	printf("lofs_truncate: need to implement!!");
	return (EOPNOTSUPP);
}

/*
 * LOFS update.
 */
int
lofs_update(ap)
	struct vop_update_args /* {
		struct vnode *a_vp;
		struct timeval *a_ta;
		struct timeval *a_tm;
		int a_waitfor;
	} */ *ap;
{

	/* Use lofs_setattr */
	printf("lofs_update: need to implement!!");
	return (EOPNOTSUPP);
}

/*
 * LOFS bwrite
 */
int
lofs_bwrite(ap)
	struct vop_bwrite_args /* {
		struct buf *a_bp;
	} */ *ap;
{

	return (EOPNOTSUPP);
}

/*
 * Global vfs data structures for ufs
 */
int (**lofs_vnodeop_p)();
struct vnodeopv_entry_desc lofs_vnodeop_entries[] = {
	{ &vop_default_desc, vn_default_error },
	{ &vop_lookup_desc, lofs_lookup },		/* lookup */
	{ &vop_create_desc, lofs_create },		/* create */
	{ &vop_mknod_desc, lofs_mknod },		/* mknod */
	{ &vop_open_desc, lofs_open },			/* open */
	{ &vop_close_desc, lofs_close },		/* close */
	{ &vop_access_desc, lofs_access },		/* access */
	{ &vop_getattr_desc, lofs_getattr },		/* getattr */
	{ &vop_setattr_desc, lofs_setattr },		/* setattr */
	{ &vop_read_desc, lofs_read },			/* read */
	{ &vop_write_desc, lofs_write },		/* write */
	{ &vop_ioctl_desc, lofs_ioctl },		/* ioctl */
	{ &vop_select_desc, lofs_select },		/* select */
	{ &vop_mmap_desc, lofs_mmap },			/* mmap */
	{ &vop_fsync_desc, lofs_fsync },		/* fsync */
	{ &vop_seek_desc, lofs_seek },			/* seek */
	{ &vop_remove_desc, lofs_remove },		/* remove */
	{ &vop_link_desc, lofs_link },			/* link */
	{ &vop_rename_desc, lofs_rename },		/* rename */
	{ &vop_mkdir_desc, lofs_mkdir },		/* mkdir */
	{ &vop_rmdir_desc, lofs_rmdir },		/* rmdir */
	{ &vop_symlink_desc, lofs_symlink },		/* symlink */
	{ &vop_readdir_desc, lofs_readdir },		/* readdir */
	{ &vop_readlink_desc, lofs_readlink },		/* readlink */
	{ &vop_abortop_desc, lofs_abortop },		/* abortop */
	{ &vop_inactive_desc, lofs_inactive },		/* inactive */
	{ &vop_reclaim_desc, lofs_reclaim },		/* reclaim */
	{ &vop_lock_desc, lofs_lock },			/* lock */
	{ &vop_unlock_desc, lofs_unlock },		/* unlock */
	{ &vop_bmap_desc, lofs_bmap },			/* bmap */
	{ &vop_strategy_desc, lofs_strategy },		/* strategy */
	{ &vop_print_desc, lofs_print },		/* print */
	{ &vop_islocked_desc, lofs_islocked },		/* islocked */
	{ &vop_pathconf_desc, lofs_pathconf },		/* pathconf */
	{ &vop_advlock_desc, lofs_advlock },		/* advlock */
	{ &vop_blkatoff_desc, lofs_blkatoff },		/* blkatoff */
	{ &vop_valloc_desc, lofs_valloc },		/* valloc */
	{ &vop_vfree_desc, lofs_vfree },		/* vfree */
	{ &vop_truncate_desc, lofs_truncate },		/* truncate */
	{ &vop_update_desc, lofs_update },		/* update */
	{ &vop_bwrite_desc, lofs_bwrite },		/* bwrite */
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc lofs_vnodeop_opv_desc =
	{ &lofs_vnodeop_p, lofs_vnodeop_entries };
