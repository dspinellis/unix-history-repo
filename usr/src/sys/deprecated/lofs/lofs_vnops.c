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
 *	@(#)lofs_vnops.c	1.1 (Berkeley) %G%
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
#include <lofs/lofs.h>

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
lofs_lookup (ap)
	struct vop_lookup_args *ap;
{
	USES_VOP_LOOKUP;
	struct vnode *newvp;
	struct vnode *targetdvp;
	int error;
	int flag = ap->a_cnp->cn_nameiop /*& OPMASK*/;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_LOOKUP(ap->a_dvp = %x->%x, \"%s\", op = %d)\n",
		ap->a_dvp, LOFSVP(ap->a_dvp), ap->a_cnp->cn_nameptr, flag);
#endif

	/*
	 * (ap->a_dvp) was locked when passed in, and it will be replaced
	 * with the target vnode, BUT that will already have been
	 * locked when (ap->a_dvp) was locked [see lofs_lock].  all that
	 * must be done here is to keep track of reference counts.
	 */
	targetdvp = LOFSVP(ap->a_dvp);
	VREF(targetdvp);
#ifdef LOFS_DIAGNOSTIC
	vprint("lofs VOP_LOOKUP", targetdvp);
#endif

	/*
	 * Call lookup on the looped vnode
	 */
	error = VOP_LOOKUP(targetdvp, ap->a_vpp, ap->a_cnp);
	vrele(targetdvp);

	if (error) {
		*ap->a_vpp = NULLVP;
#ifdef LOFS_DIAGNOSTIC
		printf("VOP_LOOKUP(%x->%x) = %d\n",
			ap->a_dvp, LOFSVP(ap->a_dvp), error);
#endif
		return (error);
	}
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_LOOKUP(%x->%x) = OK\n", ap->a_dvp, LOFSVP(ap->a_dvp));
#endif

	newvp = *ap->a_vpp;

	/*
	 * If we just found a directory then make
	 * a loopback node for it and return the loopback
	 * instead of the real vnode.  Otherwise simply
	 * return the aliased directory and vnode.
	 */
	if (newvp && newvp->v_type == VDIR && flag == LOOKUP) {
#ifdef LOFS_DIAGNOSTIC
		printf("lofs_lookup: found VDIR\n");
#endif
		/*
		 * At this point, newvp is the vnode to be looped.
		 * Activate a loopback and return the looped vnode.
		 */
		return (make_lofs(ap->a_dvp->v_mount, ap->a_vpp));
	}

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_lookup: not VDIR; vrele(%x)\n", ap->a_dvp);
#endif

	return (0);
}

/*
 * this = ni_dvp
 * ni_dvp references the locked directory.
 * ni_vp is NULL.
 */
lofs_mknod (ap)
	struct vop_mknod_args *ap;
{
	USES_VOP_MKNOD;
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_MKNOD(vp = %x->%x)\n", ap->a_dvp, LOFSVP(ap->a_dvp));
#endif

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
lofs_create (ap)
	struct vop_create_args *ap;
{
	USES_VOP_CREATE;
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_CREATE(ap->a_dvp = %x->%x)\n", ap->a_dvp, LOFSVP(ap->a_dvp));
#endif

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);

	error = VOP_CREATE(ap->a_dvp, ap->a_vpp, ap->a_cnp, ap->a_vap);

	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_CREATE(ap->a_dvp = %x->%x)\n", ap->a_dvp, LOFSVP(ap->a_dvp));
#endif

	return (error);
}

lofs_open (ap)
	struct vop_open_args *ap;
{
	USES_VOP_OPEN;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_OPEN(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_OPEN(LOFSVP(ap->a_vp), ap->a_mode, ap->a_cred, ap->a_p);
}

lofs_close (ap)
	struct vop_close_args *ap;
{
	USES_VOP_CLOSE;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_CLOSE(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_CLOSE(LOFSVP(ap->a_vp), ap->a_fflag, ap->a_cred, ap->a_p);
}

lofs_access (ap)
	struct vop_access_args *ap;
{
	USES_VOP_ACCESS;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_ACCESS(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_ACCESS(LOFSVP(ap->a_vp), ap->a_mode, ap->a_cred, ap->a_p);
}

lofs_getattr (ap)
	struct vop_getattr_args *ap;
{
	USES_VOP_GETATTR;
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_GETATTR(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

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

lofs_setattr (ap)
	struct vop_setattr_args *ap;
{
	USES_VOP_SETATTR;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_SETATTR(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_SETATTR(LOFSVP(ap->a_vp), ap->a_vap, ap->a_cred, ap->a_p);
}

lofs_read (ap)
	struct vop_read_args *ap;
{
	USES_VOP_READ;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_READ(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_READ(LOFSVP(ap->a_vp), ap->a_uio, ap->a_ioflag, ap->a_cred);
}

lofs_write (ap)
	struct vop_write_args *ap;
{
	USES_VOP_WRITE;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_WRITE(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_WRITE(LOFSVP(ap->a_vp), ap->a_uio, ap->a_ioflag, ap->a_cred);
}

lofs_ioctl (ap)
	struct vop_ioctl_args *ap;
{
	USES_VOP_IOCTL;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_IOCTL(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_IOCTL(LOFSVP(ap->a_vp), ap->a_command, ap->a_data, ap->a_fflag, ap->a_cred, ap->a_p);
}

lofs_select (ap)
	struct vop_select_args *ap;
{
	USES_VOP_SELECT;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_SELECT(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_SELECT(LOFSVP(ap->a_vp), ap->a_which, ap->a_fflags, ap->a_cred, ap->a_p);
}

lofs_mmap (ap)
	struct vop_mmap_args *ap;
{
	USES_VOP_MMAP;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_MMAP(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_MMAP(LOFSVP(ap->a_vp), ap->a_fflags, ap->a_cred, ap->a_p);
}

lofs_fsync (ap)
	struct vop_fsync_args *ap;
{
	USES_VOP_FSYNC;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_FSYNC(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_FSYNC(LOFSVP(ap->a_vp), ap->a_fflags, ap->a_cred, ap->a_waitfor, ap->a_p);
}

lofs_seek (ap)
	struct vop_seek_args *ap;
{
	USES_VOP_SEEK;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_SEEK(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_SEEK(LOFSVP(ap->a_vp), ap->a_oldoff, ap->a_newoff, ap->a_cred);
}

lofs_remove (ap)
	struct vop_remove_args *ap;
{
	USES_VOP_REMOVE;
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_REMOVE(ap->a_vp = %x->%x)\n", ap->a_dvp, LOFSVP(ap->a_dvp));
#endif

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
lofs_link (ap)
	struct vop_link_args *ap;
{
	USES_VOP_LINK;
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_LINK(ap->a_tdvp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	PUSHREF(xdvp, ap->a_vp);
	VREF(ap->a_vp);

	error = VOP_LINK(ap->a_vp, LOFSVP(ap->a_tdvp), ap->a_cnp);

	POP(xdvp, ap->a_vp);
	vrele(ap->a_vp);

	return (error);
}

lofs_rename (ap)
	struct vop_rename_args *ap;
{
	USES_VOP_RENAME;
	struct vnode *fvp, *tvp;
	struct vnode *tdvp;
#if 0
	struct vnode *fsvp, *tsvp;
#endif
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_RENAME(fdvp = %x->%x)\n", ap->a_fdvp, LOFSVP(ap->a_fdvp));
	/*printf("VOP_RENAME(tdvp = %x->%x)\n", tndp->ni_dvp, LOFSVP(tndp->ni_dvp));*/
#endif

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - switch source dvp\n");
#endif
	/*
	 * Switch source directory to point to lofsed vnode
	 */
	PUSHREF(fdvp, ap->a_fdvp);
	VREF(ap->a_fdvp);

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - switch source vp\n");
#endif
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

#if 0
#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - switch source start vp\n");
#endif
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

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - switch target dvp\n");
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

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - switch target vp\n");
#endif
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

#if 0
#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - switch target start vp\n");
#endif
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

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - VOP_RENAME(%x, %x, %x, %x)\n",
		ap->a_fdvp, ap->a_fvp, ap->a_tdvp, ap->a_tvp);
	vprint("ap->a_fdvp", ap->a_fdvp);
	vprint("ap->a_fvp", ap->a_fvp);
	vprint("ap->a_tdvp", ap->a_tdvp);
	if (ap->a_tvp) vprint("ap->a_tvp", ap->a_tvp);
	DELAY(16000000);
#endif

	error = VOP_RENAME(ap->a_fdvp, ap->a_fvp, ap->a_fcnp, ap->a_tdvp, ap->a_tvp, ap->a_tcnp);

	/*
	 * Put everything back...
	 */
 
#if 0
#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - restore target startdir\n");
#endif

	if (tsvp) {
		if (tndp->ni_startdir)
			vrele(tndp->ni_startdir);
		tndp->ni_startdir = tsvp;
	}
#endif

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - restore target vp\n");
#endif

	if (tvp) {
		ap->a_tvp = tvp;
		vrele(ap->a_tvp);
	}

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - restore target dvp\n");
#endif

	if (tdvp) {
		ap->a_tdvp = tdvp;
		vrele(ap->a_tdvp);
	}

#if 0
#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - restore source startdir\n");
#endif

	if (fsvp) {
		if (fndp->ni_startdir)
			vrele(fndp->ni_startdir);
		fndp->ni_startdir = fsvp;
	}
#endif

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - restore source vp\n");
#endif


	if (fvp) {
		ap->a_fvp = fvp;
		vrele(ap->a_fvp);
	}

#ifdef LOFS_DIAGNOSTIC
	printf("lofs_rename - restore source dvp\n");
#endif

	POP(fdvp, ap->a_fdvp);
	vrele(ap->a_fdvp);

	return (error);
}

/*
 * ni_dvp is the locked (alias) parent.
 * ni_vp is NULL.
 */
lofs_mkdir (ap)
	struct vop_mkdir_args *ap;
{
	USES_VOP_MKDIR;
	int error;
	struct vnode *xdvp;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_MKDIR(vp = %x->%x)\n", ap->a_dvp, LOFSVP(ap->a_dvp));
#endif

	xdvp = ap->a_dvp;
	ap->a_dvp = LOFSVP(xdvp);
	VREF(ap->a_dvp);

	error = VOP_MKDIR(ap->a_dvp, ap->a_vpp, ap->a_cnp, ap->a_vap);

	if (error) {
		vrele(xdvp);
		return (error);
	}

	/*
	 * Make a new lofs node
	 */
	VREF(ap->a_dvp); 

	error = make_lofs(ap->a_dvp->v_mount, ap->a_vpp);

	vrele(xdvp);
	vrele(xdvp);

	return (error);
}

/*
 * ni_dvp is the locked parent.
 * ni_vp is the entry to be removed.
 */
lofs_rmdir (ap)
	struct vop_rmdir_args *ap;
{
	USES_VOP_RMDIR;
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_RMDIR(ap->a_vp = %x->%x)\n", ap->a_dvp, LOFSVP(ap->a_dvp));
#endif

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);
	PUSHREF(xvp, ap->a_vp);
	VREF(ap->a_vp);

	error = VOP_RMDIR(ap->a_dvp, ap->a_vp, ap->a_cnp);

	POP(xvp, ap->a_vp);
	vrele(ap->a_vp);
	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

	return (error);
}

/*
 * ni_dvp is the locked parent.
 * ni_vp is NULL.
 */
lofs_symlink (ap)
	struct vop_symlink_args *ap;
{
	USES_VOP_SYMLINK;
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_SYMLINK(vp = %x->%x)\n", ap->a_dvp, LOFSVP(ap->a_dvp));
#endif

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);

	error = VOP_SYMLINK(ap->a_dvp, ap->a_vpp, ap->a_cnp, ap->a_vap, ap->a_target);

	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

	return (error);
}

lofs_readdir (ap)
	struct vop_readdir_args *ap;
{
	USES_VOP_READDIR;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_READDIR(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_READDIR(LOFSVP(ap->a_vp), ap->a_uio, ap->a_cred, ap->a_eofflagp);
}

lofs_readlink (ap)
	struct vop_readlink_args *ap;
{
	USES_VOP_READLINK;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_READLINK(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_READLINK(LOFSVP(ap->a_vp), ap->a_uio, ap->a_cred);
}

/*
 * Anyone's guess...
 */
lofs_abortop (ap)
	struct vop_abortop_args *ap;
{
	USES_VOP_ABORTOP;
	int error;

	PUSHREF(xdvp, ap->a_dvp);

	error = VOP_ABORTOP(ap->a_dvp, ap->a_cnp);

	POP(xdvp, ap->a_dvp);

	return (error);
}

lofs_inactive (ap)
	struct vop_inactive_args *ap;
{
	USES_VOP_INACTIVE;
	struct vnode *targetvp = LOFSVP(ap->a_vp);
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_INACTIVE(ap->a_vp = %x->%x)\n", ap->a_vp, targetvp);
#endif

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

lofs_reclaim (ap)
	struct vop_reclaim_args *ap;
{
	USES_VOP_RECLAIM;
	struct vnode *targetvp;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_RECLAIM(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif
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

lofs_lock (ap)
	struct vop_lock_args *ap;
{
	USES_VOP_LOCK;
	int error;
	struct vnode *targetvp = LOFSVP(ap->a_vp);

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_LOCK(ap->a_vp = %x->%x)\n", ap->a_vp, targetvp);
	/*vprint("lofs_lock ap->a_vp", ap->a_vp);
	if (targetvp)
		vprint("lofs_lock ->ap->a_vp", targetvp);
	else
		printf("lofs_lock ->ap->a_vp = NIL\n");*/
#endif

	if (targetvp) {
		error = VOP_LOCK(targetvp);
		if (error)
			return (error);
	}

	return (0);
}

lofs_unlock (ap)
	struct vop_unlock_args *ap;
{
	USES_VOP_UNLOCK;
	struct vnode *targetvp = LOFSVP(ap->a_vp);

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_UNLOCK(ap->a_vp = %x->%x)\n", ap->a_vp, targetvp);
#endif

	if (targetvp)
		return (VOP_UNLOCK(targetvp));
	return (0);
}

lofs_bmap (ap)
	struct vop_bmap_args *ap;
{
	USES_VOP_BMAP;
#ifdef LOFS_DIAGNOSTIC
	printf("VOP_BMAP(ap->a_vp = %x->%x)\n", ap->a_vp, LOFSVP(ap->a_vp));
#endif

	return VOP_BMAP(LOFSVP(ap->a_vp), ap->a_bn, ap->a_vpp, ap->a_bnp);
}

lofs_strategy (ap)
	struct vop_strategy_args *ap;
{
	USES_VOP_STRATEGY;
	int error;

#ifdef LOFS_DIAGNOSTIC
	printf("VOP_STRATEGY(vp = %x->%x)\n", ap->a_bp->b_vp, LOFSVP(ap->a_bp->b_vp));
#endif

	PUSHREF(vp, ap->a_bp->b_vp);

	error = VOP_STRATEGY(ap->a_bp);

	POP(vp, ap->a_bp->b_vp);

	return (error);
}

lofs_print (ap)
	struct vop_print_args *ap;
{
	USES_VOP_PRINT;
	struct vnode *targetvp = LOFSVP(ap->a_vp);
	printf("tag VT_LOFS ref ");
	if (targetvp)
		return (VOP_PRINT(targetvp));
	printf("NULLVP\n");
	return (0);
}

lofs_islocked (ap)
	struct vop_islocked_args *ap;
{
	USES_VOP_ISLOCKED;
	struct vnode *targetvp = LOFSVP(ap->a_vp);
	if (targetvp)
		return (VOP_ISLOCKED(targetvp));
	return (0);
}

lofs_advlock (ap)
	struct vop_advlock_args *ap;
{
	USES_VOP_ADVLOCK;
	return VOP_ADVLOCK(LOFSVP(ap->a_vp), ap->a_id, ap->a_op, ap->a_fl, ap->a_flags);
}

/*
 * LOFS directory offset lookup.
 * Currently unsupported.
 */
lofs_blkatoff (ap)
	struct vop_blkatoff_args *ap;
{

	return (EOPNOTSUPP);
}

/*
 * LOFS flat namespace lookup.
 * Currently unsupported.
 */
lofs_vget (ap)
	struct vop_vget_args *ap;
{

	return (EOPNOTSUPP);
}

/*
 * LOFS flat namespace allocation.
 * Currently unsupported.
 */
lofs_valloc (ap)
	struct vop_valloc_args *ap;
{

	return (EOPNOTSUPP);
}

/*
 * LOFS flat namespace free.
 * Currently unsupported.
 */
/*void*/
lofs_vfree (ap)
	struct vop_vfree_args *ap;
{

	return;
}

/*
 * LOFS file truncation.
 */
lofs_truncate (ap)
	struct vop_truncate_args *ap;
{

	/* Use lofs_setattr */
	printf("lofs_truncate: need to implement!!");
	return (EOPNOTSUPP);
}

/*
 * LOFS update.
 */
lofs_update (ap)
	struct vop_update_args *ap;
{

	/* Use lofs_setattr */
	printf("lofs_update: need to implement!!");
	return (EOPNOTSUPP);
}

/*
 * LOFS bwrite
 */
lofs_bwrite (ap)
	struct vop_bwrite_args *ap;
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
	{ &vop_open_desc, lofs_open },		/* open */
	{ &vop_close_desc, lofs_close },		/* close */
	{ &vop_access_desc, lofs_access },		/* access */
	{ &vop_getattr_desc, lofs_getattr },		/* getattr */
	{ &vop_setattr_desc, lofs_setattr },		/* setattr */
	{ &vop_read_desc, lofs_read },		/* read */
	{ &vop_write_desc, lofs_write },		/* write */
	{ &vop_ioctl_desc, lofs_ioctl },		/* ioctl */
	{ &vop_select_desc, lofs_select },		/* select */
	{ &vop_mmap_desc, lofs_mmap },		/* mmap */
	{ &vop_fsync_desc, lofs_fsync },		/* fsync */
	{ &vop_seek_desc, lofs_seek },		/* seek */
	{ &vop_remove_desc, lofs_remove },		/* remove */
	{ &vop_link_desc, lofs_link },		/* link */
	{ &vop_rename_desc, lofs_rename },		/* rename */
	{ &vop_mkdir_desc, lofs_mkdir },		/* mkdir */
	{ &vop_rmdir_desc, lofs_rmdir },		/* rmdir */
	{ &vop_symlink_desc, lofs_symlink },		/* symlink */
	{ &vop_readdir_desc, lofs_readdir },		/* readdir */
	{ &vop_readlink_desc, lofs_readlink },		/* readlink */
	{ &vop_abortop_desc, lofs_abortop },		/* abortop */
	{ &vop_inactive_desc, lofs_inactive },		/* inactive */
	{ &vop_reclaim_desc, lofs_reclaim },		/* reclaim */
	{ &vop_lock_desc, lofs_lock },		/* lock */
	{ &vop_unlock_desc, lofs_unlock },		/* unlock */
	{ &vop_bmap_desc, lofs_bmap },		/* bmap */
	{ &vop_strategy_desc, lofs_strategy },		/* strategy */
	{ &vop_print_desc, lofs_print },		/* print */
	{ &vop_islocked_desc, lofs_islocked },		/* islocked */
	{ &vop_advlock_desc, lofs_advlock },		/* advlock */
	{ &vop_blkatoff_desc, lofs_blkatoff },		/* blkatoff */
	{ &vop_vget_desc, lofs_vget },		/* vget */
	{ &vop_valloc_desc, lofs_valloc },		/* valloc */
	{ &vop_vfree_desc, lofs_vfree },		/* vfree */
	{ &vop_truncate_desc, lofs_truncate },		/* truncate */
	{ &vop_update_desc, lofs_update },		/* update */
	{ &vop_bwrite_desc, lofs_bwrite },		/* bwrite */
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc lofs_vnodeop_opv_desc =
	{ &lofs_vnodeop_p, lofs_vnodeop_entries };
