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
 *	@(#)lofs_vnops.c	1.2 (Berkeley) 6/18/92
 *
 * $Id: lofs_vnops.c,v 1.11 1992/05/30 10:05:43 jsp Exp jsp $
 */

/*
 * Null layer Filesystem
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
	(nd) = NULLTOLOWERVP(v.vnp)

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
null_lookup (ap)
	struct vop_lookup_args *ap;
{
	USES_VOP_LOOKUP;
	struct vnode *dvp = ap->a_dvp;
	struct vnode *newvp;
	struct vnode *targetdvp;
	int error;
	int flag = ap->a_cnp->cn_nameiop /*& OPMASK*/;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_lookup(ap->a_dvp = %x->%x, \"%s\", op = %d)\n",
		dvp, NULLTOLOWERVP(dvp), ap->a_cnp->cn_nameptr, flag);
#endif

	/*
	 * (ap->a_dvp) was locked when passed in, and it will be replaced
	 * with the target vnode, BUT that will already have been
	 * locked when (ap->a_dvp) was locked [see null_lock].  all that
	 * must be done here is to keep track of reference counts.
	 */
	targetdvp = NULLTOLOWERVP(dvp);
	/*VREF(targetdvp);*/
#ifdef NULLFS_DIAGNOSTIC
	vprint("lofs VOP_LOOKUP", targetdvp);
#endif

	/*
	 * Call lookup on the looped vnode
	 */
	error = VOP_LOOKUP(targetdvp, &newvp, ap->a_cnp);
	/*vrele(targetdvp);*/

	if (error) {
		*ap->a_vpp = NULLVP;
#ifdef NULLFS_DIAGNOSTIC
		printf("null_lookup(%x->%x) = %d\n", dvp, NULLTOLOWERVP(dvp), error);
#endif
		return (error);
	}
#ifdef NULLFS_DIAGNOSTIC
	printf("null_lookup(%x->%x) = OK\n", dvp, NULLTOLOWERVP(dvp));
#endif

	*ap->a_vpp = newvp;

	/*
	 * If we just found a directory then make
	 * a loopback node for it and return the loopback
	 * instead of the real vnode.  Otherwise simply
	 * return the aliased directory and vnode.
	 */
	if (newvp && newvp->v_type == VDIR && flag == LOOKUP) {
#ifdef NULLFS_DIAGNOSTIC
		printf("null_lookup: found VDIR\n");
#endif
		/*
		 * At this point, newvp is the vnode to be looped.
		 * Activate a loopback and return the looped vnode.
		 */
		return (make_null_node(dvp->v_mount, ap->a_vpp));
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("null_lookup: not VDIR\n");
#endif

	return (0);
}

/*
 * this = ni_dvp
 * ni_dvp references the locked directory.
 * ni_vp is NULL.
 */
null_mknod (ap)
	struct vop_mknod_args *ap;
{
	USES_VOP_MKNOD;
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_mknod(vp = %x->%x)\n", ap->a_dvp, NULLTOLOWERVP(ap->a_dvp));
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
null_create (ap)
	struct vop_create_args *ap;
{
	USES_VOP_CREATE;
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_create(ap->a_dvp = %x->%x)\n", ap->a_dvp, NULLTOLOWERVP(ap->a_dvp));
#endif

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);

	error = VOP_CREATE(ap->a_dvp, ap->a_vpp, ap->a_cnp, ap->a_vap);

	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

#ifdef NULLFS_DIAGNOSTIC
	printf("null_create(ap->a_dvp = %x->%x)\n", ap->a_dvp, NULLTOLOWERVP(ap->a_dvp));
#endif

	return (error);
}

null_open (ap)
	struct vop_open_args *ap;
{
	USES_VOP_OPEN;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_open(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_OPEN(NULLTOLOWERVP(ap->a_vp), ap->a_mode, ap->a_cred, ap->a_p);
}

null_close (ap)
	struct vop_close_args *ap;
{
	USES_VOP_CLOSE;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_close(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_CLOSE(NULLTOLOWERVP(ap->a_vp), ap->a_fflag, ap->a_cred, ap->a_p);
}

null_access (ap)
	struct vop_access_args *ap;
{
	USES_VOP_ACCESS;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_access(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_ACCESS(NULLTOLOWERVP(ap->a_vp), ap->a_mode, ap->a_cred, ap->a_p);
}

null_getattr (ap)
	struct vop_getattr_args *ap;
{
	USES_VOP_GETATTR;
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_getattr(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	/*
	 * Get the stats from the underlying filesystem
	 */
	error = VOP_GETATTR(NULLTOLOWERVP(ap->a_vp), ap->a_vap, ap->a_cred, ap->a_p);
	if (error)
		return (error);
	/*
	 * and replace the fsid field with the loopback number
	 * to preserve the namespace.
	 */
	ap->a_vap->va_fsid = ap->a_vp->v_mount->mnt_stat.f_fsid.val[0];
	return (0);
}

null_setattr (ap)
	struct vop_setattr_args *ap;
{
	USES_VOP_SETATTR;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_setattr(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_SETATTR(NULLTOLOWERVP(ap->a_vp), ap->a_vap, ap->a_cred, ap->a_p);
}

null_read (ap)
	struct vop_read_args *ap;
{
	USES_VOP_READ;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_read(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_READ(NULLTOLOWERVP(ap->a_vp), ap->a_uio, ap->a_ioflag, ap->a_cred);
}

null_write (ap)
	struct vop_write_args *ap;
{
	USES_VOP_WRITE;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_write(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_WRITE(NULLTOLOWERVP(ap->a_vp), ap->a_uio, ap->a_ioflag, ap->a_cred);
}

null_ioctl (ap)
	struct vop_ioctl_args *ap;
{
	USES_VOP_IOCTL;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_ioctl(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_IOCTL(NULLTOLOWERVP(ap->a_vp), ap->a_command, ap->a_data, ap->a_fflag, ap->a_cred, ap->a_p);
}

null_select (ap)
	struct vop_select_args *ap;
{
	USES_VOP_SELECT;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_select(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_SELECT(NULLTOLOWERVP(ap->a_vp), ap->a_which, ap->a_fflags, ap->a_cred, ap->a_p);
}

null_mmap (ap)
	struct vop_mmap_args *ap;
{
	USES_VOP_MMAP;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_mmap(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_MMAP(NULLTOLOWERVP(ap->a_vp), ap->a_fflags, ap->a_cred, ap->a_p);
}

null_fsync (ap)
	struct vop_fsync_args *ap;
{
	USES_VOP_FSYNC;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_fsync(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_FSYNC(NULLTOLOWERVP(ap->a_vp), ap->a_fflags, ap->a_cred, ap->a_waitfor, ap->a_p);
}

null_seek (ap)
	struct vop_seek_args *ap;
{
	USES_VOP_SEEK;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_seek(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_SEEK(NULLTOLOWERVP(ap->a_vp), ap->a_oldoff, ap->a_newoff, ap->a_cred);
}

null_remove (ap)
	struct vop_remove_args *ap;
{
	USES_VOP_REMOVE;
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_remove(ap->a_vp = %x->%x)\n", ap->a_dvp, NULLTOLOWERVP(ap->a_dvp));
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
null_link (ap)
	struct vop_link_args *ap;
{
	USES_VOP_LINK;
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_link(ap->a_tdvp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	PUSHREF(xdvp, ap->a_vp);
	VREF(ap->a_vp);

	error = VOP_LINK(ap->a_vp, NULLTOLOWERVP(ap->a_tdvp), ap->a_cnp);

	POP(xdvp, ap->a_vp);
	vrele(ap->a_vp);

	return (error);
}

null_rename (ap)
	struct vop_rename_args *ap;
{
	USES_VOP_RENAME;
	struct vnode *fvp, *tvp;
	struct vnode *tdvp;
#if 0
	struct vnode *fsvp, *tsvp;
#endif
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename(fdvp = %x->%x)\n", ap->a_fdvp, NULLTOLOWERVP(ap->a_fdvp));
	/*printf("null_rename(tdvp = %x->%x)\n", tndp->ni_dvp, NULLTOLOWERVP(tndp->ni_dvp));*/
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch source dvp\n");
#endif
	/*
	 * Switch source directory to point to lofsed vnode
	 */
	PUSHREF(fdvp, ap->a_fdvp);
	VREF(ap->a_fdvp);

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch source vp\n");
#endif
	/*
	 * And source object if it is lofsed...
	 */
	fvp = ap->a_fvp;
	if (fvp && fvp->v_op == null_vnodeop_p) {
		ap->a_fvp = NULLTOLOWERVP(fvp);
		VREF(ap->a_fvp);
	} else {
		fvp = 0;
	}

#if 0
#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch source start vp\n");
#endif
	/*
	 * And source startdir object if it is lofsed...
	 */
	fsvp = fndp->ni_startdir;
	if (fsvp && fsvp->v_op == null_vnodeop_p) {
		fndp->ni_startdir = NULLTOLOWERVP(fsvp);
		VREF(fndp->ni_startdir);
	} else {
		fsvp = 0;
	}
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch target dvp\n");
#endif
	/*
 	 * Switch target directory to point to lofsed vnode
	 */
	tdvp = ap->a_tdvp;
	if (tdvp && tdvp->v_op == null_vnodeop_p) {
		ap->a_tdvp = NULLTOLOWERVP(tdvp);
		VREF(ap->a_tdvp);
	} else {
		tdvp = 0;
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch target vp\n");
#endif
	/*
	 * And target object if it is lofsed...
	 */
	tvp = ap->a_tvp;
	if (tvp && tvp->v_op == null_vnodeop_p) {
		ap->a_tvp = NULLTOLOWERVP(tvp);
		VREF(ap->a_tvp);
	} else {
		tvp = 0;
	}

#if 0
#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - switch target start vp\n");
#endif
	/*
	 * And target startdir object if it is lofsed...
	 */
	tsvp = tndp->ni_startdir;
	if (tsvp && tsvp->v_op == null_vnodeop_p) {
		tndp->ni_startdir = NULLTOLOWERVP(fsvp);
		VREF(tndp->ni_startdir);
	} else {
		tsvp = 0;
	}
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - VOP_RENAME(%x, %x, %x, %x)\n",
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
#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore target startdir\n");
#endif

	if (tsvp) {
		if (tndp->ni_startdir)
			vrele(tndp->ni_startdir);
		tndp->ni_startdir = tsvp;
	}
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore target vp\n");
#endif

	if (tvp) {
		ap->a_tvp = tvp;
		vrele(ap->a_tvp);
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore target dvp\n");
#endif

	if (tdvp) {
		ap->a_tdvp = tdvp;
		vrele(ap->a_tdvp);
	}

#if 0
#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore source startdir\n");
#endif

	if (fsvp) {
		if (fndp->ni_startdir)
			vrele(fndp->ni_startdir);
		fndp->ni_startdir = fsvp;
	}
#endif

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore source vp\n");
#endif


	if (fvp) {
		ap->a_fvp = fvp;
		vrele(ap->a_fvp);
	}

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rename - restore source dvp\n");
#endif

	POP(fdvp, ap->a_fdvp);
	vrele(ap->a_fdvp);

	return (error);
}

/*
 * ni_dvp is the locked (alias) parent.
 * ni_vp is NULL.
 */
null_mkdir (ap)
	struct vop_mkdir_args *ap;
{
	USES_VOP_MKDIR;
	int error;
	struct vnode *dvp = ap->a_dvp;
	struct vnode *xdvp;
	struct vnode *newvp;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_mkdir(vp = %x->%x)\n", dvp, NULLTOLOWERVP(dvp));
#endif

	xdvp = dvp;
	dvp = NULLTOLOWERVP(xdvp);
	/*VREF(dvp);*/

	error = VOP_MKDIR(dvp, &newvp, ap->a_cnp, ap->a_vap);

	if (error) {
		*ap->a_vpp = NULLVP;
		/*vrele(xdvp);*/
		return (error);
	}

	/*
	 * Make a new lofs node
	 */
	/*VREF(dvp);*/

	error = make_null_node(dvp->v_mount, &newvp);

	*ap->a_vpp = newvp;

	return (error);
}

/*
 * ni_dvp is the locked parent.
 * ni_vp is the entry to be removed.
 */
null_rmdir (ap)
	struct vop_rmdir_args *ap;
{
	USES_VOP_RMDIR;
	struct vnode *vp = ap->a_vp;
	struct vnode *dvp = ap->a_dvp;
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_rmdir(dvp = %x->%x)\n", dvp, NULLTOLOWERVP(dvp));
#endif

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
null_symlink (ap)
	struct vop_symlink_args *ap;
{
	USES_VOP_SYMLINK;
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("VOP_SYMLINK(vp = %x->%x)\n", ap->a_dvp, NULLTOLOWERVP(ap->a_dvp));
#endif

	PUSHREF(xdvp, ap->a_dvp);
	VREF(ap->a_dvp);

	error = VOP_SYMLINK(ap->a_dvp, ap->a_vpp, ap->a_cnp, ap->a_vap, ap->a_target);

	POP(xdvp, ap->a_dvp);
	vrele(ap->a_dvp);

	return (error);
}

null_readdir (ap)
	struct vop_readdir_args *ap;
{
	USES_VOP_READDIR;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_readdir(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_READDIR(NULLTOLOWERVP(ap->a_vp), ap->a_uio, ap->a_cred, ap->a_eofflagp);
}

null_readlink (ap)
	struct vop_readlink_args *ap;
{
	USES_VOP_READLINK;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_readlink(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_READLINK(NULLTOLOWERVP(ap->a_vp), ap->a_uio, ap->a_cred);
}

/*
 * Anyone's guess...
 */
null_abortop (ap)
	struct vop_abortop_args *ap;
{
	USES_VOP_ABORTOP;
	int error;

	PUSHREF(xdvp, ap->a_dvp);

	error = VOP_ABORTOP(ap->a_dvp, ap->a_cnp);

	POP(xdvp, ap->a_dvp);

	return (error);
}

null_inactive (ap)
	struct vop_inactive_args *ap;
{
	USES_VOP_INACTIVE;
	struct vnode *targetvp = NULLTOLOWERVP(ap->a_vp);
#ifdef NULLFS_DIAGNOSTIC
	printf("null_inactive(ap->a_vp = %x->%x)\n", ap->a_vp, targetvp);
#endif

#ifdef DIAGNOSTIC
	{ extern int prtactive;
	if (prtactive && ap->a_vp->v_usecount != 0)
		vprint("null_inactive: pushing active", ap->a_vp);
	}
#endif

	if (targetvp) {
		vrele(targetvp);
		VTONULLNODE(ap->a_vp)->null_lowervp = 0;
	}
}

null_reclaim (ap)
	struct vop_reclaim_args *ap;
{
	USES_VOP_RECLAIM;
	struct vnode *targetvp;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_reclaim(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif
	remque(VTONULLNODE(ap->a_vp));
	targetvp = NULLTOLOWERVP(ap->a_vp);
	if (targetvp) {
		printf("lofs: delayed vrele of %x\n", targetvp);
		vrele(targetvp);	/* XXX should never happen */
	}
	FREE(ap->a_vp->v_data, M_TEMP);
	ap->a_vp->v_data = 0;
	return (0);
}

null_lock (ap)
	struct vop_lock_args *ap;
{
	USES_VOP_LOCK;
	int error;
	struct vnode *targetvp = NULLTOLOWERVP(ap->a_vp);

#ifdef NULLFS_DIAGNOSTIC
	printf("null_lock(ap->a_vp = %x->%x)\n", ap->a_vp, targetvp);
	/*vprint("null_lock ap->a_vp", ap->a_vp);
	if (targetvp)
		vprint("null_lock ->ap->a_vp", targetvp);
	else
		printf("null_lock ->ap->a_vp = NIL\n");*/
#endif

	if (targetvp) {
		error = VOP_LOCK(targetvp);
		if (error)
			return (error);
	}

	return (0);
}

null_unlock (ap)
	struct vop_unlock_args *ap;
{
	USES_VOP_UNLOCK;
	struct vnode *targetvp = NULLTOLOWERVP(ap->a_vp);

#ifdef NULLFS_DIAGNOSTIC
	printf("null_unlock(ap->a_vp = %x->%x)\n", ap->a_vp, targetvp);
#endif

	if (targetvp)
		return (VOP_UNLOCK(targetvp));
	return (0);
}

null_bmap (ap)
	struct vop_bmap_args *ap;
{
	USES_VOP_BMAP;
#ifdef NULLFS_DIAGNOSTIC
	printf("null_bmap(ap->a_vp = %x->%x)\n", ap->a_vp, NULLTOLOWERVP(ap->a_vp));
#endif

	return VOP_BMAP(NULLTOLOWERVP(ap->a_vp), ap->a_bn, ap->a_vpp, ap->a_bnp);
}

null_strategy (ap)
	struct vop_strategy_args *ap;
{
	USES_VOP_STRATEGY;
	int error;

#ifdef NULLFS_DIAGNOSTIC
	printf("null_strategy(vp = %x->%x)\n", ap->a_bp->b_vp, NULLTOLOWERVP(ap->a_bp->b_vp));
#endif

	PUSHREF(vp, ap->a_bp->b_vp);

	error = VOP_STRATEGY(ap->a_bp);

	POP(vp, ap->a_bp->b_vp);

	return (error);
}

null_print (ap)
	struct vop_print_args *ap;
{
	USES_VOP_PRINT;
	struct vnode *targetvp = NULLTOLOWERVP(ap->a_vp);
	printf("tag VT_LOFS ref ");
	if (targetvp)
		return (VOP_PRINT(targetvp));
	printf("NULLVP\n");
	return (0);
}

null_islocked (ap)
	struct vop_islocked_args *ap;
{
	USES_VOP_ISLOCKED;
	struct vnode *targetvp = NULLTOLOWERVP(ap->a_vp);
	if (targetvp)
		return (VOP_ISLOCKED(targetvp));
	return (0);
}

null_advlock (ap)
	struct vop_advlock_args *ap;
{
	USES_VOP_ADVLOCK;
	return VOP_ADVLOCK(NULLTOLOWERVP(ap->a_vp), ap->a_id, ap->a_op, ap->a_fl, ap->a_flags);
}

/*
 * LOFS directory offset lookup.
 * Currently unsupported.
 */
null_blkatoff (ap)
	struct vop_blkatoff_args *ap;
{

	return (EOPNOTSUPP);
}

/*
 * LOFS flat namespace lookup.
 * Currently unsupported.
 */
null_vget (ap)
	struct vop_vget_args *ap;
{

	return (EOPNOTSUPP);
}

/*
 * LOFS flat namespace allocation.
 * Currently unsupported.
 */
null_valloc (ap)
	struct vop_valloc_args *ap;
{

	return (EOPNOTSUPP);
}

/*
 * LOFS flat namespace free.
 * Currently unsupported.
 */
/*void*/
null_vfree (ap)
	struct vop_vfree_args *ap;
{

	return;
}

/*
 * LOFS file truncation.
 */
null_truncate (ap)
	struct vop_truncate_args *ap;
{

	/* Use null_setattr */
	printf("null_truncate: need to implement!!");
	return (EOPNOTSUPP);
}

/*
 * LOFS update.
 */
null_update (ap)
	struct vop_update_args *ap;
{

	/* Use null_setattr */
	printf("null_update: need to implement!!");
	return (EOPNOTSUPP);
}

/*
 * LOFS bwrite
 */
null_bwrite (ap)
	struct vop_bwrite_args *ap;
{
	return (EOPNOTSUPP);
}

/*
 * Global vfs data structures for ufs
 */
int (**null_vnodeop_p)();
struct vnodeopv_entry_desc lofs_vnodeop_entries[] = {
	{ &vop_default_desc, vn_default_error },
	{ &vop_lookup_desc, null_lookup },		/* lookup */
	{ &vop_create_desc, null_create },		/* create */
	{ &vop_mknod_desc, null_mknod },		/* mknod */
	{ &vop_open_desc, null_open },		/* open */
	{ &vop_close_desc, null_close },		/* close */
	{ &vop_access_desc, null_access },		/* access */
	{ &vop_getattr_desc, null_getattr },		/* getattr */
	{ &vop_setattr_desc, null_setattr },		/* setattr */
	{ &vop_read_desc, null_read },		/* read */
	{ &vop_write_desc, null_write },		/* write */
	{ &vop_ioctl_desc, null_ioctl },		/* ioctl */
	{ &vop_select_desc, null_select },		/* select */
	{ &vop_mmap_desc, null_mmap },		/* mmap */
	{ &vop_fsync_desc, null_fsync },		/* fsync */
	{ &vop_seek_desc, null_seek },		/* seek */
	{ &vop_remove_desc, null_remove },		/* remove */
	{ &vop_link_desc, null_link },		/* link */
	{ &vop_rename_desc, null_rename },		/* rename */
	{ &vop_mkdir_desc, null_mkdir },		/* mkdir */
	{ &vop_rmdir_desc, null_rmdir },		/* rmdir */
	{ &vop_symlink_desc, null_symlink },		/* symlink */
	{ &vop_readdir_desc, null_readdir },		/* readdir */
	{ &vop_readlink_desc, null_readlink },		/* readlink */
	{ &vop_abortop_desc, null_abortop },		/* abortop */
	{ &vop_inactive_desc, null_inactive },		/* inactive */
	{ &vop_reclaim_desc, null_reclaim },		/* reclaim */
	{ &vop_lock_desc, null_lock },		/* lock */
	{ &vop_unlock_desc, null_unlock },		/* unlock */
	{ &vop_bmap_desc, null_bmap },		/* bmap */
	{ &vop_strategy_desc, null_strategy },		/* strategy */
	{ &vop_print_desc, null_print },		/* print */
	{ &vop_islocked_desc, null_islocked },		/* islocked */
	{ &vop_advlock_desc, null_advlock },		/* advlock */
	{ &vop_blkatoff_desc, null_blkatoff },		/* blkatoff */
	{ &vop_vget_desc, null_vget },		/* vget */
	{ &vop_valloc_desc, null_valloc },		/* valloc */
	{ &vop_vfree_desc, null_vfree },		/* vfree */
	{ &vop_truncate_desc, null_truncate },		/* truncate */
	{ &vop_update_desc, null_update },		/* update */
	{ &vop_bwrite_desc, null_bwrite },		/* bwrite */
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc lofs_vnodeop_opv_desc =
	{ &null_vnodeop_p, lofs_vnodeop_entries };
