/*
 * Copyright (c) 1992, 1993, 1994 The Regents of the University of California.
 * Copyright (c) 1992, 1993, 1994 Jan-Simon Pendry.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry and by John Heidemann of the UCLA Ficus project.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)union_vnops.c	1.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/file.h>
#include <sys/filedesc.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/namei.h>
#include <sys/malloc.h>
#include <sys/buf.h>
#include "union.h"


int union_bug_bypass = 0;   /* for debugging: enables bypass printf'ing */

/*
 * This is the 10-Apr-92 bypass routine.
 *    This version has been optimized for speed, throwing away some
 * safety checks.  It should still always work, but it's not as
 * robust to programmer errors.
 *    Define SAFETY to include some error checking code.
 *
 * In general, we map all vnodes going down and unmap them on the way back.
 * As an exception to this, vnodes can be marked "unmapped" by setting
 * the Nth bit in operation's vdesc_flags.
 *
 * Also, some BSD vnode operations have the side effect of vrele'ing
 * their arguments.  With stacking, the reference counts are held
 * by the upper node, not the lower one, so we must handle these
 * side-effects here.  This is not of concern in Sun-derived systems
 * since there are no such side-effects.
 *
 * This makes the following assumptions:
 * - only one returned vpp
 * - no INOUT vpp's (Sun's vop_open has one of these)
 * - the vnode operation vector of the first vnode should be used
 *   to determine what implementation of the op should be invoked
 * - all mapped vnodes are of our vnode-type (NEEDSWORK:
 *   problems on rmdir'ing mount points and renaming?)
 */ 
int
union_bypass(ap)
	struct vop_generic_args /* {
		struct vnodeop_desc *a_desc;
		<other random data follows, presumably>
	} */ *ap;
{
	struct vnode **this_vp_p;
	int error;
	struct vnode *old_vps[VDESC_MAX_VPS];
	struct vnode **vps_p[VDESC_MAX_VPS];
	struct vnode ***vppp;
	struct vnodeop_desc *descp = ap->a_desc;
	int reles, i;

	if (union_bug_bypass)
		printf ("union_bypass: %s\n", descp->vdesc_name);

#ifdef SAFETY
	/*
	 * We require at least one vp.
	 */
	if (descp->vdesc_vp_offsets == NULL ||
	    descp->vdesc_vp_offsets[0] == VDESC_NO_OFFSET)
		panic ("union_bypass: no vp's in map.\n");
#endif

	/*
	 * Map the vnodes going in.
	 * Later, we'll invoke the operation based on
	 * the first mapped vnode's operation vector.
	 */
	reles = descp->vdesc_flags;
	for (i = 0; i < VDESC_MAX_VPS; reles >>= 1, i++) {
		if (descp->vdesc_vp_offsets[i] == VDESC_NO_OFFSET)
			break;   /* bail out at end of list */
		vps_p[i] = this_vp_p = 
			VOPARG_OFFSETTO(struct vnode **, descp->vdesc_vp_offsets[i],ap);
		/*
		 * We're not guaranteed that any but the first vnode
		 * are of our type.  Check for and don't map any
		 * that aren't.  (We must always map first vp or vclean fails.)
		 */
		if (i && (*this_vp_p)->v_op != union_vnodeop_p) {
			old_vps[i] = NULL;
		} else {
			old_vps[i] = *this_vp_p;
			*(vps_p[i]) = OTHERVP(*this_vp_p);
			/*
			 * XXX - Several operations have the side effect
			 * of vrele'ing their vp's.  We must account for
			 * that.  (This should go away in the future.)
			 */
			if (reles & 1)
				VREF(*this_vp_p);
		}
			
	}

	/*
	 * Call the operation on the lower layer
	 * with the modified argument structure.
	 */
	error = VCALL(*(vps_p[0]), descp->vdesc_offset, ap);

	/*
	 * Maintain the illusion of call-by-value
	 * by restoring vnodes in the argument structure
	 * to their original value.
	 */
	reles = descp->vdesc_flags;
	for (i = 0; i < VDESC_MAX_VPS; reles >>= 1, i++) {
		if (descp->vdesc_vp_offsets[i] == VDESC_NO_OFFSET)
			break;   /* bail out at end of list */
		if (old_vps[i]) {
			*(vps_p[i]) = old_vps[i];
			if (reles & 1)
				vrele(*(vps_p[i]));
		}
	}

	/*
	 * Map the possible out-going vpp
	 * (Assumes that the lower layer always returns
	 * a VREF'ed vpp unless it gets an error.)
	 */
	if (descp->vdesc_vpp_offset != VDESC_NO_OFFSET &&
	    !(descp->vdesc_flags & VDESC_NOMAP_VPP) &&
	    !error) {
		/*
		 * XXX - even though some ops have vpp returned vp's,
		 * several ops actually vrele this before returning.
		 * We must avoid these ops.
		 * (This should go away when these ops are regularized.)
		 */
		if (descp->vdesc_flags & VDESC_VPP_WILLRELE)
			goto out;
		vppp = VOPARG_OFFSETTO(struct vnode***,
				 descp->vdesc_vpp_offset,ap);
		panic("union: failed to handled returned vnode");
		error = union_allocvp(0, 0, 0, 0, 0, 0);
	}

out:
	return (error);
}

/*
 * Check access permission on the union vnode.
 * The access check being enforced is to check
 * against both the underlying vnode, and any
 * copied vnode.  This ensures that no additional
 * file permissions are given away simply because
 * the user caused an implicit file copy.
 */
int
union_access(ap)
	struct vop_access_args /* {
		struct vnodeop_desc *a_desc;
		struct vnode *a_vp;
		int a_mode;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{
	struct union_node *un = VTOUNION(ap->a_vp);
	struct vnode *vp;

	if (vp = un->un_lowervp) {
		int error;

		error = VOP_ACCESS(vp, ap->a_mode, ap->a_cred, ap->a_p);
		if (error)
			return (error);
	}

	if (vp = un->un_uppervp)
		return (VOP_ACCESS(vp, ap->a_mode, ap->a_cred, ap->a_p));
	
	return (0);
}

static int
union_mkshadow(dvp, cnp, vpp)
	struct vnode *dvp;
	struct componentname *cnp;
	struct vnode *vpp;
{
	int error;
	struct vattr va;
	struct proc *p = cnp->cn_proc;
	int lockparent = (cnp->cn_flags & LOCKPARENT);

	/*
	 * policy: when creating the shadow directory in the
	 * upper layer, create it owned by the current user,
	 * group from parent directory, and mode 777 modified
	 * by umask (ie mostly identical to the mkdir syscall).
	 * (jsp, kb)
	 * TODO: create the directory owned by the user who
	 * did the mount (um->um_cred).
	 */

	VATTR_NULL(&va);
	va.va_type = VDIR;
	va.va_mode = UN_DIRMODE &~ p->p_fd->fd_cmask;
	if (lockparent)
		VOP_UNLOCK(dvp);
	LEASE_CHECK(dvp, p, p->p_ucred, LEASE_WRITE);
	VOP_LOCK(dvp);
	error = VOP_MKDIR(dvp, vpp, cnp, &va);
	if (lockparent)
		VOP_LOCK(dvp);
	return (error);
}

static int
union_lookup1(dvp, vpp, cnp)
	struct vnode *dvp;
	struct vnode **vpp;
	struct componentname *cnp;
{
	int error;
	struct vnode *tdvp;
	struct mount *mp;

	if (cnp->cn_flags & ISDOTDOT) {
		for (;;) {
			if ((dvp->v_flag & VROOT) == 0 ||
			    (cnp->cn_flags & NOCROSSMOUNT))
				break;

			tdvp = dvp;
			dvp = dvp->v_mount->mnt_vnodecovered;
			vput(tdvp);
			VREF(dvp);
			VOP_LOCK(dvp);
		}
	}
	
        error = VOP_LOOKUP(dvp, &tdvp, cnp);
	if (error)
		return (error);

	dvp = tdvp;
	while ((dvp->v_type == VDIR) && (mp = dvp->v_mountedhere) &&
	       (cnp->cn_flags & NOCROSSMOUNT) == 0) {

		if (mp->mnt_flag & MNT_MLOCK) {
			mp->mnt_flag |= MNT_MWAIT;
			sleep((caddr_t) mp, PVFS);
			continue;
		}

		if (error = VFS_ROOT(mp, &tdvp)) {
			vput(dvp);
			return (error);
		}

		vput(tdvp);
		dvp = tdvp;
	}

	*vpp = dvp;
	return (0);
}

int
union_lookup(ap)
	struct vop_lookup_args /* {
		struct vnodeop_desc *a_desc;
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
	} */ *ap;
{
	int uerror, lerror;
	struct vnode *uppervp, *lowervp;
	struct vnode *upperdvp, *lowerdvp;
	struct vnode *dvp = ap->a_dvp;
	struct union_node *dun = VTOUNION(ap->a_dvp);
	struct componentname *cnp = ap->a_cnp;
	int lockparent = cnp->cn_flags & LOCKPARENT;

	upperdvp = dun->un_uppervp;
	lowerdvp = dun->un_lowervp;

	/*
	 * do the lookup in the upper level.
	 * if that level comsumes additional pathnames,
	 * then assume that something special is going
	 * on and just return that vnode.
	 */
	uppervp = 0;
	if (upperdvp) {
		uerror = union_lookup1(upperdvp, &uppervp, cnp);
		if (cnp->cn_consume != 0) {
			*ap->a_vpp = uppervp;
			return (uerror);
		}
		if (!lockparent)
			VOP_LOCK(upperdvp);
	} else {
		uerror = ENOENT;
	}

	/*
	 * in a similar way to the upper layer, do the lookup
	 * in the lower layer.   this time, if there is some
	 * component magic going on, then vput whatever we got
	 * back from the upper layer and return the lower vnode
	 * instead.
	 */
	lowervp = 0;
	if (lowerdvp) {
		lerror = union_lookup1(lowerdvp, &lowervp, cnp);
		if (cnp->cn_consume != 0) {
			if (uppervp) {
				vput(uppervp);
				uppervp = 0;
			}
			*ap->a_vpp = lowervp;
			return (lerror);
		}
		if (!lockparent)
			VOP_LOCK(lowerdvp);
	} else {
		lerror = ENOENT;
	}

	/*
	 * at this point, we have uerror and lerror indicating
	 * possible errors with the lookups in the upper and lower
	 * layers.  additionally, uppervp and lowervp are (locked)
	 * references to existing vnodes in the upper and lower layers.
	 *
	 * there are now three cases to consider.
	 * 1. if both layers returned an error, then return whatever
	 *    error the upper layer generated.
	 *
	 * 2. if the top layer failed and the bottom layer succeeded
	 *    then two subcases occur.
	 *    a.  the bottom vnode is not a directory, in which
	 *	  case just return a new union vnode referencing
	 *	  an empty top layer and the existing bottom layer.
	 *    b.  the bottom vnode is a directory, in which case
	 *	  create a new directory in the top-level and
	 *	  continue as in case 3.
	 *
	 * 3. if the top layer succeeded then return a new union
	 *    vnode referencing whatever the new top layer and
	 *    whatever the bottom layer returned.
	 */

	/* case 1. */
	if ((uerror != 0) && (lerror != 0)) {
		*ap->a_vpp = 0;
		return (uerror);
	}

	/* case 2. */
	if (uerror != 0 /* && (lerror == 0) */ ) {
		if (lowervp->v_type == VDIR) { /* case 2b. */
			uerror = union_mkshadow(upperdvp, cnp, &uppervp);
			if (uerror) {
				if (lowervp) {
					vput(lowervp);
					lowervp = 0;
				}
				return (uerror);
			}
		}
	}

	return (union_allocvp(ap->a_vpp, dvp->v_mount, dvp, cnp,
			      uppervp, lowervp));
}

/*
 * copyfile.  copy the vnode (fvp) to the vnode (tvp)
 * using a sequence of reads and writes.
 */
static int
union_copyfile(p, cred, fvp, tvp)
	struct proc *p;
	struct ucred *cred;
	struct vnode *fvp;
	struct vnode *tvp;
{
	char *buf;
	struct uio uio;
	struct iovec iov;
	int error = 0;
	off_t offset;

	/*
	 * strategy:
	 * allocate a buffer of size MAXBSIZE.
	 * loop doing reads and writes, keeping track
	 * of the current uio offset.
	 * give up at the first sign of trouble.
	 */

	uio.uio_procp = p;
	uio.uio_segflg = UIO_SYSSPACE;
	offset = 0;

	VOP_UNLOCK(fvp);				/* XXX */
	LEASE_CHECK(fvp, p, cred, LEASE_READ);
	VOP_LOCK(fvp);					/* XXX */
	VOP_UNLOCK(tvp);				/* XXX */
	LEASE_CHECK(tvp, p, cred, LEASE_WRITE);
	VOP_LOCK(tvp);					/* XXX */

	buf = malloc(MAXBSIZE, M_TEMP, M_WAITOK);
	do {
		uio.uio_iov = &iov;
		uio.uio_iovcnt = 1;
		iov.iov_base = buf;
		iov.iov_len = MAXBSIZE;
		uio.uio_resid = iov.iov_len;
		uio.uio_offset = offset;
		uio.uio_rw = UIO_READ;
		error = VOP_READ(fvp, &uio, 0, cred);

		if (error == 0) {
			uio.uio_iov = &iov;
			uio.uio_iovcnt = 1;
			iov.iov_base = buf;
			iov.iov_len = MAXBSIZE - uio.uio_resid;
			uio.uio_rw = UIO_WRITE;
			uio.uio_resid = iov.iov_len;
			uio.uio_offset = offset;

			do {
				error = VOP_WRITE(tvp, &uio, 0, cred);
			} while (error == 0 && uio.uio_resid > 0);
			if (error == 0)
				offset = uio.uio_offset;
		}
	} while ((uio.uio_resid == 0) && (error == 0));

	free(buf, M_TEMP);
	return (error);
}

int
union_open(ap)
	struct vop_open_args /* {
		struct vnodeop_desc *a_desc;
		struct vnode *a_vp;
		int a_mode;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{
	struct union_node *un = VTOUNION(ap->a_vp);
	int mode = ap->a_mode;
	struct ucred *cred = ap->a_cred;
	struct proc *p = ap->a_p;

	/*
	 * If there is an existing upper vp then simply open that.
	 */
	if (un->un_uppervp)
		return (VOP_OPEN(un->un_uppervp, mode, cred, p));

	/*
	 * If the lower vnode is being opened for writing, then
	 * copy the file contents to the upper vnode and open that,
	 * otherwise can simply open the lower vnode.
	 */
	if ((ap->a_mode & FWRITE) && (un->un_lowervp->v_type == VREG)) {
		int error;
		struct nameidata nd;
		struct filedesc *fdp = p->p_fd;
		int fmode;
		int cmode;

		/*
		 * Open the named file in the upper layer.  Note that
		 * the file may have come into existence *since* the lookup
		 * was done, since the upper layer may really be a
		 * loopback mount of some other filesystem... so open
		 * the file with exclusive create and barf if it already
		 * exists.
		 * XXX - perhaps shoudl re-lookup the node (once more with
		 * feeling) and simply open that.  Who knows.
		 */
		NDINIT(&nd, CREATE, 0, UIO_SYSSPACE, un->un_path, p);
		fmode = (O_CREAT|O_TRUNC|O_EXCL);
		cmode = UN_FILEMODE & ~fdp->fd_cmask;
		error = vn_open(&nd, fmode, cmode);
		if (error)
			return (error);
		un->un_uppervp = nd.ni_vp;
		/*
		 * Now, if the file is being opened with truncation, then
		 * the (new) upper vnode is ready to fly, otherwise the
		 * data from the lower vnode must be copied to the upper
		 * layer first.  This only works for regular files (check
		 * is made above).
		 */
		if ((mode & O_TRUNC) == 0) {
			/* XXX - should not ignore errors from VOP_CLOSE */
			error = VOP_OPEN(un->un_lowervp, FREAD, cred, p);
			if (error == 0) {
				error = union_copyfile(p, cred,
					       un->un_lowervp, un->un_uppervp);
				(void) VOP_CLOSE(un->un_lowervp, FREAD);
			}
			(void) VOP_CLOSE(un->un_uppervp, FWRITE);
		}
		if (error == 0)
			error = VOP_OPEN(un->un_uppervp, FREAD, cred, p);
		return (error);
	}

	return (VOP_OPEN(un->un_lowervp, mode, cred, p));
}

/*
 *  We handle getattr only to change the fsid.
 */
int
union_getattr(ap)
	struct vop_getattr_args /* {
		struct vnode *a_vp;
		struct vattr *a_vap;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{
	int error;

	if (error = union_bypass(ap))
		return (error);
	/* Requires that arguments be restored. */
	ap->a_vap->va_fsid = ap->a_vp->v_mount->mnt_stat.f_fsid.val[0];
	return (0);
}

/*
 * union_readdir works in concert with getdirentries and
 * readdir(3) to provide a list of entries in the unioned
 * directories.  getdirentries is responsible for walking
 * down the union stack.  readdir(3) is responsible for
 * eliminating duplicate names from the returned data stream.
 */
int
union_readdir(ap)
	struct vop_readdir_args /* {
		struct vnodeop_desc *a_desc;
		struct vnode *a_vp;
		struct uio *a_uio;
		struct ucred *a_cred;
	} */ *ap;
{
	struct union_node *un = VTOUNION(ap->a_vp);

	if (un->un_uppervp)
		return (union_bypass(ap));

	return (0);
}

int
union_inactive(ap)
	struct vop_inactive_args /* {
		struct vnode *a_vp;
	} */ *ap;
{

	/*
	 * Do nothing (and _don't_ bypass).
	 * Wait to vrele lowervp until reclaim,
	 * so that until then our union_node is in the
	 * cache and reusable.
	 *
	 * NEEDSWORK: Someday, consider inactive'ing
	 * the lowervp and then trying to reactivate it
	 * with capabilities (v_id)
	 * like they do in the name lookup cache code.
	 * That's too much work for now.
	 */
	return (0);
}

int
union_reclaim(ap)
	struct vop_reclaim_args /* {
		struct vnode *a_vp;
	} */ *ap;
{
	struct vnode *vp = ap->a_vp;
	struct union_node *un = VTOUNION(vp);
	struct vnode *uppervp = un->un_uppervp;
	struct vnode *lowervp = un->un_lowervp;
	struct vnode *dirvp = un->un_dirvp;
	char *path = un->un_path;

	/*
	 * Note: in vop_reclaim, vp->v_op == dead_vnodeop_p,
	 * so we can't call VOPs on ourself.
	 */
	/* After this assignment, this node will not be re-used. */
	un->un_uppervp = 0;
	un->un_lowervp = 0;
	un->un_dirvp = 0;
	un->un_path = NULL;
	union_freevp(vp);
	if (uppervp)
		vrele(uppervp);
	if (lowervp)
		vrele(lowervp);
	if (dirvp)
		vrele(dirvp);
	if (path)
		free(path, M_TEMP);
	return (0);
}


int
union_print(ap)
	struct vop_print_args /* {
		struct vnode *a_vp;
	} */ *ap;
{
	struct vnode *vp = ap->a_vp;

	printf("\ttag VT_UNION, vp=%x, uppervp=%x, lowervp=%x\n",
			vp, UPPERVP(vp), LOWERVP(vp));
	return (0);
}


/*
 * XXX - vop_strategy must be hand coded because it has no
 * vnode in its arguments.
 * This goes away with a merged VM/buffer cache.
 */
int
union_strategy(ap)
	struct vop_strategy_args /* {
		struct buf *a_bp;
	} */ *ap;
{
	struct buf *bp = ap->a_bp;
	int error;
	struct vnode *savedvp;

	savedvp = bp->b_vp;
	bp->b_vp = OTHERVP(bp->b_vp);

#ifdef DIAGNOSTIC
	if (bp->b_vp == 0)
		panic("union_strategy: nil vp");
	if (((bp->b_flags & B_READ) == 0) &&
	    (bp->b_vp == LOWERVP(savedvp)))
		panic("union_strategy: writing to lowervp");
#endif

	error = VOP_STRATEGY(bp);
	bp->b_vp = savedvp;

	return (error);
}


/*
 * XXX - like vop_strategy, vop_bwrite must be hand coded because it has no
 * vnode in its arguments.
 * This goes away with a merged VM/buffer cache.
 */
int
union_bwrite(ap)
	struct vop_bwrite_args /* {
		struct buf *a_bp;
	} */ *ap;
{
	struct buf *bp = ap->a_bp;
	int error;
	struct vnode *savedvp;

	savedvp = bp->b_vp;
	bp->b_vp = UPPERVP(bp->b_vp);

#ifdef DIAGNOSTIC
	if (bp->b_vp == 0)
		panic("union_bwrite: no upper vp");
#endif

	error = VOP_BWRITE(bp);

	bp->b_vp = savedvp;

	return (error);
}

int
union_lock(ap)
	struct vop_lock_args *ap;
{
	struct union_node *un = VTOUNION(ap->a_vp);

#ifdef DIAGNOSTIC
	if (un->un_pid == curproc->p_pid)
		panic("union: locking agsinst myself");
#endif
	while (un->un_flags & UN_LOCKED) {
		un->un_flags |= UN_WANT;
		sleep((caddr_t) &un->un_flags, PINOD);
	}
	un->un_flags |= UN_LOCKED;
#ifdef DIAGNOSTIC
	un->un_pid = curproc->p_pid;
#endif

	if (un->un_lowervp && !VOP_ISLOCKED(un->un_lowervp))
		VOP_LOCK(un->un_lowervp);
	if (un->un_uppervp && !VOP_ISLOCKED(un->un_uppervp))
		VOP_LOCK(un->un_uppervp);
}

int
union_unlock(ap)
	struct vop_lock_args *ap;
{
	struct union_node *un = VTOUNION(ap->a_vp);

#ifdef DIAGNOSTIC
	if (un->un_pid != curproc->p_pid)
		panic("union: unlocking other process's union node");
	if ((un->un_flags & UN_LOCKED) == 0)
		panic("union: unlock unlocked node");
#endif

	if (un->un_uppervp && VOP_ISLOCKED(un->un_uppervp))
		VOP_UNLOCK(un->un_uppervp);
	if (un->un_lowervp && VOP_ISLOCKED(un->un_lowervp))
		VOP_UNLOCK(un->un_lowervp);

	un->un_flags &= ~UN_LOCKED;
	if (un->un_flags & UN_WANT) {
		un->un_flags &= ~UN_WANT;
		wakeup((caddr_t) &un->un_flags);
	}

#ifdef DIAGNOSTIC
	un->un_pid = 0;
#endif
}

/*
 * Global vfs data structures
 */
int (**union_vnodeop_p)();
struct vnodeopv_entry_desc union_vnodeop_entries[] = {
	{ &vop_default_desc, union_bypass },

	{ &vop_getattr_desc, union_getattr },
	{ &vop_inactive_desc, union_inactive },
	{ &vop_reclaim_desc, union_reclaim },
	{ &vop_print_desc, union_print },

	{ &vop_strategy_desc, union_strategy },
	{ &vop_bwrite_desc, union_bwrite },

	{ &vop_lock_desc, union_lock }, 
	{ &vop_unlock_desc, union_unlock }, 

	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc union_vnodeop_opv_desc =
	{ &union_vnodeop_p, union_vnodeop_entries };
