/*
 * Copyright (c) 1992, 1993, 1994 The Regents of the University of California.
 * Copyright (c) 1992, 1993, 1994 Jan-Simon Pendry.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)union_vnops.c	1.3 (Berkeley) %G%
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

/*
 * Create a shadow directory in the upper layer.
 * The new vnode is returned locked.
 */
static int
union_mkshadow(dvp, cnp, vpp)
	struct vnode *dvp;
	struct componentname *cnp;
	struct vnode *vpp;
{
	int error;
	struct vattr va;
	struct proc *p = cnp->cn_proc;

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
	VOP_UNLOCK(dvp);
	LEASE_CHECK(dvp, p, p->p_ucred, LEASE_WRITE);
	VREF(dvp);
	VOP_LOCK(dvp);
	error = VOP_MKDIR(dvp, vpp, cnp, &va);
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

		vput(dvp);
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
	int error;
	int uerror, lerror;
	struct vnode *uppervp, *lowervp;
	struct vnode *upperdvp, *lowerdvp;
	struct vnode *dvp = ap->a_dvp;
	struct union_node *dun = VTOUNION(ap->a_dvp);
	struct componentname *cnp = ap->a_cnp;
	int lockparent = cnp->cn_flags & LOCKPARENT;

	cnp->cn_flags |= LOCKPARENT;

	upperdvp = dun->un_uppervp;
	lowerdvp = dun->un_lowervp;
	uppervp = 0;
	lowervp = 0;

	/*
	 * do the lookup in the upper level.
	 * if that level comsumes additional pathnames,
	 * then assume that something special is going
	 * on and just return that vnode.
	 */
	uppervp = 0;
	if (upperdvp) {
		VOP_LOCK(upperdvp);
		uerror = union_lookup1(upperdvp, &uppervp, cnp);
		VOP_UNLOCK(upperdvp);

		if (cnp->cn_consume != 0) {
			*ap->a_vpp = uppervp;
			if (!lockparent)
				cnp->cn_flags &= ~LOCKPARENT;
			return (uerror);
		}
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
		VOP_LOCK(lowerdvp);
		lerror = union_lookup1(lowerdvp, &lowervp, cnp);
		VOP_UNLOCK(lowerdvp);

		if (cnp->cn_consume != 0) {
			if (uppervp) {
				vput(uppervp);
				uppervp = 0;
			}
			*ap->a_vpp = lowervp;
			if (!lockparent)
				cnp->cn_flags &= ~LOCKPARENT;
			return (lerror);
		}
	} else {
		lerror = ENOENT;
	}

	if (!lockparent)
		cnp->cn_flags &= ~LOCKPARENT;

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
			VOP_LOCK(upperdvp);
			uerror = union_mkshadow(upperdvp, cnp, &uppervp);
			VOP_UNLOCK(upperdvp);
			if (uerror) {
				if (lowervp) {
					vput(lowervp);
					lowervp = 0;
				}
				return (uerror);
			}
		}
	}

	error = union_allocvp(ap->a_vpp, dvp->v_mount, dvp, cnp,
			      uppervp, lowervp);

	if (uppervp)
		VOP_UNLOCK(uppervp);
	if (lowervp)
		VOP_UNLOCK(lowervp);

	if (error) {
		if (uppervp)
			vrele(uppervp);
		if (lowervp)
			vrele(lowervp);
	} else {
		if (!lockparent)
			VOP_UNLOCK(*ap->a_vpp);
	}

	return (error);
}

int
union_create(ap)
	struct vop_create_args /* {
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
		struct vattr *a_vap;
	} */ *ap;
{
	struct union_node *un = VTOUNION(ap->a_dvp);
	struct vnode *dvp = un->un_uppervp;

	if (dvp) {
		int error;
		struct vnode *vp;
		struct mount *mp = ap->a_dvp->v_mount;

		VREF(dvp);
		VOP_LOCK(dvp);
		vput(ap->a_dvp);
		error = VOP_CREATE(dvp, &vp, ap->a_cnp, ap->a_vap);
		if (error)
			return (error);

		error = union_allocvp(
				ap->a_vpp,
				mp,
				NULLVP,
				ap->a_cnp,
				vp,
				NULLVP);
		VOP_UNLOCK(vp);
		if (error)
			vrele(vp);
		return (error);
	}

	vput(ap->a_dvp);
	return (EROFS);
}

int
union_mknod(ap)
	struct vop_mknod_args /* {
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
		struct vattr *a_vap;
	} */ *ap;
{
	struct union_node *un = VTOUNION(ap->a_dvp);
	struct vnode *dvp = un->un_uppervp;

	if (dvp) {
		int error;
		struct vnode *vp;
		struct mount *mp = ap->a_dvp->v_mount;

		VREF(dvp);
		VOP_LOCK(dvp);
		vput(ap->a_dvp);
		error = VOP_MKNOD(dvp, &vp, ap->a_cnp, ap->a_vap);
		if (error)
			return (error);

		if (vp) {
			error = union_allocvp(
					ap->a_vpp,
					mp,
					NULLVP,
					ap->a_cnp,
					vp,
					NULLVP);
			VOP_UNLOCK(vp);
			if (error)
				vrele(vp);
		}
		return (error);
	}

	vput(ap->a_dvp);
	return (EROFS);
}

/*
 * copyfile.  copy the vnode (fvp) to the vnode (tvp)
 * using a sequence of reads and writes.  both (fvp)
 * and (tvp) are locked on entry and exit.
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
	struct vnode *tvp;
	int mode = ap->a_mode;
	struct ucred *cred = ap->a_cred;
	struct proc *p = ap->a_p;
	int error;

	/*
	 * If there is an existing upper vp then simply open that.
	 */
	tvp = un->un_uppervp;
	if (tvp == NULLVP) {
		/*
		 * If the lower vnode is being opened for writing, then
		 * copy the file contents to the upper vnode and open that,
		 * otherwise can simply open the lower vnode.
		 */
		tvp = un->un_lowervp;
		if ((ap->a_mode & FWRITE) && (tvp->v_type == VREG)) {
			struct nameidata nd;
			struct filedesc *fdp = p->p_fd;
			int fmode;
			int cmode;

			/*
			 * Open the named file in the upper layer.  Note that
			 * the file may have come into existence *since* the
			 * lookup was done, since the upper layer may really
			 * be a loopback mount of some other filesystem...
			 * so open the file with exclusive create and barf if
			 * it already exists.
			 * XXX - perhaps shoudl re-lookup the node (once more
			 * with feeling) and simply open that.  Who knows.
			 */
			NDINIT(&nd, CREATE, 0, UIO_SYSSPACE, un->un_path, p);
			fmode = (O_CREAT|O_TRUNC|O_EXCL);
			cmode = UN_FILEMODE & ~fdp->fd_cmask;
			error = vn_open(&nd, fmode, cmode);
			if (error)
				return (error);
			un->un_uppervp = nd.ni_vp;	/* XXX */
			/* at this point, uppervp is locked */

			/*
			 * Now, if the file is being opened with truncation,
			 * then the (new) upper vnode is ready to fly,
			 * otherwise the data from the lower vnode must be
			 * copied to the upper layer first.  This only works
			 * for regular files (check is made above).
			 */
			if ((mode & O_TRUNC) == 0) {
				/*
				 * XXX - should not ignore errors
				 * from VOP_CLOSE
				 */
				VOP_LOCK(un->un_lowervp);
				error = VOP_OPEN(tvp, FREAD, cred, p);
				if (error == 0) {
					error = union_copyfile(p, cred,
						       tvp, un->un_uppervp);
					VOP_UNLOCK(tvp);
					(void) VOP_CLOSE(tvp, FREAD);
				} else {
					VOP_UNLOCK(tvp);
				}
				VOP_UNLOCK(un->un_uppervp);
				(void) VOP_CLOSE(un->un_uppervp, FWRITE);
				VOP_LOCK(un->un_uppervp);
			}
			if (error == 0)
				error = VOP_OPEN(un->un_uppervp, mode, cred, p);
			VOP_UNLOCK(un->un_uppervp);
			return (error);
		}
	}

	VOP_LOCK(tvp);
	error = VOP_OPEN(tvp, mode, cred, p);
	VOP_UNLOCK(tvp);

	return (error);
}

int
union_close(ap)
	struct vop_close_args /* {
		struct vnode *a_vp;
		int  a_fflag;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_CLOSE(OTHERVP(ap->a_vp), ap->a_fflag, ap->a_cred, ap->a_p));
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
	int error = 0;
	struct vnode *vp;

	if (vp = un->un_lowervp) {
		VOP_LOCK(vp);
		error = VOP_ACCESS(vp, ap->a_mode, ap->a_cred, ap->a_p);
		VOP_UNLOCK(vp);
		if (error)
			return (error);
	}

	if (vp = un->un_uppervp) {
		VOP_LOCK(vp);
		error = VOP_ACCESS(vp, ap->a_mode, ap->a_cred, ap->a_p);
		VOP_UNLOCK(vp);
	}

	return (error);
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
	struct vnode *vp = OTHERVP(ap->a_vp);

	VOP_LOCK(vp);
	error = VOP_GETATTR(vp, ap->a_vap, ap->a_cred, ap->a_p);
	VOP_UNLOCK(vp);

	/* Requires that arguments be restored. */
	ap->a_vap->va_fsid = ap->a_vp->v_mount->mnt_stat.f_fsid.val[0];
	return (0);
}

int
union_setattr(ap)
	struct vop_setattr_args /* {
		struct vnode *a_vp;
		struct vattr *a_vap;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{
	struct union_node *un = VTOUNION(ap->a_vp);
	int error;

	if (un->un_uppervp) {
		VOP_LOCK(un->un_uppervp);
		error = VOP_SETATTR(un->un_uppervp, ap->a_vap,
					ap->a_cred, ap->a_p);
		VOP_UNLOCK(un->un_uppervp);
	} else {
		/*
		 * XXX should do a copyfile (perhaps only if
		 * the file permission change, which would not
		 * track va_ctime correctly).
		 */
		error = EROFS;
	}

	return (error);
}

int
union_read(ap)
	struct vop_read_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		int  a_ioflag;
		struct ucred *a_cred;
	} */ *ap;
{
	int error;
	struct vnode *vp = OTHERVP(ap->a_vp);

	VOP_LOCK(vp);
	error = VOP_READ(vp, ap->a_uio, ap->a_ioflag, ap->a_cred);
	VOP_UNLOCK(vp);

	return (error);
}

int
union_write(ap)
	struct vop_read_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		int  a_ioflag;
		struct ucred *a_cred;
	} */ *ap;
{
	int error;
	struct vnode *vp = OTHERVP(ap->a_vp);

	VOP_LOCK(vp);
	error = VOP_WRITE(vp, ap->a_uio, ap->a_ioflag, ap->a_cred);
	VOP_UNLOCK(vp);

	return (error);
}

int
union_ioctl(ap)
	struct vop_ioctl_args /* {
		struct vnode *a_vp;
		int  a_command;
		caddr_t  a_data;
		int  a_fflag;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_IOCTL(OTHERVP(ap->a_vp), ap->a_command, ap->a_data,
				ap->a_fflag, ap->a_cred, ap->a_p));
}

int
union_select(ap)
	struct vop_select_args /* {
		struct vnode *a_vp;
		int  a_which;
		int  a_fflags;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_SELECT(OTHERVP(ap->a_vp), ap->a_which, ap->a_fflags,
				ap->a_cred, ap->a_p));
}

int
union_mmap(ap)
	struct vop_mmap_args /* {
		struct vnode *a_vp;
		int  a_fflags;
		struct ucred *a_cred;
		struct proc *a_p;
	} */ *ap;
{

	return (VOP_MMAP(OTHERVP(ap->a_vp), ap->a_fflags,
				ap->a_cred, ap->a_p));
}

int
union_fsync(ap)
	struct vop_fsync_args /* {
		struct vnode *a_vp;
		struct ucred *a_cred;
		int  a_waitfor;
		struct proc *a_p;
	} */ *ap;
{
	int error = 0;
	struct vnode *targetvp = OTHERVP(ap->a_vp);

	if (targetvp) {
		VOP_LOCK(targetvp);
		error = VOP_FSYNC(targetvp, ap->a_cred,
					ap->a_waitfor, ap->a_p);
		VOP_UNLOCK(targetvp);
	}

	return (error);
}

int
union_seek(ap)
	struct vop_seek_args /* {
		struct vnode *a_vp;
		off_t  a_oldoff;
		off_t  a_newoff;
		struct ucred *a_cred;
	} */ *ap;
{

	return (VOP_SEEK(OTHERVP(ap->a_vp), ap->a_oldoff, ap->a_newoff, ap->a_cred));
}

int
union_remove(ap)
	struct vop_remove_args /* {
		struct vnode *a_dvp;
		struct vnode *a_vp;
		struct componentname *a_cnp;
	} */ *ap;
{
	int error;
	struct union_node *dun = VTOUNION(ap->a_dvp);
	struct union_node *un = VTOUNION(ap->a_vp);

	if (dun->un_uppervp && un->un_uppervp) {
		struct vnode *dvp = dun->un_uppervp;
		struct vnode *vp = un->un_uppervp;

		VREF(dvp);
		VOP_LOCK(dvp);
		vput(ap->a_dvp);
		VREF(vp);
		VOP_LOCK(vp);
		vput(ap->a_vp);

		error = VOP_REMOVE(dvp, vp, ap->a_cnp);
	} else {
		/*
		 * XXX: should create a whiteout here
		 */
		vput(ap->a_dvp);
		vput(ap->a_vp);
		error = EROFS;
	}

	return (error);
}

int
union_link(ap)
	struct vop_link_args /* {
		struct vnode *a_vp;
		struct vnode *a_tdvp;
		struct componentname *a_cnp;
	} */ *ap;
{
	int error;
	struct union_node *dun = VTOUNION(ap->a_vp);
	struct union_node *un = VTOUNION(ap->a_tdvp);

	if (dun->un_uppervp && un->un_uppervp) {
		struct vnode *dvp = dun->un_uppervp;
		struct vnode *vp = un->un_uppervp;

		VREF(dvp);
		VOP_LOCK(dvp);
		vput(ap->a_vp);
		VREF(vp);
		vrele(ap->a_tdvp);

		error = VOP_LINK(dvp, vp, ap->a_cnp);
	} else {
		/*
		 * XXX: need to copy to upper layer
		 * and do the link there.
		 */
		vput(ap->a_vp);
		vrele(ap->a_tdvp);
		error = EROFS;
	}

	return (error);
}

int
union_rename(ap)
	struct vop_rename_args  /* {
		struct vnode *a_fdvp;
		struct vnode *a_fvp;
		struct componentname *a_fcnp;
		struct vnode *a_tdvp;
		struct vnode *a_tvp;
		struct componentname *a_tcnp;
	} */ *ap;
{
	int error;

	struct vnode *fdvp = ap->a_fdvp;
	struct vnode *fvp = ap->a_fvp;
	struct vnode *tdvp = ap->a_tdvp;
	struct vnode *tvp = ap->a_tvp;

	if (fdvp->v_op == union_vnodeop_p) {	/* always true */
		struct union_node *un = VTOUNION(fdvp);
		if (un->un_uppervp == 0) {
			error = EROFS;
			goto bad;
		}

		fdvp = un->un_uppervp;
		VREF(fdvp);
		vrele(ap->a_fdvp);
	}

	if (fvp->v_op == union_vnodeop_p) {	/* always true */
		struct union_node *un = VTOUNION(fvp);
		if (un->un_uppervp == 0) {
			error = EROFS;
			goto bad;
		}

		fvp = un->un_uppervp;
		VREF(fvp);
		vrele(ap->a_fvp);
	}

	if (tdvp->v_op == union_vnodeop_p) {
		struct union_node *un = VTOUNION(tdvp);
		if (un->un_uppervp == 0) {
			error = EROFS;
			goto bad;
		}

		tdvp = un->un_uppervp;
		VREF(tdvp);
		VOP_LOCK(tdvp);
		vput(ap->a_fdvp);
	}

	if (tvp && tvp->v_op == union_vnodeop_p) {
		struct union_node *un = VTOUNION(tvp);
		if (un->un_uppervp == 0) {
			error = EROFS;
			goto bad;
		}

		tvp = un->un_uppervp;
		VREF(tvp);
		VOP_LOCK(tvp);
		vput(ap->a_tvp);
	}

	return (VOP_RENAME(fdvp, fvp, ap->a_fcnp, tdvp, tvp, ap->a_tcnp));

bad:
	vrele(fdvp);
	vrele(fvp);
	vput(tdvp);
	if (tvp)
		vput(tvp);

	return (error);
}

int
union_mkdir(ap)
	struct vop_mkdir_args /* {
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
		struct vattr *a_vap;
	} */ *ap;
{
	struct union_node *un = VTOUNION(ap->a_dvp);
	struct vnode *dvp = un->un_uppervp;

	if (dvp) {
		int error;
		struct vnode *vp;
		struct mount *mp = ap->a_dvp->v_mount;

		VREF(dvp);
		VOP_LOCK(dvp);
		vput(ap->a_dvp);
		error = VOP_MKDIR(dvp, &vp, ap->a_cnp, ap->a_vap);
		if (error)
			return (error);

		error = union_allocvp(
				ap->a_vpp,
				mp,
				NULLVP,
				ap->a_cnp,
				vp,
				NULLVP);
		VOP_UNLOCK(vp);
		if (error)
			vrele(vp);
		return (error);
	}

	vput(ap->a_dvp);
	return (EROFS);
}

int
union_rmdir(ap)
	struct vop_rmdir_args /* {
		struct vnode *a_dvp;
		struct vnode *a_vp;
		struct componentname *a_cnp;
	} */ *ap;
{
	int error;
	struct union_node *dun = VTOUNION(ap->a_dvp);
	struct union_node *un = VTOUNION(ap->a_vp);

	if (dun->un_uppervp && un->un_uppervp) {
		struct vnode *dvp = dun->un_uppervp;
		struct vnode *vp = un->un_uppervp;

		VREF(dvp);
		VOP_LOCK(dvp);
		vput(ap->a_dvp);
		VREF(vp);
		VOP_LOCK(vp);
		vput(ap->a_vp);

		error = VOP_REMOVE(dvp, vp, ap->a_cnp);
	} else {
		/*
		 * XXX: should create a whiteout here
		 */
		vput(ap->a_dvp);
		vput(ap->a_vp);
		error = EROFS;
	}

	return (error);
}

int
union_symlink(ap)
	struct vop_symlink_args /* {
		struct vnode *a_dvp;
		struct vnode **a_vpp;
		struct componentname *a_cnp;
		struct vattr *a_vap;
		char *a_target;
	} */ *ap;
{
	struct union_node *un = VTOUNION(ap->a_dvp);
	struct vnode *dvp = un->un_uppervp;

	if (dvp) {
		int error;
		struct vnode *vp;
		struct mount *mp = ap->a_dvp->v_mount;

		VREF(dvp);
		VOP_LOCK(dvp);
		vput(ap->a_dvp);
		error = VOP_SYMLINK(dvp, &vp, ap->a_cnp,
					ap->a_vap, ap->a_target);
		*ap->a_vpp = 0;
		return (error);
	}

	vput(ap->a_dvp);
	return (EROFS);
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
	int error = 0;
	struct union_node *un = VTOUNION(ap->a_vp);

	if (un->un_uppervp) {
		struct vnode *vp = OTHERVP(ap->a_vp);

		VOP_LOCK(vp);
		error = VOP_READLINK(vp, ap->a_uio, ap->a_cred);
		VOP_UNLOCK(vp);
	}

	return (error);
}

int
union_readlink(ap)
	struct vop_readlink_args /* {
		struct vnode *a_vp;
		struct uio *a_uio;
		struct ucred *a_cred;
	} */ *ap;
{
	int error;
	struct vnode *vp = OTHERVP(ap->a_vp);

	VOP_LOCK(vp);
	error = VOP_READLINK(vp, ap->a_uio, ap->a_cred);
	VOP_UNLOCK(vp);

	return (error);
}

int
union_abortop(ap)
	struct vop_abortop_args /* {
		struct vnode *a_dvp;
		struct componentname *a_cnp;
	} */ *ap;
{
	int error;
	struct vnode *vp = OTHERVP(ap->a_dvp);
	struct union_node *un = VTOUNION(ap->a_dvp);
	int islocked = un->un_flags & UN_LOCKED;

	if (islocked)
		VOP_LOCK(vp);
	error = VOP_ABORTOP(vp, ap->a_cnp);
	if (islocked)
		VOP_UNLOCK(vp);

	return (error);
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
union_lock(ap)
	struct vop_lock_args *ap;
{
	struct union_node *un = VTOUNION(ap->a_vp);

	while (un->un_flags & UN_LOCKED) {
#ifdef DIAGNOSTIC
		if (un->un_pid == curproc->p_pid)
			panic("union: locking agsinst myself");
#endif
		un->un_flags |= UN_WANT;
		sleep((caddr_t) &un->un_flags, PINOD);
	}
	un->un_flags |= UN_LOCKED;
#ifdef DIAGNOSTIC
	un->un_pid = curproc->p_pid;
#endif
}

int
union_unlock(ap)
	struct vop_lock_args *ap;
{
	struct union_node *un = VTOUNION(ap->a_vp);

#ifdef DIAGNOSTIC
	if ((un->un_flags & UN_LOCKED) == 0)
		panic("union: unlock unlocked node");
	if (un->un_pid != curproc->p_pid)
		panic("union: unlocking other process's union node");
#endif

	un->un_flags &= ~UN_LOCKED;
	if (un->un_flags & UN_WANT) {
		un->un_flags &= ~UN_WANT;
		wakeup((caddr_t) &un->un_flags);
	}

#ifdef DIAGNOSTIC
	un->un_pid = 0;
#endif
}

int
union_bmap(ap)
	struct vop_bmap_args /* {
		struct vnode *a_vp;
		daddr_t  a_bn;
		struct vnode **a_vpp;
		daddr_t *a_bnp;
		int *a_runp;
	} */ *ap;
{
	int error;
	struct vnode *vp = OTHERVP(ap->a_vp);

	VOP_LOCK(vp);
	error = VOP_BMAP(vp, ap->a_bn, ap->a_vpp, ap->a_bnp, ap->a_runp);
	VOP_UNLOCK(vp);

	return (error);
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

int
union_islocked(ap)
	struct vop_islocked_args /* {
		struct vnode *a_vp;
	} */ *ap;
{

	return ((VTOUNION(ap->a_vp)->un_flags & UN_LOCKED) ? 1 : 0);
}

int
union_pathconf(ap)
	struct vop_pathconf_args /* {
		struct vnode *a_vp;
		int a_name;
		int *a_retval;
	} */ *ap;
{
	int error;
	struct vnode *vp = OTHERVP(ap->a_vp);

	VOP_LOCK(vp);
	error = VOP_PATHCONF(vp, ap->a_name, ap->a_retval);
	VOP_UNLOCK(vp);

	return (error);
}

int
union_advlock(ap)
	struct vop_advlock_args /* {
		struct vnode *a_vp;
		caddr_t  a_id;
		int  a_op;
		struct flock *a_fl;
		int  a_flags;
	} */ *ap;
{

	return (VOP_ADVLOCK(OTHERVP(ap->a_vp), ap->a_id, ap->a_op,
				ap->a_fl, ap->a_flags));
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
 * Global vfs data structures
 */
int (**union_vnodeop_p)();
struct vnodeopv_entry_desc union_vnodeop_entries[] = {
	{ &vop_default_desc, vn_default_error },
	{ &vop_lookup_desc, union_lookup },		/* lookup */
	{ &vop_create_desc, union_create },		/* create */
	{ &vop_mknod_desc, union_mknod },		/* mknod */
	{ &vop_open_desc, union_open },			/* open */
	{ &vop_close_desc, union_close },		/* close */
	{ &vop_access_desc, union_access },		/* access */
	{ &vop_getattr_desc, union_getattr },		/* getattr */
	{ &vop_setattr_desc, union_setattr },		/* setattr */
	{ &vop_read_desc, union_read },			/* read */
	{ &vop_write_desc, union_write },		/* write */
	{ &vop_ioctl_desc, union_ioctl },		/* ioctl */
	{ &vop_select_desc, union_select },		/* select */
	{ &vop_mmap_desc, union_mmap },			/* mmap */
	{ &vop_fsync_desc, union_fsync },		/* fsync */
	{ &vop_seek_desc, union_seek },			/* seek */
	{ &vop_remove_desc, union_remove },		/* remove */
	{ &vop_link_desc, union_link },			/* link */
	{ &vop_rename_desc, union_rename },		/* rename */
	{ &vop_mkdir_desc, union_mkdir },		/* mkdir */
	{ &vop_rmdir_desc, union_rmdir },		/* rmdir */
	{ &vop_symlink_desc, union_symlink },		/* symlink */
	{ &vop_readdir_desc, union_readdir },		/* readdir */
	{ &vop_readlink_desc, union_readlink },		/* readlink */
	{ &vop_abortop_desc, union_abortop },		/* abortop */
	{ &vop_inactive_desc, union_inactive },		/* inactive */
	{ &vop_reclaim_desc, union_reclaim },		/* reclaim */
	{ &vop_lock_desc, union_lock },			/* lock */
	{ &vop_unlock_desc, union_unlock },		/* unlock */
	{ &vop_bmap_desc, union_bmap },			/* bmap */
	{ &vop_strategy_desc, union_strategy },		/* strategy */
	{ &vop_print_desc, union_print },		/* print */
	{ &vop_islocked_desc, union_islocked },		/* islocked */
	{ &vop_pathconf_desc, union_pathconf },		/* pathconf */
	{ &vop_advlock_desc, union_advlock },		/* advlock */
#ifdef notdef
	{ &vop_blkatoff_desc, union_blkatoff },		/* blkatoff */
	{ &vop_valloc_desc, union_valloc },		/* valloc */
	{ &vop_vfree_desc, union_vfree },		/* vfree */
	{ &vop_truncate_desc, union_truncate },		/* truncate */
	{ &vop_update_desc, union_update },		/* update */
	{ &vop_bwrite_desc, union_bwrite },		/* bwrite */
#endif
	{ (struct vnodeop_desc*)NULL, (int(*)())NULL }
};
struct vnodeopv_desc union_vnodeop_opv_desc =
	{ &union_vnodeop_p, union_vnodeop_entries };
