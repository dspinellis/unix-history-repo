/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vfs_subr.c	7.5 (Berkeley) %G%
 */

/*
 * External virtual filesystem routines
 */

#include "param.h"
#include "mount.h"
#include "time.h"
#include "vnode.h"
#include "namei.h"
#include "ucred.h"
#include "errno.h"

/*
 * Add a new mount point to the list of mounted filesystems.
 * Lock the filesystem so that namei will not cross into the
 * the tree below the covered vnode.
 */
vfs_add(mountedvp, mp, flags)
	register struct vnode *mountedvp;
	register struct mount *mp;
	int flags;
{
	register int error;

	error = vfs_lock(mp);
	if (error)
		return (error);
	if (mountedvp == (struct vnode *)0) {
		/*
		 * We are mounting the root filesystem.
		 */
		rootfs = mp;
		mp->m_next = mp;
		mp->m_prev = mp;
	} else {
		if (mountedvp->v_mountedhere != (struct mount *)0) {
			vfs_unlock(mp);
			return(EBUSY);
		}
		/*
		 * Put the new filesystem on the mount list after root.
		 */
		mp->m_next = rootfs->m_next;
		mp->m_prev = rootfs;
		rootfs->m_next = mp;
		mp->m_next->m_prev = mp;
		mountedvp->v_mountedhere = mp;
	}
	mp->m_vnodecovered = mountedvp;
	if (flags & M_RDONLY) {
		mp->m_flag |= M_RDONLY;
	} else {
		mp->m_flag &= ~M_RDONLY;
	}
	if (flags & M_NOSUID) {
		mp->m_flag |= M_NOSUID;
	} else {
		mp->m_flag &= ~M_NOSUID;
	}
	return (0);
}

/*
 * Remove a mount point from the list of mounted filesystems.
 * Unmount of the root is illegal.
 */
void
vfs_remove(mp)
	register struct mount *mp;
{

	if (mp == rootfs)
		panic("vfs_remove: unmounting root");
	mp->m_prev->m_next = mp->m_next;
	mp->m_next->m_prev = mp->m_prev;
	mp->m_vnodecovered->v_mountedhere = (struct mount *)0;
	vfs_unlock(mp);
}

/*
 * Lock a filesystem.
 * Used to prevent access to it while mounting and unmounting.
 */
vfs_lock(mp)
	register struct mount *mp;
{

	if (mp->m_flag & M_MLOCK)
		return (EBUSY);
	mp->m_flag |= M_MLOCK;
	return (0);
}

/*
 * Unlock a locked filesystem.
 * Panic if filesystem is not locked.
 */
void
vfs_unlock(mp)
	register struct mount *mp;
{

	if ((mp->m_flag & M_MLOCK) == 0)
		panic("vfs_unlock: locked fs");
	mp->m_flag &= ~M_MLOCK;
	if (mp->m_flag & M_MWAIT) {
		mp->m_flag &= ~M_MWAIT;
		wakeup((caddr_t)mp);
	}
}

/*
 * Lookup a mount point by filesystem identifier.
 */
struct mount *
getvfs(fsid)
	fsid_t *fsid;
{
	register struct mount *mp;

	mp = rootfs;
	do {
		if (mp->m_fsid.val[0] == fsid->val[0] &&
		    mp->m_fsid.val[1] == fsid->val[1]) {
			return (mp);
		}
		mp = mp->m_next;
	} while (mp != rootfs);
	return ((struct mount *)0);
}

/*
 * Set vnode attributes to VNOVAL
 */
void vattr_null(vap)
	register struct vattr *vap;
{

	vap->va_type = VNON;
	vap->va_mode = vap->va_nlink = vap->va_uid = vap->va_gid =
		vap->va_fsid = vap->va_fileid = vap->va_size =
		vap->va_size1 = vap->va_blocksize = vap->va_rdev =
		vap->va_bytes = vap->va_bytes1 =
		vap->va_atime.tv_sec = vap->va_atime.tv_usec =
		vap->va_mtime.tv_sec = vap->va_mtime.tv_usec =
		vap->va_ctime.tv_sec = vap->va_ctime.tv_usec =
		vap->va_flags = vap->va_gen = VNOVAL;
}

/*
 * Initialize a nameidata structure
 */
ndinit(ndp)
	register struct nameidata *ndp;
{

	bzero((caddr_t)ndp, sizeof(struct nameidata));
	ndp->ni_iov = &ndp->ni_nd.nd_iovec;
	ndp->ni_iovcnt = 1;
	ndp->ni_base = (caddr_t)&ndp->ni_dent;
	ndp->ni_rw = UIO_WRITE;
	ndp->ni_segflg = UIO_SYSSPACE;
}

/*
 * Duplicate a nameidata structure
 */
nddup(ndp, newndp)
	register struct nameidata *ndp, *newndp;
{

	ndinit(newndp);
	newndp->ni_cdir = ndp->ni_cdir;
	VREF(newndp->ni_cdir);
	newndp->ni_rdir = ndp->ni_rdir;
	if (newndp->ni_rdir)
		VREF(newndp->ni_rdir);
	newndp->ni_cred = ndp->ni_cred;
	crhold(newndp->ni_cred);
}

/*
 * Release a nameidata structure
 */
ndrele(ndp)
	register struct nameidata *ndp;
{

	vrele(ndp->ni_cdir);
	if (ndp->ni_rdir)
		vrele(ndp->ni_rdir);
	crfree(ndp->ni_cred);
}
