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
 *	@(#)vfs_subr.c	7.8 (Berkeley) %G%
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

	while(mp->m_flag & M_MLOCK) {
		mp->m_flag |= M_MWAIT;
		sleep((caddr_t)mp, PVFS);
	}
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

/*
 * Routines having to do with the management of the vnode table.
 */
struct vnode *vfreeh, **vfreet;
extern struct vnodeops dead_vnodeops;

/*
 * Build vnode free list.
 */
vhinit()
{
	register struct vnode *vp = vnode;

	vfreeh = vp;
	vfreet = &vp->v_freef;
	vp->v_freeb = &vfreeh;
	vp->v_op = &dead_vnodeops;
	for (vp++; vp < vnodeNVNODE; vp++) {
		*vfreet = vp;
		vp->v_freeb = vfreet;
		vfreet = &vp->v_freef;
		vp->v_op = &dead_vnodeops;
	}
	vp--;
	vp->v_freef = NULL;
}

/*
 * Return the next vnode from the free list.
 */
getnewvnode(tag, mp, vops, vpp)
	enum vtagtype tag;
	struct mount *mp;
	struct vnodeops *vops;
	struct vnode **vpp;
{
	register struct vnode *vp, *vq;

	if ((vp = vfreeh) == NULL) {
		tablefull("vnode");
		*vpp = 0;
		return (ENFILE);
	}
	if (vp->v_count || VOP_RECLAIM(vp))
		panic("free vnode isn't");
	if (vq = vp->v_freef)
		vq->v_freeb = &vfreeh;
	vfreeh = vq;
	vp->v_freef = NULL;
	vp->v_freeb = NULL;
	vp->v_type = VNON;
	vp->v_flag = 0;
	vp->v_shlockc = 0;
	vp->v_exlockc = 0;
	vp->v_socket = 0;
	vp->v_op = vops;
	cache_purge(vp);
	vp->v_tag = tag;
	vp->v_mount = mp;
	insmntque(vp, mp);
	VREF(vp);
	*vpp = vp;
	return (0);
}

/*
 * Move a vnode from one mount queue to another.
 */
insmntque(vp, mp)
	register struct vnode *vp;
	register struct mount *mp;
{
	struct vnode *vq;

	/*
	 * Delete from old mount point vnode list, if on one.
	 */
	if (vp->v_mountb) {
		if (vq = vp->v_mountf)
			vq->v_mountb = vp->v_mountb;
		*vp->v_mountb = vq;
	}
	/*
	 * Insert into list of vnodes for the new mount point, if available.
	 */
	if (mp == NULL) {
		vp->v_mountf = NULL;
		vp->v_mountb = NULL;
		return;
	}
	if (mp->m_mounth) {
		vp->v_mountf = mp->m_mounth;
		vp->v_mountb = &mp->m_mounth;
		mp->m_mounth->v_mountb = &vp->v_mountf;
		mp->m_mounth = vp;
	} else {
		mp->m_mounth = vp;
		vp->v_mountb = &mp->m_mounth;
		vp->v_mountf = NULL;
	}
}

/*
 * Grab a particular vnode from the free list.
 */
vget(vp)
	register struct vnode *vp;
{
	register struct vnode *vq;

	if (vq = vp->v_freef)
		vq->v_freeb = vp->v_freeb;
	else
		vfreet = vp->v_freeb;
	*vp->v_freeb = vq;
	vp->v_freef = NULL;
	vp->v_freeb = NULL;
	VREF(vp);
}

/*
 * Vnode reference, just increment the count
 */
void vref(vp)
	struct vnode *vp;
{

	vp->v_count++;
}

/*
 * vput(), just unlock and vrele()
 */
void vput(vp)
	register struct vnode *vp;
{
	VOP_UNLOCK(vp);
	vrele(vp);
}

/*
 * Vnode release.
 * If count drops to zero, call inactive routine and return to freelist.
 */
void vrele(vp)
	register struct vnode *vp;
{

	if (vp == NULL)
		return;
	vp->v_count--;
	if (vp->v_count < 0)
		printf("vnode bad ref count %d, type %d, tag %d\n",
			vp->v_count, vp->v_type, vp->v_tag);
	if (vp->v_count > 0)
		return;
	VOP_INACTIVE(vp);
	if (vfreeh == (struct vnode *)0) {
		/*
		 * insert into empty list
		 */
		vfreeh = vp;
		vp->v_freeb = &vfreeh;
		vp->v_freef = NULL;
		vfreet = &vp->v_freef;
	} else if (vp->v_type == VNON) {
		/*
		 * insert at head of list
		 */
		vp->v_freef = vfreeh;
		vp->v_freeb = &vfreeh;
		vfreeh->v_freeb = &vp->v_freef;
		vfreeh = vp;
	} else {
		/*
		 * insert at tail of list
		 */
		*vfreet = vp;
		vp->v_freeb = vfreet;
		vp->v_freef = NULL;
		vfreet = &vp->v_freef;
	}
}
