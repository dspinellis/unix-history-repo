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
 *	@(#)vfs_subr.c	7.12 (Berkeley) %G%
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
#include "malloc.h"

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
extern struct vnodeops dead_vnodeops, spec_vnodeops;
struct speclist *speclisth;
struct speclist {
	struct speclist *sl_next;
	struct vnode *sl_vp;
};

/*
 * Initialize the vnode structures and initialize each file system type.
 */
vfsinit()
{
	register struct vnode *vp = vnode;
	struct vfsops **vfsp;

	/*
	 * Build vnode free list.
	 */
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
	/*
	 * Initialize the vnode name cache
	 */
	nchinit();
	/*
	 * Initialize each file system type.
	 */
	for (vfsp = &vfssw[0]; vfsp <= &vfssw[MOUNT_MAXTYPE]; vfsp++) {
		if (*vfsp == NULL)
			continue;
		(*(*vfsp)->vfs_init)();
	}
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
	if (vp->v_count)
		panic("free vnode isn't");
	if (vq = vp->v_freef)
		vq->v_freeb = &vfreeh;
	vfreeh = vq;
	vp->v_freef = NULL;
	vp->v_freeb = NULL;
	if (vp->v_type != VNON)
		vgone(vp);
	vp->v_flag = 0;
	vp->v_shlockc = 0;
	vp->v_exlockc = 0;
	vp->v_socket = 0;
	cache_purge(vp);
	vp->v_tag = tag;
	vp->v_op = vops;
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
 * Create a vnode for a block device.
 * Used for root filesystem, argdev, and swap areas.
 * Also used for memory file system special devices.
 */
bdevvp(dev, vpp)
	dev_t dev;
	struct vnode **vpp;
{
	register struct vnode *vp;
	struct vnode *nvp;
	int error;

	error = getnewvnode(VT_NON, (struct mount *)0, &spec_vnodeops, &nvp);
	if (error) {
		*vpp = 0;
		return (error);
	}
	vp = nvp;
	vp->v_type = VBLK;
	vp->v_rdev = dev;
	if (nvp = checkalias(vp, (struct mount *)0)) {
		vput(vp);
		vp = nvp;
	}
	*vpp = vp;
	return (0);
}

/*
 * Check to see if the new vnode represents a special device
 * for which we already have a vnode (either because of
 * bdevvp() or because of a different vnode representing
 * the same block device). If such an alias exists, deallocate
 * the existing contents and return the aliased inode. The
 * caller is responsible for filling it with its new contents.
 */
struct vnode *
checkalias(nvp, mp)
	register struct vnode *nvp;
	struct mount *mp;
{
	register struct vnode *vp;
	register struct speclist *slp;

	if (nvp->v_type != VBLK && nvp->v_type != VCHR)
		return ((struct vnode *)0);
loop:
	for (slp = speclisth; slp; slp = slp->sl_next) {
		vp = slp->sl_vp;
		if (nvp->v_rdev != vp->v_rdev ||
		    nvp->v_type != vp->v_type)
			continue;
		if (vget(vp))
			goto loop;
		break;
	}
	if (slp == NULL) {
		MALLOC(slp, struct speclist *, sizeof(*slp), M_VNODE, M_WAITOK);
		slp->sl_vp = nvp;
		slp->sl_next = speclisth;
		speclisth = slp;
		return ((struct vnode *)0);
	}
	VOP_UNLOCK(vp);
	vclean(vp, 0);
	vp->v_op = nvp->v_op;
	vp->v_tag = nvp->v_tag;
	nvp->v_type = VNON;
	insmntque(vp, mp);
	return (vp);
}

/*
 * Grab a particular vnode from the free list, increment its
 * reference count and lock it. The vnode lock bit is set the
 * vnode is being eliminated in vgone. The process is awakened
 * when the transition is completed, and an error returned to
 * indicate that the vnode is no longer usable (possibly having
 * been changed to a new file system type).
 */
vget(vp)
	register struct vnode *vp;
{
	register struct vnode *vq;

	if (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		return (1);
	}
	if (vp->v_count == 0) {
		if (vq = vp->v_freef)
			vq->v_freeb = vp->v_freeb;
		else
			vfreet = vp->v_freeb;
		*vp->v_freeb = vq;
		vp->v_freef = NULL;
		vp->v_freeb = NULL;
	}
	VREF(vp);
	VOP_LOCK(vp);
	return (0);
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
		panic("vrele: null vp");
	vp->v_count--;
	if (vp->v_count < 0)
		printf("vnode bad ref count %d, type %d, tag %d\n",
			vp->v_count, vp->v_type, vp->v_tag);
	if (vp->v_count > 0)
		return;
	if (vfreeh == (struct vnode *)0) {
		/*
		 * insert into empty list
		 */
		vfreeh = vp;
		vp->v_freeb = &vfreeh;
	} else {
		/*
		 * insert at tail of list
		 */
		*vfreet = vp;
		vp->v_freeb = vfreet;
	}
	vp->v_freef = NULL;
	vfreet = &vp->v_freef;
	VOP_INACTIVE(vp);
}

/*
 * Disassociate the underlying file system from a vnode.
 */
void vclean(vp, doclose)
	register struct vnode *vp;
	long doclose;
{
	struct vnodeops *origops;
	int active;

	/*
	 * Check to see if the vnode is in use.
	 * If so we have to lock it before we clean it out.
	 */
	if (active = vp->v_count) {
		VREF(vp);
		VOP_LOCK(vp);
	}
	/*
	 * Prevent the vnode from being recycled or
	 * brought into use while we clean it out.
	 */
	while (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
	}
	vp->v_flag |= VXLOCK;
	/*
	 * Prevent any further operations on the vnode from
	 * being passed through to the old file system.
	 */
	origops = vp->v_op;
	vp->v_op = &dead_vnodeops;
	vp->v_tag = VT_NON;
	/*
	 * If purging an active vnode, it must be unlocked, closed,
	 * and deactivated before being reclaimed.
	 */
	if (active) {
		(*(origops->vn_unlock))(vp);
		if (doclose)
			(*(origops->vn_close))(vp, 0, NOCRED);
		(*(origops->vn_inactive))(vp);
	}
	/*
	 * Reclaim the vnode.
	 */
	if ((*(origops->vn_reclaim))(vp))
		panic("vclean: cannot reclaim");
	if (active)
		vrele(vp);
	/*
	 * Done with purge, notify sleepers in vget of the grim news.
	 */
	vp->v_flag &= ~VXLOCK;
	if (vp->v_flag & VXWANT) {
		vp->v_flag &= ~VXWANT;
		wakeup((caddr_t)vp);
	}
}

/*
 * Eliminate all activity associated with a vnode
 * in preparation for reuse.
 */
void vgone(vp)
	register struct vnode *vp;
{
	register struct speclist *slp;
	struct speclist *pslp;
	register struct vnode *vq;

	/*
	 * Clean out the filesystem specific data.
	 */
	vclean(vp, 1);
	/*
	 * Delete from old mount point vnode list, if on one.
	 */
	if (vp->v_mountb) {
		if (vq = vp->v_mountf)
			vq->v_mountb = vp->v_mountb;
		*vp->v_mountb = vq;
		vp->v_mountf = NULL;
		vp->v_mountb = NULL;
	}
	/*
	 * If special device, remove it from special device alias list.
	 */
	if (vp->v_type == VBLK || vp->v_type == VCHR) {
		if (speclisth->sl_vp == vp) {
			slp = speclisth;
			speclisth = slp->sl_next;
		} else {
			for (pslp = speclisth, slp = pslp->sl_next; slp;
			     pslp = slp, slp = slp->sl_next) {
				if (slp->sl_vp != vp)
					continue;
				pslp->sl_next = slp->sl_next;
				break;
			}
			if (slp == NULL)
				panic("missing bdev");
		}
		FREE(slp, M_VNODE);
	}
	/*
	 * If it is on the freelist, move it to the head of the list.
	 */
	if (vp->v_freeb) {
		if (vq = vp->v_freef)
			vq->v_freeb = vp->v_freeb;
		else
			vfreet = vp->v_freeb;
		*vp->v_freeb = vq;
		vp->v_freef = vfreeh;
		vp->v_freeb = &vfreeh;
		vfreeh->v_freeb = &vp->v_freef;
		vfreeh = vp;
	}
	vp->v_type = VBAD;
}
