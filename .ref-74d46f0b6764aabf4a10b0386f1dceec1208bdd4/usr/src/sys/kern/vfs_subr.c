/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_subr.c	7.52 (Berkeley) %G%
 */

/*
 * External virtual filesystem routines
 */

#include "param.h"
#include "proc.h"
#include "mount.h"
#include "time.h"
#include "vnode.h"
#include "specdev.h"
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
	mp->mnt_prev->mnt_next = mp->mnt_next;
	mp->mnt_next->mnt_prev = mp->mnt_prev;
	mp->mnt_vnodecovered->v_mountedhere = (struct mount *)0;
	vfs_unlock(mp);
}

/*
 * Lock a filesystem.
 * Used to prevent access to it while mounting and unmounting.
 */
vfs_lock(mp)
	register struct mount *mp;
{

	while(mp->mnt_flag & MNT_MLOCK) {
		mp->mnt_flag |= MNT_MWAIT;
		sleep((caddr_t)mp, PVFS);
	}
	mp->mnt_flag |= MNT_MLOCK;
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

	if ((mp->mnt_flag & MNT_MLOCK) == 0)
		panic("vfs_unlock: not locked");
	mp->mnt_flag &= ~MNT_MLOCK;
	if (mp->mnt_flag & MNT_MWAIT) {
		mp->mnt_flag &= ~MNT_MWAIT;
		wakeup((caddr_t)mp);
	}
}

/*
 * Mark a mount point as busy.
 * Used to synchronize access and to delay unmounting.
 */
vfs_busy(mp)
	register struct mount *mp;
{

	while(mp->mnt_flag & MNT_MPBUSY) {
		mp->mnt_flag |= MNT_MPWANT;
		sleep((caddr_t)&mp->mnt_flag, PVFS);
	}
	if (mp->mnt_flag & MNT_UNMOUNT)
		return (1);
	mp->mnt_flag |= MNT_MPBUSY;
	return (0);
}

/*
 * Free a busy filesystem.
 * Panic if filesystem is not busy.
 */
vfs_unbusy(mp)
	register struct mount *mp;
{

	if ((mp->mnt_flag & MNT_MPBUSY) == 0)
		panic("vfs_unbusy: not busy");
	mp->mnt_flag &= ~MNT_MPBUSY;
	if (mp->mnt_flag & MNT_MPWANT) {
		mp->mnt_flag &= ~MNT_MPWANT;
		wakeup((caddr_t)&mp->mnt_flag);
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
		if (mp->mnt_stat.f_fsid.val[0] == fsid->val[0] &&
		    mp->mnt_stat.f_fsid.val[1] == fsid->val[1]) {
			return (mp);
		}
		mp = mp->mnt_next;
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
		vap->va_size_rsv = vap->va_blocksize = vap->va_rdev =
		vap->va_bytes = vap->va_bytes_rsv =
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
	ndp->ni_uioseg = UIO_SYSSPACE;
}

/*
 * Duplicate a nameidata structure
 */
nddup(ndp, newndp)
	register struct nameidata *ndp, *newndp;
{

	ndinit(newndp);
	newndp->ni_cred = ndp->ni_cred;
	crhold(newndp->ni_cred);
}

/*
 * Release a nameidata structure
 */
ndrele(ndp)
	register struct nameidata *ndp;
{

	crfree(ndp->ni_cred);
}

/*
 * Routines having to do with the management of the vnode table.
 */
struct vnode *vfreeh, **vfreet;
extern struct vnodeops dead_vnodeops, spec_vnodeops;
extern void vclean();
long numvnodes;
struct vattr va_null;

/*
 * Initialize the vnode structures and initialize each file system type.
 */
vfsinit()
{
	struct vfsops **vfsp;

	/*
	 * Initialize the vnode name cache
	 */
	nchinit();
	/*
	 * Initialize each file system type.
	 */
	vattr_null(&va_null);
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

	if (numvnodes < desiredvnodes) {
		vp = (struct vnode *)malloc((u_long)sizeof *vp,
		    M_VNODE, M_WAITOK);
		bzero((char *)vp, sizeof *vp);
		numvnodes++;
	} else {
		if ((vp = vfreeh) == NULL) {
			tablefull("vnode");
			*vpp = 0;
			return (ENFILE);
		}
		if (vp->v_usecount)
			panic("free vnode isn't");
		if (vq = vp->v_freef)
			vq->v_freeb = &vfreeh;
		else
			vfreet = &vfreeh;
		vfreeh = vq;
		vp->v_freef = NULL;
		vp->v_freeb = NULL;
		if (vp->v_type != VBAD)
			vgone(vp);
		vp->v_flag = 0;
		vp->v_lastr = 0;
		vp->v_socket = 0;
	}
	vp->v_type = VNON;
	cache_purge(vp);
	vp->v_tag = tag;
	vp->v_op = vops;
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
	vp->v_mount = mp;
	if (mp == NULL) {
		vp->v_mountf = NULL;
		vp->v_mountb = NULL;
		return;
	}
	if (mp->mnt_mounth) {
		vp->v_mountf = mp->mnt_mounth;
		vp->v_mountb = &mp->mnt_mounth;
		mp->mnt_mounth->v_mountb = &vp->v_mountf;
		mp->mnt_mounth = vp;
	} else {
		mp->mnt_mounth = vp;
		vp->v_mountb = &mp->mnt_mounth;
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

	if (dev == NODEV)
		return (0);
	error = getnewvnode(VT_NON, (struct mount *)0, &spec_vnodeops, &nvp);
	if (error) {
		*vpp = 0;
		return (error);
	}
	vp = nvp;
	vp->v_type = VBLK;
	if (nvp = checkalias(vp, dev, (struct mount *)0)) {
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
 * the existing contents and return the aliased vnode. The
 * caller is responsible for filling it with its new contents.
 */
struct vnode *
checkalias(nvp, nvp_rdev, mp)
	register struct vnode *nvp;
	dev_t nvp_rdev;
	struct mount *mp;
{
	register struct vnode *vp;
	struct vnode **vpp;

	if (nvp->v_type != VBLK && nvp->v_type != VCHR)
		return (NULLVP);

	vpp = &speclisth[SPECHASH(nvp_rdev)];
loop:
	for (vp = *vpp; vp; vp = vp->v_specnext) {
		if (nvp_rdev != vp->v_rdev || nvp->v_type != vp->v_type)
			continue;
		/*
		 * Alias, but not in use, so flush it out.
		 */
		if (vp->v_usecount == 0) {
			vgone(vp);
			goto loop;
		}
		if (vget(vp))
			goto loop;
		break;
	}
	if (vp == NULL || vp->v_tag != VT_NON) {
		MALLOC(nvp->v_specinfo, struct specinfo *,
			sizeof(struct specinfo), M_VNODE, M_WAITOK);
		nvp->v_rdev = nvp_rdev;
		nvp->v_hashchain = vpp;
		nvp->v_specnext = *vpp;
		nvp->v_specflags = 0;
		*vpp = nvp;
		if (vp != NULL) {
			nvp->v_flag |= VALIASED;
			vp->v_flag |= VALIASED;
			vput(vp);
		}
		return (NULLVP);
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
	if (vp->v_usecount == 0) {
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

	vp->v_usecount++;
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
	struct proc *p = curproc;		/* XXX */

	if (vp == NULL)
		panic("vrele: null vp");
	vp->v_usecount--;
	if (vp->v_usecount < 0)
		vprint("vrele: bad ref count", vp);
	if (vp->v_usecount > 0)
		return;
	if (vfreeh == NULLVP) {
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
	VOP_INACTIVE(vp, p);
}

/*
 * Page or buffer structure gets a reference.
 */
vhold(vp)
	register struct vnode *vp;
{

	vp->v_holdcnt++;
}

/*
 * Page or buffer structure frees a reference.
 */
holdrele(vp)
	register struct vnode *vp;
{

	if (vp->v_holdcnt <= 0)
		panic("holdrele: holdcnt");
	vp->v_holdcnt--;
}

/*
 * Remove any vnodes in the vnode table belonging to mount point mp.
 *
 * If MNT_NOFORCE is specified, there should not be any active ones,
 * return error if any are found (nb: this is a user error, not a
 * system error). If MNT_FORCE is specified, detach any active vnodes
 * that are found.
 */
int busyprt = 0;	/* patch to print out busy vnodes */

vflush(mp, skipvp, flags)
	struct mount *mp;
	struct vnode *skipvp;
	int flags;
{
	register struct vnode *vp, *nvp;
	int busy = 0;

	if ((mp->mnt_flag & MNT_MPBUSY) == 0)
		panic("vflush: not busy");
loop:
	for (vp = mp->mnt_mounth; vp; vp = nvp) {
		if (vp->v_mount != mp)
			goto loop;
		nvp = vp->v_mountf;
		/*
		 * Skip over a selected vnode.
		 */
		if (vp == skipvp)
			continue;
		/*
		 * Skip over a vnodes marked VSYSTEM.
		 */
		if ((flags & SKIPSYSTEM) && (vp->v_flag & VSYSTEM))
			continue;
		/*
		 * With v_usecount == 0, all we need to do is clear
		 * out the vnode data structures and we are done.
		 */
		if (vp->v_usecount == 0) {
			vgone(vp);
			continue;
		}
		/*
		 * For block or character devices, revert to an
		 * anonymous device. For all other files, just kill them.
		 */
		if (flags & FORCECLOSE) {
			if (vp->v_type != VBLK && vp->v_type != VCHR) {
				vgone(vp);
			} else {
				vclean(vp, 0);
				vp->v_op = &spec_vnodeops;
				insmntque(vp, (struct mount *)0);
			}
			continue;
		}
		if (busyprt)
			vprint("vflush: busy vnode", vp);
		busy++;
	}
	if (busy)
		return (EBUSY);
	return (0);
}

/*
 * Disassociate the underlying file system from a vnode.
 */
void vclean(vp, flags)
	register struct vnode *vp;
	int flags;
{
	struct vnodeops *origops;
	int active;
	struct proc *p = curproc;	/* XXX */

	/*
	 * Check to see if the vnode is in use.
	 * If so we have to reference it before we clean it out
	 * so that its count cannot fall to zero and generate a
	 * race against ourselves to recycle it.
	 */
	if (active = vp->v_usecount)
		VREF(vp);
	/*
	 * Prevent the vnode from being recycled or
	 * brought into use while we clean it out.
	 */
	if (vp->v_flag & VXLOCK)
		panic("vclean: deadlock");
	vp->v_flag |= VXLOCK;
	/*
	 * Even if the count is zero, the VOP_INACTIVE routine may still
	 * have the object locked while it cleans it out. The VOP_LOCK
	 * ensures that the VOP_INACTIVE routine is done with its work.
	 * For active vnodes, it ensures that no other activity can
	 * occur while the buffer list is being cleaned out.
	 */
	VOP_LOCK(vp);
	if (flags & DOCLOSE)
		vinvalbuf(vp, 1);
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
	(*(origops->vn_unlock))(vp);
	if (active) {
		if (flags & DOCLOSE)
			(*(origops->vn_close))(vp, 0, NOCRED, p);
		(*(origops->vn_inactive))(vp, p);
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
 * Eliminate all activity associated with  the requested vnode
 * and with all vnodes aliased to the requested vnode.
 */
void vgoneall(vp)
	register struct vnode *vp;
{
	register struct vnode *vq;

	if (vp->v_flag & VALIASED) {
		/*
		 * If a vgone (or vclean) is already in progress,
		 * wait until it is done and return.
		 */
		if (vp->v_flag & VXLOCK) {
			vp->v_flag |= VXWANT;
			sleep((caddr_t)vp, PINOD);
			return;
		}
		/*
		 * Ensure that vp will not be vgone'd while we
		 * are eliminating its aliases.
		 */
		vp->v_flag |= VXLOCK;
		while (vp->v_flag & VALIASED) {
			for (vq = *vp->v_hashchain; vq; vq = vq->v_specnext) {
				if (vq->v_rdev != vp->v_rdev ||
				    vq->v_type != vp->v_type || vp == vq)
					continue;
				vgone(vq);
				break;
			}
		}
		/*
		 * Remove the lock so that vgone below will
		 * really eliminate the vnode after which time
		 * vgone will awaken any sleepers.
		 */
		vp->v_flag &= ~VXLOCK;
	}
	vgone(vp);
}

/*
 * Eliminate all activity associated with a vnode
 * in preparation for reuse.
 */
void vgone(vp)
	register struct vnode *vp;
{
	register struct vnode *vq;
	struct vnode *vx;
	long count;

	/*
	 * If a vgone (or vclean) is already in progress,
	 * wait until it is done and return.
	 */
	if (vp->v_flag & VXLOCK) {
		vp->v_flag |= VXWANT;
		sleep((caddr_t)vp, PINOD);
		return;
	}
	/*
	 * Clean out the filesystem specific data.
	 */
	vclean(vp, DOCLOSE);
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
		if (*vp->v_hashchain == vp) {
			*vp->v_hashchain = vp->v_specnext;
		} else {
			for (vq = *vp->v_hashchain; vq; vq = vq->v_specnext) {
				if (vq->v_specnext != vp)
					continue;
				vq->v_specnext = vp->v_specnext;
				break;
			}
			if (vq == NULL)
				panic("missing bdev");
		}
		if (vp->v_flag & VALIASED) {
			count = 0;
			for (vq = *vp->v_hashchain; vq; vq = vq->v_specnext) {
				if (vq->v_rdev != vp->v_rdev ||
				    vq->v_type != vp->v_type)
					continue;
				count++;
				vx = vq;
			}
			if (count == 0)
				panic("missing alias");
			if (count == 1)
				vx->v_flag &= ~VALIASED;
			vp->v_flag &= ~VALIASED;
		}
		FREE(vp->v_specinfo, M_VNODE);
		vp->v_specinfo = NULL;
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

/*
 * Lookup a vnode by device number.
 */
vfinddev(dev, type, vpp)
	dev_t dev;
	enum vtype type;
	struct vnode **vpp;
{
	register struct vnode *vp;

	for (vp = speclisth[SPECHASH(dev)]; vp; vp = vp->v_specnext) {
		if (dev != vp->v_rdev || type != vp->v_type)
			continue;
		*vpp = vp;
		return (0);
	}
	return (1);
}

/*
 * Calculate the total number of references to a special device.
 */
vcount(vp)
	register struct vnode *vp;
{
	register struct vnode *vq;
	int count;

	if ((vp->v_flag & VALIASED) == 0)
		return (vp->v_usecount);
loop:
	for (count = 0, vq = *vp->v_hashchain; vq; vq = vq->v_specnext) {
		if (vq->v_rdev != vp->v_rdev || vq->v_type != vp->v_type)
			continue;
		/*
		 * Alias, but not in use, so flush it out.
		 */
		if (vq->v_usecount == 0) {
			vgone(vq);
			goto loop;
		}
		count += vq->v_usecount;
	}
	return (count);
}

/*
 * Print out a description of a vnode.
 */
static char *typename[] =
   { "VNON", "VREG", "VDIR", "VBLK", "VCHR", "VLNK", "VSOCK", "VFIFO", "VBAD" };

vprint(label, vp)
	char *label;
	register struct vnode *vp;
{
	char buf[64];

	if (label != NULL)
		printf("%s: ", label);
	printf("type %s, usecount %d, refcount %d,", typename[vp->v_type],
		vp->v_usecount, vp->v_holdcnt);
	buf[0] = '\0';
	if (vp->v_flag & VROOT)
		strcat(buf, "|VROOT");
	if (vp->v_flag & VTEXT)
		strcat(buf, "|VTEXT");
	if (vp->v_flag & VSYSTEM)
		strcat(buf, "|VSYSTEM");
	if (vp->v_flag & VXLOCK)
		strcat(buf, "|VXLOCK");
	if (vp->v_flag & VXWANT)
		strcat(buf, "|VXWANT");
	if (vp->v_flag & VBWAIT)
		strcat(buf, "|VBWAIT");
	if (vp->v_flag & VALIASED)
		strcat(buf, "|VALIASED");
	if (buf[0] != '\0')
		printf(" flags (%s)", &buf[1]);
	printf("\n\t");
	VOP_PRINT(vp);
}

int kinfo_vdebug = 1;
int kinfo_vgetfailed;
#define KINFO_VNODESLOP	10
/*
 * Dump vnode list (via kinfo).
 * Copyout address of vnode followed by vnode.
 */
/* ARGSUSED */
kinfo_vnode(op, where, acopysize, arg, aneeded)
	int op;
	char *where;
	int *acopysize, arg, *aneeded;
{
	register struct mount *mp = rootfs;
	struct mount *omp;
	struct vnode *vp;
	register char *bp = where, *savebp;
	char *ewhere = where + *acopysize;
	int error;

#define VPTRSZ	sizeof (struct vnode *)
#define VNODESZ	sizeof (struct vnode)
	if (where == NULL) {
		*aneeded = (numvnodes + KINFO_VNODESLOP) * (VPTRSZ + VNODESZ);
		return (0);
	}
		
	do {
		if (vfs_busy(mp)) {
			mp = mp->mnt_next;
			continue;
		}
		savebp = bp;
again:
		for (vp = mp->mnt_mounth; vp; vp = vp->v_mountf) {
			/*
			 * Check that the vp is still associated with
			 * this filesystem.  RACE: could have been
			 * recycled onto the same filesystem.
			 */
			if (vp->v_mount != mp) {
				if (kinfo_vdebug)
					printf("kinfo: vp changed\n");
				bp = savebp;
				goto again;
			}
			if ((bp + VPTRSZ + VNODESZ <= ewhere) && 
			    ((error = copyout((caddr_t)&vp, bp, VPTRSZ)) ||
			     (error = copyout((caddr_t)vp, bp + VPTRSZ, 
			      VNODESZ))))
				return (error);
			bp += VPTRSZ + VNODESZ;
		}
		omp = mp;
		mp = mp->mnt_next;
		vfs_unbusy(omp);
	} while (mp != rootfs);

	*aneeded = bp - where;
	if (bp > ewhere)
		*acopysize = ewhere - where;
	else
		*acopysize = bp - where;
	return (0);
}
