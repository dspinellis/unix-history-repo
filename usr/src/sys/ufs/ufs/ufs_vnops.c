/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_vnops.c	7.68 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/resourcevar.h>
#include <sys/kernel.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/conf.h>
#include <sys/mount.h>
#include <sys/vnode.h>
#include <sys/specdev.h>
#include <sys/fifo.h>
#include <sys/malloc.h>

#include <ufs/ufs/lockf.h>
#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/dir.h>
#include <ufs/ufs/ufsmount.h>
#include <ufs/ufs/ufs_extern.h>

static int ufs_chmod __P((struct vnode *, int, struct proc *));
static int ufs_chown __P((struct vnode *, u_int, u_int, struct proc *));

enum vtype iftovt_tab[16] = {
	VNON, VFIFO, VCHR, VNON, VDIR, VNON, VBLK, VNON,
	VREG, VNON, VLNK, VNON, VSOCK, VNON, VNON, VBAD,
};
int	vttoif_tab[9] = {
	0, IFREG, IFDIR, IFBLK, IFCHR, IFLNK, IFSOCK, IFIFO, IFMT,
};

/*
 * Create a regular file
 */
int
ufs_create(ndp, vap, p)
	struct nameidata *ndp;
	struct vattr *vap;
	struct proc *p;
{
	struct vnode *vp;
	int error;

	if (error =
	    ufs_makeinode(MAKEIMODE(vap->va_type, vap->va_mode), ndp, &vp))
		return (error);
	ndp->ni_vp = vp;
	return (0);
}

/*
 * Mknod vnode call
 */
/* ARGSUSED */
int
ufs_mknod(ndp, vap, cred, p)
	struct nameidata *ndp;
	struct vattr *vap;
	struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip;
	struct vnode *vp;
	int error;

	if (error =
	    ufs_makeinode(MAKEIMODE(vap->va_type, vap->va_mode), ndp, &vp))
		return (error);
	ip = VTOI(vp);
	ip->i_flag |= IACC|IUPD|ICHG;
	if (vap->va_rdev != VNOVAL) {
		/*
		 * Want to be able to use this to make badblock
		 * inodes, so don't truncate the dev number.
		 */
		ip->i_rdev = vap->va_rdev;
	}
	/*
	 * Remove inode so that it will be reloaded by iget and
	 * checked to see if it is an alias of an existing entry
	 * in the inode cache.
	 */
	vput(vp);
	vp->v_type = VNON;
	vgone(vp);
	return (0);
}

/*
 * Open called.
 *
 * Nothing to do.
 */
/* ARGSUSED */
int
ufs_open(vp, mode, cred, p)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
	struct proc *p;
{

	return (0);
}

/*
 * Close called
 *
 * Update the times on the inode.
 */
/* ARGSUSED */
int
ufs_close(vp, fflag, cred, p)
	struct vnode *vp;
	int fflag;
	struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip;

	ip = VTOI(vp);
	if (vp->v_usecount > 1 && !(ip->i_flag & ILOCKED))
		ITIMES(ip, &time, &time);
	return (0);
}

/*
 * Check mode permission on inode pointer. Mode is READ, WRITE or EXEC.
 * The mode is shifted to select the owner/group/other fields. The
 * super user is granted all permissions.
 */
int
ufs_access(vp, mode, cred, p)
	struct vnode *vp;
	register int mode;
	struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);
	register gid_t *gp;
	int i, error;

#ifdef DIAGNOSTIC
	if (!VOP_ISLOCKED(vp)) {
		vprint("ufs_access: not locked", vp);
		panic("ufs_access: not locked");
	}
#endif
#ifdef QUOTA
	if (mode & VWRITE) {
		switch (vp->v_type) {
		case VREG: case VDIR: case VLNK:
			if (error = getinoquota(ip))
				return (error);
		}
	}
#endif /* QUOTA */
	/*
	 * If you're the super-user, you always get access.
	 */
	if (cred->cr_uid == 0)
		return (0);
	/*
	 * Access check is based on only one of owner, group, public.
	 * If not owner, then check group. If not a member of the
	 * group, then check public access.
	 */
	if (cred->cr_uid != ip->i_uid) {
		mode >>= 3;
		gp = cred->cr_groups;
		for (i = 0; i < cred->cr_ngroups; i++, gp++)
			if (ip->i_gid == *gp)
				goto found;
		mode >>= 3;
found:
		;
	}
	if ((ip->i_mode & mode) != 0)
		return (0);
	return (EACCES);
}

/* ARGSUSED */
int
ufs_getattr(vp, vap, cred, p)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip;

	ip = VTOI(vp);
	ITIMES(ip, &time, &time);
	/*
	 * Copy from inode table
	 */
	vap->va_fsid = ip->i_dev;
	vap->va_fileid = ip->i_number;
	vap->va_mode = ip->i_mode & ~IFMT;
	vap->va_nlink = ip->i_nlink;
	vap->va_uid = ip->i_uid;
	vap->va_gid = ip->i_gid;
	vap->va_rdev = (dev_t)ip->i_rdev;
#ifdef tahoe
	vap->va_size = ip->i_size;
	vap->va_size_rsv = 0;
#else
	vap->va_qsize = ip->i_din.di_qsize;
#endif
	vap->va_atime.tv_sec = ip->i_atime;
	vap->va_atime.tv_usec = 0;
	vap->va_mtime.tv_sec = ip->i_mtime;
	vap->va_mtime.tv_usec = 0;
	vap->va_ctime.tv_sec = ip->i_ctime;
	vap->va_ctime.tv_usec = 0;
	vap->va_flags = ip->i_flags;
	vap->va_gen = ip->i_gen;
	/* this doesn't belong here */
	if (vp->v_type == VBLK)
		vap->va_blocksize = BLKDEV_IOSIZE;
	else if (vp->v_type == VCHR)
		vap->va_blocksize = MAXBSIZE;
	else
		vap->va_blocksize = vp->v_mount->mnt_stat.f_bsize;
	vap->va_bytes = dbtob(ip->i_blocks);
	vap->va_bytes_rsv = 0;
	vap->va_type = vp->v_type;
	return (0);
}

/*
 * Set attribute vnode op. called from several syscalls
 */
int
ufs_setattr(vp, vap, cred, p)
	register struct vnode *vp;
	register struct vattr *vap;
	register struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip;
	int error;

	/*
	 * Check for unsettable attributes.
	 */
	if ((vap->va_type != VNON) || (vap->va_nlink != VNOVAL) ||
	    (vap->va_fsid != VNOVAL) || (vap->va_fileid != VNOVAL) ||
	    (vap->va_blocksize != VNOVAL) || (vap->va_rdev != VNOVAL) ||
	    ((int)vap->va_bytes != VNOVAL) || (vap->va_gen != VNOVAL)) {
		return (EINVAL);
	}
	/*
	 * Go through the fields and update iff not VNOVAL.
	 */
	if (vap->va_uid != (u_short)VNOVAL || vap->va_gid != (u_short)VNOVAL)
		if (error = ufs_chown(vp, vap->va_uid, vap->va_gid, p))
			return (error);
	if (vap->va_size != VNOVAL) {
		if (vp->v_type == VDIR)
			return (EISDIR);
		if (error = VOP_TRUNCATE(vp, vap->va_size, 0)) /* IO_SYNC? */
			return (error);
	}
	ip = VTOI(vp);
	if (vap->va_atime.tv_sec != VNOVAL || vap->va_mtime.tv_sec != VNOVAL) {
		if (cred->cr_uid != ip->i_uid &&
		    (error = suser(cred, &p->p_acflag)))
			return (error);
		if (vap->va_atime.tv_sec != VNOVAL)
			ip->i_flag |= IACC;
		if (vap->va_mtime.tv_sec != VNOVAL)
			ip->i_flag |= IUPD;
		ip->i_flag |= ICHG;
		if (error = VOP_UPDATE(vp, &vap->va_atime, &vap->va_mtime, 1))
			return (error);
	}
	error = 0;
	if (vap->va_mode != (u_short)VNOVAL)
		error = ufs_chmod(vp, (int)vap->va_mode, p);
	if (vap->va_flags != VNOVAL) {
		if (cred->cr_uid != ip->i_uid &&
		    (error = suser(cred, &p->p_acflag)))
			return (error);
		if (cred->cr_uid == 0) {
			ip->i_flags = vap->va_flags;
		} else {
			ip->i_flags &= 0xffff0000;
			ip->i_flags |= (vap->va_flags & 0xffff);
		}
		ip->i_flag |= ICHG;
	}
	return (error);
}

/*
 * Change the mode on a file.
 * Inode must be locked before calling.
 */
static int
ufs_chmod(vp, mode, p)
	register struct vnode *vp;
	register int mode;
	struct proc *p;
{
	register struct ucred *cred = p->p_ucred;
	register struct inode *ip = VTOI(vp);
	int error;

	if (cred->cr_uid != ip->i_uid &&
	    (error = suser(cred, &p->p_acflag)))
		return (error);
	if (cred->cr_uid) {
		if (vp->v_type != VDIR && (mode & ISVTX))
			return (EFTYPE);
		if (!groupmember(ip->i_gid, cred) && (mode & ISGID))
			return (EPERM);
	}
	ip->i_mode &= ~07777;
	ip->i_mode |= mode & 07777;
	ip->i_flag |= ICHG;
	if ((vp->v_flag & VTEXT) && (ip->i_mode & ISVTX) == 0)
		(void) vnode_pager_uncache(vp);
	return (0);
}

/*
 * Perform chown operation on inode ip;
 * inode must be locked prior to call.
 */
static int
ufs_chown(vp, uid, gid, p)
	register struct vnode *vp;
	u_int uid;
	u_int gid;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);
	register struct ucred *cred = p->p_ucred;
	uid_t ouid;
	gid_t ogid;
	int error = 0;
#ifdef QUOTA
	register int i;
	long change;
#endif

	if (uid == (u_short)VNOVAL)
		uid = ip->i_uid;
	if (gid == (u_short)VNOVAL)
		gid = ip->i_gid;
	/*
	 * If we don't own the file, are trying to change the owner
	 * of the file, or are not a member of the target group,
	 * the caller must be superuser or the call fails.
	 */
	if ((cred->cr_uid != ip->i_uid || uid != ip->i_uid ||
	    !groupmember((gid_t)gid, cred)) &&
	    (error = suser(cred, &p->p_acflag)))
		return (error);
	ouid = ip->i_uid;
	ogid = ip->i_gid;
#ifdef QUOTA
	if (error = getinoquota(ip))
		return (error);
	if (ouid == uid) {
		dqrele(vp, ip->i_dquot[USRQUOTA]);
		ip->i_dquot[USRQUOTA] = NODQUOT;
	}
	if (ogid == gid) {
		dqrele(vp, ip->i_dquot[GRPQUOTA]);
		ip->i_dquot[GRPQUOTA] = NODQUOT;
	}
	change = ip->i_blocks;
	(void) chkdq(ip, -change, cred, CHOWN);
	(void) chkiq(ip, -1, cred, CHOWN);
	for (i = 0; i < MAXQUOTAS; i++) {
		dqrele(vp, ip->i_dquot[i]);
		ip->i_dquot[i] = NODQUOT;
	}
#endif
	ip->i_uid = uid;
	ip->i_gid = gid;
#ifdef QUOTA
	if ((error = getinoquota(ip)) == 0) {
		if (ouid == uid) {
			dqrele(vp, ip->i_dquot[USRQUOTA]);
			ip->i_dquot[USRQUOTA] = NODQUOT;
		}
		if (ogid == gid) {
			dqrele(vp, ip->i_dquot[GRPQUOTA]);
			ip->i_dquot[GRPQUOTA] = NODQUOT;
		}
		if ((error = chkdq(ip, change, cred, CHOWN)) == 0) {
			if ((error = chkiq(ip, 1, cred, CHOWN)) == 0)
				goto good;
			else
				(void) chkdq(ip, -change, cred, CHOWN|FORCE);
		}
		for (i = 0; i < MAXQUOTAS; i++) {
			dqrele(vp, ip->i_dquot[i]);
			ip->i_dquot[i] = NODQUOT;
		}
	}
	ip->i_uid = ouid;
	ip->i_gid = ogid;
	if (getinoquota(ip) == 0) {
		if (ouid == uid) {
			dqrele(vp, ip->i_dquot[USRQUOTA]);
			ip->i_dquot[USRQUOTA] = NODQUOT;
		}
		if (ogid == gid) {
			dqrele(vp, ip->i_dquot[GRPQUOTA]);
			ip->i_dquot[GRPQUOTA] = NODQUOT;
		}
		(void) chkdq(ip, change, cred, FORCE|CHOWN);
		(void) chkiq(ip, 1, cred, FORCE|CHOWN);
		(void) getinoquota(ip);
	}
	return (error);
good:
	if (getinoquota(ip))
		panic("chown: lost quota");
#endif /* QUOTA */
	if (ouid != uid || ogid != gid)
		ip->i_flag |= ICHG;
	if (ouid != uid && cred->cr_uid != 0)
		ip->i_mode &= ~ISUID;
	if (ogid != gid && cred->cr_uid != 0)
		ip->i_mode &= ~ISGID;
	return (0);
}

/* ARGSUSED */
int
ufs_ioctl(vp, com, data, fflag, cred, p)
	struct vnode *vp;
	int com;
	caddr_t data;
	int fflag;
	struct ucred *cred;
	struct proc *p;
{

	return (ENOTTY);
}

/* ARGSUSED */
int
ufs_select(vp, which, fflags, cred, p)
	struct vnode *vp;
	int which, fflags;
	struct ucred *cred;
	struct proc *p;
{

	/*
	 * We should really check to see if I/O is possible.
	 */
	return (1);
}

/*
 * Mmap a file
 *
 * NB Currently unsupported.
 */
/* ARGSUSED */
int
ufs_mmap(vp, fflags, cred, p)
	struct vnode *vp;
	int fflags;
	struct ucred *cred;
	struct proc *p;
{

	return (EINVAL);
}

/*
 * Seek on a file
 *
 * Nothing to do, so just return.
 */
/* ARGSUSED */
int
ufs_seek(vp, oldoff, newoff, cred)
	struct vnode *vp;
	off_t oldoff, newoff;
	struct ucred *cred;
{

	return (0);
}

/*
 * ufs remove
 * Hard to avoid races here, especially
 * in unlinking directories.
 */
int
ufs_remove(ndp, p)
	struct nameidata *ndp;
	struct proc *p;
{
	register struct inode *ip, *dp;
	int error;

	ip = VTOI(ndp->ni_vp);
	dp = VTOI(ndp->ni_dvp);
	error = ufs_dirremove(ndp);
	if (!error) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
	if (dp == ip)
		vrele(ITOV(ip));
	else
		ufs_iput(ip);
	ufs_iput(dp);
	return (error);
}

/*
 * link vnode call
 */
int
ufs_link(vp, ndp, p)
	register struct vnode *vp;
	register struct nameidata *ndp;
	struct proc *p;
{
	register struct inode *ip;
	int error;

#ifdef DIANOSTIC
	if ((ndp->ni_nameiop & HASBUF) == 0)
		panic("ufs_link: no name");
#endif
	ip = VTOI(vp);
	if ((unsigned short)ip->i_nlink >= LINK_MAX) {
		free(ndp->ni_pnbuf, M_NAMEI);
		return (EMLINK);
	}
	if (ndp->ni_dvp != vp)
		ILOCK(ip);
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	error = VOP_UPDATE(vp, &time, &time, 1);
	if (!error)
		error = ufs_direnter(ip, ndp);
	if (ndp->ni_dvp != vp)
		IUNLOCK(ip);
	FREE(ndp->ni_pnbuf, M_NAMEI);
	vput(ndp->ni_dvp);
	if (error) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
	return (error);
}

/*
 * Rename system call.
 * 	rename("foo", "bar");
 * is essentially
 *	unlink("bar");
 *	link("foo", "bar");
 *	unlink("foo");
 * but ``atomically''.  Can't do full commit without saving state in the
 * inode on disk which isn't feasible at this time.  Best we can do is
 * always guarantee the target exists.
 *
 * Basic algorithm is:
 *
 * 1) Bump link count on source while we're linking it to the
 *    target.  This also ensure the inode won't be deleted out
 *    from underneath us while we work (it may be truncated by
 *    a concurrent `trunc' or `open' for creation).
 * 2) Link source to destination.  If destination already exists,
 *    delete it first.
 * 3) Unlink source reference to inode if still around. If a
 *    directory was moved and the parent of the destination
 *    is different from the source, patch the ".." entry in the
 *    directory.
 */
int
ufs_rename(fndp, tndp, p)
	register struct nameidata *fndp, *tndp;
	struct proc *p;
{
	register struct inode *ip, *xp, *dp;
	struct dirtemplate dirbuf;
	int doingdirectory = 0, oldparent = 0, newparent = 0;
	int error = 0;

#ifdef DIANOSTIC
	if ((tndp->ni_nameiop & HASBUF) == 0 ||
	    (fndp->ni_nameiop & HASBUF) == 0)
		panic("ufs_rename: no name");
#endif
	dp = VTOI(fndp->ni_dvp);
	ip = VTOI(fndp->ni_vp);
	/*
	 * Check if just deleting a link name.
	 */
	if (fndp->ni_vp == tndp->ni_vp) {
		VOP_ABORTOP(tndp);
		vput(tndp->ni_dvp);
		vput(tndp->ni_vp);
		vrele(fndp->ni_dvp);
		if ((ip->i_mode&IFMT) == IFDIR) {
			VOP_ABORTOP(fndp);
			vrele(fndp->ni_vp);
			return (EINVAL);
		}
		doingdirectory = 0;
		goto unlinkit;
	}
	ILOCK(ip);
	if ((ip->i_mode&IFMT) == IFDIR) {
		/*
		 * Avoid ".", "..", and aliases of "." for obvious reasons.
		 */
		if ((fndp->ni_namelen == 1 && fndp->ni_ptr[0] == '.') ||
		    dp == ip || fndp->ni_isdotdot || (ip->i_flag & IRENAME)) {
			VOP_ABORTOP(tndp);
			vput(tndp->ni_dvp);
			if (tndp->ni_vp)
				vput(tndp->ni_vp);
			VOP_ABORTOP(fndp);
			vrele(fndp->ni_dvp);
			vput(fndp->ni_vp);
			return (EINVAL);
		}
		ip->i_flag |= IRENAME;
		oldparent = dp->i_number;
		doingdirectory++;
	}
	vrele(fndp->ni_dvp);

	/*
	 * 1) Bump link count while we're moving stuff
	 *    around.  If we crash somewhere before
	 *    completing our work, the link count
	 *    may be wrong, but correctable.
	 */
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	error = VOP_UPDATE(fndp->ni_vp, &time, &time, 1);
	IUNLOCK(ip);

	/*
	 * When the target exists, both the directory
	 * and target vnodes are returned locked.
	 */
	dp = VTOI(tndp->ni_dvp);
	xp = NULL;
	if (tndp->ni_vp)
		xp = VTOI(tndp->ni_vp);
	/*
	 * If ".." must be changed (ie the directory gets a new
	 * parent) then the source directory must not be in the
	 * directory heirarchy above the target, as this would
	 * orphan everything below the source directory. Also
	 * the user must have write permission in the source so
	 * as to be able to change "..". We must repeat the call 
	 * to namei, as the parent directory is unlocked by the
	 * call to checkpath().
	 */
	if (oldparent != dp->i_number)
		newparent = dp->i_number;
	if (doingdirectory && newparent) {
		VOP_LOCK(fndp->ni_vp);
		error = ufs_access(fndp->ni_vp, VWRITE, tndp->ni_cred, p);
		VOP_UNLOCK(fndp->ni_vp);
		if (error)
			goto bad;
		if (xp != NULL)
			ufs_iput(xp);
		if (error = ufs_checkpath(ip, dp, tndp->ni_cred))
			goto out;
		if ((tndp->ni_nameiop & SAVESTART) == 0)
			panic("ufs_rename: lost to startdir");
		p->p_spare[1]--;
		if (error = lookup(tndp, p))
			goto out;
		dp = VTOI(tndp->ni_dvp);
		xp = NULL;
		if (tndp->ni_vp)
			xp = VTOI(tndp->ni_vp);
	}
	/*
	 * 2) If target doesn't exist, link the target
	 *    to the source and unlink the source. 
	 *    Otherwise, rewrite the target directory
	 *    entry to reference the source inode and
	 *    expunge the original entry's existence.
	 */
	if (xp == NULL) {
		if (dp->i_dev != ip->i_dev)
			panic("rename: EXDEV");
		/*
		 * Account for ".." in new directory.
		 * When source and destination have the same
		 * parent we don't fool with the link count.
		 */
		if (doingdirectory && newparent) {
			if ((unsigned short)dp->i_nlink >= LINK_MAX) {
				error = EMLINK;
				goto bad;
			}
			dp->i_nlink++;
			dp->i_flag |= ICHG;
			if (error = VOP_UPDATE(ITOV(dp), &time, &time, 1))
				goto bad;
		}
		if (error = ufs_direnter(ip, tndp)) {
			if (doingdirectory && newparent) {
				dp->i_nlink--;
				dp->i_flag |= ICHG;
				(void)VOP_UPDATE(ITOV(dp), &time, &time, 1);
			}
			goto bad;
		}
		ufs_iput(dp);
	} else {
		if (xp->i_dev != dp->i_dev || xp->i_dev != ip->i_dev)
			panic("rename: EXDEV");
		/*
		 * Short circuit rename(foo, foo).
		 */
		if (xp->i_number == ip->i_number)
			panic("rename: same file");
		/*
		 * If the parent directory is "sticky", then the user must
		 * own the parent directory, or the destination of the rename,
		 * otherwise the destination may not be changed (except by
		 * root). This implements append-only directories.
		 */
		if ((dp->i_mode & ISVTX) && tndp->ni_cred->cr_uid != 0 &&
		    tndp->ni_cred->cr_uid != dp->i_uid &&
		    xp->i_uid != tndp->ni_cred->cr_uid) {
			error = EPERM;
			goto bad;
		}
		/*
		 * Target must be empty if a directory and have no links
		 * to it. Also, ensure source and target are compatible
		 * (both directories, or both not directories).
		 */
		if ((xp->i_mode&IFMT) == IFDIR) {
			if (!ufs_dirempty(xp, dp->i_number, tndp->ni_cred) || 
			    xp->i_nlink > 2) {
				error = ENOTEMPTY;
				goto bad;
			}
			if (!doingdirectory) {
				error = ENOTDIR;
				goto bad;
			}
			cache_purge(ITOV(dp));
		} else if (doingdirectory) {
			error = EISDIR;
			goto bad;
		}
		if (error = ufs_dirrewrite(dp, ip, tndp))
			goto bad;
		/*
		 * If the target directory is in the same
		 * directory as the source directory,
		 * decrement the link count on the parent
		 * of the target directory.
		 */
		 if (doingdirectory && !newparent) {
			dp->i_nlink--;
			dp->i_flag |= ICHG;
		}
		ufs_iput(dp);
		/*
		 * Adjust the link count of the target to
		 * reflect the dirrewrite above.  If this is
		 * a directory it is empty and there are
		 * no links to it, so we can squash the inode and
		 * any space associated with it.  We disallowed
		 * renaming over top of a directory with links to
		 * it above, as the remaining link would point to
		 * a directory without "." or ".." entries.
		 */
		xp->i_nlink--;
		if (doingdirectory) {
			if (--xp->i_nlink != 0)
				panic("rename: linked directory");
			error = VOP_TRUNCATE(ITOV(xp), (u_long)0, IO_SYNC);
		}
		xp->i_flag |= ICHG;
		ufs_iput(xp);
		xp = NULL;
	}

	/*
	 * 3) Unlink the source.
	 */
unlinkit:
	fndp->ni_nameiop &= ~MODMASK;
	fndp->ni_nameiop |= LOCKPARENT | LOCKLEAF;
	if ((fndp->ni_nameiop & SAVESTART) == 0)
		panic("ufs_rename: lost from startdir");
	p->p_spare[1]--;
	(void) lookup(fndp, p);
	if (fndp->ni_vp != NULL) {
		xp = VTOI(fndp->ni_vp);
		dp = VTOI(fndp->ni_dvp);
	} else {
		/*
		 * From name has disappeared.
		 */
		if (doingdirectory)
			panic("rename: lost dir entry");
		vrele(ITOV(ip));
		return (0);
	}
	/*
	 * Ensure that the directory entry still exists and has not
	 * changed while the new name has been entered. If the source is
	 * a file then the entry may have been unlinked or renamed. In
	 * either case there is no further work to be done. If the source
	 * is a directory then it cannot have been rmdir'ed; its link
	 * count of three would cause a rmdir to fail with ENOTEMPTY.
	 * The IRENAME flag ensures that it cannot be moved by another
	 * rename.
	 */
	if (xp != ip) {
		if (doingdirectory)
			panic("rename: lost dir entry");
	} else {
		/*
		 * If the source is a directory with a
		 * new parent, the link count of the old
		 * parent directory must be decremented
		 * and ".." set to point to the new parent.
		 */
		if (doingdirectory && newparent) {
			dp->i_nlink--;
			dp->i_flag |= ICHG;
			error = vn_rdwr(UIO_READ, ITOV(xp), (caddr_t)&dirbuf,
				sizeof (struct dirtemplate), (off_t)0,
				UIO_SYSSPACE, IO_NODELOCKED, 
				tndp->ni_cred, (int *)0, (struct proc *)0);
			if (error == 0) {
				if (dirbuf.dotdot_namlen != 2 ||
				    dirbuf.dotdot_name[0] != '.' ||
				    dirbuf.dotdot_name[1] != '.') {
					ufs_dirbad(xp, 12,
					    "rename: mangled dir");
				} else {
					dirbuf.dotdot_ino = newparent;
					(void) vn_rdwr(UIO_WRITE, ITOV(xp),
					    (caddr_t)&dirbuf,
					    sizeof (struct dirtemplate),
					    (off_t)0, UIO_SYSSPACE,
					    IO_NODELOCKED|IO_SYNC,
					    tndp->ni_cred, (int *)0,
					    (struct proc *)0);
					cache_purge(ITOV(dp));
				}
			}
		}
		error = ufs_dirremove(fndp);
		if (!error) {
			xp->i_nlink--;
			xp->i_flag |= ICHG;
		}
		xp->i_flag &= ~IRENAME;
	}
	if (dp)
		vput(ITOV(dp));
	if (xp)
		vput(ITOV(xp));
	vrele(ITOV(ip));
	return (error);

bad:
	if (xp)
		vput(ITOV(xp));
	vput(ITOV(dp));
out:
	ip->i_nlink--;
	ip->i_flag |= ICHG;
	vrele(ITOV(ip));
	return (error);
}

/*
 * A virgin directory (no blushing please).
 */
static struct dirtemplate mastertemplate = {
	0, 12, 1, ".",
	0, DIRBLKSIZ - 12, 2, ".."
};

/*
 * Mkdir system call
 */
int
ufs_mkdir(ndp, vap, p)
	struct nameidata *ndp;
	struct vattr *vap;
	struct proc *p;
{
	register struct inode *ip, *dp;
	struct vnode *tvp;
	struct vnode *dvp;
	struct dirtemplate dirtemplate;
	int error;
	int dmode;

#ifdef DIANOSTIC
	if ((ndp->ni_nameiop & HASBUF) == 0)
		panic("ufs_mkdir: no name");
#endif
	dvp = ndp->ni_dvp;
	dp = VTOI(dvp);
	if ((unsigned short)dp->i_nlink >= LINK_MAX) {
		free(ndp->ni_pnbuf, M_NAMEI);
		ufs_iput(dp);
		return (EMLINK);
	}
	dmode = vap->va_mode&0777;
	dmode |= IFDIR;
	/*
	 * Must simulate part of maknode here to acquire the inode, but
	 * not have it entered in the parent directory. The entry is made
	 * later after writing "." and ".." entries.
	 */
	if (error = VOP_VALLOC(dvp, dmode, ndp->ni_cred, &tvp)) {
		free(ndp->ni_pnbuf, M_NAMEI);
		ufs_iput(dp);
		return (error);
	}
	ip = VTOI(tvp);
	ip->i_uid = ndp->ni_cred->cr_uid;
	ip->i_gid = dp->i_gid;
#ifdef QUOTA
	if ((error = getinoquota(ip)) ||
	    (error = chkiq(ip, 1, ndp->ni_cred, 0))) {
		free(ndp->ni_pnbuf, M_NAMEI);
		VOP_VFREE(tvp, ip->i_number, dmode);
		ufs_iput(ip);
		ufs_iput(dp);
		return (error);
	}
#endif
	ip->i_flag |= IACC|IUPD|ICHG;
	ip->i_mode = dmode;
	ITOV(ip)->v_type = VDIR;	/* Rest init'd in iget() */
	ip->i_nlink = 2;
	error = VOP_UPDATE(ITOV(ip), &time, &time, 1);

	/*
	 * Bump link count in parent directory
	 * to reflect work done below.  Should
	 * be done before reference is created
	 * so reparation is possible if we crash.
	 */
	dp->i_nlink++;
	dp->i_flag |= ICHG;
	if (error = VOP_UPDATE(ITOV(dp), &time, &time, 1))
		goto bad;

	/* Initialize directory with "." and ".." from static template. */
	dirtemplate = mastertemplate;
	dirtemplate.dot_ino = ip->i_number;
	dirtemplate.dotdot_ino = dp->i_number;
	error = vn_rdwr(UIO_WRITE, ITOV(ip), (caddr_t)&dirtemplate,
	    sizeof (dirtemplate), (off_t)0, UIO_SYSSPACE,
	    IO_NODELOCKED|IO_SYNC, ndp->ni_cred, (int *)0, (struct proc *)0);
	if (error) {
		dp->i_nlink--;
		dp->i_flag |= ICHG;
		goto bad;
	}
	if (DIRBLKSIZ > VFSTOUFS(dvp->v_mount)->um_mountp->mnt_stat.f_fsize)
		panic("ufs_mkdir: blksize"); /* XXX should grow with balloc() */
	else {
		ip->i_size = DIRBLKSIZ;
		ip->i_flag |= ICHG;
	}

	/* Directory set up, now install it's entry in the parent directory. */
	if (error = ufs_direnter(ip, ndp)) {
		dp->i_nlink--;
		dp->i_flag |= ICHG;
	}
bad:
	/*
	 * No need to do an explicit VOP_TRUNCATE here, vrele will do this
	 * for us because we set the link count to 0.
	 */
	if (error) {
		ip->i_nlink = 0;
		ip->i_flag |= ICHG;
		ufs_iput(ip);
	} else
		ndp->ni_vp = ITOV(ip);
	FREE(ndp->ni_pnbuf, M_NAMEI);
	ufs_iput(dp);
	return (error);
}

/*
 * Rmdir system call.
 */
int
ufs_rmdir(ndp, p)
	register struct nameidata *ndp;
	struct proc *p;
{
	register struct inode *ip, *dp;
	int error;

	ip = VTOI(ndp->ni_vp);
	dp = VTOI(ndp->ni_dvp);
	/*
	 * No rmdir "." please.
	 */
	if (dp == ip) {
		vrele(ndp->ni_dvp);
		ufs_iput(ip);
		return (EINVAL);
	}
	/*
	 * Verify the directory is empty (and valid).
	 * (Rmdir ".." won't be valid since
	 *  ".." will contain a reference to
	 *  the current directory and thus be
	 *  non-empty.)
	 */
	error = 0;
	if (ip->i_nlink != 2 ||
	    !ufs_dirempty(ip, dp->i_number, ndp->ni_cred)) {
		error = ENOTEMPTY;
		goto out;
	}
	/*
	 * Delete reference to directory before purging
	 * inode.  If we crash in between, the directory
	 * will be reattached to lost+found,
	 */
	if (error = ufs_dirremove(ndp))
		goto out;
	dp->i_nlink--;
	dp->i_flag |= ICHG;
	cache_purge(ndp->ni_dvp);
	ufs_iput(dp);
	ndp->ni_dvp = NULL;
	/*
	 * Truncate inode.  The only stuff left
	 * in the directory is "." and "..".  The
	 * "." reference is inconsequential since
	 * we're quashing it.  The ".." reference
	 * has already been adjusted above.  We've
	 * removed the "." reference and the reference
	 * in the parent directory, but there may be
	 * other hard links so decrement by 2 and
	 * worry about them later.
	 */
	ip->i_nlink -= 2;
	error = VOP_TRUNCATE(ndp->ni_vp, (u_long)0, IO_SYNC);
	cache_purge(ITOV(ip));
out:
	if (ndp->ni_dvp)
		ufs_iput(dp);
	ufs_iput(ip);
	return (error);
}

/*
 * symlink -- make a symbolic link
 */
int
ufs_symlink(ndp, vap, target, p)
	struct nameidata *ndp;
	struct vattr *vap;
	char *target;
	struct proc *p;
{
	struct vnode *vp;
	int error;

	if (error = ufs_makeinode(IFLNK | vap->va_mode, ndp, &vp))
		return (error);
	error = vn_rdwr(UIO_WRITE, vp, target, strlen(target), (off_t)0,
		UIO_SYSSPACE, IO_NODELOCKED, ndp->ni_cred, (int *)0,
		(struct proc *)0);
	vput(vp);
	return (error);
}

/*
 * Vnode op for read and write
 */
int
ufs_readdir(vp, uio, cred, eofflagp)
	struct vnode *vp;
	register struct uio *uio;
	struct ucred *cred;
	int *eofflagp;
{
	int count, lost, error;

	count = uio->uio_resid;
	count &= ~(DIRBLKSIZ - 1);
	lost = uio->uio_resid - count;
	if (count < DIRBLKSIZ || (uio->uio_offset & (DIRBLKSIZ -1)))
		return (EINVAL);
	uio->uio_resid = count;
	uio->uio_iov->iov_len = count;
	error = VOP_READ(vp, uio, 0, cred);
	uio->uio_resid += lost;
	if ((VTOI(vp)->i_size - uio->uio_offset) <= 0)
		*eofflagp = 1;
	else
		*eofflagp = 0;
	return (error);
}

/*
 * Return target name of a symbolic link
 */
int
ufs_readlink(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{

	return (VOP_READ(vp, uiop, 0, cred));
}

/*
 * Ufs abort op, called after namei() when a CREATE/DELETE isn't actually
 * done. If a buffer has been saved in anticipation of a CREATE, delete it.
 */
/* ARGSUSED */
int
ufs_abortop(ndp)
	struct nameidata *ndp;
{

	if ((ndp->ni_nameiop & (HASBUF | SAVESTART)) == HASBUF)
		FREE(ndp->ni_pnbuf, M_NAMEI);
	return (0);
}

/*
 * Lock an inode.
 */
int
ufs_lock(vp)
	struct vnode *vp;
{
	register struct inode *ip = VTOI(vp);

	ILOCK(ip);
	return (0);
}

/*
 * Unlock an inode.
 */
int
ufs_unlock(vp)
	struct vnode *vp;
{
	register struct inode *ip = VTOI(vp);

	if (!(ip->i_flag & ILOCKED))
		panic("ufs_unlock NOT LOCKED");
	IUNLOCK(ip);
	return (0);
}

/*
 * Check for a locked inode.
 */
int
ufs_islocked(vp)
	struct vnode *vp;
{

	if (VTOI(vp)->i_flag & ILOCKED)
		return (1);
	return (0);
}

/*
 * Calculate the logical to physical mapping if not done already,
 * then call the device strategy routine.
 */
int checkoverlap = 0;

int
ufs_strategy(bp)
	register struct buf *bp;
{
	register struct inode *ip;
	struct vnode *vp;
	int error;

	ip = VTOI(bp->b_vp);
	if (bp->b_vp->v_type == VBLK || bp->b_vp->v_type == VCHR)
		panic("ufs_strategy: spec");
	if (bp->b_blkno == bp->b_lblkno) {
		if (error =
		    VOP_BMAP(bp->b_vp, bp->b_lblkno, NULL, &bp->b_blkno))
			return (error);
		if ((long)bp->b_blkno == -1)
			clrbuf(bp);
	}
	if ((long)bp->b_blkno == -1) {
		biodone(bp);
		return (0);
	}
#ifdef DIAGNOSTIC
	if (checkoverlap && bp->b_vp->v_mount->mnt_stat.f_type == MOUNT_UFS)
		ffs_checkoverlap(bp, ip);
#endif
		
	vp = ip->i_devvp;
	bp->b_dev = vp->v_rdev;
	(vp->v_op->vop_strategy)(bp);
	return (0);
}

/*
 * Print out the contents of an inode.
 */
int
ufs_print(vp)
	struct vnode *vp;
{
	register struct inode *ip = VTOI(vp);

	printf("tag VT_UFS, ino %d, on dev %d, %d", ip->i_number,
		major(ip->i_dev), minor(ip->i_dev));
#ifdef FIFO
	if (vp->v_type == VFIFO)
		fifo_printinfo(vp);
#endif /* FIFO */
	printf("%s\n", (ip->i_flag & ILOCKED) ? " (LOCKED)" : "");
	if (ip->i_spare0 == 0)
		return (0);
	printf("\towner pid %d", ip->i_spare0);
	if (ip->i_spare1)
		printf(" waiting pid %d", ip->i_spare1);
	printf("\n");
	return (0);
}

/*
 * Read wrapper for special devices.
 */
int
ufsspec_read(vp, uio, ioflag, cred)
	struct vnode *vp;
	struct uio *uio;
	int ioflag;
	struct ucred *cred;
{

	/*
	 * Set access flag.
	 */
	VTOI(vp)->i_flag |= IACC;
	return (spec_read(vp, uio, ioflag, cred));
}

/*
 * Write wrapper for special devices.
 */
int
ufsspec_write(vp, uio, ioflag, cred)
	struct vnode *vp;
	struct uio *uio;
	int ioflag;
	struct ucred *cred;
{

	/*
	 * Set update and change flags.
	 */
	VTOI(vp)->i_flag |= IUPD|ICHG;
	return (spec_write(vp, uio, ioflag, cred));
}

/*
 * Close wrapper for special devices.
 *
 * Update the times on the inode then do device close.
 */
int
ufsspec_close(vp, fflag, cred, p)
	struct vnode *vp;
	int fflag;
	struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);

	if (vp->v_usecount > 1 && !(ip->i_flag & ILOCKED))
		ITIMES(ip, &time, &time);
	return (spec_close(vp, fflag, cred, p));
}

#ifdef FIFO
/*
 * Read wrapper for fifo's
 */
int
ufsfifo_read(vp, uio, ioflag, cred)
	struct vnode *vp;
	struct uio *uio;
	int ioflag;
	struct ucred *cred;
{

	/*
	 * Set access flag.
	 */
	VTOI(vp)->i_flag |= IACC;
	return (fifo_read(vp, uio, ioflag, cred));
}

/*
 * Write wrapper for fifo's.
 */
int
ufsfifo_write(vp, uio, ioflag, cred)
	struct vnode *vp;
	struct uio *uio;
	int ioflag;
	struct ucred *cred;
{

	/*
	 * Set update and change flags.
	 */
	VTOI(vp)->i_flag |= IUPD|ICHG;
	return (fifo_write(vp, uio, ioflag, cred));
}

/*
 * Close wrapper for fifo's.
 *
 * Update the times on the inode then do device close.
 */
ufsfifo_close(vp, fflag, cred, p)
	struct vnode *vp;
	int fflag;
	struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);

	if (vp->v_usecount > 1 && !(ip->i_flag & ILOCKED))
		ITIMES(ip, &time, &time);
	return (fifo_close(vp, fflag, cred, p));
}
#endif /* FIFO */

/*
 * Advisory record locking support
 */
int
ufs_advlock(vp, id, op, fl, flags)
	struct vnode *vp;
	caddr_t id;
	int op;
	register struct flock *fl;
	int flags;
{
	register struct inode *ip = VTOI(vp);
	register struct lockf *lock;
	off_t start, end;
	int error;

	/*
	 * Avoid the common case of unlocking when inode has no locks.
	 */
	if (ip->i_lockf == (struct lockf *)0) {
		if (op != F_SETLK) {
			fl->l_type = F_UNLCK;
			return (0);
		}
	}
	/*
	 * Convert the flock structure into a start and end.
	 */
	switch (fl->l_whence) {

	case SEEK_SET:
	case SEEK_CUR:
		/*
		 * Caller is responsible for adding any necessary offset
		 * when SEEK_CUR is used.
		 */
		start = fl->l_start;
		break;

	case SEEK_END:
		start = ip->i_size + fl->l_start;
		break;

	default:
		return (EINVAL);
	}
	if (start < 0)
		return (EINVAL);
	if (fl->l_len == 0)
		end = -1;
	else
		end = start + fl->l_len - 1;
	/*
	 * Create the lockf structure
	 */
	MALLOC(lock, struct lockf *, sizeof *lock, M_LOCKF, M_WAITOK);
	lock->lf_start = start;
	lock->lf_end = end;
	lock->lf_id = id;
	lock->lf_inode = ip;
	lock->lf_type = fl->l_type;
	lock->lf_next = (struct lockf *)0;
	lock->lf_block = (struct lockf *)0;
	lock->lf_flags = flags;
	/*
	 * Do the requested operation.
	 */
	switch(op) {
	case F_SETLK:
		return (lf_setlock(lock));

	case F_UNLCK:
		error = lf_clearlock(lock);
		FREE(lock, M_LOCKF);
		return (error);

	case F_GETLK:
		error = lf_getlock(lock, fl);
		FREE(lock, M_LOCKF);
		return (error);
	
	default:
		free(lock, M_LOCKF);
		return (EINVAL);
	}
	/* NOTREACHED */
}

/*
 * Initialize the vnode associated with a new inode, handle aliased
 * vnodes.
 */
int
ufs_vinit(mntp, specops, fifoops, vpp)
	struct mount *mntp;
	struct vnodeops *specops, *fifoops;
	struct vnode **vpp;
{
	struct inode *ip, *nip;
	struct vnode *vp, *nvp;

	vp = *vpp;
	ip = VTOI(vp);
	switch(vp->v_type = IFTOVT(ip->i_mode)) {
	case VCHR:
	case VBLK:
		vp->v_op = specops;
		if (nvp = checkalias(vp, ip->i_rdev, mntp)) {
			/*
			 * Reinitialize aliased inode.
			 */
			vp = nvp;
			nip = VTOI(vp);
			nip->i_vnode = vp;
			nip->i_flag = 0;
			nip->i_din = ip->i_din;
			nip->i_dev = ip->i_dev;
			nip->i_number = ip->i_number;
			ufs_ihashins(nip);
			/*
			 * Discard unneeded inode.
			 */
			ip->i_mode = 0;
			ufs_iput(ip);
			ip = nip;
		}
		break;
	case VFIFO:
#ifdef FIFO
		vp->v_op = fifoops;
		break;
#else
		return (EOPNOTSUPP);
#endif
	}
	if (ip->i_number == ROOTINO)
                vp->v_flag |= VROOT;
	*vpp = vp;
	return (0);
}

/*
 * Allocate a new inode.
 */
int
ufs_makeinode(mode, ndp, vpp)
	int mode;
	register struct nameidata *ndp;
	struct vnode **vpp;
{
	register struct inode *ip, *pdir;
	struct vnode *tvp;
	int error;

	pdir = VTOI(ndp->ni_dvp);
#ifdef DIANOSTIC
	if ((ndp->ni_nameiop & HASBUF) == 0)
		panic("ufs_makeinode: no name");
#endif
	*vpp = NULL;
	if ((mode & IFMT) == 0)
		mode |= IFREG;

	if (error = VOP_VALLOC(ndp->ni_dvp, mode, ndp->ni_cred, &tvp)) {
		free(ndp->ni_pnbuf, M_NAMEI);
		ufs_iput(pdir);
		return (error);
	}
	ip = VTOI(tvp);
	ip->i_uid = ndp->ni_cred->cr_uid;
	ip->i_gid = pdir->i_gid;
#ifdef QUOTA
	if ((error = getinoquota(ip)) ||
	    (error = chkiq(ip, 1, ndp->ni_cred, 0))) {
		free(ndp->ni_pnbuf, M_NAMEI);
		VOP_VFREE(tvp, ip->i_number, mode);
		ufs_iput(ip);
		ufs_iput(pdir);
		return (error);
	}
#endif
	ip->i_flag |= IACC|IUPD|ICHG;
	ip->i_mode = mode;
	tvp->v_type = IFTOVT(mode);	/* Rest init'd in iget() */
	ip->i_nlink = 1;
	if ((ip->i_mode & ISGID) && !groupmember(ip->i_gid, ndp->ni_cred) &&
	    suser(ndp->ni_cred, NULL))
		ip->i_mode &= ~ISGID;

	/*
	 * Make sure inode goes to disk before directory entry.
	 */
	if (error = VOP_UPDATE(tvp, &time, &time, 1))
		goto bad;
	if (error = ufs_direnter(ip, ndp))
		goto bad;
	if ((ndp->ni_nameiop & SAVESTART) == 0)
		FREE(ndp->ni_pnbuf, M_NAMEI);
	ufs_iput(pdir);
	*vpp = tvp;
	return (0);

bad:
	/*
	 * Write error occurred trying to update the inode
	 * or the directory so must deallocate the inode.
	 */
	free(ndp->ni_pnbuf, M_NAMEI);
	ufs_iput(pdir);
	ip->i_nlink = 0;
	ip->i_flag |= ICHG;
	ufs_iput(ip);
	return (error);
}
