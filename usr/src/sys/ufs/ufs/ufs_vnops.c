/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_vnops.c	7.91 (Berkeley) %G%
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

#include <vm/vm.h>

#include <ufs/ufs/lockf.h>
#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/dir.h>
#include <ufs/ufs/ufsmount.h>
#include <ufs/ufs/ufs_extern.h>

int ufs_chmod __P((struct vnode *, int, struct ucred *, struct proc *));
int ufs_chown
	__P((struct vnode *, uid_t, gid_t, struct ucred *, struct proc *));

#ifdef _NOQUAD
#define	SETHIGH(q, h)	(q).val[_QUAD_HIGHWORD] = (h)
#define	SETLOW(q, l)	(q).val[_QUAD_LOWWORD] = (l)
#else /* QUAD */
union _qcvt {
	quad_t qcvt;
	long val[2];
};
#define SETHIGH(q, h) { \
	union _qcvt tmp; \
	tmp.qcvt = (q); \
	tmp.val[_QUAD_HIGHWORD] = (h); \
	(q) = tmp.qcvt; \
}
#define SETLOW(q, l) { \
	union _qcvt tmp; \
	tmp.qcvt = (q); \
	tmp.val[_QUAD_LOWWORD] = (l); \
	(q) = tmp.qcvt; \
}
#endif /* QUAD */

/*
 * Create a regular file
 */
int
ufs_create (ap)
	struct vop_create_args *ap;
{
	int error;

	if (error =
	    ufs_makeinode(MAKEIMODE(ap->a_vap->va_type, ap->a_vap->va_mode), ap->a_dvp, ap->a_vpp, ap->a_cnp))
		return (error);
	return (0);
}

/*
 * Mknod vnode call
 */
/* ARGSUSED */
int
ufs_mknod (ap)
	struct vop_mknod_args *ap;
{
	register struct inode *ip;
	int error;

	if (error =
	    ufs_makeinode(MAKEIMODE(ap->a_vap->va_type, ap->a_vap->va_mode), ap->a_dvp, ap->a_vpp, ap->a_cnp))
		return (error);
	ip = VTOI(*ap->a_vpp);
	ip->i_flag |= IACC|IUPD|ICHG;
	if (ap->a_vap->va_rdev != VNOVAL) {
		/*
		 * Want to be able to use this to make badblock
		 * inodes, so don't truncate the dev number.
		 */
		ip->i_rdev = ap->a_vap->va_rdev;
	}
	/*
	 * Remove inode so that it will be reloaded by iget and
	 * checked to see if it is an alias of an existing entry
	 * in the inode cache.
	 */
	vput(*ap->a_vpp);
	(*ap->a_vpp)->v_type = VNON;
	vgone(*ap->a_vpp);
	*ap->a_vpp = 0;
	return (0);
}

/*
 * Open called.
 *
 * Nothing to do.
 */
/* ARGSUSED */
int
ufs_open (ap)
	struct vop_open_args *ap;
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
ufs_close (ap)
	struct vop_close_args *ap;
{
	register struct inode *ip;

	ip = VTOI(ap->a_vp);
	if (ap->a_vp->v_usecount > 1 && !(ip->i_flag & ILOCKED))
		ITIMES(ip, &time, &time);
	return (0);
}

/*
 * Check mode permission on inode pointer. Mode is READ, WRITE or EXEC.
 * The mode is shifted to select the owner/group/other fields. The
 * super user is granted all permissions.
 */
int
ufs_access (ap)
	struct vop_access_args *ap;
{
	USES_VOP_ISLOCKED;
	register struct inode *ip = VTOI(ap->a_vp);
	register gid_t *gp;
	int i, error;

#ifdef DIAGNOSTIC
	if (!VOP_ISLOCKED(ap->a_vp)) {
		vprint("ufs_access: not locked", ap->a_vp);
		panic("ufs_access: not locked");
	}
#endif
#ifdef QUOTA
	if (ap->a_mode & VWRITE) {
		switch (ap->a_vp->v_type) {
		case VREG: case VDIR: case VLNK:
			if (error = getinoquota(ip))
				return (error);
		}
	}
#endif /* QUOTA */
	/*
	 * If you're the super-user, you always get access.
	 */
	if (ap->a_cred->cr_uid == 0)
		return (0);
	/*
	 * Access check is based on only one of owner, group, public.
	 * If not owner, then check group. If not a member of the
	 * group, then check public access.
	 */
	if (ap->a_cred->cr_uid != ip->i_uid) {
		ap->a_mode >>= 3;
		gp = ap->a_cred->cr_groups;
		for (i = 0; i < ap->a_cred->cr_ngroups; i++, gp++)
			if (ip->i_gid == *gp)
				goto found;
		ap->a_mode >>= 3;
found:
		;
	}
	if ((ip->i_mode & ap->a_mode) != 0)
		return (0);
	return (EACCES);
}

/* ARGSUSED */
int
ufs_getattr (ap)
	struct vop_getattr_args *ap;
{
	register struct inode *ip;

	ip = VTOI(ap->a_vp);
	ITIMES(ip, &time, &time);
	/*
	 * Copy from inode table
	 */
	ap->a_vap->va_fsid = ip->i_dev;
	ap->a_vap->va_fileid = ip->i_number;
	ap->a_vap->va_mode = ip->i_mode & ~IFMT;
	ap->a_vap->va_nlink = ip->i_nlink;
	ap->a_vap->va_uid = ip->i_uid;
	ap->a_vap->va_gid = ip->i_gid;
	ap->a_vap->va_rdev = (dev_t)ip->i_rdev;
#ifdef tahoe
	ap->a_vap->va_size = ip->i_size;
	ap->a_vap->va_size_rsv = 0;
#else
	ap->a_vap->va_qsize = ip->i_din.di_qsize;
#endif
	ap->a_vap->va_atime = ip->i_atime;
	ap->a_vap->va_mtime = ip->i_mtime;
	ap->a_vap->va_ctime = ip->i_ctime;
	ap->a_vap->va_flags = ip->i_flags;
	ap->a_vap->va_gen = ip->i_gen;
	/* this doesn't belong here */
	if (ap->a_vp->v_type == VBLK)
		ap->a_vap->va_blocksize = BLKDEV_IOSIZE;
	else if (ap->a_vp->v_type == VCHR)
		ap->a_vap->va_blocksize = MAXBSIZE;
	else
		ap->a_vap->va_blocksize = ap->a_vp->v_mount->mnt_stat.f_iosize;
	ap->a_vap->va_bytes = dbtob(ip->i_blocks);
#ifdef _NOQUAD
	ap->a_vap->va_bytes_rsv = 0;
#endif
	ap->a_vap->va_type = ap->a_vp->v_type;
	ap->a_vap->va_filerev = ip->i_modrev;
	return (0);
}

/*
 * Set attribute vnode op. called from several syscalls
 */
int
ufs_setattr (ap)
	struct vop_setattr_args *ap;
{
	USES_VOP_TRUNCATE;
	USES_VOP_UPDATE;
	register struct inode *ip;
	int error;

	/*
	 * Check for unsettable attributes.
	 */
	if ((ap->a_vap->va_type != VNON) || (ap->a_vap->va_nlink != VNOVAL) ||
	    (ap->a_vap->va_fsid != VNOVAL) || (ap->a_vap->va_fileid != VNOVAL) ||
	    (ap->a_vap->va_blocksize != VNOVAL) || (ap->a_vap->va_rdev != VNOVAL) ||
	    ((int)ap->a_vap->va_bytes != VNOVAL) || (ap->a_vap->va_gen != VNOVAL)) {
		return (EINVAL);
	}
	/*
	 * Go through the fields and update iff not VNOVAL.
	 */
	if (ap->a_vap->va_uid != (uid_t)VNOVAL || ap->a_vap->va_gid != (gid_t)VNOVAL)
		if (error = ufs_chown(ap->a_vp, ap->a_vap->va_uid, ap->a_vap->va_gid, ap->a_cred, ap->a_p))
			return (error);
	if (ap->a_vap->va_size != VNOVAL) {
		if (ap->a_vp->v_type == VDIR)
			return (EISDIR);
		if (error = VOP_TRUNCATE(ap->a_vp, ap->a_vap->va_size, 0, ap->a_cred))
			return (error);
	}
	ip = VTOI(ap->a_vp);
	if (ap->a_vap->va_atime.tv_sec != VNOVAL || ap->a_vap->va_mtime.tv_sec != VNOVAL) {
		if (ap->a_cred->cr_uid != ip->i_uid &&
		    (error = suser(ap->a_cred, &ap->a_p->p_acflag)))
			return (error);
		if (ap->a_vap->va_atime.tv_sec != VNOVAL)
			ip->i_flag |= IACC;
		if (ap->a_vap->va_mtime.tv_sec != VNOVAL)
			ip->i_flag |= IUPD;
		ip->i_flag |= ICHG;
		if (error = VOP_UPDATE(ap->a_vp, &ap->a_vap->va_atime, &ap->a_vap->va_mtime, 1))
			return (error);
	}
	error = 0;
	if (ap->a_vap->va_mode != (mode_t)VNOVAL)
		error = ufs_chmod(ap->a_vp, (int)ap->a_vap->va_mode, ap->a_cred, ap->a_p);
	if (ap->a_vap->va_flags != VNOVAL) {
		if (ap->a_cred->cr_uid != ip->i_uid &&
		    (error = suser(ap->a_cred, &ap->a_p->p_acflag)))
			return (error);
		if (ap->a_cred->cr_uid == 0) {
			ip->i_flags = ap->a_vap->va_flags;
		} else {
			ip->i_flags &= 0xffff0000;
			ip->i_flags |= (ap->a_vap->va_flags & 0xffff);
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
ufs_chmod(vp, mode, cred, p)
	register struct vnode *vp;
	register int mode;
	register struct ucred *cred;
	struct proc *p;
{
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
ufs_chown(vp, uid, gid, cred, p)
	register struct vnode *vp;
	uid_t uid;
	gid_t gid;
	struct ucred *cred;
	struct proc *p;
{
	register struct inode *ip = VTOI(vp);
	uid_t ouid;
	gid_t ogid;
	int error = 0;
#ifdef QUOTA
	register int i;
	long change;
#endif

	if (uid == (uid_t)VNOVAL)
		uid = ip->i_uid;
	if (gid == (gid_t)VNOVAL)
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
ufs_ioctl (ap)
	struct vop_ioctl_args *ap;
{

	return (ENOTTY);
}

/* ARGSUSED */
int
ufs_select (ap)
	struct vop_select_args *ap;
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
ufs_mmap (ap)
	struct vop_mmap_args *ap;
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
ufs_seek (ap)
	struct vop_seek_args *ap;
{

	return (0);
}

/*
 * ufs remove
 * Hard to avoid races here, especially
 * in unlinking directories.
 */
int
ufs_remove (ap)
	struct vop_remove_args *ap;
{
	register struct inode *ip, *dp;
	int error;

	ip = VTOI(ap->a_vp);
	dp = VTOI(ap->a_dvp);
	error = ufs_dirremove(ap->a_dvp, ap->a_cnp);
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
ufs_link (ap)
	struct vop_link_args *ap;
{
	USES_VOP_UPDATE;
	register struct inode *ip;
	int error;

#ifdef DIAGNOSTIC
	if ((ap->a_cnp->cn_flags & HASBUF) == 0)
		panic("ufs_link: no name");
#endif
	ip = VTOI(ap->a_tdvp);
	if ((nlink_t)ip->i_nlink >= LINK_MAX) {
		free(ap->a_cnp->cn_pnbuf, M_NAMEI);
		return (EMLINK);
	}
	if (ap->a_vp != ap->a_tdvp)
		ILOCK(ip);
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	error = VOP_UPDATE(ap->a_tdvp, &time, &time, 1);
	if (!error)
		error = ufs_direnter(ip, ap->a_vp, ap->a_cnp);
	if (ap->a_vp != ap->a_tdvp)
		IUNLOCK(ip);
	FREE(ap->a_cnp->cn_pnbuf, M_NAMEI);
	vput(ap->a_vp);
	if (error) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
	return (error);
}



/*
 * relookup - lookup a path name component
 *    Used by lookup to re-aquire things.
 */
int
relookup(dvp, vpp, cnp)
	struct vnode *dvp, **vpp;
	struct componentname *cnp;
{
	USES_VOP_LOCK;
	USES_VOP_LOOKUP;
	USES_VOP_UNLOCK;
	register struct vnode *dp = 0;	/* the directory we are searching */
	struct vnode *tdp;		/* saved dp */
	struct mount *mp;		/* mount table entry */
	int docache;			/* == 0 do not cache last component */
	int wantparent;			/* 1 => wantparent or lockparent flag */
	int rdonly;			/* lookup read-only flag bit */
	char *cp;			/* DEBUG: check name ptr/len */
	int newhash;			/* DEBUG: check name hash */
	int error = 0;

	/*
	 * Setup: break out flag bits into variables.
	 */
	wantparent = cnp->cn_flags & (LOCKPARENT|WANTPARENT);
	docache = (cnp->cn_flags & NOCACHE) ^ NOCACHE;
	if (cnp->cn_nameiop == DELETE ||
	    (wantparent && cnp->cn_nameiop != CREATE))
		docache = 0;
	rdonly = cnp->cn_flags & RDONLY;
	cnp->cn_flags &= ~ISSYMLINK;
	dp = dvp;
	VOP_LOCK(dp);

/* dirloop: */
	/*
	 * Search a new directory.
	 *
	 * The cn_hash value is for use by vfs_cache.
	 * The last component of the filename is left accessible via
	 * cnp->cn_nameptr for callers that need the name. Callers needing
	 * the name set the SAVENAME flag. When done, they assume
	 * responsibility for freeing the pathname buffer.
	 */
#ifdef NAMEI_DIAGNOSTIC
	for (newhash = 0, cp = cnp->cn_nameptr; *cp != 0 && *cp != '/'; cp++)
		newhash += (unsigned char)*cp;
	if (newhash != cnp->cn_hash)
		panic("relookup: bad hash");
	if (cnp->cn_namelen != cp - cnp->cn_nameptr)
		panic ("relookup: bad len");
	if (*cp != 0)
		panic("relookup: not last component");
	printf("{%s}: ", cnp->cn_nameptr);
#endif

	/*
	 * Check for degenerate name (e.g. / or "")
	 * which is a way of talking about a directory,
	 * e.g. like "/." or ".".
	 */
	if (cnp->cn_nameptr[0] == '\0') {
		if (cnp->cn_nameiop != LOOKUP || wantparent) {
			error = EISDIR;
			goto bad;
		}
		if (dp->v_type != VDIR) {
			error = ENOTDIR;
			goto bad;
		}
		if (!(cnp->cn_flags & LOCKLEAF))
			VOP_UNLOCK(dp);
		*vpp = dp;
		if (cnp->cn_flags & SAVESTART)
			panic("lookup: SAVESTART");
		return (0);
	}

	if (cnp->cn_flags & ISDOTDOT)
		panic ("relookup: lookup on dot-dot");

	/*
	 * We now have a segment name to search for, and a directory to search.
	 */
	if (error = VOP_LOOKUP(dp, vpp, cnp)) {
#ifdef DIAGNOSTIC
		if (*vpp != NULL)
			panic("leaf should be empty");
#endif
		if (error != EJUSTRETURN)
			goto bad;
		/*
		 * If creating and at end of pathname, then can consider
		 * allowing file to be created.
		 */
		if (rdonly || (dvp->v_mount->mnt_flag & MNT_RDONLY)) {
			error = EROFS;
			goto bad;
		}
		/* ASSERT(dvp == ndp->ni_startdir) */
		if (cnp->cn_flags & SAVESTART)
			VREF(dvp);
		/*
		 * We return with ni_vp NULL to indicate that the entry
		 * doesn't currently exist, leaving a pointer to the
		 * (possibly locked) directory inode in ndp->ni_dvp.
		 */
		return (0);
	}
	dp = *vpp;

#ifdef DIAGNOSTIC
	/*
	 * Check for symbolic link
	 */
	if (dp->v_type == VLNK && (cnp->cn_flags & FOLLOW))
		panic ("relookup: symlink found.\n");
#endif

nextname:
	/*
	 * Check for read-only file systems.
	 */
	if (cnp->cn_nameiop == DELETE || cnp->cn_nameiop == RENAME) {
		/*
		 * Disallow directory write attempts on read-only
		 * file systems.
		 */
		if (rdonly || (dp->v_mount->mnt_flag & MNT_RDONLY) ||
		    (wantparent &&
		     (dvp->v_mount->mnt_flag & MNT_RDONLY))) {
			error = EROFS;
			goto bad2;
		}
	}
	/* ASSERT(dvp == ndp->ni_startdir) */
	if (cnp->cn_flags & SAVESTART)
		VREF(dvp);
	
	if (!wantparent)
		vrele(dvp);
	if ((cnp->cn_flags & LOCKLEAF) == 0)
		VOP_UNLOCK(dp);
	return (0);

bad2:
	if ((cnp->cn_flags & LOCKPARENT) && (cnp->cn_flags & ISLASTCN))
		VOP_UNLOCK(dvp);
	vrele(dvp);
bad:
	vput(dp);
	*vpp = NULL;
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
ufs_rename (ap)
	struct vop_rename_args *ap;
{
	USES_VOP_ABORTOP;
	USES_VOP_ACCESS;
	USES_VOP_LOCK;
	USES_VOP_TRUNCATE;
	USES_VOP_UNLOCK;
	USES_VOP_UPDATE;
	register struct inode *ip, *xp, *dp;
	struct dirtemplate dirbuf;
	int doingdirectory = 0, oldparent = 0, newparent = 0;
	int error = 0;
	int fdvpneedsrele = 1, tdvpneedsrele = 1;

#ifdef DIAGNOSTIC
	if ((ap->a_tcnp->cn_flags & HASBUF) == 0 ||
	    (ap->a_fcnp->cn_flags & HASBUF) == 0)
		panic("ufs_rename: no name");
#endif
	dp = VTOI(ap->a_fdvp);
	ip = VTOI(ap->a_fvp);
	/*
	 * Check if just deleting a link name.
	 */
	if (ap->a_fvp == ap->a_tvp) {
		VOP_ABORTOP(ap->a_tdvp, ap->a_tcnp);
		vput(ap->a_tdvp);
		vput(ap->a_tvp);
		vrele(ap->a_fdvp);
		if ((ip->i_mode&IFMT) == IFDIR) {
			VOP_ABORTOP(ap->a_fdvp, ap->a_fcnp);
			vrele(ap->a_fvp);
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
		if ((ap->a_fcnp->cn_namelen == 1 && ap->a_fcnp->cn_nameptr[0] == '.') ||
		    dp == ip || (ap->a_fcnp->cn_flags&ISDOTDOT) || (ip->i_flag & IRENAME)) {
			VOP_ABORTOP(ap->a_tdvp, ap->a_tcnp);
			vput(ap->a_tdvp);
			if (ap->a_tvp)
				vput(ap->a_tvp);
			VOP_ABORTOP(ap->a_fdvp, ap->a_fcnp);
			vrele(ap->a_fdvp);
			vput(ap->a_fvp);
			return (EINVAL);
		}
		ip->i_flag |= IRENAME;
		oldparent = dp->i_number;
		doingdirectory++;
	}
	vrele(ap->a_fdvp);

	/*
	 * 1) Bump link count while we're moving stuff
	 *    around.  If we crash somewhere before
	 *    completing our work, the link count
	 *    may be wrong, but correctable.
	 */
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	error = VOP_UPDATE(ap->a_fvp, &time, &time, 1);
	IUNLOCK(ip);

	/*
	 * When the target exists, both the directory
	 * and target vnodes are returned locked.
	 */
	dp = VTOI(ap->a_tdvp);
	xp = NULL;
	if (ap->a_tvp)
		xp = VTOI(ap->a_tvp);
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
		VOP_LOCK(ap->a_fvp);
		error = VOP_ACCESS(ap->a_fvp, VWRITE, ap->a_tcnp->cn_cred, ap->a_tcnp->cn_proc);
		VOP_UNLOCK(ap->a_fvp);
		if (error)
			goto bad;
		if (xp != NULL)
			ufs_iput(xp);
		if (error = ufs_checkpath(ip, dp, ap->a_tcnp->cn_cred))
			goto out;
		if ((ap->a_tcnp->cn_flags & SAVESTART) == 0)
			panic("ufs_rename: lost to startdir");
		p->p_spare[1]--;
		if (error = relookup(ap->a_tdvp, &ap->a_tvp, ap->a_tcnp))
			goto out;
		dp = VTOI(ap->a_tdvp);
		xp = NULL;
		if (ap->a_tvp)
			xp = VTOI(ap->a_tvp);
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
			if ((nlink_t)dp->i_nlink >= LINK_MAX) {
				error = EMLINK;
				goto bad;
			}
			dp->i_nlink++;
			dp->i_flag |= ICHG;
			if (error = VOP_UPDATE(ITOV(dp), &time, &time, 1))
				goto bad;
		}
		if (error = ufs_direnter(ip, ap->a_tdvp, ap->a_tcnp)) {
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
		if ((dp->i_mode & ISVTX) && ap->a_tcnp->cn_cred->cr_uid != 0 &&
		    ap->a_tcnp->cn_cred->cr_uid != dp->i_uid &&
		    xp->i_uid != ap->a_tcnp->cn_cred->cr_uid) {
			error = EPERM;
			goto bad;
		}
		/*
		 * Target must be empty if a directory and have no links
		 * to it. Also, ensure source and target are compatible
		 * (both directories, or both not directories).
		 */
		if ((xp->i_mode&IFMT) == IFDIR) {
			if (!ufs_dirempty(xp, dp->i_number, ap->a_tcnp->cn_cred) || 
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
		if (error = ufs_dirrewrite(dp, ip, ap->a_tcnp))
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
			error = VOP_TRUNCATE(ITOV(xp), (off_t)0, IO_SYNC,
			    ap->a_tcnp->cn_cred);
		}
		xp->i_flag |= ICHG;
		ufs_iput(xp);
		xp = NULL;
	}

	/*
	 * 3) Unlink the source.
	 */
unlinkit:
	ap->a_fcnp->cn_flags &= ~MODMASK;
	ap->a_fcnp->cn_flags |= LOCKPARENT | LOCKLEAF;
	if ((ap->a_fcnp->cn_flags & SAVESTART) == 0)
		panic("ufs_rename: lost from startdir");
	p->p_spare[1]--;
	(void) relookup(ap->a_fdvp, &ap->a_fvp, ap->a_fcnp);
	if (ap->a_fvp != NULL) {
		xp = VTOI(ap->a_fvp);
		dp = VTOI(ap->a_fdvp);
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
				ap->a_tcnp->cn_cred, (int *)0, (struct proc *)0);
			if (error == 0) {
				if (dirbuf.dotdot_namlen != 2 ||
				    dirbuf.dotdot_name[0] != '.' ||
				    dirbuf.dotdot_name[1] != '.') {
					ufs_dirbad(xp, (doff_t)12,
					    "rename: mangled dir");
				} else {
					dirbuf.dotdot_ino = newparent;
					(void) vn_rdwr(UIO_WRITE, ITOV(xp),
					    (caddr_t)&dirbuf,
					    sizeof (struct dirtemplate),
					    (off_t)0, UIO_SYSSPACE,
					    IO_NODELOCKED|IO_SYNC,
					    ap->a_tcnp->cn_cred, (int *)0,
					    (struct proc *)0);
					cache_purge(ITOV(dp));
				}
			}
		}
		error = ufs_dirremove(ap->a_fdvp, ap->a_fcnp);
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
ufs_mkdir (ap)
	struct vop_mkdir_args *ap;
{
	USES_VOP_TRUNCATE;
	USES_VOP_UPDATE;
	USES_VOP_VALLOC;
	USES_VOP_VFREE;
	register struct inode *ip, *dp;
	struct vnode *tvp;
	struct dirtemplate dirtemplate;
	int error;
	int dmode;

#ifdef DIAGNOSTIC
	if ((ap->a_cnp->cn_flags & HASBUF) == 0)
		panic("ufs_mkdir: no name");
#endif
	dp = VTOI(ap->a_dvp);
	if ((nlink_t)dp->i_nlink >= LINK_MAX) {
		free(ap->a_cnp->cn_pnbuf, M_NAMEI);
		ufs_iput(dp);
		return (EMLINK);
	}
	dmode = ap->a_vap->va_mode&0777;
	dmode |= IFDIR;
	/*
	 * Must simulate part of maknode here to acquire the inode, but
	 * not have it entered in the parent directory. The entry is made
	 * later after writing "." and ".." entries.
	 */
	if (error = VOP_VALLOC(ap->a_dvp, dmode, ap->a_cnp->cn_cred, &tvp)) {
		free(ap->a_cnp->cn_pnbuf, M_NAMEI);
		ufs_iput(dp);
		return (error);
	}
	ip = VTOI(tvp);
	ip->i_uid = ap->a_cnp->cn_cred->cr_uid;
	ip->i_gid = dp->i_gid;
#ifdef QUOTA
	if ((error = getinoquota(ip)) ||
	    (error = chkiq(ip, 1, ap->a_cnp->cn_cred, 0))) {
		free(ap->a_cnp->cn_pnbuf, M_NAMEI);
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
	    IO_NODELOCKED|IO_SYNC, ap->a_cnp->cn_cred, (int *)0, (struct proc *)0);
	if (error) {
		dp->i_nlink--;
		dp->i_flag |= ICHG;
		goto bad;
	}
	if (DIRBLKSIZ > VFSTOUFS(ap->a_dvp->v_mount)->um_mountp->mnt_stat.f_bsize)
		panic("ufs_mkdir: blksize"); /* XXX should grow with balloc() */
	else {
		ip->i_size = DIRBLKSIZ;
		ip->i_flag |= ICHG;
	}

	/* Directory set up, now install it's entry in the parent directory. */
	if (error = ufs_direnter(ip, ap->a_dvp, ap->a_cnp)) {
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
		*ap->a_vpp = ITOV(ip);
	FREE(ap->a_cnp->cn_pnbuf, M_NAMEI);
	ufs_iput(dp);
	return (error);
}

/*
 * Rmdir system call.
 */
int
ufs_rmdir (ap)
	struct vop_rmdir_args *ap;
{
	USES_VOP_TRUNCATE;
	register struct inode *ip, *dp;
	int error;

	ip = VTOI(ap->a_vp);
	dp = VTOI(ap->a_dvp);
	/*
	 * No rmdir "." please.
	 */
	if (dp == ip) {
		vrele(ap->a_dvp);
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
	    !ufs_dirempty(ip, dp->i_number, ap->a_cnp->cn_cred)) {
		error = ENOTEMPTY;
		goto out;
	}
	/*
	 * Delete reference to directory before purging
	 * inode.  If we crash in between, the directory
	 * will be reattached to lost+found,
	 */
	if (error = ufs_dirremove(ap->a_dvp, ap->a_cnp))
		goto out;
	dp->i_nlink--;
	dp->i_flag |= ICHG;
	cache_purge(ap->a_dvp);
	ufs_iput(dp);
	ap->a_dvp = NULL;
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
	error = VOP_TRUNCATE(ap->a_vp, (off_t)0, IO_SYNC, ap->a_cnp->cn_cred);
	cache_purge(ITOV(ip));
out:
	if (ap->a_dvp)
		ufs_iput(dp);
	ufs_iput(ip);
	return (error);
}

/*
 * symlink -- make a symbolic link
 */
int
ufs_symlink (ap)
	struct vop_symlink_args *ap;
{
	int error;

	if (error = ufs_makeinode(IFLNK | ap->a_vap->va_mode, ap->a_dvp, ap->a_vpp, ap->a_cnp))
		return (error);
	error = vn_rdwr(UIO_WRITE, *ap->a_vpp, ap->a_target, strlen(ap->a_target), (off_t)0,
		UIO_SYSSPACE, IO_NODELOCKED, ap->a_cnp->cn_cred, (int *)0,
		(struct proc *)0);
	vput(*ap->a_vpp);
	return (error);
}

/*
 * Vnode op for reading directories.
 * 
 * The routine below assumes that the on-disk format of a directory
 * is the same as that defined by <sys/dirent.h>. If the on-disk
 * format changes, then it will be necessary to do a conversion
 * from the on-disk format that read returns to the format defined
 * by <sys/dirent.h>.
 */
int
ufs_readdir (ap)
	struct vop_readdir_args *ap;
{
	USES_VOP_READ;
	int count, lost, error;

	count = ap->a_uio->uio_resid;
	count &= ~(DIRBLKSIZ - 1);
	lost = ap->a_uio->uio_resid - count;
	if (count < DIRBLKSIZ || (ap->a_uio->uio_offset & (DIRBLKSIZ -1)))
		return (EINVAL);
	ap->a_uio->uio_resid = count;
	ap->a_uio->uio_iov->iov_len = count;
	error = VOP_READ(ap->a_vp, ap->a_uio, 0, ap->a_cred);
	ap->a_uio->uio_resid += lost;
	if ((VTOI(ap->a_vp)->i_size - ap->a_uio->uio_offset) <= 0)
		*ap->a_eofflagp = 1;
	else
		*ap->a_eofflagp = 0;
	return (error);
}

/*
 * Return target name of a symbolic link
 */
int
ufs_readlink (ap)
	struct vop_readlink_args *ap;
{
	USES_VOP_READ;

	return (VOP_READ(ap->a_vp, ap->a_uio, 0, ap->a_cred));
}

/*
 * Ufs abort op, called after namei() when a CREATE/DELETE isn't actually
 * done. If a buffer has been saved in anticipation of a CREATE, delete it.
 */
/* ARGSUSED */
int
ufs_abortop (ap)
	struct vop_abortop_args *ap;
{
	if ((ap->a_cnp->cn_flags & (HASBUF | SAVESTART)) == HASBUF)
		FREE(ap->a_cnp->cn_pnbuf, M_NAMEI);
	return (0);
}

/*
 * Lock an inode.
 */
int
ufs_lock (ap)
	struct vop_lock_args *ap;
{
	register struct inode *ip = VTOI(ap->a_vp);

	ILOCK(ip);
	return (0);
}

/*
 * Unlock an inode.
 */
int
ufs_unlock (ap)
	struct vop_unlock_args *ap;
{
	register struct inode *ip = VTOI(ap->a_vp);

	if (!(ip->i_flag & ILOCKED))
		panic("ufs_unlock NOT LOCKED");
	IUNLOCK(ip);
	return (0);
}

/*
 * Check for a locked inode.
 */
int
ufs_islocked (ap)
	struct vop_islocked_args *ap;
{

	if (VTOI(ap->a_vp)->i_flag & ILOCKED)
		return (1);
	return (0);
}

/*
 * Calculate the logical to physical mapping if not done already,
 * then call the device strategy routine.
 */
int
ufs_strategy (ap)
	struct vop_strategy_args *ap;
{
	USES_VOP_BMAP;
	register struct inode *ip;
	struct vnode *vp;
	int error;

	ip = VTOI(ap->a_bp->b_vp);
	if (ap->a_bp->b_vp->v_type == VBLK || ap->a_bp->b_vp->v_type == VCHR)
		panic("ufs_strategy: spec");
	if (ap->a_bp->b_blkno == ap->a_bp->b_lblkno) {
		if (error =
		    VOP_BMAP(ap->a_bp->b_vp, ap->a_bp->b_lblkno, NULL, &ap->a_bp->b_blkno)) {
			ap->a_bp->b_error = error;
			ap->a_bp->b_flags |= B_ERROR;
			biodone(ap->a_bp);
			return (error);
		}
		if ((long)ap->a_bp->b_blkno == -1)
			clrbuf(ap->a_bp);
	}
	if ((long)ap->a_bp->b_blkno == -1) {
		biodone(ap->a_bp);
		return (0);
	}
	vp = ip->i_devvp;
	ap->a_bp->b_dev = vp->v_rdev;
	VOCALL (vp->v_op, VOFFSET(vop_strategy), ap);
	return (0);
}

/*
 * Print out the contents of an inode.
 */
int
ufs_print (ap)
	struct vop_print_args *ap;
{
	register struct inode *ip = VTOI(ap->a_vp);

	printf("tag VT_UFS, ino %d, on dev %d, %d", ip->i_number,
		major(ip->i_dev), minor(ip->i_dev));
#ifdef FIFO
	if (ap->a_vp->v_type == VFIFO)
		fifo_printinfo(ap->a_vp);
#endif /* FIFO */
	printf("%s\n", (ip->i_flag & ILOCKED) ? " (LOCKED)" : "");
	if (ip->i_lockholder == 0)
		return (0);
	printf("\towner pid %d", ip->i_lockholder);
	if (ip->i_lockwaiter)
		printf(" waiting pid %d", ip->i_lockwaiter);
	printf("\n");
	return (0);
}

/*
 * Read wrapper for special devices.
 */
int
ufsspec_read (ap)
	struct vop_read_args *ap;
{
	extern int (**spec_vnodeop_p)();

	/*
	 * Set access flag.
	 */
	VTOI(ap->a_vp)->i_flag |= IACC;
	return (VOCALL (spec_vnodeop_p, VOFFSET(vop_read), ap));
}

/*
 * Write wrapper for special devices.
 */
int
ufsspec_write (ap)
	struct vop_write_args *ap;
{
	extern int (**spec_vnodeop_p)();

	/*
	 * Set update and change flags.
	 */
	VTOI(ap->a_vp)->i_flag |= IUPD|ICHG;
	return (VOCALL (spec_vnodeop_p, VOFFSET(vop_write), ap));
}

/*
 * Close wrapper for special devices.
 *
 * Update the times on the inode then do device close.
 */
int
ufsspec_close (ap)
	struct vop_close_args *ap;
{
	extern int (**spec_vnodeop_p)();
	register struct inode *ip = VTOI(ap->a_vp);

	if (ap->a_vp->v_usecount > 1 && !(ip->i_flag & ILOCKED))
		ITIMES(ip, &time, &time);
	return (VOCALL (spec_vnodeop_p, VOFFSET(vop_close), ap));
}

#ifdef FIFO
/*
 * Read wrapper for fifo's
 */
int
ufsfifo_read (ap)
	struct vop_read_args *ap;
{
	extern int (**fifo_vnodeop_p)();

	/*
	 * Set access flag.
	 */
	VTOI(ap->a_vp)->i_flag |= IACC;
	return (VOCALL (fifo_vnodeop_p, VOFFSET(vop_read), ap));
}

/*
 * Write wrapper for fifo's.
 */
int
ufsfifo_write (ap)
	struct vop_write_args *ap;
{
	extern int (**fifo_vnodeop_p)();

	/*
	 * Set update and change flags.
	 */
	VTOI(ap->a_vp)->i_flag |= IUPD|ICHG;
	return (VOCALL (fifo_vnodeop_p, VOFFSET(vop_write), ap));
}

/*
 * Close wrapper for fifo's.
 *
 * Update the times on the inode then do device close.
 */
ufsfifo_close (ap)
	struct vop_close_args *ap;
{
	extern int (**fifo_vnodeop_p)();
	register struct inode *ip = VTOI(ap->a_vp);

	if (ap->a_vp->v_usecount > 1 && !(ip->i_flag & ILOCKED))
		ITIMES(ip, &time, &time);
	return (VOCALL (fifo_vnodeop_p, VOFFSET(vop_close), ap));
}
#endif /* FIFO */

/*
 * Advisory record locking support
 */
int
ufs_advlock (ap)
	struct vop_advlock_args *ap;
{
	register struct inode *ip = VTOI(ap->a_vp);
	register struct lockf *lock;
	off_t start, end;
	int error;

	/*
	 * Avoid the common case of unlocking when inode has no locks.
	 */
	if (ip->i_lockf == (struct lockf *)0) {
		if (ap->a_op != F_SETLK) {
			ap->a_fl->l_type = F_UNLCK;
			return (0);
		}
	}
	/*
	 * Convert the flock structure into a start and end.
	 */
	switch (ap->a_fl->l_whence) {

	case SEEK_SET:
	case SEEK_CUR:
		/*
		 * Caller is responsible for adding any necessary offset
		 * when SEEK_CUR is used.
		 */
		start = ap->a_fl->l_start;
		break;

	case SEEK_END:
		start = ip->i_size + ap->a_fl->l_start;
		break;

	default:
		return (EINVAL);
	}
	if (start < 0)
		return (EINVAL);
	if (ap->a_fl->l_len == 0)
		end = -1;
	else
		end = start + ap->a_fl->l_len - 1;
	/*
	 * Create the lockf structure
	 */
	MALLOC(lock, struct lockf *, sizeof *lock, M_LOCKF, M_WAITOK);
	lock->lf_start = start;
	lock->lf_end = end;
	lock->lf_id = ap->a_id;
	lock->lf_inode = ip;
	lock->lf_type = ap->a_fl->l_type;
	lock->lf_next = (struct lockf *)0;
	lock->lf_block = (struct lockf *)0;
	lock->lf_flags = ap->a_flags;
	/*
	 * Do the requested operation.
	 */
	switch(ap->a_op) {
	case F_SETLK:
		return (lf_setlock(lock));

	case F_UNLCK:
		error = lf_clearlock(lock);
		FREE(lock, M_LOCKF);
		return (error);

	case F_GETLK:
		error = lf_getlock(lock, ap->a_fl);
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
	int (**specops)();
	int (**fifoops)();
	struct vnode **vpp;
{
	struct inode *ip, *nip;
	struct vnode *vp, *nvp;
	extern int (**spec_vnodeop_p)();

	vp = *vpp;
	ip = VTOI(vp);
	switch(vp->v_type = IFTOVT(ip->i_mode)) {
	case VCHR:
	case VBLK:
		vp->v_op = specops;
		if (nvp = checkalias(vp, ip->i_rdev, mntp)) {
			/*
			 * Discard unneeded vnode, but save its inode.
			 */
			remque(ip);
			IUNLOCK(ip);
			nvp->v_data = vp->v_data;
			vp->v_data = NULL;
			vp->v_op = spec_vnodeop_p;
			vrele(vp);
			vgone(vp);
			/*
			 * Reinitialize aliased inode.
			 */
			vp = nvp;
			ip->i_vnode = vp;
			ufs_ihashins(ip);
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
	/*
	 * Initialize modrev times
	 */
	SETHIGH(ip->i_modrev, mono_time.tv_sec);
	SETLOW(ip->i_modrev, mono_time.tv_usec * 4294);
	*vpp = vp;
	return (0);
}

/*
 * Allocate a new inode.
 */
int
ufs_makeinode(mode, dvp, vpp, cnp)
	int mode;
	struct vnode *dvp;
	struct vnode **vpp;
	struct componentname *cnp;
{
	USES_VOP_UPDATE;
	USES_VOP_VALLOC;
	USES_VOP_VFREE;
	register struct inode *ip, *pdir;
	struct vnode *tvp;
	int error;

	pdir = VTOI(dvp);
#ifdef DIAGNOSTIC
	if ((cnp->cn_flags & HASBUF) == 0)
		panic("ufs_makeinode: no name");
#endif
	*vpp = NULL;
	if ((mode & IFMT) == 0)
		mode |= IFREG;

	if (error = VOP_VALLOC(dvp, mode, cnp->cn_cred, &tvp)) {
		free(cnp->cn_pnbuf, M_NAMEI);
		ufs_iput(pdir);
		return (error);
	}
	ip = VTOI(tvp);
	ip->i_uid = cnp->cn_cred->cr_uid;
	ip->i_gid = pdir->i_gid;
#ifdef QUOTA
	if ((error = getinoquota(ip)) ||
	    (error = chkiq(ip, 1, cnp->cn_cred, 0))) {
		free(cnp->cn_pnbuf, M_NAMEI);
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
	if ((ip->i_mode & ISGID) && !groupmember(ip->i_gid, cnp->cn_cred) &&
	    suser(cnp->cn_cred, NULL))
		ip->i_mode &= ~ISGID;

	/*
	 * Make sure inode goes to disk before directory entry.
	 */
	if (error = VOP_UPDATE(tvp, &time, &time, 1))
		goto bad;
	if (error = ufs_direnter(ip, dvp, cnp))
		goto bad;
	if ((cnp->cn_flags & SAVESTART) == 0)
		FREE(cnp->cn_pnbuf, M_NAMEI);
	ufs_iput(pdir);
	*vpp = tvp;
	return (0);

bad:
	/*
	 * Write error occurred trying to update the inode
	 * or the directory so must deallocate the inode.
	 */
	free(cnp->cn_pnbuf, M_NAMEI);
	ufs_iput(pdir);
	ip->i_nlink = 0;
	ip->i_flag |= ICHG;
	ufs_iput(ip);
	return (error);
}
