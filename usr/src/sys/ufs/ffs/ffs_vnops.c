/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
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
 *	@(#)ffs_vnops.c	7.8 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "buf.h"
#include "proc.h"
#include "uio.h"
#include "socket.h"
#include "socketvar.h"
#include "conf.h"
#include "mount.h"
#include "vnode.h"
#include "../ufs/inode.h"
#include "../ufs/fs.h"
#include "../ufs/quota.h"

/*
 * Global vfs data structures for ufs
 */

int	ufs_lookup(),
	ufs_create(),
	ufs_mknod(),
	ufs_open(),
	ufs_close(),
	ufs_access(),
	ufs_getattr(),
	ufs_setattr(),
	ufs_read(),
	ufs_write(),
	ufs_ioctl(),
	ufs_select(),
	ufs_mmap(),
	ufs_fsync(),
	ufs_seek(),
	ufs_remove(),
	ufs_link(),
	ufs_rename(),
	ufs_mkdir(),
	ufs_rmdir(),
	ufs_symlink(),
	ufs_readdir(),
	ufs_readlink(),
	ufs_abortop(),
	ufs_inactive(),
	ufs_lock(),
	ufs_unlock(),
	ufs_bmap(),
	ufs_strategy();

struct vnodeops ufs_vnodeops = {
	ufs_lookup,
	ufs_create,
	ufs_mknod,
	ufs_open,
	ufs_close,
	ufs_access,
	ufs_getattr,
	ufs_setattr,
	ufs_read,
	ufs_write,
	ufs_ioctl,
	ufs_select,
	ufs_mmap,
	ufs_fsync,
	ufs_seek,
	ufs_remove,
	ufs_link,
	ufs_rename,
	ufs_mkdir,
	ufs_rmdir,
	ufs_symlink,
	ufs_readdir,
	ufs_readlink,
	ufs_abortop,
	ufs_inactive,
	ufs_lock,
	ufs_unlock,
	ufs_bmap,
	ufs_strategy,
};

enum vtype iftovt_tab[8] = {
	VNON, VCHR, VDIR, VBLK, VREG, VLNK, VSOCK, VBAD,
};
int	vttoif_tab[8] = {
	0, IFREG, IFDIR, IFBLK, IFCHR, IFLNK, IFSOCK, IFMT,
};

/*
 * Create a regular file
 */
ufs_create(ndp, vap)
	struct nameidata *ndp;
	struct vattr *vap;
{
	struct inode *ip;
	int error;

	if (error = maknode(MAKEIMODE(vap->va_type, vap->va_mode), ndp, &ip))
		return (error);
	ndp->ni_vp = ITOV(ip);
	return (0);
}

/*
 * Mknod vnode call
 */
/* ARGSUSED */
ufs_mknod(ndp, vap, cred)
	struct nameidata *ndp;
	struct ucred *cred;
	struct vattr *vap;
{
	struct inode *ip;
	int error;

	if (error = maknode(MAKEIMODE(vap->va_type, vap->va_mode), ndp, &ip))
		return (error);
	if (vap->va_rdev) {
		/*
		 * Want to be able to use this to make badblock
		 * inodes, so don't truncate the dev number.
		 */
		ITOV(ip)->v_rdev = ip->i_rdev = vap->va_rdev;
		ip->i_flag |= IACC|IUPD|ICHG;
	}
	iput(ip);
	/*
	 * Remove inode so that it will be reloaded by iget and
	 * checked to see if it is an alias of an existing entry
	 * in the inode cache.
	 */
	remque(ip);
	ip->i_forw = ip;
	ip->i_back = ip;
	return (0);
}

/*
 * Open called.
 *
 * Nothing to do.
 */
/* ARGSUSED */
ufs_open(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{

	return (0);
}

/*
 * Close called
 *
 * Update the times on the inode.
 */
/* ARGSUSED */
ufs_close(vp, fflag, cred)
	struct vnode *vp;
	int fflag;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);

	if (vp->v_count > 1 && !(ip->i_flag & ILOCKED))
		ITIMES(ip, &time, &time);
	return (0);
}

ufs_access(vp, mode, cred)
	struct vnode *vp;
	int mode;
	struct ucred *cred;
{

	return (iaccess(VTOI(vp), mode, cred));
}

/* ARGSUSED */
ufs_getattr(vp, vap, cred)
	struct vnode *vp;
	register struct vattr *vap;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);

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
	vap->va_size = ip->i_ic.ic_size.val[0];
	vap->va_size1 = ip->i_ic.ic_size.val[1];
	vap->va_atime.tv_sec = ip->i_atime;
	vap->va_mtime.tv_sec = ip->i_mtime;
	vap->va_ctime.tv_sec = ip->i_ctime;
	/* this doesn't belong here */
	if (vp->v_type == VBLK)
		vap->va_blocksize = BLKDEV_IOSIZE;
	else if (vp->v_type == VCHR)
		vap->va_blocksize = MAXBSIZE;
	else
		vap->va_blocksize = ip->i_fs->fs_bsize;
	/*
	 * XXX THIS IS NOT CORRECT!!, but be sure to change vn_stat()
	 * if you change it.
	 */
	vap->va_bytes = ip->i_blocks;
	vap->va_bytes1 = -1;
	vap->va_type = vp->v_type;
	return (0);
}

/*
 * Set attribute vnode op. called from several syscalls
 */
ufs_setattr(vp, vap, cred)
	register struct vnode *vp;
	register struct vattr *vap;
	register struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);
	int error = 0;

	/*
	 * Check for unsetable attributes.
	 */
	if ((vap->va_type != VNON) || (vap->va_nlink != VNOVAL) ||
	    (vap->va_fsid != VNOVAL) || (vap->va_fileid != VNOVAL) ||
	    (vap->va_blocksize != VNOVAL) || (vap->va_rdev != VNOVAL) ||
	    ((int)vap->va_bytes != VNOVAL)) {
		return (EINVAL);
	}
	/*
	 * Go through the fields and update iff not VNOVAL.
	 */
	if (vap->va_uid != (u_short)VNOVAL || vap->va_gid != (u_short)VNOVAL)
		if (error = chown1(vp, vap->va_uid, vap->va_gid, cred))
			return (error);
	if (vap->va_size != VNOVAL) {
		if (vp->v_type == VDIR)
			return (EISDIR);
		if (error = iaccess(ip, IWRITE, cred))
			return (error);
		if (error = itrunc(ip, vap->va_size))
			return (error);
	}
	/*
	 * Check whether the following attributes can be changed.
	 */
	if (cred->cr_uid != ip->i_uid &&
	    (error = suser(cred, &u.u_acflag)))
		return (error);
	if (vap->va_atime.tv_sec != VNOVAL || vap->va_mtime.tv_sec != VNOVAL) {
		if (vap->va_atime.tv_sec != VNOVAL)
			ip->i_flag |= IACC;
		if (vap->va_mtime.tv_sec != VNOVAL)
			ip->i_flag |= IUPD;
		ip->i_flag |= ICHG;
		if (error = iupdat(ip, &vap->va_atime, &vap->va_mtime, 1))
			return (error);
	}
	if (vap->va_mode != (u_short)VNOVAL)
		error = chmod1(vp, (int)vap->va_mode, cred);
	return (error);
}

/*
 * Change the mode on a file.
 * Inode must be locked before calling.
 */
chmod1(vp, mode, cred)
	register struct vnode *vp;
	register int mode;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);

	ip->i_mode &= ~07777;
	if (cred->cr_uid) {
		if (vp->v_type != VDIR)
			mode &= ~ISVTX;
		if (!groupmember(ip->i_gid, cred))
			mode &= ~ISGID;
	}
	ip->i_mode |= mode & 07777;
	ip->i_flag |= ICHG;
	if ((vp->v_flag & VTEXT) && (ip->i_mode & ISVTX) == 0)
		xrele(vp);
	return (0);
}

/*
 * Perform chown operation on inode ip;
 * inode must be locked prior to call.
 */
chown1(vp, uid, gid, cred)
	register struct vnode *vp;
	uid_t uid;
	gid_t gid;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);
#ifdef QUOTA
	register long change;
#endif
	int error;

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
	    (error = suser(cred, &u.u_acflag)))
		return (error);
#ifdef QUOTA
	if (ip->i_uid == uid)		/* this just speeds things a little */
		change = 0;
	else
		change = ip->i_blocks;
	(void) chkdq(ip, -change, 1);
	(void) chkiq(ip->i_dev, ip, ip->i_uid, 1);
	dqrele(ip->i_dquot);
#endif
	ip->i_uid = uid;
	ip->i_gid = gid;
	ip->i_flag |= ICHG;
	if (cred->cr_ruid != 0)
		ip->i_mode &= ~(ISUID|ISGID);
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
	(void) chkdq(ip, change, 1);
	(void) chkiq(ip->i_dev, (struct inode *)NULL, (uid_t)uid, 1);
	return (u.u_error);		/* should == 0 ALWAYS !! */
#else
	return (0);
#endif
}

/* ARGSUSED */
ufs_ioctl(vp, com, data, fflag, cred)
	struct vnode *vp;
	int com;
	caddr_t data;
	int fflag;
	struct ucred *cred;
{

	printf("ufs_ioctl called with type %d\n", vp->v_type);
	return (ENOTTY);
}

/* ARGSUSED */
ufs_select(vp, which, cred)
	struct vnode *vp;
	int which;
	struct ucred *cred;
{

	printf("ufs_select called with type %d\n", vp->v_type);
	return (1);		/* XXX */
}

/*
 * Mmap a file
 *
 * NB Currently unsupported.
 */
/* ARGSUSED */
ufs_mmap(vp, fflags, cred)
	struct vnode *vp;
	int fflags;
	struct ucred *cred;
{

	return (EINVAL);
}

/*
 * Synch an open file.
 */
/* ARGSUSED */
ufs_fsync(vp, fflags, cred)
	struct vnode *vp;
	int fflags;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);
	int error;

	ILOCK(ip);
	if (fflags&FWRITE)
		ip->i_flag |= ICHG;
	error = syncip(ip);
	IUNLOCK(ip);
	return (error);
}

/*
 * Seek on a file
 *
 * Nothing to do, so just return.
 */
/* ARGSUSED */
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
ufs_remove(ndp)
	struct nameidata *ndp;
{
	register struct inode *ip, *dp;
	int error;

	ip = VTOI(ndp->ni_vp);
	dp = VTOI(ndp->ni_dvp);
	error = dirremove(ndp);
	if (!error) {
		ip->i_nlink--;
		ip->i_flag |= ICHG;
	}
	if (dp == ip)
		vrele(ITOV(ip));
	else
		iput(ip);
	iput(dp);
	return (error);
}

/*
 * link vnode call
 */
ufs_link(vp, ndp)
	register struct vnode *vp;
	register struct nameidata *ndp;
{
	register struct inode *ip = VTOI(vp);
	int error;

	if (ndp->ni_dvp != vp)
		ILOCK(ip);
	if (ip->i_nlink == LINK_MAX - 1) {
		error = EMLINK;
		goto out;
	}
	ip->i_nlink++;
	ip->i_flag |= ICHG;
	error = iupdat(ip, &time, &time, 1);
	if (!error)
		error = direnter(ip, ndp);
out:
	if (ndp->ni_dvp != vp)
		IUNLOCK(ip);
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
ufs_rename(fndp, tndp)
	register struct nameidata *fndp, *tndp;
{
	register struct inode *ip, *xp, *dp;
	struct dirtemplate dirbuf;
	int doingdirectory = 0, oldparent = 0, newparent = 0;
	int error = 0;

	dp = VTOI(fndp->ni_dvp);
	ip = VTOI(fndp->ni_vp);
	ILOCK(ip);
	if ((ip->i_mode&IFMT) == IFDIR) {
		register struct direct *d = &fndp->ni_dent;

		/*
		 * Avoid ".", "..", and aliases of "." for obvious reasons.
		 */
		if ((d->d_namlen == 1 && d->d_name[0] == '.') || dp == ip ||
		    fndp->ni_isdotdot || (ip->i_flag & IRENAME)) {
			IUNLOCK(ip);
			ufs_abortop(fndp);
			ufs_abortop(tndp);
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
	error = iupdat(ip, &time, &time, 1);
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
		if (error = iaccess(ip, IWRITE, tndp->ni_cred))
			goto bad;
		tndp->ni_nameiop = RENAME | LOCKPARENT | LOCKLEAF | NOCACHE;
		do {
			dp = VTOI(tndp->ni_dvp);
			if (xp != NULL)
				vput(ITOV(xp));
			if (error = checkpath(ip, dp, tndp->ni_cred))
				goto out;
			if (error = namei(tndp))
				goto out;
			xp = NULL;
			if (tndp->ni_vp)
				xp = VTOI(tndp->ni_vp);
		} while (dp != VTOI(tndp->ni_dvp));
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
			dp->i_nlink++;
			dp->i_flag |= ICHG;
			error = iupdat(dp, &time, &time, 1);
		}
		if (error = direnter(ip, tndp))
			goto out;
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
		 * Target must be empty if a directory
		 * and have no links to it.
		 * Also, insure source and target are
		 * compatible (both directories, or both
		 * not directories).
		 */
		if ((xp->i_mode&IFMT) == IFDIR) {
			if (!dirempty(xp, dp->i_number, tndp->ni_cred) || 
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
		if (error = dirrewrite(dp, ip, tndp))
			goto bad;
		vput(ITOV(dp));
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
			error = itrunc(xp, (u_long)0);
		}
		xp->i_flag |= ICHG;
		vput(ITOV(xp));
		xp = NULL;
	}

	/*
	 * 3) Unlink the source.
	 */
	fndp->ni_nameiop = DELETE | LOCKPARENT | LOCKLEAF;
	(void)namei(fndp);
	if (fndp->ni_vp != NULL) {
		xp = VTOI(fndp->ni_vp);
		dp = VTOI(fndp->ni_dvp);
	} else {
		xp = NULL;
		dp = NULL;
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
			error = rdwri(UIO_READ, xp, (caddr_t)&dirbuf,
				sizeof (struct dirtemplate), (off_t)0,
				UIO_SYSSPACE, tndp->ni_cred, (int *)0);
			if (error == 0) {
				if (dirbuf.dotdot_namlen != 2 ||
				    dirbuf.dotdot_name[0] != '.' ||
				    dirbuf.dotdot_name[1] != '.') {
					printf("rename: mangled dir\n");
				} else {
					dirbuf.dotdot_ino = newparent;
					(void) rdwri(UIO_WRITE, xp,
					    (caddr_t)&dirbuf,
					    sizeof (struct dirtemplate),
					    (off_t)0, UIO_SYSSPACE,
					    tndp->ni_cred, (int *)0);
					cache_purge(ITOV(dp));
				}
			}
		}
		error = dirremove(fndp);
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
struct dirtemplate mastertemplate = {
	0, 12, 1, ".",
	0, DIRBLKSIZ - 12, 2, ".."
};

/*
 * Mkdir system call
 */
ufs_mkdir(ndp, vap)
	struct nameidata *ndp;
	struct vattr *vap;
{
	register struct inode *ip, *dp;
	struct inode *tip;
	struct vnode *dvp;
	struct dirtemplate dirtemplate;
	int error;
	int dmode;

	dvp = ndp->ni_dvp;
	dp = VTOI(dvp);
	dmode = vap->va_mode&0777;
	dmode |= IFDIR;
	/*
	 * Must simulate part of maknode here
	 * in order to acquire the inode, but
	 * not have it entered in the parent
	 * directory.  The entry is made later
	 * after writing "." and ".." entries out.
	 */
	error = ialloc(dp, dirpref(dp->i_fs), dmode, &tip);
	if (error) {
		iput(dp);
		return (error);
	}
	ip = tip;
#ifdef QUOTA
	if (ip->i_dquot != NODQUOT)
		panic("mkdir: dquot");
#endif
	ip->i_flag |= IACC|IUPD|ICHG;
	ip->i_mode = dmode;
	ITOV(ip)->v_type = VDIR;	/* Rest init'd in iget() */
	ip->i_nlink = 2;
	ip->i_uid = ndp->ni_cred->cr_uid;
	ip->i_gid = dp->i_gid;
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
#endif
	error = iupdat(ip, &time, &time, 1);

	/*
	 * Bump link count in parent directory
	 * to reflect work done below.  Should
	 * be done before reference is created
	 * so reparation is possible if we crash.
	 */
	dp->i_nlink++;
	dp->i_flag |= ICHG;
	error = iupdat(dp, &time, &time, 1);

	/*
	 * Initialize directory with "."
	 * and ".." from static template.
	 */
	dirtemplate = mastertemplate;
	dirtemplate.dot_ino = ip->i_number;
	dirtemplate.dotdot_ino = dp->i_number;
	error = rdwri(UIO_WRITE, ip, (caddr_t)&dirtemplate,
		sizeof (dirtemplate), (off_t)0, UIO_SYSSPACE,
		ndp->ni_cred, (int *)0);
	if (error) {
		dp->i_nlink--;
		dp->i_flag |= ICHG;
		goto bad;
	}
	if (DIRBLKSIZ > dp->i_fs->fs_fsize)
		panic("mkdir: blksize");     /* XXX - should grow w/balloc() */
	else
		ip->i_size = DIRBLKSIZ;
	/*
	 * Directory all set up, now
	 * install the entry for it in
	 * the parent directory.
	 */
	error = direnter(ip, ndp);
	dp = NULL;
	if (error) {
		ndp->ni_nameiop = LOOKUP | NOCACHE;
		error = namei(ndp);
		if (!error) {
			dp = VTOI(ndp->ni_vp);
			dp->i_nlink--;
			dp->i_flag |= ICHG;
		}
	}
bad:
	/*
	 * No need to do an explicit itrunc here,
	 * vrele will do this for us because we set
	 * the link count to 0.
	 */
	if (error) {
		ip->i_nlink = 0;
		ip->i_flag |= ICHG;
	}
	iput(ip);
	if (dp)
		iput(dp);
	return (error);
}

/*
 * Rmdir system call.
 */
ufs_rmdir(ndp)
	register struct nameidata *ndp;
{
	register struct inode *ip, *dp;
	int error = 0;

	ip = VTOI(ndp->ni_vp);
	dp = VTOI(ndp->ni_dvp);
	/*
	 * No rmdir "." please.
	 */
	if (dp == ip) {
		vrele(ITOV(dp));
		iput(ip);
		return (EINVAL);
	}
	/*
	 * Verify the directory is empty (and valid).
	 * (Rmdir ".." won't be valid since
	 *  ".." will contain a reference to
	 *  the current directory and thus be
	 *  non-empty.)
	 */
	if (ip->i_nlink != 2 || !dirempty(ip, dp->i_number, ndp->ni_cred)) {
		error = ENOTEMPTY;
		goto out;
	}
	/*
	 * Delete reference to directory before purging
	 * inode.  If we crash in between, the directory
	 * will be reattached to lost+found,
	 */
	if (error = dirremove(ndp))
		goto out;
	dp->i_nlink--;
	dp->i_flag |= ICHG;
	cache_purge(ITOV(dp));
	iput(dp);
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
	error = itrunc(ip, (u_long)0);
	cache_purge(ITOV(ip));
out:
	if (ndp->ni_dvp)
		iput(dp);
	iput(ip);
	return (error);
}

/*
 * symlink -- make a symbolic link
 */
ufs_symlink(ndp, vap, target)
	struct nameidata *ndp;
	struct vattr *vap;
	char *target;
{
	struct inode *ip;
	int error;

	error = maknode(IFLNK | vap->va_mode, ndp, &ip);
	if (error)
		return (error);
	error = rdwri(UIO_WRITE, ip, target, strlen(target), (off_t)0,
		UIO_SYSSPACE, ndp->ni_cred, (int *)0);
	iput(ip);
	return (error);
}

/*
 * Vnode op for read and write
 */
ufs_readdir(vp, uio, offp, cred)
	struct vnode *vp;
	register struct uio *uio;
	off_t *offp;
	struct ucred *cred;
{
	register struct inode *ip = VTOI(vp);
	int count, error;

	ILOCK(ip);
	uio->uio_offset = *offp;
	count = uio->uio_resid;
	count &= ~(DIRBLKSIZ - 1);
	if (vp->v_type != VDIR || uio->uio_iovcnt != 1 ||
	    (count < DIRBLKSIZ) || (uio->uio_offset & (DIRBLKSIZ -1))) {
		IUNLOCK(ip);
		return (EINVAL);
	}
	uio->uio_resid = count;
	uio->uio_iov->iov_len = count;
	error = readip(ip, uio, cred);
	*offp += count - uio->uio_resid;
	IUNLOCK(ip);
	return (error);
}

/*
 * Return target name of a symbolic link
 */
ufs_readlink(vp, uiop, cred)
	struct vnode *vp;
	struct uio *uiop;
	struct ucred *cred;
{

	return (readip(VTOI(vp), uiop, cred));
}

/*
 * Ufs abort op, called after namei() when a CREATE/DELETE isn't actually
 * done. Iff ni_vp/ni_dvp not null and locked, unlock.
 */
ufs_abortop(ndp)
	register struct nameidata *ndp;
{
	register struct inode *ip;

	if (ndp->ni_vp) {
		ip = VTOI(ndp->ni_vp);
		if (ip->i_flag & ILOCKED)
			IUNLOCK(ip);
		vrele(ndp->ni_vp);
	}
	if (ndp->ni_dvp) {
		ip = VTOI(ndp->ni_dvp);
		if (ip->i_flag & ILOCKED)
			IUNLOCK(ip);
		vrele(ndp->ni_dvp);
	}
	return;
}

ufs_lock(vp)
	struct vnode *vp;
{
	register struct inode *ip = VTOI(vp);

	ILOCK(ip);
	return (0);
}

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
 * Get access to bmap
 */
ufs_bmap(vp, bn, vpp, bnp)
	struct vnode *vp;
	daddr_t bn;
	struct vnode **vpp;
	daddr_t *bnp;
{
	struct inode *ip = VTOI(vp);

	if (vpp != NULL)
		*vpp = ip->i_devvp;
	if (bnp == NULL)
		return (0);
	return (bmap(ip, bn, bnp, (daddr_t *)0, (int *)0));
}

/*
 * Just call the device strategy routine
 */
ufs_strategy(bp)
	register struct buf *bp;
{
	(*bdevsw[major(bp->b_dev)].d_strategy)(bp);
	return (0);
}

/*
 * Make a new file.
 */
maknode(mode, ndp, ipp)
	int mode;
	register struct nameidata *ndp;
	struct inode **ipp;
{
	register struct inode *ip;
	struct inode *tip;
	register struct inode *pdir = VTOI(ndp->ni_dvp);
	ino_t ipref;
	int error;

	*ipp = 0;
	if ((mode & IFMT) == IFDIR)
		ipref = dirpref(pdir->i_fs);
	else
		ipref = pdir->i_number;
	error = ialloc(pdir, ipref, mode, &tip);
	if (error) {
		iput(pdir);
		return (error);
	}
	ip = tip;
#ifdef QUOTA
	if (ip->i_dquot != NODQUOT)
		panic("maknode: dquot");
#endif
	ip->i_flag |= IACC|IUPD|ICHG;
	if ((mode & IFMT) == 0)
		mode |= IFREG;
	ip->i_mode = mode;
	ITOV(ip)->v_type = IFTOVT(mode);	/* Rest init'd in iget() */
	ip->i_nlink = 1;
	ip->i_uid = ndp->ni_cred->cr_uid;
	ip->i_gid = pdir->i_gid;
	if ((ip->i_mode & ISGID) && !groupmember(ip->i_gid, ndp->ni_cred) &&
	    suser(ndp->ni_cred, NULL))
		ip->i_mode &= ~ISGID;
#ifdef QUOTA
	ip->i_dquot = inoquota(ip);
#endif

	/*
	 * Make sure inode goes to disk before directory entry.
	 */
	if ((error = iupdat(ip, &time, &time, 1)) ||
	    (error = direnter(ip, ndp))) {
		/*
		 * Write error occurred trying to update the inode
		 * or the directory so must deallocate the inode.
		 */
		ip->i_nlink = 0;
		ip->i_flag |= ICHG;
		iput(ip);
		return (error);
	}
	*ipp = ip;
	return (0);
}
