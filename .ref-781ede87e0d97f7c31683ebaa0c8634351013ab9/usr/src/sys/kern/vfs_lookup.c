/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_lookup.c	7.22 (Berkeley) %G%
 */

#include "param.h"
#include "time.h"
#include "namei.h"
#include "vnode.h"
#include "mount.h"
#include "errno.h"
#include "malloc.h"

#ifdef KTRACE
#include "user.h"
#include "proc.h"
#include "ktrace.h"
#endif

/*
 * Convert a pathname into a pointer to a locked inode.
 * This is a very central and rather complicated routine.
 *
 * The flag argument is LOOKUP, CREATE, RENAME, or DELETE depending on
 * whether the name is to be looked up, created, renamed, or deleted.
 * When CREATE, RENAME, or DELETE is specified, information usable in
 * creating, renaming, or deleting a directory entry may be calculated.
 * If flag has LOCKPARENT or'ed into it and the target of the pathname
 * exists, namei returns both the target and its parent directory locked.
 * When creating or renaming and LOCKPARENT is specified, the target may not
 * be ".".  When deleting and LOCKPARENT is specified, the target may be ".".
 *
 * The FOLLOW flag is set when symbolic links are to be followed
 * when they occur at the end of the name translation process.
 * Symbolic links are always followed for all other pathname
 * components other than the last.
 *
 * The segflg defines whether the name is to be copied from user
 * space or kernel space.
 *
 * Overall outline of namei:
 *
 *	copy in name
 *	get starting directory
 * dirloop:
 *	copy next component of name to ndp->ni_dent
 *	handle degenerate case where name is null string
 *	if .. and on mounted filesys, find parent
 *	call lookup routine for next component name
 *	  directory vnode returned in ni_dvp, unlocked unless LOCKPARENT set
 *	  component vnode returned in ni_vp (if it exists), locked.
 *	if symbolic link, massage name in buffer and continue at dirloop
 *	if result inode is mounted on, find mounted on vnode
 *	if more components of name, do next level at dirloop
 *	return the answer in ni_vp as locked vnode;
 *	  if LOCKPARENT set, return locked parent in ni_dvp
 *
 * NOTE: (LOOKUP | LOCKPARENT) currently returns the parent vnode unlocked.
 */
namei(ndp)
	register struct nameidata *ndp;
{
	register char *cp;		/* pointer into pathname argument */
	register struct vnode *dp = 0;	/* the directory we are searching */
	register int i;		   	/* Temp counter */
	struct vnode *tdp;		/* saved dp */
	struct mount *mp;		/* mount table entry */
	int docache;			/* == 0 do not cache last component */
	int flag;			/* LOOKUP, CREATE, RENAME or DELETE */
	int wantparent;			/* 1 => wantparent or lockparent flag */
	int lockparent;			/* 1 => lockparent flag */
	int getbuf;			/* 1 => Malloc a pathname buffer */
	int rdonly;			/* mounted read-only flag bit(s) */
	int error = 0;

	/*
	 * Setup: break out flag bits into variables.
	 */
	ndp->ni_dvp = NULL;
	flag = ndp->ni_nameiop & OPFLAG;
	wantparent = ndp->ni_nameiop & (LOCKPARENT|WANTPARENT);
	lockparent = ndp->ni_nameiop & LOCKPARENT;
	docache = (ndp->ni_nameiop & NOCACHE) ^ NOCACHE;
	getbuf = (ndp->ni_nameiop & HASBUF) ^ HASBUF;
	if (flag == DELETE || wantparent)
		docache = 0;
	rdonly = MNT_RDONLY;
	if (ndp->ni_nameiop & REMOTE)
		rdonly |= MNT_EXRDONLY;
	/*
	 * Get a buffer for the name to be translated, and copy the
	 * name into the buffer.
	 */
	if (getbuf) {
		MALLOC(ndp->ni_pnbuf, caddr_t, MAXPATHLEN, M_NAMEI, M_WAITOK);
		if (ndp->ni_segflg == UIO_SYSSPACE)
			error = copystr(ndp->ni_dirp, ndp->ni_pnbuf,
				    MAXPATHLEN, &ndp->ni_pathlen);
		else
			error = copyinstr(ndp->ni_dirp, ndp->ni_pnbuf,
				    MAXPATHLEN, &ndp->ni_pathlen);
		if (error) {
			free(ndp->ni_pnbuf, M_NAMEI);
			ndp->ni_vp = NULL;
			return (error);
		}
		ndp->ni_ptr = ndp->ni_pnbuf;
	}
	ndp->ni_loopcnt = 0;
	dp = ndp->ni_cdir;
	VREF(dp);
#ifdef KTRACE
	if (KTRPOINT(u.u_procp, KTR_NAMEI))
		ktrnamei(u.u_procp->p_tracep, ndp->ni_pnbuf);
#endif

start:
	/*
	 * Get starting directory.
	 * Done at start of translation and after symbolic link.
	 */
	if (*ndp->ni_ptr == '/') {
		vrele(dp);
		while (*ndp->ni_ptr == '/') {
			ndp->ni_ptr++;
			ndp->ni_pathlen--;
		}
		if ((dp = ndp->ni_rdir) == NULL)
			dp = rootdir;
		VREF(dp);
	}
	VOP_LOCK(dp);
	ndp->ni_endoff = 0;

	/*
	 * We come to dirloop to search a new directory.
	 */
dirloop:
	/*
	 * Copy next component of name to ndp->ni_dent.
	 * XXX kern_exec looks at d_name
	 * ??? The ni_hash value may be useful for vfs_cache
	 * XXX There must be the last component of the filename left
	 * somewhere accessible via. ndp for NFS (and any other stateless file
	 * systems) in case they are doing a CREATE. The "Towards a..." noted
	 * that ni_ptr would be left pointing to the last component, but since
	 * the ni_pnbuf gets free'd, that is not a good idea.
	 */
	if (getbuf) {
		ndp->ni_hash = 0;
		for (cp = ndp->ni_ptr, i = 0; *cp != 0 && *cp != '/'; cp++) {
			if (i >= MAXNAMLEN) {
				error = ENAMETOOLONG;
				goto bad;
			}
			if (*cp & 0200)
				if ((*cp&0377) == ('/'|0200) ||
				    flag != DELETE) {
					error = EINVAL;
					goto bad;
				}
			ndp->ni_dent.d_name[i++] = *cp;
			ndp->ni_hash += (unsigned char)*cp * i;
		}
		ndp->ni_namelen = i;
		ndp->ni_dent.d_namlen = i;
		ndp->ni_dent.d_name[i] = '\0';
		ndp->ni_pathlen -= i;
		ndp->ni_next = cp;
#ifdef NAMEI_DIAGNOSTIC
		printf("{%s}: ", ndp->ni_dent.d_name);
#endif
	}
	cp = ndp->ni_next;
	ndp->ni_makeentry = 1;
	if (*cp == '\0' && docache == 0)
		ndp->ni_makeentry = 0;
	ndp->ni_isdotdot = (ndp->ni_namelen == 2 &&
		ndp->ni_dent.d_name[1] == '.' && ndp->ni_dent.d_name[0] == '.');

	/*
	 * Check for degenerate name (e.g. / or "")
	 * which is a way of talking about a directory,
	 * e.g. like "/." or ".".
	 */
	if (ndp->ni_ptr[0] == '\0') {
		if (flag != LOOKUP || wantparent) {
			error = EISDIR;
			goto bad;
		}
		if (getbuf)
			free(ndp->ni_pnbuf, M_NAMEI);
		if (!(ndp->ni_nameiop & LOCKLEAF))
			VOP_UNLOCK(dp);
		ndp->ni_vp = dp;
		return (0);
	}

	/*
	 * Handle "..": two special cases.
	 * 1. If at root directory (e.g. after chroot)
	 *    then ignore it so can't get out.
	 * 2. If this vnode is the root of a mounted
	 *    file system, then replace it with the
	 *    vnode which was mounted on so we take the
	 *    .. in the other file system.
	 */
	if (ndp->ni_isdotdot) {
		for (;;) {
			if (dp == ndp->ni_rdir || dp == rootdir) {
				ndp->ni_dvp = dp;
				ndp->ni_vp = dp;
				VREF(dp);
				goto nextname;
			}
			if ((dp->v_flag & VROOT) == 0 ||
				(ndp->ni_nameiop & NOCROSSMOUNT))
				break;
			tdp = dp;
			dp = dp->v_mount->mnt_vnodecovered;
			vput(tdp);
			VREF(dp);
			VOP_LOCK(dp);
		}
	}

	/*
	 * We now have a segment name to search for, and a directory to search.
	 */
	if (error = VOP_LOOKUP(dp, ndp)) {
		if (ndp->ni_vp != NULL)
			panic("leaf should be empty");
#ifdef NAMEI_DIAGNOSTIC
		printf("not found\n");
#endif
		if (flag == LOOKUP || flag == DELETE ||
		    error != ENOENT || *cp != 0)
			goto bad;
		/*
		 * If creating and at end of pathname, then can consider
		 * allowing file to be created.
		 */
		if (ndp->ni_dvp->v_mount->mnt_flag & rdonly) {
			error = EROFS;
			goto bad;
		}
		/*
		 * We return with ni_vp NULL to indicate that the entry
		 * doesn't currently exist, leaving a pointer to the
		 * (possibly locked) directory inode in ndp->ni_dvp.
		 */
		if (getbuf)
			FREE(ndp->ni_pnbuf, M_NAMEI);
		return (0);	/* should this be ENOENT? */
	}
#ifdef NAMEI_DIAGNOSTIC
	printf("found\n");
#endif

	/*
	 * Check for symbolic link
	 */
	dp = ndp->ni_vp;
	if ((dp->v_type == VLNK) &&
	    ((ndp->ni_nameiop & FOLLOW) || *ndp->ni_next == '/')) {
		struct iovec aiov;
		struct uio auio;
		int linklen;

		if (!getbuf)
			panic("namei: unexpected symlink");
		if (++ndp->ni_loopcnt > MAXSYMLINKS) {
			error = ELOOP;
			goto bad2;
		}
		if (ndp->ni_pathlen > 1)
			MALLOC(cp, char *, MAXPATHLEN, M_NAMEI, M_WAITOK);
		else
			cp = ndp->ni_pnbuf;
		aiov.iov_base = cp;
		aiov.iov_len = MAXPATHLEN;
		auio.uio_iov = &aiov;
		auio.uio_iovcnt = 1;
		auio.uio_offset = 0;
		auio.uio_rw = UIO_READ;
		auio.uio_segflg = UIO_SYSSPACE;
		auio.uio_resid = MAXPATHLEN;
		if (error = VOP_READLINK(dp, &auio, ndp->ni_cred)) {
			if (ndp->ni_pathlen > 1)
				free(cp, M_NAMEI);
			goto bad2;
		}
		linklen = MAXPATHLEN - auio.uio_resid;
		if (linklen + ndp->ni_pathlen >= MAXPATHLEN) {
			if (ndp->ni_pathlen > 1)
				free(cp, M_NAMEI);
			error = ENAMETOOLONG;
			goto bad2;
		}
		if (ndp->ni_pathlen > 1) {
			bcopy(ndp->ni_next, cp + linklen, ndp->ni_pathlen);
			FREE(ndp->ni_pnbuf, M_NAMEI);
			ndp->ni_pnbuf = cp;
		} else
			ndp->ni_pnbuf[linklen] = '\0';
		ndp->ni_ptr = cp;
		vput(dp);
		dp = ndp->ni_dvp;
		if (lockparent && ndp->ni_pathlen == 1)
			VOP_UNLOCK(dp);
		ndp->ni_pathlen += linklen;
		goto start;
	}

	/*
	 * Check to see if the vnode has been mounted on;
	 * if so find the root of the mounted file system.
	 */
mntloop:
	while (dp->v_type == VDIR && (mp = dp->v_mountedhere) &&
	       (ndp->ni_nameiop & NOCROSSMOUNT) == 0) {
		while(mp->mnt_flag & MNT_MLOCK) {
			mp->mnt_flag |= MNT_MWAIT;
			sleep((caddr_t)mp, PVFS);
			goto mntloop;
		}
		error = VFS_ROOT(dp->v_mountedhere, &tdp);
		if (error)
			goto bad2;
		vput(dp);
		ndp->ni_vp = dp = tdp;
	}

nextname:
	/*
	 * Not a symbolic link.  If more pathname,
	 * continue at next component, else return.
	 */
	ndp->ni_ptr = ndp->ni_next;
	if (*ndp->ni_ptr == '/') {
		while (*ndp->ni_ptr == '/') {
			ndp->ni_ptr++;
			ndp->ni_pathlen--;
		}
		vrele(ndp->ni_dvp);
		goto dirloop;
	}
	/*
	 * Check for read-only file systems.
	 */
	if (flag == DELETE || flag == RENAME) {
		/*
		 * Disallow directory write attempts on read-only
		 * file systems.
		 */
		if ((dp->v_mount->mnt_flag & rdonly) ||
		    (wantparent && (ndp->ni_dvp->v_mount->mnt_flag & rdonly))) {
			error = EROFS;
			goto bad2;
		}
	}
	if (!wantparent)
		vrele(ndp->ni_dvp);
	if ((ndp->ni_nameiop & LOCKLEAF) == 0)
		VOP_UNLOCK(dp);
	if (getbuf)
		FREE(ndp->ni_pnbuf, M_NAMEI);
	return (0);

bad2:
	if (lockparent && *ndp->ni_next == '\0')
		VOP_UNLOCK(ndp->ni_dvp);
	vrele(ndp->ni_dvp);
bad:
	vput(dp);
	ndp->ni_vp = NULL;
	if (getbuf)
		FREE(ndp->ni_pnbuf, M_NAMEI);
	return (error);
}
