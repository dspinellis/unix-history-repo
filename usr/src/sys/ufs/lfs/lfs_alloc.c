/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_alloc.c	7.33 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/syslog.h>
#include <sys/mount.h>

#include <ufs/quota.h>
#include <ufs/inode.h>
#include <ufs/ufsmount.h>

#include <lfs/lfs.h>
#include <lfs/lfs_extern.h>

/* Allocate a new inode. */
/* ARGSUSED */
int
lfs_ialloc(pip, notused, cred, ipp)
	INODE *pip, **ipp;
	int notused;
	UCRED *cred;
{
	LFS *fs;
	BUF *bp;
	IFILE *ifp;
	INODE *ip;
	VNODE *vp;
	ino_t new_ino;
	int error;

	/* Get the head of the freelist. */
	fs = pip->i_lfs;
	new_ino = fs->lfs_free;
	if (new_ino == LFS_UNUSED_INUM) {
		/*
		 * XXX
		 * Currently, no more inodes are allocated if the ifile fills
		 * up.  The ifile should be extended instead.
		 */
		uprintf("\n%s: no inodes left\n", fs->lfs_fsmnt);
		log(LOG_ERR, "uid %d on %s: out of inodes\n",
		    cred->cr_uid, fs->lfs_fsmnt);
		return (ENOSPC);
	}
#ifdef ALLOCPRINT
	printf("lfs_ialloc: allocate inode %d\n", new_ino);
#endif

	/* Read the appropriate block from the ifile. */
	LFS_IENTRY(ifp, fs, new_ino, bp);

	if (ifp->if_daddr != LFS_UNUSED_DADDR)
		panic("lfs_ialloc: inuse inode on the free list");

	/* Remove from the free list, set the access time, write it back. */
	fs->lfs_free = ifp->if_nextfree;
	ifp->if_st_atime = time.tv_sec;
	lfs_bwrite(bp);

	/* Create a vnode to associate with the inode. */
	error = lfs_vcreate(ITOV(pip)->v_mount, new_ino, &vp);
	if (error) 
		return (error);
	*ipp = ip = VTOI(vp);

	/* Set a new generation number for this inode. */
	if (++nextgennumber < (u_long)time.tv_sec)
		nextgennumber = time.tv_sec;
	ip->i_gen = nextgennumber;

	/* Insert into the inode hash table. */
	ufs_ihashins(ip);

	/* Set superblock modified bit and increment file count. */
	fs->lfs_fmod = 1;
	++fs->lfs_nfiles;
	return (0);
}

/* Create a new vnode/inode pair and initialize what fields we can. */
int
lfs_vcreate(mp, ino, vpp)
	MOUNT *mp;
	ino_t ino;
	VNODE **vpp;
{
	INODE *ip;
	UFSMOUNT *ump;
	int error, i;

#ifdef ALLOCPRINT
	printf("lfs_vcreate: ino %d\n", ino);
#endif
	/* Create the vnode. */
	if (error = getnewvnode(VT_LFS, mp, &lfs_vnodeops, vpp))
		return (error);

	/* Get a pointer to the private mount structure. */
	ump = VFSTOUFS(mp);

	/* Initialize the inode. */
	ip = VTOI(*vpp);
	ip->i_diroff = 0;
	ip->i_devvp = ump->um_devvp;
	ip->i_dev = ump->um_dev;
	ip->i_flag = 0;
	ip->i_lfs = ump->um_lfs;
	ip->i_lockf = 0;
	ip->i_mode = 0;
	ip->i_number = ip->i_din.di_inum = ino;
	ip->i_vnode = *vpp;
#ifdef QUOTA
	for (i = 0; i < MAXQUOTAS; i++)
		ip->i_dquot[i] = NODQUOT;
#endif
	VREF(ip->i_devvp);			/* XXX: Why? */
	return (0);
}

/* Free an inode. */
/* ARGUSED */
void
lfs_ifree(ip, notused1, notused2)
	INODE *ip;
	ino_t notused1;
	int notused2;
{
	BUF *bp;
	IFILE *ifp;
	LFS *fs;
	ino_t ino;

#ifdef ALLOCPRINT
	printf("lfs_ifree: free %d\n", ip->i_number);
#endif
	/* Get the inode number and file system. */
	fs = ip->i_lfs;
	ino = ip->i_number;

	/*
	 * Read the appropriate block from the ifile.  Set the inode entry to
	 * unused, increment its version number and link it into the free chain.
	 */
	LFS_IENTRY(ifp, fs, ino, bp);
	ifp->if_daddr = LFS_UNUSED_DADDR;
	++ifp->if_version;
	ifp->if_nextfree = fs->lfs_free;
	fs->lfs_free = ino;

	lfs_bwrite(bp);

	/* Set superblock modified bit and decrement file count. */
	fs->lfs_fmod = 1;
	--fs->lfs_nfiles;
}
