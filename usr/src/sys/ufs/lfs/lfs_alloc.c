/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_alloc.c	7.30 (Berkeley) %G%
 */

#ifdef LOGFS
#include "param.h"
#include "kernel.h"
#include "buf.h"
#include "vnode.h"
#include "syslog.h"
#include "mount.h"
#include "../ufs/quota.h"
#include "../ufs/inode.h"
#include "../ufs/ufsmount.h"
#include "lfs.h"
#include "lfs_extern.h"

/* Read in the block containing a specific inode from the ifile. */
#define	LFS_IENTRY(I, F, IN, BP) \
	if (bread((F)->lfs_ivnode, (IN) / IFPB(F) + (F)->lfs_segtabsz, \
	    (F)->lfs_bsize, NOCRED, &BP)) \
		panic("lfs_ientry: read"); \
	(I) = (IFILE *)BP->b_un.b_addr + IN % IFPB(F);

/*
 * Allocate a new inode.
 */
ino_t
lfs_ialloc(fs, pip, ipp, cred)
	LFS *fs;
	INODE *pip, **ipp;
	UCRED *cred;
{
	BUF *bp;
	IFILE *ifp;
	INODE *ip;
	VNODE *vp;
	ino_t new_ino;
	int error;

	/* Get the head of the freelist. */
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
printf("lfs_ialloc: allocate inode %d\n", new_ino);

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
	lfs_hqueue(ip);

	/* Set superblock modified bit and increment file count. */
	fs->lfs_fmod = 1;
	++fs->lfs_nfiles;
	return (0);
}

/* Free an inode. */
void
lfs_ifree(ip)
	INODE *ip;
{
	BUF *bp;
	IFILE *ifp;
	LFS *fs;
	ino_t ino;

printf("lfs_ifree: free %d\n", ip->i_number);
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

/* Translate an inode number to a disk address. */
daddr_t
itod(fs, ino)
	LFS *fs;
	ino_t ino;
{
	BUF *bp;
	IFILE *ifp;
	daddr_t iaddr;

	/* Read the appropriate block from the ifile. */
	LFS_IENTRY(ifp, fs, ino, bp);

	if (ifp->if_daddr == LFS_UNUSED_DADDR)
		panic("itod: unused disk address");
	iaddr = ifp->if_daddr;
	brelse(bp);
	return (iaddr);
}

/* Search a block for a specific dinode. */
DINODE *
lfs_ifind(fs, ino, page)
	LFS *fs;
	ino_t ino;
	void *page;
{
	register DINODE *dip;
	register int cnt;

printf("lfs_ifind: inode %d\n", ino);
	dip = page;
	for (cnt = INOPB(fs); cnt--; ++dip)
		if (dip->di_inum == ino)
			return (dip);

	panic("lfs_ifind: dinode %%u not found", ino);
	/* NOTREACHED */
}

/* Create a new vnode/inode pair and initialize what fields we can. */
lfs_vcreate(mp, ino, vpp)
	MOUNT *mp;
	ino_t ino;
	VNODE **vpp;
{
	INODE *ip;
	UFSMOUNT *ump;
	int error, i;

printf("lfs_vcreate: ino %d\n", ino);
	/* Create the vnode. */
	if (error = getnewvnode(VT_LFS, mp, &lfs_vnodeops, vpp))
		return(error);

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

/* Return the current version number for a specific inode. */
u_long
lfs_getversion(fs, ino)
	LFS *fs;
	ino_t ino;
{
	BUF *bp;
	IFILE *ifp;
	u_long version;

	/*
	 * Read the appropriate block from the ifile.  Return the version
	 * number.
	 */
	LFS_IENTRY(ifp, fs, ino, bp);
	version = ifp->if_version;
	brelse(bp);
	return(version);
}

/* Set values in the ifile for the inode. */
void
lfs_iset(ip, daddr, atime)
	INODE *ip;
	daddr_t daddr;
	time_t atime;
{
	BUF *bp;
	IFILE *ifp;
	LFS *fs;
	ino_t ino;

printf("lfs_iset: setting ino %d daddr %lx time %lx\n", ip->i_number, daddr, atime);

	fs = ip->i_lfs;
	ino = ip->i_number;
	LFS_IENTRY(ifp, fs, ino, bp);

	ifp->if_daddr = daddr;
	ifp->if_st_atime = atime;
	lfs_bwrite(bp);
}
#endif /* LOGFS */
