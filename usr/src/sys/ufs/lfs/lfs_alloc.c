/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_alloc.c	7.29 (Berkeley) %G%
 */

#include "param.h"
#include "kernel.h"
#include "buf.h"
#include "vnode.h"
#include "syslog.h"
#include "../ufs/quota.h"
#include "../ufs/inode.h"
#include "mount.h"
#include "../ufs/ufsmount.h"
#include "lfs.h"
#include "lfs_extern.h"

#define	LFS_IENTRY(I, F, IN, BP) \
	if (bread((F)->lfs_ivnode, (IN) / IFPB(F) + (F)->lfs_segtabsz, \
	    (F)->lfs_bsize, NOCRED, &BP)) \
		panic("ifile read"); \
	(I) = (IFILE *)BP->b_un.b_addr + IN % IFPB(F);

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

	new_ino = fs->lfs_free;
printf("lfs_ialloc: next free %d\n", new_ino);
	if (new_ino == LFS_UNUSED_INUM) {	/* XXX -- allocate more */
		uprintf("\n%s: no inodes left\n", fs->lfs_fsmnt);
		log(LOG_ERR, "uid %d on %s: out of inodes\n",
		    cred->cr_uid, fs->lfs_fsmnt);
		return (ENOSPC);
	}

	/* Read the appropriate block from the ifile */
	vp = fs->lfs_ivnode;
	LFS_IENTRY(ifp, fs, new_ino, bp);

	if (ifp->if_daddr != LFS_UNUSED_DADDR)
		panic("lfs_ialloc: corrupt free list");

	/* Remove from free list, set the access time. */
	fs->lfs_free = ifp->if_nextfree;
	ifp->if_st_atime = time.tv_sec;
	brelse(bp);

	error = lfs_vcreate(ITOV(pip)->v_mount, new_ino, &vp);
	if (error) 
		return (error);

	ip = VTOI(vp);
	VREF(ip->i_devvp);

	/*
	 * Set up a new generation number for this inode.
	 */
	if (++nextgennumber < (u_long)time.tv_sec)
		nextgennumber = time.tv_sec;
	ip->i_gen = nextgennumber;

	lfs_hqueue(ip);

	*ipp = ip;
	return (0);
}

void
lfs_ifree(ip)
	INODE *ip;
{
	BUF *bp;
	IFILE *ifp;
	LFS *fs;
	ino_t ino;

printf("lfs_ifree: free %d\n", ip->i_number);
	fs = ip->i_lfs;
	ino = ip->i_number;
	LFS_IENTRY(ifp, fs, ino, bp);

	ifp->if_daddr = LFS_UNUSED_DADDR;
	++ifp->if_version;
	ifp->if_nextfree = fs->lfs_free;
	brelse(bp);
	fs->lfs_free = ino;
	fs->lfs_fmod = 1;
}

daddr_t
itod(fs, ino)
	LFS *fs;
	ino_t ino;
{
	BUF *bp;
	IFILE *ifp;
	daddr_t iaddr;

printf("itod: ino %d\n", ino);
	LFS_IENTRY(ifp, fs, ino, bp);

	if (ifp->if_daddr == LFS_UNUSED_DADDR)
		panic("itod: unused daddr");
	iaddr = ifp->if_daddr;
	brelse(bp);
	return (iaddr);
}

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

	(void)printf("lfs_ifind: dinode %u not found", ino);
	panic("lfs_ifind: inode not found");
	/* NOTREACHED */
}

/*
 * Create a new vnode/inode and initialize the fields we can.
 */
lfs_vcreate(mp, ino, vpp)
	MOUNT *mp;
	ino_t ino;
	VNODE **vpp;
{
	INODE *ip;
	UFSMOUNT *ump;
	int error, i;

printf("lfs_vcreate: ino %d\n", ino);
	error = getnewvnode(VT_LFS, mp, &lfs_vnodeops, vpp);
	if (error)
		return(error);

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
	return (0);
}

/* 
 * Return the current version number for a specific inode.
 */
u_long
lfs_getversion(fs, ino)
	LFS *fs;
	ino_t ino;
{
	IFILE *ifp;
	BUF *bp;
	int version;

printf("lfs_getversion: %d\n", ino);
	LFS_IENTRY(ifp, fs, ino, bp);
	version = ifp->if_version;
	brelse(bp);
	return(version);
}
