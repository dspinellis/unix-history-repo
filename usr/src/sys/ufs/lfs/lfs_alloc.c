/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_alloc.c	7.50 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/kernel.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/syslog.h>
#include <sys/mount.h>
#include <sys/malloc.h>

#include <vm/vm.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/ufsmount.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

extern u_long nextgennumber;

/* Allocate a new inode. */
/* ARGSUSED */
int
lfs_valloc(ap)
	struct vop_valloc_args /* {
		struct vnode *a_pvp;
		int a_mode;
		struct ucred *a_cred;
		struct vnode **a_vpp;
	} */ *ap;
{
	struct lfs *fs;
	struct buf *bp;
	struct ifile *ifp;
	struct inode *ip;
	struct vnode *vp;
	daddr_t blkno;
	ino_t new_ino;
	u_long i, max;
	int error;

#ifdef VERBOSE
	printf("lfs_valloc\n");
#endif
	/* Get the head of the freelist. */
	fs = VTOI(ap->a_pvp)->i_lfs;
	new_ino = fs->lfs_free;
#ifdef ALLOCPRINT
	printf("lfs_ialloc: allocate inode %d\n", new_ino);
#endif

	/*
	 * Remove the inode from the free list and write the new start
	 * of the free list into the superblock.
	 */
	LFS_IENTRY(ifp, fs, new_ino, bp);
	if (ifp->if_daddr != LFS_UNUSED_DADDR)
		panic("lfs_ialloc: inuse inode on the free list");
	fs->lfs_free = ifp->if_nextfree;
	brelse(bp);

	/* Extend IFILE so that the next lfs_valloc will succeed. */
	if (fs->lfs_free == LFS_UNUSED_INUM) {
		vp = fs->lfs_ivnode;
		ip = VTOI(vp);
		blkno = lblkno(fs, ip->i_size);
		bp = getblk(vp, blkno, fs->lfs_bsize);
		if (!bp) {
			uprintf("\n%s: no inodes left\n", fs->lfs_fsmnt);
			log(LOG_ERR, "uid %d on %s: out of inodes\n",
			    ap->a_cred->cr_uid, fs->lfs_fsmnt);
			return (ENOSPC);
		}
		i = (blkno - fs->lfs_segtabsz - fs->lfs_cleansz) *
		    fs->lfs_ifpb;
		fs->lfs_free = i;
		max = i + fs->lfs_ifpb;
		for (ifp = (struct ifile *)bp->b_un.b_words; i < max; ++ifp) {
			ifp->if_version = 1;
			ifp->if_daddr = LFS_UNUSED_DADDR;
			ifp->if_nextfree = ++i;
		}
		ifp--;
		ifp->if_nextfree = LFS_UNUSED_INUM;

		ip->i_blocks += btodb(fs->lfs_bsize);
		fs->lfs_bfree -= btodb(fs->lfs_bsize);
		ip->i_size += fs->lfs_bsize;
		vnode_pager_setsize(vp, (u_long)ip->i_size);
		vnode_pager_uncache(vp);
		LFS_UBWRITE(bp);
	}

	/* Create a vnode to associate with the inode. */
	if (error = lfs_vcreate(ap->a_pvp->v_mount, new_ino, &vp))
		return (error);
	*ap->a_vpp = vp;
	vp->v_flag |= VDIROP;
	ip = VTOI(vp);
	VREF(ip->i_devvp);

	/* Zero out the direct and indirect block addresses. */
	bzero(ip->i_db, (NDADDR + NIADDR) * sizeof(daddr_t));

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
	struct mount *mp;
	ino_t ino;
	struct vnode **vpp;
{
	extern int (**lfs_vnodeop_p)();
	struct inode *ip;
	struct ufsmount *ump;
	int error, i;

#ifdef VERBOSE
	printf("lfs_vcreate: ino %d\n", ino);
#endif
	/* Create the vnode. */
	if (error = getnewvnode(VT_LFS, mp, lfs_vnodeop_p, vpp)) {
		*vpp = NULL;
		return (error);
	}

	/* Get a pointer to the private mount structure. */
	ump = VFSTOUFS(mp);

	/* Initialize the inode. */
	MALLOC(ip, struct inode *, sizeof(struct inode), M_LFSNODE, M_WAITOK);
	(*vpp)->v_data = ip;
	ip->i_vnode = *vpp;
	ip->i_devvp = ump->um_devvp;
	ip->i_flag = 0;
	ip->i_dev = ump->um_dev;
	ip->i_number = ip->i_din.di_inum = ino;
	ip->i_lfs = ump->um_lfs;
#ifdef QUOTA
	for (i = 0; i < MAXQUOTAS; i++)
		ip->i_dquot[i] = NODQUOT;
#endif
	ip->i_lockf = 0;
	ip->i_diroff = 0;
	ip->i_mode = 0;
	ip->i_size = 0;
	ip->i_blocks = 0;
	return (0);
}

/* Free an inode. */
/* ARGUSED */
int
lfs_vfree(ap)
	struct vop_vfree_args /* {
		struct vnode *a_pvp;
		ino_t a_ino;
		int a_mode;
	} */ *ap;
{
	SEGUSE *sup;
	struct buf *bp;
	struct ifile *ifp;
	struct inode *ip;
	struct lfs *fs;
	daddr_t old_iaddr;
	ino_t ino;

	ip = VTOI(ap->a_pvp);
#ifdef VERBOSE
	printf("lfs_vfree: free %d\n", ip->i_number);
#endif
	/* Get the inode number and file system. */
	fs = ip->i_lfs;
	ino = ip->i_number;

	/*
	 * Set the ifile's inode entry to unused, increment its version number
	 * and link it into the free chain.
	 */
	LFS_IENTRY(ifp, fs, ino, bp);
	old_iaddr = ifp->if_daddr;
	ifp->if_daddr = LFS_UNUSED_DADDR;
	++ifp->if_version;
	ifp->if_nextfree = fs->lfs_free;
	fs->lfs_free = ino;
	LFS_UBWRITE(bp);

	if (old_iaddr != LFS_UNUSED_DADDR) {
		LFS_SEGENTRY(sup, fs, datosn(fs, old_iaddr), bp);
#ifdef DIAGNOSTIC
		if (sup->su_nbytes < sizeof(struct dinode))
			panic("lfs_vfree: negative byte count (segment %d)\n",
			    datosn(fs, old_iaddr));
#endif
		sup->su_nbytes -= sizeof(struct dinode);
		LFS_UBWRITE(bp);
	}

	/* Set superblock modified bit and decrement file count. */
	fs->lfs_fmod = 1;
	--fs->lfs_nfiles;
	return (0);
}
