/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_alloc.c	7.56 (Berkeley) %G%
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
	int bb, error;

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
		lfs_balloc(vp, fs->lfs_bsize, blkno, &bp);
		ip->i_size += fs->lfs_bsize;
		vnode_pager_setsize(vp, (u_long)ip->i_size);
		vnode_pager_uncache(vp);

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
		if (error = VOP_BWRITE(bp))
			return (error);
	}

	/* Create a vnode to associate with the inode. */
	if (error = lfs_vcreate(ap->a_pvp->v_mount, new_ino, &vp))
		return (error);


	ip = VTOI(vp);
	/* Zero out the direct and indirect block addresses. */
	bzero(&ip->i_din, sizeof(struct dinode));
	ip->i_din.di_inumber = new_ino;

	/* Set a new generation number for this inode. */
	if (++nextgennumber < (u_long)time.tv_sec)
		nextgennumber = time.tv_sec;
	ip->i_gen = nextgennumber;

	/* Insert into the inode hash table. */
	ufs_ihashins(ip);

	if (error = ufs_vinit(vp->v_mount, lfs_specop_p, LFS_FIFOOPS, &vp)) {
		vput(vp);
		*ap->a_vpp = NULL;
		return (error);
	}

	*ap->a_vpp = vp;
	vp->v_flag |= VDIROP;
	VREF(ip->i_devvp);

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
	ip->i_flag = IMOD;
	ip->i_dev = ump->um_dev;
	ip->i_number = ip->i_din.di_inumber = ino;
ip->i_din.di_spare[0] = 0xdeadbeef;
ip->i_din.di_spare[1] = 0xdeadbeef;
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
	++ump->um_lfs->lfs_uinodes;
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
	int error;

	/* Get the inode number and file system. */
	ip = VTOI(ap->a_pvp);
	fs = ip->i_lfs;
	ino = ip->i_number;
	if (ip->i_flag & IMOD) {
		--fs->lfs_uinodes;
		ip->i_flag &= ~(IMOD | IACC | IUPD | ICHG);
	}
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
	(void) VOP_BWRITE(bp);

	if (old_iaddr != LFS_UNUSED_DADDR) {
		LFS_SEGENTRY(sup, fs, datosn(fs, old_iaddr), bp);
#ifdef DIAGNOSTIC
		if (sup->su_nbytes < sizeof(struct dinode))
			panic("lfs_vfree: negative byte count (segment %d)\n",
			    datosn(fs, old_iaddr));
#endif
		sup->su_nbytes -= sizeof(struct dinode);
		(void) VOP_BWRITE(bp);
	}

	/* Set superblock modified bit and decrement file count. */
	fs->lfs_fmod = 1;
	--fs->lfs_nfiles;
	return (0);
}
