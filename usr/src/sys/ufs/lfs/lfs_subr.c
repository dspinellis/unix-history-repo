/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_subr.c	7.6 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/namei.h>
#include <sys/vnode.h>
#include <sys/buf.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/*
 * Return buffer with the contents of block "offset" from the beginning of
 * directory "ip".  If "res" is non-zero, fill it in with a pointer to the
 * remaining space in the directory.
 */
int
lfs_blkatoff(vp, offset, res, bpp)
	struct vnode *vp;
	off_t offset;
	char **res;
	struct buf **bpp;
{
	register struct lfs *fs;
	struct inode *ip;
	struct buf *bp;
	daddr_t lbn;
	int bsize, error;

	ip = VTOI(vp);
	fs = ip->i_lfs;
	lbn = lblkno(fs, offset);
	bsize = blksize(fs);

	*bpp = NULL;
	if (error = bread(vp, lbn, bsize, NOCRED, &bp)) {
		brelse(bp);
		return (error);
	}
	if (res)
		*res = bp->b_un.b_addr + blkoff(fs, offset);
	*bpp = bp;
	return (0);
}

/* Search a block for a specific dinode. */
DINODE *
lfs_ifind(fs, ino, page)
	struct lfs *fs;
	ino_t ino;
	void *page;
{
	register DINODE *dip;
	register int cnt;

#ifdef VERBOSE
	printf("lfs_ifind: inode %d\n", ino);
#endif
	dip = page;
	for (cnt = INOPB(fs); cnt--; ++dip)
		if (dip->di_inum == ino)
			return (dip);

	panic("lfs_ifind: dinode %u not found", ino);
	/* NOTREACHED */
}

daddr_t
lfs_itod(fs, ino)
	struct lfs *fs;
	ino_t ino;
{
	BUF *bp;
	IFILE *ifp;
	daddr_t iaddr;

#ifdef VERBOSE
	printf("lfs_itod %d\n", ino);
#endif
	/* Translate an inode number to a disk address. */
	LFS_IENTRY(ifp, fs, ino, bp);
	if (ifp->if_daddr == LFS_UNUSED_DADDR)
		panic("lfs_itod: unused disk address");
	iaddr = ifp->if_daddr;
	brelse(bp);
	return (iaddr);
}
