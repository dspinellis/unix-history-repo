/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_debug.c	7.4 (Berkeley) %G%
 */

#ifdef DEBUG
#include <sys/param.h>
#include <sys/namei.h>
#include <sys/vnode.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

void 
lfs_dump_super(lfsp)
	struct lfs *lfsp;
{
	int i;

	(void)printf("%s%lx\t%s%lx\t%s%d\t%s%d\n",
		"magic    ", lfsp->lfs_magic,
		"version  ", lfsp->lfs_version,
		"size     ", lfsp->lfs_size,
		"ssize    ", lfsp->lfs_ssize);
	(void)printf("%s%d\t%s%d\t%s%d\t%s%d\n",
		"dsize    ", lfsp->lfs_dsize,
		"bsize    ", lfsp->lfs_bsize,
		"fsize    ", lfsp->lfs_fsize,
		"frag     ", lfsp->lfs_frag);

	(void)printf("%s%d\t%s%d\t%s%d\t%s%d\n",
		"minfree  ", lfsp->lfs_minfree,
		"inopb    ", lfsp->lfs_inopb,
		"ifpb     ", lfsp->lfs_ifpb,
		"nindir   ", lfsp->lfs_nindir);

	(void)printf("%s%d\t%s%d\t%s%d\n",
		"nseg     ", lfsp->lfs_nseg,
		"nspf     ", lfsp->lfs_nspf,
		"segtabsz ", lfsp->lfs_segtabsz);

	(void)printf("%s%lx\t%s%d\t%s%lx\t%s%d\n",
		"segmask  ", lfsp->lfs_segmask,
		"segshift ", lfsp->lfs_segshift,
		"bmask    ", lfsp->lfs_bmask,
		"bshift   ", lfsp->lfs_bshift);

	(void)printf("%s%lx\t%s%d\t%s%lx\t%s%d\n",
		"ffmask   ", lfsp->lfs_ffmask,
		"ffshift  ", lfsp->lfs_ffshift,
		"fbmask   ", lfsp->lfs_fbmask,
		"fbshift  ", lfsp->lfs_fbshift);

	(void)printf("%s%d\t%s%lx\n", 
		"fsbtodb  ", lfsp->lfs_fsbtodb,
		"cksum    ", lfsp->lfs_cksum);

	(void)printf("Superblock disk addresses:");
	for (i = 0; i < LFS_MAXNUMSB; i++)
		(void)printf(" %lx", lfsp->lfs_sboffs[i]);
	(void)printf("\n");

	(void)printf("Checkpoint Info\n");
	(void)printf("%s%d\t%s%lx\t%s%d\n",
		"free     ", lfsp->lfs_free,
		"idaddr   ", lfsp->lfs_idaddr,
		"ifile    ", lfsp->lfs_ifile);
	(void)printf("%s%lx\t%s%d\t%s%lx\t%s%lx\n",
		"bfree    ", lfsp->lfs_bfree,
		"nfiles   ", lfsp->lfs_nfiles,
		"lastseg  ", lfsp->lfs_lastseg,
		"nextseg  ", lfsp->lfs_nextseg);
	(void)printf("tstamp   %lx\n", lfsp->lfs_tstamp);
}

void
lfs_dump_dinode(dip)
	DINODE *dip;
{
	int i;

	(void)printf("%s%u\t%s%d\t%s%u\t%s%u\t%s%lu\n",
		"mode  ", dip->di_mode,
		"nlink ", dip->di_nlink,
		"uid   ", dip->di_uid,
		"gid   ", dip->di_gid,
		"size  ", dip->di_size);
	(void)printf("inum  %ld\n", dip->di_inum);
	(void)printf("Direct Addresses\n");
	for (i = 0; i < NDADDR; i++) {
		(void)printf("\t%lx", dip->di_db[i]);
		if ((i % 6) == 5)
			(void)printf("\n");
	}
	for (i = 0; i < NIADDR; i++)
		(void)printf("\t%lx", dip->di_ib[i]);
	(void)printf("\n");
}

/* XXX TEMPORARY */
#include <sys/buf.h>
#include <sys/mount.h>
int
lfs_umountdebug(mp)
	struct mount *mp;
{
	struct vnode *vp;
	int dirty;

	dirty = 0;
	if ((mp->mnt_flag & MNT_MPBUSY) == 0)
		panic("umountdebug: not busy");
loop:
	for (vp = mp->mnt_mounth; vp; vp = vp->v_mountf) {
		if (vget(vp))
			goto loop;
		dirty += lfs_vinvalbuf(vp);
		vput(vp);
		if (vp->v_mount != mp)
			goto loop;
	}
	return (dirty);
}

int
lfs_vinvalbuf(vp)
	register struct vnode *vp;
{
	register struct buf *bp;
	struct buf *nbp, *blist;
	int s, dirty = 0;

	for (;;) {
		if (blist = vp->v_dirtyblkhd)
			/* void */;
		else if (blist = vp->v_cleanblkhd)
			/* void */;
		else
			break;
		for (bp = blist; bp; bp = nbp) {
	printf("lfs_vinvalbuf: ino %d, lblkno %d, blkno %lx flags %xl\n",
	     VTOI(vp)->i_number, bp->b_lblkno, bp->b_blkno, bp->b_flags);
			nbp = bp->b_blockf;
			s = splbio();
			if (bp->b_flags & B_BUSY) {
	printf("lfs_vinvalbuf: buffer busy, would normally sleep\n");
/*
				bp->b_flags |= B_WANTED;
				sleep((caddr_t)bp, PRIBIO + 1);
*/
				splx(s);
				break;
			}
			bremfree(bp);
			bp->b_flags |= B_BUSY;
			splx(s);
			if (bp->b_flags & B_DELWRI) {
				dirty++;			/* XXX */
	printf("lfs_vinvalbuf: buffer dirty (DELWRI). would normally write\n");
				break;
			}
			if (bp->b_vp != vp)
				reassignbuf(bp, bp->b_vp);
			else
				bp->b_flags |= B_INVAL;
			brelse(bp);
		}
	}
	if (vp->v_dirtyblkhd || vp->v_cleanblkhd)
		panic("lfs_vinvalbuf: flush failed");
	return (dirty);
}
#endif /* DEBUG */
