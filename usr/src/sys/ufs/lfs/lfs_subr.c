/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_subr.c	7.14 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/namei.h>
#include <sys/vnode.h>
#include <sys/buf.h>
#include <sys/mount.h>
#include <sys/malloc.h>
#include <sys/proc.h>

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
lfs_blkatoff(ap)
	struct vop_blkatoff_args /* {
		struct vnode *a_vp;
		off_t a_offset;
		char **a_res;
		struct buf **a_bpp;
	} */ *ap;
{
	register struct lfs *fs;
	struct inode *ip;
	struct buf *bp;
	daddr_t lbn;
	int bsize, error;

	ip = VTOI(ap->a_vp);
	fs = ip->i_lfs;
	lbn = lblkno(fs, ap->a_offset);
	bsize = blksize(fs);

	*ap->a_bpp = NULL;
	if (error = bread(ap->a_vp, lbn, bsize, NOCRED, &bp)) {
		brelse(bp);
		return (error);
	}
	if (ap->a_res)
		*ap->a_res = bp->b_un.b_addr + blkoff(fs, ap->a_offset);
	*ap->a_bpp = bp;
	return (0);
}


/*
 * lfs_seglock --
 *	Single thread the segment writer.
 */
void
lfs_seglock(fs, flags)
	struct lfs *fs;
	unsigned long flags;
{
	struct segment *sp;
	int s;

	if (fs->lfs_seglock)
		if (fs->lfs_lockpid == curproc->p_pid) {
			++fs->lfs_seglock;
			fs->lfs_sp->seg_flags |= flags;
			return;			
		} else while (fs->lfs_seglock)
			(void)tsleep(&fs->lfs_seglock, PRIBIO + 1,
			    "lfs seglock", 0);

	fs->lfs_seglock = 1;
	fs->lfs_lockpid = curproc->p_pid;

	sp = fs->lfs_sp = malloc(sizeof(struct segment), M_SEGMENT, M_WAITOK);
	sp->bpp = malloc(((LFS_SUMMARY_SIZE - sizeof(SEGSUM)) /
	    sizeof(daddr_t) + 1) * sizeof(struct buf *), M_SEGMENT, M_WAITOK);
	sp->seg_flags = flags;
	sp->vp = NULL;
	(void) lfs_initseg(fs);

	/*
	 * Keep a cumulative count of the outstanding I/O operations.  If the
	 * disk drive catches up with us it could go to zero before we finish,
	 * so we artificially increment it by one until we've scheduled all of
	 * the writes we intend to do.
	 */
	s = splbio();
	++fs->lfs_iocount;
	splx(s);
}
/*
 * lfs_segunlock --
 *	Single thread the segment writer.
 */
void
lfs_segunlock(fs)
	struct lfs *fs;
{
	struct segment *sp;
	unsigned long sync, ckp;
	int s;

	if (fs->lfs_seglock == 1) {

		sp = fs->lfs_sp;
		sync = sp->seg_flags & SEGM_SYNC;
		ckp = sp->seg_flags & SEGM_CKP;
		if (sp->bpp != sp->cbpp) {
			/* Free allocated segment summary */
			fs->lfs_offset -= LFS_SUMMARY_SIZE / DEV_BSIZE;
			brelvp(*sp->bpp);
			free((*sp->bpp)->b_un.b_addr, M_SEGMENT);
			free(*sp->bpp, M_SEGMENT);
		} else
			printf ("unlock to 0 with no summary");
		free(sp->bpp, M_SEGMENT);
		free(sp, M_SEGMENT);

		/*
		 * If the I/O count is non-zero, sleep until it reaches zero.
		 * At the moment, the user's process hangs around so we can
		 * sleep.
		 */
		s = splbio();
		--fs->lfs_iocount;
		/*
		 * We let checkpoints happen asynchronously.  That means
		 * that during recovery, we have to roll forward between
		 * the two segments described by the first and second
		 * superblocks to make sure that the checkpoint described
		 * by a superblock completed.
		 */
		if (sync && fs->lfs_iocount)
		    (void)tsleep(&fs->lfs_iocount, PRIBIO + 1, "lfs vflush", 0);
		splx(s);
		if (ckp) {
			fs->lfs_nactive = 0;
			lfs_writesuper(fs);
		}
		--fs->lfs_seglock;
		fs->lfs_lockpid = 0;
		wakeup(&fs->lfs_seglock);
	} else if (fs->lfs_seglock == 0) {
		panic ("Seglock not held");
	} else {
		--fs->lfs_seglock;
	}
}
