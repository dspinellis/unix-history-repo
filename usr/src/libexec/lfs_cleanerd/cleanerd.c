/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cleanerd.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mount.h>
#include <sys/time.h>

#include <ufs/ufs/dinode.h>
#include <ufs/lfs/lfs.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "clean.h"
char *special = "cleanerd";

struct seglist { 
	int sl_id;	/* segment number */
	int sl_cost; 	/* cleaning cost */
	char sl_empty;	/* is segment empty */
};

struct tossstruct {
	struct lfs *lfs;
	int seg;
};

/* function prototypes for system calls; not sure where they should go */
int	 lfs_segwait __P((fsid_t, struct timeval *));
int	 lfs_segclean __P((fsid_t, u_long));
int	 lfs_bmapv __P((fsid_t, BLOCK_INFO *, int));
int	 lfs_markv __P((fsid_t, BLOCK_INFO *, int, INODE_INFO *, int));

/* function prototypes */
int	 bi_tossold __P((const void *, const void *, const void *));
int	 choose_segments __P((FS_INFO *, struct seglist *, 
	     int (*)(FS_INFO *, SEGUSE *)));
void	 clean_fs __P((FS_INFO	*, int (*)(FS_INFO *, SEGUSE *)));
int	 clean_loop __P((FS_INFO *));
int	 clean_segment __P((FS_INFO *, int));
int	 cost_benefit __P((FS_INFO *, SEGUSE *));
int	 cost_compare __P((const void *, const void *));

/*
 * Cleaning Cost Functions:
 *
 * These return the cost of cleaning a segment.  The higher the cost value
 * the better it is to clean the segment, so empty segments have the highest
 * cost.  (It is probably better to think of this as a priority value
 * instead).
 *
 * This is the cost-benefit policy simulated and described in Rosenblum's
 * 1991 SOSP paper.
 */

int
cost_benefit(fsp, su)
	FS_INFO *fsp;		/* file system information */
	SEGUSE *su;
{
	struct lfs *lfsp;
	struct timeval t;
	int age;
	int live;

	gettimeofday(&t, NULL);

	live = su->su_nbytes;	
	age = t.tv_sec - su->su_lastmod < 0 ? 0 : t.tv_sec - su->su_lastmod;
	
	lfsp = &fsp->fi_lfs;
	if (live == 0) 
		return (t.tv_sec * lblkno(lfsp, seg_size(lfsp)));
	else {
		/* 
		 * from lfsSegUsage.c (Mendel's code).
		 * priority calculation is done using INTEGER arithmetic.
		 * sizes are in BLOCKS (that is why we use lblkno below).
		 * age is in seconds.
		 *
		 * priority = ((seg_size - live) * age) / (seg_size + live) 
		 */
#ifdef VERBOSE
		if (live < 0 || live > seg_size(lfsp)) {
			err(0, "Bad segusage count: %d", live);
			live = 0;
		}
#endif
		return (lblkno(lfsp, seg_size(lfsp) - live) * age)
			/ lblkno(lfsp, seg_size(lfsp) + live);
	}
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	FS_INFO	*lfp, *fsp;
	struct statfs *lstatfsp;	/* file system stats */
	struct timeval timeout;		/* sleep timeout */
	fsid_t fsid;
	int count;			/* number of file systems */
	int i;
	
	count = fs_getmntinfo(&lstatfsp, MOUNT_LFS);

	timeout.tv_sec = 5*60; /* five minutes */
	timeout.tv_usec = 0;
	fsid.val[0] = 0;
	fsid.val[1] = 0;

	for (fsp = get_fs_info(lstatfsp, count); ; reread_fs_info(fsp, count)) {
		for (lfp = fsp, i = 0; i < count; ++lfp, ++i)
			clean_loop(lfp);

#ifdef VERBOSE
		(void)printf("Cleaner going to sleep.\n");
#endif
		if (lfs_segwait(fsid, &timeout) < 0)
			err(0, "lfs_segwait: returned error\n");	
#ifdef VERBOSE
		(void)printf("Cleaner waking up.\n");
#endif
	}
}

/* return the number of segments cleaned */
int
clean_loop(fsp)
	FS_INFO	*fsp;	/* file system information */
{
	double loadavg[MAXLOADS];
	time_t	now;
	u_long max_free_segs;

        /*
	 * Compute the maximum possible number of free segments, given the
	 * number of free blocks.
	 */
	max_free_segs = fsp->fi_statfsp->f_bfree / fsp->fi_lfs.lfs_ssize;
	
	/* 
	 * We will clean if there are not enough free blocks or total clean
	 * space is less than BUSY_LIM % of possible clean space.
	 */
	now = time((time_t *)NULL);
	if (fsp->fi_cip->clean <= MIN_SEGS(&fsp->fi_lfs) ||
	    fsp->fi_cip->clean < max_free_segs * BUSY_LIM) {
		clean_fs(fsp, cost_benefit);
		printf("Cleaner Running  at %s (need space)\n",
		    ctime(&now));
		return (1);
	} else {
	        /* 
		 * We will also clean if the system is reasonably idle and
		 * the total clean space is less then IDLE_LIM % of possible
		 * clean space.
		 */
		if (getloadavg(loadavg, MAXLOADS) == -1) {
			perror("getloadavg: failed\n");
			return (-1);
		}
		if (loadavg[ONE_MIN] == 0.0 && loadavg[FIVE_MIN] &&
		    fsp->fi_cip->clean < max_free_segs * IDLE_LIM) {
		        clean_fs(fsp, cost_benefit);
			printf("Cleaner Running  at %s (system idle)\n",
			    ctime(&now));
			return (1);
		}
	} 
	printf("Cleaner Not Running at %s\n", ctime(&now));
	return (0);
}


void
clean_fs(fsp, cost_func)
	FS_INFO	*fsp;	/* file system information */
	int (*cost_func) __P((FS_INFO *, SEGUSE *));
{
	struct seglist *segs, *sp;
	int i;

	if ((segs = malloc(fsp->fi_lfs.lfs_nseg * sizeof(struct seglist)))
	    == NULL) {
		err(0, "malloc failed");
		return;
	}
	i = choose_segments(fsp, segs, cost_func);
#ifdef VERBOSE
	printf("clean_fs: cleaning %d segments in file system %s\n",
		i, fsp->fi_statfsp->f_mntonname);
	fflush(stdout);
#endif
	if (i)
		for (i = MIN(i, NUM_TO_CLEAN(fsp)), sp = segs; i-- ; ++sp)
			if (clean_segment(fsp, sp->sl_id) < 0)
				perror("clean_segment failed");
			else if (lfs_segclean (fsp->fi_statfsp->f_fsid,
			    sp->sl_id) < 0)
				perror("lfs_segclean failed");
	free(segs);
}

/*
 * Segment with the highest priority get sorted to the beginning of the
 * list.  This sort assumes that empty segments always have a higher
 * cost/benefit than any utilized segment.
 */
int
cost_compare(a, b)
	const void *a;
	const void *b;
{
	return (((struct seglist *)b)->sl_cost -
	    ((struct seglist *)a)->sl_cost);
}


/*
 * Returns the number of segments to be cleaned with the elements of seglist
 * filled in.
 */
int
choose_segments(fsp, seglist, cost_func)
	FS_INFO *fsp;
	struct seglist *seglist;
	int (*cost_func) __P((FS_INFO *, SEGUSE *));
{
	struct lfs *lfsp;
	struct seglist *sp;
	SEGUSE *sup;
	int i, nsegs;

	lfsp = &fsp->fi_lfs;

#ifdef VERBOSE
	(void) printf("Entering choose_segments\n");
#endif
	dump_super(lfsp);
	dump_cleaner_info(fsp->fi_cip);

	for (sp = seglist, i = 0; i < lfsp->lfs_nseg; ++i) {
		sup = SEGUSE_ENTRY(lfsp, fsp->fi_segusep, i);
		 PRINT_SEGUSE(sup, i);
		if (!(sup->su_flags & SEGUSE_DIRTY) ||
		    sup->su_flags & SEGUSE_ACTIVE)
			continue;
#ifdef VERBOSE
		(void) printf("\tchoosing segment %d\n", i);
#endif
		sp->sl_cost = (*cost_func)(fsp, sup);
		sp->sl_id = i;
		sp->sl_empty = sup->su_nbytes ? 0 : 1;
		++sp;
	}
	nsegs = sp - seglist;
	qsort(seglist, nsegs, sizeof(struct seglist), cost_compare);
#ifdef VERBOSE
	(void)printf("Returning %d segments\n", nsegs);
#endif
	return (nsegs);
}


int
clean_segment(fsp, id)
	FS_INFO *fsp;	/* file system information */
	int id;		/* segment number */
{
	BLOCK_INFO *block_array;
	INODE_INFO *inode_array;
	SEGUSE *sp;
	struct lfs *lfsp;
	struct tossstruct t;
	caddr_t seg_buf;
	int num_inodes, num_blocks;

	lfsp = &fsp->fi_lfs;
	sp = SEGUSE_ENTRY(lfsp, fsp->fi_segusep, id);

#ifdef VERBOSE
	(void) printf("cleaning segment %d: contains %lu bytes\n", id,
	    sp->su_nbytes);
	fflush(stdout);
#endif
	/* XXX could add debugging to verify that segment is really empty */
	if (sp->su_nbytes == sp->su_nsums * LFS_SUMMARY_SIZE)
		return (0);

	/* map the segment into a buffer */
	if (mmap_segment(fsp, id, &seg_buf) < 0) {
		err(0, "mmap_segment failed");
		return (-1);
	}
	/* get a list of blocks that are contained by the segment */
	if (lfs_segmapv(fsp, id, seg_buf, &block_array, &num_blocks, 
	    &inode_array, &num_inodes) < 0) {
		err(0, "clean_segment: lfs_segmapv failed");
		return (-1);
	}

#ifdef VERBOSE
	(void) printf("lfs_segmapv returned %d blocks and %d inodes\n",
	    num_blocks, num_inodes);
	fflush (stdout);
#endif

	/* get the current disk address of blocks contained by the segment */
	if (lfs_bmapv(fsp->fi_statfsp->f_fsid, block_array, num_blocks) < 0) {
		perror("clean_segment: lfs_bmapv failed\n");
		return -1;
	}

	/* Now toss any blocks not in the current segment */
	t.lfs = lfsp;
	t.seg = id;
	toss(block_array, &num_blocks, sizeof(BLOCK_INFO), bi_tossold, &t);

	/* Check if last element should be tossed */
	if (num_blocks && bi_tossold(&t, block_array + num_blocks - 1, NULL))
		--num_blocks;

#ifdef VERBOSE
	{
		BLOCK_INFO *_bip;
		INODE_INFO *_iip;
		u_long *lp;
		int i;

		(void) printf("after bmapv still have %d blocks\n", num_blocks);
		fflush (stdout);
		if (num_blocks)
			printf("BLOCK INFOS\n");
		for (_bip = block_array, i=0; i < num_blocks; ++_bip, ++i) {
			PRINT_BINFO(_bip);
			lp = (u_long *)_bip->bi_bp;
		}
		if (num_inodes)
			printf("INODE INFOS\n");
		for (_iip = inode_array, i=0; i < num_inodes; ++_iip, ++i)
			PRINT_IINFO(1, _iip);
	}
#endif
	/* rewrite the live data */
	if (num_blocks > 0 || num_inodes > 0)
		if (lfs_markv(fsp->fi_statfsp->f_fsid, block_array, num_blocks,
		    inode_array, num_inodes) < 0) {
			err(0, "clean_segment: lfs_bmapv failed");
			return (-1);
		}
	free(block_array);
	free(inode_array);
	munmap_segment(fsp, seg_buf);

	return (0);
}


int
bi_tossold(client, a, b)
	const void *client;
	const void *a;
	const void *b;
{
	const struct tossstruct *t;

	t = (struct tossstruct *)client;

	return (((BLOCK_INFO *)a)->bi_daddr == LFS_UNUSED_DADDR ||
	    datosn(t->lfs, ((BLOCK_INFO *)a)->bi_daddr) != t->seg);
}
