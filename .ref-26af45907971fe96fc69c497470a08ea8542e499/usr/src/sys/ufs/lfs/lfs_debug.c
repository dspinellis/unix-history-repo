/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_debug.c	7.9 (Berkeley) %G%
 */

#ifdef DEBUG
#include <sys/param.h>
#include <sys/namei.h>
#include <sys/vnode.h>
#include <sys/mount.h>

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

	(void)printf("%s%d\t%s%d\t%s%d\t%s%d\n",
		"nseg     ", lfsp->lfs_nseg,
		"nspf     ", lfsp->lfs_nspf,
		"cleansz  ", lfsp->lfs_cleansz,
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

	(void)printf("%s%d\t%s%d\t%s%lx\t%s%qx\n", 
		"sushift  ", lfsp->lfs_sushift,
		"fsbtodb  ", lfsp->lfs_fsbtodb,
		"cksum    ", lfsp->lfs_cksum,
		"maxfilesize ", lfsp->lfs_maxfilesize);

	(void)printf("Superblock disk addresses:");
	for (i = 0; i < LFS_MAXNUMSB; i++)
		(void)printf(" %lx", lfsp->lfs_sboffs[i]);
	(void)printf("\n");

	(void)printf("Checkpoint Info\n");
	(void)printf("%s%d\t%s%lx\t%s%d\n",
		"free     ", lfsp->lfs_free,
		"idaddr   ", lfsp->lfs_idaddr,
		"ifile    ", lfsp->lfs_ifile);
	(void)printf("%s%lx\t%s%d\t%s%lx\t%s%lx\t%s%lx\t%s%lx\n",
		"bfree    ", lfsp->lfs_bfree,
		"nfiles   ", lfsp->lfs_nfiles,
		"lastseg  ", lfsp->lfs_lastseg,
		"nextseg  ", lfsp->lfs_nextseg,
		"curseg   ", lfsp->lfs_curseg,
		"offset   ", lfsp->lfs_offset);
	(void)printf("tstamp   %lx\n", lfsp->lfs_tstamp);
}

void
lfs_dump_dinode(dip)
	struct dinode *dip;
{
	int i;

	(void)printf("%s%u\t%s%d\t%s%u\t%s%u\t%s%lu\n",
		"mode  ", dip->di_mode,
		"nlink ", dip->di_nlink,
		"uid   ", dip->di_uid,
		"gid   ", dip->di_gid,
		"size  ", dip->di_size);
	(void)printf("inum  %ld\n", dip->di_inumber);
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
#endif /* DEBUG */
