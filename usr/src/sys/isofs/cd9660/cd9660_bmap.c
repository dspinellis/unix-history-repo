/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Pace Willisson (pace@blitz.com).  The Rock Ridge Extension
 * Support code is derived from software contributed to Berkeley
 * by Atsushi Murai (amurai@spec.co.jp).
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cd9660_bmap.c	8.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/namei.h>
#include <sys/buf.h>
#include <sys/file.h>
#include <sys/vnode.h>
#include <sys/mount.h>

#include <isofs/cd9660/iso.h>
#include <isofs/cd9660/cd9660_node.h>

/*
 * Bmap converts a the logical block number of a file to its physical block
 * number on the disk. The conversion is done by using the logical block
 * number to index into the data block (extent) for the file.
 */
int
cd9660_bmap(ap)
	struct vop_bmap_args /* {
		struct vnode *a_vp;
		daddr_t  a_bn;
		struct vnode **a_vpp;
		daddr_t *a_bnp;
		int *a_runp;
	} */ *ap;
{
	struct iso_node *ip = VTOI(ap->a_vp);
	daddr_t lblkno = ap->a_bn;
	long bsize;

	/*
	 * Check for underlying vnode requests and ensure that logical
	 * to physical mapping is requested.
	 */
	if (ap->a_vpp != NULL)
		*ap->a_vpp = ip->i_devvp;
	if (ap->a_bnp == NULL)
		return (0);

	/*
	 * Compute the requested block number
	 */
	bsize = ip->i_mnt->logical_block_size;
	*ap->a_bnp = (ip->iso_start + lblkno) * btodb(bsize);

	/*
	 * Determine maximum number of readahead blocks following the
	 * requested block.
	 */
	if (ap->a_runp) {
		int nblk;

		nblk = (ip->i_size - (lblkno + 1) * bsize) / bsize;
		if (nblk <= 0)
			*ap->a_runp = 0;
		else if (nblk >= MAXBSIZE/bsize)
			*ap->a_runp = MAXBSIZE/bsize - 1;
		else
			*ap->a_runp = nblk;
	}

	return 0;
}
