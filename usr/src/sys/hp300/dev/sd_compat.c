/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sd_compat.c	7.2 (Berkeley) %G%
 */

/*
 * Compatibility for SCSI disks without labels.
 */
#include "sd.h"
#if NSD > 0

#include <sys/param.h>
#include <sys/disklabel.h>
#include <hp/dev/device.h>
#include <hp300/dev/sdvar.h>

/*
 * Since the SCSI standard tends to hide the disk structure, we define
 * partitions in terms of DEV_BSIZE blocks.  The default partition table
 * (for an unlabeled disk) reserves 512K for a boot area, has an 8 meg
 * root (A) and 32 meg of swap (B).  The rest of the space on the drive
 * goes in the G partition.  As usual, the C partition covers the entire
 * disk (including the boot area).
 *
 * We also define the D, E, F and H partitions as an alternative to B and G.
 * D is 48Mb, starts after A and is intended for swapping.
 * E is 50Mb, starts after D and is intended for /usr.
 * F starts after E and is what ever is left.
 * H starts after D and is what ever is left (i.e. combo of E and F).
 */
struct partition sddefaultpart[] = {
	{  16384,   1024, 1024, FS_BSDFFS, 8 },
	{  65536,  17408,    0, FS_SWAP,   0 },
	{      0,      0,    0, FS_BOOT,   0 },
	{  98304,  17408,    0, FS_SWAP,   0 },
	{ 102400, 115712, 1024, FS_BSDFFS, 8 },
	{      0, 218112, 1024, FS_BSDFFS, 8 },
	{      0,  82944, 1024, FS_BSDFFS, 8 },
	{      0, 115712, 1024, FS_BSDFFS, 8 }
};
int sdnumdefaultpart = sizeof(sddefaultpart)/sizeof(sddefaultpart[0]);

extern struct sd_softc sd_softc[];

sdmakedisklabel(unit, lp)
	int unit;
	register struct disklabel *lp;
{
	register struct sd_softc *sc = &sd_softc[unit];
	register struct partition *pi, *dpi;
	register int dcount;
	
	lp->d_secperunit = sc->sc_blks;
	lp->d_rpm = 3600;
	lp->d_interleave = 1;
	if (sc->sc_flags & SDF_RMEDIA)
		lp->d_flags |= D_REMOVABLE;
	lp->d_npartitions = sdnumdefaultpart;

	pi = lp->d_partitions;
	dpi = sddefaultpart;
	dcount = sdnumdefaultpart;
	while (dcount-- > 0)
		*pi++ = *dpi++;

	pi = lp->d_partitions;

	/*
	 * C gets everything
	 */
	pi[2].p_size = sc->sc_blks;
	/*
	 * G gets from end of B to end of disk
	 */
	pi[6].p_size = sc->sc_blks - pi[6].p_offset;
	/*
	 * H gets from end of D to end of disk
	 */
	pi[7].p_size = sc->sc_blks - pi[7].p_offset;
	/*
	 * If disk is big enough, define E and F
	 */
	if (sc->sc_blks > pi[5].p_offset)
		pi[5].p_size = sc->sc_blks - pi[5].p_offset;
	else {
		pi[4].p_offset = pi[4].p_size = 0;
		pi[5].p_offset = pi[5].p_size = 0;
	}
}
#endif
