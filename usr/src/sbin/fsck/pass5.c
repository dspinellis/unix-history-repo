#ifndef lint
static char version[] = "@(#)pass5.c	3.1 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include "fsck.h"

pass5()
{
	register int c, n, i, b, d;
	short bo[MAXCPG][NRPOS];
	long botot[MAXCPG];
	long frsum[MAXFRAG];
	int blk;
	daddr_t cbase;
	int blockbits = (1<<sblock.fs_frag)-1;

	bcopy(blockmap, freemap, (unsigned)bmapsz);
	dupblk = 0;
	n_index = sblock.fs_ncg * (cgdmin(&sblock, 0) - cgtod(&sblock, 0));
	for (c = 0; c < sblock.fs_ncg; c++) {
		cbase = cgbase(&sblock, c);
		bzero((char *)botot, sizeof (botot));
		bzero((char *)bo, sizeof (bo));
		bzero((char *)frsum, sizeof (frsum));
		/*
		 * need to account for the super blocks
		 * which appear (inaccurately) bad
		 */
		n_bad += cgtod(&sblock, c) - cgsblock(&sblock, c);
		if (getblk(&cgblk, cgtod(&sblock, c), sblock.fs_cgsize) == 0)
			continue;
		if (cgrp.cg_magic != CG_MAGIC) {
			pfatal("CG %d: BAD MAGIC NUMBER\n", c);
			bzero((char *)&cgrp, (int)sblock.fs_cgsize);
		}
		for (b = 0; b < sblock.fs_fpg; b += sblock.fs_frag) {
			blk = blkmap(&sblock, cgrp.cg_free, b);
			if (blk == 0)
				continue;
			if (blk == blockbits) {
				if (pass5check(cbase+b, sblock.fs_frag) == STOP)
					goto out5;
				/* this is clumsy ... */
				n_ffree -= sblock.fs_frag;
				n_bfree++;
				botot[cbtocylno(&sblock, b)]++;
				bo[cbtocylno(&sblock, b)]
				    [cbtorpos(&sblock, b)]++;
				continue;
			}
			for (d = 0; d < sblock.fs_frag; d++)
				if ((blk & (1<<d)) &&
				    pass5check(cbase + b + d, (long)1) == STOP)
					goto out5;
			fragacct(&sblock, blk, frsum, 1);
		}
		if (bcmp((char *)cgrp.cg_frsum, (char *)frsum, sizeof(frsum))) {
			if (debug)
			for (i = 0; i < sblock.fs_frag; i++)
				if (cgrp.cg_frsum[i] != frsum[i])
				printf("cg[%d].cg_frsum[%d] have %d calc %d\n",
				    c, i, cgrp.cg_frsum[i], frsum[i]);
			frsumbad++;
		}
		if (bcmp((char *)cgrp.cg_btot, (char *)botot, sizeof (botot))) {
			if (debug)
			for (n = 0; n < sblock.fs_cpg; n++)
				if (botot[n] != cgrp.cg_btot[n])
				printf("cg[%d].cg_btot[%d] have %d calc %d\n",
				    c, n, cgrp.cg_btot[n], botot[n]);
			offsumbad++;
		}
		if (bcmp((char *)cgrp.cg_b, (char *)bo, sizeof (bo))) {
			if (debug)
			for (i = 0; i < NRPOS; i++)
				if (bo[n][i] != cgrp.cg_b[n][i])
				printf("cg[%d].cg_b[%d][%d] have %d calc %d\n",
				    c, n, i, cgrp.cg_b[n][i], bo[n][i]);
			offsumbad++;
		}
	}
out5:
	if (dupblk)
		pwarn("%d DUP BLKS IN BIT MAPS\n", dupblk);
	if (fixcg == 0) {
		if ((b = n_blks+n_ffree+sblock.fs_frag*n_bfree+n_index+n_bad) != fmax) {
			pwarn("%ld BLK(S) MISSING\n", fmax - b);
			fixcg = 1;
		} else if (inosumbad + offsumbad + frsumbad + sbsumbad) {
			pwarn("SUMMARY INFORMATION %s%s%s%sBAD\n",
			    inosumbad ? "(INODE FREE) " : "",
			    offsumbad ? "(BLOCK OFFSETS) " : "",
			    frsumbad ? "(FRAG SUMMARIES) " : "",
			    sbsumbad ? "(SUPER BLOCK SUMMARIES) " : "");
			fixcg = 1;
		} else if (n_ffree != sblock.fs_cstotal.cs_nffree ||
		    n_bfree != sblock.fs_cstotal.cs_nbfree) {
			pwarn("FREE BLK COUNT(S) WRONG IN SUPERBLK");
			if (preen)
				printf(" (FIXED)\n");
			if (preen || reply("FIX") == 1) {
				sblock.fs_cstotal.cs_nffree = n_ffree;
				sblock.fs_cstotal.cs_nbfree = n_bfree;
				sbdirty();
			}
		}
	}
	if (fixcg) {
		pwarn("BAD CYLINDER GROUPS");
		if (preen)
			printf(" (SALVAGED)\n");
		else if (reply("SALVAGE") == 0)
			fixcg = 0;
	}
}

pass5check(blk, size)
	daddr_t blk;
	long size;
{

	if (outrange(blk, (int)size)) {
		fixcg = 1;
		if (preen)
			pfatal("BAD BLOCKS IN BIT MAPS.");
		if (++badblk >= MAXBAD) {
			printf("EXCESSIVE BAD BLKS IN BIT MAPS.");
			if (reply("CONTINUE") == 0)
				errexit("");
			return (STOP);
		}
	}
	for (; size > 0; blk++, size--)
		if (getfmap(blk)) {
			fixcg = 1;
			++dupblk;
		} else {
			n_ffree++;
			setfmap(blk);
		}
	return (KEEPON);
}
