/*
 * Copyright (c) 1980, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)pass5.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <ufs/ufs/dinode.h>
#include <ufs/ffs/fs.h>
#include <string.h>
#include "fsck.h"

pass5()
{
	int c, blk, frags, basesize, sumsize, mapsize, savednrpos;
	register struct fs *fs = &sblock;
	register struct cg *cg = &cgrp;
	daddr_t dbase, dmax;
	register daddr_t d;
	register long i, j;
	struct csum *cs;
	struct csum cstotal;
	struct inodesc idesc[3];
	char buf[MAXBSIZE];
	register struct cg *newcg = (struct cg *)buf;
	struct ocg *ocg = (struct ocg *)buf;

	bzero((char *)newcg, (size_t)fs->fs_cgsize);
	newcg->cg_niblk = fs->fs_ipg;
	switch ((int)fs->fs_postblformat) {

	case FS_42POSTBLFMT:
		basesize = (char *)(&ocg->cg_btot[0]) - (char *)(&ocg->cg_link);
		sumsize = &ocg->cg_iused[0] - (char *)(&ocg->cg_btot[0]);
		mapsize = &ocg->cg_free[howmany(fs->fs_fpg, NBBY)] -
			(u_char *)&ocg->cg_iused[0];
		ocg->cg_magic = CG_MAGIC;
		savednrpos = fs->fs_nrpos;
		fs->fs_nrpos = 8;
		break;

	case FS_DYNAMICPOSTBLFMT:
		newcg->cg_btotoff =
		 	&newcg->cg_space[0] - (u_char *)(&newcg->cg_link);
		newcg->cg_boff =
			newcg->cg_btotoff + fs->fs_cpg * sizeof(long);
		newcg->cg_iusedoff = newcg->cg_boff + 
			fs->fs_cpg * fs->fs_nrpos * sizeof(short);
		newcg->cg_freeoff =
			newcg->cg_iusedoff + howmany(fs->fs_ipg, NBBY);
		newcg->cg_nextfreeoff = newcg->cg_freeoff +
			howmany(fs->fs_cpg * fs->fs_spc / NSPF(fs),
				NBBY);
		newcg->cg_magic = CG_MAGIC;
		basesize = &newcg->cg_space[0] - (u_char *)(&newcg->cg_link);
		sumsize = newcg->cg_iusedoff - newcg->cg_btotoff;
		mapsize = newcg->cg_nextfreeoff - newcg->cg_iusedoff;
		break;

	default:
		errexit("UNKNOWN ROTATIONAL TABLE FORMAT %d\n",
			fs->fs_postblformat);
	}
	bzero((char *)&idesc[0], sizeof idesc);
	for (i = 0; i < 3; i++) {
		idesc[i].id_type = ADDR;
		if (doinglevel2)
			idesc[i].id_fix = FIX;
	}
	bzero((char *)&cstotal, sizeof(struct csum));
	j = blknum(fs, fs->fs_size + fs->fs_frag - 1);
	for (i = fs->fs_size; i < j; i++)
		setbmap(i);
	for (c = 0; c < fs->fs_ncg; c++) {
		getblk(&cgblk, cgtod(fs, c), fs->fs_cgsize);
		if (!cg_chkmagic(cg))
			pfatal("CG %d: BAD MAGIC NUMBER\n", c);
		dbase = cgbase(fs, c);
		dmax = dbase + fs->fs_fpg;
		if (dmax > fs->fs_size)
			dmax = fs->fs_size;
		newcg->cg_time = cg->cg_time;
		newcg->cg_cgx = c;
		if (c == fs->fs_ncg - 1)
			newcg->cg_ncyl = fs->fs_ncyl % fs->fs_cpg;
		else
			newcg->cg_ncyl = fs->fs_cpg;
		newcg->cg_ndblk = dmax - dbase;
		newcg->cg_cs.cs_ndir = 0;
		newcg->cg_cs.cs_nffree = 0;
		newcg->cg_cs.cs_nbfree = 0;
		newcg->cg_cs.cs_nifree = fs->fs_ipg;
		if (cg->cg_rotor < newcg->cg_ndblk)
			newcg->cg_rotor = cg->cg_rotor;
		else
			newcg->cg_rotor = 0;
		if (cg->cg_frotor < newcg->cg_ndblk)
			newcg->cg_frotor = cg->cg_frotor;
		else
			newcg->cg_frotor = 0;
		if (cg->cg_irotor < newcg->cg_niblk)
			newcg->cg_irotor = cg->cg_irotor;
		else
			newcg->cg_irotor = 0;
		bzero((char *)&newcg->cg_frsum[0], sizeof newcg->cg_frsum);
		bzero((char *)&cg_blktot(newcg)[0],
		      (size_t)(sumsize + mapsize));
		if (fs->fs_postblformat == FS_42POSTBLFMT)
			ocg->cg_magic = CG_MAGIC;
		j = fs->fs_ipg * c;
		for (i = 0; i < fs->fs_ipg; j++, i++) {
			switch (statemap[j]) {

			case USTATE:
				break;

			case DSTATE:
			case DCLEAR:
			case DFOUND:
				newcg->cg_cs.cs_ndir++;
				/* fall through */

			case FSTATE:
			case FCLEAR:
				newcg->cg_cs.cs_nifree--;
				setbit(cg_inosused(newcg), i);
				break;

			default:
				if (j < ROOTINO)
					break;
				errexit("BAD STATE %d FOR INODE I=%d",
				    statemap[j], j);
			}
		}
		if (c == 0)
			for (i = 0; i < ROOTINO; i++) {
				setbit(cg_inosused(newcg), i);
				newcg->cg_cs.cs_nifree--;
			}
		for (i = 0, d = dbase;
		     d < dmax;
		     d += fs->fs_frag, i += fs->fs_frag) {
			frags = 0;
			for (j = 0; j < fs->fs_frag; j++) {
				if (testbmap(d + j))
					continue;
				setbit(cg_blksfree(newcg), i + j);
				frags++;
			}
			if (frags == fs->fs_frag) {
				newcg->cg_cs.cs_nbfree++;
				j = cbtocylno(fs, i);
				cg_blktot(newcg)[j]++;
				cg_blks(fs, newcg, j)[cbtorpos(fs, i)]++;
			} else if (frags > 0) {
				newcg->cg_cs.cs_nffree += frags;
				blk = blkmap(fs, cg_blksfree(newcg), i);
				ffs_fragacct(fs, blk, newcg->cg_frsum, 1);
			}
		}
		cstotal.cs_nffree += newcg->cg_cs.cs_nffree;
		cstotal.cs_nbfree += newcg->cg_cs.cs_nbfree;
		cstotal.cs_nifree += newcg->cg_cs.cs_nifree;
		cstotal.cs_ndir += newcg->cg_cs.cs_ndir;
		cs = &fs->fs_cs(fs, c);
		if (bcmp((char *)&newcg->cg_cs, (char *)cs, sizeof *cs) != 0 &&
		    dofix(&idesc[0], "FREE BLK COUNT(S) WRONG IN SUPERBLK")) {
			bcopy((char *)&newcg->cg_cs, (char *)cs, sizeof *cs);
			sbdirty();
		}
		if (doinglevel1) {
			bcopy((char *)newcg, (char *)cg, (size_t)fs->fs_cgsize);
			cgdirty();
			continue;
		}
		if (bcmp(cg_inosused(newcg),
			 cg_inosused(cg), mapsize) != 0 &&
		    dofix(&idesc[1], "BLK(S) MISSING IN BIT MAPS")) {
			bcopy(cg_inosused(newcg), cg_inosused(cg),
			      (size_t)mapsize);
			cgdirty();
		}
		if ((bcmp((char *)newcg, (char *)cg, basesize) != 0 ||
		     bcmp((char *)&cg_blktot(newcg)[0],
			  (char *)&cg_blktot(cg)[0], sumsize) != 0) &&
		    dofix(&idesc[2], "SUMMARY INFORMATION BAD")) {
			bcopy((char *)newcg, (char *)cg, (size_t)basesize);
			bcopy((char *)&cg_blktot(newcg)[0],
			      (char *)&cg_blktot(cg)[0], (size_t)sumsize);
			cgdirty();
		}
	}
	if (fs->fs_postblformat == FS_42POSTBLFMT)
		fs->fs_nrpos = savednrpos;
	if (bcmp((char *)&cstotal, (char *)&fs->fs_cstotal, sizeof *cs) != 0
	    && dofix(&idesc[0], "FREE BLK COUNT(S) WRONG IN SUPERBLK")) {
		bcopy((char *)&cstotal, (char *)&fs->fs_cstotal, sizeof *cs);
		fs->fs_ronly = 0;
		fs->fs_fmod = 0;
		sbdirty();
	}
}
