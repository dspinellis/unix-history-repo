/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)pass5.c	5.4 (Berkeley) %G%";
#endif not lint

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include "fsck.h"

pass5()
{
	int c, blk, frags, basesize, sumsize, mapsize, savednrpos;
	daddr_t dbase, dmax;
	register daddr_t d;
	register long i, j;
	struct csum *cs;
	time_t now;
	struct csum cstotal;
	struct inodesc idesc;
	char buf[MAXBSIZE];
	register struct cg *newcg = (struct cg *)buf;
	struct ocg *ocg = (struct ocg *)buf;

	bzero((char *)newcg, sblock.fs_cgsize);
	newcg->cg_niblk = sblock.fs_ipg;
	switch (sblock.fs_postblformat) {

	case FS_42POSTBLFMT:
		basesize = (char *)(&ocg->cg_btot[0]) - (char *)(&ocg->cg_link);
		sumsize = &ocg->cg_iused[0] - (char *)(&ocg->cg_btot[0]);
		mapsize = &ocg->cg_free[howmany(sblock.fs_fpg, NBBY)] -
			(u_char *)&ocg->cg_iused[0];
		ocg->cg_magic = CG_MAGIC;
		savednrpos = sblock.fs_nrpos;
		sblock.fs_nrpos = 8;
		break;

	case FS_DYNAMICPOSTBLFMT:
		newcg->cg_btotoff =
		 	&newcg->cg_space[0] - (u_char *)(&newcg->cg_link);
		newcg->cg_boff =
			newcg->cg_btotoff + sblock.fs_cpg * sizeof(long);
		newcg->cg_iusedoff = newcg->cg_boff + 
			sblock.fs_cpg * sblock.fs_nrpos * sizeof(short);
		newcg->cg_freeoff =
			newcg->cg_iusedoff + howmany(sblock.fs_ipg, NBBY);
		newcg->cg_nextfreeoff = newcg->cg_freeoff +
			howmany(sblock.fs_cpg * sblock.fs_spc / NSPF(&sblock),
				NBBY);
		newcg->cg_magic = CG_MAGIC;
		basesize = &newcg->cg_space[0] - (u_char *)(&newcg->cg_link);
		sumsize = newcg->cg_iusedoff - newcg->cg_btotoff;
		mapsize = newcg->cg_nextfreeoff - newcg->cg_iusedoff;
		break;

	default:
		errexit("UNKNOWN ROTATIONAL TABLE FORMAT %d\n",
			sblock.fs_postblformat);
	}
	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	bzero((char *)&cstotal, sizeof(struct csum));
	(void)time(&now);
	for (i = sblock.fs_size; i < fragroundup(&sblock, sblock.fs_size); i++)
		setbmap(i);
	for (c = 0; c < sblock.fs_ncg; c++) {
		getblk(&cgblk, cgtod(&sblock, c), sblock.fs_cgsize);
		if (!cg_chkmagic(&cgrp))
			pfatal("CG %d: BAD MAGIC NUMBER\n", c);
		dbase = cgbase(&sblock, c);
		dmax = dbase + sblock.fs_fpg;
		if (dmax > sblock.fs_size)
			dmax = sblock.fs_size;
		if (now > cgrp.cg_time)
			newcg->cg_time = cgrp.cg_time;
		else
			newcg->cg_time = now;
		newcg->cg_cgx = c;
		if (c == sblock.fs_ncg - 1)
			newcg->cg_ncyl = sblock.fs_ncyl % sblock.fs_cpg;
		else
			newcg->cg_ncyl = sblock.fs_cpg;
		newcg->cg_ndblk = dmax - dbase;
		newcg->cg_cs.cs_ndir = 0;
		newcg->cg_cs.cs_nffree = 0;
		newcg->cg_cs.cs_nbfree = 0;
		newcg->cg_cs.cs_nifree = sblock.fs_ipg;
		if (cgrp.cg_rotor < newcg->cg_ndblk)
			newcg->cg_rotor = cgrp.cg_rotor;
		else
			newcg->cg_rotor = 0;
		if (cgrp.cg_frotor < newcg->cg_ndblk)
			newcg->cg_frotor = cgrp.cg_frotor;
		else
			newcg->cg_frotor = 0;
		if (cgrp.cg_irotor < newcg->cg_niblk)
			newcg->cg_irotor = cgrp.cg_irotor;
		else
			newcg->cg_irotor = 0;
		bzero((char *)&newcg->cg_frsum[0], sizeof newcg->cg_frsum);
		bzero((char *)&cg_blktot(newcg)[0], sumsize + mapsize);
		if (sblock.fs_postblformat == FS_42POSTBLFMT)
			ocg->cg_magic = CG_MAGIC;
		j = sblock.fs_ipg * c;
		for (i = 0; i < sblock.fs_ipg; j++, i++) {
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
		     d += sblock.fs_frag, i += sblock.fs_frag) {
			frags = 0;
			for (j = 0; j < sblock.fs_frag; j++) {
				if (getbmap(d + j))
					continue;
				setbit(cg_blksfree(newcg), i + j);
				frags++;
			}
			if (frags == sblock.fs_frag) {
				newcg->cg_cs.cs_nbfree++;
				j = cbtocylno(&sblock, i);
				cg_blktot(newcg)[j]++;
				cg_blks(&sblock, newcg, j)
				    [cbtorpos(&sblock, i)]++;
			} else if (frags > 0) {
				newcg->cg_cs.cs_nffree += frags;
				blk = blkmap(&sblock, cg_blksfree(newcg), i);
				fragacct(&sblock, blk, newcg->cg_frsum, 1);
			}
		}
		cstotal.cs_nffree += newcg->cg_cs.cs_nffree;
		cstotal.cs_nbfree += newcg->cg_cs.cs_nbfree;
		cstotal.cs_nifree += newcg->cg_cs.cs_nifree;
		cstotal.cs_ndir += newcg->cg_cs.cs_ndir;
		cs = &sblock.fs_cs(&sblock, c);
		if (bcmp((char *)&newcg->cg_cs, (char *)cs, sizeof *cs) != 0 &&
		    dofix(&idesc, "FREE BLK COUNT(S) WRONG IN SUPERBLK")) {
			bcopy((char *)&newcg->cg_cs, (char *)cs, sizeof *cs);
			sbdirty();
		}
		if (cvtflag) {
			bcopy((char *)newcg, (char *)&cgrp, sblock.fs_cgsize);
			cgdirty();
			continue;
		}
		if (bcmp(cg_inosused(newcg),
			 cg_inosused(&cgrp), mapsize) != 0 &&
		    dofix(&idesc, "BLK(S) MISSING IN BIT MAPS")) {
			bcopy(cg_inosused(newcg), cg_inosused(&cgrp), mapsize);
			cgdirty();
		}
		if ((bcmp((char *)newcg, (char *)&cgrp, basesize) != 0 ||
		     bcmp((char *)&cg_blktot(newcg)[0],
			  (char *)&cg_blktot(&cgrp)[0], sumsize) != 0) &&
		    dofix(&idesc, "SUMMARY INFORMATION BAD")) {
			bcopy((char *)newcg, (char *)&cgrp, basesize);
			bcopy((char *)&cg_blktot(newcg)[0],
			      (char *)&cg_blktot(&cgrp)[0], sumsize);
			cgdirty();
		}
	}
	if (sblock.fs_postblformat == FS_42POSTBLFMT)
		sblock.fs_nrpos = savednrpos;
	if (bcmp((char *)&cstotal, (char *)&sblock.fs_cstotal, sizeof *cs) != 0
	    && dofix(&idesc, "FREE BLK COUNT(S) WRONG IN SUPERBLK")) {
		bcopy((char *)&cstotal, (char *)&sblock.fs_cstotal, sizeof *cs);
		sblock.fs_ronly = 0;
		sblock.fs_fmod = 0;
		sbdirty();
	}
}
