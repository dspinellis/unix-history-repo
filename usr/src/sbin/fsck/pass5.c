#ifndef lint
static char version[] = "@(#)pass5.c	3.6 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/inode.h>
#include <sys/fs.h>
#include "fsck.h"

pass5()
{
	int c, blk, frags, sumsize, mapsize;
	daddr_t dbase, dmax, d;
	register long i, j;
	struct csum *cs;
	time_t now;
	struct csum cstotal;
	struct inodesc idesc;
	char buf[MAXBSIZE];
	register struct cg *newcg = (struct cg *)buf;

	bzero((char *)newcg, sblock.fs_cgsize);
	newcg->cg_magic = CG_MAGIC;
	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	bzero((char *)&cstotal, sizeof(struct csum));
	sumsize = cgrp.cg_iused - (char *)(&cgrp);
	mapsize = &cgrp.cg_free[howmany(sblock.fs_fpg, NBBY)] -
		(u_char *)cgrp.cg_iused;
	(void)time(&now);
	for (c = 0; c < sblock.fs_ncg; c++) {
		getblk(&cgblk, cgtod(&sblock, c), sblock.fs_cgsize);
		if (cgrp.cg_magic != CG_MAGIC)
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
		newcg->cg_niblk = sblock.fs_ipg;
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
		bzero((char *)newcg->cg_frsum, sizeof newcg->cg_frsum);
		bzero((char *)newcg->cg_btot, sizeof newcg->cg_btot);
		bzero((char *)newcg->cg_b, sizeof newcg->cg_b);
		bzero((char *)newcg->cg_free, howmany(sblock.fs_fpg, NBBY));
		bzero((char *)newcg->cg_iused, howmany(sblock.fs_ipg, NBBY));
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
				setbit(newcg->cg_iused, i);
				break;
			}
		}
		if (c == 0)
			for (i = 0; i < ROOTINO; i++) {
				setbit(newcg->cg_iused, i);
				newcg->cg_cs.cs_nifree--;
			}
		for (i = 0, d = dbase;
		     d <= dmax - sblock.fs_frag;
		     d += sblock.fs_frag, i += sblock.fs_frag) {
			frags = 0;
			for (j = 0; j < sblock.fs_frag; j++) {
				if (getbmap(d + j))
					continue;
				setbit(newcg->cg_free, i + j);
				frags++;
			}
			if (frags == sblock.fs_frag) {
				newcg->cg_cs.cs_nbfree++;
				j = cbtocylno(&sblock, i);
				newcg->cg_btot[j]++;
				newcg->cg_b[j][cbtorpos(&sblock, i)]++;
			} else if (frags > 0) {
				newcg->cg_cs.cs_nffree += frags;
				blk = blkmap(&sblock, newcg->cg_free, i);
				fragacct(&sblock, blk, newcg->cg_frsum, 1);
			}
		}
		for (frags = d; d < dmax; d++) {
			if (getbmap(d))
				continue;
			setbit(newcg->cg_free, d - dbase);
			newcg->cg_cs.cs_nffree++;
		}
		if (frags != d) {
			blk = blkmap(&sblock, newcg->cg_free, (frags - dbase));
			fragacct(&sblock, blk, newcg->cg_frsum, 1);
		}
		cstotal.cs_nffree += newcg->cg_cs.cs_nffree;
		cstotal.cs_nbfree += newcg->cg_cs.cs_nbfree;
		cstotal.cs_nifree += newcg->cg_cs.cs_nifree;
		cstotal.cs_ndir += newcg->cg_cs.cs_ndir;
		if (bcmp(newcg->cg_iused, cgrp.cg_iused, mapsize) != 0 &&
		    dofix(&idesc, "BLK(S) MISSING IN BIT MAPS")) {
			bcopy(newcg->cg_iused, cgrp.cg_iused, mapsize);
			cgdirty();
		}
		if (bcmp((char *)newcg, (char *)&cgrp, sumsize) != 0 &&
		    dofix(&idesc, "SUMMARY INFORMATION BAD")) {
			bcopy((char *)newcg, (char *)&cgrp, sumsize);
			cgdirty();
		}
		cs = &sblock.fs_cs(&sblock, c);
		if (bcmp((char *)&newcg->cg_cs, (char *)cs, sizeof *cs) != 0 &&
		    dofix(&idesc, "FREE BLK COUNT(S) WRONG IN SUPERBLK")) {
			bcopy((char *)&newcg->cg_cs, (char *)cs, sizeof *cs);
			sbdirty();
		}
	}
	if (bcmp((char *)&cstotal, (char *)&sblock.fs_cstotal, sizeof *cs) != 0
	    && dofix(&idesc, "FREE BLK COUNT(S) WRONG IN SUPERBLK")) {
		bcopy((char *)&cstotal, (char *)&sblock.fs_cstotal, sizeof *cs);
		sblock.fs_ronly = 0;
		sblock.fs_fmod = 0;
		sbdirty();
	}
}
