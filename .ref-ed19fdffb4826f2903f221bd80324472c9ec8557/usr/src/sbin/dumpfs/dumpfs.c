static	char *sccsid = "@(#)dumpfs.c	1.10 (Berkeley) %G%";

#include "../h/param.h"
#include "../h/fs.h"
#include "../h/inode.h"

/*
 * dumpfs
 */

union {
	struct fs fs;
	char pad[MAXBSIZE];
} fsun;
#define	afs	fsun.fs

union {
	struct cg cg;
	char pad[MAXBSIZE];
} cgun;
#define	acg	cgun.cg

main(argc, argv)
	char **argv;
{
	int c, i, j, k;

	close(0);
	if (open(argv[1], 0) != 0)
		perror(argv[1]), exit(1);
	lseek(0, SBLOCK * DEV_BSIZE, 0);
	if (read(0, &afs, SBSIZE) != SBSIZE)
		perror(argv[1]), exit(1);
	printf("magic\t%x\n", afs.fs_magic);
	printf("bblkno\t%d\n", afs.fs_bblkno);
	printf("sblkno\t%d\n", afs.fs_sblkno);
	printf("cblkno\t%d\n", afs.fs_cblkno);
	printf("iblkno\t%d\n", afs.fs_iblkno);
	printf("dblkno\t%d\n", afs.fs_dblkno);
	printf("time\t%s", ctime(&afs.fs_time));
	printf("size\t%d\n", afs.fs_size);
	printf("blocks\t%d\n", afs.fs_dsize);
	printf("ncg\t%d\n", afs.fs_ncg);
	printf("bsize\t%d\n", afs.fs_bsize);
	printf("fsize\t%d\n", afs.fs_fsize);
	printf("frag\t%d\n", afs.fs_frag);
	printf("minfree\t%d%%\n", afs.fs_minfree);
	printf("rotdelay %dms\n", afs.fs_rotdelay);
	printf("rps\t%d\n", afs.fs_rps);
	printf("csaddr\t%d\n", afs.fs_csaddr);
	printf("cssize\t%d\n", afs.fs_cssize);
	printf("cgsize\t%d\n", afs.fs_cgsize);
	printf("ntrak\t%d\nnsect\t%d\nspc\t%d\nncyl\t%d\n",
	    afs.fs_ntrak, afs.fs_nsect, afs.fs_spc, afs.fs_ncyl);
	printf("cpg\t%d\nbpg\t%d\nfpg\t%d\nipg\t%d\n",
	    afs.fs_cpg, afs.fs_fpg / afs.fs_frag, afs.fs_fpg, afs.fs_ipg);
	printf("ndir\t%d\nnffree\t%d\nnbfree\t%d\nnifree\t%d\n",
	    afs.fs_cstotal.cs_ndir, afs.fs_cstotal.cs_nffree,
	    afs.fs_cstotal.cs_nbfree, afs.fs_cstotal.cs_nifree);
	printf("cgrotor\t%d\nfmod\t%d\nronly\t%d\n",
	    afs.fs_cgrotor, afs.fs_fmod, afs.fs_ronly);
	if (afs.fs_cpc != 0)
		printf("blocks available in each rotational position");
	else
		printf("insufficient space to maintain rotational tables\n");
	for (c = 0; c < afs.fs_cpc; c++) {
		printf("\ncylinder number %d:", c);
		for (i = 0; i < NRPOS; i++) {
			if (afs.fs_postbl[c][i] == -1)
				continue;
			printf("\n   position %d:\t", i);
			for (j = afs.fs_postbl[c][i], k = 1; ;
			     j += afs.fs_rotbl[j], k++) {
				printf("%5d", j);
				if (k % 12 == 0)
					printf("\n\t\t");
				if (afs.fs_rotbl[j] == 0)
					break;
			}
		}
	}
	printf("\ncs[].cs_(nbfree,ndir,nifree,nffree):\n\t");
	for (i = 0; i < howmany(afs.fs_cssize, afs.fs_bsize); i++) {
		afs.fs_csp[i] = (struct csum *)calloc(1, afs.fs_bsize);
		lseek(0, fsbtodb(&afs, (afs.fs_csaddr + (i * afs.fs_frag))) *
		    DEV_BSIZE, 0);
		if (read(0, afs.fs_csp[i], afs.fs_bsize) != afs.fs_bsize)
			perror(argv[1]), exit(1);
	}
	for (i = 0; i < afs.fs_ncg; i++) {
		struct csum *cs = &afs.fs_cs(&afs, i);
		if (i && i % 4 == 0)
			printf("\n\t");
		printf("(%d,%d,%d,%d) ",
		    cs->cs_nbfree, cs->cs_ndir, cs->cs_nifree, cs->cs_nffree);
	}
	printf("\n");
	if (afs.fs_ncyl % afs.fs_cpg) {
		printf("cylinders in last group %d\n",
		    i = afs.fs_ncyl % afs.fs_cpg);
		printf("blocks in last group %d\n",
		    i * afs.fs_spc / NSPB(&afs));
	}
	printf("\n");
	for (i = 0; i < afs.fs_ncg; i++)
		dumpcg(i);
};

dumpcg(c)
	int c;
{
	int i,j;

	printf("\ncg %d:\n", c);
	lseek(0, fsbtodb(&afs, cgtod(&afs, c)) * DEV_BSIZE, 0);
	printf("tell\t%x\n", tell(0));
	if (read(0, (char *)&acg, afs.fs_bsize) != afs.fs_bsize) {
		printf("\terror reading cg\n");
		return;
	}
	printf("magic\t%x\ntime\t%s", acg.cg_magic, ctime(&acg.cg_time));
	printf("cgx\t%d\nncyl\t%d\nniblk\t%d\nndblk\t%d\n",
	    acg.cg_cgx, acg.cg_ncyl, acg.cg_niblk, acg.cg_ndblk);
	printf("nifree\t%d\nndir\t%d\nnffree\t%d\nnbfree\t%d\n",
	    acg.cg_cs.cs_nifree, acg.cg_cs.cs_ndir,
	    acg.cg_cs.cs_nffree, acg.cg_cs.cs_nbfree);
	printf("rotor\t%d\nirotor\t%d\nfrotor\t%d\nfrsum",
	    acg.cg_rotor, acg.cg_irotor, acg.cg_frotor);
	for (i = 1, j = 0; i < afs.fs_frag; i++) {
		printf("\t%d", acg.cg_frsum[i]);
		j += i * acg.cg_frsum[i];
	}
	printf("\nsum of frsum: %d\niused:\t", j);
	pbits(acg.cg_iused, afs.fs_ipg);
	printf("free:\t");
	pbits(acg.cg_free, afs.fs_fpg);
	printf("b:\n");
	for (i = 0; i < afs.fs_cpg; i++) {
		printf("   c%d:\t(%d)\t", i, acg.cg_btot[i]);
		for (j = 0; j < NRPOS; j++)
			printf(" %d", acg.cg_b[i][j]);
		printf("\n");
	}
};

pbits(cp, max)
	register char *cp;
	int max;
{
	register int i;
	int count = 0, j;

	for (i = 0; i < max; i++)
		if (isset(cp, i)) {
			if (count)
				printf(",%s", count %10 == 9 ? "\n\t" : " ");
			count++;
			printf("%d", i);
			j = i;
			while ((i+1)<max && isset(cp, i+1))
				i++;
			if (i != j)
				printf("-%d", i);
		}
	printf("\n");
}
