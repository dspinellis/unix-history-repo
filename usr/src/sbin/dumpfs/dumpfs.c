static	char *sccsid = "@(#)dumpfs.c	1.5 (Berkeley) %G%";
#include "../h/param.h"
#include "../h/fs.h"
#include "../h/inode.h"

/*
 * dumpfs
 */

union {
	struct fs fs;
	char pad[BSIZE];
} fsun;
#define	afs	fsun.fs

union {
	struct cg cg;
	char pad[BSIZE];
} cgun;
#define	acg	cgun.cg

main(argc, argv)
	char **argv;
{
	int i, j, k;

	close(0);
	if (open(argv[1], 0) != 0)
		perror(argv[1]), exit(1);
	lseek(0, SBLOCK*FSIZE, 0);
	if (read(0, &afs, BSIZE) != BSIZE)
		perror(argv[1]), exit(1);
	printf("magic\t%x\n", afs.fs_magic);
	printf("sblkno\t%d\n", afs.fs_sblkno);
	printf("time\t%s", ctime(&afs.fs_time));
	printf("size\t%d\n", afs.fs_size);
	printf("blocks\t%d\n", afs.fs_dsize);
	printf("ncg\t%d\n", afs.fs_ncg);
	printf("cssize\t%d\n", afs.fs_cssize);
	printf("cgsize\t%d\n", afs.fs_cgsize);
	printf("ntrak\t%d\nnsect\t%d\nspc\t%d\nncyl\t%d\n",
	    afs.fs_ntrak, afs.fs_nsect, afs.fs_spc, afs.fs_ncyl);
	printf("cpg\t%d\nfpg\t%d\nipg\t%d\n",
	    afs.fs_cpg, afs.fs_fpg, afs.fs_ipg);
	printf("ndir\t%d\nnffree\t%d\nnbfree\t%d\nnifree\t%d\n",
	    afs.fs_cstotal.cs_ndir, afs.fs_cstotal.cs_nffree,
	    afs.fs_cstotal.cs_nbfree, afs.fs_cstotal.cs_nifree);
	printf("cgrotor\t%d\nblocks available in each rotational position",
	    afs.fs_cgrotor);
	for (i = 0; i < NRPOS; i++) {
		if (afs.fs_postbl[i] > -1)
			printf("\nposition %d:\t", i);
		for (j = afs.fs_postbl[i], k = 1; j > -1;
		     j = afs.fs_rotbl[j], k++) {
			if (k % 15 == 0)
				printf("\n\t\t");
			printf("%4d", j);
		}
	}
	printf("\ncs[].cs_(nbfree,ndir,nifree,nffree):\n\t");
	for (i = 0; i < howmany(cssize(&afs), BSIZE); i++) {
		afs.fs_csp[i] = (struct csum *)calloc(1, BSIZE);
		lseek(0, (csaddr(&afs) + (i * FRAG)) * FSIZE, 0);
		if (read(0, afs.fs_csp[i], BSIZE) != BSIZE)
			perror(argv[1]), exit(1);
	}
	for (i = 0; i < afs.fs_ncg; i++) {
		struct csum *cs = &afs.fs_cs(i);
		if (i && i % 4 == 0)
			printf("\n\t");
		printf("(%d,%d,%d,%d) ",
		    cs->cs_nbfree, cs->cs_ndir, cs->cs_nifree, cs->cs_nffree);
	}
	printf("\n");
	printf("fmod\t%d\n", afs.fs_fmod);
	printf("ronly\t%d\n", afs.fs_ronly);
	printf("\n");
	for (i = 0; i < afs.fs_ncg; i++)
		dumpcg(i);
};

dumpcg(c)
	int c;
{
	int i,j;

	printf("\ncg %d:\n", c);
	lseek(0, cgtod(c,&afs)*FSIZE, 0);
	printf("tell\t%x\n", tell(0));
	if (read(0, (char *)&acg, afs.fs_cgsize) != afs.fs_cgsize) {
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
	for (i = 1, j = 0; i < FRAG; i++) {
		printf("\t%d", acg.cg_frsum[i]);
		j += i * acg.cg_frsum[i];
	}
	printf("\nsum of frsum: %d\niused:\t", j);
	pbits(acg.cg_iused, afs.fs_ipg);
	printf("free:\t");
	pbits(acg.cg_free, afs.fs_fpg);
	printf("b:\n");
	for (i = 0; i < afs.fs_cpg; i++) {
		printf("   c%d:\t", i);
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
