static	char *sccsid = "@(#)dumpfs.c	1.3 (Berkeley) %G%";
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

struct	csum *fscs;

union {
	struct cg cg;
	char pad[BSIZE];
} cgun;
#define	acg	cgun.cg

main(argc, argv)
	char **argv;
{
	int i;

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
	printf("ncg\t%d\n", afs.fs_ncg);
	printf("cssize\t%d\n", afs.fs_cssize);
	printf("cgsize\t%d\n", afs.fs_cgsize);
	printf("ntrak\t%d\nnsect\t%d\nspc\t%d\nncyl\t%d\n",
	    afs.fs_ntrak, afs.fs_nsect, afs.fs_spc, afs.fs_ncyl);
	printf("cpg\t%d\nfpg\t%d\nipg\t%d\n",
	    afs.fs_cpg, afs.fs_fpg, afs.fs_ipg);
	printf("nffree\t%d\nnbfree\t%d\nnifree\t%d\n",
	    afs.fs_nffree, afs.fs_nbfree, afs.fs_nifree);
	printf("cs[].cs_(nbfree,ndir,nifree):\n\t");
	fscs = (struct csum *)calloc(afs.fs_ncg, sizeof (struct csum));
	lseek(0, csaddr(&afs)*FSIZE, 0);
	if (read(0, fscs, cssize(&afs)) != cssize(&afs))
		perror(argv[1]), exit(1);
	for (i = 0; i < afs.fs_ncg; i++) {
		struct csum *cs = fscs+i;
		if (i && i % 5 == 0)
			printf("\n\t");
		printf("(%d,%d,%d) ",
		    cs->cs_nbfree, cs->cs_ndir, cs->cs_nifree);
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
	printf("nifree\t%d\nndir\t%d\nnffree\t%d\nnbfree\t%d\nfrsum",
	    acg.cg_nifree, acg.cg_ndir, acg.cg_nffree, acg.cg_nbfree);
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
