static	char *sccsid = "@(#)mkfs.c	1.12 (Berkeley) %G%";

/*
 * make file system for cylinder-group style file systems
 *
 * usage: mkfs special size [ nsect ntrak bsize fsize cpg ]
 */

#ifndef STANDALONE
#include <stdio.h>
#include <a.out.h>
#endif

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dir.h"

#define UMASK		0755
#define MAXNDIR		(MAXBSIZE / sizeof(struct direct))
#define MAXINOPB	(MAXBSIZE / sizeof(struct dinode))
#define POWEROF2(num)	(((num) & ((num) - 1)) == 0)

union {
	struct fs fs;
	char pad[MAXBSIZE];
} fsun;
#define	sblock	fsun.fs
struct	csum *fscs;

union {
	struct cg cg;
	char pad[MAXBSIZE];
} cgun;
#define	acg	cgun.cg

struct	dinode zino[MAXIPG];

char	*fsys;
time_t	utime;
int	fsi;
int	fso;
daddr_t	alloc();

main(argc, argv)
	int argc;
	char *argv[];
{
	long c, i, inos, fssize;

#ifndef STANDALONE
	argc--, argv++;
	time(&utime);
	if (argc < 2) {
		printf("usage: mkfs special size [ nsect ntrak bsize fsize cpg ]\n");
		exit(1);
	}
	fsys = argv[0];
	fssize = atoi(argv[1]);
	fso = creat(fsys, 0666);
	if(fso < 0) {
		printf("%s: cannot create\n", fsys);
		exit(1);
	}
	fsi = open(fsys, 0);
	if(fsi < 0) {
		printf("%s: cannot open\n", fsys);
		exit(1);
	}
#else
	{
		static char protos[60];
		char fsbuf[100];

		printf("file sys size: ");
		gets(protos);
		fssize = atoi(protos);
		do {
			printf("file system: ");
			gets(fsbuf);
			fso = open(fsbuf, 1);
			fsi = open(fsbuf, 0);
		} while (fso < 0 || fsi < 0);
	}
	argc = 0;
#endif
	if (fssize <= 0)
		printf("preposterous size %d\n", fssize), exit(1);
	/*
	 * collect and verify the sector and track info
	 */
	if (argc > 2)
		sblock.fs_nsect = atoi(argv[2]);
	else
		sblock.fs_nsect = DFLNSECT;
	if (argc > 3)
		sblock.fs_ntrak = atoi(argv[3]);
	else
		sblock.fs_ntrak = DFLNTRAK;
	if (sblock.fs_ntrak <= 0)
		printf("preposterous ntrak %d\n", sblock.fs_ntrak), exit(1);
	if (sblock.fs_nsect <= 0)
		printf("preposterous nsect %d\n", sblock.fs_nsect), exit(1);
	sblock.fs_spc = sblock.fs_ntrak * sblock.fs_nsect;
	/*
	 * collect and verify the block and fragment sizes
	 */
	if (argc > 4)
		sblock.fs_bsize = atoi(argv[4]);
	else
		sblock.fs_bsize = MAXBSIZE;
	if (argc > 5)
		sblock.fs_fsize = atoi(argv[5]);
	else
		sblock.fs_fsize = MAX(sblock.fs_bsize / DESFRAG, DEV_BSIZE);
	if (!POWEROF2(sblock.fs_bsize)) {
		printf("block size must be a power of 2, not %d\n",
		    sblock.fs_bsize);
		exit(1);
	}
	if (!POWEROF2(sblock.fs_fsize)) {
		printf("fragment size must be a power of 2, not %d\n",
		    sblock.fs_fsize);
		exit(1);
	}
	if (sblock.fs_fsize < DEV_BSIZE) {
		printf("fragment size %d is too small, minimum is %d\n",
		    sblock.fs_fsize, DEV_BSIZE);
		exit(1);
	}
	if (sblock.fs_bsize < MINBSIZE) {
		printf("block size %d is too small, minimum is %d\n",
		    sblock.fs_bsize, MINBSIZE);
		exit(1);
	}
	if (sblock.fs_bsize < sblock.fs_fsize) {
		printf("block size (%d) cannot be smaller than fragment size (%d)\n",
		    sblock.fs_bsize, sblock.fs_fsize);
		exit(1);
	}
	sblock.fs_frag = sblock.fs_bsize / sblock.fs_fsize;
	if (sblock.fs_frag > MAXFRAG) {
		printf("fragment size %d is too small, minimum with block size %d is %d\n",
		    sblock.fs_fsize, sblock.fs_bsize,
		    sblock.fs_bsize / MAXFRAG);
		exit(1);
	}
	/* 
	 * collect and verify the number of cylinders per group
	 */
	if (argc > 6) {
		sblock.fs_cpg = atoi(argv[6]);
		sblock.fs_fpg = (sblock.fs_cpg * sblock.fs_spc) / NSPF(&sblock);
	} else {
		sblock.fs_cpg = DESCPG;
		sblock.fs_fpg = (sblock.fs_cpg * sblock.fs_spc) / NSPF(&sblock);
		while (sblock.fs_fpg / sblock.fs_frag > MAXBPG(&sblock)) {
			--sblock.fs_cpg;
			sblock.fs_fpg =
			    (sblock.fs_cpg * sblock.fs_spc) / NSPF(&sblock);
		}
	}
	if (sblock.fs_cpg < 1) {
		printf("cylinder groups must have at least 1 cylinder\n");
		exit(1);
	}
	if (sblock.fs_cpg > MAXCPG) {
		printf("cylinder groups are limited to %d cylinders\n", MAXCPG);
		exit(1);
	}
	/*
	 * Now have size for file system and nsect and ntrak.
	 * Determine number of cylinders and blocks in the file system.
	 */
	sblock.fs_size = fssize = dbtofsb(&sblock, fssize);
	sblock.fs_ncyl = fssize * NSPF(&sblock) / sblock.fs_spc;
	if (sblock.fs_spc > MAXBPC * NSPB(&sblock)) {
		printf("too many sectors per cylinder (%d sectors)\n",
		    sblock.fs_spc);
		while (sblock.fs_spc > MAXBPC * NSPB(&sblock))
			sblock.fs_bsize <<= 1;
		printf("nsect %d, and ntrak %d, requires block size of %d\n",
		    sblock.fs_nsect, sblock.fs_ntrak, sblock.fs_bsize);
		exit(1);
	}
	/*
	 * Validate specified/determined cpg.
	 */
	if (sblock.fs_spc > MAXBPG(&sblock) * NSPB(&sblock)) {
		printf("too many sectors per cylinder (%d sectors)\n",
		    sblock.fs_spc);
		while(sblock.fs_spc > MAXBPG(&sblock) * NSPB(&sblock)) {
			sblock.fs_bsize <<= 1;
			if (sblock.fs_frag < MAXFRAG)
				sblock.fs_frag <<= 1;
			else
				sblock.fs_fsize <<= 1;
		}
		printf("nsect %d, and ntrak %d, requires block size of %d,\n",
		    sblock.fs_nsect, sblock.fs_ntrak, sblock.fs_bsize);
		printf("\tand fragment size of %d\n", sblock.fs_fsize);
		exit(1);
	}
	if (sblock.fs_fpg > MAXBPG(&sblock) * sblock.fs_frag) {
		printf("cylinder group too large (%d cylinders); ",
		    sblock.fs_cpg);
		printf("max: %d cylinders\n",
		    MAXBPG(&sblock) * sblock.fs_frag /
		    (sblock.fs_fpg / sblock.fs_cpg));
		exit(1);
	}
	sblock.fs_cgsize = roundup(sizeof(struct cg) +
	    howmany(sblock.fs_fpg, NBBY), sblock.fs_fsize);
	/*
	 * Compute/validate number of cylinder groups.
	 */
	if (fssize * NSPF(&sblock) > sblock.fs_ncyl * sblock.fs_spc) {
		printf("%d sector(s) in last cylinder unused\n",
		    fssize * NSPF(&sblock) - sblock.fs_ncyl * sblock.fs_spc);
	}
	sblock.fs_ncg = sblock.fs_ncyl / sblock.fs_cpg;
	if (sblock.fs_ncyl % sblock.fs_cpg)
		sblock.fs_ncg++;
	if ((sblock.fs_spc * sblock.fs_cpg) % NSPF(&sblock)) {
		printf("mkfs: nsect %d, ntrak %d, cpg %d is not tolerable\n",
		    sblock.fs_nsect, sblock.fs_ntrak, sblock.fs_cpg);
		printf("as this would would have cyl groups whose size\n");
		printf("is not a multiple of %d; choke!\n", sblock.fs_fsize);
		exit(1);
	}
	fscs = (struct csum *)
	    calloc(1, roundup(sblock.fs_ncg * sizeof (struct csum),
		sblock.fs_bsize));
	/*
	 * Compute number of inode blocks per cylinder group.
	 * Start with one inode per NBPI bytes; adjust as necessary.
	 */
	inos = ((fssize * sblock.fs_fsize) / MAX(NBPI, sblock.fs_fsize)) /
	    INOPB(&sblock);
	if (inos <= 0)
		inos = 1;
	sblock.fs_ipg = ((inos / sblock.fs_ncg) + 1) * INOPB(&sblock);
	if (sblock.fs_ipg > MAXIPG)
		sblock.fs_ipg = MAXIPG;
	if (cgdmin(0,&sblock) >= sblock.fs_fpg) {
		printf("inode blocks/cyl group (%d) >= data blocks (%d)\n",
		    cgdmin(0,&sblock) / sblock.fs_frag,
		    sblock.fs_fpg / sblock.fs_frag);
		printf("number of cylinder per cylinder group must be increased\n");
		exit(1);
	}
	/*
	 * calculate the available blocks for each rotational position
	 */
	for (i = 0; i < NRPOS; i++)
		sblock.fs_postbl[i] = -1;
	for (i = 0; i < sblock.fs_spc; i += NSPB(&sblock))
		/* void */;
	for (i -= NSPB(&sblock); i >= 0; i -= NSPB(&sblock)) {
		c = i % sblock.fs_nsect * NRPOS / sblock.fs_nsect;
		sblock.fs_rotbl[i / NSPB(&sblock)] = sblock.fs_postbl[c];
		sblock.fs_postbl[c] = i / NSPB(&sblock);
	}
	/*
	 * fill in remaining fields of the super block
	 */
	sblock.fs_csaddr = cgdmin(0, &sblock);
	sblock.fs_cssize = sblock.fs_ncg * sizeof(struct csum);
	sblock.fs_rotdelay = ROTDELAY;
	sblock.fs_minfree = MINFREE;
	sblock.fs_magic = FS_MAGIC;
	sblock.fs_sblkno = SBLOCK;
	sblock.fs_cgrotor = 0;
	sblock.fs_cstotal.cs_ndir = 0;
	sblock.fs_cstotal.cs_nbfree = 0;
	sblock.fs_cstotal.cs_nifree = 0;
	sblock.fs_cstotal.cs_nffree = 0;
	sblock.fs_fmod = 0;
	sblock.fs_ronly = 0;
	/*
	 * Dump out summary information about file system.
	 */
	printf("%s:\t%d sectors in %d cylinders of %d tracks, %d sectors\n",
	    fsys, sblock.fs_size * NSPF(&sblock), sblock.fs_ncyl,
	    sblock.fs_ntrak, sblock.fs_nsect);
	printf("\t%.1fMb in %d cyl groups (%d c/g, %.2fMb/g, %d i/g)\n",
	    (float)sblock.fs_size * sblock.fs_fsize * 1e-6, sblock.fs_ncg,
	    sblock.fs_cpg, (float)sblock.fs_fpg * sblock.fs_fsize * 1e-6,
	    sblock.fs_ipg);
	/*
	 * Now build the cylinders group blocks and
	 * then print out indices of cylinder groups.
	 */
	for (c = 0; c < sblock.fs_ncg; c++)
		initcg(c);
	if (sblock.fs_ncg == 1)
		printf("Warning, no super-block backups with only one cylinder group\n");
	else
		printf("\tsuper-block backups (for fsck -b#) at %d+k*%d (%d .. %d)\n",
		    SBLOCK, fsbtodb(&sblock, cgsblock(1, &sblock)) - SBLOCK,
		    fsbtodb(&sblock, cgsblock(1, &sblock)),
		    fsbtodb(&sblock, cgsblock(sblock.fs_ncg - 1, &sblock)));
	/*
	 * Now construct the initial file system,
	 * then write out the super-block.
	 */
	fsinit();
	sblock.fs_time = utime;
	wtfs(SBLOCK, SBSIZE, (char *)&sblock);
	for (i = 0; i < sblock.fs_cssize; i += sblock.fs_bsize)
		wtfs(fsbtodb(&sblock, sblock.fs_csaddr + i / sblock.fs_fsize),
			sblock.fs_bsize, ((char *)fscs) + i);
	/* 
	 * Write out the duplicate super blocks
	 */
	for (c = 1; c < sblock.fs_ncg; c++)
		wtfs(fsbtodb(&sblock, cgsblock(c, &sblock)),
			SBSIZE, (char *)&sblock);
#ifndef STANDALONE
	exit(0);
#endif
}

/*
 * Initialize a cylinder group.
 */
initcg(c)
	int c;
{
	daddr_t cbase, d, dmin, dmax;
	long i, j, s;
	register struct csum *cs;

	/*
	 * Determine block bounds for cylinder group.
	 * Allow space for super block summary information in first
	 * cylinder group.
	 */
	cbase = cgbase(c,&sblock);
	dmax = cbase + sblock.fs_fpg;
	if (dmax > sblock.fs_size)
		dmax = sblock.fs_size;
	dmin = cgdmin(c,&sblock) - cbase;
	d = cbase;
	cs = fscs+c;
	acg.cg_time = utime;
	acg.cg_magic = CG_MAGIC;
	acg.cg_cgx = c;
	acg.cg_ncyl = sblock.fs_cpg;
	acg.cg_niblk = sblock.fs_ipg;
	acg.cg_ndblk = dmax - cbase;
	acg.cg_cs.cs_ndir = 0;
	acg.cg_cs.cs_nffree = 0;
	acg.cg_cs.cs_nbfree = 0;
	acg.cg_cs.cs_nifree = 0;
	acg.cg_rotor = dmin;
	acg.cg_frotor = dmin;
	acg.cg_irotor = 0;
	for (i = 0; i < sblock.fs_frag; i++) {
		acg.cg_frsum[i] = 0;
	}
	for (i = 0; i < sblock.fs_ipg; ) {
		for (j = INOPB(&sblock); j > 0; j--) {
			clrbit(acg.cg_iused, i);
			i++;
		}
		acg.cg_cs.cs_nifree += INOPB(&sblock);
	}
	if (c == 0)
		for (i = 0; i < ROOTINO; i++) {
			setbit(acg.cg_iused, i);
			acg.cg_cs.cs_nifree--;
		}
	while (i < MAXIPG) {
		clrbit(acg.cg_iused, i);
		i++;
	}
	lseek(fso, fsbtodb(&sblock, cgimin(c,&sblock)) * DEV_BSIZE, 0);
	if (write(fso, (char *)zino, sblock.fs_ipg * sizeof (struct dinode)) !=
	    sblock.fs_ipg * sizeof (struct dinode))
		printf("write error %D\n", tell(fso) / sblock.fs_bsize);
	for (i = 0; i < MAXCPG; i++)
		for (j = 0; j < NRPOS; j++)
			acg.cg_b[i][j] = 0;
	if (c == 0) {
		dmin += howmany(sblock.fs_cssize, sblock.fs_bsize) *
			sblock.fs_frag;
	}
	for (d = 0; d < dmin; d += sblock.fs_frag)
		clrblock(&sblock, acg.cg_free, d/sblock.fs_frag);
	while ((d+sblock.fs_frag) <= dmax - cbase) {
		setblock(&sblock, acg.cg_free, d/sblock.fs_frag);
		acg.cg_cs.cs_nbfree++;
		s = d * NSPF(&sblock);
		acg.cg_b[s / sblock.fs_spc]
		    [s % sblock.fs_nsect * NRPOS / sblock.fs_nsect]++;
		d += sblock.fs_frag;
	}
	if (d < dmax - cbase)
		for (; d < dmax - cbase; d++) {
			setbit(acg.cg_free, d);
			acg.cg_cs.cs_nffree++;
		}
	for (; d < MAXBPG(&sblock); d++)
		clrbit(acg.cg_free, d);
	sblock.fs_dsize += acg.cg_ndblk - dmin;
	sblock.fs_cstotal.cs_ndir += acg.cg_cs.cs_ndir;
	sblock.fs_cstotal.cs_nffree += acg.cg_cs.cs_nffree;
	sblock.fs_cstotal.cs_nbfree += acg.cg_cs.cs_nbfree;
	sblock.fs_cstotal.cs_nifree += acg.cg_cs.cs_nifree;
	*cs = acg.cg_cs;
	wtfs(fsbtodb(&sblock, cgtod(c, &sblock)),
		sblock.fs_bsize, (char *)&acg);
}

/*
 * initialize the file system
 */
struct inode node;
#define PREDEFDIR 3
struct direct root_dir[MAXNDIR] = {
	{ ROOTINO, ".", 0, IFDIR },
	{ ROOTINO, "..", 0, IFDIR },
	{ LOSTFOUNDINO, "lost+found", 0, IFDIR },
};
struct direct lost_found_dir[MAXNDIR] = {
	{ LOSTFOUNDINO, ".", 0, IFDIR },
	{ ROOTINO, "..", 0, IFDIR },
};

fsinit()
{
	/*
	 * initialize the node
	 */
	node.i_atime = utime;
	node.i_mtime = utime;
	node.i_ctime = utime;
	/*
	 * create the lost+found directory
	 */
	node.i_number = LOSTFOUNDINO;
	node.i_mode = IFDIR | UMASK;
	node.i_nlink = 2;
	node.i_size = sblock.fs_bsize;
	node.i_db[0] = alloc(node.i_size, node.i_mode);
	wtfs(fsbtodb(&sblock, node.i_db[0]), node.i_size, lost_found_dir);
	iput(&node);
	/*
	 * create the root directory
	 */
	node.i_number = ROOTINO;
	node.i_mode = IFDIR | UMASK;
	node.i_nlink = PREDEFDIR;
	node.i_size = PREDEFDIR * sizeof(struct direct);
	node.i_db[0] = alloc(sblock.fs_fsize, node.i_mode);
	wtfs(fsbtodb(&sblock, node.i_db[0]), sblock.fs_fsize, root_dir);
	iput(&node);
}

/*
 * allocate a block or frag
 */
daddr_t
alloc(size, mode)
	int size;
	int mode;
{
	int c, i, s, frag;
	daddr_t d;

	c = 0;
	rdfs(fsbtodb(&sblock, cgtod(0,&sblock)),
		roundup(sblock.fs_cgsize, DEV_BSIZE), (char *)&acg);
	if (acg.cg_cs.cs_nbfree == 0) {
		printf("first cylinder group ran out of space\n");
		return (0);
	}
	for (d = 0; d < acg.cg_ndblk; d += sblock.fs_frag)
		if (isblock(&sblock, acg.cg_free, d / sblock.fs_frag))
			goto goth;
	printf("internal error: can't find block in cyl 0\n");
	return (0);
goth:
	clrblock(&sblock, acg.cg_free, d / sblock.fs_frag);
	acg.cg_cs.cs_nbfree--;
	sblock.fs_cstotal.cs_nbfree--;
	fscs[0].cs_nbfree--;
	if (mode & IFDIR) {
		acg.cg_cs.cs_ndir++;
		sblock.fs_cstotal.cs_ndir++;
		fscs[0].cs_ndir++;
	}
	s = d * NSPF(&sblock);
	acg.cg_b[s / sblock.fs_spc]
		[s % sblock.fs_nsect * NRPOS / sblock.fs_nsect]--;
	if (size != sblock.fs_bsize) {
		frag = howmany(size, sblock.fs_fsize);
		fscs[0].cs_nffree += sblock.fs_frag - frag;
		sblock.fs_cstotal.cs_nffree += sblock.fs_frag - frag;
		acg.cg_cs.cs_nffree += sblock.fs_frag - frag;
		acg.cg_frsum[sblock.fs_frag - frag]++;
		for (i = frag; i < sblock.fs_frag; i++)
			setbit(acg.cg_free, d+i);
	}
	wtfs(fsbtodb(&sblock, cgtod(0,&sblock)),
		roundup(sblock.fs_cgsize, DEV_BSIZE), (char *)&acg);
	return (d);
}

/*
 * Allocate an inode on the disk
 */
iput(ip)
	register struct inode *ip;
{
	struct dinode buf[MAXINOPB];
	daddr_t d;
	int c;

	c = itog(ip->i_number, &sblock);
	rdfs(fsbtodb(&sblock, cgtod(c,&sblock)),
		roundup(sblock.fs_cgsize, DEV_BSIZE), (char *)&acg);
	acg.cg_cs.cs_nifree--;
	setbit(acg.cg_iused, ip->i_number);
	wtfs(fsbtodb(&sblock, cgtod(c,&sblock)),
		roundup(sblock.fs_cgsize, DEV_BSIZE), (char *)&acg);
	sblock.fs_cstotal.cs_nifree--;
	fscs[0].cs_nifree--;
	if(ip->i_number >= sblock.fs_ipg * sblock.fs_ncg) {
		printf("fsinit: inode value out of range (%d).\n",
		    ip->i_number);
		exit(1);
	}
	d = fsbtodb(&sblock, itod(ip->i_number, &sblock));
	rdfs(d, sblock.fs_bsize, buf);
	buf[itoo(ip->i_number, &sblock)].di_ic = ip->i_ic;
	wtfs(d, sblock.fs_bsize, buf);
}

/*
 * read a block from the file system
 */
rdfs(bno, size, bf)
	daddr_t bno;
	int size;
	char *bf;
{
	int n;

	if (lseek(fsi, bno * DEV_BSIZE, 0) < 0) {
		printf("seek error: %ld\n", bno);
		perror("rdfs");
		exit(1);
	}
	n = read(fsi, bf, size);
	if(n != size) {
		printf("read error: %ld\n", bno);
		perror("rdfs");
		exit(1);
	}
}

/*
 * write a block to the file system
 */
wtfs(bno, size, bf)
	daddr_t bno;
	int size;
	char *bf;
{
	int n;

	lseek(fso, bno * DEV_BSIZE, 0);
	if (lseek(fso, bno * DEV_BSIZE, 0) < 0) {
		printf("seek error: %ld\n", bno);
		perror("wtfs");
		exit(1);
	}
	n = write(fso, bf, size);
	if(n != size) {
		printf("write error: %D\n", bno);
		perror("wtfs");
		exit(1);
	}
}

/*
 * check if a block is available
 */
isblock(fs, cp, h)
	struct fs *fs;
	unsigned char *cp;
	int h;
{
	unsigned char mask;

	switch (fs->fs_frag) {
	case 8:
		return (cp[h] == 0xff);
	case 4:
		mask = 0x0f << ((h & 0x1) << 2);
		return ((cp[h >> 1] & mask) == mask);
	case 2:
		mask = 0x03 << ((h & 0x3) << 1);
		return ((cp[h >> 2] & mask) == mask);
	case 1:
		mask = 0x01 << (h & 0x7);
		return ((cp[h >> 3] & mask) == mask);
	default:
		fprintf(stderr, "isblock bad fs_frag %d\n", fs->fs_frag);
		return;
	}
}

/*
 * take a block out of the map
 */
clrblock(fs, cp, h)
	struct fs *fs;
	unsigned char *cp;
	int h;
{
	switch ((fs)->fs_frag) {
	case 8:
		cp[h] = 0;
		return;
	case 4:
		cp[h >> 1] &= ~(0x0f << ((h & 0x1) << 2));
		return;
	case 2:
		cp[h >> 2] &= ~(0x03 << ((h & 0x3) << 1));
		return;
	case 1:
		cp[h >> 3] &= ~(0x01 << (h & 0x7));
		return;
	default:
		fprintf(stderr, "clrblock bad fs_frag %d\n", fs->fs_frag);
		return;
	}
}

/*
 * put a block into the map
 */
setblock(fs, cp, h)
	struct fs *fs;
	unsigned char *cp;
	int h;
{
	switch (fs->fs_frag) {
	case 8:
		cp[h] = 0xff;
		return;
	case 4:
		cp[h >> 1] |= (0x0f << ((h & 0x1) << 2));
		return;
	case 2:
		cp[h >> 2] |= (0x03 << ((h & 0x3) << 1));
		return;
	case 1:
		cp[h >> 3] |= (0x01 << (h & 0x7));
		return;
	default:
		fprintf(stderr, "setblock bad fs_frag %d\n", fs->fs_frag);
		return;
	}
}
