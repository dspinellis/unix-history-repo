/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)icheck.c	5.7 (Berkeley) %G%";
#endif /* not lint */

/*
 * icheck
 */
#define	NB	500
#define	MAXFN	500
#define	MAXNINDIR	(MAXBSIZE / sizeof (daddr_t))

#include <sys/param.h>
#include <ufs/dinode.h>
#include <ufs/fs.h>
#ifndef STANDALONE
#include <stdio.h>
#endif

union {
	struct	fs sb;
	char pad[SBSIZE];
} sbun;
#define	sblock sbun.sb

union {
	struct	cg cg;
	char pad[MAXBSIZE];
} cgun;
#define	cgrp cgun.cg

struct dinode itab[MAXBSIZE / sizeof(struct dinode)];

daddr_t	blist[NB];
daddr_t	fsblist[NB];
char	*bmap;

int	mflg;
int	dflg;
int	fi;
ino_t	ino;
int	cginit;

ino_t	nrfile;
ino_t	ndfile;
ino_t	nbfile;
ino_t	ncfile;
ino_t	nlfile;
ino_t	nsfile;

daddr_t	nblock;
daddr_t	nfrag;
daddr_t	nindir;
daddr_t	niindir;

daddr_t	nffree;
daddr_t	nbfree;

daddr_t	ndup;

int	nerror;
long	dev_bsize = 1;

long	atol();
#ifndef STANDALONE
char	*malloc();
char	*calloc();
#endif

main(argc, argv)
	int argc;
	char *argv[];
{
	register i;
	long n;

	blist[0] = -1;
#ifndef STANDALONE
	while (--argc) {
		argv++;
		if (**argv=='-')
		switch ((*argv)[1]) {
		case 'd':
			dflg++;
			continue;

		case 'm':
			mflg++;
			continue;

		case 'b':
			for(i=0; i<NB; i++) {
				n = atol(argv[1]);
				if(n == 0)
					break;
				blist[i] = n;
				argv++;
				argc--;
			}
			blist[i] = -1;
			continue;

		default:
			printf("Bad flag\n");
		}
		check(*argv);
	}
#else
	{
		static char fname[128];

		printf("File: ");
		gets(fname);
		check(fname);
	}
#endif
	return(nerror);
}

check(file)
	char *file;
{
	register i, j, c;
	daddr_t d, cgd, cbase, b;
	long n;
	char buf[BUFSIZ];

	fi = open(file, 0);
	if (fi < 0) {
		perror(file);
		nerror |= 04;
		return;
	}
	printf("%s:\n", file);
	nrfile = 0;
	ndfile = 0;
	ncfile = 0;
	nbfile = 0;
	nlfile = 0;
	nsfile = 0;

	nblock = 0;
	nfrag = 0;
	nindir = 0;
	niindir = 0;

	ndup = 0;
#ifndef STANDALONE
	sync();
#endif
	getsb(&sblock, file);
	if (nerror)
		return;
	for (n=0; blist[n] != -1; n++)
		fsblist[n] = dbtofsb(&sblock, blist[n]);
	ino = 0;
	n = roundup(howmany(sblock.fs_size, NBBY), sizeof(short));
#ifdef STANDALONE
	bmap = NULL;
#else
	bmap = malloc((unsigned)n);
#endif
	if (bmap==NULL) {
		printf("Not enough core; duplicates unchecked\n");
		dflg++;
	}
	ino = 0;
	cginit = 1;
	if (!dflg) {
		for (i = 0; i < (unsigned)n; i++)
			bmap[i] = 0;
		for (c = 0; c < sblock.fs_ncg; c++) {
			cgd = cgtod(&sblock, c);
			if (c == 0)
				d = cgbase(&sblock, c);
			else
				d = cgsblock(&sblock, c);
			(void)sprintf(buf, "spare super block %d", c);
			for (; d < cgd; d += sblock.fs_frag)
				chk(d, buf, sblock.fs_bsize);
			d = cgimin(&sblock, c);
			(void)sprintf(buf, "cylinder group %d", c);
			while (cgd < d) {
				chk(cgd, buf, sblock.fs_bsize);
				cgd += sblock.fs_frag;
			}
			d = cgdmin(&sblock, c);
			i = INOPB(&sblock);
			for (; cgd < d; cgd += sblock.fs_frag) {
				(void)sprintf(buf, "inodes %d-%d", ino, ino + i);
				chk(cgd, buf, sblock.fs_bsize);
				ino += i;
			}
			if (c == 0) {
				d += howmany(sblock.fs_cssize, sblock.fs_fsize);
				for (; cgd < d; cgd++)
					chk(cgd, "csum", sblock.fs_fsize);
			}
		}
	}
	ino = 0;
	cginit = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		for (i = 0;
		     i < sblock.fs_ipg / INOPF(&sblock);
		     i += sblock.fs_frag) {
			bread(fsbtodb(&sblock, cgimin(&sblock, c) + i),
			    (char *)itab, sblock.fs_bsize);
			for (j = 0; j < INOPB(&sblock); j++) {
				pass1(&itab[j]);
				ino++;
			}
		}
	}
	ino = 0;
#ifndef STANDALONE
	sync();
#endif
	nffree = 0;
	nbfree = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		cbase = cgbase(&sblock, c);
		bread(fsbtodb(&sblock, cgtod(&sblock, c)), (char *)&cgrp,
			sblock.fs_cgsize);
		if (!cg_chkmagic(&cgrp))
			printf("cg %d: bad magic number\n", c);
		for (b = 0; b < sblock.fs_fpg; b += sblock.fs_frag) {
			if (isblock(&sblock, cg_blksfree(&cgrp),
			    b / sblock.fs_frag)) {
				nbfree++;
				chk(cbase+b, "free block", sblock.fs_bsize);
			} else {
				for (d = 0; d < sblock.fs_frag; d++)
					if (isset(cg_blksfree(&cgrp), b+d)) {
						chk(cbase+b+d, "free frag", sblock.fs_fsize);
						nffree++;
					}
			}
		}
	}
	close(fi);
#ifndef STANDALONE
	if (bmap)
		free(bmap);
#endif

	i = nrfile + ndfile + ncfile + nbfile + nlfile + nsfile;
#ifndef STANDALONE
	printf("files %6u (r=%u,d=%u,b=%u,c=%u,sl=%u,sock=%u)\n",
		i, nrfile, ndfile, nbfile, ncfile, nlfile, nsfile);
#else
	printf("files %u (r=%u,d=%u,b=%u,c=%u,sl=%u,sock=%u)\n",
		i, nrfile, ndfile, nbfile, ncfile, nlfile, nsfile);
#endif
	n = (nblock + nindir + niindir) * sblock.fs_frag + nfrag;
#ifdef STANDALONE
	printf("used %ld (i=%ld,ii=%ld,b=%ld,f=%ld)\n",
		n, nindir, niindir, nblock, nfrag);
	printf("free %ld (b=%ld,f=%ld)\n", nffree + sblock.fs_frag * nbfree,
	    nbfree, nffree);
#else
	printf("used %7ld (i=%ld,ii=%ld,b=%ld,f=%ld)\n",
		n, nindir, niindir, nblock, nfrag);
	printf("free %7ld (b=%ld,f=%ld)\n", nffree + sblock.fs_frag * nbfree,
	    nbfree, nffree);
#endif
	if(!dflg) {
		n = 0;
		for (d = 0; d < sblock.fs_size; d++)
			if(!duped(d, sblock.fs_fsize)) {
				if(mflg)
					printf("%ld missing\n", d);
				n++;
			}
		printf("missing%5ld\n", n);
	}
}

pass1(ip)
	register struct dinode *ip;
{
	daddr_t ind1[MAXNINDIR];
	daddr_t ind2[MAXNINDIR];
	daddr_t db, ib;
	register int i, j, k, siz;
	int lbn;
	char buf[BUFSIZ];

	i = ip->di_mode & IFMT;
	if(i == 0)
		return;
	switch (i) {
	case IFCHR:
		ncfile++;
		return;
	case IFBLK:
		nbfile++;
		return;
	case IFDIR:
		ndfile++;
		break;
	case IFREG:
		nrfile++;
		break;
	case IFSOCK:
		nsfile++;
		break;
	case IFLNK:
		nlfile++;
		break;
	default:
		printf("bad mode %u\n", ino);
		return;
	}
	for (i = 0; i < NDADDR; i++) {
		db = ip->di_db[i];
		if (db == 0)
			continue;
		siz = dblksize(&sblock, ip, i);
		(void)sprintf(buf, "logical data block %d", i);
		chk(db, buf, siz);
		if (siz == sblock.fs_bsize)
			nblock++;
		else
			nfrag += howmany(siz, sblock.fs_fsize);
	}
	for(i = 0; i < NIADDR; i++) {
		ib = ip->di_ib[i];
		if (ib == 0)
			continue;
		if (chk(ib, "1st indirect", sblock.fs_bsize))
			continue;
		bread(fsbtodb(&sblock, ib), (char *)ind1, sblock.fs_bsize);
		nindir++;
		for (j = 0; j < NINDIR(&sblock); j++) {
			ib = ind1[j];
			if (ib == 0)
				continue;
			if (i == 0) {
				lbn = NDADDR + j;
				siz = dblksize(&sblock, ip, lbn);
				(void)sprintf(buf, "logical data block %d", lbn);
				chk(ib, buf, siz);
				if (siz == sblock.fs_bsize)
					nblock++;
				else
					nfrag += howmany(siz, sblock.fs_fsize);
				continue;
			}
			if (chk(ib, "2nd indirect", sblock.fs_bsize))
				continue;
			bread(fsbtodb(&sblock, ib), (char *)ind2,
				sblock.fs_bsize);
			niindir++;
			for (k = 0; k < NINDIR(&sblock); k++) {
				ib = ind2[k];
				if (ib == 0)
					continue;
				lbn = NDADDR + NINDIR(&sblock) * (i + j) + k;
				siz = dblksize(&sblock, ip, lbn);
				(void)sprintf(buf, "logical data block %d", lbn);
				chk(ib, buf, siz);
				if (siz == sblock.fs_bsize)
					nblock++;
				else
					nfrag += howmany(siz, sblock.fs_fsize);
			}
		}
	}
}

chk(bno, s, size)
	daddr_t bno;
	char *s;
	int size;
{
	register n, cg;
	int frags;

	cg = dtog(&sblock, bno);
	if (cginit == 0 && bno >= sblock.fs_frag * sblock.fs_size) {
		printf("%ld bad; inode=%u, class=%s\n", bno, ino, s);
		return(1);
	}
	frags = numfrags(&sblock, size);
	if (frags == sblock.fs_frag) {
		if (duped(bno, size)) {
			printf("%ld dup block; inode=%u, class=%s\n",
			    bno, ino, s);
			ndup += sblock.fs_frag;
		}
	} else {
		for (n = 0; n < frags; n++) {
			if (duped(bno + n, sblock.fs_fsize)) {
				printf("%ld dup frag; inode=%u, class=%s\n",
				    bno, ino, s);
				ndup++;
			}
		}
	}
	for (n=0; blist[n] != -1; n++)
		if (fsblist[n] >= bno && fsblist[n] < bno + frags)
			printf("%ld arg; frag %d of %d, inode=%u, class=%s\n",
				blist[n], fsblist[n] - bno, frags, ino, s);
	return(0);
}

duped(bno, size)
	daddr_t bno;
	int size;
{
	if(dflg)
		return(0);
	if (size != sblock.fs_fsize && size != sblock.fs_bsize)
		printf("bad size %d to duped\n", size);
	if (size == sblock.fs_fsize) {
		if (isset(bmap, bno))
			return(1);
		setbit(bmap, bno);
		return (0);
	}
	if (bno % sblock.fs_frag != 0)
		printf("bad bno %d to duped\n", bno);
	if (isblock(&sblock, bmap, bno/sblock.fs_frag))
		return (1);
	setblock(&sblock, bmap, bno/sblock.fs_frag);
	return(0);
}

getsb(fs, file)
	register struct fs *fs;
	char *file;
{
	int i, j, size;

	if (bread(SBOFF, fs, SBSIZE)) {
		printf("bad super block");
		perror(file);
		nerror |= 04;
		return;
	}
	if (fs->fs_magic != FS_MAGIC) {
		printf("%s: bad magic number\n", file);
		nerror |= 04;
		return;
	}
	dev_bsize = fs->fs_fsize / fsbtodb(fs, 1);
	for (i = 0, j = 0; i < sblock.fs_cssize; i += sblock.fs_bsize, j++) {
		size = sblock.fs_cssize - i < sblock.fs_bsize ?
		    sblock.fs_cssize - i : sblock.fs_bsize;
		sblock.fs_csp[j] = (struct csum *)calloc(1, size);
		bread(fsbtodb(fs, fs->fs_csaddr + (j * fs->fs_frag)),
		      (char *)fs->fs_csp[j], size);
	}
}

bread(bno, buf, cnt)
	daddr_t bno;
	char *buf;
{
	register i;

	lseek(fi, bno * dev_bsize, 0);
	if ((i = read(fi, buf, cnt)) != cnt) {
		for(i=0; i<sblock.fs_bsize; i++)
			buf[i] = 0;
		return (1);
	}
	return (0);
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
#ifdef STANDALONE
		printf("isblock bad fs_frag %d\n", fs->fs_frag);
#else
		fprintf(stderr, "isblock bad fs_frag %d\n", fs->fs_frag);
#endif
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
#ifdef STANDALONE
		printf("setblock bad fs_frag %d\n", fs->fs_frag);
#else
		fprintf(stderr, "setblock bad fs_frag %d\n", fs->fs_frag);
#endif
		return;
	}
}
