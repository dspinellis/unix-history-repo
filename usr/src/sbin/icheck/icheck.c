static	char *sccsid = "@(#)icheck.c	1.20 (Berkeley) %G%";

/*
 * icheck
 */
#define	NB	500
#define	MAXFN	500
#define	MAXNINDIR	(MAXBSIZE / sizeof (daddr_t))

#ifndef STANDALONE
#include <stdio.h>
#endif
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"

union {
	struct	fs sb;
	char pad[MAXBSIZE];
} sbun;
#define	sblock sbun.sb

union {
	struct	cg cg;
	char pad[MAXBSIZE];
} cgun;
#define	cgrp cgun.cg

struct	dinode	itab[MAXIPG];
daddr_t	blist[NB];
char	*bmap;

int	mflg;
int	sflg;
int	dflg;
int	fi;
ino_t	ino;
int	cginit;

ino_t	nrfile;
ino_t	ndfile;
ino_t	nbfile;
ino_t	ncfile;
ino_t	nlfile;

daddr_t	nblock;
daddr_t	nfrag;
daddr_t	nindir;
daddr_t	niindir;

daddr_t	nffree;
daddr_t	nbfree;

daddr_t	ndup;

int	nerror;

extern int inside[], around[];
extern unsigned char *fragtbl[];

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

		case 's':
			sflg++;
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

	fi = open(file, sflg ? 2 : 0);
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
		if (sflg) {
			printf("No Updates\n");
			sflg = 0;
		}
	}
	ino = 0;
	cginit = 1;
	if(!dflg) {
		for (i=0; i<(unsigned)n; i++)
			bmap[i] = 0;
		for (c=0; c < sblock.fs_ncg; c++) {
			cgd = cgtod(&sblock, c);
			for (d = cgbase(&sblock, c); d < cgd; d += sblock.fs_frag)
				chk(d, "badcg", sblock.fs_bsize);
			d = cgimin(&sblock, c);
			while (cgd < d) {
				chk(cgd, "cg", sblock.fs_bsize);
				cgd += sblock.fs_frag;
			}
			d = cgdmin(&sblock, c);
			for (; cgd < d; cgd += sblock.fs_frag)
				chk(cgd, "inode", sblock.fs_bsize);
			if (c == 0) {
				d += howmany(sblock.fs_cssize, sblock.fs_bsize)
				    * sblock.fs_frag;
				for (; cgd < d; cgd += sblock.fs_frag)
					chk(cgd, "csum", sblock.fs_bsize);
			}
		}
	}
	cginit = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		bread(fsbtodb(&sblock, cgimin(&sblock, c)), (char *)itab,
		    sblock.fs_ipg * sizeof (struct dinode));
		for (j=0; j < sblock.fs_ipg; j++) {
			pass1(&itab[j]);
			ino++;
		}
	}
	ino = 0;
#ifndef STANDALONE
	sync();
#endif
	if (sflg) {
		makecg();
		close(fi);
#ifndef STANDALONE
		if (bmap)
			free(bmap);
#endif
		return;
	}
	nffree = 0;
	nbfree = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		cbase = cgbase(&sblock, c);
		bread(fsbtodb(&sblock, cgtod(&sblock, c)), (char *)&cgrp,
			sblock.fs_cgsize);
		for (b = 0; b < sblock.fs_fpg; b += sblock.fs_frag) {
			if (isblock(&sblock, cgrp.cg_free,
			    b / sblock.fs_frag)) {
				nbfree++;
				chk(cbase+b, "block", sblock.fs_bsize);
			} else {
				for (d = 0; d < sblock.fs_frag; d++)
					if (isset(cgrp.cg_free, b+d)) {
						chk(cbase+b+d, "frag", sblock.fs_fsize);
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

	i = nrfile + ndfile + ncfile + nbfile + nlfile;
#ifndef STANDALONE
	printf("files %6u (r=%u,d=%u,b=%u,c=%u,sl=%u)\n",
		i, nrfile, ndfile, nbfile, ncfile, nlfile);
#else
	printf("files %u (r=%u,d=%u,b=%u,c=%u,sl=%u)\n",
		i, nrfile, ndfile, nbfile, ncfile, nlfile);
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
		chk(db, "data (block)", siz);
		if (siz == sblock.fs_bsize)
			nblock++;
		else
			nfrag += howmany(siz, sblock.fs_fsize);
	}
	for(i = 0; i < NIADDR; i++) {
		ib = ip->di_ib[i];
		if(ib == 0)
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
				siz = dblksize(&sblock, ip, NDADDR + j);
				chk(ib, "data (large)", siz);
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
				siz = dblksize(&sblock, ip,
				    NDADDR + NINDIR(&sblock) * (i + j) + k);
				chk(ib, "data (huge)", siz);
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
	if (cginit==0 &&
	    bno<cgdmin(&sblock, cg) || bno >= sblock.fs_frag * sblock.fs_size) {
		printf("%ld bad; inode=%u, class=%s\n", bno, ino, s);
		return(1);
	}
	if (size == sblock.fs_bsize) {
		if (duped(bno, size)) {
			printf("%ld dup block; inode=%u, class=%s\n",
			    bno, ino, s);
			ndup += sblock.fs_frag;
		}
	} else {
		frags = numfrags(&sblock, size);
		for (n = 0; n < frags; n++) {
			if (duped(bno + n, sblock.fs_fsize)) {
				printf("%ld dup frag; inode=%u, class=%s\n",
				    bno, ino, s);
				ndup++;
			}
		}
	}
	for (n=0; blist[n] != -1; n++)
		if (bno == blist[n])
			printf("%ld arg; inode=%u, class=%s\n", bno, ino, s);
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

makecg()
{
	int c, blk;
	daddr_t dbase, d, dmin, dmax;
	long i, j, s;
	register struct csum *cs;
	register struct dinode *dp;

	sblock.fs_cstotal.cs_nbfree = 0;
	sblock.fs_cstotal.cs_nffree = 0;
	sblock.fs_cstotal.cs_nifree = 0;
	sblock.fs_cstotal.cs_ndir = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		dbase = cgbase(&sblock, c);
		dmax = dbase + sblock.fs_fpg;
		if (dmax > sblock.fs_size) {
			for ( ; dmax >= sblock.fs_size; dmax--)
				clrbit(cgrp.cg_free, dmax - dbase);
			dmax++;
		}
		dmin = sblock.fs_dblkno;
		cs = &sblock.fs_cs(&sblock, c);
		cgrp.cg_time = time(0);
		cgrp.cg_magic = CG_MAGIC;
		cgrp.cg_cgx = c;
		cgrp.cg_ncyl = sblock.fs_cpg;
		cgrp.cg_niblk = sblock.fs_ipg;
		cgrp.cg_ndblk = dmax - dbase;
		cgrp.cg_cs.cs_ndir = 0;
		cgrp.cg_cs.cs_nffree = 0;
		cgrp.cg_cs.cs_nbfree = 0;
		cgrp.cg_cs.cs_nifree = 0;
		cgrp.cg_rotor = dmin;
		cgrp.cg_frotor = dmin;
		cgrp.cg_irotor = 0;
		for (i = 0; i < sblock.fs_frag; i++)
			cgrp.cg_frsum[i] = 0;
		bread(fsbtodb(&sblock, cgimin(&sblock, c)), (char *)itab,
		      sblock.fs_ipg * sizeof(struct dinode));
		for (i = 0; i < sblock.fs_ipg; i++) {
			cgrp.cg_cs.cs_nifree++;
			clrbit(cgrp.cg_iused, i);
			dp = &itab[i];
			if ((dp->di_mode & IFMT) != 0) {
				if ((dp->di_mode & IFMT) == IFDIR)
					cgrp.cg_cs.cs_ndir++;
				cgrp.cg_cs.cs_nifree--;
				setbit(cgrp.cg_iused, i);
				continue;
			}
		}
		while (i < MAXIPG) {
			clrbit(cgrp.cg_iused, i);
			i++;
		}
		if (c == 0)
			for (i = 0; i < ROOTINO; i++) {
				setbit(cgrp.cg_iused, i);
				cgrp.cg_cs.cs_nifree--;
			}
		for (s = 0; s < MAXCPG; s++) {
			cgrp.cg_btot[s] = 0;
			for (i = 0; i < NRPOS; i++)
				cgrp.cg_b[s][i] = 0;
		}
		if (c == 0) {
			dmin += howmany(sblock.fs_cssize, sblock.fs_bsize) *
			    sblock.fs_frag;
		}
		for (d = 0; d < dmin; d++)
			clrbit(cgrp.cg_free, d);
		for (; (d + sblock.fs_frag) <= dmax - dbase; d += sblock.fs_frag) {
			j = 0;
			for (i = 0; i < sblock.fs_frag; i++) {
				if (!isset(bmap, dbase+d+i)) {
					setbit(cgrp.cg_free, d+i);
					j++;
				} else
					clrbit(cgrp.cg_free, d+i);
			}
			if (j == sblock.fs_frag) {
				cgrp.cg_cs.cs_nbfree++;
				cgrp.cg_btot[cbtocylno(&sblock, d)]++;
				cgrp.cg_b[cbtocylno(&sblock, d)]
				    [cbtorpos(&sblock, d)]++;
			} else if (j > 0) {
				cgrp.cg_cs.cs_nffree += j;
				blk = ((cgrp.cg_free[d / NBBY] >> (d % NBBY)) &
				       (0xff >> (NBBY - sblock.fs_frag)));
				fragacct(&sblock, blk, cgrp.cg_frsum, 1);
			}
		}
		for (j = d; d < dmax - dbase; d++) {
			if (!isset(bmap, dbase+d)) {
				setbit(cgrp.cg_free, d);
				cgrp.cg_cs.cs_nffree++;
			} else
				clrbit(cgrp.cg_free, d);
		}
		if (j != d) {
			blk = ((cgrp.cg_free[j / NBBY] >> (j % NBBY)) &
			       (0xff >> (NBBY - sblock.fs_frag)));
			fragacct(&sblock, blk, cgrp.cg_frsum, 1);
		}
		for (; d < MAXBPG(&sblock); d++)
			clrbit(cgrp.cg_free, d);
		sblock.fs_cstotal.cs_nffree += cgrp.cg_cs.cs_nffree;
		sblock.fs_cstotal.cs_nbfree += cgrp.cg_cs.cs_nbfree;
		sblock.fs_cstotal.cs_nifree += cgrp.cg_cs.cs_nifree;
		sblock.fs_cstotal.cs_ndir += cgrp.cg_cs.cs_ndir;
		*cs = cgrp.cg_cs;
		bwrite(fsbtodb(&sblock, cgtod(&sblock, c)), &cgrp,
			sblock.fs_cgsize);
	}
	for (i = 0; i < howmany(sblock.fs_cssize, sblock.fs_bsize); i++) {
		bwrite(fsbtodb(&sblock,
		    sblock.fs_csaddr + (i * sblock.fs_frag)),
		    (char *)sblock.fs_csp[i], sblock.fs_bsize);
	}
	sblock.fs_ronly = 0;
	sblock.fs_fmod = 0;
	bwrite(SBLOCK, (char *)&sblock, SBSIZE);
}

/*
 * update the frsum fields to reflect addition or deletion 
 * of some frags
 */
fragacct(fs, fragmap, fraglist, cnt)
	struct fs *fs;
	int fragmap;
	long fraglist[];
	int cnt;
{
	int inblk;
	register int field, subfield;
	register int siz, pos;

	inblk = (int)(fragtbl[fs->fs_frag][fragmap] << 1);
	fragmap <<= 1;
	for (siz = 1; siz < fs->fs_frag; siz++) {
		if (((1 << siz) & inblk) == 0)
			continue;
		field = around[siz];
		subfield = inside[siz];
		for (pos = siz; pos <= fs->fs_frag; pos++) {
			if ((fragmap & field) == subfield) {
				fraglist[siz] += cnt;
				pos += siz;
				field <<= siz;
				subfield <<= siz;
			}
			field <<= 1;
			subfield <<= 1;
		}
	}
}

getsb(fs, file)
	register struct fs *fs;
	char *file;
{
	int i;

	if (bread(SBLOCK, fs, SBSIZE)) {
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
	for (i = 0; i < howmany(fs->fs_cssize, fs->fs_bsize); i++) {
		fs->fs_csp[i] = (struct csum *)calloc(1, fs->fs_bsize);
		bread(fsbtodb(fs, fs->fs_csaddr + (i * fs->fs_frag)),
		      (char *)fs->fs_csp[i], fs->fs_bsize);
	}
}

bwrite(blk, buf, size)
	char *buf;
	daddr_t blk;
	register size;
{
	if (lseek(fi, blk * DEV_BSIZE, 0) < 0) {
		perror("FS SEEK");
		return(1);
	}
	if (write(fi, buf, size) != size) {
		perror("FS WRITE");
		return(1);
	}
	return (0);
}

bread(bno, buf, cnt)
	daddr_t bno;
	char *buf;
{
	register i;

	lseek(fi, bno * DEV_BSIZE, 0);
	if ((i = read(fi, buf, cnt)) != cnt) {
		if (sflg) {
			printf("No Update\n");
			sflg = 0;
		}
		for(i=0; i<sblock.fs_bsize; i++)
			buf[i] = 0;
		return (1);
	}
	return (0);
}

/*
 * block operations
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
