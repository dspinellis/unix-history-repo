static	char *sccsid = "@(#)icheck.c	1.9 (Berkeley) %G%";

/*
 * icheck
 */
#define	NB	500
#define	MAXFN	500

#ifndef STANDALONE
#include <stdio.h>
#endif
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"

union {
	struct	fs sb;
	char pad[BSIZE];
} sbun;
#define	sblock sbun.sb

union {
	struct	cg cg;
	char pad[BSIZE];
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
ino_t	nmcfile;

daddr_t	nblock;
daddr_t	nfrag;
daddr_t	nindir;
daddr_t	niindir;

daddr_t	nffree;
daddr_t	nbfree;

daddr_t	ndup;

int	nerror;

extern int inside[], around[];
extern unsigned char fragtbl[];

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
	nmcfile = 0;

	nblock = 0;
	nfrag = 0;
	nindir = 0;
	niindir = 0;

	ndup = 0;
#ifndef STANDALONE
	sync();
#endif
	bread(SBLOCK, (char *)&sblock, BSIZE);
	if (sblock.fs_magic != FS_MAGIC) {
		printf("%s: bad magic number\n", file);
		nerror |= 04;
		return;
	}
	for (n = 0; n < howmany(cssize(&sblock), BSIZE); n++) {
		sblock.fs_csp[n] = (struct csum *)calloc(1, BSIZE);
		bread(csaddr(&sblock) + (n * FRAG),
		      (char *)sblock.fs_csp[n], BSIZE);
	}
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
			cgd = cgtod(c, &sblock);
			for (d = cgbase(c, &sblock); d < cgd; d += FRAG)
				chk(d, "badcg", BSIZE);
			d = cgimin(c, &sblock);
			while (cgd < d) {
				chk(cgd, "cg", BSIZE);
				cgd += FRAG;
			}
			d = cgdmin(c, &sblock);
			for (; cgd < d; cgd += FRAG)
				chk(cgd, "inode", BSIZE);
			if (c == 0) {
				d += howmany(cssize(&sblock), FSIZE);
				for (; cgd < d; cgd += FRAG)
					chk(cgd, "csum", BSIZE);
			}
		}
	}
	cginit = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		bread(cgimin(c,&sblock), (char *)itab,
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
	bread(SBLOCK, (char *)&sblock, sizeof(sblock));
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
		cbase = cgbase(c,&sblock);
		bread(cgtod(c,&sblock), (char *)&cgrp, sblock.fs_cgsize);
		for (b = 0; b < sblock.fs_fpg; b += FRAG) {
			if (isblock(cgrp.cg_free, b / FRAG)) {
				nbfree++;
				chk(cbase+b, "block", BSIZE);
			} else {
				for (d = 0; d < FRAG; d++)
					if (isset(cgrp.cg_free, b+d)) {
						chk(cbase+b+d, "frag", FSIZE);
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

	i = nrfile + ndfile + ncfile + nbfile + nmcfile;
#ifndef STANDALONE
	printf("files %6u (r=%u,d=%u,b=%u,c=%u,mc=%u)\n",
		i, nrfile, ndfile, nbfile, ncfile, nmcfile);
#else
	printf("files %u (r=%u,d=%u,b=%u,c=%u,mc=%u)\n",
		i, nrfile, ndfile, nbfile, ncfile, nmcfile);
#endif
	n = (nblock + nindir + niindir) * FRAG + nfrag;
#ifdef STANDALONE
	printf("used %ld (i=%ld,ii=%ld,b=%ld,f=%ld)\n",
		n, nindir, niindir, nblock, nfrag);
	printf("free %ld (b=%ld,f=%ld)\n", nffree + FRAG * nbfree,
	    nbfree, nffree);
#else
	printf("used %7ld (i=%ld,ii=%ld,b=%ld,f=%ld)\n",
		n, nindir, niindir, nblock, nfrag);
	printf("free %7ld (b=%ld,f=%ld)\n", nffree + FRAG * nbfree,
	    nbfree, nffree);
#endif
	if(!dflg) {
		n = 0;
		for (d = 0; d < sblock.fs_size; d++)
			if(!duped(d, FSIZE)) {
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
	daddr_t ind1[NINDIR];
	daddr_t ind2[NINDIR];
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
	default:
		printf("bad mode %u\n", ino);
		return;
	}
	for (i = 0; i < NDADDR; i++) {
		db = ip->di_db[i];
		if (db == 0)
			continue;
		siz = dblksize(ip, i);
		chk(db, "data (block)", siz);
		if (siz == BSIZE)
			nblock++;
		else
			nfrag += howmany(siz, FSIZE);
	}
	for(i = 0; i < NIADDR; i++) {
		ib = ip->di_ib[i];
		if(ib == 0)
			continue;
		if (chk(ib, "1st indirect", BSIZE))
			continue;
		bread(ib, (char *)ind1, BSIZE);
		nindir++;
		for (j = 0; j < NINDIR; j++) {
			ib = ind1[j];
			if (ib == 0)
				continue;
			if (i == 0) {
				siz = dblksize(ip, NDADDR + j);
				chk(ib, "data (large)", siz);
				if (siz == BSIZE)
					nblock++;
				else
					nfrag += howmany(siz, FSIZE);
				continue;
			}
			if (chk(ib, "2nd indirect", BSIZE))
				continue;
			bread(ib, (char *)ind2, BSIZE);
			niindir++;
			for (k = 0; k < NINDIR; k++) {
				ib = ind2[k];
				if (ib == 0)
					continue;
				siz = dblksize(ip,
				    NDADDR + NINDIR * (i + j) + k);
				chk(ib, "data (huge)", siz);
				if (siz == BSIZE)
					nblock++;
				else
					nfrag += howmany(siz, FSIZE);
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

	cg = dtog(bno, &sblock);
	if (cginit==0 &&
	    bno<cgdmin(cg,&sblock) || bno >= FRAG * sblock.fs_size) {
		printf("%ld bad; inode=%u, class=%s\n", bno, ino, s);
		return(1);
	}
	if (size == BSIZE) {
		if (duped(bno, size)) {
			printf("%ld dup block; inode=%u, class=%s\n",
			    bno, ino, s);
			ndup += FRAG;
		}
	} else {
		for (n = 0; n < size / FSIZE; n++) {
			if (duped(bno + n, FSIZE)) {
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
	if (size != FSIZE && size != BSIZE)
		printf("bad size %d to duped\n", size);
	if (size == FSIZE) {
		if (isset(bmap, bno))
			return(1);
		setbit(bmap, bno);
		return (0);
	}
	if (bno % FRAG != 0)
		printf("bad bno %d to duped\n", bno);
	if (isblock(bmap, bno/FRAG))
		return (1);
	setblock(bmap, bno/FRAG);
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
	for (i = 0; i < howmany(cssize(&sblock), BSIZE); i++) {
		sblock.fs_csp[i] = (struct csum *)calloc(1, BSIZE);
		bread(csaddr(&sblock) + (i * FRAG),
		      (char *)sblock.fs_csp[i], BSIZE);
	}
	for (c = 0; c < sblock.fs_ncg; c++) {
		dbase = cgbase(c, &sblock);
		dmax = dbase + sblock.fs_fpg;
		if (dmax > sblock.fs_size)
			dmax = sblock.fs_size;
		dmin = cgdmin(c, &sblock) - dbase;
		cs = &sblock.fs_cs(c);
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
		for (i = 0; i < FRAG; i++)
			cgrp.cg_frsum[i] = 0;
		bread(cgimin(c, &sblock), (char *)itab,
		      sblock.fs_ipg * sizeof(struct dinode));
		for (i = 0; i < sblock.fs_ipg; i++) {
			dp = &itab[i];
			if (dp == NULL)
				continue;
			if ((dp->di_mode & IFMT) != 0) {
				if ((dp->di_mode & IFMT) == IFDIR)
					cgrp.cg_cs.cs_ndir++;
				setbit(cgrp.cg_iused, i);
				continue;
			}
			cgrp.cg_cs.cs_nifree++;
			clrbit(cgrp.cg_iused, i);
		}
		while (i < MAXIPG) {
			clrbit(cgrp.cg_iused, i);
			i++;
		}
		for (s = 0; s < MAXCPG; s++)
			for (i = 0; i < NRPOS; i++)
				cgrp.cg_b[s][i] = 0;
		if (c == 0) {
			dmin += howmany(cssize(&sblock), BSIZE) * FRAG;
		}
		for (d = 0; d < dmin; d++)
			clrbit(cgrp.cg_free, d);
		for (; (d + FRAG) <= dmax - dbase; d += FRAG) {
			j = 0;
			for (i = 0; i < FRAG; i++) {
				if (!isset(bmap, dbase+d+i)) {
					setbit(cgrp.cg_free, d+i);
					j++;
				} else
					clrbit(cgrp.cg_free, d+i);
			}
			if (j == FRAG) {
				cgrp.cg_cs.cs_nbfree++;
				s = d * NSPF;
				cgrp.cg_b[s/sblock.fs_spc]
				  [s%sblock.fs_nsect*NRPOS/sblock.fs_nsect]++;
			} else if (j > 0) {
				cgrp.cg_cs.cs_nffree += j;
				blk = ((cgrp.cg_free[d / NBBY] >> (d % NBBY)) &
				       (0xff >> (NBBY - FRAG)));
				fragacct(blk, cgrp.cg_frsum, 1);
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
			       (0xff >> (NBBY - FRAG)));
			fragacct(blk, cgrp.cg_frsum, 1);
		}
		for (; d < MAXBPG; d++)
			clrbit(cgrp.cg_free, d);
		sblock.fs_cstotal.cs_nffree += cgrp.cg_cs.cs_nffree;
		sblock.fs_cstotal.cs_nbfree += cgrp.cg_cs.cs_nbfree;
		sblock.fs_cstotal.cs_nifree += cgrp.cg_cs.cs_nifree;
		sblock.fs_cstotal.cs_ndir += cgrp.cg_cs.cs_ndir;
		*cs = cgrp.cg_cs;
		bwrite(cgtod(c, &sblock), &cgrp, sblock.fs_cgsize);
	}
	for (i = 0; i < howmany(cssize(&sblock), BSIZE); i++) {
		bwrite(csaddr(&sblock) + (i * FRAG),
		       (char *)sblock.fs_csp[i], BSIZE);
	}
	sblock.fs_ronly = 0;
	sblock.fs_fmod = 0;
	bwrite(SBLOCK, (char *)&sblock, sizeof(sblock));
}

/*
 * update the frsum fields to reflect addition or deletion 
 * of some frags
 */
fragacct(fragmap, fraglist, cnt)
	int fragmap;
	long fraglist[];
	int cnt;
{
	int inblk;
	register int field, subfield;
	register int siz, pos;

	inblk = (int)(fragtbl[fragmap] << 1);
	fragmap <<= 1;
	for (siz = 1; siz < FRAG; siz++) {
		if (((1 << siz) & inblk) == 0)
			continue;
		field = around[siz];
		subfield = inside[siz];
		for (pos = siz; pos <= FRAG; pos++) {
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

bwrite(blk, buf, size)
	char *buf;
	daddr_t blk;
	register size;
{
	if (lseek(fi, blk * FSIZE, 0) < 0) {
		perror("FS SEEK");
		return(1);
	}
	if (write(fi, buf, size) != size) {
		perror("FS WRITE");
		return(1);
	}
}

bread(bno, buf, cnt)
	daddr_t bno;
	char *buf;
{
	register i;

	lseek(fi, bno * FSIZE, 0);
	if ((i = read(fi, buf, cnt)) != cnt) {
		if (sflg) {
			printf("No Update\n");
			sflg = 0;
		}
		for(i=0; i<BSIZE; i++)
			buf[i] = 0;
	}
}
