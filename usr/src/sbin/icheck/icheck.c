static	char *sccsid = "@(#)icheck.c	1.3 (Berkeley) %G%";
/*
 * icheck
 */
#define	NB	500
#define	BITS	8
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

int	sflg;
int	mflg;
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
long	szfrag;
daddr_t	nindir;
long	szindir;
daddr_t	niindir;

daddr_t	nffree;
long	szffree;
daddr_t	nbfree;

daddr_t	ndup;

int	nerror;

long	atol();
daddr_t	alloc();
#ifndef STANDALONE
char	*malloc();
#endif

main(argc, argv)
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

	fi = open(file, sflg?2:0);
	if (fi < 0) {
		printf("cannot open %s\n", file);
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
	szfrag = 0;
	nindir = 0;
	szindir = 0;
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
	sblock.fs_cs =
	    (struct csum *)calloc(howmany(cssize(&sblock), BSIZE), BSIZE);
	lseek(fi, csaddr(&sblock)*FSIZE, 0);
	read(fi, (char *)sblock.fs_cs, cssize(&sblock));
	ino = 0;
	n = (sblock.fs_size*FRAG + BITS-1) / BITS;
#ifdef STANDALONE
	bmap = NULL;
#else
	bmap = malloc((unsigned)n);
#endif
	if (bmap==NULL) {
		printf("Not enough core; duplicates unchecked\n");
		dflg++;
		sflg = 0;
	}
	ino = 0;
	cginit = 1;
	if(!dflg) {
		for (i=0; i<(unsigned)n; i++)
			bmap[i] = 0;
		for (c=0; c < sblock.fs_ncg; c++) {
			cgd = cgtod(c, &sblock);
			for (d = cgbase(c, &sblock); d < cgd; d++)
				chk(d, "badcg");
			d = cgimin(c, &sblock);
			while (cgd < d) {
				chk(cgd, "cg");
				cgd++;
			}
			d = cgdmin(c, &sblock);
			for (; cgd < d; cgd++)
				chk(cgd, "inode");
			if (c == 0) {
				d += howmany(cssize(&sblock), BSIZE) * FRAG;
				for (; cgd < d; cgd++)
					chk(cgd, "csum");
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
	szffree = 0;
	nbfree = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		cbase = cgbase(c,&sblock);
		bread(cgtod(c,&sblock), (char *)&cgrp, sblock.fs_cgsize);
		for (b = 0; b < sblock.fs_fpg; b += FRAG) {
			if (isblock(cgrp.cg_free, b / FRAG)) {
				nbfree++;
				for (d = 0; d < FRAG; d++)
					chk(cbase+b+d, "block");
			} else {
				for (d = 0; d < FRAG; d++)
					if (isset(cgrp.cg_free, b+d)) {
						chk(cbase+b+d, "frag");
						nffree++;
						szffree++;
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
	n = (nblock + niindir) * FRAG + szfrag + szindir;
#ifdef STANDALONE
	printf("used %ld (i=%ld,ii=%ld,b=%ld,f=%ld)\n",
		n, nindir, niindir, nblock, nfrag);
	printf("free %ld (b=%ld,f=%ld)\n", szffree + FRAG * nbfree,
	    nbfree, nffree);
#else
	printf("used %7ld (i=%ld,ii=%ld,b=%ld,f=%ld)\n",
		n, nindir, niindir, nblock, nfrag);
	printf("free %7ld (b=%ld,f=%ld)\n", szffree + FRAG * nbfree,
	    nbfree, nffree);
#endif
	if(!dflg) {
		n = 0;
		for(d=0; d<sblock.fs_size; d++)
			if(!duped(d)) {
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
	register i, j;
	int k, l, sz, ni, nib, ndb;

	i = ip->di_mode & IFMT;
	if(i == 0) {
		sblock.fs_nifree++;
		return;
	}
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
	ndb = howmany(ip->di_size, BSIZE)-1;
	for(i=0; i<NDADDR; i++) {
		if(ip->di_db[i] == 0)
			continue;
		if (i==ndb && (ip->di_size&BMASK)) {
			sz = howmany(ip->di_size - i * BSIZE, FSIZE);
			for (l = 0; l < sz; l++)
				chk(ip->di_db[i]+l, "data (frag)");
			szfrag += sz;
			nfrag++;
		} else {
			for (l = 0; l < FRAG; l++)
				chk(ip->di_db[i]+l, "data (block)");
			nblock++;
		}
	}
	for(i=NDADDR; i<NDADDR+NIADDR;i++) {
		if(ip->di_ib[i] == 0)
			continue;
		nindir++;
		if (i==NDADDR) {
			sz = howmany((ip->di_size-NDADDR*BSIZE),
			    (NINDIR/FRAG) * BSIZE);
			if (sz > FRAG)
				sz = FRAG;
		} else
			sz = FRAG;
		for (j = 0; j < FRAG; j++)
			if (chk(ip->di_ib[i]+j, "1st indirect"))
				continue;
		bread(ip->di_ib[i], (char *)ind1, sz*FSIZE);
		nib = sz * (NINDIR/FRAG);
		for(j=0; j<nib; j++) {
			if(ind1[j] == 0)
				continue;
			if(i == NDADDR) {
				for (l = 0; l < FRAG; l++)
					chk(ind1[j]+l, "data (large)");
				nblock++;
				continue;
			}
			niindir++;
			for (l = 0; l < FRAG; l++)
				if(chk(ind1[j], "2nd indirect"))
					goto skip;
			bread(ind1[j], (char *)ind2, BSIZE);
			for(k=0; k<NINDIR; k++) {
				if(ind2[k] == 0)
					continue;
				for (l = 0; l < FRAG; l++)
					chk(ind1[j]+l, "data (huge)");
				nblock++;
				continue;
			}
skip:
			;
		}
	}
}

chk(bno, s)
daddr_t bno;
char *s;
{
	register n, cg;

	cg = dtog(bno, &sblock);
	if (cginit==0 && bno<cgdmin(cg,&sblock) || bno>=FRAG*sblock.fs_size) {
		printf("%ld bad; inode=%u, class=%s\n", bno, ino, s);
		return(1);
	}
	if (duped(bno)) {
		printf("%ld dup; inode=%u, class=%s\n", bno, ino, s);
		ndup++;
	}
	for (n=0; blist[n] != -1; n++)
		if (bno == blist[n])
			printf("%ld arg; inode=%u, class=%s\n", bno, ino, s);
	return(0);
}

duped(bno)
daddr_t bno;
{
	daddr_t d;
	register m, n;

	if(dflg)
		return(0);
	m = 1 << (bno%BITS);
	n = (bno/BITS);
	if(bmap[n] & m)
		return(1);
	bmap[n] |= m;
	return(0);
}

bread(bno, buf, cnt)
daddr_t bno;
char *buf;
{
	register i;

	lseek(fi, bno*FSIZE, 0);
	if ((i = read(fi, buf, cnt)) != cnt) {
		if (sflg) {
			printf("No update\n");
			sflg = 0;
		}
		for(i=0; i<BSIZE; i++)
			buf[i] = 0;
	}
}

bwrite(bno, buf, cnt)
daddr_t bno;
char *buf;
{
	register i;

	lseek(fi, bno*FSIZE, 0);
	if (write(fi, buf, cnt) != cnt)
		printf("write error %d\n", tell(fi)/BSIZE);
}

makecg()
{
	int c;
	daddr_t dbase, d, dmin, dmax;
	long i, j, s;
	register struct csum *cs;

	sblock.fs_nbfree = 0;
	sblock.fs_nffree = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		bread(cgimin(c,&sblock), (char *)itab,
		    sblock.fs_ipg * sizeof (struct dinode));
		dbase = cgbase(c, &sblock);
		dmax = dbase + sblock.fs_fpg;
		if (dmax > sblock.fs_size)
			dmax = sblock.fs_size;
		cs = &sblock.fs_cs[c];
		cgrp.cg_time = time(0);
		cgrp.cg_magic = CG_MAGIC;
		cgrp.cg_cgx = c;
		cgrp.cg_ncyl = sblock.fs_cpg;
		cgrp.cg_niblk = sblock.fs_ipg;
		cgrp.cg_ndblk = dmax - dbase;
		cgrp.cg_ndir = 0;
		cgrp.cg_nffree = 0;
		cgrp.cg_nbfree = 0;
		cgrp.cg_nifree = 0;
		for (i = 0; i < sblock.fs_ipg; i++)
		switch (itab[i].di_mode&IFMT) {

		case 0:
			cgrp.cg_nifree++;
			clrbit(cgrp.cg_iused, i);
			continue;

		case IFDIR:
			cgrp.cg_ndir++;
			/* fall into ... */

		default:
			setbit(cgrp.cg_iused, i);
			continue;
		}
		while (i < MAXIPG) {
			clrbit(cgrp.cg_iused, i);
			i++;
		}
		for (s = 0; s < MAXCPG; s++)
			for (i = 0; i < NRPOS; i++)
				cgrp.cg_b[s][i] = 0;
		dmin = cgdmin(c, &sblock) - dbase;
		if (c == 0)
			dmin += howmany(cssize(&sblock), BSIZE) * FRAG;
		for (d = 0; d < dmin; d++)
			clrbit(cgrp.cg_free, d);
#define	getbmap(i) isset(bmap, i)
		for (; (d + FRAG) <= dmax - dbase; d += FRAG) {
			j = 0;
			for (i = 0; i < FRAG; i++) {
				if (!getbmap(dbase+d+i)) {
					setbit(cgrp.cg_free, d+i);
					j++;
				} else
					clrbit(cgrp.cg_free, d+i);
			}
			if (j == FRAG) {
				cgrp.cg_nbfree++;
				s = d * NSPF;
				cgrp.cg_b[s/sblock.fs_spc]
				  [s%sblock.fs_nsect*NRPOS/sblock.fs_nsect]++;
			} else
				cgrp.cg_nffree += j;
		}
		for (; d < dmax - dbase; d++) {
			if (!getbmap(dbase+d)) {
				setbit(cgrp.cg_free, d);
				cgrp.cg_nffree++;
			} else
				clrbit(cgrp.cg_free, d);
		}
		for (; d < MAXBPG; d++)
			clrbit(cgrp.cg_free, d);
		sblock.fs_nffree += cgrp.cg_nffree;
		sblock.fs_nbfree += cgrp.cg_nbfree;
		cs->cs_ndir = cgrp.cg_ndir;
		cs->cs_nifree = cgrp.cg_nifree;
		cs->cs_nbfree = cgrp.cg_nbfree;
		bwrite(cgtod(c, &sblock), (char *)&cgrp, sblock.fs_cgsize);
	}
	sblock.fs_ronly = 0;
	sblock.fs_fmod = 0;
	bwrite(SBLOCK, (char *)&sblock, sizeof (sblock));
	lseek(fi, csaddr(&sblock)*FSIZE, 0);
	if (write(fi,(char *)sblock.fs_cs,cssize(&sblock)) != cssize(&sblock))
		printf("write error %d\n", tell(fi)/BSIZE);
}
