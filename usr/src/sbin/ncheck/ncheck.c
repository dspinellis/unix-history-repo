/*-
 * Copyright (c) 1988, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ncheck.c	5.13 (Berkeley) %G%";
#endif /* not lint */

/*
 * ncheck -- obtain file names from reading filesystem
 */

#define	NB		500
#define	MAXNINDIR	(MAXBSIZE / sizeof (daddr_t))

#include <sys/param.h>
#include <ufs/dinode.h>
#include <ufs/fs.h>
#include <sys/dir.h>
#include <stdio.h>

struct	fs	sblock;
struct	dinode	itab[MAXBSIZE/sizeof(struct dinode)];
struct 	dinode	*gip;
struct ilist {
	ino_t	ino;
	u_short	mode;
	short	uid;
	short	gid;
} ilist[NB];
struct	htab
{
	ino_t	h_ino;
	ino_t	h_pino;
	char	*h_name;
} *htab;
char *strngtab;
long hsize;
int strngloc;

struct dirstuff {
	int loc;
	struct dinode *ip;
	char dbuf[MAXBSIZE];
};

int	aflg;
int	sflg;
int	iflg; /* number of inodes being searched for */
int	mflg;
int	fi;
ino_t	ino;
int	nhent;
int	nxfile;
int	dev_bsize = 1;

int	nerror;
daddr_t	bmap();
long	atol();
off_t	lseek();
char	*malloc(), *strcpy();
struct htab *lookup();
struct direct *nreaddir();

main(argc, argv)
	int argc;
	char *argv[];
{
	long n;

	while (--argc) {
		argv++;
		if (**argv=='-')
		switch ((*argv)[1]) {

		case 'a':
			aflg++;
			continue;

		case 'i':
			for(iflg=0; iflg<NB; iflg++) {
				n = atol(argv[1]);
				if(n == 0)
					break;
				ilist[iflg].ino = n;
				nxfile = iflg;
				argv++;
				argc--;
			}
			continue;

		case 'm':
			mflg++;
			continue;

		case 's':
			sflg++;
			continue;

		default:
			(void) fprintf(stderr, "ncheck: bad flag %c\n",
			    (*argv)[1]);
			nerror++;
		}
		check(*argv);
	}
	return(nerror);
}

check(file)
	char *file;
{
	register int i, j, c;

	fi = open(file, 0);
	if(fi < 0) {
		(void) fprintf(stderr, "ncheck: cannot open %s\n", file);
		nerror++;
		return;
	}
	nhent = 0;
	(void) printf("%s:\n", file);
	sync();
	dev_bsize = 1;
	bread(SBOFF, (char *)&sblock, (long)SBSIZE);
	if (sblock.fs_magic != FS_MAGIC) {
		(void) printf("%s: not a file system\n", file);
		nerror++;
		return;
	}
	dev_bsize = sblock.fs_fsize / fsbtodb(&sblock, 1);
	hsize = sblock.fs_ipg * sblock.fs_ncg - sblock.fs_cstotal.cs_nifree + 1;
	htab = (struct htab *)malloc((unsigned)hsize * sizeof(struct htab));
	strngtab = malloc((unsigned)(30 * hsize));
	if (htab == 0 || strngtab == 0) {
		(void) printf("not enough memory to allocate tables\n");
		nerror++;
		return;
	}
	ino = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		for (i = 0;
		     i < sblock.fs_ipg / INOPF(&sblock);
		     i += sblock.fs_frag) {
			bread(fsbtodb(&sblock, cgimin(&sblock, c) + i),
			    (char *)itab, sblock.fs_bsize);
			for (j = 0; j < INOPB(&sblock); j++) {
				if (itab[j].di_mode != 0)
					pass1(&itab[j]);
				ino++;
			}
		}
	}
	ilist[nxfile+1].ino = 0;
	ino = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		for (i = 0;
		     i < sblock.fs_ipg / INOPF(&sblock);
		     i += sblock.fs_frag) {
			bread(fsbtodb(&sblock, cgimin(&sblock, c) + i),
			    (char *)itab, sblock.fs_bsize);
			for (j = 0; j < INOPB(&sblock); j++) {
				if (itab[j].di_mode != 0)
					pass2(&itab[j]);
				ino++;
			}
		}
	}
	ino = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		for (i = 0;
		     i < sblock.fs_ipg / INOPF(&sblock);
		     i += sblock.fs_frag) {
			bread(fsbtodb(&sblock, cgimin(&sblock, c) + i),
			    (char *)itab, sblock.fs_bsize);
			for (j = 0; j < INOPB(&sblock); j++) {
				if (itab[j].di_mode != 0)
					pass3(&itab[j]);
				ino++;
			}
		}
	}
	(void) close(fi);
	for (i = 0; i < hsize; i++)
		htab[i].h_ino = 0;
	for (i = iflg; i < NB; i++)
		ilist[i].ino = 0;
	nxfile = iflg;
}

pass1(ip)
	register struct dinode *ip;
{
	int i;

	if (mflg)
		for (i = 0; i < iflg; i++)
			if (ino == ilist[i].ino) {
				ilist[i].mode = ip->di_mode;
				ilist[i].uid = ip->di_uid;
				ilist[i].gid = ip->di_gid;
			}
	if ((ip->di_mode & IFMT) != IFDIR) {
		if (sflg==0 || nxfile>=NB)
			return;
		if ((ip->di_mode&IFMT)==IFBLK || (ip->di_mode&IFMT)==IFCHR
		  || ip->di_mode&(ISUID|ISGID)) {
			ilist[nxfile].ino = ino;
			ilist[nxfile].mode = ip->di_mode;
			ilist[nxfile].uid = ip->di_uid;
			ilist[nxfile++].gid = ip->di_gid;
			return;
		}
	}
	(void) lookup(ino, 1);
}

pass2(ip)
	register struct dinode *ip;
{
	register struct direct *dp;
	struct dirstuff dirp;
	struct htab *hp;

	if((ip->di_mode&IFMT) != IFDIR)
		return;
	dirp.loc = 0;
	dirp.ip = ip;
	gip = ip;
	for (dp = nreaddir(&dirp); dp != NULL; dp = nreaddir(&dirp)) {
		if(dp->d_ino == 0)
			continue;
		hp = lookup(dp->d_ino, 0);
		if(hp == 0)
			continue;
		if(dotname(dp))
			continue;
		hp->h_pino = ino;
		hp->h_name = &strngtab[strngloc];
		strngloc += strlen(dp->d_name) + 1;
		(void) strcpy(hp->h_name, dp->d_name);
	}
}

pass3(ip)
	register struct dinode *ip;
{
	register struct direct *dp;
	struct dirstuff dirp;
	int k;

	if((ip->di_mode&IFMT) != IFDIR)
		return;
	dirp.loc = 0;
	dirp.ip = ip;
	gip = ip;
	for(dp = nreaddir(&dirp); dp != NULL; dp = nreaddir(&dirp)) {
		if(aflg==0 && dotname(dp))
			continue;
		if(sflg == 0 && iflg == 0)
			goto pr;
		for(k = 0; ilist[k].ino != 0; k++)
			if(ilist[k].ino == dp->d_ino)
				break;
		if (ilist[k].ino == 0)
			continue;
		if (mflg)
			(void) printf("mode %-6o uid %-5d gid %-5d ino ",
			    ilist[k].mode, ilist[k].uid, ilist[k].gid);
	pr:
		(void) printf("%-5lu\t", dp->d_ino);
		pname(ino, 0);
		(void) printf("/%s", dp->d_name);
		if (lookup(dp->d_ino, 0))
			(void) printf("/.");
		(void) printf("\n");
	}
}

/*
 * get next entry in a directory.
 */
struct direct *
nreaddir(dirp)
	register struct dirstuff *dirp;
{
	register struct direct *dp;
	daddr_t lbn, d;

	for(;;) {
		if (dirp->loc >= dirp->ip->di_size)
			return NULL;
		if (blkoff(&sblock, dirp->loc) == 0) {
			lbn = lblkno(&sblock, dirp->loc);
			d = bmap(lbn);
			if(d == 0)
				return NULL;
			bread(fsbtodb(&sblock, d), dirp->dbuf,
			    dblksize(&sblock, dirp->ip, lbn));
		}
		dp = (struct direct *)
		    (dirp->dbuf + blkoff(&sblock, dirp->loc));
		dirp->loc += dp->d_reclen;
		if (dp->d_ino == 0)
			continue;
		return (dp);
	}
}

dotname(dp)
	register struct direct *dp;
{

	if (dp->d_name[0]=='.')
		if (dp->d_name[1]==0 ||
		   (dp->d_name[1]=='.' && dp->d_name[2]==0))
			return(1);
	return(0);
}

pname(i, lev)
	ino_t i;
	int lev;
{
	register struct htab *hp;

	if (i==ROOTINO)
		return;
	if ((hp = lookup(i, 0)) == 0) {
		(void) printf("???");
		return;
	}
	if (lev > 10) {
		(void) printf("...");
		return;
	}
	pname(hp->h_pino, ++lev);
	(void) printf("/%s", hp->h_name);
}

struct htab *
lookup(i, ef)
	ino_t i;
	int ef;
{
	register struct htab *hp;

	for (hp = &htab[i%hsize]; hp->h_ino;) {
		if (hp->h_ino==i)
			return(hp);
		if (++hp >= &htab[hsize])
			hp = htab;
	}
	if (ef==0)
		return(0);
	if (++nhent >= hsize) {
		(void) fprintf(stderr, "ncheck: hsize of %ld is too small\n",
		    hsize);
		exit(1);
	}
	hp->h_ino = i;
	return(hp);
}

bread(bno, buf, lcount)
	daddr_t bno;
	register char *buf;
	long lcount;
{
	register int i, cnt = lcount;
	register off_t off = bno * dev_bsize;

	(void) lseek(fi, off, 0);
	if (read(fi, buf, cnt) != cnt) {
		(void) fprintf(stderr, "ncheck: read error %ld\n", bno);
		if (cnt % dev_bsize) {
			/* THIS INDICATES A SERIOUS BUG */
			/* bzero is probably not correct, but will do */
			(void) fprintf(stderr,
			    "ncheck: bread: cnt %d not multiple of %d\n",
			    cnt, dev_bsize);
			bzero(buf, cnt);
			return;
		}
		for (i = 0; i < cnt; i += dev_bsize) {
			(void) lseek(fi, off, 0);
			if (read(fi, buf, dev_bsize) != dev_bsize) {
				(void) fprintf(stderr,
				    "ncheck: re-read error %ld\n", bno);
				bzero(buf, dev_bsize);
			}
			off += dev_bsize;
			buf += dev_bsize;
			bno++;
		}
	}
}

/*
 * Swiped from standalone sys.c.
 */
#define	NBUFS	4
char	b[NBUFS][MAXBSIZE];
daddr_t	blknos[NBUFS];

daddr_t
bmap(bn)
	register daddr_t bn;
{
	register int j;
	int i, sh;
	daddr_t nb, *bap;

	if (bn < 0) {
		(void) fprintf(stderr, "ncheck: bn %ld negative\n", bn);
		return ((daddr_t)0);
	}

	/*
	 * blocks 0..NDADDR are direct blocks
	 */
	if(bn < NDADDR)
		return(gip->di_db[bn]);

	/*
	 * addresses NIADDR have single and double indirect blocks.
	 * the first step is to determine how many levels of indirection.
	 */
	sh = 1;
	bn -= NDADDR;
	for (j = NIADDR; j > 0; j--) {
		sh *= NINDIR(&sblock);
		if (bn < sh)
			break;
		bn -= sh;
	}
	if (j == 0) {
		(void) printf("ncheck: bn %ld ovf, ino %lu\n", bn, ino);
		return ((daddr_t)0);
	}

	/*
	 * fetch the first indirect block address from the inode
	 */
	nb = gip->di_ib[NIADDR - j];
	if (nb == 0) {
		(void) printf("ncheck: bn %ld void1, ino %lu\n", bn, ino);
		return ((daddr_t)0);
	}

	/*
	 * fetch through the indirect blocks
	 */
	for (; j <= NIADDR; j++) {
		if (blknos[j] != nb) {
			bread(fsbtodb(&sblock, nb), b[j], sblock.fs_bsize);
			blknos[j] = nb;
		}
		bap = (daddr_t *)b[j];
		sh /= NINDIR(&sblock);
		i = (bn / sh) % NINDIR(&sblock);
		nb = bap[i];
		if(nb == 0) {
			(void) printf("ncheck: bn %ld void2, ino %lu\n", bn,
			    ino);
			return ((daddr_t)0);
		}
	}
	return (nb);
}
