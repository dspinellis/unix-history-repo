static	char *sccsid = "@(#)ncheck.c	1.5 (Berkeley) %G%";
/*
 * ncheck -- obtain file names from reading filesystem
 */

#define	NB		500
#define	HSIZE		2503
#define	NDIR(fs)	((fs)->fs_bsize/sizeof(struct direct))
#define	MAXNDIR		(MAXBSIZE/sizeof(struct direct))
#define	MAXNINDIR	(MAXBSIZE / sizeof (daddr_t))

#include <stdio.h>
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/fs.h"

struct	fs	sblock;
struct	dinode	itab[MAXIPG];
struct 	dinode	*gip;
ino_t	ilist[NB];
struct	htab
{
	ino_t	h_ino;
	ino_t	h_pino;
	char	h_name[DIRSIZ];
} htab[HSIZE];

int	aflg;
int	sflg;
int	fi;
ino_t	ino;
int	nhent;
int	nxfile;

int	nerror;
daddr_t	bmap();
long	atol();
struct htab *lookup();

main(argc, argv)
	int argc;
	char *argv[];
{
	register i;
	long n;

	while (--argc) {
		argv++;
		if (**argv=='-')
		switch ((*argv)[1]) {

		case 'a':
			aflg++;
			continue;

		case 'i':
			for(i=0; i<NB; i++) {
				n = atol(argv[1]);
				if(n == 0)
					break;
				ilist[i] = n;
				nxfile = i;
				argv++;
				argc--;
			}
			continue;

		case 's':
			sflg++;
			continue;

		default:
			fprintf(stderr, "ncheck: bad flag %c\n", (*argv)[1]);
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
	int nfiles;

	fi = open(file, 0);
	if(fi < 0) {
		fprintf(stderr, "ncheck: cannot open %s\n", file);
		nerror++;
		return;
	}
	nhent = 0;
	printf("%s:\n", file);
	sync();
	bread(SBLOCK, (char *)&sblock, SBSIZE);
	if (sblock.fs_magic != FS_MAGIC) {
		printf("%s: not a file system\n", file);
		nerror++;
		return;
	}
	nfiles = sblock.fs_ipg * sblock.fs_ncg;
	if (nfiles > 65535) {
		printf("%s: %d is a preposterous number of files\n",
		    file, nfiles);
		nerror++;
		return;
	}
	ino = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		bread(fsbtodb(&sblock, cgimin(c, &sblock)), (char *)itab,
		    sblock.fs_ipg * sizeof (struct dinode));
		for(j=0; j<sblock.fs_ipg; j++) {
			pass1(&itab[j]);
			ino++;
		}
	}
	ilist[nxfile+1] = 0;
	ino = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		bread(fsbtodb(&sblock, cgimin(c, &sblock)), (char *)itab,
		    sblock.fs_ipg * sizeof (struct dinode));
		for(j=0; j<sblock.fs_ipg; j++) {
			pass2(&itab[j]);
			ino++;
		}
	}
	ino = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		bread(fsbtodb(&sblock, cgimin(c, &sblock)), (char *)itab,
		    sblock.fs_ipg * sizeof (struct dinode));
		for(j=0; j<sblock.fs_ipg; j++) {
			pass3(&itab[j]);
			ino++;
		}
	}
}

pass1(ip)
	register struct dinode *ip;
{
	if((ip->di_mode & IFMT) != IFDIR) {
		if (sflg==0 || nxfile>=NB)
			return;
		if ((ip->di_mode&IFMT)==IFBLK || (ip->di_mode&IFMT)==IFCHR
		  || ip->di_mode&(ISUID|ISGID))
			ilist[nxfile++] = ino;
			return;
	}
	lookup(ino, 1);
}

pass2(ip)
	register struct dinode *ip;
{
	struct direct dbuf[MAXNDIR];
	long doff;
	struct direct *dp;
	register i, j;
	int k;
	struct htab *hp;
	daddr_t d;
	ino_t kno;

	if((ip->di_mode&IFMT) != IFDIR)
		return;
	gip = ip;
	doff = 0;
	for(i=0;; i++) {
		if(doff >= ip->di_size)
			break;
		d = bmap(i);
		if(d == 0)
			break;
		bread(fsbtodb(&sblock, d), (char *)dbuf, sizeof(dbuf));
		for(j=0; j < NDIR(&sblock); j++) {
			if(doff >= ip->di_size)
				break;
			doff += sizeof(struct direct);
			dp = dbuf+j;
			kno = dp->d_ino;
			if(kno == 0)
				continue;
			hp = lookup(kno, 0);
			if(hp == 0)
				continue;
			if(dotname(dp))
				continue;
			hp->h_pino = ino;
			for(k=0; k<DIRSIZ; k++)
				hp->h_name[k] = dp->d_name[k];
		}
	}
}

pass3(ip)
	register struct dinode *ip;
{
	struct direct dbuf[MAXNDIR];
	long doff;
	struct direct *dp;
	register i, j;
	int k;
	daddr_t d;
	ino_t kno;

	if((ip->di_mode&IFMT) != IFDIR)
		return;
	gip = ip;
	doff = 0;
	for(i=0;; i++) {
		if(doff >= ip->di_size)
			break;
		d = bmap(i);
		if(d == 0)
			break;
		bread(fsbtodb(&sblock, d), (char *)dbuf, sizeof(dbuf));
		for(j=0; j < NDIR(&sblock); j++) {
			if(doff >= ip->di_size)
				break;
			doff += sizeof(struct direct);
			dp = dbuf+j;
			kno = dp->d_ino;
			if(kno == 0)
				continue;
			if(aflg==0 && dotname(dp))
				continue;
			if(ilist[0] == 0)
				goto pr;
			for(k=0; ilist[k] != 0; k++)
				if(ilist[k] == kno)
					goto pr;
			continue;
		pr:
			printf("%u	", kno);
			pname(ino, 0);
			printf("/%.14s", dp->d_name);
			if (lookup(kno, 0))
				printf("/.");
			printf("\n");
		}
	}
}

dotname(dp)
	register struct direct *dp;
{

	if (dp->d_name[0]=='.')
		if (dp->d_name[1]==0 || (dp->d_name[1]=='.' && dp->d_name[2]==0))
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
		printf("???");
		return;
	}
	if (lev > 10) {
		printf("...");
		return;
	}
	pname(hp->h_pino, ++lev);
	printf("/%.14s", hp->h_name);
}

struct htab *
lookup(i, ef)
	ino_t i;
	int ef;
{
	register struct htab *hp;

	for (hp = &htab[i%HSIZE]; hp->h_ino;) {
		if (hp->h_ino==i)
			return(hp);
		if (++hp >= &htab[HSIZE])
			hp = htab;
	}
	if (ef==0)
		return(0);
	if (++nhent >= HSIZE) {
		fprintf(stderr, "ncheck: out of core-- increase HSIZE\n");
		exit(1);
	}
	hp->h_ino = i;
	return(hp);
}

bread(bno, buf, cnt)
	daddr_t bno;
	char *buf;
	int cnt;
{
	register i;

	lseek(fi, bno * DEV_BSIZE, 0);
	if (read(fi, buf, cnt) != cnt) {
		fprintf(stderr, "ncheck: read error %d\n", bno);
		for(i=0; i < cnt; i++)
			buf[i] = 0;
	}
}

daddr_t
bmap(i)
	int i;
{
	daddr_t ibuf[MAXNINDIR];

	if(i < NDADDR)
		return(gip->di_db[i]);
	i -= NDADDR;
	if(i > NINDIR(&sblock)) {
		fprintf(stderr, "ncheck: %u - huge directory\n", ino);
		return((daddr_t)0);
	}
	bread(fsbtodb(&sblock, gip->di_ib[i]), (char *)ibuf, sizeof(ibuf));
	return(ibuf[i]);
}
