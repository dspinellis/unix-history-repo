static	char *sccsid = "@(#)dcheck.c	1.5 (Berkeley) %G%";
/*
 * dcheck - check directory consistency
 */
#define	NB	10
#define	NDIR(fs)	((fs)->fs_bsize/sizeof(struct direct))
#define	MAXNDIR		(MAXBSIZE/sizeof(struct direct))
#define	MAXNINDIR	(MAXBSIZE / sizeof (daddr_t))

#include <stdio.h>
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/fs.h"

union {
	struct	fs fs;
	char pad[MAXBSIZE];
} fsun;
#define	sblock	fsun.fs

struct	dinode	itab[MAXIPG];
struct	dinode	*gip;
ino_t	ilist[NB];

int	fi;
ino_t	ino;
ino_t	*ecount;
int	headpr;
int	nfiles;

int	nerror;
daddr_t	bmap();
long	atol();
char	*malloc();

main(argc, argv)
char *argv[];
{
	register i;
	long n;

	while (--argc) {
		argv++;
		if (**argv=='-')
		switch ((*argv)[1]) {

		case 'i':
			for(i=0; i<NB; i++) {
				n = atol(argv[1]);
				if(n == 0)
					break;
				ilist[i] = n;
				argv++;
				argc--;
			}
			ilist[i] = 0;
			continue;

		default:
			printf("Bad flag %c\n", (*argv)[1]);
			nerror++;
		}
		check(*argv);
	}
	return(nerror);
}

check(file)
char *file;
{
	register i, j, c;

	fi = open(file, 0);
	if(fi < 0) {
		printf("cannot open %s\n", file);
		nerror++;
		return;
	}
	headpr = 0;
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
		printf("%s: preposterous number of files\n", file);
		nerror++;
		return;
	}
	ecount = (ino_t *)malloc((nfiles+1) * sizeof (*ecount));
	if (ecount == 0) {
		printf("%s: not enough core for %d files\n", file, nfiles);
		exit(04);
	}
	for (i = 0; i<=nfiles; i++)
		ecount[i] = 0;
	ino = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		bread(fsbtodb(&sblock, cgimin(c, &sblock)), (char *)itab,
		    sblock.fs_ipg * sizeof (struct dinode));
		for (j = 0; j < sblock.fs_ipg; j++) {
			pass1(&itab[j]);
			ino++;
		}
	}
	ino = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		bread(fsbtodb(&sblock, cgimin(c, &sblock)), (char *)itab,
		    sblock.fs_ipg * sizeof (struct dinode));
		for (j = 0; j < sblock.fs_ipg; j++) {
			pass2(&itab[j]);
			ino++;
		}
	}
	free(ecount);
}

pass1(ip)
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
		bread(fsbtodb(&sblock, d), (char *)dbuf, sblock.fs_bsize);
		for(j=0; j < NDIR(&sblock); j++) {
			if(doff >= ip->di_size)
				break;
			doff += sizeof(struct direct);
			dp = &dbuf[j];
			kno = dp->d_ino;
			if(kno == 0)
				continue;
			if(kno > nfiles || kno < ROOTINO) {
				printf("%d bad; %d/%.*s\n",
				    kno, ino, DIRSIZ, dp->d_name);
				nerror++;
				continue;
			}
			for (k=0; ilist[k] != 0; k++)
				if (ilist[k]==kno) {
					printf("%d arg; %d/%.*s\n",
					    kno, ino, DIRSIZ, dp->d_name);
					nerror++;
				}
			ecount[kno]++;
		}
	}
}

pass2(ip)
register struct dinode *ip;
{
	register i;

	i = ino;
	if ((ip->di_mode&IFMT)==0 && ecount[i]==0)
		return;
	if (ip->di_nlink==ecount[i] && ip->di_nlink!=0)
		return;
	if (headpr==0) {
		printf("     entries  link cnt\n");
		headpr++;
	}
	printf("%u\t%d\t%d\n", ino,
	    ecount[i], ip->di_nlink);
}

bread(bno, buf, cnt)
daddr_t bno;
char *buf;
{
	register i;

	lseek(fi, bno * DEV_BSIZE, 0);
	if (read(fi, buf, cnt) != cnt) {
		printf("read error %d\n", bno);
		for(i=0; i < cnt; i++)
			buf[i] = 0;
	}
}

daddr_t
bmap(i)
{
	daddr_t ibuf[MAXNINDIR];

	if(i < NDADDR)
		return(gip->di_db[i]);
	i -= NDADDR;
	if(i > NINDIR(&sblock)) {
		printf("%u - huge directory\n", ino);
		return((daddr_t)0);
	}
	bread(fsbtodb(&sblock, gip->di_ib[0]), (char *)ibuf, sizeof(ibuf));
	return(ibuf[i]);
}
