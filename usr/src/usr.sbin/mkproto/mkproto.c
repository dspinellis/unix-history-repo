/*-
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkproto.c	5.10 (Berkeley) %G%";
#endif /* not lint */

/*
 * Make a file system prototype.
 * usage: mkproto filsys proto
 */
#include <sys/param.h>
#include <sys/time.h>
#include <sys/dir.h>
#include <ufs/ufs/dinode.h>
#include <ufs/ffs/fs.h>
#include <stdio.h>

union {
	struct	fs fs;
	char	fsx[SBSIZE];
} ufs;
#define sblock	ufs.fs
union {
	struct	cg cg;
	char	cgx[MAXBSIZE];
} ucg;
#define	acg	ucg.cg
struct	fs *fs;
struct	csum *fscs;
int	fso, fsi;
FILE	*proto;
char	token[BUFSIZ];
int	errs;
long	dev_bsize = 1;
int	ino = 10;
long	getnum();
char	*strcpy();
ino_t	ialloc();

main(argc, argv)
	int argc;
	char *argv[];
{
	int i;
	char *calloc();

	if (argc != 3) {
		fprintf(stderr, "usage: mkproto filsys proto\n");
		exit(1);
	}
	fso = open(argv[1], 1);
	fsi = open(argv[1], 0);
	if (fso < 0 || fsi < 0) {
		perror(argv[1]);
		exit(1);
	}
	fs = &sblock;
	rdfs(SBOFF, SBSIZE, (char *)fs);
	dev_bsize = fs->fs_fsize / fsbtodb(fs, 1);
	fscs = (struct csum *)calloc(1, (u_int)fs->fs_cssize);
	for (i = 0; i < fs->fs_cssize; i += fs->fs_bsize)
		rdfs(fsbtodb(fs, fs->fs_csaddr + numfrags(fs, i)),
			(int)(fs->fs_cssize - i < fs->fs_bsize ?
			    fs->fs_cssize - i : fs->fs_bsize),
			((char *)fscs) + i);
	proto = fopen(argv[2], "r");
	descend((struct dinode *)0, ROOTINO);
	wtfs(SBOFF / dev_bsize, SBSIZE, (char *)fs);
	for (i = 0; i < fs->fs_cssize; i += fs->fs_bsize)
		wtfs(fsbtodb(&sblock, fs->fs_csaddr + numfrags(&sblock, i)),
			(int)(fs->fs_cssize - i < fs->fs_bsize ?
			    fs->fs_cssize - i : fs->fs_bsize),
			((char *)fscs) + i);
	exit(errs);
}

descend(par, parinum)
	struct dinode *par;
	ino_t parinum;
{
	struct dinode in;
	ino_t inum;
	int ibc = 0;
	int i, f, c;
	struct dinode *dip, inos[MAXBSIZE / sizeof (struct dinode)];
	daddr_t ib[MAXBSIZE / sizeof (daddr_t)];
	char buf[MAXBSIZE];

	getstr();
	in.di_mode = gmode(token[0], "-bcd", IFREG, IFBLK, IFCHR, IFDIR);
	in.di_mode |= gmode(token[1], "-u", 0, ISUID, 0, 0);
	in.di_mode |= gmode(token[2], "-g", 0, ISGID, 0, 0);
	for (i = 3; i < 6; i++) {
		c = token[i];
		if (c < '0' || c > '7') {
			printf("%c/%s: bad octal mode digit\n", c, token);
			errs++;
			c = 0;
		}
		in.di_mode |= (c-'0')<<(15-3*i);
	}
	in.di_uid = getnum(); in.di_gid = getnum();
	for (i = 0; i < fs->fs_bsize; i++)
		buf[i] = 0;
	for (i = 0; i < NINDIR(fs); i++)
		ib[i] = (daddr_t)0;
	in.di_nlink = 1;
	in.di_size = 0;
	for (i = 0; i < NDADDR; i++)
		in.di_db[i] = (daddr_t)0;
	for (i = 0; i < NIADDR; i++)
		in.di_ib[i] = (daddr_t)0;
	if (par != (struct dinode *)0) {
		inum = ialloc(&in);
	} else {
		par = &in;
		i = itod(fs, ROOTINO);
		rdfs(fsbtodb(fs, i), fs->fs_bsize, (char *)inos);
		dip = &inos[ROOTINO % INOPB(fs)];
		inum = ROOTINO;
		in.di_nlink = dip->di_nlink;
		in.di_size = dip->di_size;
		in.di_db[0] = dip->di_db[0];
		rdfs(fsbtodb(fs, in.di_db[0]), fs->fs_bsize, buf);
	}

	switch (in.di_mode&IFMT) {

	case IFREG:
		getstr();
		f = open(token, 0);
		if (f < 0) {
			printf("%s: cannot open\n", token);
			errs++;
			break;
		}
		while ((i = read(f, buf, (int)fs->fs_bsize)) > 0) {
			in.di_size += i;
			newblk(buf, &ibc, ib, (int)dblksize(fs, &in, ibc));
		}
		close(f);
		break;

	case IFBLK:
	case IFCHR:
		/*
		 * special file
		 * content is maj/min types
		 */

		i = getnum() & 0377;
		f = getnum() & 0377;
		in.di_rdev = (i << 8) | f;
		break;

	case IFDIR:
		/*
		 * directory
		 * put in extra links
		 * call recursively until
		 * name of "$" found
		 */

		if (inum != ROOTINO) {
			par->di_nlink++;
			in.di_nlink++;
			entry(&in, inum, ".", buf);
			entry(&in, parinum, "..", buf);
		}
		for (;;) {
			getstr();
			if (token[0]=='$' && token[1]=='\0')
				break;
			entry(&in, (ino_t)(ino+1), token, buf);
			descend(&in, inum);
		}
		if (inum != ROOTINO)
			newblk(buf, &ibc, ib, (int)dblksize(fs, &in, 0));
		else
			wtfs(fsbtodb(fs, in.di_db[0]), (int)fs->fs_bsize, buf);
		break;
	}
	iput(&in, &ibc, ib, inum);
}

/*ARGSUSED*/
gmode(c, s, m0, m1, m2, m3)
	char c, *s;
{
	int i;

	for (i = 0; s[i]; i++)
		if (c == s[i])
			return((&m0)[i]);
	printf("%c/%s: bad mode\n", c, token);
	errs++;
	return(0);
}

long
getnum()
{
	int i, c;
	long n;

	getstr();
	n = 0;
	i = 0;
	for (i = 0; c=token[i]; i++) {
		if (c<'0' || c>'9') {
			printf("%s: bad number\n", token);
			errs++;
			return((long)0);
		}
		n = n*10 + (c-'0');
	}
	return(n);
}

getstr()
{
	int i, c;

loop:
	switch (c = getc(proto)) {

	case ' ':
	case '\t':
	case '\n':
		goto loop;

	case EOF:
		printf("Unexpected EOF\n");
		exit(1);

	case ':':
		while (getc(proto) != '\n')
			;
		goto loop;

	}
	i = 0;
	do {
		token[i++] = c;
		c = getc(proto);
	} while (c != ' ' && c != '\t' && c != '\n' && c != '\0');
	token[i] = 0;
}

entry(ip, inum, str, buf)
	struct dinode *ip;
	ino_t inum;
	char *str;
	char *buf;
{
	register struct direct *dp, *odp;
	int oldsize, newsize, spacefree;

	odp = dp = (struct direct *)buf;
	while ((int)dp - (int)buf < ip->di_size) {
		odp = dp;
		dp = (struct direct *)((int)dp + dp->d_reclen);
	}
	if (odp != dp)
		oldsize = DIRSIZ(odp);
	else
		oldsize = 0;
	spacefree = odp->d_reclen - oldsize;
	dp = (struct direct *)((int)odp + oldsize);
	dp->d_ino = inum;
	dp->d_namlen = strlen(str);
	newsize = DIRSIZ(dp);
	if (spacefree >= newsize) {
		odp->d_reclen = oldsize;
		dp->d_reclen = spacefree;
	} else {
		dp = (struct direct *)((int)odp + odp->d_reclen);
		if ((int)dp - (int)buf >= fs->fs_bsize) {
			printf("directory too large\n");
			exit(1);
		}
		dp->d_ino = inum;
		dp->d_namlen = strlen(str);
		dp->d_reclen = DIRBLKSIZ;
	}
	strcpy(dp->d_name, str);
	ip->di_size = (int)dp - (int)buf + newsize;
}

newblk(buf, aibc, ib, size)
	int *aibc;
	char *buf;
	daddr_t *ib;
	int size;
{
	int i;
	daddr_t bno, alloc();

	bno = alloc(size);
	wtfs(fsbtodb(fs, bno), (int)fs->fs_bsize, buf);
	for (i = 0; i < fs->fs_bsize; i++)
		buf[i] = 0;
	ib[(*aibc)++] = bno;
	if (*aibc >= NINDIR(fs)) {
		printf("indirect block full\n");
		errs++;
		*aibc = 0;
	}
}

iput(ip, aibc, ib, inum)
	struct dinode *ip;
	int *aibc;
	daddr_t *ib;
	ino_t inum;
{
	struct dinode buf[MAXBSIZE / sizeof (struct dinode)];
	daddr_t d, alloc();
	int i;
	struct timeval t;

	(void)gettimeofday(&t, NULL);
	ip->di_atime.ts_sec = t.tv_sec;
	ip->di_atime.ts_nsec = 0;
	ip->di_mtime = ip->di_ctime = ip->di_atime;
	switch (ip->di_mode&IFMT) {

	case IFDIR:
	case IFREG:
		for (i = 0; i < *aibc; i++) {
			if (i >= NDADDR)
				break;
			ip->di_db[i] = ib[i];
		}
		if (*aibc > NDADDR) {
			ip->di_ib[0] = alloc((int)fs->fs_bsize);
			for (i = 0; i < NINDIR(fs) - NDADDR; i++) {
				ib[i] = ib[i+NDADDR];
				ib[i+NDADDR] = (daddr_t)0;
			}
			wtfs(fsbtodb(fs, ip->di_ib[0]),
			    (int)fs->fs_bsize, (char *)ib);
		}
		break;

	case IFBLK:
	case IFCHR:
		break;

	default:
		printf("bad mode %o\n", ip->di_mode);
		exit(1);
	}
	d = fsbtodb(fs, itod(fs, inum));
	rdfs(d, (int)fs->fs_bsize, (char *)buf);
	buf[itoo(fs, inum)] = *ip;
	wtfs(d, (int)fs->fs_bsize, (char *)buf);
}

daddr_t
alloc(size)
	int size;
{
	int i, frag;
	daddr_t d;
	static int cg = 0;

again:
	rdfs(fsbtodb(&sblock, cgtod(&sblock, cg)), (int)sblock.fs_cgsize,
	    (char *)&acg);
	if (!cg_chkmagic(&acg)) {
		printf("cg %d: bad magic number\n", cg);
		return (0);
	}
	if (acg.cg_cs.cs_nbfree == 0) {
		cg++;
		if (cg >= fs->fs_ncg) {
			printf("ran out of space\n");
			return (0);
		}
		goto again;
	}
	for (d = 0; d < acg.cg_ndblk; d += sblock.fs_frag)
		if (isblock(&sblock, (u_char *)cg_blksfree(&acg),
		    d / sblock.fs_frag))
			goto goth;
	printf("internal error: can't find block in cyl %d\n", cg);
	return (0);
goth:
	clrblock(&sblock, (u_char *)cg_blksfree(&acg), d / sblock.fs_frag);
	acg.cg_cs.cs_nbfree--;
	sblock.fs_cstotal.cs_nbfree--;
	fscs[cg].cs_nbfree--;
	cg_blktot(&acg)[cbtocylno(&sblock, d)]--;
	cg_blks(&sblock, &acg, cbtocylno(&sblock, d))[cbtorpos(&sblock, d)]--;
	if (size != sblock.fs_bsize) {
		frag = howmany(size, sblock.fs_fsize);
		fscs[cg].cs_nffree += sblock.fs_frag - frag;
		sblock.fs_cstotal.cs_nffree += sblock.fs_frag - frag;
		acg.cg_cs.cs_nffree += sblock.fs_frag - frag;
		acg.cg_frsum[sblock.fs_frag - frag]++;
		for (i = frag; i < sblock.fs_frag; i++)
			setbit(cg_blksfree(&acg), d + i);
	}
	wtfs(fsbtodb(&sblock, cgtod(&sblock, cg)), (int)sblock.fs_cgsize,
	    (char *)&acg);
	return (acg.cg_cgx * fs->fs_fpg + d);
}

/*
 * Allocate an inode on the disk
 */
ino_t
ialloc(ip)
	register struct dinode *ip;
{
	ino_t inum;
	int c;

	inum = ++ino;
	c = itog(&sblock, inum);
	rdfs(fsbtodb(&sblock, cgtod(&sblock, c)), (int)sblock.fs_cgsize,
	    (char *)&acg);
	if (!cg_chkmagic(&acg)) {
		printf("cg %d: bad magic number\n", c);
		exit(1);
	}
	if (ip->di_mode & IFDIR) {
		acg.cg_cs.cs_ndir++;
		sblock.fs_cstotal.cs_ndir++;
		fscs[c].cs_ndir++;
	}
	acg.cg_cs.cs_nifree--;
	setbit(cg_inosused(&acg), inum);
	wtfs(fsbtodb(&sblock, cgtod(&sblock, c)), (int)sblock.fs_cgsize,
	    (char *)&acg);
	sblock.fs_cstotal.cs_nifree--;
	fscs[c].cs_nifree--;
	if(inum >= sblock.fs_ipg * sblock.fs_ncg) {
		printf("fsinit: inode value out of range (%lu).\n", inum);
		exit(1);
	}
	return (inum);
}

/*
 * read a block from the file system
 */
rdfs(bno, size, bf)
	int bno, size;
	char *bf;
{
	int n;
	off_t lseek();

	if (lseek(fsi, bno * dev_bsize, 0) < 0) {
		printf("seek error: %d\n", bno);
		perror("rdfs");
		exit(1);
	}
	n = read(fsi, bf, size);
	if(n != size) {
		printf("read error: %d\n", bno);
		perror("rdfs");
		exit(1);
	}
}

/*
 * write a block to the file system
 */
wtfs(bno, size, bf)
	int bno, size;
	char *bf;
{
	int n;
	off_t lseek();

	if (lseek(fso, bno * dev_bsize, 0) < 0) {
		printf("seek error: %d\n", bno);
		perror("wtfs");
		exit(1);
	}
	n = write(fso, bf, size);
	if(n != size) {
		printf("write error: %d\n", bno);
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
		fprintf(stderr, "isblock bad fs_frag %ld\n", fs->fs_frag);
		return (0);
	}
	/*NOTREACHED*/
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
		fprintf(stderr, "clrblock bad fs_frag %ld\n", fs->fs_frag);
		return;
	}
}

