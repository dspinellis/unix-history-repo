#

char	*dargv[]
{
	"/dev/rrk2",
	"/dev/rrp0",
	0
};
#define	NINODE	16*16
#define	NB	10
#include "/usr/sys/ino.h"
#include "/usr/sys/filsys.h"

struct	inode	inode[NINODE];
struct	filsys	sblock;

int	sflg;

int	fi;
int	nifiles;
int	nfile;
int	nspcl;
int	nlarg;
int	nvlarg;
int	nindir;
int	nvindir;
int	ndir;
int	nused;
int	nfree;
int	ino;
int	ndup;
int	blist[10] { -1};
int	nerror;
int	bmap[4096];

main(argc, argv)
char **argv;
{
	register char **p;
	register int n, *lp;

	if (argc == 1) {
		for (p = dargv; *p;)
			check(*p++);
		return(nerror);
	}
	while (--argc) {
		argv++;
		if (**argv=='-') switch ((*argv)[1]) {
		case 's':
			sflg++;
			continue;

		case 'b':
			lp = blist;
			while (lp < &blist[NB-1] && (n = number(argv[1]))) {
				*lp++ = n;
				argv++;
				argc--;
			}
			*lp++ = -1;
			continue;

		default:
			printf("Bad flag\n");
		}
		check(*argv);
	}
	return(nerror);
}

check(file)
char *file;
{
	register *ip, i, j;

	fi = open(file, sflg?2:0);
	if (fi < 0) {
		printf("cannot open %s\n", file);
		nerror =| 04;
		return;
	}
	printf("%s:\n", file);
	nfile = 0;
	nspcl = 0;
	nlarg = 0;
	nvlarg = 0;
	nindir = 0;
	nvindir = 0;
	ndir = 0;
	nused = 0;
	nfree = 0;
	ndup = 0;
	for (ip = bmap; ip < &bmap[4096];)
		*ip++ = 0;
	sync();
	bread(1, &sblock, 512);
	nifiles = sblock.s_isize*16;
	for(i=0; ino < nifiles; i =+ NINODE/16) {
		bread(i+2, inode, sizeof inode);
		for(j=0; j<NINODE && ino<nifiles; j++) {
			ino++;
			pass1(&inode[j]);
		}
	}
	ino = 0;
	sync();
	bread(1, &sblock, 512);
	if (sflg) {
		makefree();
		return;
	}
	while(i = alloc()) {
		if (chk(i, "free"))
			break;
		nfree++;
	}
	if (ndup) {
		printf("%l dups in free\n", ndup);
		nerror =| 02;
	}
	j = 0;
	for (ip = bmap; ip < &bmap[4096];) {
		i = *ip++;
		while (i) {
			if (i<0)
				j--;
			i =<< 1;
		}
	}
	j =+ sblock.s_fsize - sblock.s_isize - 2;
	if (j)
		printf("missing%5l\n", j);
	printf("spcl  %6l\n", nspcl);
	printf("files %6l\n", nfile);
	printf("large %6l\n", nlarg);
	if (nvlarg)
		printf("huge  %6l\n", nvlarg);
	printf("direc %6l\n", ndir);
	printf("indir %6l\n", nindir);
	if (nvindir)
		printf("indir2%6l\n", nvindir);
	printf("used  %6l\n", nused);
	printf("free  %6l\n", nfree);
	close(fi);
}

pass1(aip)
struct inode *aip;
{
	int buf[256], vbuf[256];
	register i, j, *ip;

	ip = aip;
	if ((ip->i_mode&IALLOC) == 0)
		return;
	if ((ip->i_mode&IFCHR&IFBLK) != 0) {
		nspcl++;
		return;
	}
	if ((ip->i_mode&IFMT) == IFDIR)
		ndir++;
	else
		nfile++;
	if ((ip->i_mode&ILARG) != 0) {
		nlarg++;
		for(i=0; i<7; i++)
		if (ip->i_addr[i] != 0) {
			nindir++;
			if (chk(ip->i_addr[i], "indirect"))
				continue;
			bread(ip->i_addr[i], buf, 512);
			for(j=0; j<256; j++)
			if (buf[j] != 0)
				chk(buf[j], "data (large)");
		}
		if (ip->i_addr[7]) {
			nvlarg++;
			if (chk(ip->i_addr[7], "indirect"))
				return;
			bread(ip->i_addr[7], buf, 512);
			for(i=0; i<256; i++)
			if (buf[i] != 0) {
				nvindir++;
				if (chk(buf[i], "2nd indirect"))
					continue;
				bread(buf[i], vbuf, 512);
				for(j=0; j<256; j++)
				if (vbuf[j])
					chk(vbuf[j], "data (very large)");
			}
		}
		return;
	}
	for(i=0; i<8; i++) {
		if (ip->i_addr[i] != 0)
			chk(ip->i_addr[i], "data (small)");
	}
}

chk(ab, s)
char *ab;
{
	register char *b;
	register n, m;

	b = ab;
	if (ino)
		nused++;
	if (b<sblock.s_isize+2 || b>=sblock.s_fsize) {
		printf("%l bad; inode=%l, class=%s\n", b, ino, s);
		return(1);
	}
	m = 1 << (b&017);
	n = (b>>4) & 07777;
	if (bmap[n]&m) {
		printf("%l dup; inode=%l, class=%s\n", b, ino, s);
		ndup++;
	}
	bmap[n] =| m;
	for (n=0; blist[n] != -1; n++)
		if (b == blist[n])
			printf("%l arg; inode=%l, class=%s\n", b, ino, s);
	return(0);
}

alloc()
{
	register b, i;
	int buf[256];

	i = --sblock.s_nfree;
	if (i<0 || i>=100) {
		printf("bad freeblock\n");
		return(0);
	}
	b = sblock.s_free[i];
	if (b == 0)
		return(0);
	if (sblock.s_nfree <= 0) {
		bread(b, buf, 512);
		sblock.s_nfree = buf[0];
		for(i=0; i<100; i++)
			sblock.s_free[i] = buf[i+1];
	}
	return(b);
}

bread(bno, buf, cnt)
int *buf;
{
	register *ip;

	seek(fi, bno, 3);
	if (read(fi, buf, cnt) != cnt) {
		printf("read error %d\n", bno);
		if (sflg) {
			printf("No update\n");
			sflg = 0;
		}
		for (ip = buf; ip < &buf[256];)
			*ip++ = 0;
	}
}

free(in)
{
	register i;
	int buf[256];

	if (sblock.s_nfree >= 100) {
		buf[0] = sblock.s_nfree;
		for(i=0; i<100; i++)
			buf[i+1] = sblock.s_free[i];
		sblock.s_nfree = 0;
		bwrite(in, buf);
	}
	sblock.s_free[sblock.s_nfree++] = in;
}

bwrite(bno, buf)
{

	seek(fi, bno, 3);
	if (write(fi, buf, 512) != 512)
		printf("write error %d\n", bno);
}

number(as)
char *as;
{
	register n, c;
	register char *s;

	s = as;
	n = 0;
	while ((c = *s++) >= '0' && c <= '9') {
		n = n*10+c-'0';
	}
	return(n);
}

makefree()
{
	register i;

	sblock.s_nfree = 0;
	sblock.s_ninode = 0;
	sblock.s_flock = 0;
	sblock.s_ilock = 0;
	sblock.s_fmod = 0;
	free(0);
	for(i=sblock.s_fsize-1; i>=sblock.s_isize+2; i--) {
		if ((bmap[(i>>4)&07777] & (1<<(i&017)))==0)
			free(i);
	}
	bwrite(1, &sblock);
	close(fi);
	sync();
	return;
}
