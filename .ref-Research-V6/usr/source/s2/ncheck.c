#

char	*dargv[]
{
	"/dev/rrk2",
	"/dev/rrp0",
	0
};

#define NINODE	16*16
#include "/usr/sys/ino.h"
#include "/usr/sys/filsys.h"

struct	filsys	sblock;
struct	inode	inode[NINODE];

int	sflg;
int	aflg;
#define	NI	20
#define	NDIRS	787

int	ilist[NI] { -1};
int	fi;
struct	htab {
	int	hino;
	int	hpino;
	char	hname[14];
} htab[NDIRS];
int	nhent	10;
int	(*pass[])()	{ pass1, pass2, pass3 };
char	*lasts;
int	ino;
int	nerror;
int	nffil;
int	fout;
int	nfiles;
struct dir {
	int	ino;
	char	name[14];
};

main(argc, argv)
char **argv;
{
	register char **p;
	register int n, *lp;

	nffil = dup(1);
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

		case 'a':
			aflg++;
			continue;

		case 'i':
			lp = ilist;
			while (lp < &ilist[NI-1] && (n = number(argv[1]))) {
				*lp++ = n;
				argv++;
				argc--;
			}
			*lp++ = -1;
			continue;

		default:
			printf2("Bad flag\n");
		}
		check(*argv);
	}
	return(nerror);
}

check(file)
char *file;
{
	register i, j, pno;

	fi = open(file, 0);
	if (fi < 0) {
		printf2("cannot open %s\n", file);
		return;
	}
	printf2("%s:\n", file);
	sync();
	bread(1, &sblock, 512);
	nfiles = sblock.s_isize*16;
	for (i=0; i<NDIRS; i++)
		htab[i].hino = 0;
	fout = nffil;
	flush();
	for (pno=0; pno<3; pno++) {
		ino = 0;
		for (i=0; ino<nfiles; i =+ NINODE/16) {
			bread(i+2, inode, sizeof inode);
			for (j=0; j<NINODE && ino<nfiles; j++) {
				ino++;
				(*pass[pno])(&inode[j]);
			}
		}
	}
	flush();
	fout = 1;
}

pass1(ip)
{
	if ((ip->i_mode&IALLOC)==0 || (ip->i_mode&IFMT)!=IFDIR)
		return;
	lookup(ino, 1);
}

pass2(ip)
struct inode *ip;
{
	register doff;
	register struct htab *hp;
	register struct dir *dp;
	int i;

	if ((ip->i_mode&IALLOC)==0 || (ip->i_mode&IFMT)!=IFDIR)
		return;
	doff = 0;
	while (dp = dread(ip, doff)) {
		doff =+ 16;
		if (dp->ino==0)
			continue;
		if ((hp = lookup(dp->ino, 0)) == 0)
			continue;
		if (dotname(dp))
			continue;
		hp->hpino = ino;
		for (i=0; i<14; i++)
			hp->hname[i] = dp->name[i];
	}
}

pass3(ip)
struct inode *ip;
{
	register doff;
	register struct dir *dp;
	register int *ilp;

	if ((ip->i_mode&IALLOC)==0 || (ip->i_mode&IFMT)!=IFDIR)
		return;
	doff = 0;
	while (dp = dread(ip, doff)) {
		doff =+ 16;
		if (dp->ino==0)
			continue;
		if (aflg==0 && dotname(dp))
			continue;
		for (ilp=ilist; *ilp >= 0; ilp++)
			if (*ilp == dp->ino)
				break;
		if (ilp > ilist && *ilp!=dp->ino)
			continue;
		printf("%d	", dp->ino);
		pname(ino, 0);
		printf("/%.14s\n", dp->name);
	}
}

dotname(adp)
{
	register struct dir *dp;

	dp = adp;
	if (dp->name[0]=='.')
		if (dp->name[1]==0 || dp->name[1]=='.' && dp->name[2]==0)
			return(1);
	return(0);
}

pname(i, lev)
{
	register struct htab *hp;

	if (i==1)
		return;
	if ((hp = lookup(i, 0)) == 0) {
		printf("???");
		return;
	}
	if (lev > 10) {
		printf("...");
		return;
	}
	pname(hp->hpino, ++lev);
	printf("/%.14s", hp->hname);
}

lookup(i, ef)
{
	register struct htab *hp;

	for (hp = &htab[i%NDIRS]; hp->hino;) {
		if (hp->hino==i)
			return(hp);
		if (++hp >= &htab[NDIRS])
			hp = htab;
	}
	if (ef==0)
		return(0);
	if (++nhent >= NDIRS) {
		printf2("Out of core-- increase NDIRS\n");
		flush();
		exit(1);
	}
	hp->hino = i;
	return(hp);
}

dread(aip, aoff)
{
	register b, off;
	register struct inode *ip;
	static ibuf[256];
	static char buf[512];

	off = aoff;
	ip = aip;
	if ((off&0777)==0) {
		if (off==0177000) {
			printf2("Monstrous directory %l\n", ino);
			return(0);
		}
		if ((ip->i_mode&ILARG)==0) {
			if (off>=010000 || (b = ip->i_addr[off>>9])==0)
				return(0);
			bread(b, buf, 512);
		} else {
			if (off==0) {
				if (ip->i_addr[0]==0)
					return(0);
				bread(ip->i_addr[0], ibuf, 512);
			}
			if ((b = ibuf[(off>>9)&0177])==0)
				return(0);
			bread(b, buf, 512);
		}
	}
	return(&buf[off&0777]);
}

bread(bno, buf, cnt)
{

	seek(fi, bno, 3);
	if (read(fi, buf, cnt) != cnt) {
		printf2("read error %d\n", bno);
		exit();
	}
}

bwrite(bno, buf)
{

	seek(fi, bno, 3);
	if (write(fi, buf, 512) != 512) {
		printf2("write error %d\n", bno);
		exit();
	}
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

printf2(s, a1, a2)
{
	extern fout;

	flush();
	fout = 2;
	printf(s, a1, a2);
	fout = nffil;
	flush();
}
