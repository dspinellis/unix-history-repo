#

char	*dargv[]
{
	0,
	"/dev/rk2",
	"/dev/rp0",
	0
};
int	bufa;
int	sflg;
char	lfile[]	"/tmp/ktmp";
int	lflg;
int	uflg;
int	lfdes;
int	lfptr;

struct	inode
{
	int	i_mode;
	char	i_nlink;
	char	i_uid;
	char	i_gid;
	char	i_size0;
	char	*i_size1;
	int	i_addr[8];
	int	i_atime[2];
	int	i_mtime[2];
};

/* modes */
#define	IALLOC	0100000
#define	IFMT	060000
#define		IFDIR	040000
#define		IFCHR	020000
#define		IFBLK	060000
#define	ILARG	010000

struct
{
	char	*s_isize;
	char	*s_fsize;
	int	s_nfree;
	int	s_free[100];
	int	s_ninode;
	int	s_inode[100];
	char	s_flock;
	char	s_ilock;
	char	s_fmod;
	int	time[2];
	int	pad[50];
} sblock;

char	bitcnt[]
{
	4,3,3,2,3,2,2,1,
	3,2,2,1,2,1,1,0,
};
int	ndlist;
int	dlist[100];
char	*great;
int	nblist;
int	blist[100];
char	*bmap	-1;
int	fi;
int	nfile;
int	nspcl;
int	nlarg;
int	nindir;
int	ndir;
int	nused;
int	hiwat;
int	nfree;
int	ino;
int	ndup;
int	nnf	100;

struct
{
	char	icnt[2];
} *icnt;
char	*ucnt;

struct	fname
{
	int	inum;
	int	nptr;
	int	pnum;
} *dn, *edn;

int	ldivr;
int	fout;

main(argc, argv)
char **argv;
{
	char *arg;
	register *p;

	argv[argc] = 0;
	if(argc == 1) {
		argv = dargv;
		for(argc = 1; dargv[argc]; argc++);
	}

	if(argc>1 && *argv[1] == '-') {
		argc--;
		argv++;
		arg = *argv;
		while(*arg)
		switch(*arg++) {

		case 'u':
			uflg++;

		case 'l':
			lflg++;
			continue;

		case 's':
			sflg++;
			continue;

		case 'i':
			dlist[ndlist++] = number(argv[1]);
			argc--;
			argv++;
			continue;

		case 'b':
			blist[nblist++] = number(argv[1]);
			argc--;
			argv++;
			continue;

		case 'g':
			great = number(argv[1]);
			argc--;
			argv++;
			continue;

		}
	}
	if (lflg)
		sflg = 0;
	fout = dup(1);
	argc--;
	argv++;
	fprintf("%s:\n", argv[0]);
	check(argv[0]);
	flush();
	close(fout);
	fout = 1;
	if(argc > 1)
		execv("/bin/check", argv);
}

check(file)
char *file;
{
	struct inode buf[16];
	register i, j;
	int compar(), cintr();
	struct fname *dnp;

	fi = open(file, 0);
	if(fi < 0) {
		fprintf("cannot open %s\n", file);
		return;
	}
	sync();
	bread(1, &sblock);
	if (lflg) {
		if(uflg) {
			ucnt = sbrk(sblock.s_isize*2);
			if(bad(ucnt))
				return;
		} else {
			lfdes = creat(lfile, 0666);
			if ((signal(2, 1) & 01) == 0)
				signal(2, cintr);
		}
		dn = edn = sbrk(101*6);
		if(bad(dn))
			return;
	} else {
		bmap = sbrk(8192);
		icnt = sbrk(sblock.s_isize*16);
		if(bad(bmap) || bad(icnt))
			return;
	}
	for(i=0; i<sblock.s_isize; i++) {
		bread(i+2, buf);
		for(j=0; j<16; j++) {
			ino++;
			pass1(&buf[j]);
		}
	}
	if(uflg) {
		for(ino=2; ino<sblock.s_isize*16; ino++) {
			i = ldiv(0, ino, 8);
			if(ucnt[i] & (1<<ldivr))
			if(trace(ino, 0) == 0)
				fprintf("%l\n", ino);
		}
		return;
	}
	ino = 0;
	if (lflg) {
		close(lfdes);
		lfdes = open(lfile, 0);
		qsort(dn, edn-dn, 6, compar);
		i = -1;
		j = 0;
		for (dnp=dn; dnp<edn; dnp++) {
			if(dnp->pnum == 0) {
				if(dnp->inum != 1)
					j = 1;
				continue;
			}
			printf("%4l", dnp->inum);
			if (dnp->inum == i)
				putchar('l'); else
				putchar(' ');
			i = dnp->inum;
			putchar(' ');
			if(lflg > 1)
				pstat(i, buf);
			pfilename(dnp, 0);
			if(j) {
				j = 0;
				printf("/.");
			}
			putchar('\n');
		}
		close(lfdes);
		unlink(lfile);
		return;
	}
	sync();
	bread(1, &sblock);
	if(sflg) {
		close(fi);
		fi = open(file, 1);
		if(fi < 0) {
			fprintf("cannot write %s\n", file);
			return;
		}
		sblock.s_nfree = 0;
		sblock.s_ninode = 0;
		sblock.s_flock = 0;
		sblock.s_ilock = 0;
		sblock.s_fmod = 0;
		free(0);
		for(i=sblock.s_fsize-1; i>=sblock.s_isize+2; i--) {
			ndup = 0;
			chk(i, "URK", 0);
			if(ndup == 0)
				free(i);
		}
		bwrite(1, &sblock);
		close(fi);
		sync();
		return;
	}
	while(i = alloc()) {
		if(chk(i, "free", 0))
			break;
		nfree++;
	}
	if(ndup)
		printf("%l dups in free\n", ndup);
	ndup = sblock.s_fsize - sblock.s_isize - 2;
	for(i=0; i<8192; i++) {
		j = bmap[i];
		ndup =+ bitcnt[j&017];
		ndup =+ bitcnt[(j>>4)&017];
	}
	if(ndup)
		printf("%l missing\n", ndup);
	for(i=0; i<sblock.s_isize*16; i++) {
		j = icnt->icnt[i] & 0377;
		if(j!=0 && j!=0200)
			printf("%6l %3o\n", i+1, j);
	}
	printf("spcl  %6l\n", nspcl);
	printf("files %6l\n", nfile);
	printf("large %6l\n", nlarg);
	printf("direc %6l\n", ndir);
	printf("indir %6l\n", nindir);
	printf("used  %6l\n", nused);
	printf("last  %6l\n", hiwat);
	printf("free  %6l\n", nfree);
	close(fi);
}

cintr()
{

	unlink(lfile);
	exit();
}

compar(p1, p2)
struct fname *p1, *p2;
{
	int i;

	i = p1->inum - p2->inum;
	if(i)
		return(i);
	return(p1->pnum - p2->pnum);
}

pass1(ip)
struct inode *ip;
{
	int buf[256];
	register i, j, df;

	if((ip->i_mode&IALLOC) == 0)
		return;
	if(uflg) {
		i = ldiv(0, ino, 8);
		ucnt[i] =| 1<<ldivr;
	}
	if (!lflg) {
		icnt->icnt[ino-1] =+ 0100;
		if(ip->i_nlink)
			icnt->icnt[ino-1] =+ 0100 + ip->i_nlink;
	}
	if((ip->i_mode&IFCHR&IFBLK) != 0) {
		nspcl++;
		return;
	}
	df = 0;
	if((ip->i_mode&IFMT) == IFDIR) {
		df = ldiv(ip->i_size0, ip->i_size1, 16);
		if(lflg && !uflg) {
			if(--nnf<0) {
				nnf = 100;
				edn = sbrk(6*101);
				if(bad(edn))
					lflg = 0;
			}
			edn->pnum = 0;
			edn->inum = ino;
			edn++;
		}
		ndir++;
	} else if (lflg)
		return;
	nfile++;
	if((ip->i_mode&ILARG) != 0) {
		nlarg++;
		for(i=0; i<8; i++)
		if(ip->i_addr[i] != 0) {
			nindir++;
			if(chk(ip->i_addr[i], "idir", 0))
				continue;
			bread(ip->i_addr[i], buf);
			for(j=0; j<256; j++)
			if(buf[j] != 0)
				chk(buf[j], "ldir", df);
			df =- 32;
		}
		return;
	}
	for(i=0; i<8; i++) {
		if(ip->i_addr[i] != 0)
			chk(ip->i_addr[i], "sdir", df);
		df =- 32;
	}
}

chk(ii, s, df)
char *ii;
{
	register char *i;
	register n, j;
	int b;
	int buf[256];
	struct {
		int	ino;
		char	name[14];
	};

	i = ii;
	for(j=0; j<nblist; j++)
		if(i == blist[j])
			fprintf("%l blk; i=%l(%s)\n", i, ino, s);
	if(great && i >= great && i)
		fprintf("%l geq; i=%l(%s)\n", i, ino, s);
	if(ino) {
		nused++;
		if(i > hiwat)
			hiwat = i;
	}
	if(i<sblock.s_isize+2 || i>=sblock.s_fsize) {
		fprintf("%l bad; i=%l(%s)\n", i, ino, s);
		return(1);
	}
	n = ldiv(0, i, 8);
	if (lflg==0) {
		j = (1<<ldivr);
		if(bmap[n] & j) {
			if(ino == 0) {
				ndup++;
				return(0);
			}
			fprintf("%l dup; i=%l(%s)\n", i, ino, s);
			return(1);
		}
		bmap[n] =| j;
	}
	if(df>0) {
		bread(i, buf);
		for(n=0; n<256; n=+8) {
			if(df <= 0)
				break;
			df--;
			if((b=buf[n]) == 0)
				continue;
			if (lflg)
			if(!(buf[n].name[0]=='.' &&
			  (buf[n].name[1]=='\0' ||
			  (buf[n].name[1]=='.' && buf[n].name[2]=='\0')))) {
				if (--nnf<0) {
					nnf = 100;
					edn = sbrk(6*101);
					if(bad(edn))
						lflg = 0;
				}
				edn->pnum = ino;
				edn->inum = buf[n];
				edn->nptr = lfptr;
				for (j=0; j<14; j++)
					if(buf[n].name[j] == '\0')
						break;
				j++;
				if(!uflg)
					write(lfdes, buf[n].name, j);
				lfptr =+ j;
				edn++;
			}
			for(j=0; j<ndlist; j++)
				if(b == dlist[j])
				fprintf("%l ino; i=%l(%s) \"%.16s\"\n",
				  b, ino, s, buf+n+1);
			if(b<1 || b>16*sblock.s_isize) {
				fprintf("%l din; i=%l(%s)\n", i, ino, s);
				continue;
			}
			if (!lflg)
				icnt->icnt[b-1]--;
		}
	}
	return(0);
}

pfilename(dp, recur)
struct fname *dp;
{
	register struct fname *p;
	char name[14];

	if (++recur > 10) {
		putchar('.');
		putchar('.');
		putchar('.');
	} else
	if (dp->pnum != 1) {
		for (p = dn; p<edn; p++) {
			if (p->inum==dp->pnum && p->pnum!=0) {
				pfilename(p, recur);
				goto yes;
			}
		}
		putchar('?');
		putchar('?');
		putchar('?');
	    yes:
		putchar('/');
	}
	seek(lfdes, dp->nptr,  0);
	read(lfdes, name, 14);
	printf("%.14s", name);
}

pstat(i, buf)
{
	register n, *p;

	n = ldiv(0, i+31, 16);
	if(n != bufa) {
		bufa = n;
		bread(n, buf);
	}
	p = buf + ldivr*32;
	printf("%4o ", p->i_mode&07777);
	pdate(p->i_atime);
	pdate(p->i_mtime);
}

pdate(t)
int *t;
{
	register *p;

	p = localtime(t);
	p2dig(p[5], '/');
	p2dig(p[4]+1, '/');
	p2dig(p[3], ' ');
	p2dig(p[2], ':');
	p2dig(p[1], ' ');
}

p2dig(n, c)
{
	putchar(n/10 + '0');
	putchar(n%10 + '0');
	putchar(c);
}

alloc()
{
	register b, i;
	int buf[256];

	i = --sblock.s_nfree;
	if(i<0 || i>=100) {
		fprintf("bad freeblock\n");
		return(0);
	}
	b = sblock.s_free[i];
	if(b == 0)
		return(0);
	if(sblock.s_nfree <= 0) {
		bread(b, buf);
		sblock.s_nfree = buf[0];
		for(i=0; i<100; i++)
			sblock.s_free[i] = buf[i+1];
	}
	return(b);
}

bread(bno, buf)
{

	seek(fi, bno, 3);
	if(read(fi, buf, 512) != 512) {
		fprintf("read error %d\n", bno);
		exit();
	}
}

free(in)
{
	int i;
	int buf[256];

	if(sblock.s_nfree >= 100) {
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
	if(write(fi, buf, 512) != 512) {
		fprintf("write error %d\n", bno);
		exit();
	}
}

number(s)
char *s;
{
	int n, c;

	n = 0;
	while(c = *s++) {
		if(c<'0' || c>'9')
			continue;
		n = n*10+c-'0';
	}
	return(n);
}

fprintf(a, b, c, d, e)
{
	printf(a, b, c, d, e);
	flush();
}

bad(f)
{

	if(f == -1) {
		fprintf("out of memory\n");
		return(1);
	}
	return(0);
}

trace(ii, r)
{
	register struct fname *p;
	register i, j;

	i = ii;
	r++;
	if(r > 10)
		return(0);
	for(p=dn; p<edn; p++)
	if(p->inum == i) {
		j = p->pnum;
		if(j == 0 || j == 1)
			return(j);
		if(trace(j, r) == 1) {
			setpn(j, 1);
			return(1);
		}
	}
	setpn(i, 0);
	return(0);
}

setpn(pn, n)
{
	register struct fname *p;
	register i, j;

	i = pn;
	j = n;
	for(p=dn; p<edn; p++)
		if(p->pnum == i)
			p->pnum = j;
}
