#

/*
 * incremental dump
 * dump isuodh filesystem
 * i from date in /etc/ddate
 * s specify tape size in feet (feet = blocks/9)
 * u update /etc/ddate to current date
 * 0 dump from the epoch
 * d dump specified number of days
 * h dump specified number of hours
 */

char	*dargv[]
{
	0,
	"i",
	"/dev/rp0",
	0
};
char	*dfile	"/etc/ddate";
char	*ofile	"/dev/mt0";
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

int	*talist;
int	fi;
int	buf[256];
int	dbuf[256];
int	ibuf[256];
char	*date[2];
char	*ddate[2];
int	fo;
int	pher;
int	dflg;
char	*tsize	19000;

main(argc, argv)
char **argv;
{
	char *key;
	int s, i, nfil, nblk;
	register *tap;
	register struct inode *ip;

	time(date);
	if(argc == 1) {
		argv = dargv;
		for(argc = 1; dargv[argc]; argc++);
	}

	argc--;
	argv++;
	key = *argv;
	while(*key)
	switch(*key++) {

	default:
		printf("bad character in key\n");
		exit();

	case 'i':
		i = open(dfile, 0);
		if(i >= 0) {
			read(i, ddate, 4);
			close(i);
		}
		continue;

	case 's': /* tape size */
		tsize = number(argv[1]) * 9;
		argv++;
		argc--;
		continue;

	case 'u': /* rewrite date */
		dflg++;
		continue;

	case '0': /* dump all */
		ddate[0] = ddate[1] = 0;
		continue;

	case 'd': /* dump some number of days */
		i = 21600;
		goto sd;

	case 'h': /* dump some number of hours */
		i = 900;
		goto sd;

	sd:
		ddate[0] = date[0];
		ddate[1] = date[1];
		s = number(argv[1])*4;
		argv++;
		argc--;
		while(s) {
			if(i > ddate[1])
				ddate[0]--;
			ddate[1] =- i;
			s--;
		}
		continue;
	}
	if(argc <= 1) {
		printf("no file system specified\n");
		exit();
	}
	printf("%s:\n", argv[1]);
	fi = open(argv[1], 0);
	if(fi < 0) {
		printf("cannot open %s\n", argv[1]);
		exit();
	}
	printf("incremental dump from\n");
	pdate(ddate);
	sync();
	bread(1, &sblock);
	talist = sbrk(size(0, sblock.s_isize*32)*512);
	tap = talist;
	nfil = 0;
	nblk = 0;
	for(i=0; i<sblock.s_isize; i++) {
		bread(i+2, buf);
		for(ip = &buf[0]; ip < &buf[256]; ip++) {
			if(ip->i_mode == 0 || ip->i_nlink == 0) {
				*tap++ = -1;
				continue;
			}
			if(ip->i_mtime[0] < ddate[0])
				goto no;
			if(ip->i_mtime[0] == ddate[0] &&
			   ip->i_mtime[1] <  ddate[1])
				goto no;
			s = size(ip->i_size0, ip->i_size1);
			nfil++;
			nblk =+ s;
			*tap = s+1;
		no:
			tap++;
		}
	}
	printf("%l files\n%l blocks\n", nfil, nblk);
	otape();
	tap = buf;
	clrbuf(tap);
	*tap++ = sblock.s_isize;
	*tap++ = sblock.s_fsize;
	*tap++ = date[0];
	*tap++ = date[1];
	*tap++ = ddate[0];
	*tap++ = ddate[1];
	*tap++ = tsize;
	swrite(buf);
	i = size(0, sblock.s_isize*32);
	tap = talist;
	while(i--) {
		bwrite(tap);
		tap =+ 256;
	}
	tap = talist;
	for(i=0; i<sblock.s_isize; i++) {
		bread(i+2, buf);
		for(ip = &buf[0]; ip < &buf[256]; ip++) {
			if(*tap)
				dump(ip, *tap-1);
			tap++;
		}
	}
	printf("%l phase errors\n", pher);
	if(dflg) {
		i = open(dfile, 1);
		if(i >= 0) {
			write(i, date, 4);
			printf("date updated\n");
			pdate(date);
		}
	}
}

pdate(d)
int *d;
{

	if(d[0] == 0 && d[1] == 0)
		printf("the epoch\n"); else
		printf(ctime(d));
}

dump(ip, sz)
struct inode *ip;
{
	register *p, *q;

	p = dbuf;
	q = ip;
	clrbuf(p);
	while(q < &ip->i_mtime[2])
		*p++ = *q++;
	swrite(dbuf);
	if(ip->i_mode & (IFBLK&IFCHR)) {
		if(sz != 0)
			printf("special\n");
		return;
	}
	for(p = &ip->i_addr[0]; p < &ip->i_addr[8]; p++) {
		if(*p == 0)
			continue;
		if(ip->i_mode&ILARG) {
			bread(*p, ibuf);
			for(q = &ibuf[0]; q < &ibuf[256]; q++) {
				if(*q == 0)
					continue;
				if(--sz < 0)
					goto pe;
				bread(*q, dbuf);
				bwrite(dbuf);
			}
		} else {
			if(--sz < 0)
				goto pe;
			bread(*p, dbuf);
			bwrite(dbuf);
		}
	}
	if(sz)
		goto pe;
	return;

pe:
	clrbuf(dbuf);
	while(--sz >= 0)
		bwrite(dbuf);
	pher++;
}

bread(bno, b)
{

	seek(fi, bno, 3);
	if(read(fi, b, 512) != 512) {
		printf("read error %l\n", bno);
	}
}

clrbuf(b)
int *b;
{
	register i, *p;

	p = b;
	i = 256;
	while(i--)
		*p++ = 0;
}

swrite(b)
int *b;
{
	register i, s, *p;

	i = 255;
	s = 0;
	p = b;
	while(i--)
		s =+ *p++;
	*p = 031415 - s;
	bwrite(b);
}

bwrite(b)
{
	static char *ta;

	if(ta++ > tsize) {
		printf("change tapes\n");
		close(fo);
		rdline();
		ta = 0;
		otape();
	}
wloop:
	if(write(fo, b, 512) != 512) {
		printf("write error\n");
		rdline();
		goto wloop;
	}
}

rdline()
{
	register ta;

	while((ta = getchar()) != '\n')
		if(ta == 0)
			exit();
}

number(s)
char *s;
{
	register n, c;

	n = 0;
	while(c = *s++) {
		if(c<'0' || c>'9')
			continue;
		n = n*10+c-'0';
	}
	return(n);
}

size(s0, s1)
{
	register s;
	extern ldivr;

	s = ldiv(s0&0377, s1, 512);
	if(ldivr)
		s++;
	return(s);
}

otape()
{
	fo = open(ofile, 1);
	if(fo < 0) {
		printf("can not open %s\n", ofile);
		exit();
	}
}
