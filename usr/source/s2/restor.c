#

/*
 * restore from incremental dumps
 */

char	*dargv[]
{
	0,
	"t",
	0
};
char	*ifile;
char	*ofile;
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

int	isize;
int	*talist;
int	fi;
int	buf[256];
int	dbuf[256];
int	cbuf[256];
char	*date[2];
char	*ddate[2];
int	fo;
int	pher;
char	*tsize	15000;
int	iflg;
int	wflg;
char	file[10];
int	ilist[100];

main(argc, argv)
char **argv;
{
	char *key;
	register *tap, *p;
	register struct inode *ip;
	int i, com, sz, *q, l;

	ifile = "/dev/mt0";
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
		printf("bad characteer in key\n");
		exit();

	case 't':
	case 'r':
	case 'x':
		com = key[-1];
		continue;

	case 'i':
		iflg++;
		continue;

	case '-':
		continue;

	case 'f':
		argv++;
		argc--;
		ifile = *argv;
		continue;

	case 'w':
		wflg++;
		continue;

	}
	otape();
	sread(buf, 0);
	tap = buf;
	isize = *tap++;
	*tap++;		/* fsize */
	date[0] = *tap++;
	date[1] = *tap++;
	ddate[0] = *tap++;
	ddate[1] = *tap++;
	tsize = *tap++;
	i = size(0, isize*32);
	talist = sbrk(i*512);
	tap = talist;
	while(i--) {
		tread(tap, 0);
		tap =+ 256;
	}
	switch(com) {

case 't':
	l = 0;
	com = 0;
	pdate(ddate);
	pdate(date);
	tap = talist;
	for(i=0; i<isize*16; i++) {
		sz = *tap++;
		if(sz == 0 || sz == -1) {
			if(com == 0)
				continue;
			if(i == com) {
				printf("%l", i);
				l =+ 5;
			} else {
				printf("%l-%l", com, i);
				l =+ 10;
			}
			if(l > 60) {
				printf("\n");
				l = 0;
			} else
				printf(",");
			com = 0;
		} else
		if(com == 0)
			com = i+1;
	}
	if(com)
		printf("%l-\n", com);
	exit();

case 'r':
	if(argc <= 1) {
		printf("no filesystem name\n");
		exit();
	}
	ofile = argv[1];
	fo = open(ofile, 2);
	if(fo < 0) {
		printf("can not open %s\n", ofile);
		exit();
	}
	printf("last chance before scribbling on %s\n", ofile);
	getchar();
	dread(1, &sblock);
	tap = talist;
	for(i=0; i<sblock.s_isize; i++) {
		if(i >= isize)
			break;
		dread(i+2, buf);
		for(ip = &buf[0]; ip < &buf[256]; ip++) {
			sz = *tap++;
			if(sz == 0)
				continue;
			dealoc(ip);
			if(sz == -1) {
				for(p = ip; p < &ip->i_mtime[2]; )
					*p++ = 0;
				continue;
			}
			sread(dbuf, 0);
			q = dbuf;
			for(p = ip; p < &ip->i_mtime[2]; )
				*p++ = *q++;
			restor(ip, sz-1);
		}
		dwrite(i+2, buf);
	}
	dwrite(1, &sblock);
	com = 0;
	for(; i < isize; i++)
		for(l = 0; l < 16; l++) {
			sz = *tap++;
			if(sz != 0 && sz != -1)
				com++;
		}
	if(com)
		printf("%l files not restored - small ilist\n", com);
	exit();

case 'x':
	i = 0;
	tap = ilist;
	while(argc > 1) {
		i++;
		sz = number(argv[1]);
		argv++;
		argc--;
		if(sz <= 0 || sz >=isize*16) {
			printf("%l not in range\n", sz);
			continue;
		}
		if(talist[sz-1] == 0) {
			printf("%l not dumped\n", sz);
			continue;
		}
		if(talist[sz-1] == -1) {
			printf("%l does not exist\n", sz);
			continue;
		}
		*tap++ = sz;
	}
	if(i != 0 && ilist[0] == 0)
		exit();
	tap = talist;
	for(i=0; i<isize*16; i++) {
		if(ilist[0] != 0) {
			for(sz=0; ilist[sz]; sz++)
				if(ilist[sz] == i+1)
					goto yes;
			sz = *tap++;
			goto no;
		}
	yes:
		sz = *tap++;
		if(sz == 0 || sz == -1)
			continue;
		sz--;
		tread(buf, 0);
		ip = buf;
		fo = dwait(i+1);
		if(fo < 0) {
		no:
			if(sz == -1)
				sz = 0;
			while(sz--)
				tread(dbuf, 1);
			continue;
		}
		while(sz--) {
			tread(dbuf, 0);
			com = 512;
			if(ip->i_size0 == 0 && ip->i_size1 < 512)
				com = ip->i_size1;
			write(fo, dbuf, com);
			if(com > ip->i_size1)
				ip->i_size0--;
			ip->i_size1 =- com;
		}
		close(fo);
		chmod(file, ip->i_mode);
		chown(file, ip->i_uid);
	}
	exit();

	}
}

dealoc(p)
struct inode *p;
{
	register struct inode *ip;
	register i, j;
	int xbuf[256];

	ip = p;
	if(ip->i_mode & (IFCHR&IFBLK))
		return;
	for(i=0; i<8; i++)
	if(ip->i_addr[i]) {
		if(ip->i_mode&ILARG) {
			dread(ip->i_addr[i], xbuf);
			for(j=0; j<256; j++)
			if(xbuf[j])
				free(xbuf[j]);
		}
		free(ip->i_addr[i]);
	}
}

restor(p, sz)
struct inode *p;
{
	register struct inode *ip;
	register i, j;

	ip = p;
	if(ip->i_mode & (IFCHR&IFBLK))
		return;
	for(i=0; i<8; i++)
		ip->i_addr[i] = 0;
	if(sz <= 8) {
		for(i=0; i<sz; i++)
			ip->i_addr[i] = rcop();
		ip->i_mode =& ~ILARG;
		return;
	}
	j = 0;
	while(sz >= 256) {
		for(i=0; i<256; i++)
			dbuf[i] = rcop();
		ip->i_addr[j] = alloc();
		dwrite(ip->i_addr[j++], dbuf);
		sz =- 256;
	}
	if(sz) {
		for(i=0; i<256; i++)
			dbuf[i] = 0;
		for(i=0; i<sz; i++)
			dbuf[i] = rcop();
		ip->i_addr[j] = alloc();
		dwrite(ip->i_addr[j], dbuf);
	}
	ip->i_mode =| ILARG;
}

rcop()
{
	register b;

	b = alloc();
	tread(cbuf, 0);
	dwrite(b, cbuf);
	return(b);
}

pdate(d)
int *d;
{

	if(d[0] == 0 && d[1] == 0)
		printf("the epoch\n"); else
		printf(ctime(d));
}

dread(bno, b)
{

	seek(fo, bno, 3);
	if(read(fo, b, 512) != 512) {
		printf("read error %l\n", bno);
		exit();
	}
}

dwrite(bno, b)
{

	seek(fo, bno, 3);
	if(write(fo, b, 512) != 512) {
		printf("write error %l\n", bno);
		exit();
	}
}

sread(b, flag)
int *b;
{
	register i, s, *p;

	tread(b, flag);
	if(flag)
		return;
	i = 256;
	s = 0;
	p = b;
	while(i--)
		s =+ *p++;
	if(s != 031415) {
		printf("checksum error\n");
		exit();
	}
}

tread(b, flag)
{
	static char *pta, *ata;

	if(pta++ > tsize) {
		printf("change tapes\n");
		close(fi);
		while((pta = getchar()) != '\n')
			if(pta == 0)
				exit();
		pta = 1;
		ata = 0;
		otape();
	}
	if(flag)
		return;
	ata++;
	if(pta != ata) {
		seek(fi, pta-ata, 4);
		ata = pta;
	}
	if(read(fi, b, 512) != 512) {
		printf("read error\n");
		exit();
	}
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
	fi = open(ifile, 0);
	if(fi < 0) {
		printf("can not open %s\n", ifile);
		exit();
	}
}

dwait(ino)
{
	register i;

	dconv(ino, file);
loop:
	if(wflg) {
		printf("%s ", file);
		i = getchar();
		if(i == 'x')
			exit();
		if(i == '\n')
			return(-1);
		if(i != 'y')
			goto flush;
		i = getchar();
		if(i != '\n') {
		flush:
			while((i=getchar()) != '\n')
				if(i == '\0')
					exit();
			goto loop;
		}
	}
	i = creat(file, 0666);
	return(i);
}

dconv(n, p)
char *p;
{
	register i;

	if(i = ldiv(0, n, 10))
		p = dconv(i, p);
	*p++ = lrem(0, n, 10) + '0';
	*p = '\0';
	return(p);
}

alloc()
{
	register b, i;

	i = --sblock.s_nfree;
	if(i<0 || i>=100) {
		printf("bad freeblock\n");
		exit();
	}
	b = sblock.s_free[i];
	if(b == 0) {
		printf("out of freelist\n");
		exit();
	}
	if(sblock.s_nfree <= 0) {
		dread(b, cbuf);
		sblock.s_nfree = cbuf[0];
		for(i=0; i<100; i++)
			sblock.s_free[i] = cbuf[i+1];
	}
	return(b);
}

free(in)
{
	register i;

	if(sblock.s_nfree >= 100) {
		cbuf[0] = sblock.s_nfree;
		for(i=0; i<100; i++)
			cbuf[i+1] = sblock.s_free[i];
		sblock.s_nfree = 0;
		dwrite(in, cbuf);
	}
	sblock.s_free[sblock.s_nfree++] = in;
}
