#define	NARMAG	0177545
struct
{
	char	name[14];
	long	date;
	char	uid;
	char	gid;
	int	mode;
	long	size;
} arbuf;
struct
{
	char	minor;
	char	major;
	int	inumber;
	int	flags;
	char	nlinks;
	char	usrid;
	char	grpid;
	char	siz0;
	int	siz1;
	int	addr[8];
	long	adate;
	long	mdate;
} stbuf;

#define	SKIP	1
#define	IODD	2
#define	OODD	4
#define	HEAD	8

char	*man	{ "mrxtdp" };
char	*opt	{ "uvnbai" };

long	itol();
int	done();
int	rcmd();
int	dcmd();
int	xcmd();
int	tcmd();
int	pcmd();
int	mcmd();
int	(*comfun)();
char	flg[26];
char	**namv;
int	namc;
char	*arnam;
char	*ponam;
char	*tfnam;
char	*tf1nam;
char	*tf2nam;
char	*file;
char	name[16];
int	af;
int	tf;
int	tf1;
int	tf2;
int	bastate;
int	buf[256];

main(argc, argv)
char *argv[];
{
	register i;
	register char *cp;

	for(i=1; i<4; i++)
		if((signal(i, 1) & 1) == 0)
			signal(i, done);
	if(argc < 3)
		usage();
	cp = argv[1];
	for(cp = argv[1]; *cp; cp++)
	switch(*cp) {
	case 'v':
	case 'u':
	case 'n':
	case 'a':
	case 'b':
	case 'i':
		flg[*cp - 'a']++;
		continue;

	case 'r':
		setcom(rcmd);
		continue;

	case 'd':
		setcom(dcmd);
		continue;

	case 'x':
		setcom(xcmd);
		continue;

	case 't':
		setcom(tcmd);
		continue;

	case 'p':
		setcom(pcmd);
		continue;

	case 'm':
		setcom(mcmd);
		continue;

	default:
		printf("bad option `%c'\n", *cp);
		done();
	}
	if(flg['i'-'a'])
		flg['b'-'a']++;
	if(flg['a'-'a'] || flg['b'-'a']) {
		bastate = 1;
		ponam = trim(argv[2]);
		argv++;
		argc--;
		if(argc < 3)
			usage();
	}
	arnam = argv[2];
	namv = argv+3;
	namc = argc-3;
	if(comfun == 0) {
		if(flg['u'-'a'] == 0) {
			printf("one of [%s] must be specified\n", man);
			done();
		}
		setcom(rcmd);
	}
	(*comfun)();
	notfound();
	done();
}

setcom(fun)
int (*fun)();
{

	if(comfun != 0) {
		printf("only one of [%s] allowed\n", man);
		done();
	}
	comfun = fun;
}

rcmd()
{
	register f;

	init();
	if(getaf()) {
		printf("creating %s\n", arnam);
		cleanup();
		return;
	}
	while(!getdir()) {
		bamatch();
		if(namc == 0 || match()) {
			f = stats();
			if(f < 0) {
				if(namc)
					printf("cannot open %s\n", file);
				goto cp;
			}
			if(flg['u'-'a'])
				if(stbuf.mdate <= arbuf.date) {
					close(f);
					goto cp;
				}
			mesg('r');
			copyfil(af, -1, IODD+SKIP);
			movefil(f);
			continue;
		}
	cp:
		mesg('c');
		copyfil(af, tf, IODD+OODD+HEAD);
	}
	cleanup();
}

dcmd()
{

	init();
	if(getaf())
		noar();
	while(!getdir()) {
		if(match()) {
			mesg('d');
			copyfil(af, -1, IODD+SKIP);
			continue;
		}
		mesg('c');
		copyfil(af, tf, IODD+OODD+HEAD);
	}
	install();
}

xcmd()
{
	register f;

	if(getaf())
		noar();
	while(!getdir()) {
		if(namc == 0 || match()) {
			f = creat(file, arbuf.mode & 0777);
			if(f < 0) {
				printf("%s cannot create\n", file);
				goto sk;
			}
			mesg('x');
			copyfil(af, f, IODD);
			close(f);
			continue;
		}
	sk:
		mesg('c');
		copyfil(af, -1, IODD+SKIP);
	}
}

pcmd()
{

	if(getaf())
		noar();
	while(!getdir()) {
		if(namc == 0 || match()) {
			if(flg['v'-'a'])
				printf("<%s>\n", file);
			copyfil(af, 1, IODD);
			continue;
		}
		copyfil(af, -1, IODD+SKIP);
	}
}

mcmd()
{

	init();
	if(getaf())
		noar();
	tf2nam = mktemp("/tmp/v2XXXXX");
	close(creat(tf2nam, 0600));
	tf2 = open(tf2nam, 2);
	if(tf2 < 0) {
		printf("cannot create third temp\n");
		done();
	}
	while(!getdir()) {
		bamatch();
		if(match()) {
			mesg('m');
			copyfil(af, tf2, IODD+OODD+HEAD);
			continue;
		}
		mesg('c');
		copyfil(af, tf, IODD+OODD+HEAD);
	}
	install();
}

tcmd()
{

	if(getaf())
		noar();
	while(!getdir()) {
		if(namc == 0 || match()) {
			if(flg['v'-'a'])
				longt();
			printf("%s\n", trim(file));
		}
		copyfil(af, -1, IODD+SKIP);
	}
}

init()
{

	tfnam = mktemp("/tmp/vXXXXX");
	close(creat(tfnam, 0600));
	tf = open(tfnam, 2);
	if(tf < 0) {
		printf("cannot create temp file\n");
		done();
	}
	buf[0] = NARMAG;
	if(write(tf, buf, 2) != 2) {
		perror(tfnam);
		done();
	}
}

getaf()
{

	af = open(arnam, 0);
	if(af < 0)
		return(1);
	buf[0] = 0;
	read(af, buf, 2);
	if(buf[0] != NARMAG) {
		printf("%s not in archive format\n", arnam);
		done();
	}
	return(0);
}

usage()
{
	printf("usage: ar [%s][%s] archive files ...\n", opt, man);
	done();
}

noar()
{

	printf("%s does not exist\n", arnam);
	done();
}

done()
{

	if(tfnam)
		unlink(tfnam);
	if(tf1nam)
		unlink(tf1nam);
	if(tf2nam)
		unlink(tf2nam);
	exit();
}

notfound()
{
	register i;

	for(i=0; i<namc; i++)
		if(namv[i])
			printf("%s not found\n", namv[i]);
}

cleanup()
{
	register i, f;

	for(i=0; i<namc; i++) {
		file = namv[i];
		if(file == 0)
			continue;
		namv[i] = 0;
		mesg('a');
		f = stats();
		if(f < 0) {
			printf("%s cannot open\n", file);
			continue;
		}
		movefil(f);
	}
	install();
}

install()
{
	register i;

	for(i=1; i<4; i++)
		signal(i, 1);
	close(af);
	af = creat(arnam, 0644);
	if(af < 0) {
		printf("cannot create %s\n", arnam);
		done();
	}
	seek(tf, 0, 0);
	while((i = read(tf, buf, 512)) > 0)
		if(write(af, buf, i) != i) {
			perror(arnam);
			done();
		}
	if(tf2nam) {
		seek(tf2, 0, 0);
		while((i = read(tf2, buf, 512)) > 0)
			if(write(af, buf, i) != i) {
				perror(arnam);
				done();
			}
	}
	if(tf1nam) {
		seek(tf1, 0, 0);
		while((i = read(tf1, buf, 512)) > 0)
			if(write(af, buf, i) != i) {
				perror(arnam);
				done();
			}
	}
}

/*
 * insert the file 'file'
 * into the temporary file
 */
movefil(f)
{
	register char *cp;
	register i;

	cp = trim(file);
	for(i=0; i<14; i++)
		if(arbuf.name[i] = *cp)
			cp++;
	arbuf.size = itol(stbuf.siz0&0377, stbuf.siz1);
	arbuf.date = stbuf.mdate;
	arbuf.uid = stbuf.usrid;
	arbuf.gid = stbuf.grpid;
	arbuf.mode = stbuf.flags;
	copyfil(f, tf, OODD+HEAD);
	close(f);
}

stats()
{
	register f;

	f = open(file, 0);
	if(f < 0)
		return(f);
	if(fstat(f, &stbuf) < 0) {
		close(f);
		return(-1);
	}
	return(f);
}

/*
 * copy next file
 * size given in arbuf
 */
copyfil(fi, fo, flag)
{
	register i, o;
	int pe;

	if((flag & HEAD) && fo >= 0)
		if(write(fo, &arbuf, sizeof arbuf) != sizeof arbuf) {
			perror("write");
			done();
		}
	pe = 0;
	while(arbuf.size > 0) {
		i = o = 512;
		if(arbuf.size < i) {
			i = o = arbuf.size;
			if(i&1) {
				if(flag & IODD)
					i++;
				if(flag & OODD)
					o++;
			}
		}
		if(read(fi, buf, i) != i)
			pe++;
		if((flag & SKIP) == 0)
			if(write(fo, buf, o) != o && fo >= 0) {
				perror("write");
				done();
			}
		arbuf.size =- 512;
	}
	if(pe)
		phserr();
}

getdir()
{
	register i;

	i = read(af, &arbuf, sizeof arbuf);
	if(i != sizeof arbuf) {
		if(tf1nam) {
			i = tf;
			tf = tf1;
			tf1 = i;
		}
		return(1);
	}
	for(i=0; i<14; i++)
		name[i] = arbuf.name[i];
	file = name;
	return(0);
}

match()
{
	register i;

	for(i=0; i<namc; i++) {
		if(namv[i] == 0)
			continue;
		if(equal(trim(namv[i]), file)) {
			file = namv[i];
			namv[i] = 0;
			return(1);
		}
	}
	return(0);
}

bamatch()
{
	register f;

	switch(bastate) {

	case 1:
		if(!equal(file, ponam))
			return;
		bastate = 2;
		if(flg['a'-'a'])
			return;

	case 2:
		bastate = 0;
		tf1nam = mktemp("/tmp/v1XXXXX");
		close(creat(tf1nam, 0600));
		f = open(tf1nam, 2);
		if(f < 0) {
			printf("cannot create second temp\n");
			return;
		}
		tf1 = tf;
		tf = f;
	}
}

equal(s1, s2)
char *s1, *s2;
{
	register char *p1, *p2;

	p1 = s1;
	p2 = s2;
	while(*p1++ == *p2)
		if(*p2++ == 0)
			return(1);
	return(0);
}

phserr()
{

	printf("phase error on %s\n", file);
}

mesg(c)
{

	if(flg['v'-'a'])
		if(c != 'c' || flg['v'-'a'] > 1)
			printf("%c - %s\n", c, file);
}

trim(s)
char *s;
{
	register char *p1, *p2;

	for(p1 = s; *p1; p1++)
		;
	while(p1 > s) {
		if(*--p1 != '/')
			break;
		*p1 = 0;
	}
	p2 = s;
	for(p1 = s; *p1; p1++)
		if(*p1 == '/')
			p2 = p1+1;
	return(p2);
}

#define	IFMT	060000
#define	ISARG	01000
#define	LARGE	010000
#define	SUID	04000
#define	SGID	02000
#define	ROWN	0400
#define	WOWN	0200
#define	XOWN	0100
#define	RGRP	040
#define	WGRP	020
#define	XGRP	010
#define	ROTH	04
#define	WOTH	02
#define	XOTH	01
#define	STXT	01000

longt()
{
	register char *cp;
	register t;

	pmode();
	printf("%3d/%1d", arbuf.uid&0377, arbuf.gid&0377);
	printf("%6s", locv(arbuf.size));
	cp = ctime(&arbuf.date);
	printf(" %-12.12s %-4.4s ", cp+4, cp+20);
}

int	m1[] { 1, ROWN, 'r', '-' };
int	m2[] { 1, WOWN, 'w', '-' };
int	m3[] { 2, SUID, 's', XOWN, 'x', '-' };
int	m4[] { 1, RGRP, 'r', '-' };
int	m5[] { 1, WGRP, 'w', '-' };
int	m6[] { 2, SGID, 's', XGRP, 'x', '-' };
int	m7[] { 1, ROTH, 'r', '-' };
int	m8[] { 1, WOTH, 'w', '-' };
int	m9[] { 2, STXT, 't', XOTH, 'x', '-' };

int	*m[] { m1, m2, m3, m4, m5, m6, m7, m8, m9};

pmode()
{
	register int **mp;

	for (mp = &m[0]; mp < &m[9];)
		select(*mp++);
}

select(pairp)
int *pairp;
{
	register int n, *ap;

	ap = pairp;
	n = *ap++;
	while (--n>=0 && (arbuf.mode&*ap++)==0)
		ap++;
	putchar(*ap);
}
