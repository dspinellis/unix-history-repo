
/* ar11 - archiver for PDP-11 formatted archives */

#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#define ARMAG -155  /* 017545 */
struct ar_hdr {
	char ar_name[14];
	short ar_date1;
	short ar_date2;
	char ar_uid;
	char ar_gid;
	short ar_mode;
	short ar_size1;
	short ar_size2;
};
int ar_date;
int ar_size;
#include <signal.h>
struct	stat	stbuf;
struct	ar_hdr	arbuf;
union ints
{
	struct fun
	{
		short h1;
		short h2;
	};
	int w1;
} x;

#define	SKIP	1
#define	IODD	2
#define	OODD	4
#define	HEAD	8

char	*man	= { "mrxtdp" };
char	*opt	= { "uvnbai" };

int	signum[] = {SIGHUP, SIGINT, SIGQUIT, 0};
int	sigdone();
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
char	buf[512];

char	*trim();
char	*mktemp();
char	*ctime();

main(argc, argv)
char *argv[];
{
	register i;
	register char *cp;

	for(i=0; signum[i]; i++)
		if(signal(signum[i], SIG_IGN) != SIG_IGN)
			signal(signum[i], sigdone);
	if(argc < 3)
		usage();
	cp = argv[1];
	for(cp = argv[1]; *cp; cp++)
	switch(*cp) {
	case 'c':
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
		fprintf(stderr, "ar11: bad option `%c'\n", *cp);
		done(1);
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
			fprintf(stderr, "ar11: one of [%s] must be specified\n", man);
			done(1);
		}
		setcom(rcmd);
	}
	(*comfun)();
	done(notfound());
}

setcom(fun)
int (*fun)();
{

	if(comfun != 0) {
		fprintf(stderr, "ar11: only one of [%s] allowed\n", man);
		done(1);
	}
	comfun = fun;
}

rcmd()
{
	register f;

	init();
	if(getaf() && flg['c'-'a']==0) {
		fprintf(stderr, "ar11: %s does not exist\n", arnam);
		done(1);
	}
	while(!getdir()) {
		bamatch();
		if(namc == 0 || match()) {
			f = stats();
			if(f < 0) {
				if(namc)
					fprintf(stderr, "ar11: cannot open %s\n", file);
				goto cp;
			}
			if(flg['u'-'a'])
				if(stbuf.st_mtime <= ar_date) {
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
			f = creat(file, arbuf.ar_mode & 0777);
			if(f < 0) {
				fprintf(stderr, "ar11: %s cannot create\n", file);
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
			if(flg['v'-'a']) {
				printf("\n<%s>\n\n", file);
				fflush(stdout);
			}
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
		fprintf(stderr, "ar11: cannot create third temp\n");
		done(1);
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
	static short mbuf = ARMAG;

	tfnam = mktemp("/tmp/vXXXXX");
	close(creat(tfnam, 0600));
	tf = open(tfnam, 2);
	if(tf < 0) {
		fprintf(stderr, "ar11: cannot create temp file\n");
		done(1);
	}
	if (write(tf, (char *)&mbuf, sizeof(short)) != sizeof(short))
		wrerr();
}

getaf()
{
	short mbuf;

	af = open(arnam, 0);
	if(af < 0)
		return(1);
	if (read(af, (char *)&mbuf, sizeof(short)) != sizeof(short) || mbuf!=ARMAG) {
		fprintf(stderr, "ar11: %s not in PDP-11 archive format\n", arnam);
		done(1);
	}
	return(0);
}

usage()
{
	printf("usage: ar11 [%s][%s] archive files ...\n", opt, man);
	done(1);
}

noar()
{

	fprintf(stderr, "ar11: %s does not exist\n", arnam);
	done(1);
}

sigdone()
{
	done(100);
}

done(c)
{

	if(tfnam)
		unlink(tfnam);
	if(tf1nam)
		unlink(tf1nam);
	if(tf2nam)
		unlink(tf2nam);
	exit(c);
}

notfound()
{
	register i, n;

	n = 0;
	for(i=0; i<namc; i++)
		if(namv[i]) {
			fprintf(stderr, "ar11: %s not found\n", namv[i]);
			n++;
		}
	return(n);
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
			fprintf(stderr, "ar11: %s cannot open\n", file);
			continue;
		}
		movefil(f);
	}
	install();
}

install()
{
	register i;

	for(i=0; signum[i]; i++)
		signal(signum[i], (int (*)())1);
	close(af);
	af = creat(arnam, 0666);
	if(af < 0) {
		fprintf(stderr, "ar11: cannot create %s\n", arnam);
		done(1);
	}
	lseek(tf, 0l, 0);
	while((i = read(tf, buf, 512)) > 0)
		if (write(af, buf, i) != i)
			wrerr();
	if(tf2nam) {
		lseek(tf2, 0l, 0);
		while((i = read(tf2, buf, 512)) > 0)
			if (write(af, buf, i) != i)
				wrerr();
	}
	if(tf1nam) {
		lseek(tf1, 0l, 0);
		while((i = read(tf1, buf, 512)) > 0)
			if (write(af, buf, i) != i)
				wrerr();
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
		if(arbuf.ar_name[i] = *cp)
			cp++;
	x.w1 = stbuf.st_size;
	arbuf.ar_size1 = x.h2;
	arbuf.ar_size2 = x.h1;
	x.w1 = stbuf.st_mtime;
	arbuf.ar_date1 = x.h2;
	arbuf.ar_date2 = x.h1;
	arbuf.ar_uid = stbuf.st_uid;
	arbuf.ar_gid = stbuf.st_gid;
	arbuf.ar_mode = stbuf.st_mode;
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

	if(flag & HEAD)
		if (write(fo, (char *)&arbuf, sizeof arbuf) != sizeof arbuf)
			wrerr();
	pe = 0;
	while(ar_size > 0) {
		i = o = 512;
		if(ar_size < i) {
			i = o = ar_size;
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
			if (write(fo, buf, o) != o)
				wrerr();
		ar_size -= 512;
	}
	if(pe)
		phserr();
}

getdir()
{
	register i;

	i = read(af, (char *)&arbuf, sizeof arbuf);
	if(i != sizeof arbuf) {
		if(tf1nam) {
			i = tf;
			tf = tf1;
			tf1 = i;
		}
		return(1);
	}
	for(i=0; i<14; i++)
{
		name[i] = arbuf.ar_name[i];
}
	file = name;
	ar_date = swap(&arbuf.ar_date1);
	ar_size = swap(&arbuf.ar_size1);
	return(0);
}

match()
{
	register i;

	for(i=0; i<namc; i++) {
		if(namv[i] == 0)
			continue;
		if(strcmp(trim(namv[i]), file) == 0) {
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
		if(strcmp(file, ponam) != 0)
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
			fprintf(stderr, "ar11: cannot create second temp\n");
			return;
		}
		tf1 = tf;
		tf = f;
	}
}

phserr()
{

	fprintf(stderr, "ar11: phase error on %s\n", file);
}

mesg(c)
{

	if(flg['v'-'a'])
		if(c != 'c' || flg['v'-'a'] > 1)
			printf("%c - %s\n", c, file);
}

char *
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

	pmode();
	printf("%3d/%1d", arbuf.ar_uid, arbuf.ar_gid);
	printf("%7D", ar_size);
	cp = ctime(&ar_date);
	printf(" %-12.12s %-4.4s ", cp+4, cp+20);
}

int	m1[] = { 1, ROWN, 'r', '-' };
int	m2[] = { 1, WOWN, 'w', '-' };
int	m3[] = { 2, SUID, 's', XOWN, 'x', '-' };
int	m4[] = { 1, RGRP, 'r', '-' };
int	m5[] = { 1, WGRP, 'w', '-' };
int	m6[] = { 2, SGID, 's', XGRP, 'x', '-' };
int	m7[] = { 1, ROTH, 'r', '-' };
int	m8[] = { 1, WOTH, 'w', '-' };
int	m9[] = { 2, STXT, 't', XOTH, 'x', '-' };

int	*m[] = { m1, m2, m3, m4, m5, m6, m7, m8, m9};

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
	while (--n>=0 && (arbuf.ar_mode&*ap++)==0)
		ap++;
	putchar(*ap);
}

wrerr()
{
	perror("ar write error");
	done(1);
}

swap(word)
short *word;
{
	x.h1 = ((struct fun *)word)->h2;
	x.h2 = ((struct fun *)word)->h1;
	
	return(x.w1);
}
