static	char *sccsid = "@(#)tar.c	4.6 (Berkeley) 82/02/21";
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ndir.h>
#include <sys/mtio.h>
#include <signal.h>

char	*sprintf();
char	*strcat();
daddr_t	bsrch();
#define TBLOCK	512
#define NBLOCK	20
#define NAMSIZ	100
union hblock {
	char dummy[TBLOCK];
	struct header {
		char name[NAMSIZ];
		char mode[8];
		char uid[8];
		char gid[8];
		char size[12];
		char mtime[12];
		char chksum[8];
		char linkflag;
		char linkname[NAMSIZ];
	} dbuf;
} dblock, tbuf[NBLOCK];

struct linkbuf {
	ino_t	inum;
	dev_t	devnum;
	int	count;
	char	pathname[NAMSIZ];
	struct	linkbuf *nextp;
} *ihead;

struct stat stbuf;

int	rflag, xflag, vflag, tflag, mt, cflag, mflag, fflag, oflag, pflag;
int	term, chksum, wflag, recno, first, linkerrok;
int	freemem = 1;
int	nblock = NBLOCK;

daddr_t	low;
daddr_t	high;

FILE	*tfile;
char	tname[] = "/tmp/tarXXXXXX";


char	*usefile;
char	magtape[]	= "/dev/rmt8";

char	*malloc();

main(argc, argv)
int	argc;
char	*argv[];
{
	char *cp;
	int onintr(), onquit(), onhup(), onterm();

	if (argc < 2)
		usage();

	tfile = NULL;
	usefile =  magtape;
	argv[argc] = 0;
	argv++;
	for (cp = *argv++; *cp; cp++) 
		switch(*cp) {
		case 'f':
			usefile = *argv++;
			fflag++;
			break;
		case 'c':
			cflag++;
			rflag++;
			break;
		case 'o':
			oflag++;
			break;
		case 'p':
			pflag++;
			break;
		case 'u':
			mktemp(tname);
			if ((tfile = fopen(tname, "w")) == NULL) {
				fprintf(stderr, "Tar: cannot create temporary file (%s)\n", tname);
				done(1);
			}
			fprintf(tfile, "!!!!!/!/!/!/!/!/!/! 000\n");
			/* FALL THROUGH */
		case 'r':
			rflag++;
			break;
		case 'v':
			vflag++;
			break;
		case 'w':
			wflag++;
			break;
		case 'x':
			xflag++;
			break;
		case 't':
			tflag++;
			break;
		case 'm':
			mflag++;
			break;
		case '-':
			break;
		case '0':
		case '1':
		case '4':
		case '5':
		case '7':
		case '8':
			magtape[8] = *cp;
			usefile = magtape;
			break;
		case 'b':
			nblock = atoi(*argv++);
			if (nblock > NBLOCK || nblock <= 0) {
				fprintf(stderr, "Invalid blocksize. (Max %d)\n", NBLOCK);
				done(1);
			}
			break;
		case 'l':
			linkerrok++;
			break;
		default:
			fprintf(stderr, "tar: %c: unknown option\n", *cp);
			usage();
		}

	if (rflag) {
		if (cflag && tfile != NULL) {
			usage();
			done(1);
		}
		if (signal(SIGINT, SIG_IGN) != SIG_IGN)
			signal(SIGINT, onintr);
		if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
			signal(SIGHUP, onhup);
		if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
			signal(SIGQUIT, onquit);
/*
		if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
			signal(SIGTERM, onterm);
*/
		if (strcmp(usefile, "-") == 0) {
			if (cflag == 0) {
				fprintf(stderr, "Can only create standard output archives\n");
				done(1);
			}
			mt = dup(1);
			nblock = 1;
		}
		else if ((mt = open(usefile, 2)) < 0) {
			if (cflag == 0 || (mt =  creat(usefile, 0666)) < 0) {
				fprintf(stderr, "tar: cannot open %s\n", usefile);
				done(1);
			}
		}
		dorep(argv);
	}
	else if (xflag)  {
		if (strcmp(usefile, "-") == 0) {
			mt = dup(0);
			nblock = 1;
		}
		else if ((mt = open(usefile, 0)) < 0) {
			fprintf(stderr, "tar: cannot open %s\n", usefile);
			done(1);
		}
		doxtract(argv);
	}
	else if (tflag) {
		if (strcmp(usefile, "-") == 0) {
			mt = dup(0);
			nblock = 1;
		}
		else if ((mt = open(usefile, 0)) < 0) {
			fprintf(stderr, "tar: cannot open %s\n", usefile);
			done(1);
		}
		dotable();
	}
	else
		usage();
	done(0);
}

usage()
{
	fprintf(stderr, "tar: usage  tar -{txru}[cvfblm] [tapefile] [blocksize] file1 file2...\n");
	done(1);
}

dorep(argv)
char	*argv[];
{
	register char *cp, *cp2;
	char wdir[60];

	if (!cflag) {
		getdir();
		do {
			passtape();
			if (term)
				done(0);
			getdir();
		} while (!endtape());
		if (tfile != NULL) {
			char buf[200];

			sprintf(buf, "sort +0 -1 +1nr %s -o %s; awk '$1 != prev {print; prev=$1}' %s >%sX; mv %sX %s",
				tname, tname, tname, tname, tname, tname);
			fflush(tfile);
			system(buf);
			freopen(tname, "r", tfile);
			fstat(fileno(tfile), &stbuf);
			high = stbuf.st_size;
		}
	}

	getwdir(wdir);
	while (*argv && ! term) {
		cp2 = *argv;
		if (!strcmp(cp2, "-C") && argv[1]) {
			argv++;
			if (chdir(*argv) < 0)
				perror(*argv);
			else
				getwdir(wdir);
			argv++;
			continue;
		}
		for (cp = *argv; *cp; cp++)
			if (*cp == '/')
				cp2 = cp;
		if (cp2 != *argv) {
			*cp2 = '\0';
			chdir(*argv);
			*cp2 = '/';
			cp2++;
		}
		putfile(*argv++, cp2);
		chdir(wdir);
	}
	putempty();
	putempty();
	flushtape();
	if (linkerrok == 1)
		for (; ihead != NULL; ihead = ihead->nextp)
			if (ihead->count != 0)
				fprintf(stderr, "Missing links to %s\n", ihead->pathname);
}

endtape()
{
	if (dblock.dbuf.name[0] == '\0') {
		backtape();
		return(1);
	}
	else
		return(0);
}

getdir()
{
	register struct stat *sp;
	int i;

	readtape( (char *) &dblock);
	if (dblock.dbuf.name[0] == '\0')
		return;
	sp = &stbuf;
	sscanf(dblock.dbuf.mode, "%o", &i);
	sp->st_mode = i;
	sscanf(dblock.dbuf.uid, "%o", &i);
	sp->st_uid = i;
	sscanf(dblock.dbuf.gid, "%o", &i);
	sp->st_gid = i;
	sscanf(dblock.dbuf.size, "%lo", &sp->st_size);
	sscanf(dblock.dbuf.mtime, "%lo", &sp->st_mtime);
	sscanf(dblock.dbuf.chksum, "%o", &chksum);
	if (chksum != checksum()) {
		fprintf(stderr, "directory checksum error\n");
		done(2);
	}
	if (tfile != NULL)
		fprintf(tfile, "%s %s\n", dblock.dbuf.name, dblock.dbuf.mtime);
}

passtape()
{
	long blocks;
	char buf[TBLOCK];

	if (dblock.dbuf.linkflag == '1')
		return;
	blocks = stbuf.st_size;
	blocks += TBLOCK-1;
	blocks /= TBLOCK;

	while (blocks-- > 0)
		readtape(buf);
}

putfile(longname, shortname)
char *longname;
char *shortname;
{
	int infile;
	long blocks;
	char buf[TBLOCK];
	register char *cp, *cp2;
	struct direct *dp;
	DIR *dirp;
	int i, j;

	infile = open(shortname, 0);
	if (infile < 0) {
		fprintf(stderr, "tar: %s: cannot open file\n", longname);
		return;
	}

	fstat(infile, &stbuf);

	if (tfile != NULL && checkupdate(longname) == 0) {
		close(infile);
		return;
	}
	if (checkw('r', longname) == 0) {
		close(infile);
		return;
	}

	if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
		for (i = 0, cp = buf; *cp++ = longname[i++];);
		*--cp = '/';
		*++cp = 0  ;
		if (!oflag) {
		    if( (cp - buf) >= NAMSIZ) {
			fprintf(stderr, "%s: file name too long\n", longname);
			close(infile);
			return;
		    }
		    stbuf.st_size = 0;
		    tomodes(&stbuf);
		    strcpy(dblock.dbuf.name,buf);
		    sprintf(dblock.dbuf.chksum, "%6o", checksum());
		    writetape( (char *) &dblock);
		}
		chdir(shortname);
		close(infile);
		if ((dirp = opendir(".")) == NULL) {
			fprintf(stderr, "%s: directory read error\n", longname);
			return;
		}
		while ((dp = readdir(dirp)) != NULL && !term) {
			if (dp->d_ino == 0)
				continue;
			if (!strcmp(".", dp->d_name) || !strcmp("..", dp->d_name))
				continue;
			strcpy(cp, dp->d_name);
			i = telldir(dirp);
			closedir(dirp);
			putfile(buf, cp);
			dirp = opendir(".");
			seekdir(dirp, i);
		}
		closedir(dirp);
		chdir("..");
		return;
	}
	if ((stbuf.st_mode & S_IFMT) != S_IFREG) {
		fprintf(stderr, "tar: %s is not a file. Not dumped\n", longname);
		return;
	}

	tomodes(&stbuf);

	cp2 = longname;
	for (cp = dblock.dbuf.name, i=0; (*cp++ = *cp2++) && i < NAMSIZ; i++);
	if (i >= NAMSIZ) {
		fprintf(stderr, "%s: file name too long\n", longname);
		close(infile);
		return;
	}

	if (stbuf.st_nlink > 1) {
		struct linkbuf *lp;
		int found = 0;

		for (lp = ihead; lp != NULL; lp = lp->nextp) {
			if (lp->inum == stbuf.st_ino && lp->devnum == stbuf.st_dev) {
				found++;
				break;
			}
		}
		if (found) {
			strcpy(dblock.dbuf.linkname, lp->pathname);
			dblock.dbuf.linkflag = '1';
			sprintf(dblock.dbuf.chksum, "%6o", checksum());
			writetape( (char *) &dblock);
			if (vflag) {
				fprintf(stderr, "a %s ", longname);
				fprintf(stderr, "link to %s\n", lp->pathname);
			}
			lp->count--;
			close(infile);
			return;
		}
		else {
			lp = (struct linkbuf *) malloc(sizeof(*lp));
			if (lp == NULL) {
				if (freemem) {
					fprintf(stderr, "Out of memory. Link information lost\n");
					freemem = 0;
				}
			}
			else {
				lp->nextp = ihead;
				ihead = lp;
				lp->inum = stbuf.st_ino;
				lp->devnum = stbuf.st_dev;
				lp->count = stbuf.st_nlink - 1;
				strcpy(lp->pathname, longname);
			}
		}
	}

	blocks = (stbuf.st_size + (TBLOCK-1)) / TBLOCK;
	if (vflag) {
		fprintf(stderr, "a %s ", longname);
		fprintf(stderr, "%ld blocks\n", blocks);
	}
	sprintf(dblock.dbuf.chksum, "%6o", checksum());
	writetape( (char *) &dblock);

	while ((i = read(infile, buf, TBLOCK)) > 0 && blocks > 0) {
		writetape(buf);
		blocks--;
	}
	close(infile);
	if (blocks != 0 || i != 0)
		fprintf(stderr, "%s: file changed size\n", longname);
	while (blocks-- >  0)
		putempty();
}



doxtract(argv)
char	*argv[];
{
	long blocks, bytes;
	char buf[TBLOCK];
	char **cp;
	int ofile;

	for (;;) {
		getdir();
		if (endtape())
			break;

		if (*argv == 0)
			goto gotit;

		for (cp = argv; *cp; cp++)
			if (prefix(*cp, dblock.dbuf.name))
				goto gotit;
		passtape();
		continue;

gotit:
		if (checkw('x', dblock.dbuf.name) == 0) {
			passtape();
			continue;
		}

		if(checkdir(dblock.dbuf.name))
			continue;

		if (dblock.dbuf.linkflag == '1') {
			unlink(dblock.dbuf.name);
			if (link(dblock.dbuf.linkname, dblock.dbuf.name) < 0) {
				fprintf(stderr, "%s: cannot link\n", dblock.dbuf.name);
				continue;
			}
			if (vflag)
				fprintf(stderr, "%s linked to %s\n",
					dblock.dbuf.name, dblock.dbuf.linkname);
			continue;
		}
		if ((ofile = creat(dblock.dbuf.name, stbuf.st_mode & 07777)) < 0) {
			fprintf(stderr, "tar: %s - cannot create\n", dblock.dbuf.name);
			passtape();
			continue;
		}
		chown(dblock.dbuf.name, stbuf.st_uid, stbuf.st_gid);

		blocks = ((bytes = stbuf.st_size) + TBLOCK-1)/TBLOCK;
		if (vflag)
			fprintf(stderr, "x %s, %ld bytes, %ld tape blocks\n",
				dblock.dbuf.name, bytes, blocks);
		while (blocks-- > 0) {
			readtape(buf);
			if (bytes > TBLOCK) {
				if (write(ofile, buf, TBLOCK) < 0) {
					fprintf(stderr, "tar: %s: HELP - extract write error\n", dblock.dbuf.name);
					done(2);
				}
			} else
				if (write(ofile, buf, (int) bytes) < 0) {
					fprintf(stderr, "tar: %s: HELP - extract write error\n", dblock.dbuf.name);
					done(2);
				}
			bytes -= TBLOCK;
		}
		close(ofile);
		if (mflag == 0) {
			time_t timep[2];

			timep[0] = time(NULL);
			timep[1] = stbuf.st_mtime;
			utime(dblock.dbuf.name, timep);
		}
		if (pflag)
		    chmod(dblock.dbuf.name, stbuf.st_mode & 07777);
	}
}

dotable()
{
	for (;;) {
		getdir();
		if (endtape())
			break;
		if (vflag)
			longt(&stbuf);
		printf("%s", dblock.dbuf.name);
		if (dblock.dbuf.linkflag == '1')
			printf(" linked to %s", dblock.dbuf.linkname);
		printf("\n");
		passtape();
	}
}

putempty()
{
	char buf[TBLOCK];
	char *cp;

	for (cp = buf; cp < &buf[TBLOCK]; )
		*cp++ = '\0';
	writetape(buf);
}

longt(st)
register struct stat *st;
{
	register char *cp;
	char *ctime();

	pmode(st);
	printf("%3d/%1d", st->st_uid, st->st_gid);
	printf("%7D", st->st_size);
	cp = ctime(&st->st_mtime);
	printf(" %-12.12s %-4.4s ", cp+4, cp+20);
}

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

pmode(st)
register struct stat *st;
{
	register int **mp;

	for (mp = &m[0]; mp < &m[9];)
		select(*mp++, st);
}

select(pairp, st)
int *pairp;
struct stat *st;
{
	register int n, *ap;

	ap = pairp;
	n = *ap++;
	while (--n>=0 && (st->st_mode&*ap++)==0)
		ap++;
	printf("%c", *ap);
}

checkdir(name)
register char *name;
{
	register char *cp;
	int i;
	for (cp = name; *cp; cp++) {
		if (*cp == '/') {
			*cp = '\0';
			if (access(name, 01) < 0) {
				register int pid, rp;

				if ((pid = fork()) == 0) {
					execl("/bin/mkdir", "mkdir", name, 0);
					execl("/usr/bin/mkdir", "mkdir", name, 0);
					fprintf(stderr, "tar: cannot find mkdir!\n");
					done(0);
				}
				while ((rp = wait(&i)) >= 0 && rp != pid)
					;
				chown(name, stbuf.st_uid, stbuf.st_gid);
				if (pflag)
					chmod(dblock.dbuf.name,
					    stbuf.st_mode & 0777);
			}
			*cp = '/';
		}
	}
	return(cp[-1]=='/');
}

onintr()
{
	signal(SIGINT, SIG_IGN);
	term++;
}

onquit()
{
	signal(SIGQUIT, SIG_IGN);
	term++;
}

onhup()
{
	signal(SIGHUP, SIG_IGN);
	term++;
}

onterm()
{
	signal(SIGTERM, SIG_IGN);
	term++;
}

tomodes(sp)
register struct stat *sp;
{
	register char *cp;

	for (cp = dblock.dummy; cp < &dblock.dummy[TBLOCK]; cp++)
		*cp = '\0';
	sprintf(dblock.dbuf.mode, "%6o ", sp->st_mode & 07777);
	sprintf(dblock.dbuf.uid, "%6o ", sp->st_uid);
	sprintf(dblock.dbuf.gid, "%6o ", sp->st_gid);
	sprintf(dblock.dbuf.size, "%11lo ", sp->st_size);
	sprintf(dblock.dbuf.mtime, "%11lo ", sp->st_mtime);
}

checksum()
{
	register i;
	register char *cp;

	for (cp = dblock.dbuf.chksum; cp < &dblock.dbuf.chksum[sizeof(dblock.dbuf.chksum)]; cp++)
		*cp = ' ';
	i = 0;
	for (cp = dblock.dummy; cp < &dblock.dummy[TBLOCK]; cp++)
		i += *cp;
	return(i);
}

checkw(c, name)
char *name;
{
	if (wflag) {
		printf("%c ", c);
		if (vflag)
			longt(&stbuf);
		printf("%s: ", name);
		if (response() == 'y'){
			return(1);
		}
		return(0);
	}
	return(1);
}

response()
{
	char c;

	c = getchar();
	if (c != '\n')
		while (getchar() != '\n');
	else c = 'n';
	return(c);
}

checkupdate(arg)
char	*arg;
{
	char name[100];
	long	mtime;
	daddr_t seekp;
	daddr_t	lookup();

	rewind(tfile);
	for (;;) {
		if ((seekp = lookup(arg)) < 0)
			return(1);
		fseek(tfile, seekp, 0);
		fscanf(tfile, "%s %lo", name, &mtime);
		if (stbuf.st_mtime > mtime)
			return(1);
		else
			return(0);
	}
}

done(n)
{
	unlink(tname);
	exit(n);
}

prefix(s1, s2)
register char *s1, *s2;
{
	while (*s1)
		if (*s1++ != *s2++)
			return(0);
	if (*s2)
		return(*s2 == '/');
	return(1);
}

getwdir(s)
char *s;
{
	int i;
	int	pipdes[2];

	pipe(pipdes);
	if ((i = fork()) == 0) {
		close(1);
		dup(pipdes[1]);
		execl("/bin/pwd", "pwd", 0);
		execl("/usr/bin/pwd", "pwd", 0);
		fprintf(stderr, "pwd failed!\n");
		printf("/\n");
		exit(1);
	}
	while (wait((int *)NULL) != -1)
			;
	read(pipdes[0], s, 50);
	while(*s != '\n')
		s++;
	*s = '\0';
	close(pipdes[0]);
	close(pipdes[1]);
}

#define	N	200
int	njab;
daddr_t
lookup(s)
char *s;
{
	register i;
	daddr_t a;

	for(i=0; s[i]; i++)
		if(s[i] == ' ')
			break;
	a = bsrch(s, i, low, high);
	return(a);
}

daddr_t
bsrch(s, n, l, h)
daddr_t l, h;
char *s;
{
	register i, j;
	char b[N];
	daddr_t m, m1;

	njab = 0;

loop:
	if(l >= h)
		return(-1L);
	m = l + (h-l)/2 - N/2;
	if(m < l)
		m = l;
	fseek(tfile, m, 0);
	fread(b, 1, N, tfile);
	njab++;
	for(i=0; i<N; i++) {
		if(b[i] == '\n')
			break;
		m++;
	}
	if(m >= h)
		return(-1L);
	m1 = m;
	j = i;
	for(i++; i<N; i++) {
		m1++;
		if(b[i] == '\n')
			break;
	}
	i = cmp(b+j, s, n);
	if(i < 0) {
		h = m;
		goto loop;
	}
	if(i > 0) {
		l = m1;
		goto loop;
	}
	return(m);
}

cmp(b, s, n)
char *b, *s;
{
	register i;

	if(b[0] != '\n')
		exit(2);
	for(i=0; i<n; i++) {
		if(b[i+1] > s[i])
			return(-1);
		if(b[i+1] < s[i])
			return(1);
	}
	return(b[i+1] == ' '? 0 : -1);
}

readtape(buffer)
char *buffer;
{
	register int i;

	if (recno >= nblock || first == 0) {
		if ((i = read(mt, tbuf, TBLOCK*nblock)) < 0) {
			fprintf(stderr, "Tar: tape read error\n");
			done(3);
		}
		if (first == 0) {
			if ((i % TBLOCK) != 0) {
				fprintf(stderr, "Tar: tape blocksize error\n");
				done(3);
			}
			i /= TBLOCK;
			if (i != nblock) {
				fprintf(stderr, "Tar: blocksize = %d\n", i);
				nblock = i;
			}
		}
		recno = 0;
	}
	first = 1;
	copy(buffer, &tbuf[recno++]);
	return(TBLOCK);
}

writetape(buffer)
char *buffer;
{
	first = 1;
	if (recno >= nblock) {
		if (write(mt, tbuf, TBLOCK*nblock) < 0) {
			fprintf(stderr, "Tar: tape write error\n");
			done(2);
		}
		recno = 0;
	}
	copy(&tbuf[recno++], buffer);
	if (recno >= nblock) {
		if (write(mt, tbuf, TBLOCK*nblock) < 0) {
			fprintf(stderr, "Tar: tape write error\n");
			done(2);
		}
		recno = 0;
	}
	return(TBLOCK);
}

backtape()
{
	static int mtdev = 1;
	static struct mtop mtop = {MTBSR, 1};
	struct mtget mtget;

	if (mtdev == 1)
		mtdev = ioctl(mt, MTIOCGET, &mtget);
	if (mtdev == 0) {
		if (ioctl(mt, MTIOCTOP, &mtop) < 0) {
			fprintf(stderr, "Tar: tape backspace error\n");
			done(4);
		}
	} else
		lseek(mt, (long) -TBLOCK*nblock, 1);
	recno--;
}

flushtape()
{
	write(mt, tbuf, TBLOCK*nblock);
}

copy(to, from)
register char *to, *from;
{
	register i;

	i = TBLOCK;
	do {
		*to++ = *from++;
	} while (--i);
}
