#ifndef lint
static	char *sccsid = "@(#)tar.c	4.22 (Berkeley) %G%";
#endif

/*
 * Tape Archival Program
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/time.h>
#include <signal.h>
#include <errno.h>

#define TBLOCK	512
#define NBLOCK	20
#define NAMSIZ	100

#define	writetape(b)	writetbuf(b, 1)
#define	min(a,b)  ((a) < (b) ? (a) : (b))
#define	max(a,b)  ((a) > (b) ? (a) : (b))

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
};

struct linkbuf {
	ino_t	inum;
	dev_t	devnum;
	int	count;
	char	pathname[NAMSIZ];
	struct	linkbuf *nextp;
};

union	hblock dblock;
union	hblock *tbuf;
struct	linkbuf *ihead;
struct	stat stbuf;

int	rflag;
int	xflag;
int	vflag;
int	tflag;
int	cflag;
int	mflag;
int	fflag;
int	iflag;
int	oflag;
int	pflag;
int	wflag;
int	hflag;
int	Bflag;
int	Fflag;

int	mt;
int	term;
int	chksum;
int	recno;
int	first;
int	linkerrok;
int	freemem = 1;
int	nblock = NBLOCK;
int	onintr();
int	onquit();
int	onhup();
int	onterm();

daddr_t	low;
daddr_t	high;
daddr_t	bsrch();

FILE	*vfile = stdout;
FILE	*tfile;
char	tname[] = "/tmp/tarXXXXXX";
char	*usefile;
char	magtape[] = "/dev/rmt8";
char	*malloc();
char	*sprintf();
char	*strcat();
char	*rindex();
char	*getcwd();
char	*getwd();

main(argc, argv)
int	argc;
char	*argv[];
{
	char *cp;

	if (argc < 2)
		usage();

	tfile = NULL;
	usefile =  magtape;
	argv[argc] = 0;
	argv++;
	for (cp = *argv++; *cp; cp++) 
		switch(*cp) {

		case 'f':
			if (*argv == 0) {
				fprintf(stderr,
			"tar: tapefile must be specified with 'f' option\n");
				usage();
			}
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
				fprintf(stderr,
				 "Tar: cannot create temporary file (%s)\n",
				 tname);
				done(1);
			}
			fprintf(tfile, "!!!!!/!/!/!/!/!/!/! 000\n");
			/*FALL THRU*/

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
			if (*argv == 0) {
				fprintf(stderr,
			"tar: blocksize must be specified with 'b' option\n");
				usage();
			}
			nblock = atoi(*argv);
			if (nblock <= 0) {
				fprintf(stderr,
				    "tar: invalid blocksize \"%s\"\n", *argv);
				done(1);
			}
			argv++;
			break;

		case 'l':
			linkerrok++;
			break;

		case 'h':
			hflag++;
			break;

		case 'i':
			iflag++;
			break;

		case 'B':
			Bflag++;
			break;

		case 'F':
			Fflag++;
			break;

		default:
			fprintf(stderr, "tar: %c: unknown option\n", *cp);
			usage();
		}

	if (!rflag && !xflag && !tflag)
		usage();
	if (rflag) {
		if (cflag && tfile != NULL)
			usage();
		if (signal(SIGINT, SIG_IGN) != SIG_IGN)
			signal(SIGINT, onintr);
		if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
			signal(SIGHUP, onhup);
		if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
			signal(SIGQUIT, onquit);
#ifdef notdef
		if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
			signal(SIGTERM, onterm);
#endif
		if (strcmp(usefile, "-") == 0) {
			if (cflag == 0) {
				fprintf(stderr,
			 "tar: can only create standard output archives\n");
				done(1);
			}
			vfile = stderr;
			setlinebuf(vfile);
			mt = dup(1);
			nblock = 1;
		} else if ((mt = open(usefile, 2)) < 0) {
			if (cflag == 0 || (mt =  creat(usefile, 0666)) < 0) {
				fprintf(stderr,
					"tar: cannot open %s\n", usefile);
				done(1);
			}
		}
		dorep(argv);
		done(0);
	}
	if (strcmp(usefile, "-") == 0) {
		mt = dup(0);
		nblock = 1;
	} else if ((mt = open(usefile, 0)) < 0) {
		fprintf(stderr, "tar: cannot open %s\n", usefile);
		done(1);
	}
	if (xflag)
		doxtract(argv);
	else
		dotable();
	done(0);
}

usage()
{
	fprintf(stderr,
"tar: usage: tar -{txru}[cvfblmhopwBi] [tapefile] [blocksize] file1 file2...\n");
	done(1);
}

dorep(argv)
	char *argv[];
{
	register char *cp, *cp2;
	char wdir[MAXPATHLEN], tempdir[MAXPATHLEN], *parent;

	if (!cflag) {
		getdir();
		do {
			passtape();
			if (term)
				done(0);
			getdir();
		} while (!endtape());
		backtape();
		if (tfile != NULL) {
			char buf[200];

			sprintf(buf,
"sort +0 -1 +1nr %s -o %s; awk '$1 != prev {print; prev=$1}' %s >%sX; mv %sX %s",
				tname, tname, tname, tname, tname, tname);
			fflush(tfile);
			system(buf);
			freopen(tname, "r", tfile);
			fstat(fileno(tfile), &stbuf);
			high = stbuf.st_size;
		}
	}

	(void) getcwd(wdir);
	while (*argv && ! term) {
		cp2 = *argv;
		if (!strcmp(cp2, "-C") && argv[1]) {
			argv++;
			if (chdir(*argv) < 0)
				perror(*argv);
			else
				(void) getcwd(wdir);
			argv++;
			continue;
		}
		parent = wdir;
		for (cp = *argv; *cp; cp++)
			if (*cp == '/')
				cp2 = cp;
		if (cp2 != *argv) {
			*cp2 = '\0';
			if (chdir(*argv) < 0) {
				perror(*argv);
				continue;
			}
			parent = getcwd(tempdir);
			*cp2 = '/';
			cp2++;
		}
		putfile(*argv++, cp2, parent);
		if (chdir(wdir) < 0) {
			fprintf(stderr, "cannot change back?: ");
			perror(wdir);
		}
	}
	putempty();
	putempty();
	flushtape();
	if (linkerrok == 0)
		return;
	for (; ihead != NULL; ihead = ihead->nextp) {
		if (ihead->count == 0)
			continue;
		fprintf(stderr, "tar: missing links to %s\n", ihead->pathname);
	}
}

endtape()
{
	return (dblock.dbuf.name[0] == '\0');
}

getdir()
{
	register struct stat *sp;
	int i;

top:
	readtape((char *)&dblock);
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
	if (chksum != (i = checksum())) {
		fprintf(stderr, "tar: directory checksum error (%d != %d)\n",
		    chksum, i);
		if (iflag)
			goto top;
		done(2);
	}
	if (tfile != NULL)
		fprintf(tfile, "%s %s\n", dblock.dbuf.name, dblock.dbuf.mtime);
}

passtape()
{
	long blocks;
	char *bufp;

	if (dblock.dbuf.linkflag == '1')
		return;
	blocks = stbuf.st_size;
	blocks += TBLOCK-1;
	blocks /= TBLOCK;

	while (blocks-- > 0)
		readtbuf(&bufp, TBLOCK);
}

putfile(longname, shortname, parent)
	char *longname;
	char *shortname;
	char *parent;
{
	int infile = 0;
	long blocks;
	char buf[TBLOCK];
	char *origbuf;
	char *bigbuf;
	register char *cp, *cp2;
	struct direct *dp;
	DIR *dirp;
	int i, j;
	char newparent[NAMSIZ+64];
	extern int errno;
	int	maxread;
	int	hint;		/* amount to write to get "in sync" */

	if (!hflag)
		i = lstat(shortname, &stbuf);
	else
		i = stat(shortname, &stbuf);
	if (i < 0) {
		switch (errno) {
		case EACCES:
			fprintf(stderr, "tar: %s: cannot open file\n", longname);
			break;
		case ENOENT:
			fprintf(stderr, "tar: %s: no such file or directory\n",
			    longname);
			break;
		default:
			fprintf(stderr, "tar: %s: cannot stat file\n", longname);
			break;
		}
		return;
	}
	if (tfile != NULL && checkupdate(longname) == 0)
		return;
	if (checkw('r', longname) == 0)
		return;
	if (Fflag && checkf(shortname, stbuf.st_mode, Fflag) == 0)
		return;

	switch (stbuf.st_mode & S_IFMT) {
	case S_IFDIR:
		for (i = 0, cp = buf; *cp++ = longname[i++];)
			;
		*--cp = '/';
		*++cp = 0  ;
		if (!oflag) {
			if ((cp - buf) >= NAMSIZ) {
				fprintf(stderr, "tar: %s: file name too long\n",
				    longname);
				return;
			}
			stbuf.st_size = 0;
			tomodes(&stbuf);
			strcpy(dblock.dbuf.name,buf);
			sprintf(dblock.dbuf.chksum, "%6o", checksum());
			writetape((char *)&dblock);
		}
		sprintf(newparent, "%s/%s", parent, shortname);
		if (chdir(shortname) < 0) {
			perror(shortname);
			return;
		}
		if ((dirp = opendir(".")) == NULL) {
			fprintf(stderr, "tar: %s: directory read error\n",
			    longname);
			if (chdir(parent) < 0) {
				fprintf(stderr, "cannot change back?: ");
				perror(parent);
			}
			return;
		}
		while ((dp = readdir(dirp)) != NULL && !term) {
			if (dp->d_ino == 0)
				continue;
			if (!strcmp(".", dp->d_name) ||
			    !strcmp("..", dp->d_name))
				continue;
			strcpy(cp, dp->d_name);
			i = telldir(dirp);
			closedir(dirp);
			putfile(buf, cp, newparent);
			dirp = opendir(".");
			seekdir(dirp, i);
		}
		closedir(dirp);
		if (chdir(parent) < 0) {
			fprintf(stderr, "cannot change back?: ");
			perror(parent);
		}
		break;

	case S_IFLNK:
		tomodes(&stbuf);
		if (strlen(longname) >= NAMSIZ) {
			fprintf(stderr, "tar: %s: file name too long\n",
			    longname);
			return;
		}
		strcpy(dblock.dbuf.name, longname);
		if (stbuf.st_size + 1 >= NAMSIZ) {
			fprintf(stderr, "tar: %s: symbolic link too long\n",
			    longname);
			return;
		}
		i = readlink(shortname, dblock.dbuf.linkname, NAMSIZ - 1);
		if (i < 0) {
			perror(longname);
			return;
		}
		dblock.dbuf.linkname[i] = '\0';
		dblock.dbuf.linkflag = '2';
		if (vflag) {
			fprintf(vfile, "a %s ", longname);
			fprintf(vfile, "symbolic link to %s\n",
			    dblock.dbuf.linkname);
		}
		sprintf(dblock.dbuf.size, "%11lo", 0);
		sprintf(dblock.dbuf.chksum, "%6o", checksum());
		writetape((char *)&dblock);
		break;

	case S_IFREG:
		if ((infile = open(shortname, 0)) < 0) {
			fprintf(stderr, "tar: %s: cannot open file\n", longname);
			return;
		}
		tomodes(&stbuf);
		if (strlen(longname) >= NAMSIZ) {
			fprintf(stderr, "tar: %s: file name too long\n",
			    longname);
			close(infile);
			return;
		}
		strcpy(dblock.dbuf.name, longname);
		if (stbuf.st_nlink > 1) {
			struct linkbuf *lp;
			int found = 0;

			for (lp = ihead; lp != NULL; lp = lp->nextp)
				if (lp->inum == stbuf.st_ino &&
				    lp->devnum == stbuf.st_dev) {
					found++;
					break;
				}
			if (found) {
				strcpy(dblock.dbuf.linkname, lp->pathname);
				dblock.dbuf.linkflag = '1';
				sprintf(dblock.dbuf.chksum, "%6o", checksum());
				writetape( (char *) &dblock);
				if (vflag) {
					fprintf(vfile, "a %s ", longname);
					fprintf(vfile, "link to %s\n",
					    lp->pathname);
				}
				lp->count--;
				close(infile);
				return;
			}
			lp = (struct linkbuf *) malloc(sizeof(*lp));
			if (lp == NULL) {
				if (freemem) {
					fprintf(stderr,
				"tar: out of memory, link information lost\n");
					freemem = 0;
				}
			} else {
				lp->nextp = ihead;
				ihead = lp;
				lp->inum = stbuf.st_ino;
				lp->devnum = stbuf.st_dev;
				lp->count = stbuf.st_nlink - 1;
				strcpy(lp->pathname, longname);
			}
		}
		blocks = (stbuf.st_size + (TBLOCK-1)) / TBLOCK;
		if (vflag) {
			fprintf(vfile, "a %s ", longname);
			fprintf(vfile, "%ld blocks\n", blocks);
		}
		sprintf(dblock.dbuf.chksum, "%6o", checksum());
		hint = writetape((char *)&dblock);
		maxread = max(stbuf.st_blksize, (nblock * TBLOCK));
		if ((bigbuf = malloc(maxread)) == 0) {
			maxread = TBLOCK;
			bigbuf = buf;
		}

		while ((i = read(infile, bigbuf, min((hint*TBLOCK), maxread))) > 0
		  && blocks > 0) {
		  	register int nblks;

			nblks = ((i-1)/TBLOCK)+1;
		  	if (nblks > blocks)
		  		nblks = blocks;
			hint = writetbuf(bigbuf, nblks);
			blocks -= nblks;
		}
		close(infile);
		if (bigbuf != buf)
			free(bigbuf);
		if (blocks != 0 || i != 0)
			fprintf(stderr, "tar: %s: file changed size\n",
			    longname);
		while (--blocks >=  0)
			putempty();
		break;

	default:
		fprintf(stderr, "tar: %s is not a file. Not dumped\n",
		    longname);
		break;
	}
}

doxtract(argv)
	char *argv[];
{
	long blocks, bytes;
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
		if (Fflag) {
			char *s;

			if ((s = rindex(dblock.dbuf.name, '/')) == 0)
				s = dblock.dbuf.name;
			else
				s++;
			if (checkf(s, stbuf.st_mode, Fflag) == 0) {
				passtape();
				continue;
			}
		}
		if (checkdir(dblock.dbuf.name))
			continue;
		if (dblock.dbuf.linkflag == '2') {
			unlink(dblock.dbuf.name);
			if (symlink(dblock.dbuf.linkname, dblock.dbuf.name)<0) {
				fprintf(stderr, "tar: %s: symbolic link failed\n",
				    dblock.dbuf.name);
				continue;
			}
			if (vflag)
				fprintf(vfile, "x %s symbolic link to %s\n",
				    dblock.dbuf.name, dblock.dbuf.linkname);
			continue;
		}
		if (dblock.dbuf.linkflag == '1') {
			unlink(dblock.dbuf.name);
			if (link(dblock.dbuf.linkname, dblock.dbuf.name) < 0) {
				fprintf(stderr, "tar: %s: cannot link\n",
				    dblock.dbuf.name);
				continue;
			}
			if (vflag)
				fprintf(vfile, "%s linked to %s\n",
				    dblock.dbuf.name, dblock.dbuf.linkname);
			continue;
		}
		if ((ofile = creat(dblock.dbuf.name,stbuf.st_mode&0xfff)) < 0) {
			fprintf(stderr, "tar: %s - cannot create\n",
			    dblock.dbuf.name);
			passtape();
			continue;
		}
		chown(dblock.dbuf.name, stbuf.st_uid, stbuf.st_gid);
		blocks = ((bytes = stbuf.st_size) + TBLOCK-1)/TBLOCK;
		if (vflag)
			fprintf(vfile, "x %s, %ld bytes, %ld tape blocks\n",
			    dblock.dbuf.name, bytes, blocks);
		for (; blocks > 0;) {
			register int nread;
			char	*bufp;
			register int nwant;
			
			nwant = NBLOCK*TBLOCK;
			if (nwant > (blocks*TBLOCK))
				nwant = (blocks*TBLOCK);
			nread = readtbuf(&bufp, nwant);
			if (bytes > nread) {
				if (write(ofile, bufp, nread) < 0) {
					fprintf(stderr,
					"tar: %s: HELP - extract write error\n",
					    dblock.dbuf.name);
					done(2);
				}
			} else if (write(ofile, bufp, (int) bytes) < 0) {
				fprintf(stderr,
				    "tar: %s: HELP - extract write error\n",
				    dblock.dbuf.name);
				done(2);
			}
			bytes -= nread;
			blocks -= (((nread-1)/TBLOCK)+1);
		}
		close(ofile);
		if (mflag == 0) {
			struct timeval tv[2];

			tv[0].tv_sec = time(0);
			tv[0].tv_usec = 0;
			tv[1].tv_sec = stbuf.st_mtime;
			tv[1].tv_usec = 0;
			utimes(dblock.dbuf.name, tv);
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
		if (dblock.dbuf.linkflag == '2')
			printf(" symbolic link to %s", dblock.dbuf.linkname);
		printf("\n");
		passtape();
	}
}

putempty()
{
	char buf[TBLOCK];

	bzero(buf, sizeof (buf));
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

	/*
	 * Quick check for existance of directory.
	 */
	if ((cp = rindex(name, '/')) == 0)
		return (0);
	*cp = '\0';
	if (access(name, 0) >= 0) {
		*cp = '/';
		return (cp[1] == '\0');
	}
	*cp = '/';

	/*
	 * No luck, try to make all directories in path.
	 */
	for (cp = name; *cp; cp++) {
		if (*cp != '/')
			continue;
		*cp = '\0';
		if (access(name, 0) < 0) {
			if (mkdir(name, 0777) < 0) {
				perror(name);
				*cp = '/';
				return (0);
			}
			chown(name, stbuf.st_uid, stbuf.st_gid);
			if (pflag && cp[1] == '\0')
				chmod(name, stbuf.st_mode & 0777);
		}
		*cp = '/';
	}
	return (cp[-1]=='/');
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

	for (cp = dblock.dbuf.chksum;
	     cp < &dblock.dbuf.chksum[sizeof(dblock.dbuf.chksum)]; cp++)
		*cp = ' ';
	i = 0;
	for (cp = dblock.dummy; cp < &dblock.dummy[TBLOCK]; cp++)
		i += *cp;
	return (i);
}

checkw(c, name)
	char *name;
{
	if (!wflag)
		return (1);
	printf("%c ", c);
	if (vflag)
		longt(&stbuf);
	printf("%s: ", name);
	return (response() == 'y');
}

response()
{
	char c;

	c = getchar();
	if (c != '\n')
		while (getchar() != '\n')
			;
	else
		c = 'n';
	return (c);
}

checkf(name, mode, howmuch)
	char *name;
	int mode, howmuch;
{
	int l;

	if ((mode & S_IFMT) == S_IFDIR){
		if ((strcmp(name, "SCCS")==0) || (strcmp(name, "RCS")==0)) 
			return(0); 
		return(1);
	}
	if ((l = strlen(name)) < 3)
		return (1);
	if (howmuch > 1 && name[l-2] == '.' && name[l-1] == 'o')
		return (0);
	if (strcmp(name, "core") == 0 ||
	    strcmp(name, "errs") == 0 ||
	    (howmuch > 1 && strcmp(name, "a.out") == 0))
		return (0);
	/* SHOULD CHECK IF IT IS EXECUTABLE */
	return (1);
}

checkupdate(arg)
	char *arg;
{
	char name[100];
	long mtime;
	daddr_t seekp;
	daddr_t	lookup();

	rewind(tfile);
	for (;;) {
		if ((seekp = lookup(arg)) < 0)
			return (1);
		fseek(tfile, seekp, 0);
		fscanf(tfile, "%s %lo", name, &mtime);
		return (stbuf.st_mtime > mtime);
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
			return (0);
	if (*s2)
		return (*s2 == '/');
	return (1);
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
		if (s[i] == ' ')
			break;
	a = bsrch(s, i, low, high);
	return (a);
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
	if (l >= h)
		return (-1L);
	m = l + (h-l)/2 - N/2;
	if (m < l)
		m = l;
	fseek(tfile, m, 0);
	fread(b, 1, N, tfile);
	njab++;
	for(i=0; i<N; i++) {
		if (b[i] == '\n')
			break;
		m++;
	}
	if (m >= h)
		return (-1L);
	m1 = m;
	j = i;
	for(i++; i<N; i++) {
		m1++;
		if (b[i] == '\n')
			break;
	}
	i = cmp(b+j, s, n);
	if (i < 0) {
		h = m;
		goto loop;
	}
	if (i > 0) {
		l = m1;
		goto loop;
	}
	return (m);
}

cmp(b, s, n)
	char *b, *s;
{
	register i;

	if (b[0] != '\n')
		exit(2);
	for(i=0; i<n; i++) {
		if (b[i+1] > s[i])
			return (-1);
		if (b[i+1] < s[i])
			return (1);
	}
	return (b[i+1] == ' '? 0 : -1);
}

readtape (buffer)
	char *buffer;
{
	char *bufp;
	int nread;

	if (first==0) getbuf();
	readtbuf (&bufp, TBLOCK);
	bcopy(bufp, buffer, TBLOCK);
	return(TBLOCK);
}

readtbuf(bufpp, size)
	char **bufpp;
	int size;
{
	register int i;

	if (recno >= nblock || first == 0) {
		if ((i = bread(mt, tbuf, TBLOCK*nblock)) < 0) {
			fprintf(stderr, "tar: tape read error\n");
			done(3);
		}
		if (first == 0) {
			if ((i % TBLOCK) != 0) {
				fprintf(stderr, "tar: tape blocksize error\n");
				done(3);
			}
			i /= TBLOCK;
			if (i != nblock) {
				fprintf(stderr, "tar: blocksize = %d\n", i);
				nblock = i;
			}
		}
		recno = 0;
	}
	first = 1;
	if (size > ((nblock-recno)*TBLOCK))
		size = (nblock-recno)*TBLOCK;
	*bufpp = (char *)&tbuf[recno];
	recno += (size/TBLOCK);
	return (size);
}

writetbuf(buffer, n)
	register char *buffer;
	register int n;
{
	if (first==0) getbuf();
	first = 1;
	if (recno >= nblock) {
		if (write(mt, tbuf, TBLOCK*nblock) < 0) {
			fprintf(stderr, "tar: tape write error\n");
			done(2);
		}
		recno = 0;
	}

	/*
	 *  Special case:  We have an empty tape buffer, and the
	 *  users data size is >= the tape block size:  Avoid
	 *  the bcopy and dma direct to tape.  BIG WIN.  Add the
	 *  residual to the tape buffer.
	 */
	while (recno == 0 && n >= nblock) {
		if (write(mt, buffer, TBLOCK*nblock) < 0) {
			fprintf(stderr, "tar: tape write error\n");
			done(2);
		}
		n -= nblock;
		buffer += (nblock * TBLOCK);
	}
		
	while (n-- > 0) {
		bcopy(buffer, (char *)&tbuf[recno++], TBLOCK);
		buffer += TBLOCK;
		if (recno >= nblock) {
			if (write(mt, tbuf, TBLOCK*nblock) < 0) {
				fprintf(stderr, "tar: tape write error\n");
				done(2);
			}
			recno = 0;
		}
	}

	/* Tell the user how much to write to get in sync */
	return (nblock - recno);
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
			fprintf(stderr, "tar: tape backspace error\n");
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

bread(fd, buf, size)
	int fd;
	char *buf;
	int size;
{
	int count;
	static int lastread = 0;

	if (!Bflag)
		return (read(fd, buf, size));
	for (count = 0; count < size; count += lastread) {
		if (lastread < 0) {
			if (count > 0)
				return (count);
			return (lastread);
		}
		lastread = read(fd, buf, size - count);
		buf += lastread;
	}
	return (count);
}

char *
getcwd(buf)
	char *buf;
{

	if (getwd(buf) == NULL) {
		fprintf(stderr, "tar: %s\n", buf);
		exit(1);
	}
	return (buf);
}

getbuf()
{
#ifdef nodef
	if ( mtdev == 1) {
		fstat(mt, &stbuf);
		if ((stbuf.st_mode & S_IFMT) == S_IFCHR)
			mtdev = 0;
		else
			mtdev = -1;
	}
	if (first==0 && nblock==0) {
		if (mtdev == 0)
			nblock = FILEBLOCK;
		else {
			fstat(mt, &stbuf);
			nblock = stbuf.st_blocks / TBLOCK;
		}
	}
	if (nblock ==0) nblock = FILEBLOCK;
#endif nodef
	tbuf = (union hblock *)malloc(nblock*TBLOCK);
	if (tbuf == NULL) {
		fprintf(stderr, "tar: blocksize %d too big, can't get memory\n",
		    nblock);
		done(1);
	}
}
