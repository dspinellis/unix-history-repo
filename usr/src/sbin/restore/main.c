/* Copyright (c) 1981 Regents of the University of California */

char version[] = "@(#)main.c 2.3 %G%";

/*	Modified to include h option (recursively extract all files within
 *	a subtree) and m option (recreate the heirarchical structure of
 *	that subtree and move extracted files to their proper homes).
 *	8/29/80		by Mike Litzkow
 *
 *	Includes the s (skip files) option for use with multiple dumps on
 *	a single tape.
 */

#define MAXINO	3000
#define BITS	8
#define NCACHE	3
#define SIZEINC 10

#include <stdio.h>
#include <signal.h>
#include <fstab.h>
#include "../h/param.h"
#include "../h/dir.h"
#include "../h/stat.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dumprestor.h"
#include <sys/mtio.h>

#define ODIRSIZ 14
struct odirect {
	u_short	d_ino;
	char	d_name[ODIRSIZ];
};

#define	MWORD(m,i) (m[(unsigned)(i-1)/NBBY])
#define	MBIT(i)	(1<<((unsigned)(i-1)%NBBY))
#define	BIS(i,w)	(MWORD(w,i) |=  MBIT(i))
#define	BIC(i,w)	(MWORD(w,i) &= ~MBIT(i))
#define	BIT(i,w)	(MWORD(w,i) & MBIT(i))

ino_t	ino, maxi;
struct inode *cur_ip;

int	eflag = 0, hflag = 0, mflag = 0, cvtdir = 0;

long	fssize;
dev_t	dev = 0;
char	tapename[] = "/dev/rmt8";
char	*magtape = tapename;
int	mt;
int	dumpnum = 1;
int	volno = 1;
int	curblk = 0;
int	bct = NTREC+1;
char	tbf[NTREC*TP_BSIZE];

daddr_t	seekpt;
FILE	*df;
DIR	*dirp;
int	ofile;
char	dirfile[] = "rstXXXXXX";
char	lnkbuf[MAXPATHLEN + 1];
int	pathlen;

#define INOHASH(val) (val % MAXINO)
struct inotab {
	struct inotab *t_next;
	ino_t	t_ino;
	daddr_t	t_seekpt;
} *inotab[MAXINO];

#define XISDIR	1
#define XTRACTD	2
#define XINUSE	4
#define XLINKED	8
struct xtrlist {
	struct xtrlist	*x_next;
	struct xtrlist	*x_linkedto;
	time_t		x_timep[2];
	ino_t		x_ino;
	char		x_flags;
	char 		x_name[1];
	/* actually longer */
} *xtrlist[MAXINO];
int xtrcnt = 0;

int	msiz;
char	*dumpmap;
char	*clrimap;

char	clearedbuf[MAXBSIZE];

extern char *ctime();
ino_t search();
int dirwrite();

main(argc, argv, arge)
	int argc;
	char *argv[];
	char **arge;
{
	register char *cp;
	char command;
	int (*signal())();
	int done();

	if (signal(SIGINT, done) == SIG_IGN)
		signal(SIGINT, SIG_IGN);
	if (signal(SIGTERM, done) == SIG_IGN)
		signal(SIGTERM, SIG_IGN);
	mktemp(dirfile);
	if (argc < 2) {
usage:
		fprintf(stderr, "Usage: restor x[s|m|h|v] file file..., restor r|R filesys, or restor t\n");
		done(1);
	}
	argv++;
	argc -= 2;
	for (cp = *argv++; *cp; cp++) {
		switch (*cp) {
		case '-':
			break;
		case 'f':
			magtape = *argv++;
			argc--;
			break;
		/* s dumpnum (skip to) for multifile dump tapes */
		case 's':
			dumpnum = atoi(*argv++);
			if(dumpnum <= 0) {
				fprintf(stderr, "Dump number must be a positive integer\n");
				done(1);
			}
			argc--;
			break;
		case 'h':
			hflag++;
			break;
		case 'm':
			mflag++;
			break;
		case 'r':
		case 'R':
			hflag++;
			mflag++;
		case 't':
		case 'x':
			command = *cp;
			break;
		default:
			fprintf(stderr, "Bad key character %c\n", *cp);
			goto usage;
		}
	}
	doit(command, argc, argv);
	done(0);
}

doit(command, argc, argv)
	char	command;
	int	argc;
	char	*argv[]; 
{
	struct mtop tcom;

	if ((mt = open(magtape, 0)) < 0) {
		fprintf(stderr, "%s: cannot open tape\n", magtape);
		done(1);
	}
	if (dumpnum != 1) {
		tcom.mt_op = MTFSF;
		tcom.mt_count = dumpnum -1;
		if (ioctl(mt,MTIOCTOP,&tcom) < 0)
			perror("ioctl MTFSF");
	}
	blkclr(clearedbuf, MAXBSIZE);
	switch(command) {
	case 't':
		if (readhdr(&spcl) == 0) {
			fprintf(stderr, "Tape is not a dump tape\n");
			done(1);
		}
		fprintf(stdout, "Dump   date: %s", ctime(&spcl.c_date));
		fprintf(stdout, "Dumped from: %s", ctime(&spcl.c_ddate));
		return;
	case 'R':
	case 'r':
		setdir(*argv);
		argc = 1;
		*argv = ".";
		/* and then extract it all */
	case 'x':
		df = fopen(dirfile, "w");
		if (df == 0) {
			fprintf(stderr, "restor: %s - cannot create directory temporary\n", dirfile);
			done(1);
		}
		extractfiles(argc, argv);
		return;
	}
}

extractfiles(argc, argv)
	int argc;
	char **argv;
{
	char	*ststore();
	register struct xtrlist *xp;
	struct xtrlist **xpp;
	ino_t	d;
	int	xtrfile(), xtrskip(), xtrcvtdir(), xtrcvtskip(),
		xtrlnkfile(), xtrlnkskip(), null();
	int	mode, uid, gid, i;
	char	name[BUFSIZ + 1];
	struct	stat stbuf;

	if (stat(".", &stbuf) < 0) {
		fprintf(stderr, "cannot stat .\n");
		done(1);
	}
	/*
	 * should be!!!
	 *
	fssize = stbuf.st_blksize;
	 */
	fssize = MAXBSIZE;
	if (readhdr(&spcl) == 0) {
		fprintf(stderr, "Tape is not a dump tape\n");
		done(1);
	}
	if (checkvol(&spcl, 1) == 0) {
		fprintf(stderr, "Tape is not volume 1 of the dump\n");
	}
	clrimap = 0;
	dumpmap = 0;
	pass1(1);  /* This sets the various maps on the way by */
	while (argc--) {
		if ((d = psearch(*argv)) == 0 || BIT(d,dumpmap) == 0) {
			printf("d = %d\n", d);
			fprintf(stdout, "%s: not on tape\n", *argv++);
			continue;
		}
		if (mflag)
			checkdir(*argv);
		if(hflag)
			getleaves(d, *argv++);
		else
			allocxtr(d, *argv++, XINUSE);
	}
	if (dumpnum > 1) {
		/*
		 * if this is a multi-dump tape we always start with 
		 * volume 1, so as to avoid accidentally restoring
		 * from a different dump!
		 */
		resetmt();
		dumpnum = 1;
		volno = 1;
		readhdr(&spcl);
		goto rbits;
	}
newvol:
	resetmt();
getvol:
	fprintf(stderr, "Mount desired tape volume; Specify volume #: ");
	if (gets(tbf) == NULL)
		return;
	volno = atoi(tbf);
	if (volno <= 0) {
		fprintf(stderr, "Volume numbers are positive numerics\n");
		goto getvol;
	}
	if (readhdr(&spcl) == 0) {
		fprintf(stderr, "tape is not dump tape\n");
		goto newvol;
	}
	if (checkvol(&spcl, volno) == 0) {
		fprintf(stderr, "Wrong volume (%d)\n", spcl.c_volume);
		goto newvol;
	}
rbits:
	while (gethead(&spcl) == 0)
		;
	if (checktype(&spcl, TS_INODE) == 1) {
		fprintf(stderr, "Can't find inode mask!\n");
		goto newvol;
	}
	if (checktype(&spcl, TS_BITS) == 0)
		goto rbits;
	readbits(&dumpmap);
	while (xtrcnt > 0) {
again:
		if (ishead(&spcl) == 0) {
			i = 0;
			while(gethead(&spcl) == 0)
				i++;
			fprintf(stderr, "resync restor, skipped %i blocks\n",
			    i);
		}
		if (checktype(&spcl, TS_END) == 1) {
			fprintf(stderr, "end of tape\n");
			break;
		}
		if (checktype(&spcl, TS_INODE) == 0) {
			gethead(&spcl);
			goto again;
		}
		d = spcl.c_inumber;
		for (xp = xtrlist[INOHASH(d)]; xp; xp = xp->x_next) {
			if (d != xp->x_ino)
				continue;
			if (xp->x_flags & XLINKED)
				continue;
			xp->x_timep[0] = spcl.c_dinode.di_atime;
			xp->x_timep[1] = spcl.c_dinode.di_mtime;
			mode = spcl.c_dinode.di_mode;
			if (mflag)
				strcpy(name, xp->x_name);
			else
				sprintf(name, "%u", xp->x_ino);
			switch (mode & IFMT) {
			default:
				fprintf(stderr, "%s: unknown file type\n", name);
				xp->x_flags |= XTRACTD;
				xtrcnt--;
				goto skipfile;
			case IFCHR:
			case IFBLK:
				fprintf(stdout, "extract special file %s\n", name);
				if (mknod(name, mode, spcl.c_dinode.di_rdev)) {
					fprintf(stderr, "%s: cannot create special file\n", name);
					xp->x_flags |= XTRACTD;
					xtrcnt--;
					goto skipfile;
				}
				getfile(null, null, spcl.c_dinode.di_size);
				break;
			case IFDIR:
				if (mflag) {
					fprintf(stdout, "extract directory %s\n", name);
					strncat(name, "/.", BUFSIZ);
					checkdir(name);
					chown(xp->x_name, spcl.c_dinode.di_uid, spcl.c_dinode.di_gid);
					getfile(null, null, spcl.c_dinode.di_size);
					break;
				}
				fprintf(stdout, "extract file %s\n", name);
				if ((ofile = creat(name, 0666)) < 0) {
					fprintf(stderr, "%s: cannot create file\n", name);
					xp->x_flags |= XTRACTD;
					xtrcnt--;
					goto skipfile;
				}
				chown(name, spcl.c_dinode.di_uid, spcl.c_dinode.di_gid);
				if (cvtdir) {
					getfile(xtrcvtdir, xtrcvtskip,
					    spcl.c_dinode.di_size);
					flushent(xtrfile);
				} else
					getfile(xtrfile, xtrskip,
					    spcl.c_dinode.di_size);
				close(ofile);
				break;
			case IFLNK:
				fprintf(stdout, "extract symbolic link %s\n", name);
				uid = spcl.c_dinode.di_uid;
				gid = spcl.c_dinode.di_gid;
				lnkbuf[0] = '\0';
				pathlen = 0;
				getfile(xtrlnkfile, xtrlnkskip, spcl.c_dinode.di_size);
				if (symlink(lnkbuf, name) < 0) {
					fprintf(stderr, "%s: cannot create symbolic link\n", name);
					xp->x_flags |= XTRACTD;
					xtrcnt--;
					goto finished;
				}
				chown(name, uid, gid);
				break;
			case IFREG:
				fprintf(stdout, "extract file %s\n", name);
				if ((ofile = creat(name, 0666)) < 0) {
					fprintf(stderr, "%s: cannot create file\n", name);
					xp->x_flags |= XTRACTD;
					xtrcnt--;
					goto skipfile;
				}
				chown(name, spcl.c_dinode.di_uid, spcl.c_dinode.di_gid);
				getfile(xtrfile, xtrskip, spcl.c_dinode.di_size);
				close(ofile);
				break;
			}
			chmod(name, mode);
			utime(name, xp->x_timep);
			xp->x_flags |= XTRACTD;
			xtrcnt--;
			goto finished;
		}
skipfile:
		getfile(null, null, spcl.c_dinode.di_size);
finished:
		;
	}
	if (xtrcnt == 0 && !mflag)
		return;
	for (xpp = xtrlist; xpp < &xtrlist[MAXINO]; xpp++) {
		for (xp = *xpp; xp; xp = xp->x_next) {
			if (mflag && (xp->x_flags & XISDIR))
				utime(xp->x_name, xp->x_timep);
			if (xp->x_flags & XTRACTD)
				continue;
			if ((xp->x_flags & XLINKED) == 0) {
				fprintf(stderr, "cannot find file %s\n",
					xp->x_name);
				continue;
			}
			if (!mflag)
				continue;
			fprintf(stdout, "link %s to %s\n",
				xp->x_linkedto->x_name, xp->x_name);
			if (link(xp->x_linkedto->x_name, xp->x_name) < 0)
				fprintf(stderr, "link %s to %s failed\n",
					xp->x_linkedto->x_name, xp->x_name);
		}
	}
}

/*
 * Read the tape, bulding up a directory structure for extraction
 * by name
 */
pass1(savedir)
	int savedir;
{
	register int i;
	register struct dinode *ip;
	struct direct nulldir;
	char buf[TP_BSIZE];
	int putdir(), null(), dirwrite();

	nulldir.d_ino = 1;
	nulldir.d_namlen = 1;
	strncpy(nulldir.d_name, "/", nulldir.d_namlen);
	nulldir.d_reclen = DIRSIZ(&nulldir);
	while (gethead(&spcl) == 0) {
		fprintf(stderr, "Can't find directory header!\n");
	}
	for (;;) {
		if (checktype(&spcl, TS_BITS) == 1) {
			readbits(&dumpmap);
			continue;
		}
		if (checktype(&spcl, TS_CLRI) == 1) {
			readbits(&clrimap);
			continue;
		}
		if (checktype(&spcl, TS_INODE) == 0) {
finish:
			if (savedir) {
				fclose(df);
				dirp = opendir(dirfile);
				if (dirp == NULL)
					perror("opendir");
			}
			resetmt();
			return;
		}
		ip = &spcl.c_dinode;
		i = ip->di_mode & IFMT;
		if (i != IFDIR) {
			goto finish;
		}
		if (spcl.c_inumber == ROOTINO) {
			readtape(buf);
			bct--; /* push back this block */
			if (((struct direct *)buf)->d_ino != ROOTINO) {
				if (((struct odirect *)buf)->d_ino != ROOTINO) {
					fprintf(stderr, "bad root directory\n");
					done(1);
				}
				fprintf(stderr, "converting to new directory format\n");
				cvtdir = 1;
			}
			if (!savedir && !cvtdir) {
				/* if no conversion, just return */
				goto finish;
			}
		}
		allocinotab(spcl.c_inumber, seekpt);
		if (savedir) {
			getfile(putdir, null, spcl.c_dinode.di_size);
			putent(&nulldir, dirwrite);
			flushent(dirwrite);
		} else {
			getfile(null, null, spcl.c_dinode.di_size);
		}
	}
}

/*
 * Put the directory entries in the directory file
 */
putdir(buf, size)
	char *buf;
	int size;
{
	struct direct cvtbuf;
	register struct odirect *odp;
	struct odirect *eodp;
	register struct direct *dp;
	long loc, i;

	if (cvtdir) {
		eodp = (struct odirect *)&buf[size];
		for (odp = (struct odirect *)buf; odp < eodp; odp++)
			if (odp->d_ino != 0) {
				dcvt(odp, &cvtbuf);
				putent(&cvtbuf, dirwrite);
			}
	} else {
		for (loc = 0; loc < size; ) {
			dp = (struct direct *)(buf + loc);
			i = DIRBLKSIZ - (loc & (DIRBLKSIZ - 1));
			if (dp->d_reclen <= 0 || dp->d_reclen > i) {
				loc += i;
				continue;
			}
			loc += dp->d_reclen;
			if (dp->d_ino != 0)
				putent(dp, dirwrite);
		}
	}
}

/*
 *	Recursively find names and inumbers of all files in subtree 
 *	pname and put them in xtrlist[]
 */
getleaves(ino, pname)
	ino_t ino;
	char *pname;
{
	register struct inotab *itp;
	int namelen;
	long bpt;
	register struct direct *dp;
	char locname[BUFSIZ + 1];

	if (BIT(ino, dumpmap) == 0) {
		fprintf(stdout, "%s: not on the tape\n", pname);
		return;
	}
	for (itp = inotab[INOHASH(ino)]; itp; itp = itp->t_next) {
		if (itp->t_ino != ino)
			continue;
		/*
		 * pname is a directory name 
		 */
		allocxtr(ino, pname, XISDIR);
		/*
		 * begin search through the directory
		 * skipping over "." and ".."
		 */
		strncpy(locname, pname, BUFSIZ);
		strncat(locname, "/", BUFSIZ);
		namelen = strlen(locname);
		seekdir(dirp, itp->t_seekpt, itp->t_seekpt);
		dp = readdir(dirp);
		dp = readdir(dirp);
		dp = readdir(dirp);
		bpt = telldir(dirp);
		/*
		 * "/" signals end of directory
		 */
		while (dp->d_namlen != 1 || dp->d_name[0] != '/') {
			locname[namelen] = '\0';
			if (namelen + dp->d_namlen >= BUFSIZ) {
				fprintf(stderr, "%s%s: name exceedes %d char\n",
					locname, dp->d_name, BUFSIZ);
				continue;
			}
			strncat(locname, dp->d_name, dp->d_namlen);
			getleaves(dp->d_ino, locname);
			seekdir(dirp, bpt, itp->t_seekpt);
			dp = readdir(dirp);
			bpt = telldir(dirp);
		}
		return;
	}
	/*
	 * locname is name of a simple file 
	 */
	allocxtr(ino, pname, XINUSE);
}

/*
 * Search the directory tree rooted at inode ROOTINO
 * for the path pointed at by n
 */
psearch(n)
	char	*n;
{
	register char *cp, *cp1;
	char c;

	ino = ROOTINO;
	if (*(cp = n) == '/')
		cp++;
next:
	cp1 = cp + 1;
	while (*cp1 != '/' && *cp1)
		cp1++;
	c = *cp1;
	*cp1 = 0;
	ino = search(ino, cp);
	if (ino == 0) {
		*cp1 = c;
		return(0);
	}
	*cp1 = c;
	if (c == '/') {
		cp = cp1+1;
		goto next;
	}
	return(ino);
}

/*
 * search the directory inode ino
 * looking for entry cp
 */
ino_t
search(inum, cp)
	ino_t	inum;
	char	*cp;
{
	register struct direct *dp;
	register struct inotab *itp;
	int len;

	for (itp = inotab[INOHASH(inum)]; itp; itp = itp->t_next)
		if (itp->t_ino == inum)
			goto found;
	return(0);
found:
	seekdir(dirp, itp->t_seekpt, itp->t_seekpt);
	len = strlen(cp);
	do {
		dp = readdir(dirp);
		if (dp->d_namlen == 1 && dp->d_name[0] == '/')
			return(0);
	} while (dp->d_namlen != len || strncmp(dp->d_name, cp, len));
	return(dp->d_ino);
}

/*
 * Do the file extraction, calling the supplied functions
 * with the blocks
 */
getfile(f1, f2, size)
	int	(*f2)(), (*f1)();
	long	size;
{
	register int i;
	char buf[MAXBSIZE / TP_BSIZE][TP_BSIZE];
	union u_spcl addrblk;
#	define addrblock addrblk.s_spcl

	addrblock = spcl;
	for (;;) {
		for (i = 0; i < addrblock.c_count; i++) {
			if (addrblock.c_addr[i]) {
				readtape(&buf[curblk++][0]);
				if (curblk == fssize / TP_BSIZE) {
					(*f1)(buf, size > TP_BSIZE ?
					     (long) (fssize) :
					     (curblk - 1) * TP_BSIZE + size);
					curblk = 0;
				}
			} else {
				if (curblk > 0) {
					(*f1)(buf, size > TP_BSIZE ?
					     (long) (curblk * TP_BSIZE) :
					     (curblk - 1) * TP_BSIZE + size);
					curblk = 0;
				}
				(*f2)(clearedbuf, size > TP_BSIZE ?
					(long) TP_BSIZE : size);
			}
			if ((size -= TP_BSIZE) <= 0) {
eloop:
				while (gethead(&spcl) == 0)
					;
				if (checktype(&spcl, TS_ADDR) == 1)
					goto eloop;
				goto out;
			}
		}
		if (gethead(&addrblock) == 0) {
			fprintf(stderr, "Missing address (header) block, ino%u\n", ino);
			goto eloop;
		}
		if (checktype(&addrblock, TS_ADDR) == 0) {
			spcl = addrblock;
			goto out;
		}
	}
out:
	if (curblk > 0) {
		(*f1)(buf, (curblk * TP_BSIZE) + size);
		curblk = 0;
	}
}

/*
 * The next routines are called during file extraction to
 * put the data into the right form and place.
 */
xtrfile(buf, size)
	char	*buf;
	long	size;
{
	if (write(ofile, buf, (int) size) == -1) {
		perror("extract write");
		done(1);
	}
}

xtrskip(buf, size)
	char *buf;
	long size;
{
	if (lseek(ofile, size, 1) == -1) {
		perror("extract seek");
		done(1);
	}
}

xtrcvtdir(buf, size)
	struct odirect *buf;
	long size;
{
	struct odirect *odp, *edp;
	struct direct *dp, cvtbuf;

	edp = &buf[size / sizeof(struct odirect)];
	for (odp = buf; odp < edp; odp++) {
		dcvt(odp, &cvtbuf);
		putent(&cvtbuf, xtrfile);
	}
}

xtrcvtskip(buf, size)
	char *buf;
	long size;
{
	fprintf(stderr, "unallocated block in directory\n");
	xtrskip(buf, size);
}

xtrlnkfile(buf, size)
	char	*buf;
	long	size;
{
	pathlen += size;
	if (pathlen > MAXPATHLEN) {
		fprintf(stderr, "symbolic link name: %s; too long %d\n",
		    buf, size);
		done(1);
	}
	strcat(lnkbuf, buf);
}

xtrlnkskip(buf, size)
	char *buf;
	long size;
{
	fprintf(stderr, "unallocated block in symbolic link\n");
	done(1);
}

null() {;}

/*
 * Do the tape i/o, dealing with volume changes
 * etc..
 */
readtape(b)
	char *b;
{
	register int i;
	struct s_spcl tmpbuf;
	char c;

	if (bct >= NTREC) {
		for (i = 0; i < NTREC; i++)
			((struct s_spcl *)&tbf[i*TP_BSIZE])->c_magic = 0;
		bct = 0;
		if ((i = read(mt, tbf, NTREC*TP_BSIZE)) < 0) {
			fprintf(stderr, "Tape read error, continue?");
			do	{
				fprintf(stderr, "[yn]\n");
				c = getchar();
				while (getchar() != '\n')
					/* void */;
			} while (c != 'y' && c != 'n');
			eflag++;
			if (c == 'n')
				done(1);
			i = NTREC*TP_BSIZE;
			blkclr(tbf, i);
		}
		if (i == 0) {
			bct = NTREC + 1;
			volno++;
loop:
			flsht();
			close(mt);
			fprintf(stderr, "Mount volume %d\n", volno);
			while (getchar() != '\n')
				;
			if ((mt = open(magtape, 0)) == -1) {
				fprintf(stderr, "Cannot open tape!\n");
				goto loop;
			}
			if (readhdr(&tmpbuf) == 0) {
				fprintf(stderr, "Not a dump tape.Try again\n");
				goto loop;
			}
			if (checkvol(&tmpbuf, volno) == 0) {
				fprintf(stderr, "Wrong tape. Try again\n");
				goto loop;
			}
			readtape(b);
			return;
		}
	}
	blkcpy(&tbf[(bct++*TP_BSIZE)], b, TP_BSIZE);
}

flsht()
{
	bct = NTREC+1;
}

blkcpy(from, to, size)
	char *from, *to;
	int size;
{
	asm("	movc3	12(ap),*4(ap),*8(ap)");
}

blkclr(buf, size)
	char *buf;
	int size;
{
	asm("movc5	$0,(r0),$0,8(ap),*4(ap)");
}

resetmt()
{
	struct mtop tcom;

	if(dumpnum > 1)
		tcom.mt_op = MTBSF;
	else
		tcom.mt_op = MTREW;
	tcom.mt_count = 1;
	flsht();
	if (ioctl(mt,MTIOCTOP,&tcom) == -1) {
		/* kludge for disk dumps */
		lseek(mt, (long)0, 0);
	}
	if (dumpnum > 1) {
		tcom.mt_op = MTFSF;
		tcom.mt_count = 1;
		ioctl(mt,MTIOCTOP,&tcom);
	}
}

checkvol(b, t)
	struct s_spcl *b;
	int t;
{
	if (b->c_volume == t)
		return(1);
	return(0);
}

readhdr(b)
	struct s_spcl *b;
{
	if (gethead(b) == 0)
		return(0);
	if (checktype(b, TS_TAPE) == 0)
		return(0);
	return(1);
}

/*
 * read the tape into buf, then return whether or
 * or not it is a header block.
 */
gethead(buf)
	struct s_spcl *buf;
{
	readtape((char *)buf);
	if (buf->c_magic != MAGIC || checksum((int *)buf) == 0)
		return(0);
	return(1);
}

/*
 * return whether or not the buffer contains a header block
 */
ishead(buf)
	struct s_spcl *buf;
{
	if (buf->c_magic != MAGIC || checksum((int *)buf) == 0)
		return(0);
	return(1);
}

checktype(b, t)
	struct s_spcl *b;
	int	t;
{
	return(b->c_type == t);
}

/*
 * read a bit mask from the tape into m.
 */
readbits(mapp)
	char **mapp;
{
	register int i;
	char	*m;

	i = spcl.c_count;

	if (*mapp == 0)
		*mapp = (char *)(calloc(i, (TP_BSIZE/(NBBY/BITS))));
	m = *mapp;
	while (i--) {
		readtape((char *) m);
		m += (TP_BSIZE/(NBBY/BITS));
	}
	while (gethead(&spcl) == 0)
		;
}

checksum(b)
	int *b;
{
	register int i, j;

	j = sizeof(union u_spcl) / sizeof(int);
	i = 0;
	do
		i += *b++;
	while (--j);
	if (i != CHECKSUM) {
		fprintf(stderr, "Checksum error %o, ino %u\n", i, ino);
		return(0);
	}
	return(1);
}

/*
 *	Check for access into each directory in the pathname of an extracted
 *	file and create such a directory if needed in preparation for moving 
 *	the file to its proper home.
 */
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
					fprintf(stderr, "restor: cannot find mkdir!\n");
					done(0);
				}
				while ((rp = wait(&i)) >= 0 && rp != pid)
					;
			}
			*cp = '/';
		}
	}
}

setdir(dev)
	char *dev;
{
	struct fstab *fsp;

	if (setfsent() == 0) {
		fprintf(stderr, "Can't open checklist file: %s\n", FSTAB);
		done(1);
	}
	while ((fsp = getfsent()) != 0) {
		if (strcmp(fsp->fs_spec, dev) == 0) {
			printf("%s mounted on %s\n", dev, fsp->fs_file);
			if (chdir(fsp->fs_file) >= 0)
				return;
			fprintf(stderr, "%s cannot chdir to %s\n",
			    fsp->fs_file);
			done(1);
		}
	}
	fprintf(stderr, "%s not mounted\n", dev);
	done(1);
}

/*
 * These variables are "local" to the following two functions.
 */
char dirbuf[DIRBLKSIZ];
long dirloc = 0;
long prev = 0;

/*
 * add a new directory entry to a file.
 */
putent(dp, wrtfunc)
	struct direct *dp;
	int (*wrtfunc)();
{
	if (dp->d_ino == 0)
		return;
	for (;;) {
		if (dp->d_reclen < DIRBLKSIZ - dirloc) {
			blkcpy(dp, dirbuf + dirloc, dp->d_reclen);
			prev = dirloc;
			dirloc += dp->d_reclen;
			return;
		}
		((struct direct *)(dirbuf + prev))->d_reclen = DIRBLKSIZ - prev;
		(*wrtfunc)(dirbuf, DIRBLKSIZ);
		dirloc = 0;
	}
}

/*
 * flush out a directory that is finished.
 */
flushent(wrtfunc)
	int (*wrtfunc)();
{
	((struct direct *)(dirbuf + prev))->d_reclen = DIRBLKSIZ - prev;
	(*wrtfunc)(dirbuf, dirloc);
	dirloc = 0;
}

dirwrite(buf, size)
	char *buf;
	int size;
{
	fwrite(buf, 1, size, df);
	seekpt = ftell(df);
}

dcvt(odp, ndp)
	register struct odirect *odp;
	register struct direct *ndp;
{
	struct inotab *itp;

	blkclr(ndp, sizeof *ndp);
	ndp->d_ino =  odp->d_ino;
	strncpy(ndp->d_name, odp->d_name, ODIRSIZ);
	ndp->d_namlen = strlen(ndp->d_name);
	ndp->d_reclen = DIRSIZ(ndp);
	/*
	 * this quickly calculates if this inode is a directory.
	 * Currently not maintained.
	 *
	for (itp = inotab[INOHASH(odp->d_ino)]; itp; itp = itp->t_next) {
		if (itp->t_ino != odp->d_ino)
			continue;
		ndp->d_fmt = IFDIR;
		break;
	}
	 */
}

/*
 * Open a directory.
 * Modified to allow any random file to be a legal directory.
 */
DIR *
opendir(name)
	char *name;
{
	register DIR *dirp;

	dirp = (DIR *)malloc(sizeof(DIR));
	dirp->dd_fd = open(name, 0);
	if (dirp->dd_fd == -1) {
		free(dirp);
		return NULL;
	}
	dirp->dd_loc = 0;
	return dirp;
}

/*
 * Seek to an entry in a directory.
 * Only values returned by ``telldir'' should be passed to seekdir.
 * Modified to have many directories based in one file.
 */
void
seekdir(dirp, loc, base)
	register DIR *dirp;
	long loc, base;
{
	if (loc == telldir(dirp))
		return;
	loc -= base;
	if (loc < 0)
		fprintf(stderr, "bad seek pointer to seekdir %d\n", loc);
	lseek(dirp->dd_fd, base + (loc & ~(DIRBLKSIZ - 1)), 0);
	dirp->dd_loc = loc & (DIRBLKSIZ - 1);
	if (dirp->dd_loc != 0)
		dirp->dd_size = read(dirp->dd_fd, dirp->dd_buf, DIRBLKSIZ);
}

/*
 * get next entry in a directory.
 */
struct direct *
readdir(dirp)
	register DIR *dirp;
{
	register struct direct *dp;

	for (;;) {
		if (dirp->dd_loc == 0) {
			dirp->dd_size = read(dirp->dd_fd, dirp->dd_buf, 
			    DIRBLKSIZ);
			if (dirp->dd_size <= 0)
				return NULL;
		}
		if (dirp->dd_loc >= dirp->dd_size) {
			dirp->dd_loc = 0;
			continue;
		}
		dp = (struct direct *)(dirp->dd_buf + dirp->dd_loc);
		if (dp->d_reclen <= 0 ||
		    dp->d_reclen > DIRBLKSIZ + 1 - dirp->dd_loc)
			return NULL;
		dirp->dd_loc += dp->d_reclen;
		if (dp->d_ino == 0)
			continue;
		return (dp);
	}
}

allocinotab(ino, seekpt)
	ino_t ino;
	daddr_t seekpt;
{
	register struct inotab	*itp;

	itp = (struct inotab *)calloc(1, sizeof(struct inotab));
	itp->t_next = inotab[INOHASH(ino)];
	inotab[INOHASH(ino)] = itp;
	itp->t_ino = ino;
	itp->t_seekpt = seekpt;
}

allocxtr(ino, name, flags)
	ino_t ino;
	char *name;
	char flags;
{
	register struct xtrlist	*xp, *pxp;

	xp = (struct xtrlist *)calloc(1, sizeof(struct xtrlist) + strlen(name));
	xp->x_next = xtrlist[INOHASH(ino)];
	xtrlist[INOHASH(ino)] = xp;
	xp->x_ino = ino;
	strcpy(xp->x_name, name);
	xtrcnt++;
	xp->x_flags = flags;
	for (pxp = xp->x_next; pxp; pxp = pxp->x_next)
		if (pxp->x_ino == ino && (pxp->x_flags & XLINKED) == 0) {
			xp->x_flags |= XLINKED;
			xp->x_linkedto = pxp;
			xtrcnt--;
			break;
		}
	if (xp->x_flags & XLINKED)
		fprintf(stdout, "%s: linked to %s\n", xp->x_name, pxp->x_name);
	else if (xp->x_flags & XISDIR)
		fprintf(stdout, "%s: directory inode %u\n", xp->x_name, ino);
	else
		fprintf(stdout, "%s: inode %u\n", xp->x_name, ino);
}

done(exitcode)
	int exitcode;
{
	unlink(dirfile);
	exit(exitcode);
}
