/* Copyright (c) 1981 Regents of the University of California */

char version[] = "@(#)main.c 1.6 %G%";

/*	Modified to include h option (recursively extract all files within
 *	a subtree) and m option (recreate the heirarchical structure of
 *	that subtree and move extracted files to their proper homes).
 *	8/29/80		by Mike Litzkow
 *
 *	Includes the s (skip files) option for use with multiple dumps on
 *	a single tape.
 */

/* static char *sccsid = "@(#)restor.c	4.3 (Berkeley) 6/3/81"; */

#define MAXINO	3000
#define BITS	8
#define NCACHE	3
#define SIZEINC 10

#ifndef STANDALONE
#include <stdio.h>
#include <signal.h>
#endif
#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/buf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/dumprestor.h"

#define	MWORD(m,i) (m[(unsigned)(i-1)/MLEN])
#define	MBIT(i)	(1<<((unsigned)(i-1)%MLEN))
#define	BIS(i,w)	(MWORD(w,i) |=  MBIT(i))
#define	BIC(i,w)	(MWORD(w,i) &= ~MBIT(i))
#define	BIT(i,w)	(MWORD(w,i) & MBIT(i))

ino_t	ino, maxi;

int	mt;

int	eflag = 0, hflag = 0, mflag = 0;

char	mounted = 0;
dev_t	dev = 0;
char	tapename[] = "/dev/rmt8";
char	*magtape = tapename;

#ifdef STANDALONE
char	mbuf[50];
#endif

daddr_t	seekpt;
FILE	*df;
int	ofile;
char	dirfile[] = "rstXXXXXX";

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

#include <sys/mtio.h>
struct mtop tcom;

int dumpnum = 1;
int	volno = 1;

struct inode *cur_ip;

short	dumpmap[MSIZ];
short	clrimap[MSIZ];
char	clearedbuf[BSIZE];

int curblk = 0;

int bct = NTREC+1;
char tbf[NTREC*TP_BSIZE];

extern char *ctime();
ino_t search();
char **envp;

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
#ifndef STANDALONE
	envp = arge;
	mktmp(dirfile);
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
		case 't':
		case 'x':
			command = *cp;
			break;
		default:
			fprintf(stderr, "Bad key character %c\n", *cp);
			goto usage;
		}
	}
	if (command == 'x') {
		df = fopen(dirfile, "w");
		if (df == 0) {
			fprintf(stderr, "restor: %s - cannot create directory temporary\n", dirfile);
			done(1);
		}
		xmount(envp);
		mounted++;
	}
	doit(command, argc, argv);
	done(0);
#else
	magtape = "tape";
	doit('r', 1, 0);
#endif
}

doit(command, argc, argv)
	char	command;
	int	argc;
	char	*argv[]; 
{
#ifndef STANDALONE
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
#else
	do {
		fprintf(stderr, "Tape? ");
		gets(mbuf);
		mt = open(mbuf, 0);
	} while (mt == -1);
	magtape = mbuf;
	clearbuf(clearedbuf);
#endif
	switch(command) {
#ifndef STANDALONE
	case 't':
		if (readhdr(&spcl) == 0) {
			fprintf(stderr, "Tape is not a dump tape\n");
			done(1);
		}
		fprintf(stdout, "Dump   date: %s", ctime(&spcl.c_date));
		fprintf(stdout, "Dumped from: %s", ctime(&spcl.c_ddate));
		return;
	case 'x':
		extractfiles(argc, argv);
		return;
	case 'r':
	case 'R':
		restorfiles(command, argv);
		return;
	}
#endif
}

#ifndef STANDALONE
extractfiles(argc, argv)
	int argc;
	char **argv;
{
	char	*ststore();
	register struct xtrlist *xp;
	struct xtrlist **xpp;
	ino_t	d;
	int	xtrfile(), skip(), null();
	int	mode;
	char	name[BUFSIZ + 1];

	if (readhdr(&spcl) == 0) {
		fprintf(stderr, "Tape is not a dump tape\n");
		done(1);
	}
	if (checkvol(&spcl, 1) == 0) {
		fprintf(stderr, "Tape is not volume 1 of the dump\n");
	}
	pass1();  /* This sets the various maps on the way by */
	while (argc--) {
		if ((d = psearch(*argv)) == 0 || BIT(d,dumpmap) == 0) {
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


	if(dumpnum > 1)
		tcom.mt_op = MTBSF;
	else	tcom.mt_op = MTREW;
	tcom.mt_count = 1;

newvol:
	flsht();
	ioctl(mt,MTIOCTOP,&tcom);
	if (dumpnum > 1) {
		tcom.mt_op = MTFSF;
		tcom.mt_count = 1;
		ioctl(mt,MTIOCTOP,&tcom);
	}
	lseek(mt, (long)0, 0);


getvol:
	fprintf(stderr, "Mount desired tape volume: Specify volume #: ");
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
	readbits(dumpmap);
	while (xtrcnt > 0) {
again:
		if (ishead(&spcl) == 0)
			while(gethead(&spcl) == 0)
				;
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
				if (xmknod(name, mode, spcl.c_dinode.di_rdev)) {
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
					xchown(xp->x_name, spcl.c_dinode.di_uid, spcl.c_dinode.di_gid);
					getfile(null, null, spcl.c_dinode.di_size);
					break;
				}
				/* else fall through */
			case IFREG:
				fprintf(stdout, "extract file %s\n", name);
				if ((ofile = xcreat(name, 0666)) < 0) {
					fprintf(stderr, "%s: cannot create file\n", name);
					xp->x_flags |= XTRACTD;
					xtrcnt--;
					goto skipfile;
				}
				xchown(name, spcl.c_dinode.di_uid, spcl.c_dinode.di_gid);
				getfile(xtrfile, skip, spcl.c_dinode.di_size);
				xclose(ofile);
				break;
			}
			xchmod(name, mode);
			xutime(name, xp->x_timep);
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
				xutime(xp->x_name, xp->x_timep);
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
			if (xlink(xp->x_linkedto->x_name, xp->x_name) < 0)
				fprintf(stderr, "link %s to %s failed\n",
					xp->x_linkedto->x_name, xp->x_name);
		}
	}
}
#endif

restorfiles(command, argv)
	char command;
	char **argv;
{
	int	rstrfile(), rstrskip();
	register struct dinode *dp;
	register struct inode *ip;
	struct fs *fs;
	int mode;
	char mount[BUFSIZ + 1];
	char *ptr[2];

#ifndef STANDALONE
	mount[0] = '\0';
	strcpy(mount, "MOUNT=");
	strncat(mount, *argv, BUFSIZ);
	ptr[0] = mount;
	ptr[1] = 0;
	xmount(ptr);
	iput(u.u_cdir); /* release root inode */
	iput(u.u_rdir); /* release root inode */
	mounted++;
#else
	do {
		fprintf(stderr, "Disk? ");
		gets(mount);
		fi = open(mount, 2);
	} while (fi == -1);
#endif
#ifndef STANDALONE
	if (command == 'R') {
		fprintf(stderr, "Enter starting volume number: ");
		if (gets(tbf) == EOF) {
			volno = 1;
			fprintf(stderr, "\n");
		}
		else
			volno = atoi(tbf);
	}
	else
#endif
		volno = 1;
	fprintf(stderr, "Last chance before scribbling on %s. ",
#ifdef STANDALONE
							"disk");
#else
							*argv);
#endif
	while (getchar() != '\n');
	fs = getfs(dev);
	maxi = fs->fs_ipg * fs->fs_ncg;
	if (readhdr(&spcl) == 0) {
		fprintf(stderr, "Missing volume record\n");
		done(1);
	}
	if (checkvol(&spcl, volno) == 0) {
		fprintf(stderr, "Tape is not volume %d\n", volno);
		done(1);
	}
	gethead(&spcl);
	for (;;) {
ragain:
		if (ishead(&spcl) == 0) {
			fprintf(stderr, "Missing header block\n");
			while (gethead(&spcl) == 0)
				;
			eflag++;
		}
		if (checktype(&spcl, TS_END) == 1) {
			fprintf(stderr, "End of tape\n");
			close(mt);
			return;
		}
		if (checktype(&spcl, TS_CLRI) == 1) {
			readbits(clrimap);
			for (ino = 1; ino <= maxi; ino++)
				if (BIT(ino, clrimap) == 0) {
					if (!iexist(dev, ino))
						continue;
					ip = iget(dev, ino);
					if (ip == NULL) {
						fprintf(stderr, "can't find inode %u\n", ino);
						done(1);
					}
					ip->i_nlink = 0;
					ip->i_flag |= ICHG;
					iput(ip);
				}
			goto ragain;
		}
		if (checktype(&spcl, TS_BITS) == 1) {
			readbits(dumpmap);
			goto ragain;
		}
		if (checktype(&spcl, TS_INODE) == 0) {
			fprintf(stderr, "Unknown header type\n");
			eflag++;
			gethead(&spcl);
			goto ragain;
		}
		ino = spcl.c_inumber;
		if (eflag)
			fprintf(stderr, "Resynced at inode %u\n", ino);
		eflag = 0;
		if (ino > maxi) {
			fprintf(stderr, "%u: ilist too small\n", ino);
			gethead(&spcl);
			goto ragain;
		}
		if (iexist(dev, ino)) {
			ip = iget(dev, ino);
			if (ip == NULL) {
				fprintf(stderr, "can't find inode %u\n",
					ino);
				done(1);
			}
			ip->i_nlink = 0;
			ip->i_flag |= ICHG;
			iput(ip);
		}
		dp = &spcl.c_dinode;
		ip = ialloc(dev, ino, dp->di_mode);
		if (ip == NULL || ip->i_number != ino) {
			fprintf(stderr, "can't create inode %u\n", ino);
			done(1);
		}
		ip->i_mode = mode = dp->di_mode;
		ip->i_nlink = dp->di_nlink;
		ip->i_uid = dp->di_uid;
		ip->i_gid = dp->di_gid;
		ip->i_atime = dp->di_atime;
		ip->i_mtime = dp->di_mtime;
		ip->i_ctime = dp->di_ctime;
		if ((ip->i_mode & IFMT) == IFCHR ||
		    (ip->i_mode & IFMT) == IFBLK)
			ip->i_rdev = dp->di_rdev;
		ip->i_size = 0;
		cur_ip = ip;
		u.u_offset = 0;
		u.u_segflg = 1;
		getfile(rstrfile, rstrskip, dp->di_size);
		ip->i_mode = mode;
		ip->i_flag &= ~(IUPD|IACC);
		ip->i_flag |= ICHG;
		iput(ip);
	}
}

/*
 * Read the tape, bulding up a directory structure for extraction
 * by name
 */
#ifndef STANDALONE
pass1()
{
	register int i;
	register struct dinode *ip;
	struct direct nulldir;
	int	putdir(), null();

	nulldir.d_ino = 0;
	strncpy(nulldir.d_name, "/", DIRSIZ);
	while (gethead(&spcl) == 0) {
		fprintf(stderr, "Can't find directory header!\n");
	}
	for (;;) {
		if (checktype(&spcl, TS_BITS) == 1) {
			readbits(dumpmap);
			continue;
		}
		if (checktype(&spcl, TS_CLRI) == 1) {
			readbits(clrimap);
			continue;
		}
		if (checktype(&spcl, TS_INODE) == 0) {
finish:
/*	
			close(mt);
*/
			freopen(dirfile, "r", df);
			return;
		}
		ip = &spcl.c_dinode;
		i = ip->di_mode & IFMT;
		if (i != IFDIR) {
			goto finish;
		}
		allocinotab(spcl.c_inumber, seekpt);
		getfile(putdir, null, spcl.c_dinode.di_size);
		putent(&nulldir);
	}
}
#endif

/*
 * Put the directory entries in the directory file
 */
#ifndef STANDALONE
putdir(buf, size)
	char *buf;
	int size;
{
	register struct direct *dp;
	register int i;

	for (dp = (struct direct *)buf, i = 0; i < size; dp++, i += sizeof(*dp))
		if (dp->d_ino != 0)
			putent(dp);
}

putent(dp)
	struct direct *dp;
{
	fwrite(dp, 1, sizeof(struct direct), df);
	seekpt = ftell(df);
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
	daddr_t	bpt;
	struct direct dir;
	char 	locname[BUFSIZ + 1];

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
		fseek(df, itp->t_seekpt, 0);
		fread(&dir, 1, sizeof(struct direct), df);
		fread(&dir, 1, sizeof(struct direct), df);
		fread(&dir, 1, sizeof(struct direct), df);
		bpt = ftell(df);
		/*
		 * "/" signals end of directory
		 */
		while (strncmp(dir.d_name, "/", DIRSIZ)) {
			locname[namelen] = '\0';
			strncat(locname, dir.d_name, DIRSIZ);
			if (strlen(locname) >= BUFSIZ) {
				fprintf(stderr, "%s: name exceedes %d char\n",
					locname, BUFSIZ);
				continue;
			}
			getleaves(dir.d_ino, locname);
			fseek(df, bpt, 0);
			fread(&dir, 1, sizeof(struct direct), df);
			bpt = ftell(df);
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
	struct direct dir;
	register struct inotab *itp;

	for (itp = inotab[INOHASH(inum)]; itp; itp = itp->t_next)
		if (itp->t_ino == inum)
			goto found;
	return(0);
found:
	fseek(df, itp->t_seekpt, 0);
	do {
		fread(&dir, 1, sizeof(struct direct), df);
		if (!strncmp(dir.d_name, "/", DIRSIZ))
			return(0);
	} while (strncmp(dir.d_name, cp, DIRSIZ));
	return(dir.d_ino);
}
#endif

/*
 * Do the file extraction, calling the supplied functions
 * with the blocks
 */
getfile(f1, f2, size)
	int	(*f2)(), (*f1)();
	long	size;
{
	register int i;
	char buf[BLKING * FRAG][TP_BSIZE];
	union u_spcl addrblk;
#	define addrblock addrblk.s_spcl

	addrblock = spcl;
	for (;;) {
		for (i = 0; i < addrblock.c_count; i++) {
			if (addrblock.c_addr[i]) {
				readtape(&buf[curblk++][0]);
				if (curblk == BLKING * FRAG) {
					(*f1)(buf, size > TP_BSIZE ?
					     (long) (BLKING * FRAG * TP_BSIZE) :
					     (curblk - 1) * TP_BSIZE + size);
					curblk = 0;
				}
			}
			else {
				if (curblk > 0) {
					(*f1)(buf, size > TP_BSIZE ?
					     (long) (curblk * TP_BSIZE) :
					     (curblk - 1) * TP_BSIZE + size);
					curblk = 0;
				}
				(*f2)(clearedbuf, size > TP_BSIZE ? (long) TP_BSIZE : size);
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
#ifndef STANDALONE
xtrfile(buf, size)
	char	*buf;
	long	size;
{
	if (xwrite(ofile, buf, (int) size) == -1) {
		perror("extract write:");
		done(1);
	}
}

skip(buf, size)
	char *buf;
	long size;
{
	if (xseek(ofile, size, 1) == -1) {
		perror("extract seek:");
		done(1);
	}
}
#endif


rstrfile(buf, size)
	char *buf;
	long size;
{
	u.u_base = buf;
	u.u_count = size;
	writei(cur_ip);
	if (u.u_error) {
		perror("restor write:");
		done(1);
	}
}

rstrskip(buf, size)
	char *buf;
	long size;
{
	u.u_offset += size;
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

	if (bct >= NTREC) {
		for (i = 0; i < NTREC; i++)
			((struct s_spcl *)&tbf[i*TP_BSIZE])->c_magic = 0;
		bct = 0;
		if ((i = read(mt, tbf, NTREC*TP_BSIZE)) < 0) {
			perror("Tape read error");
			eflag++;
			done(1);
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
	copy(&tbf[(bct++*TP_BSIZE)], b, TP_BSIZE);
}

flsht()
{
	bct = NTREC+1;
}

copy(f, t, s)
	register char *f, *t;
{
	register int i;

	i = s;
	do
		*t++ = *f++;
	while (--i);
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
 * tell whether an inode is allocated
 * this is drawn from ialloccg in sys/alloc.c
 */
iexist(dev, ino)
	dev_t dev;
	ino_t ino;
{
	register struct fs *fs;
	register struct cg *cgp;
	register struct buf *bp;
	int cg;

	fs = getfs(dev);
	if ((unsigned)ino >= fs->fs_ipg*fs->fs_ncg)
		return (0);
	cg = itog(ino, fs);
	bp = bread(dev, cgtod(cg, fs), BSIZE);
	if (bp->b_flags & B_ERROR)
		return(0);
	cgp = bp->b_un.b_cg;
	ino %= fs->fs_ipg;
	if (isclr(cgp->cg_iused, ino)) {
		brelse(bp);
		return(0);
	}
	brelse(bp);
	return (1);
}

/*
 * read a bit mask from the tape into m.
 */
readbits(m)
	short	*m;
{
	register int i;

	i = spcl.c_count;

	while (i--) {
		readtape((char *) m);
		m += (TP_BSIZE/(MLEN/BITS));
	}
	while (gethead(&spcl) == 0)
		;
}

done(exitcode)
	int exitcode;
{
#ifndef STANDALONE
	unlink(dirfile);
#endif
	if (mounted)
		xumount();
	exit(exitcode);
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
			if (xaccess(name, 01) < 0) {
				register int pid, rp;

				xumount();
				if ((pid = fork()) == 0) {
					execl("/bin/xmkdir", "xmkdir", name, 0);
					execl("/usr/bin/xmkdir", "xmkdir", name, 0);
					execl("./xmkdir", "xmkdir", name, 0);
					fprintf(stderr, "xrestor: cannot find xmkdir!\n");
					done(0);
				}
				while ((rp = wait(&i)) >= 0 && rp != pid)
					;
				xmount(envp);
			}
			*cp = '/';
		}
	}
}
