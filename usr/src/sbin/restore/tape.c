/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)tape.c	3.1	(Berkeley)	83/02/18";
#endif

#include "restore.h"
#include <dumprestor.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <setjmp.h>
#include <stat.h>
#include <file.h>

long	fssize;
int	mt = -1;
char	*magtape;
int	insetup = 0;
int	bct = NTREC+1;
char	tbf[NTREC*TP_BSIZE];
jmp_buf	restart;
int	gettingfile = 0;	/* restart has a valid frame */

int	ofile;
char	*map;
char	lnkbuf[MAXPATHLEN + 1];
int	pathlen;

/*
 * Set up an input source
 */
setinput(source)
	char *source;
{
#ifdef RRESTOR
	char *host;
	char *index();

	host = source;
	magtape = index(host, ':');
	if (magtape == 0) {
nohost:
		msg("need keyletter ``f'' and device ``host:tape''");
		done(1);
	}
	*magtape++ = '\0';
	if (rmthost(host) == 0)
		done(1);
	setuid(getuid());	/* no longer need or want root privileges */
#else
	magtape = source;
#endif RRESTOR
}

/*
 * Verify that the tape drive can be accessed and
 * that it actually is a dump tape.
 */
setup()
{
	struct mtop tcom;
	struct stat stbuf;
	extern char *ctime();
	extern int xtrmap(), xtrmapskip();

	vprintf(stdout, "Verify tape and initialize maps\n");
	insetup = 1;
#ifdef RRESTOR
	if ((mt = rmtopen(magtape, 0)) < 0)
#else
	if ((mt = open(magtape, 0)) < 0)
#endif
	{
		fprintf(stderr, "%s: cannot open tape\n", magtape);
		done(1);
	}
	if (dumpnum != 1) {
		tcom.mt_op = MTFSF;
		tcom.mt_count = dumpnum -1;
#ifdef RRESTOR
		rmtioctl(MTFSF,dumpnum - 1);
#else
		if (ioctl(mt,MTIOCTOP,&tcom) < 0)
			perror("ioctl MTFSF");
#endif
	}
	flsht();
	if (readhdr(&spcl) == 0) {
		bct--; /* push back this block */
		cvtflag++;
		if (readhdr(&spcl) == 0) {
			fprintf(stderr, "Tape is not a dump tape\n");
			done(1);
		}
		fprintf(stderr, "Converting to new file system format.\n");
	}
	vprintf(stdout, "Dump   date: %s", ctime(&spcl.c_date));
	vprintf(stdout, "Dumped from: %s", ctime(&spcl.c_ddate));
	dumptime = spcl.c_ddate;
	if (stat(".", &stbuf) < 0) {
		fprintf(stderr, "cannot stat .\n");
		done(1);
	}
	fssize = stbuf.st_blksize;
	if (fssize <= 0 || ((fssize - 1) & fssize) != 0) {
		fprintf(stderr, "bad block size %d\n", fssize);
		done(1);
	}
	if (checkvol(&spcl, (long)1) == 0) {
		fprintf(stderr, "Tape is not volume 1 of the dump\n");
		done(1);
	}
	if (readhdr(&spcl) == 0 || checktype(&spcl, TS_CLRI) != 1) {
		fprintf(stderr, "Cannot find file removal list\n");
		done(1);
	}
	maxino = spcl.c_count * TP_BSIZE + 1;
	map = (char *)calloc(1, (int)howmany(maxino, NBBY));
	if (map == (char *)NIL)
		panic("no memory for file removal list\n");
	curfile.action = USING;
	getfile(xtrmap, xtrmapskip);
	clrimap = map;
	if (checktype(&spcl, TS_BITS) != 1) {
		fprintf(stderr, "Cannot find file dump list\n");
		done(1);
	}
	map = (char *)calloc(1, (int)howmany(maxino, NBBY));
	if (map == (char *)NULL)
		panic("no memory for file dump list\n");
	curfile.action = USING;
	getfile(xtrmap, xtrmapskip);
	dumpmap = map;
	insetup = 0;
}

getvol(nextvol)
	long nextvol;
{
	long newvol;
	union u_spcl tmpspcl;
#	define tmpbuf tmpspcl.s_spcl

	if (dumpnum > 1) {
		/*
		 * if this is a multi-dump tape we always start with 
		 * volume 1, so as to avoid accidentally restoring
		 * from a different dump!
		 */
		resetmt();
		dumpnum = 1;
		volno = 1;
		goto rbits;
	}
again:
	if (command == 'R' || command == 'r' || curfile.action != SKIP)
		newvol = nextvol;
	else 
		newvol = 0;
	while (newvol <= 0) {
		fprintf(stderr, "Specify volume #: ");
		if (gets(tbf) == NULL)
			return;
		newvol = atoi(tbf);
		if (newvol <= 0) {
			fprintf(stderr,
			    "Volume numbers are positive numerics\n");
		}
	}
	if (newvol == volno)
		return;
	closemt();
	fprintf(stderr, "Mount tape volume %d then type return: ", newvol);
	while (getchar() != '\n')
		;
#ifdef RRESTOR
	if ((mt = rmtopen(magtape, 0)) == -1)
#else
	if ((mt = open(magtape, 0)) == -1)
#endif
	{
		fprintf(stderr, "Cannot open tape!\n");
		goto again;
	}
	volno = newvol;
	flsht();
	if (readhdr(&tmpbuf) == 0) {
		fprintf(stderr, "tape is not dump tape\n");
		volno = 0;
		goto again;
	}
	if (checkvol(&tmpbuf, volno) == 0) {
		fprintf(stderr, "Wrong volume (%d)\n", tmpbuf.c_volume);
		volno = 0;
		goto again;
	}
rbits:
	if (curfile.action == USING) {
		if (volno == 1)
			panic("active file into volume 1\n");
		return;
	}
	findinode(&spcl, curfile.action == UNKNOWN ? 1 : 0);
	if (gettingfile) {
		gettingfile = 0;
		longjmp(restart, 1);
	}
}

extractfile(name)
	char *name;
{
	int mode;
	time_t timep[2];
	struct entry *ep;
	extern int xtrlnkfile(), xtrlnkskip();
	extern int xtrfile(), xtrskip();

	curfile.name = name;
	curfile.action = USING;
	timep[0] = curfile.dip->di_atime;
	timep[1] = curfile.dip->di_mtime;
	mode = curfile.dip->di_mode;
	switch (mode & IFMT) {

	default:
		fprintf(stderr, "%s: unknown file mode 0%o\n", name, mode);
		skipfile();
		return (FAIL);

	case IFDIR:
		if (mflag) {
			ep = lookupname(name);
			if (ep == NIL || ep->e_flags & EXTRACT)
				panic("unextracted directory %s\n", name);
			skipfile();
			return (GOOD);
		}
		vprintf(stdout, "extract file %s\n", name);
		return (genliteraldir(name, curfile.ino));

	case IFLNK:
		lnkbuf[0] = '\0';
		pathlen = 0;
		getfile(xtrlnkfile, xtrlnkskip);
		if (pathlen == 0) {
			vprintf(stdout,
			    "%s: zero length symbolic link (ignored)\n", name);
		} else if (symlink(lnkbuf, name) < 0) {
			fprintf(stderr, "%s: cannot create symbolic link\n",
			    name);
			return (FAIL);
		} else
			vprintf(stdout, "extract symbolic link %s\n", name);
		return (GOOD);

	case IFCHR:
	case IFBLK:
		vprintf(stdout, "extract special file %s\n", name);
		if (mknod(name, mode, (int)curfile.dip->di_rdev) < 0) {
			fprintf(stderr, "%s: cannot create special file\n",
			    name);
			skipfile();
			return (FAIL);
		}
		chown(name, curfile.dip->di_uid, curfile.dip->di_gid);
		chmod(name, mode);
		skipfile();
		utime(name, timep);
		return (GOOD);

	case IFREG:
		vprintf(stdout, "extract file %s\n", name);
		if ((ofile = open(name, FWRONLY|FCREATE, 0666)) < 0) {
			fprintf(stderr, "%s: cannot create file\n", name);
			skipfile();
			return (FAIL);
		}
		fchown(ofile, curfile.dip->di_uid, curfile.dip->di_gid);
		fchmod(ofile, mode);
		getfile(xtrfile, xtrskip);
		close(ofile);
		utime(name, timep);
		return (GOOD);
	}
	/* NOTREACHED */
}

skipfile()
{
	extern int null();

	curfile.action = SKIP;
	getfile(null, null);
}

/*
 * Do the file extraction, calling the supplied functions
 * with the blocks
 */
getfile(f1, f2)
	int	(*f2)(), (*f1)();
{
	register int i;
	int curblk = 0;
	off_t size = spcl.c_dinode.di_size;
	static char clearedbuf[MAXBSIZE];
	char buf[MAXBSIZE / TP_BSIZE][TP_BSIZE];

	if (checktype(&spcl, TS_END) == 1)
		panic("ran off end of tape\n");
	if (checktype(&spcl, TS_INODE) == 0)
		panic("not at beginning of a file\n");
	if (setjmp(restart) != 0)
		return;
	gettingfile++;
loop:
	for (i = 0; i < spcl.c_count; i++) {
		if (spcl.c_addr[i]) {
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
			gethead(&spcl);
			goto out;
		}
	}
	if (gethead(&spcl) == 0 || checktype(&spcl, TS_ADDR) == 0) {
		fprintf(stderr, "Missing address (header) block for %s\n",
			curfile.name);
		goto out;
	}
	goto loop;
out:
	if (curblk > 0) {
		(*f1)(buf, (curblk * TP_BSIZE) + size);
	}
	findinode(&spcl, 1);
	gettingfile = 0;
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
		fprintf(stderr, "write error extracting inode %d, name %s\n",
			curfile.ino, curfile.name);
		perror("write");
		done(1);
	}
}

xtrskip(buf, size)
	char *buf;
	long size;
{

#ifdef lint
	buf = buf;
#endif
	if (lseek(ofile, size, 1) == (long)-1) {
		fprintf(stderr, "seek error extracting inode %d, name %s\n",
			curfile.ino, curfile.name);
		perror("lseek");
		done(1);
	}
}

xtrlnkfile(buf, size)
	char	*buf;
	long	size;
{

	pathlen += size;
	if (pathlen > MAXPATHLEN) {
		fprintf(stderr, "symbolic link name: %s->%s%s; too long %d\n",
		    curfile.name, lnkbuf, buf, pathlen);
		done(1);
	}
	strcat(lnkbuf, buf);
}

xtrlnkskip(buf, size)
	char *buf;
	long size;
{

#ifdef lint
	buf = buf, size = size;
#endif
	fprintf(stderr, "unallocated block in symbolic link %s\n",
		curfile.name);
	done(1);
}

xtrmap(buf, size)
	char	*buf;
	long	size;
{

	bcopy(buf, map, size);
}

xtrmapskip(buf, size)
	char *buf;
	long size;
{

#ifdef lint
	buf = buf;
	size = size;
#endif
	panic("hole in map\n");
}

null() {;}

/*
 * Do the tape i/o, dealing with volume changes
 * etc..
 */
readtape(b)
	char *b;
{
	register long i;
	long newvol;

	if (bct >= NTREC) {
		for (i = 0; i < NTREC; i++)
			((struct s_spcl *)&tbf[i*TP_BSIZE])->c_magic = 0;
		bct = 0;
#ifdef RRESTOR
		if ((i = rmtread(tbf, NTREC*TP_BSIZE)) < 0)
#else
		if ((i = read(mt, tbf, NTREC*TP_BSIZE)) < 0)
#endif
			{
			fprintf(stderr, "Tape read error while ");
			switch (curfile.action) {
			case UNKNOWN:
				fprintf(stderr, "trying to resyncronize\n");
				break;
			case USING:
				fprintf(stderr, "restoring %s\n", curfile.name);
				break;
			case SKIP:
				fprintf(stderr, "skipping over inode %d\n",
					curfile.ino);
				break;
			}
			if (!yflag && !reply("continue"))
				done(1);
			i = NTREC*TP_BSIZE;
			bzero(tbf, i);
#ifdef RRESTOR
			if (rmtseek(i, 1) < 0)
#else
			if (lseek(mt, i, 1) == (long)-1)
#endif
			{
				fprintf(stderr, "continuation failed\n");
				done(1);
			}
		}
		if (i == 0) {
			newvol = volno + 1;
			volno = 0;
			getvol(newvol);
			readtape(b);
			return;
		}
	}
	bcopy(&tbf[(bct++*TP_BSIZE)], b, (long)TP_BSIZE);
}

flsht()
{

	bct = NTREC+1;
}

resetmt()
{
	struct mtop tcom;

	if (dumpnum > 1)
		tcom.mt_op = MTBSF;
	else
		tcom.mt_op = MTREW;
	tcom.mt_count = 1;
	flsht();
#ifdef RRESTOR
	if (rmtioctl(tcom.mt_op, 1) == -1) {
		/* kludge for disk dumps */
		rmtseek((long)0, 0);
	}
#else
	if (ioctl(mt,MTIOCTOP,&tcom) == -1) {
		/* kludge for disk dumps */
		(void) lseek(mt, (long)0, 0);
	}
#endif
	if (dumpnum > 1) {
#ifdef RRESTOR
		rmtioctl(MTFSF, 1);
#else
		tcom.mt_op = MTFSF;
		tcom.mt_count = 1;
		ioctl(mt,MTIOCTOP,&tcom);
#endif
	}
}

closemt()
{
	if (mt < 0)
		return;
#ifdef RRESTOR
	rmtclose();
#else
	close(mt);
#endif
}

checkvol(b, t)
	struct s_spcl *b;
	long t;
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
	union u_ospcl {
		char dummy[TP_BSIZE];
		struct	s_ospcl {
			int	c_type;
			time_t	c_date;
			time_t	c_ddate;
			int	c_volume;
			daddr_t	c_tapea;
			ino_t	c_inumber;
			int	c_magic;
			int	c_checksum;
			struct odinode {
				unsigned short odi_mode;
				short	odi_nlink;
				short	odi_uid;
				short	odi_gid;
				off_t	odi_size;
				daddr_t	odi_rdev;
				char	odi_addr[36];
				time_t	odi_atime;
				time_t	odi_mtime;
				time_t	odi_ctime;
			} c_dinode;
			int	c_count;
			char	c_addr[TP_NINDIR];
		} s_ospcl;
	} u_ospcl;

	if (!cvtflag) {
		readtape((char *)buf);
		if (buf->c_magic != NFS_MAGIC || checksum((int *)buf) == 0)
			return(0);
		return(1);
	}
	readtape((char *)(&u_ospcl.s_ospcl));
	bzero((char *)buf, (long)TP_BSIZE);
	buf->c_type = u_ospcl.s_ospcl.c_type;
	buf->c_date = u_ospcl.s_ospcl.c_date;
	buf->c_ddate = u_ospcl.s_ospcl.c_ddate;
	buf->c_volume = u_ospcl.s_ospcl.c_volume;
	buf->c_tapea = u_ospcl.s_ospcl.c_tapea;
	buf->c_inumber = u_ospcl.s_ospcl.c_inumber;
	buf->c_checksum = u_ospcl.s_ospcl.c_checksum;
	buf->c_magic = u_ospcl.s_ospcl.c_magic;
	buf->c_dinode.di_mode = u_ospcl.s_ospcl.c_dinode.odi_mode;
	buf->c_dinode.di_nlink = u_ospcl.s_ospcl.c_dinode.odi_nlink;
	buf->c_dinode.di_uid = u_ospcl.s_ospcl.c_dinode.odi_uid;
	buf->c_dinode.di_gid = u_ospcl.s_ospcl.c_dinode.odi_gid;
	buf->c_dinode.di_size = u_ospcl.s_ospcl.c_dinode.odi_size;
	buf->c_dinode.di_rdev = u_ospcl.s_ospcl.c_dinode.odi_rdev;
	buf->c_dinode.di_atime = u_ospcl.s_ospcl.c_dinode.odi_atime;
	buf->c_dinode.di_mtime = u_ospcl.s_ospcl.c_dinode.odi_mtime;
	buf->c_dinode.di_ctime = u_ospcl.s_ospcl.c_dinode.odi_ctime;
	buf->c_count = u_ospcl.s_ospcl.c_count;
	bcopy(u_ospcl.s_ospcl.c_addr, buf->c_addr, (long)TP_NINDIR);
	if (u_ospcl.s_ospcl.c_magic != OFS_MAGIC ||
	    checksum((int *)(&u_ospcl.s_ospcl)) == 0)
		return(0);
	buf->c_magic = NFS_MAGIC;
	return(1);
}

/*
 * Find an inode header.
 * Complain if had to skip, and complain is set.
 */
findinode(header, complain)
	struct s_spcl *header;
	int complain;
{
	static int skipcnt = 0;

	curfile.name = "<name unknown>";
	curfile.action = UNKNOWN;
	curfile.dip = (struct dinode *)NIL;
	curfile.ino = 0;
	if (ishead(header) == 0)
		while (gethead(header) == 0)
			skipcnt++;
	for (;;) {
		if (checktype(header, TS_INODE) == 1) {
			curfile.dip = &header->c_dinode;
			curfile.ino = header->c_inumber;
			break;
		}
		if (checktype(header, TS_END) == 1) {
			curfile.ino = maxino;
			break;
		}
		if (insetup && checktype(header, TS_CLRI) == 1) {
			curfile.name = "<file removal list>";
			header->c_dinode.di_size = header->c_count * TP_BSIZE;
			break;
		}
		if (insetup && checktype(header, TS_BITS) == 1) {
			curfile.name = "<file dump list>";
			header->c_dinode.di_size = header->c_count * TP_BSIZE;
			break;
		}
		while (gethead(header) == 0)
			skipcnt++;
	}
	if (skipcnt > 0 && complain)
		fprintf(stderr, "resync restor, skipped %d blocks\n", skipcnt);
	skipcnt = 0;
}

/*
 * return whether or not the buffer contains a header block
 */
ishead(buf)
	struct s_spcl *buf;
{

	if (buf->c_magic != NFS_MAGIC)
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
	register int *b;
{
	register int i, j;

	j = sizeof(union u_spcl) / sizeof(int);
	i = 0;
	do
		i += *b++;
	while (--j);
	if (i != CHECKSUM) {
		fprintf(stderr, "Checksum error %o, inode %d file %s\n", i,
			curfile.ino, curfile.name);
		return(0);
	}
	return(1);
}

#ifdef RRESTOR
msg(cp, a1, a2, a3)
	char *cp;
{

	fprintf(stderr, cp, a1, a2, a3);
}
#endif RRESTOR
