/* Copyright (c) 1981 Regents of the University of California */

char version[] = "@(#)main.c 1.4 %G%";

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
#define NSIZE   100
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

struct  direct	dir;

int	fi;
ino_t	ino, maxi;

int	mt, i;
int	eflag, hflag, mflag;

char	mounted = 0;
dev_t	dev = 0;
char	tapename[] = "/dev/rmt8";
char	*magtape = tapename;

#ifdef STANDALONE
char	mbuf[50];
#endif

#ifndef STANDALONE
daddr_t	seekpt;
int	df, ofile;
char	dirfile[] = "rstXXXXXX";

struct {
	ino_t	t_ino;
	daddr_t	t_seekpt;
} inotab[MAXINO];
int	ipos;

#define ONTAPE	1
#define XTRACTD	2
#define XINUSE	4
struct xtrlist {
	ino_t	x_ino;
	char	x_flags;
	char 	*x_name;
} *xtrlist;

char	name[100];

char	drblock[BSIZE];
int	bpt, nread, xsize,
	init = 1;

#include <sys/mtio.h>
struct mtop tcom;

int dumpnum = 1;
int	volno = 1;

struct inode *cur_ip;

short	dumpmap[MSIZ];
short	clrimap[MSIZ];
char	clearedbuf[BSIZE];

int bct = NTREC+1;
char tbf[NTREC*BSIZE];

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

#ifndef STANDALONE
	envp = arge;
	mktmp(dirfile);
	if (argc < 2) {
usage:
		fprintf(stderr, "Usage: restor x[s|m|h] file file..., restor r filesys, or restor t\n");
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
		if (signal(SIGINT, done) == SIG_IGN)
			signal(SIGINT, SIG_IGN);
		if (signal(SIGTERM, done) == SIG_IGN)
			signal(SIGTERM, SIG_IGN);

		df = creat(dirfile, 0666);
		if (df < 0) {
			fprintf(stderr, "restor: %s - cannot create directory temporary\n", dirfile);
			done(1);
		}
		close(df);
		xmount(envp);
		mounted++;
		df = open(dirfile, 2);
	}
	doit(command, argc, argv);
	if (command == 'x')
		unlink(dirfile);
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
	extern char *ctime();
	char *ststore();
	register k;
	ino_t	d;
#ifndef STANDALONE
	int	xtrfile(), skip(), null();
#endif
	int	rstrfile(), rstrskip();
	register struct dinode *dp;
	register struct inode *ip;
	struct fs *fs;

#ifndef STANDALONE
	getxtrlist();
	if ((mt = open(magtape, 0)) < 0) {
		fprintf(stderr, "%s: cannot open tape\n", magtape);
		done(1);
	}
	if(dumpnum != 1) {
		tcom.mt_op = MTFSF;
		tcom.mt_count = dumpnum -1;
		if( ioctl(mt,MTIOCTOP,&tcom) < 0)
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
		fprintf(stderr, "Dump   date: %s", ctime(&spcl.c_date));
		fprintf(stderr, "Dumped from: %s", ctime(&spcl.c_ddate));
		return;
	case 'x':
		if (readhdr(&spcl) == 0) {
			fprintf(stderr, "Tape is not a dump tape\n");
			done(1);
		}
		if (checkvol(&spcl, 1) == 0) {
			fprintf(stderr, "Tape is not volume 1 of the dump\n");
		}
		pass1();  /* This sets the various maps on the way by */
		i = 0;
		while( argc-- ) {
			if(hflag)
				getleaves( *argv++ );
			else {
				if( (d = psearch(*argv)) == 0 ||
							BIT(d,dumpmap) == 0 ) {
					fprintf(stderr,  "%s: not on tape\n", *argv++ );
					continue;
				}
				xtrlist[i].x_ino = d;
				xtrlist[i].x_flags |= XINUSE;
				if( mflag )
					xtrlist[i].x_name = ststore( *argv );
				fprintf(stderr,  "%s: inode %u\n", *argv, d );
				argv++;
				if( ++i >= xsize ) getxtrlist();
			}
		}


		if(dumpnum > 1)
			tcom.mt_op = MTBSF;
		else	tcom.mt_op = MTREW;
		tcom.mt_count = 1;

newvol:
		flsht();
		ioctl(mt,MTIOCTOP,&tcom);
		if( dumpnum > 1 ) {
			tcom.mt_op = MTFSF;
			tcom.mt_count = 1;
			ioctl(mt,MTIOCTOP,&tcom);
		}
		lseek(mt, 0, 0);


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
		i = 0;
		for (k = 0; xtrlist[k].x_flags; k++) {
			if (BIT(xtrlist[k].x_ino, dumpmap)) {
				xtrlist[k].x_flags |= ONTAPE;
				i++;
			}
		}
		while (i > 0) {
again:
			if (ishead(&spcl) == 0)
				while(gethead(&spcl) == 0)
					;
			if (checktype(&spcl, TS_END) == 1) {
				fprintf(stderr, "end of tape\n");
checkdone:
				for (k = 0; xtrlist[k].x_flags; k++)
					if ((xtrlist[k].x_flags&XTRACTD) == 0) {
						/* get next tape */
						tcom.mt_op = MTREW;

						goto newvol;
					}
				return;
			}
			if (checktype(&spcl, TS_INODE) == 0) {
				gethead(&spcl);
				goto again;
			}
			d = spcl.c_inumber;
			for (k = 0; xtrlist[k].x_flags; k++) {
				if (d == xtrlist[k].x_ino) {
					if( mflag ) {
						sprintf(name, "%s", xtrlist[k].x_name);
						fprintf(stderr, "extract file %s\n",name);
						checkdir( name );
					}
					else {
						fprintf(stderr, "extract file %u\n", xtrlist[k].x_ino );
						sprintf(name, "%u", xtrlist[k].x_ino);
					}
					if ((ofile = xcreat(name, 0666)) < 0) {
						fprintf(stderr, "%s: cannot create file\n", name);
						i--;
						continue;
					}
					xchown(name, spcl.c_dinode.di_uid, spcl.c_dinode.di_gid);
					getfile(xtrfile, skip, spcl.c_dinode.di_size);
					i--;
					xtrlist[k].x_flags |= XTRACTD;
					xclose(ofile);
					goto finished;
				}
			}
			getfile(null, null, spcl.c_dinode.di_size);
finished:
			;
		}
		goto checkdone;
#endif
	case 'r':
	case 'R':
#ifndef STANDALONE
		{
			char mount[80];
			char *ptr[2];

			strcpy(mount, "MOUNT=");
			strcat(mount, *argv);
			ptr[0] = mount;
			ptr[1] = 0;
			xmount(ptr);
			iput(u.u_cdir); /* release root inode */
			iput(u.u_rdir); /* release root inode */
			mounted++;
		}
#else
		do {
			char charbuf[50];

			fprintf(stderr, "Disk? ");
			gets(charbuf);
			fi = open(charbuf, 2);
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
			ip->i_mode = dp->di_mode;
			ip->i_nlink = dp->di_nlink;
			ip->i_uid = dp->di_uid;
			ip->i_gid = dp->di_gid;
			ip->i_size = dp->di_size;
			ip->i_atime = dp->di_atime;
			ip->i_mtime = dp->di_mtime;
			ip->i_ctime = dp->di_ctime;
			cur_ip = ip;
			u.u_offset = 0;
			u.u_segflg = 1;
			getfile(rstrfile, rstrskip, dp->di_size);
			ip->i_flag |= ICHG;
			iput(ip);
		}
	}
}

/*
 * Read the tape, bulding up a directory structure for extraction
 * by name
 */
#ifndef STANDALONE
pass1()
{
	register i;
	struct dinode *ip;
	int	putdir(), null();

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
			flsh();
/*	
			close(mt);
*/
			return;
		}
		ip = &spcl.c_dinode;
		i = ip->di_mode & IFMT;
		if (i != IFDIR) {
			goto finish;
		}
		inotab[ipos].t_ino = spcl.c_inumber;
		inotab[ipos++].t_seekpt = seekpt;
		getfile(putdir, null, spcl.c_dinode.di_size);
		putent("\000\000/");
	}
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
	register i;
	struct spcl addrblock;
	char buf[BSIZE];

	addrblock = spcl;
	for (;;) {
		for (i = 0; i < addrblock.c_count; i++) {
			if (addrblock.c_addr[i]) {
				readtape(buf);
				(*f1)(buf, size > BSIZE ? (long) BSIZE : size);
			}
			else {
				(*f2)(clearedbuf, size > BSIZE ? (long) BSIZE : size);
			}
			if ((size -= BSIZE) <= 0) {
eloop:
				while (gethead(&spcl) == 0)
					;
				if (checktype(&spcl, TS_ADDR) == 1)
					goto eloop;
				return;
			}
		}
		if (gethead(&addrblock) == 0) {
			fprintf(stderr, "Missing address (header) block\n");
			goto eloop;
		}
		if (checktype(&addrblock, TS_ADDR) == 0) {
			spcl = addrblock;
			return;
		}
	}
}

/*
 * Do the tape i\/o, dealling with volume changes
 * etc..
 */
readtape(b)
	char *b;
{
	register i;
	struct spcl tmpbuf;

	if (bct >= NTREC) {
		for (i = 0; i < NTREC; i++)
			((struct spcl *)&tbf[i*BSIZE])->c_magic = 0;
		bct = 0;
		if ((i = read(mt, tbf, NTREC*BSIZE)) < 0) {
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
	copy(&tbf[(bct++*BSIZE)], b, BSIZE);
}

flsht()
{
	bct = NTREC+1;
}

copy(f, t, s)
	register char *f, *t;
{
	register i;

	i = s;
	do
		*t++ = *f++;
	while (--i);
}

clearbuf(cp)
	register char *cp;
{
	register i;

	i = BSIZE;
	do
		*cp++ = 0;
	while (--i);
}

/*
 * Put and get the directory entries from the compressed
 * directory file
 */
#ifndef STANDALONE
putent(cp)
	char	*cp;
{
	register i;

	for (i = 0; i < sizeof(ino_t); i++)
		writec(*cp++);
	for (i = 0; i < DIRSIZ; i++) {
		writec(*cp);
		if (*cp++ == 0)
			return;
	}
	return;
}

getent(bf)
	register char *bf;
{
	register i;

	for (i = 0; i < sizeof(ino_t); i++)
		*bf++ = readc();
	for (i = 0; i < DIRSIZ; i++)
		if ((*bf++ = readc()) == 0)
			return;
	return;
}

/*
 * read/write te directory file
 */
writec(c)
	char c;
{
	drblock[bpt++] = c;
	seekpt++;
	if (bpt >= BSIZE) {
		bpt = 0;
		write(df, drblock, BSIZE);
	}
}

readc()
{
	if (bpt >= BSIZE) {
		nread = read(df, drblock, BSIZE);
		bpt = 0;
	}
	return(drblock[bpt++]);
}

mseek(pt)
	daddr_t pt;
{
	bpt = BSIZE;
	lseek(df, pt, 0);
}

flsh()
{
	write(df, drblock, bpt+1);
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
	register i;

	for (i = 0; i < MAXINO; i++)
		if (inotab[i].t_ino == inum) {
			goto found;
		}
	return(0);
found:
	mseek(inotab[i].t_seekpt);
	do {
		getent((char *)&dir);
		if (direq(dir.d_name, "/"))
			return(0);
	} while (direq(dir.d_name, cp) == 0);
	return(dir.d_ino);
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

direq(s1, s2)
	register char *s1, *s2;
{
	register i;

	for (i = 0; i < DIRSIZ; i++)
		if (*s1++ == *s2) {
			if (*s2++ == 0)
				return(1);
		} else
			return(0);
	return(1);
}
#endif

/*
 * read the tape into buf, then return whether or
 * or not it is a header block.
 */
gethead(buf)
	struct spcl *buf;
{
	readtape((char *)buf);
	if (buf->c_magic != MAGIC || checksum((int *) buf) == 0)
		return(0);
	return(1);
}

/*
 * return whether or not the buffer contains a header block
 */
ishead(buf)
	struct spcl *buf;
{
	if (buf->c_magic != MAGIC || checksum((int *) buf) == 0)
		return(0);
	return(1);
}

checktype(b, t)
	struct	spcl *b;
	int	t;
{
	return(b->c_type == t);
}


checksum(b)
	int *b;
{
	register i, j;

	j = BSIZE/sizeof(int);
	i = 0;
	do
		i += *b++;
	while (--j);
	if (i != CHECKSUM) {
		fprintf(stderr, "Checksum error %o\n", i);
		return(0);
	}
	return(1);
}

checkvol(b, t)
	struct spcl *b;
	int t;
{
	if (b->c_volume == t)
		return(1);
	return(0);
}

readhdr(b)
	struct	spcl *b;
{
	if (gethead(b) == 0)
		return(0);
	if (checktype(b, TS_TAPE) == 0)
		return(0);
	return(1);
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
	xwrite(ofile, buf, (int) size);
}

null() {;}

skip(buf, size)
	char *buf;
	long size;
{
	xseek(ofile, size, 1);
}
#endif


rstrfile(buf, size)
	char *buf;
	long size;
{
	u.u_base = buf;
	u.u_count = size;
	writei(cur_ip);
}

rstrskip(buf, size)
	char *buf;
	long size;
{
	u.u_offset += size;
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

#ifndef STANDALONE
putdir(buf, size)
	char *buf;
	int size;
{
	register struct direct *dp;
	register i;

	for (dp = (struct direct *) buf, i = 0; i < size; dp++, i += sizeof(*dp)) {
		if (dp->d_ino == 0)
			continue;
		putent((char *) dp);
	}
}
#endif

/*
 * read a bit mask from the tape into m.
 */
readbits(m)
	short	*m;
{
	register i;

	i = spcl.c_count;

	while (i--) {
		readtape((char *) m);
		m += (BSIZE/(MLEN/BITS));
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

stcopy( sourcep, destp, max )
	char *sourcep, *destp;
	int max;
{
	int i;
	for( i=1; i<=max && (*destp++ = *sourcep++); i++ )
		;
	if( i > max )	return( 0 );
	else		return( 1 );
}

append( sourcep, destp, max )
	char *sourcep, *destp;
	int max;
{
	int i;
	for( i=0; *destp; i++ )
		*destp++;
	if( ++i <= max ) *destp++ = '/';
	while( ++i<=max && (*destp++ = *sourcep++) )
		;
	if( i > max )	return( 0 );
	else		return( 1 );
}
/*
 *	Truncate the rightmost file or directory name from a pathname      
 */

trunc( cp )
	char *cp;
{
	char *lstslsh;
	lstslsh = 0;
	while( *cp++ )
		if( *cp == '/' ) lstslsh = cp;
	if( lstslsh == 0 )
		return( 0 );
	else {
		*lstslsh = '\0';
		return( 1 );
	}
}
getxtrlist() {
	struct xtrlist	*malloc(),
			*realloc();

	if( init ) {
		init = 0;
		xtrlist = malloc( SIZEINC*sizeof(struct xtrlist) );
		xsize = SIZEINC;
	}
	else {
		xtrlist = realloc( xtrlist, (SIZEINC+xsize) *
						sizeof(struct xtrlist) );
		xsize += SIZEINC;
	}

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
				xchown(name, spcl.c_dinode.di_uid, spcl.c_dinode.di_gid);
			}
			*cp = '/';
		}
	}
}

/*
 *	Store a string in core returning a pointer to it.  Allocate space
 *	as needed.
 */
char *
ststore( stringp )
	char *stringp;
{
	static char *spacep;
	static int spaceleft;
	char *rtnp, *savep;

	rtnp = spacep;
	savep = stringp;
	while( spaceleft-- && (*spacep++ = *stringp++) );
	if( spaceleft >= 0 )
		return( rtnp );
	else {
		spaceleft = 10 * NSIZE;
		spacep = (char *)malloc( spaceleft );
		return( ststore(savep) );
	}
}

/*
 *	Recursively find names and inumbers of all files in subtree 
 *	pname and put them in xtrlist[]
 */
getleaves( pname )
	char *pname;
{
	int 	n, 		/* loop counter */
		bptsave, 	/* placeholder for pointer into drblock */
		readsize;	/* nbytes read into drblock at cur level
				   of recursion */
	char 	locname[NSIZE];	/* pname + an entry from drblock */
	daddr_t dptsave, 	/* disk loc where cur drblock came from */
		disk_loc;	/* used to see if getent() causes a phys read */
	ino_t 	d;		/* inode no of pname */

	stcopy( pname, locname, NSIZE );
	if( (d = psearch(locname)) == 0 || BIT( d, dumpmap) == 0 ) {
		fprintf(stderr, "%s: not on the tape\n", locname );
		return;
	}

	for( n=0; n<MAXINO; n++ ) {
		if( inotab[n].t_ino == d ) {
			/*
			 * locname is a directory name 
			 */
				/* phys disk read forced so reset readsize */
			mseek( inotab[n].t_seekpt);
			getent( (char *)&dir );
			readsize = nread;
	
				/* "/" signals end of directory */
			while( !direq(dir.d_name,"/") ) {

				if( direq(dir.d_name,".") ) {
					getent( (char *)&dir );
					continue;
				}

				if( direq(dir.d_name,"..") ) {
					getent( (char *)&dir );
					continue;
				}

				if( append(dir.d_name,locname,NSIZE) == 0 ) {
					fprintf(stderr, "name exceedes %d char\n",NSIZE);
					continue;
				}

					/* info for rereading drblock later */
				dptsave = lseek( df, 0L, 1 ) - readsize;
				bptsave = bpt;

				getleaves( locname );
	
					/* reread drblock after recursion rtn */
				lseek( df, dptsave, 0 );
				read( df, drblock, BSIZE );
				bpt = bptsave;

				if( trunc(locname) == 0 ) {
					fprintf(stderr,  "Trouble with name trunc\n" );
					abort();
				}
					/* get next entry from drblock; reset
					 * readsize iff physical disk read */
				disk_loc = lseek( df, 0L, 1 );
				getent( (char *)&dir );
				if( lseek(df,0L,1) != disk_loc )
					readsize = nread;
			}
			return;
		}
	}
	/*
	 * locname is name of a simple file 
	 */
	xtrlist[i].x_ino = d;
	xtrlist[i].x_flags |= XINUSE;
	xtrlist[i].x_name = (char *)ststore( locname );
	if( ++i >= xsize ) getxtrlist();
	fprintf(stderr,  "%s: inode %u\n", locname, d );

}


