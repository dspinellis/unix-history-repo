static	char *sccsid = "@(#)main.c	1.9 (Berkeley) %G%";

#include <stdio.h>
#include <ctype.h>
#include "../h/param.h"
#include "../h/fs.h"
#include "../h/dir.h"
#include "../h/inode.h"
#include "../h/stat.h"
#include <fstab.h>

typedef	int	(*SIG_TYP)();

#define	NDIRECT	(BSIZE/sizeof(struct direct))
#define	SPERB	(BSIZE/sizeof(short))

#define	MAXDUP	10		/* limit on dup blks (per inode) */
#define	MAXBAD	10		/* limit on bad blks (per inode) */

#define	USTATE	0		/* inode not allocated */
#define	FSTATE	01		/* inode is file */
#define	DSTATE	02		/* inode is directory */
#define	CLEAR	03		/* inode is to be cleared */

typedef struct dinode	DINODE;
typedef struct direct	DIRECT;

#define	ALLOC	((dp->di_mode & IFMT) != 0)
#define	DIR	((dp->di_mode & IFMT) == IFDIR)
#define	REG	((dp->di_mode & IFMT) == IFREG)
#define	BLK	((dp->di_mode & IFMT) == IFBLK)
#define	CHR	((dp->di_mode & IFMT) == IFCHR)
#define	MPC	((dp->di_mode & IFMT) == IFMPC)
#define	MPB	((dp->di_mode & IFMT) == IFMPB)
#define	SPECIAL	(BLK || CHR || MPC || MPB)

ino_t	startinum;		/* blk num of first in raw area */

struct bufarea {
	struct bufarea	*b_next;		/* must be first */
	daddr_t	b_bno;
	int	b_size;
	union {
		char	b_buf[BSIZE];		/* buffer space */
		short	b_lnks[SPERB];		/* link counts */
		daddr_t	b_indir[NINDIR];	/* indirect block */
		struct	fs b_fs;		/* super block */
		struct	cg b_cg;		/* cylinder group */
		struct dinode b_dinode[INOPB];	/* inode block */
		DIRECT b_dir[NDIRECT];		/* directory */
	} b_un;
	char	b_dirty;
};

typedef struct bufarea BUFAREA;

BUFAREA	inoblk;			/* inode blocks */
BUFAREA	fileblk;		/* other blks in filesys */
BUFAREA	sblk;			/* file system superblock */
BUFAREA	cgblk;

#define	initbarea(x)	(x)->b_dirty = 0;(x)->b_bno = (daddr_t)-1
#define	dirty(x)	(x)->b_dirty = 1
#define	inodirty()	inoblk.b_dirty = 1
#define	sbdirty()	sblk.b_dirty = 1
#define	cgdirty()	cgblk.b_dirty = 1

#define	dirblk		fileblk.b_un
#define	sblock		sblk.b_un.b_fs
#define	cgrp		cgblk.b_un.b_cg

struct filecntl {
	int	rfdes;
	int	wfdes;
	int	mod;
} dfile;			/* file descriptors for filesys */

#define	DUPTBLSIZE	100	/* num of dup blocks to remember */
daddr_t	duplist[DUPTBLSIZE];	/* dup block table */
daddr_t	*enddup;		/* next entry in dup table */
daddr_t	*muldup;		/* multiple dups part of table */

#define	MAXLNCNT	20	/* num zero link cnts to remember */
ino_t	badlncnt[MAXLNCNT];	/* table of inos with zero link cnts */
ino_t	*badlnp;		/* next entry in table */

char	rawflg;
char	nflag;			/* assume a no response */
char	yflag;			/* assume a yes response */
int	bflag;			/* location of alternate super block */
char	preen;			/* just fix normal inconsistencies */
char	rplyflag;		/* any questions asked? */
char	hotroot;		/* checking root device */
char	fixcg;			/* corrupted free list bit maps */

char	*blkmap;		/* ptr to primary blk allocation map */
char	*freemap;		/* ptr to secondary blk allocation map */
char	*statemap;		/* ptr to inode state table */
short	*lncntp;		/* ptr to link count table */

char	*pathp;			/* pointer to pathname position */
char	*thisname;		/* ptr to current pathname component */
char	*srchname;		/* name being searched for in dir */
char	pathname[200];

char	*lfname = "lost+found";

ino_t	inum;			/* inode we are currently working on */
ino_t	imax;			/* number of inodes */
ino_t	parentdir;		/* i number of parent directory */
ino_t	lastino;		/* hiwater mark of inodes */
ino_t	lfdir;			/* lost & found directory */
ino_t	orphan;			/* orphaned inode */

off_t	filsize;		/* num blks seen in file */
off_t	maxblk;			/* largest logical blk in file */
off_t	bmapsz;			/* num chars in blkmap */

daddr_t	n_ffree;		/* number of small free blocks */
daddr_t	n_bfree;		/* number of large free blocks */
daddr_t	n_blks;			/* number of blocks used */
daddr_t	n_files;		/* number of files seen */
daddr_t	n_index;
daddr_t	n_bad;
daddr_t	fmax;			/* number of blocks in the volume */

daddr_t	badblk;
daddr_t	dupblk;

int	inosumbad;
int	offsumbad;
int	frsumbad;

#define	howmany(x, y)	(((x)+((y)-1))/(y))
#define	roundup(x, y)	((((x)+((y)-1))/(y))*(y))
#define	zapino(x)	(*(x) = zino)
struct	dinode zino;

#define	setlncnt(x)	(lncntp[inum] = x)
#define	getlncnt()	(lncntp[inum])
#define	declncnt()	(--lncntp[inum])

#define	setbmap(x)	setbit(blkmap, x)
#define	getbmap(x)	isset(blkmap, x)
#define	clrbmap(x)	clrbit(blkmap, x)

#define	setfmap(x)	setbit(freemap, x)
#define	getfmap(x)	isset(freemap, x)
#define	clrfmap(x)	clrbit(freemap, x)

#define	setstate(x)	(statemap[inum] = x)
#define	getstate()	statemap[inum]

#define	DATA	1
#define	ADDR	0

#define	ALTERD	010
#define	KEEPON	04
#define	SKIP	02
#define	STOP	01

int	(*signal())();
long	lseek();
time_t	time();
DINODE	*ginode();
BUFAREA	*getblk();
int	dirscan();
int	findino();
int	catch();
int	mkentry();
int	chgdd();
int	pass1(), pass1b(), pass2(), pass4(), pass5();
int	(*pfunc)();
char	*rawname(), *rindex(), *unrawname();
extern int inside[], around[];
extern unsigned char fragtbl[];

char	*devname;

main(argc, argv)
int	argc;
char	*argv[];
{
	struct fstab *fsp;
	int pid, passno, anygtr, sumstatus;

	sync();
	while (--argc > 0 && **++argv == '-') {
		switch (*++*argv) {

		case 'p':
			preen++;
			break;

		case 'b':
			bflag = atoi(argv[0]+1);
			printf("Alternate super block location: %d\n", bflag);
			break;

		case 'n':	/* default no answer flag */
		case 'N':
			nflag++;
			yflag = 0;
			break;

		case 'y':	/* default yes answer flag */
		case 'Y':
			yflag++;
			nflag = 0;
			break;

		default:
			errexit("%c option?\n", **argv);
		}
	}
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, catch);
	if (argc) {
		while (argc-- > 0) {
			hotroot = 0;
			check(*argv++);
		}
		exit(0);
	}
	sumstatus = 0;
	passno = 1;
	do {
		anygtr = 0;
		if (setfsent() == 0)
			errexit("Can't open checklist file: %s\n", FSTAB);
		while ((fsp = getfsent()) != 0) {
			if (strcmp(fsp->fs_type, FSTAB_RW) &&
			    strcmp(fsp->fs_type, FSTAB_RO))
				continue;
			if (preen == 0 ||
			    passno == 1 && fsp->fs_passno == passno) {
				if (blockcheck(fsp->fs_spec) == 0 && preen)
					exit(8);
			} else if (fsp->fs_passno > passno)
				anygtr = 1;
			else if (fsp->fs_passno == passno) {
				pid = fork();
				if (pid < 0) {
					perror("fork");
					exit(8);
				}
				if (pid == 0)
					if (blockcheck(fsp->fs_spec)==0)
						exit(8);
					else
						exit(0);
			}
		}
		if (preen) {
			int status;
			while (wait(&status) != -1)
				sumstatus |= status;
		}
		passno++;
	} while (anygtr);
	if (sumstatus)
		exit(8);
	endfsent();
	exit(0);
}

blockcheck(name)
	char *name;
{
	struct stat stslash, stblock, stchar;
	char *raw;
	int looped = 0;

	hotroot = 0;
	if (stat("/", &stslash) < 0){
		error("Can't stat root\n");
		return (0);
	}
retry:
	if (stat(name, &stblock) < 0){
		error("Can't stat %s\n", name);
		return (0);
	}
	if (stblock.st_mode & S_IFBLK) {
		raw = rawname(name);
		if (stat(raw, &stchar) < 0){
			error("Can't stat %s\n", raw);
			return (0);
		}
		if (stchar.st_mode & S_IFCHR) {
			if (stslash.st_dev == stblock.st_rdev) {
				hotroot++;
				raw = unrawname(name);
			}
			check(raw);
			return (1);
		} else {
			error("%s is not a character device\n", raw);
			return (0);
		}
	} else if (stblock.st_mode & S_IFCHR) {
		if (looped) {
			error("Can't make sense out of name %s\n", name);
			return (0);
		}
		name = unrawname(name);
		looped++;
		goto retry;
	}
	error("Can't make sense out of name %s\n", name);
	return (0);
}

char *
unrawname(cp)
	char *cp;
{
	char *dp = rindex(cp, '/');
	struct stat stb;

	if (dp == 0)
		return (cp);
	if (stat(cp, &stb) < 0)
		return (cp);
	if ((stb.st_mode&S_IFMT) != S_IFCHR)
		return (cp);
	if (*(dp+1) != 'r')
		return (cp);
	strcpy(dp+1, dp+2);
	return (cp);
}

char *
rawname(cp)
	char *cp;
{
	static char rawbuf[32];
	char *dp = rindex(cp, '/');

	if (dp == 0)
		return (0);
	*dp = 0;
	strcpy(rawbuf, cp);
	*dp = '/';
	strcat(rawbuf, "/r");
	strcat(rawbuf, dp+1);
	return (rawbuf);
}

check(dev)
	char *dev;
{
	register DINODE *dp;
	register ino_t *blp;
	register int i, n;
	ino_t savino;
	int b, c;
	daddr_t d, s;

	devname = dev;
	if (setup(dev) == 0) {
		if (preen)
			pfatal("CAN'T CHECK DEVICE.");
		return;
	}
/* 1 */
	if (preen==0) {
		if (hotroot)
			printf("** Root file system\n");
		printf("** Phase 1 - Check Blocks and Sizes\n");
	}
	pfunc = pass1;
	inum = 0;
	n_blks += roundup(sblock.fs_ncg * sizeof (struct csum), BSIZE)
		/ BSIZE * FRAG;
	for (c = 0; c < sblock.fs_ncg; c++) {
		if (getblk(&cgblk, cgtod(c, &sblock), sblock.fs_cgsize) == 0)
			continue;
		n = 0;
		for (i = 0; i < sblock.fs_ipg; i++, inum++) {
			dp = ginode();
			if (dp == NULL)
				continue;
			if (ALLOC) {
				if (!isset(cgrp.cg_iused, i)) {
					/*
					printf("%d bad, not used\n", inum);
					*/
					inosumbad++;
					n++;
				}
				lastino = inum;
				if (ftypeok(dp) == 0) {
					pfatal("UNKNOWN FILE TYPE I=%u", inum);
					if (reply("CLEAR") == 1) {
						zapino(dp);
						inodirty();
						inosumbad++;
					}
					continue;
				}
				n_files++;
				if (setlncnt(dp->di_nlink) <= 0) {
					if (badlnp < &badlncnt[MAXLNCNT])
						*badlnp++ = inum;
					else {
						pfatal("LINK COUNT TABLE OVERFLOW");
						if (reply("CONTINUE") == 0)
							errexit("");
					}
				}
				setstate(DIR ? DSTATE : FSTATE);
				badblk = dupblk = 0; filsize = 0; maxblk = 0;
				ckinode(dp, ADDR);
				if (DIR && dp->di_size % sizeof(DIRECT)) {
					pwarn("DIRECTORY MISALIGNED I=%u\n",
					    inum);
					if (preen == 0)
						printf("\n");
				}
			} else {
				n++;
				if (isset(cgrp.cg_iused, i)) {
					/*
					printf("%d bad, marked used\n", inum);
					*/
					inosumbad++;
					n--;
				}
				if (dp->di_mode != 0) {
					pfatal("PARTIALLY ALLOCATED INODE I=%u", inum);
					if (reply("CLEAR") == 1) {
						zapino(dp);
						inodirty();
						inosumbad++;
					}
				}
			}
		}
		if (n != cgrp.cg_nifree) {
			printf("cg[%d].cg_nifree is %d not %d\n",
			    c, cgrp.cg_nifree, n);
			inosumbad++;
		}
	}
/* 1b */
	if (enddup != &duplist[0]) {
		if (preen)
			pfatal("INTERNAL ERROR: dups with -p");
		printf("** Phase 1b - Rescan For More DUPS\n");
		pfunc = pass1b;
		inum = 0;
		for (c = 0; c < sblock.fs_ncg; c++) {
			for (i = 0; i < sblock.fs_ipg; i++, inum++) {
				dp = ginode();
				if (dp == NULL)
					continue;
				if (getstate() != USTATE &&
				    (ckinode(dp, ADDR) & STOP))
					goto out1b;
			}
		}
	}
out1b:
	flush(&dfile, &inoblk);
/* 2 */
	if (preen == 0)
		printf("** Phase 2 - Check Pathnames\n");
	inum = ROOTINO;
	thisname = pathp = pathname;
	pfunc = pass2;
	switch (getstate()) {

	case USTATE:
		errexit("ROOT INODE UNALLOCATED. TERMINATING.\n");

	case FSTATE:
		pfatal("ROOT INODE NOT DIRECTORY");
		if (reply("FIX") == 0 || (dp = ginode()) == NULL)
			errexit("");
		dp->di_mode &= ~IFMT;
		dp->di_mode |= IFDIR;
		inodirty();
		inosumbad++;
		setstate(DSTATE);
		/* fall into ... */

	case DSTATE:
		descend();
		break;

	case CLEAR:
		pfatal("DUPS/BAD IN ROOT INODE");
		printf("\n");
		if (reply("CONTINUE") == 0)
			errexit("");
		setstate(DSTATE);
		descend();
	}
/* 3 */
	if (preen == 0)
		printf("** Phase 3 - Check Connectivity\n");
	for (inum = ROOTINO; inum <= lastino; inum++) {
		if (getstate() == DSTATE) {
			pfunc = findino;
			srchname = "..";
			savino = inum;
			do {
				orphan = inum;
				if ((dp = ginode()) == NULL)
					break;
				filsize = dp->di_size;
				parentdir = 0;
				ckinode(dp, DATA);
				if ((inum = parentdir) == 0)
					break;
			} while (getstate() == DSTATE);
			inum = orphan;
			if (linkup() == 1) {
				thisname = pathp = pathname;
				*pathp++ = '?';
				pfunc = pass2;
				descend();
			}
			inum = savino;
		}
	}
/* 4 */
	if (preen == 0)
		printf("** Phase 4 - Check Reference Counts\n");
	pfunc = pass4;
	for (inum = ROOTINO; inum <= lastino; inum++) {
		switch (getstate()) {

		case FSTATE:
			if (n = getlncnt())
				adjust((short)n);
			else {
				for (blp = badlncnt;blp < badlnp; blp++)
					if (*blp == inum) {
						clri("UNREF", 1);
						break;
					}
			}
			break;

		case DSTATE:
			clri("UNREF", 1);
			break;

		case CLEAR:
			clri("BAD/DUP", 1);
			break;
		}
	}
	if (imax - n_files != sblock.fs_nifree) {
		pwarn("FREE INODE COUNT WRONG IN SUPERBLK");
		if (preen)
			printf(" (FIXED)\n");
		if (preen || reply("FIX") == 1) {
			sblock.fs_nifree = imax - n_files;
			sbdirty();
		}
	}
	flush(&dfile, &fileblk);

/* 5 */
	if (preen == 0)
		printf("** Phase 5 - Check Cyl groups\n");
	copy(blkmap, freemap, (unsigned)bmapsz);
	dupblk = 0;
	n_index = sblock.fs_ncg * (cgdmin(0, &sblock) - cgtod(0, &sblock));
	for (c = 0; c < sblock.fs_ncg; c++) {
		daddr_t cbase = cgbase(c,&sblock);
		short bo[MAXCPG][NRPOS];
		short frsum[FRAG];
		int blk;

		for (n = 0; n < sblock.fs_cpg; n++)
			for (i = 0; i < NRPOS; i++)
				bo[n][i] = 0;
		for (i = 0; i < FRAG; i++) {
			frsum[i] = 0;
		}
		/*
		 * need to account for the spare boot and super blocks
		 * which appear (inaccurately) bad
		 */
		n_bad += cgtod(c, &sblock) - cbase;
		if (getblk(&cgblk, cgtod(c, &sblock), sblock.fs_cgsize) == 0)
			continue;
		for (b = 0; b < sblock.fs_fpg; b += FRAG) {
			if (isblock(cgrp.cg_free, b/FRAG)) {
				if (pass5(cbase+b, FRAG) == STOP)
					goto out5;
				/* this is clumsy ... */
				n_ffree -= FRAG;
				n_bfree++;
				s = b * NSPF;
				bo[s/sblock.fs_spc]
				    [s%sblock.fs_nsect*NRPOS/sblock.fs_nsect]++;
			} else {
				for (d = 0; d < FRAG; d++)
					if (isset(cgrp.cg_free, b+d))
						if (pass5(cbase+b+d,1) == STOP)
							goto out5;
				blk = ((cgrp.cg_free[b / NBBY] >> (b % NBBY)) &
				       (0xff >> (NBBY - FRAG)));
				if (blk != 0)
					fragacct(blk, frsum, 1);
			}
		}
		for (i = 0; i < FRAG; i++) {
			if (cgrp.cg_frsum[i] != frsum[i]) {
				printf("cg[%d].cg_frsum[%d] have %d calc %d\n",
					c, i, cgrp.cg_frsum[i], frsum[i]);
				frsumbad++;
			}
		}
		for (n = 0; n < sblock.fs_cpg; n++)
			for (i = 0; i < NRPOS; i++)
				if (bo[n][i] != cgrp.cg_b[n][i]) {
					printf("cg[%d].cg_b[%d][%d] have %d calc %d\n",
					    c, n, i, cgrp.cg_b[n][i], bo[n][i]);
					offsumbad++;
				}
	}
out5:
	if (dupblk)
		pwarn("%d DUP BLKS IN BIT MAPS\n", dupblk);
	if (fixcg == 0) {
		if ((b = n_blks+n_ffree+FRAG*n_bfree+n_index+n_bad) != fmax) {
			pwarn("%ld BLK(S) MISSING\n", fmax - b);
			fixcg = 1;
		} else if (inosumbad + offsumbad + frsumbad) {
			pwarn("SUMMARY INFORMATION %s%s%sBAD\n",
			    inosumbad ? "(INODE FREE) " : "",
			    offsumbad ? "(BLOCK OFFSETS) " : "",
			    frsumbad ? "(FRAG SUMMARIES) " : "");
			fixcg = 1;
		} else if (n_ffree != sblock.fs_nffree ||
		    n_bfree != sblock.fs_nbfree) {
			pwarn("FREE BLK COUNT(S) WRONG IN SUPERBLK");
			if (preen)
				printf(" (FIXED)\n");
			if (preen || reply("FIX") == 1) {
				sblock.fs_nffree = n_ffree;
				sblock.fs_nbfree = n_bfree;
				sbdirty();
			}
		}
	}
	if (fixcg) {
		pwarn("BAD CYLINDER GROUPS");
		if (preen)
			printf(" (SALVAGED)\n");
		else if (reply("SALVAGE") == 0)
			fixcg = 0;
	}

	if (fixcg) {
		if (preen == 0)
			printf("** Phase 6 - Salvage Cylinder Groups\n");
		for (i = 0; i < howmany(cssize(&sblock), BSIZE); i++) {
			sblock.fs_csp[i] = (struct csum *)calloc(1, BSIZE);
			getblk((char *)sblock.fs_csp[i],
			       csaddr(&sblock) + (i * FRAG), BSIZE);
		}
		makecg();
		for (i = 0; i < howmany(cssize(&sblock), BSIZE); i++) {
			bwrite(&dfile, (char *)sblock.fs_csp[i],
			       csaddr(&sblock) + (i * FRAG), BSIZE);
		}
		n_ffree = sblock.fs_nffree;
		n_bfree = sblock.fs_nbfree;
	}

	pwarn("%d files, %d used, %d free (%d frags, %d blocks)\n",
	    n_files, n_blks, n_ffree + FRAG * n_bfree, n_ffree, n_bfree);
	if (dfile.mod) {
		time(&sblock.fs_time);
		sbdirty();
	}
	ckfini();
	sync();
	if (dfile.mod && hotroot) {
		printf("\n***** BOOT UNIX (NO SYNC!) *****\n");
		exit(4);
	}
	if (dfile.mod && preen == 0)
		printf("\n***** FILE SYSTEM WAS MODIFIED *****\n");
	free(blkmap);
	free(freemap);
	free(statemap);
	free(lncntp);
}

/* VARARGS1 */
error(s1, s2, s3, s4)
char *s1;
{

	printf(s1, s2, s3, s4);
}

/* VARARGS1 */
errexit(s1, s2, s3, s4)
char *s1;
{
	error(s1, s2, s3, s4);
	exit(8);
}

/*
 * An inconsistency occured which shouldn't during normal operations.
 * Die if preening, otw just printf.
 */
/* VARARGS1 */
pfatal(s, a1, a2, a3)
	char *s;
{

	if (preen) {
		printf("%s: ", devname);
		printf(s, a1, a2, a3);
		printf("\n");
		preendie();
	}
	printf(s, a1, a2, a3);
}

preendie()
{

	printf("%s: UNEXPECTED INCONSISTENCY; RUN fsck MANUALLY.\n", devname);
	exit(8);
}

/*
 * Pwarn is like printf when not preening,
 * or a warning (preceded by filename) when preening.
 */
/* VARARGS1 */
pwarn(s, a1, a2, a3, a4, a5)
	char *s;
{

	if (preen)
		printf("%s: ", devname);
	printf(s, a1, a2, a3, a4, a5);
}

ckinode(dp, flg)
	DINODE *dp;
	register flg;
{
	register daddr_t *ap;
	register ret;
	int (*func)(), n, ndb, size;

	if (SPECIAL)
		return (KEEPON);
	func = (flg == ADDR) ? pfunc : dirscan;
	ndb = howmany(dp->di_size, BSIZE);
	for (ap = &dp->di_db[0]; ap < &dp->di_db[NDADDR]; ap++) {
		if (--ndb == 0 && (dp->di_size&BMASK))
			size = howmany(dp->di_size&BMASK, FSIZE);
		else
			size = FRAG;
		if (*ap && (ret = (*func)(*ap, size)) & STOP)
			return (ret);
	}
	for (ap = &dp->di_ib[0], n = 1; n <= 2; ap++, n++) {
		if (*ap && (ret = iblock(*ap, n, flg, dp->di_size - BSIZE * NDADDR)) & STOP)
			return (ret);
	}
	return (KEEPON);
}

iblock(blk, ilevel, flg, isize)
	daddr_t blk;
	register ilevel;
	int isize;
{
	register daddr_t *ap;
	register daddr_t *aplim;
	register int i, n;
	int (*func)(), nif;
	BUFAREA ib;

	if (flg == ADDR) {
		func = pfunc;
		if (((n = (*func)(blk, FRAG)) & KEEPON) == 0)
			return (n);
	} else
		func = dirscan;
	if (outrange(blk))		/* protect thyself */
		return (SKIP);
	initbarea(&ib);
	if (getblk(&ib, blk, BSIZE) == NULL)
		return (SKIP);
	ilevel--;
	if (ilevel == 0) {
		nif = isize / BSIZE + 1;
	} else /* ilevel == 1 */ {
		nif = isize / (BSIZE * NINDIR) + 1;
	}
	if (nif > NINDIR)
		nif = NINDIR;
	aplim = & ib.b_un.b_indir[nif];
	for (ap = ib.b_un.b_indir, i = 1; ap < aplim; ap++, i++)
		if (*ap) {
			if (ilevel > 0)
				n = iblock(*ap, ilevel, flg, isize - i * NINDIR * BSIZE);
			else
				n = (*func)(*ap, FRAG);
			if (n & STOP)
				return (n);
		}
	return (KEEPON);
}

pass1(blk, size)
	daddr_t blk;
	int size;
{
	register daddr_t *dlp;
	int res = KEEPON;

	for (; size > 0; blk++, size--) {
		if (outrange(blk)) {
			blkerr("BAD", blk);
			if (++badblk >= MAXBAD) {
				printf("EXCESSIVE BAD BLKS I=%u", inum);
				if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			res = SKIP;
		} else if (getbmap(blk)) {
			blkerr("DUP", blk);
			if (++dupblk >= MAXDUP) {
				printf("EXCESSIVE DUP BLKS I=%u", inum);
				if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			if (enddup >= &duplist[DUPTBLSIZE]) {
				printf("DUP TABLE OVERFLOW.");
				if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			for (dlp = duplist; dlp < muldup; dlp++)
				if (*dlp == blk) {
					*enddup++ = blk;
					break;
				}
			if (dlp >= muldup) {
				*enddup++ = *muldup;
				*muldup++ = blk;
			}
		} else {
			n_blks++;
			setbmap(blk);
		}
		filsize++;
	}
	return (res);
}

pass1b(blk, size)
	daddr_t blk;
	int size;
{
	register daddr_t *dlp;
	int res = KEEPON;

	for (; size > 0; blk++, size--) {
		if (outrange(blk))
			res = SKIP;
		for (dlp = duplist; dlp < muldup; dlp++)
			if (*dlp == blk) {
				blkerr("DUP", blk);
				*dlp = *--muldup;
				*muldup = blk;
				if (muldup == duplist)
					return (STOP);
			}
	}
	return (res);
}

pass2(dirp)
	register DIRECT *dirp;
{
	register char *p;
	register n;
	DINODE *dp;

	if ((inum = dirp->d_ino) == 0)
		return (KEEPON);
	thisname = pathp;
	for (p = dirp->d_name; p < &dirp->d_name[DIRSIZ]; )
		if ((*pathp++ = *p++) == 0) {
			--pathp;
			break;
		}
	*pathp = 0;
	n = 0;
	if (inum > imax || inum < ROOTINO)
		n = direrr("I OUT OF RANGE");
	else {
again:
		switch (getstate()) {
		case USTATE:
			n = direrr("UNALLOCATED");
			break;

		case CLEAR:
			if ((n = direrr("DUP/BAD")) == 1)
				break;
			if ((dp = ginode()) == NULL)
				break;
			setstate(DIR ? DSTATE : FSTATE);
			goto again;

		case FSTATE:
			declncnt();
			break;

		case DSTATE:
			declncnt();
			descend();
			break;
		}
	}
	pathp = thisname;
	if (n == 0)
		return (KEEPON);
	dirp->d_ino = 0;
	return (KEEPON|ALTERD);
}

pass4(blk, size)
daddr_t blk;
{
	register daddr_t *dlp;
	int res = KEEPON;

	for (; size > 0; blk++, size--) {
		if (outrange(blk))
			res = SKIP;
		else if (getbmap(blk)) {
			for (dlp = duplist; dlp < enddup; dlp++)
				if (*dlp == blk) {
					*dlp = *--enddup;
					return (KEEPON);
				}
			clrbmap(blk);
			n_blks--;
		}
	}
	return (res);
}

pass5(blk, size)
	daddr_t blk;
	int size;
{
	int res = KEEPON;

	for (; size > 0; blk++, size--) {
		if (outrange(blk)) {
			fixcg = 1;
			if (preen)
				pfatal("BAD BLOCKS IN BIT MAPS.");
			if (++badblk >= MAXBAD) {
				printf("EXCESSIVE BAD BLKS IN BIT MAPS.");
				if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
		} else if (getfmap(blk)) {
			fixcg = 1;
			if (++dupblk >= DUPTBLSIZE) {
				printf("EXCESSIVE DUP BLKS IN BIT MAPS.");
				if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
		} else {
			n_ffree++;
			setfmap(blk);
		}
	}
	return (res);
}

outrange(blk)
	daddr_t blk;
{
	register int c;

	c = dtog(blk, &sblock);
	if (blk >= fmax || blk < cgdmin(c, &sblock)) {
		return (1);
	}
	return (0);
}

blkerr(s, blk)
	daddr_t blk;
	char *s;
{
	pfatal("%ld %s I=%u", blk, s, inum);
	printf("\n");
	setstate(CLEAR);	/* mark for possible clearing */
}

descend()
{
	register DINODE *dp;
	register char *savname;
	off_t savsize;

	setstate(FSTATE);
	if ((dp = ginode()) == NULL)
		return;
	savname = thisname;
	*pathp++ = '/';
	savsize = filsize;
	filsize = dp->di_size;
	ckinode(dp, DATA);
	thisname = savname;
	*--pathp = 0;
	filsize = savsize;
}

dirscan(blk, nf)
daddr_t blk;
int nf;
{
	register DIRECT *dirp;
	register DIRECT *edirp;
	register char *p1, *p2;
	register n;
	DIRECT direntry;

	if (outrange(blk)) {
		filsize -= BSIZE;
		return (SKIP);
	}
	edirp = &dirblk.b_dir[NDIRECT*nf/FRAG];
	for (dirp = dirblk.b_dir; dirp < edirp &&
		filsize > 0; dirp++, filsize -= sizeof(DIRECT)) {
		if (getblk(&fileblk, blk, nf * FSIZE) == NULL) {
			filsize -= (&dirblk.b_dir[NDIRECT]-dirp)*sizeof(DIRECT);
			return (SKIP);
		}
		p1 = &dirp->d_name[DIRSIZ];
		p2 = &direntry.d_name[DIRSIZ];
		while (p1 > (char *)dirp)
			*--p2 = *--p1;
		if ((n = (*pfunc)(&direntry)) & ALTERD) {
			if (getblk(&fileblk, blk, nf * FSIZE) != NULL) {
				p1 = &dirp->d_name[DIRSIZ];
				p2 = &direntry.d_name[DIRSIZ];
				while (p1 > (char *)dirp)
					*--p1 = *--p2;
				sbdirty();
			} else
				n &= ~ALTERD;
		}
		if (n & STOP)
			return (n);
	}
	return (filsize > 0 ? KEEPON : STOP);
}

direrr(s)
char *s;
{
	register DINODE *dp;

	pwarn("%s ", s);
	pinode();
	printf("\n");
	if ((dp = ginode()) != NULL && ftypeok(dp))
		pfatal("%s=%s", DIR?"DIR":"FILE", pathname);
	else
		pfatal("NAME=%s", pathname);
	return (reply("REMOVE"));
}

adjust(lcnt)
	register short lcnt;
{
	register DINODE *dp;

	if ((dp = ginode()) == NULL)
		return;
	if (dp->di_nlink == lcnt) {
		if (linkup() == 0)
			clri("UNREF", 0);
	}
	else {
		pwarn("LINK COUNT %s",
			(lfdir==inum)?lfname:(DIR?"DIR":"FILE"));
		pinode();
		printf(" COUNT %d SHOULD BE %d",
			dp->di_nlink, dp->di_nlink-lcnt);
		if (preen) {
			if (lcnt < 0) {
				printf("\n");
				preendie();
			}
			printf(" (ADJUSTED)\n");
		}
		if (preen || reply("ADJUST") == 1) {
			dp->di_nlink -= lcnt;
			inodirty();
		}
	}
}

clri(s, flg)
char *s;
{
	register DINODE *dp;

	if ((dp = ginode()) == NULL)
		return;
	if (flg == 1) {
		pwarn("%s %s", s, DIR?"DIR":"FILE");
		pinode();
	}
	if (preen || reply("CLEAR") == 1) {
		if (preen)
			printf(" (CLEARED)\n");
		n_files--;
		pfunc = pass4;
		ckinode(dp, ADDR);
		zapino(dp);
		setstate(USTATE);
		inodirty();
		inosumbad++;
	}
}

setup(dev)
char *dev;
{
	dev_t rootdev;
	struct stat statb;
	int super = bflag ? bflag : SBLOCK;

	bflag = 0;
	if (stat("/", &statb) < 0)
		errexit("Can't stat root\n");
	rootdev = statb.st_dev;
	if (stat(dev, &statb) < 0) {
		error("Can't stat %s\n", dev);
		return (0);
	}
	rawflg = 0;
	if ((statb.st_mode & S_IFMT) == S_IFBLK)
		;
	else if ((statb.st_mode & S_IFMT) == S_IFCHR)
		rawflg++;
	else {
		if (reply("file is not a block or character device; OK") == 0)
			return (0);
	}
	if (rootdev == statb.st_rdev)
		hotroot++;
	if ((dfile.rfdes = open(dev, 0)) < 0) {
		error("Can't open %s\n", dev);
		return (0);
	}
	if (preen == 0)
		printf("** %s", dev);
	if (nflag || (dfile.wfdes = open(dev, 1)) < 0) {
		dfile.wfdes = -1;
		if (preen)
			pfatal("NO WRITE ACCESS");
		printf(" (NO WRITE)");
	}
	if (preen == 0)
		printf("\n");
	fixcg = 0; inosumbad = 0; offsumbad = 0; frsumbad = 0;
	dfile.mod = 0;
	n_files = n_blks = n_ffree = n_bfree = 0;
	muldup = enddup = &duplist[0];
	badlnp = &badlncnt[0];
	lfdir = 0;
	rplyflag = 0;
	initbarea(&sblk);
	initbarea(&fileblk);
	initbarea(&inoblk);
	initbarea(&cgblk);
	if (getblk(&sblk, super, BSIZE) == NULL) {
		ckfini();
		return (0);
	}
	sblk.b_bno = super;
	if (sblock.fs_magic != FS_MAGIC)
		{ badsb("MAGIC NUMBER WRONG"); return (0); }
	if (sblock.fs_ncg < 1)
		{ badsb("NCG OUT OF RANGE"); return (0); }
	if (sblock.fs_cpg < 1 || sblock.fs_cpg > MAXCPG)
		{ badsb("CPG OUT OF RANGE"); return (0); }
	if (sblock.fs_nsect < 1)
		{ badsb("NSECT < 1"); return (0); }
	if (sblock.fs_ntrak < 1)
		{ badsb("NTRAK < 1"); return (0); }
	if (sblock.fs_ipg*sblock.fs_ncg > 65535 || sblock.fs_ipg%INOPB)
		{ badsb("TOO MANY INODES IMPLIED"); return (0); }
	if (sblock.fs_ipg/INOPF+IBLOCK >=
	    sblock.fs_cpg*sblock.fs_nsect*sblock.fs_ntrak/NSPF)
		{ badsb("IMPLIES MORE INODE THAN DATA BLOCKS"); return (0); }
/* THE FOLLOWING COULD BE CHECKED MORE CLOSELY... */
	if ((sblock.fs_ncg + 1) * sblock.fs_cpg < sblock.fs_ncyl ||
	    (sblock.fs_ncg - 1) * sblock.fs_cpg > sblock.fs_ncyl)
		{ badsb("NCYL DOES NOT JIVE WITH NCG*CPG"); return (0); }
	if (sblock.fs_fpg != sblock.fs_cpg * sblock.fs_spc / NSPF)
		{ badsb("FPG DOES NOT JIVE WITH CPG & SPC"); return (0); }
	if (sblock.fs_size <=
	    (sblock.fs_ncg-1)*sblock.fs_fpg+IBLOCK+sblock.fs_ipg/INOPF)
		{ badsb("SIZE PREPOSTEROUSLY SMALL"); return (0); }
	if (sblock.fs_size*NSPF >
	    (sblock.fs_ncg+2)*sblock.fs_cpg*sblock.fs_spc)
		{ badsb("SIZE PREPOSTEROUSLY LARGE"); return (0); }
	/* rest we COULD repair... */
	if (sblock.fs_sblkno != SBLOCK)
		{ badsb("BLKNO CORRUPTED"); return (0); }
	if (sblock.fs_spc != sblock.fs_nsect * sblock.fs_ntrak)
		{ badsb("SPC DOES NOT JIVE w/NTRAK*NSECT"); return (0); }
	if (sblock.fs_cgsize != cgsize(&sblock))
		{ badsb("CGSIZE INCORRECT"); return (0); }
	if (sblock.fs_cssize != cssize(&sblock))
		{ badsb("CSSIZE INCORRECT"); return (0); }
	fmax = sblock.fs_size;
	imax = sblock.fs_ncg * sblock.fs_ipg;

	bmapsz = roundup(howmany(fmax, NBBY), sizeof(short));
	blkmap = (char *)calloc(bmapsz, sizeof (char));
	freemap = (char *)calloc(bmapsz, sizeof (char));
	statemap = (char *)calloc(imax+1, sizeof(char));
	lncntp = (short *)calloc(imax+1, sizeof(short));

	startinum = imax + 1;
	return (1);

badsb:
	ckfini();
	return (0);
}

badsb(s)
	char *s;
{

	if (preen)
		printf("%s: ", devname);
	printf("BAD SUPER BLOCK: %s\n", s);
	pwarn("USE -b OPTION TO FSCK TO SPECIFY LOCATION OF AN ALTERNATE\n");
	pfatal("SUPER-BLOCK TO SUPPLY NEEDED INFORMATION; SEE fsck(8).\n");
}

DINODE *
ginode()
{
	daddr_t iblk;

	if (inum > imax)
		return (NULL);
	if (inum < startinum || inum >= startinum + INOPB) {
		iblk = itod(inum, &sblock);
		if (getblk(&inoblk, iblk, BSIZE) == NULL) {
			return (NULL);
		}
		startinum = (inum / INOPB) * INOPB;
	}
	return (&inoblk.b_un.b_dinode[inum % INOPB]);
}

ftypeok(dp)
	DINODE *dp;
{
	switch (dp->di_mode & IFMT) {

	case IFDIR:
	case IFREG:
	case IFBLK:
	case IFCHR:
	case IFMPC:
	case IFMPB:
		return (1);

	default:
		return (0);
	}
}

reply(s)
	char *s;
{
	char line[80];

	if (preen)
		pfatal("INTERNAL ERROR: GOT TO reply()");
	rplyflag = 1;
	printf("\n%s? ", s);
	if (nflag || dfile.wfdes < 0) {
		printf(" no\n\n");
		return (0);
	}
	if (yflag) {
		printf(" yes\n\n");
		return (1);
	}
	if (getline(stdin, line, sizeof(line)) == EOF)
		errexit("\n");
	printf("\n");
	if (line[0] == 'y' || line[0] == 'Y')
		return (1);
	else
		return (0);
}

getline(fp, loc, maxlen)
	FILE *fp;
	char *loc;
{
	register n;
	register char *p, *lastloc;

	p = loc;
	lastloc = &p[maxlen-1];
	while ((n = getc(fp)) != '\n') {
		if (n == EOF)
			return (EOF);
		if (!isspace(n) && p < lastloc)
			*p++ = n;
	}
	*p = 0;
	return (p - loc);
}

BUFAREA *
getblk(bp, blk, size)
	daddr_t blk;
	register BUFAREA *bp;
	int size;
{
	register struct filecntl *fcp;

	fcp = &dfile;
	if (bp->b_bno == blk)
		return (bp);
	flush(fcp, bp);
	if (bread(fcp, bp->b_un.b_buf, blk, size) != 0) {
		bp->b_bno = blk;
		bp->b_size = size;
		return (bp);
	}
	bp->b_bno = (daddr_t)-1;
	return (NULL);
}

flush(fcp, bp)
	struct filecntl *fcp;
	register BUFAREA *bp;
{

	if (bp->b_dirty)
		bwrite(fcp, bp->b_un.b_buf, bp->b_bno, bp->b_size);
	bp->b_dirty = 0;
}

rwerr(s, blk)
	char *s;
	daddr_t blk;
{

	if (preen == 0)
		printf("\n");
	pfatal("CANNOT %s: BLK %ld", s, blk);
	if (reply("CONTINUE") == 0)
		errexit("Program terminated\n");
}

ckfini()
{

	flush(&dfile, &fileblk);
	flush(&dfile, &sblk);
	if (sblk.b_bno != SBLOCK) {
		sblk.b_bno = SBLOCK;
		sbdirty();
		flush(&dfile, &sblk);
	}
	flush(&dfile, &inoblk);
	close(dfile.rfdes);
	close(dfile.wfdes);
}

pinode()
{
	register DINODE *dp;
	register char *p;
	char uidbuf[200];
	char *ctime();

	printf(" I=%u ", inum);
	if ((dp = ginode()) == NULL)
		return;
	printf(" OWNER=");
	if (getpw((int)dp->di_uid, uidbuf) == 0) {
		for (p = uidbuf; *p != ':'; p++);
		*p = 0;
		printf("%s ", uidbuf);
	}
	else {
		printf("%d ", dp->di_uid);
	}
	printf("MODE=%o\n", dp->di_mode);
	if (preen)
		printf("%s: ", devname);
	printf("SIZE=%ld ", dp->di_size);
	p = ctime(&dp->di_mtime);
	printf("MTIME=%12.12s %4.4s ", p+4, p+20);
}

copy(fp, tp, size)
	register char *tp, *fp;
	unsigned size;
{

	while (size--)
		*tp++ = *fp++;
}

makecg()
{
	int c, blk;
	daddr_t dbase, d, dmin, dmax;
	long i, j, s;
	register struct csum *cs;
	register DINODE *dp;

	sblock.fs_nbfree = 0;
	sblock.fs_nffree = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		dbase = cgbase(c, &sblock);
		dmax = dbase + sblock.fs_fpg;
		if (dmax > sblock.fs_size)
			dmax = sblock.fs_size;
		dmin = cgdmin(c, &sblock) - dbase;
		cs = &sblock.fs_cs(c);
		cgrp.cg_time = time(0);
		cgrp.cg_magic = CG_MAGIC;
		cgrp.cg_cgx = c;
		cgrp.cg_ncyl = sblock.fs_cpg;
		cgrp.cg_niblk = sblock.fs_ipg;
		cgrp.cg_ndblk = dmax - dbase;
		cgrp.cg_ndir = 0;
		cgrp.cg_nffree = 0;
		cgrp.cg_nbfree = 0;
		cgrp.cg_nifree = 0;
		cgrp.cg_rotor = dmin;
		cgrp.cg_frotor = dmin;
		cgrp.cg_irotor = 0;
		for (i = 0; i < FRAG; i++)
			cgrp.cg_frsum[i] = 0;
		inum = sblock.fs_ipg * c;
		for (i = 0; i < sblock.fs_ipg; inum++, i++) {
			dp = ginode();
			if (dp == NULL)
				continue;
			if (ALLOC) {
				if (DIR)
					cgrp.cg_ndir++;
				setbit(cgrp.cg_iused, i);
				continue;
			}
			cgrp.cg_nifree++;
			clrbit(cgrp.cg_iused, i);
		}
		while (i < MAXIPG) {
			clrbit(cgrp.cg_iused, i);
			i++;
		}
		for (s = 0; s < MAXCPG; s++)
			for (i = 0; i < NRPOS; i++)
				cgrp.cg_b[s][i] = 0;
		if (c == 0) {
			dmin += howmany(cssize(&sblock), BSIZE) * FRAG;
		}
		for (d = 0; d < dmin; d++)
			clrbit(cgrp.cg_free, d);
		for (; (d + FRAG) <= dmax - dbase; d += FRAG) {
			j = 0;
			for (i = 0; i < FRAG; i++) {
				if (!getbmap(dbase+d+i)) {
					setbit(cgrp.cg_free, d+i);
					j++;
				} else
					clrbit(cgrp.cg_free, d+i);
			}
			if (j == FRAG) {
				cgrp.cg_nbfree++;
				s = d * NSPF;
				cgrp.cg_b[s/sblock.fs_spc]
				  [s%sblock.fs_nsect*NRPOS/sblock.fs_nsect]++;
			} else if (j > 0) {
				cgrp.cg_nffree += j;
				blk = ((cgrp.cg_free[d / NBBY] >> (d % NBBY)) &
				       (0xff >> (NBBY - FRAG)));
				fragacct(blk, cgrp.cg_frsum, 1);
			}
		}
		for (j = d; d < dmax - dbase; d++) {
			if (!getbmap(dbase+d)) {
				setbit(cgrp.cg_free, d);
				cgrp.cg_nffree++;
			} else
				clrbit(cgrp.cg_free, d);
		}
		if (j != d) {
			blk = ((cgrp.cg_free[j / NBBY] >> (j % NBBY)) &
			       (0xff >> (NBBY - FRAG)));
			fragacct(blk, cgrp.cg_frsum, 1);
		}
		for (; d < MAXBPG; d++)
			clrbit(cgrp.cg_free, d);
		sblock.fs_nffree += cgrp.cg_nffree;
		sblock.fs_nbfree += cgrp.cg_nbfree;
		cs->cs_ndir = cgrp.cg_ndir;
		cs->cs_nifree = cgrp.cg_nifree;
		cs->cs_nbfree = cgrp.cg_nbfree;
		bwrite(&dfile, &cgrp, cgtod(c, &sblock), sblock.fs_cgsize);
	}
	sblock.fs_ronly = 0;
	sblock.fs_fmod = 0;
	sbdirty();
}

/*
 * update the frsum fields to reflect addition or deletion 
 * of some frags
 */
fragacct(fragmap, fraglist, cnt)
	int fragmap;
	short fraglist[];
	int cnt;
{
	int inblk;
	register int field, subfield;
	register int siz, pos;

	inblk = (int)(fragtbl[fragmap] << 1);
	fragmap <<= 1;
	for (siz = 1; siz < FRAG; siz++) {
		if (((1 << siz) & inblk) == 0)
			continue;
		field = around[siz];
		subfield = inside[siz];
		for (pos = siz; pos <= FRAG; pos++) {
			if ((fragmap & field) == subfield) {
				fraglist[siz] += cnt;
				pos += siz;
				field <<= siz;
				subfield <<= siz;
			}
			field <<= 1;
			subfield <<= 1;
		}
	}
}

findino(dirp)
	register DIRECT *dirp;
{
	register char *p1, *p2;

	if (dirp->d_ino == 0)
		return (KEEPON);
	for (p1 = dirp->d_name, p2 = srchname;*p2++ == *p1; p1++) {
		if (*p1 == 0 || p1 == &dirp->d_name[DIRSIZ-1]) {
			if (dirp->d_ino >= ROOTINO && dirp->d_ino <= imax)
				parentdir = dirp->d_ino;
			return (STOP);
		}
	}
	return (KEEPON);
}

mkentry(dirp)
	register DIRECT *dirp;
{
	register ino_t in;
	register char *p;

	if (dirp->d_ino)
		return (KEEPON);
	dirp->d_ino = orphan;
	in = orphan;
	p = &dirp->d_name[8];
	*--p = 0;
	while (p > dirp->d_name) {
		*--p = (in % 10) + '0';
		in /= 10;
	}
	*p = '#';
	return (ALTERD|STOP);
}

chgdd(dirp)
	register DIRECT *dirp;
{
	if (dirp->d_name[0] == '.' && dirp->d_name[1] == '.' &&
	dirp->d_name[2] == 0) {
		dirp->d_ino = lfdir;
		return (ALTERD|STOP);
	}
	return (KEEPON);
}

linkup()
{
	register DINODE *dp;
	register lostdir;
	register ino_t pdir;

	if ((dp = ginode()) == NULL)
		return (0);
	lostdir = DIR;
	pdir = parentdir;
	pwarn("UNREF %s ", lostdir ? "DIR" : "FILE");
	pinode();
	if (preen && dp->di_size == 0)
		return (0);
	if (preen)
		printf(" (RECONNECTED)\n");
	else
		if (reply("RECONNECT") == 0)
			return (0);
	orphan = inum;
	if (lfdir == 0) {
		inum = ROOTINO;
		if ((dp = ginode()) == NULL) {
			inum = orphan;
			return (0);
		}
		pfunc = findino;
		srchname = lfname;
		filsize = dp->di_size;
		parentdir = 0;
		ckinode(dp, DATA);
		inum = orphan;
		if ((lfdir = parentdir) == 0) {
			pfatal("SORRY. NO lost+found DIRECTORY");
			printf("\n\n");
			return (0);
		}
	}
	inum = lfdir;
	if ((dp = ginode()) == NULL || !DIR || getstate() != FSTATE) {
		inum = orphan;
		pfatal("SORRY. NO lost+found DIRECTORY");
		printf("\n\n");
		return (0);
	}
	if (dp->di_size & BMASK) {
		dp->di_size = roundup(dp->di_size, BSIZE);
		inodirty();
	}
	filsize = dp->di_size;
	inum = orphan;
	pfunc = mkentry;
	if ((ckinode(dp, DATA) & ALTERD) == 0) {
		pfatal("SORRY. NO SPACE IN lost+found DIRECTORY");
		printf("\n\n");
		return (0);
	}
	declncnt();
	if (lostdir) {
		pfunc = chgdd;
		dp = ginode();
		filsize = dp->di_size;
		ckinode(dp, DATA);
		inum = lfdir;
		if ((dp = ginode()) != NULL) {
			dp->di_nlink++;
			inodirty();
			setlncnt(getlncnt()+1);
		}
		inum = orphan;
		pwarn("DIR I=%u CONNECTED. ", orphan);
		printf("PARENT WAS I=%u\n", pdir);
		if (preen == 0)
			printf("\n");
	}
	return (1);
}

bread(fcp, buf, blk, size)
	daddr_t blk;
	register struct filecntl *fcp;
	register size;
	char *buf;
{
	if (lseek(fcp->rfdes, blk*FSIZE, 0) < 0)
		rwerr("SEEK", blk);
	else if (read(fcp->rfdes, buf, size) == size)
		return (1);
	rwerr("READ", blk);
	return (0);
}

bwrite(fcp, buf, blk, size)
	daddr_t blk;
	register struct filecntl *fcp;
	register size;
	char *buf;
{

	if (fcp->wfdes < 0)
		return (0);
	if (lseek(fcp->wfdes, blk*FSIZE, 0) < 0)
		rwerr("SEEK", blk);
	else if (write(fcp->wfdes, buf, size) == size) {
		fcp->mod = 1;
		return (1);
	}
	rwerr("WRITE", blk);
	return (0);
}

catch()
{

	ckfini();
	exit(12);
}
