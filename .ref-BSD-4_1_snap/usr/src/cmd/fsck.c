static	char *sccsid = "@(#)fsck.c	4.13 (Berkeley) 81/03/09";
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/filsys.h>
#include <sys/dir.h>
#include <sys/fblk.h>
#include <sys/ino.h>
#include <sys/inode.h>
#include <sys/stat.h>
#include <fstab.h>

typedef	int	(*SIG_TYP)();

#define NDIRECT	(BSIZE/sizeof(struct direct))
#define SPERB	(BSIZE/sizeof(short))

#define NO	0
#define YES	1

#define	MAXDUP	10		/* limit on dup blks (per inode) */
#define	MAXBAD	10		/* limit on bad blks (per inode) */

#define STEPSIZE	9	/* default step for freelist spacing */
#define CYLSIZE		400	/* default cyl size for spacing */
#define MAXCYL		500	/* maximum cylinder size */

#define BITSPB	8		/* number bits per byte */
#define BITSHIFT	3	/* log2(BITSPB) */
#define BITMASK	07		/* BITSPB-1 */
#define LSTATE	2		/* bits per inode state */
#define STATEPB	(BITSPB/LSTATE)	/* inode states per byte */
#define USTATE	0		/* inode not allocated */
#define FSTATE	01		/* inode is file */
#define DSTATE	02		/* inode is directory */
#define CLEAR	03		/* inode is to be cleared */
#define SMASK	03		/* mask for inode state */

typedef struct dinode	DINODE;
typedef struct direct	DIRECT;

#define ALLOC	((dp->di_mode & IFMT) != 0)
#define DIR	((dp->di_mode & IFMT) == IFDIR)
#define REG	((dp->di_mode & IFMT) == IFREG)
#define BLK	((dp->di_mode & IFMT) == IFBLK)
#define CHR	((dp->di_mode & IFMT) == IFCHR)
#define MPC	((dp->di_mode & IFMT) == IFMPC)
#define MPB	((dp->di_mode & IFMT) == IFMPB)
#define SPECIAL	(BLK || CHR || MPC || MPB)

#define NINOBLK	11		/* num blks for raw reading */
#define MAXRAW	110		/* largest raw read (in blks) */
daddr_t	startib;		/* blk num of first in raw area */
unsigned niblk;			/* num of blks in raw area */

struct bufarea {
	struct bufarea	*b_next;		/* must be first */
	daddr_t	b_bno;
	union {
		char	b_buf[BSIZE];		/* buffer space */
		short	b_lnks[SPERB];		/* link counts */
		daddr_t	b_indir[NINDIR];	/* indirect block */
		struct filsys b_fs;		/* super block */
		struct fblk b_fb;		/* free block */
		struct dinode b_dinode[INOPB];	/* inode block */
		DIRECT b_dir[NDIRECT];		/* directory */
	} b_un;
	char	b_dirty;
};

typedef struct bufarea BUFAREA;

BUFAREA	inoblk;			/* inode blocks */
BUFAREA	fileblk;		/* other blks in filesys */
BUFAREA	sblk;			/* file system superblock */
BUFAREA	*poolhead;		/* ptr to first buffer in pool */

#define initbarea(x)	(x)->b_dirty = 0;(x)->b_bno = (daddr_t)-1
#define dirty(x)	(x)->b_dirty = 1
#define inodirty()	inoblk.b_dirty = 1
#define fbdirty()	fileblk.b_dirty = 1
#define sbdirty()	sblk.b_dirty = 1

#define freeblk		fileblk.b_un.b_fb
#define dirblk		fileblk.b_un
#define superblk	sblk.b_un.b_fs

struct filecntl {
	int	rfdes;
	int	wfdes;
	int	mod;
};

struct filecntl	dfile;		/* file descriptors for filesys */
struct filecntl	sfile;		/* file descriptors for scratch file */

typedef unsigned MEMSIZE;

MEMSIZE	memsize;		/* amt of memory we got */
#ifdef pdp11
#define MAXDATA	((MEMSIZE)54*1024)
#endif
#ifdef vax
#define	MAXDATA ((MEMSIZE)400*1024)
#endif

#define	DUPTBLSIZE	100	/* num of dup blocks to remember */
daddr_t	duplist[DUPTBLSIZE];	/* dup block table */
daddr_t	*enddup;		/* next entry in dup table */
daddr_t	*muldup;		/* multiple dups part of table */

#define MAXLNCNT	50	/* num zero link cnts to remember */
ino_t	badlncnt[MAXLNCNT];	/* table of inos with zero link cnts */
ino_t	*badlnp;		/* next entry in table */

char	sflag;			/* salvage free block list */
char	csflag;			/* salvage free block list (conditional) */
char	nflag;			/* assume a no response */
char	yflag;			/* assume a yes response */
char	tflag;			/* scratch file specified */
char	preen;			/* just fix normal inconsistencies */
char	rplyflag;		/* any questions asked? */
char	hotroot;		/* checking root device */
char	rawflg;			/* read raw device */
char	rmscr;			/* remove scratch file when done */
char	fixfree;		/* corrupted free list */
char	*membase;		/* base of memory we get */
char	*blkmap;		/* ptr to primary blk allocation map */
char	*freemap;		/* ptr to secondary blk allocation map */
char	*statemap;		/* ptr to inode state table */
char	*pathp;			/* pointer to pathname position */
char	*thisname;		/* ptr to current pathname component */
char	*srchname;		/* name being searched for in dir */
char	pathname[200];
char	scrfile[80];
char	*lfname =	"lost+found";
char	*checklist =	FSTAB;

short	*lncntp;		/* ptr to link count table */

int	cylsize;		/* num blocks per cylinder */
int	stepsize;		/* num blocks for spacing purposes */
int	badblk;			/* num of bad blks seen (per inode) */
int	dupblk;			/* num of dup blks seen (per inode) */
int	(*pfunc)();		/* function to call to chk blk */

ino_t	inum;			/* inode we are currently working on */
ino_t	imax;			/* number of inodes */
ino_t	parentdir;		/* i number of parent directory */
ino_t	lastino;		/* hiwater mark of inodes */
ino_t	lfdir;			/* lost & found directory */
ino_t	orphan;			/* orphaned inode */

off_t	filsize;		/* num blks seen in file */
off_t	maxblk;			/* largest logical blk in file */
off_t	bmapsz;			/* num chars in blkmap */

daddr_t	smapblk;		/* starting blk of state map */
daddr_t	lncntblk;		/* starting blk of link cnt table */
daddr_t	fmapblk;		/* starting blk of free map */
daddr_t	n_free;			/* number of free blocks */
daddr_t	n_blks;			/* number of blocks used */
daddr_t	n_files;		/* number of files seen */
daddr_t	fmin;			/* block number of the first data block */
daddr_t	fmax;			/* number of blocks in the volume */

#define howmany(x,y)	(((x)+((y)-1))/(y))
#define roundup(x,y)	((((x)+((y)-1))/(y))*(y))
#define outrange(x)	(x < fmin || x >= fmax)
#define zapino(x)	clear((char *)(x),sizeof(DINODE))

#define setlncnt(x)	dolncnt(x,0)
#define getlncnt()	dolncnt(0,1)
#define declncnt()	dolncnt(0,2)

#define setbmap(x)	domap(x,0)
#define getbmap(x)	domap(x,1)
#define clrbmap(x)	domap(x,2)

#define setfmap(x)	domap(x,0+4)
#define getfmap(x)	domap(x,1+4)
#define clrfmap(x)	domap(x,2+4)

#define setstate(x)	dostate(x,0)
#define getstate()	dostate(0,1)

#define DATA	1
#define ADDR	0
#define ALTERD	010
#define KEEPON	04
#define SKIP	02
#define STOP	01

int	(*signal())();
long	lseek();
long	time();
DINODE	*ginode();
BUFAREA	*getblk();
BUFAREA	*search();
int	dirscan();
int	findino();
int	catch();
int	mkentry();
int	chgdd();
int	pass1();
int	pass1b();
int	pass2();
int	pass3();
int	pass4();
int	pass5();

char	*devname;

main(argc,argv)
int	argc;
char	*argv[];
{
	register FILE *fp;
	register n;
	register char *p;
	char filename[50];
	char *sbrk();

	sync();
	while(--argc > 0 && **++argv == '-') {
		switch(*++*argv) {
			case 'p':
				preen++;
				break;
			case 't':
			case 'T':
				tflag++;
				if(**++argv == '-' || --argc <= 0)
					errexit("Bad -t option\n");
				p = scrfile;
				while(*p++ = **argv)
					(*argv)++;
				break;
			case 's':	/* salvage flag */
				stype(++*argv);
				sflag++;
				break;
			case 'S':	/* conditional salvage */
				stype(++*argv);
				csflag++;
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
				errexit("%c option?\n",**argv);
		}
	}
	if(nflag && (sflag || csflag))
		errexit("Incompatible options: -n and -%s\n",sflag?"s":"S");
	if(sflag && csflag)
		sflag = 0;
	memsize = (MEMSIZE)sbrk(0);
	memsize = MAXDATA - memsize - sizeof(int);
	while(memsize >= 2*sizeof(BUFAREA) &&
		(membase = sbrk(memsize)) == (char *)-1)
		memsize -= 1024;
	if(memsize < 2*sizeof(BUFAREA))
		errexit("Can't get memory\n");
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, catch);
	if(argc) {		/* arg list has file names */
		while(argc-- > 0){
			hotroot = 0;
			check(*argv++);
		}
	}
	else {			/* use default checklist */
		struct fstab *fsp;
		int pid, passno, anygtr, sumstatus = 0;
		passno = 1;
		do {
			anygtr = 0;
			if (setfsent() == 0)
				errexit("Can't open checklist file: %s\n",
					FSTAB);
			while ( (fsp = getfsent()) != 0){
				if (strcmp(fsp->fs_type, FSTAB_RW) &&
				    strcmp(fsp->fs_type, FSTAB_RO))
					continue;
				if (preen == 0 ||
				    passno == 1 && fsp->fs_passno == passno) {
					if (blockcheck(fsp->fs_spec) == NO &&
					    preen)
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
						if (blockcheck(fsp->fs_spec)==NO)
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
	}
	exit(0);
}

char *rawname(), *rindex(), *unrawname();

blockcheck(name)
	char	*name;
{
	struct	stat	stat_slash, stat_block, stat_char;
	char	*raw;
	int	looped = 0;

	hotroot = 0;
	if (stat("/", &stat_slash) < 0){
		error("Can't stat root\n");
		return(NO);
	}
  retry:
	if (stat(name, &stat_block) < 0){
		error("Can't stat %s\n", name);
		return(NO);
	}
	if (stat_block.st_mode & S_IFBLK){
		raw = rawname(name);
		if (stat(raw, &stat_char) < 0){
			error("Can't stat %s\n", raw);
			return(NO);
		}
		if (stat_char.st_mode & S_IFCHR){
			if (stat_slash.st_dev == stat_block.st_rdev) {
				hotroot++;
				raw = unrawname(name);
			}
			check(raw);
			return(YES);
		} else {
			error("%s is not a character device\n", raw);
			return(NO);
		}
	} else
	if (stat_block.st_mode & S_IFCHR){
		if (looped) {
			error("Can't make sense out of name %s\n", name);
			return(NO);
		}
		name = unrawname(name);
		looped++;
		goto retry;
	}
	error("Can't make sense out of name %s\n", name);
	return(NO);
}

char *
unrawname(cp)
	char	*cp;
{
	char	*dp = rindex(cp, '/');
	struct stat stb;
	if (dp == 0)
		return(cp);
	if (stat(cp, &stb) < 0)
		return(cp);
	if ((stb.st_mode&S_IFMT) != S_IFCHR)
		return(cp);
	if (*(dp+1) != 'r')
		return(cp);
	strcpy(dp+1, dp+2);
	return(cp);
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

	devname = dev;
	check1(dev);
	devname = 0;
}

check1(dev)
char *dev;
{
	register DINODE *dp;
	register n;
	register ino_t *blp;
	ino_t savino;
	daddr_t blk;
	BUFAREA *bp1, *bp2;

	if(setup(dev) == NO)
		return;
	if (preen==0) {
		printf("** Checking %s\n", dev);
		printf("** Phase 1 - Check Blocks and Sizes\n");
	}
	pfunc = pass1;
	for(inum = 1; inum <= imax; inum++) {
		if((dp = ginode()) == NULL)
			continue;
		if(ALLOC) {
			lastino = inum;
			if(ftypeok(dp) == NO) {
				pfatal("UNKNOWN FILE TYPE I=%u",inum);
				if(reply("CLEAR") == YES) {
					zapino(dp);
					inodirty();
				}
				continue;
			}
			n_files++;
			if(setlncnt(dp->di_nlink) <= 0) {
				if(badlnp < &badlncnt[MAXLNCNT])
					*badlnp++ = inum;
				else {
					pfatal("LINK COUNT TABLE OVERFLOW");
					if(reply("CONTINUE") == NO)
						errexit("");
				}
			}
			setstate(DIR ? DSTATE : FSTATE);
			badblk = dupblk = 0;
			filsize = 0;
			maxblk = 0;
			ckinode(dp,ADDR);
			if((n = getstate()) == DSTATE || n == FSTATE)
				sizechk(dp);
		}
		else if(dp->di_mode != 0) {
			pfatal("PARTIALLY ALLOCATED INODE I=%u",inum);
			if(reply("CLEAR") == YES) {
				zapino(dp);
				inodirty();
			}
		}
	}


	if(enddup != &duplist[0]) {
		if (preen)
			pfatal("INTERNAL ERROR: dups with -p");
		printf("** Phase 1b - Rescan For More DUPS\n");
		pfunc = pass1b;
		for(inum = 1; inum <= lastino; inum++) {
			if(getstate() != USTATE && (dp = ginode()) != NULL)
				if(ckinode(dp,ADDR) & STOP)
					break;
		}
	}
	if(rawflg) {
		if(inoblk.b_dirty)
			bwrite(&dfile,membase,startib,(int)niblk*BSIZE);
		inoblk.b_dirty = 0;
		if(poolhead) {
			clear(membase,niblk*BSIZE);
			for(bp1 = poolhead;bp1->b_next;bp1 = bp1->b_next);
			bp2 = &((BUFAREA *)membase)[(niblk*BSIZE)/sizeof(BUFAREA)];
			while(--bp2 >= (BUFAREA *)membase) {
				initbarea(bp2);
				bp2->b_next = bp1->b_next;
				bp1->b_next = bp2;
			}
		}
		rawflg = 0;

	}


	if (preen == 0)
		printf("** Phase 2 - Check Pathnames\n");
	inum = ROOTINO;
	thisname = pathp = pathname;
	pfunc = pass2;
	switch(getstate()) {
		case USTATE:
			errexit("ROOT INODE UNALLOCATED. TERMINATING.\n");
		case FSTATE:
			pfatal("ROOT INODE NOT DIRECTORY");
			if(reply("FIX") == NO || (dp = ginode()) == NULL)
				errexit("");
			dp->di_mode &= ~IFMT;
			dp->di_mode |= IFDIR;
			inodirty();
			setstate(DSTATE);
		case DSTATE:
			descend();
			break;
		case CLEAR:
			pfatal("DUPS/BAD IN ROOT INODE");
			printf("\n");
			if(reply("CONTINUE") == NO)
				errexit("");
			setstate(DSTATE);
			descend();
	}


	if (preen == 0)
		printf("** Phase 3 - Check Connectivity\n");
	for(inum = ROOTINO; inum <= lastino; inum++) {
		if(getstate() == DSTATE) {
			pfunc = findino;
			srchname = "..";
			savino = inum;
			do {
				orphan = inum;
				if((dp = ginode()) == NULL)
					break;
				filsize = dp->di_size;
				parentdir = 0;
				ckinode(dp,DATA);
				if((inum = parentdir) == 0)
					break;
			} while(getstate() == DSTATE);
			inum = orphan;
			if(linkup() == YES) {
				thisname = pathp = pathname;
				*pathp++ = '?';
				pfunc = pass2;
				descend();
			}
			inum = savino;
		}
	}


	if (preen == 0)
		printf("** Phase 4 - Check Reference Counts\n");
	pfunc = pass4;
	for(inum = ROOTINO; inum <= lastino; inum++) {
		switch(getstate()) {
			case FSTATE:
				if(n = getlncnt())
					adjust((short)n);
				else {
					for(blp = badlncnt;blp < badlnp; blp++)
						if(*blp == inum) {
							clri("UNREF",YES);
							break;
						}
				}
				break;
			case DSTATE:
				clri("UNREF",YES);
				break;
			case CLEAR:
				clri("BAD/DUP",YES);
		}
	}
	if(imax - n_files != superblk.s_tinode) {
		pwarn("FREE INODE COUNT WRONG IN SUPERBLK");
		if (preen)
			printf(" (FIXED)\n");
		if (preen || reply("FIX") == YES) {
			superblk.s_tinode = imax - n_files;
			sbdirty();
		}
	}
	flush(&dfile,&fileblk);


	if (preen == 0)
		printf("** Phase 5 - Check Free List ");
	if(sflag || (csflag && rplyflag == 0)) {
		if (preen == 0)
			printf("(Ignored)\n");
		fixfree = 1;
	}
	else {
		if (preen == 0)
			printf("\n");
		if(freemap)
			copy(blkmap,freemap,(MEMSIZE)bmapsz);
		else {
			for(blk = 0; blk < fmapblk; blk++) {
				bp1 = getblk((BUFAREA *)NULL,blk);
				bp2 = getblk((BUFAREA *)NULL,blk+fmapblk);
				copy(bp1->b_un.b_buf,bp2->b_un.b_buf,BSIZE);
				dirty(bp2);
			}
		}
		badblk = dupblk = 0;
		freeblk.df_nfree = superblk.s_nfree;
		for(n = 0; n < NICFREE; n++)
			freeblk.df_free[n] = superblk.s_free[n];
		freechk();
		if(badblk) {
			pfatal("%d BAD BLKS IN FREE LIST",badblk);
			printf("\n");
		}
		if(dupblk)
			pwarn("%d DUP BLKS IN FREE LIST\n",dupblk);
		if(fixfree == 0) {
			if((n_blks+n_free) != (fmax-fmin)) {
				pwarn("%ld BLK(S) MISSING\n",
					fmax-fmin-n_blks-n_free);
				fixfree = 1;
			}
			else if(n_free != superblk.s_tfree) {
				pwarn("FREE BLK COUNT WRONG IN SUPERBLK");
				if (preen)
					printf(" (FIXED)\n");
				if(preen || reply("FIX") == YES) {
					superblk.s_tfree = n_free;
					sbdirty();
				}
			}
		}
		if(fixfree) {
			pwarn("BAD FREE LIST");
			if (preen)
				printf(" (SALVAGED)\n");
			else if(reply("SALVAGE") == NO)
				fixfree = 0;
		}
	}


	if(fixfree) {
		if (preen == 0)
			printf("** Phase 6 - Salvage Free List\n");
		makefree();
		n_free = superblk.s_tfree;
	}


	pwarn("%ld files %ld blocks %ld free\n", n_files,n_blks,n_free);
	if(dfile.mod) {
		time(&superblk.s_time);
		sbdirty();
	}
	ckfini();
	sync();
	if(dfile.mod && preen == 0)
		printf("\n***** FILE SYSTEM WAS MODIFIED *****\n");
}

/* VARARGS1 */
error(s1,s2,s3,s4)
char *s1;
{
	printf(s1,s2,s3,s4);
}

/* VARARGS1 */
errexit(s1,s2,s3,s4)
char *s1;
{
	error(s1,s2,s3,s4);
	exit(8);
}

/*
 * Pfatal is called when an inconsistency occurs
 * which should not happen during normal operations.
 * It prints a message and then dies.
 * When not preening, this is just a printf.
 */
pfatal(s,a1,a2,a3)
{

	if (preen) {
		printf("%s: ", devname);
		printf(s, a1, a2, a3);
		printf("\n");
		preendie();
	}
	printf(s, a1, a2, a3);
}

/*
 * Fatal is called to terminate preening
 * due to unexplainable inconsistency.
 */
preendie()
{

	printf("%s: UNEXPECTED INCONSISTENCY; RUN fsck MANUALLY.\n", devname);
	exit(8);
}

/*
 * Pwarn is like printf when not preening,
 * or a warning (preceded by filename) when preening.
 */
pwarn(s,a1,a2,a3,a4,a5,a6)
{

	if (preen)
		printf("%s: ", devname);
	printf(s, a1, a2, a3, a4, a5, a6);
}

ckinode(dp,flg)
DINODE *dp;
register flg;
{
	register daddr_t *ap;
	register ret;
	int (*func)(), n;
	daddr_t	iaddrs[NADDR];

	if(SPECIAL)
		return(KEEPON);
	l3tol(iaddrs,dp->di_addr,NADDR);
	func = (flg == ADDR) ? pfunc : dirscan;
	for(ap = iaddrs; ap < &iaddrs[NADDR-3]; ap++) {
		if(*ap && (ret = (*func)(*ap)) & STOP)
			return(ret);
	}
	for(n = 1; n < 4; n++) {
		if(*ap && (ret = iblock(*ap,n,flg)) & STOP)
			return(ret);
		ap++;
	}
	return(KEEPON);
}


iblock(blk,ilevel,flg)
daddr_t blk;
register ilevel;
{
	register daddr_t *ap;
	register n;
	int (*func)();
	BUFAREA ib;

	if(flg == ADDR) {
		func = pfunc;
		if(((n = (*func)(blk)) & KEEPON) == 0)
			return(n);
	}
	else
		func = dirscan;
	if(outrange(blk))		/* protect thyself */
		return(SKIP);
	initbarea(&ib);
	if(getblk(&ib,blk) == NULL)
		return(SKIP);
	ilevel--;
	for(ap = ib.b_un.b_indir; ap < &ib.b_un.b_indir[NINDIR]; ap++) {
		if(*ap) {
			if(ilevel > 0) {
				n = iblock(*ap,ilevel,flg);
			}
			else
				n = (*func)(*ap);
			if(n & STOP)
				return(n);
		}
	}
	return(KEEPON);
}


pass1(blk)
daddr_t blk;
{
	register daddr_t *dlp;

	if(outrange(blk)) {
		blkerr("BAD",blk);
		if(++badblk >= MAXBAD) {
			printf("EXCESSIVE BAD BLKS I=%u",inum);
			if(reply("CONTINUE") == NO)
				errexit("");
			return(STOP);
		}
		return(SKIP);
	}
	if(getbmap(blk)) {
		blkerr("DUP",blk);
		if(++dupblk >= MAXDUP) {
			printf("EXCESSIVE DUP BLKS I=%u",inum);
			if(reply("CONTINUE") == NO)
				errexit("");
			return(STOP);
		}
		if(enddup >= &duplist[DUPTBLSIZE]) {
			printf("DUP TABLE OVERFLOW.");
			if(reply("CONTINUE") == NO)
				errexit("");
			return(STOP);
		}
		for(dlp = duplist; dlp < muldup; dlp++) {
			if(*dlp == blk) {
				*enddup++ = blk;
				break;
			}
		}
		if(dlp >= muldup) {
			*enddup++ = *muldup;
			*muldup++ = blk;
		}
	}
	else {
		n_blks++;
		setbmap(blk);
	}
	filsize++;
	return(KEEPON);
}


pass1b(blk)
daddr_t blk;
{
	register daddr_t *dlp;

	if(outrange(blk))
		return(SKIP);
	for(dlp = duplist; dlp < muldup; dlp++) {
		if(*dlp == blk) {
			blkerr("DUP",blk);
			*dlp = *--muldup;
			*muldup = blk;
			return(muldup == duplist ? STOP : KEEPON);
		}
	}
	return(KEEPON);
}


pass2(dirp)
register DIRECT *dirp;
{
	register char *p;
	register n;
	DINODE *dp;

	if((inum = dirp->d_ino) == 0)
		return(KEEPON);
	thisname = pathp;
	for(p = dirp->d_name; p < &dirp->d_name[DIRSIZ]; )
		if((*pathp++ = *p++) == 0) {
			--pathp;
			break;
		}
	*pathp = 0;
	n = NO;
	if(inum > imax || inum < ROOTINO)
		n = direrr("I OUT OF RANGE");
	else {
	again:
		switch(getstate()) {
			case USTATE:
				n = direrr("UNALLOCATED");
				break;
			case CLEAR:
				if((n = direrr("DUP/BAD")) == YES)
					break;
				if((dp = ginode()) == NULL)
					break;
				setstate(DIR ? DSTATE : FSTATE);
				goto again;
			case FSTATE:
				declncnt();
				break;
			case DSTATE:
				declncnt();
				descend();
		}
	}
	pathp = thisname;
	if(n == NO)
		return(KEEPON);
	dirp->d_ino = 0;
	return(KEEPON|ALTERD);
}


pass4(blk)
daddr_t blk;
{
	register daddr_t *dlp;

	if(outrange(blk))
		return(SKIP);
	if(getbmap(blk)) {
		for(dlp = duplist; dlp < enddup; dlp++)
			if(*dlp == blk) {
				*dlp = *--enddup;
				return(KEEPON);
			}
		clrbmap(blk);
		n_blks--;
	}
	return(KEEPON);
}


pass5(blk)
daddr_t blk;
{
	if(outrange(blk)) {
		fixfree = 1;
		if (preen)
			pfatal("BAD BLOCKS IN FREE LIST.");
		if(++badblk >= MAXBAD) {
			printf("EXCESSIVE BAD BLKS IN FREE LIST.");
			if(reply("CONTINUE") == NO)
				errexit("");
			return(STOP);
		}
		return(SKIP);
	}
	if(getfmap(blk)) {
		fixfree = 1;
		if(++dupblk >= DUPTBLSIZE) {
			printf("EXCESSIVE DUP BLKS IN FREE LIST.");
			if(reply("CONTINUE") == NO)
				errexit("");
			return(STOP);
		}
	}
	else {
		n_free++;
		setfmap(blk);
	}
	return(KEEPON);
}


blkerr(s,blk)
daddr_t blk;
char *s;
{
	pfatal("%ld %s I=%u",blk,s,inum);
	printf("\n");
	setstate(CLEAR);	/* mark for possible clearing */
}


descend()
{
	register DINODE *dp;
	register char *savname;
	off_t savsize;

	setstate(FSTATE);
	if((dp = ginode()) == NULL)
		return;
	savname = thisname;
	*pathp++ = '/';
	savsize = filsize;
	filsize = dp->di_size;
	ckinode(dp,DATA);
	thisname = savname;
	*--pathp = 0;
	filsize = savsize;
}


dirscan(blk)
daddr_t blk;
{
	register DIRECT *dirp;
	register char *p1, *p2;
	register n;
	DIRECT direntry;

	if(outrange(blk)) {
		filsize -= BSIZE;
		return(SKIP);
	}
	for(dirp = dirblk.b_dir; dirp < &dirblk.b_dir[NDIRECT] &&
		filsize > 0; dirp++, filsize -= sizeof(DIRECT)) {
		if(getblk(&fileblk,blk) == NULL) {
			filsize -= (&dirblk.b_dir[NDIRECT]-dirp)*sizeof(DIRECT);
			return(SKIP);
		}
		p1 = &dirp->d_name[DIRSIZ];
		p2 = &direntry.d_name[DIRSIZ];
		while(p1 > (char *)dirp)
			*--p2 = *--p1;
		if((n = (*pfunc)(&direntry)) & ALTERD) {
			if(getblk(&fileblk,blk) != NULL) {
				p1 = &dirp->d_name[DIRSIZ];
				p2 = &direntry.d_name[DIRSIZ];
				while(p1 > (char *)dirp)
					*--p1 = *--p2;
				fbdirty();
			}
			else
				n &= ~ALTERD;
		}
		if(n & STOP)
			return(n);
	}
	return(filsize > 0 ? KEEPON : STOP);
}


direrr(s)
char *s;
{
	register DINODE *dp;

	pwarn("%s ",s);
	pinode();
	printf("\n");
	if((dp = ginode()) != NULL && ftypeok(dp))
		pfatal("%s=%s",DIR?"DIR":"FILE",pathname);
	else
		pfatal("NAME=%s",pathname);
	return(reply("REMOVE"));
}


adjust(lcnt)
register short lcnt;
{
	register DINODE *dp;

	if((dp = ginode()) == NULL)
		return;
	if(dp->di_nlink == lcnt) {
		if(linkup() == NO)
			clri("UNREF",NO);
	}
	else {
		pwarn("LINK COUNT %s",
			(lfdir==inum)?lfname:(DIR?"DIR":"FILE"));
		pinode();
		printf(" COUNT %d SHOULD BE %d",
			dp->di_nlink,dp->di_nlink-lcnt);
		if (preen) {
			if (lcnt < 0) {
				printf("\n");
				preendie();
			}
			printf(" (ADJUSTED)\n");
		}
		if(preen || reply("ADJUST") == YES) {
			dp->di_nlink -= lcnt;
			inodirty();
		}
	}
}


clri(s,flg)
char *s;
{
	register DINODE *dp;

	if((dp = ginode()) == NULL)
		return;
	if(flg == YES) {
		pwarn("%s %s",s,DIR?"DIR":"FILE");
		pinode();
	}
	if(preen || reply("CLEAR") == YES) {
		if (preen)
			printf(" (CLEARED)\n");
		n_files--;
		pfunc = pass4;
		ckinode(dp,ADDR);
		zapino(dp);
		inodirty();
	}
}


setup(dev)
char *dev;
{
	register n;
	register BUFAREA *bp;
	register MEMSIZE msize;
	char *mbase;
	daddr_t bcnt, nscrblk;
	dev_t rootdev;
	off_t smapsz, lncntsz, totsz;
	struct stat statarea;

	if(stat("/",&statarea) < 0)
		errexit("Can't stat root\n");
	rootdev = statarea.st_dev;
	if(stat(dev,&statarea) < 0) {
		error("Can't stat %s\n",dev);
		return(NO);
	}
	rawflg = 0;
	if((statarea.st_mode & S_IFMT) == S_IFBLK)
		;
	else if((statarea.st_mode & S_IFMT) == S_IFCHR)
		rawflg++;
	else {
		if (reply("file is not a block or character device; OK") == NO)
			return(NO);
	}
	if(rootdev == statarea.st_rdev)
		hotroot++;
	if((dfile.rfdes = open(dev,0)) < 0) {
		error("Can't open %s\n",dev);
		return(NO);
	}
	if (preen == 0)
		printf("\n%s",dev);
	if(nflag || (dfile.wfdes = open(dev,1)) < 0) {
		dfile.wfdes = -1;
		if (preen)
			pfatal("NO WRITE ACCESS");
		printf(" (NO WRITE)");
	}
	if (preen == 0)
		printf("\n");
	fixfree = 0;
	dfile.mod = 0;
	n_files = n_blks = n_free = 0;
	muldup = enddup = &duplist[0];
	badlnp = &badlncnt[0];
	lfdir = 0;
	rplyflag = 0;
	initbarea(&sblk);
	initbarea(&fileblk);
	initbarea(&inoblk);
	sfile.wfdes = sfile.rfdes = -1;
	rmscr = 0;
	if(getblk(&sblk,SUPERB) == NULL) {
		ckfini();
		return(NO);
	}
	imax = ((ino_t)superblk.s_isize - (SUPERB+1)) * INOPB;
	fmin = (daddr_t)superblk.s_isize;	/* first data blk num */
	fmax = superblk.s_fsize;		/* first invalid blk num */
	if(fmin >= fmax || 
		(imax/INOPB) != ((ino_t)superblk.s_isize-(SUPERB+1))) {
		pfatal("Size check: fsize %ld isize %d",
			superblk.s_fsize,superblk.s_isize);
		printf("\n");
		ckfini();
		return(NO);
	}
	if (preen == 0)
		printf("File System: %.12s\n\n", superblk.s_fsmnt);
	bmapsz = roundup(howmany(fmax,BITSPB),sizeof(*lncntp));
	smapsz = roundup(howmany((long)(imax+1),STATEPB),sizeof(*lncntp));
	lncntsz = (long)(imax+1) * sizeof(*lncntp);
	if(bmapsz > smapsz+lncntsz)
		smapsz = bmapsz-lncntsz;
	totsz = bmapsz+smapsz+lncntsz;
	msize = memsize;
	mbase = membase;
	if(rawflg) {
		if(msize < (MEMSIZE)(NINOBLK*BSIZE) + 2*sizeof(BUFAREA))
			rawflg = 0;
		else {
			msize -= (MEMSIZE)NINOBLK*BSIZE;
			mbase += (MEMSIZE)NINOBLK*BSIZE;
			niblk = NINOBLK;
			startib = fmax;
		}
	}
	clear(mbase,msize);
	if((off_t)msize < totsz) {
		bmapsz = roundup(bmapsz,BSIZE);
		smapsz = roundup(smapsz,BSIZE);
		lncntsz = roundup(lncntsz,BSIZE);
		nscrblk = (bmapsz+smapsz+lncntsz)>>BSHIFT;
		if(tflag == 0) {
			printf("\nNEED SCRATCH FILE (%ld BLKS)\n",nscrblk);
			do {
				printf("ENTER FILENAME:  ");
				if((n = getline(stdin,scrfile,sizeof(scrfile))) == EOF)
					errexit("\n");
			} while(n == 0);
		}
		if(stat(scrfile,&statarea) < 0 ||
			(statarea.st_mode & S_IFMT) == S_IFREG)
			rmscr++;
		if((sfile.wfdes = creat(scrfile,0666)) < 0 ||
			(sfile.rfdes = open(scrfile,0)) < 0) {
			error("Can't create %s\n",scrfile);
			ckfini();
			return(NO);
		}
		bp = &((BUFAREA *)mbase)[(msize/sizeof(BUFAREA))];
		poolhead = NULL;
		while(--bp >= (BUFAREA *)mbase) {
			initbarea(bp);
			bp->b_next = poolhead;
			poolhead = bp;
		}
		bp = poolhead;
		for(bcnt = 0; bcnt < nscrblk; bcnt++) {
			bp->b_bno = bcnt;
			dirty(bp);
			flush(&sfile,bp);
		}
		blkmap = freemap = statemap = (char *) NULL;
		lncntp = (short *) NULL;
		smapblk = bmapsz / BSIZE;
		lncntblk = smapblk + smapsz / BSIZE;
		fmapblk = smapblk;
	}
	else {
		if(rawflg && (off_t)msize > totsz+BSIZE) {
			niblk += (unsigned)((off_t)msize-totsz)>>BSHIFT;
			if(niblk > MAXRAW)
				niblk = MAXRAW;
			msize = memsize - (niblk*BSIZE);
			mbase = membase + (niblk*BSIZE);
		}
		poolhead = NULL;
		blkmap = mbase;
		statemap = &mbase[(MEMSIZE)bmapsz];
		freemap = statemap;
		lncntp = (short *)&statemap[(MEMSIZE)smapsz];
	}
	return(YES);
}


DINODE *
ginode()
{
	register DINODE *dp;
	register char *mbase;
	daddr_t iblk;

	if(inum > imax)
		return(NULL);
	iblk = itod(inum);
	if(rawflg) {
		mbase = membase;
		if(iblk < startib || iblk >= startib+niblk) {
			if(inoblk.b_dirty)
				bwrite(&dfile,mbase,startib,(int)niblk*BSIZE);
			inoblk.b_dirty = 0;
			if(bread(&dfile,mbase,iblk,(int)niblk*BSIZE) == NO) {
				startib = fmax;
				return(NULL);
			}
			startib = iblk;
		}
		dp = (DINODE *)&mbase[(unsigned)((iblk-startib)<<BSHIFT)];
	}
	else if(getblk(&inoblk,iblk) != NULL)
		dp = inoblk.b_un.b_dinode;
	else
		return(NULL);
	return(dp + itoo(inum));
}


ftypeok(dp)
DINODE *dp;
{
	switch(dp->di_mode & IFMT) {
		case IFDIR:
		case IFREG:
		case IFBLK:
		case IFCHR:
		case IFMPC:
		case IFMPB:
			return(YES);
		default:
			return(NO);
	}
}


reply(s)
char *s;
{
	char line[80];

	if (preen)
		pfatal("INTERNAL ERROR: GOT TO reply()");
	rplyflag = 1;
	printf("\n%s? ",s);
	if(nflag || csflag || dfile.wfdes < 0) {
		printf(" no\n\n");
		return(NO);
	}
	if(yflag) {
		printf(" yes\n\n");
		return(YES);
	}
	if(getline(stdin,line,sizeof(line)) == EOF)
		errexit("\n");
	printf("\n");
	if(line[0] == 'y' || line[0] == 'Y')
		return(YES);
	else
		return(NO);
}


getline(fp,loc,maxlen)
FILE *fp;
char *loc;
{
	register n;
	register char *p, *lastloc;

	p = loc;
	lastloc = &p[maxlen-1];
	while((n = getc(fp)) != '\n') {
		if(n == EOF)
			return(EOF);
		if(!isspace(n) && p < lastloc)
			*p++ = n;
	}
	*p = 0;
	return(p - loc);
}


stype(p)
register char *p;
{
	if(*p == 0)
		return;
	if (*(p+1) == 0) {
		if (*p == '3') {
			cylsize = 200;
			stepsize = 5;
			return;
		}
		if (*p == '4') {
			cylsize = 418;
			stepsize = 9;
			return;
		}
	}
	cylsize = atoi(p);
	while(*p && *p != ':')
		p++;
	if(*p)
		p++;
	stepsize = atoi(p);
	if(stepsize <= 0 || stepsize > cylsize ||
	cylsize <= 0 || cylsize > MAXCYL) {
		error("Invalid -s argument, defaults assumed\n");
		cylsize = stepsize = 0;
	}
}


dostate(s,flg)
{
	register char *p;
	register unsigned byte, shift;
	BUFAREA *bp;

	byte = (inum)/STATEPB;
	shift = LSTATE * ((inum)%STATEPB);
	if(statemap != NULL) {
		bp = NULL;
		p = &statemap[byte];
	}
	else if((bp = getblk((BUFAREA *)NULL,(daddr_t)(smapblk+(byte/BSIZE)))) == NULL)
		errexit("Fatal I/O error\n");
	else
		p = &bp->b_un.b_buf[byte%BSIZE];
	switch(flg) {
		case 0:
			*p &= ~(SMASK<<(shift));
			*p |= s<<(shift);
			if(bp != NULL)
				dirty(bp);
			return(s);
		case 1:
			return((*p>>(shift)) & SMASK);
	}
	return(USTATE);
}


domap(blk,flg)
daddr_t blk;
{
	register char *p;
	register unsigned n;
	register BUFAREA *bp;
	off_t byte;

	byte = blk >> BITSHIFT;
	n = 1<<((unsigned)(blk & BITMASK));
	if(flg & 04) {
		p = freemap;
		blk = fmapblk;
	}
	else {
		p = blkmap;
		blk = 0;
	}
	if(p != NULL) {
		bp = NULL;
		p += (unsigned)byte;
	}
	else if((bp = getblk((BUFAREA *)NULL,blk+(byte>>BSHIFT))) == NULL)
		errexit("Fatal I/O error\n");
	else
		p = &bp->b_un.b_buf[(unsigned)(byte&BMASK)];
	switch(flg&03) {
		case 0:
			*p |= n;
			break;
		case 1:
			n &= *p;
			bp = NULL;
			break;
		case 2:
			*p &= ~n;
	}
	if(bp != NULL)
		dirty(bp);
	return(n);
}


dolncnt(val,flg)
short val;
{
	register short *sp;
	register BUFAREA *bp;

	if(lncntp != NULL) {
		bp = NULL;
		sp = &lncntp[inum];
	}
	else if((bp = getblk((BUFAREA *)NULL,(daddr_t)(lncntblk+(inum/SPERB)))) == NULL)
		errexit("Fatal I/O error\n");
	else
		sp = &bp->b_un.b_lnks[inum%SPERB];
	switch(flg) {
		case 0:
			*sp = val;
			break;
		case 1:
			bp = NULL;
			break;
		case 2:
			(*sp)--;
	}
	if(bp != NULL)
		dirty(bp);
	return(*sp);
}


BUFAREA *
getblk(bp,blk)
daddr_t blk;
register BUFAREA *bp;
{
	register struct filecntl *fcp;

	if(bp == NULL) {
		bp = search(blk);
		fcp = &sfile;
	}
	else
		fcp = &dfile;
	if(bp->b_bno == blk)
		return(bp);
	flush(fcp,bp);
	if(bread(fcp,bp->b_un.b_buf,blk,BSIZE) != NO) {
		bp->b_bno = blk;
		return(bp);
	}
	bp->b_bno = (daddr_t)-1;
	return(NULL);
}


flush(fcp,bp)
struct filecntl *fcp;
register BUFAREA *bp;
{
	if(bp->b_dirty) {
		bwrite(fcp,bp->b_un.b_buf,bp->b_bno,BSIZE);
	}
	bp->b_dirty = 0;
}


rwerr(s,blk)
char *s;
daddr_t blk;
{
	if (preen == 0)
		printf("\n");
	pfatal("CAN NOT %s: BLK %ld",s,blk);
	if(reply("CONTINUE") == NO)
		errexit("Program terminated\n");
}


sizechk(dp)
register DINODE *dp;
{
/*
	if (maxblk != howmany(dp->di_size, BSIZE))
		printf("POSSIBLE FILE SIZE ERROR I=%u (%ld,%ld)\n\n",
		    inum, maxblk, howmany(dp->di_size,BSIZE));
*/
	if(DIR && (dp->di_size % sizeof(DIRECT)) != 0) {
		pwarn("DIRECTORY MISALIGNED I=%u\n",inum);
		if (preen == 0)
			printf("\n");
	}
}


ckfini()
{
	flush(&dfile,&fileblk);
	flush(&dfile,&sblk);
	flush(&dfile,&inoblk);
	close(dfile.rfdes);
	close(dfile.wfdes);
	close(sfile.rfdes);
	close(sfile.wfdes);
	if(rmscr) {
		unlink(scrfile);
	}
}


pinode()
{
	register DINODE *dp;
	register char *p;
	char uidbuf[200];
	char *ctime();

	printf(" I=%u ",inum);
	if((dp = ginode()) == NULL)
		return;
	printf(" OWNER=");
	if(getpw((int)dp->di_uid,uidbuf) == 0) {
		for(p = uidbuf; *p != ':'; p++);
		*p = 0;
		printf("%s ",uidbuf);
	}
	else {
		printf("%d ",dp->di_uid);
	}
	printf("MODE=%o\n",dp->di_mode);
	if (preen)
		printf("%s: ", devname);
	printf("SIZE=%ld ",dp->di_size);
	p = ctime(&dp->di_mtime);
	printf("MTIME=%12.12s %4.4s ",p+4,p+20);
}


copy(fp,tp,size)
register char *tp, *fp;
MEMSIZE size;
{
	while(size--)
		*tp++ = *fp++;
}


freechk()
{
	register daddr_t *ap;

	if(freeblk.df_nfree == 0)
		return;
	do {
		if(freeblk.df_nfree <= 0 || freeblk.df_nfree > NICFREE) {
			pfatal("BAD FREEBLK COUNT");
			printf("\n");
			fixfree = 1;
			return;
		}
		ap = &freeblk.df_free[freeblk.df_nfree];
		while(--ap > &freeblk.df_free[0]) {
			if(pass5(*ap) == STOP)
				return;
		}
		if(*ap == (daddr_t)0 || pass5(*ap) != KEEPON)
			return;
	} while(getblk(&fileblk,*ap) != NULL);
}


makefree()
{
	register i, cyl, step;
	int j;
	char flg[MAXCYL];
	short addr[MAXCYL];
	daddr_t blk, baseblk;

	superblk.s_nfree = 0;
	superblk.s_flock = 0;
	superblk.s_fmod = 0;
	superblk.s_tfree = 0;
	superblk.s_ninode = 0;
	superblk.s_ilock = 0;
	superblk.s_ronly = 0;
	if(cylsize == 0 || stepsize == 0) {
		step = superblk.s_dinfo[0];
		cyl = superblk.s_dinfo[1];
	}
	else {
		step = stepsize;
		cyl = cylsize;
	}
	if(step > cyl || step <= 0 || cyl <= 0 || cyl > MAXCYL) {
		error("Default free list spacing assumed\n");
		step = STEPSIZE;
		cyl = CYLSIZE;
	}
	superblk.s_dinfo[0] = step;
	superblk.s_dinfo[1] = cyl;
	clear(flg,sizeof(flg));
	i = 0;
	for(j = 0; j < cyl; j++) {
		while(flg[i])
			i = (i + 1) % cyl;
		addr[j] = i + 1;
		flg[i]++;
		i = (i + step) % cyl;
	}
	baseblk = (daddr_t)roundup(fmax,cyl);
	clear((char *)&freeblk,BSIZE);
	freeblk.df_nfree++;
	for( ; baseblk > 0; baseblk -= cyl)
		for(i = 0; i < cyl; i++) {
			blk = baseblk - addr[i];
			if(!outrange(blk) && !getbmap(blk)) {
				superblk.s_tfree++;
				if(freeblk.df_nfree >= NICFREE) {
					fbdirty();
					fileblk.b_bno = blk;
					flush(&dfile,&fileblk);
					clear((char *)&freeblk,BSIZE);
				}
				freeblk.df_free[freeblk.df_nfree] = blk;
				freeblk.df_nfree++;
			}
		}
	superblk.s_nfree = freeblk.df_nfree;
	for(i = 0; i < NICFREE; i++)
		superblk.s_free[i] = freeblk.df_free[i];
	sbdirty();
}


clear(p,cnt)
register char *p;
MEMSIZE cnt;
{
	while(cnt--)
		*p++ = 0;
}


BUFAREA *
search(blk)
daddr_t blk;
{
	register BUFAREA *pbp, *bp;

	for(bp = (BUFAREA *) &poolhead; bp->b_next; ) {
		pbp = bp;
		bp = pbp->b_next;
		if(bp->b_bno == blk)
			break;
	}
	pbp->b_next = bp->b_next;
	bp->b_next = poolhead;
	poolhead = bp;
	return(bp);
}


findino(dirp)
register DIRECT *dirp;
{
	register char *p1, *p2;

	if(dirp->d_ino == 0)
		return(KEEPON);
	for(p1 = dirp->d_name,p2 = srchname;*p2++ == *p1; p1++) {
		if(*p1 == 0 || p1 == &dirp->d_name[DIRSIZ-1]) {
			if(dirp->d_ino >= ROOTINO && dirp->d_ino <= imax)
				parentdir = dirp->d_ino;
			return(STOP);
		}
	}
	return(KEEPON);
}


mkentry(dirp)
register DIRECT *dirp;
{
	register ino_t in;
	register char *p;

	if(dirp->d_ino)
		return(KEEPON);
	dirp->d_ino = orphan;
	in = orphan;
	p = &dirp->d_name[8];
	*--p = 0;
	while(p > dirp->d_name) {
		*--p = (in % 10) + '0';
		in /= 10;
	}
	*p = '#';
	return(ALTERD|STOP);
}


chgdd(dirp)
register DIRECT *dirp;
{
	if(dirp->d_name[0] == '.' && dirp->d_name[1] == '.' &&
	dirp->d_name[2] == 0) {
		dirp->d_ino = lfdir;
		return(ALTERD|STOP);
	}
	return(KEEPON);
}


linkup()
{
	register DINODE *dp;
	register lostdir;
	register ino_t pdir;

	if((dp = ginode()) == NULL)
		return(NO);
	lostdir = DIR;
	pdir = parentdir;
	pwarn("UNREF %s ",lostdir ? "DIR" : "FILE");
	pinode();
	if (preen && dp->di_size == 0)
		return(NO);
	if (preen)
		printf(" (RECONNECTED)\n");
	else
		if (reply("RECONNECT") == NO)
			return(NO);
	orphan = inum;
	if(lfdir == 0) {
		inum = ROOTINO;
		if((dp = ginode()) == NULL) {
			inum = orphan;
			return(NO);
		}
		pfunc = findino;
		srchname = lfname;
		filsize = dp->di_size;
		parentdir = 0;
		ckinode(dp,DATA);
		inum = orphan;
		if((lfdir = parentdir) == 0) {
			pfatal("SORRY. NO lost+found DIRECTORY");
			printf("\n\n");
			return(NO);
		}
	}
	inum = lfdir;
	if((dp = ginode()) == NULL || !DIR || getstate() != FSTATE) {
		inum = orphan;
		pfatal("SORRY. NO lost+found DIRECTORY");
		printf("\n\n");
		return(NO);
	}
	if(dp->di_size & BMASK) {
		dp->di_size = roundup(dp->di_size,BSIZE);
		inodirty();
	}
	filsize = dp->di_size;
	inum = orphan;
	pfunc = mkentry;
	if((ckinode(dp,DATA) & ALTERD) == 0) {
		pfatal("SORRY. NO SPACE IN lost+found DIRECTORY");
		printf("\n\n");
		return(NO);
	}
	declncnt();
	if(lostdir) {
		pfunc = chgdd;
		dp = ginode();
		filsize = dp->di_size;
		ckinode(dp,DATA);
		inum = lfdir;
		if((dp = ginode()) != NULL) {
			dp->di_nlink++;
			inodirty();
			setlncnt(getlncnt()+1);
		}
		inum = orphan;
		pwarn("DIR I=%u CONNECTED. ",orphan);
		printf("PARENT WAS I=%u\n",pdir);
		if (preen == 0)
			printf("\n");
	}
	return(YES);
}


bread(fcp,buf,blk,size)
daddr_t blk;
register struct filecntl *fcp;
register size;
char *buf;
{
	if(lseek(fcp->rfdes,blk<<BSHIFT,0) < 0)
		rwerr("SEEK",blk);
	else if(read(fcp->rfdes,buf,size) == size)
		return(YES);
	rwerr("READ",blk);
	return(NO);
}


bwrite(fcp,buf,blk,size)
daddr_t blk;
register struct filecntl *fcp;
register size;
char *buf;
{
	if(fcp->wfdes < 0)
		return(NO);
	if(lseek(fcp->wfdes,blk<<BSHIFT,0) < 0)
		rwerr("SEEK",blk);
	else if(write(fcp->wfdes,buf,size) == size) {
		fcp->mod = 1;
		return(YES);
	}
	rwerr("WRITE",blk);
	return(NO);
}

catch()
{
	ckfini();
	exit(12);
}
