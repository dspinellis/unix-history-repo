#ifndef lint
char version[] = "@(#)main.c	2.30 (Berkeley) %G%";
#endif

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/fs.h>
#include <sys/inode.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fstab.h>
#define KERNEL
#include <sys/dir.h>
#undef KERNEL

/* RECONSTRUCT ONLY BAD CG IN PASS 6 */

typedef	int	(*SIG_TYP)();

#define	MAXNINDIR	(MAXBSIZE / sizeof (daddr_t))
#define	MAXINOPB	(MAXBSIZE / sizeof (struct dinode))
#define	SPERB		(MAXBSIZE / sizeof(short))
#define MINDIRSIZE	(sizeof (struct dirtemplate))

#define	MAXDUP	10		/* limit on dup blks (per inode) */
#define	MAXBAD	10		/* limit on bad blks (per inode) */

#define	USTATE	0		/* inode not allocated */
#define	FSTATE	01		/* inode is file */
#define	DSTATE	02		/* inode is directory */
#define	CLEAR	03		/* inode is to be cleared */

typedef struct dinode	DINODE;
typedef struct direct	DIRECT;

#define	ALLOC	((dp->di_mode & IFMT) != 0)
#define	DIRCT	((dp->di_mode & IFMT) == IFDIR)
#define	REG	((dp->di_mode & IFMT) == IFREG)
#define	BLK	((dp->di_mode & IFMT) == IFBLK)
#define	CHR	((dp->di_mode & IFMT) == IFCHR)
#define	LNK	((dp->di_mode & IFMT) == IFLNK)
#define	SOCK	((dp->di_mode & IFMT) == IFSOCK)
#define	BADBLK	((dp->di_mode & IFMT) == IFMT)
#define	SPECIAL	(BLK || CHR)

struct bufarea {
	struct bufarea	*b_next;		/* must be first */
	daddr_t	b_bno;
	int	b_size;
	union {
		char	b_buf[MAXBSIZE];	/* buffer space */
		short	b_lnks[SPERB];		/* link counts */
		daddr_t	b_indir[MAXNINDIR];	/* indirect block */
		struct	fs b_fs;		/* super block */
		struct	cg b_cg;		/* cylinder group */
		struct dinode b_dinode[MAXINOPB]; /* inode block */
	} b_un;
	char	b_dirty;
};

typedef struct bufarea BUFAREA;

BUFAREA	inoblk;			/* inode blocks */
BUFAREA	fileblk;		/* other blks in filesys */
BUFAREA	sblk;			/* file system superblock */
BUFAREA	cgblk;			/* cylinder group blocks */

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

struct inodesc {
	char id_type;		/* type of descriptor, DATA or ADDR */
	int (*id_func)();	/* function to be applied to blocks of inode */
	ino_t id_number;	/* inode number described */
	ino_t id_parent;	/* for DATA nodes, their parent */
	daddr_t id_blkno;	/* current block number being examined */
	int id_numfrags;	/* number of frags contained in block */
	long id_filesize;	/* for DATA nodes, the size of the directory */
	int id_loc;		/* for DATA nodes, current location in dir */
	int id_entryno;		/* for DATA nodes, current entry number */
	DIRECT *id_dirp;	/* for data nodes, ptr to current entry */
	enum {DONTKNOW, NOFIX, FIX} id_fix; /* policy on fixing errors */
};
/* file types */
#define	DATA	1
#define	ADDR	2


#define	DUPTBLSIZE	100	/* num of dup blocks to remember */
daddr_t	duplist[DUPTBLSIZE];	/* dup block table */
daddr_t	*enddup;		/* next entry in dup table */
daddr_t	*muldup;		/* multiple dups part of table */

#define	MAXLNCNT	500	/* num zero link cnts to remember */
ino_t	badlncnt[MAXLNCNT];	/* table of inos with zero link cnts */
ino_t	*badlnp;		/* next entry in table */

char	rawflg;
char	nflag;			/* assume a no response */
char	yflag;			/* assume a yes response */
int	bflag;			/* location of alternate super block */
int	debug;			/* output debugging info */
char	preen;			/* just fix normal inconsistencies */
char	rplyflag;		/* any questions asked? */
char	hotroot;		/* checking root device */
char	fixcg;			/* corrupted free list bit maps */

char	*blockmap;		/* ptr to primary blk allocation map */
char	*freemap;		/* ptr to secondary blk allocation map */
char	*statemap;		/* ptr to inode state table */
short	*lncntp;		/* ptr to link count table */

char	*srchname;		/* name being searched for in dir */
char	pathname[BUFSIZ];	/* current pathname */
char	*pathp;			/* pointer to pathname position */
char	*endpathname = &pathname[BUFSIZ - 2];

char	*lfname = "lost+found";

ino_t	imax;			/* number of inodes */
ino_t	lastino;		/* hiwater mark of inodes */
ino_t	lfdir;			/* lost & found directory */

off_t	maxblk;			/* largest logical blk in file */
off_t	bmapsz;			/* num chars in blockmap */

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
int	sbsumbad;

#define	zapino(x)	(*(x) = zino)
struct	dinode zino;

#define	setbmap(x)	setbit(blockmap, x)
#define	getbmap(x)	isset(blockmap, x)
#define	clrbmap(x)	clrbit(blockmap, x)

#define	setfmap(x)	setbit(freemap, x)
#define	getfmap(x)	isset(freemap, x)
#define	clrfmap(x)	clrbit(freemap, x)

#define	ALTERED	010
#define	KEEPON	04
#define	SKIP	02
#define	STOP	01

int	(*signal())();
long	lseek();
time_t	time();
DINODE	*ginode();
DIRECT	*fsck_readdir();
BUFAREA	*getblk();
int	catch();
int	findino(), mkentry(), chgdd();
int	pass1check(), pass1bcheck(), pass2check(), pass4check();
char	*rawname(), *unrawname();
char	*calloc(), *strcpy(), *strcat(), *rindex();
extern int inside[], around[];
extern unsigned char *fragtbl[];

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
			if (argv[0][1] != '\0') {
				bflag = atoi(argv[0]+1);
			} else {
				bflag = atoi(*++argv);
				argc--;
			}
			printf("Alternate super block location: %d\n", bflag);
			break;

		case 'd':
			debug++;
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
		(void)signal(SIGINT, catch);
	if (argc) {
		while (argc-- > 0) {
			hotroot = 0;
			checkfilesys(*argv++);
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
			    strcmp(fsp->fs_type, FSTAB_RO) &&
			    strcmp(fsp->fs_type, FSTAB_RQ))
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
			union wait status;
			while (wait(&status) != -1)
				sumstatus |= status.w_retcode;
		}
		passno++;
	} while (anygtr);
	if (sumstatus)
		exit(8);
	(void)endfsent();
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
			checkfilesys(raw);
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

checkfilesys(filesys)
	char *filesys;
{

	devname = filesys;
	if (setup(filesys) == 0) {
		if (preen)
			pfatal("CAN'T CHECK FILE SYSTEM.");
		return;
	}
/* 1: scan inodes tallying blocks used */
	if (preen == 0) {
		printf("** Last Mounted on %s\n", sblock.fs_fsmnt);
		if (hotroot)
			printf("** Root file system\n");
		printf("** Phase 1 - Check Blocks and Sizes\n");
	}
	pass1();

/* 1b: locate first references to duplicates, if any */
	if (enddup != &duplist[0]) {
		if (preen)
			pfatal("INTERNAL ERROR: dups with -p");
		printf("** Phase 1b - Rescan For More DUPS\n");
		pass1b();
	}

/* 2: traverse directories from root to mark all connected directories */
	if (preen == 0)
		printf("** Phase 2 - Check Pathnames\n");
	pass2();

/* 3: scan inodes looking for disconnected directories */
	if (preen == 0)
		printf("** Phase 3 - Check Connectivity\n");
	pass3();

/* 4: scan inodes looking for disconnected files; check reference counts */
	if (preen == 0)
		printf("** Phase 4 - Check Reference Counts\n");
	pass4();

/* 5: check resource counts in cylinder groups */
	if (preen == 0)
		printf("** Phase 5 - Check Cyl groups\n");
	pass5();

	if (fixcg) {
		if (preen == 0)
			printf("** Phase 6 - Salvage Cylinder Groups\n");
		makecg();
		n_ffree = sblock.fs_cstotal.cs_nffree;
		n_bfree = sblock.fs_cstotal.cs_nbfree;
	}

	pwarn("%d files, %d used, %d free (%d frags, %d blocks)\n",
	    n_files, n_blks - howmany(sblock.fs_cssize, sblock.fs_fsize),
	    n_ffree + sblock.fs_frag * n_bfree, n_ffree, n_bfree);
	if (dfile.mod) {
		(void)time(&sblock.fs_time);
		sbdirty();
	}
	ckfini();
	free(blockmap);
	free(freemap);
	free(statemap);
	free((char *)lncntp);
	if (!dfile.mod)
		return;
	if (!preen) {
		printf("\n***** FILE SYSTEM WAS MODIFIED *****\n");
		if (hotroot)
			printf("\n***** REBOOT UNIX *****\n");
	}
	if (hotroot) {
		sync();
		exit(4);
	}
}

setup(dev)
	char *dev;
{
	dev_t rootdev;
	struct stat statb;
	daddr_t super = bflag ? bflag : SBLOCK;
	int i, j, c, d, cgd;
	long size;
	BUFAREA asblk;
#	define altsblock asblk.b_un.b_fs

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
	fixcg = 0; inosumbad = 0; offsumbad = 0; frsumbad = 0; sbsumbad = 0;
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
	initbarea(&asblk);
	/*
	 * Read in the super block and its summary info.
	 */
	if (bread(&dfile, (char *)&sblock, super, (long)SBSIZE) == 0)
		return (0);
	sblk.b_bno = super;
	sblk.b_size = SBSIZE;
	/*
	 * run a few consistency checks of the super block
	 */
	if (sblock.fs_magic != FS_MAGIC)
		{ badsb("MAGIC NUMBER WRONG"); return (0); }
	if (sblock.fs_ncg < 1)
		{ badsb("NCG OUT OF RANGE"); return (0); }
	if (sblock.fs_cpg < 1 || sblock.fs_cpg > MAXCPG)
		{ badsb("CPG OUT OF RANGE"); return (0); }
	if (sblock.fs_ncg * sblock.fs_cpg < sblock.fs_ncyl ||
	    (sblock.fs_ncg - 1) * sblock.fs_cpg >= sblock.fs_ncyl)
		{ badsb("NCYL DOES NOT JIVE WITH NCG*CPG"); return (0); }
	if (sblock.fs_sbsize > SBSIZE)
		{ badsb("SIZE PREPOSTEROUSLY LARGE"); return (0); }
	/*
	 * Set all possible fields that could differ, then do check
	 * of whole super block against an alternate super block.
	 * When an alternate super-block is specified this check is skipped.
	 */
	if (bflag)
		goto sbok;
	if (getblk(&asblk, cgsblock(&sblock, sblock.fs_ncg - 1),
	    sblock.fs_sbsize) == 0)
		return (0);
	altsblock.fs_link = sblock.fs_link;
	altsblock.fs_rlink = sblock.fs_rlink;
	altsblock.fs_time = sblock.fs_time;
	altsblock.fs_cstotal = sblock.fs_cstotal;
	altsblock.fs_cgrotor = sblock.fs_cgrotor;
	altsblock.fs_fmod = sblock.fs_fmod;
	altsblock.fs_clean = sblock.fs_clean;
	altsblock.fs_ronly = sblock.fs_ronly;
	altsblock.fs_flags = sblock.fs_flags;
	altsblock.fs_maxcontig = sblock.fs_maxcontig;
	altsblock.fs_minfree = sblock.fs_minfree;
	altsblock.fs_rotdelay = sblock.fs_rotdelay;
	altsblock.fs_maxbpg = sblock.fs_maxbpg;
	bcopy((char *)sblock.fs_csp, (char *)altsblock.fs_csp,
		sizeof sblock.fs_csp);
	bcopy((char *)sblock.fs_fsmnt, (char *)altsblock.fs_fsmnt,
		sizeof sblock.fs_fsmnt);
	if (bcmp((char *)&sblock, (char *)&altsblock, (int)sblock.fs_sbsize))
		{ badsb("TRASHED VALUES IN SUPER BLOCK"); return (0); }
sbok:
	fmax = sblock.fs_size;
	imax = sblock.fs_ncg * sblock.fs_ipg;
	n_bad = cgsblock(&sblock, 0); /* boot block plus dedicated sblock */
	/*
	 * read in the summary info.
	 */
	for (i = 0, j = 0; i < sblock.fs_cssize; i += sblock.fs_bsize, j++) {
		size = sblock.fs_cssize - i < sblock.fs_bsize ?
		    sblock.fs_cssize - i : sblock.fs_bsize;
		sblock.fs_csp[j] = (struct csum *)calloc(1, (unsigned)size);
		if (bread(&dfile, (char *)sblock.fs_csp[j],
		    fsbtodb(&sblock, sblock.fs_csaddr + j * sblock.fs_frag),
		    size) == 0)
			return (0);
	}
	/*
	 * allocate and initialize the necessary maps
	 */
	bmapsz = roundup(howmany(fmax, NBBY), sizeof(short));
	blockmap = calloc((unsigned)bmapsz, sizeof (char));
	if (blockmap == NULL) {
		printf("cannot alloc %d bytes for blockmap\n", bmapsz);
		goto badsb;
	}
	freemap = calloc((unsigned)bmapsz, sizeof (char));
	if (freemap == NULL) {
		printf("cannot alloc %d bytes for freemap\n", bmapsz);
		goto badsb;
	}
	statemap = calloc((unsigned)(imax + 1), sizeof(char));
	if (statemap == NULL) {
		printf("cannot alloc %d bytes for statemap\n", imax + 1);
		goto badsb;
	}
	lncntp = (short *)calloc((unsigned)(imax + 1), sizeof(short));
	if (lncntp == NULL) {
		printf("cannot alloc %d bytes for lncntp\n", 
		    (imax + 1) * sizeof(short));
		goto badsb;
	}
	for (c = 0; c < sblock.fs_ncg; c++) {
		cgd = cgdmin(&sblock, c);
		if (c == 0) {
			d = cgbase(&sblock, c);
			cgd += howmany(sblock.fs_cssize, sblock.fs_fsize);
		} else
			d = cgsblock(&sblock, c);
		for (; d < cgd; d++)
			setbmap(d);
	}

	return (1);

badsb:
	ckfini();
	return (0);
#	undef altsblock
}

pass1()
{
	register int c, i, n, j;
	register DINODE *dp;
	int ndb, partial;
	struct inodesc idesc;
	ino_t inumber;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	idesc.id_func = pass1check;
	inumber = 0;
	n_blks += howmany(sblock.fs_cssize, sblock.fs_fsize);
	for (c = 0; c < sblock.fs_ncg; c++) {
		if (getblk(&cgblk, cgtod(&sblock, c), sblock.fs_cgsize) == 0)
			continue;
		if (cgrp.cg_magic != CG_MAGIC) {
			pfatal("CG %d: BAD MAGIC NUMBER\n", c);
			bzero((char *)&cgrp, (int)sblock.fs_cgsize);
		}
		n = 0;
		for (i = 0; i < sblock.fs_ipg; i++, inumber++) {
			dp = ginode(inumber);
			if (dp == NULL)
				continue;
			n++;
			if (ALLOC) {
				if (!isset(cgrp.cg_iused, i)) {
					if (debug)
						printf("%d bad, not used\n",
						    inumber);
					inosumbad++;
				}
				n--;
				lastino = inumber;
				if (!preen && BADBLK &&
				    reply("HOLD BAD BLOCK") == 1) {
					dp->di_size = sblock.fs_fsize;
					dp->di_mode = IFREG|0600;
					inodirty();
				} else if (ftypeok(dp) == 0)
					goto unknown;
				if (dp->di_size < 0) {
					if (debug)
						printf("bad size %d:",
							dp->di_size);
					goto unknown;
				}
				ndb = howmany(dp->di_size, sblock.fs_bsize);
				if (SPECIAL)
					ndb++;
				for (j = ndb; j < NDADDR; j++)
					if (dp->di_db[j] != 0) {
						if (debug)
							printf("bad direct addr: %d\n",
								dp->di_db[j]);
						goto unknown;
					}
				for (j = 0, ndb -= NDADDR; ndb > 0; j++)
					ndb /= NINDIR(&sblock);
				for (; j < NIADDR; j++)
					if (dp->di_ib[j] != 0) {
						if (debug)
							printf("bad indirect addr: %d\n",
								dp->di_ib[j]);
						goto unknown;
					}
				n_files++;
				lncntp[inumber] = dp->di_nlink;
				if (dp->di_nlink <= 0) {
					if (badlnp < &badlncnt[MAXLNCNT])
						*badlnp++ = inumber;
					else {
						pfatal("LINK COUNT TABLE OVERFLOW");
						if (reply("CONTINUE") == 0)
							errexit("");
					}
				}
				statemap[inumber] = DIRCT ? DSTATE : FSTATE;
				badblk = dupblk = 0; maxblk = 0;
				idesc.id_number = inumber;
				idesc.id_filesize = 0;
				(void)ckinode(dp, &idesc);
				idesc.id_filesize *= btodb(sblock.fs_fsize);
				if (dp->di_blocks != idesc.id_filesize) {
					pwarn("INCORRECT BLOCK COUNT I=%u (%ld should be %ld)",
					    inumber, dp->di_blocks,
					    idesc.id_filesize);
					if (preen)
						printf(" (CORRECTED)\n");
					else if (reply("CORRECT") == 0)
						continue;
					dp->di_blocks = idesc.id_filesize;
					inodirty();
				}
				continue;
		unknown:
				pfatal("UNKNOWN FILE TYPE I=%u", inumber);
				if (reply("CLEAR") == 1) {
					zapino(dp);
					inodirty();
					inosumbad++;
				}
			} else {
				if (isset(cgrp.cg_iused, i)) {
					if (debug)
						printf("%d bad, marked used\n",
						    inumber);
					inosumbad++;
					n--;
				}
				partial = 0;
				for (j = 0; j < NDADDR; j++)
					if (dp->di_db[j] != 0)
						partial++;
				for (j = 0; j < NIADDR; j++)
					if (dp->di_ib[j] != 0)
						partial++;
				if (partial || dp->di_mode != 0 ||
				    dp->di_size != 0) {
					pfatal("PARTIALLY ALLOCATED INODE I=%u",
						inumber);
					if (reply("CLEAR") == 1) {
						zapino(dp);
						inodirty();
						inosumbad++;
					}
				}
			}
		}
		if (n != cgrp.cg_cs.cs_nifree) {
			if (debug)
				printf("cg[%d].cg_cs.cs_nifree is %d; calc %d\n",
				    c, cgrp.cg_cs.cs_nifree, n);
			inosumbad++;
		}
		if (cgrp.cg_cs.cs_nbfree != sblock.fs_cs(&sblock, c).cs_nbfree
		  || cgrp.cg_cs.cs_nffree != sblock.fs_cs(&sblock, c).cs_nffree
		  || cgrp.cg_cs.cs_nifree != sblock.fs_cs(&sblock, c).cs_nifree
		  || cgrp.cg_cs.cs_ndir != sblock.fs_cs(&sblock, c).cs_ndir)
			sbsumbad++;
	}
}

pass1check(idesc)
	register struct inodesc *idesc;
{
	register daddr_t *dlp;
	int res = KEEPON;
	int anyout, nfrags;
	daddr_t blkno = idesc->id_blkno;

	anyout = outrange(blkno, idesc->id_numfrags);
	for (nfrags = idesc->id_numfrags; nfrags > 0; blkno++, nfrags--) {
		if (anyout && outrange(blkno, 1)) {
			blkerr(idesc->id_number, "BAD", blkno);
			if (++badblk >= MAXBAD) {
				pwarn("EXCESSIVE BAD BLKS I=%u",
					idesc->id_number);
				if (preen)
					printf(" (SKIPPING)\n");
				else if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			res = SKIP;
		} else if (getbmap(blkno)) {
			blkerr(idesc->id_number, "DUP", blkno);
			if (++dupblk >= MAXDUP) {
				pwarn("EXCESSIVE DUP BLKS I=%u",
					idesc->id_number);
				if (preen)
					printf(" (SKIPPING)\n");
				else if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			if (enddup >= &duplist[DUPTBLSIZE]) {
				pfatal("DUP TABLE OVERFLOW.");
				if (reply("CONTINUE") == 0)
					errexit("");
				return (STOP);
			}
			for (dlp = duplist; dlp < muldup; dlp++)
				if (*dlp == blkno) {
					*enddup++ = blkno;
					break;
				}
			if (dlp >= muldup) {
				*enddup++ = *muldup;
				*muldup++ = blkno;
			}
		} else {
			n_blks++;
			setbmap(blkno);
		}
		idesc->id_filesize++;
	}
	return (res);
}

pass1b()
{
	register int c, i;
	register DINODE *dp;
	struct inodesc idesc;
	ino_t inumber;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	idesc.id_func = pass1bcheck;
	inumber = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		for (i = 0; i < sblock.fs_ipg; i++, inumber++) {
			dp = ginode(inumber);
			if (dp == NULL)
				continue;
			idesc.id_number = inumber;
			if (statemap[inumber] != USTATE &&
			    (ckinode(dp, &idesc) & STOP))
				goto out1b;
		}
	}
out1b:
	flush(&dfile, &inoblk);
}

pass1bcheck(idesc)
	register struct inodesc *idesc;
{
	register daddr_t *dlp;
	int nfrags, res = KEEPON;
	daddr_t blkno = idesc->id_blkno;

	for (nfrags = idesc->id_numfrags; nfrags > 0; blkno++, nfrags--) {
		if (outrange(blkno, 1))
			res = SKIP;
		for (dlp = duplist; dlp < muldup; dlp++)
			if (*dlp == blkno) {
				blkerr(idesc->id_number, "DUP", blkno);
				*dlp = *--muldup;
				*muldup = blkno;
				if (muldup == duplist)
					return (STOP);
			}
	}
	return (res);
}

pass2()
{
	register DINODE *dp;
	struct inodesc rootdesc;

	bzero((char *)&rootdesc, sizeof(struct inodesc));
	rootdesc.id_type = ADDR;
	rootdesc.id_func = pass2check;
	rootdesc.id_number = ROOTINO;
	pathp = pathname;
	switch (statemap[ROOTINO]) {

	case USTATE:
		errexit("ROOT INODE UNALLOCATED. TERMINATING.\n");

	case FSTATE:
		pfatal("ROOT INODE NOT DIRECTORY");
		if (reply("FIX") == 0 || (dp = ginode(ROOTINO)) == NULL)
			errexit("");
		dp->di_mode &= ~IFMT;
		dp->di_mode |= IFDIR;
		inodirty();
		inosumbad++;
		statemap[ROOTINO] = DSTATE;
		/* fall into ... */

	case DSTATE:
		descend(&rootdesc, ROOTINO);
		break;

	case CLEAR:
		pfatal("DUPS/BAD IN ROOT INODE");
		printf("\n");
		if (reply("CONTINUE") == 0)
			errexit("");
		statemap[ROOTINO] = DSTATE;
		descend(&rootdesc, ROOTINO);
	}
}

pass2check(idesc)
	struct inodesc *idesc;
{
	register DIRECT *dirp = idesc->id_dirp;
	char *curpathloc;
	int n, entrysize, ret = 0;
	DINODE *dp;
	DIRECT proto;

	/* 
	 * check for "."
	 */
	if (idesc->id_entryno != 0)
		goto chk1;
	if (dirp->d_ino != 0 && dirp->d_namlen == 1 && dirp->d_name[0] == '.') {
		if (dirp->d_ino != idesc->id_number) {
			direrr(idesc->id_number, "BAD INODE NUMBER FOR '.'");
			dirp->d_ino = idesc->id_number;
			if (reply("FIX") == 1)
				ret |= ALTERED;
		}
		goto chk1;
	}
	direrr(idesc->id_number, "MISSING '.'");
	proto.d_ino = idesc->id_number;
	proto.d_namlen = 1;
	(void)strcpy(proto.d_name, ".");
	entrysize = DIRSIZ(&proto);
	if (dirp->d_ino != 0) {
		pfatal("CANNOT FIX, FIRST ENTRY IN DIRECTORY CONTAINS %s\n",
			dirp->d_name);
	} else if (dirp->d_reclen < entrysize) {
		pfatal("CANNOT FIX, INSUFFICIENT SPACE TO ADD '.'\n");
	} else if (dirp->d_reclen < 2 * entrysize) {
		proto.d_reclen = dirp->d_reclen;
		bcopy((char *)&proto, (char *)dirp, entrysize);
		if (reply("FIX") == 1)
			ret |= ALTERED;
	} else {
		n = dirp->d_reclen - entrysize;
		proto.d_reclen = entrysize;
		bcopy((char *)&proto, (char *)dirp, entrysize);
		idesc->id_entryno++;
		lncntp[dirp->d_ino]--;
		dirp = (DIRECT *)((char *)(dirp) + entrysize);
		bzero((char *)dirp, n);
		dirp->d_reclen = n;
		if (reply("FIX") == 1)
			ret |= ALTERED;
	}
chk1:
	if (idesc->id_entryno > 1)
		goto chk2;
	proto.d_ino = idesc->id_parent;
	proto.d_namlen = 2;
	(void)strcpy(proto.d_name, "..");
	entrysize = DIRSIZ(&proto);
	if (idesc->id_entryno == 0) {
		n = DIRSIZ(dirp);
		if (dirp->d_reclen < n + entrysize)
			goto chk2;
		proto.d_reclen = dirp->d_reclen - n;
		dirp->d_reclen = n;
		idesc->id_entryno++;
		lncntp[dirp->d_ino]--;
		dirp = (DIRECT *)((char *)(dirp) + n);
		bzero((char *)dirp, n);
		dirp->d_reclen = n;
	}
	if (dirp->d_ino != 0 && dirp->d_namlen == 2 &&
	    strcmp(dirp->d_name, "..") == 0) {
		if (dirp->d_ino != idesc->id_parent) {
			direrr(idesc->id_number, "BAD INODE NUMBER FOR '..'");
			dirp->d_ino = idesc->id_parent;
			if (reply("FIX") == 1)
				ret |= ALTERED;
		}
		goto chk2;
	}
	direrr(idesc->id_number, "MISSING '..'");
	if (dirp->d_ino != 0) {
		pfatal("CANNOT FIX, SECOND ENTRY IN DIRECTORY CONTAINS %s\n",
			dirp->d_name);
	} else if (dirp->d_reclen < entrysize) {
		pfatal("CANNOT FIX, INSUFFICIENT SPACE TO ADD '..'\n");
	} else {
		proto.d_reclen = dirp->d_reclen;
		bcopy((char *)&proto, (char *)dirp, entrysize);
		if (reply("FIX") == 1)
			ret |= ALTERED;
	}
chk2:
	if (dirp->d_ino == 0)
		return (ret|KEEPON);
	if (idesc->id_entryno >= 2 &&
	    dirp->d_namlen <= 2 &&
	    dirp->d_name[0] == '.') {
		if (dirp->d_namlen == 1) {
			direrr(idesc->id_number, "EXTRA '.' ENTRY");
			dirp->d_ino = 0;
			if (reply("FIX") == 1)
				ret |= ALTERED;
			return (KEEPON | ret);
		}
		if (dirp->d_name[1] == '.') {
			direrr(idesc->id_number, "EXTRA '..' ENTRY");
			dirp->d_ino = 0;
			if (reply("FIX") == 1)
				ret |= ALTERED;
			return (KEEPON | ret);
		}
	}
	curpathloc = pathp;
	*pathp++ = '/';
	if (pathp + dirp->d_namlen >= endpathname) {
		*pathp = '\0';
		errexit("NAME TOO LONG %s%s\n", pathname, dirp->d_name);
	}
	bcopy(dirp->d_name, pathp, dirp->d_namlen + 1);
	pathp += dirp->d_namlen;
	idesc->id_entryno++;
	n = 0;
	if (dirp->d_ino > imax || dirp->d_ino <= 0) {
		direrr(dirp->d_ino, "I OUT OF RANGE");
		n = reply("REMOVE");
	} else {
again:
		switch (statemap[dirp->d_ino]) {
		case USTATE:
			direrr(dirp->d_ino, "UNALLOCATED");
			n = reply("REMOVE");
			break;

		case CLEAR:
			direrr(dirp->d_ino, "DUP/BAD");
			if ((n = reply("REMOVE")) == 1)
				break;
			if ((dp = ginode(dirp->d_ino)) == NULL)
				break;
			statemap[dirp->d_ino] = DIRCT ? DSTATE : FSTATE;
			goto again;

		case FSTATE:
			lncntp[dirp->d_ino]--;
			break;

		case DSTATE:
			descend(idesc, dirp->d_ino);
			if (statemap[dirp->d_ino] != CLEAR) {
				lncntp[dirp->d_ino]--;
			} else {
				dirp->d_ino = 0;
				ret |= ALTERED;
			}
			break;
		}
	}
	pathp = curpathloc;
	*pathp = '\0';
	if (n == 0)
		return (ret|KEEPON);
	dirp->d_ino = 0;
	return (ret|KEEPON|ALTERED);
}

pass3()
{
	register DINODE *dp;
	struct inodesc idesc;
	ino_t inumber, orphan;
	int loopcnt;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = DATA;
	for (inumber = ROOTINO; inumber <= lastino; inumber++) {
		if (statemap[inumber] == DSTATE) {
			pathp = pathname;
			*pathp++ = '?';
			*pathp = '\0';
			idesc.id_func = findino;
			srchname = "..";
			idesc.id_parent = inumber;
			loopcnt = 0;
			do {
				orphan = idesc.id_parent;
				if ((dp = ginode(orphan)) == NULL)
					break;
				idesc.id_parent = 0;
				idesc.id_filesize = dp->di_size;
				idesc.id_number = orphan;
				(void)ckinode(dp, &idesc);
				if (idesc.id_parent == 0)
					break;
				if (loopcnt >= sblock.fs_cstotal.cs_ndir)
					break;
				loopcnt++;
			} while (statemap[idesc.id_parent] == DSTATE);
			if (linkup(orphan, idesc.id_parent) == 1) {
				idesc.id_func = pass2check;
				idesc.id_number = lfdir;
				descend(&idesc, orphan);
			}
		}
	}
}

pass4()
{
	register ino_t inumber, *blp;
	int n;
	struct inodesc idesc;

	bzero((char *)&idesc, sizeof(struct inodesc));
	idesc.id_type = ADDR;
	idesc.id_func = pass4check;
	for (inumber = ROOTINO; inumber <= lastino; inumber++) {
		idesc.id_number = inumber;
		switch (statemap[inumber]) {

		case FSTATE:
			n = lncntp[inumber];
			if (n)
				adjust(&idesc, (short)n);
			else {
				for (blp = badlncnt;blp < badlnp; blp++)
					if (*blp == inumber) {
						clri(&idesc, "UNREF", 1);
						break;
					}
			}
			break;

		case DSTATE:
			clri(&idesc, "UNREF", 1);
			break;

		case CLEAR:
			clri(&idesc, "BAD/DUP", 1);
			break;
		}
	}
	if (imax - ROOTINO - n_files != sblock.fs_cstotal.cs_nifree) {
		pwarn("FREE INODE COUNT WRONG IN SUPERBLK");
		if (preen)
			printf(" (FIXED)\n");
		if (preen || reply("FIX") == 1) {
			sblock.fs_cstotal.cs_nifree = imax - ROOTINO - n_files;
			sbdirty();
		}
	}
	flush(&dfile, &fileblk);
}

pass4check(idesc)
	register struct inodesc *idesc;
{
	register daddr_t *dlp;
	int nfrags, res = KEEPON;
	daddr_t blkno = idesc->id_blkno;

	for (nfrags = idesc->id_numfrags; nfrags > 0; blkno++, nfrags--) {
		if (outrange(blkno, 1))
			res = SKIP;
		else if (getbmap(blkno)) {
			for (dlp = duplist; dlp < enddup; dlp++)
				if (*dlp == blkno) {
					*dlp = *--enddup;
					return (KEEPON);
				}
			clrbmap(blkno);
			n_blks--;
		}
	}
	return (res);
}

pass5()
{
	register int c, n, i, b, d;
	short bo[MAXCPG][NRPOS];
	long botot[MAXCPG];
	long frsum[MAXFRAG];
	int blk;
	daddr_t cbase;
	int blockbits = (1<<sblock.fs_frag)-1;

	bcopy(blockmap, freemap, (unsigned)bmapsz);
	dupblk = 0;
	n_index = sblock.fs_ncg * (cgdmin(&sblock, 0) - cgtod(&sblock, 0));
	for (c = 0; c < sblock.fs_ncg; c++) {
		cbase = cgbase(&sblock, c);
		bzero((char *)botot, sizeof (botot));
		bzero((char *)bo, sizeof (bo));
		bzero((char *)frsum, sizeof (frsum));
		/*
		 * need to account for the super blocks
		 * which appear (inaccurately) bad
		 */
		n_bad += cgtod(&sblock, c) - cgsblock(&sblock, c);
		if (getblk(&cgblk, cgtod(&sblock, c), sblock.fs_cgsize) == 0)
			continue;
		if (cgrp.cg_magic != CG_MAGIC) {
			pfatal("CG %d: BAD MAGIC NUMBER\n", c);
			bzero((char *)&cgrp, (int)sblock.fs_cgsize);
		}
		for (b = 0; b < sblock.fs_fpg; b += sblock.fs_frag) {
			blk = blkmap(&sblock, cgrp.cg_free, b);
			if (blk == 0)
				continue;
			if (blk == blockbits) {
				if (pass5check(cbase+b, sblock.fs_frag) == STOP)
					goto out5;
				/* this is clumsy ... */
				n_ffree -= sblock.fs_frag;
				n_bfree++;
				botot[cbtocylno(&sblock, b)]++;
				bo[cbtocylno(&sblock, b)]
				    [cbtorpos(&sblock, b)]++;
				continue;
			}
			for (d = 0; d < sblock.fs_frag; d++)
				if ((blk & (1<<d)) &&
				    pass5check(cbase + b + d, (long)1) == STOP)
					goto out5;
			fragacct(&sblock, blk, frsum, 1);
		}
		if (bcmp((char *)cgrp.cg_frsum, (char *)frsum, sizeof(frsum))) {
			if (debug)
			for (i = 0; i < sblock.fs_frag; i++)
				if (cgrp.cg_frsum[i] != frsum[i])
				printf("cg[%d].cg_frsum[%d] have %d calc %d\n",
				    c, i, cgrp.cg_frsum[i], frsum[i]);
			frsumbad++;
		}
		if (bcmp((char *)cgrp.cg_btot, (char *)botot, sizeof (botot))) {
			if (debug)
			for (n = 0; n < sblock.fs_cpg; n++)
				if (botot[n] != cgrp.cg_btot[n])
				printf("cg[%d].cg_btot[%d] have %d calc %d\n",
				    c, n, cgrp.cg_btot[n], botot[n]);
			offsumbad++;
		}
		if (bcmp((char *)cgrp.cg_b, (char *)bo, sizeof (bo))) {
			if (debug)
			for (i = 0; i < NRPOS; i++)
				if (bo[n][i] != cgrp.cg_b[n][i])
				printf("cg[%d].cg_b[%d][%d] have %d calc %d\n",
				    c, n, i, cgrp.cg_b[n][i], bo[n][i]);
			offsumbad++;
		}
	}
out5:
	if (dupblk)
		pwarn("%d DUP BLKS IN BIT MAPS\n", dupblk);
	if (fixcg == 0) {
		if ((b = n_blks+n_ffree+sblock.fs_frag*n_bfree+n_index+n_bad) != fmax) {
			pwarn("%ld BLK(S) MISSING\n", fmax - b);
			fixcg = 1;
		} else if (inosumbad + offsumbad + frsumbad + sbsumbad) {
			pwarn("SUMMARY INFORMATION %s%s%s%sBAD\n",
			    inosumbad ? "(INODE FREE) " : "",
			    offsumbad ? "(BLOCK OFFSETS) " : "",
			    frsumbad ? "(FRAG SUMMARIES) " : "",
			    sbsumbad ? "(SUPER BLOCK SUMMARIES) " : "");
			fixcg = 1;
		} else if (n_ffree != sblock.fs_cstotal.cs_nffree ||
		    n_bfree != sblock.fs_cstotal.cs_nbfree) {
			pwarn("FREE BLK COUNT(S) WRONG IN SUPERBLK");
			if (preen)
				printf(" (FIXED)\n");
			if (preen || reply("FIX") == 1) {
				sblock.fs_cstotal.cs_nffree = n_ffree;
				sblock.fs_cstotal.cs_nbfree = n_bfree;
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
}

pass5check(blk, size)
	daddr_t blk;
	long size;
{

	if (outrange(blk, (int)size)) {
		fixcg = 1;
		if (preen)
			pfatal("BAD BLOCKS IN BIT MAPS.");
		if (++badblk >= MAXBAD) {
			printf("EXCESSIVE BAD BLKS IN BIT MAPS.");
			if (reply("CONTINUE") == 0)
				errexit("");
			return (STOP);
		}
	}
	for (; size > 0; blk++, size--)
		if (getfmap(blk)) {
			fixcg = 1;
			++dupblk;
		} else {
			n_ffree++;
			setfmap(blk);
		}
	return (KEEPON);
}

ckinode(dp, idesc)
	DINODE *dp;
	register struct inodesc *idesc;
{
	register daddr_t *ap;
	int ret, n, ndb, offset;
	DINODE dino;

	if (SPECIAL)
		return (KEEPON);
	dino = *dp;
	idesc->id_fix = DONTKNOW;
	idesc->id_entryno = 0;
	ndb = howmany(dino.di_size, sblock.fs_bsize);
	for (ap = &dino.di_db[0]; ap < &dino.di_db[NDADDR]; ap++) {
		if (--ndb == 0 && (offset = blkoff(&sblock, dino.di_size)) != 0)
			idesc->id_numfrags =
				numfrags(&sblock, fragroundup(&sblock, offset));
		else
			idesc->id_numfrags = sblock.fs_frag;
		if (*ap == 0)
			continue;
		idesc->id_blkno = *ap;
		if (idesc->id_type == ADDR)
			ret = (*idesc->id_func)(idesc);
		else
			ret = dirscan(idesc);
		if (ret & STOP)
			return (ret);
	}
	idesc->id_numfrags = sblock.fs_frag;
	for (ap = &dino.di_ib[0], n = 1; n <= 2; ap++, n++) {
		if (*ap) {
			idesc->id_blkno = *ap;
			ret = iblock(idesc, n,
				dino.di_size - sblock.fs_bsize * NDADDR);
			if (ret & STOP)
				return (ret);
		}
	}
	return (KEEPON);
}

iblock(idesc, ilevel, isize)
	struct inodesc *idesc;
	register ilevel;
	long isize;
{
	register daddr_t *ap;
	register daddr_t *aplim;
	int i, n, (*func)(), nif;
	BUFAREA ib;

	if (idesc->id_type == ADDR) {
		func = idesc->id_func;
		if (((n = (*func)(idesc)) & KEEPON) == 0)
			return (n);
	} else
		func = dirscan;
	if (outrange(idesc->id_blkno, idesc->id_numfrags)) /* protect thyself */
		return (SKIP);
	initbarea(&ib);
	if (getblk(&ib, idesc->id_blkno, sblock.fs_bsize) == NULL)
		return (SKIP);
	ilevel--;
	if (ilevel == 0) {
		nif = lblkno(&sblock, isize) + 1;
	} else /* ilevel == 1 */ {
		nif = isize / (sblock.fs_bsize * NINDIR(&sblock)) + 1;
	}
	if (nif > NINDIR(&sblock))
		nif = NINDIR(&sblock);
	aplim = &ib.b_un.b_indir[nif];
	for (ap = ib.b_un.b_indir, i = 1; ap < aplim; ap++, i++)
		if (*ap) {
			idesc->id_blkno = *ap;
			if (ilevel > 0)
				n = iblock(idesc, ilevel,
				    isize - i*NINDIR(&sblock)*sblock.fs_bsize);
			else
				n = (*func)(idesc);
			if (n & STOP)
				return (n);
		}
	return (KEEPON);
}

outrange(blk, cnt)
	daddr_t blk;
	int cnt;
{
	register int c;

	if ((unsigned)(blk+cnt) > fmax)
		return (1);
	c = dtog(&sblock, blk);
	if (blk < cgdmin(&sblock, c)) {
		if ((blk+cnt) > cgsblock(&sblock, c)) {
			if (debug) {
				printf("blk %d < cgdmin %d;",
				    blk, cgdmin(&sblock, c));
				printf(" blk+cnt %d > cgsbase %d\n",
				    blk+cnt, cgsblock(&sblock, c));
			}
			return (1);
		}
	} else {
		if ((blk+cnt) > cgbase(&sblock, c+1)) {
			if (debug)  {
				printf("blk %d >= cgdmin %d;",
				    blk, cgdmin(&sblock, c));
				printf(" blk+cnt %d > sblock.fs_fpg %d\n",
				    blk+cnt, sblock.fs_fpg);
			}
			return (1);
		}
	}
	return (0);
}

blkerr(ino, s, blk)
	ino_t ino;
	char *s;
	daddr_t blk;
{

	pfatal("%ld %s I=%u", blk, s, ino);
	printf("\n");
	statemap[ino] = CLEAR;
}

descend(parentino, inumber)
	struct inodesc *parentino;
	ino_t inumber;
{
	register DINODE *dp;
	struct inodesc curino;

	bzero((char *)&curino, sizeof(struct inodesc));
	statemap[inumber] = FSTATE;
	if ((dp = ginode(inumber)) == NULL)
		return;
	if (dp->di_size == 0) {
		direrr(inumber, "ZERO LENGTH DIRECTORY");
		if (reply("REMOVE") == 1)
			statemap[inumber] = CLEAR;
		return;
	}
	if (dp->di_size < MINDIRSIZE) {
		direrr(inumber, "DIRECTORY TOO SHORT");
		dp->di_size = MINDIRSIZE;
		if (reply("FIX") == 1)
			inodirty();
	}
	curino.id_type = DATA;
	curino.id_func = parentino->id_func;
	curino.id_parent = parentino->id_number;
	curino.id_number = inumber;
	curino.id_filesize = dp->di_size;
	(void)ckinode(dp, &curino);
}

dirscan(idesc)
	register struct inodesc *idesc;
{
	register DIRECT *dp;
	int dsize, n;
	long blksiz;
	char dbuf[DIRBLKSIZ];

	if (idesc->id_type != DATA)
		errexit("wrong type to dirscan %d\n", idesc->id_type);
	blksiz = idesc->id_numfrags * sblock.fs_fsize;
	if (outrange(idesc->id_blkno, idesc->id_numfrags)) {
		idesc->id_filesize -= blksiz;
		return (SKIP);
	}
	idesc->id_loc = 0;
	for (dp = fsck_readdir(idesc); dp != NULL; dp = fsck_readdir(idesc)) {
		dsize = dp->d_reclen;
		bcopy((char *)dp, dbuf, dsize);
		idesc->id_dirp = (DIRECT *)dbuf;
		if ((n = (*idesc->id_func)(idesc)) & ALTERED) {
			if (getblk(&fileblk, idesc->id_blkno, blksiz) != NULL) {
				bcopy(dbuf, (char *)dp, dsize);
				dirty(&fileblk);
				sbdirty();
			} else
				n &= ~ALTERED;
		}
		if (n & STOP) 
			return (n);
	}
	return (idesc->id_filesize > 0 ? KEEPON : STOP);
}

/*
 * get next entry in a directory.
 */
DIRECT *
fsck_readdir(idesc)
	register struct inodesc *idesc;
{
	register DIRECT *dp, *ndp;
	long size, blksiz;

	blksiz = idesc->id_numfrags * sblock.fs_fsize;
	if (getblk(&fileblk, idesc->id_blkno, blksiz) == NULL) {
		idesc->id_filesize -= blksiz - idesc->id_loc;
		return NULL;
	}
	if (idesc->id_loc % DIRBLKSIZ == 0 && idesc->id_filesize > 0 &&
	    idesc->id_loc < blksiz) {
		dp = (DIRECT *)(dirblk.b_buf + idesc->id_loc);
		if (dircheck(idesc, dp))
			goto dpok;
		idesc->id_loc += DIRBLKSIZ;
		idesc->id_filesize -= DIRBLKSIZ;
		dp->d_reclen = DIRBLKSIZ;
		dp->d_ino = 0;
		dp->d_namlen = 0;
		dp->d_name[0] = '\0';
		if (dofix(idesc))
			dirty(&fileblk);
		return (dp);
	}
dpok:
	if (idesc->id_filesize <= 0 || idesc->id_loc >= blksiz)
		return NULL;
	dp = (DIRECT *)(dirblk.b_buf + idesc->id_loc);
	idesc->id_loc += dp->d_reclen;
	idesc->id_filesize -= dp->d_reclen;
	ndp = (DIRECT *)(dirblk.b_buf + idesc->id_loc);
	if ((idesc->id_filesize <= 0 && idesc->id_loc % DIRBLKSIZ != 0) ||
	    (idesc->id_loc < blksiz && idesc->id_filesize > 0 &&
	     dircheck(idesc, ndp) == 0)) {
		size = DIRBLKSIZ - (idesc->id_loc % DIRBLKSIZ);
		dp->d_reclen += size;
		idesc->id_loc += size;
		idesc->id_filesize -= size;
		if (dofix(idesc))
			dirty(&fileblk);
	}
	return (dp);
}

/*
 * Verify that a directory entry is valid.
 * This is a superset of the checks made in the kernel.
 */
dircheck(idesc, dp)
	struct inodesc *idesc;
	register DIRECT *dp;
{
	register int size;
	register char *cp;
	int spaceleft;

	size = DIRSIZ(dp);
	spaceleft = DIRBLKSIZ - (idesc->id_loc % DIRBLKSIZ);
	if (dp->d_ino < imax &&
	    dp->d_reclen != 0 &&
	    dp->d_reclen <= spaceleft &&
	    (dp->d_reclen & 0x3) == 0 &&
	    dp->d_reclen >= size &&
	    idesc->id_filesize >= size &&
	    dp->d_namlen <= MAXNAMLEN) {
		if (dp->d_ino == 0)
			return (1);
		for (cp = dp->d_name, size = 0; size < dp->d_namlen; size++)
			if (*cp == 0 || (*cp++ & 0200))
				return (0);
		if (*cp == 0)
			return (1);
	}
	return (0);
}

direrr(ino, s)
	ino_t ino;
	char *s;
{
	register DINODE *dp;

	pwarn("%s ", s);
	pinode(ino);
	printf("\n");
	if ((dp = ginode(ino)) != NULL && ftypeok(dp))
		pfatal("%s=%s\n", DIRCT?"DIR":"FILE", pathname);
	else
		pfatal("NAME=%s\n", pathname);
}

adjust(idesc, lcnt)
	register struct inodesc *idesc;
	short lcnt;
{
	register DINODE *dp;

	if ((dp = ginode(idesc->id_number)) == NULL)
		return;
	if (dp->di_nlink == lcnt) {
		if (linkup(idesc->id_number, (ino_t)0) == 0)
			clri(idesc, "UNREF", 0);
	}
	else {
		pwarn("LINK COUNT %s",
			(lfdir==idesc->id_number)?lfname:(DIRCT?"DIR":"FILE"));
		pinode(idesc->id_number);
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

clri(idesc, s, flg)
	register struct inodesc *idesc;
	char *s;
	int flg;
{
	register DINODE *dp;

	if ((dp = ginode(idesc->id_number)) == NULL)
		return;
	if (flg == 1) {
		pwarn("%s %s", s, DIRCT?"DIR":"FILE");
		pinode(idesc->id_number);
	}
	if (preen || reply("CLEAR") == 1) {
		if (preen)
			printf(" (CLEARED)\n");
		n_files--;
		(void)ckinode(dp, idesc);
		zapino(dp);
		statemap[idesc->id_number] = USTATE;
		inodirty();
		inosumbad++;
	}
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
ginode(inumber)
	ino_t inumber;
{
	daddr_t iblk;
	static ino_t startinum = 0;	/* blk num of first in raw area */


	if (inumber < ROOTINO || inumber > imax) {
		if (debug && inumber > imax)
			printf("inumber out of range (%d)\n", inumber);
		return (NULL);
	}
	if (startinum == 0 ||
	    inumber < startinum || inumber >= startinum + INOPB(&sblock)) {
		iblk = itod(&sblock, inumber);
		if (getblk(&inoblk, iblk, sblock.fs_bsize) == NULL) {
			return (NULL);
		}
		startinum = (inumber / INOPB(&sblock)) * INOPB(&sblock);
	}
	return (&inoblk.b_un.b_dinode[inumber % INOPB(&sblock)]);
}

ftypeok(dp)
	DINODE *dp;
{
	switch (dp->di_mode & IFMT) {

	case IFDIR:
	case IFREG:
	case IFBLK:
	case IFCHR:
	case IFLNK:
	case IFSOCK:
		return (1);

	default:
		if (debug)
			printf("bad file type 0%o\n", dp->di_mode);
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
	register BUFAREA *bp;
	daddr_t blk;
	long size;
{
	register struct filecntl *fcp;
	daddr_t dblk;

	fcp = &dfile;
	dblk = fsbtodb(&sblock, blk);
	if (bp->b_bno == dblk)
		return (bp);
	flush(fcp, bp);
	if (bread(fcp, bp->b_un.b_buf, dblk, size) != 0) {
		bp->b_bno = dblk;
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
		(void)bwrite(fcp, bp->b_un.b_buf, bp->b_bno, (long)bp->b_size);
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
	(void)close(dfile.rfdes);
	(void)close(dfile.wfdes);
}

pinode(ino)
	ino_t ino;
{
	register DINODE *dp;
	register char *p;
	char uidbuf[BUFSIZ];
	char *ctime();

	printf(" I=%u ", ino);
	if ((dp = ginode(ino)) == NULL)
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

makecg()
{
	int c, blk;
	daddr_t dbase, d, dlower, dupper, dmax;
	long i, j, s;
	ino_t inumber;
	register struct csum *cs;
	register DINODE *dp;

	sblock.fs_cstotal.cs_nbfree = 0;
	sblock.fs_cstotal.cs_nffree = 0;
	sblock.fs_cstotal.cs_nifree = 0;
	sblock.fs_cstotal.cs_ndir = 0;
	for (c = 0; c < sblock.fs_ncg; c++) {
		dbase = cgbase(&sblock, c);
		dmax = dbase + sblock.fs_fpg;
		if (dmax > sblock.fs_size) {
			for ( ; dmax >= sblock.fs_size; dmax--)
				clrbit(cgrp.cg_free, dmax - dbase);
			dmax++;
		}
		dlower = cgsblock(&sblock, c) - dbase;
		dupper = cgdmin(&sblock, c) - dbase;
		cs = &sblock.fs_cs(&sblock, c);
		(void)time(&cgrp.cg_time);
		cgrp.cg_magic = CG_MAGIC;
		cgrp.cg_cgx = c;
		if (c == sblock.fs_ncg - 1)
			cgrp.cg_ncyl = sblock.fs_ncyl % sblock.fs_cpg;
		else
			cgrp.cg_ncyl = sblock.fs_cpg;
		cgrp.cg_niblk = sblock.fs_ipg;
		cgrp.cg_ndblk = dmax - dbase;
		cgrp.cg_cs.cs_ndir = 0;
		cgrp.cg_cs.cs_nffree = 0;
		cgrp.cg_cs.cs_nbfree = 0;
		cgrp.cg_cs.cs_nifree = 0;
		cgrp.cg_rotor = 0;
		cgrp.cg_frotor = 0;
		cgrp.cg_irotor = 0;
		for (i = 0; i < sblock.fs_frag; i++)
			cgrp.cg_frsum[i] = 0;
		inumber = sblock.fs_ipg * c;
		for (i = 0; i < sblock.fs_ipg; inumber++, i++) {
			cgrp.cg_cs.cs_nifree++;
			clrbit(cgrp.cg_iused, i);
			dp = ginode(inumber);
			if (dp == NULL)
				continue;
			if (ALLOC) {
				if (DIRCT)
					cgrp.cg_cs.cs_ndir++;
				cgrp.cg_cs.cs_nifree--;
				setbit(cgrp.cg_iused, i);
				continue;
			}
		}
		while (i < MAXIPG) {
			clrbit(cgrp.cg_iused, i);
			i++;
		}
		if (c == 0)
			for (i = 0; i < ROOTINO; i++) {
				setbit(cgrp.cg_iused, i);
				cgrp.cg_cs.cs_nifree--;
			}
		for (s = 0; s < MAXCPG; s++) {
			cgrp.cg_btot[s] = 0;
			for (i = 0; i < NRPOS; i++)
				cgrp.cg_b[s][i] = 0;
		}
		if (c == 0) {
			dupper += howmany(sblock.fs_cssize, sblock.fs_fsize);
		}
		for (d = dlower; d < dupper; d++)
			clrbit(cgrp.cg_free, d);
		for (d = 0; (d + sblock.fs_frag) <= dmax - dbase;
		    d += sblock.fs_frag) {
			j = 0;
			for (i = 0; i < sblock.fs_frag; i++) {
				if (!getbmap(dbase + d + i)) {
					setbit(cgrp.cg_free, d + i);
					j++;
				} else
					clrbit(cgrp.cg_free, d+i);
			}
			if (j == sblock.fs_frag) {
				cgrp.cg_cs.cs_nbfree++;
				cgrp.cg_btot[cbtocylno(&sblock, d)]++;
				cgrp.cg_b[cbtocylno(&sblock, d)]
				    [cbtorpos(&sblock, d)]++;
			} else if (j > 0) {
				cgrp.cg_cs.cs_nffree += j;
				blk = blkmap(&sblock, cgrp.cg_free, d);
				fragacct(&sblock, blk, cgrp.cg_frsum, 1);
			}
		}
		for (j = d; d < dmax - dbase; d++) {
			if (!getbmap(dbase + d)) {
				setbit(cgrp.cg_free, d);
				cgrp.cg_cs.cs_nffree++;
			} else
				clrbit(cgrp.cg_free, d);
		}
		for (; d % sblock.fs_frag != 0; d++)
			clrbit(cgrp.cg_free, d);
		if (j != d) {
			blk = blkmap(&sblock, cgrp.cg_free, j);
			fragacct(&sblock, blk, cgrp.cg_frsum, 1);
		}
		for (d /= sblock.fs_frag; d < MAXBPG(&sblock); d ++)
			clrblock(&sblock, cgrp.cg_free, d);
		sblock.fs_cstotal.cs_nffree += cgrp.cg_cs.cs_nffree;
		sblock.fs_cstotal.cs_nbfree += cgrp.cg_cs.cs_nbfree;
		sblock.fs_cstotal.cs_nifree += cgrp.cg_cs.cs_nifree;
		sblock.fs_cstotal.cs_ndir += cgrp.cg_cs.cs_ndir;
		*cs = cgrp.cg_cs;
		(void)bwrite(&dfile, (char *)&cgrp,
			fsbtodb(&sblock, cgtod(&sblock, c)), sblock.fs_cgsize);
	}
	for (i = 0, j = 0; i < sblock.fs_cssize; i += sblock.fs_bsize, j++) {
		(void)bwrite(&dfile, (char *)sblock.fs_csp[j],
		    fsbtodb(&sblock, sblock.fs_csaddr + j * sblock.fs_frag),
		    sblock.fs_cssize - i < sblock.fs_bsize ?
		    sblock.fs_cssize - i : sblock.fs_bsize);
	}
	sblock.fs_ronly = 0;
	sblock.fs_fmod = 0;
	sbdirty();
}

findino(idesc)
	struct inodesc *idesc;
{
	register DIRECT *dirp = idesc->id_dirp;

	if (dirp->d_ino == 0)
		return (KEEPON);
	if (!strcmp(dirp->d_name, srchname)) {
		if (dirp->d_ino >= ROOTINO && dirp->d_ino <= imax)
			idesc->id_parent = dirp->d_ino;
		return (STOP);
	}
	return (KEEPON);
}

mkentry(idesc)
	struct inodesc *idesc;
{
	register DIRECT *dirp = idesc->id_dirp;
	DIRECT newent;
	int newlen, oldlen;

	newent.d_namlen = 11;
	newlen = DIRSIZ(&newent);
	if (dirp->d_ino != 0)
		oldlen = DIRSIZ(dirp);
	else
		oldlen = 0;
	if (dirp->d_reclen - oldlen < newlen)
		return (KEEPON);
	newent.d_reclen = dirp->d_reclen - oldlen;
	dirp->d_reclen = oldlen;
	dirp = (struct direct *)(((char *)dirp) + oldlen);
	dirp->d_ino = idesc->id_parent;	/* ino to be entered is in id_parent */
	dirp->d_reclen = newent.d_reclen;
	dirp->d_namlen = lftempname(dirp->d_name, idesc->id_parent);
	return (ALTERED|STOP);
}

chgdd(idesc)
	struct inodesc *idesc;
{
	register DIRECT *dirp = idesc->id_dirp;

	if (dirp->d_name[0] == '.' && dirp->d_name[1] == '.' &&
	dirp->d_name[2] == 0) {
		dirp->d_ino = lfdir;
		return (ALTERED|STOP);
	}
	return (KEEPON);
}

linkup(orphan, pdir)
	ino_t orphan;
	ino_t pdir;
{
	register DINODE *dp;
	int lostdir, len;
	struct inodesc idesc;

	bzero((char *)&idesc, sizeof(struct inodesc));
	if ((dp = ginode(orphan)) == NULL)
		return (0);
	lostdir = DIRCT;
	pwarn("UNREF %s ", lostdir ? "DIR" : "FILE");
	pinode(orphan);
	if (preen && dp->di_size == 0)
		return (0);
	if (preen)
		printf(" (RECONNECTED)\n");
	else
		if (reply("RECONNECT") == 0)
			return (0);
	pathp = pathname;
	*pathp++ = '/';
	*pathp = '\0';
	if (lfdir == 0) {
		if ((dp = ginode(ROOTINO)) == NULL)
			return (0);
		srchname = lfname;
		idesc.id_type = DATA;
		idesc.id_func = findino;
		idesc.id_number = ROOTINO;
		idesc.id_filesize = dp->di_size;
		(void)ckinode(dp, &idesc);
		if ((lfdir = idesc.id_parent) == 0) {
			pfatal("SORRY. NO lost+found DIRECTORY");
			printf("\n\n");
			return (0);
		}
	}
	if ((dp = ginode(lfdir)) == NULL ||
	     !DIRCT || statemap[lfdir] != FSTATE) {
		pfatal("SORRY. NO lost+found DIRECTORY");
		printf("\n\n");
		return (0);
	}
	if (fragoff(&sblock, dp->di_size)) {
		dp->di_size = fragroundup(&sblock, dp->di_size);
		inodirty();
	}
	len = strlen(lfname);
	bcopy(lfname, pathp, len + 1);
	pathp += len;
	idesc.id_type = DATA;
	idesc.id_func = mkentry;
	idesc.id_number = lfdir;
	idesc.id_filesize = dp->di_size;
	idesc.id_parent = orphan;	/* this is the inode to enter */
	idesc.id_fix = DONTKNOW;
	if ((ckinode(dp, &idesc) & ALTERED) == 0) {
		pfatal("SORRY. NO SPACE IN lost+found DIRECTORY");
		printf("\n\n");
		return (0);
	}
	lncntp[orphan]--;
	*pathp++ = '/';
	pathp += lftempname(pathp, orphan);
	if (lostdir) {
		dp = ginode(orphan);
		idesc.id_type = DATA;
		idesc.id_func = chgdd;
		idesc.id_number = orphan;
		idesc.id_filesize = dp->di_size;
		idesc.id_fix = DONTKNOW;
		(void)ckinode(dp, &idesc);
		if ((dp = ginode(lfdir)) != NULL) {
			dp->di_nlink++;
			inodirty();
			lncntp[lfdir]++;
		}
		pwarn("DIR I=%u CONNECTED. ", orphan);
		printf("PARENT WAS I=%u\n", pdir);
		if (preen == 0)
			printf("\n");
	}
	return (1);
}

/*
 * generate a temporary name for the lost+found directory.
 */
lftempname(bufp, ino)
	char *bufp;
	ino_t ino;
{
	register ino_t in;
	register char *cp;
	int namlen;

	cp = bufp + 2;
	for (in = imax; in > 0; in /= 10)
		cp++;
	*--cp = 0;
	namlen = cp - bufp;
	in = ino;
	while (cp > bufp) {
		*--cp = (in % 10) + '0';
		in /= 10;
	}
	*cp = '#';
	return (namlen);
}

bread(fcp, buf, blk, size)
	register struct filecntl *fcp;
	char *buf;
	daddr_t blk;
	long size;
{
	if (lseek(fcp->rfdes, (long)dbtob(blk), 0) < 0)
		rwerr("SEEK", blk);
	else if (read(fcp->rfdes, buf, (int)size) == size)
		return (1);
	rwerr("READ", blk);
	return (0);
}

bwrite(fcp, buf, blk, size)
	register struct filecntl *fcp;
	char *buf;
	daddr_t blk;
	long size;
{

	if (fcp->wfdes < 0)
		return (0);
	if (lseek(fcp->wfdes, (long)dbtob(blk), 0) < 0)
		rwerr("SEEK", blk);
	else if (write(fcp->wfdes, buf, (int)size) == size) {
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
	(void)strcpy(dp+1, dp+2);
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
	(void)strcpy(rawbuf, cp);
	*dp = '/';
	(void)strcat(rawbuf, "/r");
	(void)strcat(rawbuf, dp+1);
	return (rawbuf);
}

/*
 * determine whether an inode should be fixed.
 */
dofix(idesc)
	register struct inodesc *idesc;
{

	switch (idesc->id_fix) {

	case DONTKNOW:
		direrr(idesc->id_number, "DIRECTORY CORRUPTED");
		if (reply("SALVAGE") == 0) {
			idesc->id_fix = NOFIX;
			return (0);
		}
		idesc->id_fix = FIX;
		return (ALTERED);

	case FIX:
		return (ALTERED);

	case NOFIX:
		return (0);

	default:
		errexit("UNKNOWN INODESC FIX MODE %d\n", idesc->id_fix);
	}
	/* NOTREACHED */
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
 * Die if preening, otherwise just printf.
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
pwarn(s, a1, a2, a3, a4, a5, a6)
	char *s;
{

	if (preen)
		printf("%s: ", devname);
	printf(s, a1, a2, a3, a4, a5, a6);
}

#ifndef lint
/*
 * Stub for routines from kernel.
 */
panic(s)
	char *s;
{

	pfatal("INTERNAL INCONSISTENCY: %s\n", s);
	exit(12);
}
#endif
