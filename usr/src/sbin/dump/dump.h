/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dump.h	5.9 (Berkeley) %G%
 */

#define	NI		16
#define MAXINOPB	(MAXBSIZE / sizeof(struct dinode))
#define MAXNINDIR	(MAXBSIZE / sizeof(daddr_t))

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <ufs/fs.h>
#include <ufs/dinode.h>
#include <ufs/dir.h>
#include <protocols/dumprestore.h>
#include <utmp.h>
#include <signal.h>
#include <fstab.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define	MWORD(m,i)	(m[(unsigned)(i-1)/NBBY])
#define	MBIT(i)		(1<<((unsigned)(i-1)%NBBY))
#define	BIS(i,w)	(MWORD(w,i) |=  MBIT(i))
#define	BIC(i,w)	(MWORD(w,i) &= ~MBIT(i))
#define	BIT(i,w)	(MWORD(w,i) & MBIT(i))

int	msiz;
char	*clrmap;
char	*dirmap;
char	*nodmap;

/*
 *	All calculations done in 0.1" units!
 */

char	*disk;		/* name of the disk file */
char	*tape;		/* name of the tape file */
char	*increm;	/* name of the file containing incremental information*/
char	*temp;		/* name of the file for doing rewrite of increm */
char	lastincno;	/* increment number of previous dump */
char	incno;		/* increment number */
int	uflag;		/* update flag */
int	fi;		/* disk file descriptor */
int	to;		/* tape file descriptor */
int	pipeout;	/* true => output to standard output */
ino_t	ino;		/* current inumber; used globally */
int	nsubdir;
int	newtape;	/* new tape flag */
int	nadded;		/* number of added sub directories */
int	dadded;		/* directory added flag */
int	density;	/* density in 0.1" units */
long	tsize;		/* tape size in 0.1" units */
long	esize;		/* estimated tape size, blocks */
long	asize;		/* number of 0.1" units written on current tape */
int	etapes;		/* estimated number of tapes */

int	notify;		/* notify operator flag */
int	blockswritten;	/* number of blocks written on current tape */
int	tapeno;		/* current tape number */
time_t	tstart_writing;	/* when started writing the first tape block */
char	*processname;
struct fs *sblock;	/* the file system super block */
char	buf[MAXBSIZE];
long	dev_bsize;	/* block size of underlying disk device */
int	dev_bshift;	/* log2(dev_bsize) */
int	tp_bshift;	/* log2(TP_BSIZE) */

/* operator interface functions */
void	broadcast();
void	lastdump();
void	msg();
void	msgtail();
int	query();
void	set_operators();
void	timeest();

/* mapping rouintes */
void	est();
void	bmapest();
void	pass();
void	mark();
void	add();

/* file dumping routines */
void	dirdump();
void	dump();
void	blksout();
void	bitmap();
void	spclrec();
void	bread();

/* tape writing routines */
int	alloctape();
void	taprec();
void	dmpblk();
void	tflush();
void	trewind();
void	close_rewind();
void	otape();

void	dumpabort();
void	Exit();
void	getfstab();
void	quit();

char	*prdate();
char	*rawname();
struct dinode *getino();

void	interrupt();		/* in case operator bangs on console */

#define	HOUR	(60L*60L)
#define	DAY	(24L*HOUR)
#define	YEAR	(365L*DAY)

/*
 *	Exit status codes
 */
#define	X_FINOK		0	/* normal exit */
#define	X_REWRITE	2	/* restart writing from the check point */
#define	X_ABORT		3	/* abort all of dump; don't attempt checkpointing*/

#define	OPGRENT	"operator"		/* group entry to notify */
#define DIALUP	"ttyd"			/* prefix for dialups */

struct	fstab	*fstabsearch();	/* search in fs_file and fs_spec */

/*
 *	The contents of the file _PATH_DUMPDATES is maintained both on
 *	a linked list, and then (eventually) arrayified.
 */
struct	idates {
	char	id_name[MAXNAMLEN+3];
	char	id_incno;
	time_t	id_ddate;
};
struct	itime {
	struct	idates	it_value;
	struct	itime	*it_next;
};
struct	itime	*ithead;	/* head of the list version */
int	nidates;		/* number of records (might be zero) */
int	idates_in;		/* we have read the increment file */
struct	idates	**idatev;	/* the arrayfied version */
void	inititimes();
void	getitime();
void	putitime();
#define	ITITERATE(i, ip) for (ip = idatev[i = 0]; i < nidates; ip = idatev[++i])

/*
 *	We catch these interrupts
 */
void	sighup();
void	sigquit();
void	sigill();
void	sigtrap();
void	sigfpe();
void	sigkill();
void	sigbus();
void	sigsegv();
void	sigsys();
void	sigalrm();
void	sigterm();
