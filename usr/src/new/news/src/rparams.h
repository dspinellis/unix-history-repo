/*
 * rparams.h - parameters for readnews, rfuncs, and readr.
 */

/*	@(#)rparams.h	2.23	10/23/86	*/

#include "params.h"

/* flags for readnews */
#define pflag	options[0].flag
#define tflag	options[1].flag
#define aflag	options[2].flag
#define nflag	options[3].flag
#define cflag	options[4].flag
#define lflag	options[5].flag
#define rflag	options[6].flag
#define sflag	options[7].flag
#define xflag	options[8].flag
#define hflag	options[9].flag
#define Mflag	options[10].flag
#define fflag	options[11].flag
#define uflag	options[12].flag
#define eflag	options[13].flag
#define Kflag	options[14].flag

#define	NEXT	0
#define SPEC	1

#define	FORWARD	0
#define BACKWARD 1

#define UNKNOWN 0001	/* possible modes for news program */
#define MAIL	0004
#define ANY	0007

struct optable {			/* options table. */
	char	optlet;		/* option character. */
	char	filchar;	/* if to pickup string, fill character. */
	int	flag;		/* TRUE if have seen this opt. */
	int	newstate;	/* STRING if takes arg, else OPTION */
	int	oldmode;	/* OR of legal input modes. */
	int	newmode;	/* output mode. */
	char	*buf;		/* string buffer */
};

/* external declarations specific to readnews */
extern	char	*infile, *outfile, *PAGER, *ALIASES;
extern	char	*bitmap, *MAILER, *MAILPARSER;

#ifndef ROOTID
extern	int	ROOTID;
#endif

#ifdef NOTIFY
extern	char	*TELLME;
#endif

extern char	filename[],coptbuf[],datebuf[],afline[];
extern char	newsrc[],groupdir[],rcbuf[],*rcline[],*argvrc[];
extern int	mode, ngrp, line, newrc(), readmode, news;
extern long	bit, obit, last, ngsize, minartno;
extern FILE	*rcfp,*actfp;
extern time_t	atime;
extern struct optable *optpt, options[];
extern int	actdirect, rcreadok, zapng;

#ifndef lint
/* lint gets very mad about i-minartno, this is one way of shutting it up */
/* macros */
#define get(i)	((i<minartno)? 0 : (bitmap[(i-minartno) >> 3] & (1 << (i-minartno) % 8)))
#define set(i)	if (i>=minartno) bitmap[(i-minartno) >> 3] |= (1 << (i-minartno) % 8);else
#define clear(i) if (i>=minartno) bitmap[(i-minartno) >> 3] &= ~(1 << (i-minartno) % 8);else
#endif /* !lint */

#define FCLOSE(fp)	{if (fp != NULL) {fclose(fp);fp = NULL;}}
