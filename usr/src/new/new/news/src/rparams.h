/*
 * rparams.h - parameters for readnews, rfuncs, and readr.
 */

static char *Rparams = "@(#)rparams.h	2.6	4/23/83";

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
extern	char	HELPFILE[], *infile, *outfile, PAGER[];
extern	char	bitmap[], *temprc, *MAILER, USERS[], CAESAR[];

#ifndef ROOTID
extern	int	ROOTID;
#endif

#ifdef NOTIFY
extern	char	TELLFILE[], TELLME[];
#endif

extern char	filename[],coptbuf[],datebuf[],titlebuf[],afline[];
extern char	newsrc[],groupdir[],rcbuf[],*rcline[],*argvrc[];
extern int	bit, obit, mode, ngrp, last, line, newrc(), readmode;
extern FILE	*rcfp,*actfp;
extern time_t	atime;
extern struct stat statbuf;
extern struct optable *optpt, options[];
extern int	actdirect, rcreadok, zapng;
extern long	ngsize;

/* macros */
#define get(i)	(bitmap[(i-1) >> 3] & (1 << (i-1) % 8))
#define set(i)	(bitmap[(i-1) >> 3] |= (1 << (i-1) % 8))
#define clear(i)	(bitmap[(i-1) >> 3] &= ~(1 << (i-1) % 8))
