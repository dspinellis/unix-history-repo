/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)history.h	1.1 */

/*
 *	UNIX shell
 *	Header File for history mechanism
 *	written by David Korn
 *
 */



/* structure for fix command files */

#define HIS_DFLT	128		/* default size of history list */
#define HISMAX		(sizeof(int)*BUFSIZ)
#define HISBIG		(0100000-1024)	/* 1K less than maximum short */
#define HISLINE		16		/* estimate of average sized history line */
#define MAXLINE		258		/* longest history line permitted */

#define H_UNDO		0201		/* invalidate previous command */
#define H_CMDNO		0202		/* next 3 bytes give command number */
#define H_VERSION	1		/* history file format version no. */

struct fixcmd
{
	FILE	*fixfd;			/* file descriptor for history file */
	int	fixind;			/* current command number index */
	long	fixcnt;			/* offset into history file */
	int	fixmax;			/* number of accessible history lines */
	int	fixline;		/* line number within command */
	long	fixcmds[1];		/* byte offset for recent commands */
};

typedef struct
{
	short his_command;
	short his_line;
} histloc; 

extern struct fixcmd	*fc_fix;
extern void hist_close();

#ifndef KSHELL
#define FCIO	19
typedef char	MSG[];
extern char *getenv();
#define valup(s)		getenv("s")

# ifdef BSD
# define	strchr	index
# define	strrchr	rindex
# endif /* BSD */

#define NIL	((char*)0)
#define	setalt	e_setalt
#define setraw	e_setraw
#define setcooked e_setcooked
#define failed	e_failed
#define	hread	emacs_read
#define vread	vi_read
#define movstr	e_movstr
#define ungetchar	e_ungetchar
#endif	/* KSHELL */

/* the following are readonly */
extern MSG	histfname;
extern MSG	nohistory;
extern MSG	badcooked;
