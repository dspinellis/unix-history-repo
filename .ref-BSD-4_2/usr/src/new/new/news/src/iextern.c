/*
 * iextern - external definitions for inews.
 */

static char *SccsId = "@(#)iextern.c	2.4	3/27/83";

#include "iparams.h"

/*
 * The following definitions are only effective if they are not previously
 * defined in the makefile.
 */

#ifndef SPOOLDIR
/* NOTE:  The following line does NOT have a trailing quote. */
#define SPOOLDIR "/usr/spool/news
#endif

#ifndef LIBDIR
/* NOTE:  The following line does NOT have a trailing quote. */
#define LIBDIR "/usr/lib/news
#endif

#ifndef NEWSUSR
#define NEWSUSR "daemon"
#endif

#ifndef NEWSGRP
#define NEWSGRP "daemon"
#endif

unsigned uid,gid;			/* real user/group I.D. */
unsigned duid,dgid;			/* effective user/group I.D. */
int	sigtrap;			/* set if signal trapped */
int	savmask;			/* old umask */
int	mode;				/* mode of news program */
int	lockcount = 0;			/* no. of times we've called lock */
struct hbuf header;			/* general-use header structure */
char	bfr[LBUFLEN];			/* general-use scratch area */
char	filename[NAMELEN];		/* general-use file name */
char	username[BUFLEN];		/* user's login name */
char	userhome[BUFLEN];		/* user's home directory */

char	SPOOL[BUFLEN];			/* spool directory */
char	LIB[BUFLEN];			/* library directory */
char	INFILE[BUFLEN];			/* template for temp of stdin */
char	LOCKFILE[BUFLEN];		/* system lock file */
char	SEQFILE[BUFLEN];		/* system sequence no. file */
char	ACTIVE[BUFLEN];			/* active newsgroups file */
char	NGFILE[BUFLEN];			/* legal newsgroups file */
char	SUBFILE[BUFLEN];		/* system subscriptions */
char	ARTFILE[BUFLEN];		/* all articles seen */
char	ARTICLE[BUFLEN];		/* temporary article */
char	STASH[BUFLEN];			/* dir for un-recognized articles */
char	logfname[BUFLEN];		/* the log file */
#ifndef ROOTID
int	ROOTID;				/* special users id # */
#endif
#ifdef NOTIFY
char	TELLFILE[BUFLEN];		/* notify the user in this file */
char	TELLME[BUFLEN];			/* the user to tell */
#endif NOTIFY

char	SYSNAME[BUFLEN];		/* truncated system name */
char	FULLSYSNAME[BUFLEN];		/* full system name */
char	*NEWSU = NEWSUSR;		/* login name for netnews */
char	*NEWSG = NEWSGRP;		/* group name for netnews */
char	*DFLTNG = "general";		/* default newsgroup */
char	whatever[BUFLEN];		/* don't ask:  kludge */
char	nbuf[LBUFLEN];			/* local newsgroup buffer */
FILE	*infp;				/* input file-pointer */
FILE	*actfp;				/* active newsgroups file pointer */
int	tty;				/* set if infp is a tty */
char	*PARTIAL = "dead.article";	/* place to save partial news */
int	flag = FALSE;			/* set if at least one group O.K. */
char	*SHELL = "/bin/sh";		/* shell for inews to use	*/
int	defexp = FALSE;			/* set if def. expiration date used */
int	is_ctl;				/* true for a control message */
