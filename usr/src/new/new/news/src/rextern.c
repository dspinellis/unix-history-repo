/*
 * rextern - external definitions for readnews
 */

static char	*SccsId = "@(#)rextern.c	2.5	3/30/83";

#include "rparams.h"

/*
 * The following definitions are only effective if they are not previously
 * defined in the makefile.
 */

#ifndef SPOOLDIR
/* NOTE:  The following line does NOT have a trailing quote. */
#define SPOOLDIR "/xa/glickman/news/spool
#endif

#ifndef LIBDIR
/* NOTE:  The following line does NOT have a trailing quote. */
#define LIBDIR "/xa/glickman/news/lib
#endif

#ifndef NEWSUSR
#define NEWSUSR "daemon"
#endif

#ifndef NEWSGRP
#define NEWSGRP "daemon"
#endif

unsigned	uid, gid;		/* real user/group I.D.		*/
unsigned	duid, dgid;		/* effective user/group I.D.	*/
int	sigtrap;			/* set if signal trapped	*/
int	savmask;			/* old umask			*/
int	mode;				/* mode of news program		*/
struct hbuf header;			/* general-use header structure	*/
char	bfr[LBUFLEN];			/* general-use scratch area	*/
char	username[BUFLEN];		/* user's login name		*/
char	userhome[BUFLEN];		/* user's home directory	*/

char	SPOOL[BUFLEN];			/* spool directory		*/
char	LIB[BUFLEN];			/* library directory		*/
char	ACTIVE[BUFLEN];			/* active newsgroups file	*/
char	NGFILE[BUFLEN];			/* legal newsgroups file	*/
char	LOCKFILE[BUFLEN];		/* system lock file		*/
char	SEQFILE[BUFLEN];		/* system sequence no. file	*/
char	SUBFILE[BUFLEN];		/* system subscriptions		*/
char	USERS[BUFLEN];			/* user file			*/
char	HELPFILE[BUFLEN];		/* /bin/mail help		*/
char	CAESAR[BUFLEN];			/* decryptor program		*/
char	MAILPARSER[BUFLEN];		/* mail header parser mailer	*/
#ifndef ROOTID
int	ROOTID;				/* special users id #		*/
#endif
#ifdef NOTIFY
char	TELLFILE[BUFLEN];		/* notify the user in this file */
char	TELLME[BUFLEN];			/* the user to tell		*/
#endif

char	*outfile = "/tmp/M1XXXXXX";	/* output file for -M and -c	*/
char	*infile = "/tmp/M2XXXXXX";	/* -T output from Mail		*/
char	FULLSYSNAME[BUFLEN];		/* system name			*/
char	SYSNAME[BUFLEN];		/* system name chopped at 8	*/
char	ARTFILE[BUFLEN];		/* list of articles on system	*/
char	*NEWSU = NEWSUSR;		/* login name for netnews	*/
char	*NEWSG = NEWSGRP;		/* group name for netnews	*/
int	ngrp, last, line = -1;

char	filename[BUFLEN], coptbuf[BUFLEN], datebuf[BUFLEN];
char	titlebuf[BUFLEN];
char	afline[BUFLEN];
FILE	*rcfp, *actfp;
time_t	atime;
char	newsrc[BUFLEN], groupdir[BUFLEN], *rcline[LINES], rcbuf[LBUFLEN];
char	bitmap[1024], *argvrc[LINES];
int	bit, obit, readmode = NEXT;
int	defexp = FALSE;		/* set if def. expiration date used */
int	actdirect = FORWARD;	/* read direction in ACTIVE file */
int	rcreadok = FALSE;	/* NEWSRC has been read OK */
int	zapng = FALSE;		/* ! out this newsgroup on next updaterc */
long	ngsize;			/* max article # in this newsgroup */
struct stat statbuf;

#ifndef SHELL
char	*SHELL;
#endif

#ifndef MAILER
char	*MAILER;
#endif

char	PAGER[BUFSIZ];
