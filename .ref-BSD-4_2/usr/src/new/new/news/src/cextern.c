/*
 * Storage (somewhat) used by checknews
 */

#include	"iparams.h"


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
int	ROOTID;				/* special users id # */
#ifdef NOTIFY
char	TELLFILE[BUFLEN];		/* notify the user in this file */
char	TELLME[BUFLEN];			/* the user to tell */
#endif NOTIFY
