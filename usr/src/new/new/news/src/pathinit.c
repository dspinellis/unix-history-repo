/*
 * This function initializes all the strings used for the various
 * filenames.  They cannot be compiled into the program, since that
 * would be non-portable.  With this convention, the netnews sub-system
 * can be owned by any non-priviledged user.  It is also possible
 * to work when the administration randomly moves users from one
 * filesystem to another.  The convention is that a particular user
 * (HOME, see Makefile) is searched for in /etc/passwd and all files
 * are presumed relative to there.  This method also allows one copy
 * of the object code to be used on ANY machine.  (this code runs
 * un-modified on 50+ machines at IH!!)
 *
 * The disadvantage to using this method is that all netnews programs
 * (inews, readnews, rnews, checknews) must first search /etc/passwd
 * before they can start up.  This can cause significant overhead if
 * you have a big password file.
 *
 * Some games are played with ifdefs to get three .o files out of this
 * one source file.  INEW is defined for inews, READ for readnews,
 * and CHKN for checknews.
 */
static char *sccsid = "@(#)pathinit.c	1.2	3/30/83";

#ifdef INEW
#include	"iparams.h"
#endif

#ifdef READ
#include	"rparams.h"
extern char	MAILPARSER[];
#endif

#ifdef CHKN
#include <stdio.h>
extern char	SPOOL[];
extern char	LIB[];
extern char	ACTIVE[];
#endif

#ifdef EXP
#include <stdio.h>
extern char	SPOOL[];
extern char	LIB[];
extern char	ACTIVE[];
extern char	OLDNEWS[];
#endif

struct passwd *getpwname();
char *rindex();

pathinit()
{
	struct passwd	*pw;	/* struct for pw lookup			*/
	FILE	*nfd;		/* notify file descriptor		*/
	char *p;
#if INEW || READ
	struct utsname ubuf;

	uname(&ubuf);
	strcpy(FULLSYSNAME, ubuf.nodename);
	strcpy(SYSNAME, ubuf.nodename);
	SYSNAME[SNLN] = '\0';
#endif

#ifdef HOME
	/* Relative to the home directory of user HOME */
	sprintf(SPOOL, "%s/%s", logdir(HOME), SPOOLDIR);
	sprintf(LIB, "%s/%s", logdir(HOME), LIBDIR);
#else
	/* Fixed paths defined in Makefile */
	strcpy(SPOOL, SPOOLDIR);
	strcpy(LIB, LIBDIR);
#endif

	sprintf(ACTIVE, "%s/active", LIB);

#ifdef EXP
	strcpy(OLDNEWS, SPOOL);
	p = rindex(OLDNEWS, '/');
	if (p)
		strcpy(++p, "oldnews");
#endif

#if INEW || READ
	sprintf(LOCKFILE, "%s/LOCK", LIB);
	sprintf(SEQFILE, "%s/seq", LIB);
	sprintf(NGFILE, "%s/ngfile", LIB);
	sprintf(SUBFILE, "%s/sys", LIB);
	sprintf(ARTFILE, "%s/history", LIB);
# ifdef INEW
	sprintf(ARTICLE, "%s/.arXXXXXX", SPOOL);
	sprintf(INFILE, "%s/.inXXXXXX", SPOOL);
	sprintf(logfname, "%s/log", LIB);
	sprintf(STASH, "%s/junk", LIB);
# endif
# ifdef READ
	sprintf(HELPFILE, "%s/help", LIB);
	sprintf(CAESAR, "%s/caesar", LIB);
	sprintf(USERS, "%s/users", LIB);
#ifdef SENDMAIL
	strcpy(MAILPARSER, SENDMAIL);	/* SENDMAIL includes -t */
#else
	sprintf(MAILPARSER, "%s/recmail", LIB);
#endif
# endif

/*
 * The person notified by the netnews sub-system.  Again, no name is
 * compiled in, but instead the information is taken from a file.
 * If the file does not exist, a "default" person will get the mail.
 * If the file exists, but is empty, nobody will get the mail.  This
 * may seem backwards, but is a better fail-safe.
 */
# ifdef NOTIFY
	sprintf(TELLFILE, "%s/notify", LIB);
	nfd = fopen(TELLFILE, "r");
	if (nfd == NULL)
		sprintf(TELLME, "%s", NOTIFY);
	else if (fscanf(nfd, "%s", TELLME) == EOF)
		strcpy(TELLME, "");
# endif

/*
 * Since the netnews owner's id number is different on different
 * systems, we'll extract it from the /etc/passwd file.  If no entry,
 * default to root.  This id number seems to only be used to control who
 * can input certain control messages or cancel any message.  Note that
 * entry is the name from the "notify" file as found above if possible.
 * Defining ROOTID in defs.h hardwires in a number and avoids
 * another search of /etc/passwd.
 */
# ifndef ROOTID
	if ((pw = getpwname(TELLME)) != NULL)
		ROOTID =  pw->pw_uid;
	else if ((pw = getpwname(HOME)) != NULL)
		ROOTID =  pw->pw_uid;
	else
		ROOTID = 0;		/* nobody left, let only root */
# endif ROOTID
#endif INEW || READ
}
