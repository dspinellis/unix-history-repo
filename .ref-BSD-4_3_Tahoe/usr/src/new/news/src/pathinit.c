/*
 * This software is Copyright (c) 1986 by Rick Adams.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or
 * otherwise use this software as long as: there is no monetary
 * profit gained specifically from the use or reproduction or this
 * software, it is not sold, rented, traded or otherwise marketed, and
 * this copyright notice is included prominently in any copy
 * made.
 *
 * The author make no claims as to the fitness or correctness of
 * this software for any use whatsoever, and it is provided as is. 
 * Any use of this software is at the user's own risk.
 *
 * This function initializes all the strings used for the various
 * filenames.  They cannot be compiled into the program, since that
 * would be non-portable.  With this convention, the netnews sub-system
 * can be owned by any non-privileged user.  It is also possible
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
 * Some games are played with ifdefs to get four .o files out of this
 * one source file.  INEW is defined for inews, READ for readnews,
 * CHKN for checknews, and EXP for expire.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)pathinit.c	1.24	10/7/87";
#endif /* SCCSID */

#if defined(INEW) || defined(EXP)
#include	"iparams.h"
#endif /* INEW || EXP */

#ifdef READ
#include	"rparams.h"
#endif /* READ */

#if defined(CHKN)
#include "params.h"
#endif /* CHKN */


char *FROMSYSNAME, *PATHSYSNAME, *LOCALSYSNAME, *LOCALPATHSYSNAME;
char *SPOOL, *LIB, *BIN, *ACTIVE, *SUBFILE, *ARTFILE,
	*username = "Unknown", *userhome;

#ifdef INEW
char *LOCKFILE, *SEQFILE, *ARTICLE, *INFILE, *TELLME;

int c_cancel(), c_newgroup(), c_ihave(), c_sendme(), c_rmgroup(),
    c_sendsys(), c_version(), c_checkgroups(), c_unimp();

struct msgtype msgtype[] = {
	"cancel", NULL, c_cancel,
	"newgroup", NULL, c_newgroup,
	"ihave", NULL, c_ihave,
	"sendme", NULL, c_sendme,
	"sendbad", NULL, c_sendme,
	"rmgroup", NULL, c_rmgroup,
	"sendsys", NULL, c_sendsys,
	"version", NULL, c_version,
	"checkgroups", NULL, c_checkgroups,
	"delsub", NULL, c_unimp,
	NULL, NULL, NULL
};
#endif /* INEW */

#if defined(INEW) || defined(READ)
char *ALIASES;
#endif /* INEW || READ */

#ifdef EXP
char *OLDNEWS;
#endif /* EXP */

#ifdef READ
char *MAILPARSER;
#endif /* READ */


struct passwd *getpwnam();
char *rindex();

#define Sprintf(where,fmt,arg)	(void) sprintf(bfr,fmt,arg); where = AllocCpy(bfr)

char *
AllocCpy(cp)
register char *cp;
{
	register char *mp;
	char *malloc();

	mp = malloc((unsigned)strlen(cp) + 1);

	if (mp == NULL)
		xerror("malloc failed on %s", cp);

	(void) strcpy(mp, cp);
	return mp;
}

pathinit()
{
#ifndef ROOTID
	struct passwd	*pw;	/* struct for pw lookup	*/
#endif /* !ROOTID */
#ifdef EXP
	char *p;
#endif /* EXP */
#ifndef CHKN
	struct utsname ubuf;
	char buf[BUFLEN];
	extern char *mydomain();

	uname(&ubuf);

#ifdef HIDDENNET_IN_LOCALSYSNAME
	/* old compatibility code, remove when HIDDENNET is used no more */
	if (STRCMP(ubuf.nodename, HIDDENNET) != 0)
		(void) sprintf(buf, "%s.%s%s", ubuf.nodename, HIDDENNET,
			mydomain());
	else
#endif
		(void) sprintf(buf, "%s%s", ubuf.nodename, mydomain());
	LOCALSYSNAME = AllocCpy(buf);

#ifdef GENERICFROM
	(void) sprintf(buf, GENERICFROM, ubuf.nodename, mydomain());
	FROMSYSNAME = AllocCpy(buf);
#else /* !GENERICFROM */
	FROMSYSNAME = LOCALSYSNAME;
#endif /* !GENERICFROM */

	LOCALPATHSYSNAME = AllocCpy(ubuf.nodename);

#ifdef GENERICPATH
	(void) sprintf(buf, GENERICPATH, ubuf.nodename, mydomain());
	PATHSYSNAME = AllocCpy(buf);
#else	/* !GENERICPATH */
	PATHSYSNAME = LOCALPATHSYSNAME;
#endif	/* !GENERICPATH */

#endif /* !CHKN */

#ifdef HOME
	/* Relative to the home directory of user HOME */
	(void) sprintf(bfr, "%s/%s", logdir(HOME), SPOOLDIR);
	SPOOL = AllocCpy(bfr);
	(void) sprintf(bfr, "%s/%s", logdir(HOME), LIBDIR);
	LIB = AllocCpy(bfr);
#else /* !HOME */
	/* Fixed paths defined in Makefile */
	SPOOL = AllocCpy(SPOOLDIR);
	LIB = AllocCpy(LIBDIR);
#endif /* !HOME */

#ifdef LOGDIR
	(void) sprintf(bfr, "%s/%s", logdir(HOME), BINDIR);
	BIN = AllocCpy(bfr);
#else /* !LOGDIR */
	Sprintf(BIN, "%s", BINDIR);
#endif /* !LOGDIR */

	Sprintf(ACTIVE, "%s/active", LIB);

#ifdef EXP
	(void) strcpy(bfr, SPOOL);
	p = rindex(bfr, '/');
	if (p) {
		strcpy(++p, "oldnews");
		OLDNEWS = AllocCpy(bfr);
	} else
		OLDNEWS = AllocCpy("oldnews");
#endif /* EXP */

#ifndef CHKN
	Sprintf(SUBFILE, "%s/sys", LIB);
	Sprintf(ARTFILE, "%s/history", LIB);
# endif /* !CHKN */

# ifdef READ
#ifdef SENDMAIL
	Sprintf(MAILPARSER, "%s -oi -oem", SENDMAIL);
#else /* !SENDMAIL */
	Sprintf(MAILPARSER, "%s/recmail", LIB);
#endif /* !SENDMAIL */
# endif /* READ */

# if defined(READ) || defined(INEW)
	Sprintf(ALIASES, "%s/aliases", LIB);
# endif /* READ || INEW */
# ifdef INEW
	Sprintf(LOCKFILE, "%s/LOCK", LIB);
	Sprintf(SEQFILE, "%s/seq", LIB);
	Sprintf(ARTICLE, "%s/.arXXXXXX", SPOOL);
	Sprintf(INFILE, "%s/.inXXXXXX", SPOOL);
/*
 * The person notified by the netnews sub-system.  Again, no name is
 * compiled in, but instead the information is taken from a file.
 * If the file does not exist, a "default" person will get the mail.
 * If the file exists, but is empty, nobody will get the mail.  This
 * may seem backwards, but is a better fail-safe.
 */
# ifdef NOTIFY
	parse_notify();
# endif /* NOTIFY */

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
	if ((pw = getpwnam(TELLME)) != NULL)
		ROOTID =  pw->pw_uid;
	else if ((pw = getpwnam(HOME)) != NULL)
		ROOTID =  pw->pw_uid;
	else
		ROOTID = 0;		/* nobody left, let only root */
# endif /* !ROOTID */
#endif /* INEW */
}

#ifdef INEW
#ifdef NOTIFY
/*
 * Attempt to parse the LIB/notify file into the global structure msgtype[].
 */
parse_notify()
{
	FILE *nfd;
	int valid = 0, done = 0;
	register struct msgtype *mp;
	char mtype[BUFLEN], addr[BUFLEN];

	(void) sprintf(bfr, "%s/notify", LIB);
#ifndef ROOTID
	TELLME = AllocCpy(NOTIFY);	
#endif /* !ROOTID */
	if ( (nfd = fopen(bfr, "r")) == NULL) {
		/* 
		 * Set defaults to NOTIFY
		 */
#ifdef debug
		log("parse_notify: %s/notify not found", LIB);
#endif /* debug */
		(void)setmsg("all", NOTIFY);
		return;
	}
	do  {
		mtype[0] = addr[0] = 0;
		switch( get_notify(nfd, mtype, addr) ) {
		case 0:
			continue;
		case 1:
			valid += setmsg(mtype, "");
			break;
		case 2:
			valid += setmsg(mtype, addr);
			break;
		case -1:
			if( !valid ) {
#ifdef debug
				log("parse_notify: no valid entries found.");
#endif /* debug */
				setmsg("all", ""); /* send mail to no one */
			}
			done = 1;
		}
	} while( !done );

	/*
	 * point to zero length string for all entries we haven't touched
	 */
	for(mp=msgtype; mp->m_name; mp++)
		if(mp->m_who_to == 0)
			mp->m_who_to = "";
}

setmsg(what, to)
char *what, *to;
{
	register struct msgtype *mp;
#ifdef debug
	log("setmsg: what='%s', to='%s'", what, to);
#endif /* debug */
	/*
	 * Special case for "all"
	 */
	if(STRCMP(what, "all") == 0) {
		for(mp=msgtype; mp->m_name; mp++) {
			mp->m_who_to = AllocCpy(to);
#ifdef debug
			log("setmsg: '%s'='%s'", mp->m_name, mp->m_who_to);
#endif /* debug */
		}
		return 1;
	}

	for(mp=msgtype; mp->m_name; mp++)
		if(STRCMP(mp->m_name, what) == 0) {
			mp->m_who_to = AllocCpy(to);
#ifdef debug
			log("setmsg: '%s'='%s'", mp->m_name, mp->m_who_to);
#endif /* debug */
			return 1;
		}
	return 0;
}

static
get_notify(fp, s, t)
FILE *fp;
register char *s, *t;
{
	register char *cp;
	char buf[BUFSIZ];

	if( cp=fgets(buf, sizeof(buf), fp ) ) {
		if( *cp == '\n' ) 
			return 0;
		while(*cp && *cp != ' ' && *cp != '\t' && *cp != '\n')
			*s++ = *cp++;
		*s = '\0';	/* terminate first string */

		while(*cp && (*cp == ' ' || *cp == '\t' || *cp == '\n') )
			cp++;	/* look for start of second */
		if( !*cp || *cp == '\n' )
			return 1; 	/* no second string */
		
		while( *cp && *cp != '\n' )
			*t++ = *cp++;
		*t = '\0';
		return 2;
	} else
		return -1;
}
#endif /* NOTIFY */
#endif /* INEW */

#ifndef CHKN
/*
 * At sites where the are many mail domains within the support area of a single
 * news administrator, it is much nicer to be able to read the local domain of
 * a machine from a file.  What we do here is:
 * 1)	Check for the presence of a LIBDIR/localdomain file.  If it doesn't 
 * 	exist,assume that MYDOMAIN should be used instead.
 * 2)	If it does exist, we make the following assumptions:
 *	a)  If it is empty, has only comments, or only blank lines; we assume
 *	    the domain is desired to be a zero length string ( ie "").  (this
 *	    implies that the domain name is contained in the hostname.)
 *	b)  If it is not empty, we assume the first line not beginning with a
 *	    '#', blank/tab, or newline is the desired domain name.
 *	    A like '.UUCP' or '.TEK.COM' should be used.  We could insure that
 *	    the line begin with a '.' to be a valid domain name, but I don't 
 *	    think it is necessary to put that restriction on it.
 */
char *
mydomain()
{
	static char *md = NULL;
	register char *cp;
	FILE *fp = NULL;
	char fbuf[BUFLEN];
	extern char *malloc(), *strcpy(), *index();

	if(md)	/* we've been here before, so just return what we found */
		return(md);

	(void) sprintf(fbuf,"%s/localdomain", LIBDIR);
	if ( (fp = fopen(fbuf,"r")) == NULL) {
		md = MYDOMAIN;	/* No localdomain file, use MYDOMAIN instead */
	} else {
		while(fgets(fbuf, sizeof(fbuf), fp) ) {
			if( *fbuf == '\n' || *fbuf == '#' 
			    || *fbuf == ' ' || *fbuf == '\t')
				continue;
	
			if( cp = index(fbuf, '\n') )
				*cp = '\0';
	
			if ( (md = malloc(strlen(fbuf) + 1)) == NULL)
				md = MYDOMAIN;	/* punt here */
			else
				(void)strcpy(md, fbuf);
			break;
		}
	}

	if(fp)
		(void)fclose(fp);

	if( md == NULL)
		md = "";
	
	return md;
}
#endif /* !CHKN */
