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
 * inews - insert, receive, and transmit news articles.
 *
 */

#ifdef SCCSID
static char	*SccsId = "@(#)inews.c	2.82	10/15/87";
#endif /* SCCSID */

#include "iparams.h"

# ifdef LOCKF
# include <unistd.h>
# include <fcntl.h>

# if defined(F_RDLCK) && defined(F_SETLK)
struct flock news_lock;
#  endif /* F_RDLCK  && F_SETLK */
# endif /* LOCKF */

#ifdef BSD4_2
# include <sys/file.h>
#else	/* !BSD4_2 */
# if defined(USG) && !defined(LOCKF)
# include <fcntl.h>
# endif /* USG */
#endif /* !BSD4_2 */
/* local defines for inews */

#define OPTION	0	/* pick up an option string */
#define STRING	1	/* pick up a string of arguments */

#define UNKNOWN 0001	/* possible modes for news program */
#define UNPROC	0002	/* Unprocessed input */
#define PROC	0004	/* Processed input */
#define	CONTROL	0010	/* Control Message */
#define	CREATENG 0020	/* Create a new newsgroup */

#define DONT_SPOOL	0
#define	DO_SPOOL	1
#define	EXPIRE_RUNNING	2
int spool_news = DONT_SPOOL;

extern char histline[];
char	forgedname[NAMELEN];	/* A user specified -f option. */
/* Fake sys line in case they forget their own system */
struct srec dummy_srec = { "MEMEME", "", "all", "", "" };

char *Progname = "inews";	/* used by xerror to identify failing program */

struct {			/* options table. */
	char	optlet;		/* option character. */
	char	filchar;	/* if to pickup string, fill character. */
	int	flag;		/* TRUE if have seen this opt. */
	int	oldmode;	/* OR of legal input modes. */
	int	newmode;	/* output mode. */
	char	*buf;		/* string buffer */
} *optpt, options[] = { /*
optlet	filchar		flag	oldmode	newmode		buf	*/
't',	' ',		FALSE,	UNPROC,	UNKNOWN,	header.title,
'n',	NGDELIM,	FALSE,	UNPROC,	UNKNOWN,	header.nbuf,
'd',	'\0',		FALSE,	UNPROC,	UNKNOWN,	header.distribution,
'e',	' ',		FALSE,	UNPROC,	UNKNOWN,	header.expdate,
'p',	'\0',		FALSE,	UNKNOWN|PROC,	PROC,	filename,
'f',	'\0',		FALSE,	UNPROC,	UNKNOWN,	forgedname,
'F',	' ',		FALSE,	UNPROC,	UNKNOWN,	header.followid,
'c',	' ',		FALSE,	UNKNOWN,UNKNOWN,	header.ctlmsg,
'C',	' ',		FALSE,	UNKNOWN,CREATENG,	header.ctlmsg,
#define hflag	options[9].flag
'h',	'\0',		FALSE,	UNPROC,	UNKNOWN,	filename,
#define oflag	options[10].flag
'o',	'\0',		FALSE,	UNPROC, UNKNOWN,	header.organization,
#define Mflag	options[11].flag
'M',	'\0',		FALSE,	UNPROC, UNKNOWN,	filename,
'a',	'\0',		FALSE,	UNPROC, UNKNOWN,	header.approved,
'U',	'\0',		FALSE,	PROC, PROC,		filename,
#define Sflag	options[14].flag
'S',	'\0',		FALSE,	UNKNOWN|PROC, 	UNPROC,	filename,
'x',	'\0',		FALSE,	UNPROC, UNKNOWN,	not_here,
'r',	'\0',		FALSE,	UNPROC, UNKNOWN,	header.replyto,
'\0',	'\0',		0,	0,	0,		(char *)NULL
};

FILE *mailhdr();
extern int errno;

struct timeb Now;

/*
 *	Authors:
 *		Matt Glickman	glickman@ucbarpa.Berkeley.ARPA
 *		Mark Horton	mark@cbosgd.UUCP
 *		Stephen Daniels	swd@mcnc.UUCP
 *		Tom Truscott	trt@duke.UUCP
 *		Rick Adams	rick@seismo.CSS.GOV
 *	IHCC version adapted by:
 *		Larry Marek	larry@ihuxf.UUCP
 */
main(argc, argv)
int	argc;
register char **argv;
{
	int	state;		/* which type of argument to pick up	*/
	int	tlen, len;	/* temps for string processing routine	*/
	register char *ptr;	/* pointer to rest of buffer		*/
	int	filchar;	/* fill character (state = STRING)	*/
	char	*user = NULL, *home = NULL;	/* environment temps	*/
	struct passwd	*pw;	/* struct for pw lookup			*/
	struct group	*gp;	/* struct for group lookup		*/
	register int	i;
	FILE	*mfd;		/* mail file file-descriptor		*/

	/* uuxqt doesn't close all its files */
	for (i = 3; !close(i); i++)
		;
	/* set up defaults and initialize. */
	mode = UNKNOWN;
	infp = stdin;
	pathinit();
	savmask = umask(N_UMASK);	/* set up mask */
	ptr = rindex(*argv, '/');
	if (!ptr)
		ptr = *argv - 1;
	actfp = xfopen(ACTIVE, "r+");
#ifdef	LOCKF
# if	defined(F_RDLCK) && defined(F_SETLK)
	news_lock.l_type = F_RDLCK;
	if (fcntl(fileno(actfp), F_SETLK, &news_lock) < 0) {
# else /* !F_RDLCK */
	if (lockf(fileno(actfp), F_TLOCK, 0L) < 0) {
# endif /* !F_RDLCK */
		if (errno != EAGAIN && errno != EACCES)
#else	/* !LOCKF */
#ifdef BSD4_2
	if (flock(fileno(actfp), LOCK_SH|LOCK_NB) < 0) {
		if (errno != EWOULDBLOCK)
#else	/* !BSD4_2 */
	sprintf(bfr, "%s.lock", ACTIVE);
	if (LINK(ACTIVE, bfr) < 0) {
		if (errno != EEXIST)
#endif /* V7 */
#endif	/* !BSD4_2 */
			xerror("Can't lock %s: %s", ACTIVE, errmsg(errno));
		spool_news = EXPIRE_RUNNING;
	} else {
#ifdef SPOOLNEWS
		if (argc > 1 && !strcmp(*(argv+1), "-S")) {
			argc--;
			argv++;
			Sflag = 1;
		} else
			spool_news = DO_SPOOL;

#endif /* SPOOLNEWS */
	}
	if (spool_news != EXPIRE_RUNNING) {
		/* only unlock if we locked */
#ifdef	LOCKF
		(void) lockf(fileno(actfp), F_ULOCK, 0L);
#else	/* !LOCKF */
#ifdef 	BSD4_2
		(void) flock(fileno(actfp), LOCK_UN);
#else	/* !BSD4_2 */
		(void) UNLINK(bfr);
#endif 	/* V7 */
#endif	/* !BSD4_2 */
	} else {	/* expire is running */
		if (argc > 1 && !strcmp(*(argv+1), "-S"))
			exit(42);	/* inform rnews -U by exit status */
	}
	if (argc > 1 && !strcmp(*(argv+1), "-U")) {
		/* can't unspool while things are locked */
		if (spool_news == EXPIRE_RUNNING)
			xxit(0);
		dounspool();
		/* NOT REACHED */
	}

	if (!STRNCMP(ptr+1, "rnews", 5)) {
		mode = PROC;
		if (spool_news != DONT_SPOOL) {
			dospool((char *)NULL, FALSE);
			/* NOT REACHED */
		}
#ifdef NICENESS
		if (nice(0) < NICENESS)
			(void) nice(NICENESS);
#endif /* NICENESS */
	} else {
	/* it's not rnews, so it must be inews */
		if (argc < 2)
			goto usage;
#ifndef SPOOLINEWS
		if (spool_news == DO_SPOOL)
			spool_news = DONT_SPOOL;
#endif /* SPOOLINEWS */
	}

	state = OPTION;
	header.title[0] = header.nbuf[0] = filename[0] = '\0';

	/* check for existence of special files */
#ifdef DBM
	chkfile(ARTFILE);
#else
	chkdir(ARTFILE);
#endif /* DBM */
	chkfile(ACTIVE);
	SigTrap = FALSE;	/* true if a signal has been caught */
	if (mode != PROC) {
		(void) signal(SIGHUP, onsig);
		(void) signal(SIGINT, onsig);
	}
	/*
	 * Catch "filesize exceeded" signals on 4.2BSD systems
	 * - the history files may exceed this limit.
	 */
#ifdef  SIGXFSZ
	(void) signal(SIGXFSZ, SIG_IGN);
#endif /* SIGXFSZ */
	uid = getuid();
	gid = getgid();
	duid = geteuid();
	dgid = getegid();
	(void) ftime(&Now);
	if (uid == 0 && duid == 0) {
		/*
		 * Must go through with this kludge since
		 * some systems do not honor the setuid bit
		 * when root invokes a setuid program.
		 */
		if ((pw = getpwnam(NEWSUSR)) == NULL)
			xerror("Cannot get NEWSU pw entry");

		duid = pw->pw_uid;
		if ((gp = getgrnam(NEWSGRP)) == NULL)
			xerror("Cannot get NEWSG gr entry");
		dgid = gp->gr_gid;
		(void) setgid(dgid);
		(void) setuid(duid);
	}

#ifndef DOGETUSER
	/*
	 * Force the use of 'getuser()' to prevent forgery of articles
	 * by just changing $LOGNAME
	 */
	if (isatty(fileno(stderr))) {
		if ((user = getenv("USER")) == NULL)
			user = getenv("LOGNAME");
		if ((home = getenv("HOME")) == NULL)
			home = getenv("LOGDIR");
	}
#endif /* !DOGETUSER */
	if (user == NULL || home == NULL)
		getuser();
	else {
		if (STRCMP(username, "Unknown") == 0 || username[0] == 0) {
			username = AllocCpy(user);
		}
		userhome = AllocCpy(home);
	}
	getuser();

	/* loop once per arg. */

	++argv;		/* skip first arg, which is prog name. */

	while (--argc) {
	    if (state == OPTION) {
		if (**argv != '-') {
			xerror("Bad option string \"%s\"", *argv);
		}
		while (*++*argv != '\0') {
			for (optpt = options; optpt->optlet != '\0'; ++optpt) {
				if (optpt->optlet == **argv)
					goto found;
			}
			/* unknown option letter */
usage:
			fprintf(stderr, "usage: inews -t title");
			fprintf(stderr, " [ -n newsgroups ]");
			fprintf(stderr, " [ -e expiration date ]\n");
			fprintf(stderr, "\t[ -f sender]\n\n");
			xxit(1);

		    found:;
			if (optpt->flag == TRUE || (mode != UNKNOWN &&
			    (mode&optpt->oldmode) == 0)) {
				xerror("Bad %c option", **argv);
			}
			if (mode == UNKNOWN)
				mode = optpt->newmode;
			filchar = optpt->filchar;
			optpt->flag = TRUE;
			state = STRING;
			ptr = optpt->buf;
			len = BUFLEN;
		}

		argv++;		/* done with this option arg. */

	    } else {

		/*
		 * Pick up a piece of a string and put it into
		 * the appropriate buffer.
		 */
		if (**argv == '-') {
			state = OPTION;
			argc++;	/* uncount this arg. */
			continue;
		}

		if ((tlen = strlen(*argv)) >= len)
			xerror("Argument string too long");
		(void) strcpy(ptr, *argv++);
		ptr += tlen;
		if (*(ptr-1) != filchar)
			*ptr++ = filchar;
		len -= tlen + 1;
		*ptr = '\0';
	    }
	}

	/*
	 * ALL of the command line has now been processed. (!)
	 */

	if (*filename) {
		infp = freopen(filename, "r", stdin);
		if (infp == NULL)
			xerror("freopen(%s): %s", filename, errmsg(errno));
	} else
		infp = stdin;

	tty = isatty(fileno(infp));

	if (mode == CREATENG)
		createng();

	if (header.ctlmsg[0] != '\0' && header.title[0] == '\0')
		(void) strcpy(header.title, header.ctlmsg);

	if (*header.nbuf) {
		lcase(header.nbuf);
		ptr = index(header.nbuf, '\0');
		if (ptr[-1] == NGDELIM)
			*--ptr = '\0';
	}
	(void) nstrip(header.title);
	(void) nstrip(header.expdate);
	(void) nstrip(header.followid);
	if (mode != PROC) {
		if (hflag) {
			header.path[0] = '\0';
			(void) hread(&header, infp, FALSE);
			/* there are certain fields we won't let him specify. */
			if (header.from[0]) {
				(void) fixfrom(&header);
				if (Sflag && !Mflag && !header.approved[0] &
					!header.sender[0]) {
					register char *p;
					strcpy(bfr, header.from);
					p  = strpbrk(bfr, "@ !");
					if (p)
						*p = '\0';
					if ((pw = getpwnam(bfr)) != NULL) {
						uid = pw->pw_uid;
						gid = pw->pw_gid;
						username = AllocCpy(bfr);
					}
				} else {
					(void) strcpy(forgedname, header.from);
					header.from[0] = '\0';
				}
			}
			if (!header.approved[0])
				Mflag = FALSE;
			header.sender[0] = '\0';
			if (header.subdate[0] && cgtdate(header.subdate) < 0)
				header.subdate[0] = '\0';
		}

		if (header.ident[0] == '\0')
			getident(&header);

		if (forgedname[0]) {
			register char *p1;
			if (Mflag)
				sprintf(header.path, "%s!%s",
					PATHSYSNAME, username);
			else if (!header.path[0]) {
				(void) strcpy(header.path, forgedname);

				if ((p1 = strpbrk(header.path, " (<")) != NULL)
					*p1 = '\0';
			}
			if (!Mflag && !strpbrk(forgedname, "@ (<"))
				(void) sprintf(header.from,"%s@%s",
					forgedname, FROMSYSNAME);
			else
				(void) strncpy(header.from, forgedname, BUFLEN);

			(void) sprintf(header.sender, "%s@%s",
				username, FROMSYSNAME);
		} else {
			gensender(&header, username);
		}
#ifdef MYORG
		if (header.organization[0] == '\0' && !Mflag &&
			header.sender[0] == '\0') {
			strncpy(header.organization, MYORG, BUFLEN);
			if (STRNCMP(header.organization, "Frobozz", 7) == 0)
				header.organization[0] = '\0';
			if (ptr = getenv("ORGANIZATION"))
				strncpy(header.organization, ptr, BUFLEN);
			/*
			 * Note that the organization can also be turned off by
			 * setting it to the null string, either in MYORG or
			 * $ORGANIZATION in the environment.
			 */
			if (header.organization[0] == '/') {
				mfd = fopen(header.organization, "r");
				if (mfd) {
					(void) fgets(header.organization, sizeof header.organization, mfd);
					(void) fclose(mfd);
				} else {
					logerr("Couldn't open %s",
						header.organization);
					header.organization[0] = '\0';
				}
				ptr = index(header.organization, '\n');
				if (ptr)
					*ptr = '\0';
			}
		}
#endif /* MYORG */
	}

	/* Authorize newsgroups. */
	if (mode == PROC) {
		checkbatch();
		(void) signal(SIGHUP, SIG_IGN);
		(void) signal(SIGINT, SIG_IGN);
		(void) signal(SIGQUIT, SIG_IGN);
		header.ident[0] = '\0';
		if (hread(&header, infp, TRUE) == NULL)
			xerror("%s: Inbound news is garbled", filename);
		input(bfr[0] != '\n');
	}
	/* always check history */

	if (history(&header)) {
		log("Duplicate article %s rejected. Path: %s",
			header.ident, header.path);
		xxit(0);
	}

	/* Easy way to make control messages, since all.all.ctl is unblessed */
	if (mode != PROC && PREFIX(header.title, "cmsg ") && header.ctlmsg[0] == 0)
		(void) strcpy(header.ctlmsg, &header.title[5]);
	is_ctl = mode != CREATENG &&
		(ngmatch(header.nbuf, "all.all.ctl,") || header.ctlmsg[0]);
#ifdef DEBUG
	fprintf(stderr,"is_ctl set to %d\n", is_ctl);
#endif

	if (mode != CREATENG) {
		if (!*header.title)
			error("No title, ng %s from %s", header.nbuf,
				header.from);
		if (!*header.nbuf)
			(void) strcpy(header.nbuf, DFLTNG);
	}

	if (mode <= UNPROC) {
#ifdef FASCIST
		if (uid && uid != ROOTID && fascist(username, header.nbuf))
			xerror("User %s is not authorized to post to newsgroup %s",
				username, header.nbuf);
#endif /* FASCIST */
		ctlcheck();
	}

	if (mode == CREATENG)
		createng();

	/* Determine input. */
	if (mode != PROC)
		input(FALSE);
	if (header.intnumlines == 0 && !is_ctl)
		error("%s rejected: no text lines", header.ident);

	dates(&header);

	/* Do the actual insertion. */
	insert();
	/* NOTREACHED */
}

/* check for existence of file */
static chkfile(f)
char *f;
{
	FILE	*mfd;		/* mail file file-descriptor		*/
	char	cbuf[BUFLEN];	/* command buffer			*/

	if (rwaccess(f))
		return;	/* everything is ok */
	mfd = mailhdr((struct hbuf *)NULL,
		exists(f) ? "Unwritable files!" : "Missing files!");
	if (mfd == NULL)
		return;
	putc('\n', mfd);
	fprintf(mfd, "System: %s\n\nThere was a problem with %s!!\n",
		LOCALSYSNAME, f);
	(void) sprintf(cbuf, "touch %s;chmod 666 %s", f, f);
	(void) system(cbuf);
	if (rwaccess(f))
		fprintf(mfd, "The problem has been taken care of.\n");
	else
		fprintf(mfd, "Corrective action failed - check suid bits.\n");
	(void) mclose(mfd);
}

#ifndef DBM
/* check for existence of directory */
static chkdir(d)
char *d;
{
	FILE	*mfd;		/* mail file file-descriptor		*/
	char	dir[BUFLEN];	/* holds directory name			*/

	sprintf(dir, "%s.d", d);
	if (eaccess(dir, 07) == 0)
		return; /* everything is ok */
	mfd = mailhdr((struct hbuf *)NULL,
		exists(dir) ? "Unwritable directories" : "Missing directories");
	if (mfd == NULL)
		return;
	putc('\n', mfd);
	fprintf(mfd, "System: %s\n\nThere was a problem with %s!\n",
		LOCALSYSNAME, dir);
	(void) mkdir(dir, 0775);
	if (eaccess(dir, 07) == 0)
		fprintf(mfd, "The problem has been taken care of.\n");
	else
		fprintf(mfd, "Corrective action failed - check suid bits.\n");
	(void) mclose(mfd);
}

/*
 * This version of access checks against effective uid and effective gid
 */
eaccess(name, mode)
register char *name;
register int mode;
{	
	struct stat statb;
	int euserid = geteuid();
	int egroupid = getegid();

	if (stat(name, &statb) == 0) {
		if (euserid == 0) {
			if ((statb.st_mode&S_IFMT) != S_IFREG || mode != 1)
				return 0;
		    	/* root needs execute permission for someone */
			mode = (S_IEXEC|(S_IEXEC>>3)|(S_IEXEC>>6));
		}
		else if (euserid == statb.st_uid)
			mode <<= 6;
		else if (egroupid == statb.st_gid)
			mode <<= 3;
#ifdef BSD4_2
		/* in BSD4_2 you can be in several groups */
		else {
			int groups[NGROUPS];
			register int n;
			n = getgroups(NGROUPS,groups);
			while(--n >= 0) {
				if(groups[n] == statb.st_gid) {
					mode <<= 3;
					break;
				}
			}
		}
#endif /* BSD4_2 */

		if (statb.st_mode & mode)
			return 0;
	}
	return -1;
}
#endif /* DBM */

dospool(batchcmd, dolhwrite)
char *batchcmd;
int dolhwrite;
{
	register int c;
	register FILE *sp;
	register struct tm *tp;
	time_t t;
	char buf[BUFLEN], sfile[BUFLEN];
	extern struct tm *gmtime();

	(void) sprintf(sfile, "%s/.spXXXXXX", SPOOL);
	sp = xfopen(mktemp(sfile), "w");
	if (batchcmd != NULL) {
		if (not_here[0] != '\0')
			fprintf(sp, "%s -x %s\n", batchcmd, not_here);
		else
			fprintf(sp, "%s\n", batchcmd);
	} else
		if (not_here[0] != '\0')
			fprintf(sp, "#! inews -x %s -p\n", not_here);
	if (dolhwrite)
		lhwrite(&header, sp);
	while ((c = getc(infp)) != EOF)
		putc(c, sp);
	fclose(sp);

	(void) time(&t);
	tp = gmtime(&t);
	/* This file name "has to" be unique  (right?) */
#ifdef USG
	(void) sprintf(buf, "%s/.rnews/%2.2d%2.2d%2.2d%2.2d%2.2d%x",
#else
#ifdef VMS
	/* Eunice doesn't like dots in directory names */
	(void) sprintf(buf, "%s/+rnews/%02d%02d%02d%02d%02d%x",
#else /* V7 */
	(void) sprintf(buf, "%s/.rnews/%02d%02d%02d%02d%02d%x",
#endif /* V7 */
#endif /* VMS */
		SPOOL,
		tp->tm_year, tp->tm_mon+1, tp->tm_mday,
		tp->tm_hour, tp->tm_min, getpid());

#ifdef IHCC
	log("Spooling %s into %s", header.ident, (rindex(buf,'/') + 1));
#endif /* IHCC */

	if (LINK(sfile, buf) < 0) {
		char dbuf[BUFLEN];
#ifdef VMS
		sprintf(dbuf, "%s/+rnews", SPOOL);
#else /* !VMS */
		sprintf(dbuf, "%s/.rnews", SPOOL);
#endif /* !VMS */
		if (mkdir(dbuf, 0777&~N_UMASK) < 0)
			xerror("Cannot mkdir %s: %s", dbuf, errmsg(errno));
		if (LINK(sfile, buf) < 0) 
			xerror("Cannot link(%s,%s): %s", sfile, buf,
				errmsg(errno));
	}
	(void) UNLINK(sfile);
	xxit(0);
	/* NOTREACHED */
}

/*
 *	Create a newsgroup
 */
createng()
{
	register char *cp;

	/*
	 * Only certain users are allowed to create newsgroups
	 */
	if (uid != ROOTID && uid != duid && uid) {
		logerr("Please contact one of the local netnews people");
		xerror("to create group \"%s\" for you", header.ctlmsg);
	}
	if (header.distribution[0] == '\0')
#ifdef ORGDISTRIB
		strcpy(header.distribution, ORGDISTRIB);
#else /* !ORGDISTRIB */
		strcpy(header.distribution, "local");
#endif /* !ORGDISTRIB */

	(void) strcpy(header.nbuf, header.ctlmsg);
	if ((cp=index(header.nbuf, ' ')) != NULL)
		*cp = '\0';

	if (header.approved[0] == '\0')
		(void) sprintf(header.approved, "%s@%s",
				username, FROMSYSNAME);
	(void) sprintf(bfr, "%s/inews -n %s.ctl -c newgroup %s -d %s -a \"%s\"",
		LIB, header.nbuf, header.ctlmsg, header.distribution,
		header.approved);
	if (tty) {
		printf("Please type in a paragraph describing the new newsgroup.\n");
		printf("End with control D as usual.\n");
	}
	printf("%s\n", bfr);
	(void) fflush(stdout);
	(void) system(bfr);
	exit(0);
	/*NOTREACHED*/
}

char firstbufname[BUFLEN];
/*
 *	Link ARTICLE into dir for ngname and update active file.
 */
long
localize(ngname)
char	*ngname;
{
	char afline[BUFLEN];
	long ngsize;
	long fpos;
	int e;
	char *cp;

	lock();
	(void) rewind(actfp); clearerr(actfp);

	for(;;) {
		fpos = ftell(actfp);
		if (fgets(afline, sizeof afline, actfp) == NULL) {
			unlock();
			logerr("Can't find \"%s\" in active file", ngname);
			return FALSE;		/* No such newsgroup locally */
		}
		if (PREFIX(afline, ngname)) {
			(void) sscanf(afline, "%s %ld", bfr, &ngsize);
			if (STRCMP(bfr, ngname) == 0) {
				if (ngsize < 0 || ngsize > 99998) {
					logerr("found bad ngsize %ld ng %s, setting to 1", ngsize, bfr);
					ngsize = 1;
				}
				break;
			}
		}
	}
	for (;;) {
		cp = dirname(ngname);

		(void) sprintf(bfr, "%s/%ld", cp, ngsize+1);
#ifdef VMS
		/*
		 * The effect of this code is to store the article in the first
		 * newsgroup's directory and to put symbolic links elsewhere.
		 * If this is the first group, firstbufname is not yet filled
		 * in. It should be portable to other link-less systems.
		 * epimass!jbuck
		 */
		if (firstbufname[0]) {
			if (vmslink(firstbufname, bfr) == 0)
				break;
		} else if (rename(ARTICLE, bfr) == 0)
			break;
#else /* !VMS */
		if (link(ARTICLE, bfr) == 0)
			break;
#endif /* !VMS */
		if (!exists(cp))
			mknewsg(cp, ngname);
#ifdef VMS
		if (firstbufname[0]) {
			if (vmslink(firstbufname, bfr) == 0)
				break;
		} else if (rename(ARTICLE, bfr) == 0) 
			break;
#else /* !VMS */
		if (link(ARTICLE, bfr) == 0)
			break;
#endif /* !VMS */
		e = errno;	/* keep log from clobbering it */
		log("Cannot install article as %s: %s", bfr, errmsg(errno));
		if (e != EEXIST) {
			logerr("Link into %s failed (%s); check dir permissions.",
			    bfr, errmsg(e));
			unlock();
			return FALSE;
		}
		ngsize++;
	}

	/*
	 * This works around a bug in the 4.1bsd stdio
	 * on fseeks to non even offsets in r+w files
	 */
	if (fpos&1)
		(void) rewind(actfp);

	(void) fseek(actfp, fpos, 0);
	/*
	 * Has to be same size as old because of %05d.
	 * This will overflow with 99999 articles.
	 */
	fprintf(actfp, "%s %05ld", ngname, ngsize+1);
#if defined(USG) || defined(MG1)
	/*
	 * U G L Y   K L U D G E
	 * This utter piece of tripe is the only way I know of to get
	 * around the fact that ATT BROKE standard IO in System 5.2.
	 * Basically, you can't open a file for "r+" and then try and
	 * write to it. This works on all "real" USGUnix systems, It will
	 * probably break on some obscure look alike that doesnt use the
	 * real ATT stdio.h
	 * Don't blame me, blame ATT. stdio should have already done the
	 * following line for us, but it doesn't
	 * also broken in WCW MG-1 42nix 2.0
	 */
	 actfp->_flag |= _IOWRT;
#endif /* USG */
	(void) fflush(actfp);
	if (ferror(actfp))
		xerror("Active file write failed");
	unlock();
	if (firstbufname[0] == '\0')
		(void) strcpy(firstbufname, bfr);
	(void) sprintf(bfr, "%s/%ld ", ngname, ngsize+1);
	addhist(bfr);
	return ngsize+1;
}

/*
 *	Localize for each newsgroup and broadcast.
 */
insert()
{
	register char *ptr;
	register FILE *tfp;
	register int c;
	struct srec srec;	/* struct for sys file lookup	*/
	struct tm *tm, *gmtime();
	int is_invalid = FALSE;
	int exitcode = 0;
	long now;
#ifdef DOXREFS
	register char *nextref = header.xref;
#endif /* DOXREFS */

	/* Clean up Newsgroups: line */
	if (!is_ctl && mode != CREATENG)
		is_invalid = ngfcheck(mode == PROC);

	(void) time(&now);
	tm = gmtime(&now);
	if (header.expdate[0])
		addhist(" ");
#ifdef USG
	sprintf(bfr,"%2.2d/%2.2d/%d %2.2d:%2.2d\t",
#else /* !USG */
	sprintf(bfr,"%02d/%02d/%d %02d:%02d\t",
#endif /* !USG */
		tm->tm_mon+1, tm->tm_mday, tm->tm_year,tm->tm_hour, tm->tm_min);
	addhist(bfr);
	log("%s %s ng %s subj '%s' from %s", spool_news != DONT_SPOOL
		? "queued" : (mode==PROC ? "received" : "posted"),
		header.ident, header.nbuf, header.title, header.from);

	/* Write article to temp file. */
	tfp = xfopen(mktemp(ARTICLE), "w");

	if (is_invalid) {
		logerr("No valid newsgroups found, moved to junk");
		if (localize("junk"))
			savehist(histline);
		exitcode = 1;
		goto writeout;
	}

#ifdef ZAPNOTES
	if (STRNCMP(header.title, "Re: Orphaned Response", 21) == 0) {
		logerr("Orphaned Response, moved to junk");
		if (localize("junk"))
			savehist(histline);
		exitcode = 1;
		goto writeout;
	}
#endif	/* ZAPNOTES */

	if (time((time_t *)0) > (cgtdate(header.subdate) + HISTEXP) ){
		logerr("Article too old, moved to junk");
		if (localize("junk"))
			savehist(histline);
		exitcode = 1;
		goto writeout;
	}

	if (is_mod[0] != '\0' 	/* one of the groups is moderated */
		&& header.approved[0] == '\0') { /* and unapproved */
		struct hbuf mhdr;
		FILE *mfd, *mhopen();
		register char *p;
		char modadd[BUFLEN], *replyname();
#ifdef DONTFOWARD
		if(mode == PROC) {
			logerr("Unapproved article in moderated group %s",
				is_mod);
			if (localize("junk"))
				savehist(histline);
			goto writeout;
		}
#endif /* DONTFORWARD */
		fprintf(stderr,"%s is moderated and may not be posted to",
			is_mod);
		fprintf(stderr," directly.\nYour article is being mailed to");
		fprintf(stderr," the moderator who will post it for you.\n");
		/* Let's find a path to the backbone */
		sprintf(bfr, "%s/mailpaths", LIB);
		mfd = xfopen(bfr, "r");
		do {
			if (fscanf(mfd, "%s %s", bfr, modadd) != 2)
				xerror("Can't find backbone in %s/mailpaths",
					LIB);
		} while (STRCMP(bfr, "backbone") != 0 && !ngmatch(is_mod, bfr));
		(void) fclose(mfd);
		/* fake a header for mailhdr */
		mhdr.from[0] = '\0';
		mhdr.replyto[0] = '\0';
		p = is_mod;
		while (*++p)
			if (*p == '.')
				*p = '-';
		sprintf(mhdr.path, modadd, is_mod);
		mfd = mhopen(&mhdr);
		if (mfd == NULL)
			xerror("Can't send mail to %s", mhdr.path);
		fprintf(mfd, "To: %s\n", replyname(&mhdr));
		lhwrite(&header, mfd);
		putc('\n', mfd);
		while ((c = getc(infp)) != EOF)
			putc(c, mfd);
		mclose(mfd);
		log("Article mailed to %s", mhdr.path);
		xxit(0);
	}

	if (mode != PROC && spool_news != DONT_SPOOL)  {
		if (spool_news != EXPIRE_RUNNING
			&& ngmatch(header.nbuf,"to.all.ctl"))
				spool_news = DONT_SPOOL;
		if (spool_news != DONT_SPOOL) {
			fprintf(stderr,
			"Your article has been spooled for later processing.\n");
			dospool("#! inews -S -h", TRUE);
			/* NOT REACHED */
		}
	}

	if (is_ctl) {
		exitcode = control(&header);
		if (localize("control") && exitcode != 0)
			savehist(histline);
	} else {
		if (s_find(&srec, LOCALPATHSYSNAME) == FALSE) {
			logerr("Cannot find my name '%s' in %s",
				LOCALPATHSYSNAME, SUBFILE);
			srec = dummy_srec;
		}
#ifdef DOXREFS
		(void) strncpy(nextref, PATHSYSNAME, BUFLEN);
#endif /* DOXREFS */
		for (ptr = nbuf; *ptr;) {
			if (ngmatch(ptr,srec.s_nbuf) || index(ptr,'.') == NULL){
#ifdef DOXREFS
				while (*nextref++)
					;
				(void) sprintf(--nextref, " %s:%ld", ptr, localize(ptr));
#else /* !DOXREFS */
				(void) localize(ptr);
#endif /* !DOXREFS */
			}
			while (*ptr++)
				;
		}
		if (firstbufname[0] == '\0') {
			logerr("Newsgroups in active, but not sys");
			(void) localize("junk");
		}
	}
#ifdef DOXREFS
	if (index(header.nbuf, NGDELIM) == NULL)
		header.xref[0] = '\0';
#endif /* DOXREFS */

writeout:
	/* Part 1 of kludge to get around article truncation problem */
	if ( (c=getc(infp)) != EOF) {
		ungetc(c, infp);
		if (c == ' ' || c == '\t') {
			header.intnumlines++;
			(void) sprintf(header.numlines, "%d",
				header.intnumlines);
		}
	}
	/* End of part 1 */
	if (header.expdate[0] != '\0' && mode != PROC) {
		/* Make sure it's fully qualified */
		long t = cgtdate(header.expdate);
		strcpy(header.expdate, arpadate(&t));
	}

	lhwrite(&header, tfp);
	if ((c = getc(infp)) != EOF) {
		/* Part 2 of kludge to get around article truncation problem */
		if (c == ' ' || c == '\t' )
			putc('\n', tfp);
		/* End of part 2 */
		ungetc(c, infp);
		while (fgets(bfr, BUFLEN, infp) != NULL)
			fputs(bfr, tfp);
		if (bfr[strlen(bfr)-1] != '\n')
			putc('\n',tfp);
	}
	if (ferror(tfp))
		xerror("Write failed for temp file");
	(void) fclose(tfp);
	(void) fclose(infp);
	if(exitcode == 0) {
		/* article has passed all the checks, so work in background */
		if (mode != PROC) {
			int pid;
			if ((pid=fork()) < 0)
				xerror("Can't fork");
			else if (pid > 0)
				_exit(0);
		}
#ifdef SIGTTOU
		(void) signal(SIGTTOU, SIG_IGN);
#endif /* SIGTTOU */
		savehist(histline);
		if (header.supersedes[0] != '\0') {
			char *av[2];

			av[0] = "cancel";
			av[1] = header.supersedes;
			c_cancel(2, av);
		}
		broadcast(mode==PROC);
	}
	xxit((mode == PROC && filename[0] == '\0') ? 0 :
		(exitcode < 0 ? 0 : exitcode));
}

input(usegunk)
{
	register char *cp;
	register int c;
	register int empty = TRUE;
	FILE *tmpfp;
	int consec_newlines = 0;
	int linecount = 0;
	int linserted = 0;

	tmpfp = xfopen(mktemp(INFILE), "w");
	for ( ; ; ) {
		if (SigTrap)
			break;
		if (usegunk)
			usegunk = FALSE;
		else if (fgets(bfr, BUFLEN, infp) != bfr)
			break;
 		if (mode == PROC) {	/* zap trailing empty lines */
#ifdef ZAPNOTES
			if (empty && bfr[0] == '#' && bfr[2] == ':'
				&& header.nf_id[0] == '\0'
				&& header.nf_from[0] == '\0' ) {
				(void) strcpy(header.nf_id, bfr);
				(void) nstrip(header.nf_id);
				(void) fgets(bfr, BUFLEN, infp);
				(void) strcpy(header.nf_from, bfr);
				(void) nstrip(header.nf_from);
				(void) fgets(bfr, BUFLEN, infp);

				if (header.numlines[0]) {
					header.intnumlines -= 2;
					(void) sprintf(header.numlines, "%d", header.intnumlines);
				}

				/* Strip trailing " - (nf)" */
				if ((cp = rindex(header.title, '-')) != NULL
				    && !strcmp(--cp, " - (nf)"))
					*cp = '\0';
				log("Stripped notes header on %s", header.ident);
				continue;
			}
#endif /* ZAPNOTES */
 			if (bfr[0] == '\n' ||
				/* Bandage for older versions of inews */
				bfr[1] == '\n' && !isascii(bfr[0])) {
 				consec_newlines++;	/* count it, in case */
 				continue;		/* but don't write it*/
 			}
 			/* foo! a non-empty line. write out all saved lines. */
 			while (consec_newlines > 0) {
				putc('\n', tmpfp);
				consec_newlines--;
				linecount++;
			}
 		}
		if (mode != PROC && tty && STRCMP(bfr, ".\n") == 0)
			break;
		for (cp = bfr; c = toascii(*cp); cp++) {
			if (isprint(c) || isspace(c) || c == '\b')
				putc(c, tmpfp);
			if (c == '\n')
				linecount++;
		}
		if (bfr[0] == '>')
			linserted++;
		if (bfr[0] == '<') /* kludge to allow diff's to be posted */
			linserted--;
		empty = FALSE;
	}
	if (*filename)
		(void) fclose(infp);
	if (mode != PROC &&
		linecount > LNCNT && linserted > (linecount-linserted))
		error("Article rejected: %s included more text than new text",
			username);

	if (mode != PROC && !is_ctl && header.sender[0] == '\0' && !Sflag) {
		int siglines = 0;
		char sbuf[BUFLEN];
		(void) sprintf(bfr, "%s/%s", userhome, ".signature");
		if (access(bfr, 4) == 0) {
			if ((infp = fopen(bfr, "r")) == NULL) {
				(void) fprintf(stderr,
    "inews: \"%s\" left off (must be readable by \"inews\" owner)\n", bfr);
				goto finish;
			}

			while (fgets(sbuf, sizeof sbuf, infp) != NULL)
				if (++siglines > 4)
					break;
			if (siglines > 4)
				fprintf(stderr,".signature not included (> 4 lines)\n");
			else {
				rewind(infp);
				fprintf(tmpfp, "-- \n");	/* To separate */
				linecount++;
				while ((c = getc(infp)) != EOF) {
					putc(c, tmpfp);
					if (c == '\n')
						linecount++;
				}
			}
			(void) fclose(infp);
		}
	}

finish:
	if (ferror(tmpfp))
		xerror("write failed to temp file");
	(void) fclose(tmpfp);
	if (SigTrap) {
		if (tty)
			fprintf(stderr, "Interrupt\n");
		if (tty && !empty)
			fwait(fsubr(newssave, (char *) NULL, (char *) NULL));
		if (!tty)
			log("Blown away by an interrupt %d", SigTrap);
		xxit(1);
	}
	if (tty)
		fprintf(stderr, "EOT\n");
	fflush(stdout);
	infp = fopen(INFILE, "r");
	if (header.numlines[0]) {
		/*
		 * Check line count if there's already one attached to
		 * the article.  Could make this a fatal error -
		 * throwing it away if it got chopped, in hopes that
		 * another copy will come in later with a correct
		 * line count.  But that seems a bit much for now.
		 */
		if (linecount != header.intnumlines) {
			if (linecount == 0)
				error("%s rejected. linecount expected %d, got 0", header.ident, header.intnumlines);
			if (linecount > header.intnumlines ||
			    linecount+consec_newlines < header.intnumlines)
				log("linecount expected %d, got %d", header.intnumlines, linecount+consec_newlines);
		}
		/* adjust count for blank lines we stripped off */
		if (consec_newlines) {
			header.intnumlines -= consec_newlines;
			if (header.intnumlines < 0 )
				header.intnumlines = 0; /* paranoia */
			(void) sprintf(header.numlines, "%d", header.intnumlines);
		}

	} else {
		/* Attach a line count to the article. */
		header.intnumlines = linecount;
		(void) sprintf(header.numlines, "%d", linecount);
	}
}

/*
 * Make the directory for a new newsgroup.  ngname should be the
 * full pathname of the directory.  Do the other stuff too.
 * The various games with setuid and chown are to try to make sure
 * the directory is owned by NEWSUSR and NEWSGRP, which is tough to
 * do if you aren't root.  This will work on a UCB system (which allows
 * setuid(geteuid()) or a USG system (which allows you to give away files
 * you own with chown), otherwise you have to change your kernel to allow
 * one of these things or run with your dirs 777 so that it doesn't matter
 * who owns them.
 */
mknewsg(fulldir, ngname)
char	*fulldir;
char	*ngname;
{
#ifdef USG
	register char *p;
	char parent[200];
	char sysbuf[200];
	struct stat sbuf;
#endif /* USG */

	if (ngname == NULL || !isalpha(ngname[0]))
		xerror("Tried to make illegal newsgroup %s", ngname);

#ifdef USG
	/*
	 * If the parent is 755 the setuid(getuid)
	 * will fail, and since mkdir is suid, and our real uid is random,
	 * the mkdir will fail.  So we have to temporarily chmod it to 777.
	 */
	(void) strcpy(parent, fulldir);
	while (p = rindex(parent, '/')) {
		*p = '\0';
		if (stat(parent, &sbuf) == 0) {
			(void) chmod(parent, 0777);
			break;
		}
	}
#endif /* USG */

	/* Create the directory */
	mkparents(fulldir);
	if (mkdir(fulldir, 0777) < 0)
		xerror("Cannot mkdir %s: %s", fulldir, errmsg(errno));

#ifdef USG
	/*
	 * Give away the directories we just created which were assigned
	 * our real uid.
	 */
	(void) setuid(uid);
	(void) chown(fulldir, duid, dgid);

	(void) strcpy(sysbuf, fulldir);
	while (p = rindex(sysbuf, '/')) {
		*p = '\0';
		/* stop when get to last known good parent */
		if (strcmp(sysbuf, parent) == 0)
			break;
		(void) chown(sysbuf, duid, dgid);
	}
	(void) setuid(duid);
	(void) chmod(parent, (int)sbuf.st_mode);	/* put it back */
#endif /* USG */

	log("make newsgroup %s in dir %s", ngname, fulldir);
}

/*
 * If any parent directories of this dir don't exist, create them.
 */
mkparents(dname)
char *dname;
{
	char buf[200];
	register char *p;

	(void) strcpy(buf, dname);
	p = rindex(buf, '/');
	if (p)
		*p = '\0';
	if (exists(buf))
		return;
	mkparents(buf);
	if (mkdir(buf, 0777) < 0)
		xerror("Can not mkdir %s: %s", buf, errmsg(errno));
}

dounspool()
{
	register DIR	*dirp;
	register struct direct *dir;
	register int foundsome;
	int pid, status, ret;
	char spbuf[BUFLEN];
#ifdef LOCKF
	FILE* LockFd;
#endif /* LOCKF */
#ifdef VMS
	sprintf(spbuf, "%s/+rnews", SPOOL);
#else /* !VMS */
	sprintf(spbuf, "%s/.rnews", SPOOL);
#endif /* !VMS */

	if (chdir(spbuf) < 0)
		xerror("chdir(%s):%s", spbuf, errmsg(errno));

	dirp = opendir(".");
	if (dirp == NULL)	/* Boy are things screwed up */
		xerror("opendir can't open .:%s", errmsg(errno));
#ifdef	LOCKF
	LockFd = xfopen(SEQFILE, "r+w");
	if (lockf(fileno(LockFd), F_TLOCK, 0L) < 0) {
		if (errno != EAGAIN && errno != EACCES)
#else	/* !LOCKF */
#ifdef BSD4_2
	if (flock(dirp->dd_fd, LOCK_EX|LOCK_NB) < 0) {
		if (errno != EWOULDBLOCK)
#else	/* V7 */
	strcat(spbuf, ".lock");
	sprintf(bfr, "%s.tmp", spbuf);
	(void) close(creat(bfr, 0666));
	ret = LINK(bfr, spbuf);
	status = errno;
	(void) UNLINK(bfr);
	errno = status;
	if (ret < 0) {
		if (errno != EEXIST)
#endif /* V7 */
#endif	/* !LOCKF */
			xerror("Can't lock %s: %s", spbuf, errmsg(errno));
		xxit(3); /* another rnews -U is running */
	}

	do {
		foundsome = 0;

		while ((dir=readdir(dirp)) != NULL) {
			if (dir->d_name[0] == '.')
				continue;

#ifdef IHCC
			log("Unspooling from %s", dir->d_name);
#endif /* IHCC */

			if ((pid=vfork()) == -1)
				xerror("Can't fork: %s", errmsg(errno));
			if (pid == 0) {
#ifdef LOGDIR
				char bufr[BUFSIZ];
				sprintf(bufr, "%s/%s", logdir(HOME), RNEWS);
				execl(bufr, "rnews", "-S", "-p", dir->d_name,
					(char *) NULL);
#else /* !LOGDIR */
				execl(RNEWS, "rnews", "-S", "-p", dir->d_name,
					(char *) NULL);
#endif /* !LOGDIR */
				_exit(1);
			}
			
			while ((ret=wait(&status)) != pid && ret != -1)
				/* continue */;

			if (((status>>8)&0177) == 42) {
				/* expire has started up, shutdown rnews -U */
				break;
			}

			if (status != 0) {
				sprintf(bfr, "../%s", dir->d_name);
				(void) LINK(dir->d_name, bfr);
				logerr("rnews failed, status %ld. Batch saved in %s/%s",
					(long)status, SPOOL, dir->d_name);
			}
			(void) unlink(dir->d_name);
			foundsome++;
		}
		rewinddir(dirp);
	} while (foundsome); /* keep rereading the directory until it's empty */
#ifndef LOCKF
#ifndef BSD4_2
	(void) UNLINK(spbuf);
#endif
#endif

	xxit(0);
}
