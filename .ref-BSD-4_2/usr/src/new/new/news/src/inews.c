/*
 * inews - insert, receive, and transmit news articles.
 */

static char *SccsId = "@(#)inews.c	2.30	6/24/83";

#include "iparams.h"

/* local defines for inews */

#define OPTION	0	/* pick up an option string */
#define STRING	1	/* pick up a string of arguments */

#define UNKNOWN 0001	/* possible modes for news program */
#define UNPROC	0002	/* Unprocessed input */
#define PROC	0004	/* Processed input */
#define	CANCEL	0010	/* Cancel an article */
#define	CREATENG 0020	/* Create a new newsgroup */

#ifndef SYSBUF
char	SYSBUF[BUFSIZ];		/* to buffer std out */
#endif
char	forgedname[100];	/* A user specified -f option. */

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
'e',	' ',		FALSE,	UNPROC,	UNKNOWN,	header.expdate,
'p',	'\0',		FALSE,	UNKNOWN,PROC,		filename,
'f',	'\0',		FALSE,	UNPROC,	UNKNOWN,	forgedname,
'F',	' ',		FALSE,	UNPROC,	UNKNOWN,	header.followid,
'c',	'\0',		FALSE,	UNKNOWN,CANCEL,		filename,
'C',	'\0',		FALSE,	UNKNOWN,CREATENG,	header.nbuf,
#define Dflag	options[8].flag
'D',	'\0',		FALSE,	UNPROC,	UNKNOWN,	filename,
#define hflag	options[9].flag
'h',	'\0',		FALSE,	UNPROC,	UNKNOWN,	filename,
'\0',	'\0',		0,	0,	0,		(char *)NULL
};

FILE *mailhdr();

/*
 *	Authors:
 *		Matt Glickman	glickman@ucbarpa.Berkeley.ARPA
 *		Mark Horton	mark@cbosgd.UUCP
 *		Stephen Daniels	swd@mcnc.UUCP
 *		Tom Truscott	trt@duke.UUCP
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
	char	*user, *home;	/* environment temps			*/
	struct passwd	*pw;	/* struct for pw lookup			*/
	struct group	*gp;	/* struct for group lookup		*/
	struct srec	srec;	/* struct for sys file lookup		*/
	struct utsname	ubuf;
	register int	i;
	FILE	*mfd;		/* mail file file-descriptor		*/
	char	cbuf[BUFLEN];	/* command buffer			*/

	/* uuxqt doesn't close all it's files */
	for (i = 3; !close(i); i++)
		;
	/* set up defaults and initialize. */
	mode = UNKNOWN;
	pathinit();
	ptr = rindex(*argv, '/');
	if (!ptr) 
		ptr = *argv - 1;
	if (!strncmp(ptr+1, "rnews", 5))
		mode = PROC;
	else if (argc < 2)
		goto usage;

	state = OPTION;
	header.title[0] = header.nbuf[0] = filename[0] = '\0';

	/* check for existence of special files */
	if (!rwaccess(ARTFILE)) {
		mfd = mailhdr(NULL, exists(ARTFILE) ? "Unwritable files!" : "Missing files!");
		if (mfd != NULL) {
			fprintf(mfd,"System: %s\n\nThere was a problem with %s!!\n", SYSNAME, ARTFILE);
			sprintf(cbuf, "touch %s;chmod 666 %s", ARTFILE, ARTFILE);
			system(cbuf);
			if (rwaccess(ARTFILE))
				fprintf(mfd, "The problem has been taken care of.\n");
			else
				fprintf(mfd, "Corrective action failed - check suid bits.\n");
			mclose(mfd);
		}
	}
	if (!rwaccess(ACTIVE)) {
		mfd = mailhdr(NULL, exists(ACTIVE) ? "Unwritable files!" : "Missing files!");
		if (mfd != NULL) {
			fprintf(mfd, "System: %s\n\nThere was a problem with %s!!\n", SYSNAME, ACTIVE);
			sprintf(cbuf, "touch %s;chmod 666 %s", ACTIVE, ACTIVE);
			system(cbuf);
			if (rwaccess(ACTIVE))
				fprintf(mfd, "The problem has been taken care of.\n");
			else
				fprintf(mfd, "Corrective action failed - check suid bits.\n");
			mclose(mfd);
		}
	}
	setbuf(stdout, SYSBUF);
	sigtrap = FALSE;	/* true if a signal has been caught */
/*
	signal(SIGQUIT, SIG_IGN);
*/
	if (mode != PROC) {
		signal(SIGHUP, onsig);
		signal(SIGINT, onsig);
	}
	savmask = umask(N_UMASK);	/* set up mask */
	uid = (unsigned) getuid();
	gid = (unsigned) getgid();
	duid = geteuid();
	dgid = getegid();
	if (uid == 0 && geteuid() == 0) {
		/*
		 * Must go through with this kludge since
		 * some systems do not honor the setuid bit
		 * when root invokes a setuid program.
		 */
		if ((pw = getpwnam(NEWSU)) == NULL)
			xerror("Cannot get NEWSU pw entry");

		duid = pw->pw_uid;
		if ((gp = getgrnam(NEWSG)) == NULL)
			xerror("Cannot get NEWSG gr entry");
		dgid = gp->gr_gid;
		setuid(duid);
		setgid(dgid);
	}

#ifndef IHCC
	/*
	 * We force the use of 'getuser()' to prevent forgery of articles
	 * by just changing $LOGNAME
	 */
	if ((user = getenv("USER")) == NULL)
		user = getenv("LOGNAME");
	if ((home = getenv("HOME")) == NULL)
		home = getenv("LOGDIR");
#endif
	if (user == NULL || home == NULL)
		getuser();
	else {
		if (username[0] == 0) {
			strcpy(username, user);
		}
		strcpy(userhome, home);
	}
	getuser();
	strcpy(whatever, username);

	/* loop once per arg. */

	++argv;		/* skip first arg, which is prog name. */

	while (--argc) {
	    if (state == OPTION) {
		if (**argv != '-') {
			sprintf(bfr, "Bad option string \"%s\"", *argv);
			xerror(bfr);
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
			fprintf(stderr, "       inews -p [ filename ]\n");
			xxit(1);

		    found:;
			if (optpt->flag == TRUE || (mode != UNKNOWN &&
			    (mode&optpt->oldmode) == 0)) {
				sprintf(bfr, "Bad %c option", **argv);
				xerror(bfr);
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
		strcpy(ptr, *argv++);
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

	tty = isatty(fileno(stdin));
	if (!Dflag && mode != PROC && mode != CREATENG) {
		if (recording(header.nbuf)) {
			if (!tty)
				fwait(fsubr(newssave, stdin, NULL));
			xerror("aborted due to recording");
		}
	}

	/* This code is really intended to be replaced by the control message. */
	if (mode == CANCEL) {
		char *p; FILE *f;
		f = xfopen(filename, "r");
		hread(&header, f, TRUE);
		p = index(header.path, ' ');
		if (p != NULL)
			*p = 0;
		p = header.path;
		if (strncmp(whatever, p, strlen(whatever))
			&& uid != ROOTID && uid != geteuid() && uid)
			xerror("Not contributor");
		cancel();
		xxit(0);
	}

	if (*header.nbuf)
		lcase(header.nbuf);
	if (mode != PROC) {
		getident(&header);
#ifdef MYORG
		strcpy(header.organization, MYORG);
		if (strncmp(header.organization, "Frobozz", 7) == 0)
			header.organization[0] = '\0';
		if (ptr = getenv("ORGANIZATION"))
			strcpy(header.organization, ptr);
		/*
		 * Note that the organization can also be turned off by
		 * setting it to the null string, either in MYORG or
		 * $ORGANIZATION in the environment.
		 */
		if (header.organization[0] == '/') {
			mfd = fopen(header.organization, "r");
			if (mfd) {
				fgets(header.organization, sizeof header.organization, mfd);
				fclose(mfd);
			}
			ptr = index(header.organization, '\n');
			if (ptr)
				*ptr = 0;
		}
#endif
		if (hflag) {
			/* Fill in a few to make frmread return TRUE */
			strcpy(header.subdate, "today");
			strcpy(header.path, "me");
			strcpy(header.oident, "id");
			/* Allow the user to supply some headers. */
			hread(&header, stdin, FALSE);
			/* But there are certain fields we won't let him specify. */
			if (header.from)
				strcpy(forgedname, header.from);
			header.from[0] = '\0';
			header.path[0] = '\0';
			header.subdate[0] = '\0';
			header.sender[0] = '\0';
			if (strcmp(header.oident, "id") == 0)
				header.oident[0] = '\0';
			ngcat(header.nbuf);
		}
		if (forgedname[0]) {
			strcpy(header.from, forgedname);
			sprintf(header.sender, "%s@%s%s",
				username, SYSNAME, MYDOMAIN);
		} else {
			gensender(&header, username);
		}
		strcpy(header.postversion, genversion());
	}

	/* Authorize newsgroups. */
	if (mode == PROC) {
		checkbatch();
		signal(SIGHUP, SIG_IGN);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		if (hread(&header, stdin, TRUE) == NULL)
			xerror("Inbound news is garbled");
		input();
		if (history(&header)) {
			fprintf(stderr, "Duplicate article %s rejected\n", header.ident);
			log("Duplicate article %s rejected", header.ident);
			xxit(0);
		}
	}
	ngcat(header.nbuf);

	/* Easy way to make control messages, since all.all.ctl is unblessed */
	if (mode != PROC && prefix(header.title, "cmsg ") && header.ctlmsg[0] == 0)
		strcpy(header.ctlmsg, &header.title[5]);
	is_ctl = mode != CREATENG &&
		(ngmatch(header.nbuf, "all.all.ctl,") || header.ctlmsg[0]);
#ifdef DEBUG
	fprintf(stderr,"is_ctl set to %d\n", is_ctl);
#endif

			/* Must end in comma (NGDELIM) */
#define MODGROUPS	"mod.all,all.mod,all.announce,"
	if (ngmatch(header.nbuf, MODGROUPS) && !header.approved[0]) {
		mfd = mailhdr(&header, "Moderated newsgroup");
		if (mfd) {
			fprintf(mfd, "This newsgroup is moderated, and cannot be posted to directly.\n");
			fprintf(mfd, "Please mail your article to the moderator for posting.\n");
			hwrite(&header, mfd);
			if (infp)
				while ((i = getc(infp)) != EOF)
					putc(i, mfd);
			mclose(mfd);
		}
		xerror("Unapproved moderated newsgroup\n");
	}

	if (mode == PROC) {
		strcpy(nbuf, header.nbuf);
		if (s_find(&srec, FULLSYSNAME) == FALSE)
			xerror("Cannot find my name '%s' in %s", FULLSYSNAME, SUBFILE);
		ngsquash(nbuf, srec.s_nbuf);

	}
	else if (mode != CREATENG) {
		if (!*header.title)
			xerror("No title");
		if (!*header.nbuf)
			strcpy(header.nbuf, DFLTNG);
		else if (!is_ctl)
			ngfcheck(FALSE);
		ngcat(header.nbuf);
		strcpy(nbuf, header.nbuf);
	}
	else {				/* mode == CREATENG */
		ngcat(header.nbuf);
		strcpy(nbuf, header.nbuf);
	}

	for (ptr = nbuf; (ptr = index(ptr, NGDELIM)) != NULL; *ptr++ = '\0')
		;
	if (!is_ctl && header.followid[0] == '\0')
		for (ptr = nbuf; *ptr;) {
			if (!exists(dirname(ptr)))
				ngcheck(ptr);
			while (*ptr++)
				;
		}
	else if (mode <= UNPROC)
		ctlcheck();
	for (ptr = nbuf; *ptr;) {
		if (*ptr != '-')
			goto out;
		while (*ptr++)
			;
	}
	xerror("No valid newsgroups in '%s'", header.nbuf);
out:

	/* Determine input. */
	if (mode != PROC)
		input();

#ifndef VMS
	/* Go into the background so the user can get on with his business. */
	if (mode != PROC) {
		i = fork();
		if (i != 0)
			exit(0);
	}
#endif VMS

	/* Do the actual insertion. */
	insert();
}

/*
 *	Create directory named by bfr.
 *	Don't if user doesn't want to.
 */
ngcheck(ngname)
char	*ngname;
{
	char	class[120];
	char	dir[256];
	char	*cp;

	strcpy(dir, dirname(ngname));
	if (mode == PROC) {
#ifdef AUTONEWNG
		mknewsg(dir, ngname);
#endif
		return 1;
	}

	/*
	 * If a user is trying to input to a non-existent group complain.
	 * First check to see if the newsgroup ever existed.  To do this, try
	 * to find the name in the "active" file.
	 */
	if (mode != CREATENG && !is_ctl) {
		/* Ick! Figure out if the newsgroup is in the active file. */
		sprintf(dir, "grep -s '^%s ' %s", ngname, ACTIVE);
		if ((system(dir) != 0))
			xerror("There is no such newsgroup as %s.", ngname);
		else {
			strcpy(dir, dirname(ngname));
			mknewsg(dir, ngname);
			return 0;
		}
	}
	/*
	 * Only certain users are allowed to create newsgroups
	 */
	if (uid != ROOTID && uid != geteuid() && uid)
		xerror("Please contact one of the local netnews people\n\tto create this group for you");

	/* Broadcast the new newsgroup */
	strcpy(class, ngname);
	for (cp=class; *cp && *cp!='.'; cp++)
		;
	if (*cp)
		*cp = 0;
	else {
		mknewsg(dir, ngname);
		exit(0);	/* Local newsgroup */
	}
	sprintf(bfr, "inews -D -n %s.ctl -t cmsg newgroup %s", ngname, ngname);
	printf("Please type in a paragraph describing the new newsgroup.\n");
	printf("End with control D as usual.\n");
	printf("%s\n", bfr);
	fflush(stdout);
	system(bfr);
	exit(0);
}

#include <errno.h>
char firstbufname[100];
/*
 *	Link ARTICLE into dir for ngname and update active file.
 */
localize(ngname)
char	*ngname;
{
	FILE	*fp;
	struct stat	status;
	char afline[BUFLEN];
	long ngsize;
	long fpos;
	int i, e;
	extern int errno;

	lock();
	actfp = fopen(ACTIVE, "r+");
	for(;;) {
		fpos = ftell(actfp);
		if (fgets(afline, sizeof afline, actfp) == NULL) {
			unlock();
			return FALSE;		/* No such newsgroup locally */
		}
		if (prefix(afline, ngname)) {
			sscanf(afline, "%s %ld", bfr, &ngsize);
			if (strcmp(bfr, ngname) == 0) {
				break;
			}
			if (ngsize < 0 || ngsize > 99998) {
				log("found bad ngsize %d ng %s, setting to 1", ngsize, bfr);
				ngsize = 1;
			}
		}
	}
	for (;;) {
		sprintf(bfr, "%s/%ld", dirname(ngname), ngsize+1);
		if (link(ARTICLE, bfr) == 0) break;
		e = errno;	/* keep log from clobbering it */
		fprintf(stderr, "Cannot install article as %s\n", bfr);
		log("Cannot install article as %s", bfr);
		if (e != EEXIST) {
			log("Link into %s failed, errno %d, check dir permissions.", bfr, e);
			unlock();
			return FALSE;
		}
		ngsize++;
	}

	/* Next two lines program around a bug in 4.1BSD stdio. */
	fclose(actfp);
	actfp = fopen(ACTIVE, "r+");

	fseek(actfp, fpos, 0);
	/* Has to be same size as old because of %05d.
	 * This will overflow with 99999 articles.
	 */
	fprintf(actfp, "%s %05ld\n", ngname, ngsize+1);
	fclose(actfp);
	unlock();
	if (firstbufname[0] == '\0')
		strcpy(firstbufname, bfr);
	sprintf(bfr, "%s/%ld ", ngname, ngsize+1);
	addhist(bfr);
	return TRUE;
}

/*
 *	Localize for each newsgroup and broadcast.
 */
insert()
{
	register char *ptr;
	register FILE *tfp;
	int badgroup = 0, goodgroup = 0;

	/* Fill up the rest of header. */
	if (mode != PROC) {
		history(&header);
	}
	dates(&header);
	addhist(header.recdate);
	addhist("\t");
	log("%s %s ng %s subj '%s'", mode==PROC ? "received" : "posted", header.ident, header.nbuf, header.title);
	if (mode==PROC)
		log("from %s relay %s", header.from, header.relayversion);

	/* Write article to temp file. */
	tfp = xfopen(mktemp(ARTICLE), "w");
	lhwrite(&header, tfp);
	while (fgets(bfr, BUFLEN, infp) != NULL) {
		/*
		if (!strncmp(bfr, "From ", 5))
			putc('>', tfp);
		*/
		fputs(bfr, tfp);
	}
	fclose(tfp);
	fclose(infp);

	if (is_ctl) {
		control(&header);
		goodgroup++;
		if (!localize("control")) {
			sprintf(bfr, "%s/%s", SPOOL, "control");
			mknewsg(bfr, "control");
			if (!localize("control")) {
				tfp = mailhdr(NULL, "No control newsgroup");
				if (tfp) {
					fprintf(tfp, "Can't create newsgroup 'control'.\n");
					mclose(tfp);
				}
			}
		}
	} else {
		for (ptr = nbuf; *ptr;) {
			if (*ptr == '-') {
				while (*ptr++)
					;
				continue;
			}
			strcpy(bfr, dirname(ptr));
			if (!exists(bfr)) {
#ifdef AUTONEWNG
				mknewsg(bfr, ptr);
#else
				getapproval(ptr);
				badgroup++;
#endif
			}
			else
				goodgroup++;
			if (*nbuf)
				localize(ptr);
			while (*ptr++)
				;
		}
	}

#ifdef NOFORWARD
	if (*nbuf)
#endif
		if (goodgroup)
			broadcast();
	savehist();
	xxit(0);
}

input()
{
	register int empty = TRUE;
	register char *cp;
	int c;
	long chcount = 0;
	FILE *tmpfp;
	int consec_newlines = 0;
	int linecount = 0;

	tmpfp = xfopen(mktemp(INFILE), "w");
	if (*filename) {
		tty = FALSE;
		infp = xfopen(filename, "r");
	} else {
		infp = stdin;
	}
	while (!sigtrap && fgets(bfr, BUFLEN, stdin) != NULL) {
 		if (mode == PROC)	/* zap trailing empty lines */
 		{
 			if (bfr[0] == '\n')	/* 1 empty line, to go */
 			{
 				consec_newlines++;	/* count it, in case */
 				continue;		/* but don't write it*/
 			}
 			/* foo! a non-empty line. write out all saved lines. */
 			while (consec_newlines > 0)
 			{
 				putc('\n', tmpfp);
 				consec_newlines--;
				linecount++;
 			}
 		}
		if (mode != PROC && tty && strcmp(bfr, ".\n") == 0)
			break;
		for (cp = bfr; c = *cp; cp++) {
			if (isprint(c) || isspace(c) || c=='\b')
				putc(c, tmpfp);
			if (isprint(c))
				chcount++;
			if (c == '\n')
				linecount++;
		}
		empty = FALSE;
	}
	if (*filename)
		fclose(infp);

	sprintf(bfr, "%s/%s", getenv("HOME"), ".signature");
	if (mode != PROC && (infp = fopen(bfr, "r"))) {
		fprintf(tmpfp, "-- \n");	/* To separate */
		linecount++;
		while ((c = getc(infp)) != EOF) {
			putc(c, tmpfp);
			if (c == '\n')
				linecount++;
		}
		fclose(infp);
	}

	fclose(tmpfp);
	if (sigtrap) {
		if (tty)
			printf("Interrupt\n");
		if (tty && !empty)
			fwait(fsubr(newssave, (char *) NULL, (char *) NULL));
		if (!tty)
			log("Blown away by an interrupt %d", sigtrap);
		xxit(1);
	}
	if (tty)
		printf("EOT\n");
	fflush(stdout);
	infp = fopen(INFILE, "r");
	if (chcount < 5 && mode <= UNPROC && !is_ctl)
		xerror("You didn't really want to post THAT!");
	if (header.numlines[0]) {
		/*
		 * Check line count if there's already one attached to
		 * the article.  Could make this a fatal error -
		 * throwing it away if it got chopped, in hopes that
		 * another copy will come in later with a correct
		 * line count.  But that seems a bit much for now.
		 */
		if (linecount != header.intnumlines)
			log("linecount expected %d, got %d\n", header.intnumlines, linecount);
	} else {
		/* Attach a line count to the article. */
		header.intnumlines = linecount;
		sprintf(header.numlines, "%d", linecount);
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
	int	pid;
	register char *p;
	char sysbuf[200];
	char parent[200];
	struct stat sbuf;

	if (ngname == NULL || !isalpha(ngname[0]))
		xerror("Tried to make illegal newsgroup %s", ngname);

	/*
	 * If the parent is 755 and we're on a USG system, the setuid(getuid)
	 * will fail, and since mkdir is suid, and our real uid is random,
	 * the mkdir will fail.  So we have to temporarily chmod it to 755.
	 */
	strcpy(parent, fulldir);
	p = rindex(parent, '/');
	if (p)
		*p = '\0';
	if (stat(parent, &sbuf) < 0)
		sbuf.st_mode = 0777;
	chmod(parent, 0777);

	if ((pid = fork()) <= 0) {
		if (setuid(geteuid()))	/* This fails on some systems, but
					 * works on 4BSD, and 2BSD. */
#ifndef USG
			umask(0)
#endif
				;
		setgid(getegid());
		/* Create the directory */
		mkparents(fulldir);
		sprintf(sysbuf, "mkdir %s", fulldir);
		exit(system(sysbuf));
	} else if (fwait(pid)) {
		sprintf(sysbuf, "Cannot mkdir %s", fulldir);
		xerror(sysbuf);
	}

	chmod(parent, sbuf.st_mode);	/* put is back */

#ifdef USG
# ifndef CHEAP
	/*
	 * Give away the files we just created, which were assigned to our
	 * REAL uid.  This only works on USG systems.  It is an alternative
	 * to the setuid call above.  The directories we just made are owned
	 * by our real uid, so we have to temporarily set our effective uid
	 * the same to allow the chown.  Fortunately, USG lets us setuid back.
	 */
	setuid(getuid());
	chown(fulldir, duid, dgid);
	setuid(duid);
# endif
#endif

	/* Update the "active newsgroup" file. */
	if (ngname && *ngname) {
		actfp = xfopen(ACTIVE, "a");
		fprintf(actfp, "%s 00000\n", ngname);
		fclose(actfp);
	}

	log("make newsgroup %s in dir %s", ngname, fulldir);
}

/*
 * If any parent directories of this dir don't exist, create them.
 */
mkparents(dirname)
char *dirname;
{
	char buf[200], sysbuf[200];
	register char *p;

	strcpy(buf, dirname);
	p = rindex(buf, '/');
	if (p)
		*p = '\0';
	if (exists(buf))
		return;
	mkparents(buf);
	sprintf(sysbuf, "mkdir %s", buf);
	system(sysbuf);
}

cancel()
{
	register FILE *fp;

	log("cancel article %s", filename);
	fp = xfopen(filename, "r");
	if (hread(&header, fp, TRUE) == NULL)
		xerror("Article is garbled.\n");
	fclose(fp);
	unlink(filename);
}

/*
 * An article has come in that isn't in a newsgroup we know about.
 * Stash it in the junk directory and notify the local contact person.
 * Note that such articles are NOT broadcast to our neighbors, on the
 * assumption that they are a typographical error.  We only keep them
 * here because we might be a new site.
 */
getapproval(ng)
char	*ng;
{
	FILE	*fd;

	if (localize("junk"))
		return;

	sprintf(bfr, "%s/%s", SPOOL, "junk");
	mknewsg(bfr, "junk");
	if (localize("junk"))
		return;

	fd = mailhdr(NULL, "Strange Newsgroup Received");
	if (fd != NULL) {
		fprintf(fd, "\nNewsgroup '%s' has been posted to\nby %s.\n\n",
			ng, header.from[0] ? header.from : header.path);
		fprintf(fd, "I was unable to save it in the newsgroup 'junk'\n");
		fprintf(fd, "I was also unable to create the newsgroup 'junk'\n");
		mclose(fd);
	}
}
