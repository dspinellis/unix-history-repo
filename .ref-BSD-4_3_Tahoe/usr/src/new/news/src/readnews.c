/*
 * readnews - read news articles.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)readnews.c	2.33	10/15/87";
#endif /* SCCSID */

#include "rparams.h"

/*
 * readnews - article reading program
 */

#ifndef SYSBUF
char	SYSBUF[BUFSIZ];	/* to buffer std out */
#endif

#define OPTION	0	/* pick up an option string */
#define STRING	1	/* pick up a string of arguments */

struct timeb Now;

int onsig(), cleanup();

/*
 *	Authors:
 *		Matt Glickman	ucbvax!glickman
 *		Mark Horton	cbosg!mark
 *		Stephen Daniels	duke!swd
 *		Tom Truscott	duke!trt
 */

main(argc, argv)
int	argc;
register char	**argv;
{
	register char	*ptr;	/* pointer to rest of buffer		*/
	char	*user = NULL, *home = NULL;
	int	optflag = FALSE, space = FALSE;
	struct utsname ubuf;
	char	*myrc;

	/* set up defaults and initialize. */
	pathinit();
	mode = UNKNOWN;
	header.title[0] = header.nbuf[0] = '\0';
	coptbuf[0] = datebuf[0] = '\0';
	uname(&ubuf);
	strcpy(FROMSYSNAME, ubuf.nodename);

	savmask = umask(N_UMASK);	/* set up mask */
	uid = getuid();
	gid = getgid();
	duid = geteuid();
	dgid = getegid();
	(void) ftime(&Now);
	/* give reasonable error message if SPOOL directory
	 * is unaccessable... usually means system administrator
	 * has "turned off" news reading...
	 */
#ifdef SERVER
	if (open_server() < 0)
#else	/* !SERVER */
	if (access(SPOOL, 05))
#endif	/* !SERVER */
	{
		fputs("News articles are not available at this time\n",stderr);
		xxit(1);
	}
#ifndef SHELL
	if ((SHELL = getenv("SHELL")) == NULL)
		SHELL = "/bin/sh";
#endif
	/*
	 * IHCC forces the use of 'getuser()' to prevent forgery of articles
	 * by just changing $LOGNAME
	 * Note that this shouldn't matter in readnews, since inews
	 * does all the actual posting of news.
	 */
#ifndef IHCC
	if ((user = getenv("USER")) == NULL)
		user = getenv("LOGNAME");
	if ((home = getenv("HOME")) == NULL)
		home = getenv("LOGDIR");
#endif /* ! IHCC */
	if (user == NULL || home == NULL)
		getuser();
	else {
		username = AllocCpy(user);
		(void) strcpy(header.path, username);
		userhome = AllocCpy(home);
	}

	if (!(MAILER = getenv("MAILER")))
		MAILER = "mail";	/* was /bin/mail */

#ifdef PAGE
	if (myrc = getenv("PAGER"))
		PAGER = AllocCpy(myrc);
	else {
# ifdef IHCC
		(void) sprintf(bfr, "%s/bin/%s", logdir(HOME), PAGE);
		PAGER = AllocCpy(bfr);
# else /* !IHCC */
		PAGER = PAGE;
# endif /* !IHCC */
	}
#endif /* PAGE */

	if (ptr = getenv("NEWSOPTS"))
		(void) strcpy(rcbuf, ptr);
	else
		*rcbuf = '\0';
	if (*rcbuf) {
		(void) strcat(rcbuf, " \1");
		ptr = rcbuf;
		while (*++ptr)
			if (isspace(*ptr))
				*ptr = '\0';
		for (ptr = rcbuf; ; ptr++) {
			if (!*ptr)
				continue;
			if (*ptr == '\1')
				break;
			if (++line > LINES)
				xerror("Too many options.");
			if ((rcline[line] = malloc((unsigned)(strlen(ptr) + 1))) == NULL)
				xerror("Not enough memory.");
			argvrc[line] = rcline[line];
			(void) strcpy(rcline[line], ptr);
			while (*ptr)
				ptr++;
		}
	}
	myrc = getenv("NEWSRC");
	if (myrc == NULL) {
		myrc = NEWSRC;
		(void) sprintf(newsrc, "%s/%s", userhome, myrc);
	} else {
		(void) strcpy(newsrc, myrc);
	}
	if (access(newsrc, 0))
		newrc(newsrc);
	if ((rcfp = fopen(newsrc, "r")) != NULL) {
		rcreadok = FALSE;
		while (fgets(rcbuf, LBUFLEN, rcfp) != NULL) {
			if (!(space = isspace(*rcbuf)))
				optflag = FALSE;
			if (!STRNCMP(rcbuf, "options ", 8))
				optflag = TRUE;
			if (optflag) {
				(void) strcat(rcbuf, "\1");
				if (space)
					ptr = rcbuf - 1;
				else
					ptr = &rcbuf[7];
				while (*++ptr)
					if (isspace(*ptr))
						*ptr = '\0';
				if (space)
					ptr = rcbuf;
				else
					ptr = &rcbuf[8];
				for (; ; ptr++) {
					if (!*ptr)
						continue;
					if (*ptr == '\1')
						break;
					if (++line > LINES)
						xerror("Too many options.");
					if ((rcline[line] = malloc((unsigned)(strlen(ptr) + 1))) == NULL)
						xerror("Not enough memory.");
					argvrc[line] = rcline[line];
					(void) strcpy(rcline[line], ptr);
					while (*ptr)
						ptr++;
				}
			}
		}
		(void) fclose(rcfp);
		rcreadok = TRUE;
	}
	if (line != -1) {
#ifdef DEBUG
		register int i;
		for (i = 0; i <= line; i++)
			fprintf(stderr, "options:  %s\n", rcline[i]);
#endif
		process(line + 2, argvrc);
		do {
#ifdef DEBUG
			fprintf(stderr, "Freeing %d\n", line);
#endif
			free(rcline[line]);
		} while (line--);
	}

	argv++;
	(void) strcat(header.nbuf, ADMSUB);
	process(argc, argv);
	if (!nflag)
		(void) sprintf(header.nbuf, "%s,%s", ADMSUB, DFLTSUB);
	else {
		char *p = rindex(header.nbuf, ',');
		if (p && p[1] == '\0')	/* strip of trailing NGDELIM */
			*p ='\0';
	}
	lcase(header.nbuf);
	makehimask(header.nbuf, "junk");
	makehimask(header.nbuf, "control");
	makehimask(header.nbuf, "test");

	setbuf(stdout, SYSBUF);
	SigTrap = FALSE;	/* true if a signal has been caught */
	if (!pflag && !lflag && !eflag) {
		(void) signal(SIGQUIT, SIG_IGN);
		(void) signal(SIGHUP, cleanup);
		(void) signal(SIGINT, onsig);
		(void) signal(SIGPIPE, onsig);
	} else {
		if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
			(void) signal(SIGQUIT, cleanup);
		if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
			(void) signal(SIGHUP, cleanup);
		if (signal(SIGINT, SIG_IGN) != SIG_IGN)
			(void) signal(SIGINT, cleanup);
	}

	/*
	 * ALL of the command line has now been processed. (!)
	 */

	if (!*header.nbuf)
		strcpy(header.nbuf, DFLTSUB);
	if (sflag) {
		printf("Subscription list:  %s\n", header.nbuf);
		xxit(0);
	}
	if (xflag)
		line = -1;
	rcfp = xfopen(newsrc, "r");
	while (fgets(rcbuf, LBUFLEN, rcfp) != NULL) {
		if (!nstrip(rcbuf))
			xerror(".newsrc line too long");
		if (++line >= LINES)
			xerror("Too many .newsrc lines");
		if ((rcline[line] = malloc((unsigned)(strlen(rcbuf) + 1))) == NULL)
			xerror("Not enough memory");
		(void) strcpy(rcline[line], rcbuf);
	}
	fclose(rcfp);

	if (SigTrap) {
		if (SigTrap == SIGHUP || !rcreadok)
			xxit(0);
		fprintf(stdout, "Abort (n)?  ");
		(void) fflush(stdout);
		if (gets(bfr) == NULL || *bfr == 'y' || *bfr == 'Y')
			xxit(0);
		SigTrap = FALSE;
	}
#ifdef SERVER
    if ((actfp = open_active()) == NULL)
		xerror("Cannot open active newsgroups file");
    strcpy(ACTIVE, active_name());
    (void) fclose(actfp);
    actfp = NULL;
#endif	/* !SERVER */
	sortactive();
	actfp = xfopen(ACTIVE, "r");
#ifdef DEBUG
	fprintf(stderr, "header.nbuf = %s\n", header.nbuf);
#endif /* DEBUG */
	if (Kflag)
		news++;
	else {
		switch (mode) {
		case UNKNOWN:
			readr();
			break;
#ifdef TMAIL
		case MAIL:
			Mail();
			break;
#endif /* TMAIL */
		}
	}

	cleanup(0);
	/*NOTREACHED*/
}

cleanup(signo)
{
	extern short ospeed;

	(void) signal(SIGHUP, SIG_IGN);
	(void) fflush(stdout);
	if (news && !xflag && !lflag && !tflag) {
		if (*groupdir && mode != MAIL)
			updaterc();
		writeoutrc();
	}
	/*
	 * stop vnews from clearing the screen if we're
	 * killed by a hangup
	 */
	if (signo == SIGHUP)
		ospeed = 0;
	xxit(0);
}

/*
 * Write out the .newsrc file. It's already been cleaned up by sortactive()
 * Take care that data is all written, and flushed, before we destroy the
 * old copy.
 */
writeoutrc()
{
	FILE *wrcfp;
	char aline[BUFLEN];
	register int i;

	/* NEVER write it out if xflag */
	if (xflag || !rcreadok)
		return;

	(void) strcpy(aline, newsrc);
	(void) strcat(aline, ".new");

#ifdef VMS
	(void) vmsdelete(aline);
#endif
	wrcfp = xfopen(aline, "w");

	for (i = 0; i <= line; i++) {
		if (rcline[i] != NULL)
			if (fprintf(wrcfp, "%s\n", rcline[i]) < 0)
				goto fouled;
	}
	if (fclose(wrcfp) < 0)
		goto fouled;

#ifdef VMS
	(void) vmsdelete(newsrc);
#endif
	if (rename(aline, newsrc) < 0)
		xerror("Cannot rename %s to %s", aline, newsrc);
	return;

  fouled:
	xerror("Error writing new .newsrc file - no changes made\n");
	return;
}

/*
 * Forbid newsgroup ng, unless he asked for it in nbuf.
 */
makehimask(nbuf, ng)
char *nbuf, *ng;
{
	if (!findex(nbuf, ng))
		(void) sprintf(rindex(nbuf, '\0'), ",!%s", ng);
}

/*
 * Return true if the string searchfor is in string, but not if preceded by !.
 */
findex(string, searchfor)
char *string, *searchfor;
{
	register char first;
	register char *p;

	first = *searchfor;
	for (p=index(string, first); p; p = index(p+1, first)) {
		if (((p==string) || (p[-1]!='!')) && STRNCMP(p, searchfor, strlen(searchfor)) == 0)
			return TRUE;
	}
	return FALSE;
}
