/*
 * readnews - read news articles.
 */

static char	*SccsId = "@(#)readnews.c	2.10	4/23/83";

#include "rparams.h"

/*
 * readnews - article reading program
 */

#ifndef SYSBUF
char	SYSBUF[BUFSIZ];	/* to buffer std out */
#endif

#define OPTION	0	/* pick up an option string */
#define STRING	1	/* pick up a string of arguments */

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
	char	*user, *home;
	struct passwd *pw;
	struct group *gp;
	int	i, optflag = FALSE, space = FALSE;
	struct utsname ubuf;
	char	*myrc;
#ifdef NOTIFY
	FILE		*nfd;		/* notify file descriptor */
#endif

	/* set up defaults and initialize. */
	pathinit();
	mode = UNKNOWN;
	header.title[0] = header.nbuf[0] = '\0';
	titlebuf[0] = coptbuf[0] = datebuf[0] = '\0';
	uname(&ubuf);
	strcpy(SYSNAME, ubuf.nodename);
	SYSNAME[SNLN] = '\0';

	savmask = umask(N_UMASK);	/* set up mask */
	uid = getuid();
	gid = getgid();
	duid = 0;
	dgid = 0;

#ifndef V6
#ifndef SHELL
	if ((SHELL = getenv("SHELL")) == NULL)
		SHELL = "/bin/sh";
#endif
#ifndef IHCC
	/*
	 * IHCC does not allow use of $LOGNAME to prevent forgery.
	 * Note that this shouldn't matter in readnews, since inews
	 * does all the actual posting of news.
	 */
	if ((user = getenv("USER")) == NULL)
		user = getenv("LOGNAME");
	if ((home = getenv("HOME")) == NULL)
		home = getenv("LOGDIR");
#endif
	if (user == NULL || home == NULL)
		getuser();
	else {
		strcpy(username, user);
		strcpy(header.path, username);
		strcpy(userhome, home);
	}

	getuser();
	if (!(MAILER = getenv("MAILER")))
		MAILER = "mail";	/* was /bin/mail */

#ifdef PAGE
	if (myrc = getenv("PAGER"))
		strcpy(PAGER, myrc);
	else
# ifdef IHCC
		sprintf(PAGER,"%s/bin/%s",logdir(HOME),PAGE);
# else
		strcpy(PAGER, PAGE);
# endif
#else
	strcpy(PAGER, "");
#endif

	if (ptr = getenv("NEWSOPTS"))
		strcpy(rcbuf, ptr);
	else
		*rcbuf = '\0';
	if (*rcbuf) {
		strcat(rcbuf, " \1");
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
				xerror("Too many options.\n");
			if ((rcline[line] = malloc(strlen(ptr) + 1)) == NULL)
				xerror("Not enough memory.\n");
			argvrc[line] = rcline[line];
			strcpy(rcline[line], ptr);
			while (*ptr)
				ptr++;
		}
	}
#else
	getuser();
#endif
	myrc = getenv("NEWSRC");
	if (myrc == NULL) {
		myrc = NEWSRC;
		sprintf(newsrc, "%s/%s", userhome, myrc);
	} else {
		strcpy(newsrc, myrc);
	}
	if (access(newsrc, 0))
		newrc(newsrc);
	if ((rcfp = fopen(newsrc, "r")) != NULL) {
		rcreadok = FALSE;
		while (fgets(rcbuf, LBUFLEN, rcfp) != NULL) {
			if (!(space = isspace(*rcbuf)))
				optflag = FALSE;
			if (!strncmp(rcbuf, "options ", 8))
				optflag = TRUE;
			if (optflag) {
				strcat(rcbuf, "\1");
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
						xerror("Too many options.\n");
					if ((rcline[line] = malloc(strlen(ptr) + 1)) == NULL)
						xerror("Not enough memory.\n");
					argvrc[line] = rcline[line];
					strcpy(rcline[line], ptr);
					while (*ptr)
						ptr++;
				}
			}
		}
		fclose(rcfp);
		rcreadok = TRUE;
	}
	if (line != -1) {
#ifdef DEBUG
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
	strcat(header.nbuf, ADMSUB);
	ngcat(header.nbuf);
	process(argc, argv);
	if (!nflag) {
		strcpy(header.nbuf, DFLTSUB);
		ngcat(header.nbuf);
		strcat(header.nbuf, ADMSUB);
		ngcat(header.nbuf);
	}
	if (*header.nbuf)
		lcase(header.nbuf);
	makehimask(header.nbuf, "junk");
	makehimask(header.nbuf, "control");
	makehimask(header.nbuf, "test");

	setbuf(stdout, SYSBUF);
	sigtrap = FALSE;	/* true if a signal has been caught */
	if (!pflag && !lflag && !eflag) {
		signal(SIGQUIT, SIG_IGN);
		signal(SIGHUP, onsig);
		signal(SIGINT, onsig);
		signal(SIGPIPE, onsig);
	}

	/*
	 * ALL of the command line has now been processed. (!)
	 */

	if (!*header.nbuf)
		ngcat(strcpy(header.nbuf, DFLTSUB));
	if (sflag) {
		ngdel(header.nbuf);
		printf("Subscription list:  %s\n", header.nbuf);
		xxit(0);
	}
	if (!xflag && (rcfp = xfopen(newsrc, "r"))) {
		while (!sigtrap && fgets(rcbuf, LBUFLEN, rcfp) != NULL) {
			if (!nstrip(rcbuf))
				xerror(".newsrc line too long");
			if (++line >= LINES)
				xerror("Too many .newsrc lines");
			if ((rcline[line] = malloc(strlen(rcbuf) + 1)) == NULL)
				xerror("Not enough memory");
			strcpy(rcline[line], rcbuf);
		}
		fclose(rcfp);
	}
	actfp = xfopen(ACTIVE, "r");

#ifdef DEBUG
	fprintf(stderr, "header.nbuf = %s\n", header.nbuf);
#endif
	switch (mode) {
	case UNKNOWN:
		readr();
		break;
#ifdef TMAIL
	case MAIL:
		Mail();
		break;
#endif
	}
	fflush(stdout);
	if (xflag || lflag || tflag)
		xxit(0);
	if (*groupdir && mode != MAIL)
		updaterc();
	writeoutrc();
	xxit(0);

	/* Camel, R.O.H. */
}

/*
 * Write out the .newsrc file.  We sort it into "active" file order,
 * for speed in future invocations, and to get rid of junk.
 */
writeoutrc()
{
	FILE *rcfp, *actfp;
	char afline[BUFLEN];
	register int i, c;
	register char *p;

	if (!rcreadok)
		return;
#ifdef VMS
	unlink(newsrc);
#endif

	rcfp = xfopen(newsrc, "w");
	actfp = xfopen(ACTIVE, "r");

	/* Write out options line, continuations, and comments. */
	for (i=0; ;i++) {
		c = rcline[i][0];
		if (c != 'o' && c != '#' && c != ' ' && c != '\t')
			break;
		if (c == 'o' && strncmp(rcline[i], "options", 7) != 0)
			break;
		fprintf(rcfp, "%s\n", rcline[i]);
	}

	/* For each newsgroup in active, find that newsrc line and write it out. */
	while (fgets(afline, sizeof afline, actfp)) {
		p = index(afline, ' ');
		if (p)
			*p = 0;
		i = findrcline(afline);
		if (i >= 0)
			fprintf(rcfp, "%s\n", rcline[i]);
	}
	fclose(rcfp);
	fclose(actfp);
}

/*
 * Forbid newsgroup ng, unless he asked for it in nbuf.
 */
makehimask(nbuf, ng)
char *nbuf, *ng;
{
	if (!findex(nbuf, ng)) {
		ngcat(nbuf);
		strcat(nbuf, "!");
		strcat(nbuf, ng);
		ngcat(nbuf);
	}
}

/*
 * Return true if the string searchfor is in string, but not if preceeded by !.
 */
findex(string, searchfor)
char *string, *searchfor;
{
	register char first;
	register char *p;

	first = *searchfor;
	for (p=index(string, first); p; p = index(p+1, first)) {
		if (p==string || (p>string && p[-1] != '!' && strncmp(p, searchfor, strlen(searchfor)) == 0))
			return TRUE;
	}
	return FALSE;
}
