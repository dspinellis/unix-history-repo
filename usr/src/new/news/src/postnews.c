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
 * Postnews: post a news message to Usenet.  This C version replaces a shell
 * script, and does more intelligent prompting and filtering than possible
 * in a shell script.
 */
#ifdef SCCSID
static char	*SccsId = "@(#)postnews.c	1.33	10/15/87";
#endif /* SCCSID */

#include "params.h"

# ifndef ROOTID
extern int ROOTID;
# endif
#define APPEND 1
#define REPLACE 2

extern char *MAILPARSER;

char tempfname[50];		/* file name used for making article */
char original[BUFLEN];		/* file name of original, used in followup */
char homedir[BUFLEN];		/* HOME environment setting */
char user[SBUFLEN];		/* user name */
char ccname[BUFLEN];		/* file name for article copy */

/* article header information */
char subject[BUFLEN];
char distribution[BUFLEN];
char references[BUFLEN];
char newsgroups[BUFLEN];
char isfrom[BUFLEN];
char msgid[BUFLEN];
char keywords[BUFLEN];
char summary[BUFLEN];

char ngsep[] = { NGDELIM, '\0' };	/* == "," */

char *Progname = "postnews";		/* for xerror */

time_t fmodtime;
char buf[BUFLEN];

#define MAXDISTR	16
struct distr {
	char abbr[24];
	char descr[128];
} distr[MAXDISTR];

char def_distr[24] = "";	/* default distribution */
FILE *xfopen();

main(argc, argv)
char *argv[];
{
	register int c;

	init();

	if (argc == 2) {
		if (!PREFIX(argv[1], SPOOL))
			xerror("Can only followup to articles in %s", SPOOL);
		followup(argv[1]);
		(void) strcpy(original, argv[1]);
	} else
	if (askyes("Is this message in response to some other message? ","no")) {
		char ng[BUFLEN], num[BUFLEN];
		long i, j, lastnum;
		register char *p;
		int fd, dir;
		char canpost;

		getpr("In what newsgroup was the article posted? ",ng);
		if (!valid_ng(ng, &i, &j, &canpost))
			if (canpost == 'i' )
				byebye("There is no such newsgroup.");
			else if (canpost == 'n')
				byebye("You are not allowed to post to that group.");

		printf("Valid article numbers are from %ld to %ld\n", j, i);
		lastnum = i + 1;
		dir = -1;

		for(;;) {
			getpr("\nWhat was the article number? ", num);
			switch(num[0]) {
			case '+':
				dir = 1;
				sprintf(num, "%ld", lastnum + 1);
				break;
			case '-':
				dir = -1;
				/* no break */
			case '\0':
				sprintf(num, "%ld", lastnum + dir);
				break;
			}
#ifdef SERVER
			if (getarticle(ng, num, "ARTICLE") == NULL)
					goto nothere;
			strcpy(original, article_name());
#else	/* !SERVER */
			(void) sprintf(original, "%s/%s", SPOOL, ng);
			for (p=original+strlen(SPOOL)+1; *p ;++p)
				if (*p == '.')
					*p = '/';
			(void) strcat(original, "/");
			(void) strcat(original, num);
#endif	/* !SERVER */
			if ((fd=open(original,0)) >= 0) {
				(void) close(fd);
				printf("\narticle %s\n", original);
				if (article_line(original, "From: ", buf))
					printf("%s\n", buf);
				if (article_line(original, "Subject: ", buf))
					printf("%s\n", buf);
				if (askyes("Is this the one you want? ", "n"))
					break;
			} else
#ifdef SERVER
nothere:
#endif	/* !SERVER */
				printf("I can't find that article.\n");
			lastnum = atol(num);
		}

		followup(original);
	} else {
		do {
			getpr("Subject: ", subject);
			if (*subject == '?') {
printf("People read the subject line to learn what your article is about.\n");
printf("You want it to do the same job as a newspaper headline.\n");
printf("So type in something both brief and descriptive.\n");
				*subject = '\0';
			}
		} while (*subject == '\0');
		getpr("Keywords: ", keywords);

		while (!get_newsgroup())
			;
		get_distribution((char *)0);
	}

	if (pre_checks())
		exit(1);

	prep_article();
	c = 'e';
	for (;;) {
		if (c == 'e') {
			edit_article();
			post_checks();
		}
		do {
			do {
				getpr("\nWhat now?  [send, edit, list, quit, write, append] ", buf);
				c = buf[0];
			} while (c == '\0');
			if (isupper(c))
				c = tolower(c);
			if (c == 'q') {
				(void) UNLINK(tempfname);
				exit(1);
			}
			if (c == 'l') {
				char *pager = getenv("PAGER");
				char lbuf[BUFLEN];
				if (pager == NULL || *pager == '\0') {
#ifdef PAGE
# ifdef LOGDIR
					(void) sprintf(lbuf,"%s/bin/%s", logdir(HOME), PAGE);
# else /* !LOGDIR */
					(void) strcpy(lbuf, PAGE);
# endif /* !LOGDIR */
					pager = lbuf;
#else /* !PAGE */
					pager = "cat";
#endif /* !PAGE */
				}
				sprintf(buf, "exec %s %s", pager, tempfname);
				(void)  system(buf);
			}
			if (c == 'w' || c == 'a') {
				register int ifd, ofd, nbytes;
				char iobuf[BUFSIZ];
				char fname[BUFLEN];
				getpr("Filename? ", fname);
				if (fname[0] == '\0')
					continue;
				ofd = (c == 'w') ? creat(fname, 0666)
						 : open(fname, 2);
				if (ofd < 0)
					perror(fname);
				else {
					if (c == 'a')
						(void) lseek(ofd, 0L, 2);
					ifd = open(tempfname, 0);
					if (ifd < 0)
						xerror("Can't reopen %s\n", tempfname);
					while ((nbytes = read(ifd, iobuf, BUFSIZ)) > 0 )
						write(ofd, iobuf, nbytes);
					close(ofd);
					close(ifd);
				}
			}
		} while (!index("eps", c));
		if (c != 'e')
			post_article(); /* will exit if posted successfully */
	};
}

/*
 * Find out the topic of interest.
 */
get_newsgroup()
{
	int n;
	long i;
	char canpost;
	static int first = 1;

	printf("Newsgroups (enter one at a time, end with a blank line):\n");
	if (first) {
		printf("\nThe most relevant newsgroup should be the first, you should\n");
		printf("add others only if your article really MUST be read by people\n");
		printf("who choose not to read the appropriate group for your article.\n");
		printf("But DO use multiple newsgroups rather than posting many times.\n\n");
		first = 0;
	}
#ifndef SERVER
	printf("For a list of newsgroups, type ?\n");
#endif	/* !SERVER */
	n = 0;
	newsgroups[0] = '\0';

	for(;;) {
		getpr("> ", buf);
		if (buf[0] == '\0')
			if (n == 0)
				return FALSE;
			else
				return TRUE;
#ifndef SERVER
		if (buf[0] == '?'){
			char *pager = getenv("PAGER");
			char lbuf[BUFLEN];
			if (pager == NULL) {
#ifdef PAGE
# ifdef LOGDIR
				(void) sprintf(lbuf,"%s/bin/%s", logdir(HOME), PAGE);
# else /* !LOGDIR */
				(void) strcpy(lbuf, PAGE);
# endif /* !LOGDIR */
				pager = lbuf;
#else /* !PAGE */
				pager = "cat";
#endif /* !PAGE */
			}
			printf("These are the currently active groups:\n");
			(void) fflush(stdout);
			sprintf(buf, "exec %s %s/newsgroups", pager, LIB);
			(void) system(buf);
			continue;
		}
#endif	/* !SERVER */
		if (valid_ng(buf, &i, &i, &canpost)) {
			if (n++ != 0)
				(void) strcat(newsgroups, ngsep);
			(void) strcat(newsgroups, buf);
		} else {
			if (canpost == 'n')
				printf("You are not allowed to post to %s\n",
					buf);
			else if (canpost == 'i')
				printf("%s is not a valid newsgroup.\n", buf);
		}
	}
}

/*
 * Find out how widely the author wants the message distributed.
 */
get_distribution(deflt)
	char *deflt;
{
	register int i;
	register char *r;
	char def[BUFLEN];
	char *lastgroup;

	lastgroup = newsgroups;
	(void) strcpy(def, newsgroups);
	r = index(def, NGDELIM);
	if (r)
		*r = '\0';
	r = index(def, '.');
	if (r) {
		*r = '\0';
		if (STRCMP(def, "net") == 0)
			(void) strcpy(def, "world");
	} else {
		distribution[0] = '\0';
		return;
	}

	if (STRCMP(def, "to") == 0) {
		/*
		 * This only works if "to.xx" is the first (or only)
		 * newsgroup, but it usually is ..
		 * Perhaps we should make the distribution be "to.xxx" ??
		 */
		distribution[0] = '\0';
		return;		/* He's probably testing something */
	}
	if (deflt != (char *)0)
		(void) strcpy(def, deflt);
	if (ngmatch("misc.test", newsgroups))
		(void) strcpy(def, "local");
	for (i=0; distr[i].abbr[0]; i++) {
		if (STRCMP(distr[i].abbr, def) == 0)
			break;
	}
	if (distr[i].abbr[0] == '\0')
		strcpy(def, def_distr);
	for(;;) {
		do {
			(void) sprintf(buf, "Distribution (default='%s', '?' for help) : ", def);
			getpr(buf, distribution);
			if (distribution[0] == '\0') {
				if (STRCMP(def, "*None*") == 0)
					printf("You must enter a distribution, '?' for help.\n");
				(void) strcpy(distribution, def);
			}
		} while (STRCMP(distribution, "*None*") == 0);

		/* Did the user ask for help? */
		if (distribution[0] == '?') {
			printf("How widely should your article be distributed?\n\n");
			for (i=0; distr[i].abbr[0]; i++)
				printf("%s\t%s\n", distr[i].abbr, distr[i].descr);
			printf("\nEnter the word that specifies the distribution that you require.\n");
			continue;
		}

#ifdef SERVER
		return;		/* can't do this yet */
#else	/* !SERVER */
		/* Check that it's a proper distribution */
		for (i=0; distr[i].abbr[0]; i++) {
			if (strncmp(distr[i].abbr, distribution, sizeof(distr[0].abbr)) == 0) {
				return;
			}
		}
		if (STRCMP(distribution, def) != 0)
			printf("Type ? for help.\n");
		else {
			int once = TRUE;

			do {
				r = lastgroup;
				while (r = index(r, NGDELIM))
					if (!PREFIX(++r, def))
						break;
				if (r == NULL) {
					/*
					 * no newsgroups are distribution
					 * names, and user simply will
					 * not type a valid distribution,
					 * assume that the default is OK.
					 */
					distribution[0] = '\0';
					return;
				}
				lastgroup = r;
				if (once)
					printf("Sorry, '%s' is an unknown distribution.  Type ? for help.\n", def);
				once = FALSE;
				strcpy(def, r);
				r = index(def, NGDELIM);
				if (r)
					*r = '\0';
				r = index(def, '.');
			} while (r == NULL);
			*r = '\0';
			if (STRCMP(def, "net") == 0)
				strcpy(def, "world");
		}
#endif	/* !SERVER */
	}
}

/*
 * Do sanity checks before the author types in the message.
 */
pre_checks()
{
	if (recording(newsgroups))
		return 1;
	return 0;
}

/*
 * Set up the temp file with headers.
 */
prep_article()
{
	FILE *tf, *of;
	struct stat stbuf;

	(void) strcpy(tempfname, "/tmp/postXXXXXX");
	(void) mktemp(tempfname);

	/* insert a header */
	tf = xfopen(tempfname, "w");
	fprintf(tf, "Subject: %s\n", subject);
	fprintf(tf, "Newsgroups: %s\n", newsgroups);
	if (distribution[0] != '\0' && STRCMP(distribution, "world"))
		fprintf(tf, "Distribution: %s\n", distribution);

	if (keywords[0] != '\0')
		fprintf(tf, "Keywords: %s\n", keywords);
	if (summary[0] != '\0')
		fprintf(tf, "Summary: %s\n", summary);

	if (references[0] != '\0') {
		fprintf(tf, "References: %s\n\n", references);

		if (askyes("Do you want to include a copy of the article? ", "no")){
			of = xfopen(original, "r");
			while (fgets(buf, BUFSIZ, of) != NULL)
				if (buf[0] == '\n')	/* skip headers */
					break;
			fprintf(tf, "In article %s, %s writes:\n", msgid, isfrom);
			while (fgets(buf, BUFSIZ, of) != NULL)
				fprintf(tf, "> %s", buf);
			(void) fclose(of);
			printf("OK, but please edit it to suppress unnecessary verbiage, signatures, etc.\n");
			(void) fflush(stdout);
		}
	}

	fprintf(tf, "\n\n");
	(void) fflush(tf);
	(void) fstat(fileno(tf), &stbuf);
	fmodtime = stbuf.st_mtime;
	(void) fclose(tf);
}

edit_article()
{
	register char *p;
	char *editor;
	char *endflag = "";
	char *getenv();

	/* edit the file */
	editor = getenv("EDITOR");
	if (editor == NULL)
		editor = DFTEDITOR;

	p = editor + strlen(editor) - 2;
	if (STRCMP(p, "vi") == 0)
		endflag = "+";

	(void) sprintf(buf, "A=%s;export A;exec %s %s %s",
		original, editor, endflag, tempfname);

	(void) system(buf);
}

/*
 * Do sanity checks after the author has typed in the message.
 */
post_checks()
{
	char group[BUFLEN];
	register char *c, *p;
	struct stat stbuf;

	if (stat(tempfname, &stbuf) < 0) {
		printf("File deleted - no message posted.\n");
		(void) UNLINK(tempfname);
		exit(1);
	}
	if (stbuf.st_size < 5) {
		printf("File too small (<5 characters) - no message posted.\n");
		(void) UNLINK(tempfname);
		exit(1);
	}

	if (stbuf.st_mtime == fmodtime) {
		printf("File not modified - no message posted.\n");
		(void) UNLINK(tempfname);
		exit(1);
	}

	/*
	 * Is this the right number?  Most of the headers are yet to be added
	 */
	if (stbuf.st_size > 64000) {
		printf("\nYour message will probably be truncated when it\n");
		printf("passes through a notesfile site, since it is\n");
		printf("greater than 64000 characters.\n\n");
		if (!askyes("Do you still want to post it? ","")) {
			sprintf(ccname, "%s/dead.article", homedir);
			save_article();
			(void) UNLINK(tempfname);
			exit(1);
		}
	}

	/*
	 * get the newsgroups from the edited article, in
	 * case they were altered in the editor
	 */
	if (!article_line(tempfname, "Newsgroups: ", group)) {
  nogroups:
		printf("Not sending to any newsgroups - no message posted\n");
		sprintf(ccname, "%s/dead.article", homedir);
		save_article();
		(void) UNLINK(tempfname);
		exit(1);
	}
	c = &group[11];
	while (*c == ' ' || *c == '\t')
		c++;
	if (*c == '\0')
		goto nogroups;
	for (p = newsgroups; *c; c++)	/* copy to newsgroups, w/o blanks */
		if (*c != ' ' && *c != '\t')
			*p++ = *c;
	*p = '\0';

	/* Sanity checks for certain newsgroups */
	if (ngmatch(newsgroups, "all.wanted") && ngmatch(distribution,"world,na,usa,att,btl,eunet,aus")) {
		printf("Is your message something that might go in your local\n");
		printf("newspaper, for example a used car ad, or an apartment\n");
		printf("for rent? ");
		if (askyes("","")) {
			printf("It's pointless to distribute your article widely, since\n");
			printf("people more than a hundred miles away won't be interested.\n");
			printf("Please use a more restricted distribution.\n");
			get_distribution("*None*");
			modify_article(tempfname, "Distribution: ", distribution,REPLACE);
		}
	}

	if (ngmatch(newsgroups, "rec.humor,!rec.humor.all")) {
		if (askyes("Could this be offensive to anyone? ","")) {
			getpr("Whom might it offend? ", group);
			(void) sprintf(buf," - offensive to %s (rot 13)",group);
			modify_article(tempfname, "Subject: ", buf, APPEND);
			encode(tempfname);
		}
	}

	if (ngmatch(newsgroups, "comp.sources.all,!comp.sources.wanted,!comp.sources.d")) {
		if (!article_line(tempfname, "Subject: ", group)) {
  nosubj:
			printf("There seems to be no subject for this article.\n");
			getpr("Subject: ", subject);
			modify_article(tempfname, "Subject: ", subject, REPLACE);
		} else {
			c = &group[8];
			while (*c == ' ' || *c == '\t')
				c++;
			if (*c == '\0')
				goto nosubj;
			strcpy(subject, c);
		}
		if (ngmatch(newsgroups, "all.wanted") || iswanted(subject)) {
			printf("Requests for sources should not be posted to any of\n");
			printf("the comp.sources newsgroups, please post such requests\n");
			printf("to comp.sources.wanted only.     Please reenter the newsgroups.\n\n");
			while (!get_newsgroup())
				;
			modify_article(tempfname, "Newsgroups: ", newsgroups, REPLACE);
		}
		if (ngmatch(newsgroups, "comp.sources.all")) {
			if (!ngmatch(newsgroups, "comp.sources.wanted") &&
			    stbuf.st_size < (4*1024)) {
				printf("Your article seems rather small to be a source distribution.\n");
				if (!askyes("Are you certain that this is really source? ", "")) {

					while (!get_newsgroup())
						;
					modify_article(tempfname, "Newsgroups: ", newsgroups, REPLACE);
				}
			}
			if (index(newsgroups, NGDELIM)) {
				printf("Sources should be posted to one newsgroup only.\n");
				printf("Please pick the most appropriate group for your article.\n\n");
				while (!get_newsgroup())
					;
				modify_article(tempfname, "Newsgroups: ", newsgroups, REPLACE);
			}
		}
	}
}

iswanted(str)
register char *str;
{
	while (*str == ' ')
		str++;

	if (PREFIX(str, "Re:"))
		return (FALSE);

	if (isin(str, " wanted ") || isin(str, " can any") ||
	    isin(str, " need ") || isin(str, " please ") || isin(str, " help ")
	    || isin(str, " looking ") || index(str, '?'))
		return (TRUE);

	return (FALSE);
}

isin(str, words)
register char *str, *words;
{
	register char *p;
	register sc, wc;

	p = words;
	while (sc = *str++) {
		if ((wc = *p++) == '\0')
			return (TRUE);
		if (wc == ' ') {
			if (index(".,!?-; \t\n", sc))
				continue;
		} else {
			if (isupper(wc))
				wc = tolower(wc);
			if (isupper(sc))
				sc = tolower(sc);
			if (wc == sc)
				continue;
		}
		str -= p - words - 1;
		p = words;
	}
	if (*p == '\0')
		return (TRUE);
	return (FALSE);
}

/*
 * Save a copy of the article in the users NEWSARCHIVE directory.
 */
save_article()
{
	FILE *in, *out;
	int c;
	time_t timenow, time();
	char *today, *ctime();
	struct stat stbuf;
	char filename[BUFLEN];

	if (stat(ccname, &stbuf) == 0 && (stbuf.st_mode&S_IFMT) == S_IFDIR) {
		/*
		 * It would be much nicer here to write articles
		 * in MH format (numbered files, in rfc822 format)
		 *
		 * one day ..
		 */
		(void) sprintf(filename, "%s/author_copy", ccname);
		(void) strcpy(ccname, filename);
	}
	in = xfopen(tempfname, "r");
	out = xfopen(ccname, "a");
	timenow = time((time_t)0);
	today = ctime(&timenow);
	fprintf(out,"From postnews %s",today);
	while ((c=getc(in)) != EOF)
		putc(c, out);
	putc('\n', out);
	(void) fclose(in);
	(void) fclose(out);
}

/*
 * Post the article to the net.
 */
post_article()
{
	int status;

	printf("Posting article...\n");
	fflush(stdout);
	(void) sprintf(buf, "exec %s/%s -h < %s", LIB, "inews", tempfname);
	status = system(buf);

	if (status) {
		printf("Article not posted - exit status %d\n", status);
		return;
	} else
		printf("Article posted successfully.\n");

	if (ccname[0]) {
		printf("A copy has been saved in %s\n", ccname);
		save_article();
	}

	(void) UNLINK(tempfname);
	exit(0);
}

/*
 * Initialization.
 */
init()
{
	FILE *fd;
	register char *p;
	int i;
	char *getenv();
	struct passwd *pw;

	references[0] = '\0';
	distribution[0] = '\0';

	uid = getuid();
	pw = getpwuid(uid);
	if (pw == NULL) {
		fprintf(stderr,"You're not in /etc/passwd\n");
		exit(1);
	}
	p = getenv("HOME");
	if (p == NULL) {
		p = getenv("LOGDIR");
		if (p == NULL) 
			p = pw->pw_dir;
	}
	(void) strncpy(user, pw->pw_name, SBUFLEN);
	(void) strcpy(homedir, p);

	p = getenv("NEWSARCHIVE");
	if (p != NULL) {
		if (*p == '\0')
			sprintf(ccname, "%s/author_copy", homedir);
		else
			strcpy(ccname, p);
	}

	pathinit();
#ifdef SERVER
	if (open_server() < 0) 
		xerror("Server error");
			/* do something to some up with distributions */
	if ((fd = open_active()) == NULL)
		xerror("Server error");
	strcpy(ACTIVE,active_name());
#else	/* !SERVER */
	(void) sprintf(buf, "%s/%s", LIB, "distributions");

	fd = xfopen(buf, "r");
	for (i=0; i < MAXDISTR; i++) {
		if (fscanf(fd, "%s %[^\n]", distr[i].abbr, distr[i].descr)
			!= 2)
			break;
		if (STRCMP(distr[i].abbr, "default") == 0)
			strcpy(def_distr, distr[i--].descr);
	}
#endif	/* !SERVER */
	(void) fclose(fd);
	distr[i].abbr[0] = '\0';
	if (def_distr[0] == '\0')
		strcpy(def_distr, "world");	/* maybe "local" is better? */
}

/*
 * Get a yes or no answer to a question.	A default may be used.
 */
askyes(msg, def)
char *msg, *def;
{
	for(;;) {
		printf("%s", msg);
		buf[0] = 0;
		(void) gets(buf);
		switch(buf[0]) {
		case 'y':
		case 'Y':
			return TRUE;
		case 'n':
		case 'N':
			return FALSE;
		case '\0':
			switch(*def) {
			case 'y':
			case 'Y':
				return TRUE;
			case 'n':
			case 'N':
				return FALSE;
			}
		default:
			printf("Please answer yes or no.\n");
		}
	}
}

/*
 * Get a character string into buf, using prompt msg.
 */
getpr(msg, bptr)
char *msg, *bptr;
{
	static int numeof = 0;
	printf("%s", msg);
	(void) gets(bptr);
	(void) nstrip(bptr);
	if (feof(stdin)) {
		if (numeof++ > 3) {
			fprintf(stderr,"Too many EOFs\n");
			exit(1);
		}
		clearerr(stdin);
	}
}

byebye(mesg)
char *mesg;
{
	printf("%s\n", mesg);
	exit(1);
}

/*
 * make a modification to the header of an article
 *
 *	 fname -- name of article
 *	 field -- header field to modify
 *	 line	-- modification line
 *	 how	 -- APPEND or REPLACE
 *
 * example:
 *	 modify_article("/tmp/article" , "Subject:" , "new subject" , REPLACE);
 *
 *
 */
modify_article(fname, field, line, how)
char *fname, *field, *line;
{
	FILE *fpart, *fptmp;
	char *temp2fname = "/tmp/postXXXXXX";
	char lbfr[BUFLEN];
	register found = FALSE;

	mktemp(temp2fname);

	fptmp = xfopen(temp2fname, "w");
	fpart = xfopen(fname, "r");

	while (fgets(lbfr, BUFLEN, fpart) != NULL) {
		if (PREFIX(lbfr, field)) {
			found = TRUE;
			(void) nstrip(lbfr);
			if (how == APPEND) {
				/* append to current field */
				(void) strcat(lbfr, line);
				(void) strcat(lbfr, "\n");
			} else
				/* replace current field */
				(void) sprintf(lbfr, "%s%s\n", field, line);
		}
		(void) fputs(lbfr, fptmp);
	}

	fclose(fpart);
	fclose(fptmp);

	fptmp = xfopen(temp2fname, "r");
	fpart = xfopen(fname, "w");

	if (!found)
		fprintf(fpart, "%s%s\n", field, line);
	while (fgets(buf,BUFLEN,fptmp) != NULL)
		(void) fputs(buf, fpart);

	(void) fclose(fpart);
	(void) fclose(fptmp);
	(void) UNLINK(temp2fname);
}


/* verify that newsgroup exists, and get number of entries */
valid_ng(ng, maxart, minart, canpost)
char *ng;
long *maxart, *minart;
char *canpost;
{
	char ng_check[BUFLEN], ng_read[BUFLEN];
	FILE *fp;

	fp = xfopen(ACTIVE, "r");
	while (fgets(ng_read, BUFLEN, fp) != NULL) {
		switch (sscanf(ng_read, "%s %ld %ld %c", ng_check, maxart, minart, canpost)) {
		case 2:
			*minart = 1;
			/* fall through */
		case 3:
			*canpost = 'y';
			/* fall through */
		case 4:
			break;

		default:
			printf("Active file (%s) corrupted. ", ACTIVE);
			byebye("Seek help!");
		}
			
		if (STRCMP(ng_check, ng) == 0) {
			(void) fclose(fp);
			if (*canpost != 'n') {
#ifdef FASCIST
				if (uid && uid != ROOTID && fascist(user, ng)) {
					*canpost = 'n';
					return FALSE;
				}
#endif /* FASCIST */
				return TRUE;
			} else
				return FALSE;
		}
	}
	*canpost = 'i';
	*maxart = 0;
	*minart = 0;
	(void) fclose(fp);
	return FALSE;
}

/* get the line specified by field from an article */
article_line(article, field, line)
char *article, *field, *line;
{
	FILE *fp;
	char *c;

	fp = xfopen(article,"r");
	while ((c=fgets(line,BUFLEN,fp)) != NULL && !PREFIX(line, field))
		if (line[0] == '\n') {
			c = NULL;
			break;
		}
	(void) fclose(fp);
	if (c != NULL) {
		(void) nstrip(line);
		return TRUE;
	} else {
		line[0] = '\0';
		return FALSE;
	}
}

/* get the header information for a followup article */
followup(baseart)
register char *baseart;
{
	register char *p;

	/* subject */
	if (article_line(baseart, "Subject: ", buf)) {
		p = buf+9;
		for ( ; ; ) {
			while (*p == ' ' || *p == '\t')
				++p;
			if ((*p != 'r' && *p != 'R') ||
				(p[1] != 'e' && p[1] != 'E') ||
				(p[2] != ':' && p[2] != ' '))
					break;
			p += 3;
		}
		(void) sprintf(subject, "Re: %s", p);
	} else {
		if (article_line(baseart, "From: ", buf))
			(void) sprintf(subject, "Re: orphan response from %s", buf);
		else
			(void) strcpy(subject, "Re: orphan response");
	}

	/* newsgroup */
	if (article_line(baseart, "Newsgroups: ", buf))
		(void) strcpy(newsgroups, buf+12);
	if (ngmatch(newsgroups, "misc.jobs.all,!misc.jobs.misc")) {
		printf("Your followup has been directed to misc.jobs.misc\n");
		printf("It is the proper place for followup discussions\n");
		(void) strcpy(newsgroups,"misc.jobs.misc");
	}

	/* distribution */
	if (article_line(baseart, "Distribution: ", buf))
		(void) strcpy(distribution, buf+14);

	/* references */
	if (article_line(baseart, "References: ", buf)) {
		register char *rcp;
		(void) strcpy(references, buf+12);
		(void) strcat(references, " ");
		/* keep the number of references to a reasonable number */
		rcp = rindex(references, ' '); /* Can not fail */
		while ((int)(rcp - references) > 70) {
			while (*--rcp != ' ')
				;
			rcp[1] = '\0';
		}
	}
	if (article_line(baseart, "Message-ID: ", buf)) {
		(void) strcat(references, buf+12);
		(void) strcpy(msgid, buf+12);
	}

	if (article_line(baseart, "From: ", buf))
		(void) strcpy(isfrom, buf+6);

	if (article_line(baseart, "Keywords: ", buf))
		(void) strcpy(keywords, buf+10);

	if (article_line(baseart, "Followup-To: ", buf)) {
		(void) strcpy(newsgroups, buf+13);
		if (STRCMP(newsgroups, "poster") == 0)
			byebye("Mail followups directly to poster.");
	}

	get_summary();
}

get_summary()
{
	register char *p;
	register i;

	printf("Please enter a short summary of your contribution to the discussion\n");
	printf("Just one or two lines ...   (end with a blank line)\n");
	p = summary;
	for (i = 0; i < 3; i++) {	/* 3 * 80 < 256, should be safe .. */
		getpr(">\t", p);
		if (*p == '\0')
			break;
		p = index(p, '\0');
		(void) strcpy(p, "\n\t ");
		p += 3;
	}
	if (p > summary)
		p[-3] = '\0';
}

encode(article)
char *article;
{
	FILE *fpart, *fphead, *fpcoded;
	char *headerfile = "/tmp/pheadXXXXXX";
	char *codedfile = "/tmp/pcodeXXXXXX";

	(void) mktemp(headerfile);
	(void) mktemp(codedfile);

	fpart = xfopen(article, "r");

	/* place article header in "headerfile" file */
	fphead = xfopen(headerfile, "w");
	while (fgets(buf, BUFLEN, fpart) != NULL) {
		(void) fputs(buf, fphead);
		if (buf[0] == '\n')
			break;
	}
	(void) fclose(fphead);

	/* place article body in "codedfile" file */
	fpcoded = xfopen(codedfile, "w");
	while (fgets(buf, BUFLEN, fpart) != NULL)
		(void) fputs(buf, fpcoded);
	(void) fclose(fpcoded);
	(void) fclose(fpart);

	/* encode body and put back together with header */
	(void) rename(headerfile, article);

	(void) sprintf(buf,"exec %s/%s 13 < %s >> %s\n", LIB, "caesar", codedfile, article);
	printf("Encoding article -- please stand by\n");
	(void) fflush(stdout);
	if (system(buf)) {
		printf("encoding failed");
		exit(2);
	}
	(void) UNLINK(codedfile);
}


/*
 * Print a recorded message warning the poor luser what he is doing
 * and demand that he understands it before proceeding.  Only do
 * this for newsgroups listed in LIBDIR/recording.
 */
recording(ngrps)
char *ngrps;
{
	char recbuf[BUFLEN];
	FILE *fd;
	char nglist[BUFLEN], fname[BUFLEN];
	int  c, n, yes, retval = 0;

	(void) sprintf(recbuf, "%s/%s", LIB, "recording");
	fd = fopen(recbuf, "r");
	if (fd == NULL)
		return 0;
	while ((fgets(recbuf, sizeof recbuf, fd)) != NULL) {
		(void) sscanf(recbuf, "%s %s", nglist, fname);
		if (ngmatch(ngrps, nglist)) {
			(void) fclose(fd);
			if (fname[0] == '/')
				(void) strcpy(recbuf, fname);
			else
				(void) sprintf(recbuf, "%s/%s", LIB, fname);
			fd = fopen(recbuf, "r");
			if (fd == NULL)
				return 0;
			while ((c = getc(fd)) != EOF)
				putc(c, stderr);
			fclose(fd);
			fprintf(stderr, "Do you understand this?  Hit <return> to proceed, <BREAK> to abort: ");
			fflush(stderr);
			n = read(0, recbuf, 100);
			c = recbuf[0];
			yes = (c=='y' || c=='Y' || c=='\n' || c=='\n' || c==0);
			if (n <= 0 || !yes)
				retval = -1;
		}
	}
	return retval;
}

xxit(i)
{
	exit(i);
}

#if !defined(BSD4_2)
rename(from,to)
register char *from, *to;
{
	(void) unlink(to);
	if (link(from, to) < 0)
		return -1;

	(void) unlink(from);
	return 0;
}
#endif /* !BSD4_2 && ! BSD4_1C */
