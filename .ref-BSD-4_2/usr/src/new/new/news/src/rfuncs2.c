/*
 * rfuncs2 - more routines needed by readr.
 */
static char *sccsid = "@(#)rfuncs2.c	1.9	4/25/83";

#include "rparams.h"

static char	lbuf[BUFLEN*2];

FILE *popen();

/*
 * nglist is the list of newsgroups in an article we want to follow up.
 * Do any special fascist processing to prevent certain kinds of followups.
 * In this case, there are two things we want to do:
 *	All followups to "net.general" are fed to "net.followup".
 *	However, if "net.general" is mentioned along with "net.news.group",
 *		just remove the net.general.
 */
launder(nglist)
char	*nglist;
{
	char	*cp, *op;
	char	outbuf[128];
	int	seen_group = 0;

	for (cp = index(nglist, 'n'); cp; cp = index(cp + 1, 'n'))
		if (strncmp("news.group", cp, 10) == 0)
			seen_group++;
	for (cp = index(nglist, 'n'); cp; cp = index(cp + 1, 'n'))
		if (strncmp("net.general", cp, 11) == 0) {
			/* 11 = strlen("net.general") */
			strcpy(outbuf, cp + 11);
			if (!seen_group) {
				strcpy(cp, "net.followup");
				cp += 12;  /* 12 = strlen("net.followup") */
			}
			if (cp[-1] == ',' && outbuf[0] == ',')
				cp--;
			strcpy(cp, outbuf);
		}
}


/*
 * Match title.
 */
titmat(h, titlist)
register struct hbuf *h;
register char	*titlist;
{
	register char	*p;
	register int	titlen;

	while (*titlist != '\0') {
		titlen = strlen(titlist);
		for (p = h->title; *p != '\0'; p++)
			if (strncmp(p, titlist, titlen) == 0) {
				return(TRUE);
			}
		titlist += titlen + 1;
	}
	return(FALSE);
}


/*
 * Save the news item in the user's file.
 * Fri Mar 12 20:04:43 EST 1982: (ittvax!swatt)
 *	Allow files with first character as '|' to write article
 *	to program across a pipe.
 */

#define PIPECHAR '|'

save(file, to)
register char	*file, *to;
{
	register FILE *ufp, *hfp;
	struct hbuf hh;
	int	isprogram = 0;
	int	isnew = 1;

	if ((hfp = fopen(file, "r")) == NULL) {
		printf("Can't get article.\n");
		return;
	}
	if (hread(&hh, hfp, TRUE) == NULL) {
		printf("Article is garbled.\n");
		return;
	}
	ufp = fopen(to, "r");
	if (ufp != NULL) {
		fclose(ufp);
		isnew = 0;
	}
	setgid(gid);
	setuid(uid);
	umask(savmask);

	if (*to == PIPECHAR) {
		if ((ufp = popen (&to[1], "w")) == NULL) {
			printf ("Cannot execute %s\n", &to[1]);
			return;
		}
		isprogram++;
	} else if ((ufp = fopen(to, "a")) == NULL) {
		printf("Cannot append to %s.\n", to);
		return;
	}
	/*
	 * V7MAIL code is here to conform to V7 mail format.
	 * If you need a different format to be able to
	 * use your local mail command (such as four ^A's
	 * on the end of articles) substitute it here.
	 */
#ifdef V7MAIL
	fprintf(ufp, "From %s %s",
#ifdef INTERNET
				hh.from,
#else
				hh.path,
#endif
					ctime(&hh.subtime));
#endif
	hprint(&hh, ufp, 2);
#ifdef V7MAIL
	tprint(hfp, ufp, TRUE);
	putc('\n', ufp);	/* force blank line at end (ugh) */
#else
	tprint(hfp, ufp, FALSE);
#endif
	fclose(hfp);
	if (isprogram)
		pclose (ufp);
	else
		fclose(ufp);
	if (!isprogram)
		printf("%s: %s\n", to, isnew ? "New file" : "Appended");
}


/*
 * Print out the rest of the article.
 */
tprint(ifp, ofp, checkfrom)
register FILE *ifp, *ofp;
int checkfrom;
{
	register int	c;

	while ((fgets(bfr, sizeof bfr, ifp)) != NULL && !sigtrap) {
		if (checkfrom && strncmp(bfr, "From ", 5) == 0)
			putc('>', ofp);
		fputs(bfr, ofp);
	}
	if (sigtrap)
		qfflush(ofp);
	fflush(ofp);
	fprintf(ofp, (sigtrap ? "\n\n" : "\n"));
	sigtrap = FALSE;
}


/*
 * Print the file header.
 */
hprint(hp, ofp, verbose)
register struct hbuf *hp;
int	verbose;
register FILE *ofp;
{
	register char	*p1, *p2;
	char	fname[BUFLEN];
	char *tailpath();

	fname[0] = '\0';		/* init name holder */

	if (verbose == 2) {
		lhwrite(hp, ofp);
		return;
	}

	if (lflag || eflag) {
		char buf1[80], buf2[200];
		char *cp;

		strcpy(bfr, groupdir);
		for (cp=bfr; *cp; cp++)
			if (*cp == '/')
				*cp = '.';
		sprintf(buf1, "%s/%d", bfr, bit);
		sprintf(buf2, "%-20s %s", buf1, hp->title);
		fprintf(ofp, "%.76s\n", buf2);
		return;
	}

	p1 = index(hp->from, '(');	/* Find the sender's full name. */
	if (p1 == NULL && hp->path[0])
		p1 = index(hp->path, '(');
	if (p1 != NULL) {
		strcpy(fname, p1+1);
		p2 = index(fname, ')');
		if (p2 != NULL)
			*p2 = '\0';
	}

	fprintf(ofp, "Subject: %s\n", hp->title);
	if (!hflag && hp->keywords[0])
		fprintf(ofp, "Keywords: %s\n", hp->keywords);
	if (verbose) {
		fprintf(ofp, "From: %s\n", hp->from);
		fprintf(ofp, "Path: %s\n", hp->path);
		if (hp->organization[0])
			fprintf(ofp, "Organization: %s\n", hp->organization);
	}
	else {
		if (p1 != NULL)
			*--p1 = '\0';		/* bump over the '(' */
#ifdef INTERNET
		/*
		 * Prefer Path line if it's in internet format, or if we don't
		 * understand internet format here, or if there is no reply-to.
		 */
		fprintf(ofp, "From: %s", hp->from);
#else
		fprintf(ofp, "Path: %s", tailpath(hp));
#endif
		if (fname[0] != '\0') {
			fprintf(ofp, " (%s", fname);
			if (hp->organization[0] && !hflag)
				fprintf(ofp, " @ %s", hp->organization);
			fprintf(ofp, ")");
		}
		fprintf(ofp, "\n");
		if (p1 != NULL)
			*p1 = ' ';
	}

	ngdel(strcpy(bfr, hp->nbuf));
	if (verbose) {
		fprintf(ofp, "Newsgroups: %s\n", bfr);
		fprintf(ofp, "Date: %s\n", hp->subdate);
		if (hp->sender[0])
			fprintf(ofp, "Sender: %s\n", hp->sender);
		if (hp->replyto[0])
			fprintf(ofp, "Reply-To: %s\n", hp->replyto);
		if (hp->followto[0])
			fprintf(ofp, "Followup-To: %s\n", hp->followto);
	}
	else if (index(bfr, ',') || strcmp(groupdir, "junk") == 0)
		fprintf(ofp, "Newsgroups: %s\n", bfr);

	if (pflag || ofp != stdout)
		putc('\n', ofp);
}


/*
 * If ofp != stdout, close it and run the script in coptbuf.
 */
cout(ofp)
FILE *ofp;
{
	register char	*p, *q, *r;

	if (ofp == stdout)
		return;
	fclose(ofp);
	p = coptbuf;
	q = lbuf;
	while ((*q = *p++) != '\0')
		if (*q++ == FMETA) {
			q--;
			r = outfile;
			while ((*q++ = *r++) != '\0')
				;
			q--;
		}
	fwait(fsubr(ushell, lbuf, (char *)NULL));
	unlink(outfile);
}


cdump(ofp)
register FILE *ofp;
{
	if (ofp == stdout)
		return;
	fclose(ofp);
	unlink(outfile);
}


/*
 * Quiet 'flush'.
 * Empty (without fflush()) the buffer for stream fp.
 */
/* ARGSUSED */
qfflush(fp)
FILE *fp;
{
	/* Alas, stdio does not permit this */
}


/*
 * Count the number of remaining lines in file fp.
 * Do not move the file pointer.
 */
linecnt(fp)
FILE *fp;
{
	long	curpos;
	register int	nlines = 0;
	register int	c;

	if (fp == NULL)
		return 0;
	curpos = ftell(fp);
	while ((c = getc(fp)) != EOF)
		if (c == '\n')
			nlines++;
	fseek(fp, curpos, 0);
	return nlines;
}


/*
 * Transmit file to system.
 */
transmit(sp, file)
register struct srec *sp;
char	*file;
{
	register FILE *ifp, *ofp;
	register int	c;
	struct hbuf hh;
	char	TRANS[BUFLEN];

#ifdef DEBUG
	fprintf(stderr, "xmit %s to %s using %s\n", file, sp->s_name, sp->s_xmit);
#endif
	ifp = xfopen(file, "r");
	if (hread(&hh, ifp, TRUE) == NULL)
		return;
	strcpy(TRANS, "/tmp/trXXXXXX");
	ofp = xfopen(mktemp(TRANS), "w");
	if (index(sp->s_flags, 'A') == NULL)
		hwrite(&hh, ofp);
	else
		ohwrite(&hh, ofp);
	while ((c = getc(ifp)) != EOF)
		putc(c, ofp);
	fclose(ifp);
	fclose(ofp);
	if (*sp->s_xmit == '\0' || index(sp->s_flags, 'F') || index(sp->s_flags, 'U'))
		sprintf(bfr, DFTXMIT, sp->s_name, TRANS);
	else
		sprintf(bfr, "(%s) < %s", sp->s_xmit, TRANS);
#ifdef DEBUG
	fprintf(stderr, "%s\n", bfr);
#endif
	fwait(fsubr(pshell, bfr, (char *)NULL));
	unlink(TRANS);
}


/*
 * Cancel the article whose header is in hp, by posting a control message
 * to cancel it.  The scope of the control message depends on who would
 * really be willing to cancel it.  It is sent as far as it will do any good.
 * notauthor is true iff the person posting this article is not the
 * real author of the article being cancelled.
 */
cancel(ofp, hp, notauthor)
FILE *ofp;
struct hbuf *hp;
int	notauthor;
{
	FILE	*inews;
	struct utsname me;
	char	*p;
	char	distgroup[64];
	int	pid;

	fflush(stdout);
	pid = fork();
	if (pid > 0)
		return 0;
	uname(&me);
	strcpy(distgroup, hp->nbuf);
	p = index(distgroup, '.');
	if (notauthor)
		sprintf(distgroup, "to.%s", me.nodename);
	else
		sprintf(distgroup, "%s", hp->nbuf);
	sprintf(bfr, "%s -t 'cmsg cancel %s' -n %s < /dev/null",
	    INEWS, hp->ident, distgroup);
	if ((inews = popen(bfr, "w")) == NULL)
		fprintf(ofp, "Can't fork %s\n", INEWS);
	else
		pclose(inews);
	if (pid == 0)
		exit(0);
	return 0;
}


dash(num, ofp)
register int	num;
register FILE *ofp;
{
	register int	i;

	for (i = 0; i < num; i++)
		putc('-', ofp);
	putc('\n', ofp);
}


help(ofp)
register FILE *ofp;
{
	register FILE *fp;
	register int	c;

	if (cflag) {
oneline:
		fprintf(ofp, "(n)ext re(p)rint (w)rite (q)uit (r)eply\
 (c)ancel -[n] +[n] (f)ollowup (N)ext (U)nsubscribe (v)ersion\n");
		return;
	}
	if ((fp = fopen(HELPFILE, "r")) == NULL) {
		fprintf(ofp, "No help file.\n");
		goto oneline;
	}
	while ((c = getc(fp)) != EOF && !sigtrap)
		putc(c, ofp);
	fclose(fp);
}


pout(ofp)
FILE *ofp;
{
	register char	*p, *q, *r;

	p = PAGER;
	q = lbuf;
	while ((*q = *p++) != '\0')
		if (*q++ == FMETA) {
			q--;
			r = filename;
			while ((*q++ = *r++) != '\0')
				;
			q--;
		}
	fwait(fsubr(ushell, lbuf, (char *)NULL));
	fprintf(ofp, "\n");
}


/*
 * like strcat but be careful with quotes.  since there appears to be no way
 * to quote an apostrophe in sh, we change them to double quotes.
 */
strqcat(dest, src)
register char	*dest, *src;
{
	while (*dest++)
		;
	dest--;
	while (*src) {
		if (*src == '\'')
			*dest++ = '"', src++;
		else
			*dest++ = *src++;
	}
	*dest++ = 0;
}

/*
 * Print a very brief version of the date in question.
 */
char *
briefdate(datestr)
char *datestr;
{
	long dt, now;
	char *tmstr;
	char *wkday, *monthdate, *timeofday;
	static char rbuf[20];

	dt = cgtdate(datestr);
	tmstr = ctime(&dt);

	wkday = tmstr; tmstr[3] = '\0';
	monthdate = tmstr+4; tmstr[10] = '\0';
	timeofday = tmstr+11; tmstr[16] = '\0';

	time(&now);
	if (now - dt < 7 * DAYS)
		strcpy(rbuf, wkday);
	else
		strcpy(rbuf, monthdate);
	strcat(rbuf, " ");
	strcat(rbuf, timeofday);
	return rbuf;
}

/*
 * Return TRUE iff stdout is /dev/null.
 */
ignoring()
{
	struct stat ss, ns;

	fstat(1, &ss);
	stat("/dev/null", &ns);
	if (ss.st_dev == ns.st_dev && ss.st_rdev == ns.st_rdev)
		return TRUE;
	return FALSE;
}
