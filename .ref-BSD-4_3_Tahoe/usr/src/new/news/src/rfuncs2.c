/*
 * This software is Copyright 1986 by Rick Adams.
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
 * rfuncs2 - more routines needed by readr.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)rfuncs2.c	1.35	3/21/87";
#endif /* SCCSID */

/*LINTLIBRARY*/

#include "rparams.h"

static char	lbuf[LBUFLEN];
extern char *replyname();

FILE *popen();

/*
 * Match title.
 */
titmat(h, titlist)
register struct hbuf *h;
register char	*titlist;
{
	register char	*p;

	while (*titlist != '\0') {

		if (strcmp(titlist, h->ident) == 0)
			return TRUE;
		for (p = h->title; *p != '\0'; p++)
			if (prefix(p, titlist)) {
				return TRUE;
			}
		while (*titlist++ != '\0')
			;
	}
	return FALSE;
}


/*
 * Save the news item in the user's file.
 *	Allow files with first character as '|' to write article
 *	to program across a pipe.
 */

#define PIPECHAR '|'

save(file, to)
register char	*file, *to;
{
	register FILE *ufp, *hfp;
#ifdef u370
	static struct hbuf hh;
#else /* !u370 */
	struct hbuf hh;
#endif /* !u370 */
	int	isprogram = 0;
	int	isnew = 1;
	register int i;

	for(i=0;i<NUNREC;i++)
		hh.unrec[i] = NULL;

	if ((hfp = art_open(file, "r")) == NULL) {
		fprintf(stderr, "Can't get article.\n");
		return;
	}
	if (hread(&hh, hfp, TRUE) == NULL) {
		fprintf(stderr, "Article is garbled.\n");
		return;
	}
	ufp = fopen(to, "r");
	if (ufp != NULL) {
		(void) fclose(ufp);
		isnew = 0;
	}
	(void) setgid(gid);
	(void) setuid(uid);
	(void) umask(savmask);

	if (*to == PIPECHAR) {
		if ((ufp = popen (&to[1], "w")) == NULL) {
			fprintf(stderr,"Cannot execute %s\n", &to[1]);
			return;
		}
		isprogram++;
	} else if ((ufp = fopen(to, "a")) == NULL) {
		fprintf(stderr,"Cannot append to %s.\n", to);
		return;
	}
	/*
	 * V7MAIL code is here to conform to V7 mail format.
	 * If you need a different format to be able to
	 * use your local mail command (such as four ^A's
	 * on the end of articles) substitute it here.
	 */
#ifdef MMDF
	if (!isprogram)
		fprintf(ufp, "\001\001\001\001\n");  /* MMDF message header */
#endif /* MMDF */
#ifdef V7MAIL
	hh.subtime = cgtdate(hh.subdate);
	fprintf(ufp, "From %s %s", replyname(&hh), ctime(&hh.subtime));
#endif
	hprint(&hh, ufp, 2);
#ifdef V7MAIL
	tprint(hfp, ufp, TRUE);
	putc('\n', ufp);	/* force blank line at end (ugh) */
#else
	tprint(hfp, ufp, FALSE);
#endif
	(void) fclose(hfp);
#ifdef MMDF
	if (!isprogram)
		fprintf(ufp, "\001\001\001\001\n");  /* MMDF message header */
#endif /* MMDF */
	if (isprogram)
		(void) pclose (ufp);
	else
		(void) fclose(ufp);
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
	while ((fgets(bfr, sizeof bfr, ifp)) != NULL && !SigTrap) {
		if (checkfrom && strncmp(bfr, "From ", 5) == 0)
			putc('>', ofp);
		(void) fputs(bfr, ofp);
		if (ferror(ofp)) break;		/* E.g. disk full */
	}
	if (SigTrap)
		qfflush(ofp);
	(void) fflush(ofp);
	fprintf(ofp, (SigTrap ? "\n\n" : "\n"));
	SigTrap = FALSE;
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

		(void) strcpy(bfr, groupdir);
		for (cp=bfr; *cp; cp++)
			if (*cp == '/')
				*cp = '.';
		(void) sprintf(buf1, "%s/%ld", bfr, bit);
		(void) sprintf(buf2, "%-20s %s", buf1, hp->title);
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
	if (!hflag && hp->summary[0])
		fprintf(ofp, "Summary: %s\n", hp->summary);
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
		if (fname[0] || hp->organization[0]) {
			if (fname[0] == '\0') {
				(void) strcpy(fname,hp->from);
				p2 = index(fname,'@');
				if (p2)
					*p2 = '\0';
			}
			fprintf(ofp, " (%s", fname);
			if (hp->organization[0] && !hflag)
				fprintf(ofp, " @ %s", hp->organization);
			fprintf(ofp, ")");
		}
		fprintf(ofp, "\n");
		if (p1 != NULL)
			*p1 = ' ';
	}

	if (verbose) {
		fprintf(ofp, "Newsgroups: %s\n", hp->nbuf);
		fprintf(ofp, "Date: %s\n", hp->subdate);
		if (hp->sender[0])
			fprintf(ofp, "Sender: %s\n", hp->sender);
		if (hp->replyto[0])
			fprintf(ofp, "Reply-To: %s\n", hp->replyto);
		if (hp->followto[0])
			fprintf(ofp, "Followup-To: %s\n", hp->followto);
	}
	else if (index(hp->nbuf, ',') || strcmp(groupdir, "junk") == 0)
		fprintf(ofp, "Newsgroups: %s\n", hp->nbuf);

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

	if (ofp == stdout || ofp == NULL)
		return;
	(void) fclose(ofp);
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
	(void) unlink(outfile);
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
#ifdef fileno
	int	fno, err;

	fno = fileno(fp);
	err = ferror(fp);
	fileno(fp) = -1;
	(void) fflush(fp);
	fileno(fp) = fno;
	if (!err)
		(void) clearerr(fp);
#endif /* fileno */
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
	(void) fseek(fp, curpos, 0);
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
	register int	c, i;
#ifdef u370
	static struct hbuf hh;
#else /* !u370 */
	struct hbuf hh;
#endif /* !u370 */
	char	TRANS[BUFLEN];

#ifdef DEBUG
	fprintf(stderr, "xmit %s to %s using %s\n", file, sp->s_name, sp->s_xmit);
#endif
	bzero((char *)&hh, sizeof hh);
	ifp = xart_open(file, "r");
	if (hread(&hh, ifp, TRUE) == NULL)
		return;
	strcpy(TRANS, "/tmp/trXXXXXX");
	ofp = xfopen(mktemp(TRANS), "w");
	if (index(sp->s_flags, 'A') == NULL)
		hwrite(&hh, ofp);
	else {
#ifdef OLD
			fprintf(ofp, "A%s\n%s\n%s!%s\n%s\n%s\n", oident(hh.ident), hh.nbuf, PATHSYSNAME,
				hh.path, hh.subdate, hh.title);
#else /* !OLD */
			logerr("Must have OLD defined to use A flag for xmit");
			unlink(TRANS);
			return;
#endif /* !OLD */
	}
	while ((c = getc(ifp)) != EOF)
		putc(c, ofp);
	(void) fclose(ifp);
	(void) fclose(ofp);
	for (i=0;i<NUNREC;i++)
		if (hh.unrec[i] != NULL)
			free(hh.unrec[i]);
	if (*sp->s_xmit == '\0' || strpbrk(sp->s_flags, "FUMH"))
		(void) sprintf(bfr, DFTXMIT, sp->s_name, TRANS);
	else
		(void) sprintf(bfr, "(%s) < %s", sp->s_xmit, TRANS);
#ifdef DEBUG
	fprintf(stderr, "%s\n", bfr);
#endif
	(void) system(bfr);
	(void) unlink(TRANS);
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
	int	pid;

	fflush(ofp);
	pid = vfork();
	if (pid < 0) {
		perror("readnews: cancel");
		return 0;
	}
	if (pid > 0)
		return 0;
	if (notauthor)
		(void) sprintf(bfr, "%s/%s -c 'cancel %s' -n %s -d %s < /dev/null",
		    LIB, "inews", hp->ident, hp->nbuf,
#ifdef ORGDISTRIB
			ORGDISTRIB);
#else /* !ORGDISTRIB */
			"local");
#endif /* !ORGDISTRIB */
	else {
		if (hp->distribution[0] == '\0')
			(void) sprintf(bfr, "%s/%s -c 'cancel %s' -n %s < /dev/null",
			    LIB, "inews", hp->ident, hp->nbuf);
		else
			(void) sprintf(bfr, "%s/%s -c 'cancel %s' -n %s -d %s < /dev/null",
			    LIB, "inews", hp->ident, hp->nbuf, hp->distribution);
	}
	execl("/bin/sh", "sh", "-c", bfr, (char *) 0);
	perror(bfr);
	for ( ; ; )
		exit(1);
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
	char temp[BUFLEN];

	if (cflag) {
oneline:
		fprintf(ofp, "(n)ext re(p)rint (w)rite (q)uit (r)eply\
 (c)ancel -[n] +[n] (f)ollowup (N)ext (U)nsubscribe (v)ersion\n");
		return;
	}
	(void) sprintf(temp, "%s/%s", LIB, "help");
	if ((fp = fopen(temp, "r")) == NULL) {
		fprintf(ofp, "No help file.\n");
		goto oneline;
	}
	while ((c = getc(fp)) != EOF && !SigTrap)
		putc(c, ofp);
	(void) fclose(fp);
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
 * Print a very brief version of the date in question.
 */
char *
briefdate(datestr)
char *datestr;
{
	time_t dt, now;
	char *tmstr;
	char *wkday, *monthdate, *timeofday;
	static char rbuf[20];

	dt = cgtdate(datestr);
	tmstr = ctime(&dt);

	wkday = tmstr; tmstr[3] = '\0';
	monthdate = tmstr+4; tmstr[10] = '\0';
	timeofday = tmstr+11; tmstr[16] = '\0';

	(void) time(&now);
	if (now - dt < WEEKS)
		(void) strcpy(rbuf, wkday);
	else
		(void) strcpy(rbuf, monthdate);
	(void) strcat(rbuf, " ");
	(void) strcat(rbuf, timeofday);
	return rbuf;
}

/*
 * Return TRUE iff stdout is /dev/null.
 */
ignoring()
{
	struct stat ss, ns;

	if (fstat(1, &ss) < 0)
		return FALSE;
	if (stat("/dev/null", &ns) < 0)
		return FALSE;
	if (ss.st_dev == ns.st_dev && ss.st_rdev == ns.st_rdev)
		return TRUE;
	return FALSE;
}
