/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifdef notdef
static char sccsid[] = "@(#)send.c	5.8 (Berkeley) %G%";
#endif /* notdef */

#include "rcv.h"
#include <sys/wait.h>
#include <sys/stat.h>

/*
 * Mail -- a mail program
 *
 * Mail to others.
 */

/*
 * Send message described by the passed pointer to the
 * passed output buffer.  Return -1 on error, but normally
 * the number of lines written.  Adjust the status: field
 * if need be.  If doign is given, suppress ignored header fields.
 */
send(mp, obuf, doign)
	register struct message *mp;
	FILE *obuf;
	struct ignoretab *doign;
{
	long count;
	register FILE *ibuf;
	char line[LINESIZE];
	int lc, ishead, infld, ignoring, dostat;
	register char *cp, *cp2;
	register int c;
	int length;

	ibuf = setinput(mp);
	count = mp->m_size;
	ishead = 1;
	dostat = doign == 0 || !isign("status", doign);
	infld = 0;
	lc = 0;
	/*
	 * Process headers first
	 */
	while (count > 0 && ishead) {
		if (fgets(line, LINESIZE, ibuf) == NULL)
			break;
		count -= length = strlen(line);
		if (lc == 0) {
			/* 
			 * First line is the From line, so no headers
			 * there to worry about
			 */
			ignoring = 0;
		} else if (line[0] == '\n') {
			/*
			 * If line is blank, we've reached end of
			 * headers, so force out status: field
			 * and note that we are no longer in header
			 * fields
			 */
			if (dostat) {
				statusput(mp, obuf);
				dostat = 0;
			}
			ishead = 0;
			ignoring = 0;
		} else if (infld && (line[0] == ' ' || line[0] == '\t')) {
			/*
			 * If this line is a continuation (via space or tab)
			 * of a previous header field, just echo it
			 * (unless the field should be ignored).
			 * In other words, nothing to do.
			 */
		} else {
			/*
			 * Pick up the header field if we have one.
			 */
			for (cp = line; (c = *cp++) && c != ':' && !isspace(c);)
				;
			cp2 = --cp;
			while (isspace(*cp++))
				;
			if (cp[-1] != ':') {
				/*
				 * Not a header line, force out status:
				 * This happens in uucp style mail where
				 * there are no headers at all.
				 */
				if (dostat) {
					statusput(mp, obuf);
					dostat = 0;
				}
				(void) putc('\n', obuf); /* add blank line */
				lc++;
				ishead = 0;
				ignoring = 0;
			} else {
				/*
				 * If it is an ignored field and
				 * we care about such things, skip it.
				 */
				*cp2 = 0;	/* temporarily null terminate */
				if (doign && isign(line, doign))
					ignoring = 1;
				else if ((line[0] == 's' || line[0] == 'S') &&
					 icequal(line, "status")) {
					/*
					 * If the field is "status," go compute
					 * and print the real Status: field
					 */
					if (dostat) {
						statusput(mp, obuf);
						dostat = 0;
					}
					ignoring = 1;
				} else {
					ignoring = 0;
					*cp2 = c;	/* restore */
				}
				infld = 1;
			}
		}
		if (!ignoring) {
			(void) fwrite(line, sizeof *line, length, obuf);
			if (ferror(obuf))
				return -1;
			lc++;
		}
	}
	/*
	 * Copy out message body
	 */
	while (count > 0) {
		cp = line;
		c = count < LINESIZE ? count : LINESIZE;
		if ((c = fread(cp, sizeof *cp, c, ibuf)) <= 0)
			break;
		if (fwrite(cp, sizeof *cp, c, obuf) != c)
			return -1;
		count -= c;
		while (--c >= 0)
			if (*cp++ == '\n')
				lc++;
	}
	if (ishead && (mp->m_flag & MSTATUS))
		printf("failed to fix up status field\n");
	return (lc);
}

/*
 * Output a reasonable looking status field.
 * But if "status" is ignored and doign, forget it.
 */
statusput(mp, obuf)
	register struct message *mp;
	FILE *obuf;
{
	char statout[3];
	register char *cp = statout;

	if (mp->m_flag & MREAD)
		*cp++ = 'R';
	if ((mp->m_flag & MNEW) == 0)
		*cp++ = 'O';
	*cp = 0;
	if (statout[0])
		fprintf(obuf, "Status: %s\n", statout);
}

/*
 * Interface between the argument list and the mail1 routine
 * which does all the dirty work.
 */

mail(to, cc, bcc, smopts)
	struct name *to, *cc, *bcc, *smopts;
{
	struct header head;

	head.h_to = detract(to, 0);
	head.h_subject = NOSTR;
	head.h_cc = detract(cc, 0);
	head.h_bcc = detract(bcc, 0);
	head.h_smopts = detract(smopts, 0);
	head.h_seq = 0;
	(void) mail1(&head);
	return(0);
}


/*
 * Send mail to a bunch of user names.  The interface is through
 * the mail routine below.
 */

sendmail(str)
	char *str;
{
	struct header head;

	if (blankline(str))
		head.h_to = NOSTR;
	else
		head.h_to = str;
	head.h_subject = NOSTR;
	head.h_cc = NOSTR;
	head.h_bcc = NOSTR;
	head.h_smopts = NOSTR;
	head.h_seq = 0;
	(void) mail1(&head);
	return(0);
}

/*
 * Mail a message on standard input to the people indicated
 * in the passed header.  (Internal interface).
 */

mail1(hp)
	struct header *hp;
{
	register char *cp;
	int pid, i, p, gotcha;
	union wait s;
	char **namelist, *deliver;
	struct name *to, *np;
	struct stat sbuf;
	FILE *mtf, *postage;
	int remote = rflag != NOSTR || rmail;
	char **t;

	/*
	 * Collect user's mail from standard input.
	 * Get the result as mtf.
	 */

	pid = -1;
	if (hp->h_subject == NOSTR)
		hp->h_subject = sflag;
	if ((mtf = collect(hp)) == NULL)
		return(-1);
	hp->h_seq = 1;
	if (intty && value("askcc") != NOSTR)
		grabh(hp, GCC);
	else if (intty) {
		printf("EOT\n");
		(void) fflush(stdout);
	}

	/*
	 * Now, take the user names from the combined
	 * to and cc lists and do all the alias
	 * processing.
	 */

	senderr = 0;
	to = usermap(cat(extract(hp->h_bcc, GBCC),
		cat(extract(hp->h_to, GTO), extract(hp->h_cc, GCC))));
	if (to == NIL) {
		printf("No recipients specified\n");
		goto topdog;
	}

	/*
	 * Look through the recipient list for names with /'s
	 * in them which we write to as files directly.
	 */

	to = outof(to, mtf, hp);
	rewind(mtf);
	if (senderr && !remote) {
topdog:

		if (fsize(mtf) != 0) {
			(void) remove(deadletter);
			(void) exwrite(deadletter, mtf, 1);
			rewind(mtf);
		}
	}
	for (gotcha = 0, np = to; np != NIL; np = np->n_flink)
		if ((np->n_type & GDEL) == 0) {
			gotcha++;
			break;
		}
	if (!gotcha)
		goto out;
	to = elide(to);
	mechk(to);
	if (count(to) > 1)
		hp->h_seq++;
	if (hp->h_seq > 0 && !remote) {
		fixhead(hp, to);
		if (fsize(mtf) == 0)
		    if (hp->h_subject == NOSTR)
			printf("No message, no subject; hope that's ok\n");
		    else
			printf("Null message body; hope that's ok\n");
		if ((mtf = infix(hp, mtf)) == NULL) {
			fprintf(stderr, ". . . message lost, sorry.\n");
			return(-1);
		}
	}
	namelist = unpack(cat(extract(hp->h_smopts, 0), to));
	if (debug) {
		printf("Recipients of message:\n");
		for (t = namelist; *t != NOSTR; t++)
			printf(" \"%s\"", *t);
		printf("\n");
		(void) fflush(stdout);
		return 0;
	}
	if ((cp = value("record")) != NOSTR)
		(void) savemail(expand(cp), mtf);

	/*
	 * Wait, to absorb a potential zombie, then
	 * fork, set up the temporary mail file as standard
	 * input for "mail" and exec with the user list we generated
	 * far above. Return the process id to caller in case he
	 * wants to await the completion of mail.
	 */

	while (wait3(&s, WNOHANG, (struct timeval *) 0) > 0)
		;
	rewind(mtf);
	pid = fork();
	if (pid == -1) {
		perror("fork");
		(void) remove(deadletter);
		(void) exwrite(deadletter, mtf, 1);
		goto out;
	}
	if (pid == 0) {
#ifdef SIGTSTP
		if (remote == 0) {
			(void) signal(SIGTSTP, SIG_IGN);
			(void) signal(SIGTTIN, SIG_IGN);
			(void) signal(SIGTTOU, SIG_IGN);
		}
#endif
		(void) signal(SIGHUP, SIG_IGN);
		(void) signal(SIGINT, SIG_IGN);
		(void) signal(SIGQUIT, SIG_IGN);
		if (!stat(POSTAGE, &sbuf))
			if ((postage = fopen(POSTAGE, "a")) != NULL) {
				fprintf(postage, "%s %d %ld\n", myname,
				    count(to), fsize(mtf));
				(void) fclose(postage);
			}
		(void) close(0);
		(void) dup(fileno(mtf));
		for (i = getdtablesize(); --i > 2;)
			(void) close(i);
		if ((deliver = value("sendmail")) == NOSTR)
			deliver = SENDMAIL;
		execv(deliver, namelist);
		perror(deliver);
		exit(1);
	}

out:
	if (remote || (value("verbose") != NOSTR)) {
		while ((p = wait(&s)) != pid && p != -1)
			;
		if (s.w_status != 0)
			senderr++;
		pid = 0;
	}
	(void) fclose(mtf);
	return(pid);
}

/*
 * Fix the header by glopping all of the expanded names from
 * the distribution list into the appropriate fields.
 * If there are any ARPA net recipients in the message,
 * we must insert commas, alas.
 */

fixhead(hp, tolist)
	struct header *hp;
	struct name *tolist;
{
	register int f;
	register struct name *np;

	for (f = 0, np = tolist; np != NIL; np = np->n_flink)
		if (any('@', np->n_name)) {
			f |= GCOMMA;
			break;
		}

	if (debug && f & GCOMMA)
		fprintf(stderr, "Should be inserting commas in recip lists\n");
	hp->h_to = detract(tolist, GTO|f);
	hp->h_cc = detract(tolist, GCC|f);
	hp->h_bcc = detract(tolist, GBCC|f);
}

/*
 * Prepend a header in front of the collected stuff
 * and return the new file.
 */

FILE *
infix(hp, fi)
	struct header *hp;
	FILE *fi;
{
	extern char tempMail[];
	register FILE *nfo, *nfi;
	register int c;

	rewind(fi);
	if ((nfo = fopen(tempMail, "w")) == NULL) {
		perror(tempMail);
		return(fi);
	}
	if ((nfi = fopen(tempMail, "r")) == NULL) {
		perror(tempMail);
		(void) fclose(nfo);
		return(fi);
	}
	(void) remove(tempMail);
	(void) puthead(hp, nfo, GTO|GSUBJECT|GCC|GBCC|GNL);
	c = getc(fi);
	while (c != EOF) {
		(void) putc(c, nfo);
		c = getc(fi);
	}
	if (ferror(fi)) {
		perror("read");
		return(fi);
	}
	(void) fflush(nfo);
	if (ferror(nfo)) {
		perror(tempMail);
		(void) fclose(nfo);
		(void) fclose(nfi);
		return(fi);
	}
	(void) fclose(nfo);
	(void) fclose(fi);
	rewind(nfi);
	return(nfi);
}

/*
 * Dump the to, subject, cc header on the
 * passed file buffer.
 */

puthead(hp, fo, w)
	struct header *hp;
	FILE *fo;
{
	register int gotcha;

	gotcha = 0;
	if (hp->h_to != NOSTR && w & GTO)
		fmt("To: ", hp->h_to, fo), gotcha++;
	if (hp->h_subject != NOSTR && w & GSUBJECT)
		fprintf(fo, "Subject: %s\n", hp->h_subject), gotcha++;
	if (hp->h_cc != NOSTR && w & GCC)
		fmt("Cc: ", hp->h_cc, fo), gotcha++;
	if (hp->h_bcc != NOSTR && w & GBCC)
		fmt("Bcc: ", hp->h_bcc, fo), gotcha++;
	if (gotcha && w & GNL)
		(void) putc('\n', fo);
	return(0);
}

/*
 * Format the given text to not exceed 72 characters.
 */

fmt(str, txt, fo)
	register char *str, *txt;
	register FILE *fo;
{
	register int col;
	register char *bg, *bl, *pt, ch;

	col = strlen(str);
	if (col)
		fprintf(fo, "%s", str);
	pt = bg = txt;
	bl = 0;
	while (*bg) {
		pt++;
		if (++col > 72) {
			if (!bl) {
				bl = bg;
				while (*bl && !isspace(*bl))
					bl++;
			}
			if (!*bl)
				goto finish;
			ch = *bl;
			*bl = '\0';
			fprintf(fo, "%s\n    ", bg);
			col = 4;
			*bl = ch;
			pt = bg = ++bl;
			bl = 0;
		}
		if (!*pt) {
finish:
			fprintf(fo, "%s\n", bg);
			return;
		}
		if (isspace(*pt))
			bl = pt;
	}
}

/*
 * Save the outgoing mail on the passed file.
 */

/*ARGSUSED*/
savemail(name, fi)
	char name[];
	register FILE *fi;
{
	register FILE *fo;
	char buf[BUFSIZ];
	register i;
	time_t now, time();
	char *n;
	char *ctime();

	if ((fo = fopen(name, "a")) == NULL) {
		perror(name);
		return (-1);
	}
	(void) time(&now);
	if ((n = rflag) == NOSTR)
		n = myname;
	fprintf(fo, "From %s %s", n, ctime(&now));
	rewind(fi);
	while ((i = fread(buf, 1, sizeof buf, fi)) > 0)
		(void) fwrite(buf, 1, i, fo);
	(void) putc('\n', fo);
	(void) fflush(fo);
	if (ferror(fo))
		perror(name);
	(void) fclose(fo);
	return (0);
}
