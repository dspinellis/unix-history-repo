/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)collect.c	8.18 (Berkeley) %G%";
#endif /* not lint */

# include <errno.h>
# include "sendmail.h"

/*
**  COLLECT -- read & parse message header & make temp file.
**
**	Creates a temporary file name and copies the standard
**	input to that file.  Leading UNIX-style "From" lines are
**	stripped off (after important information is extracted).
**
**	Parameters:
**		fp -- file to read.
**		from -- the person we think it may be from.  If
**			there is a "From" line, we will replace
**			the name of the person by this.  If NULL,
**			do no such replacement.
**
**	Returns:
**		Name of the "from" person extracted from the
**		arpanet header.
**
**	Side Effects:
**		Temp file is created and filled.
**		The from person may be set.
*/

char	*CollectErrorMessage;
bool	CollectErrno;

maketemp(from)
	char *from;
{
	register FILE *tf;
	bool ignrdot = smtpmode ? FALSE : IgnrDot;
	time_t dbto = smtpmode ? TimeOuts.to_datablock : 0;
	register char *workbuf, *freebuf;
	bool inputerr = FALSE;
	bool headeronly = FALSE;
	char buf[MAXLINE], buf2[MAXLINE];
	extern char *hvalue();
	extern bool isheader(), flusheol();
	extern char *index();

	CollectErrorMessage = NULL;
	CollectErrno = 0;
	if (hdrp == NULL)
		hdrp = &e->e_header;
	else
		headeronly = TRUE;

	/*
	**  Create the temp file name and create the file.
	*/

	if (!headeronly)
	{
		e->e_df = queuename(e, 'd');
		e->e_df = newstr(e->e_df);
		if ((tf = dfopen(e->e_df, O_WRONLY|O_CREAT|O_TRUNC, FileMode)) == NULL)
		{
			syserr("Cannot create %s", e->e_df);
			e->e_flags |= EF_NORETURN;
			finis();
		}
		HasEightBits = FALSE;
	}

	/*
	**  Tell ARPANET to go ahead.
	*/

	if (smtpmode)
		message("354 Enter mail, end with \".\" on a line by itself");

	/* set global timer to monitor progress */
	sfgetset(dbto);

	/*
	**  Try to read a UNIX-style From line
	*/

	if (sfgets(buf, MAXLINE, fp, dbto, "initial message read") == NULL)
		goto readerr;
	fixcrlf(buf, FALSE);
# ifndef NOTUNIX
	if (!headeronly && !SaveFrom && strncmp(buf, "From ", 5) == 0)
	{
		if (!flusheol(buf, fp, dbto))
			goto readerr;
		eatfrom(buf, e);
		if (sfgets(buf, MAXLINE, fp, dbto,
				"message header read") == NULL)
			goto readerr;
		fixcrlf(buf, FALSE);
	}
# endif /* NOTUNIX */

	/*
	**  Copy fp to temp file & do message editing.
	**	To keep certain mailers from getting confused,
	**	and to keep the output clean, lines that look
	**	like UNIX "From" lines are deleted in the header.
	*/

	workbuf = buf;		/* `workbuf' contains a header field */
	freebuf = buf2;		/* `freebuf' can be used for read-ahead */
	for (;;)
	{
		char *curbuf;
		int curbuffree;
		register int curbuflen;
		char *p;

		/* first, see if the header is over */
		if (!isheader(workbuf))
		{
			fixcrlf(workbuf, TRUE);
			break;
		}

		/* if the line is too long, throw the rest away */
		if (!flusheol(workbuf, fp, dbto))
			goto readerr;

		/* it's okay to toss '\n' now (flusheol() needed it) */
		fixcrlf(workbuf, TRUE);

		curbuf = workbuf;
		curbuflen = strlen(curbuf);
		curbuffree = MAXLINE - curbuflen;
		p = curbuf + curbuflen;

		/* get the rest of this field */
		for (;;)
		{
			int clen;

			if (sfgets(freebuf, MAXLINE, fp, dbto,
					"message header read") == NULL)
			{
				freebuf[0] = '\0';
				break;
			}

			/* is this a continuation line? */
			if (*freebuf != ' ' && *freebuf != '\t')
				break;

			if (!flusheol(freebuf, fp, dbto))
				goto readerr;

			fixcrlf(freebuf, TRUE);
			clen = strlen(freebuf) + 1;

			/* if insufficient room, dynamically allocate buffer */
			if (clen >= curbuffree)
			{
				/* reallocate buffer */
				int nbuflen = ((p - curbuf) + clen) * 2;
				char *nbuf = xalloc(nbuflen);

				p = nbuf + curbuflen;
				curbuffree = nbuflen - curbuflen;
				bcopy(curbuf, nbuf, curbuflen);
				if (curbuf != buf && curbuf != buf2)
					free(curbuf);
				curbuf = nbuf;
			}
			*p++ = '\n';
			bcopy(freebuf, p, clen - 1);
			p += clen - 1;
			curbuffree -= clen;
			curbuflen += clen;
		}
		*p++ = '\0';

		e->e_msgsize += curbuflen;

		/*
		**  The working buffer now becomes the free buffer, since
		**  the free buffer contains a new header field.
		**
		**  This is premature, since we still havent called
		**  chompheader() to process the field we just created
		**  (so the call to chompheader() will use `freebuf').
		**  This convolution is necessary so that if we break out
		**  of the loop due to H_EOH, `workbuf' will always be
		**  the next unprocessed buffer.
		*/

		{
			register char *tmp = workbuf;
			workbuf = freebuf;
			freebuf = tmp;
		}

		/*
		**  Snarf header away.
		*/

		if (bitset(H_EOH, chompheader(curbuf, FALSE, e)))
			break;

		/*
		**  If the buffer was dynamically allocated, free it.
		*/

		if (curbuf != buf && curbuf != buf2)
			free(curbuf);
	}

	if (tTd(30, 1))
		printf("EOH\n");

	if (headeronly)
	{
		if (*workbuf != '\0')
			syserr("collect: lost first line of message");
		goto readerr;
	}

	if (*workbuf == '\0')
	{
		/* throw away a blank line */
		if (sfgets(buf, MAXLINE, fp, dbto,
				"message separator read") == NULL)
			goto readerr;
	}
	else if (workbuf == buf2)	/* guarantee `buf' contains data */
		(void) strcpy(buf, buf2);

	/*
	**  Collect the body of the message.
	*/

	for (;;)
	{
		register char *bp = buf;

		fixcrlf(buf, TRUE);

		/* check for end-of-message */
		if (!ignrdot && buf[0] == '.' && (buf[1] == '\n' || buf[1] == '\0'))
			break;

		/* check for transparent dot */
		if ((OpMode == MD_SMTP || OpMode == MD_DAEMON) &&
		    bp[0] == '.' && bp[1] == '.')
			bp++;

		/*
		**  Figure message length, output the line to the temp
		**  file, and insert a newline if missing.
		*/

		e->e_msgsize += strlen(bp) + 1;
		fputs(bp, tf);
		fputs("\n", tf);
		if (ferror(tf))
			tferror(tf, e);
		if (sfgets(buf, MAXLINE, fp, dbto, "message body read") == NULL)
			goto readerr;
	}

readerr:
	if ((feof(fp) && smtpmode) || ferror(fp))
	{
		if (tTd(30, 1))
			printf("collect: read error\n");
		inputerr = TRUE;
	}

	/* reset global timer */
	sfgetset((time_t) 0);

	if (headeronly)
		return;

	if (tf != NULL)
	{
		if (fflush(tf) != 0)
			tferror(tf, e);
		if (fsync(fileno(tf)) < 0 || fclose(tf) < 0)
		{
			tferror(tf, e);
			finis();
		}
	}

	if (CollectErrorMessage != NULL && Errors <= 0)
	{
		if (CollectErrno != 0)
		{
			errno = CollectErrno;
			syserr(CollectErrorMessage, e->e_df);
			finis();
		}
		usrerr(CollectErrorMessage);
	}
	else if (inputerr && (OpMode == MD_SMTP || OpMode == MD_DAEMON))
	{
		/* An EOF when running SMTP is an error */
		char *host;
		char *problem;

		host = RealHostName;
		if (host == NULL)
			host = "localhost";

		if (feof(fp))
			problem = "unexpected close";
		else if (ferror(fp))
			problem = "I/O error";
		else
			problem = "read timeout";
# ifdef LOG
		if (LogLevel > 0 && feof(fp))
			syslog(LOG_NOTICE,
			    "collect: %s on connection from %s, sender=%s: %s\n",
			    problem, host, e->e_from.q_paddr, errstring(errno));
# endif
		if (feof(fp))
			usrerr("451 collect: %s on connection from %s, from=%s",
				problem, host, e->e_from.q_paddr);
		else
			syserr("451 collect: %s on connection from %s, from=%s",
				problem, host, e->e_from.q_paddr);

		/* don't return an error indication */
		e->e_to = NULL;
		e->e_flags &= ~EF_FATALERRS;
		e->e_flags |= EF_CLRQUEUE;

		/* and don't try to deliver the partial message either */
		if (InChild)
			ExitStat = EX_QUIT;
		finis();
	}

	/*
	**  Find out some information from the headers.
	**	Examples are who is the from person & the date.
	*/

	eatheader(e, !requeueflag);

	/* collect statistics */
	if (OpMode != MD_VERIFY)
		markstats(e, (ADDRESS *) NULL);

	/*
	**  Add an Apparently-To: line if we have no recipient lines.
	*/

	if (hvalue("to", e->e_header) == NULL &&
	    hvalue("cc", e->e_header) == NULL &&
	    hvalue("bcc", e->e_header) == NULL &&
	    hvalue("apparently-to", e->e_header) == NULL)
	{
		register ADDRESS *q;

		/* create an Apparently-To: field */
		/*    that or reject the message.... */
		for (q = e->e_sendqueue; q != NULL; q = q->q_next)
		{
			if (q->q_alias != NULL)
				continue;
			if (tTd(30, 3))
				printf("Adding Apparently-To: %s\n", q->q_paddr);
			addheader("Apparently-To", q->q_paddr, &e->e_header);
		}
	}

	/* check for message too large */
	if (MaxMessageSize > 0 && e->e_msgsize > MaxMessageSize)
	{
		usrerr("552 Message exceeds maximum fixed size (%ld)",
			MaxMessageSize);
	}

	/* check for illegal 8-bit data */
	if (HasEightBits)
	{
		e->e_flags |= EF_HAS8BIT;
		if (bitset(MM_MIME8BIT, MimeMode))
		{
			/* convert it to MIME */
			if (hvalue("MIME-Version", e->e_header) == NULL)
			{
				char mimebuf[20];

				strcpy(mimebuf, "MIME-Version: 1.0");
				chompheader(mimebuf, FALSE, e);
			}
			if (e->e_bodytype == NULL)
				e->e_bodytype = "8BITMIME";
		}
		else if (!bitset(MM_PASS8BIT, MimeMode))
			usrerr("554 Eight bit data not allowed");
	}

	if ((e->e_dfp = fopen(e->e_df, "r")) == NULL)
	{
		/* we haven't acked receipt yet, so just chuck this */
		syserr("Cannot reopen %s", e->e_df);
		finis();
	}
}
/*
**  FLUSHEOL -- if not at EOL, throw away rest of input line.
**
**	Parameters:
**		buf -- last line read in (checked for '\n'),
**		fp -- file to be read from.
**
**	Returns:
**		FALSE on error from sfgets(), TRUE otherwise.
**
**	Side Effects:
**		none.
*/

bool
flusheol(buf, fp, dbto)
	char *buf;
	FILE *fp;
	time_t dbto;
{
	register char *p = buf;
	char junkbuf[MAXLINE];

	while (strchr(p, '\n') == NULL)
	{
		CollectErrorMessage = "553 header line too long";
		CollectErrno = 0;
		if (sfgets(junkbuf, MAXLINE, fp, dbto,
				"long line flush") == NULL)
			return (FALSE);
		p = junkbuf;
	}

	return (TRUE);
}
/*
**  TFERROR -- signal error on writing the temporary file.
**
**	Parameters:
**		tf -- the file pointer for the temporary file.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Gives an error message.
**		Arranges for following output to go elsewhere.
*/

tferror(tf, e)
	FILE *tf;
	register ENVELOPE *e;
{
	CollectErrno = errno;
	if (errno == ENOSPC)
	{
		struct stat st;
		long avail;
		long bsize;

		e->e_flags |= EF_NORETURN;
		if (fstat(fileno(tf), &st) < 0)
			st.st_size = 0;
		(void) freopen(e->e_df, "w", tf);
		if (st.st_size <= 0)
			fprintf(tf, "\n*** Mail could not be accepted");
		else if (sizeof st.st_size > sizeof (long))
			fprintf(tf, "\n*** Mail of at least %qd bytes could not be accepted\n",
				st.st_size);
		else
			fprintf(tf, "\n*** Mail of at least %ld bytes could not be accepted\n",
				st.st_size);
		fprintf(tf, "*** at %s due to lack of disk space for temp file.\n",
			MyHostName);
		avail = freespace(QueueDir, &bsize);
		if (avail > 0)
		{
			if (bsize > 1024)
				avail *= bsize / 1024;
			else if (bsize < 1024)
				avail /= 1024 / bsize;
			fprintf(tf, "*** Currently, %ld kilobytes are available for mail temp files.\n",
				avail);
		}
		CollectErrorMessage = "452 Out of disk space for temp file";
	}
	else
	{
		CollectErrorMessage = "cannot write message body to disk (%s)";
	}
	(void) freopen("/dev/null", "w", tf);
}
/*
**  EATFROM -- chew up a UNIX style from line and process
**
**	This does indeed make some assumptions about the format
**	of UNIX messages.
**
**	Parameters:
**		fm -- the from line.
**
**	Returns:
**		none.
**
**	Side Effects:
**		extracts what information it can from the header,
**		such as the date.
*/

# ifndef NOTUNIX

char	*DowList[] =
{
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", NULL
};

char	*MonthList[] =
{
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
	NULL
};

eatfrom(fm, e)
	char *fm;
	register ENVELOPE *e;
{
	register char *p;
	register char **dt;

	if (tTd(30, 2))
		printf("eatfrom(%s)\n", fm);

	/* find the date part */
	p = fm;
	while (*p != '\0')
	{
		/* skip a word */
		while (*p != '\0' && *p != ' ')
			p++;
		while (*p == ' ')
			p++;
		if (!(isascii(*p) && isupper(*p)) ||
		    p[3] != ' ' || p[13] != ':' || p[16] != ':')
			continue;

		/* we have a possible date */
		for (dt = DowList; *dt != NULL; dt++)
			if (strncmp(*dt, p, 3) == 0)
				break;
		if (*dt == NULL)
			continue;

		for (dt = MonthList; *dt != NULL; dt++)
			if (strncmp(*dt, &p[4], 3) == 0)
				break;
		if (*dt != NULL)
			break;
	}

	if (*p != '\0')
	{
		char *q;
		extern char *arpadate();

		/* we have found a date */
		q = xalloc(25);
		(void) strncpy(q, p, 25);
		q[24] = '\0';
		q = arpadate(q);
		define('a', newstr(q), e);
	}
}

# endif /* NOTUNIX */
