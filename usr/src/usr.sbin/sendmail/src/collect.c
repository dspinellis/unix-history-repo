/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)collect.c	5.14.1.1 (Berkeley) %G%";
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

maketemp(from)
	char *from;
{
	register FILE *tf;
	bool ignrdot = smtpmode ? FALSE : IgnrDot;
	char buf[MAXFIELD], buf2[MAXFIELD];
	register char *workbuf, *freebuf;
	extern char *hvalue();
	extern bool isheader(), flusheol();
	extern char *index();

	/*
	**  Create the temp file name and create the file.
	*/

	e->e_df = newstr(queuename(e, 'd'));
	if ((tf = dfopen(e->e_df, "w")) == NULL)
	{
		syserr("Cannot create %s", e->e_df);
		NoReturn = TRUE;
		finis();
	}
	(void) chmod(e->e_df, FileMode);

	/*
	**  Tell ARPANET to go ahead.
	*/

	if (smtpmode)
		message("354", "Enter mail, end with \".\" on a line by itself");

	/*
	**  Try to read a UNIX-style From line
	*/

	if (sfgets(buf, MAXFIELD, InChannel) == NULL)
		goto readerr;
	fixcrlf(buf, FALSE);
# ifndef NOTUNIX
	if (!SaveFrom && strncmp(buf, "From ", 5) == 0)
	{
		if (!flusheol(buf, InChannel))
			goto readerr;
		eatfrom(buf, e);
		if (sfgets(buf, MAXFIELD, InChannel) == NULL)
			goto readerr;
		fixcrlf(buf, FALSE);
	}
# endif /* NOTUNIX */

	/*
	**  Copy InChannel to temp file & do message editing.
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
		if (!flusheol(workbuf, InChannel))
			goto readerr;

		/* it's okay to toss '\n' now (flusheol() needed it) */
		fixcrlf(workbuf, TRUE);

		curbuf = workbuf;
		curbuflen = strlen(curbuf);
		curbuffree = MAXFIELD - curbuflen;
		p = curbuf + curbuflen;

		/* get the rest of this field */
		for (;;)
		{
			int clen;

			if (sfgets(freebuf, MAXFIELD, InChannel) == NULL)
				goto readerr;

			/* is this a continuation line? */
			if (*freebuf != ' ' && *freebuf != '\t')
				break;

			if (!flusheol(freebuf, InChannel))
				goto readerr;

			fixcrlf(freebuf, TRUE);
			clen = strlen(freebuf);

			/* if insufficient room, dynamically allocate buffer */
			if (clen >= curbuffree)
			{
				/* reallocate buffer */
				int nbuflen = ((p - curbuf) + clen) * 2;
				char *nbuf = xalloc(nbuflen);

				p = nbuf + (p - curbuf);
				curbuffree = nbuflen - (p - workbuf) - clen;
				bcopy(curbuf, nbuf, p - curbuf);
				if (curbuf != buf && curbuf != buf2)
					free(curbuf);
				curbuf = nbuf;
			}
			bcopy(freebuf, p, clen);
			p += clen;
			curbuffree -= clen;
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

	if (*workbuf == '\0')
	{
		/* throw away a blank line */
		if (sfgets(buf, MAXFIELD, InChannel) == NULL)
			goto readerr;
	}
	else if (workbuf == buf2)	/* guarantee `buf' contains data */
		(void) strcpy(buf, buf2);

	/*
	**  Collect the body of the message.
	*/

	do
	{
		register char *bp = buf;

		fixcrlf(buf, TRUE);

		/* check for end-of-message */
		if (!ignrdot && buf[0] == '.' && (buf[1] == '\n' || buf[1] == '\0'))
			break;

		/* check for transparent dot */
		if (OpMode == MD_SMTP && bp[0] == '.' && bp[1] == '.')
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
	} while (sfgets(buf, MAXFIELD, InChannel) != NULL);

readerr:
	if (fflush(tf) != 0)
		tferror(tf, e);
	(void) fclose(tf);

	/* An EOF when running SMTP is an error */
	if ((feof(InChannel) || ferror(InChannel)) && OpMode == MD_SMTP)
	{
		int usrerr(), syserr();
# ifdef LOG
		if (RealHostName != NULL && LogLevel > 0)
			syslog(LOG_NOTICE,
			    "collect: unexpected close on connection from %s: %m\n",
			    e->e_from.q_paddr, RealHostName);
# endif
		(feof(InChannel) ? usrerr: syserr)
			("collect: unexpected close, from=%s", e->e_from.q_paddr);

		/* don't return an error indication */
		e->e_to = NULL;
		e->e_flags &= ~EF_FATALERRS;

		/* and don't try to deliver the partial message either */
		finis();
	}

	/*
	**  Find out some information from the headers.
	**	Examples are who is the from person & the date.
	*/

	eatheader(e);

	/*
	**  Add an Apparently-To: line if we have no recipient lines.
	*/

	if (hvalue("to", e) == NULL && hvalue("cc", e) == NULL &&
	    hvalue("bcc", e) == NULL && hvalue("apparently-to", e) == NULL)
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
			addheader("apparently-to", q->q_paddr, e);
		}
	}

	if ((e->e_dfp = fopen(e->e_df, "r")) == NULL)
		syserr("Cannot reopen %s", e->e_df);
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
flusheol(buf, fp)
	char *buf;
	FILE *fp;
{
	char junkbuf[MAXLINE], *sfgets();
	register char *p = buf;

	while (strchr(p, '\n') == NULL) {
		if (sfgets(junkbuf,MAXLINE,fp) == NULL)
			return(FALSE);
		p = junkbuf;
	}

	return(TRUE);
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
	if (errno == ENOSPC)
	{
		(void) freopen(e->e_df, "w", tf);
		fputs("\nMAIL DELETED BECAUSE OF LACK OF DISK SPACE\n\n", tf);
		usrerr("452 Out of disk space for temp file");
	}
	else
		syserr("collect: Cannot write %s", e->e_df);
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
		if (!isupper(*p) || p[3] != ' ' || p[13] != ':' || p[16] != ':')
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

	if (*p != NULL)
	{
		char *q;
		extern char *arpadate();

		/* we have found a date */
		q = xalloc(25);
		(void) strncpy(q, p, 25);
		q[24] = '\0';
		define('d', q, e);
		q = arpadate(q);
		define('a', newstr(q), e);
	}
}

# endif /* NOTUNIX */
