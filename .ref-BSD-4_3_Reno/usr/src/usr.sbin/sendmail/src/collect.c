/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)collect.c	5.9 (Berkeley) 6/1/90";
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
**		sayok -- if set, give an ARPANET style message
**			to say we are ready to collect input.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Temp file is created and filled.
**		The from person may be set.
*/

collect(sayok)
	bool sayok;
{
	register FILE *tf;
	char buf[MAXFIELD], buf2[MAXFIELD];
	register char *workbuf, *freebuf;
	register int workbuflen;
	extern char *hvalue();
	extern bool isheader(), flusheol();

	/*
	**  Create the temp file name and create the file.
	*/

	CurEnv->e_df = newstr(queuename(CurEnv, 'd'));
	if ((tf = dfopen(CurEnv->e_df, "w")) == NULL)
	{
		syserr("Cannot create %s", CurEnv->e_df);
		NoReturn = TRUE;
		finis();
	}
	(void) chmod(CurEnv->e_df, FileMode);

	/*
	**  Tell ARPANET to go ahead.
	*/

	if (sayok)
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
		eatfrom(buf);
		if (sfgets(buf, MAXFIELD, InChannel) == NULL)
			goto readerr;
		fixcrlf(buf, FALSE);
	}
# endif NOTUNIX

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

		workbuflen = strlen(workbuf);

		/* get the rest of this field */
		for (;;)
		{
			if (sfgets(freebuf, MAXFIELD, InChannel) == NULL)
				goto readerr;

			/* is this a continuation line? */
			if (*freebuf != ' ' && *freebuf != '\t')
				break;

			if (!flusheol(freebuf, InChannel))
				goto readerr;

			/* yes; append line to `workbuf' if there's room */
			if (workbuflen < MAXFIELD-3)
			{
				register char *p = workbuf + workbuflen;
				register char *q = freebuf;

				/* we have room for more of this field */
				fixcrlf(freebuf, TRUE);
				*p++ = '\n'; workbuflen++;
				while(*q != '\0' && workbuflen < MAXFIELD-1)
				{
					*p++ = *q++;
					workbuflen++;
				}
				*p = '\0';
			}
		}

		CurEnv->e_msgsize += workbuflen;

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

		if (bitset(H_EOH, chompheader(freebuf, FALSE)))
			break;
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
		if (!IgnrDot && buf[0] == '.' && (buf[1] == '\n' || buf[1] == '\0'))
			break;

		/* check for transparent dot */
		if (OpMode == MD_SMTP && !IgnrDot && bp[0] == '.' && bp[1] == '.')
			bp++;

		/*
		**  Figure message length, output the line to the temp
		**  file, and insert a newline if missing.
		*/

		CurEnv->e_msgsize += strlen(bp) + 1;
		fputs(bp, tf);
		fputs("\n", tf);
		if (ferror(tf))
			tferror(tf);
	} while (sfgets(buf, MAXFIELD, InChannel) != NULL);

readerr:
	if (fflush(tf) != 0)
		tferror(tf);
	(void) fclose(tf);

	/* An EOF when running SMTP is an error */
	if ((feof(InChannel) || ferror(InChannel)) && OpMode == MD_SMTP)
	{
		int usrerr(), syserr();
# ifdef LOG
		if (RealHostName != NULL && LogLevel > 0)
			syslog(LOG_NOTICE,
			    "collect: unexpected close on connection from %s: %m\n",
			    CurEnv->e_from.q_paddr, RealHostName);
# endif
		(feof(InChannel) ? usrerr: syserr)
			("collect: unexpected close, from=%s", CurEnv->e_from.q_paddr);

		/* don't return an error indication */
		CurEnv->e_to = NULL;
		CurEnv->e_flags &= ~EF_FATALERRS;

		/* and don't try to deliver the partial message either */
		finis();
	}

	/*
	**  Find out some information from the headers.
	**	Examples are who is the from person & the date.
	*/

	eatheader(CurEnv);

	/*
	**  Add an Apparently-To: line if we have no recipient lines.
	*/

	if (hvalue("to") == NULL && hvalue("cc") == NULL &&
	    hvalue("bcc") == NULL && hvalue("apparently-to") == NULL)
	{
		register ADDRESS *q;

		/* create an Apparently-To: field */
		/*    that or reject the message.... */
		for (q = CurEnv->e_sendqueue; q != NULL; q = q->q_next)
		{
			if (q->q_alias != NULL)
				continue;
			if (tTd(30, 3))
				printf("Adding Apparently-To: %s\n", q->q_paddr);
			addheader("apparently-to", q->q_paddr, CurEnv);
		}
	}

	if ((CurEnv->e_dfp = fopen(CurEnv->e_df, "r")) == NULL)
		syserr("Cannot reopen %s", CurEnv->e_df);
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

	while (index(p, '\n') == NULL) {
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

tferror(tf)
	FILE *tf;
{
	if (errno == ENOSPC)
	{
		(void) freopen(CurEnv->e_df, "w", tf);
		fputs("\nMAIL DELETED BECAUSE OF LACK OF DISK SPACE\n\n", tf);
		usrerr("452 Out of disk space for temp file");
	}
	else
		syserr("collect: Cannot write %s", CurEnv->e_df);
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

eatfrom(fm)
	char *fm;
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
		define('d', q, CurEnv);
		q = arpadate(q);
		define('a', newstr(q), CurEnv);
	}
}

# endif NOTUNIX
