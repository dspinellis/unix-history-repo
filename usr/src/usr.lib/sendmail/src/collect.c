/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)collect.c	5.4 (Berkeley) 6/30/88";
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
	char buf[MAXFIELD+2];
	register char *p;
	extern char *hvalue();

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

	(void) sfgets(buf, sizeof buf, InChannel);
	fixcrlf(buf, FALSE);
# ifndef NOTUNIX
	if (!SaveFrom && strncmp(buf, "From ", 5) == 0)
	{
		eatfrom(buf);
		(void) sfgets(buf, sizeof buf, InChannel);
		fixcrlf(buf, FALSE);
	}
# endif NOTUNIX

	/*
	**  Copy InChannel to temp file & do message editing.
	**	To keep certain mailers from getting confused,
	**	and to keep the output clean, lines that look
	**	like UNIX "From" lines are deleted in the header.
	*/

	do
	{
		int c;
		extern bool isheader();

		/* drop out on error */
		if (ferror(InChannel))
			break;

		/* if the line is too long, throw the rest away */
		if (index(buf, '\n') == NULL)
		{
			while ((c = getc(InChannel)) != '\n' && c != EOF)
				continue;
			/* give an error? */
		}

		fixcrlf(buf, TRUE);

		/* see if the header is over */
		if (!isheader(buf))
			break;

		/* get the rest of this field */
		while ((c = getc(InChannel)) == ' ' || c == '\t')
		{
			p = &buf[strlen(buf)];
			*p++ = '\n';
			*p++ = c;
			if (sfgets(p, MAXFIELD - (p - buf), InChannel) == NULL)
				break;
			fixcrlf(p, TRUE);
		}
		if (!feof(InChannel) && !ferror(InChannel))
			(void) ungetc(c, InChannel);

		CurEnv->e_msgsize += strlen(buf);

		/*
		**  Snarf header away.
		*/

		if (bitset(H_EOH, chompheader(buf, FALSE)))
			break;
	} while (sfgets(buf, MAXFIELD, InChannel) != NULL);

# ifdef DEBUG
	if (tTd(30, 1))
		printf("EOH\n");
# endif DEBUG

	/* throw away a blank line */
	if (buf[0] == '\0')
		(void) sfgets(buf, MAXFIELD, InChannel);

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
	if (fflush(tf) != 0)
		tferror(tf);
	(void) fclose(tf);

	/* An EOF when running SMTP is an error */
	if ((feof(InChannel) || ferror(InChannel)) && OpMode == MD_SMTP)
	{
		syserr("collect: unexpected close, from=%s", CurEnv->e_from.q_paddr);

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
# ifdef DEBUG
			if (tTd(30, 3))
				printf("Adding Apparently-To: %s\n", q->q_paddr);
# endif DEBUG
			addheader("apparently-to", q->q_paddr, CurEnv);
		}
	}

	if ((CurEnv->e_dfp = fopen(CurEnv->e_df, "r")) == NULL)
		syserr("Cannot reopen %s", CurEnv->e_df);
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

# ifdef DEBUG
	if (tTd(30, 2))
		printf("eatfrom(%s)\n", fm);
# endif DEBUG

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
