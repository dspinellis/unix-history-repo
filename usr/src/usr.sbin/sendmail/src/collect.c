# include <errno.h>
# include "sendmail.h"

SCCSID(@(#)collect.c	3.49		%G%);

/*
**  COLLECT -- read & parse message header & make temp file.
**
**	Creates a temporary file name and copies the standard
**	input to that file.  While it is doing it, it looks for
**	"From:" and "Sender:" fields to use as the from-person
**	(but only if the -a flag is specified).  It prefers to
**	to use the "Sender:" field.
**
**	MIT seems to like to produce "Sent-By:" fields instead
**	of "Sender:" fields.  We used to catch this, but it turns
**	out that the "Sent-By:" field doesn't always correspond
**	to someone real ("___057", for instance), as required by
**	the protocol.  So we limp by.....
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
	char buf[MAXFIELD+1];
	register char *p;
	extern char *hvalue();
	extern char *macvalue();
	extern char *index();

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
	(void) chmod(CurEnv->e_df, 0600);

	/*
	**  Create the Received: line if we want to.
	*/

	if (Smtp && macvalue('s') != NULL)
	{
		char xbuf[50];

		/* this should be in the config file */
		(void) sprintf(xbuf, "Received: from $s by $i with SMTP; $b");
		expand(xbuf, buf, &buf[sizeof buf - 1], CurEnv);
		(void) chompheader(buf, FALSE);
	}

	/*
	**  Tell ARPANET to go ahead.
	*/

	if (sayok)
		message("354", "Enter mail, end with \".\" on a line by itself");

	/*
	**  Try to read a UNIX-style From line
	*/

	if (fgets(buf, sizeof buf, InChannel) == NULL)
		return;
	fixcrlf(buf, FALSE);
# ifndef NOTUNIX
	if (!SaveFrom && strncmp(buf, "From ", 5) == 0)
	{
		eatfrom(buf);
		(void) fgets(buf, sizeof buf, InChannel);
		fixcrlf(buf, FALSE);
	}
# endif NOTUNIX

	/*
	**  Copy InChannel to temp file & do message editing.
	**	To keep certain mailers from getting confused,
	**	and to keep the output clean, lines that look
	**	like UNIX "From" lines are deleted in the header,
	**	and prepended with ">" in the body.
	*/

	for (; !feof(InChannel); !feof(InChannel) && fgets(buf, sizeof buf, InChannel) != NULL)
	{
		register char c;
		extern bool isheader();

		/* if the line is too long, throw the rest away */
		if (index(buf, '\n') == NULL)
		{
			while ((c = getc(InChannel)) != '\n')
				continue;
			/* give an error? */
		}

		fixcrlf(buf, FALSE);

		/* see if the header is over */
		if (!isheader(buf))
			break;

		/* get the rest of this field */
		while ((c = getc(InChannel)) == ' ' || c == '\t')
		{
			p = &buf[strlen(buf)];
			*p++ = c;
			if (fgets(p, sizeof buf - (p - buf), InChannel) == NULL)
				break;
			fixcrlf(p, FALSE);
		}
		if (!feof(InChannel))
			(void) ungetc(c, InChannel);

		CurEnv->e_msgsize += strlen(buf);

		/*
		**  Snarf header away.
		*/

		if (bitset(H_EOH, chompheader(buf, FALSE)))
			break;
	}

# ifdef DEBUG
	if (tTd(30, 1))
		printf("EOH\n");
# endif DEBUG

	/* throw away a blank line */
	if (buf[0] == '\n')
	{
		(void) fgets(buf, sizeof buf, InChannel);
		fixcrlf(buf, FALSE);
	}

	/*
	**  Collect the body of the message.
	*/

	for (; !feof(InChannel); !feof(InChannel) && fgets(buf, sizeof buf, InChannel) != NULL)
	{
		register int i;
		register char *bp = buf;

		fixcrlf(buf, FALSE);

		/* check for end-of-message */
		if (!IgnrDot && buf[0] == '.' && (buf[1] == '\n' || buf[1] == '\0'))
			break;

		/* check for transparent dot */
		if (Smtp && *bp == '.')
			bp++;

# ifndef NOTUNIX
		/* Hide UNIX-like From lines */
		if (strncmp(bp, "From ", 5) == 0)
		{
			fputs(">", tf);
			CurEnv->e_msgsize++;
		}
# endif NOTUNIX

		/*
		**  Figure message length, output the line to the temp
		**  file, and insert a newline if missing.
		*/

		i = strlen(bp);
		CurEnv->e_msgsize += i;
		fputs(bp, tf);
		if (bp[i - 1] != '\n')
			fputs("\n", tf);
		if (ferror(tf))
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
	}
	(void) fclose(tf);

	/*
	**  Find out some information from the headers.
	**	Examples are who is the from person & the date.
	*/

	eatheader();

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

	/* check for hop count overflow */
	if (HopCount > MAXHOP)
		syserr("Too many hops (%d max); probably forwarding loop", MAXHOP);

	if ((TempFile = fopen(CurEnv->e_df, "r")) == NULL)
		syserr("Cannot reopen %s", CurEnv->e_df);

	/*
	**  Log collection information.
	*/

# ifdef LOG
	if (LogLevel > 1)
		syslog(LOG_INFO, "%s: from=%s, size=%ld, class=%d\n",
		       CurEnv->e_id, CurEnv->e_from.q_paddr, CurEnv->e_msgsize,
		       CurEnv->e_class);
# endif LOG
	return;
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
			*p++;
		while (*p == ' ')
			*p++;
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
		strncpy(q, p, 25);
		q[24] = '\0';
		define('d', q);
		q = arpadate(q);
		define('a', newstr(q));
	}
}

# endif NOTUNIX
