# include <errno.h>
# include "sendmail.h"

static char	SccsId[] = "@(#)collect.c	3.27	%G%";

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

long	MsgSize;		/* size of message in bytes */

maketemp(from)
	char *from;
{
	register FILE *tf;
	char buf[MAXFIELD+1];
	register char *p;
	char *xfrom;
	extern char *hvalue();
	extern char *mktemp();
	static char tempfname[40];
	extern char *QueueDir;
	extern char *index();

	/*
	**  Create the temp file name and create the file.
	*/

	strcpy(tempfname, QueueDir);
	strcat(tempfname, "/dfaXXXXXX");
	(void) mktemp(tempfname);
	(void) close(creat(tempfname, 0600));
	if ((tf = fopen(tempfname, "w")) == NULL)
	{
		syserr("Cannot create %s", tempfname);
		return;
	}
	InFileName = tempfname;

	/*
	**  Tell ARPANET to go ahead.
	*/

	if (ArpaMode == ARPA_MAIL)
	{
		extern char Arpa_Enter[];

		message(Arpa_Enter, "Enter mail, end with \".\" on a line by itself");
	}

	/*
	**  Try to read a UNIX-style From line
	*/

	if (fgets(buf, sizeof buf, stdin) == NULL)
		return;
	fixcrlf(buf, FALSE);
# ifndef NOTUNIX
	if (!SaveFrom && strncmp(buf, "From ", 5) == 0)
	{
		eatfrom(buf);
		(void) fgets(buf, sizeof buf, stdin);
		fixcrlf(buf, FALSE);
	}
# endif NOTUNIX

	/*
	**  Copy stdin to temp file & do message editting.
	**	To keep certain mailers from getting confused,
	**	and to keep the output clean, lines that look
	**	like UNIX "From" lines are deleted in the header,
	**	and prepended with ">" in the body.
	*/

	for (; !feof(stdin); !feof(stdin) && fgets(buf, sizeof buf, stdin) != NULL)
	{
		register char c;
		extern bool isheader();

		fixcrlf(buf, FALSE);

		/* see if the header is over */
		if (!isheader(buf))
			break;

		/* get the rest of this field */
		while ((c = getc(stdin)) == ' ' || c == '\t')
		{
			p = &buf[strlen(buf)];
			*p++ = c;
			if (fgets(p, sizeof buf - (p - buf), stdin) == NULL)
				break;
			fixcrlf(p, FALSE);
		}
		if (!feof(stdin))
			(void) ungetc(c, stdin);

		MsgSize += strlen(buf);

		/*
		**  Snarf header away.
		*/

		if (bitset(H_EOH, chompheader(buf, FALSE)))
			break;
	}

# ifdef DEBUG
	if (Debug)
		printf("EOH\n");
# endif DEBUG

	/* throw away a blank line */
	if (buf[0] == '\n')
	{
		(void) fgets(buf, sizeof buf, stdin);
		fixcrlf(buf, FALSE);
	}

	/*
	**  Collect the body of the message.
	*/

	for (; !feof(stdin); !feof(stdin) && fgets(buf, sizeof buf, stdin) != NULL)
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
			MsgSize++;
		}
# endif NOTUNIX

		/*
		**  Figure message length, output the line to the temp
		**  file, and insert a newline if missing.
		*/

		i = strlen(bp);
		MsgSize += i;
		fputs(bp, tf);
		if (bp[i - 1] != '\n')
			fputs("\n", tf);
		if (ferror(tf))
		{
			if (errno == ENOSPC)
			{
				(void) freopen(InFileName, "w", tf);
				fputs("\nMAIL DELETED BECAUSE OF LACK OF DISK SPACE\n\n", tf);
				usrerr("452 Out of disk space for temp file");
			}
			else
				syserr("collect: Cannot write %s", InFileName);
			(void) freopen("/dev/null", "w", tf);
		}
	}
	(void) fclose(tf);

	/*
	**  Find out some information from the headers.
	**	Examples are who is the from person & the date.
	*/

	/* message priority */
	p = hvalue("priority");
	if (p != NULL)
		MsgPriority = priencode(p);

	/* from person */
	xfrom = hvalue("sender");
	if (xfrom == NULL)
		xfrom = OrigFrom;
	if (ArpaMode != ARPA_NONE)
		setfrom(xfrom, (char *) NULL);

	/* full name of from person */
	p = hvalue("full-name");
	if (p != NULL)
		define('x', p);
	else
	{
		register char *q;

		/*
		**  Try to extract the full name from a general From:
		**  field.  We take anything which is a comment as a
		**  first choice.  Failing in that, we see if there is
		**  a "machine readable" name (in <angle brackets>); if
		**  so we take anything preceeding that clause.
		**
		**  If we blow it here it's not all that serious.
		*/

		p = hvalue("original-from");
		if (p == NULL)
			p = OrigFrom;
		q = index(p, '(');
		if (q != NULL)
		{
			int parenlev = 0;

			for (p = q; *p != '\0'; p++)
			{
				if (*p == '(')
					parenlev++;
				else if (*p == ')' && --parenlev <= 0)
					break;
			}
			if (*p == ')')
			{
				*p = '\0';
				if (*++q != '\0')
					define('x', newstr(q));
				*p = ')';
			}
		}
		else if ((q = index(p, '<')) != NULL)
		{
			char savec;

			while (*--q == ' ')
				continue;
			while (isspace(*p))
				p++;
			savec = *++q;
			*q = '\0';
			if (*p != '\0')
				define('x', newstr(p));
			*q = savec;
		}
	}

	/* date message originated */
	p = hvalue("posted-date");
	if (p == NULL)
		p = hvalue("date");
	if (p != NULL)
	{
		define('a', p);
		/* we don't have a good way to do canonical conversion ....
		define('d', newstr(arpatounix(p)));
		.... so we will ignore the problem for the time being */
	}

	if ((TempFile = fopen(InFileName, "r")) == NULL)
		syserr("Cannot reopen %s", InFileName);

# ifdef DEBUG
	if (Debug)
	{
		HDR *h;
		extern char *capitalize();

		printf("----- collected header -----\n");
		for (h = Header; h != NULL; h = h->h_link)
			printf("%s: %s\n", capitalize(h->h_field), h->h_value);
		printf("----------------------------\n");
	}
# endif DEBUG
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
	if (Debug > 1)
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

		/* we have found a date */
		q = xalloc(25);
		strncpy(q, p, 25);
		q[24] = '\0';
		define('d', q);
	}
}

# endif NOTUNIX
/*
**  PRIENCODE -- encode external priority names into internal values.
**
**	Parameters:
**		p -- priority in ascii.
**
**	Returns:
**		priority as a numeric level.
**
**	Side Effects:
**		none.
*/

struct prio
{
	char	*pri_name;	/* external name of priority */
	int	pri_val;	/* internal value for same */
};

static struct prio	Prio[] =
{
	"alert",		PRI_ALERT,
	"quick",		PRI_QUICK,
	"first-class",		PRI_FIRSTCL,
	"normal",		PRI_NORMAL,
	"second-class",		PRI_SECONDCL,
	"third-class",		PRI_THIRDCL,
	NULL,			PRI_NORMAL,
};

priencode(p)
	char *p;
{
	register struct prio *pl;
	extern bool sameword();

	for (pl = Prio; pl->pri_name != NULL; pl++)
	{
		if (sameword(p, pl->pri_name))
			break;
	}
	return (pl->pri_val);
}
