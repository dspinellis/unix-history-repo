# include <errno.h>
# include "sendmail.h"

SCCSID(@(#)collect.c	3.36		%G%);

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
	extern char *macvalue();
	extern char *index();

	/*
	**  Create the temp file name and create the file.
	*/

	strcpy(tempfname, QueueDir);
	strcat(tempfname, "/dfaXXXXXX");
	(void) mktemp(tempfname);
	if ((tf = dfopen(tempfname, "w")) == NULL)
	{
		syserr("Cannot create %s", tempfname);
		NoReturn = TRUE;
		finis();
	}
	chmod(tempfname, 0600);
	InFileName = tempfname;

	/*
	**  Create the Mail-From line if we want to.
	*/

	if (Smtp && macvalue('s') != NULL)
	{
		char xbuf[50];

		(void) sprintf(xbuf, "Mail-From: %s$s received by $i at $b",
			macvalue('r') == NULL ? "" : "$r host ");
		(void) expand(xbuf, buf, &buf[sizeof buf - 1]);
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
	if (!QueueRun)
	{
		/* adjust total priority by message priority */
		MsgPriority = MsgSize;
		p = hvalue("priority");
		if (p != NULL)
			MsgPriority -= priencode(p) * WKPRIFACT;
	}

	/* special handling */
	p = hvalue("special-handling");
	if (p != NULL)
		spechandling(p);

	/* from person */
	xfrom = hvalue("sender");
	if (xfrom == NULL)
		xfrom = OrigFrom;
	if (ArpaMode)
		setfrom(xfrom, (char *) NULL);

	/* full name of from person */
	p = hvalue("full-name");
	if (p != NULL)
		define('x', p);
	else
	{
		extern char *getxpart();

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
		p = getxpart(p);
		if (p != NULL)
			define('x', newstr(p));
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
/*
**  SPECHANDLE -- do special handling
**
**	Parameters:
**		p -- pointer to list of special handling words.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sets flags as indicated by p.
*/

struct handling
{
	char	*han_name;		/* word to get this magic */
	int	han_what;		/* what to do, see below */
};

/* modes for han_what */
# define	HAN_NONE	0	/* nothing special */
# define	HAN_RRECEIPT	1	/* give return receipt */

struct handling	Handling[] =
{
	"return-receipt-requested",	HAN_RRECEIPT,
	NULL,				HAN_NONE
};

spechandling(p)
	register char *p;
{
	register char *w;
	register struct handling *h;
	extern bool sameword();

	while (*p != '\0')
	{
		/* collect a word to compare to */
		while (*p != '\0' && (*p == ',' || isspace(*p)))
			p++;
		if (*p == '\0')
			break;
		w = p;
		while (*p != '\0' && *p != ',' && !isspace(*p))
			p++;
		if (*p != '\0')
			*p++ = '\0';

		/* scan the special handling table */
		for (h = Handling; h->han_name != NULL; h++)
			if (sameword(h->han_name, w))
				break;

		/* see if we can do anything interesting */
		switch (h->han_what)
		{
		  case HAN_NONE:	/* nothing to be done */
			break;

		  case HAN_RRECEIPT:	/* give return receipt */
			RetReceipt = TRUE;
# ifdef DEBUG
			if (Debug > 2)
				printf(">>> Return receipt requested\n");
# endif DEBUG
			break;

		  default:
			syserr("spechandling: handling %d (%s)", h->han_what, w);
		}
	}
}
