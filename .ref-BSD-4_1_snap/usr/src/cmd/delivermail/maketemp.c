# include <stdio.h>
# include <ctype.h>
# include <errno.h>
# include "dlvrmail.h"

static char	SccsId[] = "@(#)maketemp.c	2.3	12/6/80";

/*
**  MAKETEMP -- read & parse message header & make temp file.
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
**		none
**
**	Returns:
**		Name of temp file.
**
**	Side Effects:
**		Temp file is created and filled.
**
**	Called By:
**		main
**
**	Notes:
**		This is broken off from main largely so that the
**		temp buffer can be deallocated.
*/

char	MsgId[MAXNAME];		/* message-id, determined or created */
long	MsgSize;		/* size of message in bytes */
bool	GotHdr;			/* if set, "From ..." line exists */

char *
maketemp()
{
	register FILE *tf;
	char buf[MAXFIELD+1];
	static char fbuf[sizeof buf];
	extern char *prescan();
	extern char *matchhdr();
	register char *p;
	register bool inheader;
	bool firstline;
	char c;
	extern int errno;

	/*
	**  Create the temp file name and create the file.
	*/

	mktemp(InFileName);
	close(creat(InFileName, 0600));
	if ((tf = fopen(InFileName, "w")) == NULL)
	{
		syserr("Cannot create %s", InFileName);
		return (NULL);
	}

	/*
	**  Copy stdin to temp file & do message editting.
	**	From person gets copied into fbuf.  At the end of
	**	this loop, if fbuf[0] == '\0' then there was no
	**	recognized from person in the message.  We also
	**	save the message id in MsgId.  The
	**	flag 'inheader' keeps track of whether we are
	**	in the header or in the body of the message.
	**	The flag 'firstline' is only true on the first
	**	line of a message.
	**	To keep certain mailers from getting confused,
	**	and to keep the output clean, lines that look
	**	like UNIX "From" lines are deleted in the header,
	**	and prepended with ">" in the body.
	*/

	inheader = TRUE;
	firstline = TRUE;
	fbuf[0] = '\0';
	while (!feof(stdin) && fgets(buf, sizeof buf, stdin) != NULL)
	{
		if (inheader && isalnum(buf[0]))
		{
			/* get the rest of this field */
			while ((c = getc(stdin)) == ' ' || c == '\t')
			{
				p = &buf[strlen(buf)];
				*p++ = c;
				if (fgets(p, sizeof buf - (p - buf), stdin) == NULL)
					break;
			}
			if (c != EOF)
				ungetc(c, stdin);
		}

		if (!IgnrDot && buf[0] == '.' && (buf[1] == '\n' || buf[1] == '\0'))
			break;

		/* are we still in the header? */
		if ((buf[0] == '\n' || buf[0] == '\0') && inheader)
		{
			inheader = FALSE;
			if (MsgId[0] == '\0')
			{
				makemsgid();
				if (UseMsgId)
					fprintf(tf, "Message-Id: <%s>\n", MsgId);
			}
# ifdef DEBUG
			if (Debug)
				printf("EOH\n");
# endif DEBUG
		}

		/* Hide UNIX-like From lines */
		if (strncmp(buf, "From ", 5) == 0)
		{
			if (!firstline)
			{
				fputs(">", tf);
				MsgSize++;
			}
			else
				GotHdr++;
		}

		if (inheader && !isspace(buf[0]))
		{
			/* find out if this is really a header */
			for (p = buf; *p != ':' && *p != '\0' && !isspace(*p); p++)
				continue;
			while (*p != ':' && isspace(*p))
				p++;
			if (*p != ':')
			{
				inheader = FALSE;
# ifdef DEBUG
				if (Debug)
					printf("EOH?\n");
# endif DEBUG
			}
		}

		if (inheader)
		{
			/* find the sender */
			p = matchhdr(buf, "sender");
			if (p == NULL && fbuf[0] == '\0')
				p = matchhdr(buf, "from");
			if (p != NULL)
				prescan(p, fbuf, &fbuf[sizeof fbuf - 1], '\0');

			/* find the message id */
			p = matchhdr(buf, "message-id");
			if (p != NULL && MsgId[0] == '\0')
				prescan(p, MsgId, &MsgId[sizeof MsgId - 1], '\0');
		}
		MsgSize += strlen(buf);
		fputs(buf, tf);
		firstline = FALSE;
		if (ferror(tf))
		{
			if (errno == ENOSPC)
			{
				freopen(InFileName, "w", tf);
				fputs("\nMAIL DELETED BECAUSE OF LACK OF DISK SPACE\n\n", tf);
				syserr("Out of disk space for temp file");
			}
			else
				syserr("Cannot write %s", InFileName);
			freopen("/dev/null", "w", tf);
		}
	}
	fclose(tf);
	if (MsgId[0] == '\0')
		makemsgid();
	if (freopen(InFileName, "r", stdin) == NULL)
		syserr("Cannot reopen %s", InFileName);
	return (ArpaFmt && fbuf[0] != '\0' ? fbuf : NULL);
}
/*
**  MAKEMSGID -- Compute a message id for this process.
**
**	This routine creates a message id for a message if
**	it did not have one already.  If the MESSAGEID compile
**	flag is set, the messageid will be added to any message
**	that does not already have one.  Currently it is more
**	of an artifact, but I suggest that if you are hacking,
**	you leave it in -- I may want to use it someday if
**	duplicate messages turn out to be a problem.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Stores a message-id into MsgId.
**
**	Called By:
**		maketemp
*/

makemsgid()
{
	auto long t;
	extern char *MyLocName;
	extern char *ArpaHost;

	time(&t);
	sprintf(MsgId, "%ld.%d.%s@%s", t, getpid(), MyLocName, ArpaHost);
}
