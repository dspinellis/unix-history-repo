# include <stdio.h>
# include <signal.h>
# include <ctype.h>
# include "dlvrmail.h"
# ifdef LOG
# include <log.h>
# endif LOG

/*
**  DELIVERMAIL -- Deliver mail to a set of destinations
**
**	This is the basic mail router.  All user mail programs should
**	call this routine to actually deliver mail.  Delivermail in
**	turn calls a bunch of mail servers that do the real work of
**	delivering the mail.
**
**	Delivermail is driven by tables defined in config.c.  This
**	file will be different from system to system, but the rest
**	of the code will be the same.  This table could be read in,
**	but it seemed nicer to have it compiled in, since deliver-
**	mail will potentially be exercised a lot.
**
**	Usage:
**		/etc/delivermail [-f name] [-a] [-q] [-v] [-n] [-m] addr ...
**
**	Positional Parameters:
**		addr -- the address to deliver the mail to.  There
**			can be several.
**
**	Flags:
**		-f name		The mail is from "name" -- used for
**				the header in local mail, and to
**				deliver reports of failures to.
**		-r name		Same as -f; however, this flag is
**				reserved to indicate special processing
**				for remote mail delivery as needed
**				in the future.  So, network servers
**				should use -r.
**		-a		This mail should be in ARPANET std
**				format (not used).
**		-n		Don't do aliasing.  This might be used
**				when delivering responses, for
**				instance.
**		-d		Run in debug mode.
**		-em		Mail back a response if there was an
**				error in processing.  This should be
**				used when the origin of this message
**				is another machine.
**		-ew		Write back a response if the user is
**				still logged in, otherwise, act like
**				-em.
**		-eq		Don't print any error message (just
**				return exit status).
**		-ep		(default)  Print error messages
**				normally.
**		-m		In group expansion, send to the
**				sender also (stands for the Mail metoo
**				option.
**		-i		Do not terminate mail on a line
**				containing just dot.
**		-s		Save UNIX-like "From" lines on the
**				front of messages.
**
**	Return Codes:
**		As defined in <sysexits.h>.
**
**		These codes are actually returned from the auxiliary
**		mailers; it is their responsibility to make them
**		correct.
**
**	Defined Constants:
**		none
**
**	Compilation Flags:
**		BADMAIL -- the mailer used for local mail doesn't
**			return the standard set of exit codes.  This
**			causes the name to be looked up before mail
**			is ever sent.
**		LOG -- if set, everything is logged.
**		MESSAGEID -- if set, the Message-Id field is added
**			to the message header if one does not already
**			exist.  This can be used to delete duplicate
**			messages.
**
**	Compilation Instructions:
**		cc -c -O main.c config.c deliver.c parse.c
**		cc -n -s *.o -lS
**		chown root a.out
**		chmod 755 a.out
**		mv a.out delivermail
**
**	Requires:
**		signal (sys)
**		setbuf (sys)
**		initlog (libX)
**		open (sys)
**		lseek (sys)
**		close (sys)
**		dup (sys)
**		printf (sys)
**		syserr
**		atoi (sys)
**		freopen (sys)
**		openxscript
**		maketemp
**		getname
**		strcmp (sys)
**		getuid (sys)
**		parse
**		usrerr
**		finis
**		sendto
**		alias
**		recipient
**		nxtinq
**		deliver
**
**	Deficiencies:
**		It ought to collect together messages that are
**			destined for a single host and send these
**			to the auxiliary mail server together.
**		It should take "user at host" as three separate
**			parameters and combine them into one address.
**
**	Author:
**		Eric Allman, UCB/INGRES
**
**	History:
**		12/26/79 -- first written.
*/





char	ArpaFmt;	/* mail is expected to be in ARPANET format */
char	FromFlag;	/* from person is explicitly specified */
char	Debug;		/* run in debug mode */
char	MailBack;	/* mail back response on error */
char	EchoBack;	/* echo the message on error */
char	WriteBack;	/* write back response on error */
char	HasXscrpt;	/* if set, the transcript file exists */
char	NoAlias;	/* don't do aliasing */
char	ForceMail;	/* mail even if already sent a copy */
char	MeToo;		/* send to the sender also if in a group expansion */
char	SaveFrom;	/* save From lines on the front of messages */
char	IgnrDot;	/* if set, ignore dot when collecting mail */
char	Error;		/* set if errors */
char	SuprErrs;	/* supress errors if set */
char	InFileName[] = "/tmp/mailtXXXXXX";
char	Transcript[] = "/tmp/mailxXXXXXX";
addrq	From;		/* the from person */
char	*To;		/* the target person */
char	MsgId[MAXNAME];	/* the message-id for this letter */
int	HopCount;	/* hop count */
int	ExitStat;	/* the exit status byte */
addrq	SendQ;		/* queue of people to send to */
addrq	AliasQ;		/* queue of people who turned out to be aliases */






main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	extern char *maketemp();
	extern char *getname();
	extern int finis();
	extern addrq *parse();
	register addrq *q;
	extern char Version[];
	extern int errno;
	char *from;
	register int i;
	typedef int (*fnptr)();

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, finis);
	signal(SIGTERM, finis);
	setbuf(stdout, (char *) NULL);
# ifdef LOG
	initlog("delivermail", 0, LOG_INDEP);
# endif LOG
# ifdef DEBUG
# ifdef DEBUGFILE
	if ((i = open(DEBUGFILE, 1)) > 0)
	{
		lseek(i, 0L, 2);
		close(1);
		dup(i);
		close(i);
		Debug++;
	}
# endif DEBUGFILE
	if (Debug)
		printf("%s\n", Version);
# endif
	errno = 0;
	from = NULL;

	/*
	** Crack argv.
	*/

	while (--argc > 0 && (p = *++argv)[0] == '-')
	{
		switch (p[1])
		{
		  case 'r':	/* obsolete -f flag */
		  case 'f':	/* from address */
			p += 2;
			if (*p == '\0')
			{
				p = *++argv;
				if (--argc <= 0 || *p == '-')
				{
					syserr("No \"from\" person");
					argc++;
					argv--;
					break;
				}
			}
			if (from != NULL)
			{
				syserr("More than one \"from\" person");
				break;
			}
			from = p;
			break;

		  case 'h':	/* hop count */
			p += 2;
			if (*p == '\0')
			{
				p = *++argv;
				if (--argc <= 0 || *p < '0' || *p > '9')
				{
					syserr("Bad hop count (%s)", p);
					argc++;
					argv--;
					break;
				}
			}
			HopCount = atoi(p);
			break;

		  case 'e':	/* error message disposition */
			switch (p[2])
			{
			  case 'p':	/* print errors normally */
				break;	/* (default) */

			  case 'q':	/* be silent about it */
				freopen("/dev/null", "w", stdout);
				break;

			  case 'm':	/* mail back */
				MailBack++;
				openxscrpt();
				break;

			  case 'e':	/* echo back */
				EchoBack++;
				openxscrpt();
				break;

			  case 'w':	/* write back (or mail) */
				WriteBack++;
				openxscrpt();
				break;
			}
			break;

# ifdef DEBUG
		  case 'd':	/* debug */
			Debug++;
			break;
# endif DEBUG
		
		  case 'n':	/* don't alias */
			NoAlias++;
			break;

		  case 'm':	/* send to me too */
			MeToo++;
			break;

		  case 'i':	/* don't let dot stop me */
			IgnrDot++;
			break;

		  case 'a':	/* arpanet format */
			ArpaFmt++;
			break;
		
		  case 's':	/* save From lines in headers */
			SaveFrom++;
			break;

		  default:
			/* at Eric Schmidt's suggestion, this will not be an error....
			syserr("Unknown flag %s", p);
			... seems that upward compatibility will be easier. */
			break;
		}
	}

	if (from != NULL && ArpaFmt)
		syserr("-f and -a are mutually exclusive");

	/*
	** Get a temp file.
	*/

	p = maketemp();
	if (from == NULL)
		from = p;
# ifdef DEBUG
	if (Debug)
		printf("Message-Id: <%s>\n", MsgId);
# endif DEBUG

	/*
	**  Figure out who it's coming from.
	**	If we are root or "network", then allow -f.  Otherwise,
	**	insist that we figure it out ourselves.
	*/

	errno = 0;
	p = getname();
	if (p == NULL || p[0] == '\0')
	{
		syserr("Who are you? (uid=%d)", getuid());
		finis();
	}
	errno = 0;
	if (from != NULL)
	{
		if (strcmp(p, "network") != 0 && getuid() != 0 /* && strcmp(p, From) != 0 */ )
		{
			/* network sends -r regardless (why why why?) */
			/* syserr("%s, you cannot use the -f flag", p); */
			from = NULL;
		}
	}
	if (from == NULL || from[0] == '\0')
		from = p;
	else
		FromFlag++;
	SuprErrs = TRUE;
	if (parse(from, &From, 0) == NULL)
	{
		/* too many arpanet hosts generate garbage From addresses ....
		syserr("Bad from address `%s'", from);
		.... so we will just ignore this address */
		from = p;
		FromFlag = FALSE;
	}
	SuprErrs = FALSE;

# ifdef DEBUG
	if (Debug)
		printf("From person = \"%s\"\n", From.q_paddr);
# endif DEBUG

	if (argc <= 0)
		usrerr("Usage: /etc/delivermail [flags] addr...");

	/*
	**  Process Hop count.
	**	The Hop count tells us how many times this message has
	**	been processed by delivermail.  If it exceeds some
	**	fairly large threshold, then we assume that we have
	**	an infinite forwarding loop and die.
	*/

	if (++HopCount > MAXHOP)
		syserr("Infinite forwarding loop (%s->%s)", From.q_paddr, *argv);

	/*
	** Scan argv and deliver the message to everyone.
	*/

	for (; argc-- > 0; argv++)
	{
		sendto(*argv, 0);
	}

	/* if we have had errors sofar, drop out now */
	if (Error && ExitStat == EX_OK)
		ExitStat = EX_USAGE;
	if (ExitStat != EX_OK)
		finis();

	/*
	**  See if we have anyone to send to at all.
	*/

	if (nxtinq(&SendQ) == NULL && ExitStat == EX_OK)
	{
		syserr("Noone to send to!");
		ExitStat = EX_USAGE;
		finis();
	}

	/*
	**  Do aliasing.
	**	First arrange that the person who is sending the mail
	**	will not be expanded (unless explicitly requested).
	*/

	if (!MeToo)
		recipient(&From, &AliasQ);
	To = NULL;
	alias();
	if (nxtinq(&SendQ) == NULL && ExitStat == EX_OK)
	{
/*
		syserr("Vacant send queue; probably aliasing loop");
		ExitStat = EX_SOFTWARE;
		finis();
*/
		recipient(&From, &SendQ);
	}

	/*
	**  Actually send everything.
	*/

	for (q = &SendQ; (q = nxtinq(q)) != NULL; )
		deliver(q, (fnptr) NULL);

	/*
	** All done.
	*/

	finis();
}
/*
**  FINIS -- Clean up and exit.
**
**	Algorithm:
**		if we should remove the input
**			remove the input
**		exit
**
**	Parameters:
**		none
**
**	Returns:
**		never
**
**	Side Effects:
**		exits delivermail
**
**	Requires:
**		unlink (sys)
**		exit (sys)
**		savemail
**		InFileName -- the file to remove
**		ExitStat -- the status to exit with
**
**	Called By:
**		main
**		via signal on interrupt.
**
**	Deficiencies:
**		It may be that it should only remove the input
**			file if there have been no errors.
**
**	History:
**		12/26/79 -- written.
*/

finis()
{
	/* mail back the transcript on errors */
	if (ExitStat != EX_OK)
		savemail();

	if (HasXscrpt)
		unlink(Transcript);
	unlink(InFileName);
	exit(ExitStat);
}
/*
**  MAKETEMP -- Make temporary file
**
**	Creates a temporary file name and copies the standard
**	input to that file.  While it is doing it, it looks for
**	"From:" and "Sender:" fields to use as the from-person
**	(but only if the -a flag is specified).  It prefers to
**	to use the "Sender:" field -- the protocol says that
**	"Sender:" must come after "From:", so this works easily.
**	MIT seems to like to produce "Sent-By:" fields instead
**	of "Sender:" fields.  We used to catch this, but it turns
**	out that the "Sent-By:" field doesn't always correspond
**	to someone real, as required by the protocol.  So we limp
**	by.....
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
**	Requires:
**		creat (sys)
**		close (sys)
**		syserr
**		mktemp (sys)
**		fopen (sys)
**		fgets (sys)
**		makemsgid
**		fprintf (sys)
**		fputs (sys)
**		isspace (sys)
**		matchhdr
**		prescan
**		ferror (sys)
**		clearerr (sys)
**		freopen (sys)
**
**	Called By:
**		main
**
**	Notes:
**		This is broken off from main largely so that the
**		temp buffer can be deallocated.
**
**	Deficiencies:
**		It assumes that the From: field will preceed the
**		Sender: field.  This violates the Arpanet NIC 733
**		protocol, but seems reasonable in practice.  In
**		any case, the only problem is that error responses
**		may be sent to the wrong person.
**
**	History:
**		12/26/79 -- written.
*/

char *
maketemp()
{
	register FILE *tf;
	char buf[MAXLINE+1];
	static char fbuf[sizeof buf];
	extern char *prescan();
	extern char *matchhdr();
	register char *p;
	bool inheader;
	bool firstline;

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
	while (fgets(buf, sizeof buf, stdin) != NULL)
	{
		if (!IgnrDot && buf[0] == '.' && (buf[1] == '\n' || buf[1] == '\0'))
			break;

		/* are we still in the header? */
		if ((buf[0] == '\n' || buf[0] == '\0') && inheader)
		{
			inheader = FALSE;
			if (MsgId[0] == '\0')
			{
				makemsgid();
# ifdef MESSAGEID
				fprintf(tf, "Message-Id: <%s>\n", MsgId);
# endif MESSAGEID
			}
		}

		/* Hide UNIX-like From lines */
		if (buf[0] == 'F' && buf[1] == 'r' && buf[2] == 'o' &&
		    buf[3] == 'm' && buf[4] == ' ')
		{
			if (firstline && !SaveFrom)
				continue;
			fputs(">", tf);
		}

		if (inheader && !isspace(buf[0]))
		{
			/* find out if this is really a header */
			for (p = buf; *p != ':' && *p != '\0' && !isspace(*p); p++)
				continue;
			while (*p != ':' && isspace(*p))
				p++;
			if (*p != ':')
				inheader = FALSE;
		}

		if (inheader)
		{
			/* find the sender */
			p = matchhdr(buf, "from");
			if (p == NULL)
				p = matchhdr(buf, "sender");
			if (p != NULL)
				prescan(p, fbuf, &fbuf[sizeof fbuf - 1], '\0');

			/* find the message id */
			p = matchhdr(buf, "message-id");
			if (p != NULL && MsgId[0] == '\0')
				prescan(p, MsgId, &MsgId[sizeof MsgId - 1], '\0');
		}
		fputs(buf, tf);
		firstline = FALSE;
		if (ferror(tf))
		{
			syserr("Cannot write %s", InFileName);
			clearerr(tf);
			break;
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
**	Requires:
**		sprintf (sys)
**		getpid (sys)
**		time (sys)
**
**	Called By:
**		maketemp
**
**	History:
**		2/3/80 -- written.
*/

makemsgid()
{
	auto long t;
	extern char *MyLocName;

	time(&t);
	sprintf(MsgId, "%ld.%d.%s@Berkeley", t, getpid(), MyLocName);
}
/*
**  OPENXSCRPT -- Open transcript file
**
**	Creates a transcript file for possible eventual mailing or
**	sending back.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Turns the standard output into a special file
**			somewhere.
**
**	Requires:
**		mktemp (sys)
**		chmod (sys)
**		freopen (sys)
**		syserr
**		setbuf (sys)
**
**	Called By:
**		main
**
**	History:
**		1/11/80 -- written.
*/

openxscrpt()
{
	mktemp(Transcript);
	HasXscrpt++;
	if (freopen(Transcript, "w", stdout) == NULL)
		syserr("Can't create %s", Transcript);
	chmod(Transcript, 0600);
	setbuf(stdout, (char *) NULL);
}
