# include <stdio.h>
# include <signal.h>
# include <ctype.h>
# include "dlvrmail.h"
# ifdef LOG
# include <log.h>
# endif LOG

static char	SccsId[] = "@(#)main.c	1.11	10/18/80";

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
**		-ee		Send BerkNet style errors.  This
**				is equivalent to MailBack except
**				that it has gives zero return code
**				(unless there were errors during
**				returning).  This used to be
**				"EchoBack", but you know how the old
**				software bounces.
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
**	Compilation Flags:
**		LOG -- if set, everything is logged.
**
**	Compilation Instructions:
**		cc -c -O main.c config.c deliver.c parse.c
**		cc -n -s *.o -lS
**		chown root a.out
**		chmod 755 a.out
**		mv a.out delivermail
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
*/





bool	ArpaFmt;	/* mail is expected to be in ARPANET format */
bool	FromFlag;	/* from person is explicitly specified */
bool	Debug;		/* run in debug mode */
bool	MailBack;	/* mail back response on error */
bool	BerkNet;	/* called from BerkNet */
bool	WriteBack;	/* write back response on error */
bool	HasXscrpt;	/* if set, the transcript file exists */
bool	NoAlias;	/* don't do aliasing */
bool	ForceMail;	/* mail even if already sent a copy */
bool	MeToo;		/* send to the sender also if in a group expansion */
bool	SaveFrom;	/* save From lines on the front of messages */
bool	IgnrDot;	/* if set, ignore dot when collecting mail */
bool	SuprErrs;	/* supress errors if set */
int	Errors;		/* count of errors */
char	InFileName[] = "/tmp/mailtXXXXXX";
char	Transcript[] = "/tmp/mailxXXXXXX";
addrq	From;		/* the from person */
char	*To;		/* the target person */
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

			  case 'e':	/* do berknet error processing */
				BerkNet++;
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
			printf("%s\n", Version);
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
	**	Under certain circumstances allow the user to say who
	**	s/he is (using -f or -r).  These are:
	**	1.  The user's uid is zero (root).
	**	2.  The user's login name is "network" (mail from
	**	    a network server).
	**	3.  The user's login name is "uucp" (mail from the
	**	    uucp network).
	**	4.  The address the user is trying to claim has a
	**	    "!" character in it (since #3 doesn't do it for
	**	    us if we are dialing out).
	**	A better check to replace #3 & #4 would be if the
	**	effective uid is "UUCP" -- this would require me
	**	to rewrite getpwent to "grab" uucp as it went by,
	**	make getname more nasty, do another passwd file
	**	scan, or compile the UID of "UUCP" into the code,
	**	all of which are reprehensible.
	**
	**	Assuming all of these fail, we figure out something
	**	ourselves.
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
		if (strcmp(p, "network") != 0 && strcmp(p, "uucp") != 0 &&
		    index(from, '!') == NULL && getuid() != 0)
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
	if (Errors > 0 && ExitStat == EX_OK)
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
**	Parameters:
**		none
**
**	Returns:
**		never
**
**	Side Effects:
**		exits delivermail
**
**	Called By:
**		main
**		via signal on interrupt.
**
**	Deficiencies:
**		It may be that it should only remove the input
**			file if there have been no errors.
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
**	Called By:
**		main
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
