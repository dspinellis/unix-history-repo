# define  _DEFINE
# include <signal.h>
# include <pwd.h>
# include "sendmail.h"
# include <sys/stat.h>
# ifdef LOG
# include <syslog.h>
# endif LOG

SCCSID(@(#)main.c	3.62		%G%);

/*
**  SENDMAIL -- Post mail to a set of destinations.
**
**	This is the basic mail router.  All user mail programs should
**	call this routine to actually deliver mail.  Sendmail in
**	turn calls a bunch of mail servers that do the real work of
**	delivering the mail.
**
**	Sendmail is driven by tables defined in conf.c.  This
**	file will be different from system to system, but the rest
**	of the code will be the same.  This table could be read in,
**	but it seemed nicer to have it compiled in, since deliver-
**	mail will potentially be exercised a lot.
**
**	Usage:
**		/etc/sendmail [-f name] [-a] [-q] [-v] [-n] [-m] addr ...
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
**		-Ffullname	Select what the full-name should be
**				listed as.
**		-a		This mail should be in ARPANET std
**				format (obsolete version).
**		-am		Called from an FTP "MAIL" command.
**		-af		Called from an FTP "MLFL" command.
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
**		-v		Give blow-by-blow description of
**				everything that happens.
**		-t		Read "to" addresses from message.
**				Looks at To:, Cc:, and Bcc: lines.
**		-I		Initialize the DBM alias files from
**				the text format files.
**		-Cfilename	Use alternate configuration file.
**		-Afilename	Use alternate alias file.
**		-DXvalue	Define macro X to have value.
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
**	Author:
**		Eric Allman, UCB/INGRES
*/





int	NextMailer = 0;		/* "free" index into Mailer struct */
static char	*FullName;	/* sender's full name */






main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	extern char *getlogin();
	char *locname;
	extern int finis();
	extern char Version[];
	char *from;
	typedef int (*fnptr)();
	register int i;
	bool verifyonly = FALSE;	/* only verify names */
	bool safecf = TRUE;		/* this conf file is sys default */
	char ibuf[30];			/* holds HostName */
	bool queuemode = FALSE;		/* process queue requests */
	bool aliasinit = FALSE;
	extern bool safefile();
	STAB *st;
	extern time_t convtime();
	extern ADDRESS *recipient();
	bool canrename;

	argv[argc] = NULL;
	InChannel = stdin;
	OutChannel = stdout;
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGINT, finis);
	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		(void) signal(SIGHUP, finis);
	(void) signal(SIGTERM, finis);
	OldUmask = umask(0);
# ifdef LOG
	openlog("sendmail", 0);
# endif LOG
	openxscrpt();
# ifdef DEBUG
# ifdef DEBUGFILE
	if ((i = open(DEBUGFILE, 1)) > 0)
	{
		(void) lseek(i, 0L, 2);
		(void) close(1);
		(void) dup(i);
		(void) close(i);
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

		  case 'F':	/* set full name */
			p += 2;
			if (*p == '\0')
			{
				p = *++argv;
				if (--argc <= 0 || *p == '-')
				{
					syserr("Bad -F flag");
					argc++;
					argv--;
					break;
				}
			}
			FullName = p;
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
				(void) freopen("/dev/null", "w", stdout);
				break;

			  case 'm':	/* mail back */
				MailBack = TRUE;
				HoldErrs = TRUE;
				break;

			  case 'e':	/* do berknet error processing */
				BerkNet = TRUE;
				HoldErrs = TRUE;
				break;

			  case 'w':	/* write back (or mail) */
				WriteBack = TRUE;
				HoldErrs = TRUE;
				break;
			}
			break;

# ifdef DEBUG
		  case 'd':	/* debug */
			Debug = atoi(&p[2]);
			if (Debug <= 0)
				Debug = 1;
			setbuf(stdout, (char *) NULL);
			printf("Version %s Debug %d\n", Version, Debug);
			break;

		  case 'M':	/* redefine internal macro */
			define(p[2], &p[3]);
			break;
# endif DEBUG

		  case 'C':	/* select configuration file */
			if (p[2] == '\0')
				ConfFile = "sendmail.cf";
			else
				ConfFile = &p[2];
			safecf = FALSE;
			break;

		  case 'A':	/* select alias file */
			if (p[2] == '\0')
				AliasFile = "aliases";
			else
				AliasFile = &p[2];
			break;

		  case 'Q':	/* select queue dir */
			if (p[2] == '\0')
				AliasFile = "mqueue";
			else
				AliasFile = &p[2];
			break;

		  case 'T':	/* set timeout interval */
			TimeOut = convtime(&p[2]);
			break;
		
		  case 'n':	/* don't alias */
			NoAlias++;
			break;

# ifdef DBM
		  case 'I':	/* initialize alias DBM file */
			aliasinit = TRUE;
			Verbose = TRUE;
			break;
# endif DBM

		  case 'm':	/* send to me too */
			MeToo++;
			break;

		  case 'i':	/* don't let dot stop me */
			IgnrDot++;
			break;

		  case 'V':	/* verify only */
			verifyonly = TRUE;
			break;

		  case 'a':	/* arpanet format */
			ArpaMode = TRUE;
			if (p[2] == 's')
			{
				/* running smtp */
# ifdef SMTP
				Smtp = TRUE;
# else SMTP
				syserr("I don't speak SMTP");
# endif SMTP
			}
			break;

# ifdef QUEUE
		  case 'c':	/* don't connect to non-local mailers */
			NoConnect = TRUE;
			break;
# endif QUEUE
		
		  case 's':	/* save From lines in headers */
			SaveFrom++;
			break;

		  case 'v':	/* give blow-by-blow description */
			Verbose++;
			break;

		  case 't':	/* read recipients from message */
			GrabTo = TRUE;
			break;

		  case 'D':	/* run as a daemon */
			Daemon = TRUE;
			/* explicit fall-through */

		  case 'q':	/* run queue files at intervals */
# ifdef QUEUE
			queuemode = TRUE;
			QueueIntvl = atoi(&p[1]);
# else QUEUE
			syserr("I don't know about queues");
# endif QUEUE
			break;

		  case 'p':	/* fork politely after initial verification */
			ForkOff = TRUE;
			break;

		  case 'o':	/* take old-style headers (no commas) */
			OldStyle = TRUE;
			break;

		  default:
			/* at Eric Schmidt's suggestion, this will not be an error....
			syserr("Unknown flag %s", p);
			... seems that upward compatibility will be easier. */
			break;
		}
	}

	/*
	**  Read system control file.
	**	Extract special fields for local use.
	*/

	readcf(ConfFile, safecf);
	initsys();

	/* our name for SMTP codes */
	(void) expand("$i", ibuf, &ibuf[sizeof ibuf - 1]);
	HostName = ibuf;

	/* the indices of local and program mailers */
	st = stab("local", ST_MAILER, ST_FIND);
	if (st == NULL)
		syserr("No local mailer defined");
	else
		LocalMailer = st->s_mailer;
	st = stab("prog", ST_MAILER, ST_FIND);
	if (st == NULL)
		syserr("No prog mailer defined");
	else
		ProgMailer = st->s_mailer;

	/*
	**  Initialize aliases.
	*/

	initaliases(AliasFile, aliasinit);
# ifdef DBM
	if (aliasinit)
		exit(EX_OK);
# endif DBM

# ifdef DEBUG
	if (Debug > 15)
	{
		/* print configuration table (or at least part of it) */
		printrules();
		for (i = 0; i < MAXMAILERS; i++)
		{
			register struct mailer *m = Mailer[i];

			if (m == NULL)
				continue;
			printf("mailer %d: %s %s %o %s\n", i, m->m_name,
			       m->m_mailer, m->m_flags, m->m_from);
		}
	}
# endif DEBUG

	/*
	**  If a daemon, wait for a request.
	**	getrequests will always return in a child.
	*/

	if (Daemon)
		getrequests();
	
# ifdef SMTP
	/*
	if (Smtp)
	{
# ifdef QUEUE
		if (queuemode)
			runqueue(TRUE);
# endif QUEUE
		smtp();
	}
# endif SMTP

# ifdef QUEUE
	/*
	**  If collecting stuff from the queue, go start doing that.
	*/

	if (queuemode)
	{
		runqueue(FALSE);
		finis();
	}
# endif QUEUE

	/*
	**  Set the sender
	*/

	setsender(from);

	if (!Daemon && argc <= 0 && !GrabTo)
	{
		usrerr("Usage: /etc/sendmail [flags] addr...");
		finis();
	}

	/*
	**  Process Hop count.
	**	The Hop count tells us how many times this message has
	**	been processed by sendmail.  If it exceeds some
	**	fairly large threshold, then we assume that we have
	**	an infinite forwarding loop and die.
	*/

	if (++HopCount > MAXHOP)
		syserr("Infinite forwarding loop (%s->%s)", From.q_paddr, *argv);

	/*
	**  Scan argv and deliver the message to everyone.
	**	Actually, suppress delivery if we are taking To:
	**	lines from the message.
	*/

	if (GrabTo)
		DontSend = TRUE;
	sendtoargv(argv);

	/* if we have had errors sofar, drop out now */
	if (Errors > 0 && ExitStat == EX_OK)
		ExitStat = EX_USAGE;

	/*
	**  Read the input mail.
	*/

	DontSend = FALSE;
	To = NULL;
	if (!verifyonly || GrabTo)
		collect(FALSE);
	errno = 0;

	/*
	**  If we don't want to wait around for actual delivery, this
	**  is a good time to fork off.
	**	We have examined what we can without doing actual
	**		delivery, so we will inform our caller of
	**		whatever we can now.
	**	Since the parent process will go away immediately,
	**		the child will be caught by init.
	**	If the fork fails, we will just continue in the
	**		parent; this is perfectly safe, albeit
	**		slower than it must be.
	*/

	if (ForkOff)
	{
		if (fork() > 0)
		{
			/* parent -- quit */
			exit(ExitStat);
		}
	}

	initsys();

	/* collect statistics */
	Stat.stat_nf[From.q_mailer->m_mno]++;
	Stat.stat_bf[From.q_mailer->m_mno] += kbytes(MsgSize);

	/*
	**  Arrange that the person who is sending the mail
	**  will not be expanded (unless explicitly requested).
	*/

# ifdef DEBUG
	if (Debug)
		printf("From person = \"%s\"\n", From.q_paddr);
# endif DEBUG

	From.q_flags |= QDONTSEND;
	if (!MeToo)
		(void) recipient(&From);
	To = NULL;

	/*
	**  Actually send everything.
	**	If verifying, just ack.
	*/

	sendall(verifyonly);

	/*
	** All done.
	*/

	To = NULL;
	if (!verifyonly)
		poststats(StatFile);
	finis();
}
/*
**  SETFROM -- set the person who this message is from
**
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
**
**	Parameters:
**		from -- the person it is from.
**		realname -- the actual person executing sendmail.
**			If NULL, then take whoever we previously
**			thought was the from person.
**
**	Returns:
**		none.
**
**	Side Effects:
**		sets sendmail's notion of who the from person is.
*/

setfrom(from, realname)
	char *from;
	char *realname;
{
	register char **pvp;
	char frombuf[MAXNAME];
	extern char **prescan();
	extern char *index();

	if (realname == NULL)
		realname = From.q_paddr;

# ifdef DEBUG
	if (Debug > 1)
		printf("setfrom(%s, %s)\n", from, realname);
# endif DEBUG

	if (from != NULL)
	{
		if (strcmp(realname, "network") != 0 && strcmp(realname, "uucp") != 0 &&
# ifdef DEBUG
		    (Debug == 0 || getuid() != geteuid()) &&
# endif DEBUG
		    index(from, '!') == NULL && getuid() != 0)
		{
			/* network sends -r regardless (why why why?) */
			/* syserr("%s, you cannot use the -f flag", realname); */
			from = NULL;
		}
	}

	SuprErrs = TRUE;
	if (from == NULL || parse(from, &From, 1) == NULL)
	{
		from = newstr(realname);
		(void) parse(from, &From, 1);
	}
	else
		FromFlag = TRUE;
	SuprErrs = FALSE;
	From.q_uid = getuid();
	From.q_gid = getgid();
# ifndef V6
	From.q_home = getenv("HOME");
# endif V6
	if (From.q_uid != 0)
	{
		DefUid = From.q_uid;
		DefGid = From.q_gid;
	}

	/*
	**  Set up the $r and $s macros to show who it came from.
	*/

	if (From.q_host != NULL && From.q_host[0] != '\0')
	{
		define('s', From.q_host);

		/* should determine network type here */
	}

	/*
	**  Rewrite the from person to dispose of possible implicit
	**	links in the net.
	*/

	pvp = prescan(from, '\0');
	if (pvp == NULL)
	{
		syserr("cannot prescan from (%s)", from);
		finis();
	}
	rewrite(pvp, 1);
	cataddr(pvp, frombuf, sizeof frombuf);
	define('f', newstr(frombuf));
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
**		exits sendmail
*/

finis()
{
# ifdef DEBUG
	if (Debug > 2)
		printf("\n====finis: stat %d\n", ExitStat);
# endif DEBUG

	/* mail back the transcript on errors */
	if (ExitStat != EX_OK)
		savemail();

	if (Transcript != NULL)
		(void) unlink(Transcript);
	if (QueueUp)
	{
# ifdef QUEUE
		queueup(InFileName);
# else QUEUE
		syserr("finis: trying to queue %s", InFileName);
# endif QUEUE
	}
	else
		(void) unlink(InFileName);
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
*/

openxscrpt()
{
	extern char *mktemp();
	register char *p;

	p = newstr(XcriptFile);
	(void) mktemp(p);
	Xscript = fopen(p, "w");
	if (Xscript == NULL)
	{
		Xscript = stdout;
		syserr("Can't create %s", p);
	}
	Transcript = p;
	(void) chmod(p, 0600);
}
/*
**  SETSENDER -- set sendmail's idea of the sender.
**
**	Parameters:
**		from -- the person we would like to believe this
**			is from.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sets the idea of the sender.
*/

setsender(from)
	char *from;
{
	register char *p;
	extern char *getlogin();
	register struct passwd *pw;
	char *realname;
	char cfbuf[40];
	bool nofullname;
	extern char *macvalue();

	/*
	**  Figure out the real user executing us.
	**	Getlogin can return errno != 0 on non-errors.
	*/

	if (!Smtp && !QueueRun)
	{
		errno = 0;
		p = getlogin();
		errno = 0;
		nofullname = (from != NULL);
	}
	else
	{
		p = from;
		nofullname = FALSE;
	}
	if (p != NULL)
	{
		extern struct passwd *getpwnam();

		pw = getpwnam(p);
		if (pw == NULL)
		{
			if (!Smtp && !QueueRun)
				syserr("Who are you? (name=%s)", p);
			p = NULL;
		}
	}
	if (p == NULL)
	{
		extern struct passwd *getpwuid();
		int uid;

		nofullname = TRUE;
		uid = getruid();
		pw = getpwuid(uid);
		if (pw == NULL)
			syserr("Who are you? (uid=%d)", uid);
		else
			p = pw->pw_name;
	}
	if (p == NULL || p[0] == '\0' || pw == NULL)
		finis();

	realname = p;

	/*
	**  Process passwd file entry.
	*/

	/* run user's .mailcf file */
	define('z', pw->pw_dir);
	(void) expand("$z/.mailcf", cfbuf, &cfbuf[sizeof cfbuf - 1]);
	if (!nofullname && safefile(cfbuf, getruid(), S_IREAD))
		readcf(cfbuf, FALSE);

	/* if the user has given fullname already, don't redefine */
	if (FullName == NULL)
		FullName = macvalue('x');

	/* extract full name from passwd file */
	if (!nofullname && (FullName == NULL || FullName[0] == '\0') &&
	    pw != NULL && pw->pw_gecos != NULL)
	{
		char nbuf[MAXNAME];

		fullname(pw, nbuf);
		if (nbuf[0] != '\0')
			FullName = newstr(nbuf);
	}
	if (FullName != NULL && FullName[0] != '\0')
		define('x', FullName);

	setfrom(from, realname);
}
/*
**  INITSYS -- initialize instantiation of system
**
**	In Daemon mode, this is done in the child.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Initializes the system macros, some global variables,
**		etc.  In particular, the current time in various
**		forms is set.
*/

initsys()
{
	static char cbuf[5];			/* holds hop count */
	static char dbuf[30];			/* holds ctime(tbuf) */
	static char pbuf[10];			/* holds pid */
	static char tbuf[10];			/* holds "current" time */
	static char ybuf[10];			/* holds tty id */
	register char *p;
	extern char *ttyname();
	extern char *arpadate();

	/* convert timeout interval to absolute time */
	TimeOut -= CurTime;
	(void) time(&CurTime);
	TimeOut += CurTime;

	/* process id */
	(void) sprintf(pbuf, "%d", getpid());
	define('p', pbuf);

	/* hop count */
	(void) sprintf(cbuf, "%d", HopCount);
	define('c', cbuf);

	/* time as integer, unix time, arpa time */
	(void) sprintf(tbuf, "%ld", &CurTime);
	define('t', tbuf);
	(void) strcpy(dbuf, ctime(&CurTime));
	*index(dbuf, '\n') = '\0';
	if (macvalue('d') == NULL)
		define('d', dbuf);
	p = newstr(arpadate(dbuf));
	if (macvalue('a') == NULL)
		define('a', p);
	define('b', p);

	/* version */
	define('v', Version);

	/* tty name */
	if (macvalue('y') == NULL)
	{
		p = ttyname(2);
		if (p != NULL)
		{
			if (rindex(p, '/') != NULL)
				p = rindex(p, '/') + 1;
			strcpy(ybuf, p);
			define('y', ybuf);
		}
	}
}
