# define  _DEFINE
# include <signal.h>
# include <pwd.h>
# include <time.h>
# include <sys/ioctl.h>
# include "sendmail.h"
# include <sys/stat.h>

SCCSID(@(#)main.c	3.108		%G%);

/*
**  SENDMAIL -- Post mail to a set of destinations.
**
**	This is the basic mail router.  All user mail programs should
**	call this routine to actually deliver mail.  Sendmail in
**	turn calls a bunch of mail servers that do the real work of
**	delivering the mail.
**
**	Sendmail is driven by tables read in from /usr/lib/sendmail.cf
**	(read by readcf.c).  Some more static configuration info,
**	including some code that you may want to tailor for your
**	installation, is in conf.c.  You may also want to touch
**	daemon.c (if you have some other IPC mechanism), acct.c
**	(to change your accounting), names.c (to adjust the name
**	server mechanism).
**
**	Usage:
**		/etc/sendmail [flags] addr ...
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
**		-as		Speak SMTP.
**		-n		Don't do aliasing.  This might be used
**				when delivering responses, for
**				instance.
**		-dN		Run with debugging set to level N.
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
**		-bv		Verify addresses only.
**		-bd		Run as a daemon.  Berkeley 4.2 only.
**		-bf		Fork after address verification.
**		-bq		Queue up for later delivery.
**		-ba		Process mail completely.
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





int		NextMailer = 0;	/* "free" index into Mailer struct */
static char	*FullName;	/* sender's full name */
ENVELOPE	MainEnvelope;	/* the envelope around the basic letter */

#ifdef DAEMON
#ifndef SMTP
ERROR %%%%   Cannot have daemon mode without SMTP   %%%% ERROR
#endif SMTP
#endif DAEMON






main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	char *locname;
	extern int finis();
	extern char Version[];
	char *from;
	typedef int (*fnptr)();
	register int i;
	bool safecf = TRUE;		/* this conf file is sys default */
	char jbuf[30];			/* holds HostName */
	bool queuemode = FALSE;		/* process queue requests */
	bool aliasinit = FALSE;
	extern bool safefile();
	STAB *st;
	extern time_t convtime();
	extern putheader(), putbody();
	static bool reenter = FALSE;

	if (reenter)
	{
		syserr("main: reentered!");
		abort();
	}
	reenter = TRUE;
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
	Mode = MD_DEFAULT;
	MotherPid = getpid();
# ifndef V6
	FullName = getenv("NAME");
# endif V6

	/* set up the main envelope */
	MainEnvelope.e_puthdr = putheader;
	MainEnvelope.e_putbody = putbody;
	CurEnv = &MainEnvelope;

# ifdef LOG
	openlog("sendmail", 0);
# endif LOG
	openxscrpt();
	errno = 0;
	from = NULL;
	CurEnv->e_oldstyle = TRUE;
	NoConnect = TRUE;
	initmacros();

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
			tTsetup(tTdvect, sizeof tTdvect, "0-99.1");
			tTflag(&p[2]);
			setbuf(stdout, (char *) NULL);
			printf("Version %s\n", Version);
			break;

		  case 'M':	/* redefine internal macro */
			define(p[2], &p[3]);
			break;
# endif DEBUG

		  case 'L':	/* logging */
			LogLevel = atoi(&p[2]);
			break;

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
				QueueDir = "mqueue";
			else
			{
				if (strlen(&p[2]) > 50)
					syserr("Absurd length Queue path");
				else
					QueueDir = &p[2];
			}
			break;

		  case 'T':	/* set timeout interval */
			TimeOut = convtime(&p[2]);
			break;
		
		  case 'n':	/* don't alias */
			NoAlias = TRUE;
			break;

# ifdef DBM
		  case 'I':	/* initialize alias DBM file */
			aliasinit = TRUE;
			Verbose = TRUE;
			break;
# endif DBM

		  case 'm':	/* send to me too */
			MeToo = TRUE;
			break;

		  case 'i':	/* don't let dot stop me */
			IgnrDot = TRUE;
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
		  case 'c':	/* connect to non-local mailers */
			NoConnect = FALSE;
			break;
# endif QUEUE
		
		  case 's':	/* save From lines in headers */
			SaveFrom = TRUE;
			break;

		  case 'v':	/* give blow-by-blow description */
			Verbose = TRUE;
			NoConnect = FALSE;
			break;

		  case 't':	/* read recipients from message */
			GrabTo = TRUE;
			break;

		  case 'b':	/* operations mode */
			Mode = p[2];
			switch (Mode)
			{
			  case MD_DAEMON:	/* run as a daemon */
#ifdef DAEMON
				ArpaMode = Smtp = TRUE;
#else DAEMON
				syserr("Daemon mode not implemented");
#endif DAEMON
				break;

			  case '\0':	/* default: do full delivery */
				Mode = MD_DEFAULT;
				/* fall through....... */

			  case MD_DELIVER:	/* do everything (default) */
			  case MD_FORK:		/* fork after verification */
			  case MD_QUEUE:	/* queue only */
			  case MD_VERIFY:	/* verify only */
				break;

			  default:
				syserr("Unknown operation mode -b%c", Mode);
				exit(EX_USAGE);
			}
			break;

		  case 'q':	/* run queue files at intervals */
# ifdef QUEUE
			queuemode = TRUE;
			QueueIntvl = convtime(&p[2]);
# else QUEUE
			syserr("I don't know about queues");
# endif QUEUE
			break;

		  case 'o':	/* take new-style headers (with commas) */
			CurEnv->e_oldstyle = FALSE;
			break;

		  default:
			/* at Eric Schmidt's suggestion, this will not be an error....
			syserr("Unknown flag %s", p);
			... seems that upward compatibility will be easier. */
			break;
		}
	}

	/*
	**  Do basic initialization.
	**	Read system control file.
	**	Extract special fields for local use.
	*/

# ifdef LOG
	if (LogLevel > 8)
		syslog(LOG_DEBUG, "entered, uid=%d, pid=%d", getuid(), getpid());
# endif LOG
	readcf(ConfFile, safecf);
	initsys();

	/* our name for SMTP codes */
	expand("$j", jbuf, &jbuf[sizeof jbuf - 1], CurEnv);
	HostName = jbuf;

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
	if (tTd(0, 15))
	{
		/* print configuration table (or at least part of it) */
		printrules();
		for (i = 0; i < MAXMAILERS; i++)
		{
			register struct mailer *m = Mailer[i];

			if (m == NULL)
				continue;
			printf("mailer %d: %s %s %lo %d %d\n", i, m->m_name,
			       m->m_mailer, m->m_flags, m->m_s_rwset, m->m_r_rwset);
		}
	}
# endif DEBUG

#ifdef DAEMON
	/*
	**  If a daemon, wait for a request.
	**	getrequests will always return in a child.
	**	If we should also be processing the queue, start
	**		doing it in background.
	**	We check for any errors that might have happened
	**		during startup.
	*/

	if (Mode == MD_DAEMON)
	{
		if (!tTd(0, 1))
		{
			/* put us in background */
			i = fork();
			if (i < 0)
				syserr("daemon: cannot fork");
			if (i != 0)
				exit(0);

			/* get our pid right */
			MotherPid = getpid();
# ifdef LOG
			if (LogLevel > 11)
				syslog(LOG_DEBUG, "background daemon, pid=%d",
				       MotherPid);
# endif LOG

			/* disconnect from our controlling tty */
			i = open("/dev/tty", 2);
			if (i >= 0)
			{
				(void) ioctl(i, TIOCNOTTY, 0);
				(void) close(i);
			}
			errno = 0;
		}
# ifdef QUEUE
		if (queuemode)
			runqueue(TRUE);
# endif QUEUE
		checkerrors(CurEnv);
		getrequests();

		/* at this point we are in a child: reset state */
		dropenvelope(CurEnv);
		CurEnv->e_id = CurEnv->e_qf = CurEnv->e_df = NULL;
		FatalErrors = FALSE;
		openxscrpt();
		initsys();
	}
#endif DAEMON
# ifdef QUEUE
	/*
	**  If collecting stuff from the queue, go start doing that.
	*/

	if (queuemode && Mode != MD_DAEMON)
	{
		runqueue(FALSE);
		finis();
	}
# endif QUEUE

	/*
	**  Give this envelope a reality.
	**	I.e., an id and a creation time.
	*/

	(void) queuename(CurEnv, '\0');
	CurEnv->e_ctime = curtime();
	
# ifdef SMTP
	/*
	**  If running SMTP protocol, start collecting and executing
	**  commands.  This will never return.
	*/

	if (Smtp)
		smtp();
# endif SMTP

	/*
	**  Set the sender
	*/

	setsender(from);

	if (Mode != MD_DAEMON && argc <= 0 && !GrabTo)
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
		syserr("Infinite forwarding loop (%s->%s)", CurEnv->e_from.q_paddr, *argv);

	/*
	**  Scan argv and deliver the message to everyone.
	**	Actually, suppress delivery if we are taking To:
	**	lines from the message.
	*/

	if (GrabTo)
		DontSend = TRUE;
	sendtoargv(argv);

	/* if we have had errors sofar, arrange a meaningful exit stat */
	if (Errors > 0 && ExitStat == EX_OK)
		ExitStat = EX_USAGE;

	/*
	**  Read the input mail.
	*/

	DontSend = FALSE;
	CurEnv->e_to = NULL;
	if (Mode != MD_VERIFY || GrabTo)
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
	**	If we have errors sofar, this seems like a good time
	**		to dispose of them.
	*/

	if (ExitStat != EX_OK && Mode != MD_VERIFY)
		finis();

	if (Mode == MD_FORK)
	{
		if (fork() > 0)
		{
			/* parent -- quit */
			exit(ExitStat);
		}
# ifdef LOG
		if (LogLevel > 11)
			syslog(LOG_DEBUG, "background delivery, pid=%d", getpid());
# endif LOG
	}
	else if (Mode == MD_QUEUE)
	{
		queueup(CurEnv, TRUE);
		exit(ExitStat);
	}

	initsys();

	/* collect statistics */
	Stat.stat_nf[CurEnv->e_from.q_mailer->m_mno]++;
	Stat.stat_bf[CurEnv->e_from.q_mailer->m_mno] += kbytes(CurEnv->e_msgsize);

	/*
	**  Arrange that the person who is sending the mail
	**  will not be expanded (unless explicitly requested).
	*/

# ifdef DEBUG
	if (tTd(1, 1))
		printf("From person = \"%s\"\n", CurEnv->e_from.q_paddr);
# endif DEBUG

	CurEnv->e_from.q_flags |= QDONTSEND;
	if (!MeToo)
		recipient(&CurEnv->e_from, &CurEnv->e_sendqueue);
	CurEnv->e_to = NULL;

	/*
	**  Actually send everything.
	**	If verifying, just ack.
	*/

	sendall(CurEnv, Mode == MD_VERIFY);

	/*
	** All done.
	*/

	CurEnv->e_to = NULL;
	if (Mode != MD_VERIFY)
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
	extern char *macvalue();

	if (realname == NULL)
		realname = CurEnv->e_from.q_paddr;

# ifdef DEBUG
	if (tTd(1, 1))
		printf("setfrom(%s, %s)\n", from, realname);
# endif DEBUG

	/*
	**  Do validation to determine whether this user is allowed
	**  to change the sender name.
	*/

	if (from != NULL)
	{
		if (strcmp(realname, "network") != 0 &&
		    strcmp(realname, "uucp") != 0 &&
		    strcmp(realname, "daemon") != 0 &&
# ifdef DEBUG
		    (!tTd(1, 9) || getuid() != geteuid()) &&
# endif DEBUG
		    index(from, '!') == NULL && getuid() != 0)
		{
			/* network sends -r regardless (why why why?) */
			/* syserr("%s, you cannot use the -f flag", realname); */
			from = NULL;
		}
	}

	/*
	**  Parse the sender name.
	**	Arrange to send return messages to the same person.
	**	Set up some environment info.
	*/

	SuprErrs = TRUE;
	if (from == NULL || parse(from, &CurEnv->e_from, 1) == NULL)
	{
		from = newstr(realname);
		(void) parse(from, &CurEnv->e_from, 1);
	}
	else
		FromFlag = TRUE;
	CurEnv->e_returnto = &CurEnv->e_from;
	SuprErrs = FALSE;
	CurEnv->e_from.q_uid = getuid();
	CurEnv->e_from.q_gid = getgid();
# ifndef V6
	CurEnv->e_from.q_home = getenv("HOME");
# endif V6
	if (CurEnv->e_from.q_uid != 0)
	{
		DefUid = CurEnv->e_from.q_uid;
		DefGid = CurEnv->e_from.q_gid;
	}

	/*
	**  Set up the $r and $s macros to show who it came from.
	*/

	if (macvalue('s') == NULL && CurEnv->e_from.q_host != NULL &&
	    CurEnv->e_from.q_host[0] != '\0')
	{
		define('s', CurEnv->e_from.q_host);

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
	CurEnv = &MainEnvelope;

# ifdef DEBUG
	if (tTd(2, 1))
	{
		printf("\n====finis: stat %d sendreceipt %d FatalErrors %d\n",
		     ExitStat, CurEnv->e_sendreceipt, FatalErrors);
	}
# endif DEBUG

	/*
	**  Send back return receipts as requested.
	*/

	if (CurEnv->e_receiptto != NULL &&
	    (CurEnv->e_sendreceipt || ExitStat != EX_OK))
	{
		auto ADDRESS *rlist;

		sendto(CurEnv->e_receiptto, (ADDRESS *) NULL, &rlist);
		(void) returntosender("Return receipt", rlist, FALSE);
	}

	/*
	**  Arrange to return errors or queue up as appropriate.
	**	If we are running a queue file and exiting abnormally,
	**		be sure we save the queue file.
	**	This clause will arrange to return error messages.
	*/

	checkerrors(CurEnv);

	/*
	**  Now clean up temp files and exit.
	*/

	if (Transcript != NULL)
		xunlink(Transcript);
	dropenvelope(CurEnv);
# ifdef LOG
	if (LogLevel > 11)
		syslog(LOG_DEBUG, "finis, pid=%d", getpid());
# endif LOG
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
	if (p != NULL && p[0] != '\0')
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
	if (p == NULL || p[0] == '\0')
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
	expand("$z/.mailcf", cfbuf, &cfbuf[sizeof cfbuf - 1], CurEnv);
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
	static char tbuf[20];			/* holds "current" time */
	static char ybuf[10];			/* holds tty id */
	register char *p;
	extern char *ttyname();
	extern char *arpadate();
	register struct tm *tm;
	extern struct tm *gmtime();
	auto time_t now;

	/* process id */
	(void) sprintf(pbuf, "%d", getpid());
	define('p', pbuf);

	/* hop count */
	(void) sprintf(cbuf, "%d", HopCount);
	define('c', cbuf);

	/* time as integer, unix time, arpa time */
	now = curtime();
	tm = gmtime(&now);
	(void) sprintf(tbuf, "%02d%02d%02d%02d%02d", tm->tm_year, tm->tm_mon,
			tm->tm_mday, tm->tm_hour, tm->tm_min);
	define('t', tbuf);
	(void) strcpy(dbuf, ctime(&now));
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
			(void) strcpy(ybuf, p);
			define('y', ybuf);
		}
	}
}
/*
**  INITMACROS -- initialize the macro system
**
**	This just involves defining some macros that are actually
**	used internally as metasymbols to be themselves.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		initializes several macros to be themselves.
*/

struct metamac
{
	char	metaname;
	char	metaval;
};

struct metamac	MetaMacros[] =
{
	/* these are important on the LHS */
	'*', MATCHZANY,	'+', MATCHANY,	'-', MATCHONE,	'=', MATCHCLASS,

	/* these are RHS metasymbols */
	'#', CANONNET,	'@', CANONHOST,	':', CANONUSER,	'>', CALLSUBR,

	/* and finally the conditional operations */
	'?', CONDIF,	'|', CONDELSE,	'.', CONDFI,

	'\0'
};

initmacros()
{
	register struct metamac *m;
	char buf[5];
	register int c;

	for (m = MetaMacros; m->metaname != '\0'; m++)
	{
		buf[0] = m->metaval;
		buf[1] = '\0';
		define(m->metaname, newstr(buf));
	}
	buf[0] = MATCHREPL;
	buf[2] = '\0';
	for (c = '0'; c <= '9'; c++)
	{
		buf[1] = c;
		define(c, newstr(buf));
	}
}
/*
**  NEWENVELOPE -- allocate a new envelope
**
**	Supports inheritance.
**
**	Parameters:
**		e -- the new envelope to fill in.
**
**	Returns:
**		e.
**
**	Side Effects:
**		none.
*/

ENVELOPE *
newenvelope(e)
	register ENVELOPE *e;
{
	bmove((char *) CurEnv, (char *) e, sizeof *e);
	e->e_header = NULL;
	e->e_queueup = FALSE;
	e->e_dontqueue = FALSE;
	e->e_oldstyle = FALSE;
	e->e_sendreceipt = FALSE;
	e->e_receiptto = NULL;
	e->e_to = NULL;
	e->e_sendqueue = NULL;
	e->e_errorqueue = NULL;
	e->e_parent = CurEnv;
	e->e_df = NULL;
	e->e_qf = NULL;
	e->e_id = NULL;
	e->e_ctime = curtime();

	return (e);
}
/*
**  DROPENVELOPE -- deallocate an envelope.
**
**	Parameters:
**		e -- the envelope to deallocate.
**
**	Returns:
**		none.
**
**	Side Effects:
**		housekeeping necessary to dispose of an envelope.
*/

dropenvelope(e)
	register ENVELOPE *e;
{
	if (e->e_df != NULL)
		xunlink(e->e_df);
	if (e->e_qf != NULL)
		xunlink(e->e_qf);
	if (e->e_id != NULL)
		xunlink(queuename(e, 'l'));
}
/*
**  QUEUENAME -- build a file name in the queue directory for this envelope.
**
**	Assigns an id code if one does not already exist.
**
**	Parameters:
**		e -- envelope to build it in/from.
**		type -- the file type, used as the first character
**			of the file name.
**
**	Returns:
**		a pointer to the new file name (in a static buffer).
**
**	Side Effects:
**		Will create the lf and qf files if no id code is
**		already assigned.  This will cause the envelope
**		to be modified.
*/

char *
queuename(e, type)
	register ENVELOPE *e;
	char type;
{
	static char buf[MAXNAME];

	if (e->e_id == NULL)
	{
		char counter = 'a' - 1;
		char qf[MAXNAME];
		char lf[MAXNAME];
		int fx;

		/* find a unique id */
		fx = strlen(QueueDir) + 3;
		(void) sprintf(qf, "%s/qf_%05d", QueueDir, getpid());
		strcpy(lf, qf);
		lf[fx - 2] = 'l';

		for (;;)
		{
			qf[fx] = lf[fx] = ++counter;
			if (access(lf, 0) >= 0 || access(qf, 0) >= 0)
				continue;
			errno = 0;
			if (close(creat(lf, 0600)) < 0)
				continue;
			if (link(lf, qf) < 0)
				(void) unlink(lf);
			else
				break;
		}
		e->e_qf = newstr(qf);
		e->e_id = &e->e_qf[fx];
		define('i', e->e_id);
# ifdef DEBUG
		if (tTd(7, 1))
			printf("queuename: assigned id %s, env=%x\n", e->e_id, e);
# endif DEBUG
	}

	if (type == '\0')
		return (NULL);
	(void) sprintf(buf, "%s/%cf%s", QueueDir, type, e->e_id);
# ifdef DEBUG
	if (tTd(7, 2))
		printf("queuename: %s\n", buf);
# endif DEBUG
	return (buf);
}
