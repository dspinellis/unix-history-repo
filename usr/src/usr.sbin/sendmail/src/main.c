# define  _DEFINE
# include <signal.h>
# include <pwd.h>
# include <time.h>
# include <sys/ioctl.h>
# include "sendmail.h"
# include <sys/stat.h>

SCCSID(@(#)main.c	3.134		%G%);

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
ENVELOPE	BlankEnvelope;	/* a "blank" envelope */
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
	int ac;
	char **av;
	char *locname;
	extern int finis();
	extern char Version[];
	char *from;
	typedef int (*fnptr)();
	STAB *st;
	register int i;
	int pass = 0;
	bool safecf = TRUE;		/* this conf file is sys default */
	bool queuemode = FALSE;		/* process queue requests */
	static bool reenter = FALSE;
	char jbuf[30];			/* holds HostName */
	extern bool safefile();
	extern time_t convtime();
	extern putheader(), putbody();
	extern ENVELOPE *newenvelope();
	extern intsig();

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
		(void) signal(SIGINT, intsig);
	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		(void) signal(SIGHUP, intsig);
	(void) signal(SIGTERM, intsig);
	OldUmask = umask(0);
	OpMode = MD_DELIVER;
	MotherPid = getpid();
# ifndef V6
	FullName = getenv("NAME");
# endif V6

	/* set up the blank envelope */
	BlankEnvelope.e_puthdr = putheader;
	BlankEnvelope.e_putbody = putbody;
	CurEnv = &BlankEnvelope;

# ifdef LOG
	openlog("sendmail", 0);
# endif LOG
	Xscript = stderr;
	errno = 0;
	from = NULL;
	initmacros();

	/*
	** Crack argv.
	*/

  crackargs:
	ac = argc;
	av = argv;
	pass++;
	p = rindex(*av, '/');
	if (p++ == NULL)
		p = *av;
	if (strcmp(p, "newaliases") == 0)
		OpMode = MD_INITALIAS;
	else if (strcmp(p, "mailq") == 0)
		OpMode = MD_PRINT;
	while (--ac > 0 && (p = *++av)[0] == '-')
	{
		switch (p[1])
		{
		  case 'b':	/* operations mode */
			switch (p[2])
			{
			  case MD_DAEMON:
# ifndef DAEMON
				syserr("Daemon mode not implemented");
				break;
# endif DAEMON
			  case MD_SMTP:
# ifndef SMTP
				syserr("I don't speak SMTP");
				break;
# endif SMTP
			  case MD_ARPAFTP:
			  case MD_DELIVER:
			  case MD_VERIFY:
			  case MD_TEST:
			  case MD_INITALIAS:
			  case MD_PRINT:
			  case MD_FREEZE:
				OpMode = p[2];
				break;

			  default:
				syserr("Invalid operation mode %c", p[2]);
				break;
			}
			break;

		  case 'C':	/* select configuration file */
			ConfFile = &p[2];
			if (ConfFile[0] == '\0')
				ConfFile = "sendmail.cf";
			safecf = FALSE;
			break;

# ifdef DEBUG
		  case 'd':	/* debug */
			tTsetup(tTdvect, sizeof tTdvect, "0-99.1");
			tTflag(&p[2]);
			setbuf(stdout, (char *) NULL);
			printf("Version %s\n", Version);
			break;
# endif DEBUG

		  case 'f':	/* from address */
		  case 'r':	/* obsolete -f flag */
			p += 2;
			if (*p == '\0')
			{
				p = *++av;
				if (--ac <= 0 || *p == '-')
				{
					syserr("No \"from\" person");
					ac++;
					av--;
					break;
				}
			}
			if (from != NULL && pass <= 1)
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
				p = *++av;
				if (--ac <= 0 || *p == '-')
				{
					syserr("Bad -F flag");
					ac++;
					av--;
					break;
				}
			}
			FullName = p;
			break;

		  case 'h':	/* hop count */
			p += 2;
			if (*p == '\0')
			{
				p = *++av;
				if (--ac <= 0 || *p < '0' || *p > '9')
				{
					syserr("Bad hop count (%s)", p);
					ac++;
					av--;
					break;
				}
			}
			HopCount = atoi(p);
			break;
		
		  case 'n':	/* don't alias */
			NoAlias = TRUE;
			break;

		  case 'o':	/* set option */
			setoption(p[2], &p[3], FALSE, TRUE);
			break;

		  case 'q':	/* run queue files at intervals */
# ifdef QUEUE
			queuemode = TRUE;
			QueueIntvl = convtime(&p[2]);
# else QUEUE
			syserr("I don't know about queues");
# endif QUEUE
			break;

		  case 't':	/* read recipients from message */
			GrabTo = TRUE;
			break;

			/* compatibility flags */
		  case 'c':	/* connect to non-local mailers */
		  case 'e':	/* error message disposition */
		  case 'i':	/* don't let dot stop me */
		  case 'm':	/* send to me too */
		  case 'T':	/* set timeout interval */
		  case 'v':	/* give blow-by-blow description */
			setoption(p[1], &p[2], FALSE, TRUE);
			break;

		  case 's':	/* save From lines in headers */
			setoption('f', &p[2], FALSE, TRUE);
			break;

# ifdef DBM
		  case 'I':	/* initialize alias DBM file */
			OpMode = MD_INITALIAS;
			break;
# endif DBM
		}
	}

	/*
	**  Do basic initialization.
	**	Read system control file.
	**	Extract special fields for local use.
	*/

	if (pass <= 1)
	{
		if (!safecf || OpMode == MD_FREEZE || !thaw())
			readcf(ConfFile, safecf);
		else
			goto crackargs;
	}
	switch (OpMode)
	{
	  case MD_FREEZE:
		freeze();
		exit(EX_OK);

	  case MD_INITALIAS:
		Verbose = TRUE;
		break;

	  case MD_PRINT:
		usrerr("mailq mode not yet implemented");
		finis();
	}

	/* do heuristic mode adjustment */
	if (Verbose)
	{
		/* turn off noconnect option */
		setoption('c', "F", TRUE, FALSE);

		/* turn on interactive delivery */
		setoption('d', "", TRUE, FALSE);
	}

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

	/* operate in queue directory */
	if (chdir(QueueDir) < 0)
	{
		syserr("cannot chdir(%s)", QueueDir);
		exit(EX_SOFTWARE);
	}

	/*
	**  Initialize aliases.
	*/

	initaliases(AliasFile, OpMode == MD_INITALIAS);
# ifdef DBM
	if (OpMode == MD_INITALIAS)
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

	/*
	**  Switch to the main envelope.
	*/

	CurEnv = newenvelope(&MainEnvelope);
	MainEnvelope.e_oldstyle = BlankEnvelope.e_oldstyle;

	/*
	**  If test mode, read addresses from stdin and process.
	*/

	if (OpMode == MD_TEST)
	{
		char buf[MAXLINE];

		printf("ADDRESS TEST MODE\nEnter <ruleset> <address>\n");
		for (;;)
		{
			register char **pvp;
			char *q;
			extern char **prescan();
			extern char **rewrite();
			extern char *DelimChar;

			printf("> ");
			fflush(stdout);
			if (fgets(buf, sizeof buf, stdin) == NULL)
				finis();
			for (p = buf; isspace(*p); *p++)
				continue;
			q = p;
			while (*p != '\0' && !isspace(*p))
				p++;
			if (*p == '\0')
				continue;
			*p = '\0';
			do
			{
				pvp = prescan(++p, ',');
				if (pvp == NULL)
					continue;
				rewrite(pvp, 3);
				p = q;
				while (*p != '\0')
				{
					rewrite(pvp, atoi(p));
					while (*p != '\0' && *p++ != ',')
						continue;
				}
			} while (*(p = DelimChar) != '\0');
		}
	}

#ifdef DAEMON
	/*
	**  If a daemon, wait for a request.
	**	getrequests will always return in a child.
	**	If we should also be processing the queue, start
	**		doing it in background.
	**	We check for any errors that might have happened
	**		during startup.
	*/

	if (OpMode == MD_DAEMON || QueueIntvl != 0)
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
		{
			runqueue(TRUE);
			if (OpMode != MD_DAEMON)
				for (;;)
					pause();
		}
# endif QUEUE
		checkerrors(CurEnv);
		getrequests();

		/* at this point we are in a child: reset state */
		OpMode = MD_SMTP;
		dropenvelope(CurEnv);
		CurEnv->e_id = CurEnv->e_qf = CurEnv->e_df = NULL;
		FatalErrors = FALSE;
		openxscrpt();
	}
#endif DAEMON
# ifdef QUEUE
	/*
	**  If collecting stuff from the queue, go start doing that.
	*/

	if (queuemode && OpMode != MD_DAEMON)
	{
		runqueue(FALSE);
		finis();
	}
# endif QUEUE

	/* do basic system initialization */
	initsys();
	
# ifdef SMTP
	/*
	**  If running SMTP protocol, start collecting and executing
	**  commands.  This will never return.
	*/

	if (OpMode == MD_SMTP)
		smtp();
# endif SMTP

	/*
	**  Set the sender
	*/

	setsender(from);

	if (OpMode != MD_DAEMON && ac <= 0 && !GrabTo)
	{
		usrerr("Usage: /etc/sendmail [flags] addr...");
		finis();
	}
	if (OpMode == MD_VERIFY)
		SendMode = SM_VERIFY;

	/*
	**  Process Hop count.
	**	The Hop count tells us how many times this message has
	**	been processed by sendmail.  If it exceeds some
	**	fairly large threshold, then we assume that we have
	**	an infinite forwarding loop and die.
	*/

	if (++HopCount > MAXHOP)
		syserr("Infinite forwarding loop (%s->%s)", CurEnv->e_from.q_paddr, *av);

	/*
	**  Scan argv and deliver the message to everyone.
	**	Actually, suppress delivery if we are taking To:
	**	lines from the message.
	*/

	if (GrabTo)
		DontSend = TRUE;
	sendtoargv(av);

	/* if we have had errors sofar, arrange a meaningful exit stat */
	if (Errors > 0 && ExitStat == EX_OK)
		ExitStat = EX_USAGE;

	/*
	**  Read the input mail.
	*/

	DontSend = FALSE;
	CurEnv->e_to = NULL;
	if (OpMode != MD_VERIFY || GrabTo)
		collect(FALSE);
	errno = 0;

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

	sendall(CurEnv, SendMode);

	/*
	** All done.
	*/

	CurEnv->e_to = NULL;
	if (OpMode != MD_VERIFY)
		poststats(StatFile);
	finis();
}
/*
**  SETFROM -- set the person who this message is from
**
**	Under certain circumstances allow the user to say who
**	s/he is (using -f or -r).  These are:
**	1.  The user's uid is zero (root).
**	2.  The user's login name is in an approved list (typically
**	    from a network server).
**	3.  The address the user is trying to claim has a
**	    "!" character in it (since #2 doesn't do it for
**	    us if we are dialing out for UUCP).
**	A better check to replace #3 would be if the
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
		extern bool trusteduser();

		if (!trusteduser(realname) &&
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
	**  Rewrite the from person to dispose of possible implicit
	**	links in the net.
	*/

	pvp = prescan(from, '\0');
	if (pvp == NULL)
	{
		syserr("cannot prescan from (%s)", from);
		finis();
	}
	rewrite(pvp, 3);
	rewrite(pvp, 1);
	cataddr(pvp, frombuf, sizeof frombuf);
	define('f', newstr(frombuf));

	/* save the domain spec if this mailer wants it */
	if (bitset(M_CANONICAL, CurEnv->e_from.q_mailer->m_flags))
	{
		extern char **copyplist();

		while (*pvp != NULL && strcmp(*pvp, "@") != 0)
			pvp++;
		if (*pvp != NULL)
			CurEnv->e_fromdomain = copyplist(pvp, TRUE);
	}
}
/*
**  TRUSTEDUSER -- tell us if this user is to be trusted.
**
**	Parameters:
**		user -- the user to be checked.
**
**	Returns:
**		TRUE if the user is in an approved list.
**		FALSE otherwise.
**
**	Side Effects:
**		none.
*/

bool
trusteduser(user)
	char *user;
{
	register char **ulist;
	extern char *TrustedUsers[];

	for (ulist = TrustedUsers; *ulist != NULL; ulist++)
		if (strcmp(*ulist, user) == 0)
			return (TRUE);
	return (FALSE);
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
**  INTSIG -- clean up on interrupt
**
**	This just arranges to call finis.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Arranges to not unlink the qf and df files.
*/

intsig()
{
	CurEnv->e_df = CurEnv->e_qf = NULL;
	finis();
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
**		Open the transcript file.
*/

openxscrpt()
{
	register char *p;

	p = queuename(CurEnv, 'x');
	Xscript = fopen(p, "w");
	if (Xscript == NULL)
	{
		Xscript = stdout;
		syserr("Can't create %s", p);
	}
	(void) chmod(p, 0644);
	Transcript = newstr(p);
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

	if (OpMode != MD_SMTP && !QueueRun)
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
			if (OpMode != MD_SMTP && !QueueRun)
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
		FullName = macvalue('x', CurEnv);

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

	/*
	**  Give this envelope a reality.
	**	I.e., an id, a transcript, and a creation time.
	*/

	openxscrpt();
	CurEnv->e_ctime = curtime();

	/*
	**  Set OutChannel to something useful if stdout isn't it.
	**	This arranges that any extra stuff the mailer produces
	**	gets sent back to the user on error (because it is
	**	tucked away in the transcript).
	*/

	if (OpMode == MD_DAEMON && QueueRun)
		OutChannel = Xscript;

	/*
	**  Set up some basic system macros.
	*/

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
	if (macvalue('d', CurEnv) == NULL)
		define('d', dbuf);
	p = newstr(arpadate(dbuf));
	if (macvalue('a', CurEnv) == NULL)
		define('a', p);
	define('b', p);

	/* version */
	define('v', Version);

	/* tty name */
	if (macvalue('y', CurEnv) == NULL)
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
	register HDR *bh;
	register HDR **nhp;

	clear((char *) e, sizeof *e);
	bmove(&CurEnv->e_from, &e->e_from, sizeof e->e_from);
	e->e_parent = CurEnv;
	e->e_ctime = curtime();
	e->e_puthdr = CurEnv->e_puthdr;
	e->e_putbody = CurEnv->e_putbody;
	bh = BlankEnvelope.e_header;
	nhp = &e->e_header;
	while (bh != NULL)
	{
		*nhp = (HDR *) xalloc(sizeof *bh);
		bmove((char *) bh, (char *) *nhp, sizeof *bh);
		bh = bh->h_link;
		nhp = &(*nhp)->h_link;
	}

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
**	This code is very careful to avoid trashing existing files
**	under any circumstances.
**		We first create an nf file that is only used when
**		assigning an id.  This file is always empty, so that
**		we can never accidently truncate an lf file.
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
		char counter = 'A' - 1;
		char qf[20];
		char lf[20];
		char nf[20];

		/* find a unique id */
		(void) sprintf(qf, "qf_%05d", getpid());
		strcpy(lf, qf);
		lf[0] = 'l';
		strcpy(nf, qf);
		nf[0] = 'n';

		while (counter < '~')
		{
			int i;

			qf[2] = lf[2] = nf[2] = ++counter;
# ifdef DEBUG
			if (tTd(7, 20))
				printf("queuename: trying \"%s\"\n", nf);
# endif DEBUG
			if (access(lf, 0) >= 0 || access(qf, 0) >= 0)
				continue;
			errno = 0;
			i = creat(nf, FileMode);
			if (i < 0)
			{
				(void) unlink(nf);	/* kernel bug */
				continue;
			}
			(void) close(i);
			i = link(nf, lf);
			(void) unlink(nf);
			if (i < 0)
				continue;
			if (link(lf, qf) >= 0)
				break;
			(void) unlink(lf);
		}
		if (counter >= '~')
		{
			syserr("queuename: Cannot create \"%s\" in \"%s\"",
				lf, QueueDir);
			exit(EX_OSERR);
		}
		e->e_qf = newstr(qf);
		e->e_id = &e->e_qf[2];
		define('i', e->e_id);
# ifdef DEBUG
		if (tTd(7, 1))
			printf("queuename: assigned id %s, env=%x\n", e->e_id, e);
# endif DEBUG
	}

	if (type == '\0')
		return (NULL);
	(void) sprintf(buf, "%cf%s", type, e->e_id);
# ifdef DEBUG
	if (tTd(7, 2))
		printf("queuename: %s\n", buf);
# endif DEBUG
	return (buf);
}
/*
**  FREEZE -- freeze BSS & allocated memory
**
**	This will be used to efficiently load the configuration file.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Writes BSS and malloc'ed memory to FreezeFile
*/

struct frz
{
	time_t	frzstamp;		/* timestamp on this freeze */
	char	*frzbrk;		/* the current break */
	char	frzver[252];		/* sendmail version */
};

freeze()
{
	int f;
	struct frz fhdr;
	extern char edata;
	extern char *sbrk();

	if (FreezeFile == NULL)
		return;

	/* try to open the freeze file */
	f = open(FreezeFile, 1);
	if (f < 0)
	{
		syserr("Cannot freeze");
		errno = 0;
		return;
	}

	/* build the freeze header */
	fhdr.frzstamp = curtime();
	fhdr.frzbrk = sbrk(0);
	strcpy(fhdr.frzver, Version);

	/* write out the freeze header */
	if (write(f, &fhdr, sizeof fhdr) != sizeof fhdr ||
	    write(f, &edata, fhdr.frzbrk - &edata) != (fhdr.frzbrk - &edata))
		syserr("Cannot freeze");

	/* fine, clean up */
	(void) close(f);
}
/*
**  THAW -- read in the frozen configuration file.
**
**	Parameters:
**		none.
**
**	Returns:
**		TRUE if it successfully read the freeze file.
**		FALSE otherwise.
**
**	Side Effects:
**		reads FreezeFile in to BSS area.
*/

thaw()
{
	int f;
	struct frz fhdr;
	extern char edata;

	if (FreezeFile == NULL)
		return (FALSE);

	/* open the freeze file */
	f = open(FreezeFile, 0);
	if (f < 0)
	{
		errno = 0;
		return (FALSE);
	}

	/* read in the header */
	if (read(f, &fhdr, sizeof fhdr) < sizeof fhdr ||
	    strcmp(fhdr.frzver, Version) != 0)
	{
		(void) close(f);
		return (FALSE);
	}

	/* arrange to have enough space */
	if (brk(fhdr.frzbrk) < 0)
	{
		syserr("Cannot break to %x", fhdr.frzbrk);
		(void) close(f);
		return (FALSE);
	}

	/* now read in the freeze file */
	if (read(f, &edata, fhdr.frzbrk - &edata) != (fhdr.frzbrk - &edata))
	{
		/* oops!  we have trashed memory..... */
		fprintf(stderr, "Cannot read freeze file\n");
		exit(EX_SOFTWARE);
	}

	(void) close(f);
	return (TRUE);
}
