# include <signal.h>
# include <errno.h>
# include "sendmail.h"
# include <sys/stat.h>

SCCSID(@(#)deliver.c	3.116		%G%);

/*
**  DELIVER -- Deliver a message to a list of addresses.
**
**	This routine delivers to everyone on the same host as the
**	user on the head of the list.  It is clever about mailers
**	that don't handle multiple users.  It is NOT guaranteed
**	that it will deliver to all these addresses however -- so
**	deliver should be called once for each address on the
**	list.
**
**	Parameters:
**		firstto -- head of the address list to deliver to.
**
**	Returns:
**		zero -- successfully delivered.
**		else -- some failure, see ExitStat for more info.
**
**	Side Effects:
**		The standard input is passed off to someone.
*/

deliver(firstto)
	ADDRESS *firstto;
{
	char *host;			/* host being sent to */
	char *user;			/* user being sent to */
	char **pvp;
	register char **mvp;
	register char *p;
	register struct mailer *m;	/* mailer for this recipient */
	extern bool checkcompat();
	char *pv[MAXPV+1];
	char tobuf[MAXLINE-50];		/* text line of to people */
	char buf[MAXNAME];
	ADDRESS *ctladdr;
	extern ADDRESS *getctladdr();
	char tfrombuf[MAXNAME];		/* translated from person */
	extern char **prescan();
	register ADDRESS *to = firstto;
	bool clever = FALSE;		/* running user smtp to this mailer */
	ADDRESS *tochain = NULL;	/* chain of users in this mailer call */
	bool notopen = TRUE;		/* set if connection not quite open */
	register int rcode;		/* response code */

	errno = 0;
	if (!ForceMail && bitset(QDONTSEND|QPSEUDO, to->q_flags))
		return (0);

	m = to->q_mailer;
	host = to->q_host;

# ifdef DEBUG
	if (tTd(10, 1))
		printf("\n--deliver, mailer=%d, host=`%s', first user=`%s'\n",
			m->m_mno, host, to->q_user);
# endif DEBUG

	/*
	**  If this mailer is expensive, and if we don't want to make
	**  connections now, just mark these addresses and return.
	**	This is useful if we want to batch connections to
	**	reduce load.  This will cause the messages to be
	**	queued up, and a daemon will come along to send the
	**	messages later.
	**		This should be on a per-mailer basis.
	*/

	if (NoConnect && !QueueRun && bitset(M_EXPENSIVE, m->m_flags))
	{
		for (; to != NULL; to = to->q_next)
			if (!bitset(QDONTSEND, to->q_flags) &&
			    to->q_mailer == firstto->q_mailer)
				to->q_flags |= QQUEUEUP|QDONTSEND;
		return (0);
	}

	/*
	**  Do initial argv setup.
	**	Insert the mailer name.  Notice that $x expansion is
	**	NOT done on the mailer name.  Then, if the mailer has
	**	a picky -f flag, we insert it as appropriate.  This
	**	code does not check for 'pv' overflow; this places a
	**	manifest lower limit of 4 for MAXPV.
	**		The from address rewrite is expected to make
	**		the address relative to the other end.
	*/

	/* rewrite from address, using rewriting rules */
	expand("$f", buf, &buf[sizeof buf - 1], CurEnv);
	mvp = prescan(buf, '\0');
	rewrite(mvp, m->m_s_rwset);
	cataddr(mvp, tfrombuf, sizeof tfrombuf);

	define('g', tfrombuf);		/* translated sender address */
	define('h', host);		/* to host */
	Errors = 0;
	pvp = pv;
	*pvp++ = m->m_argv[0];

	/* insert -f or -r flag as appropriate */
	if (bitset(M_FOPT|M_ROPT, m->m_flags) && FromFlag)
	{
		if (bitset(M_FOPT, m->m_flags))
			*pvp++ = "-f";
		else
			*pvp++ = "-r";
		expand("$g", buf, &buf[sizeof buf - 1], CurEnv);
		*pvp++ = newstr(buf);
	}

	/*
	**  Append the other fixed parts of the argv.  These run
	**  up to the first entry containing "$u".  There can only
	**  be one of these, and there are only a few more slots
	**  in the pv after it.
	*/

	for (mvp = m->m_argv; (p = *++mvp) != NULL; )
	{
		while ((p = index(p, '$')) != NULL)
			if (*++p == 'u')
				break;
		if (p != NULL)
			break;

		/* this entry is safe -- go ahead and process it */
		expand(*mvp, buf, &buf[sizeof buf - 1], CurEnv);
		*pvp++ = newstr(buf);
		if (pvp >= &pv[MAXPV - 3])
		{
			syserr("Too many parameters to %s before $u", pv[0]);
			return (-1);
		}
	}

	/*
	**  If we have no substitution for the user name in the argument
	**  list, we know that we must supply the names otherwise -- and
	**  SMTP is the answer!!
	*/

	if (*mvp == NULL)
	{
		/* running SMTP */
# ifdef SMTP
		clever = TRUE;
		*pvp = NULL;
# else SMTP
		/* oops!  we don't implement SMTP */
		syserr("SMTP style mailer");
		return (EX_SOFTWARE);
# endif SMTP
	}

	/*
	**  At this point *mvp points to the argument with $u.  We
	**  run through our address list and append all the addresses
	**  we can.  If we run out of space, do not fret!  We can
	**  always send another copy later.
	*/

	tobuf[0] = '\0';
	CurEnv->e_to = tobuf;
	ctladdr = NULL;
	for (; to != NULL; to = to->q_next)
	{
		/* avoid sending multiple recipients to dumb mailers */
		if (tobuf[0] != '\0' && !bitset(M_MUSER, m->m_flags))
			break;

		/* if already sent or not for this host, don't send */
		if (bitset(QDONTSEND, to->q_flags) ||
		    strcmp(to->q_host, host) != 0 ||
		    to->q_mailer != firstto->q_mailer)
			continue;

		/* avoid overflowing tobuf */
		if (sizeof tobuf - (strlen(to->q_paddr) + strlen(tobuf) + 1) < 0)
			break;

# ifdef DEBUG
		if (tTd(10, 1))
		{
			printf("\nsend to ");
			printaddr(to, FALSE);
		}
# endif DEBUG

		/* compute effective uid/gid when sending */
		if (to->q_mailer == ProgMailer)
			ctladdr = getctladdr(to);

		user = to->q_user;
		CurEnv->e_to = to->q_paddr;
		to->q_flags |= QDONTSEND;

		/*
		**  Check to see that these people are allowed to
		**  talk to each other.
		*/

		if (!checkcompat(to))
		{
			giveresponse(EX_UNAVAILABLE, TRUE, m);
			continue;
		}

		/*
		**  Strip quote bits from names if the mailer is dumb
		**	about them.
		*/

		if (bitset(M_STRIPQ, m->m_flags))
		{
			stripquotes(user, TRUE);
			stripquotes(host, TRUE);
		}
		else
		{
			stripquotes(user, FALSE);
			stripquotes(host, FALSE);
		}

		/*
		**  Do initial connection setup if needed.
		*/

		if (notopen)
		{
			message(Arpa_Info, "Connecting to %s.%s...", host, m->m_name);
# ifdef SMTP
			if (clever)
			{
				/* send the initial SMTP protocol */
				rcode = smtpinit(m, pv, (ADDRESS *) NULL);
			}
# ifdef SMTP
			notopen = FALSE;
		}

		/*
		**  Pass it to the other host if we are running SMTP.
		*/

		if (clever)
		{
# ifdef SMTP
			if (rcode == EX_OK)
				rcode = smtprcpt(to);
			if (rcode != EX_OK)
			{
				if (rcode == EX_TEMPFAIL)
					to->q_flags |= QQUEUEUP;
				else
					to->q_flags |= QBADADDR;
				giveresponse(rcode, TRUE, m);
			}
# else SMTP
			syserr("trying to be clever");
# endif SMTP
		}

		/*
		**  If an error message has already been given, don't
		**	bother to send to this address.
		**
		**	>>>>>>>>>> This clause assumes that the local mailer
		**	>> NOTE >> cannot do any further aliasing; that
		**	>>>>>>>>>> function is subsumed by sendmail.
		*/

		if (bitset(QBADADDR|QQUEUEUP, to->q_flags))
			continue;

		/* save statistics.... */
		Stat.stat_nt[to->q_mailer->m_mno]++;
		Stat.stat_bt[to->q_mailer->m_mno] += kbytes(CurEnv->e_msgsize);

		/*
		**  See if this user name is "special".
		**	If the user name has a slash in it, assume that this
		**	is a file -- send it off without further ado.  Note
		**	that this type of addresses is not processed along
		**	with the others, so we fudge on the To person.
		*/

		if (m == LocalMailer)
		{
			if (user[0] == '/')
			{
				rcode = mailfile(user, getctladdr(to));
				giveresponse(rcode, TRUE, m);
				continue;
			}
		}

		/*
		**  Address is verified -- add this user to mailer
		**  argv, and add it to the print list of recipients.
		*/

		/* link together the chain of recipients */
		to->q_tchain = tochain;
		tochain = to;

		/* create list of users for error messages */
		if (tobuf[0] != '\0')
			(void) strcat(tobuf, ",");
		(void) strcat(tobuf, to->q_paddr);
		define('u', user);		/* to user */
		define('z', to->q_home);	/* user's home */

		/*
		**  Expand out this user into argument list.
		*/

		if (!clever)
		{
			expand(*mvp, buf, &buf[sizeof buf - 1], CurEnv);
			*pvp++ = newstr(buf);
			if (pvp >= &pv[MAXPV - 2])
			{
				/* allow some space for trailing parms */
				break;
			}
		}
	}

	/* see if any addresses still exist */
	if (tobuf[0] == '\0')
	{
# ifdef SMTP
		if (clever)
			smtpquit(pv[0], FALSE);
# endif SMTP
		define('g', (char *) NULL);
		return (0);
	}

	/* print out messages as full list */
	CurEnv->e_to = tobuf;

	/*
	**  Fill out any parameters after the $u parameter.
	*/

	while (!clever && *++mvp != NULL)
	{
		expand(*mvp, buf, &buf[sizeof buf - 1], CurEnv);
		*pvp++ = newstr(buf);
		if (pvp >= &pv[MAXPV])
			syserr("deliver: pv overflow after $u for %s", pv[0]);
	}
	*pvp++ = NULL;

	/*
	**  Call the mailer.
	**	The argument vector gets built, pipes
	**	are created as necessary, and we fork & exec as
	**	appropriate.
	**	If we are running SMTP, we just need to clean up.
	*/

	if (ctladdr == NULL)
		ctladdr = &CurEnv->e_from;
# ifdef SMTP
	if (clever)
	{
		rcode = smtpfinish(m, CurEnv);
		if (rcode != EX_OK)
			giveresponse(rcode, TRUE, m);
		smtpquit(pv[0], rcode == EX_OK);
	}
	else
# endif SMTP
		rcode = sendoff(m, pv, ctladdr);

	/*
	**  If we got a temporary failure, arrange to queue the
	**  addressees.
	*/

	if (rcode == EX_TEMPFAIL)
	{
		for (to = tochain; to != NULL; to = to->q_tchain)
			to->q_flags |= QQUEUEUP;
	}

	errno = 0;
	define('g', (char *) NULL);
	return (rcode);
}
/*
**  DOFORK -- do a fork, retrying a couple of times on failure.
**
**	This MUST be a macro, since after a vfork we are running
**	two processes on the same stack!!!
**
**	Parameters:
**		none.
**
**	Returns:
**		From a macro???  You've got to be kidding!
**
**	Side Effects:
**		Modifies the ==> LOCAL <== variable 'pid', leaving:
**			pid of child in parent, zero in child.
**			-1 on unrecoverable error.
**
**	Notes:
**		I'm awfully sorry this looks so awful.  That's
**		vfork for you.....
*/

# define NFORKTRIES	5
# ifdef VFORK
# define XFORK	vfork
# else VFORK
# define XFORK	fork
# endif VFORK

# define DOFORK(fORKfN) \
{\
	register int i;\
\
	for (i = NFORKTRIES; i-- > 0; )\
	{\
		pid = fORKfN();\
		if (pid >= 0)\
			break;\
		sleep(NFORKTRIES - i);\
	}\
}
/*
**  DOFORK -- simple fork interface to DOFORK.
**
**	Parameters:
**		none.
**
**	Returns:
**		pid of child in parent.
**		zero in child.
**		-1 on error.
**
**	Side Effects:
**		returns twice, once in parent and once in child.
*/

dofork()
{
	register int pid;

	DOFORK(fork);
	return (pid);
}
/*
**  SENDOFF -- send off call to mailer & collect response.
**
**	Parameters:
**		m -- mailer descriptor.
**		pvp -- parameter vector to send to it.
**		ctladdr -- an address pointer controlling the
**			user/groupid etc. of the mailer.
**
**	Returns:
**		exit status of mailer.
**
**	Side Effects:
**		none.
*/

sendoff(m, pvp, ctladdr)
	struct mailer *m;
	char **pvp;
	ADDRESS *ctladdr;
{
	auto FILE *mfile;
	auto FILE *rfile;
	register int i;
	int pid;

	/*
	**  Create connection to mailer.
	*/

	pid = openmailer(m, pvp, ctladdr, FALSE, &mfile, &rfile);
	if (pid < 0)
		return (-1);

	/*
	**  Format and send message.
	*/

	(void) signal(SIGPIPE, SIG_IGN);
	putfromline(mfile, m);
	(*CurEnv->e_puthdr)(mfile, m, CurEnv);
	fprintf(mfile, "\n");
	(*CurEnv->e_putbody)(mfile, m, FALSE);
	(void) fclose(mfile);

	i = endmailer(pid, pvp[0]);
	giveresponse(i, TRUE, m);

	/* arrange a return receipt if requested */
	if (CurEnv->e_receiptto != NULL && bitset(M_LOCAL, m->m_flags))
	{
		CurEnv->e_sendreceipt = TRUE;
		if (ExitStat == EX_OK)
			fprintf(Xscript, "%s... successfully delivered\n",
				CurEnv->e_to);
		/* do we want to send back more info? */
	}

	return (i);
}
/*
**  ENDMAILER -- Wait for mailer to terminate.
**
**	We should never get fatal errors (e.g., segmentation
**	violation), so we report those specially.  For other
**	errors, we choose a status message (into statmsg),
**	and if it represents an error, we print it.
**
**	Parameters:
**		pid -- pid of mailer.
**		name -- name of mailer (for error messages).
**
**	Returns:
**		exit code of mailer.
**
**	Side Effects:
**		none.
*/

endmailer(pid, name)
	int pid;
	char *name;
{
	register int i;
	auto int st;

	/* in the IPC case there is nothing to wait for */
	if (pid == 0)
		return (EX_OK);

	/* wait for the mailer process to die and collect status */
	do
	{
		errno = 0;
		i = wait(&st);
	} while ((i >= 0 && i != pid) || errno == EINTR);
	if (i < 0)
	{
		syserr("wait");
		return (-1);
	}

	/* see if it died a horrid death */
	if ((st & 0377) != 0)
	{
		syserr("%s: stat %o", name, st);
		ExitStat = EX_UNAVAILABLE;
		return (-1);
	}

	/* normal death -- return status */
	i = (st >> 8) & 0377;
	return (i);
}
/*
**  OPENMAILER -- open connection to mailer.
**
**	Parameters:
**		m -- mailer descriptor.
**		pvp -- parameter vector to pass to mailer.
**		ctladdr -- controlling address for user.
**		clever -- create a full duplex connection.
**		pmfile -- pointer to mfile (to mailer) connection.
**		prfile -- pointer to rfile (from mailer) connection.
**
**	Returns:
**		pid of mailer ( > 0 ).
**		-1 on error.
**		zero on an IPC connection.
**
**	Side Effects:
**		creates a mailer in a subprocess.
*/

openmailer(m, pvp, ctladdr, clever, pmfile, prfile)
	struct mailer *m;
	char **pvp;
	ADDRESS *ctladdr;
	bool clever;
	FILE **pmfile;
	FILE **prfile;
{
	int pid;
	int mpvect[2];
	int rpvect[2];
	FILE *mfile;
	FILE *rfile;
	extern FILE *fdopen();

# ifdef DEBUG
	if (tTd(11, 1))
	{
		printf("openmailer:");
		printav(pvp);
	}
# endif DEBUG
	errno = 0;

# ifdef DAEMON
	/*
	**  Deal with the special case of mail handled through an IPC
	**  connection.
	**	In this case we don't actually fork.  We must be
	**	running SMTP for this to work.  We will return a
	**	zero pid to indicate that we are running IPC.
	*/

	if (strcmp(m->m_mailer, "[IPC]") == 0)
	{
		register int i;
		register u_short port;

		if (!clever)
			syserr("non-clever IPC");
		if (pvp[2] != NULL)
			port = atoi(pvp[2]);
		else
			port = 0;
		i = makeconnection(pvp[1], port, pmfile, prfile);
		if (i != EX_OK)
		{
			ExitStat = i;
			return (-1);
		}
		else
			return (0);
	}
# endif DAEMON

	/* create a pipe to shove the mail through */
	if (pipe(mpvect) < 0)
	{
		syserr("pipe (to mailer)");
		return (-1);
	}

# ifdef SMTP
	/* if this mailer speaks smtp, create a return pipe */
	if (clever && pipe(rpvect) < 0)
	{
		syserr("pipe (from mailer)");
		(void) close(mpvect[0]);
		(void) close(mpvect[1]);
		return (-1);
	}
# endif SMTP

	/*
	**  Actually fork the mailer process.
	**	DOFORK is clever about retrying.
	*/

	(void) fflush(Xscript);				/* for debugging */
	DOFORK(XFORK);
	/* pid is set by DOFORK */
	if (pid < 0)
	{
		/* failure */
		syserr("Cannot fork");
		(void) close(mpvect[0]);
		(void) close(mpvect[1]);
		if (clever)
		{
			(void) close(rpvect[0]);
			(void) close(rpvect[1]);
		}
		return (-1);
	}
	else if (pid == 0)
	{
		/* child -- set up input & exec mailer */
		/* make diagnostic output be standard output */
		(void) signal(SIGINT, SIG_IGN);
		(void) signal(SIGHUP, SIG_IGN);
		(void) signal(SIGTERM, SIG_DFL);

		/* arrange to filter standard & diag output of command */
		if (clever)
		{
			(void) close(rpvect[0]);
			(void) close(1);
			(void) dup(rpvect[1]);
			(void) close(rpvect[1]);
		}
		else if (OutChannel != stdout)
		{
			(void) close(1);
			(void) dup(fileno(OutChannel));
		}
		(void) close(2);
		(void) dup(1);

		/* arrange to get standard input */
		(void) close(mpvect[1]);
		(void) close(0);
		if (dup(mpvect[0]) < 0)
		{
			syserr("Cannot dup to zero!");
			_exit(EX_OSERR);
		}
		(void) close(mpvect[0]);
		if (!bitset(M_RESTR, m->m_flags))
		{
			if (ctladdr->q_uid == 0)
			{
				(void) setgid(DefGid);
				(void) setuid(DefUid);
			}
			else
			{
				(void) setgid(ctladdr->q_gid);
				(void) setuid(ctladdr->q_uid);
			}
		}
# ifndef VFORK
		/*
		**  We have to be careful with vfork - we can't mung up the
		**  memory but we don't want the mailer to inherit any extra
		**  open files.  Chances are the mailer won't
		**  care about an extra file, but then again you never know.
		**  Actually, we would like to close(fileno(pwf)), but it's
		**  declared static so we can't.  But if we fclose(pwf), which
		**  is what endpwent does, it closes it in the parent too and
		**  the next getpwnam will be slower.  If you have a weird
		**  mailer that chokes on the extra file you should do the
		**  endpwent().
		**
		**  Similar comments apply to log.  However, openlog is
		**  clever enough to set the FIOCLEX mode on the file,
		**  so it will be closed automatically on the exec.
		*/

		endpwent();
# ifdef LOG
		closelog();
# endif LOG
# endif VFORK

		/* try to execute the mailer */
		execv(m->m_mailer, pvp);

		/* syserr fails because log is closed */
		/* syserr("Cannot exec %s", m->m_mailer); */
		printf("Cannot exec '%s' errno=%d\n", m->m_mailer, errno);
		(void) fflush(stdout);
		_exit(EX_UNAVAILABLE);
	}

	/*
	**  Set up return value.
	*/

	(void) close(mpvect[0]);
	mfile = fdopen(mpvect[1], "w");
	if (clever)
	{
		(void) close(rpvect[1]);
		rfile = fdopen(rpvect[0], "r");
	}

	*pmfile = mfile;
	*prfile = rfile;

	return (pid);
}
/*
**  GIVERESPONSE -- Interpret an error response from a mailer
**
**	Parameters:
**		stat -- the status code from the mailer (high byte
**			only; core dumps must have been taken care of
**			already).
**		force -- if set, force an error message output, even
**			if the mailer seems to like to print its own
**			messages.
**		m -- the mailer descriptor for this mailer.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Errors may be incremented.
**		ExitStat may be set.
*/

giveresponse(stat, force, m)
	int stat;
	bool force;
	register struct mailer *m;
{
	register char *statmsg;
	extern char *SysExMsg[];
	register int i;
	extern int N_SysEx;
	char buf[30];

	/*
	**  Compute status message from code.
	*/

	i = stat - EX__BASE;
	if (i < 0 || i > N_SysEx)
		statmsg = NULL;
	else
		statmsg = SysExMsg[i];
	if (stat == 0)
	{
		statmsg = "250 sent";
		message(Arpa_Info, &statmsg[4]);
	}
	else if (stat == EX_TEMPFAIL)
	{
		message(Arpa_Info, "deferred");
	}
	else
	{
		Errors++;
		FatalErrors = TRUE;
		if (statmsg == NULL && m->m_badstat != 0)
		{
			stat = m->m_badstat;
			i = stat - EX__BASE;
# ifdef DEBUG
			if (i < 0 || i >= N_SysEx)
				syserr("Bad m_badstat %d", stat);
			else
# endif DEBUG
				statmsg = SysExMsg[i];
		}
		if (statmsg == NULL)
			usrerr("unknown mailer response %d", stat);
		else if (force || !bitset(M_QUIET, m->m_flags) || Verbose)
			usrerr(statmsg);
		else
			fprintf(Xscript, "%s\n", &statmsg[4]);
	}

	/*
	**  Final cleanup.
	**	Log a record of the transaction.  Compute the new
	**	ExitStat -- if we already had an error, stick with
	**	that.
	*/

	if (statmsg == NULL)
	{
		(void) sprintf(buf, "554 error %d", stat);
		statmsg = buf;
	}

# ifdef LOG
	if (LogLevel > ((stat == 0 || stat == EX_TEMPFAIL) ? 3 : 2))
	{
		extern char *pintvl();

		syslog(LOG_INFO, "%s: to=%s, delay=%s, stat=%s", CurEnv->e_id,
		       CurEnv->e_to, pintvl(curtime() - CurEnv->e_ctime, TRUE),
		       &statmsg[4]);
	}
# endif LOG
	if (stat != EX_TEMPFAIL)
		setstat(stat);
}
/*
**  PUTFROMLINE -- output a UNIX-style from line (or whatever)
**
**	then passes the rest of the message through.  If we have
**	managed to extract a date already, use that; otherwise,
**	use the current date/time.
**
**	One of the ugliest hacks seen by human eyes is
**	contained herein: UUCP wants those stupid
**	"remote from <host>" lines.  Why oh why does a
**	well-meaning programmer such as myself have to
**	deal with this kind of antique garbage????
**
**	Parameters:
**		fp -- the file to output to.
**		m -- the mailer describing this entry.
**
**	Returns:
**		none
**
**	Side Effects:
**		outputs some text to fp.
*/

putfromline(fp, m)
	register FILE *fp;
	register MAILER *m;
{
	char buf[MAXLINE];
	extern char SentDate[];

	if (bitset(M_NHDR, m->m_flags))
		return;

# ifdef UGLYUUCP
	if (bitset(M_UGLYUUCP, m->m_flags))
	{
		extern char *macvalue();
		char *sys = macvalue('g', CurEnv);
		char *bang = index(sys, '!');

		if (bang == NULL)
			syserr("No ! in UUCP! (%s)", sys);
		else
			*bang = '\0';
		expand("From $f  $d remote from $g\n", buf,
				&buf[sizeof buf - 1], CurEnv);
		*bang = '!';
	}
	else
# endif UGLYUUCP
		expand("$l\n", buf, &buf[sizeof buf - 1], CurEnv);
	putline(buf, fp, bitset(M_FULLSMTP, m->m_flags));
}
/*
**  PUTHEADER -- put the header part of a message from the in-core copy
**
**	Parameters:
**		fp -- file to put it on.
**		m -- mailer to use.
**		e -- envelope to use.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

putheader(fp, m, e)
	register FILE *fp;
	register struct mailer *m;
	register ENVELOPE *e;
{
	char buf[BUFSIZ];
	register HDR *h;
	extern char *arpadate();
	extern char *capitalize();
	extern bool samefrom();
	char obuf[MAXLINE];
	register char *obp;
	bool fullsmtp = bitset(M_FULLSMTP, m->m_flags);

	for (h = e->e_header; h != NULL; h = h->h_link)
	{
		register char *p;

		if (bitset(H_CHECK|H_ACHECK, h->h_flags) && !bitset(h->h_mflags, m->m_flags))
			continue;

		p = h->h_value;
		if (bitset(H_DEFAULT, h->h_flags))
		{
			/* macro expand value if generated internally */
			expand(p, buf, &buf[sizeof buf], e);
			p = buf;
		}
		if (p == NULL || *p == '\0')
			continue;

		if (bitset(H_FROM|H_RCPT, h->h_flags))
		{
			/* address field */
			bool oldstyle = e->e_oldstyle;

			if (bitset(H_FROM, h->h_flags))
				oldstyle = FALSE;
			commaize(h, p, fp, oldstyle, m);
		}
		else
		{
			/* vanilla header line */
			(void) sprintf(obuf, "%s: %s\n", capitalize(h->h_field), p);
			putline(obuf, fp, fullsmtp);
		}
		h->h_flags |= H_USED;
	}
}
/*
**  COMMAIZE -- output a header field, making a comma-translated list.
**
**	Parameters:
**		h -- the header field to output.
**		p -- the value to put in it.
**		fp -- file to put it to.
**		oldstyle -- TRUE if this is an old style header.
**		m -- a pointer to the mailer descriptor.  If NULL,
**			don't transform the name at all.
**
**	Returns:
**		none.
**
**	Side Effects:
**		outputs "p" to file "fp".
*/

commaize(h, p, fp, oldstyle, m)
	register HDR *h;
	register char *p;
	FILE *fp;
	bool oldstyle;
	register MAILER *m;
{
	register char *obp;
	int opos;
	bool fullsmtp = FALSE;
	bool firstone = TRUE;
	char obuf[MAXLINE];

	/*
	**  Output the address list translated by the
	**  mailer and with commas.
	*/

# ifdef DEBUG
	if (tTd(14, 2))
		printf("commaize(%s: %s)\n", h->h_field, p);
# endif DEBUG

	if (m != NULL && bitset(M_FULLSMTP, m->m_flags))
		fullsmtp = TRUE;

	obp = obuf;
	(void) sprintf(obp, "%s: ", capitalize(h->h_field));
	opos = strlen(h->h_field) + 2;
	obp += opos;

	/*
	**  Run through the list of values.
	*/

	while (*p != '\0')
	{
		register char *name;
		char savechar;
		extern char *remotename();
		extern char *DelimChar;		/* defined in prescan */

		/*
		**  Find the end of the name.  New style names
		**  end with a comma, old style names end with
		**  a space character.  However, spaces do not
		**  necessarily delimit an old-style name -- at
		**  signs mean keep going.
		*/

		/* find end of name */
		while (isspace(*p) || *p == ',')
			p++;
		name = p;
		for (;;)
		{
			char *oldp;
			extern bool isatword();

			(void) prescan(p, oldstyle ? ' ' : ',');
			p = DelimChar;

			/* look to see if we have an at sign */
			oldp = p;
			while (*p != '\0' && isspace(*p))
				p++;

			if (*p != '@' && !isatword(p))
			{
				p = oldp;
				break;
			}
			p += *p == '@' ? 1 : 2;
			while (*p != '\0' && isspace(*p))
				p++;
		}
		/* at the end of one complete name */

		/* strip off trailing white space */
		while (p >= name && (isspace(*p) || *p == ',' || *p == '\0'))
			p--;
		if (++p == name)
			continue;
		savechar = *p;
		*p = '\0';

		/* translate the name to be relative */
		if (m != NULL)
			name = remotename(name, m, bitset(H_FROM, h->h_flags));
		if (*name == '\0')
		{
			*p = savechar;
			continue;
		}

		/* output the name with nice formatting */
		opos += strlen(name);
		if (!firstone)
			opos += 2;
		if (opos > 78 && !firstone)
		{
			(void) sprintf(obp, ",\n");
			putline(obuf, fp, fullsmtp);
			obp = obuf;
			(void) sprintf(obp, "        ");
			obp += strlen(obp);
			opos = 8 + strlen(name);
		}
		else if (!firstone)
		{
			(void) sprintf(obp, ", ");
			obp += 2;
		}
		(void) sprintf(obp, "%s", name);
		obp += strlen(obp);
		firstone = FALSE;
		*p = savechar;
	}
	(void) strcpy(obp, "\n");
	putline(obuf, fp, fullsmtp);
}
/*
**  PUTBODY -- put the body of a message.
**
**	Parameters:
**		fp -- file to output onto.
**		m -- a mailer descriptor.
**		xdot -- if set, use SMTP hidden dot algorithm.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The message is written onto fp.
*/

putbody(fp, m, xdot)
	FILE *fp;
	struct mailer *m;
	bool xdot;
{
	char buf[MAXLINE + 1];
	bool fullsmtp = bitset(M_FULLSMTP, m->m_flags);

	/*
	**  Output the body of the message
	*/

#ifdef lint
	/* m will be needed later for complete smtp emulation */
	if (m == NULL)
		return;
#endif lint

	if (TempFile != NULL)
	{
		rewind(TempFile);
		buf[0] = '.';
		while (!ferror(fp) && fgets(&buf[1], sizeof buf - 1, TempFile) != NULL)
			putline((xdot && buf[1] == '.') ? buf : &buf[1], fp, fullsmtp);

		if (ferror(TempFile))
		{
			syserr("putbody: read error");
			ExitStat = EX_IOERR;
		}
	}

	(void) fflush(fp);
	if (ferror(fp) && errno != EPIPE)
	{
		syserr("putbody: write error");
		ExitStat = EX_IOERR;
	}
	errno = 0;
}
/*
**  ISATWORD -- tell if the word we are pointing to is "at".
**
**	Parameters:
**		p -- word to check.
**
**	Returns:
**		TRUE -- if p is the word at.
**		FALSE -- otherwise.
**
**	Side Effects:
**		none.
*/

bool
isatword(p)
	register char *p;
{
	extern char lower();

	if (lower(p[0]) == 'a' && lower(p[1]) == 't' &&
	    p[2] != '\0' && isspace(p[2]))
		return (TRUE);
	return (FALSE);
}
/*
**  SAMEFROM -- tell if two text addresses represent the same from address.
**
**	Parameters:
**		ifrom -- internally generated form of from address.
**		efrom -- external form of from address.
**
**	Returns:
**		TRUE -- if they convey the same info.
**		FALSE -- if any information has been lost.
**
**	Side Effects:
**		none.
*/

bool
samefrom(ifrom, efrom)
	char *ifrom;
	char *efrom;
{
	register char *p;
	char buf[MAXNAME + 4];

# ifdef DEBUG
	if (tTd(3, 8))
		printf("samefrom(%s,%s)-->", ifrom, efrom);
# endif DEBUG
	if (strcmp(ifrom, efrom) == 0)
		goto success;
	p = index(ifrom, '@');
	if (p == NULL)
		goto failure;
	*p = '\0';
	(void) strcpy(buf, ifrom);
	(void) strcat(buf, " at ");
	*p++ = '@';
	(void) strcat(buf, p);
	if (strcmp(buf, efrom) == 0)
		goto success;

  failure:
# ifdef DEBUG
	if (tTd(3, 8))
		printf("FALSE\n");
# endif DEBUG
	return (FALSE);

  success:
# ifdef DEBUG
	if (tTd(3, 8))
		printf("TRUE\n");
# endif DEBUG
	return (TRUE);
}
/*
**  MAILFILE -- Send a message to a file.
**
**	If the file has the setuid/setgid bits set, but NO execute
**	bits, sendmail will try to become the owner of that file
**	rather than the real user.  Obviously, this only works if
**	sendmail runs as root.
**
**	Parameters:
**		filename -- the name of the file to send to.
**		ctladdr -- the controlling address header -- includes
**			the userid/groupid to be when sending.
**
**	Returns:
**		The exit code associated with the operation.
**
**	Side Effects:
**		none.
*/

mailfile(filename, ctladdr)
	char *filename;
	ADDRESS *ctladdr;
{
	register FILE *f;
	register int pid;

	/*
	**  Fork so we can change permissions here.
	**	Note that we MUST use fork, not vfork, because of
	**	the complications of calling subroutines, etc.
	*/

	DOFORK(fork);

	if (pid < 0)
		return (EX_OSERR);
	else if (pid == 0)
	{
		/* child -- actually write to file */
		struct stat stb;

		(void) signal(SIGINT, SIG_DFL);
		(void) signal(SIGHUP, SIG_DFL);
		(void) signal(SIGTERM, SIG_DFL);
		umask(OldUmask);
		if (stat(filename, &stb) < 0)
			stb.st_mode = 0666;
		if (bitset(0111, stb.st_mode))
			exit(EX_CANTCREAT);
		if (ctladdr == NULL)
			ctladdr = &CurEnv->e_from;
		if (!bitset(S_ISGID, stb.st_mode) || setgid(stb.st_gid) < 0)
		{
			if (ctladdr->q_uid == 0)
				(void) setgid(DefGid);
			else
				(void) setgid(ctladdr->q_gid);
		}
		if (!bitset(S_ISUID, stb.st_mode) || setuid(stb.st_uid) < 0)
		{
			if (ctladdr->q_uid == 0)
				(void) setuid(DefUid);
			else
				(void) setuid(ctladdr->q_uid);
		}
		f = dfopen(filename, "a");
		if (f == NULL)
			exit(EX_CANTCREAT);

		putfromline(f, Mailer[1]);
		(*CurEnv->e_puthdr)(f, Mailer[1], CurEnv);
		fputs("\n", f);
		(*CurEnv->e_putbody)(f, Mailer[1], FALSE);
		fputs("\n", f);
		(void) fclose(f);
		(void) fflush(stdout);

		/* reset ISUID & ISGID bits for paranoid systems */
		(void) chmod(filename, (int) stb.st_mode);
		exit(EX_OK);
		/*NOTREACHED*/
	}
	else
	{
		/* parent -- wait for exit status */
		register int i;
		auto int stat;

		while ((i = wait(&stat)) != pid)
		{
			if (i < 0)
			{
				stat = EX_OSERR << 8;
				break;
			}
		}
		if ((stat & 0377) != 0)
			stat = EX_UNAVAILABLE << 8;
		return ((stat >> 8) & 0377);
	}
}
/*
**  SENDALL -- actually send all the messages.
**
**	Parameters:
**		e -- the envelope to send.
**		verifyonly -- if set, only give verification messages.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Scans the send lists and sends everything it finds.
**		Delivers any appropriate error messages.
*/

sendall(e, verifyonly)
	ENVELOPE *e;
	bool verifyonly;
{
	register ADDRESS *q;
	bool oldverbose;

# ifdef DEBUG
	if (tTd(13, 1))
	{
		printf("\nSENDALL: verify %d, sendqueue:\n");
		printaddr(e->e_sendqueue, TRUE);
	}
# endif DEBUG

	/*
	**  Run through the list and send everything.
	*/

	oldverbose = Verbose;
	if (verifyonly)
		Verbose = TRUE;
	for (q = e->e_sendqueue; q != NULL; q = q->q_next)
	{
		if (verifyonly)
		{
			CurEnv->e_to = q->q_paddr;
			if (!bitset(QDONTSEND|QBADADDR, q->q_flags))
				message(Arpa_Info, "deliverable");
		}
		else
			(void) deliver(q);
	}
	Verbose = oldverbose;

	/*
	**  Now run through and check for errors.
	*/

	if (verifyonly)
		return;

	for (q = e->e_sendqueue; q != NULL; q = q->q_next)
	{
		register ADDRESS *qq;

# ifdef DEBUG
		if (tTd(13, 3))
		{
			printf("Checking ");
			printaddr(q, FALSE);
		}
# endif DEBUG

		if (bitset(QQUEUEUP, q->q_flags))
			e->e_queueup = TRUE;

		/* we have an address that failed -- find the parent */
		for (qq = q; qq != NULL; qq = qq->q_alias)
		{
			char obuf[MAXNAME + 6];
			extern char *aliaslookup();

			/* we can only have owners for local addresses */
			if (!bitset(M_LOCAL, qq->q_mailer->m_flags))
				continue;

			/* see if the owner list exists */
			(void) strcpy(obuf, "owner-");
			if (strncmp(qq->q_user, "owner-", 6) == 0)
				(void) strcat(obuf, "owner");
			else
				(void) strcat(obuf, qq->q_user);
			if (aliaslookup(obuf) == NULL)
				continue;

# ifdef DEBUG
			if (tTd(13, 4))
				printf("Errors to %s\n", obuf);
# endif DEBUG

			/* add in an errors-to field */
				/*   ugh... must happen before delivery.....
			addheader("errors-to", newstr(obuf), e);
				.... i guess this should go in sendto */

			/* only send errors if the message failed */
			if (!bitset(QBADADDR, q->q_flags))
				break;

			/* owner list exists -- add it to the error queue */
			qq->q_flags &= ~QPRIMARY;
			sendto(obuf, qq, &e->e_errorqueue);
			MailBack = TRUE;
			break;
		}

		/* if we did not find an owner, send to the sender */
		if (qq == NULL)
			sendto(e->e_from.q_paddr, qq, &e->e_errorqueue);
	}
}
/*
**  CHECKERRORS -- check a queue of addresses and process errors.
**
**	Parameters:
**		e -- the envelope to check.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Arranges to queue all tempfailed messages in q
**			or deliver error responses.
*/

checkerrors(e)
	register ENVELOPE *e;
{
	register ADDRESS *q;

# ifdef DEBUG
	if (tTd(4, 1))
	{
		printf("\ncheckerrors: FatalErrors %d, errorqueue:\n");
		printaddr(e->e_errorqueue, TRUE);
	}
# endif DEBUG

	/* mail back the transcript on errors */
	if (FatalErrors)
		savemail();

	/* queue up anything laying around */
	if (e->e_dontqueue)
		return;
	for (q = e->e_sendqueue; q != NULL; q = q->q_next)
	{
		if (bitset(QQUEUEUP, q->q_flags))
		{
# ifdef QUEUE
			queueup(e, FALSE);
# else QUEUE
			syserr("checkerrors: trying to queue %s", e->e_df);
# endif QUEUE
			break;
		}
	}
}
