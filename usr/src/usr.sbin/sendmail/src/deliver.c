/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)deliver.c	6.7 (Berkeley) %G%";
#endif /* not lint */

#include "sendmail.h"
#include <sys/signal.h>
#include <sys/stat.h>
#include <netdb.h>
#include <fcntl.h>
#include <errno.h>
#ifdef NAMED_BIND
#include <arpa/nameser.h>
#include <resolv.h>
#endif

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
**		e -- the envelope to deliver.
**		firstto -- head of the address list to deliver to.
**
**	Returns:
**		zero -- successfully delivered.
**		else -- some failure, see ExitStat for more info.
**
**	Side Effects:
**		The standard input is passed off to someone.
*/

deliver(e, firstto)
	register ENVELOPE *e;
	ADDRESS *firstto;
{
	char *host;			/* host being sent to */
	char *user;			/* user being sent to */
	char **pvp;
	register char **mvp;
	register char *p;
	register MAILER *m;		/* mailer for this recipient */
	ADDRESS *ctladdr;
	register MCI *mci;
	register ADDRESS *to = firstto;
	bool clever = FALSE;		/* running user smtp to this mailer */
	ADDRESS *tochain = NULL;	/* chain of users in this mailer call */
	int rcode;			/* response code */
	char *from;			/* pointer to from person */
	char *firstsig;			/* signature of firstto */
	char *pv[MAXPV+1];
	char tobuf[MAXLINE-50];		/* text line of to people */
	char buf[MAXNAME];
	char tfrombuf[MAXNAME];		/* translated from person */
	char rpathbuf[MAXNAME];		/* translated return path */
	extern int checkcompat();
	extern ADDRESS *getctladdr();
	extern char *remotename();
	extern MCI *openmailer();
	extern char *hostsignature();

	errno = 0;
	if (!ForceMail && bitset(QDONTSEND|QPSEUDO, to->q_flags))
		return (0);

#ifdef NAMED_BIND
	/* unless interactive, try twice, over a minute */
	if (OpMode == MD_DAEMON || OpMode == MD_SMTP) {
		_res.retrans = 30;
		_res.retry = 2;
	}
#endif 

	m = to->q_mailer;
	host = to->q_host;

	if (tTd(10, 1))
		printf("\n--deliver, mailer=%d, host=`%s', first user=`%s'\n",
			m->m_mno, host, to->q_user);

	/*
	**  If this mailer is expensive, and if we don't want to make
	**  connections now, just mark these addresses and return.
	**	This is useful if we want to batch connections to
	**	reduce load.  This will cause the messages to be
	**	queued up, and a daemon will come along to send the
	**	messages later.
	**		This should be on a per-mailer basis.
	*/

	if (NoConnect && !QueueRun && bitnset(M_EXPENSIVE, m->m_flags) &&
	    !Verbose)
	{
		for (; to != NULL; to = to->q_next)
		{
			if (bitset(QDONTSEND, to->q_flags) || to->q_mailer != m)
				continue;
			to->q_flags |= QQUEUEUP|QDONTSEND;
			e->e_to = to->q_paddr;
			message(Arpa_Info, "queued");
			if (LogLevel > 4)
				logdelivery("queued", e);
		}
		e->e_to = NULL;
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
	(void) strcpy(rpathbuf, remotename(e->e_returnpath, m, TRUE, TRUE, e));
	if (e->e_returnpath == e->e_sender)
	{
		from = rpathbuf;
	}
	else
	{
		(void) strcpy(tfrombuf, remotename(e->e_sender, m, TRUE, TRUE, e));
		from = tfrombuf;
	}

	define('f', e->e_returnpath, e);	/* raw return path */
	define('<', rpathbuf, e);		/* translated return path */
	define('g', from, e);			/* translated sender */
	define('h', host, e);			/* to host */
	Errors = 0;
	pvp = pv;
	*pvp++ = m->m_argv[0];

	/* insert -f or -r flag as appropriate */
	if (FromFlag && (bitnset(M_FOPT, m->m_flags) || bitnset(M_ROPT, m->m_flags)))
	{
		if (bitnset(M_FOPT, m->m_flags))
			*pvp++ = "-f";
		else
			*pvp++ = "-r";
		*pvp++ = newstr(rpathbuf);
	}

	/*
	**  Append the other fixed parts of the argv.  These run
	**  up to the first entry containing "$u".  There can only
	**  be one of these, and there are only a few more slots
	**  in the pv after it.
	*/

	for (mvp = m->m_argv; (p = *++mvp) != NULL; )
	{
		while ((p = strchr(p, '\001')) != NULL)
			if (*++p == 'u')
				break;
		if (p != NULL)
			break;

		/* this entry is safe -- go ahead and process it */
		expand(*mvp, buf, &buf[sizeof buf - 1], e);
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
# else /* SMTP */
		/* oops!  we don't implement SMTP */
		syserr("SMTP style mailer");
		return (EX_SOFTWARE);
# endif /* SMTP */
	}

	/*
	**  At this point *mvp points to the argument with $u.  We
	**  run through our address list and append all the addresses
	**  we can.  If we run out of space, do not fret!  We can
	**  always send another copy later.
	*/

	tobuf[0] = '\0';
	e->e_to = tobuf;
	ctladdr = NULL;
	firstsig = hostsignature(firstto->q_mailer, firstto->q_host, e);
	for (; to != NULL; to = to->q_next)
	{
		/* avoid sending multiple recipients to dumb mailers */
		if (tobuf[0] != '\0' && !bitnset(M_MUSER, m->m_flags))
			break;

		/* if already sent or not for this host, don't send */
		if (bitset(QDONTSEND, to->q_flags) ||
		    to->q_mailer != firstto->q_mailer ||
		    strcmp(hostsignature(to->q_mailer, to->q_host, e), firstsig) != 0)
			continue;

		/* avoid overflowing tobuf */
		if (sizeof tobuf < (strlen(to->q_paddr) + strlen(tobuf) + 2))
			break;

		if (tTd(10, 1))
		{
			printf("\nsend to ");
			printaddr(to, FALSE);
		}

		/* compute effective uid/gid when sending */
		if (to->q_mailer == ProgMailer)
			ctladdr = getctladdr(to);

		user = to->q_user;
		e->e_to = to->q_paddr;
		to->q_flags |= QDONTSEND;

		/*
		**  Check to see that these people are allowed to
		**  talk to each other.
		*/

		if (m->m_maxsize != 0 && e->e_msgsize > m->m_maxsize)
		{
			NoReturn = TRUE;
			usrerr("Message is too large; %ld bytes max", m->m_maxsize);
			giveresponse(EX_UNAVAILABLE, m, e);
			continue;
		}
		rcode = checkcompat(to, e);
		if (rcode != EX_OK)
		{
			giveresponse(rcode, m, e);
			continue;
		}

		/*
		**  Strip quote bits from names if the mailer is dumb
		**	about them.
		*/

		if (bitnset(M_STRIPQ, m->m_flags))
		{
			stripquotes(user);
			stripquotes(host);
		}

		/* hack attack -- delivermail compatibility */
		if (m == ProgMailer && *user == '|')
			user++;

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
		markstats(e, to);

		/*
		**  See if this user name is "special".
		**	If the user name has a slash in it, assume that this
		**	is a file -- send it off without further ado.  Note
		**	that this type of addresses is not processed along
		**	with the others, so we fudge on the To person.
		*/

		if (m == FileMailer)
		{
			rcode = mailfile(user, getctladdr(to), e);
			giveresponse(rcode, m, e);
			if (rcode == EX_OK)
				to->q_flags |= QSENT;
			continue;
		}

		/*
		**  Address is verified -- add this user to mailer
		**  argv, and add it to the print list of recipients.
		*/

		/* link together the chain of recipients */
		to->q_tchain = tochain;
		tochain = to;

		/* create list of users for error messages */
		(void) strcat(tobuf, ",");
		(void) strcat(tobuf, to->q_paddr);
		define('u', user, e);		/* to user */
		define('z', to->q_home, e);	/* user's home */

		/*
		**  Expand out this user into argument list.
		*/

		if (!clever)
		{
			expand(*mvp, buf, &buf[sizeof buf - 1], e);
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
		define('g', (char *) NULL, e);
		define('<', (char *) NULL, e);
		return (0);
	}

	/* print out messages as full list */
	e->e_to = tobuf + 1;

	/*
	**  Fill out any parameters after the $u parameter.
	*/

	while (!clever && *++mvp != NULL)
	{
		expand(*mvp, buf, &buf[sizeof buf - 1], e);
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
		ctladdr = &e->e_from;
#ifdef NAMED_BIND
	if (ConfigLevel < 2)
		_res.options &= ~(RES_DEFNAMES | RES_DNSRCH);	/* XXX */
#endif
	mci = openmailer(m, pv, ctladdr, clever, e);
	if (mci == NULL)
	{
		/* catastrophic error */
		rcode = -1;
		goto give_up;
	}
	else if (mci->mci_state != MCIS_OPEN)
	{
		/* couldn't open the mailer */
		rcode = mci->mci_exitstat;
		errno = mci->mci_errno;
		if (rcode == EX_OK)
		{
			/* shouldn't happen */
			rcode = EX_SOFTWARE;
		}
	}
	else if (!clever)
	{
		/*
		**  Format and send message.
		*/

		putfromline(mci->mci_out, m, e);
		(*e->e_puthdr)(mci->mci_out, m, e);
		putline("\n", mci->mci_out, m);
		(*e->e_putbody)(mci->mci_out, m, e);

		/* get the exit status */
		rcode = endmailer(mci, pv[0]);
	}
	else
#ifdef SMTP
	{
		/*
		**  Send the MAIL FROM: protocol
		*/

		rcode = smtpmailfrom(m, mci, e);
		if (rcode == EX_OK)
		{
			register char *t = tobuf;
			register int i;

			/* send the recipient list */
			tobuf[0] = '\0';
			for (to = tochain; to != NULL; to = to->q_tchain)
			{
				e->e_to = to->q_paddr;
				if ((i = smtprcpt(to, m, mci, e)) != EX_OK)
				{
					markfailure(e, to, i);
					giveresponse(i, m, e);
				}
				else
				{
					*t++ = ',';
					for (p = to->q_paddr; *p; *t++ = *p++)
						continue;
				}
			}

			/* now send the data */
			if (tobuf[0] == '\0')
			{
				e->e_to = NULL;
				if (bitset(MCIF_CACHED, mci->mci_flags))
					smtprset(m, mci, e);
			}
			else
			{
				e->e_to = tobuf + 1;
				rcode = smtpdata(m, mci, e);
			}

			/* now close the connection */
			if (!bitset(MCIF_CACHED, mci->mci_flags))
				smtpquit(m, mci, e);
		}
	}
#else /* not SMTP */
	{
		syserr("deliver: need SMTP compiled to use clever mailer");
		rcode = -1;
		goto give_up;
	}
#endif /* SMTP */
#ifdef NAMED_BIND
	if (ConfigLevel < 2)
		_res.options |= RES_DEFNAMES | RES_DNSRCH;	/* XXX */
#endif

	/* arrange a return receipt if requested */
	if (e->e_receiptto != NULL && bitnset(M_LOCAL, m->m_flags))
	{
		e->e_flags |= EF_SENDRECEIPT;
		/* do we want to send back more info? */
	}

	/*
	**  Do final status disposal.
	**	We check for something in tobuf for the SMTP case.
	**	If we got a temporary failure, arrange to queue the
	**		addressees.
	*/

  give_up:
	if (tobuf[0] != '\0')
		giveresponse(rcode, m, e);
	for (to = tochain; to != NULL; to = to->q_tchain)
	{
		if (rcode != EX_OK)
			markfailure(e, to, rcode);
		else
			to->q_flags |= QSENT;
	}

	/*
	**  Restore state and return.
	*/

	errno = 0;
	define('g', (char *) NULL, e);
	define('<', (char *) NULL, e);
	return (rcode);
}
/*
**  MARKFAILURE -- mark a failure on a specific address.
**
**	Parameters:
**		e -- the envelope we are sending.
**		q -- the address to mark.
**		rcode -- the code signifying the particular failure.
**
**	Returns:
**		none.
**
**	Side Effects:
**		marks the address (and possibly the envelope) with the
**			failure so that an error will be returned or
**			the message will be queued, as appropriate.
*/

markfailure(e, q, rcode)
	register ENVELOPE *e;
	register ADDRESS *q;
	int rcode;
{
	if (rcode == EX_OK)
		return;
	else if (rcode != EX_TEMPFAIL && rcode != EX_IOERR && rcode != EX_OSERR)
		q->q_flags |= QBADADDR;
	else if (curtime() > e->e_ctime + TimeOut)
	{
		extern char *pintvl();
		char buf[MAXLINE];

		if (!bitset(EF_TIMEOUT, e->e_flags))
		{
			(void) sprintf(buf, "Cannot send message for %s",
				pintvl(TimeOut, FALSE));
			if (e->e_message != NULL)
				free(e->e_message);
			e->e_message = newstr(buf);
			message(Arpa_Info, buf);
		}
		q->q_flags |= QBADADDR;
		e->e_flags |= EF_TIMEOUT;
	}
	else
		q->q_flags |= QQUEUEUP;
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

# ifndef FORK
# define FORK	fork
# endif

# define DOFORK(fORKfN) \
{\
	register int i;\
\
	for (i = NFORKTRIES; --i >= 0; )\
	{\
		pid = fORKfN();\
		if (pid >= 0)\
			break;\
		if (i > 0)\
			sleep((unsigned) NFORKTRIES - i);\
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

endmailer(mci, name)
	register MCI *mci;
	char *name;
{
	int st;

	/* close any connections */
	if (mci->mci_in != NULL)
		(void) fclose(mci->mci_in);
	if (mci->mci_out != NULL)
		(void) fclose(mci->mci_out);
	mci->mci_in = mci->mci_out = NULL;
	mci->mci_state = MCIS_CLOSED;

	/* in the IPC case there is nothing to wait for */
	if (mci->mci_pid == 0)
		return (EX_OK);

	/* wait for the mailer process to die and collect status */
	st = waitfor(mci->mci_pid);
	if (st == -1)
	{
		syserr("endmailer %s: wait", name);
		return (EX_SOFTWARE);
	}

	/* see if it died a horrid death */
	if ((st & 0377) != 0)
	{
		syserr("mailer %s died with signal %o", name, st);
		ExitStat = EX_TEMPFAIL;
		return (EX_TEMPFAIL);
	}

	/* normal death -- return status */
	st = (st >> 8) & 0377;
	return (st);
}
/*
**  OPENMAILER -- open connection to mailer.
**
**	Parameters:
**		m -- mailer descriptor.
**		pvp -- parameter vector to pass to mailer.
**		ctladdr -- controlling address for user.
**		clever -- create a full duplex connection.
**
**	Returns:
**		The mail connection info struct for this connection.
**		NULL on failure.
**
**	Side Effects:
**		creates a mailer in a subprocess.
*/

MCI *
openmailer(m, pvp, ctladdr, clever, e)
	MAILER *m;
	char **pvp;
	ADDRESS *ctladdr;
	bool clever;
	ENVELOPE *e;
{
	int pid;
	register MCI *mci;
	int mpvect[2];
	int rpvect[2];
	extern FILE *fdopen();

	if (tTd(11, 1))
	{
		printf("openmailer:");
		printav(pvp);
	}
	errno = 0;

	CurHostName = m->m_mailer;

	/*
	**  Deal with the special case of mail handled through an IPC
	**  connection.
	**	In this case we don't actually fork.  We must be
	**	running SMTP for this to work.  We will return a
	**	zero pid to indicate that we are running IPC.
	**  We also handle a debug version that just talks to stdin/out.
	*/

	/* check for Local Person Communication -- not for mortals!!! */
	if (strcmp(m->m_mailer, "[LPC]") == 0)
	{
		mci = (MCI *) xalloc(sizeof *mci);
		bzero((char *) mci, sizeof *mci);
		mci->mci_in = stdin;
		mci->mci_out = stdout;
		mci->mci_state = clever ? MCIS_OPENING : MCIS_OPEN;
		mci->mci_mailer = m;
	}
	else if (strcmp(m->m_mailer, "[IPC]") == 0 ||
		 strcmp(m->m_mailer, "[TCP]") == 0)
	{
#ifdef DAEMON
		register int i;
		register u_short port;
		char *curhost;
		extern MCI *mci_get();
		extern char *hostsignature();

		CurHostName = pvp[1];
		curhost = hostsignature(m, pvp[1], e);

		if (!clever)
			syserr("non-clever IPC");
		if (pvp[2] != NULL)
			port = atoi(pvp[2]);
		else
			port = 0;
		while (*curhost != '\0')
		{
			register char *p;
			char hostbuf[MAXNAME];

			/* pull the next host from the signature */
			p = strchr(curhost, ':');
			if (p == NULL)
				p = &curhost[strlen(curhost)];
			strncpy(hostbuf, curhost, p - curhost);
			hostbuf[p - curhost] = '\0';
			if (*p != '\0')
				p++;
			curhost = p;

			/* see if we already know that this host is fried */
			CurHostName = hostbuf;
			mci = mci_get(hostbuf, m);
			if (mci->mci_state != MCIS_CLOSED)
			{
				if (tTd(11, 1))
				{
					printf("openmailer: ");
					mci_dump(mci);
				}
				return mci;
			}
			mci->mci_mailer = m;
			if (mci->mci_exitstat != EX_OK)
				continue;

			/* try the connection */
			setproctitle("%s %s: %s", e->e_id, hostbuf, "user open");
			message(Arpa_Info, "Connecting to %s (%s)...",
				hostbuf, m->m_name);
			i = makeconnection(hostbuf, port, mci,
				bitnset(M_SECURE_PORT, m->m_flags));
			mci->mci_exitstat = i;
			mci->mci_errno = errno;
			if (i == EX_OK)
			{
				mci->mci_state = MCIS_OPENING;
				mci_cache(mci);
				break;
			}
			else if (tTd(11, 1))
				printf("openmailer: makeconnection => stat=%d, errno=%d\n",
					i, errno);


			/* enter status of this host */
			setstat(i);
		}
		mci->mci_pid = 0;
#else /* no DAEMON */
		syserr("openmailer: no IPC");
		if (tTd(11, 1))
			printf("openmailer: NULL\n");
		return NULL;
#endif /* DAEMON */
	}
	else
	{
		/* create a pipe to shove the mail through */
		if (pipe(mpvect) < 0)
		{
			syserr("openmailer: pipe (to mailer)");
			if (tTd(11, 1))
				printf("openmailer: NULL\n");
			return NULL;
		}

		/* if this mailer speaks smtp, create a return pipe */
		if (clever && pipe(rpvect) < 0)
		{
			syserr("openmailer: pipe (from mailer)");
			(void) close(mpvect[0]);
			(void) close(mpvect[1]);
			if (tTd(11, 1))
				printf("openmailer: NULL\n");
			return NULL;
		}

		/*
		**  Actually fork the mailer process.
		**	DOFORK is clever about retrying.
		**
		**	Dispose of SIGCHLD signal catchers that may be laying
		**	around so that endmail will get it.
		*/

		if (e->e_xfp != NULL)
			(void) fflush(e->e_xfp);		/* for debugging */
		(void) fflush(stdout);
# ifdef SIGCHLD
		(void) signal(SIGCHLD, SIG_DFL);
# endif /* SIGCHLD */
		DOFORK(FORK);
		/* pid is set by DOFORK */
		if (pid < 0)
		{
			/* failure */
			syserr("openmailer: cannot fork");
			(void) close(mpvect[0]);
			(void) close(mpvect[1]);
			if (clever)
			{
				(void) close(rpvect[0]);
				(void) close(rpvect[1]);
			}
			if (tTd(11, 1))
				printf("openmailer: NULL\n");
			return NULL;
		}
		else if (pid == 0)
		{
			int i;
			int saveerrno;
			extern int DtableSize;

			/* child -- set up input & exec mailer */
			/* make diagnostic output be standard output */
			(void) signal(SIGINT, SIG_IGN);
			(void) signal(SIGHUP, SIG_IGN);
			(void) signal(SIGTERM, SIG_DFL);

			/* arrange to filter std & diag output of command */
			if (clever)
			{
				(void) close(rpvect[0]);
				(void) close(1);
				(void) dup(rpvect[1]);
				(void) close(rpvect[1]);
			}
			else if (OpMode == MD_SMTP || HoldErrs)
			{
				/* put mailer output in transcript */
				(void) close(1);
				(void) dup(fileno(e->e_xfp));
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
			if (!bitnset(M_RESTR, m->m_flags))
			{
				if (ctladdr == NULL || ctladdr->q_uid == 0)
				{
					(void) setgid(DefGid);
					(void) initgroups(DefUser, DefGid);
					(void) setuid(DefUid);
				}
				else
				{
					(void) setgid(ctladdr->q_gid);
					(void) initgroups(ctladdr->q_ruser?
						ctladdr->q_ruser: ctladdr->q_user,
						ctladdr->q_gid);
					(void) setuid(ctladdr->q_uid);
				}
			}

			/* arrange for all the files to be closed */
			for (i = 3; i < DtableSize; i++)
			{
				register int j;
				if ((j = fcntl(i, F_GETFD, 0)) != -1)
					(void)fcntl(i, F_SETFD, j|1);
			}

			/* try to execute the mailer */
			execve(m->m_mailer, pvp, UserEnviron);
			saveerrno = errno;
			syserr("Cannot exec %s", m->m_mailer);
			if (m == LocalMailer)
				_exit(EX_TEMPFAIL);
			switch (saveerrno)
			{
			  case EIO:
			  case EAGAIN:
			  case ENOMEM:
# ifdef EPROCLIM
			  case EPROCLIM:
# endif
				_exit(EX_TEMPFAIL);
			}
			_exit(EX_UNAVAILABLE);
		}

		/*
		**  Set up return value.
		*/

		mci = (MCI *) xalloc(sizeof *mci);
		bzero((char *) mci, sizeof *mci);
		mci->mci_mailer = m;
		mci->mci_state = clever ? MCIS_OPENING : MCIS_OPEN;
		mci->mci_pid = pid;
		(void) close(mpvect[0]);
		mci->mci_out = fdopen(mpvect[1], "w");
		if (clever)
		{
			(void) close(rpvect[1]);
			mci->mci_in = fdopen(rpvect[0], "r");
		}
		else
		{
			mci->mci_flags |= MCIF_TEMP;
			mci->mci_in = NULL;
		}
	}

	/*
	**  If we are in SMTP opening state, send initial protocol.
	*/

	if (clever && mci->mci_state != MCIS_CLOSED)
	{
		smtpinit(m, mci, e);
	}
	if (tTd(11, 1))
	{
		printf("openmailer: ");
		mci_dump(mci);
	}

	return mci;
}
/*
**  GIVERESPONSE -- Interpret an error response from a mailer
**
**	Parameters:
**		stat -- the status code from the mailer (high byte
**			only; core dumps must have been taken care of
**			already).
**		m -- the mailer descriptor for this mailer.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Errors may be incremented.
**		ExitStat may be set.
*/

giveresponse(stat, m, e)
	int stat;
	register MAILER *m;
	ENVELOPE *e;
{
	register char *statmsg;
	extern char *SysExMsg[];
	register int i;
	extern int N_SysEx;
#ifdef NAMED_BIND
	extern int h_errno;
#endif
	char buf[MAXLINE];

#ifdef lint
	if (m == NULL)
		return;
#endif lint

	/*
	**  Compute status message from code.
	*/

	i = stat - EX__BASE;
	if (stat == 0)
		statmsg = "250 Sent";
	else if (i < 0 || i > N_SysEx)
	{
		(void) sprintf(buf, "554 unknown mailer error %d", stat);
		stat = EX_UNAVAILABLE;
		statmsg = buf;
	}
	else if (stat == EX_TEMPFAIL)
	{
		(void) strcpy(buf, SysExMsg[i]);
#ifdef NAMED_BIND
		if (h_errno == TRY_AGAIN)
		{
			extern char *errstring();

			statmsg = errstring(h_errno+MAX_ERRNO);
		}
		else
#endif
		{
			if (errno != 0)
			{
				extern char *errstring();

				statmsg = errstring(errno);
			}
			else
			{
#ifdef SMTP
				extern char SmtpError[];

				statmsg = SmtpError;
#else /* SMTP */
				statmsg = NULL;
#endif /* SMTP */
			}
		}
		if (statmsg != NULL && statmsg[0] != '\0')
		{
			(void) strcat(buf, ": ");
			(void) strcat(buf, statmsg);
		}
		statmsg = buf;
	}
	else
	{
		statmsg = SysExMsg[i];
	}

	/*
	**  Print the message as appropriate
	*/

	if (stat == EX_OK || stat == EX_TEMPFAIL)
		message(Arpa_Info, &statmsg[4]);
	else
	{
		Errors++;
		usrerr(statmsg);
	}

	/*
	**  Final cleanup.
	**	Log a record of the transaction.  Compute the new
	**	ExitStat -- if we already had an error, stick with
	**	that.
	*/

	if (LogLevel > ((stat == 0 || stat == EX_TEMPFAIL) ? 3 : 2))
		logdelivery(&statmsg[4], e);

	if (stat != EX_TEMPFAIL)
		setstat(stat);
	if (stat != EX_OK)
	{
		if (e->e_message != NULL)
			free(e->e_message);
		e->e_message = newstr(&statmsg[4]);
	}
	errno = 0;
#ifdef NAMED_BIND
	h_errno = 0;
#endif
}
/*
**  LOGDELIVERY -- log the delivery in the system log
**
**	Parameters:
**		stat -- the message to print for the status
**
**	Returns:
**		none
**
**	Side Effects:
**		none
*/

logdelivery(stat, e)
	char *stat;
	register ENVELOPE *e;
{
	extern char *pintvl();

# ifdef LOG
	syslog(LOG_INFO, "%s: to=%s, delay=%s, stat=%s", e->e_id,
	       e->e_to, pintvl(curtime() - e->e_ctime, TRUE), stat);
# endif /* LOG */
}
/*
**  PUTFROMLINE -- output a UNIX-style from line (or whatever)
**
**	then passes the rest of the message through.  If we have
**	managed to extract a date already, use that; otherwise,
**	use the current date/time.
**
**	One of the ugliest hacks seen by human eyes is contained herein:
**	UUCP wants those stupid "remote from <host>" lines.  Why oh why
**	does a well-meaning programmer such as myself have to deal with
**	this kind of antique garbage????
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

putfromline(fp, m, e)
	register FILE *fp;
	register MAILER *m;
	ENVELOPE *e;
{
	char *template = "\001l\n";
	char buf[MAXLINE];
	extern char SentDate[];

	if (bitnset(M_NHDR, m->m_flags))
		return;

# ifdef UGLYUUCP
	if (bitnset(M_UGLYUUCP, m->m_flags))
	{
		char *bang;
		char xbuf[MAXLINE];

		expand("\001<", buf, &buf[sizeof buf - 1], e);
		bang = strchr(buf, '!');
		if (bang == NULL)
			syserr("No ! in UUCP! (%s)", buf);
		else
		{
			*bang++ = '\0';
			(void) sprintf(xbuf, "From %s  \001d remote from %s\n", bang, buf);
			template = xbuf;
		}
	}
# endif /* UGLYUUCP */
	expand(template, buf, &buf[sizeof buf - 1], e);
	putline(buf, fp, m);
}
/*
**  PUTBODY -- put the body of a message.
**
**	Parameters:
**		fp -- file to output onto.
**		m -- a mailer descriptor to control output format.
**		e -- the envelope to put out.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The message is written onto fp.
*/

putbody(fp, m, e)
	FILE *fp;
	MAILER *m;
	register ENVELOPE *e;
{
	char buf[MAXLINE];

	/*
	**  Output the body of the message
	*/

	if (e->e_dfp == NULL)
	{
		if (e->e_df != NULL)
		{
			e->e_dfp = fopen(e->e_df, "r");
			if (e->e_dfp == NULL)
				syserr("putbody: Cannot open %s for %s from %s",
				e->e_df, e->e_to, e->e_from);
		}
		else
			putline("<<< No Message Collected >>>", fp, m);
	}
	if (e->e_dfp != NULL)
	{
		rewind(e->e_dfp);
		while (!ferror(fp) && fgets(buf, sizeof buf, e->e_dfp) != NULL)
		{
			if (buf[0] == 'F' && bitnset(M_ESCFROM, m->m_flags) &&
			    strncmp(buf, "From ", 5) == 0)
				(void) putc('>', fp);
			putline(buf, fp, m);
		}

		if (ferror(e->e_dfp))
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
**  MAILFILE -- Send a message to a file.
**
**	If the file has the setuid/setgid bits set, but NO execute
**	bits, sendmail will try to become the owner of that file
**	rather than the real user.  Obviously, this only works if
**	sendmail runs as root.
**
**	This could be done as a subordinate mailer, except that it
**	is used implicitly to save messages in ~/dead.letter.  We
**	view this as being sufficiently important as to include it
**	here.  For example, if the system is dying, we shouldn't have
**	to create another process plus some pipes to save the message.
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

mailfile(filename, ctladdr, e)
	char *filename;
	ADDRESS *ctladdr;
	register ENVELOPE *e;
{
	register FILE *f;
	register int pid;
	int mode;

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
		(void) umask(OldUmask);

		if (stat(filename, &stb) < 0)
			stb.st_mode = 0666;
		mode = stb.st_mode;

		/* limit the errors to those actually caused in the child */
		errno = 0;
		ExitStat = EX_OK;

		if (bitset(0111, stb.st_mode))
			exit(EX_CANTCREAT);
		if (ctladdr == NULL)
			ctladdr = &e->e_from;
		else
		{
			/* ignore setuid and setgid bits */
			mode &= ~(S_ISGID|S_ISUID);
		}

		/* we have to open the dfile BEFORE setuid */
		if (e->e_dfp == NULL && e->e_df != NULL)
		{
			e->e_dfp = fopen(e->e_df, "r");
			if (e->e_dfp == NULL)
			{
				syserr("mailfile: Cannot open %s for %s from %s",
					e->e_df, e->e_to, e->e_from);
			}
		}

		if (!bitset(S_ISGID, mode) || setgid(stb.st_gid) < 0)
		{
			if (ctladdr->q_uid == 0)
			{
				(void) setgid(DefGid);
				(void) initgroups(DefUser, DefGid);
			}
			else
			{
				(void) setgid(ctladdr->q_gid);
				(void) initgroups(ctladdr->q_ruser ?
					ctladdr->q_ruser : ctladdr->q_user,
					ctladdr->q_gid);
			}
		}
		if (!bitset(S_ISUID, mode) || setuid(stb.st_uid) < 0)
		{
			if (ctladdr->q_uid == 0)
				(void) setuid(DefUid);
			else
				(void) setuid(ctladdr->q_uid);
		}
		FileName = filename;
		LineNumber = 0;
		f = dfopen(filename, "a");
		if (f == NULL)
		{
			message("cannot open");
			exit(EX_CANTCREAT);
		}

		putfromline(f, ProgMailer, e);
		(*e->e_puthdr)(f, ProgMailer, e);
		putline("\n", f, ProgMailer);
		(*e->e_putbody)(f, ProgMailer, e);
		putline("\n", f, ProgMailer);
		if (ferror(f))
		{
			message("I/O error");
			setstat(EX_IOERR);
		}
		(void) fclose(f);
		(void) fflush(stdout);

		/* reset ISUID & ISGID bits for paranoid systems */
		(void) chmod(filename, (int) stb.st_mode);
		exit(ExitStat);
		/*NOTREACHED*/
	}
	else
	{
		/* parent -- wait for exit status */
		int st;

		st = waitfor(pid);
		if ((st & 0377) != 0)
			return (EX_UNAVAILABLE);
		else
			return ((st >> 8) & 0377);
		/*NOTREACHED*/
	}
}
/*
**  SENDALL -- actually send all the messages.
**
**	Parameters:
**		e -- the envelope to send.
**		mode -- the delivery mode to use.  If SM_DEFAULT, use
**			the current SendMode.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Scans the send lists and sends everything it finds.
**		Delivers any appropriate error messages.
**		If we are running in a non-interactive mode, takes the
**			appropriate action.
*/

sendall(e, mode)
	ENVELOPE *e;
	char mode;
{
	register ADDRESS *q;
	bool oldverbose;
	int pid;
	int nsent;
# ifdef LOCKF
	struct flock lfd;
# endif

	/* determine actual delivery mode */
	if (mode == SM_DEFAULT)
	{
		extern bool shouldqueue();

		if (shouldqueue(e->e_msgpriority, e->e_ctime))
			mode = SM_QUEUE;
		else
			mode = SendMode;
	}

	if (tTd(13, 1))
	{
		printf("\nSENDALL: mode %c, sendqueue:\n", mode);
		printaddr(e->e_sendqueue, TRUE);
	}

	/*
	**  Do any preprocessing necessary for the mode we are running.
	**	Check to make sure the hop count is reasonable.
	**	Delete sends to the sender in mailing lists.
	*/

	CurEnv = e;

	if (e->e_hopcount > MaxHopCount)
	{
		errno = 0;
		syserr("sendall: too many hops %d (%d max): from %s, to %s",
			e->e_hopcount, MaxHopCount, e->e_from.q_paddr, e->e_to);
		return;
	}

	if (!MeToo)
	{
		extern ADDRESS *recipient();

		e->e_from.q_flags |= QDONTSEND;
		(void) recipient(&e->e_from, &e->e_sendqueue, e);
	}

# ifdef QUEUE
	if ((mode == SM_QUEUE || mode == SM_FORK ||
	     (mode != SM_VERIFY && SuperSafe)) &&
	    !bitset(EF_INQUEUE, e->e_flags))
		queueup(e, TRUE, mode == SM_QUEUE);
#endif /* QUEUE */

	oldverbose = Verbose;
	switch (mode)
	{
	  case SM_VERIFY:
		Verbose = TRUE;
		break;

	  case SM_QUEUE:
  queueonly:
		e->e_flags |= EF_INQUEUE|EF_KEEPQUEUE;
		return;

	  case SM_FORK:
		if (e->e_xfp != NULL)
			(void) fflush(e->e_xfp);

# ifdef LOCKF
		/*
		**  Since lockf has the interesting semantic that the
		**  lock is lost when we fork, we have to risk losing
		**  the lock here by closing before the fork, and then
		**  trying to get it back in the child.
		*/

		if (e->e_lockfp != NULL)
		{
			(void) fclose(e->e_lockfp);
			e->e_lockfp = NULL;
		}
# endif /* LOCKF */

		pid = fork();
		if (pid < 0)
		{
			goto queueonly;
		}
		else if (pid > 0)
		{
			/* be sure we leave the temp files to our child */
			e->e_id = e->e_df = NULL;
# ifndef LOCKF
			if (e->e_lockfp != NULL)
				(void) fclose(e->e_lockfp);
# endif
			return;
		}

		/* double fork to avoid zombies */
		if (fork() > 0)
			exit(EX_OK);

		/* be sure we are immune from the terminal */
		disconnect(FALSE);

# ifdef LOCKF
		/*
		**  Now try to get our lock back.
		*/

		lfd.l_type = F_WRLCK;
		lfd.l_whence = lfd.l_start = lfd.l_len = 0;
		e->e_lockfp = fopen(queuename(e, 'q'), "r+");
		if (e->e_lockfp == NULL ||
		    fcntl(fileno(e->e_lockfp), F_SETLK, &lfd) < 0)
		{
			/* oops....  lost it */
# ifdef LOG
			if (LogLevel > 5)
				syslog(LOG_NOTICE, "%s: lost lock: %m",
					e->e_id);
# endif /* LOG */
			exit(EX_OK);
		}
# endif /* LOCKF */

		break;
	}

	/*
	**  Run through the list and send everything.
	*/

	nsent = 0;
	for (q = e->e_sendqueue; q != NULL; q = q->q_next)
	{
		if (mode == SM_VERIFY)
		{
			e->e_to = q->q_paddr;
			if (!bitset(QDONTSEND|QBADADDR, q->q_flags))
				message(Arpa_Info, "deliverable");
		}
		else if (!bitset(QDONTSEND, q->q_flags))
		{
# ifdef QUEUE
			/*
			**  Checkpoint the send list every few addresses
			*/

			if (nsent >= CheckpointInterval)
			{
				queueup(e, TRUE, FALSE);
				nsent = 0;
			}
# endif /* QUEUE */
			if (deliver(e, q) == EX_OK)
				nsent++;
		}
	}
	Verbose = oldverbose;

	/*
	**  Now run through and check for errors.
	*/

	if (mode == SM_VERIFY)
		return;

	for (q = e->e_sendqueue; q != NULL; q = q->q_next)
	{
		register ADDRESS *qq;

		if (tTd(13, 3))
		{
			printf("Checking ");
			printaddr(q, FALSE);
		}

		/* only send errors if the message failed */
		if (!bitset(QBADADDR, q->q_flags))
			continue;

		/* we have an address that failed -- find the parent */
		for (qq = q; qq != NULL; qq = qq->q_alias)
		{
			char obuf[MAXNAME + 6];
			extern char *aliaslookup();

			/* we can only have owners for local addresses */
			if (!bitnset(M_LOCAL, qq->q_mailer->m_flags))
				continue;

			/* see if the owner list exists */
			(void) strcpy(obuf, "owner-");
			if (strncmp(qq->q_user, "owner-", 6) == 0)
				(void) strcat(obuf, "owner");
			else
				(void) strcat(obuf, qq->q_user);
			if (!bitnset(M_USR_UPPER, qq->q_mailer->m_flags))
				makelower(obuf);
			if (aliaslookup(obuf) == NULL)
				continue;

			if (tTd(13, 4))
				printf("Errors to %s\n", obuf);

			/* owner list exists -- add it to the error queue */
			sendtolist(obuf, (ADDRESS *) NULL, &e->e_errorqueue, e);

			/* and set the return path to point to it */
			e->e_returnpath = newstr(obuf);

			ErrorMode = EM_MAIL;
			break;
		}

		/* if we did not find an owner, send to the sender */
		if (qq == NULL && bitset(QBADADDR, q->q_flags))
			sendtolist(e->e_from.q_paddr, qq, &e->e_errorqueue, e);
	}

	if (mode == SM_FORK)
		finis();
}
/*
**  HOSTSIGNATURE -- return the "signature" for a host.
**
**	The signature describes how we are going to send this -- it
**	can be just the hostname (for non-Internet hosts) or can be
**	an ordered list of MX hosts.
**
**	Parameters:
**		m -- the mailer describing this host.
**		host -- the host name.
**		e -- the current envelope.
**
**	Returns:
**		The signature for this host.
**
**	Side Effects:
**		Can tweak the symbol table.
*/

char *
hostsignature(m, host, e)
	register MAILER *m;
	char *host;
	ENVELOPE *e;
{
	register char *p;
	register STAB *s;
	int i;
	int len;
#ifdef NAMED_BIND
	int nmx;
	auto int rcode;
	char *mxhosts[MAXMXHOSTS + 1];
	static char myhostbuf[MAXNAME];
#endif

	/*
	**  Check to see if this uses IPC -- if not, it can't have MX records.
	*/

	p = m->m_mailer;
	if (strcmp(p, "[IPC]") != 0 && strcmp(p, "[TCP]") != 0)
	{
		/* just an ordinary mailer */
		return host;
	}

	/*
	**  If it is a numeric address, just return it.
	*/

	if (host[0] == '[')
		return host;

	/*
	**  Look it up in the symbol table.
	*/

	s = stab(host, ST_HOSTSIG, ST_ENTER);
	if (s->s_hostsig != NULL)
		return s->s_hostsig;

	/*
	**  Not already there -- create a signature.
	*/

#ifdef NAMED_BIND
	if (myhostbuf[0] == '\0')
		expand("\001j", myhostbuf, &myhostbuf[sizeof myhostbuf - 1], e);

	nmx = getmxrr(host, mxhosts, myhostbuf, &rcode);
	if (nmx <= 0)
	{
		register MCI *mci;
		extern int errno;
		extern MCI *mci_get();

		/* update the connection info for this host */
		mci = mci_get(host, m);
		mci->mci_exitstat = rcode;
		mci->mci_errno = errno;

		/* and return the original host name as the signature */
		s->s_hostsig = host;
		return host;
	}

	len = 0;
	for (i = 0; i < nmx; i++)
	{
		len += strlen(mxhosts[i]) + 1;
	}
	s->s_hostsig = p = xalloc(len);
	for (i = 0; i < nmx; i++)
	{
		if (i != 0)
			*p++ = ':';
		strcpy(p, mxhosts[i]);
		p += strlen(p);
	}
	makelower(s->s_hostsig);
#else
	/* not using BIND -- the signature is just the host name */
	s->s_hostsig = host;
#endif
	if (tTd(17, 1))
		printf("hostsignature(%s) = %s\n", host, s->s_hostsig);
	return s->s_hostsig;
}
