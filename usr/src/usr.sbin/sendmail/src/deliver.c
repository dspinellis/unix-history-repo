# include <stdio.h>
# include <pwd.h>
# include <signal.h>
# include <ctype.h>
# include <errno.h>
# include "sendmail.h"
# ifdef LOG
# include <syslog.h>
# endif LOG

static char SccsId[] = "@(#)deliver.c	3.24	%G%";

/*
**  DELIVER -- Deliver a message to a particular address.
**
**	Parameters:
**		to -- the address to deliver the message to.
**		editfcn -- if non-NULL, we want to call this function
**			to output the letter (instead of just out-
**			putting it raw).
**
**	Returns:
**		zero -- successfully delivered.
**		else -- some failure, see ExitStat for more info.
**
**	Side Effects:
**		The standard input is passed off to someone.
*/

deliver(to, editfcn)
	ADDRESS *to;
	int (*editfcn)();
{
	char *host;
	char *user;
	char **pvp;
	register char **mvp;
	register char *p;
	register struct mailer *m;
	register int i;
	extern putmessage();
	extern bool checkcompat();
	char *pv[MAXPV+1];
	char tobuf[MAXLINE];
	char buf[MAXNAME];
	bool firstone;

	if (bitset(QDONTSEND, to->q_flags))
		return (0);

# ifdef DEBUG
	if (Debug)
		printf("\n--deliver, mailer=%d, host=`%s', first user=`%s'\n",
			to->q_mailer, to->q_host, to->q_user);
# endif DEBUG

	/*
	**  Do initial argv setup.
	**	Insert the mailer name.  Notice that $x expansion is
	**	NOT done on the mailer name.  Then, if the mailer has
	**	a picky -f flag, we insert it as appropriate.  This
	**	code does not check for 'pv' overflow; this places a
	**	manifest lower limit of 4 for MAXPV.
	*/

	m = Mailer[to->q_mailer];
	host = to->q_host;
	define('g', m->m_from);		/* translated from address */
	define('h', host);		/* to host */
	Errors = 0;
	errno = 0;
	pvp = pv;
	*pvp++ = m->m_argv[0];

	/* insert -f or -r flag as appropriate */
	if (bitset(M_FOPT|M_ROPT, m->m_flags) && FromFlag)
	{
		if (bitset(M_FOPT, m->m_flags))
			*pvp++ = "-f";
		else
			*pvp++ = "-r";
		(void) expand("$g", buf, &buf[sizeof buf - 1]);
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
		(void) expand(*mvp, buf, &buf[sizeof buf - 1]);
		*pvp++ = newstr(buf);
		if (pvp >= &pv[MAXPV - 3])
		{
			syserr("Too many parameters to %s before $u", pv[0]);
			return (-1);
		}
	}
	if (*mvp == NULL)
		syserr("No $u in mailer argv for %s", pv[0]);

	/*
	**  At this point *mvp points to the argument with $u.  We
	**  run through our address list and append all the addresses
	**  we can.  If we run out of space, do not fret!  We can
	**  always send another copy later.
	*/

	tobuf[0] = '\0';
	firstone = TRUE;
	To = tobuf;
	for (; to != NULL; to = to->q_next)
	{
		/* avoid sending multiple recipients to dumb mailers */
		if (!firstone && !bitset(M_MUSER, m->m_flags))
			break;

		/* if already sent or not for this host, don't send */
		if (bitset(QDONTSEND, to->q_flags) || strcmp(to->q_host, host) != 0)
			continue;
		user = to->q_user;
		To = to->q_paddr;
		to->q_flags |= QDONTSEND;
		firstone = FALSE;

# ifdef DEBUG
		if (Debug)
			printf("   send to `%s'\n", user);
# endif DEBUG

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
		**  If an error message has already been given, don't
		**	bother to send to this address.
		**
		**	>>>>>>>>>> This clause assumes that the local mailer
		**	>> NOTE >> cannot do any further aliasing; that
		**	>>>>>>>>>> function is subsumed by sendmail.
		*/

		if (bitset(QBADADDR, to->q_flags))
			continue;

		/*
		**  See if this user name is "special".
		**	If the user name has a slash in it, assume that this
		**	is a file -- send it off without further ado.
		**	Note that this means that editfcn's will not
		**	be applied to the message.  Also note that
		**	this type of addresses is not processed along
		**	with the others, so we fudge on the To person.
		*/

		if (m == Mailer[M_LOCAL])
		{
			if (index(user, '/') != NULL)
			{
				i = mailfile(user);
				giveresponse(i, TRUE, m);
				continue;
			}
		}

		/* create list of users for error messages */
		if (tobuf[0] != '\0')
			(void) strcat(tobuf, ",");
		(void) strcat(tobuf, to->q_paddr);
		define('u', user);		/* to user */
		define('z', to->q_home);	/* user's home */

		/* expand out this user */
		(void) expand(user, buf, &buf[sizeof buf - 1]);
		*pvp++ = newstr(buf);
		if (pvp >= &pv[MAXPV - 2])
		{
			/* allow some space for trailing parms */
			break;
		}
	}

	/* see if any addresses still exist */
	if (tobuf[0] == '\0')
		return (0);

	/* print out messages as full list */
	To = tobuf;

	/*
	**  Fill out any parameters after the $u parameter.
	*/

	while (*++mvp != NULL)
	{
		(void) expand(*mvp, buf, &buf[sizeof buf - 1]);
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
	**
	**	Notice the tacky hack to handle private mailers.
	*/

	if (editfcn == NULL)
		editfcn = putmessage;
	i = sendoff(m, pv, editfcn);

	return (i);
}
/*
**  SENDOFF -- send off call to mailer & collect response.
**
**	Parameters:
**		m -- mailer descriptor.
**		pvp -- parameter vector to send to it.
**		editfcn -- function to pipe it through.
**
**	Returns:
**		exit status of mailer.
**
**	Side Effects:
**		none.
*/

#define NFORKTRIES	5

sendoff(m, pvp, editfcn)
	struct mailer *m;
	char **pvp;
	int (*editfcn)();
{
	auto int st;
	register int i;
	int pid;
	int pvect[2];
	FILE *mfile;
	extern putmessage();
	extern FILE *fdopen();

# ifdef DEBUG
	if (Debug)
	{
		printf("Sendoff:\n");
		printav(pvp);
	}
# endif DEBUG

	/* create a pipe to shove the mail through */
	if (pipe(pvect) < 0)
	{
		syserr("pipe");
		return (-1);
	}
	for (i = NFORKTRIES; i-- > 0; )
	{
# ifdef VFORK
		pid = vfork();
# else
		pid = fork();
# endif
		if (pid >= 0)
			break;
		sleep((unsigned) NFORKTRIES - i);
	}
	if (pid < 0)
	{
		syserr("Cannot fork");
		(void) close(pvect[0]);
		(void) close(pvect[1]);
		return (-1);
	}
	else if (pid == 0)
	{
		/* child -- set up input & exec mailer */
		/* make diagnostic output be standard output */
		(void) close(2);
		(void) dup(1);
		(void) signal(SIGINT, SIG_IGN);
		(void) close(0);
		if (dup(pvect[0]) < 0)
		{
			syserr("Cannot dup to zero!");
			_exit(EX_OSERR);
		}
		(void) close(pvect[0]);
		(void) close(pvect[1]);
		if (!bitset(M_RESTR, m->m_flags))
			(void) setuid(getuid());
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
		execv(m->m_mailer, pvp);
		/* syserr fails because log is closed */
		/* syserr("Cannot exec %s", m->m_mailer); */
		printf("Cannot exec %s\n", m->m_mailer);
		(void) fflush(stdout);
		_exit(EX_UNAVAILABLE);
	}

	/* write out message to mailer */
	(void) close(pvect[0]);
	(void) signal(SIGPIPE, SIG_IGN);
	mfile = fdopen(pvect[1], "w");
	if (editfcn == NULL)
		editfcn = putmessage;
	(*editfcn)(mfile, m);
	(void) fclose(mfile);

	/*
	**  Wait for child to die and report status.
	**	We should never get fatal errors (e.g., segmentation
	**	violation), so we report those specially.  For other
	**	errors, we choose a status message (into statmsg),
	**	and if it represents an error, we print it.
	*/

	while ((i = wait(&st)) > 0 && i != pid)
		continue;
	if (i < 0)
	{
		syserr("wait");
		return (-1);
	}
	if ((st & 0377) != 0)
	{
		syserr("%s: stat %o", pvp[0], st);
		ExitStat = EX_UNAVAILABLE;
		return (-1);
	}
	i = (st >> 8) & 0377;
	giveresponse(i, TRUE, m);
	return (i);
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
**
**	Called By:
**		deliver
*/

giveresponse(stat, force, m)
	int stat;
	int force;
	register struct mailer *m;
{
	register char *statmsg;
	extern char *SysExMsg[];
	register int i;
	extern int N_SysEx;
	extern long MsgSize;
	char buf[30];

	i = stat - EX__BASE;
	if (i < 0 || i > N_SysEx)
		statmsg = NULL;
	else
		statmsg = SysExMsg[i];
	if (stat == 0)
	{
		if (bitset(M_FINAL, m->m_flags))
			statmsg = "delivered";
		else
			statmsg = "queued";
		if (Verbose)
			message(Arpa_Info, statmsg);
	}
	else
	{
		Errors++;
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
			usrerr("%s", statmsg);
	}

	/*
	**  Final cleanup.
	**	Log a record of the transaction.  Compute the new
	**	ExitStat -- if we already had an error, stick with
	**	that.
	*/

	if (statmsg == NULL)
	{
		(void) sprintf(buf, "error %d", stat);
		statmsg = buf;
	}

# ifdef LOG
	syslog(LOG_INFO, "%s->%s: %ld: %s", From.q_paddr, To, MsgSize, statmsg);
# endif LOG
	setstat(stat);
}
/*
**  PUTMESSAGE -- output a message to the final mailer.
**
**	then passes the rest of the message through.  If we have
**	managed to extract a date already, use that; otherwise,
**	use the current date/time.
**
**	Parameters:
**		fp -- file to output onto.
**		m -- a mailer descriptor.
**
**	Returns:
**		none.
**
**	Side Effects:
**		The message is written onto fp.
*/

putmessage(fp, m)
	FILE *fp;
	struct mailer *m;
{
	char buf[BUFSIZ];
	register int i;
	HDR *h;
	register char *p;
	extern char *arpadate();
	bool anyheader = FALSE;
	extern char *capitalize();
	extern char SentDate[];

	/* output "From" line unless supressed */
	if (!bitset(M_NHDR, m->m_flags))
		fprintf(fp, "%s\n", FromLine);

	/* output all header lines */
	for (h = Header; h != NULL; h = h->h_link)
	{
		if (bitset(H_DELETE, h->h_flags))
			continue;
		if (bitset(H_CHECK|H_ACHECK, h->h_flags) && !bitset(h->h_mflags, m->m_flags))
			continue;
		if (bitset(H_DEFAULT, h->h_flags))
		{
			(void) expand(h->h_value, buf, &buf[sizeof buf]);
			p = buf;
		}
		else
			p = h->h_value;
		if (*p == '\0')
			continue;
		fprintf(fp, "%s: %s\n", capitalize(h->h_field), p);
		h->h_flags |= H_USED;
		anyheader = TRUE;
	}

	if (anyheader)
		fprintf(fp, "\n");

	/* output the body of the message */
	rewind(stdin);
	while (!ferror(fp) && (i = fread(buf, 1, BUFSIZ, stdin)) > 0)
		(void) fwrite(buf, 1, i, fp);

	if (ferror(fp) && errno != EPIPE)
	{
		syserr("putmessage: write error");
		setstat(EX_IOERR);
	}
	errno = 0;
}
/*
**  MAILFILE -- Send a message to a file.
**
**	Parameters:
**		filename -- the name of the file to send to.
**
**	Returns:
**		The exit code associated with the operation.
**
**	Side Effects:
**		none.
**
**	Called By:
**		deliver
*/

mailfile(filename)
	char *filename;
{
	register FILE *f;

	f = fopen(filename, "a");
	if (f == NULL)
		return (EX_CANTCREAT);

	putmessage(f, Mailer[1]);
	fputs("\n", f);
	(void) fclose(f);
	return (EX_OK);
}
