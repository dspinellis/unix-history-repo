/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)envelope.c	6.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <pwd.h>
#include <sys/file.h>
#include "sendmail.h"

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
	register ENVELOPE *parent;
	extern putheader(), putbody();
	extern ENVELOPE BlankEnvelope;

	parent = CurEnv;
	if (e == CurEnv && e->e_parent != NULL)
		parent = e->e_parent;
	clearenvelope(e, TRUE);
	if (e == CurEnv)
		bcopy((char *) &NullAddress, (char *) &e->e_from, sizeof e->e_from);
	else
		bcopy((char *) &CurEnv->e_from, (char *) &e->e_from, sizeof e->e_from);
	e->e_parent = parent;
	e->e_ctime = curtime();
	if (parent != NULL)
		e->e_msgpriority = parent->e_msgsize;
	e->e_puthdr = putheader;
	e->e_putbody = putbody;
	if (CurEnv->e_xfp != NULL)
		(void) fflush(CurEnv->e_xfp);

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
**		Unlocks this queue file.
*/

dropenvelope(e)
	register ENVELOPE *e;
{
	bool queueit = FALSE;
	register ADDRESS *q;

	if (tTd(50, 1))
	{
		printf("dropenvelope %x id=", e);
		xputs(e->e_id);
		printf(" flags=%o\n", e->e_flags);
	}
#ifdef LOG
	if (LogLevel > 12)
		syslog(LOG_DEBUG, "dropenvelope, id=%s, flags=%o, pid=%d",
				  e->e_id == NULL ? "(none)" : e->e_id,
				  e->e_flags, getpid());
#endif /* LOG */

	/* we must have an id to remove disk files */
	if (e->e_id == NULL)
		return;

	/*
	**  Extract state information from dregs of send list.
	*/

	for (q = e->e_sendqueue; q != NULL; q = q->q_next)
	{
		if (bitset(QQUEUEUP, q->q_flags))
			queueit = TRUE;
	}

	/*
	**  Send back return receipts as requested.
	*/

	if (e->e_receiptto != NULL && bitset(EF_SENDRECEIPT, e->e_flags))
	{
		auto ADDRESS *rlist = NULL;

		sendtolist(e->e_receiptto, (ADDRESS *) NULL, &rlist, e);
		(void) returntosender("Return receipt", rlist, FALSE, e);
	}

	/*
	**  Arrange to send error messages if there are fatal errors.
	*/

	if (bitset(EF_FATALERRS|EF_TIMEOUT, e->e_flags) && ErrorMode != EM_QUIET)
		savemail(e);

	/*
	**  Instantiate or deinstantiate the queue.
	*/

	if ((!queueit && !bitset(EF_KEEPQUEUE, e->e_flags)) ||
	    bitset(EF_CLRQUEUE, e->e_flags))
	{
		if (e->e_df != NULL)
			xunlink(e->e_df);
		xunlink(queuename(e, 'q'));
	}
	else if (queueit || !bitset(EF_INQUEUE, e->e_flags))
	{
#ifdef QUEUE
		queueup(e, FALSE, FALSE);
#else /* QUEUE */
		syserr("dropenvelope: queueup");
#endif /* QUEUE */
	}

	/* now unlock the job */
	closexscript(e);
	unlockqueue(e);

	/* make sure that this envelope is marked unused */
	e->e_id = e->e_df = NULL;
	if (e->e_dfp != NULL)
		(void) fclose(e->e_dfp);
	e->e_dfp = NULL;

#ifdef LOG
	if (LogLevel >= 10)
		syslog(LOG_INFO, "%s: done", e->e_id);
#endif /* LOG */
}
/*
**  CLEARENVELOPE -- clear an envelope without unlocking
**
**	This is normally used by a child process to get a clean
**	envelope without disturbing the parent.
**
**	Parameters:
**		e -- the envelope to clear.
**		fullclear - if set, the current envelope is total
**			garbage and should be ignored; otherwise,
**			release any resources it may indicate.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Closes files associated with the envelope.
**		Marks the envelope as unallocated.
*/

clearenvelope(e, fullclear)
	register ENVELOPE *e;
	bool fullclear;
{
	register HDR *bh;
	register HDR **nhp;
	extern ENVELOPE BlankEnvelope;

	if (!fullclear)
	{
		/* clear out any file information */
		if (e->e_xfp != NULL)
			(void) fclose(e->e_xfp);
		if (e->e_dfp != NULL)
			(void) fclose(e->e_dfp);
	}

	/* now clear out the data */
	STRUCTCOPY(BlankEnvelope, *e);
	bh = BlankEnvelope.e_header;
	nhp = &e->e_header;
	while (bh != NULL)
	{
		*nhp = (HDR *) xalloc(sizeof *bh);
		bcopy((char *) bh, (char *) *nhp, sizeof *bh);
		bh = bh->h_link;
		nhp = &(*nhp)->h_link;
	}
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

initsys(e)
	register ENVELOPE *e;
{
	static char cbuf[5];			/* holds hop count */
	static char pbuf[10];			/* holds pid */
#ifdef TTYNAME
	static char ybuf[10];			/* holds tty id */
	register char *p;
#endif /* TTYNAME */
	extern char *ttyname();
	extern char *macvalue();
	extern char Version[];

	/*
	**  Give this envelope a reality.
	**	I.e., an id, a transcript, and a creation time.
	*/

	openxscript(e);
	e->e_ctime = curtime();

	/*
	**  Set OutChannel to something useful if stdout isn't it.
	**	This arranges that any extra stuff the mailer produces
	**	gets sent back to the user on error (because it is
	**	tucked away in the transcript).
	*/

	if (OpMode == MD_DAEMON && QueueRun)
		OutChannel = e->e_xfp;

	/*
	**  Set up some basic system macros.
	*/

	/* process id */
	(void) sprintf(pbuf, "%d", getpid());
	define('p', pbuf, e);

	/* hop count */
	(void) sprintf(cbuf, "%d", e->e_hopcount);
	define('c', cbuf, e);

	/* time as integer, unix time, arpa time */
	settime(e);

#ifdef TTYNAME
	/* tty name */
	if (macvalue('y', e) == NULL)
	{
		p = ttyname(2);
		if (p != NULL)
		{
			if (strrchr(p, '/') != NULL)
				p = strrchr(p, '/') + 1;
			(void) strcpy(ybuf, p);
			define('y', ybuf, e);
		}
	}
#endif /* TTYNAME */
}
/*
**  SETTIME -- set the current time.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sets the various time macros -- $a, $b, $d, $t.
*/

settime(e)
	register ENVELOPE *e;
{
	register char *p;
	auto time_t now;
	static char tbuf[20];			/* holds "current" time */
	static char dbuf[30];			/* holds ctime(tbuf) */
	register struct tm *tm;
	extern char *arpadate();
	extern struct tm *gmtime();
	extern char *macvalue();

	now = curtime();
	tm = gmtime(&now);
	(void) sprintf(tbuf, "%04d%02d%02d%02d%02d", tm->tm_year + 1900,
			tm->tm_mon+1, tm->tm_mday, tm->tm_hour, tm->tm_min);
	define('t', tbuf, e);
	(void) strcpy(dbuf, ctime(&now));
	*strchr(dbuf, '\n') = '\0';
	if (macvalue('d', e) == NULL)
		define('d', dbuf, e);
	p = newstr(arpadate(dbuf));
	if (macvalue('a', e) == NULL)
		define('a', p, e);
	define('b', p, e);
}
/*
**  OPENXSCRIPT -- Open transcript file
**
**	Creates a transcript file for possible eventual mailing or
**	sending back.
**
**	Parameters:
**		e -- the envelope to create the transcript in/for.
**
**	Returns:
**		none
**
**	Side Effects:
**		Creates the transcript file.
*/

openxscript(e)
	register ENVELOPE *e;
{
	register char *p;
	int fd;

# ifdef LOG
	if (LogLevel > 19)
		syslog(LOG_DEBUG, "%s: openx%s", e->e_id, e->e_xfp == NULL ? "" : " (no)");
# endif /* LOG */
	if (e->e_xfp != NULL)
		return;
	p = queuename(e, 'x');
	fd = open(p, O_WRONLY|O_CREAT, 0644);
	if (fd < 0)
		syserr("Can't create %s", p);
	else
		e->e_xfp = fdopen(fd, "w");
}
/*
**  CLOSEXSCRIPT -- close the transcript file.
**
**	Parameters:
**		e -- the envelope containing the transcript to close.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

closexscript(e)
	register ENVELOPE *e;
{
	if (e->e_xfp == NULL)
		return;
	(void) fclose(e->e_xfp);
	e->e_xfp = NULL;
}
/*
**  SETSENDER -- set the person who this message is from
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
**		from -- the person we would like to believe this message
**			is from, as specified on the command line.
**		e -- the envelope in which we would like the sender set.
**
**	Returns:
**		none.
**
**	Side Effects:
**		sets sendmail's notion of who the from person is.
*/

setsender(from, e)
	char *from;
	register ENVELOPE *e;
{
	register char **pvp;
	char *realname = NULL;
	register struct passwd *pw;
	char buf[MAXNAME];
	char pvpbuf[PSBUFSIZE];
	extern struct passwd *getpwnam();
	extern char *macvalue();
	extern char **prescan();
	extern bool safefile();
	extern char *FullName;

	if (tTd(45, 1))
		printf("setsender(%s)\n", from == NULL ? "" : from);

	/*
	**  Figure out the real user executing us.
	**	Username can return errno != 0 on non-errors.
	*/

	if (QueueRun || OpMode == MD_SMTP)
		realname = from;
	if (realname == NULL || realname[0] == '\0')
	{
		extern char *username();

		realname = username();
	}

	/*
	**  Determine if this real person is allowed to alias themselves.
	*/

	if (from != NULL)
	{
		extern bool trusteduser();

		if (!trusteduser(realname) && getuid() != geteuid() &&
		    strchr(from, '!') == NULL && getuid() != 0)
		{
			/* network sends -r regardless (why why why?) */
			/* syserr("%s, you cannot use the -f flag", realname); */
			from = NULL;
		}
	}

/*
	SuprErrs = TRUE;
*/
	if (from == NULL || parseaddr(from, &e->e_from, 1, '\0', e) == NULL)
	{
		/* log garbage addresses for traceback */
# ifdef LOG
		if (from != NULL && LogLevel >= 1)
		{
			char *host = RealHostName;

			if (host == NULL)
				host = MyHostName;
			syslog(LOG_NOTICE,
				"from=%s unparseable, received from %s@%s",
				from, realname, host);
		}
# endif /* LOG */
		if (from != NULL)
			SuprErrs = TRUE;
		if (from == realname ||
		    parseaddr(from = newstr(realname), &e->e_from, 1, '\0', e) == NULL)
		{
			SuprErrs = TRUE;
			if (parseaddr("postmaster", &e->e_from, 1, '\0', e) == NULL)
				syserr("setsender: can't even parse postmaster!");
		}
	}
	else
		FromFlag = TRUE;
	e->e_from.q_flags |= QDONTSEND;
	loweraddr(&e->e_from);
	SuprErrs = FALSE;

	pvp = NULL;
	if (e->e_from.q_mailer == LocalMailer)
	{
# ifdef USERDB
		register char *p;
		extern char *udbsender();
# endif

		/* if the user has given fullname already, don't redefine */
		if (FullName == NULL)
			FullName = macvalue('x', e);
		if (FullName != NULL && FullName[0] == '\0')
			FullName = NULL;

# ifdef USERDB
		p = udbsender(from);

		if (p != NULL)
		{
			/*
			**  We have an alternate address for the sender
			*/

			pvp = prescan(p, '\0', pvpbuf);
		}
# endif /* USERDB */

		if ((pw = getpwnam(e->e_from.q_user)) != NULL)
		{
			/*
			**  Process passwd file entry.
			*/


			/* extract home directory */
			e->e_from.q_home = newstr(pw->pw_dir);
			define('z', e->e_from.q_home, e);

			/* extract user and group id */
			e->e_from.q_uid = pw->pw_uid;
			e->e_from.q_gid = pw->pw_gid;

			/* extract full name from passwd file */
			if (FullName == NULL && pw->pw_gecos != NULL &&
			    strcmp(pw->pw_name, e->e_from.q_user) == 0)
			{
				buildfname(pw->pw_gecos, e->e_from.q_user, buf);
				if (buf[0] != '\0')
					FullName = newstr(buf);
			}
		}
		if (FullName != NULL)
			define('x', FullName, e);
	}
	else
	{
		if (e->e_from.q_home == NULL)
			e->e_from.q_home = getenv("HOME");
		e->e_from.q_uid = getuid();
		e->e_from.q_gid = getgid();
	}

	/*
	**  Rewrite the from person to dispose of possible implicit
	**	links in the net.
	*/

	if (pvp == NULL)
		pvp = prescan(from, '\0', pvpbuf);
	if (pvp == NULL)
	{
# ifdef LOG
		if (LogLevel >= 1)
			syslog(LOG_NOTICE, "cannot prescan from (%s)", from);
# endif
		usrerr("cannot prescan from (%s)", from);
		finis();
	}
	rewrite(pvp, 3);
	rewrite(pvp, 1);
	rewrite(pvp, 4);
	cataddr(pvp, buf, sizeof buf);
	e->e_sender = e->e_returnpath = newstr(buf);

	define('f', e->e_sender, e);

	/* save the domain spec if this mailer wants it */
	if (e->e_from.q_mailer != NULL &&
	    bitnset(M_CANONICAL, e->e_from.q_mailer->m_flags))
	{
		extern char **copyplist();

		while (*pvp != NULL && strcmp(*pvp, "@") != 0)
			pvp++;
		if (*pvp != NULL)
			e->e_fromdomain = copyplist(pvp, TRUE);
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
