/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)envelope.c	5.19 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <pwd.h>
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
	if (e == CurEnv)
		parent = e->e_parent;
	clearenvelope(e, TRUE);
	if (e == CurEnv)
		bcopy((char *) &NullAddress, (char *) &e->e_from, sizeof e->e_from);
	else
		bcopy((char *) &CurEnv->e_from, (char *) &e->e_from, sizeof e->e_from);
	e->e_parent = parent;
	e->e_ctime = curtime();
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
	if (LogLevel > 10)
		syslog(LOG_DEBUG, "dropenvelope, id=%s, flags=%o, pid=%d",
				  e->e_id == NULL ? "(none)" : e->e_id,
				  e->e_flags, getpid());
#endif LOG

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

		sendtolist(CurEnv->e_receiptto, (ADDRESS *) NULL, &rlist);
		(void) returntosender("Return receipt", rlist, FALSE);
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
#else QUEUE
		syserr("dropenvelope: queueup");
#endif QUEUE
	}

	/* now unlock the job */
	closexscript(e);
	unlockqueue(e);

	/* make sure that this envelope is marked unused */
	e->e_id = e->e_df = NULL;
	if (e->e_dfp != NULL)
		(void) fclose(e->e_dfp);
	e->e_dfp = NULL;
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

initsys()
{
	static char cbuf[5];			/* holds hop count */
	static char pbuf[10];			/* holds pid */
#ifdef TTYNAME
	static char ybuf[10];			/* holds tty id */
	register char *p;
#endif TTYNAME
	extern char *ttyname();
	extern char *macvalue();
	extern char Version[];

	/*
	**  Give this envelope a reality.
	**	I.e., an id, a transcript, and a creation time.
	*/

	openxscript(CurEnv);
	CurEnv->e_ctime = curtime();

	/*
	**  Set OutChannel to something useful if stdout isn't it.
	**	This arranges that any extra stuff the mailer produces
	**	gets sent back to the user on error (because it is
	**	tucked away in the transcript).
	*/

	if (OpMode == MD_DAEMON && QueueRun)
		OutChannel = CurEnv->e_xfp;

	/*
	**  Set up some basic system macros.
	*/

	/* process id */
	(void) sprintf(pbuf, "%d", getpid());
	define('p', pbuf, CurEnv);

	/* hop count */
	(void) sprintf(cbuf, "%d", CurEnv->e_hopcount);
	define('c', cbuf, CurEnv);

	/* time as integer, unix time, arpa time */
	settime();

#ifdef TTYNAME
	/* tty name */
	if (macvalue('y', CurEnv) == NULL)
	{
		p = ttyname(2);
		if (p != NULL)
		{
			if (rindex(p, '/') != NULL)
				p = rindex(p, '/') + 1;
			(void) strcpy(ybuf, p);
			define('y', ybuf, CurEnv);
		}
	}
#endif TTYNAME
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

settime()
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
	(void) sprintf(tbuf, "%02d%02d%02d%02d%02d", tm->tm_year, tm->tm_mon+1,
			tm->tm_mday, tm->tm_hour, tm->tm_min);
	define('t', tbuf, CurEnv);
	(void) strcpy(dbuf, ctime(&now));
	*index(dbuf, '\n') = '\0';
	if (macvalue('d', CurEnv) == NULL)
		define('d', dbuf, CurEnv);
	p = newstr(arpadate(dbuf));
	if (macvalue('a', CurEnv) == NULL)
		define('a', p, CurEnv);
	define('b', p, CurEnv);
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

# ifdef LOG
	if (LogLevel > 19)
		syslog(LOG_DEBUG, "%s: openx%s", e->e_id, e->e_xfp == NULL ? "" : " (no)");
# endif LOG
	if (e->e_xfp != NULL)
		return;
	p = queuename(e, 'x');
	e->e_xfp = fopen(p, "w");
	if (e->e_xfp == NULL)
		syserr("Can't create %s", p);
	else
		(void) chmod(p, 0644);
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
**
**	Returns:
**		none.
**
**	Side Effects:
**		sets sendmail's notion of who the from person is.
*/

setsender(from)
	char *from;
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

	if (QueueRun || OpMode == MD_SMTP || OpMode == MD_ARPAFTP)
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
		    index(from, '!') == NULL && getuid() != 0)
		{
			/* network sends -r regardless (why why why?) */
			/* syserr("%s, you cannot use the -f flag", realname); */
			from = NULL;
		}
	}

	SuprErrs = TRUE;
	if (from == NULL || parseaddr(from, &CurEnv->e_from, 1, '\0') == NULL)
	{
		/* log garbage addresses for traceback */
		if (from != NULL)
		{
# ifdef LOG
			if (LogLevel >= 1)
			    if (realname == from && RealHostName != NULL)
				syslog(LOG_NOTICE,
				    "from=%s unparseable, received from %s",
				    from, RealHostName);
			    else
				syslog(LOG_NOTICE,
				    "Unparseable username %s wants from=%s",
				    realname, from);
# endif LOG
		}
		from = newstr(realname);
		if (parseaddr(from, &CurEnv->e_from, 1, '\0') == NULL &&
		    parseaddr("postmaster", &CurEnv->e_from, 1, '\0') == NULL)
		{
			syserr("setsender: can't even parse postmaster!");
		}
	}
	else
		FromFlag = TRUE;
	CurEnv->e_from.q_flags |= QDONTSEND;
	loweraddr(&CurEnv->e_from);
	SuprErrs = FALSE;

	if (CurEnv->e_from.q_mailer == LocalMailer &&
	    (pw = getpwnam(CurEnv->e_from.q_user)) != NULL)
	{
		/*
		**  Process passwd file entry.
		*/


		/* extract home directory */
		CurEnv->e_from.q_home = newstr(pw->pw_dir);
		define('z', CurEnv->e_from.q_home, CurEnv);

		/* extract user and group id */
		CurEnv->e_from.q_uid = pw->pw_uid;
		CurEnv->e_from.q_gid = pw->pw_gid;

		/* if the user has given fullname already, don't redefine */
		if (FullName == NULL)
			FullName = macvalue('x', CurEnv);
		if (FullName != NULL && FullName[0] == '\0')
			FullName = NULL;

		/* extract full name from passwd file */
		if (FullName == NULL && pw->pw_gecos != NULL &&
		    strcmp(pw->pw_name, CurEnv->e_from.q_user) == 0)
		{
			buildfname(pw->pw_gecos, CurEnv->e_from.q_user, buf);
			if (buf[0] != '\0')
				FullName = newstr(buf);
		}
		if (FullName != NULL)
			define('x', FullName, CurEnv);
	}
	else
	{
		if (CurEnv->e_from.q_home == NULL)
			CurEnv->e_from.q_home = getenv("HOME");
		CurEnv->e_from.q_uid = getuid();
		CurEnv->e_from.q_gid = getgid();
	}

	if (CurEnv->e_from.q_uid != 0)
	{
		DefUid = CurEnv->e_from.q_uid;
		DefGid = CurEnv->e_from.q_gid;
	}

	/*
	**  Rewrite the from person to dispose of possible implicit
	**	links in the net.
	*/

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
	define('f', newstr(buf), CurEnv);

	/* save the domain spec if this mailer wants it */
	if (CurEnv->e_from.q_mailer != NULL &&
	    bitnset(M_CANONICAL, CurEnv->e_from.q_mailer->m_flags))
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
