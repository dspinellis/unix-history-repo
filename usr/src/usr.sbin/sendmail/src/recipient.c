/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)recipient.c	8.26 (Berkeley) %G%";
#endif /* not lint */

# include "sendmail.h"
# include <pwd.h>

/*
**  SENDTOLIST -- Designate a send list.
**
**	The parameter is a comma-separated list of people to send to.
**	This routine arranges to send to all of them.
**
**	The `ctladdr' is the address that expanded to be this one,
**	e.g., in an alias expansion.  This is used for a number of
**	purposed, most notably inheritance of uid/gid for protection
**	purposes.  It is also used to detect self-reference in group
**	expansions and the like.
**
**	Parameters:
**		list -- the send list.
**		ctladdr -- the address template for the person to
**			send to -- effective uid/gid are important.
**			This is typically the alias that caused this
**			expansion.
**		sendq -- a pointer to the head of a queue to put
**			these people into.
**		e -- the envelope in which to add these recipients.
**		qflags -- special flags to set in the q_flags field.
**
**	Returns:
**		pointer to chain of addresses.
**
**	Side Effects:
**		none.
*/

# define MAXRCRSN	10

ADDRESS *
sendto(list, copyf, ctladdr, qflags)
	char *list;
	ADDRESS *ctladdr;
	ADDRESS **sendq;
	register ENVELOPE *e;
	u_short qflags;
{
	register char *p;
	register ADDRESS *al;	/* list of addresses to send to */
	bool firstone;		/* set on first address sent */
	char delimiter;		/* the address delimiter */
	int naddrs;
	char *oldto = e->e_to;
	ADDRESS *sibl;		/* sibling pointer in tree */
	ADDRESS *prev;		/* previous sibling */

	if (list == NULL)
	{
		syserr("sendtolist: null list");
		return 0;
	}

	if (tTd(25, 1))
	{
		printf("sendto: %s\n   ctladdr=", list);
		printaddr(ctladdr, FALSE);
	}

	/* heuristic to determine old versus new style addresses */
	if (ctladdr == NULL &&
	    (strchr(list, ',') != NULL || strchr(list, ';') != NULL ||
	     strchr(list, '<') != NULL || strchr(list, '(') != NULL))
		e->e_flags &= ~EF_OLDSTYLE;
	delimiter = ' ';
	if (!bitset(EF_OLDSTYLE, e->e_flags) || ctladdr != NULL)
		delimiter = ',';

	firstone = TRUE;
	al = NULL;
	naddrs = 0;

	for (p = list; *p != '\0'; )
	{
		auto char *delimptr;
		register ADDRESS *a;

		/* parse the address */
		while ((isascii(*p) && isspace(*p)) || *p == ',')
			p++;
		a = parseaddr(p, NULLADDR, RF_COPYALL, delimiter, &delimptr, e);
		p = delimptr;
		if (a == NULL)
			continue;
		a->q_next = al;
		a->q_alias = ctladdr;
		if (ctladdr != NULL)
			a->q_flags |= ctladdr->q_flags & ~QPRIMARY;
		a->q_flags |= qflags;

		/* see if this should be marked as a primary address */
		if (ctladdr == NULL ||
		    (firstone && *p == '\0' && bitset(QPRIMARY, ctladdr->q_flags)))
			a->q_flags |= QPRIMARY;

		if (ctladdr != NULL && sameaddr(ctladdr, a))
			ctladdr->q_flags |= QSELFREF;
		al = a;
		firstone = FALSE;
	}

	/* arrange to send to everyone on the local send list */
	prev = sibl = NULL;
	if (ctladdr != NULL)
		prev = ctladdr->q_child;
	while (al != NULL)
	{
		register ADDRESS *a = al;
		extern ADDRESS *recipient();

		al = a->q_next;
		sibl = recipient(a);
		if (sibl != NULL)
		{
			extern ADDRESS *addrref();

			/* inherit full name */
			if (sibl->q_fullname == NULL && ctladdr != NULL)
				sibl->q_fullname = ctladdr->q_fullname;

			/* link tree together (but only if the node is new) */
			if (sibl == a)
			{
				sibl->q_sibling = prev;
				prev = sibl;
			}
		}
	}

	e->e_to = oldto;
	return (naddrs);
	if (ctladdr != NULL)
		ctladdr->q_child = prev;
	return (prev);
}
/*
**  ADDRREF -- return pointer to address that references another address.
**
**	Parameters:
**		a -- address to check.
**		r -- reference to find.
**
**	Returns:
**		address of node in tree rooted at 'a' that references
**			'r'.
**		NULL if no such node exists.
**
**	Side Effects:
**		none.
*/

ADDRESS *
addrref(a, r)
	register ADDRESS *a;
	register ADDRESS *r;
{
	register ADDRESS *q;

	while (a != NULL)
	{
		if (a->q_child == r || a->q_sibling == r)
			return (a);
		q = addrref(a->q_child, r);
		if (q != NULL)
			return (q);
		a = a->q_sibling;
	}
	return (NULL);
}
/*
**  RECIPIENT -- Designate a message recipient
**
**	Saves the named person for future mailing.
**
**	Parameters:
**		a -- the (preparsed) address header for the recipient.
**		sendq -- a pointer to the head of a queue to put the
**			recipient in.  Duplicate supression is done
**			in this queue.
**		e -- the current envelope.
**
**	Returns:
**		pointer to address actually inserted in send list.
**
**	Side Effects:
**		none.
*/

ADDRESS *
ADDRESS *
recipient(a, sendq, e)
	register ADDRESS *a;
	register ADDRESS **sendq;
	register ENVELOPE *e;
{
	register ADDRESS *q;
	ADDRESS **pq;
	register struct mailer *m;
	register char *p;
	bool quoted = FALSE;		/* set if the addr has a quote bit */
	int findusercount = 0;
	char buf[MAXNAME];		/* unquoted image of the user name */
	extern int safefile();

	e->e_to = a->q_paddr;
	m = a->q_mailer;
	errno = 0;
	if (tTd(26, 1))
	{
		printf("\nrecipient: ");
		printaddr(a, FALSE);
	}

	/* if this is primary, add it to the original recipient list */
	if (a->q_alias == NULL)
	{
		if (e->e_origrcpt == NULL)
			e->e_origrcpt = a->q_paddr;
		else if (e->e_origrcpt != a->q_paddr)
			e->e_origrcpt = "";
	}

	/* break aliasing loops */
	if (AliasLevel > MAXRCRSN)
	{
		usrerr("554 aliasing/forwarding loop broken");
		return (NULL);
	}

	/*
	**  Finish setting up address structure.
	*/

	/* set the queue timeout */
	a->q_timeout = TimeOuts.to_q_return;

	/* get unquoted user for file, program or user.name check */
	(void) strcpy(buf, a->q_user);
	for (p = buf; *p != '\0' && !quoted; p++)
	{
		if (*p == '\\')
			quoted = TRUE;
	}
	stripquotes(buf);

	/* check for direct mailing to restricted mailers */
	if (a->q_alias == NULL && m == ProgMailer)
	{
		a->q_flags |= QBADADDR;
		usrerr("550 Cannot mail directly to programs");
	}

	/*
	**  Look up this person in the recipient list.
	**	If they are there already, return, otherwise continue.
	**	If the list is empty, just add it.  Notice the cute
	**	hack to make from addresses suppress things correctly:
	**	the QDONTSEND bit will be set in the send list.
	**	[Please note: the emphasis is on "hack."]
	*/

	for (pq = sendq; (q = *pq) != NULL; pq = &q->q_next)
	{
		if (sameaddr(q, a))
		{
			/* if this is a reinsertion, just go ahead */
			if (bitset(QVERIFIED, q->q_flags))
				break;

			if (tTd(26, 1))
			{
				printf("%s in sendq: ", a->q_paddr);
				printaddr(q, FALSE);
			}
			if (!bitset(QPRIMARY, q->q_flags))
			{
				if (!bitset(QDONTSEND, a->q_flags))
					message("duplicate suppressed");
				q->q_flags |= a->q_flags;
			}
			if (!bitset(QPSEUDO, a->q_flags))
				q->q_flags &= ~QPSEUDO;
			return (q);
		}
	}

	/* add address on list */
	if (*pq != a)
	{
		*pq = a;
		a->q_next = NULL;
	}

	a->q_flags &= ~QVERIFIED;

	/*
	**  Alias the name and handle special mailer types.
	*/

  trylocaluser:
	if (tTd(29, 7))
		printf("at trylocaluser %s\n", a->q_user);

	if (bitset(QDONTSEND|QBADADDR|QVERIFIED, a->q_flags))
		goto testselfdestruct;

	if (m == InclMailer)
	{
		a->q_flags |= QDONTSEND;
		if (a->q_alias == NULL)
		{
			a->q_flags |= QBADADDR;
			usrerr("550 Cannot mail directly to :include:s");
		}
		else
		{
			int ret;

			message("including file %s", a->q_user);
			ret = include(a->q_user, FALSE, a, sendq, e);
			if (transienterror(ret))
			{
#ifdef LOG
				if (LogLevel > 2)
					syslog(LOG_ERR, "%s: include %s: transient error: %e",
						e->e_id, a->q_user, errstring(ret));
#endif
				a->q_flags |= QQUEUEUP;
				usrerr("451 Cannot open %s: %s",
					a->q_user, errstring(ret));
			}
			else if (ret != 0)
			{
				a->q_flags |= QBADADDR;
				usrerr("550 Cannot open %s: %s",
					a->q_user, errstring(ret));
			}
		}
	}
	else if (m == FileMailer)
	{
		extern bool writable();

		/* check if writable or creatable */
		if (a->q_alias == NULL)
		{
			a->q_flags |= QBADADDR;
			usrerr("550 Cannot mail directly to files");
		}
		else if (!writable(buf))
		{
			a->q_flags |= QBADADDR;
			giveresponse(EX_CANTCREAT, m, NULL, a->q_alias, e);
		}
	}

	if (m != LocalMailer)
	{
		if (!bitset(QDONTSEND, a->q_flags))
			e->e_nrcpts++;
		goto testselfdestruct;
	}

	/* try aliasing */
	alias(a, sendq, e);

# ifdef USERDB
	/* if not aliased, look it up in the user database */
	if (!bitset(QDONTSEND|QNOTREMOTE|QVERIFIED, a->q_flags))
	{
		extern int udbexpand();

		if (udbexpand(a, sendq, e) == EX_TEMPFAIL)
		{
			a->q_flags |= QQUEUEUP;
			if (e->e_message == NULL)
				e->e_message = newstr("Deferred: user database error");
# ifdef LOG
			if (LogLevel > 8)
				syslog(LOG_INFO, "%s: deferred: udbexpand: %s",
					e->e_id, errstring(errno));
# endif
			message("queued (user database error): %s",
				errstring(errno));
			e->e_nrcpts++;
			goto testselfdestruct;
		}
	}
# endif

	/* if it was an alias or a UDB expansion, just return now */
	if (bitset(QDONTSEND|QQUEUEUP|QVERIFIED, a->q_flags))
		goto testselfdestruct;

	/*
	**  If we have a level two config file, then pass the name through
	**  Ruleset 5 before sending it off.  Ruleset 5 has the right
	**  to send rewrite it to another mailer.  This gives us a hook
	**  after local aliasing has been done.
	*/

	if (tTd(29, 5))
	{
		printf("recipient: testing local?  cl=%d, rr5=%x\n\t",
			ConfigLevel, RewriteRules[5]);
		printaddr(a, FALSE);
	}
	if (!bitset(QNOTREMOTE, a->q_flags) && ConfigLevel >= 2 &&
	    RewriteRules[5] != NULL)
	{
		maplocaluser(a, sendq, e);
	}

	/*
	**  If it didn't get rewritten to another mailer, go ahead
	**  and deliver it.
	*/

	if (!bitset(QDONTSEND|QQUEUEUP, a->q_flags))
	{
		auto bool fuzzy;
		register struct passwd *pw;
		extern struct passwd *finduser();

		/* warning -- finduser may trash buf */
		pw = finduser(buf, &fuzzy);
		if (pw == NULL)
		{
			a->q_flags |= QBADADDR;
			giveresponse(EX_NOUSER, m, NULL, a->q_alias, e);
		}
		else
		{
			char nbuf[MAXNAME];

			if (fuzzy)
			{
				/* name was a fuzzy match */
				a->q_user = newstr(pw->pw_name);
				if (findusercount++ > 3)
				{
					a->q_flags |= QBADADDR;
					usrerr("554 aliasing/forwarding loop for %s broken",
						pw->pw_name);
					return (a);
				}

				/* see if it aliases */
				(void) strcpy(buf, pw->pw_name);
				goto trylocaluser;
			}
			a->q_home = newstr(pw->pw_dir);
			a->q_uid = pw->pw_uid;
			a->q_gid = pw->pw_gid;
			a->q_ruser = newstr(pw->pw_name);
			a->q_flags |= QGOODUID;
			buildfname(pw->pw_gecos, pw->pw_name, nbuf);
			if (nbuf[0] != '\0')
				a->q_fullname = newstr(nbuf);
			if (!quoted)
				forward(a, sendq, e);
		}
	}
	if (!bitset(QDONTSEND, a->q_flags))
		e->e_nrcpts++;

  testselfdestruct:
	if (tTd(26, 8))
	{
		printf("testselfdestruct: ");
		printaddr(a, TRUE);
	}
	if (a->q_alias == NULL && a != &e->e_from &&
	    bitset(QDONTSEND, a->q_flags))
	{
		q = *sendq;
		while (q != NULL && bitset(QDONTSEND, q->q_flags))
			q = q->q_next;
		if (q == NULL)
		{
			a->q_flags |= QBADADDR;
			usrerr("554 aliasing/forwarding loop broken");
		}
	}
	return (a);

	return (a);
}
/*
**  FINDUSER -- find the password entry for a user.
**
**	This looks a lot like getpwnam, except that it may want to
**	do some fancier pattern matching in /etc/passwd.
**
**	This routine contains most of the time of many sendmail runs.
**	It deserves to be optimized.
**
**	Parameters:
**		name -- the name to match against.
**		fuzzyp -- an outarg that is set to TRUE if this entry
**			was found using the fuzzy matching algorithm;
**			set to FALSE otherwise.
**
**	Returns:
**		A pointer to a pw struct.
**		NULL if name is unknown or ambiguous.
**
**	Side Effects:
**		may modify name.
*/

struct passwd *
finduser(name, fuzzyp)
	char *name;
	bool *fuzzyp;
{
	register struct passwd *pw;
	register char *p;
	extern struct passwd *getpwent();
	extern struct passwd *getpwnam();

	if (tTd(29, 4))
		printf("finduser(%s): ", name);

	*fuzzyp = FALSE;

	/* DEC Hesiod getpwnam accepts numeric strings -- short circuit it */
	for (p = name; *p != '\0'; p++)
		if (!isascii(*p) || !isdigit(*p))
			break;
	if (*p == '\0')
	{
		if (tTd(29, 4))
			printf("failed (numeric input)\n");
		return NULL;
	}

	/* look up this login name using fast path */
	if ((pw = getpwnam(name)) != NULL)
	{
		if (tTd(29, 4))
			printf("found (non-fuzzy)\n");
		return (pw);
	}

#ifdef MATCHGECOS
	/* see if fuzzy matching allowed */
	if (!MatchGecos)
	{
		if (tTd(29, 4))
			printf("not found (fuzzy disabled)\n");
		return NULL;
	}

	/* search for a matching full name instead */
	for (p = name; *p != '\0'; p++)
	{
		if (*p == (SpaceSub & 0177) || *p == '_')
			*p = ' ';
	}
	(void) setpwent();
	while ((pw = getpwent()) != NULL)
	{
		char buf[MAXNAME];

		fullname(pw, buf);
		if (strchr(buf, ' ') != NULL && !strcasecmp(buf, name))
		{
			if (tTd(29, 4))
				printf("fuzzy matches %s\n", pw->pw_name);
				message(Arpa_Info, "sending to %s <%s>",
				    buf, pw->pw_name);
			return (pw);
		}
	}
	if (tTd(29, 4))
		printf("no fuzzy match found\n");
#else
	if (tTd(29, 4))
		printf("not found (fuzzy disabled)\n");
#endif
	return (NULL);
}
/*
**  WRITABLE -- predicate returning if the file is writable.
**
**	This routine must duplicate the algorithm in sys/fio.c.
**	Unfortunately, we cannot use the access call since we
**	won't necessarily be the real uid when we try to
**	actually open the file.
**
**	Notice that ANY file with ANY execute bit is automatically
**	not writable.  This is also enforced by mailfile.
**
**	Parameters:
**		s -- pointer to a stat struct for the file.
**
**	Returns:
**		TRUE -- if we will be able to write this file.
**		FALSE -- if we cannot write this file.
**
**	Side Effects:
**		none.
*/

bool
writable(filename)
	char *filename;
{
	uid_t euid;
	gid_t egid;
	int bits;
	register char *p;
	char *uname;
	struct stat stb;
	extern char RealUserName[];

	if (tTd(29, 5))
		printf("writable(%s)\n", filename);

#ifdef HASLSTAT
	if (lstat(filename, &stb) < 0)
#else
	if (stat(filename, &stb) < 0)
#endif
	{
		/* file does not exist -- see if directory is safe */
		p = strrchr(filename, '/');
		if (p == NULL)
			return FALSE;
		*p = '\0';
		if (safefile(filename, RealUid, RealGid, RealUserName,
			     SF_MUSTOWN, S_IWRITE|S_IEXEC) != 0)
		{
			*p = '/';
			return FALSE;
		}
		*p = '/';
		return TRUE;
	}

	/*
	**  File does exist -- check that it is writable.
	*/

	if (bitset(0111, stb.st_mode))
	{
		if (tTd(29, 5))
			printf("failed (mode %o: x bits)\n", stb.st_mode);
		return (FALSE);
	}

	euid = RealUid;
	uname = RealUserName;
	if (euid == 0)
	{
		euid = DefUid;
		uname = DefUser;
	}
	egid = RealGid;
	if (egid == 0)
		egid = DefGid;
	if (geteuid() == 0)
	{
		if (bitset(S_ISUID, stb.st_mode))
		{
			euid = stb.st_uid;
			uname = NULL;
		}
		if (bitset(S_ISGID, stb.st_mode))
			egid = stb.st_gid;
	}

	if (tTd(29, 5))
		printf("\teu/gid=%d/%d, st_u/gid=%d/%d\n",
			euid, egid, stb.st_uid, stb.st_gid);

	return safefile(filename, euid, egid, uname, SF_NOSLINK, S_IWRITE) == 0;
}
/*
**  INCLUDE -- handle :include: specification.
**
**	Parameters:
**		fname -- filename to include.
**		forwarding -- if TRUE, we are reading a .forward file.
**			if FALSE, it's a :include: file.
**		ctladdr -- address template to use to fill in these
**			addresses -- effective user/group id are
**			the important things.
**		sendq -- a pointer to the head of the send queue
**			to put these addresses in.
**
**	Returns:
**		open error status
**
**	Side Effects:
**		reads the :include: file and sends to everyone
**		listed in that file.
*/

static jmp_buf	CtxIncludeTimeout;
static int	includetimeout();

int
include(fname, forwarding, ctladdr, sendq, e)
	char *fname;
	bool forwarding;
	ADDRESS *ctladdr;
	ADDRESS **sendq;
	ENVELOPE *e;
{
	register FILE *fp = NULL;
	char *oldto = e->e_to;
	char *oldfilename = FileName;
	int oldlinenumber = LineNumber;
	register EVENT *ev = NULL;
	int nincludes;
	register ADDRESS *ca;
	uid_t saveduid, uid;
	gid_t savedgid, gid;
	char *uname;
	int rval = 0;
	int sfflags = forwarding ? SF_MUSTOWN : 0;
	char buf[MAXLINE];

	if (tTd(27, 2))
		printf("include(%s)\n", fname);
	if (tTd(27, 4))
		printf("   ruid=%d euid=%d\n", getuid(), geteuid());
	if (tTd(27, 14))
	{
		printf("ctladdr ");
		printaddr(ctladdr, FALSE);
	}

	if (tTd(27, 9))
		printf("include: old uid = %d/%d\n", getuid(), geteuid());

	ca = getctladdr(ctladdr);
	if (ca == NULL)
	{
		uid = DefUid;
		gid = DefGid;
		uname = DefUser;
		saveduid = -1;
	}
	else
	{
		uid = ca->q_uid;
		gid = ca->q_gid;
		uname = ca->q_user;
#ifdef HASSETREUID
		saveduid = geteuid();
		savedgid = getegid();
		if (saveduid == 0)
		{
			initgroups(uname, gid);
			if (uid != 0)
				(void) setreuid(0, uid);
		}
#endif                   
	}

	if (tTd(27, 9))
		printf("include: new uid = %d/%d\n", getuid(), geteuid());

	/*
	**  If home directory is remote mounted but server is down,
	**  this can hang or give errors; use a timeout to avoid this
	*/

	if (setjmp(CtxIncludeTimeout) != 0)
	{
		ctladdr->q_flags |= QQUEUEUP;
		errno = 0;
		usrerr("451 open timeout on %s", fname);

		/* return pseudo-error code */
		rval = EOPENTIMEOUT;
		goto resetuid;
	}
	ev = setevent((time_t) 60, includetimeout, 0);

	/* the input file must be marked safe */
	rval = safefile(fname, uid, gid, uname, sfflags, S_IREAD);
	if (rval != 0)
	{
		/* don't use this :include: file */
		clrevent(ev);
		if (tTd(27, 4))
			printf("include: not safe (uid=%d): %s\n",
				uid, errstring(rval));
		goto resetuid;
	}

	fp = fopen(fname, "r");
	if (fp == NULL)
	{
		rval = errno;
		if (tTd(27, 4))
			printf("include: open: %s\n", errstring(rval));
	}
	else if (ca == NULL)
	{
		struct stat st;

		if (fstat(fileno(fp), &st) < 0)
		{
			rval = errno;
			syserr("Cannot fstat %s!", fname);
		}
		else
		{
			ctladdr->q_uid = st.st_uid;
			ctladdr->q_gid = st.st_gid;
			ctladdr->q_flags |= QGOODUID;
		}
	}

	clrevent(ev);

resetuid:

#ifdef HASSETREUID
	if (saveduid == 0)
	{
		if (uid != 0)
			if (setreuid(-1, 0) < 0 || setreuid(RealUid, 0) < 0)
				syserr("setreuid(%d, 0) failure (real=%d, eff=%d)",
					RealUid, getuid(), geteuid());
		setgid(savedgid);
	}
#endif

	if (tTd(27, 9))
		printf("include: reset uid = %d/%d\n", getuid(), geteuid());

	if (fp == NULL)
		return rval;

	if (bitset(EF_VRFYONLY, e->e_flags))
	{
		/* don't do any more now */
		ctladdr->q_flags |= QVERIFIED;
		e->e_nrcpts++;
		xfclose(fp, "include", fname);
		return rval;
	}

	/* read the file -- each line is a comma-separated list. */
	FileName = fname;
	LineNumber = 0;
	ctladdr->q_flags &= ~QSELFREF;
	nincludes = 0;
	while (fgets(buf, sizeof buf, fp) != NULL)
	{
		register char *p = strchr(buf, '\n');

		LineNumber++;
		if (p != NULL)
			*p = '\0';
		if (buf[0] == '#' || buf[0] == '\0')
			continue;
		e->e_to = NULL;
		message("%s to %s",
			forwarding ? "forwarding" : "sending", buf);
#ifdef LOG
		if (forwarding && LogLevel > 9)
			syslog(LOG_INFO, "%s: forward %s => %s",
				e->e_id, oldto, buf);
#endif

		AliasLevel++;
		sendto(buf, 1, ctladdr, 0);
		AliasLevel--;
	}

	if (ferror(fp) && tTd(27, 3))
		printf("include: read error: %s\n", errstring(errno));
	if (nincludes > 0 && !bitset(QSELFREF, ctladdr->q_flags))
	{
		if (tTd(27, 5))
		{
			printf("include: QDONTSEND ");
			printaddr(ctladdr, FALSE);
		}
		ctladdr->q_flags |= QDONTSEND;
	}

	(void) xfclose(fp, "include", fname);
	FileName = oldfilename;
	LineNumber = oldlinenumber;
	e->e_to = oldto;
	return rval;
}

static
includetimeout()
{
	longjmp(CtxIncludeTimeout, 1);
}
/*
**  SENDTOARGV -- send to an argument vector.
**
**	Parameters:
**		argv -- argument vector to send to.
**		e -- the current envelope.
**
**	Returns:
**		none.
**
**	Side Effects:
**		puts all addresses on the argument vector onto the
**			send queue.
*/

sendtoargv(argv, e)
	register char **argv;
	register ENVELOPE *e;
{
	register char *p;

	while ((p = *argv++) != NULL)
	{
		sendto(p, 0, (ADDRESS *) NULL, 0);
	}
}
/*
**  GETCTLADDR -- get controlling address from an address header.
**
**	If none, get one corresponding to the effective userid.
**
**	Parameters:
**		a -- the address to find the controller of.
**
**	Returns:
**		the controlling address.
**
**	Side Effects:
**		none.
*/

ADDRESS *
getctladdr(a)
	register ADDRESS *a;
{
	while (a != NULL && !bitset(QGOODUID, a->q_flags))
		a = a->q_alias;
	return (a);
}
