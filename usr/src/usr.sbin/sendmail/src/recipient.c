# include <pwd.h>
# include <sys/types.h>
# include <sys/stat.h>
# include "sendmail.h"

static char SccsId[] = "@(#)recipient.c	3.25	%G%";

/*
**  SENDTO -- Designate a send list.
**
**	The parameter is a comma-separated list of people to send to.
**	This routine arranges to send to all of them.
**
**	Parameters:
**		list -- the send list.
**		copyf -- the copy flag; passed to parse.
**		ctladdr -- the address template for the person to
**			send to -- effective uid/gid are important.
**
**	Returns:
**		none
**
**	Side Effects:
**		none.
*/

# define MAXRCRSN	10

sendto(list, copyf, ctladdr)
	char *list;
	int copyf;
	ADDRESS *ctladdr;
{
	register char *p;
	bool more;		/* set if more addresses to send to */
	ADDRESS *al;		/* list of addresses to send to */
	bool firstone;		/* set on first address sent */
	bool selfref;		/* set if this list includes ctladdr */

# ifdef DEBUG
	if (Debug > 1)
	{
		printf("sendto: %s\n   ctladdr=", list);
		printaddr(ctladdr, FALSE);
	}
# endif DEBUG

	more = TRUE;
	firstone = TRUE;
	selfref = FALSE;
	al = NULL;
	for (p = list; more; )
	{
		register char *q;
		register char c;
		ADDRESS *a;

		/* find the end of this address */
		while (*p == ' ' || *p == '\t')
			p++;
		q = p;
		while ((c = *p++) != '\0' && c != ',' && c != '\n')
			continue;
		more = c != '\0';
		*--p = '\0';
		if (more)
			p++;
		if (*q == '\0')
			continue;

		/* parse the address */
		if ((a = parse(q, (ADDRESS *) NULL, copyf)) == NULL)
			continue;
		a->q_next = al;
		a->q_alias = ctladdr;

		/* see if this should be marked as a primary address */
		if (ctladdr == NULL ||
		    (firstone && !more && bitset(QPRIMARY, ctladdr->q_flags)))
			a->q_flags |= QPRIMARY;

		/* put on send queue or suppress self-reference */
		if (ctladdr != NULL && sameaddr(ctladdr, a, FALSE))
			selfref = TRUE;
		else
			al = a;
		firstone = FALSE;
	}

	/* if this alias doesn't include itself, delete ctladdr */
	if (!selfref && ctladdr != NULL)
		ctladdr->q_flags |= QDONTSEND;

	/* arrange to send to everyone on the local send list */
	while (al != NULL)
	{
		register ADDRESS *a = al;

		al = a->q_next;
		recipient(a);
	}

	To = NULL;
}
/*
**  RECIPIENT -- Designate a message recipient
**
**	Saves the named person for future mailing.
**
**	Parameters:
**		a -- the (preparsed) address header for the recipient.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

recipient(a)
	register ADDRESS *a;
{
	register ADDRESS *q;
	ADDRESS **pq;
	register struct mailer *m;
	extern ADDRESS *getctladdr();

	To = a->q_paddr;
	m = Mailer[a->q_mailer];
	errno = 0;
# ifdef DEBUG
	if (Debug)
	{
		printf("\nrecipient: ");
		printaddr(a, FALSE);
	}
# endif DEBUG

	/* break aliasing loops */
	if (AliasLevel > MAXRCRSN)
	{
		usrerr("aliasing/forwarding loop broken");
		return;
	}

	/*
	**  Do sickly crude mapping for program mailing, etc.
	*/

	if (a->q_mailer == MN_LOCAL)
	{
		if (a->q_user[0] == '|')
		{
			a->q_mailer = MN_PROG;
			m = Mailer[MN_PROG];
			a->q_user++;
			if (a->q_alias == NULL && Debug == 0)
			{
				usrerr("Cannot mail directly to programs");
				a->q_flags |= QDONTSEND;
			}
		}
	}

	/*
	**  Look up this person in the recipient list.
	**	If they are there already, return, otherwise continue.
	**	If the list is empty, just add it.  Notice the cute
	**	hack to make from addresses suppress things correctly:
	**	the QDONTSEND bit will be set in the send list.
	**	[Please note: the emphasis is on "hack."]
	*/

	for (pq = &m->m_sendq; (q = *pq) != NULL; pq = &q->q_next)
	{
		if (!ForceMail && sameaddr(q, a, FALSE))
		{
# ifdef DEBUG
			if (Debug)
			{
				printf("%s in sendq: ", a->q_paddr);
				printaddr(q, FALSE);
			}
# endif DEBUG
			if (Verbose && !bitset(QDONTSEND, a->q_flags))
				message(Arpa_Info, "duplicate suppressed");
			if (!bitset(QPRIMARY, q->q_flags))
				q->q_flags |= a->q_flags;
			return;
		}
	}

	/* add address on list */
	*pq = a;
	a->q_next = NULL;
	if (DontSend)
		a->q_flags |= QDONTSEND;

	/*
	**  Alias the name and handle :include: specs.
	*/

	if (a->q_mailer == MN_LOCAL)
	{
		if (strncmp(a->q_user, ":include:", 9) == 0)
		{
			a->q_flags |= QDONTSEND;
			if (a->q_alias == NULL && Debug == 0)
				usrerr("Cannot mail directly to :include:s");
			else
			{
				if (Verbose)
					message(Arpa_Info, "including file %s", &a->q_user[9]);
				include(&a->q_user[9], " sending", a);
			}
		}
		else
			alias(a);
	}

	/*
	**  If the user is local and still being sent, verify that
	**  the address is good.  If it is, try to forward.
	**  If the address is already good, we have a forwarding
	**  loop.  This can be broken by just sending directly to
	**  the user (which is probably correct anyway).
	*/

	if (!bitset(QDONTSEND, a->q_flags) && a->q_mailer == MN_LOCAL)
	{
		char buf[MAXNAME];
		register char *p;
		struct stat stb;
		extern bool writable();
		bool quoted = FALSE;

		strcpy(buf, a->q_user);
		for (p = buf; *p != '\0' && !quoted; p++)
		{
			if (!isascii(*p))
				quoted = TRUE;
		}
		stripquotes(buf, TRUE);

		/* see if this is to a file */
		if ((p = rindex(buf, '/')) != NULL)
		{
			/* check if writable or creatable */
			if (a->q_alias == NULL && Debug == 0)
			{
				usrerr("Cannot mail directly to files");
				a->q_flags |= QDONTSEND;
			}
			else if ((stat(buf, &stb) >= 0) ? (!writable(&stb)) :
			    (*p = '\0', !safefile(buf, getruid(), S_IWRITE|S_IEXEC)))
			{
				a->q_flags |= QBADADDR;
				giveresponse(EX_CANTCREAT, TRUE, m);
			}
		}
		else
		{
			register struct passwd *pw;
			extern struct passwd *finduser();

			/* warning -- finduser may trash buf */
			pw = finduser(buf);
			if (pw == NULL)
			{
				a->q_flags |= QBADADDR;
				giveresponse(EX_NOUSER, TRUE, m);
			}
			else
			{
				if (strcmp(a->q_user, pw->pw_name) != 0)
				{
					a->q_user = newstr(pw->pw_name);
					strcpy(buf, pw->pw_name);
				}
				a->q_home = newstr(pw->pw_dir);
				a->q_uid = pw->pw_uid;
				a->q_gid = pw->pw_gid;
				a->q_flags |= QGOODUID;
				if (!quoted)
					forward(a);
			}
		}
	}
}
/*
**  FINDUSER -- find the password entry for a user.
**
**	This looks a lot like getpwnam, except that it may want to
**	do some fancier pattern matching in /etc/passwd.
**
**	Parameters:
**		name -- the name to match against.
**
**	Returns:
**		A pointer to a pw struct.
**		NULL if name is unknown or ambiguous.
**
**	Side Effects:
**		may modify name.
*/

struct passwd *
finduser(name)
	char *name;
{
	extern struct passwd *getpwent();
	register struct passwd *pw;
	register char *p;

	/*
	**  Make name canonical.
	*/

	for (p = name; *p != '\0'; p++)
	{
		if (*p == (SPACESUB & 0177) || *p == '_')
			*p = ' ';
	}

	setpwent();
	while ((pw = getpwent()) != NULL)
	{
		char buf[MAXNAME];
		extern bool sameword();

		if (strcmp(pw->pw_name, name) == 0)
			return (pw);
		buildfname(pw->pw_gecos, pw->pw_name, buf);
		if (index(buf, ' ') != NULL && sameword(buf, name))
		{
			if (Verbose)
				message(Arpa_Info, "sending to login name %s",
				    pw->pw_name);
			return (pw);
		}
	}
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
writable(s)
	register struct stat *s;
{
	int euid, egid;
	int bits;

	if (bitset(0111, s->st_mode))
		return (FALSE);
	euid = getruid();
	egid = getrgid();
	if (geteuid() == 0)
	{
		if (bitset(S_ISUID, s->st_mode))
			euid = s->st_uid;
		if (bitset(S_ISGID, s->st_mode))
			egid = s->st_gid;
	}

	if (euid == 0)
		return (TRUE);
	bits = S_IWRITE;
	if (euid != s->st_uid)
	{
		bits >>= 3;
		if (egid != s->st_gid)
			bits >>= 3;
	}
	return ((s->st_mode & bits) != 0);
}
/*
**  INCLUDE -- handle :include: specification.
**
**	Parameters:
**		fname -- filename to include.
**		msg -- message to print in verbose mode.
**		ctladdr -- address template to use to fill in these
**			addresses -- effective user/group id are
**			the important things.
**
**	Returns:
**		none.
**
**	Side Effects:
**		reads the :include: file and sends to everyone
**		listed in that file.
*/

include(fname, msg, ctladdr)
	char *fname;
	char *msg;
	ADDRESS *ctladdr;
{
	char buf[MAXLINE];
	register FILE *fp;
	char *oldto = To;

	fp = fopen(fname, "r");
	if (fp == NULL)
	{
		usrerr("Cannot open %s", fname);
		return;
	}
	if (getctladdr(ctladdr) == NULL)
	{
		struct stat st;

		if (fstat(fileno(fp), &st) < 0)
			syserr("Cannot fstat %s!", fname);
		ctladdr->q_uid = st.st_uid;
		ctladdr->q_gid = st.st_gid;
		ctladdr->q_flags |= QGOODUID;
	}

	/* read the file -- each line is a comma-separated list. */
	while (fgets(buf, sizeof buf, fp) != NULL)
	{
		register char *p = index(buf, '\n');

		if (p != NULL)
			*p = '\0';
		if (buf[0] == '\0')
			continue;
		To = oldto;
		if (Verbose)
			message(Arpa_Info, "%s to %s", msg, buf);
		AliasLevel++;
		sendto(buf, 1, ctladdr);
		AliasLevel--;
	}

	(void) fclose(fp);
}
/*
**  SENDTOARGV -- send to an argument vector.
**
**	Parameters:
**		argv -- argument vector to send to.
**
**	Returns:
**		none.
**
**	Side Effects:
**		puts all addresses on the argument vector onto the
**			send queue.
*/

sendtoargv(argv)
	register char **argv;
{
	register char *p;
	extern bool sameword();

	while ((p = *argv++) != NULL)
	{
		if (argv[0] != NULL && argv[1] != NULL && sameword(argv[0], "at"))
		{
			char nbuf[MAXNAME];

			if (strlen(p) + strlen(argv[1]) + 2 > sizeof nbuf)
				usrerr("address overflow");
			else
			{
				(void) strcpy(nbuf, p);
				(void) strcat(nbuf, "@");
				(void) strcat(nbuf, argv[1]);
				p = newstr(nbuf);
				argv += 2;
			}
		}
		sendto(p, 0, NULL);
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
