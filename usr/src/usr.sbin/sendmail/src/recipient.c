# include <stdio.h>
# include <pwd.h>
# include <signal.h>
# include <ctype.h>
# include <errno.h>
# include "sendmail.h"
# ifdef LOG
# include <syslog.h>
# endif LOG

static char SccsId[] = "@(#)recipient.c	3.3	%G%";

/*
**  SENDTO -- Designate a send list.
**
**	The parameter is a comma-separated list of people to send to.
**	This routine arranges to send to all of them.
**
**	Parameters:
**		list -- the send list.
**		copyf -- the copy flag; passed to parse.
**
**	Returns:
**		none
**
**	Side Effects:
**		none.
*/

# define MAXRCRSN	10

sendto(list, copyf)
	char *list;
	int copyf;
{
	register char *p;
	register char *q;
	register char c;
	ADDRESS *a;
	bool more;

	/* more keeps track of what the previous delimiter was */
	more = TRUE;
	for (p = list; more; )
	{
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

		/* parse the address */
		if ((a = parse(q, (ADDRESS *) NULL, copyf)) == NULL)
			continue;

		/* arrange to send to this person */
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
	register struct mailer *m;
	char buf[MAXNAME];

	To = a->q_paddr;
	m = Mailer[a->q_mailer];
	errno = 0;
# ifdef DEBUG
	if (Debug)
		printf("recipient(%s)\n", To);
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

	if (a->q_mailer == M_LOCAL)
	{
		if (a->q_user[0] == '|')
		{
			a->q_mailer = M_PROG;
			m = Mailer[M_PROG];
			a->q_user++;
		}
	}

	/*
	**  Look up this person in the recipient list.  If they
	**  are there already, return, otherwise continue.
	**  If the list is empty, just add it.
	*/

	if (m->m_sendq == NULL)
	{
		m->m_sendq = a;
	}
	else
	{
		ADDRESS *pq;

		for (q = m->m_sendq; q != NULL; pq = q, q = q->q_next)
		{
			if (!ForceMail && sameaddr(q, a, FALSE))
			{
# ifdef DEBUG
				if (Debug)
					printf("(%s in sendq)\n", a->q_paddr);
# endif DEBUG
				if (Verbose && !bitset(QDONTSEND, a->q_flags))
					message(Arpa_Info, "duplicate supressed");
				return;
			}
		}

		/* add address on list */
		q = pq;
		q->q_next = a;
	}
	a->q_next = NULL;

	/*
	**  Alias the name and handle :include: specs.
	*/

	if (a->q_mailer == M_LOCAL)
	{
		if (strncmp(a->q_user, ":include:", 9) == 0)
		{
			a->q_flags |= QDONTSEND;
			if (Verbose)
				message(Arpa_Info, "including file %s", &a->q_user[9]);
			include(&a->q_user[9], " sending");
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

	if (!bitset(QDONTSEND, a->q_flags) && a->q_mailer == M_LOCAL)
	{
		char buf[MAXNAME];

		strcpy(buf, a->q_user);
		stripquotes(buf, TRUE);

		/* see if this is to a file */
		if (index(buf, '/') != NULL)
		{
			if (access(buf, 2) < 0)
			{
				a->q_flags |= QBADADDR;
				giveresponse(EX_CANTCREAT, TRUE, m);
			}
		}
		else
		{
			register struct passwd *pw;
			extern struct passwd *getpwnam();
			pw = getpwnam(buf);
			if (pw == NULL)
			{
				a->q_flags |= QBADADDR;
				giveresponse(EX_NOUSER, TRUE, m);
			}
			else
			{
				a->q_home = newstr(pw->pw_dir);
				if (strcmp(buf, a->q_user) == 0)
					forward(a);
			}
		}
	}
}
/*
**  INCLUDE -- handle :include: specification.
**
**	Parameters:
**		fname -- filename to include.
**		msg -- message to print in verbose mode.
**
**	Returns:
**		none.
**
**	Side Effects:
**		reads the :include: file and sends to everyone
**		listed in that file.
*/

include(fname, msg)
	char *fname;
	char *msg;
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
		sendto(buf, 1);
		AliasLevel--;
	}

	fclose(fp);
}
