# include <stdio.h>
# include <ctype.h>
# include <pwd.h>
# include "sendmail.h"

static char SccsId[] = "@(#)alias.c	3.7	%G%";

/*
**  ALIAS -- Compute aliases.
**
**	Scans the file ALIASFILE for a set of aliases.
**	If found, it arranges to deliver to them.  Uses libdbm
**	database if -DDBM.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Aliases found are expanded.
**
**	Defined Constants:
**		MAXRCRSN -- the maximum recursion depth.
**
**	Files:
**		ALIASFILE -- the mail aliases.  The format is
**			a series of lines of the form:
**				alias:name1,name2,name3,...
**			where 'alias' expands to all of
**			'name[i]'.  Continuations begin with
**			space or tab.
**		ALIASFILE.pag, ALIASFILE.dir: libdbm version
**			of alias file.  Keys are aliases, datums
**			(data?) are name1,name2, ...
**
**	Notes:
**		If NoAlias (the "-n" flag) is set, no aliasing is
**			done.
**
**	Deficiencies:
**		It should complain about names that are aliased to
**			nothing.
**		It is unsophisticated about line overflows.
*/


# define MAXRCRSN	10

#ifdef DBM
typedef struct
{
	char	*dptr;
	int dsize;
} datum;
datum lhs, rhs;
extern datum fetch();
#endif DBM

alias()
{
	register ADDRESS *q;
	ADDRESS *q2;
	FILE *af;
	char line[MAXLINE+1];
	register char *p;
	extern int errno;
	bool didalias;
	bool gotmatch;
	auto ADDRESS al;
	extern bool sameaddr();
	extern ADDRESS *parse();
	int mno;

	if (NoAlias)
		return;
# ifdef DEBUG
	if (Debug)
		printf("--- alias ---\n");
# endif

	/* open alias file if not already open */
#ifndef DBM
# ifdef DEBUG
	if (Debug && (af = fopen("mailaliases", "r")) != NULL)
		printf(" [using local alias file]\n");
	else
# endif
	if ((af = fopen(ALIASFILE, "r")) == NULL)
	{
# ifdef DEBUG
		if (Debug)
			printf("Can't open %s\n", ALIASFILE);
# endif
		errno = 0;
		return;
	}
#else DBM
	dbminit(ALIASFILE);
#endif DBM

#ifndef DBM
	/*
	**  Scan alias file.
	**	If we find any user that any line matches any user, we
	**	will send to the line rather than to the user.
	**
	**	We pass through the file several times.  Didalias tells
	**	us if we took some alias on this pass through the file;
	**	when it goes false at the top of the loop we don't have
	**	to scan any more.  Gotmatch tells the same thing, but
	**	on a line-by-line basis; it is used for processing
	**	continuation lines.
	*/

	do
	{
		didalias = FALSE;
		gotmatch = FALSE;
		rewind(af);
		while (fgets(line, sizeof line, af) != NULL)
		{
			/* comments begin with `#' */
			if (line[0] == '#')
				continue;

			/* check for continuation lines */
			if (isspace(line[0]))
			{
				if (gotmatch)
				{
					sendto(line, 1);
				}
				continue;
			}
			gotmatch = FALSE;

			/*
			**  Check to see if this pseudonym exists.
			**	Turn the alias into canonical form.
			**	Then scan the send queue until you
			**	do (or do not) find that address.
			*/

			/*  Get a canonical form for the alias. */
			for (p = line; *p != '\0' && *p != ':' && *p != '\n'; p++)
				continue;
			if (*p == '\0' || *p == '\n')
			{
			 syntaxerr:
				syserr("Bad alias line `%s'", line);
				continue;
			}
			*p++ = '\0';
			if (parse(line, &al, -1) == NULL)
			{
				*--p = ':';
				goto syntaxerr;
			}

			/* if already queued up, don't realias */
			for (q = Mailer[al.q_mailer]->m_sendq; q != NULL; q = q->q_next)
			{
				if (sameaddr(&al, q, TRUE))
					break;
			}
			if (q == NULL || bitset(QDONTSEND, q->q_flags))
				continue;

			/*
			**  Match on Alias.
			**	Deliver to the target list.
			**	Remove the alias from the send queue
			**	  and put it on the Alias queue.
			*/

# ifdef DEBUG
			if (Debug)
				printf("%s (%s, %s) aliased to %s (%s,%s,%s)\n",
				    q->q_paddr, q->q_host, q->q_user,
				    p, al.q_paddr, al.q_host, al.q_user);
# endif
			if (Verbose)
				message("050", "aliased to %s", p);
			q->q_flags |= QDONTSEND;
			didalias++;
			gotmatch++;
			sendto(p, 1);
		}
	} while (didalias);
	fclose(af);
#else DBM
	/*
	**  Scan send queues
	**	We only have to do this once, since anything we alias
	**	to is being put at the end of the queue we are
	**	scanning.
	*/

	for (mno = 0; Mailer[mno] != NULL; mno++)
	{
		for (q = Mailer[mno]->m_sendq; q != NULL; q = q->q_next)
		{
			To = q->q_paddr;

			/* don't realias already aliased names */
			if (bitset(QDONTSEND, q->q_flags))
				continue;

			/* only alias local users */
			if (q->q_mailer != M_LOCAL)
				continue;

			/* create a key for fetch */
			lhs.dptr = q->q_user;
			lhs.dsize = strlen(q->q_user) + 1;
			rhs = fetch(lhs);

			/* find this alias? */
			p = rhs.dptr;
			if (p == NULL)
				continue;

			/*
			**  Match on Alias.
			**	Deliver to the target list.
			**	Remove the alias from the send queue
			**	  and put it on the Alias queue.
			*/

# ifdef DEBUG
			if (Debug)
				printf("%s (%s, %s) aliased to %s\n",
				    q->q_paddr, q->q_host, q->q_user, p);
# endif
			if (Verbose)
				message("050", "aliased to %s", p);
			q->q_flags |= QDONTSEND;
			sendto(p, 1);
		}
	}
#endif DBM
}
/*
**  FORWARD -- Try to forward mail
**
**	This is similar but not identical to aliasing.
**
**	Currently it is undefined, until the protocol for userinfo
**	databases is finalized.
**
**	Parameters:
**		user -- the name of the user who's mail we
**			would like to forward to.
**
**	Returns:
**		TRUE -- we have forwarded it somewhere.
**		FALSE -- not forwarded; go ahead & deliver.
**
**	Side Effects:
**		New names are added to send queues.
*/

bool
forward(user)
	ADDRESS *user;
{
	char buf[60];
	register FILE *fp;
	register char *p;
	extern char *index();

	if (user->q_mailer != M_LOCAL || bitset(QBADADDR, user->q_flags))
		return (FALSE);

	/* good address -- look for .forward file in home */
	expand("$z/.forward", buf, &buf[sizeof buf - 1]);
	fp = fopen(buf, "r");
	if (fp == NULL)
		return (FALSE);

	/* we do have an address to forward to -- do it */
	fgets(buf, sizeof buf, fp);
	if ((p = index(buf, '\n')) != NULL)
		*p = '\0';
	fclose(fp);
	if (buf[0] == '\0')
		return (FALSE);
	if (Verbose)
		message("050", "forwarded to %s", buf);
	sendto(buf, 1);
	return (TRUE);
}
