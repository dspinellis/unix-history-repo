# include <stdio.h>
# include <ctype.h>
# include <pwd.h>
# include "dlvrmail.h"

static char SccsId[] = "@(#)alias.c	1.8	10/28/80";

/*
**  ALIAS -- Compute aliases.
**
**	Scans the file ALIASFILE for a set of aliases.
**	If found, it arranges to deliver to them by inserting the
**	new names onto the SendQ queue.  Uses libdbm database if -DDBM.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Aliases found on SendQ are removed and put onto
**		AliasQ; replacements are added to SendQ.  This is
**		done until no such replacement occurs.
**
**	Defined Constants:
**		MAXRCRSN -- the maximum recursion depth.
**
**	Called By:
**		main
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
typedef struct {char *dptr; int dsize;} datum;
datum lhs, rhs;
extern datum fetch();
#endif DBM

alias()
{
	register addrq *q;
	addrq *q2;
	FILE *af;
	char line[MAXLINE+1];
	register char *p;
	extern int errno;
	bool didalias;
	bool gotmatch;
	auto addrq al;
	extern bool sameaddr();
	extern addrq *parse();

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
			**  Check to see if this pseudonym exists in SendQ.
			**	Turn the alias into canonical form.
			**	Then scan SendQ until you do (or do not)
			**	find that address.
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

			/*  Scan SendQ for that canonical form. */
			for (q = &SendQ; (q = nxtinq(q)) != NULL; )
			{
				if (sameaddr(&al, q, TRUE))
					break;
			}
			if (q != NULL)
			{
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
				tkoffq(q, &SendQ);
				putonq(q, &AliasQ);
				didalias++;
				gotmatch++;
				sendto(p, 1);
			}
		}
	} while (didalias);
	fclose(af);
#else DBM
	/*
	**  Scan SendQ
	**	We only have to do this once, since anything we alias
	**	two is being put at the end of the queue we are
	**	scanning.
	*/

	for (q2 = nxtinq(&SendQ); (q = q2) != NULL; )
	{
		/* save ptr to next address */
		q2 = nxtinq(q);

		/* only alias local users */
		if (q->q_mailer != &Mailer[0])
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
		tkoffq(q, &SendQ);
		putonq(q, &AliasQ);
		sendto(p, 1);

		/* if our last entry had an alias, process them */
		if (q2 == NULL)
			q2 = nxtinq(&SendQ);
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
**		New names are added to SendQ.
**
**	Called By:
**		recipient
*/

bool
forward(user)
	addrq *user;
{
	return (FALSE);
}
