# include <stdio.h>
# include <ctype.h>
# include <pwd.h>
# include "dlvrmail.h"

/*
**  ALIAS -- Compute aliases.
**
**	Scans the file /usr/lib/mailaliases for a set of aliases.
**	If found, it arranges to deliver to them by inserting the
**	new names onto the SendQ queue.
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
**		ALIASFILE -- the pathname of the alias file.
**
**	Requires:
**		fopen (stdio)
**		fgets (stdio)
**		rewind (stdio)
**		isspace (sys)
**		printf (sys)
**		sendto
**		syserr
**		parse
**		nxtinq
**		sameaddr
**		tkoffq
**		putonq
**		fclose (sys)
**
**	Called By:
**		main
**
**	Files:
**		/usr/lib/mailaliases -- the mail aliases.
**
**	Notes:
**		If NoAlias (the "-n" flag) is set, no aliasing is
**			done.
**
**	Deficiencies:
**		It should complain about names that are aliased to
**			nothing.
**		It is unsophisticated about line overflows.
**
**	History:
**		3/5/80 -- extensive mods to change internal address
**			format.
**		12/27/79 -- written.
*/


# define ALIASFILE	"/usr/lib/mailaliases"
# define MAXRCRSN	10


alias()
{
	register addrq *q;
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

	didalias = TRUE;
	while (didalias)
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
# ifdef DEBUG
					if (Debug)
						printf("   ... also aliased to %s", line);
# endif
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
	}
	fclose(af);
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
**	Requires:
**		none
**
**	Called By:
**		recipient
**
**	History:
**		3/5/80 -- return value changed.
**		1/23/80 -- null version written.
*/

bool
forward(user)
	addrq *user;
{
	return (FALSE);
}
