# include <stdio.h>
# include <ctype.h>
# include <pwd.h>
# include "sendmail.h"

static char SccsId[] = "@(#)alias.c	3.11	%G%";

/*
**  ALIAS -- Compute aliases.
**
**	Scans the file /usr/lib/aliases for a set of aliases.
**	If found, it arranges to deliver to them.  Uses libdbm
**	database if -DDBM.
**
**	Parameters:
**		a -- address to alias.
**
**	Returns:
**		none
**
**	Side Effects:
**		Aliases found are expanded.
**
**	Files:
**		/usr/lib/aliases -- the mail aliases.  The format is
**			a series of lines of the form:
**				alias:name1,name2,name3,...
**			where 'alias' expands to all of
**			'name[i]'.  Continuations begin with
**			space or tab.
**		/usr/lib/aliases.pag, /usr/lib/aliases.dir: libdbm version
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


#ifdef DBM
typedef struct
{
	char	*dptr;
	int dsize;
} datum;
datum lhs, rhs;
extern datum fetch();
#endif DBM

alias(a)
	register ADDRESS *a;
{
	register char *p;
# ifndef DBM
	register STAB *s;
# endif DBM

	if (NoAlias)
		return;
# ifdef DEBUG
	if (Debug)
		printf("alias(%s)\n", a->q_paddr);
# endif

	/* don't realias already aliased names */
	if (bitset(QDONTSEND, a->q_flags))
		return;

	To = a->q_paddr;

# ifdef DBM
	/* create a key for fetch */
	lhs.dptr = a->q_user;
	lhs.dsize = strlen(a->q_user) + 1;
	rhs = fetch(lhs);

	/* find this alias? */
	p = rhs.dptr;
	if (p == NULL)
		return;
# else DBM
	s = stab(a->q_user, ST_ALIAS, ST_FIND);
	if (s == NULL)
		return;
	p = s->s_alias;
# endif DBM

	/*
	**  Match on Alias.
	**	Deliver to the target list.
	*/

# ifdef DEBUG
	if (Debug)
		printf("%s (%s, %s) aliased to %s\n",
		    a->q_paddr, a->q_host, a->q_user, p);
# endif
	if (Verbose)
		message("050", "aliased to %s", p);
	a->q_flags |= QDONTSEND;
	AliasLevel++;
	sendto(p, 1);
	AliasLevel--;
}
/*
**  INITALIASES -- initialize for aliasing
**
**	Very different depending on whether we are running DBM or not.
**
**	Parameters:
**		aliasfile -- location of aliases.
**
**	Returns:
**		none.
**
**	Side Effects:
**		initializes aliases:
**		if DBM:  opens the database.
**		if ~DBM: reads the aliases into the symbol table.
*/

initaliases(aliasfile)
	char *aliasfile;
{
# ifdef DBM
	dbminit(aliasfile);
# else DBM
	char line[BUFSIZ];
	register char *p;
	char *p2;
	char *rhs;
	bool skipping;
	ADDRESS al, bl;
	extern char *prescan();
	bool contin;

	if ((af = fopen(aliasfile, "r")) == NULL)
	{
# ifdef DEBUG
		if (Debug)
			printf("Can't open %s\n", AliasFile);
# endif
		errno = 0;
		NoAlias++;
		return;
	}
	/* read and interpret lines */
	lineno = 0;
	skipping = FALSE;
	while (fgets(line, sizeof (line), af) != NULL)
	{
		lineno++;
		switch (line[0])
		{
		  case '#':
		  case '\n':
		  case '\0':
			skipping = FALSE;
			continue;

		  case ' ':
		  case '\t':
			if (!skipping)
				syserr("aliases: %d: Non-continuation line starts with space", lineno);
			skipping = TRUE;
			continue;
		}
		skipping = FALSE;

		/* process the LHS */
		for (p = line; *p != '\0' && *p != ':' && *p != '\n'; p++)
			continue;
		if (*p == '\0' || *p == '\n')
		{
		 syntaxerr:
			syserr("aliases: %d: missing colon", lineno);
			continue;
		}
		*p++ = '\0';
		if (parse(line, &al, 1) == NULL)
		{
			*--p = ':';
			goto syntaxerr;
		}
		rhs = p;
		contin = FALSE;
		for (;;)
		{
			register char c;

			/* do parsing & compression of addresses */
			c = *p;
			while (c != '\0')
			{
				p2 = p;
				while (*p != '\n' && *p != ',' && *p != '\0')
					p++;
				c = *p;
				*p++ = '\0';
				if (*p2 == '\0')
				{
					p[-1] = c;
					continue;
				}
				parse(p2, &bl, -1);
				contin = (c == ',');
				p[-1] = c;
				while (isspace(*p))
					p++;
			}

			/* see if there should be a continuation line */
			if (!contin)
				break;

			/* read continuation line */
			p--;
			if (fgets(p, sizeof line - (p - line), af) == NULL)
				break;
			lineno++;

			if (!isspace(*p))
				syserr("aliases: %d: continuation line missing", lineno);
		}
		if (al.q_mailer != M_LOCAL)
		{
			syserr("aliases: %d: cannot alias non-local names", lineno);
			continue;
		}
		s = stab(al.q_user, ST_ALIAS, rhs);
	}
	(void) fclose(af);
# endif DBM
}
/*
**  FORWARD -- Try to forward mail
**
**	This is similar but not identical to aliasing.
**
**	Parameters:
**		user -- the name of the user who's mail we
**			would like to forward to.
**
**	Returns:
**		none.
**
**	Side Effects:
**		New names are added to send queues.
**		Sets the QDONTSEND bit in addresses that are forwarded.
*/

forward(user)
	ADDRESS *user;
{
	char buf[60];
	register FILE *fp;
	register char *p;

# ifdef DEBUG
	if (Debug)
		printf("forward(%s)\n", user->q_paddr);
# endif DEBUG

	if (user->q_mailer != M_LOCAL || bitset(QBADADDR, user->q_flags))
		return;

	/* good address -- look for .forward file in home */
	define('z', user->q_home);
	(void) expand("$z/.forward", buf, &buf[sizeof buf - 1]);
	fp = fopen(buf, "r");
	if (fp == NULL)
		return;

	/* we do have an address to forward to -- do it */
	user->q_flags |= QDONTSEND;
	(void) fgets(buf, sizeof buf, fp);
	if ((p = index(buf, '\n')) != NULL)
		*p = '\0';
	(void) fclose(fp);
	if (buf[0] == '\0')
		return;
	if (Verbose)
		message("050", "forwarded to %s", buf);
	AliasLevel++;
	sendto(buf, 1);
	AliasLevel--;
	return;
}
