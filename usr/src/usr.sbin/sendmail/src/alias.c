/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
*/

#ifndef lint
# ifdef DBM
static char	SccsId[] = "@(#)alias.c	5.2 (Berkeley) %G%	(with DBM)";
# else DBM
static char	SccsId[] = "@(#)alias.c	5.2 (Berkeley) %G%	(without DBM)";
# endif DBM
#endif not lint

# include <pwd.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <signal.h>
# include <errno.h>
# include "sendmail.h"
# ifdef FLOCK
# include <sys/file.h>
# endif FLOCK


/*
**  ALIAS -- Compute aliases.
**
**	Scans the alias file for an alias for the given address.
**	If found, it arranges to deliver to the alias list instead.
**	Uses libdbm database if -DDBM.
**
**	Parameters:
**		a -- address to alias.
**		sendq -- a pointer to the head of the send queue
**			to put the aliases in.
**
**	Returns:
**		none
**
**	Side Effects:
**		Aliases found are expanded.
**
**	Notes:
**		If NoAlias (the "-n" flag) is set, no aliasing is
**			done.
**
**	Deficiencies:
**		It should complain about names that are aliased to
**			nothing.
*/


#ifdef DBM
typedef struct
{
	char	*dptr;
	int	dsize;
} DATUM;
extern DATUM fetch();
#endif DBM

alias(a, sendq)
	register ADDRESS *a;
	ADDRESS **sendq;
{
	register char *p;
	extern ADDRESS *sendto();
	extern char *aliaslookup();

	if (NoAlias)
		return;
# ifdef DEBUG
	if (tTd(27, 1))
		printf("alias(%s)\n", a->q_paddr);
# endif

	/* don't realias already aliased names */
	if (bitset(QDONTSEND, a->q_flags))
		return;

	CurEnv->e_to = a->q_paddr;

	/*
	**  Look up this name
	*/

	p = aliaslookup(a->q_user);
	if (p == NULL)
		return;

	/*
	**  Match on Alias.
	**	Deliver to the target list.
	*/

# ifdef DEBUG
	if (tTd(27, 1))
		printf("%s (%s, %s) aliased to %s\n",
		    a->q_paddr, a->q_host, a->q_user, p);
# endif
	message(Arpa_Info, "aliased to %s", p);
	AliasLevel++;
	a->q_child = sendto(p, 1, a, 0);
	AliasLevel--;
}
/*
**  ALIASLOOKUP -- look up a name in the alias file.
**
**	Parameters:
**		name -- the name to look up.
**
**	Returns:
**		the value of name.
**		NULL if unknown.
**
**	Side Effects:
**		none.
**
**	Warnings:
**		The return value will be trashed across calls.
*/

char *
aliaslookup(name)
	char *name;
{
# ifdef DBM
	DATUM rhs, lhs;

	/* create a key for fetch */
	lhs.dptr = name;
	lhs.dsize = strlen(name) + 1;
	rhs = fetch(lhs);
	return (rhs.dptr);
# else DBM
	register STAB *s;

	s = stab(name, ST_ALIAS, ST_FIND);
	if (s == NULL)
		return (NULL);
	return (s->s_alias);
# endif DBM
}
/*
**  INITALIASES -- initialize for aliasing
**
**	Very different depending on whether we are running DBM or not.
**
**	Parameters:
**		aliasfile -- location of aliases.
**		init -- if set and if DBM, initialize the DBM files.
**
**	Returns:
**		none.
**
**	Side Effects:
**		initializes aliases:
**		if DBM:  opens the database.
**		if ~DBM: reads the aliases into the symbol table.
*/

# define DBMMODE	0666

initaliases(aliasfile, init)
	char *aliasfile;
	bool init;
{
#ifdef DBM
	int atcnt;
	char buf[MAXNAME];
	time_t modtime;
#endif DBM
	struct stat stb;

	if (aliasfile == NULL || stat(aliasfile, &stb) < 0)
	{
		NoAlias = TRUE;
		errno = 0;
		return;
	}

# ifdef DBM
	/*
	**  Check to see that the alias file is complete.
	**	If not, we will assume that someone died, and it is up
	**	to us to rebuild it.
	*/

	dbminit(aliasfile);
	atcnt = SafeAlias * 2;
	if (atcnt > 0)
	{
		while (!init && atcnt-- >= 0 && aliaslookup("@") == NULL)
			sleep(30);
	}
	else
		atcnt = 1;

	/*
	**  See if the DBM version of the file is out of date with
	**  the text version.  If so, go into 'init' mode automatically.
	**	This only happens if our effective userid owns the DBM
	**	version or if the mode of the database is 666 -- this
	**	is an attempt to avoid protection problems.  Note the
	**	unpalatable hack to see if the stat succeeded.
	*/

	modtime = stb.st_mtime;
	(void) strcpy(buf, aliasfile);
	(void) strcat(buf, ".pag");
	stb.st_ino = 0;
	if (!init && (stat(buf, &stb) < 0 || stb.st_mtime < modtime || atcnt < 0))
	{
		errno = 0;
		if (AutoRebuild && stb.st_ino != 0 &&
		    ((stb.st_mode & 0777) == 0666 || stb.st_uid == geteuid()))
		{
			init = TRUE;
			message(Arpa_Info, "rebuilding alias database");
		}
		else
		{
#ifdef LOG
			syslog(LOG_INFO, "alias database out of date");
#endif LOG
			message(Arpa_Info, "Warning: alias database out of date");
		}
	}


	/*
	**  If necessary, load the DBM file.
	**	If running without DBM, load the symbol table.
	*/

	if (init)
	{
		readaliases(aliasfile, TRUE);
	}
# else DBM
	readaliases(aliasfile, init);
# endif DBM
}
/*
**  READALIASES -- read and process the alias file.
**
**	This routine implements the part of initaliases that occurs
**	when we are not going to use the DBM stuff.
**
**	Parameters:
**		aliasfile -- the pathname of the alias file master.
**		init -- if set, initialize the DBM stuff.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Reads aliasfile into the symbol table.
**		Optionally, builds the .dir & .pag files.
*/

static
readaliases(aliasfile, init)
	char *aliasfile;
	bool init;
{
	register char *p;
	char *p2;
	char *lhs;
	char *rhs;
	bool skipping;
	int naliases, bytes, longest;
	FILE *af;
	int (*oldsigint)();
	ADDRESS al, bl;
	register STAB *s;
	char line[BUFSIZ];

	if ((af = fopen(aliasfile, "r")) == NULL)
	{
# ifdef DEBUG
		if (tTd(27, 1))
			printf("Can't open %s\n", aliasfile);
# endif
		errno = 0;
		NoAlias++;
		return;
	}

# ifdef DBM
# ifdef FLOCK
	/* see if someone else is rebuilding the alias file already */
	if (flock(fileno(af), LOCK_EX | LOCK_NB) < 0 && errno == EWOULDBLOCK)
	{
		/* yes, they are -- wait until done and then return */
		message(Arpa_Info, "Alias file is already being rebuilt");
		if (OpMode != MD_INITALIAS)
		{
			/* wait for other rebuild to complete */
			(void) flock(fileno(af), LOCK_EX);
		}
		fclose(af);
		errno = 0;
		return;
	}
# endif FLOCK
# endif DBM

	/*
	**  If initializing, create the new DBM files.
	*/

	if (init)
	{
		oldsigint = signal(SIGINT, SIG_IGN);
		(void) strcpy(line, aliasfile);
		(void) strcat(line, ".dir");
		if (close(creat(line, DBMMODE)) < 0)
		{
			syserr("cannot make %s", line);
			(void) signal(SIGINT, oldsigint);
			return;
		}
		(void) strcpy(line, aliasfile);
		(void) strcat(line, ".pag");
		if (close(creat(line, DBMMODE)) < 0)
		{
			syserr("cannot make %s", line);
			(void) signal(SIGINT, oldsigint);
			return;
		}
	}

	/*
	**  Read and interpret lines
	*/

	FileName = aliasfile;
	LineNumber = 0;
	naliases = bytes = longest = 0;
	skipping = FALSE;
	while (fgets(line, sizeof (line), af) != NULL)
	{
		int lhssize, rhssize;

		LineNumber++;
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
				syserr("Non-continuation line starts with space");
			skipping = TRUE;
			continue;
		}
		skipping = FALSE;

		/*
		**  Process the LHS
		**
		**	Find the final colon, and parse the address.
		**	It should resolve to a local name.
		**
		**	Alternatively, it can be "@hostname" for host
		**	aliases -- all we do here is map case.  Hostname
		**	need not be a single token.
		*/

		for (p = line; *p != '\0' && *p != ':' && *p != '\n'; p++)
			continue;
		if (*p != ':')
		{
			syserr("%s, line %d: syntax error", aliasfile, lineno);
			continue;
		}
		*p++ = '\0';
		if (line[0] == '@')
		{
			/* a host alias */
			makelower(line);
			lhs = line;
		}
		else
		{
			/* a user alias */
			if (parseaddr(line, &al, 1, ':') == NULL)
			{
				syserr("illegal alias name");
				continue;
			}
			loweraddr(&al);
			if (al.q_mailer != LocalMailer)
			{
				syserr("cannot alias non-local names");
				continue;
			}
			lhs = al.q_user;
		}

		/*
		**  Process the RHS.
		**	'al' is the internal form of the LHS address.
		**	'p' points to the text of the RHS.
		**		'p' may begin with a colon (i.e., the
		**		separator was "::") which will use the
		**		first address as the person to send
		**		errors to -- i.e., designates the
		**		list maintainer.
		*/

		if (*p == ':')
		{
			ADDRESS *maint;

			while (isspace(*++p))
				continue;
			rhs = p;
			while (*p != '\0' && *p != ',')
				p++;
			if (*p != ',')
				goto syntaxerr;
			*p++ = '\0';
			maint = parse(p, (ADDRESS *) NULL, 1);
			if (maint == NULL)
				syserr("Illegal list maintainer for list %s", al.q_user);
			else if (CurEnv->e_returnto == &CurEnv->e_from)
			{
				CurEnv->e_returnto = maint;
				MailBack++;
			}
		}
			
		rhs = p;
		for (;;)
		{
			register char c;

			if (init)
			{
				/* do parsing & compression of addresses */
				c = *p;
				while (c != '\0')
				{
					p2 = p;
					while (*p != '\n' && *p != ',' && *p != '\0')
						p++;
					c = *p;
					if (c == '\n')
						c = '\0';
					*p = '\0';
					if (*p2 != '\0')
						(void) parseaddr(p2, &bl, -1, ',');
					if (c != '\0')
						*p++ = c;
				}
			}
			else
			{
				p = &p[strlen(p)];
				if (p[-1] == '\n')
					*--p = '\0';
			}

			/* see if there should be a continuation line */
			c = fgetc(af);
			if (!feof(af))
				(void) ungetc(c, af);
			if (c != ' ' && c != '\t')
				break;

			/* read continuation line */
			if (fgets(p, sizeof line - (p - line), af) == NULL)
				break;
			LineNumber++;
		}

		/*
		**  Insert alias into symbol table or DBM file
		*/

		lhssize = strlen(lhs) + 1;
		rhssize = strlen(rhs) + 1;

# ifdef DBM
		if (init)
		{
			DATUM key, content;

			key.dsize = lhssize;
			key.dptr = al.q_user;
			content.dsize = rhssize;
			content.dptr = rhs;
			store(key, content);
		}
		else
# endif DBM
		{
			s = stab(al.q_user, ST_ALIAS, ST_ENTER);
			s->s_alias = newstr(rhs);
		}

		/* statistics */
		naliases++;
		bytes += lhssize + rhssize;
		if (rhssize > longest)
			longest = rhssize;
	}

# ifdef DBM
	if (init)
	{
		/* add the distinquished alias "@" */
		DATUM key;

		key.dsize = 2;
		key.dptr = "@";
		store(key, key);

		/* restore the old signal */
		(void) signal(SIGINT, oldsigint);
	}
# endif DBM

	/* closing the alias file drops the lock */
	(void) fclose(af);
	CurEnv->e_to = NULL;
	FileName = NULL;
	message(Arpa_Info, "%d aliases, longest %d bytes, %d bytes total",
			naliases, longest, bytes);
}
/*
**  FORWARD -- Try to forward mail
**
**	This is similar but not identical to aliasing.
**
**	Parameters:
**		user -- the name of the user who's mail we would like
**			to forward to.  It must have been verified --
**			i.e., the q_home field must have been filled
**			in.
**		sendq -- a pointer to the head of the send queue to
**			put this user's aliases in.
**
**	Returns:
**		none.
**
**	Side Effects:
**		New names are added to send queues.
*/

forward(user, sendq)
	ADDRESS *user;
	ADDRESS **sendq;
{
	char buf[60];
	extern bool safefile();

# ifdef DEBUG
	if (tTd(27, 1))
		printf("forward(%s)\n", user->q_paddr);
# endif DEBUG

	if (user->q_mailer != LocalMailer || bitset(QBADADDR, user->q_flags))
		return;
# ifdef DEBUG
	if (user->q_home == NULL)
		syserr("forward: no home");
# endif DEBUG

	/* good address -- look for .forward file in home */
	define('z', user->q_home, CurEnv);
	expand("\001z/.forward", buf, &buf[sizeof buf - 1], CurEnv);
	if (!safefile(buf, user->q_uid, S_IREAD))
		return;

	/* we do have an address to forward to -- do it */
	include(buf, "forwarding", user, sendq);
}
/*
**  MAPHOST -- given a host description, produce a mapping.
**
**	This is done by looking up the name in the alias file,
**	preceeded by an "@".  This can be used for UUCP mapping.
**	For example, a call with {blia, ., UUCP} as arguments
**	might return {ucsfcgl, !, blia, ., UUCP} as the result.
**
**	We first break the input into three parts -- before the
**	lookup, the lookup itself, and after the lookup.  We
**	then do the lookup, concatenate them together, and rescan
**	the result.
**
**	Parameters:
**		pvp -- the parameter vector to map.
**
**	Returns:
**		The result of the mapping.  If nothing found, it
**		should just concatenate the three parts together and
**		return that.
**
**	Side Effects:
**		none.
*/

char **
maphost(pvp)
	char **pvp;
{
	register char **avp;
	register char **bvp;
	char *p;
	char buf1[MAXNAME];
	char buf2[MAXNAME];
	char buf3[MAXNAME];
	extern char **prescan();

	/*
	**  Extract the three parts of the input as strings.
	*/

	/* find the part before the lookup */
	for (bvp = pvp; *bvp != NULL && **bvp != MATCHLOOKUP; bvp++)
		continue;
	if (*bvp == NULL)
		return (pvp);
	p = *bvp;
	*bvp = NULL;
	cataddr(pvp, buf1, sizeof buf1);
	*bvp++ = p;

	/* find the rest of the lookup */
	for (avp = bvp; *pvp != NULL && **bvp != MATCHELOOKUP; bvp++)
		continue;
	if (*bvp == NULL)
		return (pvp);
	p = *bvp;
	*bvp = NULL;
	cataddr(avp, buf2, sizeof buf2);
	*bvp++ = p;

	/* save the part after the lookup */
	cataddr(bvp, buf3, sizeof buf3);

	/*
	**  Now look up the middle part.
	*/

	p = aliaslookup(buf2);
	if (p != NULL)
		strcpy(buf2, p);

	/*
	**  Put the three parts back together and break into tokens.
	*/

	strcat(buf1, buf2);
	strcat(buf1, buf3);
	avp = prescan(buf1, '\0');

	/* return this mapping */
	return (avp);
}
