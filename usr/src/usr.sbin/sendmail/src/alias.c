/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

# include <sys/types.h>
# include <sys/stat.h>
# include <signal.h>
# include "sendmail.h"
# include <sys/file.h>
# include <pwd.h>
# ifdef LOCKF
# include <fcntl.h>
# endif

# ifdef DBM
# error DBM is no longer supported -- use NDBM instead.
# endif

# ifdef NEWDB
# include <db.h>
# endif

# ifdef NDBM
# include <ndbm.h>
# endif

#ifndef lint
#ifdef NEWDB
static char sccsid[] = "@(#)alias.c	5.39 (Berkeley) %G% (with NEWDB)";
#else
#ifdef NDBM
static char sccsid[] = "@(#)alias.c	5.39 (Berkeley) %G% (with NDBM)";
#else
static char sccsid[] = "@(#)alias.c	5.39 (Berkeley) %G% (without NDBM)";
#endif
#endif
#endif /* not lint */
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


/*
**  Sun YP servers read the dbm files directly, so we have to build them
**  even if NEWDB
*/

#ifdef NDBM
# ifndef NEWDB
#  define IF_MAKEDBMFILES
# else
#  ifdef YPCOMPAT
#   define IF_MAKEDBMFILES		if (makedbmfiles)
#  endif
# endif
#endif

typedef union
{
#ifdef NDBM
	datum	dbm;
#endif
#ifdef NEWDB
	DBT	dbt;
#endif
	struct
	{
		char	*data;
		int	size;
	} xx;
} DBdatum;

#ifdef NEWDB
static DB	*AliasDBptr;
#endif
#ifdef NDBM
static DBM	*AliasDBMptr;
#endif

alias(a, sendq, e)
	register ADDRESS *a;
	ADDRESS **sendq;
	register ENVELOPE *e;
{
	register char *p;
	extern ADDRESS *sendto();
	extern char *aliaslookup();

	if (tTd(27, 1))
		printf("alias(%s)\n", a->q_paddr);

	/* don't realias already aliased names */
	if (bitset(QDONTSEND, a->q_flags))
		return;

	e->e_to = a->q_paddr;

	/*
	**  Look up this name
	*/

	if (NoAlias)
		p = NULL;
	else
		p = aliaslookup(a->q_user);
	if (p == NULL)
		return;

	/*
	**  Match on Alias.
	**	Deliver to the target list.
	*/

	if (tTd(27, 1))
		printf("%s (%s, %s) aliased to %s\n",
		    a->q_paddr, a->q_host, a->q_user, p);
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
# if defined(NEWDB) || defined(NDBM)
	DBdatum rhs, lhs;
	int s;

	/* create a key for fetch */
	lhs.xx.data = name;
	lhs.xx.size = strlen(name) + 1;
# ifdef NEWDB
	if (AliasDBptr != NULL)
	{
		s = AliasDBptr->get(AliasDBptr, &lhs.dbt, &rhs.dbt, 0);
		if (s == 0)
			return (rhs.dbt.data);
	}
# ifdef NDBM
	else
	{
		rhs.dbm = dbm_fetch(AliasDBMptr, lhs.dbm);
		return (rhs.dbm.dptr);
	}
# endif /* NDBM */
# else /* not NEWDB */
	rhs.dbm = dbm_fetch(AliasDBMptr, lhs.dbm);
	return (rhs.dbm.dptr);
# endif /* NEWDB */
# else /* neither NEWDB nor NDBM */
	register STAB *s;

	s = stab(name, ST_ALIAS, ST_FIND);
	if (s != NULL)
		return (s->s_alias);
# endif
	return (NULL);
}
/*
**  INITALIASES -- initialize for aliasing
**
**	Very different depending on whether we are running NDBM or not.
**
**	Parameters:
**		aliasfile -- location of aliases.
**		init -- if set and if NDBM, initialize the NDBM files.
**
**	Returns:
**		none.
**
**	Side Effects:
**		initializes aliases:
**		if NDBM:  opens the database.
**		if ~NDBM: reads the aliases into the symbol table.
*/

# define DBMMODE	0644

initaliases(aliasfile, init, e)
	char *aliasfile;
	bool init;
	register ENVELOPE *e;
{
#if defined(NDBM) || defined(NEWDB)
	int atcnt;
	time_t modtime;
	bool automatic = FALSE;
	char buf[MAXNAME];
#endif
	struct stat stb;
	static bool initialized = FALSE;
	static int readaliases();

	if (initialized)
		return;
	initialized = TRUE;

	if (aliasfile == NULL || stat(aliasfile, &stb) < 0)
	{
		if (aliasfile != NULL && init)
			syserr("Cannot open %s", aliasfile);
		NoAlias = TRUE;
		errno = 0;
		return;
	}

# if defined(NDBM) || defined(NEWDB)
	/*
	**  Check to see that the alias file is complete.
	**	If not, we will assume that someone died, and it is up
	**	to us to rebuild it.
	*/

	if (!init)
	{
# ifdef NEWDB
		(void) strcpy(buf, aliasfile);
		(void) strcat(buf, ".db");
		AliasDBptr = dbopen(buf, O_RDONLY, DBMMODE, DB_HASH, NULL);
		if (AliasDBptr == NULL)
		{
# ifdef NDBM
			AliasDBMptr = dbm_open(aliasfile, O_RDONLY, DBMMODE);
# else
			syserr("initaliases: cannot open %s", buf);
			NoAlias = TRUE;
			return;
# endif
		}
# else
		AliasDBMptr = dbm_open(aliasfile, O_RDONLY, DBMMODE);
# endif
	}
	atcnt = SafeAlias * 2;
	if (atcnt > 0)
	{
		while (!init && atcnt-- >= 0 && aliaslookup("@") == NULL)
		{
			/*
			**  Reinitialize alias file in case the new
			**  one is mv'ed in instead of cp'ed in.
			**
			**	Only works with new DBM -- old one will
			**	just consume file descriptors forever.
			**	If you have a dbmclose() it can be
			**	added before the sleep(30).
			*/

# ifdef NEWDB
			if (AliasDBptr != NULL)
				AliasDBptr->close(AliasDBptr);
# endif
# ifdef NDBM
			if (AliasDBMptr != NULL)
				dbm_close(AliasDBMptr);
# endif

			sleep(30);
# ifdef NEWDB
			(void) strcpy(buf, aliasfile);
			(void) strcat(buf, ".db");
			AliasDBptr =
			    dbopen(buf, O_RDONLY, DBMMODE, DB_HASH, NULL);
			if (AliasDBptr == NULL)
			{
# ifdef NDBM
				AliasDBMptr = dbm_open(aliasfile, O_RDONLY, DBMMODE);
# else
				syserr("initaliases: cannot open %s", buf);
				NoAlias = TRUE;
				return;
# endif
			}
# else
# ifdef NDBM
			AliasDBMptr = dbm_open(aliasfile, O_RDONLY, DBMMODE);
# endif
# endif
		}
	}
	else
		atcnt = 1;

	/*
	**  See if the NDBM version of the file is out of date with
	**  the text version.  If so, go into 'init' mode automatically.
	**	This only happens if our effective userid owns the DBM.
	**	Note the unpalatable hack to see if the stat succeeded.
	*/

	modtime = stb.st_mtime;
	(void) strcpy(buf, aliasfile);
# ifdef NEWDB
	(void) strcat(buf, ".db");
# else
	(void) strcat(buf, ".pag");
# endif
	stb.st_ino = 0;
	if (!init && (stat(buf, &stb) < 0 || stb.st_mtime < modtime || atcnt < 0))
	{
		errno = 0;
		if (AutoRebuild && stb.st_ino != 0 && stb.st_uid == geteuid())
		{
			init = TRUE;
			automatic = TRUE;
			message(Arpa_Info, "rebuilding alias database");
#ifdef LOG
			if (LogLevel >= 7)
				syslog(LOG_INFO, "rebuilding alias database");
#endif /* LOG */
		}
		else
		{
#ifdef LOG
			if (LogLevel >= 7)
				syslog(LOG_INFO, "alias database out of date");
#endif /* LOG */
			message(Arpa_Info, "Warning: alias database out of date");
		}
	}


	/*
	**  If necessary, load the NDBM file.
	**	If running without NDBM, load the symbol table.
	*/

	if (init)
	{
#ifdef LOG
		if (LogLevel >= 6)
		{
			extern char *username();

			syslog(LOG_NOTICE, "alias database %srebuilt by %s",
				automatic ? "auto" : "", username());
		}
#endif /* LOG */
		readaliases(aliasfile, TRUE, e);
	}
# else /* NDBM */
	readaliases(aliasfile, init, e);
# endif /* NDBM */
}
/*
**  READALIASES -- read and process the alias file.
**
**	This routine implements the part of initaliases that occurs
**	when we are not going to use the DBM stuff.
**
**	Parameters:
**		aliasfile -- the pathname of the alias file master.
**		init -- if set, initialize the NDBM stuff.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Reads aliasfile into the symbol table.
**		Optionally, builds the .dir & .pag files.
*/

static
readaliases(aliasfile, init, e)
	char *aliasfile;
	bool init;
	register ENVELOPE *e;
{
	register char *p;
	char *lhs;
	char *rhs;
	bool skipping;
	int naliases, bytes, longest;
	FILE *af;
	bool makedbmfiles;
	void (*oldsigint)();
	ADDRESS al, bl;
	register STAB *s;
# ifdef NEWDB
	DB *dbp;
# endif
# ifdef NDBM
	DBM *dbmp;
# endif
# ifdef LOCKF
	struct flock fld;
# endif
	char line[BUFSIZ];

	if ((af = fopen(aliasfile, "r+")) == NULL)
	{
		if (tTd(27, 1))
			printf("Can't open %s\n", aliasfile);
		errno = 0;
		NoAlias++;
		return;
	}

# if defined(NDBM) || defined(NEWDB)
	/* see if someone else is rebuilding the alias file already */
# ifdef LOCKF
	fld.l_type = F_WRLCK;
	fld.l_whence = fld.l_start = fld.l_len = 0;
	if (fcntl(fileno(af), F_SETLK, &fld) < 0)
# else
	if (flock(fileno(af), LOCK_EX | LOCK_NB) < 0 && errno == EWOULDBLOCK)
# endif
	{
		/* yes, they are -- wait until done and then return */
		message(Arpa_Info, "Alias file is already being rebuilt");
		if (OpMode != MD_INITALIAS)
		{
			/* wait for other rebuild to complete */
# ifdef LOCKF
			(void) fcntl(fileno(af), F_SETLKW, &fld);
# else
			(void) flock(fileno(af), LOCK_EX);
# endif
		}
		(void) fclose(af);
		errno = 0;
		return;
	}
# endif /* NDBM */

	/*
	**  If initializing, create the new DBM files.
	*/

	if (init)
	{
		oldsigint = signal(SIGINT, SIG_IGN);
# ifdef NEWDB
		(void) strcpy(line, aliasfile);
		(void) strcat(line, ".db");
		dbp = dbopen(line,
		    O_RDWR|O_CREAT|O_TRUNC, DBMMODE, DB_HASH, NULL);
		if (dbp == NULL)
		{
			syserr("readaliases: cannot create %s", line);
			(void) signal(SIGINT, oldsigint);
			return;
		}
# endif
# ifdef IF_MAKEDBMFILES
# ifdef NEWDB
		makedbmfiles = access("/var/yp/Makefile", R_OK) == 0;
# endif
		IF_MAKEDBMFILES
		{
			dbmp = dbm_open(aliasfile,
					       O_RDWR|O_CREAT|O_TRUNC, DBMMODE);
			if (dbmp == NULL)
			{
				syserr("readaliases: cannot create %s.{dir,pag}",
					aliasfile);
				(void) signal(SIGINT, oldsigint);
				return;
			}
		}
# endif
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
		p = strchr(line, '\n');
		if (p != NULL)
			*p = '\0';
		switch (line[0])
		{
		  case '#':
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

			if (init && CheckAliases)
			{
				/* do parsing & compression of addresses */
				while (*p != '\0')
				{
					extern char *DelimChar;

					while (isspace(*p) || *p == ',')
						p++;
					if (*p == '\0')
						break;
					if (parseaddr(p, &bl, -1, ',', e) == NULL)
						usrerr("%s... bad address", p);
					p = DelimChar;
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

# if defined(NDBM) || defined(NEWDB)
		if (init)
		{
			DBdatum key, content;

			key.xx.size = lhssize;
			key.xx.data = al.q_user;
			content.xx.size = rhssize;
			content.xx.data = rhs;
# ifdef NEWDB
			if (dbp->put(dbp, &key.dbt, &content.dbt, 0) != 0)
				syserr("readaliases: db put (%s)", al.q_user);
# endif
# ifdef IF_MAKEDBMFILES
			IF_MAKEDBMFILES
				if (dbm_store(dbmp, key.dbm, content.dbm, DBM_REPLACE) != 0)
					syserr("readaliases: dbm store (%s)",
						al.q_user);
# endif
		}
		else
# endif /* NDBM */
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

# if defined(NDBM) || defined(NEWDB)
	if (init)
	{
		/* add the distinquished alias "@" */
		DBdatum key;

		key.xx.size = 2;
		key.xx.data = "@";
# ifdef NEWDB
		if (dbp->sync(dbp) != 0 ||
		    dbp->put(dbp, &key.dbt, &key.dbt, 0) != 0 ||
		    dbp->close(dbp) != 0)
			syserr("readaliases: db close failure");
# endif
# ifdef IF_MAKEDBMFILES
		IF_MAKEDBMFILES
		{
			if (dbm_store(dbmp, key.dbm, key.dbm, DBM_REPLACE) != 0 ||
			    dbm_error(dbmp))
				syserr("readaliases: dbm close failure");
			dbm_close(dbmp);
		}
# endif

		/* restore the old signal */
		(void) signal(SIGINT, oldsigint);
	}
# endif /* NDBM */

	/* closing the alias file drops the lock */
	(void) fclose(af);
	e->e_to = NULL;
	FileName = NULL;
	message(Arpa_Info, "%d aliases, longest %d bytes, %d bytes total",
			naliases, longest, bytes);
# ifdef LOG
	if (LogLevel >= 8)
		syslog(LOG_INFO, "%d aliases, longest %d bytes, %d bytes total",
			naliases, longest, bytes);
# endif /* LOG */
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

forward(user, sendq, e)
	ADDRESS *user;
	ADDRESS **sendq;
	register ENVELOPE *e;
{
	char buf[60];
	extern bool safefile();

	if (tTd(27, 1))
		printf("forward(%s)\n", user->q_paddr);

	if (user->q_mailer != LocalMailer || bitset(QBADADDR, user->q_flags))
		return;
	if (user->q_home == NULL)
		syserr("forward: no home");

	/* good address -- look for .forward file in home */
	define('z', user->q_home, e);
	expand("\001z/.forward", buf, &buf[sizeof buf - 1], e);
	include(buf, TRUE, user, sendq, e);
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
