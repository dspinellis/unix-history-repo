/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

# include "sendmail.h"
# include <signal.h>
# include <pwd.h>
# ifdef DBM
ERROR: DBM is no longer supported -- use NDBM instead.
# endif
# ifdef NEWDB
# include <db.h>
# endif
# ifdef NDBM
# include <ndbm.h>
# endif
# ifdef NIS
# include <rpcsvc/ypclnt.h>
# endif

#ifndef lint
#ifdef NEWDB
#ifdef NDBM
static char sccsid[] = "@(#)alias.c	6.41 (Berkeley) %G% (with NEWDB and NDBM)";
#else
static char sccsid[] = "@(#)alias.c	6.41 (Berkeley) %G% (with NEWDB)";
#endif
#else
#ifdef NDBM
static char sccsid[] = "@(#)alias.c	6.41 (Berkeley) %G% (with NDBM)";
#else
static char sccsid[] = "@(#)alias.c	6.41 (Berkeley) %G% (without NEWDB or NDBM)";
#endif
#endif
#endif /* not lint */
/*
**  Alias data structures
*/
#define ALIASDB		struct _aliasdb


ALIASDB
{
	ALIASCLASS	*ad_class;	/* class of this database */
	char		*ad_name;	/* name of alias file */
	char		*ad_domain;	/* name of (NIS) domain */
	void		*ad_dbp;	/* ndbm/nis database pointer */
#ifdef NEWDB
	DB		*ad_ndbp;	/* newdb database pointer */
#endif
	short		ad_flags;	/* flag bits */
};

/* bits for ad_flags */
#define ADF_VALID	0x0001		/* database initialized */
#define ADF_WRITABLE	0x0002		/* open for write */
#define ADF_IMPLHASH	0x0004		/* IMPL: underlying hash database */
#define ADF_IMPLNDBM	0x0008		/* IMPL: underlying NDBM database */


ALIASCLASS
{
	char	*ac_name;		/* name of alias class */
	char	*(*ac_lookup)__P((ALIASDB *, char *, ENVELOPE *));
					/* lookup func */
	void	(*ac_store)__P((ALIASDB *, char *, char *, ENVELOPE *));
					/* database store func */
	bool	(*ac_init)__P((ALIASDB *, ENVELOPE *));
					/* initialization func */
	void	(*ac_rebuild)__P((ALIASDB *, FILE *, ENVELOPE *));
					/* initialization func */
	void	(*ac_close)__P((ALIASDB *, ENVELOPE *));
					/* close function */
	short	ac_flags;		/* flag bits */
};

/* bits for ac_flags */
#define ACF_BUILDABLE	0x0001		/* can build a cached version */


ALIASDB	AliasDB[MAXALIASDB + 1];	/* actual database list */
int	NAliasDBs;			/* number of alias databases */
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
**		e -- the current envelope.
**
**	Returns:
**		none
**
**	Side Effects:
**		Aliases found are expanded.
**
**	Deficiencies:
**		It should complain about names that are aliased to
**			nothing.
*/

alias(a, sendq, e)
	register ADDRESS *a;
	ADDRESS **sendq;
	register ENVELOPE *e;
{
	register char *p;
	int naliases;
	char *owner;
	char obuf[MAXNAME + 6];
	extern ADDRESS *sendto();
	extern char *aliaslookup();

	if (tTd(27, 1))
		printf("alias(%s)\n", a->q_paddr);

	/* don't realias already aliased names */
	if (bitset(QDONTSEND|QBADADDR|QVERIFIED, a->q_flags))
		return;

	if (NoAlias)
		return;

	e->e_to = a->q_paddr;

	/*
	**  Look up this name
	*/

	p = aliaslookup(a->q_user, e);
	if (p == NULL)
		return;

	/*
	**  Match on Alias.
	**	Deliver to the target list.
	*/

	if (tTd(27, 1))
		printf("%s (%s, %s) aliased to %s\n",
		    a->q_paddr, a->q_host, a->q_user, p);
	if (bitset(EF_VRFYONLY, e->e_flags))
	{
		a->q_flags |= QVERIFIED;
		e->e_nrcpts++;
		return;
	}
	message("aliased to %s", p);
#ifdef LOG
	if (LogLevel > 9)
		syslog(LOG_INFO, "%s: alias %s => %s", e->e_id, a->q_paddr, p);
#endif
	a->q_flags &= ~QSELFREF;
	AliasLevel++;
	a->q_child = sendto(p, 1, a, 0);
	AliasLevel--;
	if (naliases > 0 && !bitset(QSELFREF, a->q_flags))
	{
		if (tTd(27, 5))
		{
			printf("alias: QDONTSEND ");
			printaddr(a, FALSE);
		}
		a->q_flags |= QDONTSEND;
	}

	/*
	**  Look for owner of alias
	*/

	(void) strcpy(obuf, "owner-");
	if (strncmp(a->q_user, "owner-", 6) == 0)
		(void) strcat(obuf, "owner");
	else
		(void) strcat(obuf, a->q_user);
	if (!bitnset(M_USR_UPPER, a->q_mailer->m_flags))
		makelower(obuf);
	owner = aliaslookup(obuf, e);
	if (owner != NULL)
	{
		if (strchr(owner, ',') != NULL)
			owner = obuf;
		a->q_owner = newstr(owner);
	}
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
aliaslookup(name, e)
	char *name;
	ENVELOPE *e;
{
	register int dbno;
	register ALIASDB *ad;
	register char *p;

	for (dbno = 0; dbno < NAliasDBs; dbno++)
	{
		ad = &AliasDB[dbno];
		if (!bitset(ADF_VALID, ad->ad_flags))
			continue;
		p = (*ad->ad_class->ac_lookup)(ad, name, e);
		if (p != NULL)
			return p;
	}
	return NULL;
}
/*
**  SETALIAS -- set up an alias map
**
**	Called when reading configuration file.
**
**	Parameters:
**		spec -- the alias specification
**
**	Returns:
**		none.
*/

setalias(spec)
	char *spec;
{
	register char *p;
	register ALIASDB *ad;
	char *class;
	STAB *s;

	if (tTd(27, 8))
		printf("setalias(%s)\n", spec);

	if (NAliasDBs >= MAXALIASDB)
	{
		syserr("Too many alias databases defined, %d max", MAXALIASDB);
		return;
	}
	ad = &AliasDB[NAliasDBs];

	for (p = spec; *p != '\0'; p++)
	{
		if (strchr(" /:", *p) != NULL)
			break;
	}

	if (*p == ':')
	{
		/* explicit class listed */
		*p++ = '\0';
		class = spec;
		spec = p;
	}
	else
	{
		/* implicit class */
		class = "implicit";
	}

	/* look up class */
	s = stab(class, ST_ALIASCLASS, ST_FIND);
	if (s == NULL)
		syserr("Unknown alias class %s", class);
	else
	{
		ad->ad_class = s->s_aliasclass;
		ad->ad_name = newstr(spec);
		NAliasDBs++;
	}
}
/*
**  INITALIASES -- initialize for aliasing
**
**	Very different depending on whether we are running NDBM or not.
**
**	Parameters:
**		rebuild -- if TRUE, this rebuilds the cached versions.
**		e -- current envelope.
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

initaliases(rebuild, e)
	bool rebuild;
	register ENVELOPE *e;
{
	int dbno;
	register ALIASDB *ad;

	for (dbno = 0; dbno < NAliasDBs; dbno++)
	{
		ad = &AliasDB[dbno];

		if (tTd(27, 2))
			printf("initaliases(%s:%s)\n",
				ad->ad_class->ac_name, ad->ad_name);

		if (rebuild)
		{
			rebuildaliases(ad, FALSE, e);
		}
		else
		{
			if (ad->ad_class->ac_init(ad, e))
				ad->ad_flags |= ADF_VALID;
		}
	}
}
/*
**  ALIASWAIT -- wait for distinguished @:@ token to appear.
**
**	This can decide to reopen or rebuild the alias file
*/

aliaswait(ad, ext, e)
	ALIASDB *ad;
	char *ext;
	ENVELOPE *e;
{
	int atcnt;
	time_t mtime;
	struct stat stb;
	char buf[MAXNAME];

	if (tTd(27, 3))
		printf("aliaswait\n");

	atcnt = SafeAlias * 2;
	if (atcnt > 0)
	{
		while (atcnt-- >= 0 &&
		       ad->ad_class->ac_lookup(ad, "@", e) == NULL)
		{
			/*
			**  Close and re-open the alias database in case
			**  the one is mv'ed instead of cp'ed in.
			*/

			if (tTd(27, 2))
				printf("aliaswait: sleeping\n");

			ad->ad_class->ac_close(ad, e);
			sleep(30);
			ad->ad_class->ac_init(ad, e);
		}
	}

	/* see if we need to go into auto-rebuild mode */
	if (stat(ad->ad_name, &stb) < 0)
		return;
	mtime = stb.st_mtime;
	(void) strcpy(buf, ad->ad_name);
	(void) strcat(buf, ext);
	if (stat(buf, &stb) < 0 || stb.st_mtime < mtime || atcnt < 0)
	{
		/* database is out of date */
		if (AutoRebuild && stb.st_ino != 0 && stb.st_uid == geteuid())
		{
			message("auto-rebuilding alias database %s",
				ad->ad_name);
			rebuildaliases(ad, TRUE, e);
		}
		else
		{
#ifdef LOG
			if (LogLevel > 3)
				syslog(LOG_INFO, "alias database %s out of date",
					ad->ad_name);
#endif /* LOG */
			message("Warning: alias database %s out of date",
				ad->ad_name);
		}
	}
}
/*
**  REBUILDALIASES -- rebuild the alias database.
**
**	Parameters:
**		ad -- the database to rebuild.
**		automatic -- set if this was automatically generated.
**		e -- current envelope.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Reads the text version of the database, builds the
**		DBM or DB version.
*/

rebuildaliases(ad, automatic, e)
	register ALIASDB *ad;
	bool automatic;
	register ENVELOPE *e;
{
	FILE *af;
	void (*oldsigint)();

	if (!bitset(ACF_BUILDABLE, ad->ad_class->ac_flags))
		return;

#ifdef LOG
	if (LogLevel > 7)
	{
		extern char *username();

		syslog(LOG_NOTICE, "alias database %s %srebuilt by %s",
			ad->ad_name, automatic ? "auto" : "", username());
	}
#endif /* LOG */

	/* try to lock the source file */
	if ((af = fopen(ad->ad_name, "r+")) == NULL)
	{
		syserr("554 Can't open %s", ad->ad_name);
		printf("Can't open %s\n", ad->ad_name);
		errno = 0;
		return;
	}

	/* see if someone else is rebuilding the alias file */
	if (!lockfile(fileno(af), ad->ad_name, LOCK_EX|LOCK_NB))
	{
		/* yes, they are -- wait until done */
		message("Alias file %s is already being rebuilt",
			ad->ad_name);
		if (OpMode != MD_INITALIAS)
		{
			/* wait for other rebuild to complete */
			(void) lockfile(fileno(af), ad->ad_name,
					LOCK_EX);
		}
		(void) fclose(af);
		errno = 0;
		return;
	}

	oldsigint = signal(SIGINT, SIG_IGN);

	ad->ad_class->ac_rebuild(ad, af, e);

	/* close the file, thus releasing locks */
	fclose(af);

	/* add distinguished entries and close the database */
	if (bitset(ADF_VALID, ad->ad_flags))
		ad->ad_class->ac_close(ad, e);

	/* restore the old signal */
	(void) signal(SIGINT, oldsigint);
}
/*
**  READALIASES -- read and process the alias file.
**
**	This routine implements the part of initaliases that occurs
**	when we are not going to use the DBM stuff.
**
**	Parameters:
**		ad -- the alias database descriptor.
**		af -- file to read the aliases from.
**		e -- the current alias file.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Reads aliasfile into the symbol table.
**		Optionally, builds the .dir & .pag files.
*/

static
readaliases(ad, af, e)
	register ALIASDB *ad;
	FILE *af;
	register ENVELOPE *e;
{
	register char *p;
	char *lhs;
	char *rhs;
	bool skipping;
	long naliases, bytes, longest;
	ADDRESS al, bl;
	register STAB *s;
	char line[BUFSIZ];

	/*
	**  Read and interpret lines
	*/

	FileName = ad->ad_name;
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
				syserr("554 Non-continuation line starts with space");
			skipping = TRUE;
			continue;
		}
		skipping = FALSE;

		/*
		**  Process the LHS
		**
		**	Find the colon separator, and parse the address.
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

		while (isascii(*p) && isspace(*p))
			p++;
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
			register char *nlp;

			nlp = &p[strlen(p)];
			if (nlp[-1] == '\n')
				*--nlp = '\0';

			if (CheckAliases)
			{
				/* do parsing & compression of addresses */
				while (*p != '\0')
				{
					auto char *delimptr;

					while ((isascii(*p) && isspace(*p)) ||
								*p == ',')
						p++;
					if (*p == '\0')
						break;
					if (parseaddr(p, &bl, -1, ',', &delimptr, e) == NULL)
						usrerr("553 %s... bad address", p);
					p = delimptr;
				}
			}
			else
			{
				p = nlp;
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

			/* check for line overflow */
			if (strchr(p, '\n') == NULL)
			{
				usrerr("554 alias too long");
				break;
			}
		}

		/*
		**  Insert alias into symbol table or DBM file
		*/

		lhssize = strlen(lhs) + 1;

		lhssize = strlen(al.q_user);
		rhssize = strlen(rhs);
		ad->ad_class->ac_store(ad, al.q_user, rhs, e);

		if (al.q_paddr != NULL)
			free(al.q_paddr);
		if (al.q_host != NULL)
			free(al.q_host);
		if (al.q_user != NULL)
			free(al.q_user);

		/* statistics */
		naliases++;
		bytes += lhssize + rhssize;
		if (rhssize > longest)
			longest = rhssize;
	}

	e->e_to = NULL;
	FileName = NULL;
	message("%s: %d aliases, longest %d bytes, %d bytes total",
			ad->ad_name, naliases, longest, bytes);
# ifdef LOG
	if (LogLevel > 7)
		syslog(LOG_INFO, "%s: %d aliases, longest %d bytes, %d bytes total",
			ad->ad_name, naliases, longest, bytes);
# endif /* LOG */
}
/*
**  NDBM modules
*/

#ifdef NDBM

/*
**  NDBM_ALOOKUP -- look up alias in ndbm file
*/

char *
ndbm_alookup(ad, name, e)
	register ALIASDB *ad;
	char *name;
	ENVELOPE *e;
{
	int i;
	datum rhs, lhs;
	char keybuf[MAXNAME + 1];

	if (tTd(27, 20))
		printf("ndbm_lookup(%s)\n", name);

	/* create a key for fetch */
	i = strlen(name) + 1;
	if (i > sizeof keybuf)
		i = sizeof keybuf;
	bcopy(name, keybuf, i);
	if (!bitnset(M_USR_UPPER, LocalMailer->m_flags))
		makelower(keybuf);

	lhs.dsize = i;
	lhs.dptr = keybuf;
	rhs = dbm_fetch((DBM *) ad->ad_dbp, lhs);
	return (rhs.dptr);
}


/*
**  NDBM_ASTORE -- store a datum in the database
*/

void
ndbm_astore(ad, lhs, rhs, e)
	register ALIASDB *ad;
	char *lhs;
	char *rhs;
	ENVELOPE *e;
{
	datum key;
	datum data;
	int stat;

	key.dsize = strlen(lhs) + 1;
	key.dptr = lhs;

	data.dsize = strlen(rhs) + 1;
	data.dptr = rhs;

	stat = dbm_store((DBM *) ad->ad_dbp, key, data, DBM_INSERT);
	if (stat > 0)
	{
		usrerr("050 Warning: duplicate alias name %s", lhs);
		stat = dbm_store((DBM *) ad->ad_dbp, key, data, DBM_REPLACE);
	}
	if (stat != 0)
		syserr("readaliases: dbm put (%s)", lhs);
}


/*
**  NDBM_AINIT -- initialize DBM database
*/

bool
ndbm_ainit(ad, e)
	register ALIASDB *ad;
	ENVELOPE *e;
{
	char buf[MAXNAME];

	if (tTd(27, 2))
		printf("ndbm_ainit(%s)\n", ad->ad_name);

	/* open the database */
	ad->ad_dbp = (void *) dbm_open(ad->ad_name, O_RDONLY, DBMMODE);
	if (ad->ad_dbp == NULL)
		return FALSE;

	/* wait for @:@ to appear */
	aliaswait(ad, ".pag", e);

	return TRUE;
}


/*
**  NDBM_AREBUILD -- rebuild hash database
*/

void
ndbm_arebuild(ad, fp, e)
	register ALIASDB *ad;
	FILE *fp;
	ENVELOPE *e;
{
	register DBM *db;
	int i;
	char buf[MAXNAME];

	if (tTd(27, 2))
		printf("ndbm_arebuild(%s)\n", ad->ad_name);

	db = dbm_open(ad->ad_name, O_RDWR|O_CREAT|O_TRUNC, DBMMODE);
	if (db == NULL)
	{
		syserr("ndbm_arebuild: cannot create %s", buf);
		return;
	}
	ad->ad_dbp = (void *) db;
	ad->ad_flags |= ADF_WRITABLE|ADF_VALID;

	/* read and store the aliases */
	readaliases(ad, fp, e);
}

/*
**  NDBM_ACLOSE -- close the database
*/

void
ndbm_aclose(ad, e)
	register ALIASDB  *ad;
	ENVELOPE *e;
{
	if (bitset(ADF_WRITABLE, ad->ad_flags))
	{
#ifdef YPCOMPAT
		char buf[200];

		(void) sprintf(buf, "%010ld", curtime());
		ndbm_astore(ad, "YP_LAST_MODIFIED", buf, e);

		(void) myhostname(buf, sizeof buf);
		ndbm_astore(ad, "YP_MASTER_NAME", buf, e);
#endif

		/* write out the distinguished alias */
		ndbm_astore(ad, "@", "@", e);
	}
	dbm_close((DBM *) ad->ad_dbp);
}

#endif
/*
**  HASH (NEWDB) Modules
*/

#ifdef NEWDB

/*
**  HASH_ALOOKUP -- look up alias in hash file
*/

char *
hash_alookup(ad, name, e)
	register ALIASDB *ad;
	char *name;
	ENVELOPE *e;
{
	int i;
	DBT rhs, lhs;
	int s;
	char keybuf[MAXNAME + 1];

	if (tTd(27, 20))
		printf("hash_alookup(%s)\n", name);

	/* create a key for fetch */
	i = strlen(name) + 1;
	if (i > sizeof keybuf)
		i = sizeof keybuf;
	bcopy(name, keybuf, i);
	if (!bitnset(M_USR_UPPER, LocalMailer->m_flags))
		makelower(keybuf);

	lhs.size = i;
	lhs.data = keybuf;
	i = ad->ad_ndbp->get(ad->ad_ndbp, &lhs, &rhs, 0);
	if (i == 0)
		return (rhs.data);
	return (NULL);
}


/*
**  HASH_ASTORE -- store a datum in the database
*/

void
hash_astore(ad, lhs, rhs, e)
	register ALIASDB *ad;
	char *lhs;
	char *rhs;
	ENVELOPE *e;
{
	int stat;
	DBT key;
	DBT data;

	if (tTd(27, 20))
		printf("hash_astore(%s, %s)\n", lhs, rhs);

	key.size = strlen(lhs) + 1;
	key.data = lhs;

	data.size = strlen(rhs) + 1;
	data.data = rhs;

	stat = ad->ad_ndbp->put(ad->ad_ndbp, &key, &data, R_NOOVERWRITE);
	if (stat > 0)
	{
		usrerr("050 Warning: duplicate alias name %s", lhs);
		stat = ad->ad_ndbp->put(ad->ad_ndbp, &key, &data, 0);
	}
	if (stat != 0)
		syserr("readaliases: db put (%s)", lhs);
}


/*
**  HASH_AINIT -- initialize hash database
*/

bool
hash_ainit(ad, e)
	register ALIASDB *ad;
	ENVELOPE *e;
{
	char buf[MAXNAME];

	if (tTd(27, 2))
		printf("hash_ainit(%s)\n", ad->ad_name);

	/* open the database */
	(void) strcpy(buf, ad->ad_name);
	(void) strcat(buf, ".db");
	ad->ad_ndbp = dbopen(buf, O_RDONLY, DBMMODE, DB_HASH, NULL);
	if (ad->ad_ndbp == NULL)
		return FALSE;

	/* wait for @:@ to appear */
	aliaswait(ad, ".db", e);
	return TRUE;
}


/*
**  HASH_AREBUILD -- rebuild hash database
*/

void
hash_arebuild(ad, fp, e)
	register ALIASDB *ad;
	FILE *fp;
	ENVELOPE *e;
{
	register DB *db;
	char buf[MAXNAME];

	if (tTd(27, 2))
		printf("hash_arebuild(%s)\n", ad->ad_name);

	(void) strcpy(buf, ad->ad_name);
	(void) strcat(buf, ".db");
	db = dbopen(buf, O_RDWR|O_CREAT|O_TRUNC, DBMMODE, DB_HASH, NULL);
	if (db == NULL)
	{
		syserr("hash_arebuild: cannot create %s", buf);
		return;
	}
	ad->ad_ndbp = db;
	ad->ad_flags |= ADF_WRITABLE|ADF_VALID;

	/* read and store the aliases */
	readaliases(ad, fp, e);
}


/*
**  HASH_ACLOSE -- add distinguished entries and close the database
*/

void
hash_aclose(ad, e)
	ALIASDB *ad;
	ENVELOPE *e;
{
	if (tTd(27, 9))
		printf("hash_aclose(%x)\n", ad->ad_flags);

	if (bitset(ADF_WRITABLE, ad->ad_flags))
	{
		/* write out the distinguished alias */
		hash_astore(ad, "@", "@", e);
	}

	if (ad->ad_ndbp->close(ad->ad_ndbp) != 0)
		syserr("readaliases: db close failure");
}

#endif
/*
**  STAB (Symbol Table) Modules
*/


/*
**  STAB_ALOOKUP -- look up alias in symbol table
*/

char *
stab_alookup(ad, name, e)
	register ALIASDB *ad;
	char *name;
	ENVELOPE *e;
{
	register STAB *s;

	if (tTd(27, 20))
		printf("stab_lookup(%s)\n", name);

	s = stab(name, ST_ALIAS, ST_FIND);
	if (s != NULL)
		return (s->s_alias);
	return (NULL);
}


/*
**  STAB_ASTORE -- store in symtab (actually using during init, not rebuild)
*/

void
stab_astore(ad, lhs, rhs, e)
	register ALIASDB *ad;
	char *lhs;
	char *rhs;
	ENVELOPE *e;
{
	register STAB *s;

	s = stab(lhs, ST_ALIAS, ST_ENTER);
	s->s_alias = newstr(rhs);
}


/*
**  STAB_AINIT -- initialize (reads data file)
*/

bool
stab_ainit(ad, e)
	register ALIASDB *ad;
	ENVELOPE *e;
{
	FILE *af;

	if (tTd(27, 2))
		printf("stab_ainit(%s)\n", ad->ad_name);

	af = fopen(ad->ad_name, "r");
	if (af == NULL)
	{
		syserr("554 Can't open %s", ad->ad_name);
		errno = 0;
		return FALSE;
	}

	readaliases(ad, af, e);
}


/*
**  STAB_AREBUILD -- rebuild alias file
*/

void
stab_arebuild(ad, fp, e)
	ALIASDB *ad;
	FILE *fp;
	ENVELOPE *e;
{
	if (tTd(27, 2))
		printf("stab_arebuild(%s)\n", ad->ad_name);

	ad->ad_flags |= ADF_WRITABLE|ADF_VALID;
}


/*
**  STAB_ACLOSE -- close symbol table (???)
*/

void
stab_aclose(ad, e)
	ALIASDB *ad;
	ENVELOPE *e;
{
	/* ignore it */
}
/*
**  NIS Modules
*/

#ifdef NIS

/*
**  NIS_ALOOKUP
*/

char *
nis_alookup(ad, name, e)
	ALIASDB *ad;
	char *name;
	ENVELOPE *e;
{
	auto char *vp;
	auto int vsize;

	if (tTd(27, 20))
		printf("nis_lookup(%s)\n", name);

	if (ypmatch(ad->ad_domain, ad->ad_name, name, strlen(name),
			&vp, &vsize) != 0)
		return NULL;
	return vp;
}

/*
**  NIS_ASTORE
*/

void
nis_astore(ad, lhs, rhs, e)
	ALIASDB *ad;
	char *lhs;
	char *rhs;
	ENVELOPE *e;
{
	/* nothing */
}

/*
**  NIS_AINIT
*/

bool
nis_ainit(ad, e)
	ALIASDB *ad;
	ENVELOPE *e;
{
	register char *p;
	auto char *ypmaster;

	if (tTd(27, 2))
		printf("nis_ainit(%s)\n", ad->ad_name);

	p = strchr(ad->ad_name, '@');
	if (p != NULL)
	{
		*p++ = '\0';
		if (*p != '\0')
			ad->ad_domain = p;
	}
	if (ad->ad_domain == NULL)
		yp_get_default_domain(&ad->ad_domain);

	if (*ad->ad_name == '\0')
		ad->ad_name = "mail.aliases";

	yperr = yp_master(ad->ad_domain, ad->ad_name, &ypmaster);
	return (yperr == 0);
}

/*
**  NIS_AREBUILD
*/

void
nis_arebuild(ad, fp, e)
	ALIASDB *ad;
	FILE *fp;
	ENVELOPE *e;
{
	if (tTd(27, 2))
		printf("nis_arebuild(%s)\n", ad->ad_name);
}


/*
**  NIS_ACLOSE
*/

void
nis_aclose(ad, e)
	ALIASDB *ad;
	ENVELOPE *e;
{
	/* nothing */
}

#endif /* NIS */
/*
**  Implicit Modules
**
**	Tries several types.  For back compatibility.
*/

/*
**  IMPL_ALOOKUP -- lookup in best open database
*/

char *
impl_alookup(ad, name, e)
	ALIASDB *ad;
	char *name;
	ENVELOPE *e;
{
	if (tTd(27, 20))
		printf("impl_lookup(%s)\n", name);

#ifdef NEWDB
	if (bitset(ADF_IMPLHASH, ad->ad_flags))
		return hash_alookup(ad, name, e);
#endif
#ifdef NDBM
	if (bitset(ADF_IMPLNDBM, ad->ad_flags))
		return ndbm_alookup(ad, name, e);
#endif
	return stab_alookup(ad, name, e);
}

/*
**  IMPL_ASTORE -- store in open databases
*/

void
impl_astore(ad, lhs, rhs, e)
	ALIASDB *ad;
	char *lhs;
	char *rhs;
	ENVELOPE *e;
{
#ifdef NEWDB
	if (bitset(ADF_IMPLHASH, ad->ad_flags))
		hash_astore(ad, lhs, rhs, e);
#endif
#ifdef NDBM
	if (bitset(ADF_IMPLNDBM, ad->ad_flags))
		ndbm_astore(ad, lhs, rhs, e);
#endif
	stab_astore(ad, lhs, rhs, e);
}

/*
**  IMPL_AINIT -- implicit database lookup
*/

bool
impl_ainit(ad, e)
	ALIASDB *ad;
	ENVELOPE *e;
{
	if (tTd(27, 2))
		printf("impl_ainit(%s)\n", ad->ad_name);

	/* implicit class */
#ifdef NEWDB
	ad->ad_flags |= ADF_IMPLHASH;
	if (hash_ainit(ad, e))
	{
		return TRUE;
	}
	ad->ad_flags &= ~ADF_IMPLHASH;
#endif
#ifdef NDBM
	ad->ad_flags |= ADF_IMPLNDBM;
	if (ndbm_ainit(ad, e))
	{
		return TRUE;
	}
	ad->ad_flags &= ~ADF_IMPLNDBM;
#endif
	syserr("WARNING: cannot open alias database %s", ad->ad_name);

	if (stab_ainit(ad, e))
	{
		return TRUE;
	}

	return FALSE;
}

/*
**  IMPL_AREBUILD -- rebuild alias database
*/

void
impl_arebuild(ad, fp, e)
	ALIASDB *ad;
	FILE *fp;
	ENVELOPE *e;
{
#ifdef NEWDB
	DB *ndb;
	char buf[MAXNAME];
#endif

	if (tTd(27, 2))
		printf("impl_arebuild(%s)\n", ad->ad_name);

#ifdef NEWDB
	(void) strcpy(buf, ad->ad_name);
	(void) strcat(buf, ".db");
	ndb = dbopen(buf, O_RDWR|O_CREAT|O_TRUNC, DBMMODE, DB_HASH, NULL);
	if (ndb == NULL)
	{
		syserr("rebuildaliases: cannot create %s", buf);
	}
	else
	{
		ad->ad_ndbp = ndb;
		ad->ad_flags |= ADF_IMPLHASH;
#if defined(NDBM) && defined(YPCOMPAT)
		if (access("/var/yp/Makefile", R_OK) != 0)
#endif
			goto readem;
	}
#endif

#ifdef NDBM
	ad->ad_dbp = (void *) dbm_open(ad->ad_name, O_RDWR|O_CREAT|O_TRUNC, DBMMODE);
	if (ad->ad_dbp == NULL)
	{
		syserr("rebuildaliases: cannot create %s.{pag,dir}",
			ad->ad_name);
	}
	else
	{
		ad->ad_flags |= ADF_IMPLNDBM;
	}
#endif

	if (!bitset(ADF_IMPLHASH|ADF_IMPLNDBM, ad->ad_flags))
		return;

  readem:
	ad->ad_flags |= ADF_WRITABLE|ADF_VALID;

	/* read and store aliases */
	readaliases(ad, fp, e);
}


/*
**  IMPL_ACLOSE -- close any open database(s)
*/

void
impl_aclose(ad, e)
	ALIASDB *ad;
	ENVELOPE *e;
{
#ifdef NEWDB
	if (bitset(ADF_IMPLHASH, ad->ad_flags))
		hash_aclose(ad, e);
#endif

#ifdef NDBM
	if (bitset(ADF_IMPLNDBM, ad->ad_flags))
		ndbm_aclose(ad, e);
#endif
}
/*
**  SETUPALIASES -- set up aliases classes
*/

#ifdef NEWDB
ALIASCLASS	HashAClass =
{
	"hash",		hash_alookup,	hash_astore,
	hash_ainit,	hash_arebuild,	hash_aclose,
	ACF_BUILDABLE
};
#endif

#ifdef NDBM
ALIASCLASS	DbmAClass =
{
	"dbm",		ndbm_alookup,	ndbm_astore,
	ndbm_ainit,	ndbm_arebuild,	ndbm_aclose,
	ACF_BUILDABLE
};
#endif

#ifdef NIS
ALIASCLASS	NisAClass =
{
	"nis",		nis_alookup,	nis_astore,
	nis_ainit,	nis_arebuild,	nis_aclose,
	0
};
#endif

ALIASCLASS	StabAClass =
{
	"stab",		stab_alookup,	stab_astore,
	stab_ainit,	stab_arebuild,	stab_aclose,
	0
};

ALIASCLASS	ImplAClass =
{
	"implicit",	impl_alookup,	impl_astore,
	impl_ainit,	impl_arebuild,	impl_aclose,
	ACF_BUILDABLE
};

setupaliases()
{
	register STAB *s;

#ifdef NEWDB
	s = stab("hash", ST_ALIASCLASS, ST_ENTER);
	s->s_aliasclass = &HashAClass;
#endif

#ifdef NDBM
	s = stab("dbm", ST_ALIASCLASS, ST_ENTER);
	s->s_aliasclass = &DbmAClass;
#endif

#ifdef NIS
	s = stab("nis", ST_ALIASCLASS, ST_ENTER);
	s->s_aliasclass = &NisAClass;
#endif

#if !defined(NEWDB) && !defined(NDBM)
	s = stab("stab", ST_ALIASCLASS, ST_ENTER);
	s->s_aliasclass = &StabAClass;
#endif

	s = stab("implicit", ST_ALIASCLASS, ST_ENTER);
	s->s_aliasclass = &ImplAClass;
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
	char *pp;
	char *ep;

	if (tTd(27, 1))
		printf("forward(%s)\n", user->q_paddr);

	if (user->q_mailer != LocalMailer || bitset(QBADADDR, user->q_flags))
		return;
	if (user->q_home == NULL)
	{
		syserr("554 forward: no home");
		user->q_home = "/nosuchdirectory";
	}

	/* good address -- look for .forward file in home */
	define('z', user->q_home, e);
	define('u', user->q_user, e);
	define('h', user->q_host, e);
	if (ForwardPath == NULL)
		ForwardPath = newstr("\201z/.forward");

	for (pp = ForwardPath; pp != NULL; pp = ep)
	{
		int err;
		char buf[MAXPATHLEN+1];

		ep = strchr(pp, ':');
		if (ep != NULL)
			*ep = '\0';
		expand(pp, buf, &buf[sizeof buf - 1], e);
		if (ep != NULL)
			*ep++ = ':';
		if (tTd(27, 3))
			printf("forward: trying %s\n", buf);
		err = include(buf, TRUE, user, sendq, e);
		if (err == 0)
			break;
		if (transienterror(err))
		{
			/* we have to suspend this message */
			if (tTd(27, 2))
				printf("forward: transient error on %s\n", buf);
#ifdef LOG
			if (LogLevel > 2)
				syslog(LOG_ERR, "%s: forward %s: transient error: %s",
					e->e_id, buf, errstring(err));
#endif
			message("%s: %s: message queued", buf, errstring(err));
			user->q_flags |= QQUEUEUP|QDONTSEND;
			return;
		}
	}
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
