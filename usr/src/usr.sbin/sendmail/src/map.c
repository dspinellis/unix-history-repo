/*
 * Copyright (c) 1992 Eric P. Allman.
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)map.c	6.14 (Berkeley) %G%";
#endif /* not lint */

#include "sendmail.h"

#ifdef NDBM
#include <ndbm.h>
#endif
#ifdef NEWDB
#include <db.h>
#endif
#ifdef NIS
#include <rpcsvc/ypclnt.h>
#endif

/*
**  MAP.C -- implementations for various map classes.
**
**	Each map class implements a series of functions:
**
**	bool map_parse(MAP *map, char *args)
**		Parse the arguments from the config file.  Return TRUE
**		if they were ok, FALSE otherwise.  Fill in map with the
**		values.
**
**	char *map_lookup(MAP *map, char buf[], int bufsize,
**			 char **args, int *pstat)
**		Look up the key given in buf in the given map.  If found,
**		do any rewriting the map wants (including "args" if desired)
**		and return the value.  Set *pstat to the appropriate status
**		on error and return NULL.
**
**	void map_store(MAP *map, char *key, char *value)
**		Store the key:value pair in the map.
**
**	void map_rebuild(MAP *map, FILE *fp, int automatic)
**		Rebuild the map.  If automatic is set, this is an
**		auto-rebuild.
**
**	bool map_open(MAP *map, int mode)
**		Open the map for the indicated mode.  Return TRUE if it
**		was opened successfully, FALSE otherwise.
**
**	void map_close(MAP *map)
**		Close the map.
*/

#define DBMMODE		0644
/*
**  MAP_PARSEARGS -- parse config line arguments for database lookup
**
**	This is a generic version of the map_parse method.
**
**	Parameters:
**		map -- the map being initialized.
**		ap -- a pointer to the args on the config line.
**
**	Returns:
**		TRUE -- if everything parsed OK.
**		FALSE -- otherwise.
**
**	Side Effects:
**		null terminates the filename; stores it in map
*/

bool
map_parseargs(map, ap)
	MAP *map;
	char *ap;
{
	register char *p = ap;

	for (;;)
	{
		while (isascii(*p) && isspace(*p))
			p++;
		if (*p != '-')
			break;
		switch (*++p)
		{
		  case 'N':
			map->map_flags |= MF_INCLNULL;
			break;

		  case 'o':
			map->map_flags |= MF_OPTIONAL;
			break;

		  case 'f':
			map->map_flags |= MF_NOFOLDCASE;
			break;

		  case 'm':
			map->map_flags |= MF_MATCHONLY;
			break;

		  case 'a':
			map->map_app = ++p;
			break;

		  case 'd':
			map->map_domain = ++p;
			break;
		}
		while (*p != '\0' && !(isascii(*p) && isspace(*p)))
			p++;
		if (*p != '\0')
			*p++ = '\0';
	}
	if (map->map_app != NULL)
		map->map_app = newstr(map->map_app);
	if (map->map_domain != NULL)
		map->map_domain = newstr(map->map_domain);

	if (*p != '\0')
	{
		map->map_file = p;
		while (*p != '\0' && !(isascii(*p) && isspace(*p)))
			p++;
		if (*p != '\0')
			*p++ = '\0';
		map->map_file = newstr(map->map_file);
	}

	while (*p != '\0' && isascii(*p) && isspace(*p))
		p++;
	if (*p != '\0')
		map->map_rebuild = newstr(p);

	if (map->map_file == NULL)
	{
		syserr("No file name for %s map %s",
			map->map_class->map_cname, map->map_mname);
		return FALSE;
	}
	return TRUE;
}
/*
**  MAP_REWRITE -- rewrite a database key, interpolating %n indications.
**
**	It also adds the map_app string.  It can be used as a utility
**	in the map_lookup method.
**
**	Parameters:
**		map -- the map that causes this.
**		s -- the string to rewrite, NOT necessarily null terminated.
**		slen -- the length of s.
**		av -- arguments to interpolate into buf.
**
**	Returns:
**		Pointer to rewritten result.
**
**	Side Effects:
**		none.
*/

char *
map_rewrite(map, s, slen, av)
	register MAP *map;
	register char *s;
	int slen;
	char **av;
{
	register char *bp;
	register char c;
	char **avp;
	register char *ap;
	int i;
	int len;
	static int buflen = -1;
	static char *buf = NULL;

	if (tTd(23, 1))
	{
		printf("map_rewrite(%.*s), av =\n", slen, s);
		for (avp = av; *avp != NULL; avp++)
			printf("\t%s\n", *avp);
	}

	/* count expected size of output (can safely overestimate) */
	i = len = slen;
	if (av != NULL)
	{
		bp = s;
		for (i = slen; --i >= 0 && (c = *bp++) != 0; )
		{
			if (c != '%')
				continue;
			if (--i < 0)
				break;
			c = *bp++;
			if (!(isascii(c) && isdigit(c)))
				continue;
			c -= 0;
			for (avp = av; --c >= 0 && *avp != NULL; avp++)
				continue;
			if (*avp == NULL)
				continue;
			len += strlen(*avp);
		}
	}
	if (map->map_app != NULL)
		len += strlen(map->map_app);
	if (buflen < ++len)
	{
		/* need to malloc additional space */
		buflen = len;
		if (buf != NULL)
			free(buf);
		buf = xalloc(buflen);
	}

	bp = buf;
	if (av == NULL)
	{
		bcopy(s, bp, slen);
		bp += slen;
	}
	else
	{
		while (--slen >= 0 && (c = *s++) != '\0')
		{
			if (c != '%')
			{
  pushc:
				*bp++ = c;
				continue;
			}
			if (--slen < 0 || (c = *s++) == '\0')
				c = '%';
			if (c == '%')
				goto pushc;
			if (!(isascii(c) && isdigit(c)))
			{
				*bp++ = '%';
				goto pushc;
			}
			c -= '0';
			for (avp = av; --c >= 0 && *avp != NULL; avp++)
				continue;
			if (*avp == NULL)
				continue;

			/* transliterate argument into output string */
			for (ap = *avp; (c = *ap++) != '\0'; )
				*bp++ = c;
		}
	}
	if (map->map_app != NULL)
		strcpy(bp, map->map_app);
	else
		*bp = '\0';
	if (tTd(23, 1))
		printf("map_rewrite => %s\n", buf);
	return buf;
}
/*
**  NDBM modules
*/

#ifdef NDBM

/*
**  DBM_MAP_OPEN -- DBM-style map open
*/

bool
ndbm_map_open(map, mode)
	MAP *map;
	int mode;
{
	DBM *dbm;

	if (tTd(27, 2))
		printf("ndbm_map_open(%s, %d)\n", map->map_file, mode);

	/* open the database */
	dbm = dbm_open(map->map_file, mode, DBMMODE);
	if (dbm == NULL)
	{
		if (!bitset(MF_OPTIONAL, map->map_flags))
			syserr("Cannot open DBM database %s", map->map_file);
		return FALSE;
	}
	map->map_db1 = (void *) dbm;
	return TRUE;
}


/*
**  DBM_MAP_LOOKUP -- look up a datum in a DBM-type map
*/

char *
ndbm_map_lookup(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
	datum key, val;
	char keybuf[MAXNAME + 1];

	if (tTd(27, 20))
		printf("ndbm_map_lookup(%s)\n", name);

	key.dptr = name;
	key.dsize = strlen(name);
	if (!bitset(MF_NOFOLDCASE, map->map_flags))
	{
		if (key.dsize > sizeof keybuf - 1)
			key.dsize = sizeof keybuf - 1;
		bcopy(key.dptr, keybuf, key.dsize + 1);
		makelower(keybuf);
		key.dptr = keybuf;
	}
	if (bitset(MF_INCLNULL, map->map_flags))
		key.dsize++;
	(void) lockfile(dbm_dirfno((DBM *) map->map_db1), map->map_file, LOCK_SH);
	val = dbm_fetch((DBM *) map->map_db1, key);
	(void) lockfile(dbm_dirfno((DBM *) map->map_db1), map->map_file, LOCK_UN);
	if (val.dptr == NULL)
		return NULL;
	if (bitset(MF_MATCHONLY, map->map_flags))
		av = NULL;
	return map_rewrite(map, val.dptr, val.dsize, av);
}


/*
**  DBM_MAP_STORE -- store a datum in the database
*/

void
ndbm_map_store(map, lhs, rhs)
	register MAP *map;
	char *lhs;
	char *rhs;
{
	datum key;
	datum data;
	int stat;

	if (tTd(27, 12))
		printf("ndbm_map_store(%s, %s)\n", lhs, rhs);

	key.dsize = strlen(lhs);
	key.dptr = lhs;

	data.dsize = strlen(rhs);
	data.dptr = rhs;

	if (bitset(MF_INCLNULL, map->map_flags))
	{
		key.dsize++;
		data.dsize++;
	}

	stat = dbm_store((DBM *) map->map_db1, key, data, DBM_INSERT);
	if (stat > 0)
	{
		usrerr("050 Warning: duplicate alias name %s", lhs);
		stat = dbm_store((DBM *) map->map_db1, key, data, DBM_REPLACE);
	}
	if (stat != 0)
		syserr("readaliases: dbm put (%s)", lhs);
}


/*
**  DBM_MAP_REBUILD -- rebuild DBM database
*/

void
ndbm_map_rebuild(map, fp, automatic)
	register MAP *map;
	FILE *fp;
	int automatic;
{
	register DBM *db;
	int i;
	char buf[MAXNAME];

	if (tTd(27, 2))
		printf("ndbm_map_rebuild(%s)\n", map->map_file);

	db = dbm_open(map->map_file, O_RDWR|O_CREAT|O_TRUNC, DBMMODE);
	if (db == NULL)
	{
		syserr("ndbm_map_rebuild: cannot create %s", buf);
		return;
	}
	map->map_db1 = (void *) db;
	map->map_flags |= MF_WRITABLE|MF_VALID;
}

/*
**  NDBM_ACLOSE -- close the database
*/

void
ndbm_map_close(map)
	register MAP  *map;
{
	if (bitset(MF_WRITABLE, map->map_flags))
	{
#ifdef YPCOMPAT
		char buf[200];

		(void) sprintf(buf, "%010ld", curtime());
		ndbm_map_store(map, "YP_LAST_MODIFIED", buf);

		(void) myhostname(buf, sizeof buf);
		ndbm_map_store(map, "YP_MASTER_NAME", buf);
#endif

		/* write out the distinguished alias */
		ndbm_map_store(map, "@", "@");
	}
	dbm_close((DBM *) map->map_db1);
}

#endif
/*
**  HASH (NEWDB) Modules
*/

#ifdef NEWDB

/*
**  BTREE_MAP_PARSE -- BTREE-style map initialization
*/

bool
bt_map_open(map, mode)
	MAP *map;
	int mode;
{
	DB *db;
	char buf[MAXNAME];

	if (tTd(27, 2))
		printf("bt_map_open(%s, %d)\n", map->map_file, mode);

	(void) sprintf(buf, "%s.db", map->map_file);
	db = dbopen(buf, mode, 0644, DB_BTREE, NULL);
	if (db == NULL)
	{
		if (!bitset(MF_OPTIONAL, map->map_flags))
			syserr("Cannot open BTREE database %s", map->map_file);
		return FALSE;
	}
	map->map_db2 = (void *) db;
	return TRUE;
}


/*
**  HASH_MAP_INIT -- HASH-style map initialization
*/

bool
hash_map_open(map, mode)
	MAP *map;
	int mode;
{
	DB *db;
	char buf[MAXNAME];

	if (tTd(27, 2))
		printf("hash_map_open(%s, %d)\n", map->map_file, mode);

	(void) sprintf(buf, "%s.db", map->map_file);
	db = dbopen(buf, mode, 0644, DB_HASH, NULL);
	if (db == NULL)
	{
		if (!bitset(MF_OPTIONAL, map->map_flags))
			syserr("Cannot open HASH database %s", map->map_file);
		return FALSE;
	}
	map->map_db2 = (void *) db;
	return TRUE;
}


/*
**  DB_MAP_LOOKUP -- look up a datum in a BTREE- or HASH-type map
*/

char *
db_map_lookup(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
	DBT key, val;
	char keybuf[MAXNAME + 1];

	if (tTd(27, 20))
		printf("db_map_lookup(%s)\n", name);

	key.size = strlen(name);
	if (key.size > sizeof keybuf - 1)
		key.size = sizeof keybuf - 1;
	key.data = keybuf;
	bcopy(name, keybuf, key.size + 1);
	if (!bitset(MF_NOFOLDCASE, map->map_flags))
		makelower(keybuf);
	if (bitset(MF_INCLNULL, map->map_flags))
		key.size++;
	if (((DB *) map->map_db2)->get((DB *) map->map_db2, &key, &val, 0) != 0)
		return NULL;
	if (bitset(MF_MATCHONLY, map->map_flags))
		av = NULL;
	return map_rewrite(map, val.data, val.size, av);
}


/*
**  DB_MAP_STORE -- store a datum in the NEWDB database
*/

void
db_map_store(map, lhs, rhs)
	register MAP *map;
	char *lhs;
	char *rhs;
{
	int stat;
	DBT key;
	DBT data;
	register DB *db = map->map_db2;

	if (tTd(27, 20))
		printf("db_map_store(%s, %s)\n", lhs, rhs);

	key.size = strlen(lhs);
	key.data = lhs;

	data.size = strlen(rhs);
	data.data = rhs;

	if (bitset(MF_INCLNULL, map->map_flags))
	{
		key.size++;
		data.size++;
	}

	stat = db->put(db, &key, &data, R_NOOVERWRITE);
	if (stat > 0)
	{
		usrerr("050 Warning: duplicate alias name %s", lhs);
		stat = db->put(db, &key, &data, 0);
	}
	if (stat != 0)
		syserr("readaliases: db put (%s)", lhs);
}


/*
**  HASH_MAP_REBUILD -- rebuild hash database
*/

void
db_map_rebuild(map, fp, automatic, e)
	register MAP *map;
	FILE *fp;
	int automatic;
	ENVELOPE *e;
{
	register DB *db;
	char buf[MAXNAME];

	if (tTd(27, 2))
		printf("hash_map_rebuild(%s)\n", map->map_file);

	(void) strcpy(buf, map->map_file);
	(void) strcat(buf, ".db");
	db = dbopen(buf, O_RDWR|O_CREAT|O_TRUNC, DBMMODE, DB_HASH, NULL);
	if (db == NULL)
	{
		syserr("hash_map_rebuild: cannot create %s", buf);
		return;
	}
	map->map_db2 = db;
	map->map_flags |= MF_WRITABLE|MF_VALID;
}


/*
**  DB_MAP_CLOSE -- add distinguished entries and close the database
*/

void
db_map_close(map)
	MAP *map;
{
	register DB *db = map->map_db2;

	if (tTd(27, 9))
		printf("db_map_close(%s, %x)\n", map->map_file, map->map_flags);

	if (bitset(MF_WRITABLE, map->map_flags))
	{
		/* write out the distinguished alias */
		db_map_store(map, "@", "@");
	}

	if (db->close(db) != 0)
		syserr("readaliases: db close failure");
}

#endif
/*
**  NIS Modules
*/

# ifdef NIS

/*
**  NIS_MAP_OPEN -- open DBM map
*/

bool
nis_map_open(map, mode)
	MAP *map;
	int mode;
{
	int yperr;
	char *master;

	if (tTd(27, 2))
		printf("nis_map_open(%s)\n", map->map_file);

	if (map->map_domain == NULL)
		yp_get_default_domain(&map->map_domain);

	/* check to see if this map actually exists */
	yperr = yp_master(map->map_domain, map->map_file, &master);
	if (yperr == 0)
		return TRUE;
	if (!bitset(MF_OPTIONAL, map->map_flags))
		syserr("Cannot bind to domain %s: %s", map->map_domain,
			yperr_string(yperr));
	return FALSE;
}

bool
nis_map_open(map, mode)
	MAP *map;
	int mode;
{
	register char *p;
	int yperr;
	auto char *vp;
	auto int vsize;

	p = strchr(map->map_file, '@');
	if (p != NULL)
	{
		*p++ = '\0';
		if (*p != '\0')
			map->map_domain = p;
	}
	if (map->map_domain == NULL)
		yp_get_default_domain(&map->map_domain);

	if (*map->map_file == '\0')
		map->map_file = "mail.aliases";

	yperr = yp_match(map->map_domain, map->map_file, "@", 1,
			&vp, &vsize);
	if (tTd(27, 10))
		printf("nis_map_open: yp_match(%s, %s) => %s\n",
			map->map_domain, map->map_file, yperr_string(yperr));
	if (yperr == 0 || yperr == YPERR_KEY || yperr == YPERR_BUSY)
		return TRUE;
	return FALSE;
}


/*
**  NIS_MAP_LOOKUP -- look up a datum in a NIS map
*/

char *
nis_map_lookup(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
	char *vp;
	auto int vsize;
	int buflen;
	char keybuf[MAXNAME + 1];

	if (tTd(27, 20))
		printf("nis_map_lookup(%s)\n", name);

	buflen = strlen(name);
	if (buflen > sizeof keybuf - 1)
		buflen = sizeof keybuf - 1;
	bcopy(name, keybuf, buflen + 1);
	if (!bitset(MF_NOFOLDCASE, map->map_flags))
		makelower(keybuf);
	if (bitset(MF_INCLNULL, map->map_flags))
		buflen++;
	yperr = yp_match(map->map_domain, map->map_file, keybuf, buflen,
		     &vp, &vsize);
	if (yperr != 0)
	{
		if (yperr != YPERR_KEY && yperr != YPERR_BUSY)
			map->map_flags &= ~MF_VALID;
		return NULL;
	}
	if (bitset(MF_MATCHONLY, map->map_flags))
		av = NULL;
	return map_rewrite(map, val.dptr, val.dsize, av);
}


/*
**  NIS_ASTORE
*/

void
nis_map_store(map, lhs, rhs)
	MAP *map;
	char *lhs;
	char *rhs;
{
	/* nothing */
}

/*
**  NIS_AREBUILD
*/

void
nis_map_rebuild(map, fp, automatic, e)
	MAP *map;
	FILE *fp;
	int automatic;
	ENVELOPE *e;
{
	if (tTd(27, 2))
		printf("nis_map_rebuild(%s)\n", map->map_file);
}


/*
**  NIS_ACLOSE
*/

void
nis_map_close(map)
	MAP *map;
{
	/* nothing */
}

#endif /* NIS */
/*
**  STAB (Symbol Table) Modules
*/


/*
**  STAB_ALOOKUP -- look up alias in symbol table
*/

char *
stab_map_lookup(map, name)
	register MAP *map;
	char *name;
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
stab_map_store(map, lhs, rhs)
	register MAP *map;
	char *lhs;
	char *rhs;
{
	register STAB *s;

	s = stab(lhs, ST_ALIAS, ST_ENTER);
	s->s_alias = newstr(rhs);
}


/*
**  STAB_AINIT -- initialize (reads data file)
*/

bool
stab_map_open(map, mode)
	register MAP *map;
	int mode;
{
	FILE *af;

	if (tTd(27, 2))
		printf("stab_map_open(%s)\n", map->map_file);

	if (mode != O_RDONLY)
		return FALSE;

	af = fopen(map->map_file, "r");
	if (af == NULL)
		return FALSE;
	return TRUE;
}


/*
**  STAB_AREBUILD -- rebuild alias file
*/

void
stab_map_rebuild(map, fp, automatic, e)
	MAP *map;
	FILE *fp;
	int automatic;
	ENVELOPE *e;
{
	if (tTd(27, 2))
		printf("stab_map_rebuild(%s)\n", map->map_file);

	map->map_flags |= MF_WRITABLE|MF_VALID;
}


/*
**  STAB_ACLOSE -- close symbol table (???)
*/

void
stab_map_close(map)
	MAP *map;
{
	/* ignore it */
}
/*
**  Implicit Modules
**
**	Tries several types.  For back compatibility of aliases.
*/


/*
**  IMPL_ALOOKUP -- lookup in best open database
*/

char *
impl_map_lookup(map, name, av, pstat)
	MAP *map;
	char *name;
	char **av;
	int *pstat;
{
	if (tTd(27, 20))
		printf("impl_map_lookup(%s)\n", name);

#ifdef NEWDB
	if (bitset(MF_IMPL_HASH, map->map_flags))
		return db_map_lookup(map, name, av, pstat);
#endif
#ifdef NDBM
	if (bitset(MF_IMPL_NDBM, map->map_flags))
		return ndbm_map_lookup(map, name, av, pstat);
#endif
	return stab_map_lookup(map, name, av, pstat);
}

/*
**  IMPL_ASTORE -- store in open databases
*/

void
impl_map_store(map, lhs, rhs)
	MAP *map;
	char *lhs;
	char *rhs;
{
#ifdef NEWDB
	if (bitset(MF_IMPL_HASH, map->map_flags))
		db_map_store(map, lhs, rhs);
#endif
#ifdef NDBM
	if (bitset(MF_IMPL_NDBM, map->map_flags))
		ndbm_map_store(map, lhs, rhs);
#endif
	stab_map_store(map, lhs, rhs);
}

/*
**  IMPL_MAP_OPEN -- implicit database open
*/

bool
impl_map_open(map, mode)
	MAP *map;
	int mode;
{
	struct stat stb;

	if (tTd(27, 2))
		printf("impl_map_open(%s)\n", map->map_file);

	if (stat(map->map_file, &stb) < 0)
	{
		/* no alias file at all */
		return FALSE;
	}

#ifdef NEWDB
	if (hash_map_open(map, mode))
	{
		map->map_flags |= MF_IMPL_HASH;
		return TRUE;
	}
#endif
#ifdef NDBM
	if (ndbm_map_open(map, mode))
	{
		map->map_flags |= MF_IMPL_NDBM;
		return TRUE;
	}
#endif

	if (Verbose)
		message("WARNING: cannot open alias database %s", map->map_file);

	if (stab_map_open(map, mode))
	{
		return TRUE;
	}

	return FALSE;
}

/*
**  IMPL_AREBUILD -- rebuild alias database
*/

void
impl_map_rebuild(map, fp, automatic, e)
	MAP *map;
	FILE *fp;
	int automatic;
	ENVELOPE *e;
{
#ifdef NEWDB
	DB *ndb;
	char buf[MAXNAME];
#endif

	if (tTd(27, 2))
		printf("impl_map_rebuild(%s)\n", map->map_file);

#ifdef NEWDB
	(void) strcpy(buf, map->map_file);
	(void) strcat(buf, ".db");
	ndb = dbopen(buf, O_RDWR|O_CREAT|O_TRUNC, DBMMODE, DB_HASH, NULL);
	if (ndb == NULL)
	{
		syserr("rebuildaliases: cannot create %s", buf);
	}
	else
	{
		map->map_db2 = ndb;
		map->map_flags |= MF_IMPL_HASH;
#if defined(NDBM) && defined(YPCOMPAT)
		if (access("/var/yp/Makefile", R_OK) != 0)
#endif
			goto readem;
	}
#endif

#ifdef NDBM
	map->map_db1 = (void *) dbm_open(map->map_file, O_RDWR|O_CREAT|O_TRUNC, DBMMODE);
	if (map->map_db1 == NULL)
	{
		syserr("rebuildaliases: cannot create %s.{pag,dir}",
			map->map_file);
	}
	else
	{
		map->map_flags |= MF_IMPL_NDBM;
	}
#endif

	if (!bitset(MF_IMPL_HASH|MF_IMPL_NDBM, map->map_flags))
		return;

  readem:
	map->map_flags |= MF_WRITABLE|MF_VALID;
}


/*
**  IMPL_ACLOSE -- close any open database(s)
*/

void
impl_map_close(map, e)
	MAP *map;
	ENVELOPE *e;
{
#ifdef NEWDB
	if (bitset(MF_IMPL_HASH, map->map_flags))
		db_map_close(map, e);
#endif

#ifdef NDBM
	if (bitset(MF_IMPL_NDBM, map->map_flags))
		ndbm_map_close(map, e);
#endif
}
/*
**  SETUPALIASES -- set up aliases classes
*/

extern bool	host_map_init __P((MAP *, char *));
extern char	*host_map_lookup __P((MAP *, char *, char **, int *));

extern bool	dequote_init __P((MAP *, char *));
extern char	*dequote_map __P((MAP *, char *, char **, int *));

#if 0
extern bool	udb_map_parse __P((MAP *, char *));
extern char	*udb_map_lookup __P((MAP *, char *, char **, int *));
#endif

static MAPCLASS	MapClasses[] =
{
#ifdef NEWDB
	{
		"hash",		".db",	map_parseargs,
		db_map_lookup,		db_map_store,
		db_map_rebuild,		hash_map_open,	db_map_close,
	},

	{
		"btree",	".db",	map_parseargs,
		db_map_lookup,		db_map_store,
		db_map_rebuild,		bt_map_open,	db_map_close,
	},
#endif

#ifdef NDBM
	{
		"dbm",		".dir",	map_parseargs,
		ndbm_map_lookup,	ndbm_map_store,
		ndbm_map_rebuild,	ndbm_map_open,	ndbm_map_close,
	},
#endif

#ifdef NIS
	{
		"nis",		NULL,	map_parseargs,
		nis_map_lookup,		NULL,
		NULL,			nis_map_open,	nis_map_close,
	},
#endif

	{
		"stab",		NULL,	map_parseargs,
		stab_map_lookup,	stab_map_store,
		NULL,			stab_map_open,	stab_map_close,
	},

	{
		"implicit",	NULL,	map_parseargs,
		impl_map_lookup,	impl_map_store,
		impl_map_rebuild,	impl_map_open,	impl_map_close,
	},

	/* host DNS lookup */
	{
		"host",		NULL,	host_map_init,
		host_map_lookup,	NULL,
		NULL,			NULL,		NULL,
	},

	/* dequote map */
	{
		"dequote",	NULL,	dequote_init,
		dequote_map,		NULL,
		NULL,			NULL,		NULL,
	},

#if 0
# ifdef USERDB
	/* user database */
	{
		"udb",		".db",	udb_map_parse,
		udb_map_lookup,		NULL,
		NULL,			NULL,		NULL,
	},
# endif
#endif

	{
		NULL
	}
};

setupmaps()
{
	register MAPCLASS *mc;
	register STAB *s;

	for (mc = MapClasses; mc->map_cname != NULL; mc++)
	{
		s = stab(mc->map_cname, ST_MAPCLASS, ST_ENTER);
		s->s_mapclass = mc;
	}
}
