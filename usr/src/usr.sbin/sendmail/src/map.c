/*
 * Copyright (c) 1992 Eric P. Allman.
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)map.c	6.16 (Berkeley) %G%";
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
			map->map_mflags |= MF_INCLNULL;
			break;

		  case 'o':
			map->map_mflags |= MF_OPTIONAL;
			break;

		  case 'f':
			map->map_mflags |= MF_NOFOLDCASE;
			break;

		  case 'm':
			map->map_mflags |= MF_MATCHONLY;
			break;

		  case 'a':
			map->map_app = ++p;
			break;
		}
		while (*p != '\0' && !(isascii(*p) && isspace(*p)))
			p++;
		if (*p != '\0')
			*p++ = '\0';
	}
	if (map->map_app != NULL)
		map->map_app = newstr(map->map_app);

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

	if (mode == O_RDWR)
		mode |= O_CREAT|O_TRUNC;

	/* open the database */
	dbm = dbm_open(map->map_file, mode, DBMMODE);
	if (dbm == NULL)
	{
		if (!bitset(MF_OPTIONAL, map->map_mflags))
			syserr("Cannot open DBM database %s", map->map_file);
		return FALSE;
	}
	map->map_db1 = (void *) dbm;
	if (mode == O_RDONLY && bitset(MF_ALIAS, map->map_mflags))
		aliaswait(map, ".dir");
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
	if (!bitset(MF_NOFOLDCASE, map->map_mflags))
	{
		if (key.dsize > sizeof keybuf - 1)
			key.dsize = sizeof keybuf - 1;
		bcopy(key.dptr, keybuf, key.dsize + 1);
		makelower(keybuf);
		key.dptr = keybuf;
	}
	if (bitset(MF_INCLNULL, map->map_mflags))
		key.dsize++;
	(void) lockfile(dbm_dirfno((DBM *) map->map_db1), map->map_file, LOCK_SH);
	val = dbm_fetch((DBM *) map->map_db1, key);
	(void) lockfile(dbm_dirfno((DBM *) map->map_db1), map->map_file, LOCK_UN);
	if (val.dptr == NULL)
		return NULL;
	if (bitset(MF_MATCHONLY, map->map_mflags))
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

	if (bitset(MF_INCLNULL, map->map_mflags))
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
**  NDBM_MAP_CLOSE -- close the database
*/

void
ndbm_map_close(map)
	register MAP  *map;
{
	if (bitset(MF_WRITABLE, map->map_mflags))
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

	if (mode == O_RDWR)
		mode |= O_CREAT|O_TRUNC;

	(void) sprintf(buf, "%s.db", map->map_file);
	db = dbopen(buf, mode, 0644, DB_BTREE, NULL);
	if (db == NULL)
	{
		if (!bitset(MF_OPTIONAL, map->map_mflags))
			syserr("Cannot open BTREE database %s", map->map_file);
		return FALSE;
	}
	map->map_db2 = (void *) db;
	if (mode == O_RDONLY && bitset(MF_ALIAS, map->map_mflags))
		aliaswait(map, ".db");
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

	if (mode == O_RDWR)
		mode |= O_CREAT|O_TRUNC;

	(void) sprintf(buf, "%s.db", map->map_file);
	db = dbopen(buf, mode, 0644, DB_HASH, NULL);
	if (db == NULL)
	{
		if (!bitset(MF_OPTIONAL, map->map_mflags))
			syserr("Cannot open HASH database %s", map->map_file);
		return FALSE;
	}
	map->map_db2 = (void *) db;
	if (mode == O_RDONLY && bitset(MF_ALIAS, map->map_mflags))
		aliaswait(map, ".db");
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
	if (!bitset(MF_NOFOLDCASE, map->map_mflags))
		makelower(keybuf);
	if (bitset(MF_INCLNULL, map->map_mflags))
		key.size++;
	if (((DB *) map->map_db2)->get((DB *) map->map_db2, &key, &val, 0) != 0)
		return NULL;
	if (bitset(MF_MATCHONLY, map->map_mflags))
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

	if (bitset(MF_INCLNULL, map->map_mflags))
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
**  DB_MAP_CLOSE -- add distinguished entries and close the database
*/

void
db_map_close(map)
	MAP *map;
{
	register DB *db = map->map_db2;

	if (tTd(27, 9))
		printf("db_map_close(%s, %x)\n", map->map_file, map->map_mflags);

	if (bitset(MF_WRITABLE, map->map_mflags))
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
	register char *p;
	auto char *vp;
	auto int vsize;
	char *master;

	if (tTd(27, 2))
		printf("nis_map_open(%s)\n", map->map_file);

	if (mode != O_RDONLY)
	{
		errno = ENODEV;
		return FALSE;
	}

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

	/* check to see if this map actually exists */
	yperr = yp_match(map->map_domain, map->map_file, "@", 1,
			&vp, &vsize);
	if (tTd(27, 10))
		printf("nis_map_open: yp_match(%s, %s) => %s\n",
			map->map_domain, map->map_file, yperr_string(yperr));
	if (yperr == 0 || yperr == YPERR_KEY || yperr == YPERR_BUSY)
		return TRUE;

	if (!bitset(MF_OPTIONAL, map->map_mflags))
		syserr("Cannot bind to domain %s: %s", map->map_domain,
			yperr_string(yperr));

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
	int yperr;
	char keybuf[MAXNAME + 1];

	if (tTd(27, 20))
		printf("nis_map_lookup(%s)\n", name);

	buflen = strlen(name);
	if (buflen > sizeof keybuf - 1)
		buflen = sizeof keybuf - 1;
	bcopy(name, keybuf, buflen + 1);
	if (!bitset(MF_NOFOLDCASE, map->map_mflags))
		makelower(keybuf);
	if (bitset(MF_INCLNULL, map->map_mflags))
		buflen++;
	yperr = yp_match(map->map_domain, map->map_file, keybuf, buflen,
		     &vp, &vsize);
	if (yperr != 0)
	{
		if (yperr != YPERR_KEY && yperr != YPERR_BUSY)
			map->map_mflags &= ~(MF_VALID|MF_OPEN);
		return NULL;
	}
	if (bitset(MF_MATCHONLY, map->map_mflags))
		av = NULL;
	return map_rewrite(map, vp, vsize, av);
}


/*
**  NIS_MAP_STORE
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
**  NIS_MAP_CLOSE
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
**  STAB_MAP_LOOKUP -- look up alias in symbol table
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
**  STAB_MAP_STORE -- store in symtab (actually using during init, not rebuild)
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
**  STAB_MAP_OPEN -- initialize (reads data file)
**
**	This is a wierd case -- it is only intended as a fallback for
**	aliases.  For this reason, opens for write (only during a
**	"newaliases") always fails, and opens for read open the
**	actual underlying text file instead of the database.
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
	{
		errno = ENODEV;
		return FALSE;
	}

	return TRUE;
}


/*
**  STAB_MAP_CLOSE -- close symbol table (???)
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
**  IMPL_MAP_LOOKUP -- lookup in best open database
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
	if (bitset(MF_IMPL_HASH, map->map_mflags))
		return db_map_lookup(map, name, av, pstat);
#endif
#ifdef NDBM
	if (bitset(MF_IMPL_NDBM, map->map_mflags))
		return ndbm_map_lookup(map, name, av, pstat);
#endif
	return stab_map_lookup(map, name, av, pstat);
}

/*
**  IMPL_MAP_STORE -- store in open databases
*/

void
impl_map_store(map, lhs, rhs)
	MAP *map;
	char *lhs;
	char *rhs;
{
#ifdef NEWDB
	if (bitset(MF_IMPL_HASH, map->map_mflags))
		db_map_store(map, lhs, rhs);
#endif
#ifdef NDBM
	if (bitset(MF_IMPL_NDBM, map->map_mflags))
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
	map->map_mflags |= MF_IMPL_HASH;
	if (hash_map_open(map, mode))
	{
#if defined(NDBM) && defined(YPCOMPAT)
		if (mode == O_RDONLY || access("/var/yp/Makefile", R_OK) == 0)
#endif
			return TRUE;
	}
	else
		map->map_mflags &= ~MF_IMPL_HASH;
#endif
#ifdef NDBM
	map->map_mflags |= MF_IMPL_NDBM;
	if (ndbm_map_open(map, mode))
	{
		return TRUE;
	}
	else
		map->map_mflags &= ~MF_IMPL_NDBM;
#endif

#if !defined(NEWDB) && !defined(NDBM)
	if (Verbose)
		message("WARNING: cannot open alias database %s", map->map_file);
#endif

	return stab_map_open(map, mode);
}


/*
**  IMPL_MAP_CLOSE -- close any open database(s)
*/

void
impl_map_close(map)
	MAP *map;
{
#ifdef NEWDB
	if (bitset(MF_IMPL_HASH, map->map_mflags))
	{
		db_map_close(map);
		map->map_mflags &= ~MF_IMPL_HASH;
	}
#endif

#ifdef NDBM
	if (bitset(MF_IMPL_NDBM, map->map_mflags))
	{
		ndbm_map_close(map);
		map->map_mflags &= ~MF_IMPL_NDBM;
	}
#endif
}
/*
**  NULL stubs
*/

bool
null_map_open(map, mode)
	MAP *map;
	int mode;
{
	return TRUE;
}

void
null_map_close(map)
	MAP *map;
{
	return;
}

void
null_map_store(map, key, val)
	MAP *map;
	char *key;
	char *val;
{
	return;
}
