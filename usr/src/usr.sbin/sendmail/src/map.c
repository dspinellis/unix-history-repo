/*
 * Copyright (c) 1992 Eric P. Allman.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)map.c	8.46 (Berkeley) %G%";
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
**	char *map_lookup(MAP *map, char *key, char **args, int *pstat)
**		Look up the key in the given map.  If found, do any
**		rewriting the map wants (including "args" if desired)
**		and return the value.  Set *pstat to the appropriate status
**		on error and return NULL.  Args will be NULL if called
**		from the alias routines, although this should probably
**		not be relied upon.  It is suggested you call map_rewrite
**		to return the results -- it takes care of null termination
**		and uses a dynamically expanded buffer as needed.
**
**	void map_store(MAP *map, char *key, char *value)
**		Store the key:value pair in the map.
**
**	bool map_open(MAP *map, int mode)
**		Open the map for the indicated mode.  Mode should
**		be either O_RDONLY or O_RDWR.  Return TRUE if it
**		was opened successfully, FALSE otherwise.  If the open
**		failed an the MF_OPTIONAL flag is not set, it should
**		also print an error.  If the MF_ALIAS bit is set
**		and this map class understands the @:@ convention, it
**		should call aliaswait() before returning.
**
**	void map_close(MAP *map)
**		Close the map.
*/

#define DBMMODE		0644

extern bool	aliaswait __P((MAP *, char *, int));
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

	map->map_mflags |= MF_TRY0NULL | MF_TRY1NULL;
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
			map->map_mflags &= ~MF_TRY0NULL;
			break;

		  case 'O':
			map->map_mflags &= ~MF_TRY1NULL;
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

		  case 'A':
			map->map_mflags |= MF_APPEND;
			break;

		  case 'a':
			map->map_app = ++p;
			break;

		  case 'k':
			while (isascii(*++p) && isspace(*p))
				continue;
			map->map_keycolnm = p;
			break;

		  case 'v':
			while (isascii(*++p) && isspace(*p))
				continue;
			map->map_valcolnm = p;
			break;

		  case 'z':
			if (*++p != '\\')
				map->map_coldelim = *p;
			else
			{
				switch (*++p)
				{
				  case 'n':
					map->map_coldelim = '\n';
					break;

				  case 't':
					map->map_coldelim = '\t';
					break;

				  default:
					map->map_coldelim = '\\';
				}
			}
			break;
#ifdef RESERVED_FOR_SUN
		  case 'd':
			map->map_mflags |= MF_DOMAIN_WIDE;
			break;

		  case 's':
			/* info type */
			break;
#endif
		}
		while (*p != '\0' && !(isascii(*p) && isspace(*p)))
			p++;
		if (*p != '\0')
			*p++ = '\0';
	}
	if (map->map_app != NULL)
		map->map_app = newstr(map->map_app);
	if (map->map_keycolnm != NULL)
		map->map_keycolnm = newstr(map->map_keycolnm);
	if (map->map_valcolnm != NULL)
		map->map_valcolnm = newstr(map->map_valcolnm);

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

	if (map->map_file == NULL &&
	    !bitset(MCF_OPTFILE, map->map_class->map_cflags))
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
**		Pointer to rewritten result.  This is static data that
**		should be copied if it is to be saved!
**
**	Side Effects:
**		none.
*/

struct rwbuf
{
	int	rwb_len;	/* size of buffer */
	char	*rwb_buf;	/* ptr to buffer */
};

struct rwbuf	RwBufs[2];	/* buffers for rewriting output */

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
	register struct rwbuf *rwb;
	int i;
	int len;

	if (tTd(39, 1))
	{
		printf("map_rewrite(%.*s), av =", slen, s);
		if (av == NULL)
			printf(" (nullv)");
		else
		{
			for (avp = av; *avp != NULL; avp++)
				printf("\n\t%s", *avp);
		}
		printf("\n");
	}

	rwb = RwBufs;
	if (av == NULL)
		rwb++;

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
			for (avp = av; --c >= '0' && *avp != NULL; avp++)
				continue;
			if (*avp == NULL)
				continue;
			len += strlen(*avp);
		}
	}
	if (map->map_app != NULL)
		len += strlen(map->map_app);
	if (rwb->rwb_len < ++len)
	{
		/* need to malloc additional space */
		rwb->rwb_len = len;
		if (rwb->rwb_buf != NULL)
			free(rwb->rwb_buf);
		rwb->rwb_buf = xalloc(rwb->rwb_len);
	}

	bp = rwb->rwb_buf;
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
			for (avp = av; --c >= '0' && *avp != NULL; avp++)
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
	if (tTd(39, 1))
		printf("map_rewrite => %s\n", rwb->rwb_buf);
	return rwb->rwb_buf;
}
/*
**  INITMAPS -- initialize for aliasing
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

initmaps(rebuild, e)
	bool rebuild;
	register ENVELOPE *e;
{
	extern void map_init();

#ifdef XDEBUG
	checkfd012("entering initmaps");
#endif
	CurEnv = e;
	if (rebuild)
	{
		stabapply(map_init, 1);
		stabapply(map_init, 2);
	}
	else
	{
		stabapply(map_init, 0);
	}
#ifdef XDEBUG
	checkfd012("exiting initmaps");
#endif
}

void
map_init(s, rebuild)
	register STAB *s;
	int rebuild;
{
	register MAP *map;

	/* has to be a map */
	if (s->s_type != ST_MAP)
		return;

	map = &s->s_map;
	if (!bitset(MF_VALID, map->map_mflags))
		return;

	if (tTd(38, 2))
		printf("map_init(%s:%s, %s, %d)\n",
			map->map_class->map_cname == NULL ? "NULL" :
				map->map_class->map_cname,
			map->map_mname == NULL ? "NULL" : map->map_mname,
			map->map_file == NULL ? "NULL" : map->map_file,
			rebuild);

	if (rebuild == (bitset(MF_ALIAS, map->map_mflags) &&
		    bitset(MCF_REBUILDABLE, map->map_class->map_cflags) ? 1 : 2))
	{
		if (tTd(38, 3))
			printf("\twrong pass\n");
		return;
	}

	/* if already open, close it (for nested open) */
	if (bitset(MF_OPEN, map->map_mflags))
	{
		map->map_class->map_close(map);
		map->map_mflags &= ~(MF_OPEN|MF_WRITABLE);
	}

	if (rebuild == 2)
	{
		rebuildaliases(map, FALSE);
	}
	else
	{
		if (map->map_class->map_open(map, O_RDONLY))
		{
			if (tTd(38, 4))
				printf("\t%s:%s %s: valid\n",
					map->map_class->map_cname == NULL ? "NULL" :
						map->map_class->map_cname,
					map->map_mname == NULL ? "NULL" :
						map->map_mname,
					map->map_file == NULL ? "NULL" :
						map->map_file);
			map->map_mflags |= MF_OPEN;
		}
		else
		{
			if (tTd(38, 4))
				printf("\t%s:%s %s: invalid: %s\n",
					map->map_class->map_cname == NULL ? "NULL" :
						map->map_class->map_cname,
					map->map_mname == NULL ? "NULL" :
						map->map_mname,
					map->map_file == NULL ? "NULL" :
						map->map_file,
					errstring(errno));
			if (!bitset(MF_OPTIONAL, map->map_mflags))
			{
				extern MAPCLASS BogusMapClass;

				map->map_class = &BogusMapClass;
				map->map_mflags |= MF_OPEN;
			}
		}
	}
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
	register DBM *dbm;
	struct stat st;

	if (tTd(38, 2))
		printf("ndbm_map_open(%s, %s, %d)\n",
			map->map_mname, map->map_file, mode);

	if (mode == O_RDWR)
		mode |= O_CREAT|O_TRUNC;

	/* open the database */
	dbm = dbm_open(map->map_file, mode, DBMMODE);
	if (dbm == NULL)
	{
		if (aliaswait(map, ".pag", FALSE))
			return TRUE;
		if (!bitset(MF_OPTIONAL, map->map_mflags))
			syserr("Cannot open DBM database %s", map->map_file);
		return FALSE;
	}
	map->map_db1 = (void *) dbm;
	if (mode == O_RDONLY)
	{
		if (bitset(MF_ALIAS, map->map_mflags) &&
		    !aliaswait(map, ".pag", TRUE))
			return FALSE;
	}
	else
	{
		int fd;

		/* exclusive lock for duration of rebuild */
		fd = dbm_dirfno((DBM *) map->map_db1);
		if (fd >= 0 && !bitset(MF_LOCKED, map->map_mflags) &&
		    lockfile(fd, map->map_file, ".dir", LOCK_EX))
			map->map_mflags |= MF_LOCKED;
	}
	if (fstat(dbm_dirfno((DBM *) map->map_db1), &st) >= 0)
		map->map_mtime = st.st_mtime;
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
	int fd;
	char keybuf[MAXNAME + 1];

	if (tTd(38, 20))
		printf("ndbm_map_lookup(%s, %s)\n",
			map->map_mname, name);

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
	fd = dbm_dirfno((DBM *) map->map_db1);
	if (fd >= 0 && !bitset(MF_LOCKED, map->map_mflags))
		(void) lockfile(fd, map->map_file, ".dir", LOCK_SH);
	val.dptr = NULL;
	if (bitset(MF_TRY0NULL, map->map_mflags))
	{
		val = dbm_fetch((DBM *) map->map_db1, key);
		if (val.dptr != NULL)
			map->map_mflags &= ~MF_TRY1NULL;
	}
	if (val.dptr == NULL && bitset(MF_TRY1NULL, map->map_mflags))
	{
		key.dsize++;
		val = dbm_fetch((DBM *) map->map_db1, key);
		if (val.dptr != NULL)
			map->map_mflags &= ~MF_TRY0NULL;
	}
	if (fd >= 0 && !bitset(MF_LOCKED, map->map_mflags))
		(void) lockfile(fd, map->map_file, ".dir", LOCK_UN);
	if (val.dptr == NULL)
		return NULL;
	if (bitset(MF_MATCHONLY, map->map_mflags))
		return map_rewrite(map, name, strlen(name), NULL);
	else
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

	if (tTd(38, 12))
		printf("ndbm_map_store(%s, %s, %s)\n",
			map->map_mname, lhs, rhs);

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
		if (!bitset(MF_APPEND, map->map_mflags))
			usrerr("050 Warning: duplicate alias name %s", lhs);
		else
		{
			static char *buf = NULL;
			static int bufsiz = 0;
			datum old;

			old.dptr = ndbm_map_lookup(map, key.dptr, NULL);
			if (old.dptr != NULL && *old.dptr != '\0')
			{
				old.dsize = strlen(old.dptr);
				if (data.dsize + old.dsize + 2 > bufsiz)
				{
					if (buf != NULL)
						(void) free(buf);
					bufsiz = data.dsize + old.dsize + 2;
					buf = xalloc(bufsiz);
				}
				sprintf(buf, "%s,%s", data.dptr, old.dptr);
				data.dsize = data.dsize + old.dsize + 1;
				data.dptr = buf;
				if (tTd(38, 9))
					printf("ndbm_map_store append=%s\n", data.dptr);
			}
		}
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
	if (tTd(38, 9))
		printf("ndbm_map_close(%s, %s, %x)\n",
			map->map_mname, map->map_file, map->map_mflags);

	if (bitset(MF_WRITABLE, map->map_mflags))
	{
#ifdef NIS
		bool inclnull;
		char buf[200];

		inclnull = bitset(MF_INCLNULL, map->map_mflags);
		map->map_mflags &= ~MF_INCLNULL;

		(void) sprintf(buf, "%010ld", curtime());
		ndbm_map_store(map, "YP_LAST_MODIFIED", buf);

		(void) gethostname(buf, sizeof buf);
		ndbm_map_store(map, "YP_MASTER_NAME", buf);

		if (inclnull)
			map->map_mflags |= MF_INCLNULL;
#endif

		/* write out the distinguished alias */
		ndbm_map_store(map, "@", "@");
	}
	dbm_close((DBM *) map->map_db1);
}

#endif
/*
**  NEWDB (Hash and BTree) Modules
*/

#ifdef NEWDB

/*
**  BT_MAP_OPEN, HASH_MAP_OPEN -- database open primitives.
**
**	These do rather bizarre locking.  If you can lock on open,
**	do that to avoid the condition of opening a database that
**	is being rebuilt.  If you don't, we'll try to fake it, but
**	there will be a race condition.  If opening for read-only,
**	we immediately release the lock to avoid freezing things up.
**	We really ought to hold the lock, but guarantee that we won't
**	be pokey about it.  That's hard to do.
*/

bool
bt_map_open(map, mode)
	MAP *map;
	int mode;
{
	DB *db;
	int i;
	int omode;
	int fd;
	struct stat st;
	char buf[MAXNAME];

	if (tTd(38, 2))
		printf("bt_map_open(%s, %s, %d)\n",
			map->map_mname, map->map_file, mode);

	omode = mode;
	if (omode == O_RDWR)
	{
		omode |= O_CREAT|O_TRUNC;
#if defined(O_EXLOCK) && HASFLOCK
		omode |= O_EXLOCK;
# if !OLD_NEWDB
	}
	else
	{
		omode |= O_SHLOCK;
# endif
#endif
	}

	(void) strcpy(buf, map->map_file);
	i = strlen(buf);
	if (i < 3 || strcmp(&buf[i - 3], ".db") != 0)
		(void) strcat(buf, ".db");
	db = dbopen(buf, omode, DBMMODE, DB_BTREE, NULL);
	if (db == NULL)
	{
#ifdef MAYBENEXTRELEASE
		if (aliaswait(map, ".db", FALSE))
			return TRUE;
#endif
		if (!bitset(MF_OPTIONAL, map->map_mflags))
			syserr("Cannot open BTREE database %s", map->map_file);
		return FALSE;
	}
#if !OLD_NEWDB
	fd = db->fd(db);
# if HASFLOCK
#  if !defined(O_EXLOCK)
	if (mode == O_RDWR && fd >= 0)
	{
		if (lockfile(fd, map->map_file, ".db", LOCK_EX))
			map->map_mflags |= MF_LOCKED;
	}
#  else
	if (mode == O_RDONLY && fd >= 0)
		(void) lockfile(fd, map->map_file, ".db", LOCK_UN);
	else
		map->map_mflags |= MF_LOCKED;
#  endif
# endif
#endif

	/* try to make sure that at least the database header is on disk */
	if (mode == O_RDWR)
#if OLD_NEWDB
		(void) db->sync(db);
#else
		(void) db->sync(db, 0);

	if (fd >= 0 && fstat(fd, &st) >= 0)
		map->map_mtime = st.st_mtime;
#endif

	map->map_db2 = (void *) db;
	if (mode == O_RDONLY && bitset(MF_ALIAS, map->map_mflags))
		if (!aliaswait(map, ".db", TRUE))
			return FALSE;
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
	int i;
	int omode;
	int fd;
	struct stat st;
	char buf[MAXNAME];

	if (tTd(38, 2))
		printf("hash_map_open(%s, %s, %d)\n",
			map->map_mname, map->map_file, mode);

	omode = mode;
	if (omode == O_RDWR)
	{
		omode |= O_CREAT|O_TRUNC;
#if defined(O_EXLOCK) && HASFLOCK
		omode |= O_EXLOCK;
# if !OLD_NEWDB
	}
	else
	{
		omode |= O_SHLOCK;
# endif
#endif
	}

	(void) strcpy(buf, map->map_file);
	i = strlen(buf);
	if (i < 3 || strcmp(&buf[i - 3], ".db") != 0)
		(void) strcat(buf, ".db");
	db = dbopen(buf, omode, DBMMODE, DB_HASH, NULL);
	if (db == NULL)
	{
#ifdef MAYBENEXTRELEASE
		if (aliaswait(map, ".db", FALSE))
			return TRUE;
#endif
		if (!bitset(MF_OPTIONAL, map->map_mflags))
			syserr("Cannot open HASH database %s", map->map_file);
		return FALSE;
	}
#if !OLD_NEWDB
	fd = db->fd(db);
# if HASFLOCK
#  if !defined(O_EXLOCK)
	if (mode == O_RDWR && fd >= 0)
	{
		if (lockfile(fd, map->map_file, ".db", LOCK_EX))
			map->map_mflags |= MF_LOCKED;
	}
#  else
	if (mode == O_RDONLY && fd >= 0)
		(void) lockfile(fd, map->map_file, ".db", LOCK_UN);
	else
		map->map_mflags |= MF_LOCKED;
#  endif
# endif
#endif

	/* try to make sure that at least the database header is on disk */
	if (mode == O_RDWR)
#if OLD_NEWDB
		(void) db->sync(db);
#else
		(void) db->sync(db, 0);

	if (fd >= 0 && fstat(fd, &st) >= 0)
		map->map_mtime = st.st_mtime;
#endif

	map->map_db2 = (void *) db;
	if (mode == O_RDONLY && bitset(MF_ALIAS, map->map_mflags))
		if (!aliaswait(map, ".db", TRUE))
			return FALSE;
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
	register DB *db = (DB *) map->map_db2;
	int st;
	int saveerrno;
	int fd;
	char keybuf[MAXNAME + 1];

	if (tTd(38, 20))
		printf("db_map_lookup(%s, %s)\n",
			map->map_mname, name);

	key.size = strlen(name);
	if (key.size > sizeof keybuf - 1)
		key.size = sizeof keybuf - 1;
	key.data = keybuf;
	bcopy(name, keybuf, key.size + 1);
	if (!bitset(MF_NOFOLDCASE, map->map_mflags))
		makelower(keybuf);
#if !OLD_NEWDB
	fd = db->fd(db);
	if (fd >= 0 && !bitset(MF_LOCKED, map->map_mflags))
		(void) lockfile(db->fd(db), map->map_file, ".db", LOCK_SH);
#endif
	st = 1;
	if (bitset(MF_TRY0NULL, map->map_mflags))
	{
		st = db->get(db, &key, &val, 0);
		if (st == 0)
			map->map_mflags &= ~MF_TRY1NULL;
	}
	if (st != 0 && bitset(MF_TRY1NULL, map->map_mflags))
	{
		key.size++;
		st = db->get(db, &key, &val, 0);
		if (st == 0)
			map->map_mflags &= ~MF_TRY0NULL;
	}
	saveerrno = errno;
#if !OLD_NEWDB
	if (fd >= 0 && !bitset(MF_LOCKED, map->map_mflags))
		(void) lockfile(fd, map->map_file, ".db", LOCK_UN);
#endif
	if (st != 0)
	{
		errno = saveerrno;
		if (st < 0)
			syserr("db_map_lookup: get (%s)", name);
		return NULL;
	}
	if (bitset(MF_MATCHONLY, map->map_mflags))
		return map_rewrite(map, name, strlen(name), NULL);
	else
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

	if (tTd(38, 20))
		printf("db_map_store(%s, %s, %s)\n",
			map->map_mname, lhs, rhs);

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
		if (!bitset(MF_APPEND, map->map_mflags))
			usrerr("050 Warning: duplicate alias name %s", lhs);
		else
		{
			static char *buf = NULL;
			static int bufsiz = 0;
			DBT old;

			old.data = db_map_lookup(map, key.data, NULL, &stat);
			if (old.data != NULL)
			{
				old.size = strlen(old.data);
				if (data.size + old.size + 2 > bufsiz)
				{
					if (buf != NULL)
						(void) free(buf);
					bufsiz = data.size + old.size + 2;
					buf = xalloc(bufsiz);
				}
				sprintf(buf, "%s,%s", data.data, old.data);
				data.size = data.size + old.size + 1;
				data.data = buf;
				if (tTd(38, 9))
					printf("db_map_store append=%s\n", data.data);
			}
		}
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

	if (tTd(38, 9))
		printf("db_map_close(%s, %s, %x)\n",
			map->map_mname, map->map_file, map->map_mflags);

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

# ifndef YPERR_BUSY
#  define YPERR_BUSY	16
# endif

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

	if (tTd(38, 2))
		printf("nis_map_open(%s, %s)\n",
			map->map_mname, map->map_file);

	if (mode != O_RDONLY)
	{
		/* issue a pseudo-error message */
#ifdef ENOSYS
		errno = ENOSYS;
#else
# ifdef EFTYPE
		errno = EFTYPE;
# else
		errno = ENXIO;
# endif
#endif
		return FALSE;
	}

	p = strchr(map->map_file, '@');
	if (p != NULL)
	{
		*p++ = '\0';
		if (*p != '\0')
			map->map_domain = p;
	}

	if (*map->map_file == '\0')
		map->map_file = "mail.aliases";

	if (map->map_domain == NULL)
	{
		yperr = yp_get_default_domain(&map->map_domain);
		if (yperr != 0)
		{
			if (!bitset(MF_OPTIONAL, map->map_mflags))
				syserr("421 NIS map %s specified, but NIS not running\n",
					map->map_file);
			return FALSE;
		}
	}

	/* check to see if this map actually exists */
	yperr = yp_match(map->map_domain, map->map_file, "@", 1,
			&vp, &vsize);
	if (tTd(38, 10))
		printf("nis_map_open: yp_match(%s, %s) => %s\n",
			map->map_domain, map->map_file, yperr_string(yperr));
	if (yperr == 0 || yperr == YPERR_KEY || yperr == YPERR_BUSY)
	{
		if (!bitset(MF_ALIAS, map->map_mflags) ||
		    aliaswait(map, NULL, TRUE))
			return TRUE;
	}

	if (!bitset(MF_OPTIONAL, map->map_mflags))
		syserr("421 Cannot bind to domain %s: %s", map->map_domain,
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

	if (tTd(38, 20))
		printf("nis_map_lookup(%s, %s)\n",
			map->map_mname, name);

	buflen = strlen(name);
	if (buflen > sizeof keybuf - 1)
		buflen = sizeof keybuf - 1;
	bcopy(name, keybuf, buflen + 1);
	if (!bitset(MF_NOFOLDCASE, map->map_mflags))
		makelower(keybuf);
	yperr = YPERR_KEY;
	if (bitset(MF_TRY0NULL, map->map_mflags))
	{
		yperr = yp_match(map->map_domain, map->map_file, keybuf, buflen,
			     &vp, &vsize);
		if (yperr == 0)
			map->map_mflags &= ~MF_TRY1NULL;
	}
	if (yperr == YPERR_KEY && bitset(MF_TRY1NULL, map->map_mflags))
	{
		buflen++;
		yperr = yp_match(map->map_domain, map->map_file, keybuf, buflen,
			     &vp, &vsize);
		if (yperr == 0)
			map->map_mflags &= ~MF_TRY0NULL;
	}
	if (yperr != 0)
	{
		if (yperr != YPERR_KEY && yperr != YPERR_BUSY)
			map->map_mflags &= ~(MF_VALID|MF_OPEN);
		return NULL;
	}
	if (bitset(MF_MATCHONLY, map->map_mflags))
		return map_rewrite(map, name, strlen(name), NULL);
	else
		return map_rewrite(map, vp, vsize, av);
}

#endif
/*
**  NISPLUS Modules
**
**	This code donated by Sun Microsystems.
*/

#ifdef NISPLUS

#undef NIS /* symbol conflict in nis.h */
#include <rpcsvc/nis.h>
#include <rpcsvc/nislib.h>

#define EN_col(col)	zo_data.objdata_u.en_data.en_cols.en_cols_val[(col)].ec_value.ec_value_val
#define COL_NAME(res,i)	((res->objects.objects_val)->TA_data.ta_cols.ta_cols_val)[i].tc_name
#define COL_MAX(res)	((res->objects.objects_val)->TA_data.ta_cols.ta_cols_len)
#define PARTIAL_NAME(x)	((x)[strlen(x) - 1] != '.')

/*
**  NISPLUS_MAP_OPEN -- open nisplus table
*/

bool
nisplus_map_open(map, mode)
	MAP *map;
	int mode;
{
	register char *p;
	char qbuf[MAXLINE + NIS_MAXNAMELEN];
	nis_result *res = NULL;
	u_int objs_len;
	nis_object *obj_ptr;
	int retry_cnt, max_col, i;

	if (tTd(38, 2))
		printf("nisplus_map_open(%s, %s, %d)\n",
			map->map_mname, map->map_file, mode);

	if (mode != O_RDONLY)
	{
		errno = ENODEV;
		return FALSE;
	}

	if (*map->map_file == '\0')
		map->map_file = "mail_aliases.org_dir";

	if (PARTIAL_NAME(map->map_file) && map->map_domain == NULL)
	{
		/* set default NISPLUS Domain to $m */
		extern char *nisplus_default_domain();

		map->map_domain = newstr(nisplus_default_domain());
		if (tTd(38, 2))
			printf("nisplus_map_open(%s): using domain %s\n",
				 map->map_file, map->map_domain);
	}
	if (!PARTIAL_NAME(map->map_file))
		map->map_domain = newstr("");

	/* check to see if this map actually exists */
	if (PARTIAL_NAME(map->map_file))
		sprintf(qbuf, "%s.%s", map->map_file, map->map_domain);
	else
		strcpy(qbuf, map->map_file);
	
	retry_cnt = 0;
	while (res == NULL || res->status != NIS_SUCCESS)
	{
		res = nis_lookup(qbuf, FOLLOW_LINKS);
		switch (res->status)
		{
		  case NIS_SUCCESS:
		  case NIS_TRYAGAIN:
		  case NIS_RPCERROR:
		  case NIS_NAMEUNREACHABLE:
			break;

		  default:		/* all other nisplus errors */
#if 0
			if (!bitset(MF_OPTIONAL, map->map_mflags))
				syserr("421 Cannot find table %s.%s: %s",
					map->map_file, map->map_domain,
					nis_sperrno(res->status));
#endif
			errno = EBADR;
			return FALSE;
		}
		sleep(2);		/* try not to overwhelm hosed server */
		if (retry_cnt++ > 4)
		{
			errno = EBADR;
			return FALSE;
		}
	}

	if (NIS_RES_NUMOBJ(res) != 1 ||
	    (NIS_RES_OBJECT(res)->zo_data.zo_type != TABLE_OBJ))
	{
		if (tTd(38, 10))
			printf("nisplus_map_open: %s is not a table\n", qbuf);
#if 0
		if (!bitset(MF_OPTIONAL, map->map_mflags))
			syserr("421 %s.%s: %s is not a table",
				map->map_file, map->map_domain,
				nis_sperrno(res->status));
#endif
		errno = EBADR;
		return FALSE;
	}
	/* default key column is column 0 */
	if (map->map_keycolnm == NULL)
		map->map_keycolnm = newstr(COL_NAME(res,0));

	max_col = COL_MAX(res);
	
	/* verify the key column exist */
	for (i=0; i< max_col; i++)
	{
		if (!strcmp(map->map_keycolnm, COL_NAME(res,i)))
			break;
	}
	if (i == max_col)
	{
		if (tTd(38, 2))
			printf("nisplus_map_open(%s): can not find key column %s\n",
				map->map_file, map->map_keycolnm);
		errno = EBADR;
		return FALSE;
	}

	/* default value column is the last column */
	if (map->map_valcolnm == NULL)
	{
		map->map_valcolno = max_col - 1;
		return TRUE;
	}

	for (i=0; i< max_col; i++)
	{
		if (strcmp(map->map_valcolnm, COL_NAME(res,i)) == 0)
		{
			map->map_valcolno = i;
			return TRUE;
		}
	}

	if (tTd(38, 2))
		printf("nisplus_map_open(%s): can not find column %s\n",
			 map->map_file, map->map_keycolnm);
	errno = EBADR;
	return FALSE;
}


/*
**  NISPLUS_MAP_LOOKUP -- look up a datum in a NISPLUS table
*/

char *
nisplus_map_lookup(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
	char *vp;
	auto int vsize;
	int buflen;
	char search_key[MAXNAME + 1];
	char qbuf[MAXLINE + NIS_MAXNAMELEN];
	nis_result *result;

	if (tTd(38, 20))
		printf("nisplus_map_lookup(%s, %s)\n",
			map->map_mname, name);

	if (!bitset(MF_OPEN, map->map_mflags))
	{
		if (nisplus_map_open(map, O_RDONLY))
			map->map_mflags |= MF_OPEN;
		else
		{
			*statp = EX_UNAVAILABLE;
			return NULL;
		}
	}
		
	buflen = strlen(name);
	if (buflen > sizeof search_key - 1)
		buflen = sizeof search_key - 1;
	bcopy(name, search_key, buflen + 1);
	if (!bitset(MF_NOFOLDCASE, map->map_mflags))
		makelower(search_key);

	/* construct the query */
	if (PARTIAL_NAME(map->map_file))
		sprintf(qbuf, "[%s=%s],%s.%s", map->map_keycolnm,
			search_key, map->map_file, map->map_domain);
	else
		sprintf(qbuf, "[%s=%s],%s", map->map_keycolnm,
			search_key, map->map_file);

	if (tTd(38, 20))
		printf("qbuf=%s\n", qbuf);
	result = nis_list(qbuf, FOLLOW_LINKS | FOLLOW_PATH, NULL, NULL);
	if (result->status == NIS_SUCCESS)
	{
		int count;
		char *str;

		if ((count = NIS_RES_NUMOBJ(result)) != 1)
		{
			if (LogLevel > 10)
				syslog(LOG_WARNING,
				  "%s:Lookup error, expected 1 entry, got (%d)",
				    map->map_file, count);

			/* ignore second entry */
			if (tTd(38, 20))
				printf("nisplus_map_lookup(%s), got %d entries, additional entries ignored\n",
					name, count);
		}

		vp = ((NIS_RES_OBJECT(result))->EN_col(map->map_valcolno));
		/* set the length of the result */
		if (vp == NULL)
			vp = "";
		vsize = strlen(vp);
		if (tTd(38, 20))
			printf("nisplus_map_lookup(%s), found %s\n",
				name, vp);
		if (bitset(MF_MATCHONLY, map->map_mflags))
			str = map_rewrite(map, name, strlen(name), NULL);
		else
			str = map_rewrite(map, vp, vsize, av);
		nis_freeresult(result);
#ifdef MAP_EXIT_STAT
		*statp = EX_OK;
#endif
		return str;
	}
	else
	{
#ifdef MAP_EXIT_STAT
		if (result->status == NIS_NOTFOUND)
			*statp = EX_NOTFOUND;
		else if (result->status == NIS_TRYAGAIN)
			*statp = EX_TEMPFAIL;
		else
		{
			*statp = EX_UNAVAILABLE;
			map->map_mflags &= ~(MF_VALID|MF_OPEN);
		}
#else
		if ((result->status != NIS_NOTFOUND) &&
		    (result->status != NIS_TRYAGAIN))
			map->map_mflags &= ~(MF_VALID|MF_OPEN);
#endif
	}
	if (tTd(38, 20))
		printf("nisplus_map_lookup(%s), failed\n", name);
	nis_freeresult(result);
	return NULL;
}


char *
nisplus_default_domain()
{
	static char default_domain[MAXNAME] = "";
	char *p;

	if (default_domain[0] != '\0')
		return(default_domain);
	
	p = nis_local_directory();
	strcpy(default_domain, p);
	return default_domain;
}

#endif /* NISPLUS */
/*
**  HESIOD Modules
*/

#ifdef HESIOD

#include <hesiod.h>

char *
hes_map_lookup(map, name, av, statp)
        MAP *map;
        char *name;
        char **av;
        int *statp;
{
	char **hp;
	char *retdata = NULL;
	int i;

	if (tTd(38, 20))
		printf("hes_map_lookup(%s, %s)\n", map->map_file, name);

	hp = hes_resolve(name, map->map_file);
	if (hp == NULL)
		return NULL;
	
	if (hp[0] != NULL)
	{
		if (tTd(38, 20))
			printf("  %d %s\n", i, hp[0]);
		if (bitset(MF_MATCHONLY, map->map_mflags))
			retdata = map_rewrite(map, name, strlen(name), NULL);
		else
			retdata = map_rewrite(map, hp[0], strlen(hp[0]), av);
	}

	for (i = 0; hp[i] != NULL; i++)
		free(hp[i]);
	free(hp);
	return retdata;
}

#endif
/*
**  NeXT NETINFO Modules
*/

#ifdef NETINFO

#define NETINFO_DEFAULT_DIR		"/aliases"
#define NETINFO_DEFAULT_PROPERTY	"members"


/*
**  NI_MAP_OPEN -- open NetInfo Aliases
*/

bool
ni_map_open(map, mode)
	MAP *map;
	int mode;
{
	char *p;

	if (tTd(38, 20))
		printf("ni_map_open: %s\n", map->map_file);

	if (*map->map_file == '\0')
		map->map_file = NETINFO_DEFAULT_DIR;

	if (map->map_valcolnm == NULL)
		map->map_valcolnm = NETINFO_DEFAULT_PROPERTY;

	if (map->map_coldelim == '\0' && bitset(MF_ALIAS, map->map_mflags))
		map->map_coldelim = ',';

	return TRUE;
}


/*
**  NI_MAP_LOOKUP -- look up a datum in NetInfo
*/

char *
ni_map_lookup(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
	char *res;
	char *propval;
	extern char *ni_propval();

	if (tTd(38, 20))
		printf("ni_map_lookup(%s, %s)\n",
			map->map_mname, name);

	propval = ni_propval(map->map_file, map->map_keycolnm, name,
			     map->map_valcolnm, map->map_coldelim);

	if (propval == NULL)
		return NULL;

	if (bitset(MF_MATCHONLY, map->map_mflags))
		res = map_rewrite(map, name, strlen(name), NULL);
	else
		res = map_rewrite(map, propval, strlen(propval), av);
	free(propval);
	return res;
}

#endif
/*
**  TEXT (unindexed text file) Modules
**
**	This code donated by Sun Microsystems.
*/


/*
**  TEXT_MAP_OPEN -- open text table
*/

bool
text_map_open(map, mode)
	MAP *map;
	int mode;
{
	struct stat sbuf;

	if (tTd(38, 2))
		printf("text_map_open(%s, %s, %d)\n",
			map->map_mname, map->map_file, mode);

	if (mode != O_RDONLY)
	{
		errno = ENODEV;
		return FALSE;
	}

	if (*map->map_file == '\0')
	{
		if (tTd(38, 2))
			printf("text_map_open: file name required\n");
		return FALSE;
	}

	if (map->map_file[0] != '/')
	{
		if (tTd(38, 2))
			printf("text_map_open(%s): file name must be fully qualified\n",
				map->map_file);
		return FALSE;
	}
	/* check to see if this map actually accessable */
	if (access(map->map_file, R_OK) <0)
		return FALSE;

	/* check to see if this map actually exist */
	if (stat(map->map_file, &sbuf) <0)
	{
		if (tTd(38, 2))
			printf("text_map_open(%s): can not stat %s\n",
				map->map_file, map->map_file);
		return FALSE;
	}

	if (!S_ISREG(sbuf.st_mode))
	{
		if (tTd(38, 2))
			printf("text_map_open(%s): %s is not a file\n",
				map->map_file, map->map_file);
		return FALSE;
	}

	if (map->map_keycolnm == NULL)
		map->map_keycolno = 0;
	else
	{
		if (!isdigit(*map->map_keycolnm))
		{
			if (tTd(38, 2))
				printf("text_map_open(%s): -k should specify a number, not %s\n",
					map->map_file, map->map_keycolnm);
			return FALSE;
		}
		map->map_keycolno = atoi(map->map_keycolnm);
	}

	if (map->map_valcolnm == NULL)
		map->map_valcolno = 0;
	else
	{
		if (!isdigit(*map->map_valcolnm))
		{
			if (tTd(38, 2))
				printf("text_map_open(%s): -v should specify a number, not %s\n",
					map->map_file, map->map_valcolnm);
			return FALSE;
		}
		map->map_valcolno = atoi(map->map_valcolnm);
	}

	if (map->map_coldelim == '\0')
		map->map_coldelim = ':';

	if (tTd(38, 2))
	{
		printf("text_map_open(%s): delimiter = %c\n",
			map->map_file, map->map_coldelim);
	}

	return TRUE;
}


/*
**  TEXT_MAP_LOOKUP -- look up a datum in a TEXT table
*/

char *
text_map_lookup(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
	char *vp;
	auto int vsize;
	int buflen;
	char search_key[MAXNAME + 1];
	char linebuf[MAXLINE];
	FILE *f;
	char buf[MAXNAME+1];
	char delim;
	int key_idx;
	bool found_it;
	extern char *get_column();


	found_it = FALSE;
	if (tTd(38, 20))
		printf("text_map_lookup(%s)\n", name);

	buflen = strlen(name);
	if (buflen > sizeof search_key - 1)
		buflen = sizeof search_key - 1;
	bcopy(name, search_key, buflen + 1);
	if (!bitset(MF_NOFOLDCASE, map->map_mflags))
		makelower(search_key);

	f = fopen(map->map_file, "r");
	if (f == NULL)
	{
		map->map_mflags &= ~(MF_VALID|MF_OPEN);
		*statp = EX_UNAVAILABLE;
		return NULL;
	}
	key_idx = map->map_keycolno;
	delim = map->map_coldelim;
	while (fgets(linebuf, MAXLINE, f))
	{
		char *lf;
		if (linebuf[0] == '#')
			continue; /* skip comment line */
		if (lf = strchr(linebuf, '\n'))
			*lf = '\0';
		if (!strcasecmp(search_key,
				get_column(linebuf, key_idx, delim, buf)))
		{
			found_it = TRUE;
			break;
		}
	}
	fclose(f);
	if (!found_it)
	{
#ifdef MAP_EXIT_STAT
		*statp = EX_NOTFOUND;
#endif
		return(NULL);
	}
	vp = get_column(linebuf, map->map_valcolno, delim, buf);
	vsize = strlen(vp);
#ifdef MAP_EXIT_STAT
	*statp = EX_OK;
#endif
	if (bitset(MF_MATCHONLY, map->map_mflags))
		return map_rewrite(map, name, strlen(name), NULL);
	else
		return map_rewrite(map, vp, vsize, av);
}
/*
**  STAB (Symbol Table) Modules
*/


/*
**  STAB_MAP_LOOKUP -- look up alias in symbol table
*/

char *
stab_map_lookup(map, name, av, pstat)
	register MAP *map;
	char *name;
	char **av;
	int *pstat;
{
	register STAB *s;

	if (tTd(38, 20))
		printf("stab_lookup(%s, %s)\n",
			map->map_mname, name);

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
	struct stat st;

	if (tTd(38, 2))
		printf("stab_map_open(%s, %s)\n",
			map->map_mname, map->map_file);

	if (mode != O_RDONLY)
	{
		errno = ENODEV;
		return FALSE;
	}

	af = fopen(map->map_file, "r");
	if (af == NULL)
		return FALSE;
	readaliases(map, af, FALSE, FALSE);

	if (fstat(fileno(af), &st) >= 0)
		map->map_mtime = st.st_mtime;
	fclose(af);

	return TRUE;
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
	if (tTd(38, 20))
		printf("impl_map_lookup(%s, %s)\n",
			map->map_mname, name);

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

	if (tTd(38, 2))
		printf("impl_map_open(%s, %s, %d)\n",
			map->map_mname, map->map_file, mode);

	if (stat(map->map_file, &stb) < 0)
	{
		/* no alias file at all */
		if (tTd(38, 3))
			printf("no map file\n");
		return FALSE;
	}

#ifdef NEWDB
	map->map_mflags |= MF_IMPL_HASH;
	if (hash_map_open(map, mode))
	{
#if defined(NDBM) && defined(NIS)
		if (mode == O_RDONLY || access("/var/yp/Makefile", R_OK) != 0)
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

#if defined(NEWDB) || defined(NDBM)
	if (Verbose)
		message("WARNING: cannot open alias database %s", map->map_file);
#else
	if (mode != O_RDONLY)
		usrerr("Cannot rebuild aliases: no database format defined");
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
	if (tTd(38, 20))
		printf("impl_map_close(%s, %s, %x)\n",
			map->map_mname, map->map_file, map->map_mflags);
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
**  User map class.
**
**	Provides access to the system password file.
*/

/*
**  USER_MAP_OPEN -- open user map
**
**	Really just binds field names to field numbers.
*/

bool
user_map_open(map, mode)
	MAP *map;
	int mode;
{
	if (tTd(38, 2))
		printf("user_map_open(%s)\n", map->map_mname);

	if (mode != O_RDONLY)
	{
		/* issue a pseudo-error message */
#ifdef ENOSYS
		errno = ENOSYS;
#else
# ifdef EFTYPE
		errno = EFTYPE;
# else
		errno = ENXIO;
# endif
#endif
		return FALSE;
	}
	if (map->map_valcolnm == NULL)
		/* nothing */ ;
	else if (strcasecmp(map->map_valcolnm, "name") == 0)
		map->map_valcolno = 1;
	else if (strcasecmp(map->map_valcolnm, "passwd") == 0)
		map->map_valcolno = 2;
	else if (strcasecmp(map->map_valcolnm, "uid") == 0)
		map->map_valcolno = 3;
	else if (strcasecmp(map->map_valcolnm, "gid") == 0)
		map->map_valcolno = 4;
	else if (strcasecmp(map->map_valcolnm, "gecos") == 0)
		map->map_valcolno = 5;
	else if (strcasecmp(map->map_valcolnm, "dir") == 0)
		map->map_valcolno = 6;
	else if (strcasecmp(map->map_valcolnm, "shell") == 0)
		map->map_valcolno = 7;
	else
	{
		syserr("User map %s: unknown column name %s",
			map->map_mname, map->map_valcolnm);
		return FALSE;
	}
	return TRUE;
}


/*
**  USER_MAP_LOOKUP -- look up a user in the passwd file.
*/

#include <pwd.h>

char *
user_map_lookup(map, key, av, statp)
	MAP *map;
	char *key;
	char **av;
	int *statp;
{
	struct passwd *pw;

	if (tTd(38, 20))
		printf("user_map_lookup(%s, %s)\n",
			map->map_mname, key);

	pw = getpwnam(key);
	if (pw == NULL)
		return NULL;
	if (bitset(MF_MATCHONLY, map->map_mflags))
		return map_rewrite(map, key, strlen(key), NULL);
	else
	{
		char *rwval = NULL;
		char buf[30];

		switch (map->map_valcolno)
		{
		  case 0:
		  case 1:
			rwval = pw->pw_name;
			break;

		  case 2:
			rwval = pw->pw_passwd;
			break;

		  case 3:
			sprintf(buf, "%d", pw->pw_uid);
			rwval = buf;
			break;

		  case 4:
			sprintf(buf, "%d", pw->pw_gid);
			rwval = buf;
			break;

		  case 5:
			rwval = pw->pw_gecos;
			break;

		  case 6:
			rwval = pw->pw_dir;
			break;

		  case 7:
			rwval = pw->pw_shell;
			break;
		}
		return map_rewrite(map, rwval, strlen(rwval), av);
	}
}
/*
**  BESTMX -- find the best MX for a name
**
**	This is really a hack, but I don't see any obvious way
**	to generalize it at the moment.
*/

#if NAMED_BIND

char *
bestmx_map_lookup(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
        int nmx;
        auto int rcode;
        char *mxhosts[MAXMXHOSTS + 1];

	nmx = getmxrr(name, mxhosts, FALSE, &rcode);
	if (nmx <= 0)
		return NULL;
	if (bitset(MF_MATCHONLY, map->map_mflags))
		return map_rewrite(map, name, strlen(name), NULL);
	else
		return map_rewrite(map, mxhosts[0], strlen(mxhosts[0]), av);
}

#endif
/*
**  Sequenced map type.
**
**	Tries each map in order until something matches, much like
**	implicit.  Stores go to the first map in the list that can
**	support storing.
**
**	This is slightly unusual in that there are two interfaces.
**	The "sequence" interface lets you stack maps arbitrarily.
**	The "switch" interface builds a sequence map by looking
**	at a system-dependent configuration file such as
**	/etc/nsswitch.conf on Solaris or /etc/svc.conf on Ultrix.
**
**	We don't need an explicit open, since all maps are
**	opened during startup, including underlying maps.
*/

/*
**  SEQ_MAP_PARSE -- Sequenced map parsing
*/

bool
seq_map_parse(map, ap)
	MAP *map;
	char *ap;
{
	int maxmap;

	if (tTd(38, 2))
		printf("seq_map_parse(%s, %s)\n", map->map_mname, ap);
	maxmap = 0;
	while (*ap != '\0')
	{
		register char *p;
		STAB *s;

		/* find beginning of map name */
		while (isascii(*ap) && isspace(*ap))
			ap++;
		for (p = ap; isascii(*p) && isalnum(*p); p++)
			continue;
		if (*p != '\0')
			*p++ = '\0';
		while (*p != '\0' && (!isascii(*p) || !isalnum(*p)))
			p++;
		if (*ap == '\0')
		{
			ap = p;
			continue;
		}
		s = stab(ap, ST_MAP, ST_FIND);
		if (s == NULL)
		{
			syserr("Sequence map %s: unknown member map %s",
				map->map_mname, ap);
		}
		else if (maxmap == MAXMAPSTACK)
		{
			syserr("Sequence map %s: too many member maps (%d max)",
				map->map_mname, MAXMAPSTACK);
			maxmap++;
		}
		else if (maxmap < MAXMAPSTACK)
		{
			map->map_stack[maxmap++] = &s->s_map;
		}
		ap = p;
	}
	return TRUE;
}


/*
**  SWITCH_MAP_OPEN -- open a switched map
**
**	This looks at the system-dependent configuration and builds
**	a sequence map that does the same thing.
**
**	Every system must define a switch_map_find routine in conf.c
**	that will return the list of service types associated with a
**	given service class.
*/

bool
switch_map_open(map, mode)
	MAP *map;
	int mode;
{
	int mapno;
	int nmaps;
	char *maptype[MAXMAPSTACK];

	if (tTd(38, 2))
		printf("switch_map_open(%s, %s, %d)\n",
			map->map_mname, map->map_file, mode);

	nmaps = switch_map_find(map->map_file, maptype, map->map_return);
	if (tTd(38, 19))
	{
		printf("\tswitch_map_find => %d\n", nmaps);
		for (mapno = 0; mapno < nmaps; mapno++)
			printf("\t\t%s\n", maptype[mapno]);
	}
	if (nmaps <= 0 || nmaps > MAXMAPSTACK)
		return FALSE;

	for (mapno = 0; mapno < nmaps; mapno++)
	{
		register STAB *s;
		char nbuf[MAXNAME + 1];

		if (maptype[mapno] == NULL)
			continue;
		(void) sprintf(nbuf, "%s.%s", map->map_file, maptype[mapno]);
		s = stab(nbuf, ST_MAP, ST_FIND);
		if (s == NULL)
		{
			syserr("Switch map %s: unknown member map %s",
				map->map_mname, nbuf);
		}
		else
		{
			map->map_stack[mapno] = &s->s_map;
			if (tTd(38, 4))
				printf("\tmap_stack[%d] = %s:%s\n",
					mapno, s->s_map.map_class->map_cname,
					nbuf);
		}
	}
	return TRUE;
}


/*
**  SEQ_MAP_CLOSE -- close all underlying maps
*/

seq_map_close(map)
	MAP *map;
{
	int mapno;

	if (tTd(38, 20))
		printf("seq_map_close(%s)\n", map->map_mname);
	for (mapno = 0; mapno < MAXMAPSTACK; mapno++)
	{
		MAP *mm = map->map_stack[mapno];

		if (mm == NULL || !bitset(MF_OPEN, mm->map_mflags))
			continue;
		mm->map_class->map_close(mm);
	}
}


/*
**  SEQ_MAP_LOOKUP -- sequenced map lookup
*/

char *
seq_map_lookup(map, key, args, pstat)
	MAP *map;
	char *key;
	char **args;
	int *pstat;
{
	int mapno;
	int mapbit = 0x01;

	if (tTd(38, 20))
		printf("seq_map_lookup(%s, %s)\n", map->map_mname, key);

	for (mapno = 0; mapno < MAXMAPSTACK; mapbit <<= 1, mapno++)
	{
		MAP *mm = map->map_stack[mapno];
		int stat = 0;
		char *rv;

		if (mm == NULL)
			continue;
		if (!bitset(MF_OPEN, mm->map_mflags))
		{
			if (bitset(mapbit, map->map_return[MA_UNAVAIL]))
			{
				*pstat = EX_UNAVAILABLE;
				return NULL;
			}
			continue;
		}
		rv = mm->map_class->map_lookup(mm, key, args, &stat);
		if (rv != NULL)
			return rv;
		if (stat == 0 && bitset(mapbit, map->map_return[MA_NOTFOUND]))
			return NULL;
		if (stat != 0 && bitset(mapbit, map->map_return[MA_TRYAGAIN]))
		{
			*pstat = stat;
			return NULL;
		}
	}
	return NULL;
}


/*
**  SEQ_MAP_STORE -- sequenced map store
*/

void
seq_map_store(map, key, val)
	MAP *map;
	char *key;
	char *val;
{
	int mapno;

	if (tTd(38, 12))
		printf("seq_map_store(%s, %s, %s)\n",
			map->map_mname, key, val);

	for (mapno = 0; mapno < MAXMAPSTACK; mapno++)
	{
		MAP *mm = map->map_stack[mapno];

		if (mm == NULL || !bitset(MF_WRITABLE, mm->map_mflags))
			continue;

		mm->map_class->map_store(mm, key, val);
		return;
	}
	syserr("seq_map_store(%s, %s, %s): no writable map",
		map->map_mname, key, val);
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


/*
**  BOGUS stubs
*/

char *
bogus_map_lookup(map, key, args, pstat)
	MAP *map;
	char *key;
	char **args;
	int *pstat;
{
	*pstat = EX_TEMPFAIL;
	return NULL;
}

MAPCLASS	BogusMapClass =
{
	"bogus-map",		NULL,		0,
	NULL,		bogus_map_lookup,	null_map_store,
	null_map_open,	null_map_close,
};
