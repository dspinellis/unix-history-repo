/*
 * Copyright (c) 1992 Eric P. Allman.
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)map.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "sendmail.h"
#include <sys/file.h>

#ifdef DBM_MAP
#include <ndbm.h>
#endif
#if defined(HASH_MAP) || defined(BTREE_MAP)
#include <db.h>
#endif


#ifdef DBM_MAP

/*
**  DBM_MAP_INIT -- DBM-style map initialization
**
**	Parameters:
**		map -- the pointer to the actual map
**		mapname -- the name of the map (for error messages)
**		args -- a pointer to the config file line arguments
**
**	Returns:
**		TRUE -- if it could successfully open the map.
**		FALSE -- otherwise.
**
**	Side Effects:
**		Gives an error if it can't open the map.
*/

bool
dbm_map_init(map, mapname, args)
	MAP *map;
	char *mapname;
	char *args;
{
	DBM *dbm;

	map_parseargs(map, &args, mapname);
	if (map->map_file == NULL)
		return FALSE;
	dbm = dbm_open(map->map_file, O_RDONLY, 0644);
	if (dbm == NULL)
	{
		syserr("Cannot open DBM database %s", map->map_file);
		return FALSE;
	}
	map->map_db = (void *) dbm;
	return TRUE;
}
/*
**  DBM_MAP_LOOKUP -- look up a datum in a DBM-type map
**
**	Parameters:
**		map -- the map to look up in.
**		buf -- a pointer to to the buffer containing the key.
**			This is a null terminated string.
**		bufsiz -- the size of buf -- note that this is in general
**			larger that strlen(buf), and buf can be changed
**			in place if desired.
**		av -- arguments from the config file (can be interpolated
**			into the final result).
**
**	Returns:
**		A pointer to the rewritten result.
**		NULL if not found in the map.
*/

char *
dbm_map_lookup(map, buf, bufsiz, av)
	MAP *map;
	char buf[];
	int bufsiz;
	char **av;
{
	datum key, val;

	key.dptr = buf;
	key.dsize = strlen(buf);
	if (bitset(MF_INCLNULL, map->map_flags))
		key.dsize++;
	val = dbm_fetch(map->map_db, key);
	if (val.dptr == NULL)
		return NULL;
	map_rewrite(val.dptr, val.dsize, buf, bufsiz, av);
	return buf;
}

#endif /* DBM_MAP */

#ifdef BTREE_MAP

/*
**  BTREE_MAP_INIT -- BTREE-style map initialization
**
**	Parameters:
**		map -- the pointer to the actual map
**		mapname -- the name of the map (for error messages)
**		args -- a pointer to the config file line arguments
**
**	Returns:
**		TRUE -- if it could successfully open the map.
**		FALSE -- otherwise.
**
**	Side Effects:
**		Gives an error if it can't open the map.
*/

bool
bt_map_init(map, mapname, args)
	MAP *map;
	char *mapname;
	char *args;
{
	DB *db;

	map_parseargs(map, &args, mapname);
	if (map->map_file == NULL)
		return FALSE;
	db = dbopen(map->map_file, O_RDONLY, 0644, DB_BTREE, NULL);
	if (db == NULL)
	{
		syserr("Cannot open BTREE database %s", map->map_file);
		return FALSE;
	}
	map->map_db = (void *) db;
	return TRUE;
}

#endif /* BTREE_MAP */

#ifdef HASH_MAP

/*
**  HASH_MAP_INIT -- HASH-style map initialization
**
**	Parameters:
**		map -- the pointer to the actual map
**		mapname -- the name of the map (for error messages)
**		args -- a pointer to the config file line arguments
**
**	Returns:
**		TRUE -- if it could successfully open the map.
**		FALSE -- otherwise.
**
**	Side Effects:
**		Gives an error if it can't open the map.
*/

bool
hash_map_init(map, mapname, args)
	MAP *map;
	char *mapname;
	char *args;
{
	DB *db;

	map_parseargs(map, &args, mapname);
	if (map->map_file == NULL)
		return FALSE;
	db = dbopen(map->map_file, O_RDONLY, 0644, DB_HASH, NULL);
	if (db == NULL)
	{
		syserr("Cannot open HASH database %s", map->map_file);
		return FALSE;
	}
	map->map_db = (void *) db;
	return TRUE;
}

#endif /* HASH_MAP */

#if defined(BTREE_MAP) || defined(HASH_MAP)

/*
**  DB_MAP_LOOKUP -- look up a datum in a BTREE- or HASH-type map
**
**	Parameters:
**		map -- the map to look up in.
**		buf -- a pointer to to the buffer containing the key.
**			This is a null terminated string.
**		bufsiz -- the size of buf -- note that this is in general
**			larger that strlen(buf), and buf can be changed
**			in place if desired.
**		av -- arguments from the config file (can be interpolated
**			into the final result).
**
**	Returns:
**		A pointer to the rewritten result.
**		NULL if not found in the map.
*/

char *
db_map_lookup(map, buf, bufsiz, av)
	MAP *map;
	char buf[];
	int bufsiz;
	char **av;
{
	DBT key, val;

	key.data = buf;
	key.size = strlen(buf);
	if (bitset(MF_INCLNULL, map->map_flags))
		key.size++;
	if ((*((DB *) map->map_db)->get)((DB *) map->map_db, &key, &val, 0) != 0)
		return NULL;
	map_rewrite(val.data, val.size, buf, bufsiz, av);
	return buf;
}

#endif /* BTREE_MAP || HASH_MAP */
/*
**  MAP_PARSEARGS -- parse config line arguments for database lookup
**
**	Parameters:
**		map -- the map being initialized.
**		pp -- an indirect pointer to the config line.  It will
**			be replaced with a pointer to the next field
**			on the line.
**		mapname -- the name of the map (for errors).
**
**	Returns:
**		none
**
**	Side Effects:
**		null terminates the filename; stores it in map
*/

map_parseargs(map, pp, mapname)
	MAP *map;
	char **pp;
	char *mapname;
{
	register char *p = *pp;

	for (;;)
	{
		while (isspace(*p))
			p++;
		if (*p != '-')
			break;
		switch (*++p)
		{
		  case 'N':
			map->map_flags |= MF_INCLNULL;
			break;
		}
		while (*p != '\0' && !isspace(*p))
			p++;
	}

	if (*p == '\0')
	{
		syserr("No file name for map %s", mapname);
		return NULL;
	}
	map->map_file = p;
	while (*p != '\0' && !isspace(*p))
		p++;
	if (*p != '\0')
		*p++ = '\0';
	*pp = p;
}
/*
**  MAP_REWRITE -- rewrite a database key, interpolating %n indications.
**
**	Parameters:
**		s -- the string to rewrite, NOT necessarily null terminated.
**		slen -- the length of s.
**		buf -- the place to write it.
**		buflen -- the length of buf.
**		av -- arguments to interpolate into buf.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

map_rewrite(s, slen, buf, buflen, av)
	register char *s;
	int slen;
	char buf[];
	int buflen;
	char **av;
{
	register char *bp;
	char *buflim;
	register char c;
	char **avp;
	register char *ap;

	if (tTd(23, 1))
	{
		printf("map_rewrite(%.*s), av =\n", slen, s);
		for (avp = av; *avp != NULL; avp++)
			printf("\t%s\n", *avp);
	}

	bp = buf;
	buflim = &buf[buflen - 2];
	while (--slen >= 0 && (c = *s++) != '\0')
	{
		if (c != '%')
		{
  pushc:
			if (bp < buflim)
				*bp++ = c;
			continue;
		}
		if (--slen < 0 || (c = *s++) == '\0')
			c = '%';
		if (c == '%')
			goto pushc;
		if (!isdigit(c))
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
		{
			if (bp < buflim)
				*bp++ = c;
		}
	}
	*bp++ = '\0';
	if (tTd(23, 1))
		printf("map_rewrite => %s\n", buf);
}
