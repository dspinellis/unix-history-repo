/*
 * Copyright (c) 1992 Eric P. Allman.
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)makemap.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <sysexits.h>
#include <sys/file.h>
#include <ctype.h>
#include <string.h>
#include "useful.h"
#include "conf.h"

#ifdef DBM_MAP
#include <ndbm.h>
#endif

#if defined(HASH_MAP) || defined(BTREE_MAP)
#include <db.h>
#endif

enum type { T_DBM, T_BTREE, T_HASH, T_ERR, T_UNKNOWN };

union dbent
{
#ifdef DBM_MAP
	datum	dbm;
#endif
#if defined(HASH_MAP) || defined(BTREE_MAP)
	DBT	db;
#endif
	struct
	{
		char	*data;
		int	size;
	} xx;
};

#define BUFSIZE		1024

main(argc, argv)
	int argc;
	char **argv;
{
	char *progname;
	bool inclnull = FALSE;
	bool notrunc = FALSE;
	bool allowreplace = FALSE;
	bool verbose = FALSE;
	int exitstat;
	int opt;
	char *typename;
	char *mapname;
	int lineno;
	int st;
	int mode;
	enum type type;
	union
	{
#ifdef DBM_MAP
		DBM	*dbm;
#endif
#if defined(HASH_MAP) || defined(BTREE_MAP)
		DB	*db;
#endif
		void	*dbx;
	} dbp;
	union dbent key, val;
	char ibuf[BUFSIZE];
	extern char *optarg;
	extern int optind;

	progname = argv[0];

	while ((opt = getopt(argc, argv, "Norv")) != EOF)
	{
		switch (opt)
		{
		  case 'N':
			inclnull = TRUE;
			break;

		  case 'o':
			notrunc = TRUE;
			break;

		  case 'r':
			allowreplace = TRUE;
			break;

		  case 'v':
			verbose = TRUE;
			break;

		  default:
			type = T_ERR;
			break;
		}
	}

	argc -= optind;
	argv += optind;
	if (argc != 2)
		type = T_ERR;
	else
	{
		typename = argv[0];
		mapname = argv[1];

		if (strcmp(typename, "dbm") == 0)
			type = T_DBM;
		else if (strcmp(typename, "btree") == 0)
			type = T_BTREE;
		else if (strcmp(typename, "hash") == 0)
			type = T_HASH;
		else
			type = T_UNKNOWN;
	}

	switch (type)
	{
	  case T_ERR:
		fprintf(stderr, "Usage: %s [-N] [-o] [-v] type mapname\n", progname);
		exit(EX_USAGE);

	  case T_UNKNOWN:
		fprintf(stderr, "%s: Unknown database type %s\n",
			progname, typename);
		exit(EX_USAGE);

#ifndef DBM_MAP
	  case T_DBM:
#endif
#ifndef BTREE_MAP
	  case T_BTREE:
#endif
#ifndef HASH_MAP
	  case T_HASH:
#endif
		fprintf(stderr, "%s: Type %s not supported in this version\n",
			progname, typename);
		exit(EX_UNAVAILABLE);
	}

	/*
	**  Create the database.
	*/

	mode = O_RDWR;
	if (!notrunc)
		mode |= O_CREAT|O_TRUNC;
	switch (type)
	{
#ifdef DBM_MAP
	  case T_DBM:
		dbp.dbm = dbm_open(mapname, mode, 0644);
		break;
#endif

#ifdef HASH_MAP
	  case T_HASH:
		dbp.db = dbopen(mapname, mode, 0644, DB_HASH, NULL);
		break;
#endif

#ifdef BTREE_MAP
	  case T_BTREE:
		dbp.db = dbopen(mapname, mode, 0644, DB_BTREE, NULL);
		break;
#endif

	  default:
		fprintf(stderr, "%s: internal error: type %d\n", progname, type);
		exit(EX_SOFTWARE);
	}

	if (dbp.dbx == NULL)
	{
		fprintf(stderr, "%s: cannot create type %s map %s\n",
			progname, typename, mapname);
		exit(EX_CANTCREAT);
	}

	/*
	**  Copy the data
	*/

	lineno = 0;
	exitstat = EX_OK;
	while (fgets(ibuf, sizeof ibuf, stdin) != NULL)
	{
		register char *p;

		lineno++;

		/*
		**  Parse the line.
		*/

		p = strchr(ibuf, '\n');
		if (*p != '\0')
			*p = '\0';
		if (ibuf[0] == '\0' || ibuf[0] == '#')
			continue;
		if (isspace(ibuf[0]))
		{
			fprintf(stderr, "%s: %s: line %d: syntax error (leading space)\n",
				progname, mapname, lineno);
			continue;
		}
		key.xx.data = ibuf;
		for (p = ibuf; *p != '\0' && !isspace(*p); p++)
			continue;
		key.xx.size = p - key.xx.data;
		if (inclnull)
			key.xx.size++;
		if (*p != '\0')
			*p++ = '\0';
		while (isspace(*p))
			p++;
		if (*p == '\0')
		{
			fprintf(stderr, "%s: %s: line %d: no RHS for LHS %s\n",
				progname, mapname, lineno, key.xx.data);
			continue;
		}
		val.xx.data = p;
		val.xx.size = strlen(p);
		if (inclnull)
			val.xx.size++;

		/*
		**  Do the database insert.
		*/

		if (verbose)
		{
			printf("key=`%s', val=`%s'\n", key.xx.data, val.xx.data);
		}

		switch (type)
		{
#ifdef DBM_MAP
		  case T_DBM:
			st = dbm_store(dbp.dbm, key.dbm, val.dbm,
					allowreplace ? DBM_REPLACE : DBM_INSERT);
			break;
#endif

#if defined(BTREE_MAP) || defined(HASH_MAP)
		  case T_BTREE:
		  case T_HASH:
			st = (*dbp.db->put)(dbp.db, &key.db, &val.db,
					allowreplace ? 0 : R_NOOVERWRITE);
			break;
#endif
		}

		if (st < 0)
		{
			fprintf(stderr, "%s: %s: line %d: key %s: put error\n",
				progname, mapname, lineno, key.xx.data);
			perror(mapname);
			exitstat = EX_IOERR;
		}
		else if (st > 0)
		{
			fprintf(stderr, "%s: %s: line %d: key %s: duplicate key\n",
				progname, mapname, lineno, key.xx.data);
		}
	}

	/*
	**  Now close the database.
	*/

	switch (type)
	{
#ifdef DBM_MAP
	  case T_DBM:
		dbm_close(dbp.dbm);
		break;
#endif

#if defined(HASH_MAP) || defined(BTREE_MAP)
	  case T_HASH:
	  case T_BTREE:
		if ((*dbp.db->close)(dbp.db) < 0)
		{
			fprintf(stderr, "%s: %s: error on close\n",
				progname, mapname);
			perror(mapname);
			exitstat = EX_IOERR;
		}
#endif
	}

	exit (exitstat);
}
