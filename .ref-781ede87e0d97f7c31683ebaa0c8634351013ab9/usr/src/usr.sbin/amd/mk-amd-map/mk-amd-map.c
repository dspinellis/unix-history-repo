/*
 * $Id: mk-amd-map.c,v 5.2 90/06/23 22:20:10 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mk-amd-map.c	5.1 (Berkeley) %G%
 */

/*
 * Convert a file map into an ndbm map
 */

#ifndef lint
char copyright[] = "\
@(#)Copyright (c) 1990 Jan-Simon Pendry\n\
@(#)Copyright (c) 1990 Imperial College of Science, Technology & Medicine\n\
@(#)Copyright (c) 1990 The Regents of the University of California.\n\
@(#)All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char rcsid[] = "$Id: mk-amd-map.c,v 5.2 90/06/23 22:20:10 jsp Rel $";
static char sccsid[] = "@(#)mk-amd-map.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "am.h"

#ifdef OS_HAS_GDBM
#define HAS_DATABASE
#include "gdbm.h"
#endif /* OS_HAS_GDBM */

#ifndef HAS_DATABASE
#ifdef OS_HAS_NDBM
#define HAS_DATABASE
#define	USE_NDBM
#include <ndbm.h>

#define create_database(name) dbm_open(name, O_RDWR|O_CREAT, 0444)

static int store_data(db, k, v)
voidp db;
char *k, *v;
{
	datum key, val;

	key.dptr = k; val.dptr = v;
	key.dsize = strlen(k) + 1;
	val.dsize = strlen(v) + 1;
	return dbm_store((DBM *) db, key, val, DBM_INSERT);
}

#endif /* OS_HAS_NDBM */
#endif /* !OS_HAS_DATABASE */

#ifdef HAS_DATABASE
#include <fcntl.h>
#include <ctype.h>

static int read_line(buf, size, fp)
char *buf;
int size;
FILE *fp;
{
	int done = 0;

	do {
		while (fgets(buf, size, fp)) {
			int len = strlen(buf);
			done += len;
			if (len > 1 && buf[len-2] == '\\' &&
					buf[len-1] == '\n') {
				int ch;
				buf += len - 2;
				size -= len - 2;
				/*
				 * Skip leading white space on next line
				 */
				while ((ch = getc(fp)) != EOF &&
					isascii(ch) && isspace(ch))
						;
				(void) ungetc(ch, fp);
			} else {
				return done;
			}
		}
	} while (size > 0 && !feof(fp));

	return done;
}

/*
 * Read through a map
 */
static int read_file(fp, map, db)
FILE *fp;
char *map;
voidp db;
{
	char key_val[2048];
	int chuck = 0;
	int line_no = 0;
	int errs = 0;

	while (read_line(key_val, sizeof(key_val), fp)) {
		char *kp;
		char *cp;
		char *hash;
		int len = strlen(key_val);
		line_no++;

		/*
		 * Make sure we got the whole line
		 */
		if (key_val[len-1] != '\n') {
			fprintf(stderr, "line %d in \"%s\" is too long", line_no, map);
			chuck = 1;
		} else {
			key_val[len-1] = '\0';
		}

		/*
		 * Strip comments
		 */
		hash = strchr(key_val, '#');
		if (hash)
			*hash = '\0';

		/*
		 * Find start of key
		 */
		for (kp = key_val; *kp && isascii(*kp) && isspace(*kp); kp++)
			;

		/*
		 * Ignore blank lines
		 */
		if (!*kp)
			goto again;

		/*
		 * Find end of key
		 */
		for (cp = kp; *cp&&(!isascii(*cp)||!isspace(*cp)); cp++)
			;

		/*
		 * Check whether key matches, or whether
		 * the entry is a wildcard entry.
		 */
		if (*cp)
			*cp++ = '\0';
		while (*cp && isascii(*cp) && isspace(*cp))
			cp++;
		if (*kp == '+') {
			fprintf(stderr, "Can't interpolate %s\n", kp);
			errs++;
		} else if (*cp) {
#ifdef DEBUG
			printf("%s\t%s\n", kp, cp);
#endif /* DEBUG */
			if (store_data(db, kp, cp) < 0) {
				fprintf(stderr, "Could store %s -> %s\n", kp, cp);
				errs++;
			}
		} else {
			fprintf(stderr, "%s: line %d has no value field", map, line_no);
			errs++;
		}

again:
		/*
		 * If the last read didn't get a whole line then
		 * throw away the remainder before continuing...
		 */
		if (chuck) {
			while (fgets(key_val, sizeof(key_val), fp) &&
				!strchr(key_val, '\n'))
					;
			chuck = 0;
		}
	}
	return errs;
}

static int remove(f)
char *f;
{
	if (unlink(f) < 0 && errno != ENOENT)
		return -1;
	return 0;
}

main(argc, argv)
int argc;
char *argv[];
{
	FILE *mapf;
	char *map;
	int rc = 0;
	DBM *mapd;
	char *maptmp = "dbmXXXXXX";
	char maptpag[16], maptdir[16];
	char *mappag, *mapdir;
	int len;
	char *sl;

	if (argc != 2) {
		fputs("Usage: mk-amd-map file-map\n", stderr);
		exit(1);
	}

	map = argv[1];
	sl = strrchr(map, '/');
	if (sl) {
		*sl = '\0';
		if (chdir(map) < 0) {
			fputs("Can't chdir to ", stderr);
			perror(map);
			exit(1);
		}
		map = sl + 1;
	}
#ifdef USE_NDBM
	len = strlen(map);
	mappag = (char *) malloc(len + 5);
	mapdir = (char *) malloc(len + 5);
	if (!mappag || !mapdir) {
		perror("malloc");
		exit(1);
	}
	mktemp(maptmp);
	sprintf(maptpag, "%s.pag", maptmp);
	sprintf(maptdir, "%s.dir", maptmp);
	if (remove(maptpag) < 0 || remove(maptdir) < 0) {
		fprintf(stderr, "Can't remove existing temporary files; %s and", maptpag);
		perror(maptdir);
		exit(1);
	}
#endif /* USE_NDBM */
	mapf =  fopen(map, "r");
	if (mapf)
		mapd = create_database(maptmp);
	else
		mapd = 0;
#ifndef DEBUG
	signal(SIGINT, SIG_IGN);
#endif /* DEBUG */
	if (mapd) {
		int error = read_file(mapf, map, mapd);
		(void) fclose(mapf);
		if (error) {
			fprintf(stderr, "Error creating ndbm map for %s\n", map);
			rc = 1;
		}
#ifdef USE_NDBM
		sprintf(mappag, "%s.pag", map);
		sprintf(mapdir, "%s.dir", map);
		if (rename(maptpag, mappag) < 0) {
			fprintf(stderr, "Couldn't rename %s to ", maptpag);
			perror(mappag);
			/* Throw away the temporary map */
			unlink(maptpag);
			unlink(maptdir);
			rc = 1;
		} else if (rename(maptdir, mapdir) < 0) {
			fprintf(stderr, "Couldn't rename %s to ", maptdir);
			perror(mapdir);
			/* Put the .pag file back */
			rename(mappag, maptpag);
			/* Throw away remaining part of original map */
			unlink(mapdir);
			fprintf(stderr, "WARNING: existing map \"%s.{dir,pag}\" destroyed\n", map);
			rc = 1;
		}
#endif /* USE_NDBM */
	} else {
#ifdef USE_NDBM
		fprintf(stderr, "Can't open \"%s.{dir,pag}\" for ", map);
#endif /* USE_NDBM */
		perror("writing");
		rc = 1;
	}
	exit(rc);
}
#else
main()
{
	fputs("This system does not support hashed database files\n", stderr);
	exit(0);
}
#endif /* HAS_DATABASE */
