/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dev_mkdb.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#undef DIRBLKSIZ
#include <dirent.h>
#include <nlist.h>
#include <kvm.h>
#include <db.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <paths.h>
#include <stdlib.h>
#include <string.h>

static void error(), usage();

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register DIR *dirp;
	register struct dirent *dp;
	struct stat sb;
	struct {
		mode_t type;
		dev_t dev;
	} bkey;
	DB *db;
	DBT data, key;
	int ch;
	u_char buf[MAXNAMLEN + 1];
	char dbtmp[MAXPATHLEN + 1], dbname[MAXPATHLEN + 1];

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch((char)ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (chdir(_PATH_DEV))
		error(_PATH_DEV);

	dirp = opendir(".");

	(void)snprintf(dbtmp, sizeof(dbtmp), "%s/dev.tmp", _PATH_VARRUN);
	(void)snprintf(dbname, sizeof(dbtmp), "%s/dev.db", _PATH_VARRUN);
	db = hash_open(dbtmp, O_CREAT|O_WRONLY|O_EXCL, DEFFILEMODE,
	    (HASHINFO *)NULL);
	if (!db)
		error(dbtmp);

	/*
	 * Keys are a mode_t followed by a dev_t.  The former is the type of
	 * the file (mode & S_IFMT), the latter is the st_rdev field.
	 */
	key.data = &bkey;
	key.size = sizeof(bkey);
	data.data = buf;
	while (dp = readdir(dirp)) {
		if (stat(dp->d_name, &sb)) {
			(void)fprintf(stderr, "dev_mkdb: can't stat %s\n",
				dp->d_name);
			continue;
		}

		/* Create the key. */
		if (S_ISCHR(sb.st_mode))
			bkey.type = S_IFCHR;
		else if (S_ISBLK(sb.st_mode))
			bkey.type = S_IFBLK;
		else
			continue;
		bkey.dev = sb.st_rdev;

		/*
		 * Create the data; nul terminate the name so caller doesn't
		 * have to.
		 */
		bcopy(dp->d_name, buf, dp->d_namlen);
		buf[dp->d_namlen] = '\0';
		data.size = dp->d_namlen + 1;
		if ((db->put)(db, &key, &data, 0))
			error(dbtmp);
	}
	(void)(db->close)(db);
	if (rename(dbtmp, dbname)) {
		(void)fprintf(stderr, "dev_mkdb: %s to %s: %s.\n",
		    dbtmp, dbname, strerror(errno));
		exit(1);
	}
	exit(0);
}

void
error(n)
	char *n;
{
	(void)fprintf(stderr, "dev_mkdb: %s: %s\n", n, strerror(errno));
	exit(1);
}

void
usage()
{
	(void)fprintf(stderr, "usage: dev_mkdb\n");
	exit(1);
}
