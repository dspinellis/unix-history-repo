/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1991, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)driver2.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * Test driver, to try to tackle the large ugly-split problem.
 */

#include <sys/file.h>
#include <stdio.h>
#include "ndbm.h"

int my_hash(key, len)
	char	*key;
	int	len;
{
	return(17);		/* So I'm cruel... */
}

main(argc, argv)
	int	argc;
{
	DB	*db;
	DBT	key, content;
	char	keybuf[2049];
	char	contentbuf[2049];
	char	buf[256];
	int	i;
	HASHINFO	info;

	info.bsize = 1024;
	info.ffactor = 5;
	info.nelem = 1;
	info.cachesize = NULL;
#ifdef HASH_ID_PROGRAM_SPECIFIED
	info.hash_id = HASH_ID_PROGRAM_SPECIFIED;
	info.hash_func = my_hash;
#else
	info.hash = my_hash;
#endif
	info.lorder = 0;
	if (!(db = dbopen("bigtest", O_RDWR | O_CREAT, 0644, DB_HASH, &info))) {
		sprintf(buf, "dbopen: failed on file bigtest");
		perror(buf);
		exit(1);
	}
	srandom(17);
	key.data = keybuf;
	content.data = contentbuf;
	bzero(keybuf, sizeof(keybuf));
	bzero(contentbuf, sizeof(contentbuf));
	for (i=1; i <= 500; i++) {
		key.size = 128 + (random()&1023);
		content.size = 128 + (random()&1023);
/*		printf("%d: Key size %d, data size %d\n", i, key.size,
		       content.size); */
		sprintf(keybuf, "Key #%d", i);
		sprintf(contentbuf, "Contents #%d", i);
		if ((db->put)(db, &key, &content, R_NOOVERWRITE)) {
			sprintf(buf, "dbm_store #%d", i);
			perror(buf);
		}
	}
	if ((db->close)(db)) {
		perror("closing hash file");
		exit(1);
	}
	exit(0);
}

	

