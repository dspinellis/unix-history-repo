/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dbconv.c	5.2 (Berkeley) 11/19/91";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ndbm.h>
#include <errno.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	DB *db;
	DBM *dbm;
	DBT t_key, t_data;
	datum f_key, f_data;
	int ch, dup, rec;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch((char)ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	dbm = dbm_open(*argv, O_RDONLY, 0);
	if (!dbm)
		error(*argv);
	db = dbopen(*++argv, O_CREAT|O_WRONLY, DEFFILEMODE, DB_HASH, NULL);
	if (!db)
		error(*argv);

	dup = rec = 0;
	for (f_key = dbm_firstkey(dbm); f_key.dptr; f_key = dbm_nextkey(dbm)) {
		f_data = dbm_fetch(dbm, f_key);
		t_key.data = f_key.dptr;
		t_key.size = f_key.dsize;
		t_data.data = f_data.dptr;
		t_data.size = f_data.dsize;
		switch((db->put)(db, &t_key, &t_data, R_NOOVERWRITE)) {
		case -1:
			error(*argv);
		case 0:
			++rec;
			break;
		case 1:
			if (!dup++)
				(void)fprintf(stderr,
				    "dbconv: duplicate records discarded\n");
			break;
		}
	}
	(void)(db->close)(db);
	(void)printf("%d records, %d duplicates discarded.\n", rec + dup, dup);
	exit(dup ? 1 : 0);
}

error(p)
	char *p;
{
	(void)fprintf(stderr, "dbconv: %s: %s\n", p, strerror(errno));
	exit(1);
}

usage()
{
	(void)fprintf(stderr, "usage: dbconv from to\n");
	exit(1);
}
