/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Hugh Smith at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)move.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>
#include <stdio.h>
#include <ar.h>
#include "archive.h"
#include "extern.h"
#include "pathnames.h"

extern CHDR chdr;			/* converted header */
extern char *archive;			/* archive name */
extern char *tname;                     /* temporary file "name" */

/*
 * move --
 *	Change location of named members in archive - if 'b' or 'i' option
 *	selected then named members are placed before 'posname'.  If 'a'
 *	option selected members go after 'posname'.  If no options, members
 *	are moved to end of archive.
 */
move(argv)
	char **argv;
{
	extern char *posarg, *posname;	/* positioning file names */
	CF cf;
	off_t size, tsize;
	int afd, curfd, mods, tfd1, tfd2, tfd3;
	char *file;

	afd = open_archive(O_RDWR);
	mods = options & (AR_A|AR_B);

	tfd1 = tmp();			/* Files before key file. */
	tfd2 = tmp();			/* Files selected by user. */
	tfd3 = tmp();			/* Files after key file. */

	/*
	 * Break archive into three parts -- selected entries and entries
	 * before and after the key entry.  If positioning before the key,
	 * place the key at the beginning of the after key entries and if
	 * positioning after the key, place the key at the end of the before
	 * key entries.  Put it all back together at the end.
	 */

	/* Read and write to an archive; pad on both. */
	SETCF(afd, archive, 0, tname, RPAD|WPAD);
	for (curfd = tfd1; get_arobj(afd);) {	
		if (*argv && (file = files(argv))) {
			if (options & AR_V)
				(void)printf("m - %s\n", file);
			cf.wfd = tfd2;
			put_arobj(&cf, (struct stat *)NULL);
			continue;
		}
		if (mods && compare(posname)) {
			mods = 0;
			if (options & AR_B)
				curfd = tfd3;
			cf.wfd = curfd;
			put_arobj(&cf, (struct stat *)NULL);
			if (options & AR_A)
				curfd = tfd3;
		} else {
			cf.wfd = curfd;
			put_arobj(&cf, (struct stat *)NULL);
		}
	}

	if (mods) {
		(void)fprintf(stderr, "ar: %s: archive member not found.\n",
		    posarg);
		close_archive(afd);
		return(1);
	}
	(void)lseek(afd, (off_t)SARMAG, SEEK_SET);

	SETCF(tfd1, tname, afd, archive, NOPAD);
	tsize = size = lseek(tfd1, (off_t)0, SEEK_CUR);
	(void)lseek(tfd1, (off_t)0, SEEK_SET);
	copy_ar(&cf, size);

	tsize += size = lseek(tfd2, (off_t)0, SEEK_CUR);
	(void)lseek(tfd2, (off_t)0, SEEK_SET);
	cf.rfd = tfd2;
	copy_ar(&cf, size);

	tsize += size = lseek(tfd3, (off_t)0, SEEK_CUR);
	(void)lseek(tfd3, (off_t)0, SEEK_SET);
	cf.rfd = tfd3;
	copy_ar(&cf, size);

	(void)ftruncate(afd, tsize + SARMAG);
	close_archive(afd);

	if (*argv) {
		orphans(argv);
		return(1);
	}
	return(0);
}	
