/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)edhup.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <limits.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * If a SIGHUP is received then user contact is severed. Try, if possible,
 * to save the buffer. But be nice and don't save over remembered filename
 * (you can figure out why, can't you?).  The buffer is saved in a file
 * named "ed.hup" in the directory that ed was started-up in.  If a write
 * cannot be made to that directory (say because it is read-only) then try
 * writting "ed.hup" in the user's $HOME directory. Then exit.
 */
__dead void
do_hup()
{
	char l_filename[FILENAME_LEN], *l_temp;
	FILE *l_fp;

	sigspecial++;
	if (change_flag == 0)
		exit(exit_code+2);		/* No need to save buffer contents. */
	if ((l_fp = fopen("ed.hup", "w")) == NULL) {
		/* Try writting ed.hup to the $HOME directory instead. */
		l_temp = getenv("HOME");
		if ((l_temp == NULL) || ((strlen(l_temp) + 7) > FILENAME_LEN))
			exit(exit_code+2);
		strcpy(l_filename, l_temp);
		strcat(l_filename, "/ed.hup");
		if ((l_fp = fopen(l_filename, "w")) == NULL)
			exit(exit_code+2);		/* We tried... */
	}
	edwrite(l_fp, top, bottom);
	fclose(l_fp);
#ifdef STDIO
	fclose(fhtmp);
	unlink(template);
#endif
#ifdef DBI
	(dbhtmp->close) (dbhtmp);
	unlink(template);
#endif
	exit(exit_code+2);				/* Hangup */
}
