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
static char sccsid[] = "@(#)f.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <limits.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h" 
#include "extern.h"

/*
 * Prints out or sets the remembered filename.
 */
void
f(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	char *l_temp;

	l_temp = filename(inputt, errnum);
	if (*errnum == 1) {
		sigspecial++;
			/* user wants the name from a sh command */
		if (l_temp && l_temp[FILENAME_LEN+1]) {
			FILE   *namestream, *popen();
			int l_len;

			if (l_temp[0] == '\0') {
				strcpy(help_msg, "no command given");
				*errnum = -1;
				return;
			}
			if (((namestream = popen(l_temp, "r")) == NULL) ||
			    ((fgets(l_temp, FILENAME_LEN - 1, namestream)) == NULL)) {
				strcpy(help_msg, "error executing command");
				*errnum = -1;
				if (namestream != NULL)
					pclose(namestream);
				ungetc('\n', inputt);
				return;
			}
			l_len = strlen(l_temp) - 1;
			if (l_temp[l_len] == '\n')
				l_temp[l_len] = '\0';
			pclose(namestream);
		}
		free(filename_current);
		filename_current = l_temp;
		sigspecial--;
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
	} else
		if (*errnum == -2)
			while (((ss = getc(inputt)) != '\n') || (ss == EOF));
		else
			if (*errnum < 0)
				return;
	if (filename_current)
		fwrite(filename_current,
		    sizeof(char), strlen(filename_current), stdout);
	putchar('\n');
	*errnum = 1;
}
