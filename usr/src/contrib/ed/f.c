/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)f.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

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
