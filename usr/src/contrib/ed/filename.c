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
static char sccsid[] = "@(#)filename.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <db.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ed.h" 
#include "extern.h"

/*
 * A central function for any command that has to deal with a filename
 * (to be or not to be remembered).
 */
char *
filename(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	register int l_cnt = 0;
	char *l_fname;
	int l_esc = 0, l_bang_flag = 0, l_len;

	l_fname = calloc(FILENAME_LEN, sizeof(char));
	if (l_fname == NULL) {
		*errnum = -1;
		strcpy(help_msg, "out of memory error");
		return (NULL);
	}
	if ((ss = getc(inputt)) != ' ') {
		if (ss == '\n') {
			ungetc(ss, inputt);
			/*
			 * It's not really an error, but to flag remembered
			 * filename is to be used.
			 */
			*errnum = -2;
		} else {
			*errnum = -1;
			strcpy(help_msg,
			    "space required before filename given");
		}
		return (NULL);
	}
	while (ss = getc(inputt))
		if (ss != ' ') {
			ungetc(ss, inputt);
			break;
		}
	for (;;) {
		ss = getc(inputt);
		if ((ss == '\\') && (l_esc == 0)) {
			ss = getchar();
			l_esc = 1;
		} else
			l_esc = 0;
		if ((ss == '\n') || (ss == EOF)) {
			l_fname[l_cnt] = '\0';
			break;
		} else
			if ((ss == '!') && (l_esc == 0))
				l_bang_flag = 1;
			else
				if (ss != ' ')
					l_fname[l_cnt++] = ss;
				else
					continue;

		if (l_cnt >= FILENAME_LEN) {
			strcpy(help_msg, "filename+path length too long");
			*errnum = -1;
			ungetc('\n', inputt);
			return (NULL);
		}
	}

	if (l_bang_flag == 1) {	/* user wants the name from a sh command */
		FILE   *namestream, *popen();

		if (l_fname[0] == '\0') {
			strcpy(help_msg, "no command given");
			*errnum = -1;
			return (NULL);
		}
		if (((namestream = popen(l_fname, "r")) == NULL) ||
		    ((fgets(l_fname, FILENAME_LEN - 1, namestream)) == NULL)) {
			strcpy(help_msg, "error executing command");
			*errnum = -1;
			if (namestream != NULL)
				pclose(namestream);
			return (NULL);
		}
		l_len = strlen(l_fname) - 1;
		if (l_fname[l_len] == '\n')
			l_fname[l_len] = '\0';
		pclose(namestream);
	} else
		if (l_fname[0] == '\0')
			strcpy(l_fname, filename_current);
		else
			*errnum = 1;
	return (l_fname);
}
