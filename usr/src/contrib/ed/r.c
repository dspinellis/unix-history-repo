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
static char sccsid[] = "@(#)r.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <a.out.h>
#include <db.h>
#include <errno.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ed.h"
#include "extern.h"

/*
 * This sets up things for the central input routine to place the
 * incoming text at the proper place in the buffer.
 */
void
r(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	struct exec l_magic;
	struct stat l_s_buf;
	FILE *l_fp;
	long l_num;
	char *l_filename_read, *l_temp;
	int l_srv;

	if (filename_flag == 1) {
		l_filename_read = filename_current;
		filename_flag = 0;
	} else {
		l_temp = filename(inputt, errnum);
		if (*errnum == 1)
			l_filename_read = l_temp;
		else
			if (*errnum == -2) {
				while (((ss = getc(inputt)) != '\n') ||
				    (ss == EOF));
				l_filename_read = filename_current;
			} else
				if (*errnum < 0)
					return;
		*errnum = 0;
	}

	if (filename_current == NULL) {
		if (l_filename_read == NULL) {
			strcpy(help_msg, "no filename given");
			*errnum = -1;
			if (ss)
				ungetc('\n', inputt);
			return;
		} else
			filename_current = l_filename_read;
	}
	if (sigint_flag)
		SIGINT_ACTION;

	/*
	 * Determine if the file can be read.  If not set the help message to
	 * something descriptive that the user should understand.
	 */
	if (((l_srv = stat(l_filename_read, &l_s_buf)) == -1) ||
	    (l_s_buf.st_mode & S_IFDIR)) {
		if (l_srv == -1)
			strcpy(help_msg, strerror(errno));
		else
			strcpy(help_msg,
			    "filename is directory, not a text file");
		printf("?%s\n", l_filename_read);
		*errnum = 0;
		return;
	}
	if ((l_fp = fopen(l_filename_read, "r")) == NULL) {
		strcpy(help_msg, "permission lacking to read file");
		printf("?%s\n", l_filename_read);
		*errnum = 0;
		return;
	}
	/*
	 * There is permission to read the file, but if it's an executable
	 * file of the object code and linked type, we really don't want to
	 * look at it (according to ed spec's).
	 */
	if (fread(&l_magic, sizeof(struct exec), 1, l_fp) != 0) {
		if (!(N_BADMAG(l_magic))) {
			strcpy(help_msg, "unable to read executable file");
			printf("?%s\n", l_filename_read);
			*errnum = 0;
			return;
		}
	}
	fseek(l_fp, (off_t)0, 0);
	if (g_flag == 0)
		u_clr_stk();
	l_num = input_lines(l_fp, errnum);
	if (sigint_flag == 1)
		goto point;
	if (*errnum < 0)
		return;
	*errnum = 0;

	if (explain_flag)	/* !=0 */
		printf("%ld\n", l_num);
	if (l_filename_read != filename_current)
		free(l_filename_read);

point:	fclose(l_fp);
	change_flag = 1;
	if (sigint_flag)
		SIGINT_ACTION;
	*errnum = 1;
}
