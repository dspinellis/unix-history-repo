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
static char sccsid[] = "@(#)e.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <db.h>
#include <fcntl.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ed.h"
#include "extern.h"

/*
 * Places a new file in the buffer to be editted. The current contents
 * of the buffer are deleted - no undo can be perfomed. A warning is
 * issued once if no write has occurred since the last buffer
 * modification (unless 'E' spec'd).
 */

void
e(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	int l_which;		/* which is it? 'e' or 'E' */
	char *l_temp;

	l_which = ss;

	l_temp = filename(inputt, errnum);
	if (sigint_flag)
		SIGINT_ACTION;
	if (*errnum == 1) {
		free(filename_current);
		filename_current = l_temp;
	} else
		if (*errnum == -2)
			while (((ss = getc(inputt)) != '\n') || (ss == EOF));
		else
			if (*errnum < 0)
				return;
	*errnum = 0;

	/* Note: 'E' will bypass this if stmt., which warns of no save. */
	if ((change_flag == 1L) && (l_which == 'e')) {
		change_flag = 0L;
		strcpy(help_msg, "warning: buffer changes not saved");
		*errnum = -1;
		ungetc('\n', inputt);
		return;
	}
	start = top;
	End = bottom;
	start_default = End_default = 0;
	if (start == NULL && bottom == NULL);
	else {
		ungetc(ss, inputt);
		d(inputt, errnum);	/* delete the whole buffer */
	}
	if (sigint_flag)
		SIGINT_ACTION;

	/* An 'e' clears all traces of last doc'mt, even in 'g'. */
	u_clr_stk();
	if (*errnum < 0)
		return;
	*errnum = 0;
	if (dbhtmp != NULL) {
		(dbhtmp->close) (dbhtmp);
		unlink(template);
	}

	name_set = 1;
	e2(inputt, errnum);

	*errnum = 1;
}

/*
 * This is pulled out of e.c to make the "simulated 'e'" at startup easier to
 * handle.
 */
void
e2(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	RECNOINFO l_dbaccess;

	if (template == NULL) {
		template = (char *) calloc(FILENAME_LEN, sizeof(char));
		if (template == NULL)
			ed_exit(4);
	}
	/* create the buffer using the method favored at compile time */
	bcopy("/tmp/_4.4bsd_ed_XXXXXX\0", template, 22);
	mktemp(template);
	(l_dbaccess.bval) = (u_char) '\0';
	(l_dbaccess.cachesize) = 0;
	(l_dbaccess.flags) = R_NOKEY;
	(l_dbaccess.lorder) = 0;
	(l_dbaccess.reclen) = 0;
	dbhtmp = dbopen(template, O_CREAT | O_RDWR,
	    S_IRUSR | S_IWUSR, (DBTYPE) DB_RECNO, &l_dbaccess);
	current = top;
	start = top;
	End = bottom;

	if (sigint_flag)
		SIGINT_ACTION;
	if (name_set) {
		/* So 'r' knows the filename is already read in. */
		filename_flag = 1;
		r(inputt, errnum);
	}
	change_flag = 0;
}
