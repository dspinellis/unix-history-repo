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
static char sccsid[] = "@(#)get_line.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <db.h>
#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>

#include "ed.h"
#include "extern.h"

/*
 * Get the specified line from the buffer. Note that in the buffer
 * we stored lengths of text, not strings (NULL terminated, except
 * under MEMORY). So we want to put a terminating NULL in for
 * whatever command is going to be handling the line.
 */

/* these variables are here (instead of main and ed.h) because they
 * are only used with the buffer when run under STDIO. STDIO is a
 * resource pig with most of the OS's I've tested this with. The
 * use of these variables improves performance up to 100% in several
 * cases. The piggyness is thus: fseek causes the current STDIO buf
 * to be flushed out and a new one read in...even when it is not necessary!
 * Read 512 (or 1024) when you don't have to for almost every access
 * and you'll slow down too. So these variable are used to control unneeded
 * fseeks.
 * I've been told the newer BSD STDIO has fixed this, but don't
 * currently have a copy.
 */
int file_loc=0;

/* Get a particular line of length len from ed's buffer and place it in
 * 'text', the standard repository for the "current" line.
 */
void
get_line(loc, len)
	recno_t loc;
	int len;
{
	DBT db_key, db_data;

	(db_key.data) = &loc;
	(db_key.size) = sizeof(recno_t);
	(dbhtmp->get) (dbhtmp, &db_key, &db_data, (u_int) 0);
	strcpy(text, db_data.data);
	text[len] = '\0';
}
