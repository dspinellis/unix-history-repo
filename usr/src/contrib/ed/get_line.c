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
static char sccsid[] = "@(#)get_line.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

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
extern int file_loc;

/* Get a particular line of length len from ed's buffer and place it in
 * 'text', the standard repository for the "current" line.
 */

void
get_line(loc, len)

#ifdef STDIO
long loc;
#endif
#ifdef DBI
recno_t loc;
#endif
#ifdef MEMORY
char *loc;
#endif
int len;

{
#ifdef STDIO

  if (file_loc != loc)
    fseek(fhtmp, loc, 0);
  file_seek = 1;
  file_loc = loc + fread(text, sizeof(char), len, fhtmp);
#endif

#ifdef DBI
  DBT db_key, db_data;

  (db_key.data) = &loc;
  (db_key.size) = sizeof(recno_t);
  (dbhtmp->get)(dbhtmp, &db_key, &db_data, (u_int)0);
  strcpy(text, db_data.data);
#endif

#ifdef MEMORY
  tmp = (char *)loc;
  bcopy(loc, text, len);
#endif
  text[len] = '\0';
} /* end-get_line */
