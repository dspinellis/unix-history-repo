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
static char sccsid[] = "@(#)add_line.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * This is where the lines actually are put into the buffer. Notice
 * all the #ifdef's for the various methods of buffering - POSIX said
 * no "no attempt is made to imply a specific implementation". So,
 * you get your choice: standard I/O, core memory, or a database.
 */

#ifdef STDIO
long
add_line(p, len)
#endif
#ifdef DBI
recno_t
add_line(p, len)
#endif
#ifdef MEMORY
char
*add_line(p, len)
#endif

char *p;
long len;

{
#ifdef STDIO
  long l_key;
  extern int file_loc;

  if (file_seek)  /* x-ref to get_line for what this does */
    {
      file_seek = 0;
      fseek(fhtmp, 0L, 2); /* set to end-to-file */
    }
  l_key = ftell(fhtmp);
  file_loc = l_key + fwrite(p, sizeof(char), len, fhtmp); /* keeps user time down 20%approx */
  return(l_key);
#endif

#ifdef DBI
  DBT db_key, db_data;
  static recno_t l_key=0;

  l_key++;
  (db_key.data) = &l_key;
  (db_key.size) = sizeof(recno_t);
  (db_data.data) = p;
  (db_data.size) = len;
  (dbhtmp->put)(dbhtmp, &db_key, &db_data, (u_int)(R_NOOVERWRITE));
  return(l_key);
#endif

#ifdef MEMORY
  char *tmp;
  tmp = (char *)calloc(len+1, sizeof(char));
  if (tmp)
    {
      bcopy(p, tmp, len);
      tmp[len] = '\0';
    }
  return(tmp);
#endif

} /* end-add_line */
