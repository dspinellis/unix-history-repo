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
static char sccsid[] = "@(#)edhup.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * If a SIGHUP is received then user contact is severed. Try, if
 * possible, to save the buffer. But be nice and don't save over
 * remembered filename (you can figure out why, can't you?).
 * The buffer is saved in a file named "ed.hup" in the directory that
 * ed was started-up in. If a write cannot be made to that directory (say
 * because it is read-only) then try writting "ed.hup" in the user's $HOME
 * direcory. Then exit.
 */

do_hup()

{
  char l_filename[FILENAME_LEN], *l_temp;
  FILE *l_fp;

  if (change_flag == 0)
    exit(1); /* no need to save buffer contents */
  if ((l_fp = fopen("ed.hup", "w")) == NULL)
    {
      /* try writting ed.hup to the $HOME directory instead */
      l_temp = getenv("HOME");
      if ((l_temp == NULL) || ((strlen(l_temp)+7)>FILENAME_LEN))
        exit(1);
      strcpy(l_filename, l_temp);
      strcat(l_filename, "/ed.hup");
      if ((l_fp = fopen(l_filename, "w")) == NULL)
        exit(1); /* we tried... */
    }
  edwrite(l_fp, top, bottom);
  fclose(l_fp);
#ifdef STDIO
  fclose(fhtmp);
  unlink(template);
#endif
#ifdef DBI
  (dbhtmp->close)(dbhtmp);
  unlink(template);
#endif
  exit(1); /* hangup */

} /* end-do_hup */
