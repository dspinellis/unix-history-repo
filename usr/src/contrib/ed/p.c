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
static char sccsid[] = "@(#)p.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * Both the n and p code are here because they're almost identical.
 * Print out the line. If it's n print the line number, tab, and then
 * the line.
 */

void
p(inputt, errnum, flag)

FILE *inputt;
int *errnum, flag;

{
  int l_ln;

  if (start_default && End_default)
    start = End = current;
  else if (start_default)
    start = End;
  start_default = End_default = 0;

  if (start == NULL)
    {
      strcpy(help_msg, "bad address");
      *errnum = -1;
      return;
    }

  if (rol(inputt, errnum))  /* for "command-suffix pairs" */
    return;

  if (flag == 1)
    l_ln = line_number(start);
  if (sigint_flag)
    SIGINT_ACTION;
  current = start;
  while (1)
       {
         /* print out the lines */
         if (sigint_flag)
           SIGINT_ACTION;
         if (current == NULL)
           break;
         get_line(current->handle, current->len);
         if (flag == 1) /* when 'n' */
           printf("%d\t", l_ln++);
         fwrite(text, sizeof(char), current->len, stdout);
         putchar('\n');
         if (current == End)
           break;
         current = current->below;
       } /* end-while */

  *errnum = 1;
} /* end-p */
