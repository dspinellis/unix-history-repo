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
static char sccsid[] = "@(#)line_number.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * Converts a LINE to a line number (int) that can be printed to
 * the device the user is at. Used by n.
 */

int
line_number(line_addr)

LINE *line_addr;

{
  int l_cnt=1; /* yes! =1 */
  LINE *l_temp1;

  l_temp1 = top;
  if ((line_addr == NULL) && (top == NULL))
    return(0);

  while (1)
       {
         if (sigint_flag)
           SIGINT_ACTION;
         if (line_addr == l_temp1)
           break;
         l_temp1 = l_temp1->below;
         l_cnt++;
       } /* end-while */
  return(l_cnt);
} /* end-line_number */
