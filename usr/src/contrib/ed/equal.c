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
static char sccsid[] = "@(#)equal.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * Print out what the line number of the address given is; default to
 * end-of-buffer ($).
 */
 
void
equal(inputt, errnum)

FILE *inputt;
int *errnum;

{
  if (End_default)
    start = bottom;
  else
    start = End;
  start_default = End_default = 0;

  if (rol(inputt, errnum))
    return;

  printf("%d\n", line_number(start));
  *errnum = 1;
} /* end-equal */
