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
static char sccsid[] = "@(#)a.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * This sets things up for the central input routine to place the text
 * at the proper location for an append.
 */

void
a(inputt, errnum)

FILE *inputt;
int *errnum;

{
  if (rol(inputt, errnum))
    return;

  if (g_flag == 0)
    u_clr_stk();
  if (sigint_flag)
    SIGINT_ACTION;
  add_flag = 1;
  input_lines(inputt, errnum);
  add_flag = 0;
} /* end-a */
