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
static char sccsid[] = "@(#)m.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * Move the specified lines to the new location. It's quick 'cause
 * just a couple of pointers are redirected.
 */

void
m(inputt, errnum)

FILE *inputt;
int *errnum;

{
  LINE *l_dest, *l_old_top, *l_old_bottom;

  /* set l_dest here */
  if (((ss=getc(inputt)) != '\n') && (ss != EOF))
    {
      while (1)
           {
             if (ss != ' ')
               {
                 ungetc(ss, inputt);
                 break;
               }
             ss = getc(inputt);
           }
      l_dest = address_conv(NULL, inputt, errnum);
    }
  else
    (ungetc(ss, inputt), *errnum = -1);
  if (sigint_flag)
    SIGINT_ACTION;
  if (*errnum < 0)
    {
      strcpy(help_msg, "bad destination address");
      return;
    } /* end-if */
  *errnum = 0;
  if (rol(inputt, errnum))
    return;

  if (start_default && End_default)
    start = End = current;
  else if (start_default)
    start = End;
  if (start == NULL)
    {
      strcpy(help_msg, "bad address");
      *errnum = -1;
      return;
    }
  start_default = End_default = 0;
  if (sigint_flag)
    SIGINT_ACTION;

  /* do some address checking */
  if ((l_dest) && ((l_dest == start) || (address_check(l_dest, start) == -1)) && (address_check(End, l_dest) == -1))
    {
      ungetc(ss, inputt);
      *errnum = -1;
      strcpy(help_msg, "destination address in address range");
      return;
    }

  change_flag = 1;
  if (g_flag == 0)
    u_clr_stk();

  /* some more address checking. These are "legal" command constructions
   * but are kind-a useless since the buffer doesn't change */
  if ((start == l_dest) || (End == l_dest))
    return;
  if ((start == top) && (End == bottom))
    return;
  if ((start == top) && (l_dest == NULL))
    return;

  l_old_top = top;
  l_old_bottom = bottom;

  if (start == top)
    {
      top = End->below;
      u_add_stk(&(End->below->above));
      top->above = NULL;
    }
  else if (End == bottom)
    {
      bottom = start->above;
      u_add_stk(&(start->above->below));
      bottom->below = NULL;
    }
  else
    {
      u_add_stk(&(start->above->below));
      start->above->below = End->below;
      u_add_stk(&(End->below->above));
      End->below->above = start->above;
    }

  if (l_dest == NULL)
    {
      u_add_stk(&(start->above));
      start->above = NULL;
      u_add_stk(&(End->below));
      End->below = l_old_top;
      u_add_stk(&(l_old_top->above));
      l_old_top->above = End;
      top = start;
    }
  else if (l_dest == l_old_bottom)
    {
      u_add_stk(&(End->below));
      End->below = NULL;
      u_add_stk(&(start->above));
      start->above = l_dest;
      u_add_stk(&(l_dest->below));
      l_dest->below = start;
      bottom = End;
    }
  else
    {
      u_add_stk(&(start->above));
      start->above = l_dest;
      u_add_stk(&(End->below));
      End->below = l_dest->below;
      u_add_stk(&(l_dest->below->above));
      l_dest->below->above = End;
      u_add_stk(&(l_dest->below));
      l_dest->below = start;
    }

  if (l_dest)
    l_dest->below = start;
  current = start;

  *errnum = 1;
} /* end-m */
