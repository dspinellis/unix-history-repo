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
static char sccsid[] = "@(#)j.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h" 

/*
 * Join the spec'd lines onto one line. Make the new line from the
 * old lines and then delete the old ones (undo-able).
 */

void
j(inputt, errnum)

FILE *inputt;
int *errnum;

{
  LINE *l_ptr, *l_temp_line;
  char *l_temp1;
  long l_ttl=0;

  if (start_default && End_default)
    {
      start = current;
      *errnum = 1;
      if ((start->below) != NULL)
        End = start->below;
      else
        return;
    }
  else if (start_default)
    {
      if (rol(inputt, errnum))
        return;

      if (End == NULL)
        {
          strcpy(help_msg, "bad address");
          *errnum = -1;
        }
      else
        {
#ifdef BSD
          /* for BSD a 'j' with one address sets "current" to that line */
          if (start)
            current = start;
#endif
          *errnum = 1;
        }
      return;
    }
  if (start == NULL)
    {
      strcpy(help_msg, "bad address");
      *errnum = -1;
      ungetc('\n', inputt);
      return;
    }
  start_default = End_default = 0;
  if (sigint_flag)
    SIGINT_ACTION;

  if (rol(inputt, errnum))
    return;

  if (g_flag == 0)
    u_clr_stk();
  u_set = 1; /* set for d */

  /* figure out what the length of the joined lines will be */
  for (l_ptr=start; l_ptr!=(End->below); l_ptr=(l_ptr->below) )
     l_ttl = l_ptr->len + l_ttl;

  if (l_ttl > nn_max)
    {
      /* the new line is bigger than any so far, so make more space */
      free(text);
      nn_max = l_ttl;
      text = (char *)calloc(l_ttl+2, sizeof(char));
      if (text == NULL)
        {
          *errnum = -1;
          strcpy(help_msg, "out of memory error");
          return;
        }
    }

  if (sigint_flag)
    SIGINT_ACTION;
  l_temp1 = (char *)calloc(l_ttl+2, sizeof(char));
  if (text == NULL)
    {
      *errnum = -1;
      strcpy(help_msg, "out of memory error");
      return;
    }
  l_temp1[0] = '\0';

  l_ptr = start;
  while (1)
     {
       /* get each line and catenate */
       if (sigint_flag)
         goto point;
       get_line(l_ptr->handle, l_ptr->len);
       strcat(l_temp1, text);
       l_ptr = l_ptr->below;
       if (l_ptr == End->below)
         break;
     }

  l_temp_line = (LINE *)malloc(sizeof(LINE));
  if (text == NULL)
    {
      *errnum = -1;
      strcpy(help_msg, "out of memory error");
      return;
    }
  (l_temp_line->len) = l_ttl;
  /* add the new line to the buffer */
  (l_temp_line->handle) = add_line(l_temp1, l_ttl);
  if (start == top)
    {
      top = l_temp_line;
      (l_temp_line->above) = NULL;
    }
  else
    {
      (l_temp_line->above) = start->above;
      u_add_stk(&(l_temp_line->above->below));
      (l_temp_line->above->below) = l_temp_line;
    }
  (l_temp_line->below) = start;
  u_add_stk(&(start->above));
  (start->above) = l_temp_line;

  ungetc(ss, inputt);
  /* delete the lines used to make the joined line */
  d(inputt, errnum);
  if (*errnum < 0)
    return;
  *errnum = 0;
  current = l_temp_line;

  *errnum = 1;

  point:
  u_set = 0;
  free(l_temp1);
} /* end-j */
