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
static char sccsid[] = "@(#)t.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * The transcribe function. POSIX calls it copy, but 't' for transcribe
 * is more mneumonic and that's what I've always called it. Transcribes
 * the spec'd lines into the buffer at the spec'd location.
 */

void
t(inputt, errnum)

FILE *inputt;
int *errnum;

{
  LINE *l_ptr, *l_tb, *l_te, *l_temp1, *l_temp2, *l_dest;

  l_tb = NULL;
  l_te = NULL;

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

  if (g_flag == 0)
    u_clr_stk();

  for (l_ptr=start; l_ptr!=(End->below); l_ptr=(l_ptr->below) )
     {
       get_line(l_ptr->handle, l_ptr->len);
       l_temp1 = (LINE *)malloc(sizeof(LINE));
       if (l_temp1 == NULL)
         {
           *errnum = -1;
           strcpy(help_msg, "out of memory error");
           return;
         }
       if (l_tb == NULL)
         {
           l_tb = l_temp1;
           (l_temp1->above) = NULL;
           (l_temp1->below) = NULL;
         }
       else
         {
           (l_temp1->above) = l_te;
           (l_temp1->below) = NULL;
           (l_te->below) = l_temp1;
         }
       l_te = l_temp1;
       (l_temp1->len) = l_ptr->len;
       /* add it into the buffer at the spec'd location */
       (l_temp1->handle) = add_line(text, l_ptr->len);
       if (sigint_flag)
         break;
     } /* end-for */

  if (l_dest == NULL)
    l_temp2 = top;
  else
    {
      u_add_stk(&(l_dest->below));
      l_temp2 = l_dest->below;
    }

  if (l_dest == NULL)
    {
      u_add_stk(&(top->above));
      (top->above) = l_tb;
      top = l_tb;
      (l_tb->above) = NULL;
    } /* end-if, */
  else
    {
      (l_tb->above) = l_dest;
      (l_dest->below) = l_tb;
    } /* end-if,else */

  if (l_dest == bottom)
    {
      bottom = l_te;
      (l_te->below) = NULL;
    } /* end-if, */
  else
    {
      (l_te->below) = l_temp2;
      u_add_stk(&(l_temp2->above));
      (l_temp2->above) = l_te;
    } /* end-if,else */

  current = l_te;
  change_flag = 1;
  *errnum = 1;
  return;
} /* end-t */
