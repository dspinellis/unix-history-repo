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
static char sccsid[] = "@(#)d.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * This removes lines in the buffer from user access. The specified
 * lines are not really deleted yet(!) as they might be called back
 * by an undo. So the pointers from start, End, and neighbours are placed
 * in a stack for deletion later when no undo can be performed on these lines.
 * The lines in the buffer are freed then as well.
 */

void
d(inputt, errnum)

FILE *inputt;
int *errnum;

{
  LINE *l_temp1, *l_temp2;

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

  if (rol(inputt, errnum))
    return;

  if ((u_set == 0) && (g_flag == 0))
    u_clr_stk();  /* for undo */

  if ((start == NULL) && (End == NULL)) /* nothing to do... */
    {
      *errnum = 1;
      return;
    }

  if (sigint_flag)
    SIGINT_ACTION;

  d_add(start, End); /* for buffer clearing later(!) */

  /* now change preserve the pointers in case of undo and then adjust
   * them.
   */
  if (start == top)
    {
      top = End->below;
      if (top != NULL)
        {
          u_add_stk(&(top->above));
          (top->above) = NULL;
        }
    }
  else
    {
      l_temp1 = start->above;
      u_add_stk(&(l_temp1->below));
      (l_temp1->below) = End->below;
    }

  if (End == bottom)
    {
      bottom = start->above;
      current = bottom;
    }
  else
    {
      l_temp2 = End->below;
      u_add_stk(&(l_temp2->above));
      (l_temp2->above) = start->above;
      current = l_temp2;
    }

  /* to keep track of the marks */
  ku_chk(start, End, NULL);
  change_flag = 1L;

  if (sigint_flag) /* next stable spot */
    SIGINT_ACTION;

  if (start == End)
    {
      *errnum = 1;
      return;
    }

  *errnum = 1;
} /* end-d */


/*
 * This keeps a stack of the start and end lines deleted for clean-up
 * later (in d_do()). A stack is used because of the global commands.
 */

void
d_add(top_d, bottom_d)

LINE *top_d, *bottom_d;

{
  struct d_layer *l_temp;

  l_temp = (struct d_layer *)malloc(sizeof(struct d_layer));
  (l_temp->begin) = top_d;
  (l_temp->end) = bottom_d;
  (l_temp->next) = d_stk;
  d_stk = l_temp;

} /* end-d_add */


/*
 * This cleans up the LINE structures deleted and no longer accessible
 * to undo. It performs garbage clean-up on the now non-referenced
 * text in the buffer. All three buffer methods here #ifdef'd
 */

void
d_do()

{
  struct d_layer *l_temp;
  LINE *l_temp2, *l_temp3;
  int l_flag;
#ifdef DBI
  DBT l_db_key;
#endif

  l_temp = d_stk;

  do
    {
      l_flag = 0;
      l_temp2 = l_temp->begin;
      do
        {
          l_temp3 = l_temp2;
#ifdef STDIO
          /* no garbage collection done currently */
#endif
#ifdef DBI
          /* we do it, but db(3) says it doesn't really do it yet */
          l_db_key.size = sizeof(recno_t);
          l_db_key.data = &(l_temp2->handle);
          (dbhtmp->del)(dbhtmp, &l_db_key, (u_int)0);
#endif
#ifdef MEMORY
          free(l_temp2->handle);
#endif
          if ((l_temp->end) == l_temp2)
            l_flag = 1;
          l_temp2 = l_temp3->below;
          free(l_temp3);
        } while (l_flag == 0);

        d_stk = d_stk->next;
        free(l_temp);
        l_temp = d_stk;
      } while (d_stk);

  d_stk = NULL; /* just to be sure */
} /* end-d_do */
