/* Calculate what ins/del line to do, and do it, for Emacs redisplay.
   Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include "config.h"
#include "termchar.h"
#include "termhooks.h"
#include "dispextern.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

struct matrix_elt
  {
    /* Cost of outputting through this line
       if no insert/delete is done just above it.  */
    int writecost;
    /* Cost of outputting through this line
       if an insert is done just above it.  */
    int insertcost;
    /* Cost of outputting through this line
       if a delete is done just above it.  */
    int deletecost;
    /* Number of inserts so far in this run of inserts,
       for the cost in insertcost.  */
    char insertcount;
    /* Number of deletes so far in this run of deletes,
       for the cost in deletecost.  */
    char deletecount;
  };

/* This exceeds the sum of any reasonable number of INFINITY's.  */
#define SUPER_INFINITY (1000 * INFINITY)

/* See CalcIDCosts for on the arrays below */
int *ILcost;
int *DLcost;
int *ILncost;
int *DLncost;

scrolling_1 (window_size, unchanged_at_top, unchanged_at_bottom,
	     draw_cost, old_hash, new_hash, free_at_end)
     int window_size, unchanged_at_top, unchanged_at_bottom;
     int *draw_cost;
     int *old_hash;
     int *new_hash;
     int free_at_end;
{
  struct matrix_elt *matrix;
  matrix = ((struct matrix_elt *)
	    alloca ((window_size + 1) * (window_size + 1) * sizeof *matrix));

  calculate_scrolling (matrix, window_size, unchanged_at_bottom,
		       draw_cost, old_hash, new_hash,
		       free_at_end);
  do_scrolling (matrix, window_size, unchanged_at_top);
}

/* Determine, in matrix[i,j], the cost of updating the first j old lines
   into the first i new lines.
   This involves using insert or delete somewhere if i != j.
   For each matrix elements, three kinds of costs are recorded:
   the smallest cost that ends with an insert, the smallest
   cost that ends with a delete, and the smallest cost that
   ends with neither one.  These are kept separate because
   on some terminals the cost of doing an insert varies
   depending on whether one was just done, etc.  */

/* draw_cost[VPOS] is the cost of outputting new line at VPOS.
   old_hash[VPOS] is the hash code of the old line at VPOS.
   new_hash[VPOS] is the hash code of the new line at VPOS.
   Note that these are not true screen vpos's, but relative
   to the place at which the first mismatch between old and
   new contents appears.  */

calculate_scrolling (matrix, window_size, lines_below,
		     draw_cost, old_hash, new_hash,
		     free_at_end)
     /* matrix is of size window_size + 1 on each side.  */
     struct matrix_elt *matrix;
     int window_size;
     int *draw_cost;
     int *old_hash;
     int *new_hash;
     int free_at_end;
{
  register int i, j;
  register struct matrix_elt *p, *p1;
  register int cost, cost1;

  int lines_moved = window_size + (scroll_region_ok ? 0 : lines_below);
  /* We subtract 1 to compensate for the fact that i and j have values
     starting with 1.  */
  int *first_insert_cost = &ILcost[screen_height - lines_moved - 1];
  int *first_delete_cost = &DLcost[screen_height - lines_moved - 1];
  int *next_insert_cost = &ILncost[screen_height - lines_moved - 1];
  int *next_delete_cost = &DLncost[screen_height - lines_moved - 1];

  /* initialize the top left corner of the matrix */
  matrix->writecost = 0;
  matrix->insertcost = SUPER_INFINITY;
  matrix->deletecost = SUPER_INFINITY;
  matrix->insertcount = 0;
  matrix->deletecount = 0;

  /* initialize the left edge of the matrix */
  cost = first_insert_cost[1] - next_insert_cost[1];
  for (i = 1; i <= window_size; i++)
    {
      p = matrix + i * (window_size + 1);
      cost += draw_cost[i] + next_insert_cost[i];
      p->insertcost = cost;
      p->writecost = SUPER_INFINITY;
      p->deletecost = SUPER_INFINITY;
      p->insertcount = i;
      p->deletecount = 0;
    }

  /* initialize the top edge of the matrix */
  cost = first_delete_cost[1] - next_delete_cost[1];
  for (j = 1; j <= window_size; j++)
    {
      cost += next_delete_cost[j];
      matrix[j].deletecost = cost;
      matrix[j].writecost = SUPER_INFINITY;
      matrix[j].insertcost = SUPER_INFINITY;
      matrix[j].deletecount = j;
      matrix[j].insertcount = 0;
    }

  /* `i' represents the vpos among new screen contents.
     `j' represents the vpos among the old screen contents.  */
  p = matrix + window_size + 2;	/* matrix [1, 1] */
  for (i = 1; i <= window_size; i++, p++)
    for (j = 1; j <= window_size; j++, p++)
      {
	/* p contains the address of matrix [i, j] */

	/* First calculate the cost assuming we do
	   not insert or delete above this line.
	   That is, if we update through line i-1
	   based on old lines through j-1,
	   and then just change old line j to new line i.  */
	p1 = p - window_size - 2; /* matrix [i-1, j-1] */
	cost = p1->writecost;
	if (cost > p1->insertcost)
	  cost = p1->insertcost;
	if (cost > p1->deletecost)
	  cost = p1->deletecost;
	if (old_hash[j] != new_hash[i])
	  cost += draw_cost[i];
	p->writecost = cost;

	/* Calculate the cost if we do an insert-line
	   before outputting this line.
	   That is, we update through line i-1
	   based on old lines through j,
	   do an insert-line on line i,
	   and then output line i from scratch,
	   leaving old lines starting from j for reuse below.  */
	p1 = p - window_size - 1; /* matrix [i-1, j] */
	/* No need to think about doing a delete followed
	   immediately by an insert.  It cannot be as good
	   as not doing either of them.  */
	if (free_at_end == i)
	  {
	    cost = p1->writecost;
	    cost1 = p1->insertcost;
	  }
	else
	  {
	    cost = p1->writecost + first_insert_cost[i];
	    if (p1->insertcount > i)
	      abort ();
	    cost1 = p1->insertcost + next_insert_cost[i - p1->insertcount];
	  }
	p->insertcost = min (cost, cost1) + draw_cost[i];
	p->insertcount = (cost < cost1) ? 1 : p1->insertcount + 1;
	if (p->insertcount > i)
	  abort ();

	/* Calculate the cost if we do a delete line after
	   outputting this line.
	   That is, we update through line i
	   based on old lines through j-1,
	   and throw away old line j.  */
	p1 = p - 1;		/* matrix [i, j-1] */
	/* No need to think about doing an insert followed
	   immediately by a delete.  */
	if (free_at_end == i)
	  {
	    cost = p1->writecost;
	    cost1 = p1->deletecost;
	  }
	else
	  {
	    cost = p1->writecost + first_delete_cost[i];
	    cost1 = p1->deletecost + next_delete_cost[i];
	  }
	p->deletecost = min (cost, cost1);
	p->deletecount = (cost < cost1) ? 1 : p1->deletecount + 1;
      }
}

/* Perform insert-lines and delete-lines operations
 according to the costs in the matrix.
 Updates the contents of current_screen to record what was done. */

do_scrolling (matrix, window_size, unchanged_at_top)
     struct matrix_elt *matrix;
     int window_size;
     int unchanged_at_top;
{
  register struct matrix_elt *p;
  register int i, j;
  struct queue { int count, pos; } *queue;
  int offset = unchanged_at_top;
  int qi = 0;
  int window = 0;
  register int tem;
  int next;

  queue = (struct queue *) alloca (screen_height * sizeof (struct queue));

  bcopy (current_screen->contents, temp_screen->contents,
	 current_screen->height * sizeof (char *));
  bcopy (current_screen->used, temp_screen->used,
	 current_screen->height * sizeof (int));
  bcopy (current_screen->highlight, temp_screen->highlight,
	 current_screen->height);
  bzero (temp_screen->enable, temp_screen->height);

/* First do all deletions of lines; queue up insertions.
   Also move lines to correct slots in current_screen.  */

  i = j = window_size;

  while (i > 0 || j > 0)
    {
      p = matrix + i * (window_size + 1) + j;
      tem = p->insertcost;
      if (tem < p->writecost && tem < p->deletecost)
	{
	  /* Insert should be done at vpos i-1, plus maybe some before */
	  queue[qi].count = p->insertcount;
	  i -= p->insertcount;
	  queue[qi++].pos = i + unchanged_at_top;
	}
      else if (p->deletecost < p->writecost)
	{
	  /* Old line at vpos j-1, and maybe some before it,
	     should be deleted */
	  j -= p->deletecount;
 	  if (!window)
	    {
	      set_terminal_window (window_size + unchanged_at_top);
	      window = 1;
	    }
	  ins_del_lines (j + unchanged_at_top, - p->deletecount);
	}
      else
	{
	  /* Best thing done here is no insert or delete */
	  /* Old line at vpos j-1 ends up at vpos i-1 */
	  current_screen->contents[i + offset - 1]
	    = temp_screen->contents[j + offset - 1];
	  current_screen->used[i + offset - 1]
	    = temp_screen->used[j + offset - 1];
	  current_screen->highlight[i + offset - 1]
	    = temp_screen->highlight[j + offset - 1];

	  temp_screen->enable[j + offset - 1] = 1;
	  i--;
	  j--;
	}
    }

  if (!window && qi)
    {
      set_terminal_window (window_size + unchanged_at_top);
      window = 1;
    }

  /* Now do all insertions */

  next = unchanged_at_top;
  for (i = qi - 1; i >= 0; i--)
    {
      ins_del_lines (queue[i].pos, queue[i].count);
      /* Mark the inserted lines as clear,
	 and put into them the line-contents strings
	 that were discarded during the deletions.
	 Those are the ones for which temp_screen->enable was not set.  */
      tem = queue[i].pos;
      for (j = tem + queue[i].count - 1; j >= tem; j--)
	{
	  current_screen->enable[j] = 0;
	  while (temp_screen->enable[next]) next++;
	  current_screen->contents[j] = temp_screen->contents[next++];
	}
    }

  if (window)
    set_terminal_window (0);
}

/* Return number of lines in common between current screen contents
   and the text to be displayed,
   considering only vpos range START to END (not including END).
   Ignores short lines (length < 20) on the assumption that
   avoiding redrawing such a line will have little weight.  */

int
scrolling_max_lines_saved (start, end, oldhash, newhash, cost)
     int start, end;
     int *oldhash, *newhash, *cost;
{
  struct { int hash; int count; } lines[01000];
  register int i, h;
  register int matchcount = 0;

  bzero (lines, sizeof lines);

  /* Put new lines' hash codes in hash table.  */
  for (i = start; i < end; i++)
    {
      if (cost[i] > 20)
	{
	  h = newhash[i] & 0777;
	  lines[h].hash = newhash[i];
	  lines[h].count++;
	}
    }

  /* Look up old line hash codes in the hash table.
     Count number of matches between old lines and new.  */

  for (i = start; i < end; i++)
    {
      h = oldhash[i] & 0777;
      if (oldhash[i] == lines[h].hash)
	{
	  matchcount++;
	  if (--lines[h].count == 0)
	    lines[h].hash = 0;
	}
    }

  return matchcount;
}

/* Return a measure of the cost of moving the lines
   starting with vpos FROM, up to but not including vpos TO,
   down by AMOUNT lines (AMOUNT may be negative).
   These are the same arguments that might be given to
   scroll_screen_lines to perform this scrolling.  */

scroll_cost (from, to, amount)
     int from, to, amount;
{
  /* Compute how many lines, at bottom of screen,
     will not be involved in actual motion.  */
  int ok_below = screen_height - to;
  if (amount > 0) ok_below -= amount;
  if (! scroll_region_ok) ok_below = 0;

  if (amount == 0)
    return 0;

  if (amount < 0)
    {
      int temp = to;
      to = from + amount;
      from = temp + amount;
      amount = - amount;
    }

  from += ok_below;
  to += ok_below;

  return (ILcost[from] + (amount - 1) * ILncost[from]
	  + DLcost[to] + (amount - 1) * DLncost[to]);
}

/* Calculate the insert and delete line costs.

   We keep the ID costs in a precomputed array based on the position
   at which the I or D is performed.  Also, there are two kinds of ID
   costs: the "once-only" and the "repeated".  This is to handle both
   those terminals that are able to insert N lines at a time (once-
   only) and those that must repeatedly insert one line.

   The cost to insert N lines at line L is
   	    [tt.t_ILov  + (screen_height + 1 - L) * tt.t_ILpf] +
	N * [tt.t_ILnov + (screen_height + 1 - L) * tt.t_ILnpf]

   ILov represents the basic insert line overhead.  ILpf is the padding
   required to allow the terminal time to move a line: insertion at line
   L changes (screen_height + 1 - L) lines.

   The first bracketed expression above is the overhead; the second is
   the multiply factor.  Both are dependent only on the position at
   which the insert is performed.  We store the overhead in ILcost and
   the multiply factor in ILncost.  Note however that any insertion
   must include at least one multiply factor.  Rather than compute this
   as ILcost[line]+ILncost[line], we add ILncost into ILcost.  This is
   reasonable because of the particular algorithm used in calcM.

   Deletion is essentially the same as insertion.
 */

CalcIDCosts (ins_line_string, multi_ins_string,
	     del_line_string, multi_del_string,
	     setup_string, cleanup_string)
     char *ins_line_string, *multi_ins_string;
     char *del_line_string, *multi_del_string;
     char *setup_string, *cleanup_string;
{
  /* Discourage long scrolls slightly on fast lines.
     This says that scrolling nearly the full length of the screen
     is not worth it if reprinting takes less than 1/4 second.  */
  int extra = baud_rate / (10 * 4 * screen_height);

  if (ILcost != 0)
    {
      ILcost = (int *) xrealloc (ILcost, screen_height * sizeof (int));
      DLcost = (int *) xrealloc (DLcost, screen_height * sizeof (int));
      ILncost = (int *) xrealloc (ILncost, screen_height * sizeof (int));
      DLncost = (int *) xrealloc (DLncost, screen_height * sizeof (int));
    }
  else
    {
      ILcost = (int *) xmalloc (screen_height * sizeof (int));
      DLcost = (int *) xmalloc (screen_height * sizeof (int));
      ILncost = (int *) xmalloc (screen_height * sizeof (int));
      DLncost = (int *) xmalloc (screen_height * sizeof (int));
    }

  CalcIDCosts1 (ins_line_string, multi_ins_string,
		setup_string, cleanup_string,
		ILcost, ILncost, extra);
  CalcIDCosts1 (del_line_string, multi_del_string,
		setup_string, cleanup_string,
		DLcost, DLncost, 0);
}

CalcIDCosts1 (one_line_string, multi_string,
	      setup_string, cleanup_string,
	      costvec, ncostvec, extra)
     char *one_line_string, *multi_string;
     char *setup_string, *cleanup_string;
     int *costvec, *ncostvec;
     int extra;
{
  if (calculate_costs_hook)
    (*calculate_costs_hook) (extra, costvec, ncostvec);
  else if (dont_calculate_costs)
    CalcLID (0, 0, 0, 0, costvec, ncostvec);
  else if (multi_string)
    CalcLID (string_cost (multi_string),
	     per_line_cost (multi_string),
	     extra, 0, costvec, ncostvec);
  else if (one_line_string)
    CalcLID (string_cost (setup_string) + string_cost (cleanup_string), 0,
	     string_cost (one_line_string) + extra,
	     per_line_cost (one_line_string),
	     costvec, ncostvec);
  else
    CalcLID (9999, 0, 9999, 0,
	     costvec, ncostvec);
}

/* Calculate the line ID overhead and multiply factor values */
CalcLID (ov1, pf1, ovn, pfn, ov, mf)
     int ov1, ovn;
     int pf1, pfn;
     register int *ov, *mf;
{
  register int i;
  register int insert_overhead = ov1 * 10 + screen_height * pf1;
  register int next_insert_cost = ovn * 10 + screen_height * pfn;

  for (i = 0; i < screen_height; i++)
    {
      *mf++ = next_insert_cost / 10;
      next_insert_cost -= pfn;
      *ov++ = (insert_overhead + next_insert_cost) / 10;
      insert_overhead -= pf1;
    }
}
