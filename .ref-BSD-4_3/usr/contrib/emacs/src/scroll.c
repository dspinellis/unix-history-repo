/* Calculate what ins/del line to do, and do it, for Emacs redisplay.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include "config.h"
#include "termchar.h"
#include "dispextern.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

/* All costs measured in characters.  Therefore, no cost
   can exceed MScreenLength * MScreenWidth (or so).
   That is about 10000, which fits in a short.  */

#define INFINITY 15000

struct matrix_elt
  {
    /* Cost of outputting through this line
       if no insert/delete is done just above it.  */
    short writecost;
    /* Cost of outputting through this line
       if an insert is done just above it.  */
    short insertcost;
    /* Cost of outputting through this line
       if a delete is done just above it.  */
    short deletecost;
    /* Number of inserts so far in this run of inserts,
       for the cost in insertcost.  */
    char insertcount;
    /* Number of deletes so far in this run of deletes,
       for the cost in deletecost.  */
    char deletecount;
  };

/* See CalcIDCosts for on the arrays below */
int ILcost[MScreenLength];/* ov(n) + 1*mf(n) */
int DLcost[MScreenLength];/* ov(n) + 1*mf(n) */
int ILncost[MScreenLength];/* mf(n) */
int DLncost[MScreenLength];/* mf(n) */

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
  int *first_insert_cost = &ILcost[screen_height - lines_moved];
  int *first_delete_cost = &DLcost[screen_height - lines_moved];
  int *next_insert_cost = &ILncost[screen_height - lines_moved];
  int *next_delete_cost = &DLncost[screen_height - lines_moved];

  /* initialize the top left corner of the matrix */
  matrix->writecost = 0;
  matrix->insertcost = INFINITY;
  matrix->deletecost = INFINITY;
  matrix->insertcount = 0;
  matrix->deletecount = 0;

  /* initialize the left edge of the matrix */
  cost = first_insert_cost[1] - next_insert_cost[1];
  for (i = 1; i <= window_size; i++)
    {
      p = matrix + i * (window_size + 1);
      cost += draw_cost[i] + next_insert_cost[i];
      p->insertcost = cost;
      p->writecost = INFINITY;
      p->deletecost = INFINITY;
      p->insertcount = i;
      p->deletecount = 0;
    }

  /* initialize the top edge of the matrix */
  cost = first_delete_cost[1] - next_delete_cost[1];
  for (j = 1; j <= window_size; j++)
    {
      cost += next_delete_cost[j];
      matrix[j].deletecost = cost;
      matrix[j].writecost = INFINITY;
      matrix[j].insertcost = INFINITY;
      matrix[j].deletecount = j;
      matrix[j].insertcount = j;
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
	    cost1 = p1->insertcost + next_insert_cost[i - p1->insertcount];
	  }
	p->insertcost = min (cost, cost1) + draw_cost[i];
	p->insertcount = (cost < cost1) ? 1 : p1->insertcount + 1;

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
 Updates the contents of PhysScreen to record what was done. */

do_scrolling (matrix, window_size, unchanged_at_top)
     struct matrix_elt *matrix;
     int window_size;
     int unchanged_at_top;
{
  register struct matrix_elt *p;
  register int i, j;
  struct { int count, pos; } queue[MScreenLength];
  int offset = unchanged_at_top;
  int qi = 0;
  int window = 0;
  register int tem;
  extern struct display_line *PhysScreen[MScreenLength + 1];
  extern struct display_line *OPhysScreen[MScreenLength + 1];

/* First do all deletions of lines; queue up insertions.
  Also move lines to correct slots in PhysScreen */

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
	  PhysScreen[i + offset] = OPhysScreen[j + offset];
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

  for (i = qi - 1; i >= 0; i--)
    {
      ins_del_lines (queue[i].pos, queue[i].count);
      /* mark the inserted lines as clear */
      tem = queue[i].pos;
      for (j = tem + queue[i].count; j > tem; j--)
	PhysScreen[j] = 0;
    }

  if (window)
    set_terminal_window (0);
}

/* Return number of lines in common between PhysScreen and DesiredScreen,
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
  if (multi_string)
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

  for (i = screen_height - 1; i > 0; i--)
    {
      *mf++ = next_insert_cost / 10;
      next_insert_cost -= pfn;
      *ov++ = (insert_overhead + next_insert_cost) / 10;
      insert_overhead -= pf1;
    }
}
