/* Newly written part of redisplay code.
   Copyright (C) 1985, 1986, 1987, 1988, 1990 Free Software Foundation, Inc.

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


#include <signal.h>

#include "config.h"
#include <stdio.h>

#ifdef HAVE_TIMEVAL
#ifdef HPUX
#include <time.h>
#else
#include <sys/time.h>
#endif
#endif

#ifdef HAVE_TERMIO
#include <termio.h>
#ifdef TCOUTQ
#undef TIOCOUTQ
#define TIOCOUTQ TCOUTQ
#include <fcntl.h>
#endif /* TCOUTQ defined */
#else
#ifndef VMS
#include <sys/ioctl.h>
#endif /* not VMS */
#endif /* not HAVE_TERMIO */

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

/* We are unable to use interrupts if FIONREAD is not available,
   so flush SIGIO so we won't try. */
#ifndef FIONREAD
#ifdef SIGIO
#undef SIGIO
#endif
#endif

#undef NULL

#include "termchar.h"
#include "termopts.h"
#include "cm.h"
#include "dispextern.h"
#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include "commands.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

#ifndef PENDING_OUTPUT_COUNT
/* Get number of chars of output now in the buffer of a stdio stream.
   This ought to be built in in stdio, but it isn't.
   Some s- files override this because their stdio internals differ.  */
#ifdef __GNU_LIBRARY__
#define	PENDING_OUTPUT_COUNT(FILE) ((FILE)->__bp - (FILE)->__buf)
#else
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_ptr - (FILE)->_base)
#endif
#endif /* No PENDING_OUTPUT_COUNT */

/* Nonzero means do not assume anything about current
   contents of actual terminal screen */

int screen_garbaged;

/* Desired terminal cursor position (to show position of point),
   origin zero */

int cursor_hpos, cursor_vpos;

/* Nonzero means last display completed and cursor is really at
   cursor_hpos, cursor_vpos.  Zero means it was preempted. */

int display_completed;

/* Lisp variable visible-bell; enables use of screen-flash
   instead of audible bell.  */

int visible_bell;

/* Invert the color of the whole screen, at a low level.  */

int inverse_video;

/* Line speed of the terminal.  */

int baud_rate;

/* nil or a symbol naming the window system
   under which emacs is running
   ('x is the only current possibility).  */

Lisp_Object Vwindow_system;

/* Version number of window system, or nil if no window system.  */

Lisp_Object Vwindow_system_version;

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.  */

int cursor_in_echo_area;

/* Description of actual screen contents.  */
 
struct matrix *current_screen;

/* Description of desired screen contents.  */

struct matrix *new_screen;

/* Buffer sometimes used to hold partial screen contents.  */

struct matrix *temp_screen;

/* Stdio stream being used for copy of all terminal output.  */

FILE *termscript;

/* Structure for info on cursor positioning */

struct cm Wcm;

int in_display;		/* 1 if in redisplay: can't handle SIGWINCH now.  */

int delayed_size_change;  /* 1 means SIGWINCH happened when not safe.  */
int delayed_screen_height;  /* Remembered new screen height.  */
int delayed_screen_width;   /* Remembered new screen width.  */

/* This buffer records the history of display preemption.  */

struct preempt
{
  /* Number of keyboard characters read so far at preempt.  */
  int keyboard_char_count;
  /* Vertical position at which preemption occurred.  */
  int vpos;
};

#define N_PREEMPTIONS 50

/* Circular buffer recording recent display preemptions.  */
struct preempt preemptions[N_PREEMPTIONS];

/* Index of next element in preemptions.  */
int preemption_index;

/* Set these variables in the debugger to force a display preemption.  */
int debug_preemption_vpos = -1;
int debug_preemption_char_count = -1;

extern int num_input_chars;

/* Free and reallocate current_screen and new_screen.  */

struct matrix *make_screen_structure ();

remake_screen_structures ()
{
  if (current_screen)
    free_screen_structure (current_screen);
  if (new_screen)
    free_screen_structure (new_screen);
  if (temp_screen)
    free_screen_structure (temp_screen);

  current_screen = make_screen_structure (0);
  new_screen = make_screen_structure (0);
  temp_screen = make_screen_structure (1);

  if (message_buf)
    message_buf = (char *) xrealloc (message_buf, screen_width + 1);
  else
    message_buf = (char *) xmalloc (screen_width + 1);
}

struct matrix *
make_screen_structure (empty)
     int empty;
{
  int i;
  struct matrix *new = (struct matrix *) xmalloc (sizeof (struct matrix));

  new->height = screen_height;
  new->width = screen_width;
  new->highlight = (char *) xmalloc (screen_height);
  new->enable = (char *) xmalloc (screen_height);
  new->contents = (unsigned char **) xmalloc (screen_height * sizeof (char *));
  new->used = (int *) xmalloc (screen_height * sizeof (int));
  if (empty)
    {
      /* Make the buffer used by decode_mode_spec.  */
      new->total_contents = (unsigned char *) xmalloc (screen_width + 2);
      bzero (new->contents, screen_height * sizeof (char *));
    }
  else
    {
      /* Add 2 to leave extra bytes at beginning and end of each line.  */ 
      new->total_contents = (unsigned char *) xmalloc (screen_height * (screen_width + 2));
      bzero (new->total_contents, screen_height * (screen_width + 2));
      for (i = 0; i < screen_height; i++)
	new->contents[i] = new->total_contents + i * (screen_width + 2) + 1;
    }
  bzero (new->enable, screen_height);
  return new;
}

free_screen_structure (matrix)
     struct matrix *matrix;
{
  if (matrix->total_contents)
    free (matrix->total_contents);
  free (matrix->contents);
  free (matrix->highlight);
  free (matrix->enable);
  free (matrix->used);
  free (matrix);
}

/* Return the hash code of contents of line VPOS of screen-matrix M.  */

int
line_hash_code (m, vpos)
     struct matrix *m;
     int vpos;
{
  register unsigned char *body;
  register int h = 0;
  /* Give all lighlighted lines the same hash code
     so as to encourage scrolling to leave them in place.  */
  if (m->highlight[vpos])
    return -1;

  body = m->contents[vpos];

  if (must_write_spaces)
    {
      while (1)
	{
	  int c = *body++;
	  if (c == 0)
	    break;
	  h = (((h << 4) + (h >> 24)) & 0x0fffffff) + c - ' ';
	}
    }
  else
    {
      while (1)
	{
	  int c = *body++;
	  if (c == 0)
	    break;
	  h = (((h << 4) + (h >> 24)) & 0x0fffffff) + c;
	}
    }
  if (h)
    return h;
  return 1;
}

/* Return number of characters in line in M at vpos VPOS,
   except don't count leading and trailing spaces
   unless the terminal requires those to be explicitly output.  */

int
line_draw_cost (m, vpos)
     struct matrix *m;
     int vpos;
{
  register unsigned char *body;
  register int i;

  if (must_write_spaces)
    return m->used[vpos];

  body = m->contents[vpos];
  for (i = m->used[vpos]; i > 0 && body[i - 2] == ' '; i--);

  i -= count_blanks (body);
  return max (i, 0);
}

/* The functions on this page are the interface from xdisp.c to redisplay.

 The only other interface into redisplay is through setting
 cursor_hpos and cursor_vpos (in xdisp.c) and setting screen_garbaged. */

/* cancel_line eliminates any request to display a line at position `vpos' */

cancel_line (vpos)
     int vpos;
{
  new_screen->enable[vpos] = 0;
}

clear_screen_records ()
{
  int i;

  bzero (current_screen->enable, screen_height);
}

/* Get ready to display on line `vpos'
   and set it up for outputting starting at `hpos' within it.
   Return the text string where that line is stored.  */

unsigned char *
get_display_line (vpos, hpos)
     int vpos;
     register int hpos;
{
  if (new_screen->enable[vpos] && new_screen->used[vpos] > hpos)
    abort ();
  if (! new_screen->enable[vpos])
    {
      new_screen->used[vpos] = 0;
      new_screen->highlight[vpos] = 0;
      new_screen->enable[vpos] = 1;
    }

  if (hpos > new_screen->used[vpos])
    {
      unsigned char *p = new_screen->contents[vpos] + new_screen->used[vpos];
      unsigned char *end = new_screen->contents[vpos] + hpos;
      new_screen->used[vpos] = hpos;
      while (p != end)
	*p++ = ' ';
    }

  return new_screen->contents[vpos];
}

/* Scroll lines from vpos `from' up to but not including vpos `end'
 down by `amount' lines (`amount' may be negative).
 Returns nonzero if done, zero if terminal cannot scroll them. */

int
scroll_screen_lines (from, end, amount)
     int from, end, amount;
{
  register int i;

  if (!line_ins_del_ok)
    return 0;

  if (amount == 0)
    return 1;
  if (amount > 0)
    {
      set_terminal_window (end + amount);
      if (!scroll_region_ok)
	ins_del_lines (end, -amount);
      ins_del_lines (from, amount);
      set_terminal_window (0);

      rotate_vector (current_screen->contents + from,
		     sizeof (char *) * (end + amount - from),
		     amount * sizeof (char *));
      safe_bcopy (current_screen->used + from,
		  current_screen->used + from + amount,
		  (end - from) * sizeof current_screen->used[0]);
      safe_bcopy (current_screen->highlight + from,
		  current_screen->highlight + from + amount,
		  (end - from) * sizeof current_screen->highlight[0]);
      safe_bcopy (current_screen->enable + from,
		  current_screen->enable + from + amount,
		  (end - from) * sizeof current_screen->enable[0]);
      /* Mark the lines made empty by scrolling as enabled, empty and
	 normal video.  */
      bzero (current_screen->used + from,
	     amount * sizeof current_screen->used[0]);
      bzero (current_screen->highlight + from,
	     amount * sizeof current_screen->highlight[0]);
      for (i = from; i < from + amount; i++)
	{
	  current_screen->contents[i][0] = '\0';
	  current_screen->enable[i] = 1;
	}
    }
  if (amount < 0)
    {
      set_terminal_window (end);
      ins_del_lines (from + amount, amount);
      if (!scroll_region_ok)
	ins_del_lines (end + amount, -amount);
      set_terminal_window (0);

      rotate_vector (current_screen->contents + from + amount,
		     sizeof (char *) * (end - from - amount),
		     (end - from) * sizeof (char *));
      safe_bcopy (current_screen->used + from,
		  current_screen->used + from + amount,
		  (end - from) * sizeof current_screen->used[0]);
      safe_bcopy (current_screen->highlight + from,
		  current_screen->highlight + from + amount,
		  (end - from) * sizeof current_screen->highlight[0]);
      safe_bcopy (current_screen->enable + from,
		  current_screen->enable + from + amount,
		  (end - from) * sizeof current_screen->enable[0]);
      /* Mark the lines made empty by scrolling as enabled, empty and
	 normal video.  */
      bzero (current_screen->used + end + amount,
	     - amount * sizeof current_screen->used[0]);
      bzero (current_screen->highlight + end + amount,
	     - amount * sizeof current_screen->highlight[0]);
      for (i = end + amount; i < end; i++)
	{
	  current_screen->contents[i][0] = '\0';
	  current_screen->enable[i] = 1;
	}
    }
  return 1;
}

/* Rotate a vector of SIZE bytes, by DISTANCE bytes.
   DISTANCE may be negative.  */

rotate_vector (vector, size, distance)
     char *vector;
     int size;
     int distance;
{
  char *temp = (char *) alloca (size);

  if (distance < 0)
    distance += size;

  bcopy (vector, temp + distance, size - distance);
  bcopy (vector + size - distance, temp, distance);
  bcopy (temp, vector, size);
}

/* Like bcopy except never gets confused by overlap.  */

safe_bcopy (from, to, size)
     char *from, *to;
     int size;
{
  register char *endf;
  register char *endt;

  if (size == 0)
    return;
  if (from > to)
    {
      /* If destination is lower in memory, we can go from the beginning.  */
      endf = from + size;
      while (from != endf)
	*to++ = *from++;
      return;
    }

  /* If destination is higher in memory, we can go backwards from the end.  */
  endf = from + size;
  endt = to + size;

  do
    *--endt = *--endf;
  while (endf != from);
}

/* After updating a window w that isn't the full screen wide,
 copy all the columns that w does not occupy
 from current_screen to new_screen,
 so that update_screen will not change those columns.  */

preserve_other_columns (w)
     struct window *w;
{
  register int vpos;
  int start = XFASTINT (w->left);
  int end = XFASTINT (w->left) + XFASTINT (w->width);
  int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if (current_screen->enable[vpos] && new_screen->enable[vpos])
	{
	  if (start > 0)
	    {
	      int len;

	      bcopy (current_screen->contents[vpos],
		     new_screen->contents[vpos], start);
	      len = min (start, current_screen->used[vpos]);
	      if (new_screen->used[vpos] < len)
		new_screen->used[vpos] = len;
	    }
	  if (current_screen->used[vpos] > end
	      && new_screen->used[vpos] < current_screen->used[vpos])
	    {
	      while (new_screen->used[vpos] < end)
		new_screen->contents[vpos][new_screen->used[vpos]++] = ' ';
	      bcopy (current_screen->contents[vpos] + end,
		     new_screen->contents[vpos] + end,
		     current_screen->used[vpos] - end);
	      new_screen->used[vpos] = current_screen->used[vpos];
	    }
	}
    }
}

/* On discovering that the redisplay for a window was no good,
 cancel the columns of that window,
 so that when the window is displayed over again
 get_display_line will not complain. */

cancel_my_columns (w)
     struct window *w;
{
  register int vpos;
  register int start = XFASTINT (w->left);
  register int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    if (new_screen->enable[vpos] && new_screen->used[vpos] >= start)
      new_screen->used[vpos] = start;
}

/* These functions try to perform directly and immediately on the screen
   the necessary output for one change in the buffer.
   They may return 0 meaning nothing was done if anything is difficult,
   or 1 meaning the output was performed properly.
   They assume that the screen was up to date before the buffer
   change being displayed.  THey make various other assumptions too;
   see command_loop_1 where these are called.  */

int
direct_output_for_insert (c)
     int c;
{
#ifndef COMPILER_REGISTER_BUG
  register
#endif COMPILER_REGISTER_BUG
    struct window *w = XWINDOW (selected_window);
#ifndef COMPILER_REGISTER_BUG
  register
#endif COMPILER_REGISTER_BUG
    int hpos = cursor_hpos;
#ifndef COMPILER_REGISTER_BUG
  register
#endif COMPILER_REGISTER_BUG
    int vpos = cursor_vpos;

  /* Give up if about to continue line */
  if (hpos - XFASTINT (w->left) + 1 + 1 >= XFASTINT (w->width)

  /* Avoid losing if cursor is in invisible text off left margin */
      || XINT (w->hscroll) && hpos == XFASTINT (w->left)
    
  /* Give up if cursor outside window (in minibuf, probably) */
      || cursor_vpos < XFASTINT (w->top)
      || cursor_vpos >= XFASTINT (w->top) + XFASTINT (w->height)

  /* Give up if cursor not really at cursor_hpos, cursor_vpos */
      || !display_completed

  /* Give up if w is minibuffer and a message is being displayed there */
      || EQ (selected_window, minibuf_window) && echo_area_contents)
    return 0;

  current_screen->contents[vpos][hpos] = c;
  unchanged_modified = MODIFF;
  beg_unchanged = GPT - BEG;
  XFASTINT (w->last_point) = point;
  XFASTINT (w->last_point_x) = cursor_hpos;
  XFASTINT (w->last_modified) = MODIFF;

  reassert_line_highlight (0, cursor_vpos);
  output_chars (&current_screen->contents[vpos][hpos], 1);
  fflush (stdout);
  ++cursor_hpos;
  if (hpos == current_screen->used[vpos])
    {
      current_screen->used[vpos] = hpos + 1;
      current_screen->contents[vpos][hpos + 1] = 0;
    }
  return 1;
}

int
direct_output_forward_char (n)
     int n;
{
  register struct window *w = XWINDOW (selected_window);

  /* Avoid losing if cursor is in invisible text off left margin */
  if (XINT (w->hscroll) && cursor_hpos == XFASTINT (w->left))
    return 0;

  cursor_hpos += n;
  XFASTINT (w->last_point_x) = cursor_hpos;
  XFASTINT (w->last_point) = point;
  move_cursor (cursor_vpos, cursor_hpos);
  fflush (stdout);
  return 1;
}

/* Update the actual terminal screen based on the data in new_screen.
   Value is nonzero if redisplay stopped due to pending input.
   FORCE nonzero means do not stop for pending input.  */

update_screen (force, inhibit_hairy_id)
     int force;
     int inhibit_hairy_id;
{
  register struct display_line **p;
  register struct display_line *l, *lnew;
  register int i;
  int pause;
  int preempt_count = baud_rate / 2400 + 1;
  extern input_pending;

  if (screen_height == 0) abort (); /* Some bug zeros some core */

  detect_input_pending ();
  if (!force
      && ((num_input_chars == debug_preemption_char_count
	   && debug_preemption_vpos == screen_height - 1)
	  || input_pending))
    {
      pause = screen_height;
      goto do_pause;
    }

  update_begin ();

  if (!line_ins_del_ok)
    inhibit_hairy_id = 1;

  /* Don't compute for i/d line if just want cursor motion. */
  for (i = 0; i < screen_height; i++)
    if (new_screen->enable)
      break;

  /* Try doing i/d line, if not yet inhibited.  */
  if (!inhibit_hairy_id && i < screen_height)
    force |= scrolling ();

  /* Update the individual lines as needed.  Do bottom line first.  */

  if (new_screen->enable[screen_height - 1])
    update_line (screen_height - 1);
  for (i = 0; i < screen_height - 1 && (force || !input_pending); i++)
    {
      if (!force && num_input_chars == debug_preemption_char_count
	  && debug_preemption_vpos == i)
	break;
      if (new_screen->enable[i])
	{
	  /* Flush out every so many lines.
	     Also flush out if likely to have more than 1k buffered
	     otherwise.   I'm told that telnet connections get really
	     screwed by more than 1k output at once.  */
	  int outq = PENDING_OUTPUT_COUNT (stdout);
	  if (outq > 900
	      || (outq > 20 && ((i - 1) % preempt_count == 0)))
	    {
	      fflush (stdout);
	      if (preempt_count == 1)
		{
#ifdef TIOCOUTQ
		  if (ioctl (0, TIOCOUTQ, &outq) < 0)
		    /* Probably not a tty.  Ignore the error and reset
		     * the outq count. */
		    outq = PENDING_OUTPUT_COUNT (stdout);
#endif
		  outq *= 10;
		  sleep (outq / baud_rate);
		}
	    }
	  if ((i - 1) % preempt_count == 0)
	    detect_input_pending ();
	  /* Now update this line.  */
	  update_line (i);
	}
    }
  pause = (i < screen_height - 1) ? i + 1 : 0;

  /* Now just clean up termcap drivers and set cursor, etc.  */
  if (!pause)
    {
      if (cursor_in_echo_area < 0)
	move_cursor (screen_height - 1, 0);
      else if (cursor_in_echo_area > 0
	       && !current_screen->enable[screen_height - 1])
	move_cursor (screen_height - 1, 0);
      else if (cursor_in_echo_area)
	move_cursor (screen_height - 1,
		     min (screen_width - 1,
			  current_screen->used[screen_height - 1]));
      else
	move_cursor (cursor_vpos, max (min (cursor_hpos, screen_width - 1), 0));
    }

  update_end ();

  if (termscript)
    fflush (termscript);
  fflush (stdout);

  /* Here if output is preempted because input is detected.  */
 do_pause:

  if (screen_height == 0) abort (); /* Some bug zeros some core */
  display_completed = !pause;
  if (pause)
    {
      preemptions[preemption_index].vpos = pause - 1;
      preemptions[preemption_index].keyboard_char_count = num_input_chars;
      preemption_index++;
      if (preemption_index == N_PREEMPTIONS)
	preemption_index = 0;
    }

  bzero (new_screen->enable, screen_height);
  return pause;
}

/* Called when about to quit, to check for doing so
   at an improper time.  */

void
quit_error_check ()
{
  if (new_screen == 0)
    return;
  if (new_screen->enable[0])
    abort ();
  if (new_screen->enable[screen_height - 1])
    abort ();
}

/* Decide what insert/delete line to do, and do it */

scrolling ()
{
  int unchanged_at_top, unchanged_at_bottom;
  int window_size;
  int changed_lines;
  int *old_hash = (int *) alloca (screen_height * sizeof (int));
  int *new_hash = (int *) alloca (screen_height * sizeof (int));
  int *draw_cost = (int *) alloca (screen_height * sizeof (int));
  register int i;
  int free_at_end_vpos = screen_height;
  
  /* Compute hash codes of all the lines.
     Also calculate number of changed lines,
     number of unchanged lines at the beginning,
     and number of unchanged lines at the end.  */

  changed_lines = 0;
  unchanged_at_top = 0;
  unchanged_at_bottom = screen_height;
  for (i = 0; i < screen_height; i++)
    {
      /* Give up on this scrolling if some old lines are not enabled.  */
      if (!current_screen->enable[i])
	return 0;
      old_hash[i] = line_hash_code (current_screen, i);
      if (!new_screen->enable[i])
	new_hash[i] = old_hash[i];
      else
	new_hash[i] = line_hash_code (new_screen, i);
      if (old_hash[i] != new_hash[i])
	{
	  changed_lines++;
	  unchanged_at_bottom = screen_height - i - 1;
	}
      else if (i == unchanged_at_top)
	unchanged_at_top++;
      /* If line is not changing, its redraw cost is infinite,
	 since we can't redraw it.  */
      if (!new_screen->enable[i])
	draw_cost[i] = INFINITY;
      else
	draw_cost[i] = line_draw_cost (new_screen, i);
    }

  /* If changed lines are few, don't allow preemption, don't scroll.  */
  if (changed_lines < baud_rate / 2400 || unchanged_at_bottom == screen_height)
    return 1;

  window_size = screen_height - unchanged_at_top - unchanged_at_bottom;

  if (scroll_region_ok)
    free_at_end_vpos -= unchanged_at_bottom;
  else if (memory_below_screen)
    free_at_end_vpos = -1;

  /* If large window, fast terminal and few lines in common between
     current_screen and new_screen, don't bother with i/d calc.  */
  if (window_size >= 18 && baud_rate > 2400
      && (window_size >=
	  10 * scrolling_max_lines_saved (unchanged_at_top,
					  screen_height - unchanged_at_bottom,
					  old_hash, new_hash, draw_cost)))
    return 0;

  scrolling_1 (window_size, unchanged_at_top, unchanged_at_bottom,
	       draw_cost + unchanged_at_top - 1,
	       old_hash + unchanged_at_top - 1,
	       new_hash + unchanged_at_top - 1,
	       free_at_end_vpos - unchanged_at_top);

  return 0;
}

update_line (vpos)
     int vpos;
{
  register unsigned char *obody, *nbody, *op1, *op2, *np1;
  int tem;
  int osp, nsp, begmatch, endmatch, olen, nlen;
  int save;
  unsigned char *temp;

  /* Check for highlighting change.  */
  if (new_screen->highlight[vpos]
      != (current_screen->enable[vpos] && current_screen->highlight[vpos]))
    {
      change_line_highlight (new_screen->highlight[vpos], vpos,
			     (current_screen->enable[vpos]
			      ? current_screen->used[vpos] : 0));
      current_screen->enable[vpos] = 0;
    }
  else
    reassert_line_highlight (new_screen->highlight[vpos], vpos);

  /* ??? */
  if (! current_screen->enable[vpos])
    {
      olen = 0;
    }
  else
    {
      obody = current_screen->contents[vpos];
      olen = current_screen->used[vpos];
      if (! current_screen->highlight[vpos])
	{
	  /* Note obody[-1] is always 0.  */
	  if (!must_write_spaces)
	    while (obody[olen - 1] == ' ')
	      olen--;
	}
      else
	{
	  /* For an inverse-video line, remember we gave it
	     spaces all the way to the screen edge
	     so that the reverse video extends all the way across.  */
	  while (olen < screen_width - 1)
	    obody[olen++] = ' ';
	}
    }

  /* One way or another, this will enable the line being updated.  */
  current_screen->enable[vpos] = 1;
  current_screen->used[vpos] = new_screen->used[vpos];
  current_screen->highlight[vpos] = new_screen->highlight[vpos];

  if (!new_screen->enable[vpos])
    {
      nlen = 0;
      goto just_erase;
    }

  nbody = new_screen->contents[vpos];
  nlen = new_screen->used[vpos];

  /* Pretend trailing spaces are not there at all,
     unless for one reason or another we must write all spaces.  */
  /* We know that the previous character byte contains 0.  */
  if (! new_screen->highlight[vpos])
    {
      if (!must_write_spaces)
	while (nbody[nlen - 1] == ' ')
	  nlen--;
    }
  else
    {
      /* For an inverse-video line, give it extra trailing spaces
	 all the way to the screen edge
	 so that the reverse video extends all the way across.  */
      while (nlen < screen_width - 1)
	nbody[nlen++] = ' ';
    }

  /* If there's no i/d char, quickly do the best we can without it.  */
  if (!char_ins_del_ok)
    {
      int i,j;

      for (i = 0; i < nlen; i++)
	{
	  if (i >= olen || nbody[i] != obody[i])
	    {
	      /* We found a non-matching char.  */
	      move_cursor (vpos, i);
	      for (j = 1; (i + j < nlen &&
			   (i + j >= olen || nbody[i+j] != obody[i+j]));
		   j++);
	      /* Output this run of non-matching chars.  */ 
	      output_chars (nbody + i, j);
	      i += j - 1;
	      /* Now find the next non-match.  */
	    }
	}
      /* Clear the rest of the line, or the non-clear part of it.  */
      if (olen > nlen)
	{
	  move_cursor (vpos, nlen);
	  clear_end_of_line (olen);
	}

      /* Exchange contents between current_screen and new_screen.  */
      temp = new_screen->contents[vpos];
      new_screen->contents[vpos] = current_screen->contents[vpos];
      current_screen->contents[vpos] = temp;
      return;
    }

  if (!olen)
    {
      nsp = (must_write_spaces || new_screen->highlight[vpos])
	      ? 0 : count_blanks (nbody);
      if (nlen > nsp)
	{
	  move_cursor (vpos, nsp);
	  output_chars (nbody + nsp, nlen - nsp);
	}

      /* Exchange contents between current_screen and new_screen.  */
      temp = new_screen->contents[vpos];
      new_screen->contents[vpos] = current_screen->contents[vpos];
      current_screen->contents[vpos] = temp;
      return;
    }

  obody[olen] = 1;
  save = nbody[nlen];
  nbody[nlen] = 0;

  /* Compute number of leading blanks in old and new contents.  */
  osp = count_blanks (obody);
  if (!new_screen->highlight[vpos])
    nsp = count_blanks (nbody);
  else
    nsp = 0;

  /* Compute number of matching chars starting with first nonblank.  */
  begmatch = count_match (obody + osp, nbody + nsp);

  /* Spaces in new match implicit space past the end of old.  */
  /* A bug causing this to be a no-op was fixed in 18.29.  */
  if (!must_write_spaces && osp + begmatch == olen)
    {
      np1 = nbody + nsp;
      while (np1[begmatch] == ' ')
	begmatch++;
    }

  /* Avoid doing insert/delete char
     just cause number of leading spaces differs
     when the following text does not match. */
  if (begmatch == 0 && osp != nsp)
    osp = nsp = min (osp, nsp);

  /* Find matching characters at end of line */
  op1 = obody + olen;
  np1 = nbody + nlen;
  op2 = op1 + begmatch - min (olen - osp, nlen - nsp);
  while (op1 > op2 && op1[-1] == np1[-1])
    {
      op1--;
      np1--;
    }
  endmatch = obody + olen - op1;

  /* Put correct value back in nbody[nlen].
     This is important because direct_output_for_insert
     can write into the line at a later point.  */
  nbody[nlen] = save;

  /* tem gets the distance to insert or delete.
     endmatch is how many characters we save by doing so.
     Is it worth it?  */

  tem = (nlen - nsp) - (olen - osp);
  if (endmatch && tem && endmatch <= DCICcost[tem])
    endmatch = 0;

  /* nsp - osp is the distance to insert or delete.
     begmatch + endmatch is how much we save by doing so.
     Is it worth it?  */

  if (begmatch + endmatch > 0 && nsp != osp
      && begmatch + endmatch <= DCICcost[nsp - osp])
    {
      begmatch = 0;
      endmatch = 0;
      osp = nsp = min (osp, nsp);
    }

  /* Now go through the line, inserting, writing and deleting as appropriate.  */

  if (osp > nsp)
    {
      move_cursor (vpos, nsp);
      delete_chars (osp - nsp);
    }
  else if (nsp > osp)
    {
      /* If going to delete chars later in line
	 and insert earlier in the line,
	 must delete first to avoid losing data in the insert */
      if (endmatch && nlen < olen + nsp - osp)
	{
	  move_cursor (vpos, nlen - endmatch + osp - nsp);
	  delete_chars (olen + nsp - osp - nlen);
	  olen = nlen - (nsp - osp);
	}
      move_cursor (vpos, osp);
      insert_chars ((char *)0, nsp - osp);
    }
  olen += nsp - osp;

  tem = nsp + begmatch + endmatch;
  if (nlen != tem || olen != tem)
    {
      move_cursor (vpos, nsp + begmatch);
      if (!endmatch || nlen == olen)
	{
	  /* If new text being written reaches right margin,
	     there is no need to do clear-to-eol at the end.
	     (and it would not be safe, since cursor is not
	     going to be "at the margin" after the text is done) */
	  if (nlen == screen_width)
	    olen = 0;
	  output_chars (nbody + nsp + begmatch, nlen - tem);
#ifdef obsolete
/* the following code loses disastrously if tem == nlen.
   Rather than trying to fix that case, I am trying the simpler
   solution found above.  */
	  /* If the text reaches to the right margin,
	     it will lose one way or another (depending on AutoWrap)
	     to clear to end of line after outputting all the text.
	     So pause with one character to go and clear the line then.  */
	  if (nlen == screen_width && fast_clear_end_of_line && olen > nlen)
	    {
	      /* endmatch must be zero, and tem must equal nsp + begmatch */
	      output_chars (nbody + tem, nlen - tem - 1);
	      clear_end_of_line (olen);
	      olen = 0;		/* Don't let it be cleared again later */
	      output_chars (nbody + nlen - 1, 1);
	    }
	  else
	    output_chars (nbody + nsp + begmatch, nlen - tem);
#endif
	}
      else if (nlen > olen)
	{
	  output_chars (nbody + nsp + begmatch, olen - tem);
	  insert_chars (nbody + nsp + begmatch + olen - tem, nlen - olen);
	  olen = nlen;
	}
      else if (olen > nlen)
	{
	  output_chars (nbody + nsp + begmatch, nlen - tem);
	  delete_chars (olen - nlen);
	  olen = nlen;
	}
    }

 just_erase:
  /* If any unerased characters remain after the new line, erase them.  */
  if (olen > nlen)
    {
      move_cursor (vpos, nlen);
      clear_end_of_line (olen);
    }
  
  /* Exchange contents between current_screen and new_screen.  */
  temp = new_screen->contents[vpos];
  new_screen->contents[vpos] = current_screen->contents[vpos];
  current_screen->contents[vpos] = temp;
}

count_blanks (str)
     char *str;
{
  register char *p = str;
  while (*str++ == ' ');
  return str - p - 1;
}

count_match (str1, str2)
     char *str1, *str2;
{
  register char *p1 = str1;
  register char *p2 = str2;
  while (*p1++ == *p2++);
  return p1 - str1 - 1;
}

DEFUN ("open-termscript", Fopen_termscript, Sopen_termscript,
  1, 1, "FOpen termscript file: ",
  "Start writing all terminal output to FILE as well as the terminal.\n\
FILE = nil means just close any termscript file currently open.")
  (file)
     Lisp_Object file;
{
  if (termscript != 0) fclose (termscript);
  termscript = 0;

  if (! NULL (file))
    {
      file = Fexpand_file_name (file, Qnil);
      termscript = fopen (XSTRING (file)->data, "w");
      if (termscript == 0)
	report_file_error ("Opening termscript", Fcons (file, Qnil));
    }
  return Qnil;
}

DEFUN ("set-screen-height", Fset_screen_height, Sset_screen_height, 1, 2, 0,
  "Tell redisplay that the screen has LINES lines.\n\
Optional second arg non-nil means that redisplay should use LINES lines\n\
but that the idea of the actual height of the screen should not be changed.")
  (n, pretend)
     Lisp_Object n, pretend;
{
  CHECK_NUMBER (n, 0);
  change_screen_size (XINT (n), 0, !NULL (pretend));
  return Qnil;
}

DEFUN ("set-screen-width", Fset_screen_width, Sset_screen_width, 1, 2, 0,
  "Tell redisplay that the screen has COLS columns.\n\
Optional second arg non-nil means that redisplay should use COLS columns\n\
but that the idea of the actual width of the screen should not be changed.")
  (n, pretend)
     Lisp_Object n, pretend;
{
  CHECK_NUMBER (n, 0);
  change_screen_size (0, XINT (n), !NULL (pretend));
  return Qnil;
}

DEFUN ("screen-height", Fscreen_height, Sscreen_height, 0, 0, 0,
  "Return number of lines on screen available for display.")
  ()
{
  return make_number (screen_height);
}

DEFUN ("screen-width", Fscreen_width, Sscreen_width, 0, 0, 0,
  "Return number of columns on screen available for display.")
  ()
{
  return make_number (screen_width);
}

#ifdef SIGWINCH
window_change_signal ()
{
  int width, height;
  extern int errno;
  int old_errno = errno;

  get_screen_size (&width, &height);
  /* Record the new size, but don't reallocate the data structures now.
     Let that be done later outside of the signal handler.  */
  in_display++;
  change_screen_size (height, width, 0);
  in_display--;
  signal (SIGWINCH, window_change_signal);

  errno = old_errno;
}
#endif /* SIGWINCH */

/* Do any change in screen size that was requested by a signal.  */

do_pending_window_change ()
{
  /* If change_screen_size should have run before, run it now.  */
  while (delayed_size_change)
    {
      int newwidth = delayed_screen_width;
      int newheight = delayed_screen_height;
      delayed_size_change = 0;
      change_screen_size_1 (newheight, newwidth, 0);
    }
}

/* Change the screen height and/or width.  Values may be given as zero to
   indicate no change is to take place.
   PRETEND is normally 0; 1 means change used-size only
   but don't change the size used for calculations;
   -1 means don't redisplay.  */

change_screen_size (newlength, newwidth, pretend)
     register int newlength, newwidth, pretend;
{
  /* If we can't deal with the change now, queue it for later.  */
  if (in_display)
    {
      delayed_screen_width = newwidth;
      delayed_screen_height = newlength;
      delayed_size_change = 1;
      return;
    }
  delayed_size_change = 0;
  change_screen_size_1 (newlength, newwidth, pretend);
}

change_screen_size_1 (newlength, newwidth, pretend)
     register int newlength, newwidth, pretend;
{
  if ((newlength == 0 || newlength == screen_height)
      && (newwidth == 0 || newwidth == screen_width))
    return;
  if (newlength && newlength != screen_height)
    {
      set_window_height (XWINDOW (minibuf_window)->prev, newlength - 1, 0);
      XFASTINT (XWINDOW (minibuf_window)->top) = newlength - 1;
      set_window_height (minibuf_window, 1, 0);
      screen_height = newlength;
      if (pretend <= 0)
	ScreenRows = newlength;
      set_terminal_window (0);
    }
  if (newwidth && newwidth != screen_width)
    {
      set_window_width (XWINDOW (minibuf_window)->prev, newwidth, 0);
      set_window_width (minibuf_window, newwidth, 0);
      screen_width = newwidth;
      if (pretend <= 0)
	ScreenCols = newwidth;
    }
  remake_screen_structures ();
  screen_garbaged = 1;
  calculate_costs ();
  if (pretend >= 0)
    redisplay_preserve_echo_area ();
}

DEFUN ("baud-rate", Fbaud_rate, Sbaud_rate, 0, 0, 0,
  "Return the output baud rate of the terminal.")
  ()
{
  Lisp_Object temp;
  XSET (temp, Lisp_Int, baud_rate);
  return temp;
}

DEFUN ("send-string-to-terminal", Fsend_string_to_terminal,
  Ssend_string_to_terminal, 1, 1, 0,
  "Send STRING to the terminal without alteration.\n\
Control characters in STRING will have terminal-dependent effects.")
  (str)
     Lisp_Object str;
{
  CHECK_STRING (str, 0);
  fwrite (XSTRING (str)->data, 1, XSTRING (str)->size, stdout);
  fflush (stdout);
  if (termscript)
    {
      fwrite (XSTRING (str)->data, 1, XSTRING (str)->size, termscript);
      fflush (termscript);
    }
  return Qnil;
}

DEFUN ("ding", Fding, Sding, 0, 1, 0,
  "Beep, or flash the screen.\n\
Terminates any keyboard macro currently executing unless an argument\n\
is given.")
  (arg)
  Lisp_Object arg;
{
  if (!NULL (arg))
    {
      bell ();
      fflush (stdout);
    }
  else
    bell ();
  return Qnil;
}

bell ()
{
  if (noninteractive)
    putchar (07);
  else if (!FROM_KBD)  /* Stop executing a keyboard macro. */
    error ("Keyboard macro terminated by a command ringing the bell");
  else
    ring_bell ();
  fflush (stdout);
}

DEFUN ("sleep-for", Fsleep_for, Ssleep_for, 1, 1, 0,
  "Pause, without updating display, for ARG seconds.")
  (n)
     Lisp_Object n;
{
  register int t;
#ifndef subprocesses
#ifdef HAVE_TIMEVAL
  struct timeval timeout, end_time, garbage1;
#endif /* HAVE_TIMEVAL */
#endif /* no subprocesses */

  CHECK_NUMBER (n, 0);
  t = XINT (n);
  if (t <= 0)
    return Qnil;

#ifdef subprocesses
  wait_reading_process_input (t, 0, 0);
#else /* No subprocesses */
  immediate_quit = 1;
  QUIT;

#ifdef VMS
  sys_sleep (t);
#else /* not VMS */
/* The reason this is done this way 
    (rather than defined (H_S) && defined (H_T))
   is because the VMS preprocessor doesn't grok `defined' */
#ifdef HAVE_SELECT
#ifdef HAVE_TIMEVAL
  gettimeofday (&end_time, &garbage1);
  end_time.tv_sec += t;

  while (1)
    {
      gettimeofday (&timeout, &garbage1);
      timeout.tv_sec = end_time.tv_sec - timeout.tv_sec;
      timeout.tv_usec = end_time.tv_usec - timeout.tv_usec;
      if (timeout.tv_usec < 0)
	timeout.tv_usec += 1000000,
      timeout.tv_sec--;
      if (timeout.tv_sec < 0)
	break;
      if (!select (1, 0, 0, 0, &timeout))
	break;
    }
#else /* not HAVE_TIMEVAL */
  /* Is it safe to quit out of `sleep'?  I'm afraid to trust it.  */
  sleep (t);
#endif /* HAVE_TIMEVAL */
#else /* not HAVE_SELECT */
  sleep (t);
#endif /* HAVE_SELECT */
#endif /* not VMS */
  
  immediate_quit = 0;
#endif /* no subprocesses */
  return Qnil;
}

DEFUN ("sit-for", Fsit_for, Ssit_for, 1, 2, 0,
  "Perform redisplay, then wait for ARG seconds or until input is available.\n\
Optional second arg non-nil means don't redisplay.\n\
Redisplay is preempted as always if input arrives, and does not happen\n\
if input is available before it starts.\n\
Value is t if waited the full time with no input arriving.")
  (n, nodisp)
     Lisp_Object n, nodisp;
{
#ifndef subprocesses
#ifdef HAVE_TIMEVAL
  struct timeval timeout;
#else
  int timeout_sec;
#endif
  int waitchannels;
#endif /* no subprocesses */

  CHECK_NUMBER (n, 0);

  if (detect_input_pending ())
    return Qnil;

  if (EQ (nodisp, Qnil))
    redisplay_preserve_echo_area ();
  if (XINT (n) > 0)
    {
#ifdef subprocesses
#ifdef SIGIO
      gobble_input ();
#endif				/* SIGIO */
      wait_reading_process_input (XINT (n), 1, 1);
#else				/* no subprocesses */
      immediate_quit = 1;
      QUIT;

      waitchannels = 1;
#ifdef VMS
      input_wait_timeout (XINT (n));
#else				/* not VMS */
#ifndef HAVE_TIMEVAL
      timeout_sec = XINT (n);
      select (1, &waitchannels, 0, 0, &timeout_sec);
#else				/* HAVE_TIMEVAL */
      timeout.tv_sec = XINT (n);  
      timeout.tv_usec = 0;
      select (1, &waitchannels, 0, 0, &timeout);
#endif				/* HAVE_TIMEVAL */
#endif				/* not VMS */

      immediate_quit = 0;
#endif				/* no subprocesses */
    }
  return detect_input_pending () ? Qnil : Qt;
}

char *terminal_type;

/* Initialization done when Emacs fork is started, before doing stty. */
/* Determine terminal type and set terminal_driver */
/* Then invoke its decoding routine to set up variables
  in the terminal package */

init_display ()
{
#ifdef HAVE_X_WINDOWS
  extern Lisp_Object Vxterm;
  Vxterm = Qnil;
#endif

  Vwindow_system = Qnil;
  meta_key = 0;
  inverse_video = 0;
  cursor_in_echo_area = 0;
  terminal_type = (char *) 0;

  if (!inhibit_window_system)
    {
#ifdef HAVE_X_WINDOWS
      extern char *alternate_display;
      char *disp = (char *) egetenv ("DISPLAY");

      /* Note KSH likes to provide an empty string as an envvar value.  */
      if (alternate_display || (disp && *disp))
	{
	  x_term_init ();
	  Vxterm = Qt;
	  Vwindow_system = intern ("x");
#ifdef X11
	  Vwindow_system_version = make_number (11);
#else
	  Vwindow_system_version = make_number (10);
#endif
	  goto term_init_done;
	}
#endif /* HAVE_X_WINDOWS */
      ;
    }
  /* Record we aren't using a window system.  */
  inhibit_window_system = 1;

  /* Look at the TERM variable */
  terminal_type = (char *) getenv ("TERM");
  if (!terminal_type)
    {
#ifdef VMS
      fprintf (stderr, "Please specify your terminal type.\n\
For types defined in VMS, use  set term /device=TYPE.\n\
For types not defined in VMS, use  define emacs_term \"TYPE\".\n\
\(The quotation marks are necessary since terminal types are lower case.)\n");
#else
      fprintf (stderr, "Please set the environment variable TERM; see tset(1).\n");
#endif
      exit (1);
    }
  term_init (terminal_type);

 term_init_done:
  remake_screen_structures ();
  calculate_costs ();

#ifdef SIGWINCH
#ifndef CANNOT_DUMP
  if (initialized)
#endif /* CANNOT_DUMP */
    if (inhibit_window_system)
      signal (SIGWINCH, window_change_signal);
#endif /* SIGWINCH */
}

syms_of_display ()
{
  defsubr (&Sopen_termscript);
  defsubr (&Sding);
  defsubr (&Ssit_for);
  defsubr (&Sscreen_height);
  defsubr (&Sscreen_width);
  defsubr (&Sset_screen_height);
  defsubr (&Sset_screen_width);
  defsubr (&Ssleep_for);
  defsubr (&Sbaud_rate);
  defsubr (&Ssend_string_to_terminal);

  DEFVAR_BOOL ("inverse-video", &inverse_video,
    "*Non-nil means use inverse-video.");
  DEFVAR_BOOL ("visible-bell", &visible_bell,
    "*Non-nil means try to flash the screen to represent a bell.");
  DEFVAR_BOOL ("no-redraw-on-reenter", &no_redraw_on_reenter,
    "*Non-nil means no need to redraw entire screen after suspending.\n\
It is up to you to set this variable to inform Emacs.");
  DEFVAR_LISP ("window-system", &Vwindow_system,
    "A symbol naming the window-system under which Emacs is running,\n\
\(such as `x'), or nil if emacs is running on an ordinary terminal.");
  DEFVAR_LISP ("window-system-version", &Vwindow_system_version,
    "Version number of the window system Emacs is running under.");
  DEFVAR_BOOL ("cursor-in-echo-area", &cursor_in_echo_area,
    "Non-nil means put cursor in minibuffer after any message displayed there.");

  /* Initialize `window-system', unless init_display already decided it.  */
#ifdef CANNOT_DUMP
  if (noninteractive)
#endif
    {
      Vwindow_system_version = Qnil;
      Vwindow_system = Qnil;
    }
}
