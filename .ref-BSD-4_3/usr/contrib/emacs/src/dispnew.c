/* Newly written part of redisplay code.
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

#ifdef USG
#include <termio.h>
#else /* not USG */
#include <sys/ioctl.h>
#endif /* not USG */

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

/* Nonzero means do not assume anything about current
 contents of actual terminal screen */

int screen_garbaged;

/* Desired terminal cursor position (to show position of point),
 origin zero */

int cursX, cursY;

/* Nonzero means last display completed and cursor is really at cursX, cursY.
 Zero means it was preempted. */

int display_completed;

int visible_bell;	/* If true and the terminal will support it
			   then the screen will flash instead of
			   feeping when an error occurs */
int inverse_video;	/* If true and the terminal will support it
			   then we will use inverse video */

int baud_rate;		/* Terminal speed, so we can calculate
			   the number of characters required to
			   make the cursor sit still for n secs. */

/* the current (physical) screen */
struct display_line *PhysScreen[MScreenLength + 1];

/* temporary Copy of PhysScreen made in update_screen */
struct display_line *OPhysScreen[MScreenLength + 1];

/* the desired (virtual) screen */
struct display_line *DesiredScreen[MScreenLength + 1];

/* Record here all the display line objects, for debugging.  */
static struct display_line *all_lines[2 * MScreenLength];

FILE *termscript;	/* Stdio stream being used for copy of all kbdinput.  */

struct cm Wcm;		/* Structure for info on cursor positioning */

extern short ospeed;	/* Output speed (from sg_ospeed) */

/* Use these to chain together free lines */

#define LINE_NEXT(l) (*(struct display_line **) l)
#define SET_LINE_NEXT(l, next) (*((struct display_line **) l) = next)

/* Chain of free display_line structures, chained thru LINE_NEXT.  */

struct display_line *free_display_lines;

/* Number of lines now free.  */

int free_line_count;

/* Allocate as many display_line structures
   as we are ever supposed to need.
   Called at startup, and also if screen size is changed.  */

make_display_lines ()
{
  register int i;
  register struct display_line *p, *p1;

  /* First, free any that are already allocated */

  for (p = free_display_lines; p;)
    {
      p1 = p;
      p = LINE_NEXT (p);
      free (p1);
    }
  free_display_lines = 0;
  free_line_count = 0;

  for (i = 0; i <= MScreenLength; i++)
    if (PhysScreen[i])
      {
	free (PhysScreen[i]);
	PhysScreen[i] = 0;
      }

  screen_garbaged = 1;

  /* Now allocate as many as we can possibly validly need */

  for (i = - screen_height; i < screen_height; i++)
    {
      p = (struct display_line *) malloc (sizeof (struct display_line) + screen_width - MScreenWidth);
      if (!p) abort ();
      SET_LINE_NEXT (p, free_display_lines);
      free_display_lines = p;
      all_lines[i + screen_height] = p;
    }
  free_line_count = 2 * screen_height;
}

/* Get one of the previously malloc'd display_line structures
   from the free pool.  */

struct display_line *
new_display_line ()
{
  register struct display_line *p = free_display_lines;
  /* If we ever use up all the display lines that have been
     allocated, it indicates a bug, since we are supposed
     to need at most two for each line on the screen.  */
  if (!p)
    abort ();
  free_display_lines = LINE_NEXT (p);

  bzero (p, p->body - (char *) p);
  SET_LINE_NEXT (p, (struct display_line *)1);	/* Mark as in use.  */
  free_line_count--;
  return p;
}

/* Put a display_line back in the free pool.  */

return_display_line (p)
     struct display_line *p;
{
  if (!p)
    return;
  if ((int) LINE_NEXT (p) != 1)
    abort ();			/* Already free.  */
  SET_LINE_NEXT (p, free_display_lines);
  free_display_lines = p;
  free_line_count++;
}

clear_screen_records ()
{
  register int i;
  for (i = 1; i <= screen_height; i++)
    if (PhysScreen[i])
      return_display_line (PhysScreen[i]);
  bzero (PhysScreen, (screen_height + 1) * sizeof PhysScreen[0]);
}

/* Return the hash code of display_line p.  */
line_hash_code (p)
     register struct display_line *p;
{
  register char *body, *end;
  register int h = 0;
  if (!p)
    return 0;
  /* Give all lighlighted lines the same hash code
     so as to encourage scrolling to leave them in place.  */
  if (p->highlighted)
    return -1;

  body = p->body;
  end = body + p->length;
  *end = 0;
  if (!must_write_spaces)
    {
      while (*body++ == ' ');
      body--;
      if (body == end)
	return 1;
      while (end[-1] == ' ') end--;
    }
  while (body != end)
    h = (h << 5) + h + *body++;
  if (h)
    return h;
  return 1;
}

/* Return number of characters in display_line p,
   except don't count leading and trailing spaces
   unless the terminal requires those to be explicitly output.  */

line_draw_cost (p)
     struct display_line *p;
{
  register char *body;
  register int i;

  if (!p)
    return 0;

  if (must_write_spaces)
    return p->length;

  body = p->body - 1;
  for (i = p->length; i > 0 && body[i - 1] == ' '; i--);

  i -= count_blanks (p->body);
  return max (i, 0);
}

/* The functions on this page are the interface from xdisp.c to redisplay.
 They take cursor position arguments in origin 0.

 The only other interface into redisplay is through setting
 cursX and cursY (in xdisp.c) and setting screen_garbaged. */

/* cancel_line eliminates any request to display a line at position `vpos' */

cancel_line (vpos)
     int vpos;
{
  return_display_line (DesiredScreen[vpos + 1]);
  DesiredScreen[vpos + 1] = 0;
}

/* Get a display_line for displaying on line `vpos'
 and set it up for outputting starting at `hpos' within it.  */

struct display_line *
get_display_line (vpos, hpos)
     int vpos;
     register int hpos;
{
  register struct display_line *line;
  register char *p;

  if (vpos < 0) abort ();

  line = DesiredScreen[vpos + 1];
  if (line && line->length > hpos)
    abort ();
  if (!line)
    line = new_display_line ();

  if (hpos > line->length)
    {
      p = line->body + line->length;
      hpos -= line->length;
      line->length += hpos;
      while (--hpos >= 0)
	*p++ = ' ';
    }

  DesiredScreen[vpos + 1] = line;

  return line;
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

      for (i = end + amount; i >= end + 1; i--)
	return_display_line (PhysScreen[i]);
      for (i = end; i >= from + 1; i--)
	PhysScreen[i + amount] = PhysScreen[i];
      for (i = from + amount; i >= from + 1; i--)
	PhysScreen[i] = 0;
    }
  if (amount < 0)
    {
      set_terminal_window (end);
      ins_del_lines (from + amount, amount);
      if (!scroll_region_ok)
	ins_del_lines (end + amount, -amount);
      set_terminal_window (0);

      for (i = from + amount + 1; i <= from; i++)
	return_display_line (PhysScreen[i]);
      for (i = from + 1; i <= end ; i++)
	PhysScreen[i + amount] = PhysScreen[i];
      for (i = end + amount + 1; i <= end; i++)
	PhysScreen[i] = 0;
    }
  return 1;
}

/* After updating a window w that isn't the full screen wide,
 copy all the columns that w does not occupy
 into the DesiredScreen lines from the PhysScreen lines
 so that update_screen will not change those columns.  */

preserve_other_columns (w)
     struct window *w;
{
  register int vpos;
  register struct display_line *l1, *l2;
  int start = XFASTINT (w->left);
  int end = XFASTINT (w->left) + XFASTINT (w->width);
  int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if ((l1 = DesiredScreen[vpos + 1])
	  && (l2 = PhysScreen[vpos + 1]))
	{
	  if (start > 0)
	    {
	      bcopy (l2->body, l1->body, start);
	      if (l1->length < start && l1->length < l2->length)
		l1->length = min (start, l2->length);
	    }
	  if (l2->length > end && l1->length < l2->length)
	    {
	      while (l1->length < end)
		l1->body[l1->length++] = ' ';
	      bcopy (l2->body + end, l1->body + end, l2->length - end);
	      l1->length = l2->length;
	    }
	}
    }
}

#ifdef NOTDEF

/* If window w does not need to be updated and isn't the full screen wide,
 copy all the columns that w does occupy
 into the DesiredScreen lines from the PhysScreen lines
 so that update_screen will not change those columns.

 Have not been able to figure out how to use this correctly.  */

preserve_my_columns (w)
     struct window *w;
{
  register int vpos, fin;
  register struct display_line *l1, *l2;
  int start = XFASTINT (w->left);
  int end = XFASTINT (w->left) + XFASTINT (w->width);
  int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if ((l1 = DesiredScreen[vpos + 1])
	  && (l2 = PhysScreen[vpos + 1]))
	{
	  if (l2->length > start && l1->length < l2->length)
	    {
	      fin = l2->length;
	      if (fin > end) fin = end;
	      while (l1->length < start)
		l1->body[l1->length++] = ' ';
	      bcopy (l2->body + start, l1->body + start, fin - start);
	      l1->length = fin;
	    }
	}
    }
}

#endif /* NOTDEF */

/* On discovering that the redisplay for a window was no good,
 cancel the columns of that window,
 so that when the window is displayed over again
 get_display_line will not complain. */

cancel_my_columns (w)
     struct window *w;
{
  register int vpos;
  register struct display_line *l;
  register int start = XFASTINT (w->left);
  register int bot = XFASTINT (w->top) + XFASTINT (w->height);

  for (vpos = XFASTINT (w->top); vpos < bot; vpos++)
    {
      if ((l = DesiredScreen[vpos + 1])
	  && l->length >= start)
	l->length = start;
    }
}

direct_output_for_insert (c)
     int c;
{
  register struct display_line *p = PhysScreen[cursY + 1];
#ifndef COMPILER_REGISTER_BUG
  register
#endif COMPILER_REGISTER_BUG
    struct window *w = XWINDOW (selected_window);
#ifndef COMPILER_REGISTER_BUG
  register
#endif COMPILER_REGISTER_BUG
    int hpos = cursX;

  /* Give up if about to continue line */
  if (hpos - XFASTINT (w->left) + 1 + 1 >= XFASTINT (w->width))
    return;

  /* Avoid losing if cursor is in invisible text off left margin */
  if (XINT (w->hscroll) && hpos == XFASTINT (w->left))
    return;
    
  /* Give up if cursor outside window (in minibuf, probably) */
  if (cursY < XFASTINT (w->top)
      || cursY >= XFASTINT (w->top) + XFASTINT (w->height))
    return;

  /* Give up if cursor not really at cursX, cursY */
  if (!display_completed)
    return;

  /* Give up if w is minibuffer and a message is being displayed there */
  if (EQ (selected_window, minibuf_window) && minibuf_message)
    return;

  p->body[hpos] = c;
  unchanged_modified = bf_modified;
  beg_unchanged = bf_s1;
  XFASTINT (w->last_point) = point;
  XFASTINT (w->last_point_x) = cursX;
  XFASTINT (w->last_modified) = bf_modified;

  reassert_line_highlight (0, cursY);
  write_chars (p->body + hpos, 1);
  fflush (stdout);
  ++cursX;
  p->length = max (p->length, cursX);
  p->body[p->length] = 0;
}

direct_output_forward_char (n)
     int n;
{
  register struct window *w = XWINDOW (selected_window);

  /* Avoid losing if cursor is in invisible text off left margin */
  if (XINT (w->hscroll) && cursX == XFASTINT (w->left))
    return;

  cursX += n;
  XFASTINT (w->last_point_x) = cursX;
  XFASTINT (w->last_point) = point;
  topos (cursY, cursX);
  fflush (stdout);
}

/* At the time this function is called,
 no line is common to PhysScreen and DesiredScreen.
 That is true again when this function returns. */

/* `force' nonzero means do not stop for pending input */

/* Value is nonzero if redisplay stopped due to pending input */
update_screen (force, inhibit_hairy_id)
     int force;
     int inhibit_hairy_id;
{
    register struct display_line **p;
    register struct display_line *l, *lnew;
    register int i;
    int pause;
    int preempt_count;
    int outq;
    extern input_pending;

    if (screen_height == 0) abort (); /* Some bug zeros some core */

    bcopy (PhysScreen, OPhysScreen, sizeof PhysScreen);

    detect_input_pending ();
    if (input_pending && !force)
      {
	pause = 1;
	goto do_pause;
      }

    update_begin ();

    if (!line_ins_del_ok)
      inhibit_hairy_id = 1;

    /* Don't compute for i/d line if just want cursor motion. */
    for (p = &DesiredScreen[screen_height]; p != DesiredScreen && *p == 0; p--);

    /* Try doing i/d line, if not yet inhibited.  */
    if (!inhibit_hairy_id && p != DesiredScreen)
      force |= scrolling ();

    /* Update the individual lines as needed.  Do bottom line first.  */

    l = DesiredScreen[screen_height];
    if (l && l != PhysScreen[screen_height])
      update_line (PhysScreen[screen_height], l, screen_height - 1);
    preempt_count = baud_rate / 2400;
    for (i = 1; i < screen_height && (force || !input_pending); i++)
      {
	l = PhysScreen[i];
	lnew = DesiredScreen[i];
	if (lnew && lnew != l)
	  {
	    /* Flush out every so many lines.
	       Also flush out if likely to have more than 1k buffered otherwise.
	       I'm told that telnet connections get really screwed by more
	       than 1k output at once.  */
	    outq = stdout->_ptr - stdout->_base;
	    if (outq > ((--preempt_count < 0) ? 20 : 900))
	      {
		fflush (stdout);
		if (baud_rate < 2400)
		  {
#ifdef TIOCOUTQ
		    if (ioctl (0, TIOCOUTQ, &outq) < 0)
		      /* Probably not a tty.  Ignore the error and reset
		       * the outq count. */
		      outq = stdout->_ptr - stdout->_base;
#endif
		    outq *= 10;
		    outq /= baud_rate;	/* outq is now in seconds */
		    if (outq)
		      sleep (outq);
		  }
		detect_input_pending ();

		preempt_count = baud_rate / 2400;
	      }
	    /* Now update this line.  */
	    update_line (l, lnew, i - 1);
	  }
      }
    pause = (i < screen_height) ? i : 0;

    /* Now just clean up termcap drivers and set cursor, etc.  */
    if (!pause)
      topos (cursY, max (min (cursX, screen_width - 1), 0));

    update_end ();

    if (termscript)
      fflush (termscript);
    fflush (stdout);

  do_pause:
    if (screen_height == 0) abort (); /* Some bug zeros some core */
    display_completed = !pause;
    /* Free any lines still in desired screen but not in phys screen */
    /* Free any lines that used to be in phys screen but are no longer */
    for (p = &PhysScreen[screen_height]; p != PhysScreen; p--)
      if (p[0]) p[0]->physical = 1;
    for (p = &DesiredScreen[screen_height]; p != DesiredScreen; p--)
      {
	if (l = *p)
	  {
	    if (!l->physical)
	      {
		return_display_line (l);
		/* Prevent line in both DesiredScreen and OPhysScreen
		   from being freed twice.  */
		l->physical = 1;
	      }
	  }
      }
    for (p = &OPhysScreen[screen_height]; p != OPhysScreen; p--)
      {
	if (l = *p)
	  {
	    if (!l->physical)
	      return_display_line (l);
	  }
      }
    i = 0;
    for (p = &PhysScreen[screen_height]; p != PhysScreen; p--)
      if (p[0])
	{
	  i++;
	  p[0]->physical = 0;
	}

    {
      extern int debug_end_pos;
      if (debug_end_pos && i + free_line_count != 2 * screen_height)
	abort ();
    }

    bzero (OPhysScreen, (screen_height + 1) * sizeof OPhysScreen[0]);
    bzero (DesiredScreen, (screen_height + 1) * sizeof DesiredScreen[0]);
    return pause;
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
      old_hash[i] = line_hash_code (PhysScreen[i + 1]);
      if (!DesiredScreen[i + 1])
	DesiredScreen[i + 1] = PhysScreen[i + 1];
      if (PhysScreen[i + 1] == DesiredScreen[i + 1])
	new_hash[i] = old_hash[i];
      else
	new_hash[i] = line_hash_code (DesiredScreen[i + 1]);
      if (old_hash[i] != new_hash[i])
	{
	  changed_lines++;
	  unchanged_at_bottom = screen_height - i - 1;
	}
      else if (i == unchanged_at_top)
	unchanged_at_top++;
      draw_cost[i] = line_draw_cost (DesiredScreen[i + 1]);
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
     PhysScreen and DesiredScreen, don't bother with i/d calc.  */
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

update_line (old, new, vpos)
     struct display_line *old, *new;
     int vpos;
{
  register char *obody, *nbody, *op1, *op2, *np1;
  int tem;
  int osp, nsp, m1, m2, olen, nlen;
  int save;

  if (old == new)
    return;

  /* Mark physical screen as containing the line `new' */
  PhysScreen[vpos + 1] = new;

  if ((new && new->highlighted) != (old && old->highlighted))
    {
      change_line_highlight (new && new->highlighted, vpos, old ? old->length : 0);
      old = 0;
    }
  else
    reassert_line_highlight (new && new->highlighted, vpos);

  if (!old)
    {
      olen = 0;
    }
  else
    {
      obody = old -> body;
      olen = old->length;
      if (!must_write_spaces)
	while (obody[olen - 1] == ' ')
	  olen--;
    }

  if (!new)
    {
      nlen = 0;
      goto just_erase;
    }

  nbody = new -> body;
  nlen = new->length;

  /* We know that the previous character is the `physical' field
     and it is zero or one.  */
  if (!must_write_spaces)
    while (nbody[nlen - 1] == ' ')
      nlen--;

  if (!olen)
    {
      nsp = (must_write_spaces || new->highlighted)
	      ? 0 : count_blanks (nbody);
      if (nlen > nsp)
	{
	  topos (vpos, nsp);
	  write_chars (nbody + nsp, nlen - nsp);
	}
      return;
    }

  obody[olen] = 1;
  save = nbody[nlen];
  nbody[nlen] = 0;

  /* Compute number of leading blanks in old and new contents.  */
  osp = count_blanks (obody);
  if (!new->highlighted)
    nsp = count_blanks (nbody);
  else
    nsp = 0;

  /* Compute number of matching chars starting with first nonblank.  */
  m1 = count_match (obody + osp, nbody + nsp);

  /* Spaces in new match implicit space past the end of old.  */
  /* This isn't really doing anything; osp should be osp + m1.
     I don't dare fix it now since maybe if it does anything
     it will do something bad.  */
  if (!must_write_spaces && osp == olen)
    {
      np1 = nbody + nsp;
      while (np1[m1] == ' ')
	m1++;
    }

  /* Avoid doing insert/delete char
     just cause number of leading spaces differs
     when the following text does not match. */
  if (m1 == 0 && osp != nsp)
    osp = nsp = min (osp, nsp);

  /* Find matching characters at end of line */
  op1 = obody + olen;
  np1 = nbody + nlen;
  op2 = op1 + m1 - min (olen - osp, nlen - nsp);
  while (op1 > op2 && op1[-1] == np1[-1])
    {
      op1--;
      np1--;
    }
  m2 = obody + olen - op1;

  /* Put correct value back in nbody[nlen].
     This is important because direct_output_for_insert
     can write into the line at a later point.  */
  nbody[nlen] = save;

  /* tem gets the distance to insert or delete.
     m2 is how many characters we save by doing so.
     Is it worth it?  */

  tem = (nlen - nsp) - (olen - osp);
  if (m2 && tem && m2 <= DCICcost[tem])
    m2 = 0;

  /* nsp - osp is the distance to insert or delete.
     m1 + m2 is how much we save by doing so.
     Is it worth it?  */

  if (m1 + m2 && nsp != osp && m1 + m2 <= DCICcost[nsp - osp])
    {
      m1 = 0;
      m2 = 0;
      osp = nsp = min (osp, nsp);
    }

  /* Now go through the line, inserting, writing and deleting as appropriate.  */

  if (osp > nsp)
    {
      topos (vpos, nsp);
      delete_chars (osp - nsp);
    }
  else if (nsp > osp)
    {
      /* If going to delete chars later in line
	 and insert earlier in the line,
	 must delete first to avoid losing data in the insert */
      if (m2 && nlen < olen + nsp - osp)
	{
	  topos (vpos, nlen - m2 + osp - nsp);
	  delete_chars (olen + nsp - osp - nlen);
	  olen = nlen - (nsp - osp);
	}
      topos (vpos, osp);
      insert_chars ((char *)0, nsp - osp);
    }
  olen += nsp - osp;
  osp = nsp;

  tem = nsp + m1 + m2;
  if (nlen != tem || olen != tem)
    {
      topos (vpos, nsp + m1);
      if (!m2 || nlen == olen)
	{
	  /* If new text being written reaches right margin,
	     there is no need to do clear-to-eol at the end.
	     (and it would not be safe, since cursor is not
	     going to be "at the margin" after the text is done) */
	  if (nlen == screen_width)
	    olen = 0;
	  write_chars (nbody + nsp + m1, nlen - tem);
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
	      /* m2 must be zero, and tem must equal nsp + m1 */
	      write_chars (nbody + tem, nlen - tem - 1);
	      clear_end_of_line (olen);
	      olen = 0;		/* Don't let it be cleared again later */
	      write_chars (nbody + nlen - 1, 1);
	    }
	  else
	    write_chars (nbody + nsp + m1, nlen - tem);
#endif
	}
      else if (nlen > olen)
	{
	  write_chars (nbody + nsp + m1, olen - tem);
	  insert_chars (nbody + nsp + m1 + olen - tem, nlen - olen);
	  olen = nlen;
	}
      else if (olen > nlen)
	{
	  write_chars (nbody + nsp + m1, nlen - tem);
	  delete_chars (olen - nlen);
	  olen = nlen;
	}
    }

 just_erase:
  /* If any unerased characters remain after the new line, erase them.  */
  if (olen > nlen)
    {
      topos (vpos, nlen);
      clear_end_of_line (olen);
    }
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
  "Start writing all terminal output to FILE as well.")
  (file)
     Lisp_Object file;
{
  file = Fexpand_file_name (file, Qnil);
  termscript = fopen (XSTRING (file)->data, "w");
  return Qnil;
}

DEFUN ("set-screen-height", Fset_screen_height, Sset_screen_height, 1, 1, 0,
  "Set number of lines on screen available for use in windows.")
  (n)
     Lisp_Object n;
{
  CHECK_NUMBER (n, 0);
  change_screen_size (XINT (n), 0);
  return Qnil;
}

DEFUN ("set-screen-width", Fset_screen_width, Sset_screen_width, 1, 1, 0,
  "Set number of columns on screen available for display.")
  (n)
     Lisp_Object n;
{
  CHECK_NUMBER (n, 0);
  change_screen_size (0, XINT (n));
  return Qnil;
}

DEFUN ("screen-height", Fscreen_height, Sscreen_height, 0, 0, 0,
  "Return number of lines on screen available for use in windows.")
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

/* Change the screen height and/or width.  Values may be given as zero to
   indicate no change is to take place. */
change_screen_size (newlength, newwidth)
     register int newlength, newwidth;
{
  if ((newlength == 0 || newlength == screen_height)
	  && (newwidth == 0 || newwidth == screen_width))
      return;
  if (newlength && newlength != screen_height)
    {
      if (newlength > MScreenLength)
	newlength = MScreenLength;
      set_window_height (XWINDOW (minibuf_window)->prev, newlength - 1, 0);
      XFASTINT (XWINDOW (minibuf_window)->top) = newlength - 1;
      set_window_height (minibuf_window, 1, 0);
      screen_height = newlength;
      set_terminal_window (0);
    }
  if (newwidth && newwidth != screen_width)
    {
      if (newwidth > MScreenWidth)
	newwidth = MScreenWidth;
      set_window_width (XWINDOW (minibuf_window)->prev, newwidth, 0);
      set_window_width (minibuf_window, newwidth, 0);
      screen_width = newwidth;
    }
  make_display_lines ();
  calculate_costs ();
  DoDsp (1);
}

DEFSIMPLE ("baud-rate", Fbaud_rate, Sbaud_rate,
	   "Return the output baud rate of the terminal.",
	   Lisp_Int, XSETINT, baud_rate)

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

DEFUN ("ding", Fding, Sding, 0, 0, 0,
  "Beep, or flash the screen.\n\
Terminates any keyboard macro currently executing.")
  ()
{
  Ding ();
  return Qnil;
}

Ding ()
{
  if (noninteractive)
    putchar (07);
  else if (!INTERACTIVE)  /* Stop executing a keyboard macro. */
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

#if defined(HAVE_SELECT) && defined(HAVE_TIMEVAL)
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
#else /* not both HAVE_SELECT and HAVE_TIMEVAL */
  /* Is it safe to quit out of `sleep'?  I'm afraid to trust it.  */
  sleep (t);
#endif /* not both HAVE_SELECT and HAVE_TIMEVAL */

  immediate_quit = 0;
#endif /* no subprocesses */
  return Qnil;
}

DEFUN ("sit-for", Fsit_for, Ssit_for, 1, 1, 0,
  "Perform redisplay, then wait for ARG seconds or until input is available")
  (n)
     Lisp_Object n;
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

  DoDsp (1);			/* Make the screen correct */
  if (XINT (n) <= 0) return Qnil;

#ifdef subprocesses
#ifdef SIGIO
  gobble_input ();
#endif /* SIGIO */
  wait_reading_process_input (XINT (n), 1, 1);
#else /* no subprocesses */
  immediate_quit = 1;
  QUIT;

  waitchannels = 1;
#ifndef HAVE_TIMEVAL
  timeout_sec = XINT (n);
  select (1, &waitchannels, 0, 0, &timeout_sec);
#else /* HAVE_TIMEVAL */
  timeout.tv_sec = XINT (n);  
  timeout.tv_usec = 0;
  select (1, &waitchannels, 0, 0, &timeout);
#endif /* HAVE_TIMEVAL */

  immediate_quit = 0;
#endif /* no subprocesses */
  return Qnil;
}

char *terminal_type;

/* Initialization done when Emacs fork is started, before doing stty. */
/* Determine terminal type and set terminal_driver */
/* Then invoke its decoding routine to set up variables
  in the terminal package */

init_display ()
{
  MetaFlag = 0;
  inverse_video = 0;

  /* Look at the TERM variable and set terminal_driver.  */

  terminal_type = (char *) getenv ("TERM");
  if (!terminal_type)
    {
      fprintf (stderr, "Please set the environment variable TERM; see tset(1).\n");
      exit (1);
    }
#ifdef HAVE_X_WINDOWS
  if (!strncmp (terminal_type, "xterm", 5))
    x_term_init ();
  else
#endif /* HAVE_X_WINDOWS */
    term_init (terminal_type);

  make_display_lines ();

  cursX = 0;		/* X and Y coordinates of the cursor */
  cursY = 0;		/* between updates. */
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

  DefBoolVar ("inverse-video", &inverse_video,
    "*Non-nil means use inverse-video.");
  DefBoolVar ("visible-bell", &visible_bell,
    "*Non-nil means try to flash the screen to represent a bell.");
}
