/* Window creation, deletion and examination for GNU Emacs.
   Does not include redisplay.
   Copyright (C) 1985, 1986, 1987, 1990 Free Software Foundation, Inc.

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


#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "window.h"
#include "commands.h"
#include "indent.h"
#include "termchar.h"

Lisp_Object Qwindowp;

Lisp_Object Fnext_window (), Fdelete_window (), Fselect_window ();
Lisp_Object Fset_window_buffer (), Fsplit_window (), Frecenter ();

/* This is the window which displays the minibuffer.
It is always the same window.  */

Lisp_Object minibuf_window;

/* This is the window in which the terminal's cursor should
 be left when nothing is being done with it.  This must
 always be a leaf window, and its buffer is selected by
 the top level editing loop at the end of each command.  */

Lisp_Object selected_window;

/* Non-nil means it is the window for C-M-v to scroll
   when the minibuffer is selected.  */

Lisp_Object Vminibuf_scroll_window;

/* Non-nil means it's function to call to display temp buffers.  */

Lisp_Object Vtemp_buffer_show_hook;

/* If a window gets smaller than either of these, it is removed. */

int window_min_height;
int window_min_width;

/* Nonzero implies pop_to_buffer should create windows. */

int pop_up_windows;

/* display-buffer always splits the largest window 
 if that window is more than this high */

int split_height_threshold;

/* Number of lines of continuity in scrolling by screenfuls.  */

int next_screen_context_lines;

/* Incremented for each window created.  */

static int sequence_number;

DEFUN ("windowp", Fwindowp, Swindowp, 1, 1, 0,
  "Returns t if OBJ is a window.")
  (obj)
     Lisp_Object obj;
{
  return XTYPE (obj) == Lisp_Window ? Qt : Qnil;
}

static Lisp_Object
make_window ()
{
  register Lisp_Object val;
  register struct window *p;

  /* Add sizeof (Lisp_Object) here because sizeof (struct Lisp_Vector)
     includes the first element.  */
  val = Fmake_vector (
    make_number ((sizeof (struct window) - sizeof (struct Lisp_Vector)
		  + sizeof (Lisp_Object))
		 / sizeof (Lisp_Object)),
    Qnil);
  XSETTYPE (val, Lisp_Window);
  p = XWINDOW (val);
  XFASTINT (p->sequence_number) = ++sequence_number;
  XFASTINT (p->left) = XFASTINT (p->top)
    = XFASTINT (p->height) = XFASTINT (p->width)
      = XFASTINT (p->hscroll) = 0;
  XFASTINT (p->last_point_x) = XFASTINT (p->last_point_y) = 0;
  p->start = Fmake_marker ();
  p->pointm = Fmake_marker ();
  XFASTINT (p->use_time) = 0;
  return val;
}

DEFUN ("selected-window", Fselected_window, Sselected_window, 0, 0, 0,
  "Return the window that the cursor now appears in and commands apply to.")
  ()
{
  return selected_window;
}

DEFUN ("minibuffer-window", Fminibuffer_window, Sminibuffer_window, 0, 0, 0,
  "Return the window used for minibuffers.")
  ()
{
  return minibuf_window;
}

DEFUN ("pos-visible-in-window-p", Fpos_visible_in_window_p,
  Spos_visible_in_window_p, 0, 2, 0,
  "Return t if position POS is currently on the screen in WINDOW.\n\
Returns nil if that position is scrolled vertically out of view.\n\
POS defaults to point; WINDOW, to the selected window.")
  (pos, window)
     Lisp_Object pos, window;
{
  register struct window *w;
  register int top;
  register int height;
  register int posint;
  register struct buffer *buf;
  struct position posval;

  if (NULL (pos))
    posint = point;
  else
    {
      CHECK_NUMBER_COERCE_MARKER (pos, 0);
      posint = XINT (pos);
    }

  if (NULL (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 1);
  w = XWINDOW (window);
  top = marker_position (w->start);

  if (posint < top)
    return Qnil;

  height = XFASTINT (w->height) - !EQ (window, minibuf_window);

  buf = XBUFFER (w->buffer);
  if (XFASTINT (w->last_modified) >= BUF_MODIFF (buf))
    {
      /* If screen is up to date,
	 use the info recorded about how much text fit on it. */
      if (posint < BUF_Z (buf) - XFASTINT (w->window_end_pos)
	  || (XFASTINT (w->window_end_vpos) < height))
	return Qt;
      return Qnil;
    }
  else
    {
      if (posint > BUF_Z (buf))
	return Qnil;
      /* If that info is not correct, calculate afresh */
      posval = *compute_motion (top, 0, 0,
			       posint, height, 0,
			       XFASTINT (w->width) - 1
			       - (XFASTINT (w->width) + XFASTINT (w->left) != XFASTINT (XWINDOW (minibuf_window)->width)),

			       XINT (w->hscroll), 0);
      return posval.vpos < height ? Qt : Qnil;
    }
}

static struct window *
decode_window (window)
     register Lisp_Object window;
{
  if (NULL (window))
    return XWINDOW (selected_window);

  CHECK_WINDOW (window, 0);
  return XWINDOW (window);
}

DEFUN ("window-buffer", Fwindow_buffer, Swindow_buffer, 0, 1, 0,
  "Return the buffer that WINDOW is displaying.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->buffer;
}

DEFUN ("window-height", Fwindow_height, Swindow_height, 0, 1, 0,
  "Return the number of lines in WINDOW (including its mode line).")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->height;
}

DEFUN ("window-width", Fwindow_width, Swindow_width, 0, 1, 0,
  "Return the number of columns in WINDOW.")
  (window)
     Lisp_Object window;
{
  register int w = decode_window (window)->width;
  /* If this window does not end at the right margin,
     must deduct one column for the border */
  if (w + decode_window (window)->left == screen_width)
    return w;
  return w - 1;
}

DEFUN ("window-hscroll", Fwindow_hscroll, Swindow_hscroll, 0, 1, 0,
  "Return the number of columns by which WINDOW is scrolled from left margin.")
  (window)
     Lisp_Object window;
{
  return decode_window (window)->hscroll;
}

DEFUN ("set-window-hscroll", Fset_window_hscroll, Sset_window_hscroll, 2, 2, 0,
  "Set number of columns WINDOW is scrolled from left margin to NCOL.\n\
NCOL should be zero or positive.")
  (window, ncol)
     register Lisp_Object window, ncol;
{
  register struct window *w;

  CHECK_NUMBER (ncol, 1);
  if (XINT (ncol) < 0) XFASTINT (ncol) = 0;
  if (XFASTINT (ncol) >= (1 << (SHORTBITS - 1)))
    args_out_of_range (ncol, Qnil);
  w = decode_window (window);
  if (w->hscroll != ncol)
    clip_changed = 1;		/* Prevent redisplay shortcuts */
  w->hscroll = ncol;
  return ncol;
}

DEFUN ("window-edges", Fwindow_edges, Swindow_edges, 0, 1, 0,
  "Return a list of the edge coordinates of WINDOW.\n\
\(LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at top left corner of screen.\n\
RIGHT is one more than the rightmost column used by WINDOW,\n\
and BOTTOM is one more than the bottommost row used by WINDOW\n\
 and its mode-line.")
  (window)
     Lisp_Object window;
{
  register struct window *w = decode_window (window);

  return Fcons (w->left, Fcons (w->top,
           Fcons (make_number (XFASTINT (w->left) + XFASTINT (w->width)),
		  Fcons (make_number (XFASTINT (w->top)
				      + XFASTINT (w->height)),
			 Qnil))));
}

DEFUN ("window-point", Fwindow_point, Swindow_point, 0, 1, 0,
  "Return current value of point in WINDOW.\n\
For a nonselected window, this is the value point would have\n\
if that window were selected.\n\
\n\
Note that, when WINDOW is the selected window and its buffer\n\
is also currently selected, the value returned is the same as (point).\n\
It would be more strictly correct to return the `top-level' value\n\
of point, outside of any  save-excursion  forms.\n\
But that is hard to define.")
  (window)
     Lisp_Object window;
{
  register struct window *w = decode_window (window);

  if (w == XWINDOW (selected_window)
      && current_buffer == XBUFFER (w->buffer))
    return Fpoint ();
  return Fmarker_position (w->pointm);
}

DEFUN ("window-start", Fwindow_start, Swindow_start, 0, 1, 0,
  "Return position at which display currently starts in WINDOW.")
  (window)
     Lisp_Object window;
{
  return Fmarker_position (decode_window (window)->start);
}

DEFUN ("set-window-point", Fset_window_point, Sset_window_point, 2, 2, 0,
  "Make point value in WINDOW be at position POS in WINDOW's buffer.")
  (window, pos)
     Lisp_Object window, pos;
{
  register struct window *w = decode_window (window);

  CHECK_NUMBER_COERCE_MARKER (pos, 1);
  if (w == XWINDOW (selected_window))
    Fgoto_char (pos);
  else
    set_marker_restricted (w->pointm, pos, w->buffer);
  return pos;
}

DEFUN ("set-window-start", Fset_window_start, Sset_window_start, 2, 3, 0,
  "Make display in WINDOW start at position POS in WINDOW's buffer.\n\
Optional third arg NOFORCE non-nil inhibits next redisplay\n\
from overriding motion of point in order to display at this exact start.")
  (window, pos, noforce)
     Lisp_Object window, pos, noforce;
{
  register struct window *w = decode_window (window);

  CHECK_NUMBER_COERCE_MARKER (pos, 1);
  set_marker_restricted (w->start, pos, w->buffer);
  /* this is not right, but much easier than doing what is right.  */
  w->start_at_line_beg = Qnil;
  if (NULL (noforce))
    w->force_start = Qt;
  w->update_mode_line = Qt;
  XFASTINT (w->last_modified) = 0;
  return pos;
}

DEFUN ("delete-window", Fdelete_window, Sdelete_window, 0, 1, "",
  "Remove WINDOW from the display.  Default is selected window.")
  (window)
     register Lisp_Object window;
{
  int osize;
  register Lisp_Object tem, parent;
  register struct window *p;
  register struct window *par;

  if (NULL (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

  p = XWINDOW (window);
  parent = p->parent;
  if (NULL (parent))
    error ("Attempt to delete minibuffer or sole ordinary window");
  par=XWINDOW (parent);

  windows_or_buffers_changed++;

  if (EQ (window, selected_window))
    Fselect_window (Fnext_window (window, Qnil));

  tem = p->buffer;
  /* tem is null for dummy parent windows
     (which have inferiors but not any contents themselves) */
  if (!NULL (tem))
    {
      unshow_buffer (p);
      unchain_marker (p->pointm);
      unchain_marker (p->start);
    }

  tem = p->next;
  if (!NULL (tem))
    XWINDOW (tem)->prev = p->prev;

  tem = p->prev;
  if (!NULL (tem))
    XWINDOW (tem)->next = p->next;

  if (EQ (window, par->hchild))
    par->hchild = p->next;
  if (EQ (window, par->vchild))
    par->vchild = p->next;

  /* Stretch the siblings to use all the available space */
  if (!NULL (par->vchild))
    {
      /* It's a vertical combination */
      osize = XFASTINT (par->height);
      XFASTINT (par->height)
	-= XFASTINT (p->height);
      set_window_height (parent, osize, 1);
    }
  if (!NULL (par->hchild))
    {
      /* It's a horizontal combination */
      osize = XFASTINT (par->width);
      XFASTINT (par->width)
	-= XFASTINT (p->width);
      set_window_width (parent, osize, 1);
    }

  /* If parent now has only one child,
     put the child into the parent's place.  */

  tem = par->hchild;
  if (NULL (tem))
    tem = par->vchild;
  if (NULL (XWINDOW (tem)->next))
    replace_window (parent, tem);
  return Qnil;
}

/* Put replacement into the window structure in place of old. */
static
replace_window (old, replacement)
     Lisp_Object old, replacement;
{
  register Lisp_Object tem;
  register struct window *o = XWINDOW (old), *p = XWINDOW (replacement);

  p->left = o->left;
  p->top = o->top;
  p->width = o->width;
  p->height = o->height;

  p->next = tem = o->next;
  if (!NULL (tem))
    XWINDOW (tem)->prev = replacement;

  p->prev = tem = o->prev;
  if (!NULL (tem))
    XWINDOW (tem)->next = replacement;

  p->parent = tem = o->parent;
  if (!NULL (tem))
    {
      if (EQ (XWINDOW (tem)->vchild, old))
	XWINDOW (tem)->vchild = replacement;
      if (EQ (XWINDOW (tem)->hchild, old))
	XWINDOW (tem)->hchild = replacement;
    }

/*** Here, if replacement is a vertical combination
and so is its new parent, we should make replacement's
children be children of that parent instead.  ***/
}

DEFUN ("next-window", Fnext_window, Snext_window, 0, 2, 0,
  "Return next window after WINDOW in canonical ordering of windows.\n\
Optional second arg MINIBUF t means count the minibuffer window\n\
even if not active.  If MINIBUF is neither t nor nil it means\n\
not to count the minibuffer even if it is active.")
  (window, mini)
     register Lisp_Object window, mini;
{
  register Lisp_Object tem;
  if (NULL (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);
  do
    {
      while (tem = XWINDOW (window)->next, NULL (tem))
	if (tem = XWINDOW (window)->parent, !NULL (tem))
	  window = tem;
        else  /* window must be minibuf_window now */
	  {
	    tem = XWINDOW (window)->prev;
	    break;
	  }
      window = tem;
      while (1)
	{
	  if (!NULL (XWINDOW (window)->hchild))
	    window = XWINDOW (window)->hchild;
	  else if (!NULL (XWINDOW (window)->vchild))
	    window = XWINDOW (window)->vchild;
	  else break;
	}
    }
  while (EQ (window, minibuf_window) && !EQ (mini, Qt)
	 && (!NULL (mini) || minibuf_level == 0));
  return window;
}

DEFUN ("previous-window", Fprevious_window, Sprevious_window, 0, 1, 0,
  "Return previous window before WINDOW in canonical ordering of windows.")
  (window)
     register Lisp_Object window;
{
  register Lisp_Object tem;
  if (NULL (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);
  do  /* at least once, and until not the minibuffer */
    {
      while (tem = XWINDOW (window)->prev, NULL (tem))
	if (tem = XWINDOW (window)->parent, !NULL (tem))
	  window = tem;
        else  /* window must be the root window now */
	  {
	    tem = minibuf_window;
	    break;
	  }
      window = tem;
      while (1)
	{
	  if (!NULL (XWINDOW (window)->hchild))
	    window = XWINDOW (window)->hchild;
	  else if (!NULL (XWINDOW (window)->vchild))
	    window = XWINDOW (window)->vchild;
	  else break;
	  while (tem = XWINDOW (window)->next, !NULL (tem))
	    window = tem;
	}
    }
  while (EQ (window, minibuf_window) && minibuf_level == 0);
  return window;
}

DEFUN ("other-window", Fother_window, Sother_window, 1, 1, "p",
  "Select the ARG'th different window.")
  (n)
     register Lisp_Object n;
{
  register int i;
  register Lisp_Object w;

  CHECK_NUMBER (n, 0);
  w = selected_window;
  i = XINT (n);

  while (i > 0)
    {
      w = Fnext_window (w, Qnil);
      i--;
    }
  while (i < 0)
    {
      w = Fprevious_window (w);
      i++;
    }
  Fselect_window (w);
  return Qnil;
}

static Lisp_Object
window_loop (type, obj)
     int type;
     register Lisp_Object obj;
{
  register Lisp_Object w, tem, ret_w;
  Lisp_Object w1, start_w;
  register struct window *p, *q;

  w = minibuf_window;
  ret_w = Qnil;
  while (1)
    {
      p = XWINDOW (w);
      w1 = Fnext_window (w, Qt);
      if (!EQ (w, minibuf_window))
	switch (type)
	  {
	  case 1:
	    if (XBUFFER (p->buffer) == XBUFFER (obj))
	      return w;
	    break;

	  case 2:
	    /* t as arg means consider only full-width windows */
	    if (!NULL (obj) && XFASTINT (p->width) != screen_width)
	      break;
	    if (NULL (ret_w) ||
		XFASTINT (XWINDOW (ret_w)->use_time) > XFASTINT (p->use_time))
	      ret_w = w;
	    break;

	  case 3:
	    if (p != XWINDOW (obj))
	      Fdelete_window (w);
	    break;

	  case 4:
	    if (EQ (p->buffer, obj))
	      {
		if (NULL (p->parent))
		  {
		    tem = Fother_buffer (obj);
		    if (NULL (tem))
		      tem = Fget_buffer_create (build_string ("*scratch*"));
		    Fset_window_buffer (w, tem);
		    Fset_buffer (p->buffer);
		  }
		else
		  Fdelete_window (w);
	      }
	    break;

	  case 5:
	    q = XWINDOW (ret_w);
	    if (NULL (ret_w) ||
		(XFASTINT (p->height) * XFASTINT (p->width))
		>
		(XFASTINT (q->height) * XFASTINT (q->width)))
	      ret_w = w;
	    break;

	  case 6:
	    if (EQ (p->buffer, obj))
	      {
		tem = Fother_buffer (obj);
		if (NULL (tem))
		  tem = Fget_buffer_create (build_string ("*scratch*"));
		Fset_window_buffer (w, tem);
	      }
	    break;
	  }
      w = w1;
      if (EQ (w, minibuf_window))
	return ret_w;
    }
}     

DEFUN ("get-lru-window", Fget_lru_window, Sget_lru_window, 0, 0, 0,
  "Return the window least recently selected or used for display.")
  ()
{
  register Lisp_Object w;
  /* First try for a window that is full-width */
  w = window_loop (2, Qt);
  if (!NULL (w) && !EQ (w, selected_window))
    return w;
  /* If none of them, try the rest */
  return window_loop (2, Qnil);
}

DEFUN ("get-largest-window", Fget_largest_window, Sget_largest_window, 0, 0, 0,
  "Return the largest window in area.")
  ()
{
  return window_loop (5, Qnil);
}

DEFUN ("get-buffer-window", Fget_buffer_window, Sget_buffer_window, 1, 1, 0,
  "Return a window currently displaying BUFFER, or nil if none.")
  (buffer)
     Lisp_Object buffer;
{
  buffer = Fget_buffer (buffer);
  if (XTYPE (buffer) == Lisp_Buffer)
    return window_loop (1, buffer);
  else return Qnil;
}

DEFUN ("delete-other-windows", Fdelete_other_windows, Sdelete_other_windows,
  0, 1, "",
  "Make WINDOW (or the selected window) fill the screen.")
  (w)
     Lisp_Object w;
{
  window_loop (3, !NULL (w) ? w : selected_window);
  return Qnil;
}

DEFUN ("delete-windows-on", Fdelete_windows_on, Sdelete_windows_on,
  1, 1, "bDelete windows on (buffer): ",
  "Delete all windows showing BUFFER.")
  (buffer)
     Lisp_Object buffer;
{
  if (!NULL (buffer))
    {
      buffer = Fget_buffer (buffer);
      CHECK_BUFFER (buffer, 0);
      window_loop (4, buffer);
    }
  return Qnil;
}

DEFUN ("replace-buffer-in-windows", Freplace_buffer_in_windows,
  Sreplace_buffer_in_windows,
  1, 1, "bReplace buffer in windows: ",
  "Replace BUFFER with some other buffer in all windows showing it.")
  (buffer)
     Lisp_Object buffer;
{
  if (!NULL (buffer))
    {
      buffer = Fget_buffer (buffer);
      CHECK_BUFFER (buffer, 0);
      window_loop (6, buffer);
    }
  return Qnil;
}

/* Set the height of WINDOW and all its inferiors.  */
/* Normally the window is deleted if it gets too small.
   nodelete nonzero means do not do this.
   (The caller should check later and do so if appropriate)  */

set_window_height (window, height, nodelete)
     Lisp_Object window;
     int height;
     int nodelete;
{
  register struct window *w = XWINDOW (window);
  register struct window *c;
  int oheight = XFASTINT (w->height);
  int top, pos, lastbot, opos, lastobot;
  Lisp_Object child;

  if (window_min_height < 2)
    window_min_height = 2;

  if (!nodelete
      && ! NULL (w->parent)
      && height < (EQ(window, minibuf_window) ? 1 : window_min_height))
    {
      Fdelete_window (window);
      return;
    }

  XFASTINT (w->last_modified) = 0;
  windows_or_buffers_changed++;
  XFASTINT (w->height) = height;
  if (!NULL (w->hchild))
    {
      for (child = w->hchild; !NULL (child); child = XWINDOW (child)->next)
	{
	  XWINDOW (child)->top = w->top;
	  set_window_height (child, height, nodelete);
	}
    }
  else if (!NULL (w->vchild))
    {
      lastbot = top = XFASTINT (w->top);
      lastobot = 0;
      for (child = w->vchild; !NULL (child); child = c->next)
	{
	  c = XWINDOW (child);

	  opos = lastobot + XFASTINT (c->height);

	  XFASTINT (c->top) = lastbot;

	  pos = (((opos * height) << 1) + oheight) / (oheight << 1);

	  /* Avoid confusion: inhibit deletion of child if becomes too small */
	  set_window_height (child, pos + top - lastbot, 1);

	  /* Now advance child to next window,
	     and set lastbot if child was not just deleted.  */
	  lastbot = pos + top, lastobot = opos;
	}
      /* Now delete any children that became too small.  */
      if (!nodelete)
	for (child = w->vchild; !NULL (child); child = XWINDOW (child)->next)
	  {
	    set_window_height (child, XINT (XWINDOW (child)->height), 0);
	  }
    }
}

/* Recursively set width of WINDOW and its inferiors. */

set_window_width (window, width, nodelete)
     Lisp_Object window;
     int width;
     int nodelete;
{
  register struct window *w = XWINDOW (window);
  register struct window *c;
  int owidth = XFASTINT (w->width);
  int left, pos, lastright, opos, lastoright;
  Lisp_Object child;

  if (!nodelete
      && ! NULL (w->parent)
      && width < window_min_width)
    {
      Fdelete_window (window);
      return;
    }

  XFASTINT (w->last_modified) = 0;
  windows_or_buffers_changed++;
  XFASTINT (w->width) = width;
  if (!NULL (w->vchild))
    {
      for (child = w->vchild; !NULL (child); child = XWINDOW (child)->next)
	{
	  XWINDOW (child)->left = w->left;
	  set_window_width (child, width, nodelete);
	}
    }
  else if (!NULL (w->hchild))
    {
      lastright = left = XFASTINT (w->left);
      lastoright = 0;
      for (child = w->hchild; !NULL (child); child = c->next)
	{
	  c = XWINDOW (child);

	  opos = lastoright + XFASTINT (c->width);

	  XFASTINT (c->left) = lastright;

	  pos = (((opos * width) << 1) + owidth) / (owidth << 1);

	  /* Inhibit deletion for becoming too small */
	  set_window_width (child, pos + left - lastright, 1);

	  /* Now advance child to next window,
	     and set lastright if child was not just deleted.  */
	  lastright = pos + left, lastoright = opos;
	}
      /* Delete children that became too small */
      if (!nodelete)
	for (child = w->hchild; !NULL (child); child = XWINDOW (child)->next)
	  {
	    set_window_width (child, XINT (XWINDOW (child)->width), 0);
	  }
    }
}

static int window_select_count;

DEFUN ("set-window-buffer", Fset_window_buffer, Sset_window_buffer, 2, 2, 0,
  "Make WINDOW display BUFFER as its contents.\n\
BUFFER can be a buffer or buffer name.")
  (window, buffer)
     register Lisp_Object window, buffer;
{
  register Lisp_Object tem;
  register struct window *w = decode_window (window);

  buffer = Fget_buffer (buffer);
  CHECK_BUFFER (buffer, 1);

  if (NULL (XBUFFER (buffer)->name))
    error ("Attempt to display deleted buffer");

  tem = w->buffer;
  if (!NULL (tem))
    unshow_buffer (w);

  w->buffer = buffer;
  Fset_marker (w->pointm,
	       make_number (BUF_PT (XBUFFER (buffer))),
	       buffer);
  set_marker_restricted (w->start,
			 make_number (XBUFFER (buffer)->last_window_start),
			 buffer);
  w->start_at_line_beg = Qnil;
  XFASTINT (w->last_modified) = 0;
  windows_or_buffers_changed++;
  if (EQ (window, selected_window))
    Fset_buffer (buffer);

  return Qnil;
}

/* Record info on buffer window w is displaying
   when it is about to cease to display that buffer.  */
static
unshow_buffer (w)
     register struct window *w;
{
  register Lisp_Object buf;
  buf = w->buffer;

  if (XBUFFER (buf) != XMARKER (w->pointm)->buffer)
    abort ();

  if (w == XWINDOW (selected_window)
      || ! EQ (buf, XWINDOW (selected_window)->buffer))
    /* Do this except when the selected window's buffer
       is being removed from some other window.  */
    XBUFFER (buf)->last_window_start = marker_position (w->start);

  /* Point in the selected window's buffer
     is actually stored in that buffer, and the window's pointm isn't used.
     So don't clobber point in that buffer.  */
  if (! EQ (buf, XWINDOW (selected_window)->buffer))
    BUF_PT (XBUFFER (buf)) = marker_position (w->pointm);
}

DEFUN ("select-window", Fselect_window, Sselect_window, 1, 1, 0,
  "Select WINDOW.  Most editing will apply to WINDOW's buffer.\n\
The main editor command loop selects the buffer of the selected window\n\
before each command.")
  (window)
     register Lisp_Object window;
{
  register struct window *w;
  register struct window *ow = XWINDOW (selected_window);

  CHECK_WINDOW (window, 0);

  w = XWINDOW (window);

  if (NULL (w->buffer))
    error ("Trying to select window with no buffer");

  XFASTINT (w->use_time) = ++window_select_count;
  if (EQ (window, selected_window))
    return window;

  Fset_marker (ow->pointm, make_number (BUF_PT (XBUFFER (ow->buffer))),
	       ow->buffer);

  selected_window = window;

  record_buffer (w->buffer);
  Fset_buffer (w->buffer);

  /* Go to the point recorded in the window.
     This is important when the buffer is in more
     than one window.  It also matters when
     redisplay_window has altered point after scrolling,
     because it makes the change only in the window.  */
  SET_PT (marker_position (w->pointm));
  if (point < BEGV)
    point = BEGV;
  if (point > ZV)
    point = ZV;

  windows_or_buffers_changed++;

  return window;
}

DEFUN ("display-buffer", Fdisplay_buffer, Sdisplay_buffer, 1, 2, 0,
  "Make BUFFER appear in some window but don't select it.\n\
BUFFER can be a buffer or a buffer name.\n\
If BUFFER is shown already in some window, just uses that one,\n\
unless the window is the selected window and NOTTHISWINDOW is non-nil.\n\
Returns the window displaying BUFFER.")
  (buffer, notthiswindow)
     register Lisp_Object buffer, notthiswindow;
{
  register Lisp_Object window;

  buffer = Fget_buffer (buffer);
  CHECK_BUFFER (buffer, 0);

  if (NULL (notthiswindow)
      && XBUFFER (XWINDOW (selected_window)->buffer) == XBUFFER (buffer))
    return selected_window;

  window = Fget_buffer_window (buffer);
  if (!NULL (window)
      && (NULL (notthiswindow) || !EQ (window, selected_window)))
    return window;

  if (pop_up_windows)
    {
      /* Don't try to create a window if would get an error */
      if (window_min_height < 2)
	window_min_height = 2;
      if (split_height_threshold < window_min_height << 1)
	split_height_threshold = window_min_height << 1;

      window = Fget_largest_window ();
      if (window_height (window) >= split_height_threshold
	  &&
	  XFASTINT (XWINDOW (window)->width) == screen_width)
	window = Fsplit_window (window, Qnil, Qnil);
      else
	{
	  window = Fget_lru_window ();
	  if ((EQ (window, selected_window)
	       || (EQ (selected_window, minibuf_window)
		   && EQ (window, XWINDOW (minibuf_window)->prev)))
	      && window_height (window) >= window_min_height << 1)
	    window = Fsplit_window (window, Qnil, Qnil);
	}
    }
  else
    window = Fget_lru_window ();

  Fset_window_buffer (window, buffer);
  return window;
}

temp_output_buffer_show (buf)
     register Lisp_Object buf;
{
  register struct buffer *old = current_buffer;
  register Lisp_Object window;
  register struct window *w;

  Fset_buffer (buf);
  XBUFFER (buf)->save_modified = MODIFF;
  BEGV = BEG;
  ZV = Z;
  SET_PT (BEG);
  clip_changed = 1;
  set_buffer_internal (old);

  if (!EQ (Vtemp_buffer_show_hook, Qnil))
    call1 (Vtemp_buffer_show_hook, buf);
  else
    {
      window = Fdisplay_buffer (buf, Qnil);
      Vminibuf_scroll_window = window;
      w = XWINDOW (window);
      XFASTINT (w->hscroll) = 0;
      set_marker_restricted (w->start, make_number (1), buf);
      set_marker_restricted (w->pointm, make_number (1), buf);
    }
}

static
make_dummy_parent (window)
     Lisp_Object window;
{
  register Lisp_Object old, new;
  register struct window *o, *p;

  old = window;
  XSETTYPE (old, Lisp_Vector);
  new = Fcopy_sequence (old);
  XSETTYPE (new, Lisp_Window);

  o = XWINDOW (old);
  p = XWINDOW (new);
  XFASTINT (p->sequence_number) = ++sequence_number;

  /* Put new into window structure in place of window */
  replace_window (window, new);

  o->next = Qnil;
  o->prev = Qnil;
  o->vchild = Qnil;
  o->hchild = Qnil;
  o->parent = new;

  p->start = Qnil;
  p->pointm = Qnil;
  p->buffer = Qnil;
}

DEFUN ("split-window", Fsplit_window, Ssplit_window, 0, 3, "",
  "Split WINDOW, putting SIZE lines in the first of the pair.\n\
WINDOW defaults to selected one and SIZE to half its size.\n\
If optional third arg HOR-FLAG is non-nil, split side by side\n\
and put SIZE columns in the first of the pair.")
  (window, chsize, horflag)
     Lisp_Object window, chsize, horflag;
{
  register Lisp_Object new;
  register struct window *o, *p;
  register int size;

  if (NULL (window))
    window = selected_window;
  else
    CHECK_WINDOW (window, 0);

  o = XWINDOW (window);

  if (NULL (chsize))
    {
      if (!NULL (horflag))
	/* Add 1 so we round up rather than down.
	   This puts an excess column into the left-hand window,
	   which is the one that certainly contains a border line.  */
	size = (1 + XFASTINT (o->width)) >> 1;
      else
	size = XFASTINT (o->height) >> 1;
    }
  else
    {
      CHECK_NUMBER (chsize, 1);
      size = XINT (chsize);
    }

  if (EQ (window, minibuf_window))
    error ("Attempt to split minibuffer window");

  if (NULL (horflag))
    {
      if (window_min_height < 2)
	window_min_height = 2;

      if (size < window_min_height ||
	  size + window_min_height > XFASTINT (o->height))
	args_out_of_range_3 (window, chsize, horflag);
      if (NULL (o->parent) ||
	  NULL (XWINDOW (o->parent)->vchild))
	{
	  make_dummy_parent (window);
	  new = o->parent;
	  XWINDOW (new)->vchild = window;
	}
    }
  else
    {
      if (size < window_min_width ||
	  size + window_min_width > XFASTINT (o->width))
	args_out_of_range_3 (window, chsize, horflag);
      if (NULL (o->parent) ||
	  NULL (XWINDOW (o->parent)->hchild))
	{
	  make_dummy_parent (window);
	  new = o->parent;
	  XWINDOW (new)->hchild = window;
	}
    }

  /* Now we know that window's parent is a vertical combination
     if we are dividing vertically, or a horizontal combination
     if we are making side-by-side windows */

  windows_or_buffers_changed++;
  new = make_window ();
  p = XWINDOW (new);

  p->next = o->next;
  if (!NULL (p->next))
    XWINDOW (p->next)->prev = new;
  p->prev = window;
  o->next = new;
  p->parent = o->parent;

  Fset_window_buffer (new, o->buffer);

  /* Apportion the available screen space among the two new windows */

  if (!NULL (horflag))
    {
      p->height = o->height;
      p->top = o->top;
      XFASTINT (p->width) = XFASTINT (o->width) - size;
      XFASTINT (o->width) = size;
      XFASTINT (p->left) = XFASTINT (o->left) + size;
    }
  else
    {
      p->left = o->left;
      p->width = o->width;
      XFASTINT (p->height) = XFASTINT (o->height) - size;
      XFASTINT (o->height) = size;
      XFASTINT (p->top) = XFASTINT (o->top) + size;
    }

  return new;
}

DEFUN ("enlarge-window", Fenlarge_window, Senlarge_window, 1, 2, "p",
  "Make current window ARG lines bigger.\n\
From program, optional second arg non-nil means grow sideways ARG columns.")
  (n, side)
     register Lisp_Object n, side;
{
  CHECK_NUMBER (n, 0);
  change_window_height (XINT (n), !NULL (side));
  return Qnil;
}

DEFUN ("shrink-window", Fshrink_window, Sshrink_window, 1, 2, "p",
  "Make current window ARG lines smaller.\n\
From program, optional second arg non-nil means shrink sideways ARG columns.")
  (n, side)
     register Lisp_Object n, side;
{
  CHECK_NUMBER (n, 0);
  change_window_height (-XINT (n), !NULL (side));
  return Qnil;
}

int
window_height (window)
     Lisp_Object window;
{
  register struct window *p = XWINDOW (window);
  return XFASTINT (p->height);
}

int
window_width (window)
     Lisp_Object window;
{
  register struct window *p = XWINDOW (window);
  return XFASTINT (p->width);
}

#define MINSIZE(window) \
  (widthflag ? window_min_width  \
   : (EQ (window, minibuf_window) ? 1 : window_min_height))

#define CURBEG(w) \
  *(widthflag ? (int *) &w->left : (int *) &w->top)

#define CURSIZE(w) \
  *(widthflag ? (int *) &w->width : (int *) &w->height)

/* Unlike set_window_height, this function
 also changes the heights of the siblings so as to
 keep everything consistent. */

change_window_height (delta, widthflag)
     register int delta;
     int widthflag;
{
  register Lisp_Object parent;
  Lisp_Object window;
  register struct window *p;
  int *sizep;
  int (*sizefun) () = widthflag ? window_width : window_height;
  register int (*setsizefun) () = widthflag ? set_window_width : set_window_height;

  if (window_min_height < 2)
    window_min_height = 2;

  window = selected_window;
  while (1)
    {
      p = XWINDOW (window);
      parent = p->parent;
      if (NULL (parent))
	{
	  if (widthflag)
	    error ("No other window to side of this one");
	  break;
	}
      if (widthflag ? !NULL (XWINDOW (parent)->hchild)
	  : !NULL (XWINDOW (parent)->vchild))
	break;
      window = parent;
    }

  sizep = &CURSIZE (p);

  if (*sizep + delta < MINSIZE (window))
    {
      Fdelete_window (window);
      return;
    }

  {
    register int maxdelta;
    register Lisp_Object tem;

    maxdelta = (!NULL (parent) ? (*sizefun) (parent) - *sizep
		: (tem = (!NULL (p->next) ? p->next : p->prev),
		   (*sizefun) (tem) - MINSIZE (tem)));

    if (delta > maxdelta)
      /* This case traps trying to make the minibuffer
	 the full screen, or make the only window aside from the
	 minibuffer the full screen.  */
      delta = maxdelta;
  }

  if (!NULL (p->next) &&
      (*sizefun) (p->next) - delta >= MINSIZE (p->next))
    {
      (*setsizefun) (p->next, (*sizefun) (p->next) - delta, 0);
      (*setsizefun) (window, *sizep + delta, 0);
      CURBEG (XWINDOW (p->next)) += delta;
      /* This does not change size of p->next,
	 but it propagates the new top edge to its children */
      (*setsizefun) (p->next, (*sizefun) (p->next), 0);
    }
  else if (!NULL (p->prev) &&
	   (*sizefun) (p->prev) - delta >= MINSIZE (p->prev))
    {
      (*setsizefun) (p->prev, (*sizefun) (p->prev) - delta, 0);
      CURBEG (p) -= delta;
      (*setsizefun) (window, *sizep + delta, 0);
    }
  else
    {
      register int delta1;
      register int opht = (*sizefun) (parent);

      /* If trying to grow this window to or beyond size of the parent,
	 make delta1 so big that, on shrinking back down,
	 all the siblings end up with less than one line and are deleted.  */
      if (opht <= *sizep + delta)
	delta1 = opht * opht * 2;
      /* Otherwise, make delta1 just right so that if we add delta1
	 lines to this window and to the parent, and then shrink
	 the parent back to its original size, the new proportional
	 size of this window will increase by delta.  */
      else
	delta1 = (delta * opht * 100) / ((opht - *sizep - delta) * 100);

      /* Add delta1 lines or columns to this window, and to the parent,
	 keeping things consistent while not affecting siblings.  */
      CURSIZE (XWINDOW (parent)) = opht + delta1;
      (*setsizefun) (window, *sizep + delta1, 0);

      /* Squeeze out delta1 lines or columns from our parent,
	 shriking this window and siblings proportionately.
	 This brings parent back to correct size.
	 Delta1 was calculated so this makes this window the desired size,
	 taking it all out of the siblings.  */
      (*setsizefun) (parent, opht, 0);
    }

  XFASTINT (p->last_modified) = 0;
}
#undef MINSIZE
#undef CURBEG
#undef CURSIZE


static
window_scroll (window, n)
     Lisp_Object window;
     int n;
{
  register struct window *w = XWINDOW (window);
  register int opoint = point;
  register int ht, pos;
  register Lisp_Object tem;
  int lose;
  Lisp_Object bolp;

  ht = XFASTINT (w->height) - !EQ (window, minibuf_window);

  XFASTINT (tem) = point;
  tem = Fpos_visible_in_window_p (tem, window);

  if (NULL (tem))
    {
      Fvertical_motion (make_number (- ht / 2));
      XFASTINT (tem) = point;
      Fset_marker (w->start, tem, w->buffer);
      w->force_start = Qt;
    }

  SET_PT (marker_position (w->start));
  lose = n < 0 && point == BEGV;
  Fvertical_motion (make_number (n));
  pos = point;
  bolp = Fbolp ();
  SET_PT (opoint);

  if (lose)
    Fsignal (Qbeginning_of_buffer, Qnil);

  if (pos < ZV)
    {
      set_marker_restricted (w->start, make_number (pos), w->buffer);
      w->start_at_line_beg = bolp;
      w->update_mode_line = Qt;
      XFASTINT (w->last_modified) = 0;
      if (pos > opoint)
	SET_PT (pos);
      if (n < 0)
	{
	  SET_PT (pos);
	  tem = Fvertical_motion (make_number (ht));
	  if (point > opoint || XFASTINT (tem) < ht)
	    SET_PT (opoint);
	  else
	    Fvertical_motion (make_number (-1));
	}
    }
  else
    Fsignal (Qend_of_buffer, Qnil);
}

scroll_command (n, direction)
     register Lisp_Object n;
     int direction;
{
  register int defalt
    = direction * (window_height (selected_window) - 1
		   - next_screen_context_lines);

  if (NULL (n))
    window_scroll (selected_window, defalt);
  else if (EQ (n, Qminus))
    window_scroll (selected_window, - defalt);
  else
    {
      n = Fprefix_numeric_value (n);
      window_scroll (selected_window, XINT (n) * direction);
    }
}

DEFUN ("scroll-up", Fscroll_up, Sscroll_up, 0, 1, "P",
  "Scroll text of current window upward ARG lines; or near full screen if no ARG.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     Lisp_Object n;
{
  scroll_command (n, 1);
  return Qnil;
}

DEFUN ("scroll-down", Fscroll_down, Sscroll_down, 0, 1, "P",
  "Scroll text of current window downward ARG lines; or near full screen if no ARG.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     Lisp_Object n;
{
  scroll_command (n, -1);
  return Qnil;
}

DEFUN ("scroll-left", Fscroll_left, Sscroll_left, 1, 1, "P",
  "Scroll selected window display ARG columns left.\n\
Default for ARG is window width minus 2.")
  (arg)
     register Lisp_Object arg;
{
  if (NULL (arg))
    XFASTINT (arg) = XFASTINT (XWINDOW (selected_window)->width) - 2;
  else
    arg = Fprefix_numeric_value (arg);

  return Fset_window_hscroll (selected_window,
			      make_number (XINT (XWINDOW (selected_window)->hscroll)
					   + XINT (arg)));
}

DEFUN ("scroll-right", Fscroll_right, Sscroll_right, 1, 1, "P",
  "Scroll selected window display ARG columns right.\n\
Default for ARG is window width minus 2.")
  (arg)
     register Lisp_Object arg;
{
  if (NULL (arg))
    XFASTINT (arg) = XFASTINT (XWINDOW (selected_window)->width) - 2;
  else
    arg = Fprefix_numeric_value (arg);

  return Fset_window_hscroll (selected_window,
			      make_number (XINT (XWINDOW (selected_window)->hscroll)
					   - XINT (arg)));
}

DEFUN ("scroll-other-window", Fscroll_other_window, Sscroll_other_window, 0, 1, "P",
  "Scroll text of next window upward ARG lines; or near full screen if no ARG.\n\
The next window is the one below the current one; or the one at the top\n\
if the current one is at the bottom.\n\
When calling from a program, supply a number as argument or nil.")
  (n)
     register Lisp_Object n;
{
  register Lisp_Object window;
  struct buffer *old = current_buffer;
  register int ht;
  register int opoint = point;
  register struct window *w;

  if (EQ (selected_window, minibuf_window)
      && !NULL (Vminibuf_scroll_window))
    window = Vminibuf_scroll_window;
  else
    window = Fnext_window (selected_window, Qnil);
  CHECK_WINDOW (window, 0);
  ht = window_height (window) - 1;

  if (EQ (window, selected_window))
    error ("There is no other window");

  w = XWINDOW (window);
  Fset_buffer (w->buffer);
  SET_PT (marker_position (w->pointm));

  if (NULL (n))
    window_scroll (window, ht - next_screen_context_lines);
  else if (EQ (n, Qminus))
    window_scroll (window, next_screen_context_lines - ht);
  else
    {
      if (XTYPE (n) == Lisp_Cons)
	n = Fcar (n);
      CHECK_NUMBER (n, 0);
      window_scroll (window, XINT (n));
    }

  Fset_marker (w->pointm, make_number (point), Qnil);
  set_buffer_internal (old);
  SET_PT (opoint);
  return Qnil;
}

DEFUN ("recenter", Frecenter, Srecenter, 0, 1, "P",
  "Center point in window and redisplay screen.  With ARG, put point on line ARG.\n\
The desired position of point is always relative to the current window.\n\
Just C-u as prefix means put point in the center of the screen.\n\
No arg (i.e., it is nil) erases the entire screen and then\n\
redraws with point in the center.")
  (n)
     register Lisp_Object n;
{
  register int ht = window_height (selected_window)
    - !EQ (selected_window, minibuf_window);
  register struct window *w = XWINDOW (selected_window);
  register int opoint = point;

  if (NULL (n))
    {
      extern int screen_garbaged;
      screen_garbaged++;
      XFASTINT (n) = ht / 2;
    }
  else if (XTYPE (n) == Lisp_Cons) /* Just C-u. */
    {
      XFASTINT (n) = ht / 2;
    }
  else
    {
      n = Fprefix_numeric_value (n);
      CHECK_NUMBER (n, 0);
    }

  if (XINT (n) < 0)
    XSETINT (n, XINT (n) + ht);

  XSETINT (n, - XINT (n));

  Fvertical_motion (n);
  Fset_marker (w->start, make_number (point), w->buffer);
  w->start_at_line_beg = Fbolp ();

  SET_PT (opoint);
  w->force_start = Qt;

  return Qnil;
}

DEFUN ("move-to-window-line", Fmove_to_window_line, Smove_to_window_line,
  1, 1, "P",
  "Position point relative to window.\n\
With no argument, position at text at center of window.\n\
An argument specifies screen line; zero means top of window,\n\
negative means relative to bottom of window.")
  (arg)
     register Lisp_Object arg;
{
  register struct window *w = XWINDOW (selected_window);
  register int height = XFASTINT (w->height);
  register int start;

  if (!EQ (selected_window, minibuf_window)) height--;

  if (NULL (arg))
    XFASTINT (arg) = height / 2;
  else
    {
      arg = Fprefix_numeric_value (arg);
      if (XINT (arg) < 0)
	XSETINT (arg, XINT (arg) + height);
    }

  start = marker_position (w->start);
  if (start < BEGV || start > ZV)
    {
      Fvertical_motion (make_number (- height / 2));
      Fset_marker (w->start, make_number (point), w->buffer);
      w->start_at_line_beg = Fbolp ();
      w->force_start = Qt;
    }
  else
    SET_PT (start);

  return Fvertical_motion (arg);
}

struct save_window_data
  {
    int size_from_Lisp_Vector_struct;
    struct Lisp_Vector *next_from_Lisp_Vector_struct;
    Lisp_Object screen_width, screen_height;
    Lisp_Object current_window;
    Lisp_Object current_buffer;
    Lisp_Object minibuf_scroll_window;
    /* A vector, interpreted as a struct saved_window */
    Lisp_Object saved_windows;
  };
#define SAVE_WINDOW_DATA_SIZE 6 /* Arg to Fmake_vector */

/* This is saved as a Lisp_Vector */
struct saved_window
  {
    /* these first two must agree with struct Lisp_Vector in lisp.h */
    int size_from_Lisp_Vector_struct;
    struct Lisp_Vector *next_from_Lisp_Vector_struct;

    Lisp_Object window;
    Lisp_Object buffer, start, pointm, mark;
    Lisp_Object left, top, width, height, hscroll;
    Lisp_Object parent, prev;
    Lisp_Object start_at_line_beg;
  };
#define SAVED_WINDOW_VECTOR_SIZE 13 /* Arg to Fmake_vector */

#define SAVED_WINDOW_N(swv,n) \
  ((struct saved_window *) (XVECTOR ((swv)->contents[(n)])))

DEFUN ("set-window-configuration",
       Fset_window_configuration, Sset_window_configuration,
       1, 1, 0,
       "Restore the configuration of Emacs' windows and buffers to\n\
the state specified by CONFIGURATION.  CONFIGURATION must be a value\n\
retrned by  current-window-configuration  -- see the documentation of that\n\
function for more information.")
     (arg)
     Lisp_Object arg;
{
  register struct window *w;
  register struct save_window_data *data;
  struct Lisp_Vector *saved_windows;
  register struct saved_window *p;
  register Lisp_Object tem;
  Lisp_Object new_current_buffer;
  int k;

  /* Save screen height here so we can go back to it at the end.  */
  int previous_screen_height = screen_height;
  int previous_screen_width = screen_width;
  int screen_size_change = 0;

  while (XTYPE (arg) != Lisp_Window_Configuration)
    {
      /* the function window-configuration-p isn't actually defined
	 at present --- is there a need for it? */
      arg = wrong_type_argument (intern ("window-configuration-p"), arg);
    }

  data = (struct save_window_data *) XVECTOR (arg);
  saved_windows = XVECTOR (data->saved_windows);

  /* Set the screen height to the value it had at save time.  */
  if (XFASTINT (data->screen_height) != screen_height
      || XFASTINT (data->screen_width) != screen_width)
    {
      change_screen_size (data->screen_height, data->screen_width, 0);
      screen_size_change = 1;
    }

  windows_or_buffers_changed++;
  new_current_buffer = data->current_buffer;
  if (NULL (XBUFFER (new_current_buffer)->name))
    new_current_buffer = Qnil;

  for (k = 0; k < saved_windows->size; k++)
    {
      p = SAVED_WINDOW_N (saved_windows, k);
      w = XWINDOW (p->window);
      w->next = Qnil;

      if (!NULL (p->parent))
	w->parent = SAVED_WINDOW_N (saved_windows, XFASTINT (p->parent))->window;
      else
	w->parent = Qnil;

      if (!NULL (p->prev))
	{
	  w->prev = SAVED_WINDOW_N (saved_windows, XFASTINT (p->prev))->window;
	  XWINDOW (w->prev)->next = p->window;
	}
      else
	{
	  w->prev = Qnil;
	  if (!NULL (w->parent))
	    {
	      if (EQ (p->width, XWINDOW (w->parent)->width))
		{
		  XWINDOW (w->parent)->vchild = p->window;
		  XWINDOW (w->parent)->hchild = Qnil;
		}
	      else
		{
		  XWINDOW (w->parent)->hchild = p->window;
		  XWINDOW (w->parent)->vchild = Qnil;
		}
	    }
	}
      w->left = p->left;
      w->top = p->top;
      w->width = p->width;
      w->height = p->height;
      w->hscroll = p->hscroll;
      XFASTINT (w->last_modified) = 0;

      /* Reinstall the saved buffer and pointers into it.  */
      if (NULL (p->buffer))
	w->buffer = p->buffer;
      else
	{
	  if (!NULL (XBUFFER (p->buffer)->name))
	    /* If saved buffer is alive, install it.  */
	    {
	      w->buffer = p->buffer;
	      w->start_at_line_beg = p->start_at_line_beg;
	      set_marker_restricted (w->start,
				     Fmarker_position (p->start), w->buffer);
	      set_marker_restricted (w->pointm,
				     Fmarker_position (p->pointm), w->buffer);
	      Fset_marker (XBUFFER (w->buffer)->mark,
			   Fmarker_position (p->mark), w->buffer);

	      if (!EQ (p->buffer, new_current_buffer) &&
		  XBUFFER (p->buffer) == current_buffer)
		Fgoto_char (w->pointm);
	    }
	  else if (NULL (XBUFFER (w->buffer)->name))
	    /* Else if window's old buffer is dead too, get a live one.  */
	    {
	      w->buffer = Fcdr (Fcar (Vbuffer_alist));
	      /* Set window markers at start of buffer.
		 Rely on set_marker_restricted to put them
		 within the restriction.  */
	      set_marker_restricted (w->start, make_number (0), w->buffer);
	      set_marker_restricted (w->pointm, make_number (0), w->buffer);
	      w->start_at_line_beg = Qt;
	    }
	  else
	    /* Keeping window's old buffer; make sure the markers are real.  */
	    /* Else if window's old buffer is dead too, get a live one.  */
	    {
	      /* Set window markers at start of buffer.
		 Rely on set_marker_restricted to put them within the restriction.  */
	      if (XMARKER (w->start)->buffer == 0)
		set_marker_restricted (w->start, make_number (0), w->buffer);
	      if (XMARKER (w->pointm)->buffer == 0)
		set_marker_restricted (w->pointm,
				       make_number (BUF_PT (XBUFFER (w->buffer))),
				       w->buffer);
	      w->start_at_line_beg = Qt;
	    }
	}
    }

  /* Set the screen height to the value it had before this function.  */
  if (screen_size_change)
    change_screen_size (previous_screen_height, previous_screen_width, 0);

  Fselect_window (data->current_window);
  if (!NULL (new_current_buffer))
    Fset_buffer (new_current_buffer);
  else
    Fset_buffer (XWINDOW (selected_window)->buffer);
  Vminibuf_scroll_window = data->minibuf_scroll_window;
  return (Qnil);
}


static int
count_windows (window)
     register struct window *window;
{
  register int count = 1;
  if (!NULL (window->next))
    count += count_windows (XWINDOW (window->next));
  if (!NULL (window->vchild))
    count += count_windows (XWINDOW (window->vchild));
  if (!NULL (window->hchild))
    count += count_windows (XWINDOW (window->hchild));
  return count;
}

DEFUN ("current-window-configuration",
	Fcurrent_window_configuration, Scurrent_window_configuration, 0, 0, 0,
       "Return an object representing Emacs' current window configuration,\n\
namely the number of windows, their sizes and current buffers, and for\n\
each displayed buffer, where display starts, and the positions of\n\
point and mark.  An exception is made for point in (current-buffer) --\n\
its value is -not- saved.")
  ()
{
  register Lisp_Object tem;
  register int n_windows;
  register struct save_window_data *data;
  register int i;

  n_windows = count_windows (XWINDOW (XWINDOW (minibuf_window)->prev));
  data = (struct save_window_data *)
           XVECTOR (Fmake_vector (make_number (SAVE_WINDOW_DATA_SIZE),
				  Qnil));
  XFASTINT (data->screen_width) = screen_width;
  XFASTINT (data->screen_height) = screen_height;
  data->current_window = selected_window;
  XSET (data->current_buffer, Lisp_Buffer, current_buffer);
  data->minibuf_scroll_window = Vminibuf_scroll_window;
  tem = Fmake_vector (make_number (n_windows), Qnil);
  data->saved_windows = tem;
  for (i = 0; i < n_windows; i++)
    XVECTOR (tem)->contents[i]
      = Fmake_vector (make_number (SAVED_WINDOW_VECTOR_SIZE), Qnil);
  save_window_save (XWINDOW (minibuf_window)->prev,
		    XVECTOR (tem),
		    0, n_windows);
  XSET (tem, Lisp_Window_Configuration, data);
  return (tem);
}

static int
save_window_save (window, vector, i, maxwindow)
     Lisp_Object window;
     struct Lisp_Vector *vector;
     int i;
     int maxwindow;
{
  register struct saved_window *p;
  register struct window *w;
  register Lisp_Object tem;

  for (;!NULL (window); window = w->next)
    {
      /* If you get a crash here, you may be seeing a very weird bug.
	 When it happened to me, it seems that count_windows returned
	 a value that was too small--only two, when there were two
	 visible windows, a parent, and the minibuffer (inactive).
	 If this starts happening for you, please run under a debugger
	 with a breakpoint at the abort, so that you can at least try calling
	 count_windows again to see if it will lose again.
	 If it does, you can find the bug.  */
      if (i == maxwindow)
	abort ();

      p = SAVED_WINDOW_N (vector, i);
      w = XWINDOW (window);

      XFASTINT (w->temslot) = i++;

      p->window = window;
      p->buffer = w->buffer;
      p->left = w->left;
      p->top = w->top;
      p->width = w->width;
      p->height = w->height;
      p->hscroll = w->hscroll;
      if (!NULL (w->buffer))
	{
	  if (EQ (window, selected_window)
	      && XBUFFER (w->buffer) == current_buffer)
	    p->pointm = Fpoint_marker ();
	  else
	    p->pointm = Fcopy_marker (w->pointm);

	  p->start = Fcopy_marker (w->start);
	  p->start_at_line_beg = w->start_at_line_beg;

	  tem = XBUFFER (w->buffer)->mark;
	  p->mark = Fcopy_marker (tem);
	}
      else
	{
	  p->pointm = Qnil;
	  p->start = Qnil;
	  p->mark = Qnil;
	  p->start_at_line_beg = Qnil;
	}

      if (NULL (w->parent))
	p->parent = Qnil;
      else
	p->parent = XWINDOW (w->parent)->temslot;

      if (NULL (w->prev))
	p->prev = Qnil;
      else
	p->prev = XWINDOW (w->prev)->temslot;

      if (!NULL (w->vchild))
	i = save_window_save (w->vchild, vector, i, maxwindow);
      if (!NULL (w->hchild))
	i = save_window_save (w->hchild, vector, i, maxwindow);
    }

  return i;
}

DEFUN ("save-window-excursion", Fsave_window_excursion, Ssave_window_excursion,
  0, UNEVALLED, 0,
  "Execute body, preserving window sizes and contents.\n\
Restores which buffer appears in which window, where display starts,\n\
as well as the current buffer.\n\
Does not restore the value of point in current buffer.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  register int count = specpdl_ptr - specpdl;

  record_unwind_protect (Fset_window_configuration,
			 Fcurrent_window_configuration ());
  val = Fprogn (args);
  unbind_to (count);
  return val;
}

init_window_once ()
{
  extern Lisp_Object get_minibuffer ();
  register Lisp_Object root_window;

  root_window = make_window (0);
  minibuf_window = make_window (0);

  XWINDOW (root_window)->next = minibuf_window;
  XWINDOW (minibuf_window)->prev = root_window;

  /* These values 9 and 10 are arbitrary,
     just so that there is "something there."
     Correct values are put in in init_xdisp */

  XFASTINT (XWINDOW (root_window)->width) = 10;
  XFASTINT (XWINDOW (minibuf_window)->width) = 10;

  XFASTINT (XWINDOW (root_window)->height) = 9;
  XFASTINT (XWINDOW (minibuf_window)->top) = 9;
  XFASTINT (XWINDOW (minibuf_window)->height) = 1;

  Fset_window_buffer (root_window, Fcurrent_buffer ());
  Fset_window_buffer (minibuf_window, get_minibuffer (0));

  selected_window = root_window;
}

syms_of_window ()
{
  Qwindowp = intern ("windowp");
  staticpro (&Qwindowp);

  /* Make sure all windows get marked */
  staticpro (&minibuf_window);

  DEFVAR_LISP ("temp-buffer-show-hook", &Vtemp_buffer_show_hook,
    "Non-nil means call as function to display a help buffer.\n\
Used by with-output-to-temp-buffer.");
  Vtemp_buffer_show_hook = Qnil;

  DEFVAR_LISP ("minibuffer-scroll-window", &Vminibuf_scroll_window,
    "Non-nil means it is the window that C-M-v in minibuffer should scroll.");
  Vminibuf_scroll_window = Qnil;

  DEFVAR_BOOL ("pop-up-windows", &pop_up_windows,
    "*Non-nil means display-buffer should make new windows.");
  pop_up_windows = 1;

  DEFVAR_INT ("next-screen-context-lines", &next_screen_context_lines,
    "*Number of lines of continuity when scrolling by screenfuls.");
  next_screen_context_lines = 2;

  DEFVAR_INT ("split-height-threshold", &split_height_threshold,
    "*display-buffer would prefer to split the largest window if this large.\n\
If there is only one window, it is split regardless of this value.");
  split_height_threshold = 500;

  DEFVAR_INT ("window-min-height", &window_min_height,
    "*Delete any window less than this tall (including its mode line).");
  window_min_height = 4;

  DEFVAR_INT ("window-min-width", &window_min_width,
    "*Delete any window less than this wide.");
  window_min_width = 10;

  defsubr (&Sselected_window);
  defsubr (&Sminibuffer_window);
  defsubr (&Swindowp);
  defsubr (&Spos_visible_in_window_p);
  defsubr (&Swindow_buffer);
  defsubr (&Swindow_height);
  defsubr (&Swindow_width);
  defsubr (&Swindow_hscroll);
  defsubr (&Sset_window_hscroll);
  defsubr (&Swindow_edges);
  defsubr (&Swindow_point);
  defsubr (&Swindow_start);
  defsubr (&Sset_window_point);
  defsubr (&Sset_window_start);
  defsubr (&Snext_window);
  defsubr (&Sprevious_window);
  defsubr (&Sother_window);
  defsubr (&Sget_lru_window);
  defsubr (&Sget_largest_window);
  defsubr (&Sget_buffer_window);
  defsubr (&Sdelete_other_windows);
  defsubr (&Sdelete_windows_on);
  defsubr (&Sreplace_buffer_in_windows);
  defsubr (&Sdelete_window);
  defsubr (&Sset_window_buffer);
  defsubr (&Sselect_window);
  defsubr (&Sdisplay_buffer);
  defsubr (&Ssplit_window);
  defsubr (&Senlarge_window);
  defsubr (&Sshrink_window);
  defsubr (&Sscroll_up);
  defsubr (&Sscroll_down);
  defsubr (&Sscroll_left);
  defsubr (&Sscroll_right);
  defsubr (&Sscroll_other_window);
  defsubr (&Srecenter);
  defsubr (&Smove_to_window_line);
  defsubr (&Sset_window_configuration);
  defsubr (&Scurrent_window_configuration);
  defsubr (&Ssave_window_excursion);
}

keys_of_window ()
{
  ndefkey (Vctl_x_map, '1', "delete-other-windows");
  ndefkey (Vctl_x_map, '2', "split-window");
  ndefkey (Vctl_x_map, '0', "delete-window");
  ndefkey (Vctl_x_map, 'o', "other-window");
  ndefkey (Vctl_x_map, '^', "enlarge-window");
  ndefkey (Vctl_x_map, '<', "scroll-left");
  ndefkey (Vctl_x_map, '>', "scroll-right");

  ndefkey (Vglobal_map, Ctl ('V'), "scroll-up");
  ndefkey (Vesc_map, Ctl ('V'), "scroll-other-window");
  ndefkey (Vesc_map, 'v', "scroll-down");

  ndefkey (Vglobal_map, Ctl('L'), "recenter");
  ndefkey (Vesc_map, 'r', "move-to-window-line");
}
