/* Window definitions for GNU Emacs.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


/* Windows are allocated as if they were vectors, but then the
Lisp data type is changed to Lisp_Window.  They are garbage
collected along with the vectors.

All windows in use are arranged into a tree, with pointers up and down.

Windows that are leaves of the tree are actually displayed
and show the contents of buffers.  Windows that are not leaves
are used for representing the way groups of leaf windows are
arranged on the screen.  Leaf windows never become non-leaves.
They are deleted only by calling delete-window on them (but
this can be done implicitly).  Combination windows can be created
and deleted at any time.

A leaf window has a non-nil buffer field, and also
 has markers in its start and pointm fields.  Non-leaf windows
 have nil in these fields.

Non-leaf windows are either vertical or horizontal combinations.

A vertical combination window has children that are arranged
one above the next.  Its vchild field points to the uppermost child.
The parent field of each of the children points to the vertical
combination window.  The next field of each child points to the
child below it, or is nil for the lowest child.  The prev field
or each child points to the child above it, or is nil for the highest child.

A horizontal combination window has children that are side by side.
Its hchild field points to the leftmost child.  In each child
the next field points to the child to the right and the prev field
points to the child to the left.

The children of a vertical combination window may be leaf windows
or horizontal combination windows.  The children of a horizontal
combination window may be leaf windows or vertical combination windows.

At the top of the tree are two windows which have nil as parent.
The second of these is minibuf_window.  The first one manages all
the screen area that is not minibuffer, and is called the root window.
Different windows can be the root at different times;
initially the root window is a leaf window, but if more windows
are created then that leaf window ceases to be root and a newly
made combination window becomes root instead.

In any case, prev of the minibuf window is the root window and
next of the root window is the minibuf window.  To find the
root window at any time, do XWINDOW (minibuf_window)->prev.

*/

struct window
  {
    /* The first two fields are really the header of a vector */
    /* The window code does not refer to them.  */
    int size;
    struct Lisp_Vector *vec_next;
    /* Following child (to right or down) at same level of tree */
    Lisp_Object next;
    /* Preceding child (to left or up) at same level of tree */
    Lisp_Object prev;
    /* First child of this window. */
    /* vchild is used if this is a vertical combination,
       hchild if this is a horizontal combination. */
    Lisp_Object hchild, vchild;
    /* The window this one is a child of. */
    Lisp_Object parent;
    /* The upper left corner coordinates of this window,
       as integers relative to upper left corner of screen = 0, 0 */
    Lisp_Object left;
    Lisp_Object top;
    /* The size of the window */
    Lisp_Object height;
    Lisp_Object width;
    /* The buffer displayed in this window */
    /* Of the fields vchild, hchild and buffer, only one is non-nil.  */
    Lisp_Object buffer;
    /* A marker pointing to where in the text to start displaying */
    Lisp_Object start;
    /* A marker pointing to where in the text point is in this window,
       used only when the window is not selected.
       This exists so that when multiple windows show one buffer
       each one can have its own value of point.  */
    Lisp_Object pointm;
    /* Non-nil means next redisplay must use the value of start
       set up for it in advance.  Set by scrolling commands.  */
    Lisp_Object force_start;
    /* Number of columns display within the window is scrolled to the left.  */
    Lisp_Object hscroll;
    /* Number saying how recently window was selected */
    Lisp_Object use_time;
    /* Unique number of window assigned when it was created */
    Lisp_Object sequence_number;
    /* No permanent meaning; used by save-window-excursion's bookkeeping */
    Lisp_Object temslot;
    /* text.modified of displayed buffer as of last time display completed */
    Lisp_Object last_modified;
    /* Value of point at that time */
    Lisp_Object last_point;
/* The rest are currently not used or only half used */
    /* Screen coords of point at that time */
    Lisp_Object last_point_x;
    Lisp_Object last_point_y;
    /* Screen coords of mark as of last time display completed */
    /* May be nil if mark does not exist or was not on screen */
    Lisp_Object last_mark_x;
    Lisp_Object last_mark_y;
    /* Number of characters in buffer past bottom of window,
       as of last redisplay that finished. */
    Lisp_Object window_end_pos;
    /* t if window_end_pos is truly valid.
       This is nil if nontrivial redisplay is preempted
       since in that case the screen image that window_end_pos
       did not get onto the screen.  */
    Lisp_Object window_end_valid;
    /* Vertical position (relative to window top) of that buffer position
       of the first of those characters */
    Lisp_Object window_end_vpos;
    /* Non-nil means must regenerate mode line of this window */
    Lisp_Object update_mode_line;
    /* Non-nil means current value of `start'
       was the beginning of a line when it was chosen.  */
    Lisp_Object start_at_line_beg;
  };

/* This is the window which displays the minibuffer.
   It is always the same window.  */

extern Lisp_Object minibuf_window;

/* This is the window in which the terminal's cursor should
   be left when nothing is being done with it.  This must
   always be a leaf window, and its buffer is selected by
   the top level editing loop at the end of each command.  */

extern Lisp_Object selected_window;

/* Non-nil => window to for C-M-v to scroll
   when the minibuffer is selected.  */
extern Lisp_Object Vminibuf_scroll_window;

extern Lisp_Object Fnext_window ();
extern Lisp_Object Fselect_window ();
extern Lisp_Object Fdisplay_buffer ();
extern Lisp_Object Fset_window_buffer ();

/* Prompt to display in front of the minibuffer contents.  */
extern char *minibuf_prompt;

/* Message to display instead of minibuffer contents.  */
extern char *echo_area_contents;

/* Depth in minibuffer invocations.  */
extern int minibuf_level;

/* Nonzero means redisplay all mode lines.  */
extern int update_mode_lines;

/* Minimum value of GPT since last redisplay that finished.  */

extern int beg_unchanged;

/* Minimum value of Z-GPT since last redisplay that finished.  */

extern int end_unchanged;

/* MODIFF as of last redisplay that finished;
   if it matches MODIFF, beg_unchanged and end_unchanged
   contain no useful information */
extern int unchanged_modified;

/* Nonzero if BEGV-BEG or Z-ZV of current buffer has changed
   since last redisplay that finished */
extern int clip_changed;

/* Nonzero if window sizes or contents have changed
   since last redisplay that finished */
extern int windows_or_buffers_changed;

/* Number of windows displaying the selected buffer.
   Normally this is 1, but it can be more.  */
extern int buffer_shared;
