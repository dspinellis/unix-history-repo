/* Window definitions for GNU Emacs.
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
    /* Number of columns display within the window is scrolled to the left.
       Currently always zero since this is not implemented.  */
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
    /* Vertical position (relative to window top) of that buffer position
       of the first of those characters */
    Lisp_Object window_end_vpos;
    /* Non-nil means must regenerate mode line of this window */
    Lisp_Object redo_mode_line;
  };

/* This is the window which displays the minibuffer.
It is always the same window.  */

extern Lisp_Object minibuf_window;

/* This is the window in which the terminal's cursor should
 be left when nothing is being done with it.  This must
 always be a leaf window, and its buffer is selected by
 the top level editing loop at the end of each command.  */

extern Lisp_Object selected_window;

#define new_windows 1

Lisp_Object Fnext_window ();
Lisp_Object Fselect_window ();
Lisp_Object Fdisplay_buffer ();
Lisp_Object Fshow_buffer ();

extern char *minibuf_prompt;	/* Prompt to display in front of the minibuffer contents */

extern char *minibuf_message;	/* Message to display instead of minibuffer contents */
				/* This is what the functions error and message make, */
				/* and command echoing uses it as well. */
				/* It overrides the minibuf_prompt as well as the buffer */

extern int RecurseDepth;	/* Depth in recursive edits */

extern int MinibufDepth;	/* Depth in minibuffer invocations */

extern int RedoModes;		/* true iff we should redraw the mode lines
				   on the next redisplay */

/* Minimum value of bf_s1 since last redisplay that finished.
 Valid for current buffer unless Cant1WinOpt is nonzero. */

extern int beg_unchanged;

/* Minimum value of bf_s2 since last redisplay that finished.
 Valid for current buffer unless Cant1WinOpt is nonzero. */

extern int end_unchanged;

/* bf_modified as of last redisplay that finished;
 if it matches bf_modified, beg_unchanged and end_unchanged
 contain no useful information */
extern int unchanged_modified;

/* Nonzero if head_clip or tail_clip of current buffer has changed
 since last redisplay that finished */
extern int clip_changed;

/* Nonzero if window sizes or contents have changed
 since last redisplay that finished */
extern int windows_or_buffers_changed;
