/* Interface definitions for display code.
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


/* Nonzero means do not assume anything about current
 contents of actual terminal screen */

extern int screen_garbaged;

/* Desired terminal cursor position (to show position of dot),
 origin one */

extern int cursX, cursY;

/* Nonzero means last display completed and cursor is really at cursX, cursY.
 Zero means it was preempted. */

extern int display_completed;

/* Display line structure.
This structure records the contents of a line
either as already on the display
or as we desire to have it on the display.

PhysScreen is a vector of pointers to lines
 describing the actual contents of the screen.
DesiredScreen is a vector of pointers to lines
 describing what we want to put on the screen.
 These were made from the buffers being displayed
 by the file window.c

The code in this file compares those two vectors of display lines
 and performs the updating.

As display lines are used only to go in those vectors,
 the most display lines that ever ought to exist is
 twice the maximum screen size.  That many are created
 initially and put in a pool.  If the pool is ever empty
 and a line is needed, that indicates a bug.
*/

struct display_line
  {
    struct display_line *next;	/* Chain for free lines. */
    short   length;		/* the number of valid characters in body */
    char    highlighted;	/* 1 means write this line in standout mode */
    char    physical;		/* Mark bit for gc'ing, in update_screen */
    char    body[MScreenWidth + 4];	/* the actual text of the line */
};

/* Allocate a line structure for screen line `vpos' (origin 0)
 to start output in it at `hpos' (origin 0, may be negative) */

struct display_line *get_display_line (), *new_display_line ();
