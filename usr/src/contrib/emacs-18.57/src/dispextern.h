/* Interface definitions for display code.
   Copyright (C) 1985, 1990 Free Software Foundation, Inc.

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


/* Nonzero means do not assume anything about current
 contents of actual terminal screen */

extern int screen_garbaged;

/* Desired terminal cursor position (to show position of point),
 origin zero.  */

extern int cursor_hpos, cursor_vpos;

/* Nonzero means last display completed
   and cursor is really at cursor_hpos, cursor_vpos.
   Zero means it was preempted. */

extern int display_completed;

struct matrix
{
  /* Height of this matrix.  */
  int height;
  /* Width of this matrix.  */
  int width;
  /* Vector of used widths of lines, indexed by vertical position.  */
  int *used;
  /* Vector of line contents.
     m->contents[V][H] is the character at position V, H.
     Note that ->contents[...][screen_width] is always 0
     and so is ->contents[...][-1].  */
  unsigned char **contents;
  /* Long vector from which the line contents are taken.  */
  unsigned char *total_contents;
  /* Vector indicating, for each line, whether it is highlighted.  */
  char *highlight;
  /* Vector indicating, for each line, whether its contents mean anything.  */
  char *enable;
};

/* Current screen contents.  */
extern struct matrix *current_screen;
/* Screen contents to be displayed.  */
extern struct matrix *new_screen;
/* Temporary buffer for screen contents.  */
extern struct matrix *temp_screen;

/* Get ready to display on screen line VPOS at column HPOS
   and return the string where the text of that line is stored.  */

unsigned char *get_display_line ();

/* Buffer used by `message' for formatting a message, and by print.c.  */
extern char *message_buf;

/* All costs measured in characters.
   So no cost can exceed the area of a screen, measured in characters.
   This should not be more than million.
   Meanwhile, we can add lots of millions together without overflow.  */

#define INFINITY 1000000
