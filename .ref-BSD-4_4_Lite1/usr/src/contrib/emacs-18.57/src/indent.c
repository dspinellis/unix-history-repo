/* Indentation functions.
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


#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "indent.h"
#include "window.h"
#include "termchar.h"
#include "termopts.h"

#define CR '\015'

/* Indentation can insert tabs if this is non-zero;
   otherwise always uses spaces */
int indent_tabs_mode;

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* These three values memoize the current column to avoid recalculation */
/* Some things in set last_known_column_point to -1
  to mark the memoized value as invalid */
/* Last value returned by current_column */
int last_known_column;
/* Value of point when current_column was called */
int last_known_column_point;
/* Value of MODIFF when current_column was called */
int last_known_column_modified;

extern int minibuf_prompt_width;

DEFUN ("current-column", Fcurrent_column, Scurrent_column, 0, 0, 0,
  "Return the horizontal position of point.  Beginning of line is column 0.\n\
This is calculated by adding together the widths of all the displayed\n\
representations of the character between the start of the previous line\n\
and point.  (eg control characters will have a width of 2 or 4, tabs\n\
will have a variable width)\n\
Ignores finite width of screen, which means that this function may return\n\
values greater than (screen-width).\n\
Whether the line is visible (if `selective-display' is t) has no effect.")
  ()
{
  Lisp_Object temp;
  XFASTINT (temp) = current_column ();
  return temp;
}

int
current_column ()
{
  register int col;
  register unsigned char *ptr, *stop, c;
  register int tab_seen;
  register int post_tab;
  register int tab_width = XINT (current_buffer->tab_width);
  int ctl_arrow = !NULL (current_buffer->ctl_arrow);

  if (point == last_known_column_point
      && MODIFF == last_known_column_modified)
    return last_known_column;

  /* Make a pointer for decrementing through the chars before point.  */
  ptr = &FETCH_CHAR (point - 1) + 1;
  /* Make a pointer to where consecutive chars leave off,
     going backwards from point.  */
  if (point == BEGV)
    stop = ptr;
  else if (point <= GPT || BEGV > GPT)
    stop = BEGV_ADDR;
  else
    stop = GAP_END_ADDR;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  col = 0, tab_seen = 0, post_tab = 0;

  while (1)
    {
      if (ptr == stop)
	{
	  /* We stopped either for the beginning of the buffer
	     or for the gap.  */
	  if (ptr == BEGV_ADDR)
	    break;
	  /* It was the gap.  Jump back over it.  */
	  stop = BEGV_ADDR;
	  ptr = GPT_ADDR;
	  /* Check whether that brings us to beginning of buffer.  */
	  if (BEGV >= GPT) break;
	}

      c = *--ptr;
      if (c >= 040 && c < 0177)
	{
	  col++;
	}
      else if (c == '\n')
	break;
      else if (c == '\r' && EQ (current_buffer->selective_display, Qt))
	break;
      else if (c == '\t')
	{
	  if (tab_seen)
	    col = ((col + tab_width) / tab_width) * tab_width;

	  post_tab += col;
	  col = 0;
	  tab_seen = 1;
	}
      else
	col += (ctl_arrow && c < 0200) ? 2 : 4;
    }

  if (tab_seen)
    {
      col = ((col + tab_width) / tab_width) * tab_width;
      col += post_tab;
    }

  last_known_column = col;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;

  return col;
}

ToCol (col)
     int col;
{
  register int fromcol = current_column ();
  register int n;
  register int tab_width = XINT (current_buffer->tab_width);

  if (fromcol > col)
    return;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  if (indent_tabs_mode)
    {
      n = col / tab_width - fromcol / tab_width;
      if (n)
	{
	  while (n-- > 0)
	    insert ("\t", 1);

	  fromcol = (col / tab_width) * tab_width;
	}
    }

  while (fromcol < col)
    {
      insert ("        ", min (8, col - fromcol));
      fromcol += min (8, col - fromcol);
    }

  last_known_column = col;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;
}

DEFUN ("indent-to", Findent_to, Sindent_to, 1, 2, "NIndent to column: ",
  "Indent from point with tabs and spaces until COLUMN is reached.\n\
Always do at least MIN spaces even if that goes past COLUMN;\n\
by default, MIN is zero.")
  (col, minimum)
     Lisp_Object col, minimum;
{
  int mincol;
  register int fromcol;
  register int tab_width = XINT (current_buffer->tab_width);

  CHECK_NUMBER (col, 0);
  if (NULL (minimum))
    XFASTINT (minimum) = 0;
  CHECK_NUMBER (minimum, 1);

  fromcol = current_column ();
  mincol = fromcol + XINT (minimum);
  if (mincol < XINT (col)) mincol = XINT (col);

  if (fromcol == mincol)
    return make_number (fromcol);

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  if (indent_tabs_mode)
    {
      Lisp_Object n;
      XFASTINT (n) = mincol / tab_width - fromcol / tab_width;
      if (XFASTINT (n) != 0)
	{
	  Finsert_char (make_number ('\t'), n);

	  fromcol = (mincol / tab_width) * tab_width;
	}
    }

  XFASTINT (col) = mincol - fromcol;
  Finsert_char (make_number (' '), col);

  last_known_column = mincol;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;

  XSETINT (col, mincol);
  return col;
}

DEFUN ("current-indentation", Fcurrent_indentation, Scurrent_indentation,
  0, 0, 0,
  "Return the indentation of the current line.\n\
This is the horizontal position of the character\n\
following any initial whitespace.")
  ()
{
  Lisp_Object val;

  XFASTINT (val) = position_indentation (find_next_newline (point, -1));
  return val;
}

position_indentation (pos)
     register int pos;
{
  register int column = 0;
  register int tab_width = XINT (current_buffer->tab_width);
  register unsigned char *p;
  register unsigned char *stop;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;

  stop = &FETCH_CHAR (BufferSafeCeiling (pos)) + 1;
  p = &FETCH_CHAR (pos);
  while (1)
    {
      while (p == stop)
	{
	  if (pos == ZV)
	    return column;
	  pos += p - &FETCH_CHAR (pos);
	  p = &FETCH_CHAR (pos);
	  stop = &FETCH_CHAR (BufferSafeCeiling (pos)) + 1;
	}
      switch (*p++)
	{
	case ' ':
	  column++;
	  break;
	case '\t':
	  column += tab_width - column % tab_width;
	  break;
	default:
	  return column;
	}
    }
}

DEFUN ("move-to-column", Fmove_to_column, Smove_to_column, 1, 1, 0,
  "Move point to column COLUMN in the current line.\n\
COLUMN is calculated by adding together the widths of all the displayed\n\
representations of the character between the start of the previous line\n\
and point.  (eg control characters will have a width of 2 or 4, tabs\n\
will have a variable width)\n\
Ignores finite width of screen, which means that this function may be\n\
passed values greater than (screen-width)")
  (column)
     Lisp_Object column;
{
  register int pos = point;
  register int col = current_column ();
  register int goal;
  register int end = ZV;
  register int tab_width = XINT (current_buffer->tab_width);
  register int ctl_arrow = !NULL (current_buffer->ctl_arrow);

  Lisp_Object val;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;
  CHECK_NUMBER (column, 0);
  goal = XINT (column);
  if (col > goal)
    {
      pos = find_next_newline (pos, -1);
      col = 0;
    }

  while (col < goal && pos < end)
    {
      int c = FETCH_CHAR (pos);
      if (c == '\n')
	break;
      if (c == '\r' && EQ (current_buffer->selective_display, Qt))
	break;
      pos++;
      col++;
      if (c == '\t')
	{
	  col += tab_width - 1;
	  col = col / tab_width * tab_width;
	}
      else if (ctl_arrow && (c < 040 || c == 0177))
        col++;
      else if (c < 040 || c >= 0177)
        col += 3;
    }

  SET_PT (pos);

  last_known_column = col;
  last_known_column_point = point;
  last_known_column_modified = MODIFF;

  XFASTINT (val) = col;
  return val;
}

struct position val_compute_motion;

struct position *
compute_motion (from, fromvpos, fromhpos, to, tovpos, tohpos, width, hscroll, tab_offset)
     int from, fromvpos, fromhpos, to, tovpos, tohpos;
     register int width;
     int hscroll, tab_offset;
{
/* Note that `cpos' is CURRENT_VPOS << SHORTBITS + CURRENT_HPOS,
   and that CURRENT_HPOS may be negative.  Use these macros
   to extract the hpos or the vpos from cpos or anything like it.
 */
#ifndef SHORT_CAST_BUG
#define HPOS(VAR) (short) (VAR)
#else
#define HPOS(VAR) (((VAR) & (1 << (SHORTBITS - 1)) \
		    ? ~((1 << SHORTBITS) - 1) : 0) \
		   | (VAR) & ((1 << SHORTBITS) - 1))
/* #define HPOS(VAR) (((VAR) & 0x8000 ? 0xffff0000 : 0) | ((VAR) & 0xffff)) */
#endif /* SHORT_CAST_BUG */

#define VPOS(VAR) (((VAR) >> SHORTBITS) + (HPOS (VAR) < 0))


#ifndef TAHOE_REGISTER_BUG
  register
#endif /* TAHOE_REGISTER_BUG */
    int cpos = fromhpos + (fromvpos << SHORTBITS);
  register int target = tohpos + (tovpos << SHORTBITS);
  register int pos;
  register int c;
  register int tab_width = XFASTINT (current_buffer->tab_width);
  register int ctl_arrow = !NULL (current_buffer->ctl_arrow);
  int selective
    = XTYPE (current_buffer->selective_display) == Lisp_Int
      ? XINT (current_buffer->selective_display)
	: !NULL (current_buffer->selective_display) ? -1 : 0;
  int prevpos;

  if (tab_width <= 0 || tab_width > 20) tab_width = 8;
  for (pos = from; pos < to && cpos < target; pos++)
    {
      prevpos = cpos;
      c = FETCH_CHAR (pos);
      if (c >= 040 && c < 0177)
	cpos++;
      else if (c == '\t')
	{
	  cpos += tab_width
	    - HPOS (cpos + tab_offset + hscroll - (hscroll > 0)
		    /* Add tab_width here to make sure positive.
		       cpos can be negative after continuation
		       but can't be less than -tab_width.  */
		    + tab_width)
	      % tab_width;
	}
      else if (c == '\n')
	{
	  if (selective > 0 && position_indentation (pos + 1) >= selective)
	    {
	      /* Skip any number of invisible lines all at once */
	      do
		{
		  while (++pos < to && FETCH_CHAR(pos) != '\n');
		}
	      while (selective > 0 && position_indentation (pos + 1) >= selective);
	      pos--;
	      /* Allow for the " ..." that is displayed for them. */
	      if (!NULL (current_buffer->selective_display_ellipses))
		{
		  cpos += 4;
		  if (HPOS (cpos) >= width)
		    cpos -= HPOS (cpos) - width;
		}
	    }
	  else
	    cpos += (1 << SHORTBITS) - HPOS (cpos);
	  cpos -= hscroll;
	  if (hscroll > 0) cpos++; /* Count the ! on column 0 */
	  tab_offset = 0;
	}
      else if (c == CR && selective < 0)
	{
	  /* In selective display mode,
	     everything from a ^M to the end of the line is invisible */
	  while (pos < to && FETCH_CHAR(pos) != '\n') pos++;
	  pos--;
	  /* Allow for the " ..." that is displayed for them. */
	  if (!NULL (current_buffer->selective_display_ellipses))
	    {
	      cpos += 4;
	      if (HPOS (cpos) >= width)
		cpos -= HPOS (cpos) - width;
	    }
	}
      else
	cpos += (ctl_arrow && c < 0200) ? 2 : 4;

      if (HPOS (cpos) >= width
	  && (HPOS (cpos) > width
	      || (pos < ZV - 1
		  && FETCH_CHAR (pos + 1) != '\n')))
	{
	  if (cpos >= target)
	    break;
	  if (hscroll
	      || (truncate_partial_width_windows
		  && width + 1 < screen_width)
	      || !NULL (current_buffer->truncate_lines))
	    {
	      while (pos < to && FETCH_CHAR(pos) != '\n') pos++;
	      pos--;
	    }
	  else
	    {
	      cpos += (1 << SHORTBITS) - width;
	      tab_offset += width;
	    }

	}
    }

  val_compute_motion.bufpos = pos;
  val_compute_motion.hpos = HPOS (cpos);
  val_compute_motion.vpos = VPOS (cpos);
  val_compute_motion.prevhpos = HPOS (prevpos);

  /* Nonzero if have just continued a line */
  val_compute_motion.contin
    = pos != from
      && (val_compute_motion.vpos != VPOS (prevpos))
      && c != '\n';

  return &val_compute_motion;
}
#undef HPOS
#undef VPOS


pos_tab_offset (w, pos)
     struct window *w;
     register int pos;
{
  int opoint = point;
  int col;

  if (pos == BEGV || FETCH_CHAR (pos - 1) == '\n')
    return 0;
  SET_PT (pos);
  col = current_column ();
  SET_PT (opoint);
  return col - (col % (XFASTINT (w->width) - 1));
}

/* start_hpos is the hpos of the first character of the buffer:
   zero except for the minibuffer window,
   where it is the width of the prompt.  */

struct position val_vmotion;

struct position *
vmotion (from, vtarget, width, hscroll, window)
     register int from, vtarget, width;
     int hscroll;
     Lisp_Object window;
{
  struct position pos;
  /* vpos is cumulative vertical position, changed as from is changed */
  register int vpos = 0;
  register int prevline;
  register int first;
  int lmargin = hscroll > 0 ? 1 - hscroll : 0;
  int selective
    = XTYPE (current_buffer->selective_display) == Lisp_Int
      ? XINT (current_buffer->selective_display)
	: !NULL (current_buffer->selective_display) ? -1 : 0;
  int start_hpos = (EQ (window, minibuf_window) ? minibuf_prompt_width : 0);

 retry:
  if (vtarget > vpos)
    {
      /* Moving downward is simple, but must calculate from beg of line 
	 to determine hpos of starting point */
      if (from > BEGV && FETCH_CHAR (from - 1) != '\n')
	{
	  prevline = find_next_newline (from, -1);
	  while (selective > 0
		 && prevline > BEGV
		 && position_indentation (prevline) >= selective)
	    prevline = find_next_newline (prevline - 1, -1);
	  pos = *compute_motion (prevline, 0,
				 lmargin + (prevline == 1 ? start_hpos : 0),
				 from, 10000, 10000,
				 width, hscroll, 0);
	}
      else
	{
	  pos.hpos = lmargin + (from == 1 ? start_hpos : 0);
	  pos.vpos = 0;
	}
      return compute_motion (from, vpos, pos.hpos,
			     ZV, vtarget, - (1 << (SHORTBITS - 1)),
			     width, hscroll, pos.vpos * width);
    }

  /* To move upward, go a line at a time until
     we have gone at least far enough */

  first = 1;

  while ((vpos > vtarget || first) && from > BEGV)
    {
      prevline = from;
      while (1)
	{
	  prevline = find_next_newline (prevline - 1, -1);
	  if (prevline == BEGV
	      || selective <= 0
	      || position_indentation (prevline) < selective)
	    break;
	}
      pos = *compute_motion (prevline, 0,
			     lmargin + (prevline == 1 ? start_hpos : 0),
			     from, 10000, 10000,
			     width, hscroll, 0);
      vpos -= pos.vpos;
      first = 0;
      from = prevline;
    }

  /* If we made exactly the desired vertical distance,
     or if we hit beginning of buffer,
     return point found */
  if (vpos >= vtarget)
    {
      val_vmotion.bufpos = from;
      val_vmotion.vpos = vpos;
      val_vmotion.hpos = lmargin;
      val_vmotion.contin = 0;
      val_vmotion.prevhpos = 0;
      return &val_vmotion;
    }
  
  /* Otherwise find the correct spot by moving down */
  goto retry;
}

DEFUN ("vertical-motion", Fvertical_motion, Svertical_motion, 1, 1, 0,
  "Move to start of screen line LINES lines down.\n\
If LINES is negative, this is moving up.\n\
Sets point to position found; this may be start of line\n\
 or just the start of a continuation line.\n\
Returns number of lines moved; may be closer to zero than LINES\n\
 if beginning or end of buffer was reached.")
  (lines)
     Lisp_Object lines;
{
  struct position pos;
  register struct window *w = XWINDOW (selected_window);

  CHECK_NUMBER (lines, 0);

  pos = *vmotion (point, XINT (lines),
		  XFASTINT (w->width) - 1
		  - (XFASTINT (w->width) + XFASTINT (w->left)
		     != XFASTINT (XWINDOW (minibuf_window)->width)),
		  /* Not XFASTINT since perhaps could be negative */
		  XINT (w->hscroll), selected_window);

  SET_PT (pos.bufpos);
  return make_number (pos.vpos);
}

syms_of_indent ()
{
  DEFVAR_BOOL ("indent-tabs-mode", &indent_tabs_mode,
    "*Indentation can insert tabs if this is non-nil.\n\
Setting this variable automatically makes it local to the current buffer.");
  indent_tabs_mode = 1;

  defsubr (&Scurrent_indentation);
  defsubr (&Sindent_to);
  defsubr (&Scurrent_column);
  defsubr (&Smove_to_column);
  defsubr (&Svertical_motion);
}
