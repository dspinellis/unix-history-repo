/* terminal control module for terminals described by TERMCAP
   Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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
#include <ctype.h>
#include "config.h"
#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "cm.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

#define OUTPUT(a) tputs (a, screen_height - curY, cmputc)
#define OUTPUT1(a) tputs (a, 1, cmputc)
#define OUTPUTL(a, lines) tputs (a, lines, cmputc)
#define OUTPUT_IF(a) { if (a) tputs (a, screen_height - curY, cmputc); }
#define OUTPUT1_IF(a) { if (a) tputs (a, 1, cmputc); }

/* Terminal charateristics that higher levels want to look at.
   These are all extern'd in termchar.h */

int screen_width;		/* Number of usable columns */
int screen_height;		/* Number of lines */
int must_write_spaces;		/* Nonzero means spaces in the text
				   must actually be output; can't just skip
				   over some columns to leave them blank.  */
int min_padding_speed;		/* Speed below which no padding necessary */

int line_ins_del_ok;		/* Terminal can insert and delete lines */
int char_ins_del_ok;		/* Terminal can insert and delete chars */
int scroll_region_ok;		/* Terminal supports setting the scroll window */
int memory_below_screen;	/* Terminal remembers lines scrolled off bottom */
int fast_clear_end_of_line;	/* Terminal has a `ce' string */

int dont_calculate_costs;	/* Nonzero means don't bother computing */
				/* various cost tables; we won't use them.  */

/* Nonzero means no need to redraw the entire screen on resuming
   a suspended Emacs.  This is useful on terminals with multiple pages,
   where one page is used for Emacs and another for all else. */
int no_redraw_on_reenter;

/* DCICcost[n] is cost of inserting N characters.
   DCICcost[-n] is cost of deleting N characters. */

#define DCICcost (&DC_ICcost[screen_width])
int *DC_ICcost;

/* Hook functions that you can set to snap out the functions in this file.
   These are all extern'd in termhooks.h  */

int (*move_cursor_hook) ();
int (*raw_move_cursor_hook) ();

int (*clear_to_end_hook) ();
int (*clear_screen_hook) ();
int (*clear_end_of_line_hook) ();

int (*ins_del_lines_hook) ();

int (*change_line_highlight_hook) ();
int (*reassert_line_highlight_hook) ();

int (*insert_chars_hook) ();
int (*output_chars_hook) ();
int (*delete_chars_hook) ();

int (*ring_bell_hook) ();

int (*reset_terminal_modes_hook) ();
int (*set_terminal_modes_hook) ();
int (*update_begin_hook) ();
int (*update_end_hook) ();
int (*set_terminal_window_hook) ();

int (*read_socket_hook) ();
int (*fix_screen_hook) ();
int (*calculate_costs_hook) ();

/* Strings, numbers and flags taken from the termcap entry.  */

char *TS_ins_line;		/* termcap "al" */
char *TS_ins_multi_lines;	/* "AL" (one parameter, # lines to insert) */
char *TS_bell;			/* "bl" */
char *TS_clr_to_bottom;		/* "cd" */
char *TS_clr_line;		/* "ce", clear to end of line */
char *TS_clr_screen;		/* "cl" */
char *TS_set_scroll_region;	/* "cs" (2 params, first line and last line) */
char *TS_set_scroll_region_1;   /* "cS" (4 params: total lines,
				   lines above scroll region, lines below it,
				   total lines again) */
char *TS_del_char;		/* "dc" */
char *TS_del_multi_chars;	/* "DC" (one parameter, # chars to delete) */
char *TS_del_line;		/* "dl" */
char *TS_del_multi_lines;	/* "DL" (one parameter, # lines to delete) */
char *TS_delete_mode;		/* "dm", enter character-delete mode */
char *TS_end_delete_mode;	/* "ed", leave character-delete mode */
char *TS_end_insert_mode;	/* "ei", leave character-insert mode */
char *TS_ins_char;		/* "ic" */
char *TS_ins_multi_chars;	/* "IC" (one parameter, # chars to insert) */
char *TS_insert_mode;		/* "im", enter character-insert mode */
char *TS_pad_inserted_char;	/* "ip".  Just padding, no commands.  */
char *TS_end_keypad_mode;	/* "ke" */
char *TS_keypad_mode;		/* "ks" */
char *TS_pad_char;		/* "pc", char to use as padding */
char *TS_repeat;		/* "rp" (2 params, # times to repeat
				   and character to be repeated) */
char *TS_end_standout_mode;	/* "se" */
char *TS_fwd_scroll;		/* "sf" */
char *TS_standout_mode;		/* "so" */
char *TS_rev_scroll;		/* "sr" */
char *TS_end_termcap_modes;	/* "te" */
char *TS_termcap_modes;		/* "ti" */
char *TS_visible_bell;		/* "vb" */
char *TS_end_visual_mode;	/* "ve" */
char *TS_visual_mode;		/* "vi" */
char *TS_set_window;		/* "wi" (4 params, start and end of window,
				   each as vpos and hpos) */

int TF_hazeltine;	/* termcap hz flag. */
int TF_insmode_motion;	/* termcap mi flag: can move while in insert mode. */
int TF_standout_motion;	/* termcap mi flag: can move while in standout mode. */
int TF_underscore;	/* termcap ul flag: _ underlines if overstruck on
			   nonblank position.  Must clear before writing _.  */
int TF_teleray;		/* termcap xt flag: many weird consequences.  For t1061. */

int TF_xs;		/* Nonzero for "xs".  If set together with
			   TN_standout_width == 0, it means don't bother
			   to write any end-standout cookies.  */

int TN_standout_width;	/* termcap sg number: width occupied by standout markers */

static int RPov;	/* # chars to start a TS_repeat */

static int delete_in_insert_mode;	/* delete mode == insert mode */

static int se_is_so;	/* 1 if same string both enters and leaves standout mode */

/* internal state */

/* Number of chars of space used for standout marker at beginning of line,
   or'd with 0100.  Zero if no standout marker at all.  */
/* used iff TN_standout_width >= 0. */
char *chars_wasted;
static char *copybuf;

/* nonzero means supposed to write text in standout mode.  */
int standout_requested;

int insert_mode;			/* Nonzero when in insert mode.  */
int standout_mode;			/* Nonzero when in standout mode.  */

/* Size of window specified by higher levels.
   This is the number of lines, starting from top of screen,
   to participate in ins/del line operations.
   Effectively it excludes the bottom
      screen_height - specified_window_size
   lines from those operations.  */

int specified_window;

char *tparam ();

ring_bell ()
{
  if (ring_bell_hook)
    {
      (*ring_bell_hook) ();
      return;
    }
  OUTPUT (TS_visible_bell && visible_bell ? TS_visible_bell : TS_bell);
}

set_terminal_modes ()
{
  if (set_terminal_modes_hook)
    {
      (*set_terminal_modes_hook) ();
      return;
    }
  OUTPUT_IF (TS_termcap_modes);
  OUTPUT_IF (TS_visual_mode);
  OUTPUT_IF (TS_keypad_mode);
  losecursor ();
}

reset_terminal_modes ()
{
  if (reset_terminal_modes_hook)
    {
      (*reset_terminal_modes_hook) ();
      return;
    }
  if (TN_standout_width < 0)
    turn_off_highlight ();
  turn_off_insert ();
  OUTPUT_IF (TS_end_keypad_mode);
  OUTPUT_IF (TS_end_visual_mode);
  OUTPUT_IF (TS_end_termcap_modes);
}

update_begin ()
{
  if (update_begin_hook)
    (*update_begin_hook) ();
}

update_end ()
{
  if (update_end_hook)
    {
      (*update_end_hook) ();
      return;
    }
  turn_off_insert ();
  background_highlight ();
  standout_requested = 0;
}

set_terminal_window (size)
     int size;
{
  if (set_terminal_window_hook)
    {
      (*set_terminal_window_hook) (size);
      return;
    }
  specified_window = size ? size : screen_height;
  if (!scroll_region_ok)
    return;
  set_scroll_region (0, specified_window);
}

set_scroll_region (start, stop)
     int start, stop;
{
  char *buf;
  if (TS_set_scroll_region)
    {
      buf = tparam (TS_set_scroll_region, 0, 0, start, stop - 1);
    }
  else if (TS_set_scroll_region_1)
    {
      buf = tparam (TS_set_scroll_region_1, 0, 0,
		    screen_height, start, screen_height - stop, screen_height);
    }
  else
    {
      buf = tparam (TS_set_window, 0, 0, start, 0, stop, screen_width);
    }
  OUTPUT (buf);
  free (buf);
  losecursor ();
}

turn_on_insert ()
{
  if (!insert_mode)
    OUTPUT (TS_insert_mode);
  insert_mode = 1;
}

turn_off_insert ()
{
  if (insert_mode)
    OUTPUT (TS_end_insert_mode);
  insert_mode = 0;
}

/* Handle highlighting when TN_standout_width (termcap sg) is not specified.
   In these terminals, output is affected by the value of standout
   mode when the output is written.

   These functions are called on all terminals, but do nothing
   on terminals whose standout mode does not work that way.  */

turn_off_highlight ()
{
  if (TN_standout_width < 0)
    {
      if (standout_mode)
	OUTPUT_IF (TS_end_standout_mode);
      standout_mode = 0;
    }
}

turn_on_highlight ()
{
  if (TN_standout_width < 0)
    {
      if (!standout_mode)
	OUTPUT_IF (TS_standout_mode);
      standout_mode = 1;
    }
}

/* Set standout mode to the state it should be in for
   empty space inside windows.  What this is,
   depends on the user option inverse-video.  */

background_highlight ()
{
  if (TN_standout_width >= 0)
    return;
  if (inverse_video)
    turn_on_highlight ();
  else
    turn_off_highlight ();
}

/* Set standout mode to the mode specified for the text to be output.  */

static
highlight_if_desired ()
{
  if (TN_standout_width >= 0)
    return;
  if (!inverse_video == !standout_requested)
    turn_off_highlight ();
  else
    turn_on_highlight ();
}

/* Handle standout mode for terminals in which TN_standout_width >= 0.
   On these terminals, standout is controlled by markers that
   live inside the screen memory.  TN_standout_width is the width
   that the marker occupies in memory.  Standout runs from the marker
   to the end of the line on some terminals, or to the next
   turn-off-standout marker (TS_end_standout_mode) string
   on other terminals.  */

/* Write a standout marker or end-standout marker at the front of the line
   at vertical position vpos.  */

write_standout_marker (flag, vpos)
     int flag, vpos;
{
  if (flag || (TS_end_standout_mode && !TF_teleray && !se_is_so
	       && !(TF_xs && TN_standout_width == 0)))
    {
      cmgoto (vpos, 0);
      cmplus (TN_standout_width);
      OUTPUT (flag ? TS_standout_mode : TS_end_standout_mode);
      chars_wasted[curY] = TN_standout_width | 0100;
    }
}

/* External interface to control of standout mode.
   Call this when about to modify line at position VPOS
   and not change whether it is highlighted.  */

reassert_line_highlight (highlight, vpos)
     int highlight;
     int vpos;
{
  if (reassert_line_highlight_hook)
    {
      (*reassert_line_highlight_hook) (highlight, vpos);
      return;
    }
  if (TN_standout_width < 0)
    /* Handle terminals where standout takes affect at output time */
    standout_requested = highlight;
  else if (chars_wasted[vpos] == 0)
    /* For terminals with standout markers, write one on this line
       if there isn't one already.  */
    write_standout_marker (highlight, vpos);
}

/* Call this when about to modify line at position VPOS
   and change whether it is highlighted.  */

change_line_highlight (new_highlight, vpos, first_unused_hpos)
     int new_highlight, vpos, first_unused_hpos;
{
  standout_requested = new_highlight;
  if (change_line_highlight_hook)
    {
      (*change_line_highlight_hook) (new_highlight, vpos, first_unused_hpos);
      return;
    }

  move_cursor (vpos, 0);

  if (TN_standout_width < 0)
    background_highlight ();
  /* If line starts with a marker, delete the marker */
  else if (TS_clr_line && chars_wasted[curY])
    {
      turn_off_insert ();
      /* On Teleray, make sure to erase the SO marker.  */
      if (TF_teleray)
	{
	  cmgoto (curY - 1, screen_width - 4);
	  OUTPUT ("\033S");
	  curY++;		/* ESC S moves to next line where the TS_standout_mode was */
	  curX = 0;
	}
      else
	cmgoto (curY, 0);	/* reposition to kill standout marker */
    }
  clear_end_of_line_raw (first_unused_hpos);
  reassert_line_highlight (new_highlight, curY);
}

/* Move to absolute position, specified origin 0 */

move_cursor (row, col)
{
  col += chars_wasted[row] & 077;
  if (move_cursor_hook)
    {
      (*move_cursor_hook) (row, col);
      return;
    }
  if (curY == row && curX == col)
    return;
  if (!TF_standout_motion)
    background_highlight ();
  if (!TF_insmode_motion)
    turn_off_insert ();
  cmgoto (row, col);
}

/* Similar but don't take any account of the wasted characters.  */

raw_move_cursor (row, col)
{
  if (raw_move_cursor_hook)
    {
      (*raw_move_cursor_hook) (row, col);
      return;
    }
  if (curY == row && curX == col)
    return;
  if (!TF_standout_motion)
    background_highlight ();
  if (!TF_insmode_motion)
    turn_off_insert ();
  cmgoto (row, col);
}

/* Erase operations */

/* clear from cursor to end of screen */
clear_to_end ()
{
  register int i;

  if (clear_to_end_hook)
    {
      (*clear_to_end_hook) ();
      return;
    }
  if (TS_clr_to_bottom)
    {
      background_highlight ();
      OUTPUT (TS_clr_to_bottom);
      bzero (chars_wasted + curY, screen_height - curY);
    }
  else
    {
      for (i = curY; i < screen_height; i++)
	{
	  move_cursor (i, 0);
	  clear_end_of_line_raw (screen_width);
	}
    }
}

/* Clear entire screen */

clear_screen ()
{
  if (clear_screen_hook)
    {
      (*clear_screen_hook) ();
      return;
    }
  if (TS_clr_screen)
    {
      background_highlight ();
      OUTPUT (TS_clr_screen);
      bzero (chars_wasted, screen_height);
      cmat (0, 0);
    }
  else
    {
      move_cursor (0, 0);
      clear_to_end ();
    }
}

/* Clear to end of line, but do not clear any standout marker.
   Assumes that the cursor is positioned at a character of real text,
   which implies it cannot be before a standout marker
   unless the marker has zero width.

   Note that the cursor may be moved.  */

clear_end_of_line (first_unused_hpos)
     int first_unused_hpos;
{
  if (TN_standout_width == 0 && curX == 0 && chars_wasted[curY] != 0)
    output_chars (" ", 1);
  clear_end_of_line_raw (first_unused_hpos);
}

/* Clear from cursor to end of line.
   Assume that the line is already clear starting at column first_unused_hpos.
   If the cursor is at a standout marker, erase the marker.

   Note that the cursor may be moved, on terminals lacking a `ce' string.  */

clear_end_of_line_raw (first_unused_hpos)
     int first_unused_hpos;
{
  register int i;
  first_unused_hpos += chars_wasted[curY] & 077;
  if (clear_end_of_line_hook)
    {
      (*clear_end_of_line_hook) (first_unused_hpos);
      return;
    }
  if (curX >= first_unused_hpos)
    return;
  /* Notice if we are erasing a magic cookie */
  if (curX == 0)
    chars_wasted[curY] = 0;
  background_highlight ();
  if (TS_clr_line)
    {
      OUTPUT1 (TS_clr_line);
    }
  else
    {			/* have to do it the hard way */
      turn_off_insert ();
      for (i = curX; i < first_unused_hpos; i++)
	{
	  if (termscript)
	    fputc (' ', termscript);
	  putchar (' ');
	}
      cmplus (first_unused_hpos - curX);
    }
}

output_chars (string, len)
     register char *string;
     int len;
{
  register char *p;
  register int n;
  register char *buf;
  register int c;
  char *first_check;

  if (output_chars_hook)
    {
      (*output_chars_hook) (string, len);
      return;
    }
  highlight_if_desired ();
  turn_off_insert ();

  /* Don't dare write in last column of bottom line, if AutoWrap,
     since that would scroll the whole screen on some terminals.  */

  if (AutoWrap && curY + 1 == screen_height
      && curX + len - (chars_wasted[curY] & 077) == screen_width)
    len --;

  cmplus (len);

  first_check = string;

  if (RPov > len && !TF_underscore && !TF_hazeltine)
    {
      fwrite (string, 1, len, stdout);
      if (ferror (stdout))
	clearerr (stdout);
      if (termscript)
	fwrite (string, 1, len, termscript);
    }
  else
    while (--len >= 0)
      {
	c = *string;
	if (RPov + 1 < len && string >= first_check)
	  {
	    int repeat_count;

	    p = string + 1;

	    /* Now, len is number of chars left starting at p */
	    while (*p++ == c);
	    p--;

	    repeat_count = p - string;
	    if (repeat_count > RPov)
	      {
		buf = tparam (TS_repeat, 0, 0, *string, repeat_count);
		tputs (buf, repeat_count, cmputc);
		free (buf);
		string = p;
		len -= repeat_count - 1;
		continue;
	      }
	    else
	      /* If all N identical chars are too few,
		 don't even consider the last N-1, the last N-2,...  */
	      first_check = p;
	  }
	if (c == '_' && TF_underscore)
	  {
	    if (termscript)
	      fputc (' ', termscript);
	    putchar (' ');
	    OUTPUT (Left);
	  }
	if (TF_hazeltine && c == '~')
	  c = '`';
	if (termscript)
	  fputc (c, termscript);
	putchar (c);
	string++;
      }
}

/* If start is zero, insert blanks instead of a string at start */

insert_chars (start, len)
     register char *start;
     int len;
{
  register char *buf;
  register int c;

  if (insert_chars_hook)
    {
      (*insert_chars_hook) (start, len);
      return;
    }
  highlight_if_desired ();

  if (TS_ins_multi_chars)
    {
      buf = tparam (TS_ins_multi_chars, 0, 0, len);
      OUTPUT1 (buf);
      free (buf);
      if (start)
	output_chars (start, len);
      return;
    }

  turn_on_insert ();
  cmplus (len);

  if (!TF_underscore && !TF_hazeltine && start
      && TS_pad_inserted_char == 0 && TS_ins_char == 0)
    {
      fwrite (start, 1, len, stdout);
      if (termscript)
	fwrite (start, 1, len, termscript);
    }
  else
    while (--len >= 0)
      {
	OUTPUT1_IF (TS_ins_char);
	if (!start)
	  c = ' ';
	else
	  {
	    c = *start++;
	    if (TF_hazeltine && c == '~')
	      c = '`';
	  }
	if (termscript)
	  fputc (c, termscript);
	putchar (c);
	OUTPUT1_IF (TS_pad_inserted_char);
    }
}

delete_chars (n)
     register int n;
{
  char *buf;
  register int i;

  if (delete_chars_hook)
    {
      (*delete_chars_hook) (n);
      return;
    }

  if (delete_in_insert_mode)
    {
      turn_on_insert ();
    }
  else
    {
      turn_off_insert ();
      OUTPUT_IF (TS_delete_mode);
    }

  if (TS_del_multi_chars)
    {
      buf = tparam (TS_del_multi_chars, 0, 0, n);
      OUTPUT1 (buf);
      free (buf);
    }
  else
    for (i = 0; i < n; i++)
      OUTPUT1 (TS_del_char);
  if (!delete_in_insert_mode)
    OUTPUT_IF (TS_end_delete_mode);
}

/* Insert N lines at vpos VPOS.  If N is negative, delete -N lines.  */

ins_del_lines (vpos, n)
     int vpos, n;
{
  char *multi = n > 0 ? TS_ins_multi_lines : TS_del_multi_lines;
  char *single = n > 0 ? TS_ins_line : TS_del_line;
  char *scroll = n > 0 ? TS_rev_scroll : TS_fwd_scroll;

  register int i = n > 0 ? n : -n;
  register char *buf;

  if (ins_del_lines_hook)
    {
      (*ins_del_lines_hook) (vpos, n);
      return;
    }

  /* If the lines below the insertion are being pushed
     into the end of the window, this is the same as clearing;
     and we know the lines are already clear, since the matching
     deletion has already been done.  So can ignore this.  */
  /* If the lines below the deletion are blank lines coming
     out of the end of the window, don't bother,
     as there will be a matching inslines later that will flush them. */
  if (scroll_region_ok && vpos + i >= specified_window)
    return;
  if (!memory_below_screen && vpos + i >= screen_height)
    return;

  if (multi)
    {
      raw_move_cursor (vpos, 0);
      background_highlight ();
      buf = tparam (multi, 0, 0, i);
      OUTPUT (buf);
      free (buf);
    }
  else if (single)
    {
      raw_move_cursor (vpos, 0);
      background_highlight ();
      while (--i >= 0)
	OUTPUT (single);
      if (TF_teleray)
	curX = 0;
    }
  else
    {
      set_scroll_region (vpos, specified_window);
      if (n < 0)
	raw_move_cursor (specified_window - 1, 0);
      else
	raw_move_cursor (vpos, 0);
      background_highlight ();
      while (--i >= 0)
	OUTPUTL (scroll, specified_window - vpos);
      set_scroll_region (0, specified_window);
    }

  if (TN_standout_width >= 0)
    {
      register lower_limit
	= scroll_region_ok ? specified_window : screen_height;
      if (n < 0)
	{
	  bcopy (&chars_wasted[vpos - n], &chars_wasted[vpos],
		 lower_limit - vpos + n);
	  bzero (&chars_wasted[lower_limit + n], - n);
	}
      else
	{
	  bcopy (&chars_wasted[vpos], &copybuf[vpos], lower_limit - vpos - n);
	  bcopy (&copybuf[vpos], &chars_wasted[vpos + n],
		 lower_limit - vpos - n);
	  bzero (&chars_wasted[vpos], n);
	}
    }
  if (!scroll_region_ok && memory_below_screen && n < 0)
    {
      move_cursor (screen_height + n, 0);
      clear_to_end ();
    }
}

extern int cost;		/* In cm.c */
extern evalcost ();

/* Compute cost of sending "str", in characters,
   not counting any line-dependent padding.  */
string_cost (str)
     char *str;
{
  cost = 0;
  if (str)
    tputs (str, 0, evalcost);
  return cost;
}

/* Compute cost of sending "str", in characters,
   counting any line-dependent padding at one line.  */
string_cost_one_line (str)
     char *str;
{
  cost = 0;
  if (str)
    tputs (str, 1, evalcost);
  return cost;
}

/* Compute per line amount of line-dependent padding,
   in tenths of characters.  */
per_line_cost (str)
     register char *str;
{
  cost = 0;
  if (str)
    tputs (str, 0, evalcost);
  cost = - cost;
  if (str)
    tputs (str, 10, evalcost);
  return cost;
}

/* ARGSUSED */
calculate_ins_del_char_costs ()
{
  int ins_startup_cost, del_startup_cost;
  int ins_cost_per_char, del_cost_per_char;
  register int i;
  register int *p;

  if (TS_ins_multi_chars)
    {
      ins_cost_per_char = 0;
      ins_startup_cost = string_cost_one_line (TS_ins_multi_chars);
    }
  else if (TS_ins_char || TS_pad_inserted_char
	   || (TS_insert_mode && TS_end_insert_mode))
    {
      ins_startup_cost = (30 * (string_cost (TS_insert_mode) + string_cost (TS_end_insert_mode))) / 100;
      ins_cost_per_char = (string_cost_one_line (TS_ins_char)
			   + string_cost_one_line (TS_pad_inserted_char));
    }
  else
    {
      ins_startup_cost = 9999;
      ins_cost_per_char = 0;
    }

  if (TS_del_multi_chars)
    {
      del_cost_per_char = 0;
      del_startup_cost = string_cost_one_line (TS_del_multi_chars);
    }
  else if (TS_del_char)
    {
      del_startup_cost = (string_cost (TS_delete_mode)
			  + string_cost (TS_end_delete_mode));
      if (delete_in_insert_mode)
	del_startup_cost /= 2;
      del_cost_per_char = string_cost_one_line (TS_del_char);
    }
  else
    {
      del_startup_cost = 9999;
      del_cost_per_char = 0;
    }

  /* Delete costs are at negative offsets */
  p = &DCICcost[0];
  for (i = screen_width; --i >= 0;)
    *--p = (del_startup_cost += del_cost_per_char);

  /* Doing nothing is free */
  p = &DCICcost[0];
  *p++ = 0;

  /* Insert costs are at positive offsets */
  for (i = screen_width; --i >= 0;)
    *p++ = (ins_startup_cost += ins_cost_per_char);
}

calculate_costs ()
{
  register char *s
    = TS_set_scroll_region ? TS_set_scroll_region : TS_set_scroll_region_1;

  if (chars_wasted != 0)
    chars_wasted = (char *) xrealloc (chars_wasted, screen_height);
  else
    chars_wasted = (char *) xmalloc (screen_height);
  bzero (chars_wasted, screen_height);

  if (copybuf != 0)
    copybuf = (char *) xrealloc (copybuf, screen_height);
  else
    copybuf = (char *) xmalloc (screen_height);

  if (DC_ICcost != 0)
    DC_ICcost = (int *) xrealloc (DC_ICcost,
				  (2 * screen_width + 1) * sizeof (int));
  else
    DC_ICcost = (int *) xmalloc ((2 * screen_width + 1) * sizeof (int));

  /* Always call CalcIDCosts because it allocates some vectors.
     That function handles dont_calculate_costs.  */
  if (s && (!TS_ins_line && !TS_del_line))
    CalcIDCosts (TS_rev_scroll, TS_ins_multi_lines,
		 TS_fwd_scroll, TS_del_multi_lines,
		 s, s);
  else
    CalcIDCosts (TS_ins_line, TS_ins_multi_lines,
		 TS_del_line, TS_del_multi_lines,
		 0, 0);

  if (dont_calculate_costs)
    {
      bzero (DC_ICcost, 2 * screen_width * sizeof (int));
      return;
    }

  calculate_ins_del_char_costs ();

  /* Don't use TS_repeat if its padding is worse than sending the chars */
  if (TS_repeat && per_line_cost (TS_repeat) * baud_rate < 9000)
    RPov = string_cost (TS_repeat);
  else
    RPov = screen_width * 2;

  cmcostinit ();		/* set up cursor motion costs */
}

term_init (terminal_type)
     char *terminal_type;
{
  char *area;
  char **address = &area;
  char buffer[2044];
  register char *p;
  int status;

  extern char *tgetstr ();

  Wcm_clear ();
  dont_calculate_costs = 0;

  status = tgetent (buffer, terminal_type);
  if (status < 0)
    fatal ("Cannot open termcap database file.\n");
  if (status == 0)
    fatal ("Terminal type %s is not defined.\n", terminal_type);

#ifdef TERMINFO
  area = (char *) malloc (2044);
#else
  area = (char *) malloc (strlen (buffer));
#endif /* not TERMINFO */
  if (area == 0)
    abort ();

  TS_ins_line = tgetstr ("al", address);
  TS_ins_multi_lines = tgetstr ("AL", address);
  TS_bell = tgetstr ("bl", address);
  TS_clr_to_bottom = tgetstr ("cd", address);
  TS_clr_line = tgetstr ("ce", address);
  TS_clr_screen = tgetstr ("cl", address);
  ColPosition = tgetstr ("ch", address);
  AbsPosition = tgetstr ("cm", address);
  CR = tgetstr ("cr", address);
  TS_set_scroll_region = tgetstr ("cs", address);
  TS_set_scroll_region_1 = tgetstr ("cS", address);
  RowPosition = tgetstr ("cv", address);
  TS_del_char = tgetstr ("dc", address);
  TS_del_multi_chars = tgetstr ("DC", address);
  TS_del_line = tgetstr ("dl", address);
  TS_del_multi_lines = tgetstr ("DL", address);
  TS_delete_mode = tgetstr ("dm", address);
  TS_end_delete_mode = tgetstr ("ed", address);
  TS_end_insert_mode = tgetstr ("ei", address);
  Home = tgetstr ("ho", address);
  TS_ins_char = tgetstr ("ic", address);
  TS_ins_multi_chars = tgetstr ("IC", address);
  TS_insert_mode = tgetstr ("im", address);
  TS_pad_inserted_char = tgetstr ("ip", address);
  TS_end_keypad_mode = tgetstr ("ke", address);
  TS_keypad_mode = tgetstr ("ks", address);
  LastLine = tgetstr ("ll", address);
  Right = tgetstr ("nd", address);
  Down = tgetstr ("do", address);
  if (!Down)
    Down = tgetstr ("nl", address); /* Obsolete name for "do" */
#ifdef VMS
  /* VMS puts a carriage return before each linefeed,
     so it is not safe to use linefeeds.  */
  if (Down && Down[0] == '\n' && Down[1] == '\0')
    Down = 0;
#endif /* VMS */
  if (tgetflag ("bs"))
    Left = "\b";		  /* can't possibly be longer! */
  else				  /* (Actually, "bs" is obsolete...) */
    Left = tgetstr ("le", address);
  if (!Left)
    Left = tgetstr ("bc", address); /* Obsolete name for "le" */
  TS_pad_char = tgetstr ("pc", address);
  TS_repeat = tgetstr ("rp", address);
  TS_end_standout_mode = tgetstr ("se", address);
  TS_fwd_scroll = tgetstr ("sf", address);
  TS_standout_mode = tgetstr ("so", address);
  TS_rev_scroll = tgetstr ("sr", address);
  Wcm.cm_tab = tgetstr ("ta", address);
  TS_end_termcap_modes = tgetstr ("te", address);
  TS_termcap_modes = tgetstr ("ti", address);
  Up = tgetstr ("up", address);
  TS_visible_bell = tgetstr ("vb", address);
  TS_end_visual_mode = tgetstr ("ve", address);
  TS_visual_mode = tgetstr ("vs", address);
  TS_set_window = tgetstr ("wi", address);

  AutoWrap = tgetflag ("am");
  memory_below_screen = tgetflag ("db");
  TF_hazeltine = tgetflag ("hz");
  must_write_spaces = tgetflag ("in");
  meta_key = tgetflag ("km") || tgetflag ("MT");
  TF_insmode_motion = tgetflag ("mi");
  TF_standout_motion = tgetflag ("ms");
  TF_underscore = tgetflag ("ul");
  MagicWrap = tgetflag ("xn");
  TF_xs = tgetflag ("xs");
  TF_teleray = tgetflag ("xt");

  /* Get screen size from system, or else from termcap.  */
  get_screen_size (&screen_width, &screen_height);
  if (screen_width <= 0)
    screen_width = tgetnum ("co");
  if (screen_height <= 0)
    screen_height = tgetnum ("li");

  min_padding_speed = tgetnum ("pb");
  TN_standout_width = tgetnum ("sg");
  TabWidth = tgetnum ("tw");

#ifdef VMS
  /* These capabilities commonly use ^J.
     I don't know why, but sending them on VMS does not work;
     it causes following spaces to be lost, sometimes.
     For now, the simplest fix is to avoid using these capabilities ever.  */
  if (Down && Down[0] == '\n')
    Down = 0;
#endif /* VMS */

  if (!TS_bell)
    TS_bell = "\07";

  if (!TS_fwd_scroll)
    TS_fwd_scroll = Down;

  PC = TS_pad_char ? *TS_pad_char : 0;

  if (TabWidth < 0)
    TabWidth = 8;
  
/* Turned off since /etc/termcap seems to have :ta= for most terminals
   and newer termcap doc does not seem to say there is a default.
  if (!Wcm.cm_tab)
    Wcm.cm_tab = "\t";
*/

  if (TS_standout_mode == 0)
    {
      TN_standout_width = tgetnum ("ug");
      TS_end_standout_mode = tgetstr ("ue", address);
      TS_standout_mode = tgetstr ("us", address);
    }

  if (TF_teleray)
    {
      Wcm.cm_tab = 0;
      /* Teleray: most programs want a space in front of TS_standout_mode,
	   but Emacs can do without it (and give one extra column).  */
      TS_standout_mode = "\033RD";
      TN_standout_width = 1;
      /* But that means we cannot rely on ^M to go to column zero! */
      CR = 0;
      /* LF can't be trusted either -- can alter hpos */
      /* if move at column 0 thru a line with TS_standout_mode */
      Down = 0;
    }

  /* Special handling for certain terminal types known to need it */

  if (!strcmp (terminal_type, "supdup"))
    {
      memory_below_screen = 1;
      Wcm.cm_losewrap = 1;
    }
  if (!strncmp (terminal_type, "c10", 3)
      || !strcmp (terminal_type, "perq"))
    {
      /* Supply a makeshift :wi string.
	 This string is not valid in general since it works only
	 for windows starting at the upper left corner;
	 but that is all Emacs uses.

	 This string works only if the screen is using
	 the top of the video memory, because addressing is memory-relative.
	 So first check the :ti string to see if that is true.

	 It would be simpler if the :wi string could go in the termcap
	 entry, but it can't because it is not fully valid.
	 If it were in the termcap entry, it would confuse other programs.  */
      if (!TS_set_window)
	{
	  p = TS_termcap_modes;
	  while (*p && strcmp (p, "\033v  "))
	    p++;
	  if (*p)
	    TS_set_window = "\033v%C %C %C %C ";
	}
      /* Termcap entry often fails to have :in: flag */
      must_write_spaces = 1;
      /* :ti string typically fails to have \E^G! in it */
      /* This limits scope of insert-char to one line.  */
      strcpy (area, TS_termcap_modes);
      strcat (area, "\033\007!");
      TS_termcap_modes = area;
      area += strlen (area) + 1;
      p = AbsPosition;
      /* Change all %+ parameters to %C, to handle
	 values above 96 correctly for the C100.  */
      while (*p)
	{
	  if (p[0] == '%' && p[1] == '+')
	    p[1] = 'C';
	  p++;
	}
    }

  ScreenRows = screen_height;
  ScreenCols = screen_width;
  specified_window = screen_height;

  if (Wcm_init ())	/* can't do cursor motion */
#ifdef VMS
    fatal ("Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have, use either the\n\
DCL command `SET TERMINAL/DEVICE= ...' for DEC-compatible terminals,\n\
or `define EMACS_TERM \"terminal type\"' for non-DEC terminals.\n",
           terminal_type);
#else
    fatal ("Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have,\n\
use the C-shell command `setenv TERM ...' to specify the correct type.\n\
It may be necessary to do `unsetenv TERMCAP' as well.\n",
	   terminal_type);
#endif

  delete_in_insert_mode
    = TS_delete_mode && TS_insert_mode
      && !strcmp (TS_delete_mode, TS_insert_mode);

  se_is_so = TS_standout_mode && TS_end_standout_mode
    && !strcmp (TS_standout_mode, TS_end_standout_mode);

  /* Remove width of standout marker from usable width of line */
  if (TN_standout_width > 0)
    screen_width -= TN_standout_width;

  UseTabs = tabs_safe_p () && TabWidth == 8;

  scroll_region_ok = TS_set_window || TS_set_scroll_region
    || TS_set_scroll_region_1;

  line_ins_del_ok = (((TS_ins_line || TS_ins_multi_lines)
		      && (TS_del_line || TS_del_multi_lines))
		     || (scroll_region_ok
			 && TS_fwd_scroll
			 && TS_rev_scroll));

  char_ins_del_ok = ((TS_ins_char || TS_insert_mode ||
		      TS_pad_inserted_char || TS_ins_multi_chars)
		     && (TS_del_char || TS_del_multi_chars));

  fast_clear_end_of_line = TS_clr_line != 0;

  init_baud_rate ();
  if (read_socket_hook)		/* Baudrate is somewhat */
				/* meaningless in this case */
    baud_rate = 9600;
}

/* VARARGS 1 */
fatal (str, arg1, arg2)
     char *str;
{
  fprintf (stderr, "emacs: ");
  fprintf (stderr, str, arg1, arg2);
  fflush (stderr);
  exit (1);
}
