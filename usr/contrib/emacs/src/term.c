/* terminal control module for terminals described by TERMCAP
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

/* DCICcost[n] is cost of inserting N characters.
   DCICcost[-n] is cost of deleting N characters. */

#define DCICcost (&DC_ICcost[MScreenWidth])
int DC_ICcost[1 + 2 * MScreenWidth];


/* Hook functions that you can set to snap out the functions in this file.
   These are all extern'd in termhooks.h  */

int (*topos_hook) ();
int (*raw_topos_hook) ();

int (*clear_to_end_hook) ();
int (*clear_screen_hook) ();
int (*clear_end_of_line_hook) ();

int (*ins_del_lines_hook) ();

int (*change_line_highlight_hook) ();
int (*reassert_line_highlight_hook) ();

int (*insert_chars_hook) ();
int (*write_chars_hook) ();
int (*delete_chars_hook) ();

int (*ring_bell_hook) ();

int (*reset_terminal_modes_hook) ();
int (*set_terminal_modes_hook) ();
int (*update_begin_hook) ();
int (*update_end_hook) ();
int (*set_terminal_window_hook) ();

int (*read_socket_hook) ();
int (*fix_screen_hook) ();

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

int TF_hazeltine;		/* termcap hz flag. */
int TF_insmode_motion;		/* termcap mi flag: can move while in insert mode. */
int TF_standout_motion;		/* termcap mi flag: can move while in standout mode. */
int TF_underscore;		/* termcap ul flag: _ underlines if overstruck on
				   nonblank position.  Must clear before writing _.  */
int TF_teleray;			/* termcap xt flag: many weird consequences.  For t1061. */

int TN_standout_width;		/* termcap sg number: width occupied by standout markers */

static int RPov;		/* # chars to start a TS_repeat */

static int delete_in_insert_mode;		/* delete mode == insert mode */

static int se_is_so;		/* 1 if same string both enters and leaves standout mode */

/* internal state */

/* Number of chars of space used for standout marker at beginning of line,
   or'd with 0100.  Zero if no standout marker at all.  */
/* used iff TN_standout_width >= 0. */
char chars_wasted[MScreenLength];

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
      buf = (char *) alloca (strlen (TS_set_scroll_region) + 10);
      tparam (TS_set_scroll_region, buf, start, stop - 1);
    }
  else if (TS_set_scroll_region_1)
    {
      buf = (char *) alloca (strlen (TS_set_scroll_region_1) + 20);
      tparam (TS_set_scroll_region_1, buf,
	      screen_height, start, screen_height - stop, screen_height);
    }
  else
    {
      buf = (char *) alloca (strlen (TS_set_window) + 20);
      tparam (TS_set_window, buf, start, 0, stop, screen_width);
    }
  OUTPUT (buf);
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
  if (flag || (TS_end_standout_mode && !TF_teleray && !se_is_so))
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

  topos (vpos, 0);

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

topos (row, col)
{
  col += chars_wasted[row] & 077;
  if (topos_hook)
    {
      (*topos_hook) (row, col);
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

raw_topos (row, col)
{
  if (raw_topos_hook)
    {
      (*raw_topos_hook) (row, col);
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
	  topos (i, 0);
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
      topos (0, 0);
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
    write_chars (" ", 1);
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

write_chars (start, len)
     register char *start;
     int len;
{
  register char *p;
  register int n;
  register char *buf;
  register int c;

  if (write_chars_hook)
    {
      (*write_chars_hook) (start, len);
      return;
    }
  highlight_if_desired ();
  turn_off_insert ();

  /* Don't dare write in last column of bottom line, if AutoWrap,
     since that would scroll the whole screen on some terminals.  */
  if (AutoWrap && curY + 1 == screen_height
      && curX + len == screen_width)
    len --;

  cmplus (len);

  if (RPov > len && !TF_underscore && !TF_hazeltine)
    {
      fwrite (start, 1, len, stdout);
      if (ferror (stdout))
	clearerr (stdout);
      if (termscript)
	fwrite (start, 1, len, termscript);
    }
  else
    while (--len >= 0)
      {
	if (RPov + 1 < len && *start == start[1])
	  {
	    p = start + 1;

	    /* Now, len is number of chars left starting at p */
	    while (*p++ == *start);
	    /* n is number of identical chars in this run */
	    n = p - start;
	    if (n > RPov)
	      {
		buf = (char *) alloca (strlen (TS_repeat) + 10);
		tparam (TS_repeat, buf, *start, n);
		tputs (buf, n, cmputc);
		start = p;
		len -= n - 1;
		continue;
	      }
	  }
	c = *start++;
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
      buf = (char *) alloca (strlen (TS_ins_multi_chars) + 10);
      tparam (TS_ins_multi_chars, buf, len);
      OUTPUT1 (buf);
      if (start)
	write_chars (start, len);
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
      buf = (char *) alloca (strlen (TS_del_multi_chars) + 10);
      tparam (TS_del_multi_chars, buf, n);
      OUTPUT1 (buf);
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
  char copybuf[MScreenWidth];

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
      raw_topos (vpos, 0);
      background_highlight ();
      buf = (char *) alloca (strlen (multi) + 10);
      tparam (multi, buf, i);
      OUTPUT (buf);
    }
  else if (single)
    {
      raw_topos (vpos, 0);
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
	raw_topos (specified_window - 1, 0);
      else
	raw_topos (vpos, 0);
      background_highlight ();
      while (--i >= 0)
	OUTPUTL (scroll, specified_window - vpos);
      set_scroll_region (0, specified_window);
    }

  if (TN_standout_width >= 0)
    {
      if (n < 0)
	{
	  bcopy (&chars_wasted[curY - n], &chars_wasted[curY], screen_height - curY + n);
	  bzero (&chars_wasted[screen_height + n], - n);
	}
      else
	{
	  bcopy (&chars_wasted[curY], &copybuf[curY], screen_height - curY - n);
	  bcopy (&copybuf[curY], &chars_wasted[curY + n], screen_height - curY - n);
	  bzero (&chars_wasted[curY], n);
	}
    }
  if (!scroll_region_ok && memory_below_screen && n < 0)
    {
      topos (screen_height + n, 0);
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
      ins_startup_cost = 0.3 * (string_cost (TS_insert_mode) + string_cost (TS_end_insert_mode));
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
			  + string_cost (TS_end_delete_mode))
	* (delete_in_insert_mode ? 0.5 : 1.0);
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

  if (dont_calculate_costs)
    return;

  if (s && (!TS_ins_line && !TS_del_line))
    CalcIDCosts (TS_rev_scroll, TS_ins_multi_lines,
		 TS_fwd_scroll, TS_del_multi_lines,
		 s, s);
  else
    CalcIDCosts (TS_ins_line, TS_ins_multi_lines,
		 TS_del_line, TS_del_multi_lines,
		 0, 0);

  calculate_ins_del_char_costs ();

  /* Don't use TS_repeat if its padding is worse than sending the chars */
  if (TS_repeat && per_line_cost (TS_repeat) * baud_rate < 9000)
    RPov = string_cost (TS_repeat);
  else
    RPov = MScreenWidth;

  cmcostinit ();		/* set up cursor motion costs */
}

term_init (terminal_type)
     char *terminal_type;
{
  char *combuf;
  char *fill;
  char tbuf[2044];
  register char *p;

  extern char *tgetstr ();

  Wcm_clear ();
  dont_calculate_costs = 0;

  if (tgetent (tbuf, terminal_type) <= 0)
    fatal ("Terminal type %s is not defined.\n", terminal_type);

#ifdef TERMINFO
  combuf = (char *) malloc (2044);
#else
  combuf = (char *) malloc (strlen (tbuf));
#endif /* not TERMINFO */
  if (combuf == 0)
    abort ();
  fill = combuf;

  TS_ins_line = tgetstr ("al", &fill);
  TS_ins_multi_lines = tgetstr ("AL", &fill);
  Left = tgetstr ("bc", &fill);
  TS_bell = tgetstr ("bl", &fill);
  TS_clr_to_bottom = tgetstr ("cd", &fill);
  TS_clr_line = tgetstr ("ce", &fill);
  TS_clr_screen = tgetstr ("cl", &fill);
  ColPosition = tgetstr ("ch", &fill);
  AbsPosition = tgetstr ("cm", &fill);
  CR = tgetstr ("cr", &fill);
  TS_set_scroll_region = tgetstr ("cs", &fill);
  TS_set_scroll_region_1 = tgetstr ("cS", &fill);
  RowPosition = tgetstr ("cv", &fill);
  TS_del_char = tgetstr ("dc", &fill);
  TS_del_multi_chars = tgetstr ("DC", &fill);
  TS_del_line = tgetstr ("dl", &fill);
  TS_del_multi_lines = tgetstr ("DL", &fill);
  TS_delete_mode = tgetstr ("dm", &fill);
  TS_end_delete_mode = tgetstr ("ed", &fill);
  TS_end_insert_mode = tgetstr ("ei", &fill);
  Home = tgetstr ("ho", &fill);
  TS_ins_char = tgetstr ("ic", &fill);
  TS_ins_multi_chars = tgetstr ("IC", &fill);
  TS_insert_mode = tgetstr ("im", &fill);
  TS_pad_inserted_char = tgetstr ("ip", &fill);
  TS_end_keypad_mode = tgetstr ("ke", &fill);
  TS_keypad_mode = tgetstr ("ks", &fill);
  LastLine = tgetstr ("ll", &fill);
  Right = tgetstr ("nd", &fill);
  Down = tgetstr ("nl", &fill);
  TS_pad_char = tgetstr ("pc", &fill);
  TS_repeat = tgetstr ("rp", &fill);
  TS_end_standout_mode = tgetstr ("se", &fill);
  TS_fwd_scroll = tgetstr ("sf", &fill);
  TS_standout_mode = tgetstr ("so", &fill);
  TS_rev_scroll = tgetstr ("sr", &fill);
  Tab = tgetstr ("ta", &fill);
  TS_end_termcap_modes = tgetstr ("te", &fill);
  TS_termcap_modes = tgetstr ("ti", &fill);
  Up = tgetstr ("up", &fill);
  TS_visible_bell = tgetstr ("vb", &fill);
  TS_end_visual_mode = tgetstr ("ve", &fill);
  TS_visual_mode = tgetstr ("vs", &fill);
  TS_set_window = tgetstr ("wi", &fill);

  AutoWrap = tgetflag ("am");
  memory_below_screen = tgetflag ("db");
  TF_hazeltine = tgetflag ("hz");
  must_write_spaces = tgetflag ("in");
  MetaFlag = tgetflag ("km") || tgetflag ("MT");
  TF_insmode_motion = tgetflag ("mi");
  TF_standout_motion = tgetflag ("ms");
  TF_underscore = tgetflag ("ul");
  MagicWrap = tgetflag ("xn");
  TF_teleray = tgetflag ("xt");

  /* Get screen size fro system, or else from termcap.  */
  get_screen_size (&screen_width, &screen_height);
  if (screen_width <= 0)
    screen_width = tgetnum ("co");
  if (screen_height <= 0)
    screen_height = tgetnum ("li");

  min_padding_speed = tgetnum ("pb");
  TN_standout_width = tgetnum ("sg");
  TabWidth = tgetnum ("tw");

  if (tgetflag ("bs"))
    Left = "\b";		/* can't possibly be longer! */
  else if (!Left)
    Left = tgetstr ("le", &fill);

  if (!Down)
    Down = tgetstr ("do", &fill);

  if (!TS_bell)
    TS_bell = "\07";

  if (!TS_fwd_scroll)
    TS_fwd_scroll = Down;

  PC = TS_pad_char ? *TS_pad_char : 0;

  if (TabWidth < 0)
    TabWidth = 8;
  
  if (!Tab)
    Tab = "\t";

  if (TS_standout_mode == 0)
    {
      TN_standout_width = tgetnum ("ug");
      TS_end_standout_mode = tgetstr ("ue", &fill);
      TS_standout_mode = tgetstr ("us", &fill);
    }

  if (TF_teleray)
    {
      Tab = 0;
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
      strcpy (fill, TS_termcap_modes);
      strcat (fill, "\033\007!");
      TS_termcap_modes = fill;
      fill += strlen (fill) + 1;
      p = combuf;
      /* Change all %+ parameters to %C, to handle
	 values above 96 correctly for the C100.  */
      while (p != fill)
	{
	  if (p[0] == '%' && p[1] == '+')
	    p[1] = 'C';
	  p++;
	}
    }

  screen_height = min (screen_height, MScreenLength);
  screen_width = min (screen_width, MScreenWidth);

  ScreenRows = screen_height;
  ScreenCols = screen_width;
  specified_window = screen_height;

  if (Wcm_init ())	/* can't do cursor motion */
    fatal ("Terminal type \"%s\" is not powerful enough to run Emacs.\n\
It lacks the ability to position the cursor.\n\
If that is not the actual type of terminal you have,\n\
use the C-shell command `setenv TERM ...' to specify the correct type.\n",
	   terminal_type);

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

  char_ins_del_ok = ((TS_ins_char || TS_ins_multi_chars)
		     && (TS_del_char || TS_del_multi_chars));

  fast_clear_end_of_line = TS_clr_line != 0;

  init_baud_rate ();
  if (read_socket_hook)		/* Baudrate is somewhat */
				/* meaningless in this case */
    baud_rate = 9600;

  calculate_costs ();
}

/* VARARGS 1 */
fatal (str, arg1, arg2)
{
  fprintf (stderr, "emacs: ");
  fprintf (stderr, str, arg1, arg2);
  exit (1);
}
