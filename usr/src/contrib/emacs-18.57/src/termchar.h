/* Flags and parameters describing terminal's characteristics.
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


extern int baud_rate;		/* Output speed in baud */
extern int screen_width;	/* Number of usable columns */
extern int screen_height;	/* Number of lines */
extern int must_write_spaces;	/* Nonzero means spaces in the text
				   must actually be output; can't just skip
				   over some columns to leave them blank.  */
extern int min_padding_speed;	/* Speed below which no padding necessary */
extern int fast_clear_end_of_line; /* Nonzero means terminal has command for this */

extern int line_ins_del_ok;	/* Terminal can insert and delete lines */
extern int char_ins_del_ok;	/* Terminal can insert and delete chars */
extern int scroll_region_ok;	/* Terminal supports setting the scroll window */
extern int memory_below_screen;	/* Terminal remembers lines scrolled off bottom */
extern int fast_clear_end_of_line; /* Terminal has a `ce' string */

extern int dont_calculate_costs; /* Nonzero means don't bother computing */
				/* various cost tables; we won't use them.  */

/* Nonzero means no need to redraw the entire screen on resuming
   a suspended Emacs.  This is useful on terminals with multiple pages,
   where one page is used for Emacs and another for all else. */
extern int no_redraw_on_reenter;

/* DCICcost[n] is cost of inserting N characters.
   DCICcost[-n] is cost of deleting N characters. */

#define DCICcost (&DC_ICcost[screen_width])
extern int *DC_ICcost;
