/* modechange.h -- definitions for file mode manipulation
   Copyright (C) 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Masks for the `flags' field in a `struct mode_change'. */

/* Affect the execute bits only if at least one execute bit is set already,
   or if the file is a directory. */
#define MODE_X_IF_ANY_X 01

/* If set, copy some existing permissions for u, g, or o onto the other two.
   Which of u, g, or o is copied is determined by which bits are set in the
   `value' field. */
#define MODE_COPY_EXISTING 02

struct mode_change
{
  char op;			/* One of "=+-". */
  char flags;			/* Special operations. */
  unsigned short affected;	/* Set for u/g/o/s/s/t, if to be affected. */
  unsigned short value;		/* Bits to add/remove. */
  struct mode_change *next;	/* Link to next change in list. */
};

/* Masks for mode_compile argument. */
#define MODE_MASK_EQUALS 1
#define MODE_MASK_PLUS 2
#define MODE_MASK_MINUS 4

/* Error return values for mode_compile. */
#define MODE_INVALID (struct mode_change *) 0
#define MODE_MEMORY_EXHAUSTED (struct mode_change *) 1

#ifdef __STDC__
struct mode_change *mode_compile (char *, unsigned);
unsigned short mode_adjust (unsigned, struct mode_change *);
void mode_free (struct mode_change *);
#else
struct mode_change *mode_compile ();
unsigned short mode_adjust ();
void mode_free ();
#endif
