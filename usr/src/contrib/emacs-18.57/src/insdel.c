/* Buffer insertion/deletion and gap motion for GNU Emacs.
   Copyright (C) 1985, 1986, 1990 Free Software Foundation, Inc.

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

/* Move gap to position `pos'.
   Note that this can quit!  */

move_gap (pos)
     int pos;
{
  if (pos < GPT)
    gap_left (pos, 0);
  else if (pos > GPT)
    gap_right (pos);
}

/* Move the gap to POS, which is less than the current GPT.
   If NEWGAP is nonzero, then don't update beg_unchanged and end_unchanged.  */

gap_left (pos, newgap)
     register int pos;
     int newgap;
{
  register unsigned char *to, *from;
  register int i;
  int new_s1;

  pos--;

  if (!newgap)
    {
      if (unchanged_modified == MODIFF)
	{
	  beg_unchanged = pos;
	  end_unchanged = Z - pos - 1;
	}
      else
	{
	  if (Z - GPT < end_unchanged)
	    end_unchanged = Z - GPT;
	  if (pos < beg_unchanged)
	    beg_unchanged = pos;
	}
    }

  i = GPT;
  to = GAP_END_ADDR;
  from = GPT_ADDR;
  new_s1 = GPT - BEG;

  /* Now copy the characters.  To move the gap down,
     copy characters up.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = new_s1 - pos;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
      new_s1 -= i;
      while (--i >= 0)
	*--to = *--from;
    }

  /* Adjust markers, and buffer data structure, to put the gap at POS.
     POS is where the loop above stopped, which may be what was specified
     or may be where a quit was detected.  */
  adjust_markers (pos + 1, GPT, GAP_SIZE);
  GPT = pos + 1;
  QUIT;
}

gap_right (pos)
     register int pos;
{
  register unsigned char *to, *from;
  register int i;
  int new_s1;

  pos--;

  if (unchanged_modified == MODIFF)
    {
      beg_unchanged = pos;
      end_unchanged = Z - pos - 1;
    }
  else
    {
      if (Z - pos - 1 < end_unchanged)
	end_unchanged = Z - pos - 1;
      if (GPT - BEG < beg_unchanged)
	beg_unchanged = GPT - BEG;
    }

  i = GPT;
  from = GAP_END_ADDR;
  to = GPT_ADDR;
  new_s1 = GPT - 1;

  /* Now copy the characters.  To move the gap up,
     copy characters down.  */

  while (1)
    {
      /* I gets number of characters left to copy.  */
      i = pos - new_s1;
      if (i == 0)
	break;
      /* If a quit is requested, stop copying now.
	 Change POS to be where we have actually moved the gap to.  */
      if (QUITP)
	{
	  pos = new_s1;
	  break;
	}
      /* Move at most 32000 chars before checking again for a quit.  */
      if (i > 32000)
	i = 32000;
      new_s1 += i;
      while (--i >= 0)
	*to++ = *from++;
    }

  adjust_markers (GPT + GAP_SIZE, pos + 1 + GAP_SIZE, - GAP_SIZE);
  GPT = pos + 1;
  QUIT;
}

/* Add `amount' to the position of every marker in the current buffer
   whose current position is between `from' (exclusive) and `to' (inclusive).
   Also, any markers past the outside of that interval, in the direction
   of adjustment, are first moved back to the near end of the interval
   and then adjusted by `amount'.  */

adjust_markers (from, to, amount)
     register int from, to, amount;
{
  Lisp_Object marker;
  register struct Lisp_Marker *m;
  register int mpos;

  marker = current_buffer->markers;

  while (!NULL (marker))
    {
      m = XMARKER (marker);
      mpos = m->bufpos;
      if (amount > 0)
	{
	  if (mpos > to && mpos < to + amount)
	    mpos = to + amount;
	}
      else
	{
	  if (mpos > from + amount && mpos <= from)
	    mpos = from + amount;
	}
      if (mpos > from && mpos <= to)
	mpos += amount;
      m->bufpos = mpos;
      marker = m->chain;
    }
}

/* Make the gap INCREMENT characters longer.  */

make_gap (increment)
     int increment;
{
  unsigned char *memory;
  Lisp_Object tem;
  int real_gap_loc;
  int old_gap_size;

  /* If we have to get more space, get enough to last a while.  */
  increment += 2000;

  memory = (unsigned char *) realloc (BEG_ADDR,
				      Z - BEG + GAP_SIZE + increment);
  if (memory == 0)
    memory_full ();
  BEG_ADDR = memory;

  /* Prevent quitting in move_gap.  */
  tem = Vinhibit_quit;
  Vinhibit_quit = Qt;

  real_gap_loc = GPT;
  old_gap_size = GAP_SIZE;
  /* Call the newly allocated space a gap at the end of the whole space.  */
  GPT = Z + GAP_SIZE;
  GAP_SIZE = increment;
  /* Move the new gap down to be consecutive with the end of the old one.
     This adjusts the markers properly too.  */
  gap_left (real_gap_loc + old_gap_size, 1);
  /* Now combine the two into one large gap.  */
  GAP_SIZE += old_gap_size;
  GPT = real_gap_loc;

  Vinhibit_quit = tem;
}

/* Insert the character c before point */

insert_char (c)
     unsigned char c;
{
  insert (&c, 1);
}

/* Insert the null-terminated string s before point */

InsStr (s)
     char *s;
{
  insert (s, strlen (s));
}

/* Insert a string of specified length before point */

insert (string, length)
     register unsigned char *string;
     register length;
{
  register Lisp_Object temp;

  if (length < 1)
    return;

  /* Make sure point-max won't overflow after this insertion.  */
  XSET (temp, Lisp_Int, length + Z);
  if (length + Z != XINT (temp))
    error ("maximum buffer size exceeded");

  prepare_to_modify_buffer ();

  if (point != GPT)
    move_gap (point);
  if (GAP_SIZE < length)
    make_gap (length - GAP_SIZE);

  record_insert (point, length);
  MODIFF++;

  bcopy (string, GPT_ADDR, length);

  GAP_SIZE -= length;
  GPT += length;
  ZV += length;
  Z += length;
  point += length;
}

/* like insert except that all markers pointing at the place where
   the insertion happens are adjusted to point after it.  */

insert_before_markers (string, length)
     unsigned char *string;
     register int length;
{
  register int opoint = point;
  insert (string, length);
  adjust_markers (opoint - 1, opoint, length);
}

/* Delete characters in current buffer
  from `from' up to (but not incl) `to' */

del_range (from, to)
     register int from, to;
{
  register int numdel;

  /* Make args be valid */
  if (from < BEGV)
    from = BEGV;
  if (to > ZV)
    to = ZV;

  if ((numdel = to - from) <= 0)
    return;

  /* Make sure the gap is somewhere in or next to what we are deleting */
  if (from > GPT)
    gap_right (from);
  if (to < GPT)
    gap_left (to, 0);

  prepare_to_modify_buffer ();
  record_delete (from, numdel);
  MODIFF++;

  /* Relocate point as if it were a marker.  */
  if (from < point)
    {
      if (point < to)
	point = from;
      else
	point -= numdel;
    }

  /* Relocate all markers pointing into the new, larger gap
     to point at the end of the text before the gap.  */
  adjust_markers (to + GAP_SIZE, to + GAP_SIZE, - numdel - GAP_SIZE);

  GAP_SIZE += numdel;
  ZV -= numdel;
  Z -= numdel;
  GPT = from;

  if (GPT - BEG < beg_unchanged)
    beg_unchanged = GPT - BEG;
  if (Z - GPT < end_unchanged)
    end_unchanged = Z - GPT;
}

modify_region (start, end)
     int start, end;
{
  prepare_to_modify_buffer ();
  if (start - 1 < beg_unchanged || unchanged_modified == MODIFF)
    beg_unchanged = start - 1;
  if (Z - end < end_unchanged
      || unchanged_modified == MODIFF)
    end_unchanged = Z - end;
  MODIFF++;
}

prepare_to_modify_buffer ()
{
  if (!NULL (current_buffer->read_only))
    Fbarf_if_buffer_read_only();

#ifdef CLASH_DETECTION
  if (!NULL (current_buffer->filename)
      && current_buffer->save_modified >= MODIFF)
    lock_file (current_buffer->filename);
#else
  /* At least warn if this file has changed on disk since it was visited.  */
  if (!NULL (current_buffer->filename)
      && current_buffer->save_modified >= MODIFF
      && NULL (Fverify_visited_file_modtime (Fcurrent_buffer ()))
      && !NULL (Ffile_exists_p (current_buffer->filename)))
    call1 (intern ("ask-user-about-supersession-threat"),
	   current_buffer->filename);
#endif /* not CLASH_DETECTION */
}
