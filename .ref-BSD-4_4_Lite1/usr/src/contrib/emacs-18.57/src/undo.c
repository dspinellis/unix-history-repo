/* undo handling for GNU Emacs.
   Copyright (C) 1990 Free Software Foundation, Inc.

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

/* Last buffer for which undo information was recorded.  */
Lisp_Object last_undo_buffer;

/* Record an insertion that just happened or is about to happen,
   for LENGTH characters at position BEG.
   (It is possible to record an insertion before or after the fact
   because we don't need to record the contents.)  */

record_insert (beg, length)
     Lisp_Object beg, length;
{
  Lisp_Object lbeg, lend;

  if (current_buffer != XBUFFER (last_undo_buffer))
    Fundo_boundary ();
  XSET (last_undo_buffer, Lisp_Buffer, current_buffer);

  if (EQ (current_buffer->undo_list, Qt))
    return;
  if (MODIFF <= current_buffer->save_modified)
    record_first_change ();

  /* If this is following another insertion and consecutive with it
     in the buffer, combine the two.  */
  if (XTYPE (current_buffer->undo_list) == Lisp_Cons)
    {
      Lisp_Object elt;
      elt = XCONS (current_buffer->undo_list)->car;
      if (XTYPE (elt) == Lisp_Cons
	  && XTYPE (XCONS (elt)->car) == Lisp_Int
	  && XTYPE (XCONS (elt)->cdr) == Lisp_Int
	  && XINT (XCONS (elt)->cdr) == beg)
	{
	  XSETINT (XCONS (elt)->cdr, beg + length);
	  return;
	}
    }

  XFASTINT (lbeg) = beg;
  XFASTINT (lend) = beg + length;
  current_buffer->undo_list = Fcons (Fcons (lbeg, lend), current_buffer->undo_list);
}

/* Record that a deletion is about to take place,
   for LENGTH characters at location BEG.  */

record_delete (beg, length)
     int beg, length;
{
  Lisp_Object lbeg, llength, lend, sbeg;

  if (current_buffer != XBUFFER (last_undo_buffer))
    Fundo_boundary ();
  XSET (last_undo_buffer, Lisp_Buffer, current_buffer);

  if (EQ (current_buffer->undo_list, Qt))
    return;
  if (MODIFF <= current_buffer->save_modified)
    record_first_change ();

  if (point == beg + length)
    XSET (sbeg, Lisp_Int, -beg);
  else
    XFASTINT (sbeg) = beg;
  XFASTINT (lbeg) = beg;
  XFASTINT (llength) = length;
  XFASTINT (lend) = beg + length;
  current_buffer->undo_list = Fcons (Fcons (Fbuffer_substring (lbeg, lend), sbeg),
			     current_buffer->undo_list);
}

/* Record that a replacement is about to take place,
   for LENGTH characters at location BEG.
   The replacement does not change the number of characters.  */

record_change (beg, length)
     int beg, length;
{
  record_delete (beg, length);
  record_insert (beg, length);
}

/* Record that an unmodified buffer is about to be changed.
   Record the file modification date so that when undoing this entry
   we can tell whether it is obsolete because the file was saved again.  */

record_first_change ()
{
  Lisp_Object high, low;
  XFASTINT (high) = (current_buffer->modtime >> 16) & 0xffff;
  XFASTINT (low) = current_buffer->modtime & 0xffff;
  current_buffer->undo_list = Fcons (Fcons (Qt, Fcons (high, low)), current_buffer->undo_list);
}

DEFUN ("undo-boundary", Fundo_boundary, Sundo_boundary, 0, 0, 0,
  "Mark a boundary between units of undo.\n\
An undo command will stop at this point,\n\
but another undo command will undo to the previous boundary.")
  ()
{
  Lisp_Object tem;
  if (EQ (current_buffer->undo_list, Qt))
    return Qnil;
  tem = Fcar (current_buffer->undo_list);
  if (!NULL (tem))
    current_buffer->undo_list = Fcons (Qnil, current_buffer->undo_list);
  return Qnil;
}

/* At garbage collection time, make an undo list shorter at the end,
   returning the truncated list.
   MINSIZE and MAXSIZE are the limits on size allowed, as described below.
   In practice, these are the values of undo-threshold and
   undo-high-threshold.  */

Lisp_Object
truncate_undo_list (list, minsize, maxsize)
     Lisp_Object list;
     int minsize, maxsize;
{
  Lisp_Object prev, next, save_prev;
  int size_so_far = 0;

  prev = Qnil;
  next = list;
  save_prev = Qnil;

  while (XTYPE (next) == Lisp_Cons)
    {
      Lisp_Object elt;
      elt = XCONS (next)->car;

      /* When we get to a boundary, decide whether to truncate
	 either before or after it.  The lower threshold, MINSIZE,
	 tells us to truncate after it.  If its size pushes past
	 the higher threshold MAXSIZE as well, we truncate before it.  */
      if (NULL (elt))
	{
	  if (size_so_far > maxsize)
	    break;
	  save_prev = prev;
	  if (size_so_far > minsize)
	    break;
	}

      /* Add in the space occupied by this element and its chain link.  */
      size_so_far += 8;
      if (XTYPE (elt) == Lisp_Cons)
	{
	  size_so_far += 8;
	  if (XTYPE (XCONS (elt)->car) == Lisp_String)
	    size_so_far += 6 + XSTRING (XCONS (elt)->car)->size;
	}

      /* Advance to next element.  */
      prev = next;
      next = XCONS (next)->cdr;
    }

  /* If we scanned the whole list, it is short enough; don't change it.  */
  if (NULL (next))
    return list;

  /* Truncate at the boundary where we decided to truncate.  */
  if (!NULL (save_prev))
    {
      XCONS (save_prev)->cdr = Qnil;
      return list;
    }
  else
    return Qnil;
}

DEFUN ("primitive-undo", Fprimitive_undo, Sprimitive_undo, 2, 2, 0,
  "Undo N records from the front of the list LIST.\n\
Return what remains of the list.")
  (count, list)
     Lisp_Object count, list;
{
  register int arg = XINT (count);
#if 0  /* This is a good feature, but would make undo-start
	  unable to do what is expected.  */
  Lisp_Object tem;

  /* If the head of the list is a boundary, it is the boundary
     preceding this command.  Get rid of it and don't count it.  */
  tem = Fcar (list);
  if (NULL (tem))
    list = Fcdr (list);
#endif

  while (arg > 0)
    {
      while (1)
	{
	  Lisp_Object next, car, cdr;
	  next = Fcar (list);
	  list = Fcdr (list);
	  if (NULL (next))
	    break;
	  car = Fcar (next);
	  cdr = Fcdr (next);
	  if (EQ (car, Qt))
	    {
	      Lisp_Object high, low;
	      int mod_time;
	      high = Fcar (cdr);
	      low = Fcdr (cdr);
	      mod_time = (high << 16) + low;
	      /* If this records an obsolete save
		 (not matching the actual disk file)
		 then don't mark unmodified.  */
	      if (mod_time != current_buffer->modtime)
		break;
#ifdef CLASH_DETECTION
	      Funlock_buffer ();
#endif /* CLASH_DETECTION */
	      Fset_buffer_modified_p (Qnil);
	    }
	  else if (XTYPE (car) == Lisp_Int && XTYPE (cdr) == Lisp_Int)
	    {
	      Lisp_Object end;
	      if (XINT (car) < BEGV
		  || XINT (cdr) > ZV)
		error ("Changes to be undone are outside visible portion of buffer");
	      Fdelete_region (car, cdr);
	      Fgoto_char (car);
	    }
	  else if (XTYPE (car) == Lisp_String && XTYPE (cdr) == Lisp_Int)
	    {
	      Lisp_Object membuf;
	      int pos = XINT (cdr);
	      membuf = car;
	      if (pos < 0)
		{
		  if (-pos < BEGV || -pos > ZV)
		    error ("Changes to be undone are outside visible portion of buffer");
		  SET_PT (-pos);
		  Finsert (1, &membuf);
		}
	      else
		{
		  if (pos < BEGV || pos > ZV)
		    error ("Changes to be undone are outside visible portion of buffer");
		  SET_PT (pos);
		  Finsert (1, &membuf);
		  SET_PT (pos);
		}
	    }
	}
      arg--;
    }

  return list;
}

syms_of_undo ()
{
  defsubr (&Sprimitive_undo);
  defsubr (&Sundo_boundary);
}
