/* Markers: examining, setting and killing.
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


#include "config.h"
#include "lisp.h"
#include "buffer.h"

/* Operations on markers. */

DEFUN ("marker-buffer", Fmarker_buffer, Smarker_buffer, 1, 1, 0,
  "Return the buffer that MARKER points into, or nil if none.\n\
Returns nil if MARKER points into a dead buffer.")
  (marker)
     Lisp_Object marker;
{
  Lisp_Object buf;
  CHECK_MARKER (marker, 0);
  if (XMARKER (marker)->buffer)
    {
      XSET (buf, Lisp_Buffer, XMARKER (marker)->buffer);
      /* Return marker's buffer only if it is not dead.  */
      if (!NULL (XBUFFER (buf)->name))
	return buf;
    }
  return Qnil;
}

DEFUN ("marker-position", Fmarker_position, Smarker_position, 1, 1, 0,
  "Return the position MARKER points at, as a character number.")
  (marker)
     Lisp_Object marker;
{
  Lisp_Object pos;
  int i;
  struct buffer *buf;
  struct buffer_text *text;

  CHECK_MARKER (marker, 0);
  if (XMARKER (marker)->buffer)
    {
      buf = XMARKER (marker)->buffer;
      i = XMARKER (marker)->bufpos;
      text = (buf == bf_cur) ? &bf_text : &buf->text;

      if (i > text->size1 + text->gap + 1)
	i -= text->gap;
      else if (i > text->size1 + 1)
	i = text->size1 + 1;

      if (i < 1 || i > text->size1 + text->size2 + 1)
	abort ();

      XFASTINT (pos) = i;
      return pos;
    }
  return Qnil;
}

DEFUN ("set-marker", Fset_marker, Sset_marker, 2, 3, 0,
  "Position MARKER before character number NUMBER in BUFFER.\n\
BUFFER defaults to the current buffer.\n\
If NUMBER is nil, makes marker point nowhere.\n\
Then it no longer slows down editing in any buffer.\n\
Returns MARKER.")
  (marker, pos, buffer)
     Lisp_Object marker, pos, buffer;
{
  int charno;
  register struct buffer *b;
  register struct buffer_text *text;
  register struct Lisp_Marker *m;

  CHECK_MARKER (marker, 0);
  if (NULL (pos))
    {
      unchain_marker (marker);
      XMARKER (marker)->buffer = 0;
      return marker;
    }

  CHECK_NUMBER_COERCE_MARKER (pos, 1);
  if (NULL (buffer))
    b = bf_cur;
  else
    {
      CHECK_BUFFER (buffer, 1);
      b = XBUFFER (buffer);
    }

  charno = XINT (pos);
  m = XMARKER (marker);

  if (bf_cur == b)
    text = &bf_text;
  else
    text = &b->text;

  if (charno < text->head_clip) charno = text->head_clip;
  if (charno > text->size1 + text->size2 + 1 - text->tail_clip)
    charno = text->size1 + text->size2 + 1 - text->tail_clip;
  if (charno > text->size1 + 1) charno += text->gap;
  m->bufpos = charno;

  if (m->buffer != b)
    {
      unchain_marker (marker);
      m->chain = b->markers;
      b->markers = marker;
      m->buffer = b;
    }
  
  return marker;
}

/* This is called during garbage collection,
 so we must be careful to ignore and preserve mark bits,
 including those in chain fields of markers.  */

unchain_marker (marker)
     Lisp_Object marker;
{
  Lisp_Object tail, prev, next;
  register int omark;

  if (!XMARKER (marker)->buffer)
    return;

  tail = XMARKER (marker)->buffer->markers;
  prev = Qnil;
  while (XSYMBOL (tail) != XSYMBOL (Qnil))
    {
      next = XMARKER (tail)->chain;
      XUNMARK (next);

      if (XMARKER (marker) == XMARKER (tail))
	{
	  if (NULL (prev))
	    XMARKER (marker)->buffer->markers = next;
	  else
	    {
	      omark = XMARKBIT (XMARKER (prev)->chain);
	      XMARKER (prev)->chain = next;
	      XSETMARKBIT (XMARKER (prev)->chain, omark);
	    }
	  break;
	}
      else
	prev = tail;
      tail = next;
    }
  XMARKER (marker)->buffer = 0;
}

marker_position (marker)
     Lisp_Object marker;
{
  register struct Lisp_Marker *m = XMARKER (marker);
  register struct buffer *buf = m->buffer;
  register int i = m->bufpos;
  register struct buffer_text *text
    = (buf == bf_cur) ? &bf_text : &buf->text;

  if (!buf)
    error ("Marker does not point anywhere");

  if (i > text->size1 + text->gap + 1)
    i -= text->gap;
  else if (i > text->size1 + 1)
    i = text->size1 + 1;

  if (i < 1 || i > text->size1 + text->size2 + 1)
    abort ();

  return i;
}

DEFUN ("copy-marker", Fcopy_marker, Scopy_marker, 1, 1, 0,
  "Return a new marker pointing at the same place as MARKER.\n\
If argument is a number, makes a new marker pointing\n\
at that position in the current buffer.")
  (marker)
     Lisp_Object marker;
{
  Lisp_Object new;

  while (1)
    {
      if (XTYPE (marker) == Lisp_Int
	  || XTYPE (marker) == Lisp_Marker)
	{
	  new = Fmake_marker ();
	  Fset_marker (new, marker,
		       XTYPE (marker) == Lisp_Marker
		       ? Fmarker_buffer (marker)
		       : Qnil);
	  return new;
	}
      else
	marker = wrong_type_argument (Qinteger_or_marker_p, marker);
    }
}

syms_of_marker ()
{
  defsubr (&Smarker_position);
  defsubr (&Smarker_buffer);
  defsubr (&Sset_marker);
  defalias (&Sset_marker, "move-marker");
  defsubr (&Scopy_marker);
}
