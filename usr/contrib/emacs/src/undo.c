/* Support routines for the undo facility.
   Copyright (C) 1985 Fen Labalme and Richard M. Stallman.

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
#include "undo.h"
#include "commands.h"
#include "buffer.h"

/* Access undo records of current buffer */
/* These assume that `u' points to the buffer's undodata */
#define UndoRQ (u->undorecs)
#define UndoCQ (u->undochars)
#define FillRQ (u->nextrec)
#define FillCQ (u->nextchar)

/* Record last undo record made, and what buffer made in */
static struct UndoRec *LastUndoRec;
static struct buffer *LastUndoBuf;

/* Record progress of undoing */
static NUndone;
static NCharsLeft;
static LastUndoneC;
static LastUndone;
static struct buffer *LastUndoneBuf;

Lisp_Object Fundo_boundary ();

make_undo_records (b)
     struct buffer *b;
{
  register struct UndoData *u;
  b->undodata = u = (struct UndoData *) xmalloc (sizeof (struct UndoData));
  u->undorecs
    = (struct UndoRec *) xmalloc (sizeof (struct UndoRec) * InitNUndoR);
  u->undochars = (char *) xmalloc (InitNUndoC);
  u->undorecs[InitNUndoR - 1].kind = Unundoable;
  u->nextrec = 0;
  u->nextchar = 0;
  u->num_undorecs = InitNUndoR;
  u->num_undochars = InitNUndoC;
}

free_undo_records (b)
     struct buffer *b;
{
  register struct UndoData *u = b->undodata;
  free (u->undorecs);
  free (u->undochars);
  free (u);
}

struct UndoRec *
NewUndo (kind, pos, len)
     enum Ukinds kind;
{
  register struct UndoData *u = bf_cur->undodata;
  register struct UndoRec *p = &UndoRQ[FillRQ];
  register struct UndoRec *np;

  FillRQ++;
  if (FillRQ >= NUndoR)
    FillRQ = 0;
  else if (FillRQ >= u->num_undorecs)
    {
      np = (struct UndoRec *) xrealloc (UndoRQ, NUndoR * sizeof *p);
      if (np)
	{
	  UndoRQ = np;
	  p = &UndoRQ[FillRQ-1];
	  u->num_undorecs = NUndoR;
	  np[NUndoR - 1].kind = Unundoable;
	}
      else
	FillRQ = 0;
    }
  UndoRQ[FillRQ].kind = Unundoable;
  p -> kind = kind;
  p -> pos = pos;
  p -> len = len;
  LastUndoRec = p;
  LastUndoBuf = bf_cur;
  if (kind != Uboundary)
    LastUndone = -1;
  return p;
}

RecordInsert (pos, n)
{
  register struct UndoRec *p = LastUndoRec;
  if (!bf_cur->undodata)
    return;
  if (LastUndoBuf != bf_cur)
    {
      Fundo_boundary ();
      p = 0;
    }

  if (bf_modified <= bf_cur->save_modified)
    NewUndo (Uunmod, pos, 0);

  if (p && p -> kind == Udelete && p -> pos + p -> len == pos)
    p -> len += n;
  else
    NewUndo (Udelete, pos, n);
}

RecordDelete (pos, n)
     int pos, n;
{
  register struct UndoRec *p = LastUndoRec;

  if (!bf_cur->undodata)
    return;
  if (LastUndoBuf != bf_cur)
    {
      Fundo_boundary ();
      p = 0;
    }

  if (bf_modified <= bf_cur->save_modified)
    NewUndo (Uunmod, pos, 0);

  if (p && p->kind == Uinsert && p->pos == pos)
    p->len += n;
  else
    NewUndo (Uinsert, pos, n);

  record_chars (pos, n);
}

record_chars (pos, n)
     register int pos, n;
{
  if (pos < bf_s1 + 1 && pos + n > bf_s1 + 1)
    {
      record_block (&CharAt (pos), bf_s1 + 1 - pos);
      n -= bf_s1 + 1 - pos;
      pos = bf_s1 + 1;
    }

  record_block (&CharAt (pos), n);
}

record_block (p, n)
     register char *p;
     register int n;
{
  register char *cp;
  register struct UndoData *u = bf_cur->undodata;
  register int i;

  NCharsLeft -= n;
  cp = &UndoCQ[FillCQ];

  while (n > 0)
    {
      i = u->num_undochars - FillCQ;
      if (i > n) i = n;
      if (i > 0)
	{
	  bcopy (p, cp, i);
	  p += i;
	  cp += i;
	  FillCQ += i;
	  n -= i;
	}

      if (FillCQ >= NUndoC)
	{
	  FillCQ = 0;
	  cp = UndoCQ;
	}
      else
	{
	  cp = (char *) xrealloc (UndoCQ, NUndoC);
	  UndoCQ = cp;
	  cp += FillCQ;
	  NCharsLeft += NUndoC - u->num_undochars;
	  u->num_undochars = NUndoC;
	}
    }
}

RecordChange (pos, n)
     int pos, n;
{
  register struct UndoRec *p = LastUndoRec;
  if (!bf_cur->undodata)
    return;
  if (LastUndoBuf != bf_cur)
    {
      Fundo_boundary ();
      p = 0;
    }

  if (bf_modified <= bf_cur->save_modified)
    NewUndo (Uunmod, pos, 0);

  if (p && p -> kind == Uchange && p -> pos + p -> len == pos)
    p -> len += n;
  else
    NewUndo (Uchange, pos, n);

  record_chars (pos, n);
}

RecordChange1 (pos, bufp, n)
     int pos;
     char *bufp;
     int n;
{
  register struct UndoRec *p = LastUndoRec;
  if (!bf_cur->undodata)
    return;
  if (LastUndoBuf != bf_cur)
    {
      Fundo_boundary ();
      p = 0;
    }
  if (p && p -> kind == Uchange && p -> pos + p -> len == pos)
    p -> len += n;
  else
    NewUndo (Uchange, pos, n);

  record_block (bufp, n);
}

DoneIsDone ()
{
  register struct UndoData *u = bf_cur->undodata;
  register struct UndoRec *p;

  if (!u)
    return 0;

  p = &UndoRQ[(FillRQ + u->num_undorecs - 1) % u->num_undorecs];
  if (p->kind != Unundoable)
    NewUndo (Unundoable, point, 0);
  return 0;
}

DEFUN ("undo-boundary", Fundo_boundary, Sundo_boundary, 0, 0, 0,
  "Mark a boundary between units of undo.\n\
An undo command will stop at this point,\n\
but another undo command will undo to the previous boundary.")
  ()
{
  register struct UndoData *u = bf_cur->undodata;
  register struct UndoRec *p;

  if (!u)
    return Qnil;

  p = &UndoRQ[(FillRQ + u->num_undorecs - 1) % u->num_undorecs];
  if (p->kind != Uboundary)
    NewUndo (Uboundary, point, 0);
  return Qnil;
}

DEFUN ("undo-more", Fundo_more, Sundo_more, 1, 1, 0,
  "Undo back N undo-boundaries beyond what was already undone recently.\n\
Call undo-start to get ready to undo recent changes,\n\
then call undo-more one or more times to undo them.")
  (pfxarg)
     Lisp_Object pfxarg;
{
  register struct UndoData *u = bf_cur->undodata;
  register int n = 0;
  register int chars;
  register int i = LastUndone;
  register int arg = XINT (pfxarg);
  register int len, pos;
  char tembuf[NUndoC];

  if (!u)
    return Qnil;

  if (LastUndoneBuf != bf_cur ||
      i == -1)
    error ("Cannot undo more: changes have been made since the last undo");

  while (1)
    {
      while (UndoRQ[i = (!i ? u->num_undorecs-1 : i-1)].kind != Uboundary)
	{
	  if (((UndoRQ[i].kind == Uinsert || UndoRQ[i].kind == Uchange)
	       && (NCharsLeft -= UndoRQ[i].len) < 0)
	      || UndoRQ[i].kind == Unundoable || NUndone >= u->num_undorecs)
	    error ("No further undo information available");
	  NUndone++;
	  n++;
	}
      NUndone++;
      n++;
      if (--arg <= 0)
	break;
    }

  i = LastUndone;
  chars = LastUndoneC;
  while (--n >= 0)
    {
      if (!i)
	i = u->num_undorecs;
      i--;

      len = UndoRQ[i].len;
      pos = UndoRQ[i].pos;
#ifdef SWITCH_ENUM_BUG
      switch ((int) UndoRQ[i].kind)
#else
      switch (UndoRQ[i].kind)
#endif
	{
	case Uboundary: 
	  break;

	case Udelete: 
	  if (pos < FirstCharacter
	      || pos + len > NumCharacters + 1)
	    error ("Changes to be undone are outside visible portion of buffer");
	  SetPoint (pos);
	  del_range (point, point + len);
	  break;

	case Uchange:
	  if (pos < FirstCharacter
	      || pos + len > NumCharacters + 1)
	    error ("Changes to be undone are outside visible portion of buffer");
	  SetPoint (pos);
	  if (len > NUndoC)
	    /* Should have already said "No more undo info available" */
	    abort ();
	  save_undone_chars (pos, len, tembuf);
	  chars -= len;
	  if (chars < 0)
	    {
	      replace_chars (point - chars, len + chars, UndoCQ);
	      replace_chars (point, - chars, UndoCQ + chars + u->num_undochars);
	      chars += u->num_undochars;
	    }
	  else
	    replace_chars (point, len, UndoCQ + chars);
	  RecordChange1 (point, tembuf, len);
	  break;

	case Uinsert:
	  if (pos < FirstCharacter
	      || pos > NumCharacters + 1)
	    error ("Changes to be undone are outside visible portion of buffer");
	  SetPoint (pos);
	  chars -= len;
	  if (chars < 0)
	    {
	      InsCStr (UndoCQ + chars + u->num_undochars, - chars);
	      InsCStr (UndoCQ, len + chars);
	      chars += u->num_undochars;
	    }
	  else
	    InsCStr (UndoCQ + chars, len);
	  SetPoint (pos);
	  break;

	case Uunmod:
#ifdef CLASH_DETECTION
	  Funlock_buffer ();
#endif /* CLASH_DETECTION */
	  bf_cur->save_modified = bf_modified;
	  RedoModes++;
	  break;

	default: 
	  error ("Something rotten in undo");
	  return Qnil;
	}
    }
  LastUndone = i;
  LastUndoneC = chars;
  return Qnil;
}

replace_chars (pos, n, string)
     register int pos, n;
     register unsigned char *string;
{
  modify_region (pos, pos + n);
  while (--n >= 0)
    {
      CharAt (pos) = *string++;
      pos++;
    }
}

save_undone_chars (pos, n, p)
     register int pos, n;
     register char *p;
{
  if (pos < bf_s1 + 1 && pos + n > bf_s1 + 1)
    {
      bcopy (&CharAt (pos), p, bf_s1 + 1 - pos);
      p += bf_s1 + 1 - pos;
      n -= bf_s1 + 1 - pos;
      pos = bf_s1 + 1;
    }

  bcopy (&CharAt (pos), p, n);
}

DEFUN ("undo-start", Fundo_start, Sundo_start, 0, 0, 0,
  "Move undo-pointer to front of undo records.\n\
The next call to undo-more will undo the most recently made change.")
  ()
{
  register struct UndoData *u = bf_cur->undodata;

  if (!u)
    error ("Undo information not kept for this buffer");
  LastUndoneBuf = bf_cur;
  NCharsLeft = u->num_undochars;
  NUndone = 0;
  LastUndone = FillRQ;
  LastUndoneC = FillCQ;
  return Qnil;
}

syms_of_undo ()
{
  defsubr (&Sundo_start);
  defsubr (&Sundo_boundary);
  defsubr (&Sundo_more);
}
