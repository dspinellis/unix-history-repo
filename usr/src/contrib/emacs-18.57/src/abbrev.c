/* Primitives for word-abbrev mode.
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


#include "config.h"
#include <stdio.h>
#undef NULL
#include "lisp.h"
#include "commands.h"
#include "buffer.h"
#include "window.h"

/* An abbrev table is an obarray.
 Each defined abbrev is represented by a symbol in that obarray
 whose print name is the abbreviation.
 The symbol's value is a string which is the expansion.
 If its function definition is non-nil, it is called
  after the expansion is done.
 The plist slot of the abbrev symbol is its usage count. */

/* List of all abbrev-table name symbols:
 symbols whose values are abbrev tables.  */

Lisp_Object Vabbrev_table_name_list;

/* The table of global abbrevs.  These are in effect
 in any buffer in which abbrev mode is turned on. */

Lisp_Object Vglobal_abbrev_table;

/* The local abbrev table used by default (in Fundamental Mode buffers) */

Lisp_Object Vfundamental_mode_abbrev_table;

/* Set nonzero when an abbrev definition is changed */

int abbrevs_changed;

int abbrev_all_caps;

/* Non-nil => use this location as the start of abbrev to expand
 (rather than taking the word before point as the abbrev) */

Lisp_Object Vabbrev_start_location;

/* Buffer that Vabbrev_start_location applies to */
Lisp_Object Vabbrev_start_location_buffer;

/* The symbol representing the abbrev most recently expanded */

Lisp_Object Vlast_abbrev;

/* A string for the actual text of the abbrev most recently expanded.
   This has more info than Vlast_abbrev since case is significant.  */

Lisp_Object Vlast_abbrev_text;

/* Character address of start of last abbrev expanded */

int last_abbrev_point;

extern Lisp_Object oblookup ();

DEFUN ("make-abbrev-table", Fmake_abbrev_table, Smake_abbrev_table, 0, 0, 0,
  "Create a new, empty abbrev table object.")
  ()
{
  return Fmake_vector (make_number (59), make_number (0));
}

DEFUN ("clear-abbrev-table", Fclear_abbrev_table, Sclear_abbrev_table, 1, 1, 0,
  "Undefine all abbrevs in abbrev table TABLE, leaving it empty.")
  (table)
     Lisp_Object table;
{
  int i, size;

  CHECK_VECTOR (table, 0);
  size = XVECTOR (table)->size;
  abbrevs_changed = 1;
  for (i = 0; i < size; i++)
    XVECTOR (table)->contents[i] = make_number (0);
  return Qnil;
}

DEFUN ("define-abbrev", Fdefine_abbrev, Sdefine_abbrev, 3, 5, 0,
  "Define an abbrev in TABLE named NAME, to expand to EXPANSION or call HOOK.\n\
NAME and EXPANSION are strings.  HOOK is a function or nil.\n\
To undefine an abbrev, define it with EXPANSION = nil")
  (table, name, expansion, hook, count)
     Lisp_Object table, name, expansion, hook, count;
{
  Lisp_Object sym, oexp, ohook, tem;
  CHECK_VECTOR (table, 0);
  CHECK_STRING (name, 1);
  CHECK_STRING (expansion, 2);
  if (NULL (count))
    count = make_number (0);
  else
    CHECK_NUMBER (count, 0);

  sym = Fintern (name, table);

  oexp = XSYMBOL (sym)->value;
  ohook = XSYMBOL (sym)->function;
  if (!((EQ (oexp, expansion)
	 || (XTYPE (oexp) == Lisp_String && XTYPE (expansion) == Lisp_String
	     && (tem = Fstring_equal (oexp, expansion), !NULL (tem))))
	&&
	(EQ (ohook, hook)
	 || (tem = Fequal (ohook, hook), !NULL (tem)))))
    abbrevs_changed = 1;

  Fset (sym, expansion);
  Ffset (sym, hook);
  Fsetplist (sym, count);

  return name;
}

DEFUN ("define-global-abbrev", Fdefine_global_abbrev, Sdefine_global_abbrev, 2, 2,
  "sDefine global abbrev: \nsExpansion for %s: ",
  "Define ABBREV as a global abbreviation for EXPANSION.")
  (name, expansion)
     Lisp_Object name, expansion;
{
  Fdefine_abbrev (Vglobal_abbrev_table, Fdowncase (name),
		  expansion, Qnil, make_number (0));
  return name;
}

DEFUN ("define-mode-abbrev", Fdefine_mode_abbrev, Sdefine_mode_abbrev, 2, 2,
  "sDefine mode abbrev: \nsExpansion for %s: ",
  "Define ABBREV as a mode-specific abbreviation for EXPANSION.")
  (name, expansion)
     Lisp_Object name, expansion;
{
  if (NULL (current_buffer->abbrev_table))
    error ("Major mode has no abbrev table");

  Fdefine_abbrev (current_buffer->abbrev_table, Fdowncase (name),
		  expansion, Qnil, make_number (0));
  return name;
}

DEFUN ("abbrev-symbol", Fabbrev_symbol, Sabbrev_symbol, 1, 2, 0,
  "Return the symbol representing abbrev named ABBREV.\n\
Value is nil if that abbrev is not defined.\n\
Optional second arg TABLE is abbrev table to look it up in.\n\
Default is try buffer's mode-specific abbrev table, then global table.")
  (abbrev, table)
     Lisp_Object abbrev, table;
{
  Lisp_Object sym;
  CHECK_STRING (abbrev, 0);
  if (!NULL (table))
    sym = Fintern_soft (abbrev, table);
  else
    {
      sym = Qnil;
      if (!NULL (current_buffer->abbrev_table))
	sym = Fintern_soft (abbrev, current_buffer->abbrev_table);
      if (NULL (XSYMBOL (sym)->value))
	sym = Qnil;
      if (NULL (sym))
	sym = Fintern_soft (abbrev, Vglobal_abbrev_table);
    }
  if (NULL (XSYMBOL (sym)->value)) return Qnil;
  return sym;
}

DEFUN ("abbrev-expansion", Fabbrev_expansion, Sabbrev_expansion, 1, 2, 0,
  "Return the string that ABBREV expands into in the current buffer.\n\
Optionally specify an abbrev table; then ABBREV is looked up in that table only.")
  (abbrev, table)
     Lisp_Object abbrev, table;
{
  Lisp_Object sym;
  sym = Fabbrev_symbol (abbrev, table);
  if (NULL (sym)) return sym;
  return Fsymbol_value (sym);
}

/* Expand the word before point, if it is an abbrev.
  Returns 1 if an expansion is done. */

DEFUN ("expand-abbrev", Fexpand_abbrev, Sexpand_abbrev, 0, 0, "",
  "Expand the abbrev before point, if it is an abbrev.\n\
Effective when explicitly called even when abbrev-mode is not enabled.\n\
Returns t if expansion took place.")
  ()
{
  char buffer[200];
  register char *p = buffer;
  register int wordstart, idx;
  int uccount = 0, lccount = 0;
  register Lisp_Object sym;
  Lisp_Object expansion, hook, tem;

  if (XBUFFER (Vabbrev_start_location_buffer) != current_buffer)
    Vabbrev_start_location = Qnil;
  if (!NULL (Vabbrev_start_location))
    {
      tem = Vabbrev_start_location;
      CHECK_NUMBER_COERCE_MARKER (tem, 0);
      wordstart = XINT (tem);
      Vabbrev_start_location = Qnil;
      if (FETCH_CHAR (wordstart) == '-')
	del_range (wordstart, wordstart + 1);
    }
  else
    wordstart = scan_words (point, -1);

  if (!wordstart || point - wordstart >= sizeof buffer || point <= wordstart)
    return Qnil;

  for (idx = wordstart; idx < point; idx++)
    {
      register int c = FETCH_CHAR (idx);
      if (UPPERCASEP (c))
	c = DOWNCASE (c), uccount++;
      else if (! NOCASEP (c))
	lccount++;
      *p++ = c;
    }

  if (XTYPE (current_buffer->abbrev_table) == Lisp_Vector)
    sym = oblookup (current_buffer->abbrev_table, buffer, p - buffer);
  else
    XFASTINT (sym) = 0;
  if (XTYPE (sym) == Lisp_Int ||
      NULL (XSYMBOL (sym)->value))
    sym = oblookup (Vglobal_abbrev_table, buffer, p - buffer);
  if (XTYPE (sym) == Lisp_Int ||
      NULL (XSYMBOL (sym)->value))
    return Qnil;

  if (FROM_KBD && !EQ (minibuf_window, selected_window))
    {
      SET_PT (wordstart + p - buffer);
      Fundo_boundary ();
    }
  SET_PT (wordstart);
  Vlast_abbrev_text
    = Fbuffer_substring (make_number (point),
			 make_number (point + (p - buffer)));
  del_range (point, point + (p - buffer));

  /* Now sym is the abbrev symbol. */
  Vlast_abbrev = sym;
  last_abbrev_point = point;

  if (XTYPE (XSYMBOL (sym)->plist) == Lisp_Int)
    XSETINT (XSYMBOL (sym)->plist,
	     XINT (XSYMBOL (sym)->plist) + 1);	/* Increment use count */

  expansion = XSYMBOL (sym)->value;
  insert (XSTRING (expansion)->data, XSTRING (expansion)->size);

  if (uccount && !lccount)
    {
      /* Abbrev was all caps */
      /* If expansion is multiple words, normally capitalize each word */
      /* This used to be if (!... && ... >= ...) Fcapitalize; else Fupcase
	 but Megatest 68000 compiler can't handle that */
      if (!abbrev_all_caps)
	if (scan_words (point, -1) > scan_words (wordstart, 1))
	  {
	    upcase_initials_region (make_number (wordstart),
				    make_number (point));
	    goto caped;
	  }
      /* If expansion is one word, or if user says so, upcase it all. */
      Fupcase_region (make_number (wordstart), make_number (point));
    caped: ;
    }
  else if (uccount)
    {
      /* Abbrev included some caps.  Cap first initial of expansion */
      idx = point;
      SET_PT (wordstart);
      Fcapitalize_word (make_number (1));
      SET_PT (idx);
    }

  hook = XSYMBOL (sym)->function;
  if (!NULL (hook))
    call0 (hook);

  return Qt;
}

DEFUN ("unexpand-abbrev", Funexpand_abbrev, Sunexpand_abbrev, 0, 0, "",
  "Undo the expansion of the last abbrev that expanded.")
  ()
{
  int opoint = point;
  int adjust = 0;
  if (last_abbrev_point < BEGV
      || last_abbrev_point >= ZV)
    return Qnil;
  SET_PT (last_abbrev_point);
  if (XTYPE (Vlast_abbrev_text) == Lisp_String)
    {
      /* This isn't correct if Vlast_abbrev->function was used
         to do the expansion */
      Lisp_Object val;
      XSET (val, Lisp_String, XSYMBOL (Vlast_abbrev)->value);
      adjust = XSTRING (val)->size;
      del_range (point, point + adjust);
      insert (XSTRING (Vlast_abbrev_text)->data,
	       XSTRING (Vlast_abbrev_text)->size);
      adjust -= XSTRING (Vlast_abbrev_text)->size;
      Vlast_abbrev_text = Qnil;
    }
  SET_PT (last_abbrev_point < opoint ? opoint - adjust : opoint);
  return Qnil;
}

static
write_abbrev (sym, stream)
     Lisp_Object sym, stream;
{
  Lisp_Object name;
  if (NULL (XSYMBOL (sym)->value))
    return;
  insert ("    (", 5);
  XSET (name, Lisp_String, XSYMBOL (sym)->name);
  Fprin1 (name, stream);
  insert (" ", 1);
  Fprin1 (XSYMBOL (sym)->value, stream);
  insert (" ", 1);
  Fprin1 (XSYMBOL (sym)->function, stream);
  insert (" ", 1);
  Fprin1 (XSYMBOL (sym)->plist, stream);
  insert (")\n", 2);
}

static
describe_abbrev (sym, stream)
     Lisp_Object sym, stream;
{
  Lisp_Object one;

  if (NULL (XSYMBOL (sym)->value))
    return;
  one = make_number (1);
  Fprin1 (Fsymbol_name (sym), stream);
  Findent_to (make_number (15), one);
  Fprin1 (XSYMBOL (sym)->plist, stream);
  Findent_to (make_number (20), one);
  Fprin1 (XSYMBOL (sym)->value, stream);
  if (!NULL (XSYMBOL (sym)->function))
    {
      Findent_to (make_number (45), one);
      Fprin1 (XSYMBOL (sym)->function, stream);
    }
  Fterpri (stream);
}

DEFUN ("insert-abbrev-table-description",
  Finsert_abbrev_table_description, Sinsert_abbrev_table_description,
  2, 2, 0,
  "Insert before point a description of abbrev table named NAME.\n\
NAME is a symbol whose value is an abbrev table.\n\
If 2nd arg READABLE is non-nil, a readable description is inserted.\n\
Otherwise description is an expression,\n\
a call to define-abbrev-table which would\n\
define NAME exactly as it is currently defined.")
  (name, readable)
     Lisp_Object name, readable;
{
  Lisp_Object table;
  Lisp_Object stream;

  CHECK_SYMBOL (name, 0);
  table = Fsymbol_value (name);
  CHECK_VECTOR (table, 0);

  XSET (stream, Lisp_Buffer, current_buffer);

  if (!NULL (readable))
    {
      InsStr ("(");
      Fprin1 (name, stream);
      InsStr (")\n\n");
      map_obarray (table, describe_abbrev, stream);
      InsStr ("\n\n");
    }
  else
    {
      InsStr ("(define-abbrev-table '");
      Fprin1 (name, stream);
      InsStr (" '(\n");
      map_obarray (table, write_abbrev, stream);
      InsStr ("    ))\n\n");
    }

  return Qnil;
}

DEFUN ("define-abbrev-table", Fdefine_abbrev_table, Sdefine_abbrev_table,
       2, 2, 0,
  "Define TABNAME (a symbol) as an abbrev table name.\n\
Define abbrevs in it according to DEFINITIONS, a list of elements\n\
of the form (ABBREVNAME EXPANSION HOOK USECOUNT).")
  (tabname, defns)
     Lisp_Object tabname, defns;
{
  Lisp_Object name, exp, hook, count;
  Lisp_Object table, elt;

  CHECK_SYMBOL (tabname, 0);
  table = Fboundp (tabname);
  if (NULL (table) || (table = Fsymbol_value (tabname), NULL (table)))
    {
      table = Fmake_abbrev_table ();
      Fset (tabname, table);
      Vabbrev_table_name_list =
	Fcons (tabname, Vabbrev_table_name_list);
    }
  CHECK_VECTOR (table, 0);

  for (;!NULL (defns); defns = Fcdr (defns))
    {
      elt = Fcar (defns);
      name = Fcar (elt);
      elt = Fcdr (elt);
      exp = Fcar (elt);
      elt = Fcdr (elt);
      hook = Fcar (elt);
      elt = Fcdr (elt);
      count = Fcar (elt);
      Fdefine_abbrev (table, name, exp, hook, count);
    }
  return Qnil;
}

syms_of_abbrev ()
{
  DEFVAR_LISP ("abbrev-table-name-list", &Vabbrev_table_name_list,
    "List of symbols whose values are  abbrev tables.");
  Vabbrev_table_name_list = Fcons (intern ("fundamental-mode-abbrev-table"),
				   Fcons (intern ("global-abbrev-table"),
					  Qnil));

  DEFVAR_LISP ("global-abbrev-table", &Vglobal_abbrev_table,
    "The abbrev table whose abbrevs affect all buffers.\n\
Each buffer may also have a local abbrev table.\n\
If it does, the local table overrides the global one\n\
for any particular abbrev defined in both.");
  Vglobal_abbrev_table = Fmake_abbrev_table ();

  DEFVAR_LISP ("fundamental-mode-abbrev-table", &Vfundamental_mode_abbrev_table,
    "The abbrev table of mode-specific abbrevs for Fundamental Mode.");
  Vfundamental_mode_abbrev_table = Fmake_abbrev_table ();
  current_buffer->abbrev_table = Vfundamental_mode_abbrev_table;

  DEFVAR_LISP ("last-abbrev", &Vlast_abbrev,
    "The abbrev-symbol of the last abbrev expanded.");

  DEFVAR_LISP ("last-abbrev-text", &Vlast_abbrev_text,
    "The exact text of the last abbrev expanded.\n\
nil if the abbrev has already been unexpanded.");

  DEFVAR_INT ("last-abbrev-location", &last_abbrev_point,
    "The location of the last abbrev expanded.");

  Vlast_abbrev = Qnil;
  Vlast_abbrev_text = Qnil;
  last_abbrev_point = 0;

  DEFVAR_LISP ("abbrev-start-location", &Vabbrev_start_location,
    "Buffer position for expand-abbrev to use as the start of the abbrev.\n\
nil means use the word before point as the abbrev.\n\
Set to nil each time expand-abbrev is called.");
  Vabbrev_start_location = Qnil;

  DEFVAR_LISP ("abbrev-start-location-buffer", &Vabbrev_start_location_buffer,
    "Buffer that abbrev-start-location has been set for.\n\
Trying to expand an abbrev in any other buffer clears abbrev-start-location.");
  Vabbrev_start_location_buffer = Qnil;

  DEFVAR_PER_BUFFER ("local-abbrev-table", &current_buffer->abbrev_table,
    "Local (mode-specific) abbrev table of current buffer.");

  DEFVAR_BOOL ("abbrevs-changed", &abbrevs_changed,
    "Set non-nil by defining or altering any word abbrevs.");
  abbrevs_changed = 0;

  DEFVAR_BOOL ("abbrev-all-caps", &abbrev_all_caps,
    "*Set non-nil means expand multi-word abbrevs all caps if abbrev was so.");
  abbrev_all_caps = 0;

  defsubr (&Smake_abbrev_table);
  defsubr (&Sclear_abbrev_table);
  defsubr (&Sdefine_abbrev);
  defsubr (&Sdefine_global_abbrev);
  defsubr (&Sdefine_mode_abbrev);
  defsubr (&Sabbrev_expansion);
  defsubr (&Sabbrev_symbol);
  defsubr (&Sexpand_abbrev);
  defsubr (&Sunexpand_abbrev);
  defsubr (&Sinsert_abbrev_table_description);
  defsubr (&Sdefine_abbrev_table);
}
