/* Buffer manipulation primitives for GNU Emacs.
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


#include <sys/param.h>

#ifndef MAXPATHLEN
/* in 4.1, param.h fails to define this. */
#define MAXPATHLEN 1024
#endif /* not MAXPATHLEN */

#undef NULL
#include "config.h"
#include "lisp.h"
#include "window.h"
#include "commands.h"
#include "buffer.h"
#include "syntax.h"

struct buffer *bf_cur;		/* the current buffer */

/* This structure contains data describing the text of the current buffer.
 Switching buffers swaps their text data in and out of here */

struct buffer_text bf_text;

/* First buffer in chain of all buffers (in reverse order of creation).
   Threaded through ->next.  */

struct buffer *all_buffers;

Lisp_Object Fset_buffer ();

/* Alist of all buffer names vs the buffers. */
/* This used to be a variable, but is no longer,
 to prevent lossage due to user rplac'ing this alist or its elements.  */
Lisp_Object Vbuffer_alist;

/* Function to call to install major mode.
  nil means use the major mode of the selected buffer.  */

Lisp_Object Vdefault_major_mode;

Lisp_Object Qfundamental_mode;

Lisp_Object QSFundamental;	/* A string "Fundamental" */

/* For debugging; temporary.  See SetBfp.  */
Lisp_Object Qlisp_mode, Vcheck_symbol;

Lisp_Object Vdefault_mode_line_format;

int default_case_fold_search;

int default_tab_width;
int default_ctl_arrow;
int default_truncate_lines;

int default_fill_column;
int default_left_margin;

Lisp_Object Vdefault_abbrev_mode;

nsberror (spec)
     Lisp_Object spec;
{
  if (XTYPE (spec) == Lisp_String)
    error ("No buffer named %s", XSTRING (spec)->data);
  error ("Invalid buffer argument");
}

DEFUN ("buffer-list", Fbuffer_list, Sbuffer_list, 0, 0, 0,
  "Return a list of all buffers.")
  ()
{
  return Fmapcar (Qcdr, Vbuffer_alist);
}

DEFUN ("get-buffer", Fget_buffer, Sget_buffer, 1, 1, 0,
  "Return the buffer named NAME (a string).\n\
It is found by looking up NAME in  buffer-alist.\n\
If there is no buffer named NAME, nil is returned.\n\
NAME may also be a buffer; it is returned.")
  (name)
     Lisp_Object name;
{
  if (XTYPE (name) == Lisp_Buffer)
    return name;
  CHECK_STRING (name, 0);

  return Fcdr (Fassoc (name, Vbuffer_alist));
}

DEFUN ("get-file-buffer", Fget_file_buffer, Sget_file_buffer, 1, 1, 0,
  "Return the buffer visiting file FILENAME (a string).\n\
If there is no such buffer, nil is returned.")
  (filename)
     Lisp_Object filename;
{
  register Lisp_Object tail, buf, tem;
  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);

  for (tail = Vbuffer_alist; LISTP (tail); tail = XCONS (tail)->cdr)
    {
      buf = Fcdr (XCONS (tail)->car);
      if (XTYPE (buf) != Lisp_Buffer) continue;
      if (XTYPE (XBUFFER (buf)->filename) != Lisp_String) continue;
      tem = Fstring_equal (XBUFFER (buf)->filename, filename);
      if (!NULL (tem))
	return buf;
    }
  return Qnil;
}

/* Incremented for each buffer created, to assign the buffer number. */
int buffer_count;

DEFUN ("get-buffer-create", Fget_buffer_create, Sget_buffer_create, 1, 1, 0,
  "Like get-buffer but creates a buffer named NAME and returns it if none already exists.")
  (name)
     Lisp_Object name;
{
  Lisp_Object buf, function;
  int count = specpdl_ptr - specpdl;
  register struct buffer *b;
  struct buffer *bx;
  unsigned char *data;

  buf = Fget_buffer (name);
  if (!NULL (buf)) return buf;

  b = (struct buffer *) malloc (sizeof (struct buffer));
  if (!b) memory_full ();

  data = (unsigned char *) malloc (b->text.gap = 20);
  if (!data) memory_full ();
  b->text.p1 = data - 1;
  b->text.p2 = data - 1 + b->text.gap;
  b->text.size1 = b->text.size2 = 0;
  b->text.modified = 1;
  b->text.pointloc = 1;
  b->text.head_clip = 1;
  b->text.tail_clip = 0;

  b->next = all_buffers;
  all_buffers = b;

  b->save_length = make_number (0);
  b->last_window_start = 1;
  b->markers = Qnil;
  b->mark = Qnil;
  b->number = make_number (++buffer_count);
  b->name = name;
  if (XSTRING (name)->data[0] != ' ')
    make_undo_records (b);
  else
    b->undodata = 0;

  reset_buffer (b);

  XSETTYPE (buf, Lisp_Buffer);
  bx = b;			/* Use of bx avoids compiler bug on Sun */
  XSETBUFFER (buf, bx);
  Vbuffer_alist = nconc2 (Vbuffer_alist, Fcons (Fcons (name, buf), Qnil));

  function = Vdefault_major_mode;
  if (NULL (function))
    function = bf_cur->major_mode;

  if (NULL (function) || EQ (function, Qfundamental_mode))
    return buf;

  /* To select a nonfundamental mode,
     select the buffer temporarily and then call the mode function. */

  record_unwind_protect (save_excursion_restore, save_excursion_save ());
			 
  Fset_buffer (buf);
  Fapply (function, Qnil);

  unbind_to (count);
  return buf;
}

void
reset_buffer (b)
     register struct buffer *b;
{
  b->filename = Qnil;
  b->directory = (bf_cur) ? bf_cur->directory : Qnil;
  b->modtime = 0;
  b->save_modified = 1;
  b->backed_up = *(int*) &Qnil;
  b->auto_save_modified = 0;
  b->auto_save_file_name = Qnil;
  b->read_only = Qnil;
  reset_buffer_local_variables(b);
}

reset_buffer_local_variables(b)
     register struct buffer *b;
{
  b->keymap = Qnil;
  b->abbrev_table = Vfundamental_mode_abbrev_table;
  b->tab_width = make_number (default_tab_width);
  b->fill_column = make_number (default_fill_column);
  b->left_margin = make_number (default_left_margin);
  b->case_fold_search = default_case_fold_search ? Qt : Qnil;

  b->syntax_table_v = XVECTOR (Vstandard_syntax_table);
  b->mode_line_format = Vdefault_mode_line_format;
  b->auto_fill_hook = Qnil;
  b->local_var_alist = Qnil;
  b->ctl_arrow = default_ctl_arrow ? Qt : Qnil;
  b->truncate_lines = default_truncate_lines ? Qt : Qnil;
  b->selective_display = Qnil;
  b->overwrite_mode = Qnil;
  b->abbrev_mode = Vdefault_abbrev_mode;

  b->major_mode = Qfundamental_mode;
  b->mode_name = QSFundamental;
  b->minor_modes = Qnil;
}

/* create-file-buffer moved into lisp code in lisp/files.el */

DEFUN ("generate-new-buffer", Fgenerate_new_buffer, Sgenerate_new_buffer,
  1, 1, 0,
  "Creates and returns a buffer named NAME if one does not already exist,\n\
else tries adding successive suffixes to NAME until a new buffer-name is\n\
formed, then creates and returns a new buffer with that new name.")
 (name)
     Lisp_Object name;
{
  Lisp_Object gentemp, tem;
  int count;
  char number[10];

  CHECK_STRING (name, 0);

  tem = Fget_buffer (name);
  if (NULL (tem))
    return Fget_buffer_create (name);

  count = 1;
  while (1)
    {
      sprintf (number, "<%d>", ++count);
      gentemp = concat2 (name, build_string (number));
      tem = Fget_buffer (gentemp);
      if (NULL (tem))
	return Fget_buffer_create (gentemp);
    }
}


DEFUN ("buffer-name", Fbuffer_name, Sbuffer_name, 0, 1, 0,
  "Return the name of BUFFER, as a string.\n\
No arg means return name of current buffer.")
  (buffer)
     Lisp_Object buffer;
{
  if (NULL (buffer))
    return bf_cur->name;
  CHECK_BUFFER (buffer, 0);
  return XBUFFER (buffer)->name;
}

DEFUN ("buffer-number", Fbuffer_number, Sbuffer_number, 0, 1, 0,
  "Return the number of BUFFER.\n\
No arg means return number of current buffer.")
  (buffer)
     Lisp_Object buffer;
{
  if (NULL (buffer))
    return bf_cur->number;
  CHECK_BUFFER (buffer, 0);
  return XBUFFER (buffer)->number;
}

DEFUN ("buffer-file-name", Fbuffer_file_name, Sbuffer_file_name, 0, 1, 0,
  "Return name of file BUFFER is visiting, or NIL if none.\n\
No argument means use current buffer as BUFFER.")
  (buffer)
     Lisp_Object buffer;
{
  if (NULL (buffer))
    return bf_cur->filename;
  CHECK_BUFFER (buffer, 0);
  return XBUFFER (buffer)->filename;
}

DEFUN ("buffer-local-variables", Fbuffer_local_variables,
  Sbuffer_local_variables,
  0, 1, 0,
  "Return alist of buffer-local variables of BUFFER.\n\
Each element looks like (SYMBOL . VALUE).\n\
No argument means use current buffer as BUFFER.")
  (buffer)
     Lisp_Object buffer;
{
  if (NULL (buffer))
    return bf_cur->local_var_alist;
  CHECK_BUFFER (buffer, 0);
  return XBUFFER (buffer)->local_var_alist;
}

DEFUN ("buffer-modified-p", Fbuffer_modified_p, Sbuffer_modified_p,
  0, 1, 0,
  "Return t if BUFFER is modified since file last read in or saved.\n\
No argument means use current buffer as BUFFER.")
  (buffer)
     Lisp_Object buffer;
{
  register struct buffer *buf;
  if (NULL (buffer))
    buf = bf_cur;
  else
    {
      CHECK_BUFFER (buffer, 0);
      buf = XBUFFER (buffer);
    }

  bf_cur->text.modified = bf_modified;
  return buf->save_modified < buf->text.modified ? Qt : Qnil;
}

DEFUN ("set-buffer-modified-p", Fset_buffer_modified_p, Sset_buffer_modified_p,
  1, 1, 0,
  "Mark current buffer as modified or unmodified according to FLAG.")
  (flag)
     Lisp_Object flag;
{
  register int already;
  register Lisp_Object fn;

#ifdef CLASH_DETECTION
  /* If buffer becoming modified, lock the file.
     If buffer becoming unmodified, unlock the file.  */

  fn = bf_cur->filename;
  if (!NULL (fn))
    {
      already = bf_cur->save_modified < bf_modified;
      if (!already && !NULL (flag))
	lock_file (fn);
      else if (already && NULL (flag))
	unlock_file (fn);
    }
#endif /* CLASH_DETECTION */

  bf_cur->save_modified = NULL (flag) ? bf_modified : 0;
  RedoModes++;
  return flag;
}

/* Return number of modified buffers that exist now. */

int
ModExist ()
{
  register Lisp_Object tail, buf;
  register struct buffer *b;
  register int modcount = 0;

  bf_cur->text.modified = bf_modified;

  for (tail = Vbuffer_alist; !NULL (tail); tail = Fcdr (tail))
    {
      buf = Fcdr (Fcar (tail));
      b = XBUFFER (buf);
      if (!NULL (b->filename) && b->save_modified < b->text.modified)
	modcount++;
    }

  return modcount;
}

DEFUN ("rename-buffer", Frename_buffer, Srename_buffer, 1, 1,
  "sRename buffer (to new name): ",
  "Change current buffer's name to NEWNAME (a string).")
  (name)
     Lisp_Object name;
{
  register Lisp_Object tem, buf;

  CHECK_STRING (name, 0);
  tem = Fget_buffer (name);
  if (!NULL (tem))
    error("Buffer \"%s\" already exists", XSTRING (name)->data);

  bf_cur->name = name;
  XSET (buf, Lisp_Buffer, bf_cur);
  return Fsetcar (Frassq (buf, Vbuffer_alist), name);
}

DEFUN ("other-buffer", Fother_buffer, Sother_buffer, 0, 1, 0,
  "Return most recently selected buffer other than BUFFER.\n\
Buffers not visible in windows are preferred to visible buffers.\n\
If no other exists, the buffer *scratch* is returned.\n\
If BUFFER is omitted or nil, some interesting buffer is returned.")
  (buffer)
     Lisp_Object buffer;
{
  register Lisp_Object tail, buf, notsogood, tem;
  notsogood = Qnil;

  for (tail = Vbuffer_alist; !NULL (tail); tail = Fcdr (tail))
    {
      buf = Fcdr (Fcar (tail));
      if (EQ (buf, buffer))
	continue;
      if (XSTRING (XBUFFER (buf)->name)->data[0] == ' ')
	continue;
      tem = Fget_buffer_window (buf);
      if (NULL (tem))
	return buf;
      if (!NULL (notsogood))
	notsogood = buf;
    }
  if (!NULL (notsogood))
    return notsogood;
  return Fget_buffer_create (build_string ("*scratch*"));
}

DEFUN ("buffer-flush-undo", Fbuffer_flush_undo, Sbuffer_flush_undo, 1, 1, 0,
  "Make BUFFER stop keeping undo information.")
  (buf)
     Lisp_Object buf;
{
  CHECK_BUFFER (buf, 0);
  if (XBUFFER (buf)->undodata)
    free_undo_records (XBUFFER (buf));
  XBUFFER (buf)->undodata = 0;
  return Qnil;
}

Lisp_Object
Fdelete_buffer_internal (buf)
     Lisp_Object buf;
{
  register struct buffer *b = XBUFFER (buf);
  register Lisp_Object tem;
  register struct Lisp_Marker *m;

  if (NULL (b->name))
    return Qnil;

#ifdef CLASH_DETECTION
  /* Unlock this buffer's file, if it is locked.  */
  Funlock_buffer ();
#endif /* CLASH_DETECTION */

  /* make this buffer not be current */
  if (b == bf_cur)
    {
      tem = Fother_buffer (buf);
      if (NULL (tem))
	tem = Fget_buffer_create (build_string ("*scratch*"));
      Fset_buffer (tem);
    }

#ifdef subprocesses
  kill_buffer_processes (buf);
#endif subprocesses

  Vbuffer_alist = Fdelq (Frassq (buf, Vbuffer_alist), Vbuffer_alist);
  Freplace_buffer_in_windows (buf);

  /* Unchain all markers of this buffer
     and leave them pointing nowhere.  */
  for (tem = b->markers; !EQ (tem, Qnil); )
    {
      m = XMARKER (tem);
      m->buffer = 0;
      tem = m->chain;
      m->chain = Qnil;
    }

  b->name = Qnil;
  free (b->text.p1 + 1);
  if (b->undodata)
    free_undo_records (b);

  return Qnil;
}

DEFUN ("kill-buffer", Fkill_buffer, Skill_buffer, 1, 1, "bKill buffer: ",
  "One arg, a string or a buffer.  Get rid of the specified buffer.")
  (bufname)
     Lisp_Object bufname;
{
  register Lisp_Object buf, answer;

  if (NULL (bufname))
    buf = Fcurrent_buffer ();
  else
    buf = Fget_buffer (bufname);
  if (NULL (buf))
    nsberror (bufname);
  bufname = XBUFFER (buf)->name;

  bf_cur->text.modified = bf_modified;

  if (INTERACTIVE && !NULL (XBUFFER (buf)->filename)
      && XBUFFER (buf)->text.modified > XBUFFER (buf)->save_modified)
    {
      answer = Fyes_or_no_p (format1 ("Buffer %s modified; kill anyway? ",
				      XSTRING (bufname)->data));
      if (NULL (answer))
	return Qnil;
    }
  Fdelete_buffer_internal (buf);
  return Qnil;
}

/* Put the element for buffer `buf' at the front of buffer-alist.
 This is done when a buffer is selected "visibly".
 It keeps buffer-alist in the order of recency of selection
 so that other_buffer will return something nice.  */

record_buffer (buf)
     Lisp_Object buf;
{
  register Lisp_Object aelt, link;
  aelt = Frassq (buf, Vbuffer_alist);
  link = Fmemq (aelt, Vbuffer_alist);
  XCONS(link)->cdr = Fdelq (aelt, Vbuffer_alist);
  Vbuffer_alist = link;
}

DEFUN ("switch-to-buffer", Fswitch_to_buffer, Sswitch_to_buffer, 1, 2, "BSwitch to buffer: ",
  "One arg, a string or buffer.  Select the specified buffer\n\
in the current window.  Optional arg NORECORD non-nil means\n\
do not put this buffer at the front of the list of recently selected ones.")
  (bufname, norecord)
     Lisp_Object bufname, norecord;
{
  register Lisp_Object buf;
  if (NULL (bufname))
    buf = Fother_buffer (Fcurrent_buffer ());
  else
    buf = Fget_buffer_create (bufname);
  Fset_buffer (buf);
  if (NULL (norecord))
    record_buffer (buf);

  Fshow_buffer (EQ (selected_window, minibuf_window)
		  ? Fnext_window (minibuf_window, Qnil) : selected_window,
		buf);

  return Qnil;
}

DEFUN ("pop-to-buffer", Fpop_to_buffer, Spop_to_buffer, 1, 2, 0,
  "Select buffer BUFFER in some window, preferably a different one.\n\
If  pop-up-windows  is non-nil, windows can be split to do this.\n\
If second arg  OTHER-WINDOW is non-nil, insist on finding another\n\
window even if BUFFER is already visible in the selected window.")
  (bufname, other)
     Lisp_Object bufname, other;
{
  register Lisp_Object buf;
  if (NULL (bufname))
    buf = Fother_buffer (Fcurrent_buffer ());
  else
    buf = Fget_buffer_create (bufname);
  Fset_buffer (buf);
  record_buffer (buf);
  Fselect_window (Fdisplay_buffer (buf, other));
  return Qnil;
}

DEFUN ("current-buffer", Fcurrent_buffer, Scurrent_buffer, 0, 0, 0,
  "Return the current buffer as a Lisp buffer object.")
  ()
{
  register Lisp_Object buf;
  XSET (buf, Lisp_Buffer, bf_cur);
  return buf;
}

DEFUN ("set-buffer", Fset_buffer, Sset_buffer, 1, 1, 0,
  "Set the current buffer to the buffer or buffer name supplied as argument.\n\
That buffer will then be the default for editing operations and printing.\n\
This function's effect can't last past end of current command\n\
because returning to command level\n\
selects the chosen buffer of the current window,\n\
and this function has no effect on what buffer that is.\n\
Use  switch-to-buffer  or  pop-to-buffer  for interactive buffer selection.")
  (bufname)
     Lisp_Object bufname;
{
  register Lisp_Object buffer;
  buffer = Fget_buffer (bufname);
  if (NULL (buffer))
    nsberror (bufname);
  SetBfp (XBUFFER (buffer));
  return buffer;
}

DEFUN ("barf-if-buffer-read-only", Fbarf_if_buffer_read_only,
				   Sbarf_if_buffer_read_only, 0, 0, 0,
  "Signal a  buffer-read-only  error if the current buffer is read-only.")
  ()
{
  if (!NULL (bf_cur->read_only))
    Fsignal (Qbuffer_read_only, Qnil);
  return Qnil;
}

DEFUN ("bury-buffer", Fbury_buffer, Sbury_buffer, 1, 1, 0,
  "Put BUFFER at the end of the list of all buffers.\n\
There it is the least likely candidate for other-buffer to return;\n\
thus, the least likely buffer for \\[switch-to-buffer] to select by default.")
  (buf)
     Lisp_Object buf;
{
  register Lisp_Object aelt, link;

  buf = Fget_buffer (buf);

  aelt = Frassq (buf, Vbuffer_alist);
  link = Fmemq (aelt, Vbuffer_alist);
  Vbuffer_alist = Fdelq (aelt, Vbuffer_alist);
  XCONS (link)->cdr = Qnil;
  Vbuffer_alist = nconc2 (Vbuffer_alist, link);
  return Qnil;
}

extern int last_known_column_point;

/* set the current buffer to p */
SetBfp (p)
     register struct buffer *p;
{
  register struct buffer *c = bf_cur;
  register struct window *w = XWINDOW (selected_window);
  register struct buffer *swb;
  Lisp_Object tail, valcontents;
  enum Lisp_Type tem;

  if (c == p)
    return;

  if (w)
    swb = NULL (selected_window) ? 0 : XBUFFER (w->buffer);

  if (p && NULL (p->name))
    error ("Selecting deleted buffer");
  windows_or_buffers_changed = 1;

  if (c)
    {
      if (c == swb)
	Fset_marker (w->pointm, make_number (point), w->buffer);

      if (point < FirstCharacter || point > NumCharacters + 1)
	abort ();

      c->text = bf_text;
    }
  bf_cur = p;
  bf_text = p->text;
  if (p == swb)
    {
      SetPoint (marker_position (w->pointm));
      if (point < FirstCharacter)
	point = FirstCharacter;
      if (point > NumCharacters + 1)
	point = NumCharacters + 1;
    }
  last_known_column_point = -1;   /* invalidate indentation cache */

  /* Vcheck_symbol is set up to the symbol paragraph-start
     in order to check for the bug that clobbers it.  */
  if (c && EQ (c->major_mode, Qlisp_mode)
      && XFASTINT (Vcheck_symbol) != 0
      && !NULL (Vcheck_symbol))
    {
      valcontents = XSYMBOL (Vcheck_symbol)->value;
      if (XTYPE (valcontents) != Lisp_Some_Buffer_Local_Value)
	abort ();
      if (c == XBUFFER (XCONS (XCONS (valcontents)->cdr)->car)
	  && (XTYPE (XCONS (valcontents)->car) != Lisp_String
	      || XSTRING (XCONS (valcontents)->car)->size != 6))
	abort ();
    }

  /* Look down buffer's list of local Lisp variables
     to find and update any that forward into C variables. */

  for (tail = p->local_var_alist; !NULL (tail); tail = XCONS (tail)->cdr)
    {
      valcontents = XSYMBOL (XCONS (XCONS (tail)->car)->car)->value;
      if ((XTYPE (valcontents) == Lisp_Buffer_Local_Value
	   || XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
	  && (tem = XTYPE (XCONS (valcontents)->car),
	      (tem == Lisp_Boolfwd || tem == Lisp_Intfwd
	       || tem == Lisp_Objfwd)))
	/* Just reference the variable
	     to cause it to become set for this buffer.  */
	Fsymbol_value (XCONS (XCONS (tail)->car)->car);
    }

  /* Do the same with any others that were local to the previous buffer */

  if (c)
    for (tail = c->local_var_alist; !NULL (tail); tail = XCONS (tail)->cdr)
      {
	valcontents = XSYMBOL (XCONS (XCONS (tail)->car)->car)->value;
	if ((XTYPE (valcontents) == Lisp_Buffer_Local_Value
	     || XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
	    && (tem = XTYPE (XCONS (valcontents)->car),
		(tem == Lisp_Boolfwd || tem == Lisp_Intfwd
		 || tem == Lisp_Objfwd)))
	  /* Just reference the variable
               to cause it to become set for this buffer.  */
	  Fsymbol_value (XCONS (XCONS (tail)->car)->car);
      }
  /* Vcheck_symbol is set up to the symbol paragraph-start
     in order to check for the bug that clobbers it.  */
  if (EQ (p->major_mode, Qlisp_mode)
      && Vcheck_symbol
      && !NULL (Vcheck_symbol))
    {
      valcontents = XSYMBOL (Vcheck_symbol)->value;
      if (XTYPE (valcontents) != Lisp_Some_Buffer_Local_Value)
	abort ();
      if (p == XBUFFER (XCONS (XCONS (valcontents)->cdr)->car)
	  && (XTYPE (XCONS (valcontents)->car) != Lisp_String
	      || XSTRING (XCONS (valcontents)->car)->size != 6))
	abort ();
      Fsymbol_value (Vcheck_symbol);
      valcontents = XSYMBOL (Vcheck_symbol)->value;
      if (p != XBUFFER (XCONS (XCONS (valcontents)->cdr)->car)
	  || XTYPE (XCONS (valcontents)->car) != Lisp_String
	  || XSTRING (XCONS (valcontents)->car)->size != 6)
	abort ();
    }
}

/* set the current buffer to p "just for redisplay" */
SetBfx (p)
     register struct buffer *p;
{
  if (bf_cur == p)
    return;

  bf_cur->text = bf_text;
  bf_cur = p;
  bf_text = p->text;
}

DEFUN ("erase-buffer", Ferase_buffer, Serase_buffer, 0, 0, 0,
  "Delete the entire contents of the current buffer.")
  ()
{
  Fwiden ();
  del_range (1, NumCharacters + 1);
  bf_cur->last_window_start = 1;
  return Qnil;
}

validate_region (b, e)
     register Lisp_Object *b, *e;
{
  register int i;

  CHECK_NUMBER_COERCE_MARKER (*b, 0);
  CHECK_NUMBER_COERCE_MARKER (*e, 1);

  if (XINT (*b) > XINT (*e))
    {
      i = XFASTINT (*b);	/* This is legit even if *b is < 0 */
      *b = *e;
      XFASTINT (*e) = i;	/* because this is all we do with i.  */
    }

  if (!(FirstCharacter <= XINT (*b) && XINT (*b) <= XINT (*e)
        && XINT (*e) <= 1 + NumCharacters))
    args_out_of_range (*b, *e);
}

Lisp_Object
list_buffers_1 (files)
     Lisp_Object files;
{
  Lisp_Object tail, buf, col1, col2, col3, minspace, tem, mode;
  register struct buffer *old = bf_cur, *b;
  int desired_point = 0;

  bf_cur->text.modified = bf_modified;

  XFASTINT (col1) = 19;
  XFASTINT (col2) = 25;
  XFASTINT (col3) = 40;
  XFASTINT (minspace) = 1;

  SetBfp (XBUFFER (Vstandard_output));

  mode = intern ("Buffer-menu-mode");
  if (!EQ (mode, bf_cur->major_mode)
      && (tem = Ffboundp (mode), !NULL (tem)))
    Fapply (mode, Qnil);
  Fbuffer_flush_undo (Vstandard_output);
  bf_cur->read_only = Qnil;

  write_string ("\
 MR Buffer         Size  Mode           File\n\
 -- ------         ----  ----           ----\n", -1);

  for (tail = Vbuffer_alist; !NULL (tail); tail = Fcdr (tail))
    {
      buf = Fcdr (Fcar (tail));
      b = XBUFFER (buf);
      /* Don't mention the minibuffers. */
      if (XSTRING (b->name)->data[0] == ' ')
	continue;
      /* Optionally don't mention buffers that lack files. */
      if (!NULL (files) && NULL (b->filename))
	continue;
      /* Identify the current buffer. */
      if (b == old)
	desired_point = point;
      write_string (b == old ? "." : " ", -1);
      /* Identify modified buffers */
      write_string (b->text.modified > b->save_modified ? "*" : " ", -1);
      write_string (NULL (b->read_only) ? "  " : "% ", -1);
      Fprinc (b->name, Qnil);
      Findent_to (col1, make_number (2));
      XFASTINT (tem) = b->text.size1 + b->text.size2;
      Fprin1 (tem, Qnil);
      Findent_to (col2, minspace);
      Fprinc (b->mode_name, Qnil);
      Findent_to (col3, minspace);
      if (!NULL (b->filename))
	Fprinc (b->filename, Qnil);
      write_string ("\n", -1);
    }

  bf_cur->read_only = Qt;
  SetBfp (old);
  /* Foo.  This doesn't work since temp_output_buffer_show sets point to 1 */
  if (desired_point)
    XBUFFER (Vstandard_output)->text.pointloc = desired_point;
  return Qnil;
}

DEFUN ("list-buffers", Flist_buffers, Slist_buffers, 0, 1, "",
  "Display a list of names of existing buffers.\n\
Inserts it in buffer *Buffer List* and displays that.\n\
Note that buffers with names starting with spaces are omitted.\n\
Non-null optional arg FILES-ONLY means mention only file buffers.")
  (files)
     Lisp_Object files;
{
  internal_with_output_to_temp_buffer ("*Buffer List*",
				       list_buffers_1, files);
  return Qnil;
}

/* note: this leaves us in fundamental-mode, not default-major-mode
   should anything be done about this?
*/
DEFUN ("kill-all-local-variables", Fkill_all_local_variables, Skill_all_local_variables,
  0, 0, 0,
  "Eliminate all the buffer-local variable values of the current buffer.\n\
This buffer will then see the default values of all variables.")
  ()
{
  register Lisp_Object alist, sym, tem;

  for (alist = bf_cur->local_var_alist; !NULL (alist); alist = XCONS (alist)->cdr)
    {
      sym = XCONS (XCONS (alist)->car)->car;

      /* Need not do anything if some other buffer's binding is now encached.  */
      tem = XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->car;
      if (XBUFFER (tem) == bf_cur)
	{
	  /* Symbol is set up for this buffer's old local value.
	     Set it up for the current buffer with the default value.  */

	  tem = XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->cdr;
	  XCONS (tem)->car = tem;
	  XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->car = Fcurrent_buffer ();
	  store_symval_forwarding (sym, XCONS (XSYMBOL (sym)->value)->car,
				   XCONS (tem)->cdr);
	}
    }

  reset_buffer_local_variables (bf_cur);
  return Qnil;
}

extern Lisp_Object Vprin1_to_string_buffer;	/* in print.c */
init_buffer_once ()
{
  register Lisp_Object tem;

  /* Must do these before making the first buffer! */

  Vdefault_mode_line_format
    = build_string ("--%1*%1*-Emacs: %17b   %M   %[(%m)%]----%3p-%-");
  Vdefault_abbrev_mode = Qnil;
  default_case_fold_search = 1;

  default_tab_width = 8;
  default_truncate_lines = 0;
  default_ctl_arrow = 1;

  default_fill_column = 70;
  default_left_margin = 0;

  Vbuffer_alist = Qnil;
  bf_cur = 0;
  all_buffers = 0;

  QSFundamental = build_string ("Fundamental");

  Qfundamental_mode = intern ("fundamental-mode");
  Vdefault_major_mode = Qfundamental_mode;

  Vprin1_to_string_buffer = Fget_buffer_create (build_string (" prin1"));
  /* super-magic invisible buffer */
  Vbuffer_alist = Qnil;

  tem = Fset_buffer (Fget_buffer_create (build_string ("*scratch*")));
  /* Want no undo records for *scratch*
     until after Emacs is dumped */
  Fbuffer_flush_undo (tem);
}

init_buffer ()
{
  char buf[MAXPATHLEN+1];

  Fset_buffer (Fget_buffer_create (build_string ("*scratch*")));
  getwd (buf);
  if (buf[strlen (buf) - 1] != '/')
    strcat (buf, "/");
  bf_cur->directory = build_string (buf);
  if (NULL (Vpurify_flag))
    make_undo_records (bf_cur);
}

/* initialize the buffer routines */
syms_of_buffer ()
{
  staticpro (&Qfundamental_mode);
  staticpro (&QSFundamental);
  staticpro (&Vbuffer_alist);

  staticpro (&Qlisp_mode);
  Qlisp_mode = intern ("lisp-mode");

  DefLispVar ("default-mode-line-format", &Vdefault_mode_line_format,
    "Default value of mode-line-format for new buffers.");

  DefBufferLispVar ("mode-line-format", &bf_cur->mode_line_format,
    "Template string for displaying mode line for current buffer.\n\
Each buffer has its own value of this variable.\n\
The string is printed verbatim in the mode line\n\
except for %-constructs:\n\
  %b -- print buffer name.   %f -- print visited file name.\n\
  %* -- print *, % or hyphen.   %m -- print value of mode-name.\n\
  %s -- print process status.   %M -- print value of global-mode-string.\n\
  %p -- print percent of buffer above top of window, or top, bot or all.\n\
  %[ -- print one [ for each recursive editing level.  %] similar.\n\
  %% -- print %.   %- -- print infinitely many dashes.\n\
Decimal digits after the % specify field width to pad or truncate to.");

  DefLispVar ("default-abbrev-mode", &Vdefault_abbrev_mode,
    "Default value of abbrev-mode for new buffers.");

  DefBufferLispVar ("abbrev-mode", &bf_cur->abbrev_mode,
    "*Non-nil turns on automatic expansion of abbrevs when inserted.");

  DefBoolVar ("default-case-fold-search", &default_case_fold_search,
    "*Default value of case-fold-search for new buffers.");
  DefBufferLispVar ("case-fold-search", &bf_cur->case_fold_search,
    "*Non-nil if searches should ignore case.\n\
Separate value in each buffer.");

  DefBufferLispVar ("mode-name", &bf_cur->mode_name,
    "Pretty name of current buffer's major mode (a string).");

  DefBufferLispVar ("minor-modes", &bf_cur->minor_modes,
    "List of minor modes enabled in current buffer.\n\
Each element is (FUNCTION-SYMBOL . PRETTY-STRING).");

  DefIntVar ("default-fill-column", &default_fill_column,
    "*Default value of fill-column for new buffers.");
  DefBufferLispVar ("fill-column", &bf_cur->fill_column,
    "*Column beyond which automatic line-wrapping should happen.\n\
Separate value in each buffer.");

  DefIntVar ("default-left-margin", &default_left_margin,
    "*Default value of left-margin for buffers that don't override it.");
  DefBufferLispVar ("left-margin", &bf_cur->left_margin,
    "*Column for the default indent-line-function to indent to.\n\
Linefeed indents to this column in Fundamental mode.");

  DefIntVar ("default-tab-width", &default_tab_width,
    "*Default value of tab-width for new buffers.");
  DefBufferLispVar ("tab-width", &bf_cur->tab_width,
    "*Distance between tab stops (for display of tab characters), in columns.\n\
Separate value in each buffer.");

  DefBoolVar ("default-ctl-arrow", &default_ctl_arrow,
    "*Default value of ctl-arrow for new buffers.");
  DefBufferLispVar ("ctl-arrow", &bf_cur->ctl_arrow,
    "*Non-nil means display control chars with uparrow.\n\
Nil means use backslash and octal digits.\n\
Separate value in each buffer.");

  DefBoolVar ("default-truncate-lines", &default_truncate_lines,
    "*Default value of truncate-lines for new buffers.");
  DefBufferLispVar ("truncate-lines", &bf_cur->truncate_lines,
    "*Non-nil means do not display continuation lines;\n\
give each line of text one screen line.\n\
Separate value in each buffer.");

  DefBufferLispVar ("default-directory", &bf_cur->directory,
    "*Name of default directory of current buffer.  Should end with slash.");

  DefBufferLispVar ("auto-fill-hook", &bf_cur->auto_fill_hook,
    "Function called (if non-nil) after self-inserting a space at column beyond fill-column");

  DefBufferLispVar ("buffer-file-name", &bf_cur->filename,
    "Name of file visited in current buffer, or nil if not visiting a file.");

  DefBufferLispVar ("buffer-auto-save-file-name",
		    &bf_cur->auto_save_file_name,
    "Name of file for auto-saving current buffer,\n\
or nil if buffer should not be auto-saved.");

  DefBufferLispVar ("buffer-read-only", &bf_cur->read_only,
    "*Non-nil if this buffer is read-only.");

/* LMCL: Second arg should really be a Lisp_Object but it needs this address.
 * A Lisp_Object had better take up only one word! */
  DefBufferLispVar ("buffer-backed-up", &bf_cur->backed_up,
    "Non-nil if this buffer's file has been backed up.\n\
Backing up is done before the first time the file is saved.");

  DefBufferLispVar ("buffer-saved-size", &bf_cur->save_length,
    "Length of current buffer when last read in, saved or auto-saved.\n\
0 initially.");

  DefBufferLispVar ("selective-display", &bf_cur->selective_display,
    "t enables selective display:\n\
 after a ^M, all the rest of the line is invisible.\n\
 ^M's in the file are written into files as newlines.\n\
Integer n as value means display only lines\n\
 that start with less than n columns of space.");

  DefBufferLispVar ("overwrite-mode", &bf_cur->overwrite_mode,
    "*Non-nil if self-insertion should replace existing text.");

  DefLispVar ("default-major-mode", &Vdefault_major_mode,
    "*Major mode for new buffers.  Defaults to fundamental-mode.\n\
nil here means use current buffer's major mode.");

  DefBufferLispVar ("major-mode", &bf_cur->major_mode,
    "Symbol for buffer's major mode.");

  DefLispVar ("debug-check-symbol", &Vcheck_symbol,
    "Don't ask.");

  defsubr (&Sbuffer_list);
  defsubr (&Sget_buffer);
  defsubr (&Sget_file_buffer);
  defsubr (&Sget_buffer_create);
  defsubr (&Sgenerate_new_buffer);
  defsubr (&Sbuffer_name);
  defsubr (&Sbuffer_number);
  defsubr (&Sbuffer_file_name);
  defsubr (&Sbuffer_local_variables);
  defsubr (&Sbuffer_modified_p);
  defsubr (&Sset_buffer_modified_p);
  defsubr (&Srename_buffer);
  defsubr (&Sother_buffer);
  defsubr (&Sbuffer_flush_undo);
  defsubr (&Skill_buffer);
  defsubr (&Serase_buffer);
  defsubr (&Sswitch_to_buffer);
  defsubr (&Spop_to_buffer);
  defsubr (&Scurrent_buffer);
  defsubr (&Sset_buffer);
  defsubr (&Sbarf_if_buffer_read_only);
  defsubr (&Sbury_buffer);
  defsubr (&Slist_buffers);
  defsubr (&Skill_all_local_variables);
}

keys_of_buffer ()
{
  defkey (CtlXmap, 'b', "switch-to-buffer");
  defkey (CtlXmap, 'k', "kill-buffer");
  defkey (CtlXmap, Ctl ('B'), "list-buffers");
}
