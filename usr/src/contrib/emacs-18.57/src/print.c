/* Lisp object printing and output streams.
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
#include <stdio.h>
#undef NULL
#include "lisp.h"

#ifndef standalone
#include "buffer.h"
#include "window.h"
#include "process.h"
#include "dispextern.h"
#include "termchar.h"
#endif /* not standalone */

Lisp_Object Vstandard_output, Qstandard_output;

/* Avoid actual stack overflow in print.  */
int print_depth;

/* Maximum length of list to print in full; noninteger means
   effectively infinity */

Lisp_Object Vprint_length;

/* Nonzero means print newlines in strings as \n.  */

int print_escape_newlines;

/* Nonzero means print newline before next minibuffer message.
   Defined in xdisp.c */

extern int noninteractive_need_newline;
#ifdef MAX_PRINT_CHARS
static int print_chars;
static int max_print;
#endif /* MAX_PRINT_CHARS */

/* Low level output routines for charaters and strings */

/* Lisp functions to do output using a stream
 must have the stream in a variable called printcharfun
 and must start with PRINTPREPARE and end with PRINTFINISH.
 Use PRINTCHAR to output one character,
 or call strout to output a block of characters.
 Also, each one must have the declarations
   struct buffer *old = current_buffer;
   int old_point = -1, start_point;
   Lisp_Object original;
*/ 

#define PRINTPREPARE \
   original = printcharfun; \
   if (NULL (printcharfun)) printcharfun = Qt; \
   if (XTYPE (printcharfun) == Lisp_Buffer) \
     { if (XBUFFER (printcharfun) != current_buffer) Fset_buffer (printcharfun); \
       printcharfun = Qnil;}\
   if (XTYPE (printcharfun) == Lisp_Marker) \
     { if (XMARKER (original)->buffer != current_buffer) \
         set_buffer_internal (XMARKER (original)->buffer); \
       old_point = point; \
       SET_PT (marker_position (printcharfun)); \
       start_point = point; \
       printcharfun = Qnil;}

#define PRINTFINISH \
   if (XTYPE (original) == Lisp_Marker) \
     Fset_marker (original, make_number (point), Qnil); \
   if (old_point >= 0) \
     SET_PT ((old_point >= start_point ? point - start_point : 0) + old_point); \
   if (old != current_buffer) \
     set_buffer_internal (old)

#define PRINTCHAR(ch) printchar (ch, printcharfun)

/* Index of first unused element of message_buf.  */
static int printbufidx;

static void
printchar (ch, fun)
     unsigned char ch;
     Lisp_Object fun;
{
  Lisp_Object ch1;

#ifdef MAX_PRINT_CHARS
  if (max_print)
    print_chars++;
#endif /* MAX_PRINT_CHARS */
#ifndef standalone
  if (EQ (fun, Qnil))
    {
      QUIT;
      insert (&ch, 1);
      return;
    }
  if (EQ (fun, Qt))
    {
      if (noninteractive)
	{
	  putchar (ch);
	  noninteractive_need_newline = 1;
	  return;
	}
      if (echo_area_contents != message_buf)
	echo_area_contents = message_buf, printbufidx = 0;
      if (printbufidx < screen_width)
	message_buf[printbufidx++] = ch;
      message_buf[printbufidx] = 0;
      return;
    }
#endif /* not standalone */

  XFASTINT (ch1) = ch;
  call1 (fun, ch1);
}

static void
strout (ptr, size, printcharfun)
     char *ptr;
     int size;
     Lisp_Object printcharfun;
{
  int i = 0;

  if (EQ (printcharfun, Qnil))
    {
      insert (ptr, size >= 0 ? size : strlen (ptr));
#ifdef MAX_PRINT_CHARS
      if (max_print)
        print_chars += size >= 0 ? size : strlen(ptr);
#endif /* MAX_PRINT_CHARS */
      return;
    }
  if (EQ (printcharfun, Qt))
    {
      i = size >= 0 ? size : strlen (ptr);
#ifdef MAX_PRINT_CHARS
      if (max_print)
        print_chars += i;
#endif /* MAX_PRINT_CHARS */
      if (noninteractive)
	{
	  fwrite (ptr, 1, i, stdout);
	  noninteractive_need_newline = 1;
	  return;
	}
      if (echo_area_contents != message_buf)
	echo_area_contents = message_buf, printbufidx = 0;
      if (i > screen_width - printbufidx)
	i = screen_width - printbufidx;
      bcopy (ptr, &message_buf[printbufidx], i);
      printbufidx += i;
      message_buf[printbufidx] = 0;
      return;
    }
  if (size >= 0)
    while (i < size)
      PRINTCHAR (ptr[i++]);
  else
    while (ptr[i])
      PRINTCHAR (ptr[i++]);
}

DEFUN ("write-char", Fwrite_char, Swrite_char, 1, 2, 0,
  "Output character CHAR to stream STREAM.\n\
STREAM defaults to the value of `standard-output' (which see).")
  (ch, printcharfun)
     Lisp_Object ch, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NULL (printcharfun))
    printcharfun = Vstandard_output;
  CHECK_NUMBER (ch, 0);
  PRINTPREPARE;
  PRINTCHAR (XINT (ch));
  PRINTFINISH;
  return ch;
}

write_string (data, size)
     char *data;
     int size;
{
  struct buffer *old = current_buffer;
  Lisp_Object printcharfun;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  printcharfun = Vstandard_output;

  PRINTPREPARE;
  strout (data, size, printcharfun);
  PRINTFINISH;
}

write_string_1 (data, size, printcharfun)
     char *data;
     int size;
     Lisp_Object printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  PRINTPREPARE;
  strout (data, size, printcharfun);
  PRINTFINISH;
}


#ifndef standalone

temp_output_buffer_setup (bufname)
    char *bufname;
{
  register struct buffer *old = current_buffer;
  register Lisp_Object buf;

  Fset_buffer (Fget_buffer_create (build_string (bufname)));

  current_buffer->read_only = Qnil;
  Ferase_buffer ();

  XSET (buf, Lisp_Buffer, current_buffer);
  specbind (Qstandard_output, buf);

  set_buffer_internal (old);
}

Lisp_Object
internal_with_output_to_temp_buffer (bufname, function, args)
     char *bufname;
     Lisp_Object (*function) ();
     Lisp_Object args;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object buf, val;

  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());
  temp_output_buffer_setup (bufname);
  buf = Vstandard_output;

  val = (*function) (args);

  temp_output_buffer_show (buf);

  unbind_to (count);
  return val;
}

DEFUN ("with-output-to-temp-buffer", Fwith_output_to_temp_buffer, Swith_output_to_temp_buffer,
       1, UNEVALLED, 0,
  "Binding `standard-output' to buffer named BUFNAME, execute BODY then display that buffer.\n\
The buffer is cleared out initially, and marked as unmodified when done.\n\
All output done by BODY is inserted in that buffer by default.\n\
It is displayed in another window, but not selected.\n\
The value of the last form in BODY is returned.\n\
If variable `temp-buffer-show-hook' is non-nil, call it at the end\n\
to get the buffer displayed.  It gets one argument, the buffer to display.")
  (args)
     Lisp_Object args;
{
  struct gcpro gcpro1;
  Lisp_Object name;
  int count = specpdl_ptr - specpdl;
  Lisp_Object buf, val;

  GCPRO1(args);
  name = Feval (Fcar (args));
  UNGCPRO;

  CHECK_STRING (name, 0);
  temp_output_buffer_setup (XSTRING (name)->data);
  buf = Vstandard_output;

  val = Fprogn (Fcdr (args));

  temp_output_buffer_show (buf);

  unbind_to (count);
  return val;
}
#endif /* not standalone */

static void print ();

DEFUN ("terpri", Fterpri, Sterpri, 0, 1, 0,
  "Output a newline to STREAM (or value of standard-output).")
  (printcharfun)
     Lisp_Object printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NULL (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  PRINTCHAR ('\n');
  PRINTFINISH;
  return Qt;
}

DEFUN ("prin1", Fprin1, Sprin1, 1, 2, 0,
  "Output the printed representation of OBJECT, any Lisp object.\n\
Quoting characters are printed when needed to make output that `read'\n\
can handle, whenever this is possible.\n\
Output stream is STREAM, or value of `standard-output' (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

#ifdef MAX_PRINT_CHARS
  max_print = 0;
#endif /* MAX_PRINT_CHARS */
  if (NULL (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, 1);
  PRINTFINISH;
  return obj;
}

/* a buffer which is used to hold output being built by prin1-to-string */
Lisp_Object Vprin1_to_string_buffer;

DEFUN ("prin1-to-string", Fprin1_to_string, Sprin1_to_string, 1, 1, 0,
  "Return a string containing the printed representation of OBJECT,\n\
any Lisp object.  Quoting characters are used when needed to make output\n\
that `read' can handle, whenever this is possible.")
  (obj)
     Lisp_Object obj;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original, printcharfun;
  struct gcpro gcpro1;

  printcharfun = Vprin1_to_string_buffer;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, 1);
  /* Make Vprin1_to_string_buffer be the default buffer after PRINTFINSH */
  PRINTFINISH;
  set_buffer_internal (XBUFFER (Vprin1_to_string_buffer));
  obj = Fbuffer_string ();
  GCPRO1 (obj);
  Ferase_buffer ();
  set_buffer_internal (old);
  UNGCPRO;
  return obj;
}

DEFUN ("princ", Fprinc, Sprinc, 1, 2, 0,
  "Output the printed representation of OBJECT, any Lisp object.\n\
No quoting characters are used; no delimiters are printed around\n\
the contents of strings.\n\
Output stream is STREAM, or value of standard-output (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;

  if (NULL (printcharfun))
    printcharfun = Vstandard_output;
  PRINTPREPARE;
  print_depth = 0;
  print (obj, printcharfun, 0);
  PRINTFINISH;
  return obj;
}

DEFUN ("print", Fprint, Sprint, 1, 2, 0,
  "Output the printed representation of OBJECT, with newlines around it.\n\
Quoting characters are printed when needed to make output that `read'\n\
can handle, whenever this is possible.\n\
Output stream is STREAM, or value of `standard-output' (which see).")
  (obj, printcharfun)
     Lisp_Object obj, printcharfun;
{
  struct buffer *old = current_buffer;
  int old_point = -1;
  int start_point;
  Lisp_Object original;
  struct gcpro gcpro1;

#ifdef MAX_PRINT_CHARS
  print_chars = 0;
  max_print = MAX_PRINT_CHARS;
#endif /* MAX_PRINT_CHARS */
  if (NULL (printcharfun))
    printcharfun = Vstandard_output;
  GCPRO1 (obj);
  PRINTPREPARE;
  print_depth = 0;
  PRINTCHAR ('\n');
  print (obj, printcharfun, 1);
  PRINTCHAR ('\n');
  PRINTFINISH;
#ifdef MAX_PRINT_CHARS
  max_print = 0;
  print_chars = 0;
#endif /* MAX_PRINT_CHARS */
  UNGCPRO;
  return obj;
}

static void
print (obj, printcharfun, escapeflag)
#ifndef RTPC_REGISTER_BUG
     register Lisp_Object obj;
#else
     Lisp_Object obj;
#endif
     register Lisp_Object printcharfun;
     int escapeflag;
{
  char buf[30];

  QUIT;

  print_depth++;
  if (print_depth > 200)
    error ("Apparently circular structure being printed");
#ifdef MAX_PRINT_CHARS
  if (max_print && print_chars > max_print)
    {
      PRINTCHAR ('\n');
      print_chars = 0;
    }
#endif /* MAX_PRINT_CHARS */

#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (obj))
#else
  switch (XTYPE (obj))
#endif
    {
    default:
      /* We're in trouble if this happens!
	 Probably should just abort () */
      strout ("#<EMACS BUG: INVALID DATATYPE ", -1, printcharfun);
      sprintf (buf, "(#o%3o)", (int) XTYPE (obj));
      strout (buf, -1, printcharfun);
      strout (" Save your buffers immediately and please report this bug>",
	      -1, printcharfun);
      break;

    case Lisp_Int:
      sprintf (buf, "%d", XINT (obj));
      strout (buf, -1, printcharfun);
      break;

    case Lisp_String:
      if (!escapeflag)
	strout (XSTRING (obj)->data, XSTRING (obj)->size, printcharfun);
      else
	{
	  register int i;
	  register unsigned char *p = XSTRING (obj)->data;
	  register unsigned char c;

	  PRINTCHAR ('\"');
	  for (i = XSTRING (obj)->size; i > 0; i--)
	    {
	      QUIT;
	      c = *p++;
	      if (c == '\n' && print_escape_newlines)
		{
		  PRINTCHAR ('\\');
		  PRINTCHAR ('n');
		}
	      else
		{
		  if (c == '\"' || c == '\\')
		    PRINTCHAR ('\\');
		  PRINTCHAR (c);
		}
	    }
	  PRINTCHAR ('\"');
	}
      break;

    case Lisp_Symbol:
      {
	register int confusing;
	register unsigned char *p = XSYMBOL (obj)->name->data;
	register unsigned char *end = p + XSYMBOL (obj)->name->size;
	register unsigned char c;

	if (p != end && (*p == '-' || *p == '+')) p++;
        if (p == end)
	  confusing = 0;
	else
	  {
	    while (p != end && *p >= '0' && *p <= '9')
	      p++;
	    confusing = (end == p);
	  }

	p = XSYMBOL (obj)->name->data;
	while (p != end)
	  {
	    QUIT;
	    c = *p++;
	    if (escapeflag)
	      {
		if (c == '\"' || c == '\\' || c == '\'' || c == ';' || c == '#' ||
		    c == '(' || c == ')' || c == ',' || c =='.' || c == '`' ||
		    c == '[' || c == ']' || c == '?' || c <= 040 || confusing)
		  PRINTCHAR ('\\'), confusing = 0;
	      }
	    PRINTCHAR (c);
	  }
      }
      break;

    case Lisp_Cons:
      PRINTCHAR ('(');
      {
	register int i = 0;
	register int max = 0;

	if (XTYPE (Vprint_length) == Lisp_Int)
	  max = XINT (Vprint_length);
	while (CONSP (obj))
	  {
	    if (i++)
	      PRINTCHAR (' ');
	    if (max && i > max)
	      {
		strout ("...", 3, printcharfun);
		break;
	      }
	    print (Fcar (obj), printcharfun, escapeflag);
	    obj = Fcdr (obj);
	  }
      }
      if (!NULL (obj) && !CONSP (obj))
	{
	  strout (" . ", 3, printcharfun);
	  print (obj, printcharfun, escapeflag);
	}
      PRINTCHAR (')');
      break;

    case Lisp_Vector:
      PRINTCHAR ('[');
      {
	register int i;
	register Lisp_Object tem;
	for (i = 0; i < XVECTOR (obj)->size; i++)
	  {
	    if (i) PRINTCHAR (' ');
	    tem = XVECTOR (obj)->contents[i];
	    print (tem, printcharfun, escapeflag);
	  }
      }
      PRINTCHAR (']');
      break;

#ifndef standalone
    case Lisp_Buffer:
      if (NULL (XBUFFER (obj)->name))
	strout ("#<killed buffer>", -1, printcharfun);
      else if (escapeflag)
	{
	  strout ("#<buffer ", -1, printcharfun);
	  strout (XSTRING (XBUFFER (obj)->name)->data, -1, printcharfun);
	  PRINTCHAR ('>');
	}
      else
	strout (XSTRING (XBUFFER (obj)->name)->data, -1, printcharfun);
      break;

    case Lisp_Process:
      if (escapeflag)
	{
	  strout ("#<process ", -1, printcharfun);
	  strout (XSTRING (XPROCESS (obj)->name)->data, -1, printcharfun);
	  PRINTCHAR ('>');
	}
      else
	strout (XSTRING (XPROCESS (obj)->name)->data, -1, printcharfun);
      break;

    case Lisp_Window:
      strout ("#<window ", -1, printcharfun);
      sprintf (buf, "%d", XFASTINT (XWINDOW (obj)->sequence_number));
      strout (buf, -1, printcharfun);
      if (!NULL (XWINDOW (obj)->buffer))
	{
	  unsigned char *p = XSTRING (XBUFFER (XWINDOW (obj)->buffer)->name)->data;
	  strout (" on ", -1, printcharfun);
	  strout (p, -1, printcharfun);
	}
      PRINTCHAR ('>');
      break;

    case Lisp_Window_Configuration:
      strout ("#<window-configuration>", -1, printcharfun);
      break;

    case Lisp_Marker:
      strout ("#<marker ", -1, printcharfun);
      if (!(XMARKER (obj)->buffer))
	strout ("in no buffer", -1, printcharfun);
      else
	{
	  sprintf (buf, "at %d", marker_position (obj));
	  strout (buf, -1, printcharfun);
	  strout (" in ", -1, printcharfun);
	  strout (XSTRING (XMARKER (obj)->buffer->name)->data, -1, printcharfun);
	}
      PRINTCHAR ('>');
      break;
#endif /* standalone */

    case Lisp_Subr:
      strout ("#<subr ", -1, printcharfun);
      strout (XSUBR (obj)->symbol_name, -1, printcharfun);
      PRINTCHAR ('>');
      break;
    }

  print_depth--;
}

void
syms_of_print ()
{
  DEFVAR_LISP ("standard-output", &Vstandard_output,
    "Function print uses by default for outputting a character.\n\
This may be any function of one argument.\n\
It may also be a buffer (output is inserted before point)\n\
or a marker (output is inserted and the marker is advanced)\n\
or the symbol t (output appears in the minibuffer line).");
  Vstandard_output = Qt;
  Qstandard_output = intern ("standard-output");
  staticpro (&Qstandard_output);

  DEFVAR_LISP ("print-length", &Vprint_length,
    "Maximum length of list to print before abbreviating.\
`nil' means no limit.");
  Vprint_length = Qnil;

  DEFVAR_BOOL ("print-escape-newlines", &print_escape_newlines,
    "Non-nil means print newlines in strings as backslash-n.");
  print_escape_newlines = 0;

  /* prin1_to_string_buffer initialized in init_buffer_once in buffer.c */
  staticpro (&Vprin1_to_string_buffer);

  defsubr (&Sprin1);
  defsubr (&Sprin1_to_string);
  defsubr (&Sprinc);
  defsubr (&Sprint);
  defsubr (&Sterpri);
  defsubr (&Swrite_char);
#ifndef standalone
  defsubr (&Swith_output_to_temp_buffer);
#endif /* not standalone */
}
