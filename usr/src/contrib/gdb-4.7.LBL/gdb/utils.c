/* General utility routines for GDB, the GNU debugger.
   Copyright 1986, 1989, 1990, 1991, 1992 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "defs.h"
#if !defined(__GO32__)
#include <sys/ioctl.h>
#include <sys/param.h>
#include <pwd.h>
#endif
#include <varargs.h>
#include <ctype.h>
#include <string.h>

#include "signals.h"
#include "gdbcmd.h"
#include "terminal.h"
#include "bfd.h"
#include "target.h"
#include "demangle.h"

/* Prototypes for local functions */

#if !defined (NO_MALLOC_CHECK)

static void
malloc_botch PARAMS ((void));

#endif /* NO_MALLOC_CHECK  */

static void
fatal_dump_core ();	/* Can't prototype with <varargs.h> usage... */

static void
prompt_for_continue PARAMS ((void));

static void 
set_width_command PARAMS ((char *, int, struct cmd_list_element *));

/* If this definition isn't overridden by the header files, assume
   that isatty and fileno exist on this system.  */
#ifndef ISATTY
#define ISATTY(FP)	(isatty (fileno (FP)))
#endif

/* Chain of cleanup actions established with make_cleanup,
   to be executed if an error happens.  */

static struct cleanup *cleanup_chain;

/* Nonzero means a quit has been requested.  */

int quit_flag;

/* Nonzero means quit immediately if Control-C is typed now,
   rather than waiting until QUIT is executed.  */

int immediate_quit;

/* Nonzero means that encoded C++ names should be printed out in their
   C++ form rather than raw.  */

int demangle = 1;

/* Nonzero means that encoded C++ names should be printed out in their
   C++ form even in assembler language displays.  If this is set, but
   DEMANGLE is zero, names are printed raw, i.e. DEMANGLE controls.  */

int asm_demangle = 0;

/* Nonzero means that strings with character values >0x7F should be printed
   as octal escapes.  Zero means just print the value (e.g. it's an
   international character, and the terminal or window can cope.)  */

int sevenbit_strings = 0;

/* String to be printed before error messages, if any.  */

char *error_pre_print;
char *warning_pre_print = "\nwarning: ";

/* Add a new cleanup to the cleanup_chain,
   and return the previous chain pointer
   to be passed later to do_cleanups or discard_cleanups.
   Args are FUNCTION to clean up with, and ARG to pass to it.  */

struct cleanup *
make_cleanup (function, arg)
     void (*function) PARAMS ((PTR));
     PTR arg;
{
  register struct cleanup *new
    = (struct cleanup *) xmalloc (sizeof (struct cleanup));
  register struct cleanup *old_chain = cleanup_chain;

  new->next = cleanup_chain;
  new->function = function;
  new->arg = arg;
  cleanup_chain = new;

  return old_chain;
}

/* Discard cleanups and do the actions they describe
   until we get back to the point OLD_CHAIN in the cleanup_chain.  */

void
do_cleanups (old_chain)
     register struct cleanup *old_chain;
{
  register struct cleanup *ptr;
  while ((ptr = cleanup_chain) != old_chain)
    {
      cleanup_chain = ptr->next;	/* Do this first incase recursion */
      (*ptr->function) (ptr->arg);
      free (ptr);
    }
}

/* Discard cleanups, not doing the actions they describe,
   until we get back to the point OLD_CHAIN in the cleanup_chain.  */

void
discard_cleanups (old_chain)
     register struct cleanup *old_chain;
{
  register struct cleanup *ptr;
  while ((ptr = cleanup_chain) != old_chain)
    {
      cleanup_chain = ptr->next;
      free ((PTR)ptr);
    }
}

/* Set the cleanup_chain to 0, and return the old cleanup chain.  */
struct cleanup *
save_cleanups ()
{
  struct cleanup *old_chain = cleanup_chain;

  cleanup_chain = 0;
  return old_chain;
}

/* Restore the cleanup chain from a previously saved chain.  */
void
restore_cleanups (chain)
     struct cleanup *chain;
{
  cleanup_chain = chain;
}

/* This function is useful for cleanups.
   Do

     foo = xmalloc (...);
     old_chain = make_cleanup (free_current_contents, &foo);

   to arrange to free the object thus allocated.  */

void
free_current_contents (location)
     char **location;
{
  free (*location);
}

/* Provide a known function that does nothing, to use as a base for
   for a possibly long chain of cleanups.  This is useful where we
   use the cleanup chain for handling normal cleanups as well as dealing
   with cleanups that need to be done as a result of a call to error().
   In such cases, we may not be certain where the first cleanup is, unless
   we have a do-nothing one to always use as the base. */

/* ARGSUSED */
void
null_cleanup (arg)
    char **arg;
{
}


/* Provide a hook for modules wishing to print their own warning messages
   to set up the terminal state in a compatible way, without them having
   to import all the target_<...> macros. */

void
warning_setup ()
{
  target_terminal_ours ();
  wrap_here("");			/* Force out any buffered output */
  fflush (stdout);
}

/* Print a warning message.
   The first argument STRING is the warning message, used as a fprintf string,
   and the remaining args are passed as arguments to it.
   The primary difference between warnings and errors is that a warning
   does not force the return to command level. */

/* VARARGS */
void
warning (va_alist)
     va_dcl
{
  va_list args;
  char *string;

  va_start (args);
  target_terminal_ours ();
  wrap_here("");			/* Force out any buffered output */
  fflush (stdout);
  if (warning_pre_print)
    fprintf (stderr, warning_pre_print);
  string = va_arg (args, char *);
  vfprintf (stderr, string, args);
  fprintf (stderr, "\n");
  va_end (args);
}

/* Print an error message and return to command level.
   The first argument STRING is the error message, used as a fprintf string,
   and the remaining args are passed as arguments to it.  */

/* VARARGS */
NORETURN void
error (va_alist)
     va_dcl
{
  va_list args;
  char *string;

  va_start (args);
  target_terminal_ours ();
  wrap_here("");			/* Force out any buffered output */
  fflush (stdout);
  if (error_pre_print)
    fprintf_filtered (stderr, error_pre_print);
  string = va_arg (args, char *);
  vfprintf_filtered (stderr, string, args);
  fprintf_filtered (stderr, "\n");
  va_end (args);
  return_to_top_level ();
}

/* Print an error message and exit reporting failure.
   This is for a error that we cannot continue from.
   The arguments are printed a la printf.

   This function cannot be declared volatile (NORETURN) in an
   ANSI environment because exit() is not declared volatile. */

/* VARARGS */
NORETURN void
fatal (va_alist)
     va_dcl
{
  va_list args;
  char *string;

  va_start (args);
  string = va_arg (args, char *);
  fprintf (stderr, "\ngdb: ");
  vfprintf (stderr, string, args);
  fprintf (stderr, "\n");
  va_end (args);
  exit (1);
}

/* Print an error message and exit, dumping core.
   The arguments are printed a la printf ().  */

/* VARARGS */
static void
fatal_dump_core (va_alist)
     va_dcl
{
  va_list args;
  char *string;

  va_start (args);
  string = va_arg (args, char *);
  /* "internal error" is always correct, since GDB should never dump
     core, no matter what the input.  */
  fprintf (stderr, "\ngdb internal error: ");
  vfprintf (stderr, string, args);
  fprintf (stderr, "\n");
  va_end (args);

  signal (SIGQUIT, SIG_DFL);
  kill (getpid (), SIGQUIT);
  /* We should never get here, but just in case...  */
  exit (1);
}

/* The strerror() function can return NULL for errno values that are
   out of range.  Provide a "safe" version that always returns a
   printable string. */

char *
safe_strerror (errnum)
     int errnum;
{
  char *msg;
  static char buf[32];

  if ((msg = strerror (errnum)) == NULL)
    {
      sprintf (buf, "(undocumented errno %d)", errnum);
      msg = buf;
    }
  return (msg);
}

/* The strsignal() function can return NULL for signal values that are
   out of range.  Provide a "safe" version that always returns a
   printable string. */

char *
safe_strsignal (signo)
     int signo;
{
  char *msg;
  static char buf[32];

  if ((msg = strsignal (signo)) == NULL)
    {
      sprintf (buf, "(undocumented signal %d)", signo);
      msg = buf;
    }
  return (msg);
}


/* Print the system error message for errno, and also mention STRING
   as the file name for which the error was encountered.
   Then return to command level.  */

void
perror_with_name (string)
     char *string;
{
  char *err;
  char *combined;

  err = safe_strerror (errno);
  combined = (char *) alloca (strlen (err) + strlen (string) + 3);
  strcpy (combined, string);
  strcat (combined, ": ");
  strcat (combined, err);

  /* I understand setting these is a matter of taste.  Still, some people
     may clear errno but not know about bfd_error.  Doing this here is not
     unreasonable. */
  bfd_error = no_error;
  errno = 0;

  error ("%s.", combined);
}

/* Print the system error message for ERRCODE, and also mention STRING
   as the file name for which the error was encountered.  */

void
print_sys_errmsg (string, errcode)
     char *string;
     int errcode;
{
  char *err;
  char *combined;

  err = safe_strerror (errcode);
  combined = (char *) alloca (strlen (err) + strlen (string) + 3);
  strcpy (combined, string);
  strcat (combined, ": ");
  strcat (combined, err);

  fprintf (stderr, "%s.\n", combined);
}

/* Control C eventually causes this to be called, at a convenient time.  */

void
quit ()
{
  target_terminal_ours ();
  wrap_here ((char *)0);		/* Force out any pending output */
#if !defined(__GO32__)
#ifdef HAVE_TERMIO
  ioctl (fileno (stdout), TCFLSH, 1);
#else /* not HAVE_TERMIO */
  ioctl (fileno (stdout), TIOCFLUSH, 0);
#endif /* not HAVE_TERMIO */
#ifdef TIOCGPGRP
  error ("Quit");
#else
  error ("Quit (expect signal %d when inferior is resumed)", SIGINT);
#endif /* TIOCGPGRP */
#endif
}

/* Control C comes here */

void
request_quit (signo)
     int signo;
{
  quit_flag = 1;

#ifdef USG
  /* Restore the signal handler.  */
  signal (signo, request_quit);
#endif

  if (immediate_quit)
    quit ();
}


/* Memory management stuff (malloc friends).  */

#if defined (NO_MMALLOC)

PTR
mmalloc (md, size)
     PTR md;
     long size;
{
  return (malloc (size));
}

PTR
mrealloc (md, ptr, size)
     PTR md;
     PTR ptr;
     long size;
{
  if (ptr == 0)		/* Guard against old realloc's */
    return malloc (size);
  else
    return realloc (ptr, size);
}

void
mfree (md, ptr)
     PTR md;
     PTR ptr;
{
  free (ptr);
}

#endif	/* NO_MMALLOC */

#if defined (NO_MMALLOC) || defined (NO_MMALLOC_CHECK)

void
init_malloc (md)
     PTR md;
{
}

#else /* have mmalloc and want corruption checking  */

static void
malloc_botch ()
{
  fatal_dump_core ("Memory corruption");
}

/* Attempt to install hooks in mmalloc/mrealloc/mfree for the heap specified
   by MD, to detect memory corruption.  Note that MD may be NULL to specify
   the default heap that grows via sbrk.

   Note that for freshly created regions, we must call mmcheck prior to any
   mallocs in the region.  Otherwise, any region which was allocated prior to
   installing the checking hooks, which is later reallocated or freed, will
   fail the checks!  The mmcheck function only allows initial hooks to be
   installed before the first mmalloc.  However, anytime after we have called
   mmcheck the first time to install the checking hooks, we can call it again
   to update the function pointer to the memory corruption handler.

   Returns zero on failure, non-zero on success. */

void
init_malloc (md)
     PTR md;
{
  if (!mmcheck (md, malloc_botch))
    {
      warning ("internal error: failed to install memory consistency checks");
    }

  mmtrace ();
}

#endif /* Have mmalloc and want corruption checking  */

/* Called when a memory allocation fails, with the number of bytes of
   memory requested in SIZE. */

NORETURN void
nomem (size)
     long size;
{
  if (size > 0)
    {
      fatal ("virtual memory exhausted: can't allocate %ld bytes.", size);
    }
  else
    {
      fatal ("virtual memory exhausted.");
    }
}

/* Like mmalloc but get error if no storage available, and protect against
   the caller wanting to allocate zero bytes.  Whether to return NULL for
   a zero byte request, or translate the request into a request for one
   byte of zero'd storage, is a religious issue. */

PTR
xmmalloc (md, size)
     PTR md;
     long size;
{
  register PTR val;

  if (size == 0)
    {
      val = NULL;
    }
  else if ((val = mmalloc (md, size)) == NULL)
    {
      nomem (size);
    }
  return (val);
}

/* Like mrealloc but get error if no storage available.  */

PTR
xmrealloc (md, ptr, size)
     PTR md;
     PTR ptr;
     long size;
{
  register PTR val;

  if (ptr != NULL)
    {
      val = mrealloc (md, ptr, size);
    }
  else
    {
      val = mmalloc (md, size);
    }
  if (val == NULL)
    {
      nomem (size);
    }
  return (val);
}

/* Like malloc but get error if no storage available, and protect against
   the caller wanting to allocate zero bytes.  */

PTR
xmalloc (size)
     long size;
{
  return (xmmalloc ((void *) NULL, size));
}

/* Like mrealloc but get error if no storage available.  */

PTR
xrealloc (ptr, size)
     PTR ptr;
     long size;
{
  return (xmrealloc ((void *) NULL, ptr, size));
}


/* My replacement for the read system call.
   Used like `read' but keeps going if `read' returns too soon.  */

int
myread (desc, addr, len)
     int desc;
     char *addr;
     int len;
{
  register int val;
  int orglen = len;

  while (len > 0)
    {
      val = read (desc, addr, len);
      if (val < 0)
	return val;
      if (val == 0)
	return orglen - len;
      len -= val;
      addr += val;
    }
  return orglen;
}

/* Make a copy of the string at PTR with SIZE characters
   (and add a null character at the end in the copy).
   Uses malloc to get the space.  Returns the address of the copy.  */

char *
savestring (ptr, size)
     const char *ptr;
     int size;
{
  register char *p = (char *) xmalloc (size + 1);
  memcpy (p, ptr, size);
  p[size] = 0;
  return p;
}

char *
msavestring (md, ptr, size)
     void *md;
     const char *ptr;
     int size;
{
  register char *p = (char *) xmmalloc (md, size + 1);
  memcpy (p, ptr, size);
  p[size] = 0;
  return p;
}

/* The "const" is so it compiles under DGUX (which prototypes strsave
   in <string.h>.  FIXME: This should be named "xstrsave", shouldn't it?
   Doesn't real strsave return NULL if out of memory?  */
char *
strsave (ptr)
     const char *ptr;
{
  return savestring (ptr, strlen (ptr));
}

char *
mstrsave (md, ptr)
     void *md;
     const char *ptr;
{
  return (msavestring (md, ptr, strlen (ptr)));
}

void
print_spaces (n, file)
     register int n;
     register FILE *file;
{
  while (n-- > 0)
    fputc (' ', file);
}

/* Ask user a y-or-n question and return 1 iff answer is yes.
   Takes three args which are given to printf to print the question.
   The first, a control string, should end in "? ".
   It should not say how to answer, because we do that.  */

/* VARARGS */
int
query (va_alist)
     va_dcl
{
  va_list args;
  char *ctlstr;
  register int answer;
  register int ans2;

  /* Automatically answer "yes" if input is not from a terminal.  */
  if (!input_from_terminal_p ())
    return 1;

  while (1)
    {
      wrap_here ("");		/* Flush any buffered output */
      fflush (stdout);
      va_start (args);
      ctlstr = va_arg (args, char *);
      vfprintf_filtered (stdout, ctlstr, args);
      va_end (args);
      printf_filtered ("(y or n) ");
      fflush (stdout);
      answer = fgetc (stdin);
      clearerr (stdin);		/* in case of C-d */
      if (answer == EOF)	/* C-d */
        return 1;
      if (answer != '\n')	/* Eat rest of input line, to EOF or newline */
	do 
	  {
	    ans2 = fgetc (stdin);
	    clearerr (stdin);
	  }
        while (ans2 != EOF && ans2 != '\n');
      if (answer >= 'a')
	answer -= 040;
      if (answer == 'Y')
	return 1;
      if (answer == 'N')
	return 0;
      printf_filtered ("Please answer y or n.\n");
    }
}


/* Parse a C escape sequence.  STRING_PTR points to a variable
   containing a pointer to the string to parse.  That pointer
   should point to the character after the \.  That pointer
   is updated past the characters we use.  The value of the
   escape sequence is returned.

   A negative value means the sequence \ newline was seen,
   which is supposed to be equivalent to nothing at all.

   If \ is followed by a null character, we return a negative
   value and leave the string pointer pointing at the null character.

   If \ is followed by 000, we return 0 and leave the string pointer
   after the zeros.  A value of 0 does not mean end of string.  */

int
parse_escape (string_ptr)
     char **string_ptr;
{
  register int c = *(*string_ptr)++;
  switch (c)
    {
    case 'a':
      return 007;		/* Bell (alert) char */
    case 'b':
      return '\b';
    case 'e':			/* Escape character */
      return 033;
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\n':
      return -2;
    case 0:
      (*string_ptr)--;
      return 0;
    case '^':
      c = *(*string_ptr)++;
      if (c == '\\')
	c = parse_escape (string_ptr);
      if (c == '?')
	return 0177;
      return (c & 0200) | (c & 037);
      
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      {
	register int i = c - '0';
	register int count = 0;
	while (++count < 3)
	  {
	    if ((c = *(*string_ptr)++) >= '0' && c <= '7')
	      {
		i *= 8;
		i += c - '0';
	      }
	    else
	      {
		(*string_ptr)--;
		break;
	      }
	  }
	return i;
      }
    default:
      return c;
    }
}

/* Print the character C on STREAM as part of the contents
   of a literal string whose delimiter is QUOTER.  */

void
printchar (c, stream, quoter)
     register int c;
     FILE *stream;
     int quoter;
{

  c &= 0xFF;			/* Avoid sign bit follies */

  if (              c < 0x20  ||		/* Low control chars */	
      (c >= 0x7F && c < 0xA0) ||		/* DEL, High controls */
      (sevenbit_strings && c >= 0x80)) {	/* high order bit set */
    switch (c)
      {
      case '\n':
	fputs_filtered ("\\n", stream);
	break;
      case '\b':
	fputs_filtered ("\\b", stream);
	break;
      case '\t':
	fputs_filtered ("\\t", stream);
	break;
      case '\f':
	fputs_filtered ("\\f", stream);
	break;
      case '\r':
	fputs_filtered ("\\r", stream);
	break;
      case '\033':
	fputs_filtered ("\\e", stream);
	break;
      case '\007':
	fputs_filtered ("\\a", stream);
	break;
      default:
	fprintf_filtered (stream, "\\%X", (unsigned int) c);
	break;
      }
  } else {
    if (c == '\\' || c == quoter)
      fputs_filtered ("\\", stream);
    fprintf_filtered (stream, "%c", c);
  }
}

/* Number of lines per page or UINT_MAX if paging is disabled.  */
static unsigned int lines_per_page;
/* Number of chars per line or UNIT_MAX is line folding is disabled.  */
static unsigned int chars_per_line;
/* Current count of lines printed on this page, chars on this line.  */
static unsigned int lines_printed, chars_printed;

/* Buffer and start column of buffered text, for doing smarter word-
   wrapping.  When someone calls wrap_here(), we start buffering output
   that comes through fputs_filtered().  If we see a newline, we just
   spit it out and forget about the wrap_here().  If we see another
   wrap_here(), we spit it out and remember the newer one.  If we see
   the end of the line, we spit out a newline, the indent, and then
   the buffered output.

   wrap_column is the column number on the screen where wrap_buffer begins.
     When wrap_column is zero, wrapping is not in effect.
   wrap_buffer is malloc'd with chars_per_line+2 bytes. 
     When wrap_buffer[0] is null, the buffer is empty.
   wrap_pointer points into it at the next character to fill.
   wrap_indent is the string that should be used as indentation if the
     wrap occurs.  */

static char *wrap_buffer, *wrap_pointer, *wrap_indent;
static int wrap_column;

/* ARGSUSED */
static void 
set_width_command (args, from_tty, c)
     char *args;
     int from_tty;
     struct cmd_list_element *c;
{
  if (!wrap_buffer)
    {
      wrap_buffer = (char *) xmalloc (chars_per_line + 2);
      wrap_buffer[0] = '\0';
    }
  else
    wrap_buffer = (char *) xrealloc (wrap_buffer, chars_per_line + 2);
  wrap_pointer = wrap_buffer;	/* Start it at the beginning */
}

extern FILE *instream;

static void
instream_cleanup(stream)
    FILE *stream;
{
  instream = stream;
}

static void
prompt_for_continue ()
{
  if (ISATTY(stdin) && ISATTY(stdout))
    {
      struct cleanup *old_chain = make_cleanup(instream_cleanup, instream);
      char *cp;

      instream = stdin;
      immediate_quit++;
      cp = gdb_readline ("---Type <return> to continue---");
      if (cp)
        free (cp);
      chars_printed = lines_printed = 0;
      immediate_quit--;
      do_cleanups(old_chain);
      dont_repeat ();		/* Forget prev cmd -- CR won't repeat it. */
    }
}

/* Reinitialize filter; ie. tell it to reset to original values.  */

void
reinitialize_more_filter ()
{
  lines_printed = 0;
  chars_printed = 0;
}

/* Indicate that if the next sequence of characters overflows the line,
   a newline should be inserted here rather than when it hits the end. 
   If INDENT is nonzero, it is a string to be printed to indent the
   wrapped part on the next line.  INDENT must remain accessible until
   the next call to wrap_here() or until a newline is printed through
   fputs_filtered().

   If the line is already overfull, we immediately print a newline and
   the indentation, and disable further wrapping.

   If we don't know the width of lines, but we know the page height,
   we must not wrap words, but should still keep track of newlines
   that were explicitly printed.

   INDENT should not contain tabs, as that
   will mess up the char count on the next line.  FIXME.  */

void
wrap_here(indent)
  char *indent;
{
  if (wrap_buffer[0])
    {
      *wrap_pointer = '\0';
      fputs (wrap_buffer, stdout);
    }
  wrap_pointer = wrap_buffer;
  wrap_buffer[0] = '\0';
  if (chars_per_line == UINT_MAX)		/* No line overflow checking */
    {
      wrap_column = 0;
    }
  else if (chars_printed >= chars_per_line)
    {
      puts_filtered ("\n");
      puts_filtered (indent);
      wrap_column = 0;
    }
  else
    {
      wrap_column = chars_printed;
      wrap_indent = indent;
    }
}

/* Like fputs but pause after every screenful, and can wrap at points
   other than the final character of a line.
   Unlike fputs, fputs_filtered does not return a value.
   It is OK for LINEBUFFER to be NULL, in which case just don't print
   anything.

   Note that a longjmp to top level may occur in this routine
   (since prompt_for_continue may do so) so this routine should not be
   called when cleanups are not in place.  */

void
fputs_filtered (linebuffer, stream)
     const char *linebuffer;
     FILE *stream;
{
  const char *lineptr;

  if (linebuffer == 0)
    return;
  
  /* Don't do any filtering if it is disabled.  */
  if (stream != stdout
   || (lines_per_page == UINT_MAX && chars_per_line == UINT_MAX))
    {
      fputs (linebuffer, stream);
      return;
    }

  /* Go through and output each character.  Show line extension
     when this is necessary; prompt user for new page when this is
     necessary.  */
  
  lineptr = linebuffer;
  while (*lineptr)
    {
      /* Possible new page.  */
      if (lines_printed >= lines_per_page - 1)
	prompt_for_continue ();

      while (*lineptr && *lineptr != '\n')
	{
	  /* Print a single line.  */
	  if (*lineptr == '\t')
	    {
	      if (wrap_column)
		*wrap_pointer++ = '\t';
	      else
		putc ('\t', stream);
	      /* Shifting right by 3 produces the number of tab stops
	         we have already passed, and then adding one and
		 shifting left 3 advances to the next tab stop.  */
	      chars_printed = ((chars_printed >> 3) + 1) << 3;
	      lineptr++;
	    }
	  else
	    {
	      if (wrap_column)
		*wrap_pointer++ = *lineptr;
	      else
	        putc (*lineptr, stream);
	      chars_printed++;
	      lineptr++;
	    }
      
	  if (chars_printed >= chars_per_line)
	    {
	      unsigned int save_chars = chars_printed;

	      chars_printed = 0;
	      lines_printed++;
	      /* If we aren't actually wrapping, don't output newline --
		 if chars_per_line is right, we probably just overflowed
		 anyway; if it's wrong, let us keep going.  */
	      if (wrap_column)
		putc ('\n', stream);

	      /* Possible new page.  */
	      if (lines_printed >= lines_per_page - 1)
		prompt_for_continue ();

	      /* Now output indentation and wrapped string */
	      if (wrap_column)
		{
		  if (wrap_indent)
		    fputs (wrap_indent, stream);
		  *wrap_pointer = '\0';		/* Null-terminate saved stuff */
		  fputs (wrap_buffer, stream);	/* and eject it */
		  /* FIXME, this strlen is what prevents wrap_indent from
		     containing tabs.  However, if we recurse to print it
		     and count its chars, we risk trouble if wrap_indent is
		     longer than (the user settable) chars_per_line. 
		     Note also that this can set chars_printed > chars_per_line
		     if we are printing a long string.  */
		  chars_printed = strlen (wrap_indent)
				+ (save_chars - wrap_column);
		  wrap_pointer = wrap_buffer;	/* Reset buffer */
		  wrap_buffer[0] = '\0';
		  wrap_column = 0;		/* And disable fancy wrap */
 		}
	    }
	}

      if (*lineptr == '\n')
	{
	  chars_printed = 0;
	  wrap_here ((char *)0);  /* Spit out chars, cancel further wraps */
	  lines_printed++;
	  putc ('\n', stream);
	  lineptr++;
	}
    }
}


/* fputs_demangled is a variant of fputs_filtered that
   demangles g++ names.*/

void
fputs_demangled (linebuffer, stream, arg_mode)
     char *linebuffer;
     FILE *stream;
     int arg_mode;
{
#define SYMBOL_MAX 1024

#define SYMBOL_CHAR(c) (isascii(c) \
  && (isalnum(c) || (c) == '_' || (c) == CPLUS_MARKER))

  char buf[SYMBOL_MAX+1];
# define DMSLOP 5		/* How much room to leave in buf */
  char *p;

  if (linebuffer == NULL)
    return;

  /* If user wants to see raw output, no problem.  */
  if (!demangle) {
    fputs_filtered (linebuffer, stream);
    return;
  }

  p = linebuffer;

  while ( *p != (char) 0 ) {
    int i = 0;

    /* collect non-interesting characters into buf */
    while (*p != (char) 0 && !SYMBOL_CHAR(*p) && i < (int)sizeof(buf)-DMSLOP ) {
      buf[i++] = *p;
      p++;
    }
    if (i > 0) {
      /* output the non-interesting characters without demangling */
      buf[i] = (char) 0;
      fputs_filtered(buf, stream);
      i = 0;  /* reset buf */
    }

    /* and now the interesting characters */
    while (i < SYMBOL_MAX
     && *p != (char) 0
     && SYMBOL_CHAR(*p)
     && i < (int)sizeof(buf) - DMSLOP) {
      buf[i++] = *p;
      p++;
    }
    buf[i] = (char) 0;
    if (i > 0) {
      char * result;
      
      if ( (result = cplus_demangle(buf, arg_mode)) != NULL ) {
	fputs_filtered(result, stream);
	free(result);
      }
      else {
	fputs_filtered(buf, stream);
      }
    }
  }
}

/* Print a variable number of ARGS using format FORMAT.  If this
   information is going to put the amount written (since the last call
   to REINITIALIZE_MORE_FILTER or the last page break) over the page size,
   print out a pause message and do a gdb_readline to get the users
   permision to continue.

   Unlike fprintf, this function does not return a value.

   We implement three variants, vfprintf (takes a vararg list and stream),
   fprintf (takes a stream to write on), and printf (the usual).

   Note that this routine has a restriction that the length of the
   final output line must be less than 255 characters *or* it must be
   less than twice the size of the format string.  This is a very
   arbitrary restriction, but it is an internal restriction, so I'll
   put it in.  This means that the %s format specifier is almost
   useless; unless the caller can GUARANTEE that the string is short
   enough, fputs_filtered should be used instead.

   Note also that a longjmp to top level may occur in this routine
   (since prompt_for_continue may do so) so this routine should not be
   called when cleanups are not in place.  */

#define	MIN_LINEBUF	255

void
vfprintf_filtered (stream, format, args)
     FILE *stream;
     char *format;
     va_list args;
{
  char line_buf[MIN_LINEBUF+10];
  char *linebuffer = line_buf;
  int format_length;

  format_length = strlen (format);

  /* Reallocate buffer to a larger size if this is necessary.  */
  if (format_length * 2 > MIN_LINEBUF)
    {
      linebuffer = alloca (10 + format_length * 2);
    }

  /* This won't blow up if the restrictions described above are
     followed.   */
  vsprintf (linebuffer, format, args);

  fputs_filtered (linebuffer, stream);
}

/* VARARGS */
void
fprintf_filtered (va_alist)
     va_dcl
{
  va_list args;
  FILE *stream;
  char *format;

  va_start (args);
  stream = va_arg (args, FILE *);
  format = va_arg (args, char *);

  /* This won't blow up if the restrictions described above are
     followed.   */
  vfprintf_filtered (stream, format, args);
  va_end (args);
}

/* Like fprintf_filtered, but prints it's result indent.
   Called as fprintfi_filtered (spaces, format, arg1, arg2, ...); */

/* VARARGS */
void
fprintfi_filtered (va_alist)
     va_dcl
{
  va_list args;
  int spaces;
  FILE *stream;
  char *format;

  va_start (args);
  spaces = va_arg (args, int);
  stream = va_arg (args, FILE *);
  format = va_arg (args, char *);
  print_spaces_filtered (spaces, stream);

  /* This won't blow up if the restrictions described above are
     followed.   */
  vfprintf_filtered (stream, format, args);
  va_end (args);
}

/* VARARGS */
void
printf_filtered (va_alist)
     va_dcl
{
  va_list args;
  char *format;

  va_start (args);
  format = va_arg (args, char *);

  vfprintf_filtered (stdout, format, args);
  va_end (args);
}

/* Like printf_filtered, but prints it's result indented.
   Called as printfi_filtered (spaces, format, arg1, arg2, ...); */

/* VARARGS */
void
printfi_filtered (va_alist)
     va_dcl
{
  va_list args;
  int spaces;
  char *format;

  va_start (args);
  spaces = va_arg (args, int);
  format = va_arg (args, char *);
  print_spaces_filtered (spaces, stdout);
  vfprintf_filtered (stdout, format, args);
  va_end (args);
}

/* Easy -- but watch out!

   This routine is *not* a replacement for puts()!  puts() appends a newline.
   This one doesn't, and had better not!  */

void
puts_filtered (string)
     char *string;
{
  fputs_filtered (string, stdout);
}

/* Return a pointer to N spaces and a null.  The pointer is good
   until the next call to here.  */
char *
n_spaces (n)
     int n;
{
  register char *t;
  static char *spaces;
  static int max_spaces;

  if (n > max_spaces)
    {
      if (spaces)
	free (spaces);
      spaces = (char *) xmalloc (n+1);
      for (t = spaces+n; t != spaces;)
	*--t = ' ';
      spaces[n] = '\0';
      max_spaces = n;
    }

  return spaces + max_spaces - n;
}

/* Print N spaces.  */
void
print_spaces_filtered (n, stream)
     int n;
     FILE *stream;
{
  fputs_filtered (n_spaces (n), stream);
}

/* C++ demangler stuff.  */

/* Make a copy of a symbol, applying C++ demangling if demangling is enabled
   and a demangled version exists.  Note that the value returned from
   cplus_demangle is already allocated in malloc'd memory. */

char *
strdup_demangled (name)
     const char *name;
{
  char *demangled = NULL;

  if (demangle)
    {
      demangled = cplus_demangle (name, DMGL_PARAMS | DMGL_ANSI);
    }
  return ((demangled != NULL) ? demangled : strdup (name));
}


/* Print NAME on STREAM, demangling if necessary.  */
void
fprint_symbol (stream, name)
     FILE *stream;
     char *name;
{
  char *demangled;
  if ((!demangle)
      || NULL == (demangled = cplus_demangle (name, DMGL_PARAMS | DMGL_ANSI)))
    fputs_filtered (name, stream);
  else
    {
      fputs_filtered (demangled, stream);
      free (demangled);
    }
}

/* Do a strcmp() type operation on STRING1 and STRING2, ignoring any
   differences in whitespace.  Returns 0 if they match, non-zero if they
   don't (slightly different than strcmp()'s range of return values).
   
   As an extra hack, string1=="FOO(ARGS)" matches string2=="FOO".
   This "feature" is useful for demangle_and_match(), which is used
   when searching for matching C++ function names (such as if the
   user types 'break FOO', where FOO is a mangled C++ function). */

int
strcmp_iw (string1, string2)
     const char *string1;
     const char *string2;
{
  while ((*string1 != '\0') && (*string2 != '\0'))
    {
      while (isspace (*string1))
	{
	  string1++;
	}
      while (isspace (*string2))
	{
	  string2++;
	}
      if (*string1 != *string2)
	{
	  break;
	}
      if (*string1 != '\0')
	{
	  string1++;
	  string2++;
	}
    }
  return (*string1 != '\0' && *string1 != '(') || (*string2 != '\0');
}

/* Demangle NAME and compare the result with LOOKFOR, ignoring any differences
   in whitespace.
   
   If a match is found, returns a pointer to the demangled version of NAME
   in malloc'd memory, which needs to be freed by the caller after use.
   If a match is not found, returns NULL.

   OPTIONS is a flags word that controls the demangling process and is just
   passed on to the demangler.

   When the caller sees a non-NULL result, it knows that NAME is the mangled
   equivalent of LOOKFOR, and it can use either NAME, the "official demangled"
   version of NAME (the return value) or the "unofficial demangled" version
   of NAME (LOOKFOR, which it already knows). */

char *
demangle_and_match (name, lookfor, options)
     const char *name;
     const char *lookfor;
     int options;
{
  char *demangled;

  if ((demangled = cplus_demangle (name, options)) != NULL)
    {
      if (strcmp_iw (demangled, lookfor) != 0)
	{
	  free (demangled);
	  demangled = NULL;
	}
    }
  return (demangled);
}

#ifdef TIOCGWINSZ
#ifdef SIGWINCH
static void
sigwinch()
{
	struct winsize win;

	if (ioctl(0, TIOCGWINSZ, (char *)&win) < 0) {
		perror("TIOCGWINSZ");
		return;
	}
	lines_per_page = win.ws_row;
	chars_per_line = win.ws_col;
}

#ifndef SIGWINCH_HANDLER
#define SIGWINCH_HANDLER sigwinch
#endif

#endif

termdim()
{
	SIGWINCH_HANDLER();
#ifdef SIGWINCH
	signal(SIGWINCH, SIGWINCH_HANDLER);
#endif
}

#else
/* Initialize the screen height and width from termcap.  */
termdim()
{
	register int v;
	register char *cp;
	/* 2048 is large enough for all known terminals, according to the
	   GNU termcap manual.  */
	char term_buffer[2048];

	if ((termtype = getenv ("TERM")) == 0 || tgetent(term_buffer, cp) <= 0)
		return;

	v = tgetnum("li");
	if (v >= 0)
		lines_per_page = v;
	else
		/* The number of lines per page is not mentioned
		   in the terminal description.  This probably means
		   that paging is not useful (e.g. emacs shell window),
		   so disable paging.  */
		lines_per_page = UINT_MAX;
	
	v = tgetnum("co");
	if (v >= 0)
		chars_per_line = v;
}
#endif

void
_initialize_utils ()
{
  struct cmd_list_element *c;

  c = add_set_cmd ("width", class_support, var_uinteger, 
		  (char *)&chars_per_line,
		  "Set number of characters gdb thinks are in a line.",
		  &setlist);
  add_show_from_set (c, &showlist);
  c->function.sfunc = set_width_command;

  add_show_from_set
    (add_set_cmd ("height", class_support,
		  var_uinteger, (char *)&lines_per_page,
		  "Set number of lines gdb thinks are in a page.", &setlist),
     &showlist);
  
  /* These defaults will be used if we are unable to get the correct
     values from termcap.  */
#if defined(__GO32__)
  lines_per_page = ScreenRows();
  chars_per_line = ScreenCols();
#else  
  lines_per_page = 24;
  chars_per_line = 80;
  termdim();
#endif
  /* If the output is not a terminal, don't paginate it.  */
  if (!ISATTY (stdout))
    lines_per_page = UINT_MAX;

  set_width_command ((char *)NULL, 0, c);

  add_show_from_set
    (add_set_cmd ("demangle", class_support, var_boolean, 
		  (char *)&demangle,
		"Set demangling of encoded C++ names when displaying symbols.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("sevenbit-strings", class_support, var_boolean, 
		  (char *)&sevenbit_strings,
   "Set printing of 8-bit characters in strings as \\nnn.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("asm-demangle", class_support, var_boolean, 
		  (char *)&asm_demangle,
	"Set demangling of C++ names in disassembly listings.",
		  &setprintlist),
     &showprintlist);
}

/* Machine specific function to handle SIGWINCH signal. */

#ifdef  SIGWINCH_HANDLER_BODY
        SIGWINCH_HANDLER_BODY
#endif
