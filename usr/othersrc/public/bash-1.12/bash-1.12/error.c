/* error.c -- Functions for handling errors. */

#include <stdio.h>

#if defined (HAVE_VFPRINTF)
#include <varargs.h>
#endif

#include <errno.h>
extern int errno;
extern char *strerror ();

#include "flags.h"
#include "error.h"

/* Report an error having to do with FILENAME. */
void
file_error (filename)
     char *filename;
{
  report_error ("%s: %s", filename, strerror (errno));
}

#if !defined (HAVE_VFPRINTF)
void
programming_error (reason, arg1, arg2, arg3, arg4, arg5)
     char *reason;
{
  extern char *the_current_maintainer;

#if defined (JOB_CONTROL)
  {
    extern pid_t shell_pgrp;
    give_terminal_to (shell_pgrp);
  }
#endif /* JOB_CONTROL */

  report_error (reason, arg1, arg2);
  fprintf (stderr, "Tell %s to fix this someday.\n", the_current_maintainer);

#if defined (MAKE_BUG_REPORTS)
  if (1)
    {
      fprintf (stderr, "Mailing a bug report...");
      fflush (stderr);
      make_bug_report (reason, arg1, arg2, arg3, arg4, arg5);
      fprintf (stderr, "done.\n");
    }
#endif /* MAKE_BUG_REPORTS */

  fprintf (stderr, "Stopping myself...");
  fflush (stderr);
  abort ();
}

void
report_error (format, arg1, arg2, arg3, arg4, arg5)
     char *format;
{
#if defined (NOTDEF)
  extern char *shell_name, *base_pathname ();

  fprintf (stderr, "%s: ", base_pathname (shell_name));
#endif /* NOTDEF */

  fprintf (stderr, format, arg1, arg2, arg3, arg4, arg5);
  fprintf (stderr, "\n");
  if (exit_immediately_on_error)
    exit (1);
}  

void
fatal_error (format, arg1, arg2, arg3, arg4, arg5)
     char *format;
{
  report_error (format, arg1, arg2, arg3, arg4, arg5);
  exit (2);
}

#else /* We have VARARGS support, so use it. */

void
programming_error (va_alist)
     va_dcl
{
  extern char *the_current_maintainer, *shell_name;
  extern char *base_pathname ();
  va_list args;
  char *format;

#if defined (JOB_CONTROL)
  {
    extern pid_t shell_pgrp;
    give_terminal_to (shell_pgrp);
  }
#endif /* JOB_CONTROL */

  va_start (args);
  format = va_arg (args, char *);
  vfprintf (stderr, format, args);
  fprintf (stderr, "\n");
  va_end (args);

  fprintf (stderr, "Tell %s to fix this someday.\n", the_current_maintainer);

#if defined (MAKE_BUG_REPORTS)
  if (1)
    {
      fprintf (stderr, "Mailing a bug report...");
      fflush (stderr);
      make_bug_report (va_alist);
      fprintf (stderr, "done.\n");
    }
#endif
  fprintf (stderr, "Stopping myself...");
  fflush (stderr);
  abort ();
}

void
report_error (va_alist)
     va_dcl
{
  va_list args;
  char *format;

#if defined (NOTDEF)
  extern char *shell_name, *base_pathname ();

  fprintf (stderr, "%s: ", base_pathname (shell_name));
#endif /* NOTDEF */
  va_start (args);
  format = va_arg (args, char *);
  vfprintf (stderr, format, args);
  fprintf (stderr, "\n");

  va_end (args);
  if (exit_immediately_on_error)
    exit (1);
}

void
fatal_error (va_alist)
     va_dcl
{
  va_list args;
  char *format;
  extern char *shell_name, *base_pathname ();

  fprintf (stderr, "%s: ", base_pathname (shell_name));
  va_start (args);
  format = va_arg (args, char *);
  vfprintf (stderr, format, args);
  fprintf (stderr, "\n");

  va_end (args);
  exit (2);
}
#endif /* HAVE_VFPRINTF */
