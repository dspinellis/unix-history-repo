/* Provides a useful variable-length argument error handling abstraction.

   Copyright (C) 1989 Free Software Foundation, Inc.
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)

This file is part of GNU GPERF.

GNU GPERF is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU GPERF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GPERF; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <std.h>
#include <stdarg.h>
#include "std-err.h"
#include "trace.h"

char * Std_Err::program_name;

/* Sets name of program. */

void 
Std_Err::set_program_name (char *prog_name) 
{ 
  T (Trace t ("Std_Err::set_program_name");)
  program_name = prog_name;
}

/* Valid Options (prefixed by '%', as in printf format strings) include:
   'a': exit the program at this point (var-argument is the exit status!)
   'c': print a character
   'd': print a decimal number
   'e': call the function pointed to by the corresponding argument
   'f','g': print a double
   'n': print the name of the program (NULL if not set in constructor or elsewhere)
   'p': print out the appropriate errno value from sys_errlist
   's': print out a character string
   '%': print out a single percent sign, '%' */

void 
Std_Err::report_error (char *format, ...) 
{ 
  T (Trace t ("Std_Err::report_error");)
  extern int errno, sys_nerr;
  extern char *sys_errlist[];
  typedef void (*PTF)();
  va_list argp;

  va_start (argp, format);
  for ( ; *format; format++) 
    {
      if (*format != '%') 
        putc (*format, stderr);
      else 
        {
          switch (*++format) 
            {
            case '%' : putc ('%', stderr); break;
            case 'a' : exit (va_arg (argp, int));
            case 'c' : putc (va_arg (argp, int), stderr); break;
            case 'd' : fprintf (stderr, "%d", va_arg (argp, int)); break;
            case 'e' : (*va_arg (argp, PTF))(); break;
            case 'f' : fprintf (stderr, "%g", va_arg (argp, double)); break;
            case 'n' : fputs (program_name ? program_name : "error", stderr); break;
            case 'p' : 
              if (errno >= 0 && errno < sys_nerr) 
                fprintf (stderr, "%s: %s", va_arg (argp, char *), sys_errlist[errno]);
              else 
                fprintf (stderr, "<unknown error> %d", errno);
              break;
            case 's' : fputs (va_arg (argp, char *), stderr); break;
            }
        }
    }
  va_end (argp);
}
