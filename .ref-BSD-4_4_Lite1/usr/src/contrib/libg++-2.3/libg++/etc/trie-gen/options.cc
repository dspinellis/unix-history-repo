/* Handles parsing the Options provided to the user.
   Copyright (C) 1989 Free Software Foundation, Inc.
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)

This file is part of GNU TRIE-GEN.

GNU TRIE-GEN is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU TRIE-GEN is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU TRIE-GEN; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <builtin.h>
#include <assert.h>
#include <stdarg.h>
#include <GetOpt.h>
#include "options.h"

/* Global option coordinator for the entire program. */
Options option;       

/* Current program version. */
extern char *version_string;

static void
report_error (const char *format, ...)
{
  va_list args;
  va_start (args, format);
  char c;

  while (c = *format++)
    {
      if (c=='%')
        switch (c = *format++)
          {
          case 'a':
            exit (1);
          case 'n':
            fputs (option.program_name (), stderr);
            break;
          case 'e':
            typedef void (*CallbackT) (void);
            CallbackT callback = va_arg (args, CallbackT);
            (*callback) ();
            break;
          case 's':
            fputs (va_arg (args, const char *), stderr);
            break;
          default:
            fputc (c, stderr);
          }
      else
        fputc (c, stderr);
    }
  va_end (args);
}

/* Prints program usage to standard error stream. */

inline void 
Options::usage (void) 
{ 
  report_error ("Usage: %n [-cdf] (type %n -h for help)\n");
}

/* Output command-line Options. */

void 
Options::print_options (void)
{ 
  int i;

  printf ("/* Command-line: ");

  for (i = 0; i < argument_count; i++) 
    printf ("%s ", argument_vector[i]);
   
  printf (" */");
}
/* Sets the default Options. */

int     Options::option_word = 0; 
int     Options::argument_count = 0;
char ** Options::argument_vector = 0;

Options::Options (void) 
{ 
  option_word = 0;
}

/* Dumps option status when debug is set. */

Options::~Options (void) 
{ 
  if (option_word & DEBUG)
    {
      fprintf (stderr, "\ndumping Options:\nCOMPACT is.......: %s\nDEBUG is.......: %s"
               "\nFULL is........: %s\nCONST is.......: %s",
               option_word & COMPACT ? "enabled" : "disabled",
               option_word & DEBUG ? "enabled" : "disabled",
               option_word & FULL ? "enabled" : "disabled",
               option_word & CONST ? "enabled" : "disabled");

      fprintf (stderr, "\nfinished dumping Options\n");
    }
}


/* Parses the command line Options and sets appropriate flags in option_word. */

void 
Options::operator () (int argc, char *argv[])
{ 
  GetOpt getopt (argc, argv, "cCdfh");
  int    option_char;

  argument_count = argc;
  argument_vector = argv;

  while ((option_char = getopt ()) != EOF)
    {
      switch (option_char)
        {
        case 'c':               /* Compact the output tables. */
          {
            option_word |= COMPACT;
            break;
          }
        case 'C':               /* Make the generated tables readonly (const). */
          {
            option_word |= CONST;
            break;
          }
        case 'd':               /* Enable debugging option. */
          { 
            option_word |= DEBUG;
            report_error ("Starting program %n, version %s, with debuggin on.\n",
                          version_string);
            break;
          }   
        case 'f':               /* Generate a full table */
          {
            option_word |= FULL;
            break;
          }
        case 'h':
          {
            report_error ("-c\tCompact the generated trie.\n"
                          "-C\tMake strings in the generated lookup table constant, i.e., readonly.\n"
                          "-d\tEnable debugging (produces verbose output to standard error).\n"
                          "-f\tGenerates a `full' trie rather than a minimal-prefix trie.\n"
                          "-h\tPrints out the help diagnostic.\n%a");
          }
        default: 
          report_error ("%e%a", usage);
        }
    }
  
  if (argv[getopt.optind] && ! freopen (argv[getopt.optind], "r", stdin))
    report_error ("Unable to read key word file %s.\n%e%a", argv[getopt.optind], usage);
  
  if (++getopt.optind < argc) 
    report_error ("Extra trailing arguments to %n.\n%e%a", usage);
}

#ifndef __OPTIMIZE__
const char *
Options::program_name (void)
{
  return argument_vector[0];
}

/* TRUE if option enable, else FALSE. */
int  
Options::operator[] (Option_Type option) 
{ 
  return option_word & option;
}

/* Enables option OPT. */
void
Options::operator= (enum Option_Type opt) 
{
  option_word |= opt;
}

/* Disables option OPT. */
void
Options::operator!= (enum Option_Type opt) 
{
  option_word &= ~opt;
}

#endif /* not defined __OPTIMIZE__ */


