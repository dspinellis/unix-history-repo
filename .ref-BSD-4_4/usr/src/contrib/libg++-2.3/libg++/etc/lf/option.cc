/* Process directory listing program options. */
#include <stdio.h>
#include <std.h>
#include <GetOpt.h>
#include "option.h"

/* Initialize the program options. */

unsigned  Option_Handler::option_word;
char    * Option_Handler::program_name;

Option_Handler::Option_Handler (void)
{     
  option_word = 0;
}

/* Prints program usage to standard error stream, then exits. */

void 
Option_Handler::usage (void)
{ 
  fprintf (stderr, "usage: %s [-ahl] [directory]\n", program_name);
  exit (1);
}

/* Sets the program options. */

void 
Option_Handler::operator () (int argc, char *argv[])
{
  GetOpt getopt (argc, argv, "ahl");
  int option_char;

  program_name = argv[0];

  while ((option_char = getopt ()) != EOF)
    switch (option_char)
      {
      case 'a':                 /* Print out hidden files (those starting with '.'). */
        option_word |= HIDDEN;
        break;
      case 'l':
        option_word |= LINK;
        break;
      case 'h': /* Print help message and exit. */
      default:
        usage ();
      }

  /* Change the working directory if default is not ".". This saves
     time during the directory entry decoding phase. */

  if (argv[getopt.optind])
    chdir (argv[getopt.optind]);
}

#ifndef __OPTIMIZE__
/* TRUE if OPTION enable, else FALSE. */

int
Option_Handler::operator[] (option_type option) 
{ 
  return option_word & option;
}
#endif // __OPTIMIZE__
