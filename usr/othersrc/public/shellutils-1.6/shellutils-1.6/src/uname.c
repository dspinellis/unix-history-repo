/* uname -- print system information
   Copyright (C) 1989, 1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Option		Example

   -s, --sysname	SunOS
   -n, --nodename	rocky8
   -r, --release	4.0
   -v, --version	
   -m, --machine	sun
   -a, --all		SunOS rocky8 4.0  sun

   The default behavior is equivalent to `-s'.

   David MacKenzie <djm@ai.mit.edu> */

#include <stdio.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <getopt.h>
#include "system.h"

void error ();
void print_element ();
void usage ();

/* Values that are bitwise or'd into `toprint'. */
/* Operating system name. */
#define PRINT_SYSNAME 1

/* Node name on a communications network. */
#define PRINT_NODENAME 2

/* Operating system release. */
#define PRINT_RELEASE 4

/* Operating system version. */
#define PRINT_VERSION 8

/* Machine hardware name. */
#define PRINT_MACHINE 16

/* Mask indicating which elements of the name to print. */
unsigned char toprint;

/* The name this program was run with, for error messages. */
char *program_name;

struct option long_options[] =
{
  {"sysname", 0, NULL, 's'},
  {"nodename", 0, NULL, 'n'},
  {"release", 0, NULL, 'r'},
  {"version", 0, NULL, 'v'},
  {"machine", 0, NULL, 'm'},
  {"all", 0, NULL, 'a'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  struct utsname name;
  int c;

  program_name = argv[0];
  toprint = 0;

  while ((c = getopt_long (argc, argv, "snrvma", long_options, (int *) 0))
	 != EOF)
    {
      switch (c)
	{
	case 's':
	  toprint |= PRINT_SYSNAME;
	  break;
	case 'n':
	  toprint |= PRINT_NODENAME;
	  break;
	case 'r':
	  toprint |= PRINT_RELEASE;
	  break;
	case 'v':
	  toprint |= PRINT_VERSION;
	  break;
	case 'm':
	  toprint |= PRINT_MACHINE;
	  break;
	case 'a':
	  toprint = PRINT_SYSNAME | PRINT_NODENAME | PRINT_RELEASE |
	    PRINT_VERSION | PRINT_MACHINE;
	  break;
	default:
	  usage ();
	}
    }

  if (optind != argc)
    usage ();

  if (toprint == 0)
    toprint = PRINT_SYSNAME;

  if (uname (&name) == -1)
    error (1, errno, "cannot get system name");

  print_element (PRINT_SYSNAME, name.sysname);
  print_element (PRINT_NODENAME, name.nodename);
  print_element (PRINT_RELEASE, name.release);
  print_element (PRINT_VERSION, name.version);
  print_element (PRINT_MACHINE, name.machine);

  exit (0);
}

/* If the name element set in MASK is selected for printing in `toprint',
   print ELEMENT; then print a space unless it is the last element to
   be printed, in which case print a newline. */

void
print_element (mask, element)
     unsigned char mask;
     char *element;
{
  if (toprint & mask)
    {
      toprint &= ~mask;
      printf ("%s%c", element, toprint ? ' ' : '\n');
    }
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-snrvma] [--sysname] [--nodename] [--release] [--version]\n\
       [--machine] [--all]\n", program_name);
  exit (1);
}
