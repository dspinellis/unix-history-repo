/* printf - format and print data
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

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

/* Usage: printf format [argument...]

   A front end to the printf function that lets it be used from the shell.

   Backslash escapes:

   \" = double quote
   \\ = backslash
   \a = alert (bell)
   \b = backspace
   \c = produce no further output
   \f = form feed
   \n = new line
   \r = carriage return
   \t = horizontal tab
   \v = vertical tab
   \0ooo = octal number (ooo is 0 to 3 digits)
   \xhhh = hexadecimal number (hhh is 1 to 3 digits)

   Additional directive:

   %b = print an argument string, interpreting backslash escapes

   David MacKenzie <djm@ai.mit.edu> */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include "system.h"

#define isodigit(c) ((c) >= '0' && (c) <= '7')
#define hextobin(c) ((c)>='a'&&(c)<='f' ? (c)-'a'+10 : (c)>='A'&&(c)<='F' ? (c)-'A'+10 : (c)-'0')
#define octtobin(c) ((c) - '0')

char *xmalloc ();
int print_esc ();
void error ();
void print_direc ();
void print_esc_char ();
void print_esc_string ();

/* The name this program was run with. */
char *program_name;

void
main (argc, argv)
     int argc;
     char **argv;
{
  char *format;			/* Pointer into the format argument. */
  char *direc_start;		/* Start of % directive. */
  int direc_length;		/* Length of % directive. */
  int field_width;		/* Arg to first '*', or -1 if none. */
  int precision;		/* Arg to second '*', or -1 if none. */

  program_name = argv[0];

  if (argc == 1)
    {
      fprintf (stderr, "Usage: %s format [argument...]\n", program_name);
      exit (1);
    }

  format = argv[1];
  argc -= 2;
  argv += 2;

  for (; *format; ++format)
    {
      switch (*format)
	{
	case '%':
	  direc_start = format++;
	  direc_length = 1;
	  field_width = precision = -1;
	  if (*format == '%')
	    {
	      putchar ('%');
	      break;
	    }
	  if (*format == 'b')
	    {
	      if (argc < 1)
		error (1, 0, "missing argument");
	      print_esc_string (*argv);
	      ++argv;
	      --argc;
	      break;
	    }
	  if (index ("-+ #", *format))
	    {
	      ++format;
	      ++direc_length;
	    }
	  if (*format == '*')
	    {
	      if (argc < 2)
		error (1, 0, "missing argument");
	      ++format;
	      ++direc_length;
	      field_width = atoi (*argv);
	      if (field_width < 0)
		error (1, 0, "negative field width");
	      ++argv;
	      --argc;
	    }
	  else
	    while (isdigit (*format))
	      {
		++format;
		++direc_length;
	      }
	  if (*format == '.')
	    {
	      ++format;
	      ++direc_length;
	      if (*format == '*')
		{
		  if (argc < 2)
		    error (1, 0, "missing argument");
		  ++format;
		  ++direc_length;
		  precision = atoi (*argv);
		  if (precision < 0)
		    error (1, 0, "negative precision");
		  ++argv;
		  --argc;
		}
	      else
		while (isdigit (*format))
		  {
		    ++format;
		    ++direc_length;
		  }
	    }
	  if (*format == 'l' || *format == 'L' || *format == 'h')
	    {
	      ++format;
	      ++direc_length;
	    }
	  if (!index ("diouxXfeEgGcs", *format))
	    error (1, 0, "%%%c: invalid directive", *format);
	  ++direc_length;
	  if (argc < 1)
	    error (1, 0, "missing argument");
	  print_direc (direc_start, direc_length, field_width, precision, *argv);
	  ++argv;
	  --argc;
	  break;

	case '\\':
	  format += print_esc (format);
	  break;

	default:
	  putchar (*format);
	}
    }
  exit (0);
}

/* Print a \ escape sequence starting at ESCSTART.
   Return the number of characters in the escape sequence. */

int
print_esc (escstart)
     char *escstart;
{
  register char *p = escstart + 1;
  int esc_value = 0;		/* Value of \nnn escape. */
  int esc_length;		/* Length of \nnn escape. */

  /* \0ooo and \xhhh escapes have maximum length of 3 chars. */
  if (*p == 'x')
    {
      for (esc_length = 0, ++p;
	   esc_length < 3 && isxdigit (*p);
	   ++esc_length, ++p)
	esc_value = esc_value * 16 + hextobin (*p);
      if (esc_length == 0)
	error (1, 0, "missing hexadecimal number in escape");
      putchar (esc_value);
    }
  else if (*p == '0')
    {
      for (esc_length = 0, ++p;
	   esc_length < 3 && isodigit (*p);
	   ++esc_length, ++p)
	esc_value = esc_value * 8 + octtobin (*p);
      putchar (esc_value);
    }
  else if (!index ("\"\\abcfnrtv", *p))
    error (1, 0, "\\%c: invalid escape", *p);
  print_esc_char (*p);
  return p - escstart;
}

/* Output a single-character \ escape.  */

void
print_esc_char (c)
     char c;
{
  switch (c)
    {
    case 'a':			/* Alert. */
      putchar (7);
      break;
    case 'b':			/* Backspace. */
      putchar (8);
      break;
    case 'c':			/* Cancel the rest of the output. */
      exit (0);
      break;
    case 'f':			/* Form feed. */
      putchar (12);
      break;
    case 'n':			/* New line. */
      putchar (10);
      break;
    case 'r':			/* Carriage return. */
      putchar (13);
      break;
    case 't':			/* Horizontal tab. */
      putchar (9);
      break;
    case 'v':			/* Vertical tab. */
      putchar (11);
      break;
    default:
      putchar (c);
      break;
    }
}

/* Print string STR, evaluating \ escapes. */

void
print_esc_string (str)
     char *str;
{
  for (; *str; str++)
    if (*str == '\\')
      str += print_esc (str);
    else
      putchar (*str);
}

/* Output a % directive.  START is the start of the directive,
   LENGTH is its length, and ARGUMENT is its argument.
   If FIELD_WIDTH or PRECISION is non-negative, they are args for
   '*' values in those fields. */

void
print_direc (start, length, field_width, precision, argument)
     char *start;
     int length;
     int field_width;
     int precision;
     char *argument;
{
  char *p;		/* Null-terminated copy of % directive. */

  p = xmalloc ((unsigned) (length + 1));
  strncpy (p, start, length);
  p[length] = 0;

  switch (p[length - 1])
    {
    case 'd':
    case 'i':
    case 'o':
    case 'u':
    case 'x':
    case 'X':
      if (field_width < 0)
	{
	  if (precision < 0)
	    printf (p, atol (argument));
	  else
	    printf (p, precision, atol (argument));
	}
      else
	{
	  if (precision < 0)
	    printf (p, field_width, atol (argument));
	  else
	    printf (p, field_width, precision, atol (argument));
	}
      break;
    case 'f':
    case 'e':
    case 'E':
    case 'g':
    case 'G':
      if (field_width < 0)
	{
	  if (precision < 0)
	    printf (p, atof (argument));
	  else
	    printf (p, precision, atof (argument));
	}
      else
	{
	  if (precision < 0)
	    printf (p, field_width, atof (argument));
	  else
	    printf (p, field_width, precision, atof (argument));
	}
      break;
    case 'c':
      printf (p, *argument);
      break;
    case 's':
      if (field_width < 0)
	{
	  if (precision < 0)
	    printf (p, argument);
	  else
	    printf (p, precision, argument);
	}
      else
	{
	  if (precision < 0)
	    printf (p, field_width, argument);
	  else
	    printf (p, field_width, precision, argument);
	}
      break;
    }
  free (p);
}
