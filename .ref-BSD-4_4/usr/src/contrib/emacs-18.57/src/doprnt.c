/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
   Copyright (C) 1985 Free Software Foundation, Inc.

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


#include <stdio.h>
#include <ctype.h>

doprnt (buffer, bufsize, format, nargs, args)
     char *buffer;
     register int bufsize;
     char *format;
     int nargs;
     char **args;
{
  int cnt = 0;			/* Number of arg to gobble next */
  register char *fmt = format;	/* Pointer into format string */
  register char *bufptr = buffer; /* Pointer into output buffer.. */
  char tembuf[80];
  register int tem;
  char *string;
  char fmtcpy[20];
  int minlen;

  bufsize--;
  while (*fmt && bufsize > 0)	/* Loop until end of format string or buffer full */
    {
      if (*fmt == '%')	/* Check for a '%' character */
	{
	  fmt++;
	  /* Copy this one %-spec into fmtcopy.  */
	  string = fmtcpy;
	  *string++ = '%';
	  while (1)
	    {
	      *string++ = *fmt;
	      if (! (*fmt >= '0' && *fmt <= '9') && *fmt != '-' && *fmt != ' ')
		break;
	      fmt++;
	    }
	  *string = 0;
	  minlen = 0;
	  switch (*fmt++)
	    {
	    default:
	      error ("Invalid format operation %%%c", fmt[-1]);

	    case 'b':
	    case 'd':
	    case 'o':
	    case 'x':
	      if (cnt == nargs)
		error ("Format string wants too many arguments");
	      sprintf (tembuf, fmtcpy, args[cnt++]);
	      /* Now copy tembuf into final output, truncating as nec.  */
	      string = tembuf;
	      goto doit;

	    case 's':
	      if (cnt == nargs)
		error ("Format string wants too many arguments");
	      string = args[cnt++];
	      if (fmtcpy[1] != 's')
		minlen = atoi (&fmtcpy[1]);
	      /* Copy string into final output, truncating if no room.  */
	    doit:
	      tem = strlen (string);
	      if (minlen > 0)
		{
		  while (minlen > tem && bufsize > 0)
		    {
		      *bufptr++ = ' ';
		      bufsize--;
		      minlen--;
		    }
		  minlen = 0;
		}
	      if (tem > bufsize)
		tem = bufsize;
	      strncpy (bufptr, string, tem);
	      bufptr += tem;
	      bufsize -= tem;
	      if (minlen < 0)
		{
		  while (minlen < - tem && bufsize > 0)
		    {
		      *bufptr++ = ' ';
		      bufsize--;
		      minlen++;
		    }
		  minlen = 0;
		}
	      continue;

	    case 'c':
	      if (cnt == nargs)
		error ("Format string wants too many arguments");
	      *bufptr++ = (int) args[cnt++];
	      bufsize--;
	      continue;

	    case '%':
	      fmt--;    /* Drop thru and this % will be treated as normal */
	    }
	}
      *bufptr++ = *fmt++;	/* Just some characters; Copy 'em */
      bufsize--;
    };

  *bufptr = 0;		/* Make sure our string end with a '\0' */
  return bufptr - buffer;
}
