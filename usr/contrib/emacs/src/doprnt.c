/* Output like sprintf to a buffer of specified size.
   Also takes args differently: pass one pointer to an array of strings
   in addition to the format string which is separate.
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


#include <stdio.h>
#include <ctype.h>

doprnt (buffer, bufsize, format, args)
     char *buffer;
     register int bufsize;
     char *format;
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
	      sprintf (tembuf, fmtcpy, args[cnt++]);
	      /* Now copy tembuf into final output, truncating as nec.  */
	      string = tembuf;
	      goto doit;

	    case 's':
	      string = args[cnt++];
	      if (fmtcpy[1] != 's')
		minlen = atoi (&fmtcpy[1]);
	      /* Copy string into final output, truncating if no room.  */
	    doit:
	      tem = strlen (string);
	      minlen -= tem;
	      while (minlen > 0 && bufsize > 0)
		{
		  *bufptr++ = ' ';
		  bufsize--;
		  minlen--;
		}
	      if (tem > bufsize)
		tem = bufsize;
	      strncpy (bufptr, string, tem);
	      bufptr += tem;
	      bufsize -= tem;
	      continue;

	    case 'c':
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
