/* protf.c
   The 'f' protocol.

   Copyright (C) 1991, 1992 Ian Lance Taylor

   This file is part of the Taylor UUCP package.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   The author of the program may be contacted at ian@airs.com or
   c/o AIRS, P.O. Box 520, Waltham, MA 02254.

   $Log: protf.c,v $
   Revision 1.15  1992/03/30  04:49:10  ian
   Niels Baggesen: added debugging types abnormal and uucp-proto

   Revision 1.14  1992/03/17  01:03:03  ian
   Miscellaneous cleanup

   Revision 1.13  1992/03/13  22:59:25  ian
   Have breceive_char go through freceive_data

   Revision 1.12  1992/03/12  19:56:10  ian
   Debugging based on types rather than number

   Revision 1.11  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.10  1992/01/16  18:16:58  ian
   Niels Baggesen: add some debugging messages

   Revision 1.9  1992/01/14  04:35:23  ian
   Chip Salzenberg: implement this patch correctly

   Revision 1.8  1992/01/14  04:21:59  ian
   Chip Salzenberg: avoid use before set warning

   Revision 1.7  1991/12/31  19:34:19  ian
   Added number of bytes to pffile protocol entry point

   Revision 1.6  1991/12/20  03:02:01  ian
   Oleg Tabarovsky: added statistical messages to 'g' and 'f' protocols

   Revision 1.5  1991/12/20  00:01:54  ian
   Franc,ois Pinard: don't crash 'f' protocol because of an illegal byte

   Revision 1.4  1991/11/16  00:31:01  ian
   Increased default 't' and 'f' protocol timeouts

   Revision 1.3  1991/11/15  23:32:15  ian
   Don't use 1 second timeouts--loses data on System V

   Revision 1.2  1991/11/15  21:00:59  ian
   Efficiency hacks for 'f' and 't' protocols

   Revision 1.1  1991/11/11  04:21:16  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char protf_rcsid[] = "$Id: protf.c,v 1.15 1992/03/30 04:49:10 ian Rel $";
#endif

#include <ctype.h>
#include <errno.h>

#include "prot.h"
#include "port.h"
#include "system.h"

/* This implementation is based on code by Piet Beertema, CWI,
   Amsterdam, Sep 1984.

   This code implements the 'f' protocol, which requires a
   flow-controlled error-free seven-bit data path.  It does check for
   errors, but only at the end of each file transmission, so a noisy
   line without error correcting modems will be unusable.

   The conversion to seven bit data is done as follows, where b
   represents the character to convert:

      0 <= b <=  037: 0172, b + 0100 (0100 to 0137)
    040 <= b <= 0171:       b        ( 040 to 0171)
   0172 <= b <= 0177: 0173, b - 0100 ( 072 to 077)
   0200 <= b <= 0237: 0174, b - 0100 (0100 to 0137)
   0240 <= b <= 0371: 0175, b - 0200 ( 040 to 0171)
   0372 <= b <= 0377: 0176, b - 0300 ( 072 to 077)

   This causes all output bytes to be in the range 040 to 0176; these
   are the printable ASCII characters.  */

/* Internal functions.  */
static boolean ffprocess_data P((boolean *pfexit, int *pcneed));

/* The size of the buffer we allocate to store outgoing data in.  */
#define CFBUFSIZE (256)

/* The timeout to wait for data to arrive before giving up.  */
static int cFtimeout = 120;

/* The maximum number of retries.  */
static int cFmaxretries = 2;

/* The buffer we allocate for outgoing data.  */
static char *zFbuf;

/* TRUE if we are receiving a file rather than a command.  */
static boolean fFfile;

/* The checksum so far.  */
static unsigned int iFcheck;

/* The last special byte (0172 to 0176) or 0 if none.  */
static char bFspecial;

/* The number of times we have retried this file.  */
static int cFretries;

struct scmdtab asFproto_params[] =
{
  { "timeout", CMDTABTYPE_INT, (pointer) &cFtimeout, NULL },
  { "retries", CMDTABTYPE_INT, (pointer) &cFmaxretries, NULL },
  { NULL, 0, NULL, NULL }
};

/* Statistics.  */

/* The number of data bytes sent in files.  */
static long cFsent_data;

/* The number of actual bytes sent in files.  */
static long cFsent_bytes;

/* The number of data bytes received in files.  */
static long cFrec_data;

/* The number of actual bytes received in files.  */
static long cFrec_bytes;

/* The number of file retries when sending.  */
static long cFsend_retries;

/* The number of file retries when receiving.  */
static long cFrec_retries;

/* Start the protocol.  */

/*ARGSUSED*/
boolean
ffstart (fmaster)
     boolean fmaster;
{
  cFsent_data = 0;
  cFsent_bytes = 0;
  cFrec_data = 0;
  cFrec_bytes = 0;
  cFsend_retries = 0;
  cFrec_retries = 0;

  /* Allow XON/XOFF to work.  */
  if (! fport_set (PORTSETTING_SEVEN))
    return FALSE;

  /* We sleep to allow the other side to reset the terminal; this is
     what Mr. Beertema's code does.  */
  usysdep_sleep (2);

  return TRUE;
}

/* Shutdown the protocol.  */

boolean
ffshutdown ()
{
  xfree ((pointer) zFbuf);
  zFbuf = NULL;
  ulog (LOG_NORMAL,
	"Protocol 'f': sent %ld bytes for %ld, received %ld bytes for %ld",
	cFsent_bytes, cFsent_data, cFrec_bytes, cFrec_data);
  if (cFsend_retries != 0 || cFrec_retries != 0)
    ulog (LOG_NORMAL, "Protocol 'f' file retries: %ld sending, %ld receiving",
	  cFsend_retries, cFrec_retries);
  return TRUE;
}

/* Send a command string.  We just send the string followed by a carriage
   return.  */

boolean
ffsendcmd (z)
     const char *z;
{
  int clen;
  char *zalc;

  DEBUG_MESSAGE1 (DEBUG_UUCP_PROTO, "ffsendcmd: Sending command \"%s\"", z);

  clen = strlen (z);
  zalc = (char *) alloca (clen + 2);
  sprintf (zalc, "%s\r", z);
  return fsend_data (zalc, clen + 1, TRUE);
}

/* Get space to be filled with data.  We allocate the space from the
   heap.  */

char *
zfgetspace (pclen)
     int *pclen;
{
  *pclen = CFBUFSIZE;
  if (zFbuf == NULL)
    zFbuf = (char *) xmalloc (CFBUFSIZE);
  return zFbuf;
}

/* Send out a data packet.  We have to encode the data into seven bits
   and accumulate a checksum.  */

boolean
ffsenddata (zdata, cdata)
     char *zdata;
     int cdata;
{
  char ab[CFBUFSIZE * 2];
  char *ze;
  register unsigned int itmpchk;
      
  cFsent_data += cdata;

  ze = ab;
  itmpchk = iFcheck;
  while (cdata-- > 0)
    {
      register int b;

      /* Rotate the checksum left.  */
      if ((itmpchk & 0x8000) == 0)
	itmpchk <<= 1;
      else
	{
	  itmpchk <<= 1;
	  ++itmpchk;
	}

      /* Add the next byte into the checksum.  */
      b = *zdata++ & 0xff;
      itmpchk += b;

      /* Encode the byte.  */
      if (b <= 0177)
	{
	  if (b <= 037)
	    {
	      *ze++ = '\172';
	      *ze++ = (char) (b + 0100);
	    }
	  else if (b <= 0171)
	    *ze++ = (char) b;
	  else
	    {
	      *ze++ = '\173';
	      *ze++ = (char) (b - 0100);
	    }
	}
      else
	{
	  if (b <= 0237)
	    {
	      *ze++ = '\174';
	      *ze++ = (char) (b - 0100);
	    }
	  else if (b <= 0371)
	    {
	      *ze++ = '\175';
	      *ze++ = (char) (b - 0200);
	    }
	  else
	    {
	      *ze++ = '\176';
	      *ze++ = (char) (b - 0300);
	    }
	}
    }

  iFcheck = itmpchk;

  cFsent_bytes += ze - ab;

  /* Passing FALSE tells fsend_data not to bother looking for incoming
     information, since we really don't expect any.  */
  return fsend_data (ab, ze - ab, FALSE);
}

/* Process any data in the receive buffer.  */

boolean
ffprocess (pfexit)
     boolean *pfexit;
{
  return ffprocess_data (pfexit, (int *) NULL);
}

/* Process data and return the amount of data we are looking for in
   *pcneed.  The 'f' protocol doesn't really reveal this, but when
   transferring file we know that we need at least seven characters
   for the checksum.  */

static boolean
ffprocess_data (pfexit, pcneed)
     boolean *pfexit;
     int *pcneed;
{
  int i;
  register unsigned int itmpchk;

  if (pcneed != NULL)
    *pcneed = 1;

  if (! fFfile)
    {
      /* A command continues until a '\r' character, which we turn
	 into '\0' before calling fgot_data.  */

      while (iPrecstart != iPrecend)
	{
	  for (i = iPrecstart; i < CRECBUFLEN && i != iPrecend; i++)
	    {
	      if (abPrecbuf[i] == '\r')
		{
		  int istart;

		  abPrecbuf[i] = '\0';
		  istart = iPrecstart;
		  iPrecstart = (i + 1) % CRECBUFLEN;
		  return fgot_data (abPrecbuf + istart, i - istart + 1,
				    TRUE, FALSE, pfexit);
		}
	    }

	  if (! fgot_data (abPrecbuf + iPrecstart, i - iPrecstart,
			   TRUE, FALSE, pfexit))
	    return FALSE;

	  iPrecstart = i % CRECBUFLEN;
	}

      *pfexit = FALSE;
      return TRUE;
    }

  /* Here the data is destined for a file, and we must decode it.  */

  itmpchk = iFcheck;

  while (iPrecstart != iPrecend)
    {
      char *zstart, *zto, *zfrom;
      int c;

      zto = zfrom = zstart = abPrecbuf + iPrecstart;

      c = iPrecend - iPrecstart;
      if (c < 0)
	c = CRECBUFLEN - iPrecstart;

      while (c-- != 0)
	{
	  int b;

	  b = *zfrom++ & 0xff;
	  if (b < 040 || b > 0176)
	    {
	      ulog (LOG_ERROR, "Illegal byte %d", b);
	      continue;
	    }

	  /* Characters >= 0172 are always special characters.  The
	     only legal pair of consecutive special characters
	     are 0176 0176 which immediately precede the four
	     digit checksum.  */

	  if (b >= 0172)
	    {
	      if (bFspecial != 0)
		{
		  if (bFspecial != 0176 || b != 0176)
		    {
		      ulog (LOG_ERROR, "Illegal bytes %d %d",
			    bFspecial, b);
		      bFspecial = 0;
		      continue;
		    }

		  /* Pass any initial data.  */
		  if (zto != zstart)
		    {
		      /* Don't count the checksum in the received bytes.  */
		      cFrec_bytes += zfrom - zstart - 2;
		      cFrec_data += zto - zstart;
		      if (! fgot_data (zstart, zto - zstart, FALSE,
				       TRUE, pfexit))
			return FALSE;
		    }

		  /* The next characters we want to read are the
		     checksum, so skip the second 0176.  */
		  iPrecstart = (iPrecstart + zfrom - zstart) % CRECBUFLEN;

		  iFcheck = itmpchk;

		  /* Tell fgot_data that we've read the entire file by
		     passing 0 length data.  This will set *pfexit to
		     TRUE and call fffile to verify the checksum.  */
		  return fgot_data ((char *) NULL, 0, FALSE, TRUE, pfexit);
		}

	      /* Here we have encountered a special character that
		 does not follow another special character.  */
	      bFspecial = (char) b;
	    }
	  else
	    {
	      int bnext;

	      /* Here we have encountered a nonspecial character.  */

	      switch (bFspecial)
		{
		default:
		  bnext = b;
		  break;
		case 0172:
		  bnext = b - 0100;
		  break;
		case 0173:
		case 0174:
		  bnext = b + 0100;
		  break;
		case 0175:
		  bnext = b + 0200;
		  break;
		case 0176:
		  bnext = b + 0300;
		  break;
		}

	      *zto++ = (char) bnext;
	      bFspecial = 0;

	      /* Rotate the checksum left.  */
	      if ((itmpchk & 0x8000) == 0)
		itmpchk <<= 1;
	      else
		{
		  itmpchk <<= 1;
		  ++itmpchk;
		}

	      /* Add the next byte into the checksum.  */
	      itmpchk += bnext;
	    }
	}

      if (zto != zstart)
	{
	  DEBUG_MESSAGE1 (DEBUG_PROTO,
			  "ffprocess: Calling fgot_data with %d bytes",
			  zto - zstart);

	  cFrec_data += zto - zstart;
	  if (! fgot_data (zstart, zto - zstart, FALSE, TRUE, pfexit))
	    return FALSE;
	}

      cFrec_bytes += zfrom - zstart;

      iPrecstart = (iPrecstart + zfrom - zstart) % CRECBUFLEN;
    }

  iFcheck = itmpchk;

  if (pcneed != NULL)
    {
      /* At this point we may have seen the first 0176 in the checksum
	 but not the second.  The checksum is at least seven
	 characters long (0176 0176 a b c d \r).  This won't help
	 much, but reading seven characters is a lot better than
	 reading two, which is what I saw in a 2400 baud log file.  */
      if (bFspecial == 0176)
	*pcneed = 6;
      else
	*pcneed = 7;
    }

  *pfexit = FALSE;
  return TRUE;
}

/* Wait for data to come in and process it until we've finished a
   command or a file.  */

boolean
ffwait ()
{
  while (TRUE)
    {
      boolean fexit;
      int cneed, crec;

      if (! ffprocess_data (&fexit, &cneed))
	return FALSE;
      if (fexit)
	return TRUE;

      /* We only ask for one character at a time.  This could wind up
	 being quite inefficient, since we might only get one
	 character back from each read.  We really want to do
	 something like get all available characters, then sleep for
	 half a second and get all available characters again, and
	 keep this up until we don't get anything after sleeping.  */

      if (! freceive_data (cneed, &crec, cFtimeout, TRUE))
	return FALSE;

      if (crec == 0)
	{
	  ulog (LOG_ERROR, "Timed out waiting for data");
	  return FALSE;
	}
    }
}

/* File level operations.  Reset the checksums when starting to send
   or receive a file, and output the checksum when we've finished
   sending a file.  */

/*ARGSUSED*/
boolean
fffile (fstart, fsend, pfredo, cbytes)
     boolean fstart;
     boolean fsend;
     boolean *pfredo;
     long cbytes;
{
  if (fstart)
    {
      iFcheck = 0xffff;
      cFretries = 0;
      if (! fsend)
	{
	  bFspecial = 0;
	  fFfile = TRUE;
	}
      return TRUE;
    }
  else
    {
      const char *z;

      *pfredo = FALSE;

      if (fsend)
	{
	  char ab[8];

	  /* Send the final checksum.  */

	  sprintf (ab, "\176\176%04x\r", iFcheck & 0xffff);
	  if (! fsend_data (ab, 7, TRUE))
	    return FALSE;

	  /* Now look for the acknowledgement.  */
	  z = zgetcmd ();
	  if (z == NULL)
	    return FALSE;

	  /* An R means to retry sending the file.  */
	  if (*z == 'R')
	    {
	      ++cFretries;
	      if (cFretries > cFmaxretries)
		{
		  ulog (LOG_ERROR, "Too many retries");
		  return FALSE;
		}
	      *pfredo = TRUE;
	      iFcheck = 0xffff;
	      ++cFsend_retries;
	      return TRUE;
	    }

	  if (*z == 'G')
	    return TRUE;

	  DEBUG_MESSAGE1 (DEBUG_PROTO, "fffile: Got \"%s\"", z);

	  ulog (LOG_ERROR, "File send failed");
	  return FALSE;
	}
      else
	{
	  unsigned int icheck;

	  /* We next expect to receive a command.  */
	  fFfile = FALSE;

	  /* Get the checksum.  */
	  z = zgetcmd ();
	  if (z == NULL)
	    return FALSE;

	  if (strlen (z) != 4
	      || ! isxdigit (z[0])
	      || ! isxdigit (z[1])
	      || ! isxdigit (z[2])
	      || ! isxdigit (z[3]))
	    {
	      ulog (LOG_ERROR, "Bad checksum format");
	      return FALSE;
	    }
	  
	  icheck = strtol (z, (char **) NULL, 16);

	  if (icheck != (iFcheck & 0xffff))
	    {
	      DEBUG_MESSAGE2 (DEBUG_PROTO | DEBUG_ABNORMAL,
			      "Checksum failed; calculated 0x%x, got 0x%x",
			      iFcheck & 0xffff, icheck);

	      ++cFretries;
	      if (cFretries > cFmaxretries)
		{
		  ulog (LOG_ERROR, "Too many retries");
		  (void) ffsendcmd ("Q");
		  return FALSE;
		}

	      *pfredo = TRUE;
	      iFcheck = 0xffff;
	      bFspecial = 0;
	      fFfile = TRUE;
	      ++cFrec_retries;

	      /* Send an R to tell the other side to resend the file.  */
	      return ffsendcmd ("R");
	    }

	  /* Send a G to tell the other side the file was received
	     correctly.  */
	  return ffsendcmd ("G");
	}
    }
}
