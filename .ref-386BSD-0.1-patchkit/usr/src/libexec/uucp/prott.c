/* prott.c
   The 't' protocol.

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

   $Log: prott.c,v $
   Revision 1.16  1992/03/30  04:49:10  ian
   Niels Baggesen: added debugging types abnormal and uucp-proto

   Revision 1.15  1992/03/17  01:03:03  ian
   Miscellaneous cleanup

   Revision 1.14  1992/03/13  22:59:25  ian
   Have breceive_char go through freceive_data

   Revision 1.13  1992/03/12  19:56:10  ian
   Debugging based on types rather than number

   Revision 1.12  1992/02/08  19:55:32  ian
   Shifts of integers by more than 15 are not portable

   Revision 1.11  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.10  1992/01/16  18:16:58  ian
   Niels Baggesen: add some debugging messages

   Revision 1.9  1991/12/31  19:34:19  ian
   Added number of bytes to pffile protocol entry point

   Revision 1.8  1991/12/28  03:49:23  ian
   Added HAVE_MEMFNS and HAVE_BFNS; changed uses of memset to bzero

   Revision 1.7  1991/12/17  22:13:14  ian
   David Nugent: zero out garbage before sending data

   Revision 1.6  1991/11/16  00:31:01  ian
   Increased default 't' and 'f' protocol timeouts

   Revision 1.5  1991/11/15  23:27:21  ian
   Include system.h in prott.c and protf.c

   Revision 1.4  1991/11/15  23:20:59  ian
   Changed sleep to usysdep_sleep

   Revision 1.3  1991/11/15  21:00:59  ian
   Efficiency hacks for 'f' and 't' protocols

   Revision 1.2  1991/11/13  20:44:20  ian
   Learned author

   Revision 1.1  1991/11/12  18:26:03  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char prott_rcsid[] = "$Id: prott.c,v 1.16 1992/03/30 04:49:10 ian Rel $";
#endif


#include "prot.h"
#include "port.h"
#include "system.h"

/* This implementation is based on code written by Rick Adams.

   This code implements the 't' protocol, which does no error checking
   whatsoever and thus requires an end-to-end verified eight bit
   communication line, such as is provided by TCP.  Using it with a
   modem is unadvisable, since errors can occur between the modem and
   the computer.  */

/* The buffer size we use.  */
#define CTBUFSIZE (1024)

/* The offset in the buffer to the data.  */
#define CTFRAMELEN (4)

/* Commands are sent in multiples of this size.  */
#define CTPACKSIZE (512)

/* A pointer to the buffer we will use.  */
static char *zTbuf;

/* True if we are receiving a file.  */
static boolean fTfile;

/* The timeout we use.  */
static int cTtimeout = 120;

struct scmdtab asTproto_params[] =
{
  { "timeout", CMDTABTYPE_INT, (pointer) &cTtimeout, NULL },
  { NULL, 0, NULL, NULL }
};

/* Local function.  */

static boolean ftprocess_data P((boolean *pfexit, int *pcneed));

/* Start the protocol.  */

/*ARGSUSED*/
boolean
ftstart (fmaster)
     boolean fmaster;
{
  if (! fport_set (PORTSETTING_EIGHT))
    return FALSE;
  zTbuf = (char *) xmalloc (CTBUFSIZE + CTFRAMELEN);
  /* The first two bytes of the buffer are always zero.  */
  zTbuf[0] = 0;
  zTbuf[1] = 0;
  fTfile = FALSE;
  usysdep_sleep (2);
  return TRUE;
}

/* Stop the protocol.  */

boolean 
ftshutdown ()
{
  xfree ((pointer) zTbuf);
  zTbuf = NULL;
  return TRUE;
}

/* Send a command string.  We send everything up to and including the
   null byte.  The number of bytes we send must be a multiple of
   TPACKSIZE.  */

boolean
ftsendcmd (z)
     const char *z;
{
  int clen;
  char *zalc;

  DEBUG_MESSAGE1 (DEBUG_UUCP_PROTO, "ftsendcmd: Sending command \"%s\"", z);

  clen = strlen (z);

  /* We need to send the smallest multiple of CTPACKSIZE which is
     greater than clen (not equal to clen, since we need room for the
     null byte).  */
  clen = ((clen / CTPACKSIZE) + 1) * CTPACKSIZE;

  zalc = (char *) alloca (clen);
  bzero (zalc, clen);
  strcpy (zalc, z);

  return fsend_data (zalc, clen, TRUE);
}

/* Get space to be filled with data.  We provide a buffer which has
   four bytes at the start available to hold the length.  */

char *
ztgetspace (pclen)
     int *pclen;
{
  *pclen = CTBUFSIZE;
  return zTbuf + CTFRAMELEN;
}

/* Send out some data.  We are allowed to modify the four bytes
   preceding the buffer.  This allows us to send the entire block with
   header bytes in a single call.  */

boolean
ftsenddata (zdata, cdata)
     char *zdata;
     int cdata;
{
  /* Here we do htonl by hand, since it doesn't exist everywhere.  We
     know that the amount of data cannot be greater than CTBUFSIZE, so
     the first two bytes of this value will always be 0.  They were
     set to 0 in ftstart so we don't touch them here.  This is useful
     because we cannot portably right shift by 24 or 16, since we
     might be dealing with sixteen bit integers.  */
  zdata[-2] = (char) ((cdata >> 8) & 0xff);
  zdata[-1] = (char) (cdata & 0xff);

  /* We pass FALSE to fsend_data since we don't expect the other side
     to be sending us anything just now.  */
  return fsend_data (zdata - CTFRAMELEN, cdata + CTFRAMELEN, FALSE);
}

/* Process any data in the receive buffer.  */

boolean
ftprocess (pfexit)
     boolean *pfexit;
{
  return ftprocess_data (pfexit, (int *) NULL);
}

/* Process data and return the amount we need in *pfneed.  */

static boolean
ftprocess_data (pfexit, pcneed)
     boolean *pfexit;
     int *pcneed;
{
  int cinbuf, cfirst, clen;

  *pfexit = FALSE;

  cinbuf = iPrecend - iPrecstart;
  if (cinbuf < 0)
    cinbuf += CRECBUFLEN;

  if (! fTfile)
    {
      /* We are not receiving a file.  Commands are read in chunks of
	 CTPACKSIZE.  */
      while (cinbuf >= CTPACKSIZE)
	{
	  cfirst = CRECBUFLEN - iPrecstart;
	  if (cfirst > CTPACKSIZE)
	    cfirst = CTPACKSIZE;

	  DEBUG_MESSAGE1 (DEBUG_PROTO,
			  "ftprocess_data: Got %d command bytes",
			  cfirst);

	  if (! fgot_data (abPrecbuf + iPrecstart, cfirst, TRUE, FALSE,
			   pfexit))
	    return FALSE;
	  if (cfirst < CTPACKSIZE && ! *pfexit)
	    {
	      if (! fgot_data (abPrecbuf, CTPACKSIZE - cfirst, TRUE, FALSE,
			       pfexit))
		return FALSE;
	    }

	  iPrecstart = (iPrecstart + CTPACKSIZE) % CRECBUFLEN;

	  if (*pfexit)
	    return TRUE;

	  cinbuf -= CTPACKSIZE;
	}

      if (pcneed != NULL)
	*pcneed = CTPACKSIZE - cinbuf;

      return TRUE;
    }

  /* Here we are receiving a file.  The data comes in blocks.  The
     first four bytes contain the length, followed by that amount of
     data.  */

  while (cinbuf >= CTFRAMELEN)
    {
      /* The length is stored in network byte order, MSB first.  */

      clen = (((((((abPrecbuf[iPrecstart] & 0xff) << 8)
		  + (abPrecbuf[(iPrecstart + 1) % CRECBUFLEN] & 0xff)) << 8)
		+ (abPrecbuf[(iPrecstart + 2) % CRECBUFLEN] & 0xff)) << 8)
	      + (abPrecbuf[(iPrecstart + 3) % CRECBUFLEN] & 0xff));

      if (cinbuf < clen + CTFRAMELEN)
	{
	  if (pcneed != NULL)
	    *pcneed = clen + CTFRAMELEN - cinbuf;
	  return TRUE;
	}

      iPrecstart = (iPrecstart + CTFRAMELEN) % CRECBUFLEN;

      cfirst = CRECBUFLEN - iPrecstart;
      if (cfirst > clen)
	cfirst = clen;

      DEBUG_MESSAGE1 (DEBUG_PROTO,
		      "ftprocess_data: Got %d data bytes",
		      cfirst);

      if (! fgot_data (abPrecbuf + iPrecstart, cfirst, FALSE, TRUE,
		       pfexit))
	return FALSE;
      if (cfirst < clen)
	{
	  if (! fgot_data (abPrecbuf, clen - cfirst, FALSE, TRUE,
			   pfexit))
	    return FALSE;
	}

      iPrecstart = (iPrecstart + clen) % CRECBUFLEN;
			   
      if (*pfexit)
	return TRUE;

      cinbuf -= clen + CTFRAMELEN;
    }

  if (pcneed != NULL)
    *pcneed = CTFRAMELEN - cinbuf;

  return TRUE;
}

/* Wait for data to come in and process it until we've reached the end
   of a command or a file.  */

boolean
ftwait ()
{
  while (TRUE)
    {
      boolean fexit;
      int cneed, crec;

      if (! ftprocess_data (&fexit, &cneed))
	return FALSE;
      if (fexit)
	return TRUE;

      if (! freceive_data (cneed, &crec, cTtimeout, TRUE))
	return FALSE;

      if (crec == 0)
	{
	  ulog (LOG_ERROR, "Timed out waiting for data");
	  return FALSE;
	}
    }
}

/* File level routine, to set fTfile correctly.  */

/*ARGSUSED*/
boolean
ftfile (fstart, fsend, pfredo, cbytes)
     boolean fstart;
     boolean fsend;
     boolean *pfredo;
     long cbytes;
{
  if (pfredo != NULL)
    *pfredo = FALSE;

  if (! fsend)
    fTfile = fstart;

  return TRUE;
}
