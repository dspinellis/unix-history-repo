/* prote.c
   The 'e' protocol.

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

   $Log: prote.c,v $
   Revision 1.7  1992/03/30  04:49:10  ian
   Niels Baggesen: added debugging types abnormal and uucp-proto

   Revision 1.6  1992/03/17  01:03:03  ian
   Miscellaneous cleanup

   Revision 1.5  1992/03/13  22:59:25  ian
   Have breceive_char go through freceive_data

   Revision 1.4  1992/03/12  19:56:10  ian
   Debugging based on types rather than number

   Revision 1.3  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.2  1992/01/16  18:16:58  ian
   Niels Baggesen: add some debugging messages

   Revision 1.1  1991/12/31  19:43:30  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char prote_rcsid[] = "$Id: prote.c,v 1.7 1992/03/30 04:49:10 ian Rel $";
#endif

#include "prot.h"
#include "port.h"
#include "system.h"

/* This implementation is based on my implementation of the 't'
   protocol, which is fairly similar.  The main difference between the
   protocols seems to be that 't' breaks the file into packets and
   transmits the size of the packet with each packet, whereas 'e'
   sends the size of the entire file and then sends all the data in a
   single enormous packet.

   The 'e' protocol does no error checking whatsoever and thus
   requires an end-to-end verified eight bit communication line, such
   as is provided by TCP.  Using it with a modem is inadvisable, since
   errors can occur between the modem and the computer.  */

/* The buffer size we use.  */
#define CEBUFSIZE (CRECBUFLEN / 2)

/* The size of the initial file size message.  */
#define CEFRAMELEN (20)

/* A pointer to the buffer we will use.  */
static char *zEbuf;

/* True if we are receiving a file.  */
static boolean fEfile;

/* The number of bytes we have left to send or receive.  */
static long cEbytes;

/* The timeout we use.  */
static int cEtimeout = 120;

struct scmdtab asEproto_params[] =
{
  { "timeout", CMDTABTYPE_INT, (pointer) &cEtimeout, NULL },
  { NULL, 0, NULL, NULL }
};

/* Local function.  */

static boolean feprocess_data P((boolean *pfexit, int *pcneed));

/* Start the protocol.  */

/*ARGSUSED*/
boolean
festart (fmaster)
     boolean fmaster;
{
  if (! fport_set (PORTSETTING_EIGHT))
    return FALSE;
  zEbuf = (char *) xmalloc (CEBUFSIZE);
  fEfile = FALSE;
  usysdep_sleep (2);
  return TRUE;
}

/* Stop the protocol.  */

boolean 
feshutdown ()
{
  xfree ((pointer) zEbuf);
  zEbuf = NULL;
  return TRUE;
}

/* Send a command string.  We send everything up to and including the
   null byte.   */

boolean
fesendcmd (z)
     const char *z;
{
  DEBUG_MESSAGE1 (DEBUG_UUCP_PROTO, "fesendcmd: Sending command \"%s\"", z);

  return fsend_data (z, strlen (z) + 1, TRUE);
}

/* Get space to be filled with data.  We provide a buffer which has
   20 bytes at the start available to hold the length.  */

char *
zegetspace (pclen)
     int *pclen;
{
  *pclen = CEBUFSIZE;
  return zEbuf;
}

/* Send out some data.  We are allowed to modify the 20 bytes
   preceding the buffer.  This allows us to send the entire block with
   header bytes in a single call.  */

boolean
fesenddata (zdata, cdata)
     char *zdata;
     int cdata;
{
#if DEBUG > 0
  /* Keep track of the number of bytes we send out to make sure it all
     adds up.  */
  cEbytes -= cdata;
  if (cEbytes < 0)
    {
      ulog (LOG_ERROR, "Protocol 'e' internal error");
      return FALSE;
    }
#endif

  /* We pass FALSE to fsend_data since we don't expect the other side
     to be sending us anything just now.  */
  return fsend_data (zdata, cdata, FALSE);
}

/* Process any data in the receive buffer.  */

boolean
feprocess (pfexit)
     boolean *pfexit;
{
  return feprocess_data (pfexit, (int *) NULL);
}

/* Process data and return the amount we need in *pfneed.  */

static boolean
feprocess_data (pfexit, pcneed)
     boolean *pfexit;
     int *pcneed;
{
  int cinbuf, cfirst, clen;

  *pfexit = FALSE;

  cinbuf = iPrecend - iPrecstart;
  if (cinbuf < 0)
    cinbuf += CRECBUFLEN;

  if (! fEfile)
    {
      /* We are not receiving a file.  Commands continue up to a null
	 byte.  */
      while (cinbuf > 0)
	{
	  char *pnull;

	  cfirst = CRECBUFLEN - iPrecstart;
	  if (cfirst > cinbuf)
	    cfirst = cinbuf;

	  pnull = memchr (abPrecbuf + iPrecstart, '\0', cfirst);
	  if (pnull != NULL)
	    cfirst = pnull - (abPrecbuf + iPrecstart) + 1;

	  DEBUG_MESSAGE1 (DEBUG_PROTO,
			  "feprocess_data: Got %d command bytes",
			  cfirst);

	  if (! fgot_data (abPrecbuf + iPrecstart, cfirst, TRUE, FALSE,
			   pfexit))
	    return FALSE;

	  iPrecstart = (iPrecstart + cfirst) % CRECBUFLEN;

	  if (*pfexit)
	    return TRUE;

	  cinbuf = iPrecend - iPrecstart;
	  if (cinbuf < 0)
	    cinbuf += CRECBUFLEN;
	}

      if (pcneed != NULL)
	*pcneed = 1;

      return TRUE;
    }

  /* Here we are receiving a file.  We want cEbytes in total.  If we
     don't have cEbytes yet, we have to get it first.  */

  if (cEbytes == -1)
    {
      char ab[CEFRAMELEN + 1];

      if (cinbuf < CEFRAMELEN)
	{
	  if (pcneed != NULL)
	    *pcneed = CEFRAMELEN - cinbuf;
	  return TRUE;
	}

      cfirst = CRECBUFLEN - iPrecstart;
      if (cfirst >= CEFRAMELEN)
	memcpy (ab, abPrecbuf + iPrecstart, CEFRAMELEN);
      else
	{
	  memcpy (ab, abPrecbuf + iPrecstart, cfirst);
	  memcpy (ab + cfirst, abPrecbuf, CEFRAMELEN - cfirst);
	}

      ab[CEFRAMELEN] = '\0';
      cEbytes = strtol (ab, (char **) NULL, 10);

      iPrecstart = (iPrecstart + CEFRAMELEN) % CRECBUFLEN;

      cinbuf = iPrecend - iPrecstart;
      if (cinbuf < 0)
	cinbuf += CRECBUFLEN;
    }

  /* Here we can read real data for the file.  */

  while (cinbuf > 0)
    {
      clen = cinbuf;
      if ((long) clen > cEbytes)
	clen = (int) cEbytes;

      cfirst = CRECBUFLEN - iPrecstart;
      if (cfirst > clen)
	cfirst = clen;

      DEBUG_MESSAGE1 (DEBUG_PROTO,
		      "feprocess_data: Got %d data bytes",
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
      cEbytes -= clen;

      if (cEbytes == 0)
	{
	  if (! fgot_data (abPrecbuf, 0, FALSE, TRUE, pfexit))
	    return FALSE;
	  if (*pfexit)
	    return TRUE;
	}

      cinbuf -= clen;
    }

  if (pcneed != NULL)
    {
      if (cEbytes > CRECBUFLEN / 2)
	*pcneed = CRECBUFLEN / 2;
      else
	*pcneed = (int) cEbytes;
    }

  return TRUE;
}

/* Wait for data to come in and process it until we've reached the end
   of a command or a file.  */

boolean
fewait ()
{
  while (TRUE)
    {
      boolean fexit;
      int cneed, crec;

      if (! feprocess_data (&fexit, &cneed))
	return FALSE;
      if (fexit)
	return TRUE;

      if (! freceive_data (cneed, &crec, cEtimeout, TRUE))
	return FALSE;

      if (crec == 0)
	{
	  ulog (LOG_ERROR, "Timed out waiting for data");
	  return FALSE;
	}
    }
}

/* File level routine, to handle transferring the amount of data and
   to set fEfile correctly.  */

boolean
fefile (fstart, fsend, pfredo, cbytes)
     boolean fstart;
     boolean fsend;
     boolean *pfredo;
     long cbytes;
{
  if (pfredo != NULL)
    *pfredo = FALSE;

  if (fstart)
    {
      if (fsend)
	{
	  char ab[CEFRAMELEN];

	  DEBUG_MESSAGE1 (DEBUG_PROTO,
			  "Protocol 'e' starting to send %ld bytes",
			  cbytes);

	  bzero (ab, CEFRAMELEN);
	  sprintf (ab, "%ld", cbytes);
	  if (! fsend_data (ab, CEFRAMELEN, TRUE))
	    return FALSE;
	  cEbytes = cbytes;
	}
      else
	{
	  cEbytes = -1;
	  fEfile = TRUE;
	}
    }
  else
    {
      if (! fsend)
	fEfile = FALSE;
#if DEBUG > 0
      if (cEbytes != 0)
	{
	  ulog (LOG_ERROR,
		"Protocol 'e' internal error: %ld bytes left over",
		cEbytes);
	  return FALSE;
	}
#endif
    }

  return TRUE;
}
