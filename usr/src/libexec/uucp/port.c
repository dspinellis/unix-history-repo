/* port.c
   Port manipulation routines for the Taylor UUCP package.

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

   $Log: port.c,v $
   Revision 1.17  1992/04/01  22:31:05  ian
   Initialize stdin port

   Revision 1.16  1992/03/28  19:57:22  ian
   David J. MacKenzie: send port device for /Y rather than port name

   Revision 1.15  1992/03/17  01:03:03  ian
   Miscellaneous cleanup

   Revision 1.14  1992/03/15  06:58:12  ian
   Show fport_read and fport_write calls under DEBUG_PORT

   Revision 1.13  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.12  1992/03/04  00:36:44  ian
   Michael Richardson: better chat script debugging

   Revision 1.11  1992/02/08  19:34:07  ian
   Cast result of alloca

   Revision 1.10  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.9  1991/12/28  04:20:36  ian
   Cleaned up debugging code

   Revision 1.8  1991/12/20  02:18:35  ian
   Complete diagnostics for fport_io

   Revision 1.7  1991/12/15  03:42:33  ian
   Added tprocess_chat_cmd for all chat commands, and added CMDTABTYPE_PREFIX

   Revision 1.6  1991/12/10  19:45:05  ian
   Added ulog_device to record device name for log file

   Revision 1.5  1991/11/14  03:20:13  ian
   Added seven-bit and reliable commands to help when selecting protocols

   Revision 1.4  1991/11/13  20:38:00  ian
   Added TCP port type for connections over TCP

   Revision 1.3  1991/11/11  23:47:24  ian
   Added chat-program to run a program to do a chat script

   Revision 1.2  1991/11/11  00:39:45  ian
   Open port in seven bit mode, added fport_set to change to eight bit

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char port_rcsid[] = "$Id: port.c,v 1.17 1992/04/01 22:31:05 ian Rel $";
#endif

#include <ctype.h>

#include "port.h"

#if DEBUG > 1

/* A debugging routine used when displaying buffers.  */

int
cdebug_char (z, ichar)
     char *z;
     int ichar;
{
  char b;

  if (isprint (BUCHAR (ichar))
      && ichar != '\"'
      && ichar != '\\')
    {
      *z++ = (char) ichar;
      *z = '\0';
      return 1;
    }

  *z++ = '\\';

  switch (ichar)
    {
    case '\n':
      b = 'n';
      break;
    case '\r':
      b = 'r';
      break;
    case '\"':
      b = '\"';
      break;
    case '\\':
      b = '\\';
      break;
    default:
      sprintf (z, "%03o", (unsigned int) BUCHAR (ichar));
      return strlen (z) + 1;
    }

  *z++ = b;
  *z = '\0';
  return 2;
}      

/* Display a buffer when debugging.  */

void
udebug_buffer (zhdr, zbuf, clen)
     const char *zhdr;
     const char *zbuf;
     int clen;
{
  char *z, *zalc;
  int i;

  z = zalc = (char *) xmalloc (clen * 4 + 1);
  for (i = 0; i < clen && i < 80; i++)
    z += cdebug_char (z, zbuf[i]);
  if (i < clen)
    {
      *z++ = '.';
      *z++ = '.';
      *z++ = '.';
    }
  *z = '\0';
  
  ulog (LOG_DEBUG, "%s %d \"%s\"", zhdr, clen, zalc);
  xfree ((pointer) zalc);
}

#endif /* DEBUG > 1 */

/* The open port.  */

struct sport *qPort;

/* The port dispatch table.  The order of entries in this table must
   be the same as the tporttype enumeration.  */

const struct sportcmds asPortcmds[] =
{
  { fsysdep_stdin_lock, fsysdep_stdin_open, fsysdep_stdin_close,
      fsysdep_stdin_reset, NULL, fsysdep_stdin_read,
      fsysdep_stdin_write, fsysdep_stdin_io, fsysdep_stdin_break,
      fsysdep_stdin_set, fsysdep_stdin_chat, isysdep_stdin_baud },
  { fsysdep_modem_lock, fsysdep_modem_open, fsysdep_modem_close,
      fsysdep_modem_reset, fmodem_dial, fsysdep_modem_read,
      fsysdep_modem_write, fsysdep_modem_io, fsysdep_modem_break,
      fsysdep_modem_set, fsysdep_modem_chat, isysdep_modem_baud },
  { fsysdep_direct_lock, fsysdep_direct_open, fsysdep_direct_close,
      fsysdep_direct_reset, NULL, fsysdep_direct_read,
      fsysdep_direct_write, fsysdep_direct_io, fsysdep_direct_break,
      fsysdep_direct_set, fsysdep_direct_chat, isysdep_direct_baud },
#if HAVE_TCP
  { ftcp_lock, ftcp_open, ftcp_close,
      ftcp_reset, ftcp_dial, fsysdep_tcp_read,
      fsysdep_tcp_write, fsysdep_tcp_io, NULL,
      NULL, fsysdep_tcp_chat, itcp_baud },
#endif /* HAVE_TCP */
};

/* Port dispatch routines.  These could probably become macros,
   although the port argument would have to be evaluated twice.  */

/* Lock a port.  */

boolean
fport_lock (qport, fin)
     struct sport *qport;
     boolean fin;
{
  return (*asPortcmds[(int) qport->ttype].pflock) (qport, fin);
}

/* The port structure we use when opening standard input as a port.  */

static struct sport sPstdin =
{
  "stdin",
  PORTTYPE_STDIN,
  NULL,
  0,
  NULL,
  0,
  NULL
};

/* Open a port.  */

boolean
fport_open (qport, ibaud, ihighbaud, fwait)
     struct sport *qport;
     long ibaud;
     long ihighbaud;
     boolean fwait;
{
  boolean fret;

#if DEBUG > 1
  if (FDEBUGGING (DEBUG_PORT))
    {
      char abspeed[20];

      if (ibaud == (long) 0)
	strcpy (abspeed, "default speed");
      else
	sprintf (abspeed, "speed %ld", ibaud);

      if (qport == NULL)
	ulog (LOG_DEBUG, "fport_open: Opening stdin port (%s)",
	      abspeed);
      else if (qport->zname == NULL)
	ulog (LOG_DEBUG, "fport_open: Opening unnamed port (%s)",
	      abspeed);
      else
	ulog (LOG_DEBUG, "fport_open: Opening port %s (%s)",
	      qport->zname, abspeed);
    }
#endif

  if (qport == NULL)
    {
      qport = &sPstdin;
      SYSDEP_STDIN_INIT (&qport->u.sstdin.s);
    }
  qPort = NULL;

  /* If the system provides a range of baud rates, we select the
     highest baud rate supported by the port.  */
  if (ihighbaud != 0)
    {
      ibaud = ihighbaud;
      if (qport->ttype == PORTTYPE_MODEM)
	{
	  if (qport->u.smodem.ihighbaud != 0)
	    {
	      if (qport->u.smodem.ihighbaud < ihighbaud)
		ibaud = qport->u.smodem.ihighbaud;
	    }
	  else if (qport->u.smodem.ibaud != 0)
	    ibaud = qport->u.smodem.ibaud;
	}
      else if (qport->ttype == PORTTYPE_DIRECT)
	{
	  if (qport->u.sdirect.ibaud != 0)
	    ibaud = qport->u.sdirect.ibaud;
	}
    }

  /* This will normally be overridden by the port specific open
     routine.  */
  ulog_device (qport->zname);

  fret = (*asPortcmds[(int) qport->ttype].pfopen) (qport, ibaud, fwait);
  if (fret)
    qPort = qport;
  else
    ulog_device ((const char *) NULL);
  return fret;
}

/* Close and unlock the port.  */

boolean
fport_close (fsuccess)
     boolean fsuccess;
{
  boolean fret;

  if (qPort == NULL)
    return TRUE;

  DEBUG_MESSAGE0 (DEBUG_PORT, "fport_close: Closing port");

  fret = (*asPortcmds[(int) qPort->ttype].pfclose) (qPort, fsuccess);
  qPort = NULL;
  ulog_device ((const char *) NULL);
  return fret;
}

/* Reset the port.  */

boolean
fport_reset ()
{
  DEBUG_MESSAGE0 (DEBUG_PORT, "fport_reset: Resetting port");

  return (*asPortcmds[(int) qPort->ttype].pfreset) (qPort);
}

/* Dial out on the port.  */

boolean
fport_dial (qsys, pcproto_params, pqproto_params, pireliable)
     const struct ssysteminfo *qsys;
     int *pcproto_params;
     struct sproto_param **pqproto_params;
     int *pireliable;
{
  boolean (*pfdial) P((struct sport *, const struct ssysteminfo *,
		       int *, struct sproto_param **, int *));

  pfdial = asPortcmds[(int) qPort->ttype].pfdial;
  if (pfdial == NULL)
    {
      *pcproto_params = 0;
      *pqproto_params = NULL;
      *pireliable = 0;
      return TRUE;
    }
  return (*pfdial) (qPort, qsys, pcproto_params, pqproto_params,
		    pireliable);
}

/* Read data from the port.  */

boolean
fport_read (zbuf, pclen, cmin, ctimeout, freport)
     char *zbuf;
     int *pclen;
     int cmin;
     int ctimeout;
     boolean freport;
{
  boolean fret;

  fret = (*asPortcmds[(int) qPort->ttype].pfread) (qPort, zbuf, pclen,
						   cmin, ctimeout,
						   freport);
#if DEBUG > 1
  if (FDEBUGGING (DEBUG_INCOMING))
    udebug_buffer ("fport_read: Read", zbuf, *pclen);
  else if (FDEBUGGING (DEBUG_PORT))
    ulog (LOG_DEBUG, "fport_read: Read %d", *pclen);
#endif

  return fret;
}

/* Write data to the port.  */

boolean
fport_write (zbuf, clen)
     const char *zbuf;
     int clen;
{
#if DEBUG > 1
  if (FDEBUGGING (DEBUG_OUTGOING))
    udebug_buffer ("fport_write: Writing", zbuf, clen);
  else if (FDEBUGGING (DEBUG_PORT))
    ulog (LOG_DEBUG, "fport_write: Writing %d", clen);
#endif

  return (*asPortcmds[(int) qPort->ttype].pfwrite) (qPort, zbuf, clen);
}

/* Read and write data.  */

boolean
fport_io (zwrite, pcwrite, zread, pcread)
     const char *zwrite;
     int *pcwrite;
     char *zread;
     int *pcread;
{
  boolean fret;
#if DEBUG > 1
  int cwrite = *pcwrite;
  int cread = *pcread;

  if (cread <= 0 || cwrite <= 0)
    ulog (LOG_FATAL, "fport_io: cread %d; cwrite %d", cread, cwrite);
#endif

#if DEBUG > 1
  if (FDEBUGGING (DEBUG_OUTGOING))
    udebug_buffer ("fport_io: Writing", zwrite, cwrite);
#endif

  fret = (*asPortcmds[(int) qPort->ttype].pfio) (qPort, zwrite, pcwrite,
						 zread, pcread);

  DEBUG_MESSAGE4 (DEBUG_PORT,
		  "fport_io: Wrote %d of %d, read %d of %d",
		  *pcwrite, cwrite, *pcread, cread);

#if DEBUG > 1
  if (*pcread > 0 && FDEBUGGING (DEBUG_INCOMING))
    udebug_buffer ("fport_io: Read", zread, *pcread);
#endif

  return fret;
}

/* Send a break character to a port.  Some port types may not support
   break characters, in which case we just return TRUE.  */

boolean
fport_break ()
{
  boolean (*pfbreak) P((struct sport *));

  DEBUG_MESSAGE0 (DEBUG_PORT, "fport_break: Sending break character");

  pfbreak = asPortcmds[(int) qPort->ttype].pfbreak;
  if (pfbreak == NULL)
    return TRUE;
  return (*pfbreak) (qPort);
}

/* Change the setting of a port.  Some port types may not support
   this, in which case we just return TRUE.  */

boolean
fport_set (tset)
     enum tportsetting tset;
{
  boolean (*pfset) P((struct sport *, enum tportsetting));

  DEBUG_MESSAGE1 (DEBUG_PORT,
		  "fport_set: Changing setting to %d", (int) tset);

  pfset = asPortcmds[(int) qPort->ttype].pfset;
  if (pfset == NULL)
    return TRUE;
  return (*pfset) (qPort, tset);
}

/* Run a chat program on a port.  */

boolean
fport_run_chat (zprog)
     const char *zprog;
{
  return (*asPortcmds[(int) qPort->ttype].pfchat) (qPort, zprog);
}

/* Get the baud rate of a port.  */

long
iport_baud ()
{
  return (*asPortcmds[(int) qPort->ttype].pibaud) (qPort);
}

/* Modem dialing routines.  */

static boolean fpdo_dial P((struct sdialer *qdial, const char *zphone,
			    boolean ftranslate));

boolean
fmodem_dial (qport, qsys, pcproto_params, pqproto_params, pireliable)
     struct sport *qport;
     const struct ssysteminfo *qsys;
     int *pcproto_params;
     struct sproto_param **pqproto_params;
     int *pireliable;
{
  if (qport->u.smodem.zdialer != NULL)
    {
      char *zcopy, *zdialer;
      boolean ffirst;

      /* The zdialer string is a sequence of dialer/token pairs.  The
	 dialer portion names a dialer to use.  The token portion is
	 what \D and \T in the chat script expand to.  If there is no
	 token for the last dialer, the phone number for the system is
	 used.  */

      ffirst = TRUE;
      *pcproto_params = 0;
      *pqproto_params = NULL;
      *pireliable = 0;

      zcopy = (char *) alloca (strlen (qport->u.smodem.zdialer) + 1);
      strcpy (zcopy, qport->u.smodem.zdialer);

      zdialer = strtok (zcopy, " \t");
      while (zdialer != NULL)
	{
	  struct sdialer s;
	  char *ztoken;
	  boolean ftranslate, ftoken;

	  if (! fread_dialer_info (zdialer, &s))
	    return FALSE;

	  ztoken = strtok ((char *) NULL, " \t");
	  ftoken = ztoken != NULL;

	  ftranslate = FALSE;
	  if (ztoken == NULL
	      || strcmp (ztoken, "\\D") == 0)
	    ztoken = qsys->zphone;
	  else if (strcmp (ztoken, "\\T") == 0)
	    {
	      ztoken = qsys->zphone;
	      ftranslate = TRUE;
	    }

	  /* Perhaps we should accumulate the protocol parameters.  */
	  if (ffirst)
	    {
	      *pcproto_params = s.cproto_params;
	      *pqproto_params = s.qproto_params;
	      *pireliable = s.ireliable;
	      ffirst = FALSE;
	    }

	  if (! fpdo_dial (&s, ztoken, ftranslate))
	    return FALSE;

	  if (! ftoken)
	    zdialer = NULL;
	  else
	    zdialer = strtok ((char *) NULL, " \t");
	}

      return TRUE;
    }
  else if (qport->u.smodem.qdialer != NULL)
    {
      *pcproto_params = qport->u.smodem.qdialer->cproto_params;
      *pqproto_params = qport->u.smodem.qdialer->qproto_params;
      *pireliable = qport->u.smodem.qdialer->ireliable;
      return fpdo_dial (qport->u.smodem.qdialer, qsys->zphone, FALSE);
    }
  else
    {
      ulog (LOG_ERROR, "No dialer information");
      return FALSE;
    }
}

/* Actually use a dialer.  We set up the modem (which may include
   opening the dialer device), run the chat script, and finish dealing
   with the modem.  */

static boolean
fpdo_dial (qdial, zphone, ftranslate)
     struct sdialer *qdial;
     const char *zphone;
     boolean ftranslate;
{
  if (! fsysdep_modem_begin_dial (qPort, qdial))
    return FALSE;

  if (! fchat (&qdial->schat, (const struct ssysteminfo *) NULL, qdial,
	       zphone, ftranslate, qPort->zname, iport_baud ()))
    return FALSE;

  return fsysdep_modem_end_dial (qPort, qdial);
}

/* Permit no carrier from a port.  */

boolean
fport_no_carrier ()
{
  return fsysdep_modem_no_carrier (qPort);
}

/* Require carrier from a port.  */

boolean
fport_need_carrier ()
{
  return fsysdep_modem_need_carrier (qPort);
}
