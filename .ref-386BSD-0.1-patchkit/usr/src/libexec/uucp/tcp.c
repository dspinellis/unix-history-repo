/* tcp.c
   Code to handle TCP connections.

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

   $Log: tcp.c,v $
   Revision 1.16  1992/03/30  15:37:22  ian
   Petri Helenius: TCP server never started uuxqt

   Revision 1.15  1992/03/17  02:44:08  ian
   Declare _exit

   Revision 1.14  1992/03/17  00:34:10  ian
   Cast argument to bzero

   Revision 1.13  1992/03/15  04:51:17  ian
   Keep an array of signals we've received rather than a single variable

   Revision 1.12  1992/03/15  01:54:46  ian
   All execs are now done in isspawn, all waits are done in iswait

   Revision 1.11  1992/03/12  19:56:10  ian
   Debugging based on types rather than number

   Revision 1.10  1992/03/11  02:09:57  ian
   Franc,ois Pinard: retry fork several times before giving up

   Revision 1.9  1992/03/10  22:57:03  ian
   Petri Helenius: have server fork twice to avoid zombies

   Revision 1.8  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.7  1991/12/28  03:49:23  ian
   Added HAVE_MEMFNS and HAVE_BFNS; changed uses of memset to bzero

   Revision 1.6  1991/12/26  22:54:32  ian
   Monty Solomon: cast arguments to avoid prototype errors

   Revision 1.5  1991/12/22  22:14:19  ian
   Monty Solomon: added HAVE_UNISTD_H configuration parameter

   Revision 1.4  1991/12/10  19:45:05  ian
   Added ulog_device to record device name for log file

   Revision 1.3  1991/11/24  20:06:08  ian
   Michael Haberler: itcp_port_number of a number wasn't calling htons

   Revision 1.2  1991/11/14  03:20:13  ian
   Added seven-bit and reliable commands to help when selecting protocols

   Revision 1.1  1991/11/13  20:38:00  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char tcp_rcsid[] = "$Id: tcp.c,v 1.16 1992/03/30 15:37:22 ian Rel $";
#endif

#if HAVE_TCP

#include <errno.h>

#if USE_STDIO && HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>

#include "sysdep.h"
#include "port.h"
#include "system.h"

/* External functions.  */
extern int close ();
extern int socket (), bind (), listen (), accept (), connect ();
extern void _exit ();

/* This code handles TCP connections by providing a set of routines
   that are called by the port routines.  It assumes a Berkeley socket
   interface.  */

/* The normal "uucp" port number.  */
#define IUUCP_PORT (540)

/* Local function to get the port number from a name.  */
static int itcp_port_number P((const char *zport));

/* Lock a port.  There is no need to lock a TCP port.  */

boolean
ftcp_lock (qport, fin)
     struct sport *qport;
     boolean fin;
{
  return TRUE;
}

/* Open a port.  If the fwait argument is TRUE, we are running as a
   server.  Otherwise we are just trying to reach another system.  */

boolean
ftcp_open (qport, ibaud, fwait)
     struct sport *qport;
     long ibaud;
     boolean fwait;
{
  struct sockaddr_in s;

  ulog_device ("TCP");

  qport->u.stcp.o = socket (AF_INET, SOCK_STREAM, 0);
  if (qport->u.stcp.o == -1)
    {
      ulog (LOG_ERROR, "socket: %s", strerror (errno));
      return FALSE;
    }

  /* If we aren't waiting for a connection, we're done.  */

  if (! fwait)
    return TRUE;

  /* Run as a server and wait for a new connection.  The code in
     uucico.c has already detached us from our controlling terminal.
     From this point on if the server gets an error we exit; we only
     return if we have received a connection.  It would be more robust
     to respawn the server if it fails; someday.  */

  bzero ((pointer) &s, sizeof s);
  s.sin_family = AF_INET;
  s.sin_port = itcp_port_number (qport->u.stcp.zport);
  s.sin_addr.s_addr = htonl (INADDR_ANY);

  if (bind (qport->u.stcp.o, (struct sockaddr *) &s, sizeof s) == -1)
    {
      ulog (LOG_ERROR, "bind: %s", strerror (errno));
      usysdep_exit (FALSE);
    }
  if (listen (qport->u.stcp.o, 5) == -1)
    {
      ulog (LOG_ERROR, "listen: %s", strerror (errno));
      usysdep_exit (FALSE);
    }

  while (! FGOT_SIGNAL ())
    {
      int clen;
      int onew;
      pid_t ipid;

      DEBUG_MESSAGE0 (DEBUG_PORT,
		      "ftcp_open: Waiting for connections");

      clen = sizeof s;
      onew = accept (qport->u.stcp.o, (struct sockaddr *) &s, &clen);
      if (onew == -1)
	{
	  ulog (LOG_ERROR, "accept: %s", strerror (errno));
	  usysdep_exit (FALSE);
	}

      DEBUG_MESSAGE0 (DEBUG_PORT,
		      "ftcp_open: Got connection; forking");

      ipid = isfork ();
      if (ipid < 0)
	{
	  ulog (LOG_ERROR, "fork: %s", strerror (errno));
	  usysdep_exit (FALSE);
	}
      if (ipid == 0)
	{
	  (void) close (qport->u.stcp.o);
	  qport->u.stcp.o = onew;

	  /* Now we fork and let our parent die, so that we become
	     a child of init.  This lets the main server code wait
	     for its child and then continue without accumulating
	     zombie children.  */

	  ipid = isfork ();
	  if (ipid < 0)
	    {
	      ulog (LOG_ERROR, "fork: %s", strerror (errno));
	      _exit (EXIT_FAILURE);
	    }
	      
	  if (ipid != 0)
	    _exit (EXIT_SUCCESS);

	  return TRUE;
	}

      (void) close (onew);

      /* Now wait for the child.  */
      (void) iswait ((unsigned long) ipid, (const char *) NULL);
    }

  /* We got a signal.  */
  usysdep_exit (FALSE);

  /* Avoid compiler warnings.  */
  return FALSE;
}

/* Close the port.  */

boolean
ftcp_close (qport, fsuccess)
     struct sport *qport;
     boolean fsuccess;
{
  if (close (qport->u.stcp.o) == -1)
    {
      ulog (LOG_ERROR, "close: %s", strerror (errno));
      return FALSE;
    }
  return TRUE;
}

/* Reset the port.  This will be called by a child which was forked
   off in ftcp_open, above.  We don't want uucico to continue looping
   and giving login prompts, so we pretend that we received a SIGINT
   signal.  This should probably be handled more cleanly.  */

boolean
ftcp_reset (qport)
     struct sport *qport;
{
  afSignal[INDEXSIG_SIGINT] = TRUE;
  return TRUE;
}

/* Dial out on a TCP port, so to speak: connect to a remote computer.  */

boolean
ftcp_dial (qport, qsys, pcproto_params, pqproto_params, pireliable)
     struct sport *qport;
     const struct ssysteminfo *qsys;
     int *pcproto_params;
     struct sproto_param **pqproto_params;
     int *pireliable;
{
  const char *zhost;
  struct sockaddr_in s;
  struct hostent *q;

  /* There are no dialer protocol parameters, but a TCP connection is
     fully reliable.  */
  *pcproto_params = 0;
  *pqproto_params = NULL;
  *pireliable = (RELIABLE_SPECIFIED | RELIABLE_ENDTOEND
		 | RELIABLE_RELIABLE | RELIABLE_EIGHT);

  zhost = qsys->zphone;
  if (zhost == NULL)
    zhost = qsys->zname;

  /* Cast argument to avoid prototype error on NeXT.  */
  q = gethostbyname ((char *) zhost);
  if (q == NULL)
    {
      ulog (LOG_ERROR, "gethostbyname (%s): %s", zhost, strerror (errno));
      return FALSE;
    }

  s.sin_family = q->h_addrtype;
  s.sin_port = itcp_port_number (qport->u.stcp.zport);
  memcpy (&s.sin_addr.s_addr, q->h_addr, q->h_length);

  if (connect (qport->u.stcp.o, (struct sockaddr *) &s, sizeof s) == -1)
    {
      ulog (LOG_ERROR, "connect: %s", strerror (errno));
      return FALSE;
    }

  return TRUE;
}

/* Return the baud rate.  */

long
itcp_baud (qport)
     struct sport *qport;
{
  return 0;
}

/* Get the port number given a name.  The argument will almost always
   be "uucp" so we cache that value.  The return value is always in
   network byte order.  This returns -1 on error.  */

static int
itcp_port_number (zname)
     const char *zname;
{
  boolean fuucp;
  static int iuucp;
  int i;
  char *zend;
  struct servent *q;

  fuucp = strcmp (zname, "uucp") == 0;
  if (fuucp && iuucp != 0)
    return iuucp;

  /* Try it as a number first.  */
  i = strtol (zname, &zend, 10);
  if (i != 0 && *zend == '\0')
    return htons (i);

  /* Cast arguments to avoid prototype errors on NeXT.  */
  q = getservbyname ((char *) zname, (char *) "tcp");
  if (q == NULL)
    {
      /* We know that the "uucp" service should be 540, even if isn't
	 in /etc/services.  */
      if (fuucp)
	{
	  iuucp = htons (IUUCP_PORT);
	  return iuucp;
	}
      ulog (LOG_ERROR, "getservbyname (%s): %s", zname, strerror (errno));
      return -1;
    }

  if (fuucp)
    iuucp = q->s_port;

  return q->s_port;
}

#endif /* HAVE_TCP */
