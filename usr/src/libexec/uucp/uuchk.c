/* uuchk.c
   Display what we think the permissions of systems are.

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

   $Log: uuchk.c,v $
   Revision 1.28  1992/03/28  20:31:55  ian
   Franc,ois Pinard: allow a name to be given to an alternate

   Revision 1.27  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.26  1992/03/10  21:47:39  ian
   Added protocol command for ports

   Revision 1.25  1992/03/09  18:10:26  ian
   Zacharias Beckman: put acceptable commands and path on different lines

   Revision 1.24  1992/03/07  02:56:30  ian
   Rewrote time routines

   Revision 1.23  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.22  1992/02/23  19:50:50  ian
   Handle READ and WRITE in Permissions correctly

   Revision 1.21  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.20  1992/02/08  22:33:32  ian
   Only get the current working directory if it's going to be needed

   Revision 1.19  1992/02/08  20:33:57  ian
   Handle all possible signals raised by abort

   Revision 1.18  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.17  1992/01/15  07:06:29  ian
   Set configuration directory in Makefile rather than sysdep.h

   Revision 1.16  1992/01/14  04:04:17  ian
   Chip Salzenberg: strcmp is a macro on AIX

   Revision 1.15  1992/01/05  03:09:17  ian
   Changed abProgram and abVersion to non const to avoid compiler bug

   Revision 1.14  1991/12/23  05:15:54  ian
   David Nugent: set debugging level for a specific system

   Revision 1.13  1991/12/22  22:14:53  ian
   Added externs for strcasecmp or strncasecmp

   Revision 1.12  1991/12/18  03:54:14  ian
   Made error messages to terminal appear more normal

   Revision 1.11  1991/12/17  23:14:08  ian
   T. William Wells: allow dialer complete and abort to be chat scripts

   Revision 1.10  1991/12/15  04:17:11  ian
   Added chat-seven-bit command to control parity bit stripping

   Revision 1.9  1991/12/15  03:42:33  ian
   Added tprocess_chat_cmd for all chat commands, and added CMDTABTYPE_PREFIX

   Revision 1.8  1991/12/01  14:45:53  ian
   Bob Izenberg: report dialer/token pairs correctly

   Revision 1.7  1991/11/21  22:17:06  ian
   Add version string, print version when printing usage

   Revision 1.6  1991/11/13  20:38:00  ian
   Added TCP port type for connections over TCP

   Revision 1.5  1991/11/12  19:47:04  ian
   Add called-chat set of commands to run a chat script on an incoming call

   Revision 1.4  1991/11/11  23:47:24  ian
   Added chat-program to run a program to do a chat script

   Revision 1.3  1991/11/11  16:19:21  ian
   Added message for no protocol specified

   Revision 1.2  1991/09/19  02:22:44  ian
   Chip Salzenberg's patch to allow ";retrytime" at the end of a time string

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char uuchk_rcsid[] = "$Id: uuchk.c,v 1.28 1992/03/28 20:31:55 ian Rel $";
#endif

#include "getopt.h"

#include "port.h"
#include "system.h"
#include "sysdep.h"
#include "uutime.h"

/* External functions.  */
extern int strcasecmp ();

/* Program name.  */
char abProgram[] = "uuchk";

/* Local functions.  */

static void ukusage P((void));
static void ukshow P((const struct ssysteminfo *qsys));
static boolean fkshow_port P((struct sport *qport, boolean fin));
static void ukshow_dialer P((struct sdialer *qdial));
static void ukshow_chat P((const struct schat_info *qchat,
			   const char *zhdr));
static void ukshow_size P((const char *z, boolean fcall, boolean flocal));
static void ukshow_proto_params P((int c, struct sproto_param *pas,
				   int cindent));
static void ukshow_time P((const struct sspan *));
static struct sspan *qcompress_span P((struct sspan *));

/* Local variables.  */
static boolean fKgot_port;

/* Long getopt options.  */

static const struct option asKlongopts[] = { { NULL, 0, NULL, 0 } };

const struct option *_getopt_long_options = asKlongopts;

int
main (argc, argv)
     int argc;
     char **argv;
{
  int iopt;
  /* The configuration file name.  */
  const char *zconfig = NULL;
  int c;
  struct ssysteminfo *pas;
  int i;

  while ((iopt = getopt (argc, argv, "I:x:")) != EOF)
    {
      switch (iopt)
	{
	case 'I':
	  /* Set the configuration file name.  */
	  zconfig = optarg;
	  break;

	case 'x':
#if DEBUG > 1
	  /* Set the debugging level.  */
	  iDebug |= idebug_parse (optarg);
#endif
	  break;

	case 0:
	  /* Long option found and flag set.  */
	  break;

	default:
	  ukusage ();
	  break;
	}
    }

  if (optind != argc)
    ukusage ();

  uread_config (zconfig);

  usysdep_initialize (FALSE, FALSE);

  uread_all_system_info (&c, &pas);

  for (i = 0; i < c; i++)
    {
      ukshow (&pas[i]);
      if (i < c - 1)
	printf ("\n");
    }

  ulog_close ();

  usysdep_exit (TRUE);

  /* Avoid errors about not returning a value.  */
  return 0;
}

/* Print a usage message and die.  */

static void
ukusage ()
{
  fprintf (stderr,
	   "Taylor UUCP version %s, copyright (C) 1991, 1992 Ian Lance Taylor\n",
	   abVersion);
  fprintf (stderr,
	   "Usage: uuchk [-I file] [-x debug]\n");
  fprintf (stderr,
	   " -x debug: Set debugging level (0 for none, 9 is max)\n");
#if HAVE_TAYLOR_CONFIG
  fprintf (stderr,
	   " -I file: Set configuration file to use (default %s%s)\n",
	   NEWCONFIGLIB, CONFIGFILE);
#endif /* HAVE_TAYLOR_CONFIG */
  exit (EXIT_FAILURE);
}

/* Dump out the information for a system.  */

static void
ukshow (qsys)
     const struct ssysteminfo *qsys;
{
  int i;
  const struct ssysteminfo *qlast;

  printf ("System: %s", qsys->zname);
  if (qsys->zalias != NULL)
    printf (" (%s)", qsys->zalias);
  printf ("\n");

  qlast = NULL;
  for (i = 0; qsys != NULL; qlast = qsys, qsys = qsys->qalternate, i++)
    {
      boolean fcall, fcalled;
      struct sspan *qtime, *qspan;

      if (i != 0 || qsys->qalternate != NULL)
	{
	  printf ("Alternate %d", i);
	  if (qsys->zalternate != NULL)
	    printf (" (%s)", qsys->zalternate);
	  printf ("\n");
	}

      /* See if this alternate could be used when calling out.  */

      fcall = (i == 0
	       || qsys->ztime != qlast->ztime
	       || qsys->zport != qlast->zport
	       || qsys->qport != qlast->qport
	       || qsys->ibaud != qlast->ibaud
	       || qsys->zphone != qlast->zphone
	       || qsys->schat.zprogram != qlast->schat.zprogram
	       || qsys->schat.zchat != qlast->schat.zchat);

      if (! fcall)
	qtime = NULL;
      else
	{
	  qtime = qtimegrade_parse (qsys->ztime);
	  if (qtime == NULL)
	    fcall = FALSE;
	}

      /* If this is the first alternate, it can be used to accept
	 a call.  Otherwise it can be used if it specifies a different
	 login name than the previous alternate.  */
      fcalled = (i == 0
		 || (qlast != NULL
		     && qsys->zcalled_login != NULL
		     && (qlast->zcalled_login == NULL
			 || strcmp (qsys->zcalled_login,
				    qlast->zcalled_login) != 0)));

      if (! fcall && ! fcalled)
	{
	  printf (" This alternate is never used\n");
	  continue;
	}

      if (fcalled)
	{
	  if (qsys->zcalled_login != NULL
	      && strcmp (qsys->zcalled_login, "ANY") != 0)
	    {
	      if (i == 0 && qsys->qalternate == NULL)
		printf (" Caller must log in as %s\n", qsys->zcalled_login);
	      else
		printf (" When called using login name %s\n",
			qsys->zcalled_login);
	    }
	  else
	    printf (" When called using any login name\n");

	  if (qsys->zlocalname != NULL)
	    printf (" Will use %s as name of local system\n",
		    qsys->zlocalname);
	}

      if (fcalled && qsys->fcallback)
	{
	  printf (" If called, will call back\n");
	  fcalled = FALSE;
	}

      if (fcall)
	{
	  if (i == 0 && qsys->qalternate == NULL)
	    printf (" Call out");
	  else
	    printf (" This alternate applies when calling");
	  
	  if (qsys->zport != NULL || qsys->qport != NULL)
	    {
	      printf (" using ");
	      if (qsys->zport != NULL)
		printf ("port %s", qsys->zport);
	      else
		printf ("a specially defined port");
	      if (qsys->ibaud != 0)
		{
		  printf (" at speed %ld", qsys->ibaud);
		  if (qsys->ihighbaud != 0)
		    printf (" to %ld", qsys->ihighbaud);
		}
	      printf ("\n");
	    }
	  else if (qsys->ibaud != 0)
	    {
	      printf (" at speed %ld", qsys->ibaud);
	      if (qsys->ihighbaud != 0)
		printf (" to %ld", qsys->ihighbaud);
	      printf ("\n");
	    }
	  else
	    printf (" using any port\n");

	  if (qsys->qport != NULL)
	    {
	      printf (" The port is defined as:\n");
	      (void) fkshow_port (qsys->qport, FALSE);
	    }
	  else
	    {
	      struct sport sdummy;

	      printf (" The possible ports are:\n");
	      fKgot_port = FALSE;
	      (void) ffind_port (qsys->zport, qsys->ibaud,
				 qsys->ihighbaud, &sdummy,
				 fkshow_port, FALSE);
	      if (! fKgot_port)
		printf (" *** There are no matching ports\n");
	    }

	  if (qsys->zphone != NULL)
	    {
#if HAVE_TCP
	      if ((qsys->zport != NULL
		   && strcmp (qsys->zport, "TCP") == 0)
		  || (qsys->qport != NULL
		      && qsys->qport->ttype == PORTTYPE_TCP))
		printf (" Remote address %s\n", qsys->zphone);
	      else
#endif /* HAVE_TCP */
		printf (" Phone number %s\n", qsys->zphone);
	    }

	  ukshow_chat (&qsys->schat, " Chat");

	  if (qsys->zcall_login != NULL)
	    {
	      if (strcmp (qsys->zcall_login, "*") != 0)
		printf (" Login name %s\n", qsys->zcall_login);
	      else
		{
		  char *zlogin, *zpass;

		  if (! fcallout_login (qsys, &zlogin, &zpass))
		    printf (" Can not determine login name\n");
		  else
		    {
		      printf (" Login name %s\n", zlogin);
		      xfree ((pointer) zlogin);
		      xfree ((pointer) zpass);
		    }
		}
	    }

	  if (qsys->zcall_password != NULL)
	    {
	      if (strcmp (qsys->zcall_password, "*") != 0)
		printf (" Password %s\n", qsys->zcall_password);
	      else
		{
		  char *zlogin, *zpass;

		  if (! fcallout_login (qsys, &zlogin, &zpass))
		    printf (" Can not determine password\n");
		  else
		    {
		      printf (" Password %s\n", zpass);
		      xfree ((pointer) zlogin);
		      xfree ((pointer) zpass);
		    }
		}
	    }

	  qtime = qcompress_span (qtime);

	  for (qspan = qtime; qspan != NULL; qspan = qspan->qnext)
	    {
	      printf (" ");
	      ukshow_time (qspan);
	      printf (" may call if ");
	      if ((char) qspan->ival == BGRADE_LOW)
		printf ("any work");
	      else
		printf ("work grade %c or higher", (char) qspan->ival);
	      if (qspan->cretry != 0)
		printf (" (retry %d)", qspan->cretry);
	      printf ("\n");
	    }

	  utimespan_free (qtime);

	  if (qsys->zcalltimegrade != NULL)
	    {
	      boolean fprint, fother;

	      qtime = qtimegrade_parse (qsys->zcalltimegrade);
	      qtime = qcompress_span (qtime);
	      fprint = FALSE;
	      fother = FALSE;
	      if (qtime->istart != 0)
		fother = TRUE;
	      for (qspan = qtime; qspan != NULL; qspan = qspan->qnext)
		{
		  if ((char) qspan->ival == BGRADE_LOW)
		    {
		      fother = TRUE;
		      continue;
		    }
		  fprint = TRUE;
		  printf (" ");
		  ukshow_time (qspan);
		  printf (" may accept work grade %c or higher\n",
			  (char) qspan->ival);
		  if (qspan->qnext == NULL)
		    {
		      if (qspan->iend != 7 * 24 * 60)
			fother = TRUE;
		    }
		  else
		    {
		      if (qspan->iend != qspan->qnext->istart)
			fother = TRUE;
		    }
		}
	      if (fprint && fother)
		printf (" (At other times may accept any work)\n");

	      utimespan_free (qtime);
	    }
	}

      if (qsys->fsequence)
	printf (" Sequence numbers are used\n");

      if (fcalled)
	ukshow_chat (&qsys->scalled_chat, " When called, chat");

#if DEBUG > 1
      if (qsys->idebug != 0)
	printf (" Debugging level 0%o\n", (unsigned int) qsys->idebug);
      if (qsys->imax_remote_debug != 0)
	printf (" Max remote debugging level 0%o\n",
		(unsigned int) qsys->imax_remote_debug);
#endif

      if (fcall)
	{
	  ukshow_size (qsys->zcall_local_size, TRUE, TRUE);
	  ukshow_size (qsys->zcall_remote_size, TRUE, FALSE);
	}
      if (fcalled)
	{
	  ukshow_size (qsys->zcalled_local_size, FALSE, TRUE);
	  ukshow_size (qsys->zcalled_remote_size, FALSE, TRUE);
	}

      if (fcall)
	{
	  printf (" %sllow remote requests when calling\n",
		  qsys->fcall_request ? "A" : "Do not a");
	  printf (" May %smake local requests when calling\n",
		  qsys->fcall_transfer ? "" : "not ");
	}

      if (fcalled)
	{
	  printf (" %sllow remote requests when called\n",
		  qsys->fcalled_request ? "A" : "Do not a");
	  printf (" May %smake local requests when called\n",
		  qsys->fcalled_transfer ? "" : "not ");
	}

      if (qsys->fcall_transfer || qsys->fcalled_transfer)
	{
	  if (qsys->zcalled_local_send == NULL)
	    printf (" May send by local request: %s\n", qsys->zlocal_send);
	  else
	    {
	      if (fcall && qsys->fcall_transfer)
		printf (" May send by local request when calling: %s\n",
			qsys->zlocal_send);
	      if (fcalled && qsys->fcalled_transfer)
		printf (" May send by local request when called: %s\n",
			qsys->zcalled_local_send);
	    }	    
	}
      if (qsys->fcall_request || qsys->fcalled_request)
	{
	  if (qsys->zcalled_remote_send == NULL)
	    printf (" May send by remote request: %s\n", qsys->zremote_send);
	  else
	    {
	      if (fcall && qsys->fcall_request)
		printf (" May send by remote request when calling: %s\n",
			qsys->zremote_send);
	      if (fcalled && qsys->fcalled_request)
		printf (" May send by remote request when called: %s\n",
			qsys->zcalled_remote_send);
	    }
	}
      if (qsys->fcall_transfer || qsys->fcalled_transfer)
	{
	  if (qsys->zcalled_local_receive == NULL)
	    printf (" May accept by local request: %s\n",
		    qsys->zlocal_receive);
	  else
	    {
	      if (fcall && qsys->fcall_transfer)
		printf (" May accept by local request when calling: %s\n",
			qsys->zlocal_receive);
	      if (fcalled && qsys->fcalled_transfer)
		printf (" May accept by local request when called: %s\n",
			qsys->zcalled_local_receive);
	    }
	}
      if (qsys->fcall_request || qsys->fcalled_request)
	{
	  if (qsys->zcalled_remote_receive == NULL)
	    printf (" May accept by remote request: %s\n",
		    qsys->zremote_receive);
	  else
	    {
	      if (fcall && qsys->fcall_request)
		printf (" May accept by remote request when calling: %s\n",
			qsys->zremote_receive);
	      if (fcalled && qsys->fcalled_request)
		printf (" May accept by remote request when called: %s\n",
			qsys->zcalled_remote_receive);
	    }
	}

      printf (" May execute %s\n", qsys->zcmds);
      printf (" Execution path %s\n", qsys->zpath);

      if (qsys->cfree_space != 0)
	printf (" Will leave %ld bytes available\n", qsys->cfree_space);

      if (qsys->zpubdir != NULL)
	printf (" Public directory is %s\n", qsys->zpubdir);

      if (qsys->zprotocols != NULL)
	printf (" Will use protocols %s\n", qsys->zprotocols);
      else
	printf (" Will use any known protocol\n");

      if (qsys->cproto_params != 0)
	ukshow_proto_params (qsys->cproto_params, qsys->qproto_params, 1);
    }
}

/* Show information about a port.  */

/*ARGSUSED*/
static boolean
fkshow_port (qport, fin)
     struct sport *qport;
     boolean fin;
{
  fKgot_port = TRUE;

  printf ("  Port name %s\n", qport->zname);
  switch (qport->ttype)
    {
    case PORTTYPE_STDIN:
      printf ("   Port type stdin\n");
      break;
    case PORTTYPE_DIRECT:
      printf ("   Port type direct\n");
      if (qport->u.sdirect.zdevice != NULL)
	printf ("   Device %s\n", qport->u.sdirect.zdevice);
      printf ("   Speed %ld\n", qport->u.sdirect.ibaud);
      break;
    case PORTTYPE_MODEM:
      printf ("   Port type modem\n");
      if (qport->u.smodem.zdevice != NULL)
	printf ("   Device %s\n", qport->u.smodem.zdevice);
      if (qport->u.smodem.zdial_device != NULL)
	printf ("   Dial device %s\n", qport->u.smodem.zdial_device);
      printf ("   Speed %ld\n", qport->u.smodem.ibaud);
      if (qport->u.smodem.ilowbaud != qport->u.smodem.ihighbaud)
	printf ("   Speed range %ld to %ld\n", qport->u.smodem.ilowbaud,
		qport->u.smodem.ihighbaud);
      printf ("   Carrier %savailable\n",
	      qport->u.smodem.fcarrier ? "" : "not ");
      if (qport->u.smodem.qdialer != NULL)
	{
	  printf ("   Specially defined dialer\n");
	  ukshow_dialer (qport->u.smodem.qdialer);
	}
      else if (qport->u.smodem.zdialer != NULL)
	{
	  const char *zc;
	  struct sdialer sdial;

	  /* This might be a single dialer name, or it might be a
	     sequence of dialer/token pairs.  */

	  zc = qport->u.smodem.zdialer;
	  if (zc[strcspn (zc, " \t")] == '\0')
	    {
	      if (! fread_dialer_info (qport->u.smodem.zdialer, &sdial))
		printf ("   *** No dialer %s\n", qport->u.smodem.zdialer);
	      else
		{
		  printf ("   Dialer %s\n", qport->u.smodem.zdialer);
		  ukshow_dialer (&sdial);
		}
	    }
	  else
	    {
	      char *z, *zdialer;

	      printf ("   Dialer sequence %s\n", zc);

	      z = (char *) alloca (strlen (zc) + 1);
	      strcpy (z, zc);

	      zdialer = strtok (z, " \t");
	      while (zdialer != NULL)
		{
		  char *ztoken;
	       
		  if (! fread_dialer_info (zdialer, &sdial))
		    printf ("   *** No dialer %s\n", zdialer);
		  else
		    {
		      printf ("   Dialer %s\n", zdialer);
		      ukshow_dialer (&sdial);
		    }

		  ztoken = strtok ((char *) NULL, " \t");
		  if (ztoken == NULL)
		    zdialer = NULL;
		  else
		    zdialer = strtok ((char *) NULL, " \t");
		}
	    }
	}
      else
	printf ("   *** No dialer information\n");
      break;
#if HAVE_TCP
    case PORTTYPE_TCP:
      printf ("   Port type tcp\n");
      printf ("   TCP service %s\n", qport->u.stcp.zport);
      break;
#endif /* HAVE_TCP */
    default:
      printf ("   CAN'T HAPPEN\n");
      break;
    }

  if (qport->zprotocols != NULL)
    printf ("   Will use protocols %s\n", qport->zprotocols);

  if (qport->cproto_params != 0)
    ukshow_proto_params (qport->cproto_params, qport->qproto_params, 3);

  /* Return FALSE to force ffind_port to continue searching.  */
  return FALSE;
}

/* Show information about a dialer.  */

static void
ukshow_dialer (q)
     struct sdialer *q;
{
  ukshow_chat (&q->schat, "    Chat");
  if (q->zdialtone != NULL)
    printf ("    Wait for dialtone %s\n", q->zdialtone);
  if (q->zpause != NULL)
    printf ("    Pause while dialing %s\n", q->zpause);
  printf ("    Carrier %savailable\n", q->fcarrier ? "" : "not ");
  if (q->fcarrier)
    printf ("    Wait %d seconds for carrier\n", q->ccarrier_wait);
  if (q->fdtr_toggle)
    {
      printf ("    Toggle DTR");
      if (q->fdtr_toggle_wait)
	printf (" and wait");
      printf ("\n");
    }
  ukshow_chat (&q->scomplete, "    When complete chat");
  ukshow_chat (&q->sabort, "    When aborting chat");
  if (q->cproto_params != 0)
    ukshow_proto_params (q->cproto_params, q->qproto_params, 4);
}

/* Show a chat script.  */

static void
ukshow_chat (qchat, zhdr)
     const struct schat_info *qchat;
     const char *zhdr;
{
  if (qchat->zprogram != NULL)
    printf ("%s program %s\n", zhdr, qchat->zprogram);

  if (qchat->zchat != NULL)
    {
      printf ("%s script %s\n", zhdr, qchat->zchat);
      printf ("%s script timeout %d\n", zhdr, qchat->ctimeout);
      if (qchat->zfail != NULL)
	printf ("%s failure strings %s\n", zhdr, qchat->zfail);
      if (qchat->fstrip)
	printf ("%s script incoming bytes stripped to seven bits\n", zhdr);
    }
}

/* Show a size/time restriction.  */

static void
ukshow_size (zstring, fcall, flocal)
     const char *zstring;
     boolean fcall;
     boolean flocal;
{
  struct sspan *qspan, *q;
  boolean fother;

  qspan = qcompress_span (qtimesize_parse (zstring));
  if (qspan == NULL)
    return;

  printf (" If call%s the following applies to a %s request:\n",
	  fcall ? "ing" : "ed", flocal ? "local" : "remote");

  fother = FALSE;
  if (qspan->istart >= 60)
    fother = TRUE;

  for (q = qspan; q != NULL; q = q->qnext)
    {
      printf ("  ");
      ukshow_time (q);
      printf (" may transfer files %ld bytes or smaller\n", q->ival);
      if (q->qnext == NULL)
	{
	  if (q->iend <= 6 * 24 * 60 + 23 * 60)
	    fother = TRUE;
	}
      else
	{
	  if (q->iend + 60 <= q->qnext->istart)
	    fother = TRUE;
	}
    }

  if (fother)
    printf ("  (At other times may send files of any size)\n");

  utimespan_free (qspan);
}

/* Show protocol parameters.  */

static void
ukshow_proto_params (c, pas, cindent)
     int c;
     struct sproto_param *pas;
     int cindent;
{
  int ip;

  for (ip = 0; ip < c; ip++)
    {
      int ie, isp;

      for (isp = 0; isp < cindent; isp++)
	printf (" ");
      printf ("For protocol %c will use the following parameters\n",
	      pas[ip].bproto);
      for (ie = 0; ie < pas[ip].centries; ie++)
	{
	  int ia;
	  struct sproto_param_entry *qe;

	  qe = &pas[ip].qentries[ie];
	  for (isp = 0; isp < cindent; isp++)
	    printf (" ");
	  for (ia = 0; ia < qe->cargs; ia++)
	    printf (" %s", qe->azargs[ia]);
	  printf ("\n");
	}
    }
}

/* Display a time span.  */

static void
ukshow_time (q)
     const struct sspan *q;
{
  int idaystart, idayend;
  int ihourstart, ihourend;
  int iminutestart, iminuteend;
  const char * const zdays = "Sun\0Mon\0Tue\0Wed\0Thu\0Fri\0Sat\0Sun";

  if (q->istart == 0 && q->iend == 7 * 24 * 60)
    {
      printf ("At any time");
      return;
    }

  idaystart = q->istart / (24 * 60);
  ihourstart = (q->istart % (24 * 60)) / 60;
  iminutestart = q->istart % 60;
  idayend = q->iend / (24 * 60);
  ihourend = (q->iend % (24 * 60)) / 60;
  iminuteend = q->iend % 60;

  if (idaystart == idayend)
    printf ("%s from %02d:%02d to %02d:%02d", zdays + idaystart * 4,
	    ihourstart, iminutestart, ihourend, iminuteend);
  else
    printf ("From %s %02d:%02d to %s %02d:%02d",
	    zdays + idaystart * 4, ihourstart, iminutestart,
	    zdays + idayend * 4, ihourend, iminuteend);
}

/* Compress a time span by merging any two adjacent spans with
   identical values.  This isn't necessary for uucico, but it looks
   nicer when printed out.  */

static struct sspan *
qcompress_span (qlist)
     struct sspan *qlist;
{
  struct sspan **pq;

  pq = &qlist;
  while (*pq != NULL)
    {
      if ((*pq)->qnext != NULL
	  && (*pq)->iend == (*pq)->qnext->istart
	  && (*pq)->ival == (*pq)->qnext->ival)
	{
	  struct sspan *qfree;

	  qfree = (*pq)->qnext;
	  (*pq)->qnext = qfree->qnext;
	  (*pq)->iend = qfree->iend;
	  xfree ((pointer) qfree);
	}
      else
	pq = &(*pq)->qnext;
    }

  return qlist;
}
