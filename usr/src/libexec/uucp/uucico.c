/* uucico.c
   This is the main UUCP communication program.

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

   $Log: uucico.c,v $
   Revision 1.92  1992/04/06  21:10:14  ian
   Marc Boucher: set *pqsys to NULL in faccept_call

   Revision 1.91  1992/03/30  04:07:13  ian
   Dirk Musstopf: remove temporary file if receive fails

   Revision 1.90  1992/03/28  22:06:38  ian
   Michael I Bushnell: renamed enum tstatus to avoid header file conflict

   Revision 1.89  1992/03/28  20:52:11  ian
   Petri Helenius: must dump controlling terminal when going to next alternate

   Revision 1.88  1992/03/28  20:31:55  ian
   Franc,ois Pinard: allow a name to be given to an alternate

   Revision 1.87  1992/03/28  19:40:26  ian
   Close log and statistics file at each master/slave role switch

   Revision 1.86  1992/03/28  04:45:31  ian
   Mark E. Mallett: minor cleanup

   Revision 1.85  1992/03/17  18:42:21  ian
   T. William Wells: set current time in status file when call completes

   Revision 1.84  1992/03/17  01:03:03  ian
   Miscellaneous cleanup

   Revision 1.83  1992/03/16  05:16:03  ian
   Recognize SVR4 -U flag

   Revision 1.82  1992/03/16  04:38:00  ian
   Turn off DEBUG_PORT for handshake debugging

   Revision 1.81  1992/03/15  04:51:17  ian
   Keep an array of signals we've received rather than a single variable

   Revision 1.80  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.79  1992/03/11  19:53:55  ian
   Improved chat script debugging

   Revision 1.78  1992/03/11  00:18:50  ian
   Save temporary file if file send fails

   Revision 1.77  1992/03/10  23:01:20  ian
   Don't run uuxqt if we got a SIGTERM

   Revision 1.76  1992/03/10  21:47:39  ian
   Added protocol command for ports

   Revision 1.75  1992/03/09  20:14:37  ian
   Ted Lindgreen: added max-remote-debug command

   Revision 1.74  1992/03/09  19:52:50  ian
   Ted Lindgreen: strip parity from initial handshake strings

   Revision 1.73  1992/03/09  19:42:43  ian
   Ted Lindgreen: don't send mail for nonexistent file

   Revision 1.72  1992/03/09  05:37:10  ian
   Only look for hangup string in debugging mode

   Revision 1.71  1992/03/09  05:29:20  ian
   Ted Lindgreen: report requested grade on an incoming call

   Revision 1.70  1992/03/09  05:08:16  ian
   Added status for wrong time to call, not used if system can't be called

   Revision 1.69  1992/03/08  17:45:41  ian
   Ted Lindgreen: start uuxqt for only one system if appropriate

   Revision 1.68  1992/03/08  17:08:20  ian
   Ted Lindgreen: ignore -u option

   Revision 1.67  1992/03/08  16:42:41  ian
   Ted Lindgreen: report port and login name in log file

   Revision 1.66  1992/03/07  02:56:30  ian
   Rewrote time routines

   Revision 1.65  1992/03/04  15:05:51  ian
   Michael Haberler: some systems send \n after Shere

   Revision 1.64  1992/03/04  00:36:44  ian
   Michael Richardson: better chat script debugging

   Revision 1.63  1992/03/03  21:01:20  ian
   Use strict timeout in fsserial_read, eliminate all race conditions

   Revision 1.62  1992/03/03  06:06:48  ian
   T. William Wells: don't complain about missing configuration files

   Revision 1.61  1992/02/29  04:07:08  ian
   Added -j option to uucp and uux

   Revision 1.60  1992/02/29  01:06:59  ian
   Chip Salzenberg: recheck file permissions before sending

   Revision 1.59  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.58  1992/02/24  22:38:45  ian
   Don't treat an extra argument as a port

   Revision 1.57  1992/02/24  04:58:47  ian
   Only permit files to be received into directories that are world-writeable

   Revision 1.56  1992/02/23  19:50:50  ian
   Handle READ and WRITE in Permissions correctly

   Revision 1.55  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.54  1992/02/20  22:57:19  ian
   Chip Salzenberg: some systems truncate the Shere= machine name

   Revision 1.53  1992/02/19  19:36:07  ian
   Rearranged time functions

   Revision 1.52  1992/02/14  21:32:50  ian
   Niels Baggesen: under HAVE_BNU_LOGGING, don't lost system name when dieing

   Revision 1.51  1992/02/09  05:21:55  ian
   Bob Denny: call fmail_transfer before fsysdep_did_work

   Revision 1.50  1992/02/08  22:33:32  ian
   Only get the current working directory if it's going to be needed

   Revision 1.49  1992/02/08  20:33:57  ian
   Handle all possible signals raised by abort

   Revision 1.48  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.47  1992/02/07  17:08:15  ian
   Bob Denny: retry time not reached is not an error

   Revision 1.46  1992/01/29  18:37:27  ian
   Patrick Smith: only wait a short time for the hangup message

   Revision 1.45  1992/01/28  04:34:10  ian
   Marty Shannon: -f uucp flag not handled correctly

   Revision 1.44  1992/01/28  03:50:42  ian
   Chip Salzenberg: set .Status correctly if wrong time to call

   Revision 1.43  1992/01/21  19:39:12  ian
   Chip Salzenberg: uucp and uux start uucico for right system, not any

   Revision 1.42  1992/01/20  16:44:54  ian
   Marty Shannon: update .Status file if it's the wrong time to call

   Revision 1.41  1992/01/19  02:27:00  ian
   Marty Shannon: update .Status file on incoming calls

   Revision 1.40  1992/01/18  22:48:53  ian
   Reworked sending of mail and general handling of failed transfers

   Revision 1.39  1992/01/15  07:06:29  ian
   Set configuration directory in Makefile rather than sysdep.h

   Revision 1.38  1992/01/14  04:38:43  ian
   Chip Salzenberg: only declare sportinfo if it will be used

   Revision 1.37  1992/01/12  19:53:05  ian
   John Antypas: pass in sportinfo structure for fdo_call to use

   Revision 1.36  1992/01/05  03:09:17  ian
   Changed abProgram and abVersion to non const to avoid compiler bug

   Revision 1.35  1992/01/04  21:53:36  ian
   Start up uuxqt even if a call fails

   Revision 1.34  1991/12/31  19:43:13  ian
   Added 'e' protocol

   Revision 1.33  1991/12/28  04:33:09  ian
   Set fmasterdone correctly in slave mode

   Revision 1.32  1991/12/23  05:15:54  ian
   David Nugent: set debugging level for a specific system

   Revision 1.31  1991/12/21  23:10:43  ian
   Terry Gardner: record failed file transfers in statistics file

   Revision 1.30  1991/12/21  22:17:20  ian
   Change protocol ordering to 't', 'g', 'f'

   Revision 1.29  1991/12/21  22:07:47  ian
   John Theus: don't warn if port file does not exist

   Revision 1.28  1991/12/20  04:30:24  ian
   Terry Gardner: record conversation time in log file

   Revision 1.27  1991/12/20  00:42:24  ian
   Clear user name from error message given by getting next command

   Revision 1.26  1991/12/18  05:12:00  ian
   Added -l option to uucico to prompt for login name once and then exit

   Revision 1.25  1991/12/18  03:54:14  ian
   Made error messages to terminal appear more normal

   Revision 1.24  1991/12/17  04:55:01  ian
   David Nugent: ignore SIGHUP in uucico and uuxqt

   Revision 1.23  1991/12/15  03:42:33  ian
   Added tprocess_chat_cmd for all chat commands, and added CMDTABTYPE_PREFIX

   Revision 1.22  1991/12/11  03:59:19  ian
   Create directories when necessary; don't just assume they exist

   Revision 1.21  1991/11/21  22:17:06  ian
   Add version string, print version when printing usage

   Revision 1.20  1991/11/16  00:33:28  ian
   Remove ?: operator between string literal and variable

   Revision 1.19  1991/11/14  03:40:10  ian
   Try to figure out whether stdin is a TCP port

   Revision 1.18  1991/11/14  03:20:13  ian
   Added seven-bit and reliable commands to help when selecting protocols

   Revision 1.17  1991/11/13  23:08:40  ian
   Expand remote pathnames in uucp and uux; fix up uux special cases

   Revision 1.16  1991/11/12  19:47:04  ian
   Add called-chat set of commands to run a chat script on an incoming call

   Revision 1.15  1991/11/12  18:25:33  ian
   Added 't' protocol

   Revision 1.14  1991/11/11  23:47:24  ian
   Added chat-program to run a program to do a chat script

   Revision 1.13  1991/11/11  19:32:03  ian
   Added breceive_char to read characters through protocol buffering

   Revision 1.12  1991/11/11  18:55:52  ian
   Get protocol parameters from port and dialer for incoming calls

   Revision 1.11  1991/11/11  16:59:05  ian
   Eliminate fread_port_info, allow NULL pflock arg to ffind_port

   Revision 1.10  1991/11/11  04:21:16  ian
   Added 'f' protocol

   Revision 1.9  1991/11/10  19:24:22  ian
   Added pffile protocol entry point for file level control

   Revision 1.8  1991/11/09  18:53:07  ian
   Reworked protocol interface

   Revision 1.7  1991/11/07  18:15:38  ian
   Chip Salzenberg: move CMAXRETRIES to conf.h for easy configuration

   Revision 1.6  1991/09/19  03:06:04  ian
   Chip Salzenberg: put BNU temporary files in system's directory

   Revision 1.5  1991/09/19  02:30:37  ian
   From Chip Salzenberg: check whether signal is ignored differently

   Revision 1.4  1991/09/19  02:22:44  ian
   Chip Salzenberg's patch to allow ";retrytime" at the end of a time string

   Revision 1.3  1991/09/12  05:04:26  ian
   Changed sense of \0 return from btime_low_grade on calltimegrade

   Revision 1.2  1991/09/11  02:33:14  ian
   Added ffork argument to fsysdep_run
  
   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision
  
   */

#include "uucp.h"

#if USE_RCS_ID
char uucico_rcsid[] = "$Id: uucico.c,v 1.92 1992/04/06 21:10:14 ian Rel $";
#endif

#include <ctype.h>

#include "getopt.h"

#include "port.h"
#include "prot.h"
#include "system.h"
#include "uutime.h"

/* The program name.  */
char abProgram[] = "uucico";

/* Define the known protocols.
   bname, ffullduplex, qcmds, pfstart, pfshutdown, pfsendcmd, pzgetspace,
   pfsenddata, pfprocess, pfwait, pffile  */

static struct sprotocol asProtocols[] =
{
  { 't', FALSE, RELIABLE_ENDTOEND | RELIABLE_RELIABLE | RELIABLE_EIGHT,
      asTproto_params, ftstart, ftshutdown, ftsendcmd, ztgetspace,
      ftsenddata, ftprocess, ftwait, ftfile },
  { 'e', FALSE, RELIABLE_ENDTOEND | RELIABLE_RELIABLE | RELIABLE_EIGHT,
      asEproto_params, festart, feshutdown, fesendcmd, zegetspace,
      fesenddata, feprocess, fewait, fefile },
  { 'g', FALSE, RELIABLE_EIGHT,
      asGproto_params, fgstart, fgshutdown, fgsendcmd, zggetspace,
      fgsenddata, fgprocess, fgwait, NULL },
  { 'f', FALSE, RELIABLE_RELIABLE,
      asFproto_params, ffstart, ffshutdown, ffsendcmd, zfgetspace,
      ffsenddata, ffprocess, ffwait, fffile },
};

#define CPROTOCOLS (sizeof asProtocols / sizeof asProtocols[0])

/* Locked system.  */

static boolean fLocked_system;
static struct ssysteminfo sLocked_system;

/* Local functions.  */

static void uusage P((void));
static void uabort P((void));
static boolean fcall P((const struct ssysteminfo *qsys,
			struct sport *qport,
			boolean fforce, int bgrade,
			boolean fnodetach));
static boolean fdo_call P((const struct ssysteminfo *qsys,
			   struct sport *qport,
			   struct sstatus *qstat, int cretry,
			   boolean *pfcalled, struct sport *quse));
static boolean fcall_failed P((const struct ssysteminfo *qsys,
			       enum tstatus_type twhy,
			       struct sstatus *qstat, int cretry));
static boolean flogin_prompt P((struct sport *qport));
static boolean faccept_call P((const char *zlogin, struct sport *qport,
			       const struct ssysteminfo **pqsys));
static boolean fuucp P((boolean fmaster, const struct ssysteminfo *qsys,
			int bgrade, boolean fnew, long cmax_receive));
static boolean fdo_xcmd P((const struct ssysteminfo *qsys,
			   boolean fcaller,
			   const struct scmd *qcmd));
static boolean fok_to_send P((const char *zfrom, boolean flocal,
			      boolean fcaller, boolean fspool,
			      const struct ssysteminfo *qsys,
			      const char *zuser));
static boolean fok_to_receive P((const char *zto, boolean flocal,
				 boolean fcaller,
				 const struct ssysteminfo *qsys,
				 const char *zuser));
static boolean frequest_ok P((boolean flocal, boolean fcaller,
			      const struct ssysteminfo *qsys,
			      const char *zuser));
static boolean fsend_uucp_cmd P((const char *z));
static const char *zget_uucp_cmd P((boolean frequired));
static const char *zget_typed_line P((void));

/* Long getopt options.  */

static const struct option asLongopts[] = { { NULL, 0, NULL, 0 } };

const struct option *_getopt_long_options = asLongopts;

int
main (argc, argv)
     int argc;
     char **argv;
{
  /* getopt return value  */
  int iopt;
  /* Don't detach from controlling terminal.  */
  boolean fnodetach = FALSE;
  /* Configuration file name  */
  const char *zconfig = NULL;
  /* System to call  */
  const char *zsystem = NULL;
  /* Port to use; in master mode, call out on this port.  In slave mode,
     accept logins on this port.  If port not specified, then in master
     mode figure it out for each system, and in slave mode use stdin and
     stdout.  */
  const char *zport = NULL;
  /* Port information for the port name in zport.  */
  struct sport sportinfo;
  /* Pointer to port to use, or NULL if unknown.  */
  struct sport *qport;
  /* Whether to start uuxqt when done.  */
  boolean fuuxqt = TRUE;
  /* Whether to force a call despite status of previous call  */
  boolean fforce = FALSE;
  /* Whether we are the master  */
  boolean fmaster = FALSE;
  /* Whether to give a single login prompt.  */
  boolean flogin = FALSE;
  /* Whether to do an endless loop of accepting calls  */
  boolean floop = FALSE;
  /* Whether to wait for an inbound call after doing an outbound call  */
  boolean fwait = FALSE;
  boolean fret = TRUE;
#if DEBUG > 1
  int iholddebug;
#endif

  while ((iopt = getopt (argc, argv,
			 "DefI:lp:qr:s:S:u:x:X:w")) != EOF)
    {
      switch (iopt)
	{
	case 'D':
	  /* Don't detach from controlling terminal.  */
	  fnodetach = TRUE;
	  break;

	case 'e':
	  /* Do an endless loop of accepting calls.  */
	  floop = TRUE;
	  break;

	case 'f':
	  /* Force a call even if it hasn't been long enough since the last
	     failed call.  */
	  fforce = TRUE;
	  break;

	case 'I':
	  /* Set configuration file name (default is in sysdep.h).  */
	  zconfig = optarg;
	  break;

	case 'l':
	  /* Prompt for login name and password.  */
	  flogin = TRUE;
	  break;

	case 'p':
	  /* Port to use  */
	  zport = optarg;
	  break;

	case 'q':
	  /* Don't start uuxqt.  */
	  fuuxqt = FALSE;
	  break;

	case 'r':
	  /* Set mode: -r1 for master, -r0 for slave (default)  */
	  if (optarg[0] == '1' && optarg[1] == '\0')
	    fmaster = TRUE;
	  else if (optarg[0] == '0' && optarg[1] == '\0')
	    fmaster = FALSE;
	  else
	    uusage ();
	  break;
    
	case 's':
	  /* Set system name  */
	  zsystem = optarg;
	  fmaster = TRUE;
	  break;

	case 'S':
	  /* Set system name and force call like -f  */
	  zsystem = optarg;
	  fforce = TRUE;
	  fmaster = TRUE;
	  break;

	case 'u':
	  /* Some versions of uucpd invoke uucico with a -u argument
	     specifying the login name.  I'm told it is safe to ignore
	     this value, although perhaps we should use it rather than
	     zsysdep_login_name ().  */
	  break;

	case 'x':
	case 'X':
#if DEBUG > 1
	  /* Set debugging level  */
	  iDebug |= idebug_parse (optarg);
#endif
	  break;

	case 'w':
	  /* Call out and then wait for a call in  */
	  fwait = TRUE;
	  break;

	case 0:
	  /* Long option found, and flag value set.  */
	  break;

	default:
	  uusage ();
	  break;
	}
    }

  if (optind != argc)
    uusage ();

  if (fwait && zport == NULL)
    {
      fprintf (stderr, "%s: -w requires -e\n", abProgram);
      uusage ();
    }

  uread_config (zconfig);

#ifdef SIGINT
  usysdep_signal (SIGINT);
#endif
#ifdef SIGHUP
  usysdep_signal (SIGHUP);
#endif
#ifdef SIGQUIT
  usysdep_signal (SIGQUIT);
#endif
#ifdef SIGTERM
  usysdep_signal (SIGTERM);
#endif
#ifdef SIGPIPE
  usysdep_signal (SIGPIPE);
#endif

  usysdep_initialize (TRUE, FALSE);

  ulog_to_file (TRUE);
  ulog_fatal_fn (uabort);

  /* If a port was named, get its information.  */
  if (zport == NULL)
    qport = NULL;
  else
    {
      if (! ffind_port (zport, (long) 0, (long) 0, &sportinfo,
			(boolean (*) P((struct sport *, boolean))) NULL,
			FALSE))
	{
	  ulog (LOG_ERROR, "%s: No such port", zport);
	  ulog_close ();
	  usysdep_exit (FALSE);
	}
      qport = &sportinfo;
    }

  if (fmaster)
    {
      /* If a system was named, call it up.  Otherwise check all the
	 known systems, and call all the ones which have work to do.  */
      if (zsystem != NULL)
	{
	  if (! fread_system_info (zsystem, &sLocked_system))
	    ulog (LOG_FATAL, "Unknown system %s", zsystem);

	  /* Detach from the controlling terminal for the call.  This
	     probably makes sense only on Unix.  We want the modem
	     line to become the controlling terminal.  */
	  if (! fnodetach &&
	      (qport == NULL || qport->ttype != PORTTYPE_STDIN))
	    usysdep_detach ();

	  ulog_system (sLocked_system.zname);

#if DEBUG > 1
	  iholddebug = iDebug;
	  iDebug |= sLocked_system.idebug;
#endif

	  if (! fsysdep_lock_system (&sLocked_system))
	    {
	      ulog (LOG_ERROR, "System already locked");
	      fret = FALSE;
	    }
	  else
	    {
	      fLocked_system = TRUE;
	      fret = fcall (&sLocked_system, qport, fforce, BGRADE_HIGH,
			    fnodetach);
	      (void) fsysdep_unlock_system (&sLocked_system);
	      fLocked_system = FALSE;
	    }

#if DEBUG > 1
	  iDebug = iholddebug;
#endif

	  ulog_system ((const char *) NULL);
	}
      else
	{
	  int csystems;
	  struct ssysteminfo *pas;
	  int i;
	  char bgrade;
	  boolean fdidone;

	  fret = TRUE;
	  fdidone = FALSE;
	  uread_all_system_info (&csystems, &pas);
	  for (i = 0; i < csystems && ! FGOT_SIGNAL (); i++)
	    {
	      if (fsysdep_has_work (&pas[i], &bgrade))
		{
		  fdidone = TRUE;

		  /* Detach from the controlling terminal.  On Unix
		     this means that we will wind up forking a new
		     process for each system we call.  */
		  if (! fnodetach
		      && (qport == NULL
			  || qport->ttype != PORTTYPE_STDIN))
		    usysdep_detach ();

		  ulog_system (pas[i].zname);

#if DEBUG > 1
		  iholddebug = iDebug;
		  iDebug |= pas[i].idebug;
#endif

		  if (! fsysdep_lock_system (&pas[i]))
		    {
		      ulog (LOG_ERROR, "System already locked");
		      fret = FALSE;
		    }
		  else
		    {
		      sLocked_system = pas[i];
		      fLocked_system = TRUE;
		      if (! fcall (&pas[i], qport, fforce, bgrade,
				   fnodetach))
			fret = FALSE;

		      /* Now ignore any SIGHUP that we got.  */
		      afSignal[INDEXSIG_SIGHUP] = FALSE;

		      (void) fsysdep_unlock_system (&pas[i]);
		      fLocked_system = FALSE;
		    }

#if DEBUG > 1
		  iDebug = iholddebug;
#endif

		  ulog_system ((const char *) NULL);
		}
	    }

	  if (! fdidone)
	    ulog (LOG_NORMAL, "No work");
	}

      /* If requested, wait for calls after dialing out.  */
      if (fwait)
	{
	  floop = TRUE;
	  fmaster = FALSE;
	}
    }

  if (! fmaster)
    {
      /* If a port was specified by name, we go into endless loop
	 mode.  In this mode, we wait for calls and prompt them with
	 "login:" and "Password:", so that they think we are a regular
	 UNIX system.  If we aren't in endless loop mode, we have been
	 called by some other system.  If flogin is TRUE, we prompt
	 with "login:" and "Password:" a single time.  */

      fret = TRUE;
      zsystem = NULL;

      if (qport != NULL)
	{
	  /* Detach from the controlling terminal, so that the port we
	     are about to use becomes our controlling terminal.  */
	  if (! fnodetach && qport->ttype != PORTTYPE_STDIN)	      
	    usysdep_detach ();

	  floop = TRUE;
	  if (! fport_lock (qport, TRUE))
	    {
	      ulog (LOG_ERROR, "Port %s is locked", qport->zname);
	      fret = FALSE;
	    }
	}

      if (fret)
	{
	  if (! fport_open (qport, (long) 0, (long) 0, TRUE))
	    fret = FALSE;
	}

      if (fret)
	{
	  if (floop)
	    {
	      while (! FGOT_SIGNAL () && flogin_prompt (qport))
		{
		  /* Now ignore any SIGHUP that we got.  */
		  afSignal[INDEXSIG_SIGHUP] = FALSE;

		  if (fLocked_system)
		    {
		      (void) fsysdep_unlock_system (&sLocked_system);
		      fLocked_system = FALSE;
		    }
		  if (! fport_reset ())
		    break;
		}
	      fret = FALSE;
	    }
	  else
	    {
	      if (flogin)
		fret = flogin_prompt (qport);
	      else
		{
		  const struct ssysteminfo *qsys;

#if DEBUG > 1
		  iholddebug = iDebug;
#endif
		  fret = faccept_call (zsysdep_login_name (), qport,
				       &qsys);
		  if (qsys != NULL)
		    zsystem = qsys->zname;
#if DEBUG > 1
		  iDebug = iholddebug;
#endif
		}
	    }

	  (void) fport_close (fret);

	  if (fLocked_system)
	    {
	      (void) fsysdep_unlock_system (&sLocked_system);
	      fLocked_system = FALSE;
	    }
	}
    }

  ulog_close ();
  ustats_close ();

  /* If we got a SIGTERM, perhaps because the system is going down,
     don't run uuxqt.  We go ahead and run it for any other signal,
     since I think they indicate more temporary conditions.  */
  if (afSignal[INDEXSIG_SIGTERM])
    fuuxqt = FALSE;

  if (fuuxqt)
    {
      /* Detach from the controlling terminal before starting up uuxqt,
	 so that it runs as a true daemon.  */
      if (! fnodetach)
	usysdep_detach ();
      if (zsystem == NULL)
	fret = fsysdep_run (FALSE, "uuxqt", (const char *) NULL,
			    (const char *) NULL);
      else
	fret = fsysdep_run (FALSE, "uuxqt", "-s", zsystem);
    }

  usysdep_exit (fret);

  /* Avoid complaints about not returning.  */
  return 0;
}

/* Print out a usage message.  */

static void
uusage ()
{
  fprintf (stderr,
	   "Taylor UUCP version %s, copyright (C) 1991, 1992 Ian Lance Taylor\n",
	   abVersion);
  fprintf (stderr,
	   "Usage: uucico [options]\n");
  fprintf (stderr,
	   " -s,-S system: Call system (-S implies -f)\n");
  fprintf (stderr,
	   " -f: Force call despite system status\n");
  fprintf (stderr,
	   " -r state: 1 for master, 0 for slave (default)\n");
  fprintf (stderr,
	   " -p port: Specify port (implies -e)\n");
  fprintf (stderr,
	   " -l: prompt for login name and password\n");
  fprintf (stderr,
	   " -e: Endless loop of login prompts and daemon execution\n");
  fprintf (stderr,
	   " -w: After calling out, wait for incoming calls\n");
  fprintf (stderr,
	   " -q: Don't start uuxqt when done\n");
  fprintf (stderr,
	   " -x,-X debug: Set debugging level\n");
#if HAVE_TAYLOR_CONFIG
  fprintf (stderr,
	   " -I file: Set configuration file to use (default %s%s)\n",
	   NEWCONFIGLIB, CONFIGFILE);
#endif /* HAVE_TAYLOR_CONFIG */

  exit (EXIT_FAILURE);
}

/* This function is called when a LOG_FATAL error occurs.  */

static void
uabort ()
{
  ustats_failed ();

#if ! HAVE_BNU_LOGGING
  /* When using BNU logging, it's a pain to have no system name.  */
  ulog_system ((const char *) NULL);
#endif

  ulog_user ((const char *) NULL);

  if (qPort != NULL)
    (void) fport_close (FALSE);

  if (fLocked_system)
    {
      (void) fsysdep_unlock_system (&sLocked_system);
      fLocked_system = FALSE;
    }

  ulog_close ();
  ustats_close ();

  usysdep_exit (FALSE);
}

/* Call another system, trying all the possible sets of calling
   instructions.  The fprepare_call function should already have been
   called.  The qsys argument is the system to call.  The qport
   argument is the port to use, and may be NULL.  If the fforce
   argument is TRUE, a call is forced even if not enough time has
   passed since the last failed call.  The bgrade argument is the
   highest grade of work to be done for the system.  The qstat
   argument holds the status of the system.  */

static boolean fcall (qsys, qport, fforce, bgrade, fnodetach)
     const struct ssysteminfo *qsys;
     struct sport *qport;
     boolean fforce;
     int bgrade;
     boolean fnodetach;
{
  boolean fbadtime, fnevertime;
  const struct ssysteminfo *qorigsys;
  struct sstatus sstat;

  qorigsys = qsys;
  if (! fsysdep_get_status (qorigsys, &sstat))
    return FALSE;

  /* Make sure it's been long enough since the last failed call.  */
  if (! fforce)
    {
#ifdef CMAXRETRIES
#if CMAXRETRIES > 0
      if (sstat.cretries >= CMAXRETRIES)
	{
	  ulog (LOG_ERROR, "Too many retries");
	  return FALSE;
	}
#endif /* CMAXRETRIES > 0 */
#endif /* defined (CMAXRETRIES) */

      if (sstat.ttype != STATUS_COMPLETE
	  && sstat.ilast + sstat.cwait > isysdep_time ((long *) NULL))
	{
	  ulog (LOG_NORMAL, "Retry time not reached");
	  return FALSE;
	}
    }

  fbadtime = TRUE;
  fnevertime = TRUE;

  do
    {
      struct sspan *qtime;
      const struct ssysteminfo *qnext;

      qtime = qtimegrade_parse (qsys->ztime);
      if (qtime != NULL)
	{
	  long ival;
	  int cretry;
	  boolean fmatch;

	  fnevertime = FALSE;

	  /* The value returned in ival by fspan_match is the lowest
	     grade which may be done at this time.  */

	  fmatch = (ftimespan_match (qtime, &ival, &cretry)
		    && igradecmp (bgrade, (int) ival) <= 0);

	  utimespan_free (qtime);

	  if (fmatch)
	    {
	      boolean fret, fcalled;
	      struct sport sportinfo;
	  
	      if (FGOT_SIGNAL ())
		return FALSE;

	      fbadtime = FALSE;

	      fret = fdo_call (qsys, qport, &sstat, cretry, &fcalled,
			       &sportinfo);
	      (void) fport_close (fret);

	      if (fret)
		return TRUE;
	      if (fcalled)
		return FALSE;

	      /* Now we have to dump that port so that we can aquire a
		 new one.  */
	      if (! fnodetach)
		usysdep_detach ();
	    }
	}

      /* Look for the next alternate with different calling
	 instructions.  */
      qnext = qsys;
      do
	{
	  qnext = qnext->qalternate;
	}
      while (qnext != NULL
	     && qsys->ztime == qnext->ztime
	     && qsys->zport == qnext->zport
	     && qsys->qport == qnext->qport
	     && qsys->ibaud == qnext->ibaud
	     && qsys->zphone == qnext->zphone
	     && qsys->schat.zprogram == qnext->schat.zprogram
	     && qsys->schat.zchat == qnext->schat.zchat);

      qsys = qnext;
    }
  while (qsys != NULL);

  if (fbadtime)
    {
      ulog (LOG_ERROR, "Wrong time to call");

      /* Update the status, unless the system can never be called.  If
	 the system can never be called, there is little point to
	 putting in a ``wrong time to call'' message.  We don't change
	 the number of retries, although we do set the wait until the
	 next retry to 0.  */
      if (! fnevertime)
	{
	  sstat.ttype = STATUS_WRONG_TIME;
	  sstat.ilast = isysdep_time ((long *) NULL);
	  sstat.cwait = 0;
	  (void) fsysdep_set_status (qorigsys, &sstat);
	}
    }

  return FALSE;
}

/* Do the actual work of calling another system, such as dialing and
   logging in.  The qsys argument is the system to call, the qport
   argument is the port to use, and the qstat argument holds the
   current status of the ssystem.  If we log in successfully, set
   *pfcalled to TRUE; this is used to distinguish a failed dial from a
   failure during the call.  The quse argument is passed in because
   this function does not call fport_close, so if it reads in a port
   structure to open it must not keep it on the stack.  */

static boolean fdo_call (qsys, qport, qstat, cretry, pfcalled, quse)
     const struct ssysteminfo *qsys;
     struct sport *qport;
     struct sstatus *qstat;
     int cretry;
     boolean *pfcalled;
     struct sport *quse;
{
  const char *zstr;
  boolean fnew;
  int cdial_proto_params;
  struct sproto_param *qdial_proto_params;
  int idial_reliable;
  long istart_time;

  *pfcalled = FALSE;

  /* If no port was specified on the command line, use any port
     defined for the system.  To select the system port: 1) see if
     port information was specified directly; 2) see if a port was
     named; 3) get an available port given the baud rate.  We don't
     change the system status if a port is unavailable; i.e. we don't
     force the system to wait for the retry time.  */

  if (qport == NULL)
    qport = qsys->qport;
  if (qport != NULL)
    {
      if (! fport_lock (qport, FALSE))
	{
	  ulog (LOG_ERROR, "Port \"%s\" already locked", qport->zname);
	  return FALSE;
	}
    }
  else
    {
      if (! ffind_port (qsys->zport, qsys->ibaud, qsys->ihighbaud,
			quse, fport_lock, TRUE))
	return FALSE;
      qport = quse;
      /* The port is locked by ffind_port.  */
    }

  /* Now try to call the system.  */

  if (! fport_open (qport, qsys->ibaud, qsys->ihighbaud, FALSE))
    {
      (void) fcall_failed (qsys, STATUS_PORT_FAILED, qstat, cretry);
      return FALSE;
    }

  if (qsys->zalternate == NULL)
    ulog (LOG_NORMAL, "Calling system %s (port %s)", qsys->zname,
	  zLdevice == NULL ? "unknown" : zLdevice);
  else
    ulog (LOG_NORMAL, "Calling system %s (alternate %s, port %s)",
	  qsys->zname, qsys->zalternate,
	  zLdevice == NULL ? "unknown" : zLdevice);

  cdial_proto_params = 0;
  qdial_proto_params = NULL;
  if (! fport_dial (qsys, &cdial_proto_params, &qdial_proto_params,
		    &idial_reliable))
    {
      (void) fcall_failed (qsys, STATUS_DIAL_FAILED, qstat, cretry);
      return FALSE;
    }

  if (! fchat (&qsys->schat, qsys, (const struct sdialer *) NULL,
	       (const char *) NULL, FALSE, qPort->zname, iport_baud ()))
    {
      (void) fcall_failed (qsys, STATUS_LOGIN_FAILED, qstat, cretry);
      return FALSE;
    }

  qstat->ttype = STATUS_TALKING;
  qstat->ilast = isysdep_time ((long *) NULL);
  qstat->cretries = 0;
  qstat->cwait = 0;
  if (! fsysdep_set_status (qsys, qstat))
    return FALSE;

  ulog (LOG_NORMAL, "Login successful");

  *pfcalled = TRUE;
  istart_time = isysdep_time ((long *) NULL);

  /* We should now see "Shere" from the other system.  Apparently
     some systems send "Shere=foo" where foo is the remote name.  */

  zstr = zget_uucp_cmd (TRUE);
  if (zstr == NULL)
    {
      (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
      return FALSE;
    }

  if (strncmp (zstr, "Shere", 5) != 0)
    {
      (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
      ulog (LOG_ERROR, "Bad initialization string");
      return FALSE;
    }

  if (zstr[5] == '=')
    {
      const char *zheresys;
      int icmp;

      /* Some UUCP packages only provide seven characters in the Shere
	 machine name.  */
      zheresys = zstr + 6;
      if (strlen (zheresys) == 7)
	icmp = strncmp (zheresys, qsys->zname, 7);
      else
	icmp = strcmp (zheresys, qsys->zname);
      if (icmp != 0)
	{
	  (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat,
			       cretry);
	  ulog (LOG_ERROR, "Called wrong system (%s)", zheresys);
	  return FALSE;
	}
    }
#if DEBUG > 1
  else if (zstr[5] != '\0')
    DEBUG_MESSAGE1 (DEBUG_HANDSHAKE,
		    "fdo_call: Strange Shere: %s", zstr);
#endif

  /* We now send "S" name switches, where name is our UUCP name.  If
     we are using sequence numbers with this system, we send a -Q
     argument with the sequence number.  If the call-timegrade command
     was used, we send a -p argument and a -vgrade= argument with the
     grade to send us (we send both argument to make it more likely
     that one is recognized).  We always send a -N (for new) switch to
     indicate that we are prepared to accept file sizes.  */
  {
    char bgrade;
    const char *zuse_local;
    char *zsend;

    /* Determine the grade we should request of the other system.  A
       '\0' means that no restrictions have been made.  */
    bgrade = btimegrade (qsys->zcalltimegrade);

    if (qsys->zlocalname != NULL)
      zuse_local = qsys->zlocalname;
    else
      zuse_local = zLocalname;

    zsend = (char *) alloca (strlen (zuse_local) + 70);
    if (! qsys->fsequence)
      {
	if (bgrade == '\0')
	  sprintf (zsend, "S%s -N", zuse_local);
	else
	  sprintf (zsend, "S%s -p%c -vgrade=%c -N", zuse_local, bgrade,
		   bgrade);
      }
    else
      {
	long iseq;

	iseq = isysdep_get_sequence (qsys);
	if (iseq < 0)
	  {
	    (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat,
				 cretry);
	    return FALSE;
	  }
	if (bgrade == '\0')
	  sprintf (zsend, "S%s -Q%ld -N", zuse_local, iseq);
	else
	  sprintf (zsend, "S%s -Q%ld -p%c -vgrade=%c -N", zuse_local, iseq,
		   bgrade, bgrade);
      }

    if (! fsend_uucp_cmd (zsend))
      {
	(void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat,
			     cretry);
	return FALSE;
      }
  }

  /* Now we should see ROK or Rreason where reason gives a cryptic
     reason for failure.  If we are talking to a counterpart, we will
     get back ROKN.  */
  zstr = zget_uucp_cmd (TRUE);
  if (zstr == NULL)
    {
      (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
      return FALSE;
    }

  if (zstr[0] != 'R')
    {
      (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
      ulog (LOG_ERROR, "Bad reponse to handshake string (%s)",
	    zstr);
      return FALSE;
    }

  if (strcmp (zstr + 1, "OKN") == 0)
    fnew = TRUE;
  else if (strcmp (zstr + 1, "OK") == 0)
    fnew = FALSE;
  else if (strcmp (zstr + 1, "CB") == 0)
    {
      ulog (LOG_NORMAL, "Remote system will call back");
      qstat->ttype = STATUS_COMPLETE;
      (void) fsysdep_set_status (qsys, qstat);
      return TRUE;
    }
  else
    {
      (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
      ulog (LOG_ERROR, "Handshake failed (%s)", zstr + 1);
      return FALSE;
    }

  /* The slave should now send \020Pprotos\0 where protos is a list of
     supported protocols.  Each protocol is a single character.  */

  zstr = zget_uucp_cmd (TRUE);
  if (zstr == NULL)
    {
      (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
      return FALSE;
    }

  if (zstr[0] != 'P')
    {
      (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
      ulog (LOG_ERROR, "Bad protocol handshake (%s)", zstr);
      return FALSE;
    }

  /* Now decide which protocol to use.  The system and the port may
     have their own list of protocols.  */
  {
    int i;
    char ab[5];

    i = CPROTOCOLS;
    if (qsys->zprotocols != NULL || qPort->zprotocols != NULL)
      {
	const char *zproto;

	if (qsys->zprotocols != NULL)
	  zproto = qsys->zprotocols;
	else
	  zproto = qPort->zprotocols;
	for (; *zproto != '\0'; zproto++)
	  {
	    if (strchr (zstr + 1, *zproto) != NULL)
	      {
		for (i = 0; i < CPROTOCOLS; i++)
		  if (asProtocols[i].bname == *zproto)
		    break;
		if (i < CPROTOCOLS)
		  break;
	      }
	  }
      }
    else
      {
	int ir;

	/* If neither the system nor the port specified a list of
	   protocols, we want only protocols that match the known
	   reliability of the dialer and the port.  If we have no
	   reliability information, we default to a reliable eight bit
	   connection.  */

	ir = 0;
	if ((qPort->ireliable & RELIABLE_SPECIFIED) != 0)
	  ir = qPort->ireliable;
	if ((idial_reliable & RELIABLE_SPECIFIED) != 0)
	  {
	    if (ir != 0)
	      ir &= idial_reliable;
	    else
	      ir = idial_reliable;
	  }
	if (ir == 0)
	  ir = RELIABLE_RELIABLE | RELIABLE_EIGHT | RELIABLE_SPECIFIED;

	for (i = 0; i < CPROTOCOLS; i++)
	  {
	    int ipr;

	    ipr = asProtocols[i].ireliable;
	    if ((ipr & ir) != ipr)
	      continue;
	    if (strchr (zstr + 1, asProtocols[i].bname) != NULL)
	      break;
	  }
      }

    if (i >= CPROTOCOLS)
      {
	(void) fsend_uucp_cmd ("UN");
	(void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
	ulog (LOG_ERROR, "No mutually supported protocols");
	return FALSE;
      }

    qProto = &asProtocols[i];

    sprintf (ab, "U%c", qProto->bname);
    if (! fsend_uucp_cmd (ab))
      {
	(void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat, cretry);
	return FALSE;
      }
  }

  /* Run any protocol parameter commands.  */

  if (qProto->qcmds != NULL)
    {
      if (qsys->cproto_params != 0)
	uapply_proto_params (qProto->bname, qProto->qcmds,
			     qsys->cproto_params, qsys->qproto_params);
      if (qPort->cproto_params != 0)
	uapply_proto_params (qProto->bname, qProto->qcmds,
			     qPort->cproto_params, qPort->qproto_params);
      if (cdial_proto_params != 0)
	uapply_proto_params (qProto->bname, qProto->qcmds,
			     cdial_proto_params, qdial_proto_params);
    }

  /* Turn on the selected protocol.  */

  if (! (*qProto->pfstart) (TRUE))
    {
      (void) fcall_failed (qsys, STATUS_HANDSHAKE_FAILED, qstat,
			   cretry);
      return FALSE;
    }

  /* Now we have succesfully logged in as the master.  */

  ulog (LOG_NORMAL, "Handshake successful");

  {
    boolean fret;
    long iend_time;

    fret = fuucp (TRUE, qsys, '\0', fnew, (long) -1);
    ulog_user ((const char *) NULL);
    usysdep_get_work_free (qsys);

    /* If we jumped out due to an error, shutdown the protocol.  */
    if (! fret)
      {
	(void) (*qProto->pfshutdown) ();
	ustats_failed ();
      }

    /* Now send the hangup message.  As the caller, we send six O's
       and expect to receive seven O's.  We send the six O's twice
       to help the other side.  We don't worry about errors here.  */
    if (fsend_uucp_cmd ("OOOOOO")
	&& fsend_uucp_cmd ("OOOOOO"))
      {
	/* We don't even look for the hangup string from the other
	   side unless we're in debugging mode.  */
#if DEBUG > 1
	if (fret && FDEBUGGING (DEBUG_HANDSHAKE))
	  {
	    zstr = zget_uucp_cmd (FALSE);
	    if (zstr != NULL)
	      {
		/* The Ultrix UUCP only sends six O's, although I
		   think it should send seven.  Because of this, we
		   only check for six.  */
		if (strstr (zstr, "OOOOOO") == NULL)
		  ulog (LOG_DEBUG, "No hangup from remote");
	      }
	  }
#endif
      }

    iend_time = isysdep_time ((long *) NULL);

    ulog (LOG_NORMAL, "Call complete (%ld seconds)",
	  iend_time - istart_time);

    if (! fret)
      {
	(void) fcall_failed (qsys, STATUS_FAILED, qstat, cretry);
	return FALSE;
      }
    else
      {
	qstat->ttype = STATUS_COMPLETE;
	qstat->ilast = iend_time;
	(void) fsysdep_set_status (qsys, qstat);
	return TRUE;
      }
  }
}

/* A small helper routine to write out the system status when something
   goes wrong.  */

static boolean
fcall_failed (qsys, twhy, qstat, cretry)
     const struct ssysteminfo *qsys;
     enum tstatus_type twhy;
     struct sstatus *qstat;
     int cretry;
{
  DEBUG_MESSAGE2 (DEBUG_HANDSHAKE,
		  "fcall_failed: Cause %d (%s)", (int) twhy,
		  azStatus[(int) twhy]);

  qstat->ttype = twhy;
  qstat->cretries++;
  qstat->ilast = isysdep_time ((long *) NULL);
  if (cretry == 0)
    qstat->cwait = CRETRY_WAIT (qstat->cretries);
  else
    qstat->cwait = cretry * 60;
  return fsysdep_set_status (qsys, qstat);
}

/* Prompt for a login name and a password, and run as the slave.  */

static boolean flogin_prompt (qport)
     struct sport *qport;
{
  const char *zuser, *zpass;

  DEBUG_MESSAGE0 (DEBUG_HANDSHAKE, "flogin_prompt: Waiting for login");

  do
    {
      if (! fport_write ("login: ", sizeof "login: " - 1))
	return FALSE;
      zuser = zget_typed_line ();
    }
  while (zuser != NULL && *zuser == '\0');

  if (zuser != NULL)
    {
      char *zhold;

      zhold = (char *) alloca (strlen (zuser) + 1);
      strcpy (zhold, zuser);

      if (! fport_write ("Password:", sizeof "Password:" - 1))
	return FALSE;

      zpass = zget_typed_line ();
      if (zpass != NULL)
	{
	  if (fcheck_login (zhold, zpass))
	    {
#if DEBUG > 1
	      int iholddebug;
#endif

	      /* We ignore the return value of faccept_call because we
		 really don't care whether the call succeeded or not.
		 We are going to reset the port anyhow.  */
#if DEBUG > 1
	      iholddebug = iDebug;
#endif
	      (void) faccept_call (zhold, qport,
				   (const struct ssysteminfo **) NULL);
#if DEBUG > 1
	      iDebug = iholddebug;
#endif
	    }
	}
    }

  return TRUE;
}

/* Accept a call from a remote system.  If pqsys is not NULL, *pqsys
   will be set to the system that called in if known.  */

static boolean
faccept_call (zlogin, qport, pqsys)
     const char *zlogin;
     struct sport *qport;
     const struct ssysteminfo **pqsys;
{
  long istart_time;
  int cdial_proto_params;
  struct sproto_param *qdial_proto_params;
  int idial_reliable;
  boolean ftcp_port;
  const char *zport;
  char *zsend, *zspace;
  const char *zstr;
  struct ssysteminfo ssys;
  const struct ssysteminfo *qsys;
  boolean fnew;
  char bgrade;
  const char *zuse_local;
  struct sstatus sstat;
  long cmax_receive;
  boolean frestart;
#if HAVE_TAYLOR_CONFIG
  struct sport sportinfo;
#endif

  if (pqsys != NULL)
    *pqsys = NULL;

  ulog (LOG_NORMAL, "Incoming call (login %s port %s)",
	zlogin == NULL ? "unknown" : zlogin,
	zLdevice == NULL ? "unknown" : zLdevice);

  istart_time = isysdep_time ((long *) NULL);

  /* Figure out protocol parameters determined by the port.  If no
     port was specified we're reading standard input, so try to get
     the port name and read information from the port file.  We only
     use the port information to get protocol parameters; we don't
     want to start treating the port as though it were a modem, for
     example.  */

  if (qport != NULL)
    {
      zport = qport->zname;
      ftcp_port = FALSE;
    }
  else
    {
      zport = zsysdep_port_name (&ftcp_port);

      /* We want to get the protocol parameters for the port.  If we
	 aren't using HAVE_TAYLOR_CONFIG, that information isn't
	 stored anyhow, so we don't bother to look it up.  */

#if HAVE_TAYLOR_CONFIG

      if (zport != NULL && zPortfile != NULL)
	{
	  if (ffind_port (zport, (long) 0, (long) 0, &sportinfo,
			  (boolean (*) P((struct sport *, boolean))) NULL,
			  FALSE))
	    qport = &sportinfo;
	}

#endif /* HAVE_TAYLOR_CONFIG */

      if (zport == NULL)
	zport = "unknown";
    }

  /* If we've managed to figure out that this is a modem port, now try
     to get protocol parameters from the dialer.  */

  cdial_proto_params = 0;
  qdial_proto_params = NULL;
  idial_reliable = 0;
  if (qport != NULL)
    {
      if (qport->ttype == PORTTYPE_MODEM)
	{
	  if (qport->u.smodem.zdialer != NULL)
	    {
	      char *zcopy;
	      char *zdial;
	      struct sdialer sdialerinfo;

	      /* We use the first dialer in the sequence.  */
	      zcopy = (char *) alloca (strlen (qport->u.smodem.zdialer)
				       + 1);
	      strcpy (zcopy, qport->u.smodem.zdialer);

	      zdial = strtok (zcopy, " \t");
	      if (fread_dialer_info (zdial, &sdialerinfo))
		{
		  cdial_proto_params = sdialerinfo.cproto_params;
		  qdial_proto_params = sdialerinfo.qproto_params;
		  idial_reliable = sdialerinfo.ireliable;
		}
	    }
	  else if (qport->u.smodem.qdialer != NULL)
	    {
	      cdial_proto_params = qport->u.smodem.qdialer->cproto_params;
	      qdial_proto_params = qport->u.smodem.qdialer->qproto_params;
	      idial_reliable = qport->u.smodem.qdialer->ireliable;
	    }
	}	  
#if HAVE_TCP
      else if (qport->ttype == PORTTYPE_TCP)
	ftcp_port = TRUE;
#endif
    }

  /* If it's a TCP port, it's fully reliable.  Even if HAVE_TCP is not
     supported, zsysdep_port_name may be able to figure this out (not
     on Unix, though).  */
  if (ftcp_port)
    idial_reliable = (RELIABLE_SPECIFIED | RELIABLE_ENDTOEND
		      | RELIABLE_RELIABLE | RELIABLE_EIGHT);

  /* We have to check to see whether some system uses this login name
     to indicate a different local name.  Obviously, this means that
     any system which uses this login name must expect the alternate
     system name.  */
  zuse_local = NULL;
  if (fUnknown_ok)
    {
      for (qsys = &sUnknown; qsys != NULL;  qsys = qsys->qalternate)
	{
	  if (qsys->zlocalname != NULL
	      && qsys->zcalled_login != NULL
	      && strcmp (qsys->zcalled_login, zlogin) == 0)
	    {
	      zuse_local = qsys->zlocalname;
	      break;
	    }
	}
    }
  if (zuse_local == NULL)
    {
      struct ssysteminfo *pas;
      int isys, csystems;

      zuse_local = zLocalname;

      uread_all_system_info (&csystems, &pas);
      for (isys = 0; isys < csystems; isys++)
	{
	  for (qsys = &pas[isys]; qsys != NULL; qsys = qsys->qalternate)
	    {
	      if (qsys->zlocalname != NULL
		  && qsys->zcalled_login != NULL
		  && strcmp (qsys->zcalled_login, zlogin) == 0)
		{
		  zuse_local = qsys->zlocalname;
		  break;
		}
	    }
	  if (qsys != NULL)
	    break;
	}
    }

  /* Tell the remote system who we are.   */
  zsend = (char *) alloca (strlen (zuse_local) + 10);
  sprintf (zsend, "Shere=%s", zuse_local);
  if (! fsend_uucp_cmd (zsend))
    return FALSE;

  zstr = zget_uucp_cmd (TRUE);
  if (zstr == NULL)
    return FALSE;

  if (zstr[0] != 'S')
    {
      ulog (LOG_ERROR, "Bad introduction string");
      return FALSE;
    }
  ++zstr;

  zspace = strchr (zstr, ' ');
  if (zspace != NULL)
    *zspace = '\0';
  if (fread_system_info (zstr, &ssys))
    qsys = &ssys;
  else
    {
      /* We have no information on this system.  */
      if (! fUnknown_ok)
	{
	  (void) fsend_uucp_cmd ("RYou are unknown to me");
	  ulog (LOG_ERROR, "Call from unknown system %s", zstr);
	  return FALSE;
	}

      /* We have to translate the name to a canonical form for the
	 benefit of systems which only allow short system names.  */
      sUnknown.zname = ztranslate_system (zstr);
      if (sUnknown.zname == NULL)
	{
	  (void) fsend_uucp_cmd ("RYou are unknown to me");
	  return FALSE;
	}

      qsys = &sUnknown;
    }

  if (pqsys != NULL)
    *pqsys = qsys;

  if (! fcheck_validate (zlogin, qsys->zname))
    {
      (void) fsend_uucp_cmd ("RLOGIN");
      ulog (LOG_ERROR, "System %s used wrong login name %s",
	    zstr, zlogin);
      return FALSE;
    }

  if (qsys->zcalled_login != NULL)
    {
      const struct ssysteminfo *qany;

      /* Choose an alternate system definition based on the
	 login name.  */
      qany = NULL;
      for (; qsys != NULL; qsys = qsys->qalternate)
	{
	  if (qsys->zcalled_login != NULL)
	    {
	      if (qany == NULL
		  && strcmp (qsys->zcalled_login, "ANY") == 0)
		qany = qsys;
	      else if (strcmp (qsys->zcalled_login, zlogin) == 0)
		break;
	    }
	}

      if (qsys == NULL)
	{
	  if (qany == NULL)
	    {
	      (void) fsend_uucp_cmd ("RLOGIN");
	      ulog (LOG_ERROR, "System %s used wrong login name %s",
		    zstr, zlogin);
	      return FALSE;
	    }
	  qsys = qany;
	}
    }

  ulog_system (qsys->zname);

#if DEBUG > 1
  iDebug |= qsys->idebug;
#endif

  /* See if we are supposed to call the system back.  This will queue
     up an empty command.  It would be better to actually call back
     directly at this point as well.  */
  if (qsys->fcallback)
    {
      (void) fsend_uucp_cmd ("RCB");
      ulog (LOG_NORMAL, "Will call back");
      (void) zsysdep_spool_commands (qsys, BGRADE_HIGH, 0,
				     (const struct scmd *) NULL);
      return TRUE;
    }

  /* We only permit one call at a time from a remote system.  Lock it.  */
  if (! fsysdep_lock_system (qsys))
    {
      (void) fsend_uucp_cmd ("RLCK");
      ulog (LOG_ERROR, "System already locked");
      return FALSE;
    }
  sLocked_system = *qsys;
  fLocked_system = TRUE;

  /* Set the system status.  We don't really care if we can't get the
     earlier status.  We also don't want to kill the conversation just
     because we can't output the .Status file, so we ignore any
     errors.  */
  if (! fsysdep_get_status (qsys, &sstat))
    {
      sstat.cretries = 0;
      sstat.cwait = 0;
    }

  sstat.ttype = STATUS_TALKING;
  sstat.ilast = isysdep_time ((long *) NULL);
  (void) fsysdep_set_status (qsys, &sstat);

  /* Check the arguments of the remote system.  We accept -x# to set
     our debugging level and -Q# for a sequence number.  We may insist
     on a sequence number.  The -p and -vgrade= arguments are taken to
     specify the lowest job grade that we should transfer; I think
     this is the traditional meaning, but I don't know.  The -N switch
     means that we are talking to another instance of ourselves.  The
     -U switch specifies the ulimit of the remote system, which we
     treat as the maximum file size that may be sent.  The -R switch
     means that the remote system supports file restart; we don't.  */

  fnew = FALSE;
  bgrade = BGRADE_LOW;
  cmax_receive = (long) -1;
  frestart = FALSE;

  if (zspace == NULL)
    {
      if (qsys->fsequence)
	{
	  (void) fsend_uucp_cmd ("RBADSEQ");
	  ulog (LOG_ERROR, "No sequence number (call rejected)");
	  sstat.ttype = STATUS_FAILED;
	  (void) fsysdep_set_status (qsys, &sstat);
	  return FALSE;
	}
    }
  else
    {
      ++zspace;
      while (isspace (BUCHAR (*zspace)))
	++zspace;

      while (*zspace != '\0')
	{
	  boolean frecognized;
	  char *znext;
	  
	  frecognized = FALSE;
	  if (*zspace == '-')
	    {
	      switch (zspace[1])
		{
		case 'x':
		  frecognized = TRUE;
#if DEBUG > 1
		  {
		    int iwant;

		    iwant = atoi (zspace + 2);
		    if (! fnew)
		      iwant = (1 << iwant) - 1;
		    iwant &= qsys->imax_remote_debug;
		    if ((iDebug | iwant) != iDebug)
		      {
			iDebug |= iwant;
			ulog (LOG_NORMAL, "Setting debugging mode to 0%o",
			      iDebug);
		      }
		  }
#endif
		  break;
		case 'Q':
		  frecognized = TRUE;
		  {
		    long iseq;

		    if (! qsys->fsequence)
		      break;
		    iseq = atol (zspace + 2);
		    if (iseq != isysdep_get_sequence (qsys))
		      {
			(void) fsend_uucp_cmd ("RBADSEQ");
			ulog (LOG_ERROR, "Out of sequence call rejected");
			sstat.ttype = STATUS_FAILED;
			(void) fsysdep_set_status (qsys, &sstat);
			return FALSE;
		      }
		  }
		  break;
		case 'p':
		  /* We don't accept a space between the -p and the
		     grade, although we should.  */
		  frecognized = TRUE;
		  if (FGRADE_LEGAL (zspace[2]))
		    bgrade = zspace[2];
		  break;
		case 'v':
		  if (strncmp (zspace + 1, "vgrade=",
			       sizeof "vgrade=" - 1) == 0)
		    {
		      frecognized = TRUE;
		      if (FGRADE_LEGAL (zspace[sizeof "vgrade="]))
			bgrade = zspace[sizeof "vgrade="];
		    }
		  break;
		case 'N':
		  frecognized = TRUE;
		  fnew = TRUE;
		  break;
		case 'U':
		  frecognized = TRUE;
		  {
		    long c;

		    c = strtol (zspace + 2, (char **) NULL, 0);
		    if (c > 0)
		      cmax_receive = c * (long) 512;
		  }
		  break;
		case 'R':
		  frecognized = TRUE;
		  frestart = TRUE;
		  break;
		default:
		  break;
		}
	    }

	  znext = zspace;
	  while (*znext != '\0' && ! isspace (BUCHAR (*znext)))
	    ++znext;

	  if (! frecognized)
	    {
	      int clen;
	      char *zcopy;

	      /* We could just use %.*s for this, but it's probably
		 not portable.  */
	      clen = znext - zspace;
	      zcopy = (char *) alloca (clen + 1);
	      strncpy (zcopy, zspace, clen);
	      zcopy[clen] = '\0';
	      ulog (LOG_NORMAL, "Unrecognized argument %s", zcopy);
	    }

	  zspace = znext;
	  while (isspace (BUCHAR (*zspace)))
	    ++zspace;
	}
    }

  /* We recognized the system, and the sequence number (if any) was
     OK.  Send an ROK, and send a list of protocols.  If we got the -N
     switch, send ROKN to confirm it.  */

  if (! fsend_uucp_cmd (fnew ? "ROKN" : "ROK"))
    {
      sstat.ttype = STATUS_FAILED;
      (void) fsysdep_set_status (qsys, &sstat);
      return FALSE;
    }

  {
    int i;
   
    if (qsys->zprotocols != NULL ||
	(qport != NULL && qport->zprotocols != NULL))
      {
	const char *zprotos;

	if (qsys->zprotocols != NULL)
	  zprotos = qsys->zprotocols;
	else
	  zprotos = qport->zprotocols;
	zsend = (char *) alloca (strlen (zprotos) + 2);
	sprintf (zsend, "P%s", zprotos);
      }
    else
      {
	char *zset;
	int ir;

	zsend = (char *) alloca (CPROTOCOLS + 2);
	zset = zsend;
	*zset++ = 'P';

	/* If the system did not specify a list of protocols, we want
	   only protocols that match the known reliability of the
	   dialer and the port.  If we have no information, we default
	   to a reliable eight bit connection.  */

	ir = 0;
	if (qport != NULL
	    && (qport->ireliable & RELIABLE_SPECIFIED) != 0)
	  ir = qport->ireliable;
	if ((idial_reliable & RELIABLE_SPECIFIED) != 0)
	  {
	    if (ir != 0)
	      ir &= idial_reliable;
	    else
	      ir = idial_reliable;
	  }
	if (ir == 0)
	  ir = RELIABLE_RELIABLE | RELIABLE_EIGHT | RELIABLE_SPECIFIED;

	for (i = 0; i < CPROTOCOLS; i++)
	  {
	    int ipr;

	    ipr = asProtocols[i].ireliable;
	    if ((ipr & ir) != ipr)
	      continue;
	    *zset++ = asProtocols[i].bname;
	  }
	*zset = '\0';
      }

    if (! fsend_uucp_cmd (zsend))
      {
	sstat.ttype = STATUS_FAILED;
	(void) fsysdep_set_status (qsys, &sstat);
	return FALSE;
      }
    
    /* The master will now send back the selected protocol.  */
    zstr = zget_uucp_cmd (TRUE);
    if (zstr == NULL)
      {
	sstat.ttype = STATUS_FAILED;
	(void) fsysdep_set_status (qsys, &sstat);
	return FALSE;
      }

    if (zstr[0] != 'U' || zstr[2] != '\0')
      {
	ulog (LOG_ERROR, "Bad protocol response string");
	sstat.ttype = STATUS_FAILED;
	(void) fsysdep_set_status (qsys, &sstat);
	return FALSE;
      }

    if (zstr[1] == 'N')
      {
	ulog (LOG_ERROR, "No supported protocol");
	sstat.ttype = STATUS_FAILED;
	(void) fsysdep_set_status (qsys, &sstat);
	return FALSE;
      }

    for (i = 0; i < CPROTOCOLS; i++)
      if (asProtocols[i].bname == zstr[1])
	break;

    if (i >= CPROTOCOLS)
      {
	ulog (LOG_ERROR, "No supported protocol");
	sstat.ttype = STATUS_FAILED;
	(void) fsysdep_set_status (qsys, &sstat);
	return FALSE;
      }

    qProto = &asProtocols[i];
  }

  /* Run the chat script for when a call is received.  */

  if (! fchat (&qsys->scalled_chat, qsys, (const struct sdialer *) NULL,
	       (const char *) NULL, FALSE, zport, iport_baud ()))
    {
      sstat.ttype = STATUS_FAILED;
      sstat.ilast = isysdep_time ((long *) NULL);
      (void) fsysdep_set_status (qsys, &sstat);
      return FALSE;
    }

  /* Run any protocol parameter commands.  There should be a way to
     read the dialer information if there is any to permit modem
     specific protocol parameters, but for now there isn't.  */
  
  if (qProto->qcmds != NULL)
    {
      if (qsys->cproto_params != 0)
	uapply_proto_params (qProto->bname, qProto->qcmds,
			     qsys->cproto_params, qsys->qproto_params);
      if (qport != NULL
	  && qport->cproto_params != 0)
	uapply_proto_params (qProto->bname, qProto->qcmds,
			     qport->cproto_params, qport->qproto_params);
      if (cdial_proto_params != 0)
	uapply_proto_params (qProto->bname, qProto->qcmds,
			     cdial_proto_params, qdial_proto_params);
    }

  /* Turn on the selected protocol.  */

  if (! (*qProto->pfstart)(FALSE))
    {
      sstat.ttype = STATUS_FAILED;
      sstat.ilast = isysdep_time ((long *) NULL);
      (void) fsysdep_set_status (qsys, &sstat);
      return FALSE;
    }

  /* If we using HAVE_BNU_LOGGING, then the previous ``incoming call''
     message went to the general log, since we didn't know the system
     name at that point.  In that case, we repeat the port and login
     names.  */
#if HAVE_BNU_LOGGING
  if (bgrade == BGRADE_LOW)
    ulog (LOG_NORMAL, "Handshake successful (login %s port %s)",
	  zlogin == NULL ? "unknown" : zlogin,
	  zLdevice == NULL ? "unknown" : zLdevice);
  else
    ulog (LOG_NORMAL, "Handshake successful (login %s port %s grade %c)",
	  zlogin == NULL ? "unknown" : zlogin,
	  zLdevice == NULL ? "unknown" : zLdevice,
	  bgrade);
#else /* ! HAVE_BNU_LOGGING */
  if (bgrade == BGRADE_LOW)
    ulog (LOG_NORMAL, "Handshake successful");
  else
    ulog (LOG_NORMAL, "Handshake successful (grade %c)", bgrade);
#endif /* ! HAVE_BNU_LOGGING */

  {
    boolean fret;
    long iend_time;

    fret = fuucp (FALSE, qsys, bgrade, fnew, cmax_receive);
    ulog_user ((const char *) NULL);
    usysdep_get_work_free (qsys);

    /* If we bombed out due to an error, shut down the protocol.  */
    if (! fret)
      {
	(void) (*qProto->pfshutdown) ();
	ustats_failed ();
      }

    /* Hangup.  As the answerer, we send seven O's and expect to see
       six.  */
    if (fsend_uucp_cmd ("OOOOOOO")
	&& fsend_uucp_cmd ("OOOOOOO"))
      {
	/* We don't even look for the hangup string from the other
	   side unless we're in debugging mode.  */
#if DEBUG > 1
	if (fret && FDEBUGGING (DEBUG_HANDSHAKE))
	  {
	    zstr = zget_uucp_cmd (FALSE);
	    if (zstr != NULL)
	      {
		if (strstr (zstr, "OOOOOO") == NULL)
		  ulog (LOG_DEBUG, "No hangup from remote");
	      }
	  }
#endif
      }

    iend_time = isysdep_time ((long *) NULL);

    ulog (LOG_NORMAL, "Call complete (%ld seconds)",
	  iend_time - istart_time);
    if (fret)
      sstat.ttype = STATUS_COMPLETE;
    else
      sstat.ttype = STATUS_FAILED;
    sstat.ilast = iend_time;
    (void) fsysdep_set_status (qsys, &sstat);
    return fret;
  }
}

/* This function runs the main UUCP protocol.  It is called when the
   two systems have succesfully connected.  It transfers files back
   and forth until neither system has any more work to do.  The
   traditional UUCP protocol has a master which sends files to the
   slave or requests files from the slave (a single file is requested
   with the R command; a wildcarded file name is requested with the X
   command).  The slave simply obeys the commands of the master.  When
   the master has done all its work, it requests a hangup.  If the
   slave has work to do it refuses the hangup and becomes the new
   master.

   This is essentially a half-duplex connection, in that files are
   only transferred in one direction at a time.  This is not
   unreasonable, since generally one site is receiving a lot of news
   from the other site, and I believe that Telebit modems are
   basically half-duplex in that it takes a comparatively long time to
   turn the line around.  However, it is possible to design a
   full-duplex protocol which would be useful in some situtations when
   using V.32 (or a network) and this function attempts to support
   this possibility.

   Traditionally the work to be done is kept in a set of files whose
   names begin with C.[system][grade][pid], where system is the remote
   system name, grade is the grade of transfer, and pid makes the file
   name unique.  Each line in these files is a command, and each line
   can be treated independently.  We let the system dependent layer
   handle all of this.  This will let us use some other scheme on
   systems in which the fourteen character filename length limit
   restricts the name of the remote system to seven characters (the
   usual restriction cited is six characters; I do not yet know where
   this comes from).

   Here are the types of commands, along with the definitions of the
   variables they use in the fuucp function.

   'S' -- Send a file from master to slave.
     zfrom -- master file name
     zto -- slave file name
     zuser -- user who requested the transfer
     zoptions -- list of options
     ztemp -- temporary file name on master (used unless option c)
     imode -- mode to give file
     znotify -- user to notify (if option n)

     The options are:
     C -- file copied to spool (use ztemp rather than zfrom)
     c -- file not copied to spool (use zfrom rather than ztemp)
     d -- create directories if necessary
     f -- do not create directories
     m -- notify originator (in zuser) when complete
     n -- notify recipient (in znotify) when complete

     I assume that the n option is implemented by the remote system.

   'R' -- Retrieve a file from slave to master.
     zfrom -- slave file name
     zto -- master file name
     zuser -- user who requested the transfer
     zoptions -- list of options

     The options are the same as in case 'S', except that option n is
     not supported.  If zto is a directory, we must create a file in
     that directory using the last component of zfrom.

   'X' -- Execute wildcard transfer from slave to master.
     zfrom -- wildcard file name
     zto -- local file (hopefully a directory)
     zuser -- user who requested the transfer
     zoptions -- list of options

     The options are presumably the same as in case 'R'.  It may be
     permissible to have no zuser or zoptions.  The zto name will have
     local! prepended to it already (where local is the local system
     name).

     This command is merely sent over to the remote system, where it
     is executed.  When the remote system becomes the master, it sends
     the files back.

   'H' -- Hangup
     This is used by the master to indicate a transfer of control.  If
     slave has nothing to do, it responds with HY and the conversation
     is finished.  Otherwise, the slave becomes the master, and
     vice-versa.  */

static boolean
fuucp (fmaster, qsys, bgrade, fnew, cmax_receive)
     boolean fmaster;
     const struct ssysteminfo *qsys;
     int bgrade;
     boolean fnew;
     long cmax_receive;
{
  boolean fcaller, fmasterdone, fnowork;
  const char *zlocal_size, *zremote_size;
  long clocal_size, cremote_size, cmax_ever;

  fcaller = fmaster;

  fmasterdone = FALSE;
  if (! qProto->ffullduplex && ! fmaster)
    fmasterdone = TRUE;

  /* Make sure we have a spool directory for this system.  */
  if (! fsysdep_make_spool_dir (qsys))
    return FALSE;

  /* If we are not the caller, the grade will be passed in as an
     argument.  If we are the caller, we compute the grade in this
     function so that we can recompute if time has passed.  */

  if (fcaller)
    bgrade = btimegrade (qsys->ztime);

  if (bgrade == '\0')
    fnowork = TRUE;
  else
    {
      if (! fsysdep_get_work_init (qsys, bgrade))
	return FALSE;
      fnowork = FALSE;
    }

  /* Determine the maximum sizes we can send and receive.  */

  if (fcaller)
    {
      zlocal_size = qsys->zcall_local_size;
      zremote_size = qsys->zcall_remote_size;
    }
  else
    {
      zlocal_size = qsys->zcalled_local_size;
      zremote_size = qsys->zcalled_remote_size;
    }

  clocal_size = cmax_size_now (zlocal_size);
  cremote_size = cmax_size_now (zremote_size);
  cmax_ever = (long) -2;

  /* Loop while we have local commands to execute and while we receive
     remote commands.  */

  while (TRUE)
    {
#if ! HAVE_ALLOCA
      /* This only works if we know that no caller of this function is
	 holding an alloca'ed pointer.  */
      (void) alloca (0);
#endif

#if DEBUG > 1
      /* If we're doing any debugging, close the log and debugging
	 files regularly.  This will let people copy them off and
	 remove them while the conversation is in progresss.  */
      if (iDebug != 0)
	{
	  ulog_close ();
	  ustats_close ();
	}
#endif

      /* We send a command to the remote system if
	 we are the master or
	 this is full duplex protocol which is ready for a command and
	 we haven't finished executing commands.  */
      if (fmaster ||
	  (qProto->ffullduplex && ! fmasterdone))
	{
	  struct scmd s;
	  const char *zmail, *zuse;
	  boolean fspool, fnever;
	  openfile_t e = EFILECLOSED;
	  boolean fgone;

	  /* Get the next work line for this system.  All the arguments
	     are left pointing into a static buffer, so they must be
	     copied out before the next call.  */
	  ulog_user ((const char *) NULL);
	  if (fnowork)
	    s.bcmd = 'H';
	  else
	    {
	      s.zuser = NULL;
	      if (! fsysdep_get_work (qsys, bgrade, &s))
		return FALSE;
	      ulog_user (s.zuser);
	    }

	  switch (s.bcmd)
	    {
	    case 'S':
	      /* Send a file.  */

	      fspool = fspool_file (s.zfrom);

	      if (! fspool)
		{
		  zuse = zsysdep_real_file_name (qsys, s.zfrom,
						 (const char *) NULL);
		  if (zuse == NULL)
		    {
		      (void) fmail_transfer (FALSE, s.zuser,
					     (const char *) NULL,
					     "cannot form file name",
					     s.zfrom, zLocalname,
					     s.zto, qsys->zname,
					     (const char *) NULL);
		      (void) fsysdep_did_work (s.pseq);
		      break;
		    }

		  /* The 'C' option means that the file has been
		     copied to the spool directory.  */
		  if (strchr (s.zoptions, 'C') != NULL)
		    fspool = TRUE;

		  if (! fok_to_send (zuse, TRUE, fcaller, fspool, qsys,
				     s.zuser))
		    {
		      ulog (LOG_ERROR, "Not permitted to send %s", zuse);
		      (void) fmail_transfer (FALSE, s.zuser,
					     (const char *) NULL,
					     "not permitted to send",
					     s.zfrom, zLocalname,
					     s.zto, qsys->zname,
					     (const char *) NULL);
		      (void) fsysdep_did_work (s.pseq);
		      break;
		    }

		  /* If we're copying the real file, use its mode
		     directly rather than the mode copied into the
		     command file.  */
		  if (! fspool)
		    e = esysdep_open_send (qsys, zuse, TRUE, s.zuser,
					   &s.imode, &s.cbytes, &fgone);
		}

	      if (fspool)
		{
		  unsigned int idummy;

		  zuse = zsysdep_spool_file_name (qsys, s.ztemp);
		  if (zuse == NULL)
		    {
		      (void) fmail_transfer (FALSE, s.zuser,
					     (const char *) NULL,
					     "cannot form file name",
					     s.zfrom, zLocalname,
					     s.zto, qsys->zname,
					     (const char *) NULL);
		      (void) fsysdep_did_work (s.pseq);
		      break;
		    }
		  e = esysdep_open_send (qsys, zuse, FALSE,
					 (const char *) NULL, &idummy,
					 &s.cbytes, &fgone);
		}

	      if (! ffileisopen (e))
		{
		  /* If the file does not exist, fgone will be set to
		     TRUE.  In this case we might have sent the file
		     the last time we talked to the remote system,
		     because we might have been interrupted in the
		     middle of a command file.  To avoid confusion, we
		     don't send a mail message.  */
		  if (! fgone)
		    (void) fmail_transfer (FALSE, s.zuser,
					   (const char *) NULL,
					   "cannot open file",
					   s.zfrom, zLocalname,
					   s.zto, qsys->zname,
					   (const char *) NULL);
		  (void) fsysdep_did_work (s.pseq);
		  break;
		}

	      if (s.cbytes != -1)
		{
		  boolean fsmall;
		  const char *zerr;

		  fsmall = FALSE;
		  fnever = FALSE;
		  zerr = NULL;

		  if (cmax_receive != -1 && cmax_receive < s.cbytes)
		    {
		      fsmall = TRUE;
		      fnever = TRUE;
		      zerr = "too large for receiver";
		    }
		  else if (clocal_size != -1 && clocal_size < s.cbytes)
		    {
		      fsmall = TRUE;

		      if (cmax_ever == -2)
			{
			  long c1, c2;

			  c1 = cmax_size_ever (qsys->zcall_local_size);
			  c2 = cmax_size_ever (qsys->zcalled_local_size);
			  if (c1 > c2)
			    cmax_ever = c1;
			  else
			    cmax_ever = c2;
			}
		      
		      if (cmax_ever == -1 || cmax_ever >= s.cbytes)
			zerr = "too large to send now";
		      else
			{
			  fnever = TRUE;
			  zerr = "too large to send";
			}
		    }

		  if (fsmall)
		    {
		      ulog (LOG_ERROR, "File %s is %s", s.zfrom, zerr);

		      if (fnever)
			{
			  const char *zsaved;

			  zsaved = zsysdep_save_temp_file (s.pseq);
			  (void) fmail_transfer (FALSE, s.zuser,
						 (const char *) NULL,
						 zerr,
						 s.zfrom, zLocalname,
						 s.zto, qsys->zname,
						 zsaved);
			  (void) fsysdep_did_work (s.pseq);
			}

		      (void) ffileclose (e);
		      break;
		    }
		}

	      ulog (LOG_NORMAL, "Sending %s", s.zfrom);

	      /* The send file function is responsible for notifying
		 the user upon success (if option m) or failure, and
		 for closing the file.  This allows it to not complete
		 immediately.  */
	      if (strchr (s.zoptions, 'm') == NULL)
		zmail = NULL;
	      else
		zmail = s.zuser;
				      
	      if (! fsend_file (TRUE, e, &s, zmail, qsys->zname, fnew))
		return FALSE;

	      break;

	    case 'R':
	      /* Receive a file.  */

	      if (fspool_file (s.zto))
		{
		  /* Normal users are not allowed to receive files in
		     the spool directory, and to make it particularly
		     difficult we require a special option '9'.  This
		     is used only by uux when a file must be requested
		     from one system and then sent to another.  */
		  if (s.zto[0] != 'D'
		      || strchr (s.zoptions, '9') == NULL)
		    {
		      ulog (LOG_ERROR, "Not permitted to receive %s",
			    s.zto);
		      (void) fmail_transfer (FALSE, s.zuser,
					     (const char *) NULL,
					     "not permitted to receive",
					     s.zfrom, qsys->zname,
					     s.zto, zLocalname,
					     (const char *) NULL);
		      (void) fsysdep_did_work (s.pseq);
		      break;
		    }

		  zuse = zsysdep_spool_file_name (qsys, s.zto);
		  if (zuse == NULL)
		    {
		      (void) fmail_transfer (FALSE, s.zuser,
					     (const char *) NULL,
					     "cannot form file name",
					     s.zfrom, qsys->zname,
					     s.zto, zLocalname,
					     (const char *) NULL);
		      (void) fsysdep_did_work (s.pseq);
		      break;
		    }
		}
	      else
		{
		  zuse = zsysdep_real_file_name (qsys, s.zto, s.zfrom);
		  if (zuse == NULL)
		    {
		      (void) fmail_transfer (FALSE, s.zuser,
					     (const char *) NULL,
					     "cannot form file name",
					     s.zfrom, qsys->zname,
					     s.zto, zLocalname,
					     (const char *) NULL);
		      (void) fsysdep_did_work (s.pseq);
		      break;
		    }

		  /* Check permissions.  */
		  if (! fok_to_receive (zuse, TRUE, fcaller, qsys, s.zuser))
		    {
		      ulog (LOG_ERROR, "Not permitted to receive %s",
			    s.zto);
		      (void) fmail_transfer (FALSE, s.zuser,
					     (const char *) NULL,
					     "not permitted to receive",
					     s.zfrom, qsys->zname,
					     s.zto, zLocalname,
					     (const char *) NULL);
		      (void) fsysdep_did_work (s.pseq);
		      break;
		    }

		  /* The 'f' option means that directories should not
		     be created if they do not already exist.  */
		  if (strchr (s.zoptions, 'f') == NULL)
		    {
		      if (! fsysdep_make_dirs (zuse, TRUE))
			{
			  (void) fmail_transfer (FALSE, s.zuser,
						 (const char *) NULL,
						 "cannot create directories",
						 s.zfrom, qsys->zname,
						 s.zto, zLocalname,
						 (const char *) NULL);
			  (void) fsysdep_did_work (s.pseq);
			  break;
			}
		    }
		}

	      e = esysdep_open_receive (qsys, zuse, &s.ztemp, &s.cbytes);
	      if (! ffileisopen (e))
		{
		  (void) fmail_transfer (FALSE, s.zuser,
					 (const char *) NULL,
					 "cannot open file",
					 s.zfrom, qsys->zname,
					 s.zto, zLocalname,
					 (const char *) NULL);
		  (void) fsysdep_did_work (s.pseq);
		  break;
		}

	      /* Here s.cbytes represents the amount of free space we
		 have.  We want to adjust it by the amount of free
		 space permitted for this system.  If there is a
		 maximum transfer size, we may want to use that as an
		 amount of free space.  */
	      if (s.cbytes != -1)
		{
		  s.cbytes -= qsys->cfree_space;
		  if (s.cbytes < 0)
		    s.cbytes = 0;
		}

	      if (clocal_size != -1
		  && (s.cbytes == -1 || clocal_size < s.cbytes))
		s.cbytes = clocal_size;

	      ulog (LOG_NORMAL, "Receiving %s", zuse);
	      s.zto = zuse;

	      /* As with the send routine, this function is
		 responsible for mailing a message to the user on
		 failure or on success if the m option is set, and is
		 also responsible for closing the file.  */
	      if (strchr (s.zoptions, 'm') == NULL)
		zmail = NULL;
	      else
		zmail = s.zuser;

	      /* The imode argument (passed as 0666) will be corrected
		 with information from the remote system.  */
	      s.imode = 0666;

	      if (! freceive_file (TRUE, e, &s, zmail, qsys->zname, fnew))
		return FALSE;

	      break;

	    case 'X':
	      /* Request a file copy.  This is used to request a file
		 to be sent to another machine, as well as to get a
		 wildcarded filespec.  */
	      ulog (LOG_NORMAL, "Requesting work: %s to %s", s.zfrom,
		    s.zto);

	      if (! fxcmd (&s, &fnever))
		return FALSE;

	      if (fnever)
		(void) fmail_transfer (FALSE, s.zuser,
				       (const char *) NULL,
				       "wildcard request denied",
				       s.zfrom, qsys->zname,
				       s.zto, zLocalname,
				       (const char *) NULL);
	      (void) fsysdep_did_work (s.pseq);
	      break;

	    case 'H':
	      /* There is nothing left to do; hang up.  If we are not the
		 master, take no action (this allows for two-way
		 protocols.  */

	      fmasterdone = TRUE;
	      if (fmaster)
		{
		  if (! fhangup_request ())
		    {
		      ulog (LOG_ERROR, "Hangup failed");
		      return FALSE;
		    }

		  fmaster = FALSE;

		  /* Close the log file at every master/slave switch.
		     This will cut down on the amount of time we have
		     an old log file open.  */
		  ulog_close ();
		  ustats_close ();
		}
	      break;

	    default:
	      ulog (LOG_ERROR, "Unknown command '%c'", s.bcmd);
	      break;
	    }
	}

      /* We look for a command from the other system if we are the
	 slave or this is a full-duplex protocol and the slave still
	 has work to do.  */
      if (! fmaster || qProto->ffullduplex)
	{
	  struct scmd s;
	  const char *zuse, *zmail;
	  openfile_t e;
	  char bhave_grade;
	  long cbytes;

	  /* We are the slave.  Get the next command from the other
	     system.  */
	  ulog_user ((const char *) NULL);
	  if (! fgetcmd (fmaster, &s))
	    return FALSE;

	  if (s.bcmd != 'H' && s.bcmd != 'Y')
	    ulog_user (s.zuser);

	  switch (s.bcmd)
	    {
	    case 'S':
	      /* The master wants to send a file to us.  */

	      if (fspool_file (s.zto))
		{
		  zuse = zsysdep_spool_file_name (qsys, s.zto);
		  /* We don't accept remote command files.  */
		  if (zuse == NULL || s.zto[0] == 'C')
		    {
		      if (! ftransfer_fail ('S', FAILURE_PERM))
			return FALSE;
		      break;
		    }
		}
	      else
		{
		  zuse = zsysdep_real_file_name (qsys, s.zto, s.zfrom);
		  if (zuse == NULL)
		    {
		      if (! ftransfer_fail ('S', FAILURE_PERM))
			return FALSE;
		      break;
		    }

		  /* Check permissions.  */
		  if (! fok_to_receive (zuse, FALSE, fcaller, qsys,
					s.zuser))
		    {
		      ulog (LOG_ERROR, "Not permitted to receive %s", zuse);
		      if (! ftransfer_fail ('S', FAILURE_PERM))
			return FALSE;
		      break;
		    }

		  if (strchr (s.zoptions, 'f') == NULL)
		    {
		      if (! fsysdep_make_dirs (zuse, TRUE))
			{
			  if (! ftransfer_fail ('S', FAILURE_PERM))
			    return FALSE;
			  break;
			}
		    }
		}

	      e = esysdep_open_receive (qsys, zuse, &s.ztemp, &cbytes);
	      if (! ffileisopen (e))
		{
		  if (! ftransfer_fail ('S', FAILURE_OPEN))
		    return FALSE;
		  break;
		}

	      /* Adjust the number of bytes we are prepared to receive
		 according to the amount of free space we are supposed
		 to leave available and the maximum file size we are
		 permitted to transfer.  */
	      if (cbytes != -1)
		{
		  cbytes -= qsys->cfree_space;
		  if (cbytes < 0)
		    cbytes = 0;
		}
	      
	      if (cremote_size != -1
		  && (cbytes == -1 || cremote_size < cbytes))
		cbytes = cremote_size;

	      /* If the number of bytes we are prepared to receive
		 is less than the file size, we must fail.  */

	      if (s.cbytes != -1
		  && cbytes != -1
		  && cbytes < s.cbytes)
		{
		  ulog (LOG_ERROR, "%s is too big to receive", zuse);
		  (void) ffileclose (e);
		  (void) remove (s.ztemp);
		  if (! ftransfer_fail ('S', FAILURE_SIZE))
		    return FALSE;
		  break;
		}

	      ulog (LOG_NORMAL, "Receiving %s", zuse);
	      s.zto = zuse;

	      if (strchr (s.zoptions, 'n') == NULL)
		zmail = NULL;
	      else
		zmail = s.znotify;
	      s.pseq = NULL;
	      if (! freceive_file (FALSE, e, &s, zmail, qsys->zname, fnew))
		return FALSE;

	      break;

	    case 'R':
	      /* The master wants to get a file from us.  */

	      if (fspool_file (s.zfrom))
		{
		  ulog (LOG_ERROR, "No permission to send %s", s.zfrom);
		  if (! ftransfer_fail ('R', FAILURE_PERM))
		    return FALSE;
		  break;
		}

	      zuse = zsysdep_real_file_name (qsys, s.zfrom,
					     (const char *) NULL);
	      if (zuse == NULL)
		{
		  if (! ftransfer_fail ('R', FAILURE_PERM))
		    return FALSE;
		  break;
		}

	      if (! fok_to_send (zuse, FALSE, fcaller, FALSE, qsys,
				 s.zuser))
		{
		  ulog (LOG_ERROR, "No permission to send %s", zuse);
		  if (! ftransfer_fail ('R', FAILURE_PERM))
		    return FALSE;
		  break;
		}

	      e = esysdep_open_send (qsys, zuse, TRUE, (const char *) NULL,
				     &s.imode, &cbytes, (boolean *) NULL);
	      if (! ffileisopen (e))
		{
		  if (! ftransfer_fail ('R', FAILURE_OPEN))
		    return FALSE;
		  break;
		}

	      /* If the file is larger than the amount of space
		 the other side reported, we can't send it.  */
	      if (cbytes != -1
		  && ((s.cbytes != -1 && s.cbytes < cbytes)
		      || (cremote_size != -1 && cremote_size < cbytes)
		      || (cmax_receive != -1 && cmax_receive < cbytes)))
		{
		  ulog (LOG_ERROR, "%s is too large to send", zuse);
		  if (! ftransfer_fail ('R', FAILURE_SIZE))
		    return FALSE;
		  (void) ffileclose (e);
		  break;
		}

	      ulog (LOG_NORMAL, "Sending %s", zuse);

	      /* Pass in the real size of the file.  */
	      s.cbytes = cbytes;

	      if (! fsend_file (FALSE, e, &s, (const char *) NULL,
				qsys->zname, fnew))
		return FALSE;

	      break;

	    case 'X':
	      /* This is an execution request.  We are being asked to
		 send one or more files to a destination on either the
		 local or a remote system.  We do this by spooling up
		 commands for the destination system.  */
	      ulog (LOG_NORMAL, "Work requested: %s to %s", s.zfrom,
		    s.zto);

	      if (fdo_xcmd (qsys, fcaller, &s))
		{
		  if (! fxcmd_confirm ())
		    return FALSE;
		}
	      else
		{
		  if (! ftransfer_fail ('X', FAILURE_PERM))
		    return FALSE;
		}

	      break;

	    case 'H':
	      /* The master wants to hang up.  If we have something to
		 do, become the master.  Otherwise, agree to hang up.
		 We recheck the grades allowed at this time, since a
		 lot of time may have passed.  */
	      if (fcaller)
		bgrade = btimegrade (qsys->ztime);
	      if (bgrade != '\0'
		  && fsysdep_has_work (qsys, &bhave_grade)
		  && igradecmp (bgrade, bhave_grade) >= 0)
		{
		  if (fmasterdone)
		    {
		      if (! fsysdep_get_work_init (qsys, bgrade))
			return FALSE;
		      fnowork = FALSE;
		    }

		  fmasterdone = FALSE;
		  
		  if (! fhangup_reply (FALSE))
		      return FALSE;
		  fmaster = TRUE;

		  /* Recalculate the maximum sizes we can send, since
		     the time might have changed significantly.  */
		  clocal_size = cmax_size_now (zlocal_size);
		  cremote_size = cmax_size_now (zremote_size);

		  /* Close the log file at every switch of master and
		     slave.  */
		  ulog_close ();
		  ustats_close ();
		}
	      else
		{
		  /* The hangup_reply function will shut down the
		     protocol.  */
		  return fhangup_reply (TRUE);
		}
	      break;

	    case 'Y':
	      /* This is returned when a hangup has been confirmed and
		 the protocol has been shut down.  */
	      return TRUE;

	    default:
	      ulog (LOG_ERROR, "Unknown command %c", s.bcmd);
	      break;
	    }
	}
    }
}

/* Do an 'X' request for another system.  The other system has
   basically requested us to execute a uucp command for them.  */

static boolean
fdo_xcmd (qsys, fcaller, q)
     const struct ssysteminfo *qsys;
     boolean fcaller;
     const struct scmd *q;
{
  const char *zexclam;
  const char *zdestfile;
  char *zcopy;
  struct ssysteminfo sdestsys;
  const struct ssysteminfo *qdestsys;
  char *zuser = NULL;
  char aboptions[5];
  char *zoptions = NULL;
  boolean fmkdirs;
  const char *zfile;

  zexclam = strchr (q->zto, '!');
  if (zexclam == NULL
      || zexclam == q->zto
      || strncmp (zLocalname, q->zto, zexclam - q->zto) == 0)
    {
      /* The files are supposed to be copied to the
	 local system.  */
      qdestsys = NULL;
      if (zexclam == NULL)
	zdestfile = q->zto;
      else
	zdestfile = zexclam + 1;
    }
  else
    {
      int clen;

      clen = zexclam - q->zto;
      zcopy = (char *) alloca (clen + 1);
      strncpy (zcopy, q->zto, clen);
      zcopy[clen] = '\0';

      if (! fread_system_info (zcopy, &sdestsys))
	{
	  if (! fUnknown_ok)
	    {
	      ulog (LOG_ERROR, "Destination system %s unknown",
		    zcopy);
	      return FALSE;
	    }
	  sdestsys = sUnknown;
	  sdestsys.zname = zcopy;
	}
      qdestsys = &sdestsys;
      zdestfile = zexclam + 1;
    }

  if (qdestsys != NULL)
    {
      zuser = (char *) alloca (strlen (qdestsys->zname)
			       + strlen (q->zuser) + sizeof "!");
      sprintf (zuser, "%s!%s", qdestsys->zname,
	       q->zuser);
      zoptions = aboptions;
      *zoptions++ = 'C';
      if (strchr (q->zoptions, 'd') != NULL)
	*zoptions++ = 'd';
      if (strchr (q->zoptions, 'm') != NULL)
	*zoptions++ = 'm';
      *zoptions = '\0';
      fmkdirs = TRUE;
    }
  else
    fmkdirs = strchr (q->zoptions, 'f') != NULL;

  /* Now we have to process each source file.  The
     source specification may or may use wildcards.  */
  if (! fsysdep_wildcard_start (qsys, q->zfrom))
    return FALSE;

  while ((zfile = zsysdep_wildcard (qsys, q->zfrom)) != NULL)
    {
      const char *zsend;
      const char *zto;
      char abtname[CFILE_NAME_LEN];

      zcopy = (char *) alloca (strlen (zfile) + 1);
      strcpy (zcopy, zfile);
      zfile = zcopy;

      /* Make sure the remote system is permitted to read the
	 specified file.  */
      zsend = qsys->zremote_send;
      if (! fcaller && qsys->zcalled_remote_send != NULL)
	zsend = qsys->zcalled_remote_send;

      if (! fin_directory_list (qsys, zfile, zsend, TRUE, TRUE,
				(const char *) NULL))
	{
	  ulog (LOG_ERROR, "Not permitted to send %s", zfile);
	  (void) fsysdep_wildcard_end ();
	  return FALSE;
	}

      if (qdestsys != NULL)
	{
	  /* We really should get the original grade here.  */
	  zto = zsysdep_data_file_name (qdestsys, BDEFAULT_UUCP_GRADE,
					abtname, (char *) NULL,
					(char *) NULL);
	}
      else
	{
	  const char *zrec;

	  zto = zsysdep_real_file_name (qsys, zexclam + 1, zfile);
	  if (zto == NULL)
	    {
	      (void) fsysdep_wildcard_end ();
	      return FALSE;
	    }
	  /* We only accept a local destination if the remote system
	     has the right to create files there.  */
	  zrec = qsys->zremote_receive;
	  if (! fcaller && qsys->zcalled_remote_receive != NULL)
	    zrec = qsys->zcalled_remote_receive;

	  if (! fin_directory_list (qsys, zto, zrec, TRUE, FALSE,
				    (const char *) NULL))
	    {
	      ulog (LOG_ERROR, "Not permitted to receive %s", zto);
	      (void) fsysdep_wildcard_end ();
	      return FALSE;
	    }
	}

      /* Copy the file either to the final destination or to the
	 spool directory.  */
      if (! fcopy_file (zfile, zto, qdestsys == NULL, fmkdirs))
	{
	  (void) fsysdep_wildcard_end ();
	  return FALSE;
	}

      /* If there is a destination system, queue it up.  */
      if (qdestsys != NULL)
	{
	  struct scmd ssend;

	  ssend.bcmd = 'S';
	  ssend.pseq = NULL;
	  ssend.zfrom = zfile;
	  ssend.zto = zdestfile;
	  ssend.zuser = zuser;
	  ssend.zoptions = aboptions;
	  ssend.ztemp = abtname;
	  ssend.imode = isysdep_file_mode (zfile);
	  if (ssend.imode == 0)
	    {
	      (void) fsysdep_wildcard_end ();
	      return FALSE;
	    }
	  ssend.znotify = "";
	  ssend.cbytes = -1;

	  if (zsysdep_spool_commands (qdestsys, BDEFAULT_UUCP_GRADE,
				      1, &ssend) == NULL)
	    {
	      (void) fsysdep_wildcard_end ();
	      return FALSE;
	    }
	}
    }

  if (! fsysdep_wildcard_end ())
    return FALSE;

  return TRUE;
}

/* See whether it's OK to send a file to another system, according to
   the permissions recorded for that system.  If the file is not in
   the spool directory, this also makes sure that the user has
   permission to access the file and all its containing directories.

   zfile -- file to send
   flocal -- TRUE if the send was requested locally
   fcaller -- TRUE if the local system called the other system
   fspool -- TRUE if file was copied to spool directory
   qsys -- remote system information
   zuser -- user who requested the action  */

static boolean
fok_to_send (zfile, flocal, fcaller, fspool, qsys, zuser)
     const char *zfile;
     boolean flocal;
     boolean fcaller;
     boolean fspool;
     const struct ssysteminfo *qsys;
     const char *zuser;
{
  const char *z;

  if (! frequest_ok (flocal, fcaller, qsys, zuser))
    return FALSE;

  if (flocal)
    {
      z = qsys->zlocal_send;
      if (! fcaller && qsys->zcalled_local_send != NULL)
	z = qsys->zcalled_local_send;
    }
  else
    {
      z = qsys->zremote_send;
      if (! fcaller && qsys->zcalled_remote_send != NULL)
	z = qsys->zcalled_remote_send;
    }

  /* If fspool is TRUE, we don't want to check file accessibility.  If
     this was not a local request, we pass a NULL down as the user
     name, since zuser has no meaning on this system.  */
  return fin_directory_list (qsys, zfile, z, ! fspool, TRUE,
			     flocal ? zuser : (const char *) NULL);
}

/* See whether it's OK to receive a file from another system.  */

/*ARGSUSED*/
static boolean
fok_to_receive (zto, flocal, fcaller, qsys, zuser)
     const char *zto;
     boolean flocal;
     boolean fcaller;
     const struct ssysteminfo *qsys;
     const char *zuser;
{
  const char *z;

  if (! frequest_ok (flocal, fcaller, qsys, zuser))
    return FALSE;

  if (flocal)
    {
      z = qsys->zlocal_receive;
      if (! fcaller && qsys->zcalled_local_receive != NULL)
	z = qsys->zcalled_local_receive;
    }
  else
    {
      z = qsys->zremote_receive;
      if (! fcaller && qsys->zcalled_remote_receive != NULL)
	z = qsys->zcalled_remote_receive;
    }

  return fin_directory_list (qsys, zto, z, TRUE, FALSE,
			     flocal ? zuser : (const char *) NULL);
}

/* See whether a request is OK.  This depends on which system placed
   the call and which system made the request.  */

/*ARGSUSED*/
static boolean
frequest_ok (flocal, fcaller, qsys, zuser)
     boolean flocal;
     boolean fcaller;
     const struct ssysteminfo *qsys;
     const char *zuser;
{
  if (flocal)
    {
      if (fcaller)
	return qsys->fcall_transfer;
      else
	return qsys->fcalled_transfer;
    }
  else
    {
      if (fcaller)
	return qsys->fcall_request;
      else
	return qsys->fcalled_request;
    }
}

/* Send a string to the other system beginning with a DLE
   character and terminated with a null byte.  This is only
   used when no protocol is in force.  */

static boolean
fsend_uucp_cmd (z)
     const char *z;
{
  char *zalc;
  int cwrite;

  cwrite = strlen (z) + 2;

  zalc = (char *) alloca (cwrite);
  sprintf (zalc, "\020%s", z);

  return fport_write (zalc, cwrite);
}

/* Get a UUCP command beginning with a DLE character and ending with a
   null byte.  This is only used when no protocol is in force.  This
   implementation has the potential of being seriously slow.  It also
   doesn't have any real error recovery.  The frequired argument is
   passed as TRUE if we need the string; we don't care that much if
   we're closing down the connection anyhow.  */

#define CTIMEOUT (120)
#define CSHORTTIMEOUT (10)
#define CINCREMENT (10)

static const char *
zget_uucp_cmd (frequired)
     boolean frequired;
{
  static char *zalc;
  static int calc;
  int cgot;
  long iendtime;
  int ctimeout;
#if DEBUG > 1
  int cchars;
  int iolddebug;
#endif

  iendtime = isysdep_time ((long *) NULL);
  if (frequired)
    iendtime += CTIMEOUT;
  else
    iendtime += CSHORTTIMEOUT;

#if DEBUG > 1
  cchars = 0;
  iolddebug = iDebug;
  if (FDEBUGGING (DEBUG_HANDSHAKE))
    {
      ulog (LOG_DEBUG_START, "zget_uucp_cmd: Got \"");
      iDebug &=~ (DEBUG_INCOMING | DEBUG_PORT);
    }
#endif

  cgot = -1;
  while ((ctimeout = (int) (iendtime - isysdep_time ((long *) NULL))) > 0)
    {
      int b;
      
      b = breceive_char (ctimeout, frequired);
      /* Now b == -1 on timeout, -2 on error.  */
      if (b < 0)
	{
#if DEBUG > 1
	  if (FDEBUGGING (DEBUG_HANDSHAKE))
	    {
	      ulog (LOG_DEBUG_END, "\" (%s)",
		    b == -1 ? "timeout" : "error");
	      iDebug = iolddebug;
	    }
#endif
	  if (b == -1 && frequired)
	    ulog (LOG_ERROR, "Timeout");
	  return NULL;
	}

      /* Apparently some systems use parity on these strings, so we
	 strip the parity bit.  This may need to be configurable at
	 some point, although only if system names can have eight bit
	 characters.  */
      if (! isprint (BUCHAR (b)))
	b &= 0x7f;

#if DEBUG > 1
      if (FDEBUGGING (DEBUG_HANDSHAKE))
	{
	  char ab[5];

	  ++cchars;
	  if (cchars > 60)
	    {
	      ulog (LOG_DEBUG_END, "\"");
	      ulog (LOG_DEBUG_START, "zget_uucp_cmd: Got \"");
	      cchars = 0;
	    }
	  (void) cdebug_char (ab, b);
	  ulog (LOG_DEBUG_CONTINUE, "%s", ab);
	}
#endif

      if (cgot < 0)
	{
	  if (b != '\020')
	    continue;
	  cgot = 0;
	  continue;
	}

      /* If we see another DLE, something has gone wrong; continue
	 as though this were the first one we saw.  */
      if (b == '\020')
	{
	  cgot = 0;
	  continue;
	}

      /* Some systems send a trailing \n on the Shere line.  As far as
	 I can tell this line can never contain a \n, so this
	 modification should be safe enough.  */
      if (b == '\r' || b == '\n')
	b = '\0';

      if (cgot >= calc)
	{
	  calc += CINCREMENT;
	  zalc = (char *) xrealloc ((pointer) zalc, calc);
	}

      zalc[cgot] = (char) b;
      ++cgot;

      if (b == '\0')
	{
#if DEBUG > 1
	  if (FDEBUGGING (DEBUG_HANDSHAKE))
	    {
	      ulog (LOG_DEBUG_END, "\"");
	      iDebug = iolddebug;
	    }
#endif
	  return zalc;
	}
    }

#if DEBUG > 1
  if (FDEBUGGING (DEBUG_HANDSHAKE))
    {
      ulog (LOG_DEBUG_END, "\" (timeout)");
      iDebug = iolddebug;
    }
#endif

  if (frequired)
    ulog (LOG_ERROR, "Timeout");
  return NULL;
}

/* Read a sequence of characters up to a newline or carriage return, and
   return the line without the line terminating character.  */

static const char *
zget_typed_line ()
{
  static char *zalc;
  static int calc;
  int cgot;

#if DEBUG > 1
  int cchars;
  int iolddebug;

  cchars = 0;
  iolddebug = iDebug;
  if (FDEBUGGING (DEBUG_CHAT))
    {
      ulog (LOG_DEBUG_START, "zget_typed_line: Got \"");
      iDebug &=~ (DEBUG_INCOMING | DEBUG_PORT);
    }
#endif

  cgot = 0;
  while (TRUE)
    {
      int b;
      
      b = breceive_char (CTIMEOUT, FALSE);

      /* Now b == -1 on timeout, -2 on error.  */

      if (b == -2 || FGOT_SIGNAL ())
	{
#if DEBUG > 1
	  if (FDEBUGGING (DEBUG_CHAT))
	    {
	      ulog (LOG_DEBUG_END, "\" (error)");
	      iDebug = iolddebug;
	    }
#endif
	  return NULL;
	}

      if (b == -1)
	continue;

#if DEBUG > 1
      if (FDEBUGGING (DEBUG_CHAT))
	{
	  char ab[5];

	  ++cchars;
	  if (cchars > 60)
	    {
	      ulog (LOG_DEBUG_END, "\"");
	      ulog (LOG_DEBUG_START, "zget_typed_line: Got \"");
	      cchars = 0;
	    }
	  (void) cdebug_char (ab, b);
	  ulog (LOG_DEBUG_CONTINUE, "%s", ab);
	}
#endif

      if (cgot >= calc)
	{
	  calc += CINCREMENT;
	  zalc = (char *) xrealloc ((pointer) zalc, calc);
	}

      if (b == '\r' || b == '\n')
	b = '\0';

      zalc[cgot] = (char) b;
      ++cgot;

      if (b == '\0')
	{
#if DEBUG > 1
	  if (FDEBUGGING (DEBUG_CHAT))
	    {
	      ulog (LOG_DEBUG_END, "\"");
	      iDebug = iolddebug;
	    }
#endif
	  return zalc;
	}
    }
}
