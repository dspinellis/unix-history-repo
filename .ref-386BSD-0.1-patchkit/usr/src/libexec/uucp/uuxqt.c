/* uuxqt.c
   Run uux commands.

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

   $Log: uuxqt.c,v $
   Revision 1.43  1992/03/17  04:42:41  ian
   Correct bug in previous patch

   Revision 1.42  1992/03/17  03:15:40  ian
   Pass command to fsysdep_execute as first element of argument array

   Revision 1.41  1992/03/17  01:07:15  ian
   Miscellaneous cleanup

   Revision 1.40  1992/03/16  19:44:45  ian
   Cast result of alloca

   Revision 1.39  1992/03/15  04:51:17  ian
   Keep an array of signals we've received rather than a single variable

   Revision 1.38  1992/03/15  01:54:46  ian
   All execs are now done in isspawn, all waits are done in iswait

   Revision 1.37  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.36  1992/03/11  22:34:25  ian
   Chip Salzenberg: support Internet mail addresses in uuxqt replies

   Revision 1.35  1992/03/11  22:06:37  ian
   Marty Shannon: added max-uuxqts command

   Revision 1.34  1992/03/11  17:04:53  ian
   Jon Zeeff: retry execution later if temporary failure

   Revision 1.33  1992/03/11  02:09:19  ian
   Correct bug in previous change

   Revision 1.32  1992/03/04  02:32:26  ian
   Handle executions on local system

   Revision 1.31  1992/02/29  04:07:08  ian
   Added -j option to uucp and uux

   Revision 1.30  1992/02/29  01:06:59  ian
   Chip Salzenberg: recheck file permissions before sending

   Revision 1.29  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.28  1992/02/24  04:58:47  ian
   Only permit files to be received into directories that are world-writeable

   Revision 1.27  1992/02/23  19:50:50  ian
   Handle READ and WRITE in Permissions correctly

   Revision 1.26  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.25  1992/02/18  19:03:02  ian
   Pass fdaemon argument correctly to usysdep_initialize

   Revision 1.24  1992/02/18  04:53:26  ian
   T. William Wells: make sure sh execution uses absolute path

   Revision 1.23  1992/02/14  21:32:50  ian
   Niels Baggesen: under HAVE_BNU_LOGGING, don't lost system name when dieing

   Revision 1.22  1992/02/08  22:33:32  ian
   Only get the current working directory if it's going to be needed

   Revision 1.21  1992/02/08  20:33:57  ian
   Handle all possible signals raised by abort

   Revision 1.20  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.19  1992/01/16  17:48:41  ian
   Niels Baggesen: was checking strcmp return incorrectly

   Revision 1.18  1992/01/15  07:06:29  ian
   Set configuration directory in Makefile rather than sysdep.h

   Revision 1.17  1992/01/05  03:09:17  ian
   Changed abProgram and abVersion to non const to avoid compiler bug

   Revision 1.16  1992/01/04  21:43:24  ian
   Chip Salzenberg: added ALLOW_FILENAME_ARGUMENTS to permit them

   Revision 1.15  1992/01/04  04:12:54  ian
   David J. Fiander: make sure execution arguments are not bad file names

   Revision 1.14  1991/12/29  04:22:41  ian
   The mailing address was not getting initialized

   Revision 1.13  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.12  1991/12/20  00:17:38  ian
   Don't process execute files for unknown systems

   Revision 1.11  1991/12/19  03:52:07  ian
   David Nugent: rescan the list of execute files until nothing can be done

   Revision 1.10  1991/12/18  03:54:14  ian
   Made error messages to terminal appear more normal

   Revision 1.9  1991/12/17  04:55:01  ian
   David Nugent: ignore SIGHUP in uucico and uuxqt

   Revision 1.8  1991/12/09  18:49:06  ian
   Richard Todd: the requestor address is relative to the requesting system

   Revision 1.7  1991/12/06  23:42:18  ian
   Don't acknowledge success by default

   Revision 1.6  1991/12/01  01:28:44  ian
   Mitch Mitchell: fixed comment listing supported commands

   Revision 1.5  1991/11/21  22:17:06  ian
   Add version string, print version when printing usage

   Revision 1.4  1991/11/07  20:52:33  ian
   Chip Salzenberg: pass command as single argument to /bin/sh

   Revision 1.3  1991/09/19  16:15:58  ian
   Chip Salzenberg: configuration option for permitting execution via sh

   Revision 1.2  1991/09/19  02:30:37  ian
   From Chip Salzenberg: check whether signal is ignored differently

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char uuxqt_rcsid[] = "$Id: uuxqt.c,v 1.43 1992/03/17 04:42:41 ian Rel $";
#endif

#include <errno.h>
#include <ctype.h>

#include "getopt.h"

#include "system.h"
#include "sysdep.h"

/* External functions.  */
extern int fclose ();

/* The program name.  */
char abProgram[] = "uuxqt";

/* Static variables used to unlock things if we get a fatal error.  */

static int iQunlock_seq = -1;
static const char *zQunlock_cmd;
static const char *zQunlock_file;
static boolean fQunlock_directory;

/* Local functions.  */

static void uqusage P((void));
static void uqabort P((void));
static void uqdo_xqt_file P((const char *zfile,
			     const struct ssysteminfo *qsys,
			     const char *zcmd, boolean *pfprocessed));
static void uqcleanup P((const char *zfile, int iflags));

/* Long getopt options.  */

static const struct option asQlongopts[] = { { NULL, 0, NULL, 0 } };

const struct option *_getopt_long_options = asQlongopts;

int
main (argc, argv)
     int argc;
     char **argv;
{
  int iopt;
  /* The type of command to execute (NULL for any type).  */
  const char *zcmd = NULL;
  /* The configuration file name.  */
  const char *zconfig = NULL;
  /* The system to execute commands for.  */
  const char *zdosys = NULL;
  boolean fany;
  const char *z;
  const char *zgetsys;
  boolean ferr;
  struct ssysteminfo sreadsys;
  const struct ssysteminfo *qreadsys;

  while ((iopt = getopt (argc, argv, "c:I:s:x:")) != EOF)
    {
      switch (iopt)
	{
	case 'c':
	  /* Set the type of command to execute.  */
	  zcmd = optarg;
	  break;

	case 'I':
	  /* Set the configuration file name.  */
	  zconfig = optarg;
	  break;

	case 's':
	  zdosys = optarg;
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
	  uqusage ();
	  break;
	}
    }

  if (optind != argc)
    uqusage ();

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
  ulog_fatal_fn (uqabort);

  /* Limit the number of uuxqt processes, and make sure we're the only
     uuxqt daemon running for this command.  */
  iQunlock_seq = isysdep_lock_uuxqt (zcmd);
  if (iQunlock_seq < 0)
    {
      ulog_close ();
      usysdep_exit (TRUE);
    }
  zQunlock_cmd = zcmd;

  /* Keep scanning the execute files until we don't process any of
     them.  */

  do
    {
      fany = FALSE;

      /* Look for each execute file, and run it.  */

      if (! fsysdep_get_xqt_init ())
	{
	  ulog_close ();
	  usysdep_exit (FALSE);
	}

      qreadsys = NULL;

      while ((z = zsysdep_get_xqt (&zgetsys, &ferr)) != NULL)
	{
	  char *zcopy;
	  boolean fprocessed;
	  const struct ssysteminfo *qusesys;

#if ! HAVE_ALLOCA
	  /* Clear out any accumulated alloca buffers.  */
	  (void) alloca (0);
#endif

	  /* It would be more efficient to pass zdosys down to the
	     routines which retrieve execute files.  */
	  if (zdosys != NULL && strcmp (zdosys, zgetsys) != 0)
	    continue;

	  if (strcmp (zgetsys, zLocalname) == 0)
	    qusesys = &sLocalsys;
	  else
	    {
	      if (qreadsys == NULL
		  || strcmp (qreadsys->zname, zgetsys) != 0)
		{
		  if (fread_system_info (zgetsys, &sreadsys))
		    qreadsys = &sreadsys;
		  else
		    {
		      if (! fUnknown_ok)
			{
			  ulog (LOG_ERROR,
				"%s: Execute file for unknown system %s",
				z, zgetsys);
			  (void) remove (z);
			  continue;
			}
		      qreadsys = &sUnknown;
		      sUnknown.zname = xstrdup (zgetsys);
		    }

		  if (! fsysdep_make_spool_dir (qreadsys))
		    continue;
		}

	      qusesys = qreadsys;
	    }

	  /* If we've received a signal, get out of the loop.  */
	  if (FGOT_SIGNAL ())
	    break;

	  zcopy = xstrdup (z);

	  ulog_system (qusesys->zname);
	  uqdo_xqt_file (zcopy, qusesys, zcmd, &fprocessed);
	  ulog_system ((const char *) NULL);
	  ulog_user ((const char *) NULL);

	  if (fprocessed)
	    fany = TRUE;
	  xfree ((pointer) zcopy);
	}

      usysdep_get_xqt_free ();
    }
  while (fany && ! FGOT_SIGNAL ());

  (void) fsysdep_unlock_uuxqt (iQunlock_seq, zcmd);
  iQunlock_seq = -1;

  ulog_close ();

  if (FGOT_SIGNAL ())
    ferr = TRUE;

  usysdep_exit (! ferr);

  /* Avoid errors about not returning a value.  */
  return 0;
}

static void
uqusage ()
{
  fprintf (stderr,
	   "Taylor UUCP version %s, copyright (C) 1991, 1992 Ian Lance Taylor\n",
	   abVersion);
  fprintf (stderr,
	   "Usage: uuxqt [-c cmd] [-I file] [-s system] [-x debug]\n");
  fprintf (stderr,
	   " -c cmd: Set type of command to execute\n");
  fprintf (stderr,
	   " -s system: Execute commands only for named system\n");
  fprintf (stderr,
	   " -x debug: Set debugging level (0 for none, 9 is max)\n");
#if HAVE_TAYLOR_CONFIG
  fprintf (stderr,
	   " -I file: Set configuration file to use (default %s%s)\n",
	   NEWCONFIGLIB, CONFIGFILE);
#endif /* HAVE_TAYLOR_CONFIG */
  exit (EXIT_FAILURE);
}

/* This is the abort function called when we get a fatal error.  */

static void
uqabort ()
{
#if ! HAVE_BNU_LOGGING
  /* When using BNU logging, it's a pain to have no system name.  */
  ulog_system ((const char *) NULL);
#endif

  ulog_user ((const char *) NULL);

  if (fQunlock_directory)
    (void) fsysdep_unlock_uuxqt_dir ();

  if (zQunlock_file != NULL)
    (void) fsysdep_unlock_uuxqt_file (zQunlock_file);

  if (iQunlock_seq >= 0)
    (void) fsysdep_unlock_uuxqt (iQunlock_seq, zQunlock_cmd);

  ulog_close ();

  usysdep_exit (FALSE);
}

/* An execute file is a series of lines.  The first character of each
   line is a command.  The following commands are defined:

   C command-line
   I standard-input
   O standard-output [ system ]
   F required-file filename-to-use
   R requestor-address
   U user system
   Z (acknowledge if command failed; default)
   N (no acknowledgement on failure)
   n (acknowledge if command succeeded)
   B (return command input on error)
   e (process with sh)
   E (process with exec)
   M status-file
   # comment

   Unrecognized commands are ignored.  We actually do not recognize
   the Z command, since it requests default behaviour.  We always send
   mail on failure, unless the N command appears.  We never send mail
   on success, unless the n command appears.

   This code does not currently support the B or M commands.  */

/* Command arguments.  */
static const char **azQargs;
/* Command as a complete string.  */
static char *zQcmd;
/* Standard input file name.  */
static const char *zQinput;
/* Standard output file name.  */
static const char *zQoutfile;
/* Standard output system.  */
static const char *zQoutsys;
/* Number of required files.  */
static int cQfiles;
/* Names of required files.  */
static char **azQfiles;
/* Names required files should be renamed to (NULL if original is OK).  */
static char **azQfiles_to;
/* Requestor address (this is where mail should be sent).  */
static const char *zQrequestor;
/* User name.  */
static const char *zQuser;
/* System name.  */
static const char *zQsystem;
/* This is set by the N flag, meaning that no acknowledgement should
   be mailed on failure.  */
static boolean fQno_ack;
/* This is set by the n flag, meaning that acknowledgement should be
   mailed if the command succeeded.  */
static boolean fQsuccess_ack;
/* This is set by the B flag, meaning that command input should be
   mailed to the requestor if an error occurred.  */
static boolean fQsend_input;
/* This is set by the E flag, meaning that exec should be used to
   execute the command.  */
static boolean fQuse_exec;
/* The status should be copied to this file on the requesting host.  */
static const char *zQstatus_file;
#if ALLOW_SH_EXECUTION
/* This is set by the e flag, meaning that sh should be used to
   execute the command.  */
static boolean fQuse_sh;
#endif /* ALLOW_SH_EXECUTION */

static enum tcmdtabret tqcmd P((int argc, char **argv, pointer pvar,
				const char *zerr));
static enum tcmdtabret tqout P((int argc, char **argv, pointer pvar,
				const char *zerr));
static enum tcmdtabret tqfile P((int argc, char **argv, pointer pvar,
				 const char *zerr));
static enum tcmdtabret tquser P((int argc, char **argv, pointer pvar,
				 const char *zerr));
static enum tcmdtabret tqset P((int argc, char **argv, pointer pvar,
				const char *zerr));

static struct scmdtab asQcmds[] =
{
  { "C", CMDTABTYPE_FN | 0, NULL, tqcmd },
  { "I", CMDTABTYPE_STRING, (pointer) &zQinput, NULL },
  { "O", CMDTABTYPE_FN | 0, NULL, tqout },
  { "F", CMDTABTYPE_FN | 0, NULL, tqfile },
  { "R", CMDTABTYPE_STRING, (pointer) &zQrequestor, NULL },
  { "U", CMDTABTYPE_FN | 3, NULL, tquser },
  { "N", CMDTABTYPE_FN | 1, (pointer) &fQno_ack, tqset },
  { "n", CMDTABTYPE_FN | 1, (pointer) &fQsuccess_ack, tqset },
  { "B", CMDTABTYPE_FN | 1, (pointer) &fQsend_input, tqset },
#if ALLOW_SH_EXECUTION
  { "e", CMDTABTYPE_FN | 1, (pointer) &fQuse_sh, tqset },
#endif
  { "E", CMDTABTYPE_FN | 1, (pointer) &fQuse_exec, tqset },
  { "M", CMDTABTYPE_STRING, (pointer) &zQstatus_file, NULL },
  { NULL, 0, NULL, NULL }
};

/* Handle the C command: store off the arguments.  */

/*ARGSUSED*/
static enum tcmdtabret
tqcmd (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  int i;
  int clen;

  if (argc <= 1)
    return CMDTABRET_FREE;

  azQargs = (const char **) xmalloc (argc * sizeof (char *));
  clen = 0;
  for (i = 1; i < argc; i++)
    {
      azQargs[i - 1] = argv[i];
      clen += strlen (argv[i]) + 1;
    }
  azQargs[i - 1] = NULL;

  zQcmd = (char *) xmalloc (clen);
  zQcmd[0] = '\0';
  for (i = 1; i < argc - 1; i++)
    {
      strcat (zQcmd, argv[i]);
      strcat (zQcmd, " ");
    }
  strcat (zQcmd, argv[i]);

  return CMDTABRET_CONTINUE;
}

/* Handle the O command, which may have one or two arguments.  */

/*ARGSUSED*/
static enum tcmdtabret
tqout (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  if (argc != 2 && argc != 3)
    {
      ulog (LOG_ERROR, "%s: %s: Wrong number of arguments",
	    zerr, argv[0]);
      return CMDTABRET_FREE;
    }

  zQoutfile = argv[1];
  if (argc == 3)
    zQoutsys = argv[2];

  return CMDTABRET_CONTINUE;
}

/* Handle the F command, which may have one or two arguments.  */

/*ARGSUSED*/
static enum tcmdtabret
tqfile (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  if (argc != 2 && argc != 3)
    {
      ulog (LOG_ERROR, "%s: %s: Wrong number of arguments",
	    zerr, argv[0]);
      return CMDTABRET_FREE;
    }

  /* If this file is not in the spool directory, just ignore it.  */
  if (! fspool_file (argv[1]))
    return CMDTABRET_FREE;

  ++cQfiles;
  azQfiles = (char **) xrealloc ((pointer) azQfiles,
				 cQfiles * sizeof (char *));
  azQfiles_to = (char **) xrealloc ((pointer) azQfiles_to,
				    cQfiles * sizeof (char *));

  azQfiles[cQfiles - 1] = xstrdup (argv[1]);
  if (argc == 3)
    azQfiles_to[cQfiles - 1] = xstrdup (argv[2]);
  else
    azQfiles_to[cQfiles - 1] = NULL;

  return CMDTABRET_FREE;
}

/* Handle the U command, which takes two arguments.  */

/*ARGSUSED*/
static enum tcmdtabret
tquser (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  zQuser = argv[1];
  zQsystem = argv[2];
  return CMDTABRET_CONTINUE;
}

/* Handle various commands which just set boolean variables.  */

/*ARGSUSED*/
static enum tcmdtabret
tqset (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  boolean *pf = (boolean *) pvar;

  *pf = TRUE;
  return CMDTABRET_FREE;
}

/* The execution processing does a lot of things that have to be
   cleaned up.  Rather than try to add the appropriate statements
   to each return point, we keep a set of flags indicating what
   has to be cleaned up.  The actual clean up is done by the
   function uqcleanup.  */

#define REMOVE_FILE (01)
#define REMOVE_NEEDED (02)
#define FREE_QINPUT (04)

/* Process an execute file.  The zfile argument is the name of the
   execute file.  The qsys argument describes the system it came from.
   The zcmd argument is the name of the command we are executing (from
   the -c option) or NULL if any command is OK.  This sets
   *pfprocessed to TRUE if the file is ready to be executed.  */

static void
uqdo_xqt_file (zfile, qsys, zcmd, pfprocessed)
     const char *zfile;
     const struct ssysteminfo *qsys;
     const char *zcmd;
     boolean *pfprocessed;
{
  const char *zcmds;
  const char *zabsolute;
  boolean ferr;
  FILE *e;
  int i;
  int iclean;
  const char *zmail;
  const char *zoutput;
  char abtemp[CFILE_NAME_LEN];
  char abdata[CFILE_NAME_LEN];
  const char *zerror;
  struct ssysteminfo soutsys;
  const struct ssysteminfo *qoutsys;
  boolean fshell;
  char *zfullcmd;
  boolean ftemp;

  *pfprocessed = FALSE;

  /* If we're not permitted to execute anything for this system,
     we can just clobber the file without even looking at it.  */
  zcmds = qsys->zcmds;

  if (*zcmds == '\0')
    {
      ulog (LOG_ERROR, "%s: No commands permitted for system %s",
	    zfile, qsys->zname);
      (void) remove (zfile);
      return;
    }

  /* If we are only willing to execute a particular command, and it
     is not one of those accepted by this system, quit now.  */
  if (zcmd != NULL
      && strcmp (zcmds, "ALL") != 0
      && strstr (zcmds, zcmd) == NULL)
    return;

  e = fopen (zfile, "r");
  if (e == NULL)
    return;

  azQargs = NULL;
  zQcmd = NULL;
  zQinput = NULL;
  zQoutfile = NULL;
  zQoutsys = NULL;
  cQfiles = 0;
  azQfiles = NULL;
  azQfiles_to = NULL;
  zQrequestor = NULL;
  zQuser = NULL;
  zQsystem = NULL;
  fQno_ack = FALSE;
  fQsuccess_ack = FALSE;
  fQsend_input = FALSE;
  fQuse_exec = FALSE;
  zQstatus_file = NULL;
#if ALLOW_SH_EXECUTION
  fQuse_sh = FALSE;
#endif

  uprocesscmds (e, (struct smulti_file *) NULL, asQcmds, zfile, 0);

  (void) fclose (e);

  iclean = 0;

  if (azQargs == NULL)
    {
      ulog (LOG_ERROR, "%s: No command given", zfile);
      uqcleanup (zfile, iclean | REMOVE_FILE);
      return;
    }

  if (zcmd != NULL)
    {
      if (strcmp (zcmd, azQargs[0]) != 0)
	{
	  uqcleanup (zfile, iclean);
	  return;
	}
    }
  else
    {
      /* If there is a lock file for this particular command already,
	 it means that some other uuxqt is supposed to handle it.  */
      if (fsysdep_uuxqt_locked (azQargs[0]))
	{
	  uqcleanup (zfile, iclean);
	  return;
	}
    }

  /* Lock this particular file.  */

  if (! fsysdep_lock_uuxqt_file (zfile))
    {
      uqcleanup (zfile, iclean);
      return;
    }

  zQunlock_file = zfile;

  if (zQuser != NULL)
    ulog_user (zQuser);
  else if (zQrequestor != NULL)
    ulog_user (zQrequestor);
  else
    ulog_user ("unknown");

  /* Make sure that all the required files exist, and get their
     full names in the spool directory.  */

  for (i = 0; i < cQfiles; i++)
    {
      const char *zreal;

      zreal = zsysdep_spool_file_name (qsys, azQfiles[i]);
      if (zreal == NULL)
	{
	  uqcleanup (zfile, iclean);
	  return;
	}
      if (! fsysdep_file_exists (zreal))
	{
	  uqcleanup (zfile, iclean);
	  return;
	}
      xfree ((pointer) azQfiles[i]);
      azQfiles[i] = xstrdup (zreal);
    }

  /* See if we need the execute directory, and lock it if we do.  */

  for (i = 0; i < cQfiles; i++)
    {
      int itries;

      if (azQfiles_to[i] == NULL)
	continue;

      for (itries = 0; itries < 5; itries++)
	{
	  if (fsysdep_lock_uuxqt_dir ())
	    break;
	  usysdep_sleep (30);
	}
      if (itries >= 5)
	{
	  ulog (LOG_ERROR, "Could not lock execute directory");
	  uqcleanup (zfile, iclean);
	  return;
	}

      fQunlock_directory = TRUE;
      break;
    }

  iclean |= REMOVE_FILE | REMOVE_NEEDED;
  *pfprocessed = TRUE;

  /* Get the address to mail results to.  Prepend the system from
     which the execute file originated, since mail addresses are
     relative to it.  */

  zmail = NULL;
  if (zQrequestor != NULL)
    zmail = zQrequestor;
  else if (zQuser != NULL)
    zmail = zQuser;
  if (zmail != NULL
      && zQsystem != NULL
#if HAVE_INTERNET_MAIL
      && strchr (zmail, '@') == NULL
#endif
      && strcmp (zQsystem, zLocalname) != 0)
    {
      char *zset;

      zset = (char *) alloca (strlen (zQsystem) + strlen (zmail) + 2);

      sprintf (zset, "%s!%s", zQsystem, zmail);
      zmail = zset;
    }

  /* Get the pathname to execute.  */

  zabsolute = zsysdep_find_command (azQargs[0], zcmds, qsys->zpath,
				    &ferr);
  if (zabsolute == NULL)
    {
      if (ferr)
	{
	  /* If we get an error, try again later.  */
	  uqcleanup (zfile, iclean &~ (REMOVE_FILE | REMOVE_NEEDED));
	  *pfprocessed = FALSE;
	  return;
	}

      /* Not permitted.  Send mail to requestor.  */
	  
      ulog (LOG_ERROR, "Not permitted to execute %s",
	    azQargs[0]);

      if (zmail != NULL && ! fQno_ack)
	{
	  const char *az[20];

	  i = 0;
	  az[i++] = "Your execution request failed because you are not";
	  az[i++] = " permitted to execute\n\t";
	  az[i++] = azQargs[0];
	  az[i++] = "\non this system.\n";
	  az[i++] = "Execution requested was:\n\t";
	  az[i++] = zQcmd;
	  az[i++] = "\n";

	  (void) fsysdep_mail (zmail, "Execution failed", i, az);
	}

      uqcleanup (zfile, iclean);
      return;
    }

  {
    char *zcopy;

    zcopy = (char *) alloca (strlen (zabsolute) + 1);
    strcpy (zcopy, zabsolute);
    zabsolute = zcopy;
  }

  azQargs[0] = zabsolute;

#if ! ALLOW_FILENAME_ARGUMENTS

  /* Check all the arguments to make sure they don't try to specify
     files they are not permitted to access.  */

  for (i = 1; azQargs[i] != NULL; i++)
    {
      if (! fsysdep_xqt_check_file (qsys, azQargs[i]))
	{
	  if (zmail != NULL && ! fQno_ack)
	    {
	      const char *az[20];
	      const char *zfailed;

	      zfailed = azQargs[i];
	      i = 0;
	      az[i++] = "Your execution request failed because you are not";
	      az[i++] = " permitted to refer to file\n\t";
	      az[i++] = zfailed;
	      az[i++] = "\non this system.\n";
	      az[i++] = "Execution requested was:\n\t";
	      az[i++] = zQcmd;
	      az[i++] = "\n";

	      (void) fsysdep_mail (zmail, "Execution failed", i, az);
	    }

	  uqcleanup (zfile, iclean);
	  return;
	}
    }

#endif /* ! ALLOW_FILENAME_ARGUMENTS */

  ulog (LOG_NORMAL, "Executing %s (%s)", zfile, zQcmd);

  if (zQinput != NULL)
    {
      boolean fspool;

      fspool = fspool_file (zQinput);
      if (fspool)
	zQinput = zsysdep_spool_file_name (qsys, zQinput);
      else
	zQinput = zsysdep_real_file_name (qsys, zQinput,
					  (const char *) NULL);
      if (zQinput == NULL)
	{
	  /* If we get an error, try again later.  */
	  uqcleanup (zfile, iclean &~ (REMOVE_FILE | REMOVE_NEEDED));
	  *pfprocessed = FALSE;
	  return;
	}

      zQinput = xstrdup (zQinput);
      iclean |= FREE_QINPUT;

      if (! fspool
	  && ! fin_directory_list (qsys, zQinput,
				   qsys->zremote_send, TRUE, TRUE,
				   (const char *) NULL))
	{
	  ulog (LOG_ERROR, "Not permitted to read %s", zQinput);
	      
	  if (zmail != NULL && ! fQno_ack)
	    {
	      const char *az[20];

	      i = 0;
	      az[i++] = "Your execution request failed because you are";
	      az[i++] = " not permitted to read\n\t";
	      az[i++] = zQinput;
	      az[i++] = "\non this system.\n";
	      az[i++] = "Execution requested was:\n\t";
	      az[i++] = zQcmd;
	      az[i++] = "\n";

	      (void) fsysdep_mail (zmail, "Execution failed", i, az);
	    }

	  uqcleanup (zfile, iclean);
	  return;
	}
    }

  if (zQoutfile == NULL)
    {
      zoutput = NULL;
      qoutsys = NULL;
    }
  else if (zQoutsys != NULL
	   && strcmp (zQoutsys, zLocalname) != 0)
    {
      const char *zdata;
      char *zcopy;
	 
      /* The output file is destined for some other system, so we must
	 use a temporary file to catch standard output.  */

      if (strcmp (zQoutsys, qsys->zname) == 0)
	qoutsys = qsys;
      else
	{
	  if (! fread_system_info (zQoutsys, &soutsys))
	    {
	      if (! fUnknown_ok)
		{
		  ulog (LOG_ERROR,
			"Can't send standard output to unknown system %s",
			zQoutsys);
		  /* We don't send mail to unknown systems, either.
		     Maybe we should.  */
		  uqcleanup (zfile, iclean);
		  return;
		}
	      soutsys = sUnknown;
	      soutsys.zname = zQoutsys;
	    }

	  qoutsys = &soutsys;
	 
	  if (! fsysdep_make_spool_dir (qoutsys))
	    {
	      /* If we get an error, try again later.  */
	      uqcleanup (zfile, iclean &~ (REMOVE_FILE | REMOVE_NEEDED));
	      *pfprocessed = FALSE;
	      return;
	    }
	}

      zdata = zsysdep_data_file_name (qoutsys, BDEFAULT_UUX_GRADE, abtemp,
				      abdata, (char *) NULL);
      if (zdata == NULL)
	{
	  /* If we get an error, try again later.  */
	  uqcleanup (zfile, iclean &~ (REMOVE_FILE | REMOVE_NEEDED));
	  *pfprocessed = FALSE;
	  return;
	}
	 
      zcopy = (char *) alloca (strlen (zdata) + 1);
      strcpy (zcopy, zdata);
      zoutput = zcopy;
    }
  else
    {
      boolean fok;
      char *zcopy;
	 
      qoutsys = NULL;

      /* If we permitted the standard output to be redirected into
	 the spool directory, people could set up phony commands.  */

      if (fspool_file (zQoutfile))
	fok = FALSE;
      else
	{
	  zQoutfile = zsysdep_real_file_name (&sLocalsys, zQoutfile,
					      (const char *) NULL);
	  if (zQoutfile == NULL)
	    {
	      /* If we get an error, try again later.  */
	      uqcleanup (zfile, iclean &~ (REMOVE_FILE | REMOVE_NEEDED));
	      *pfprocessed = FALSE;
	      return;
	    }

	  /* Make sure it's OK to receive this file.  Note that this
	     means that a locally executed uux (which presumably
	     requires remote files) will only be able to create files
	     in standard directories.  If we don't it this way, users
	     could clobber files which uucp has access to; still, it
	     would be nice to allow them to direct the output to their
	     home directory.  */
      
	  fok = fin_directory_list (qsys, zQoutfile, qsys->zremote_receive,
				    TRUE, FALSE, (const char *) NULL);
	}

      if (! fok)
	{
	  ulog (LOG_ERROR, "Not permitted to write to %s", zQoutfile);
	      
	  if (zmail != NULL && ! fQno_ack)
	    {
	      const char *az[20];

	      i = 0;
	      az[i++] = "Your execution request failed because you are";
	      az[i++] = " not permitted to write to\n\t";
	      az[i++] = zQoutfile;
	      az[i++] = "\non this system.\n";
	      az[i++] = "Execution requested was:\n\t";
	      az[i++] = zQcmd;
	      az[i++] = "\n";

	      (void) fsysdep_mail (zmail, "Execution failed", i, az);
	    }

	  uqcleanup (zfile, iclean);
	  return;
	}

      zcopy = (char *) alloca (strlen (zQoutfile) + 1);
      strcpy (zcopy, zQoutfile);
      zQoutfile = zcopy;
      zoutput = zcopy;
    }

  /* Move the required files to the execution directory if necessary.  */

  for (i = 0; i < cQfiles; i++)
    {
      if (azQfiles_to[i] != NULL)
	{
	  const char *zname;

	  /* Move the file to the execute directory.  */

	  zname = zsysdep_in_dir (XQTDIR, azQfiles_to[i]);
	  if (zname == NULL)
	    {
	      /* If we get an error, try again later.  */
	      uqcleanup (zfile, iclean &~ (REMOVE_FILE | REMOVE_NEEDED));
	      *pfprocessed = FALSE;
	      return;
	    }
	  if (! fsysdep_move_file (azQfiles[i], zname, 0, FALSE,
				   (const char *) NULL))
	    {
	      /* If we get an error, try again later.  This may not be
		 correct, depending on what kind of error we get.  */
	      uqcleanup (zfile, iclean &~ (REMOVE_FILE | REMOVE_NEEDED));
	      *pfprocessed = FALSE;
	      return;
	    }

	  /* If we just moved the standard input file, adjust the
	     file name.  */
	  if (zQinput != NULL && strcmp (azQfiles[i], zQinput) == 0)
	    {
	      xfree ((pointer) zQinput);
	      zQinput = xstrdup (zname);
	    }
	}
    }

#if ALLOW_SH_EXECUTION
  fshell = fQuse_sh;
#else
  fshell = FALSE;
#endif

  /* Get a shell command which uses the full path of the command to
     execute.  */
  zfullcmd = (char *) alloca (strlen (zQcmd) + strlen (azQargs[0]) + 2);
  *zfullcmd = '\0';
  for (i = 0; azQargs[i] != NULL; i++)
    {
      strcat (zfullcmd, azQargs[i]);
      strcat (zfullcmd, " ");
    }
  zfullcmd[strlen (zfullcmd) - 1] = '\0';

  if (! fsysdep_execute (qsys,
			 zQuser == NULL ? (const char *) "uucp" : zQuser,
			 azQargs, zfullcmd, zQinput, zoutput, fshell,
			 &zerror, &ftemp))
    {
      if (ftemp)
	{
	  ulog (LOG_NORMAL, "Will retry later (%s)", zfile);
	  if (zoutput != NULL)
	    (void) remove (zoutput);
	  if (zerror != NULL)
	    (void) remove (zerror);
	  uqcleanup (zfile, iclean &~ (REMOVE_FILE | REMOVE_NEEDED));
	  *pfprocessed = FALSE;
	  return;
	}

      ulog (LOG_NORMAL, "Execution failed (%s)", zfile);

      if (zmail != NULL && ! fQno_ack)
	{
	  const char **pz;
	  int cgot;
	  FILE *eerr;
	  int istart;

	  cgot = 20;
	  pz = (const char **) xmalloc (cgot * sizeof (const char *));
	  i = 0;
	  pz[i++] = "Execution request failed:\n\t";
	  pz[i++] = zQcmd;
	  pz[i++] = "\n";

	  if (zerror == NULL)
	    eerr = NULL;
	  else
	    eerr = fopen (zerror, "r");
	  if (eerr == NULL)
	    {
	      pz[i++] = "There was no output on standard error\n";
	      istart = i;
	    }
	  else
	    {
	      char *zline;

	      pz[i++] = "Standard error output was:\n";
	      istart = i;

	      while ((zline = zfgets (eerr, FALSE)) != NULL)
		{
		  if (i >= cgot)
		    {
		      cgot += 20;
		      pz = ((const char **)
			    xrealloc ((pointer) pz,
				      cgot * sizeof (const char *)));
		    }
		  pz[i++] = zline;
		}

	      (void) fclose (eerr);
	    }

	  (void) fsysdep_mail (zmail, "Execution failed", i, pz);

	  for (; istart < i; istart++)
	    xfree ((pointer) pz[istart]);
	  xfree ((pointer) pz);
	}

      if (qoutsys != NULL)
	(void) remove (zoutput);
    }
  else
    {
      if (zmail != NULL && fQsuccess_ack)
	{
	  const char *az[20];

	  i = 0;
	  az[i++] = "\nExecution request succeeded:\n\t";
	  az[i++] = zQcmd;
	  az[i++] = "\n";

	  (void) fsysdep_mail (zmail, "Execution succeded", i, az);
	}

      /* Now we may have to uucp the output to some other machine.  */

      if (qoutsys != NULL)
	{
	  struct scmd s;

	  /* Fill in the command structure.  */

	  s.bcmd = 'S';
	  s.pseq = NULL;
	  s.zfrom = abtemp;
	  s.zto = zQoutfile;
	  if (zQuser != NULL)
	    s.zuser = zQuser;
	  else
	    s.zuser = "uucp";
	  if (zmail != NULL && fQsuccess_ack)
	    s.zoptions = "Cn";
	  else
	    s.zoptions = "C";
	  s.ztemp = abtemp;
	  s.imode = 0666;
	  if (zmail != NULL && fQsuccess_ack)
	    s.znotify = zmail;
	  else
	    s.znotify = "";
	  /* The number of bytes will be filled in when the file is
	     actually sent.  */
	  s.cbytes = -1;

	  (void) zsysdep_spool_commands (qoutsys, BDEFAULT_UUX_GRADE,
					 1, &s);
	}
    }

  if (zerror != NULL)
    (void) remove (zerror);

  uqcleanup (zfile, iclean);
}

/* Clean up the results of uqdo_xqt_file.  */

static void
uqcleanup (zfile, iflags)
     const char *zfile;
     int iflags;
{
  int i;

  DEBUG_MESSAGE2 (DEBUG_SPOOLDIR,
		  "uqcleanup: %s, %d", zfile, iflags);

  if (zQunlock_file != NULL)
    {
      (void) fsysdep_unlock_uuxqt_file (zQunlock_file);
      zQunlock_file = NULL;
    }

  if ((iflags & REMOVE_FILE) != 0)
    (void) remove (zfile);

  if ((iflags & REMOVE_NEEDED) != 0)
    {
      for (i = 0; i < cQfiles; i++)
	{
	  if (azQfiles[i] != NULL)
	    (void) remove (azQfiles[i]);
	}
    }

  if ((iflags & FREE_QINPUT) != 0)
    xfree ((pointer) zQinput);

  if (fQunlock_directory)
    {
      (void) fsysdep_unlock_uuxqt_dir ();
      fQunlock_directory = FALSE;
    }

  for (i = 0; i < cQfiles; i++)
    {
      xfree ((pointer) azQfiles[i]);
      xfree ((pointer) azQfiles_to[i]);
    }

  xfree ((pointer) azQargs);
  azQargs = NULL;

  xfree ((pointer) zQcmd);
  zQcmd = NULL;

  xfree ((pointer) azQfiles);
  azQfiles = NULL;

  xfree ((pointer) azQfiles_to);
  azQfiles_to = NULL;
}
