/* sysinf.c
   Functions to read system information for the UUCP package.

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

   $Log: sysinf.c,v $
   Revision 1.24  1992/04/02  22:51:09  ian
   Add gcc 2.0 format checking to ulog, and fixed discovered problems

   Revision 1.23  1992/03/30  04:49:10  ian
   Niels Baggesen: added debugging types abnormal and uucp-proto

   Revision 1.22  1992/03/28  21:47:55  ian
   David J. MacKenzie: allow backslash to quote newline in config files

   Revision 1.21  1992/03/28  20:31:55  ian
   Franc,ois Pinard: allow a name to be given to an alternate

   Revision 1.20  1992/03/24  17:18:33  ian
   Fixed handling of alternates in file-wide defaults

   Revision 1.19  1992/03/16  04:30:57  ian
   Permit a retry time for the time and timegrade commands

   Revision 1.18  1992/03/13  16:17:42  ian
   Chip Salzenberg: set default login chat script timeout to 10 seconds

   Revision 1.17  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.16  1992/03/09  20:14:37  ian
   Ted Lindgreen: added max-remote-debug command

   Revision 1.15  1992/03/07  02:56:30  ian
   Rewrote time routines

   Revision 1.14  1992/03/03  06:06:48  ian
   T. William Wells: don't complain about missing configuration files

   Revision 1.13  1992/02/23  19:50:50  ian
   Handle READ and WRITE in Permissions correctly

   Revision 1.12  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.11  1992/01/13  05:17:30  ian
   Mike Park: wrong number of arguments to ulog call

   Revision 1.10  1992/01/07  15:23:50  ian
   Niels Baggesen: allocate number of protocol parameters correctly

   Revision 1.9  1991/12/28  03:49:23  ian
   Added HAVE_MEMFNS and HAVE_BFNS; changed uses of memset to bzero

   Revision 1.8  1991/12/23  05:15:54  ian
   David Nugent: set debugging level for a specific system

   Revision 1.7  1991/12/17  17:08:02  ian
   Marc Unangst: allow true and false for boolean strings as documented

   Revision 1.6  1991/12/15  04:17:11  ian
   Added chat-seven-bit command to control parity bit stripping

   Revision 1.5  1991/12/15  03:42:33  ian
   Added tprocess_chat_cmd for all chat commands, and added CMDTABTYPE_PREFIX

   Revision 1.4  1991/11/13  20:38:00  ian
   Added TCP port type for connections over TCP

   Revision 1.3  1991/11/12  19:47:04  ian
   Add called-chat set of commands to run a chat script on an incoming call

   Revision 1.2  1991/11/11  23:47:24  ian
   Added chat-program to run a program to do a chat script

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char sysinf_rcsid[] = "$Id: sysinf.c,v 1.24 1992/04/02 22:51:09 ian Rel $";
#endif

#include <ctype.h>
#include <errno.h>

#include "port.h"
#include "system.h"
#include "uutime.h"

/* Whether we accept calls from unknown systems.  */
boolean fUnknown_ok = FALSE;

/* Information we hold for an unknown system.  */
struct ssysteminfo sUnknown;

/* Information we hold for the local system.  */
struct ssysteminfo sLocalsys;

#if HAVE_TAYLOR_CONFIG

/* I don't find the system file format used by either V2 or BNU UUCP
   very intuitive, so I've developed my own format.  This replaces the
   files L.sys and USERFILE for V2, and Systems and Permissions for
   BNU.  The complete format is described in a separate document.  */

/* Some local functions.  */
static void uiset_clear P((boolean falternate));

/* Set when we read the first line of a new file, to indicate that a
   new set of defaults should be gathered.  */
static boolean fIfirst;

/* This structure holds system information as it is gathered.  */
static struct ssysteminfo sIhold;

/* Default information.  */
static struct ssysteminfo sIdefault;

/* The name of the next system.  */
static char *zInext_system;

/* The list of alternates to draw from.  */
static struct ssysteminfo *qIalternates;

/* Whether to use default alternates.  */
static boolean fIdefault_alternates;

/* There are several commands for which we want to take the default if
   provided, but we don't want to add to the default.  For example,
   each system can have multiple time specifications (using different
   grades), which is implemented by adding each new time specification
   to the previous ones. We want to allow default time specifications.
   However, if a system has time specifications, we don't want to add
   its time specifications to the default ones.  We handle this with a
   set of boolean variables which we set to tell the adding functions
   to ignore any existing entry; when an entry has been added, the
   boolean variable is cleared.  */
static boolean fIclear_alias;
static boolean fIclear_alternate;
static boolean fIclear_time;
static boolean fIclear_calltimegrade;
static boolean fIclear_call_local_size;
static boolean fIclear_call_remote_size;
static boolean fIclear_called_local_size;
static boolean fIclear_called_remote_size;
static boolean fIclear_port;
static boolean fIclear_chat_fail;
static boolean fIclear_proto_param;
static boolean fIclear_called_chat_fail;

/* Local functions needed to parse the system information file.  */

#define CMDTABFN(z) \
  static enum tcmdtabret z P((int, char **, pointer, const char *))

CMDTABFN (ticlear);
CMDTABFN (tisystem);
CMDTABFN (tialias);
CMDTABFN (tialternate);
CMDTABFN (titime);
CMDTABFN (titimegrade);
CMDTABFN (ticall_local_size);
CMDTABFN (ticall_remote_size);
CMDTABFN (ticalled_local_size);
CMDTABFN (ticalled_remote_size);
CMDTABFN (titimetable);
CMDTABFN (tiport);
CMDTABFN (tichat);
CMDTABFN (ticalled_login);
CMDTABFN (tiproto_param);
CMDTABFN (tirequest);
CMDTABFN (titransfer);

#undef CMDTABFN

/* The commands accepted from the system information file.  */

static const struct scmdtab asIcmds[] =
{
  { "#", CMDTABTYPE_FN | 1, NULL, ticlear },
  { "system", CMDTABTYPE_FN | 2, NULL, tisystem },
  { "alias", CMDTABTYPE_FN | 2, NULL, tialias },
  { "alternate", CMDTABTYPE_FN | 0, NULL, tialternate },
  { "default-alternates", CMDTABTYPE_BOOLEAN,
      (pointer) &fIdefault_alternates, NULL },
  { "time", CMDTABTYPE_FN | 0, NULL, titime },
  { "timegrade", CMDTABTYPE_FN | 0, (pointer) &sIhold.ztime, titimegrade },
  { "call-timegrade", CMDTABTYPE_FN | 3, (pointer) &sIhold.zcalltimegrade,
      titimegrade },
  { "call-local-size", CMDTABTYPE_FN | 3, NULL, ticall_local_size },
  { "call-remote-size", CMDTABTYPE_FN | 3, NULL, ticall_remote_size },
  { "called-local-size", CMDTABTYPE_FN | 3, NULL, ticalled_local_size },
  { "called-remote-size", CMDTABTYPE_FN | 3, NULL, ticalled_remote_size },
  { "timetable", CMDTABTYPE_FN | 3, NULL, titimetable },
  { "baud", CMDTABTYPE_LONG, (pointer) &sIhold.ibaud, NULL },
  { "speed", CMDTABTYPE_LONG, (pointer) &sIhold.ibaud, NULL },
  { "port", CMDTABTYPE_FN | 0, NULL, tiport },
  { "phone", CMDTABTYPE_STRING, (pointer) &sIhold.zphone, NULL },
  { "address", CMDTABTYPE_STRING, (pointer) &sIhold.zphone, NULL },
  { "chat", CMDTABTYPE_PREFIX | 0, (pointer) &sIhold.schat, tichat },
  { "call-login", CMDTABTYPE_STRING, (pointer) &sIhold.zcall_login, NULL },
  { "call-password", CMDTABTYPE_STRING, (pointer) &sIhold.zcall_password,
      NULL },
  { "called-login", CMDTABTYPE_FN | 0, NULL, ticalled_login },
  { "callback", CMDTABTYPE_BOOLEAN, (pointer) &sIhold.fcallback, NULL },
  { "sequence", CMDTABTYPE_BOOLEAN, (pointer) &sIhold.fsequence, NULL },
  { "protocol", CMDTABTYPE_STRING, (pointer) &sIhold.zprotocols, NULL },
  { "protocol-parameter", CMDTABTYPE_FN | 0, NULL, tiproto_param },
  { "called-chat", CMDTABTYPE_PREFIX | 0, (pointer) &sIhold.scalled_chat,
      tichat },
#if DEBUG > 1
  { "debug", CMDTABTYPE_FN | 0, (pointer) &sIhold.idebug, tidebug_parse },
  { "max-remote-debug", CMDTABTYPE_FN | 0,
      (pointer) &sIhold.imax_remote_debug, tidebug_parse },
#endif
  { "call-request", CMDTABTYPE_BOOLEAN, (pointer) &sIhold.fcall_request,
      NULL },
  { "called-request", CMDTABTYPE_BOOLEAN, (pointer) &sIhold.fcalled_request,
      NULL },
  { "request", CMDTABTYPE_FN | 2, NULL, tirequest },
  { "call-transfer", CMDTABTYPE_BOOLEAN, (pointer) &sIhold.fcall_transfer,
      NULL },
  { "called-transfer", CMDTABTYPE_BOOLEAN,
      (pointer) &sIhold.fcalled_transfer, NULL },
  { "transfer", CMDTABTYPE_FN | 2, NULL, titransfer },
  { "local-send", CMDTABTYPE_FULLSTRING, (pointer) &sIhold.zlocal_send,
      NULL },
  { "remote-send", CMDTABTYPE_FULLSTRING, (pointer) &sIhold.zremote_send,
      NULL },
  { "local-receive", CMDTABTYPE_FULLSTRING,
      (pointer) &sIhold.zlocal_receive, NULL },
  { "remote-receive", CMDTABTYPE_FULLSTRING,
      (pointer) &sIhold.zremote_receive, NULL },
  { "command-path", CMDTABTYPE_FULLSTRING, (pointer) &sIhold.zpath, NULL },
  { "commands", CMDTABTYPE_FULLSTRING, (pointer) &sIhold.zcmds, NULL },
  { "free-space", CMDTABTYPE_LONG, (pointer) &sIhold.cfree_space, NULL },
  { "forwardto", CMDTABTYPE_FULLSTRING, (pointer) &sIhold.zforwardto, NULL },
  { "forward-to", CMDTABTYPE_FULLSTRING, (pointer) &sIhold.zforwardto, NULL },
  { "pubdir", CMDTABTYPE_STRING, (pointer) &sIhold.zpubdir, NULL },
  { "myname", CMDTABTYPE_STRING, (pointer) &sIhold.zlocalname, NULL },
  { NULL, 0, NULL, NULL }
};

/* This is called for the first line of each file.  It clears the
   defaults, so that each file has a different set.  */

/*ARGSUSED*/
static enum tcmdtabret
ticlear (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  fIfirst = TRUE;
  return CMDTABRET_FREE_AND_EXIT;
}

/* Process the system command.  We store away the system name and exit
   out of processing commands to let the main loop handle it.  */

/*ARGSUSED*/
static enum tcmdtabret
tisystem (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  const char *z;

  /* System names may only contain alphanumeric characters,
     underscores, dashes and dots, and they may not begin with a dot,
     at least for now.  */
  for (z = argv[1]; *z != '\0'; z++)
    {
      if (! isalnum (BUCHAR (*z))
	  && *z != '_'
	  && *z != '-'
	  && (*z != '.' || z == argv[1]))
	{
	  ulog (LOG_ERROR, "%s: %s %s: Illegal character in system name",
		zerr, argv[0], argv[1]);
	  return CMDTABRET_FREE;
	}
    }

  if (strlen (argv[1]) > cSysdep_max_name_len)
    {
      ulog (LOG_ERROR, "System name \"%s\" too long (max %d)", argv[1],
	    cSysdep_max_name_len);
      return CMDTABRET_FREE;
    }

  zInext_system = argv[1];

  DEBUG_MESSAGE1 (DEBUG_CONFIG,
		  "tisystem: Reading system %s", zInext_system);

  return CMDTABRET_EXIT;
}

/* Process the alias command.  */

/*ARGSUSED*/
static enum tcmdtabret
tialias (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  if (fIclear_alias)
    {
      sIhold.zalias = NULL;
      fIclear_alias = FALSE;
    }
  uadd_string (&sIhold.zalias, argv[1], ' ');
  return CMDTABRET_FREE;
}

/* Process the alternate command.  The current information is in
   sIhold.  We link this onto a chain of alternates starting at
   sIalternate.  We then set up sIhold with the defaults for the next
   alternate.  qIalternates holds the list of alternates for the
   file-wide defaults, and we use to set up sIhold.  We also call
   uiset_clear to set all the fIclear_* variables so that commands
   like ``time'' know that they should ignore any existing entry.  */

static struct ssysteminfo sIalternate;

/*ARGSUSED*/
static enum tcmdtabret
tialternate (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  if (fIclear_alternate)
    {
      sIalternate = sIhold;
      fIclear_alternate = FALSE;
    }
  else
    {
      struct ssysteminfo *qnew;
      struct ssysteminfo **pq;

      qnew = (struct ssysteminfo *) xmalloc (sizeof (struct ssysteminfo));
      *qnew = sIhold;
      for (pq = &sIalternate.qalternate; *pq != NULL; pq = &(*pq)->qalternate)
	;
      *pq = qnew;
      sIhold = sIalternate;
      sIhold.qalternate = NULL;
    }

  /* Clear the name of the next alternate.  */
  sIhold.zalternate = NULL;

  /* Now, if there is a default alternate to base this on, we must
     override everything not changed before the first ``alternate''
     command to the default alternate.  */
  if (fIdefault_alternates
      && qIalternates != NULL)
    {
      if (qIalternates->zalternate != NULL)
	sIhold.zalternate = qIalternates->zalternate;

#define TEST(x) \
      if (sIhold.x == sIdefault.x) \
	sIhold.x = qIalternates->x;

      TEST (ztime);
      TEST (zcalltimegrade);
      TEST (zcall_local_size);
      TEST (zcall_remote_size);
      TEST (zcalled_local_size);
      TEST (zcalled_remote_size);
      TEST (ibaud);
      TEST (zport);
      TEST (qport);
      TEST (zphone);
      TEST (schat.zchat);
      TEST (schat.zprogram);
      TEST (schat.ctimeout);
      TEST (schat.zfail);
      TEST (zcall_login);
      TEST (zcalled_login);
      TEST (fcallback);
      TEST (zprotocols);
      TEST (cproto_params);
      TEST (qproto_params);
      TEST (scalled_chat.zchat);
      TEST (scalled_chat.zprogram);
      TEST (scalled_chat.ctimeout);
      TEST (scalled_chat.zfail);
#if DEBUG > 1
      TEST (idebug);
      TEST (imax_remote_debug);
#endif
      TEST (fcall_request);
      TEST (fcalled_request);
      TEST (fcall_transfer);
      TEST (fcalled_transfer);
      TEST (zlocal_send);
      TEST (zcalled_local_send);
      TEST (zremote_send);
      TEST (zcalled_remote_send);
      TEST (zlocal_receive);
      TEST (zcalled_local_receive);
      TEST (zremote_receive);
      TEST (zcalled_remote_receive);
      TEST (zpath);
      TEST (zcmds);
      TEST (cfree_space);
      TEST (zforwardto);
      TEST (zpubdir);
      TEST (zlocalname);

#undef TEST

      qIalternates = qIalternates->qalternate;
    }

  /* If there is a name for this alternate, put it in.  */
  if (argc > 1)
    sIhold.zalternate = xstrdup (argv[1]);

  uiset_clear (TRUE);

  return CMDTABRET_FREE;
}
      
/* Process the time command.  */

/*ARGSUSED*/
static enum tcmdtabret
titime (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  char *pznew[4];
  char ab[2];

  pznew[0] = argv[0];
  ab[0] = 'z';
  ab[1] = '\0';
  pznew[1] = ab;
  pznew[2] = argv[1];
  if (argc > 2)
    pznew[3] = argv[2];

  return titimegrade (argc + 1, pznew, (pointer) &sIhold.ztime, zerr);
}

/* Process the timegrade and the call-timegrade commands.  */

static enum tcmdtabret
titimegrade (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  char **pztime = (char **) pvar;
  int clen;
  char *z;

  if (argc < 3 || argc > 4)
    {
      ulog (LOG_ERROR, "%s: %s: Wrong number of arguments", zerr, argv[0]);
      return CMDTABRET_FREE;
    }

  if (argv[1][1] != '\0' || ! FGRADE_LEGAL (argv[1][0]))
    {
      ulog (LOG_ERROR, "%s: %s: Illegal grade '%s'", zerr, argv[0],
	    argv[1]);
      return CMDTABRET_FREE;
    }

  /* We should probably check whether the time string is legal.  A
     timegrade string is a single character grade, then a time string,
     then an optional semicolon and a retry time.  */

  clen = strlen (argv[2]) + 2;
  if (argc > 3)
    clen += strlen (argv[3]) + 1;
  z = (char *) alloca (clen);
  *z = argv[1][0];
  strcpy (z + 1, argv[2]);
  if (argc > 3)
    {
      strcat (z, ";");
      strcat (z, argv[3]);
    }

  if (pztime == &sIhold.ztime)
    {
      if (fIclear_time)
	{
	  *pztime = NULL;
	  fIclear_time = FALSE;
	}
    }
  else
    {
      if (fIclear_calltimegrade)
	{
	  *pztime = NULL;
	  fIclear_calltimegrade = FALSE;
	}
    }

  uadd_string (pztime, z, ' ');

  return CMDTABRET_FREE;
}

/* Add a size command.  */

static enum tcmdtabret tiadd_size P((int argc, char **argv,
				     const char *zerr, boolean *pf,
				     char **pz));

static enum tcmdtabret
tiadd_size (argc, argv, zerr, pf, pz)
     int argc;
     char **argv;
     const char *zerr;
     boolean *pf;
     char **pz;
{
  long cbytes;
  char *zend;
  char *zarg;

  cbytes = strtol (argv[1], &zend, 10);
  if (*zend != '\0')
    {
      ulog (LOG_ERROR, "%s: %s: Bad number", zerr, argv[0]);
      return CMDTABRET_FREE;
    }

  /* We should check the legality of the time string here.  */

  if (*pf)
    {
      *pz = NULL;
      *pf = FALSE;
    }

  zarg = (char *) alloca (strlen (argv[2]) + 20);
  sprintf (zarg, "%ld %s", cbytes, argv[2]);

  uadd_string (pz, zarg, ' ');

  return CMDTABRET_FREE;
}

/* Process the call-local-size command.  */

/*ARGSUSED*/
static enum tcmdtabret
ticall_local_size (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  return tiadd_size (argc, argv, zerr, &fIclear_call_local_size,
		     &sIhold.zcall_local_size);
}

/* Process the call-remote-size command.  */

/*ARGSUSED*/
static enum tcmdtabret
ticall_remote_size (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  return tiadd_size (argc, argv, zerr, &fIclear_call_remote_size,
		     &sIhold.zcall_remote_size);
}

/* Process the called-local-size command.  */

/*ARGSUSED*/
static enum tcmdtabret
ticalled_local_size (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  return tiadd_size (argc, argv, zerr, &fIclear_called_local_size,
		     &sIhold.zcalled_local_size);
}

/* Process the called-remote-size command.  */

/*ARGSUSED*/
static enum tcmdtabret
ticalled_remote_size (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  return tiadd_size (argc, argv, zerr, &fIclear_called_remote_size,
		     &sIhold.zcalled_remote_size);
}

/* Process the timetable command.  */

/*ARGSUSED*/
static enum tcmdtabret
titimetable (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  uaddtimetable (argv[1], argv[2]);
  return CMDTABRET_CONTINUE;
}

/* Process the port command.  */

/*ARGSUSED*/
static enum tcmdtabret
tiport (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  if (argc < 2)
    {
      ulog (LOG_ERROR, "%s: %s: Wrong number of arguments", zerr, argv[0]);
      return CMDTABRET_FREE;
    }
  
  if (fIclear_port)
    {
      sIhold.zport = NULL;
      sIhold.qport = NULL;
      fIclear_port = FALSE;
    }

  if (argc > 2)
    {
      enum tcmdtabret tret;

      if (sIhold.zport != NULL)
	{
	  ulog (LOG_ERROR,
		"%s: %s: Ignoring port specification following port name",
		zerr, argv[0]);
	  return CMDTABRET_FREE;
	}
      tret = tprocess_port_cmd (argc - 1, argv + 1, (pointer) &sIhold.qport,
				zerr);
      if (sIhold.qport != NULL && sIhold.qport->zname == NULL)
	{
	  char *zname;

	  if (sIhold.zname == NULL)
	    sIhold.qport->zname = "default system file port";
	  else
	    {
	      zname = (char *) alloca (strlen (sIhold.zname)
				       + sizeof "system  port");
	      sprintf (zname, "system %s port", sIhold.zname);
	      sIhold.qport->zname = xstrdup (zname);
	    }
	}	

#if DEBUG > 1
      if (sIhold.qport != NULL)
	DEBUG_MESSAGE2 (DEBUG_CONFIG,
			"tiport: Command %s to port %s", argv[1],
			sIhold.qport->zname);
#endif

      return tret;
    }
  else
    {
      if (sIhold.qport != NULL)
	{
	  ulog (LOG_ERROR,
		"%s: %s: Ignoring port name following port specification",
		zerr, argv[0]);
	  return CMDTABRET_FREE;
	}    
      
      sIhold.zport = argv[1];

      return CMDTABRET_CONTINUE;
    }
}

/* Process one of the chat commands.  We have a special version for
   systems just so that we clear out the chat failure strings.  It
   would be nice if there were a cleaner way to do this.  */

static enum tcmdtabret
tichat (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  if (strcmp (argv[0], "chat-fail") == 0)
    {
      if (fIclear_chat_fail)
	{
	  sIhold.schat.zfail = NULL;
	  fIclear_chat_fail = FALSE;
	}
    }
  else if (strcmp (argv[0], "called-chat-fail") == 0)
    {
      if (fIclear_called_chat_fail)
	{
	  sIhold.scalled_chat.zfail = NULL;
	  fIclear_called_chat_fail = FALSE;
	}
    }

  return tprocess_chat_cmd (argc, argv, pvar, zerr);
}

/* Process the called-login command.  */

/*ARGSUSED*/
static enum tcmdtabret
ticalled_login (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  if (argc < 2)
    {
      ulog (LOG_ERROR, "%s: %s: Wrong number of arguments", zerr, argv[0]);
      return CMDTABRET_FREE;
    }

  sIhold.zcalled_login = argv[1];

  if (argc > 2)
    uadd_validate (argv[1], argc - 2, (const char **) argv + 2);

  return CMDTABRET_CONTINUE;
}

/* Process the protocol parameter command.  */

/*ARGSUSED*/
static enum tcmdtabret
tiproto_param (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  if (fIclear_proto_param)
    {
      sIhold.cproto_params = 0;
      sIhold.qproto_params = NULL;
      fIclear_proto_param = FALSE;
    }

  return tadd_proto_param (&sIhold.cproto_params, &sIhold.qproto_params,
			   zerr, argc - 1, argv + 1);
}

/* Process the request command.  */

/*ARGSUSED*/
static enum tcmdtabret
tirequest (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  char b;
  boolean fset;

  b = argv[1][0];
  if (b == 'y' || b == 'Y' || b == 't' || b == 'T')
    fset = TRUE;
  else if (b == 'n' || b == 'N' || b == 'f' || b == 'F')
    fset = FALSE;
  else
    {
      ulog (LOG_ERROR, "%s: %s: %s: Bad boolean", zerr, argv[0], argv[1]);
      return CMDTABRET_FREE;
    }

  sIhold.fcall_request = fset;
  sIhold.fcalled_request = fset;

  return CMDTABRET_FREE;
}

/* Process the transfer command.  */

/*ARGSUSED*/
static enum tcmdtabret
titransfer (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  char b;
  boolean fset;

  b = argv[1][0];
  if (b == 'y' || b == 'Y' || b == 't' || b == 'T')
    fset = TRUE;
  else if (b == 'n' || b == 'N' || b == 'f' || b == 'F')
    fset = FALSE;
  else
    {
      ulog (LOG_ERROR, "%s: %s: %s: Bad boolean", zerr, argv[0], argv[1]);
      return CMDTABRET_FREE;
    }

  sIhold.fcall_transfer = fset;
  sIhold.fcalled_transfer = fset;

  return CMDTABRET_FREE;
}

/* Mark all the contents of sIhold to be cleared before they are set.
   If the falternate argument is TRUE, then only prepare to clear
   those contents that should be cleared for an alternate.  */

static void
uiset_clear (falternate)
     boolean falternate;
{
  if (! falternate)
    {
      fIclear_alias = TRUE;
      fIclear_alternate = TRUE;
    }
  fIclear_time = TRUE;
  fIclear_calltimegrade = TRUE;
  fIclear_call_local_size = TRUE;
  fIclear_call_remote_size = TRUE;
  fIclear_called_local_size = TRUE;
  fIclear_called_remote_size = TRUE;
  fIclear_port = TRUE;
  fIclear_chat_fail = TRUE;
  fIclear_proto_param = TRUE;
  fIclear_called_chat_fail = TRUE;
}

#endif /* HAVE_TAYLOR_CONFIG */

/* Set up the default values advertised in the documentation.  */

void
uset_system_defaults (qsys)
     struct ssysteminfo *qsys;
{
  qsys->zname = NULL;
  qsys->zalias = NULL;
  qsys->qalternate = NULL;
  qsys->zalternate = NULL;
  qsys->ztime = xmalloc (1 + sizeof "Never");
  sprintf (qsys->ztime, "%cNever", BGRADE_LOW);
  qsys->zcalltimegrade = NULL;
  qsys->zcall_local_size = NULL;
  qsys->zcall_remote_size = NULL;
  qsys->zcalled_local_size = NULL;
  qsys->zcalled_remote_size = NULL;
  qsys->ibaud = 0L;
  qsys->ihighbaud = 0L;
  qsys->zport = NULL;
  qsys->zphone = NULL;
  qsys->qport = NULL;
  INIT_CHAT (&qsys->schat);
  qsys->schat.zchat =
    (char *) "\"\" \\r\\c ogin:-BREAK-ogin:-BREAK-ogin: \\L word: \\P";
  qsys->schat.ctimeout = 10;
  qsys->zcall_login = NULL;
  qsys->zcall_password = NULL;
  qsys->zcalled_login = NULL;
  qsys->fcallback = FALSE;
  qsys->fsequence = FALSE;
  qsys->zprotocols = NULL;
  qsys->cproto_params = 0;
  qsys->qproto_params = NULL;
  INIT_CHAT (&qsys->scalled_chat);
#if DEBUG > 1
  qsys->idebug = 0;
  qsys->imax_remote_debug = DEBUG_ABNORMAL | DEBUG_CHAT | DEBUG_HANDSHAKE;
#endif
  qsys->fcall_request = TRUE;
  qsys->fcalled_request = TRUE;
  qsys->fcall_transfer = TRUE;
  qsys->fcalled_transfer = TRUE;
  qsys->zlocal_send = ZROOTDIR;
  qsys->zcalled_local_send = NULL;
  qsys->zremote_send = "~";
  qsys->zcalled_remote_send = NULL;
  qsys->zlocal_receive = "~";
  qsys->zcalled_local_receive = NULL;
  qsys->zremote_receive = "~";
  qsys->zcalled_remote_receive = NULL;
  qsys->zpath = CMDPATH;
  qsys->zcmds = "rnews rmail";
  qsys->cfree_space = DEFAULT_FREE_SPACE;
  qsys->zforwardto = NULL;
  qsys->zpubdir = NULL;
  qsys->zlocalname = NULL;
}

/* Variables to store the loaded system information.  */

static boolean fIhave_systems;
static int cIsystems;
static struct ssysteminfo *pasIsystems;

/* Read information about all systems.  */

static void uiread_systems P((void));

static void
uiread_systems ()
{
  if (fIhave_systems)
    return;

  fIhave_systems = TRUE;

#if HAVE_TAYLOR_CONFIG
  if (zSysfile == NULL)
    {
      boolean fmore;

      /* Only warn about a missing file if we aren't going to read the
	 V2 or BNU files.  */
      fmore = FALSE;
#if HAVE_V2_CONFIG
      if (fV2)
	fmore = TRUE;
#endif
#if HAVE_BNU_CONFIG
      if (fBnu)
	fmore = TRUE;
#endif
      if (! fmore)
	{
	  ulog (LOG_ERROR, "%s%s: file not found", NEWCONFIGLIB,
		SYSFILE);
	  return;
	}
    }
  else
    {
      struct smulti_file *qmulti;
      int calc;
      boolean fdefaults, fsystem;
      struct ssysteminfo *qalternates;

      qmulti = qmulti_open (zSysfile);
      if (qmulti != NULL)
	{
	  calc = 0;
	  fdefaults = FALSE;
	  fsystem = FALSE;
	  qalternates = NULL;

	  fIfirst = FALSE;
	  zInext_system = NULL;

	  while (TRUE)
	    {
	      qIalternates = qalternates;
	      fIdefault_alternates = TRUE;

	      /* Read commands.  This will exit when it encounters the
		 start of a file (which it will when we first start
		 reading) and when it reads a ``system'' command.  */
	      uprocesscmds ((FILE *) NULL, qmulti, asIcmds,
			    (const char *) NULL,
			    CMDFLAG_WARNUNRECOG | CMDFLAG_BACKSLASH);

	      /* The handling of alternates can get complex.  Before
		 we start reading a system, fIclear_alternate is set
		 to TRUE (this is done in uiset_clear).  After we have
		 finished reading a system, then if fIclear_alternate
		 is still TRUE then no ``alternate'' command was used
		 and the system information is in sIhold.  Otherwise,
		 if fIclear_alternate is FALSE, an ``alternate''
		 command was used and we must call tialternate one
		 more time; after this final call to tialternate, the
		 system information will be in sIalternate.

		 The final call to tialternate is needed because each
		 occurrence of the ``alternate'' command links the
		 previous alternate into sIalternate and sets up
		 sIhold with the defaults for the next alternate.  The
		 final call will link the last alternate into
		 sIalternate.  */

	      if (fdefaults)
		{
		  /* We were reading default information.  Save it.  */
		  if (fIclear_alternate)
		    sIdefault = sIhold;
		  else
		    {
		      (void) tialternate (0, (char **) NULL,
					  (pointer) NULL, "alternate");
		      sIdefault = sIalternate;
		    }
		  qalternates = sIdefault.qalternate;
		  sIdefault.qalternate = NULL;
		  fdefaults = FALSE;
		}
	      else if (fsystem)
		{
		  /* We just either finished a file or encountered a
		     ``system'' command after we had started reading a
		     system.  Finish up the information for the system
		     we were reading.  */
		  if (cIsystems >= calc)
		    {
		      calc += 10;
		      pasIsystems =
			((struct ssysteminfo *)
			 xrealloc ((pointer) pasIsystems,
				   calc * sizeof (struct ssysteminfo)));
		    }

		  /* We must now attach any remaining default
		     alternates.  */
		  if (fIdefault_alternates)
		    {
		      while (qIalternates != NULL)
			(void) tialternate (0, (char **) NULL,
					    (pointer) NULL, "alternate");
		    }

		  if (fIclear_alternate)
		    pasIsystems[cIsystems] = sIhold;
		  else
		    {
		      (void) tialternate (0, (char **) NULL,
					  (pointer) NULL, "alternate");
		      pasIsystems[cIsystems] = sIalternate;
		    }

		  ++cIsystems;

		  fsystem = FALSE;
		}

	      if (fIfirst)
		{
		  /* We just started reading a new file.  Reset the
		     default information.  The next time around the
		     loop we will read the default information.  */
		  uset_system_defaults (&sIhold);
		  uiset_clear (FALSE);
		  qalternates = NULL;
		  fdefaults = TRUE;
		  fIfirst = FALSE;
		}
	      else if (zInext_system != NULL)
		{
		  /* We just encountered a ``system'' command.  Save
		     the name, reset the system information to the
		     defaults, and go on to read the system
		     information.  */
		  sIhold = sIdefault;
		  sIhold.zname = zInext_system;
		  uiset_clear (FALSE);
		  fsystem = TRUE;
		  zInext_system = NULL;
		}	  
	      else
		{
		  /* We have reached the end of the files to read.  */
		  break;
		}
	    }

	  (void) fmulti_close (qmulti);
	}
    }
#endif /* HAVE_TAYLOR_CONFIG */

#if HAVE_V2_CONFIG
  if (fV2)
    {
      int cv2;
      struct ssysteminfo *pasv2;

      uv2_read_systems (&cv2, &pasv2);
      if (cv2 > 0)
	{
	  pasIsystems =
	    ((struct ssysteminfo *)
	     xrealloc ((pointer) pasIsystems,
		       (cIsystems + cv2) * sizeof (struct ssysteminfo)));
	  memcpy (pasIsystems + cIsystems, pasv2,
		  cv2 * sizeof (struct ssysteminfo));
	  cIsystems += cv2;
	}
    }
#endif /* HAVE_V2_CONFIG */

#if HAVE_BNU_CONFIG
  if (fBnu)
    {
      int cbnu;
      struct ssysteminfo *pasbnu;

      ubnu_read_systems (&cbnu, &pasbnu);
      if (cbnu > 0)
	{
	  pasIsystems =
	    ((struct ssysteminfo *)
	     xrealloc ((pointer) pasIsystems,
		       (cIsystems + cbnu) * sizeof (struct ssysteminfo)));
	  memcpy (pasIsystems + cIsystems, pasbnu,
		  cbnu * sizeof (struct ssysteminfo));
	  cIsystems += cbnu;
	}
    }
#endif /* HAVE_BNU_CONFIG */
}

/* Get information about all systems.  */

void
uread_all_system_info (pc, ppas)
     int *pc;
     struct ssysteminfo **ppas;
{
  if (! fIhave_systems)
    uiread_systems ();

  *pc = cIsystems;
  *ppas = pasIsystems;
}

/* Get information about a specific system.  */

boolean
fread_system_info (zsystem, qsys)
     const char *zsystem;
     struct ssysteminfo *qsys;
{
  int i;

  DEBUG_MESSAGE1 (DEBUG_CONFIG,
		  "fread_system_info: Reading information for system %s",
		  zsystem);

  if (! fIhave_systems)
    uiread_systems ();

  for (i = 0; i < cIsystems; i++)
    {
      char *z;

      if (strcmp (zsystem, pasIsystems[i].zname) == 0)
	{
	  *qsys = pasIsystems[i];

	  DEBUG_MESSAGE1 (DEBUG_CONFIG,
			  "fread_system_info: Got information for system %s",
			  qsys->zname);

	  return TRUE;
	}
      
      z = pasIsystems[i].zalias;
      if (z == NULL)
	continue;
      while (TRUE)
	{
	  char *znext;

	  znext = z + strcspn (z, " ");
	  if (strncmp (zsystem, z, znext - z) == 0)
	    {
	      *qsys = pasIsystems[i];

	      DEBUG_MESSAGE1 (DEBUG_CONFIG,
			      "fread_system_info: Got system %s",
			      qsys->zname);

	      return TRUE;
	    }
	  z = znext;
	  if (*z == ' ')
	    ++z;
	  else
	    break;
	}
    }

  DEBUG_MESSAGE1 (DEBUG_CONFIG,
		  "fread_system_info: Could not find system %s",
		  zsystem);

  return FALSE;
}

/* Prepare to read commands defining unknown systems.  */

void
uiunknown_start ()
{
#if HAVE_TAYLOR_CONFIG
  uset_system_defaults (&sIhold);
  uiset_clear (FALSE);
#else /* ! HAVE_TAYLOR_CONFIG */
  uset_system_defaults (&sUnknown);
#endif /* ! HAVE_TAYLOR_CONFIG */
}

#if HAVE_TAYLOR_CONFIG

/* Process a command defining unknown systems.  This is actually
   called from the main configuration file, not the system file.  */

enum tcmdtabret
tiunknown (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  fUnknown_ok = TRUE;
  return tprocess_one_cmd (argc - 1, argv + 1, asIcmds, zerr,
			   CMDFLAG_WARNUNRECOG);
}

#endif /* HAVE_TAYLOR_CONFIG */

/* Finish up after all commands defining unknwon systems.  */

void
uiunknown_end ()
{
#if HAVE_TAYLOR_CONFIG
  if (fUnknown_ok)
    {
      /* Add the final alternate.  */
      if (fIclear_alternate)
	sUnknown = sIhold;
      else
	{
	  (void) tialternate (0, (char **) NULL, (pointer) NULL, "alternate");
	  sUnknown = sIalternate;
	}
    }
#endif /* HAVE_TAYLOR_CONFIG */
}

/* Initialize the local system information.  Perhaps it would be
   desirable to allow the user to customize this as well.  This is
   called after the configuration file has been read in and the system
   name has been determined.  Only a few elements of this structure
   are ever actually used, probably just zname and zremote_receive.  */

void
uisetup_localsys ()
{
  uset_system_defaults (&sLocalsys);
  sLocalsys.zname = zLocalname;
}

/* Translate a system name into something we can use locally.  This should
   be more intelligent than it is.  Right now we just truncate the name;
   if this matches the name of another system, we reject the call.  */
   
const char *
ztranslate_system (zsystem)
     const char *zsystem;
{
  char *z;
  struct ssysteminfo s;

  if (strlen (zsystem) <= cSysdep_max_name_len)
    return zsystem;
  z = (char *) xmalloc (cSysdep_max_name_len + 1);
  strncpy (z, zsystem, cSysdep_max_name_len);
  z[cSysdep_max_name_len] = '\0';
  if (fread_system_info (z, &s))
    {
      xfree ((pointer) z);
      return NULL;
    }
  return z;
}

#if HAVE_TAYLOR_CONFIG

/* Get the login name and password for a system out of the call file.
   The call file is simple a sequence of lines.  The first word on
   each line is the system name, the second word is the login name,
   and the third word is the password.  We read it using uprocesscmds,
   since it's easy.  */

struct silogpass
{
  char **pzlog;
  char **pzpass;
};

static enum tcmdtabret tilog_pass P((int argc, char **argv, pointer pvar,
				     const char *zerr));

/*ARGSUSED*/
static enum tcmdtabret
tilog_pass (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  struct silogpass *q = (struct silogpass *) pvar;

  *q->pzlog = xstrdup (argv[1]);
  *q->pzpass = xstrdup (argv[2]);
  return CMDTABRET_FREE_AND_EXIT;
}

#endif /* HAVE_TAYLOR_CONFIG */

/* Get the login name and password to use for a system.  */

boolean
fcallout_login (qsys, pzlog, pzpass)
     const struct ssysteminfo *qsys;
     char **pzlog;
     char **pzpass;
{
  *pzlog = NULL;
  *pzpass = NULL;

#if HAVE_TAYLOR_CONFIG
  {
    struct smulti_file *qmulti;
    struct scmdtab as[2];
    struct silogpass s;

    qmulti = qmulti_open (zCallfile);
    if (qmulti == NULL)
      return FALSE;

    s.pzlog = pzlog;
    s.pzpass = pzpass;

    as[0].zcmd = qsys->zname;
    as[0].itype = CMDTABTYPE_FN | 3;
    as[0].pvar = (pointer)&s;
    as[0].ptfn = tilog_pass;

    as[1].zcmd = NULL;

    uprocesscmds ((FILE *) NULL, qmulti, as, (const char *) NULL,
		  CMDFLAG_BACKSLASH);

    (void) fmulti_close (qmulti);
  }
#endif /* HAVE_TAYLOR_CONFIG */

  if (*pzlog == NULL)
    {
      ulog (LOG_ERROR, "No call out login for system %s", qsys->zname);
      return FALSE;
    }

  return TRUE;
}

#if HAVE_TAYLOR_CONFIG

/* Check whether a login name and password gathered by the UUCP program
   itself are correct.  */

static enum tcmdtabret ticheck_login P((int argc, char **argv,
					pointer pvar, const char *zerr));

/*ARGSUSED*/
static enum tcmdtabret
ticheck_login (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  char **pz = (char **) pvar;

  *pz = xstrdup (argv[1]);
  return CMDTABRET_FREE_AND_EXIT;
}

#endif /* HAVE_TAYLOR_CONFIG */

boolean
fcheck_login (zuser, zpass)
     const char *zuser;
     const char *zpass;
{
#if HAVE_TAYLOR_CONFIG
  struct smulti_file *qmulti;
  struct scmdtab as[2];
  char *zfilepass;
  boolean fok;
  
  if (zPwdfile == NULL)
    {
      ulog (LOG_ERROR, "%s%s: file not found", NEWCONFIGLIB,
	    PASSWDFILE);
      return FALSE;
    }

  qmulti = qmulti_open (zPwdfile);
  if (qmulti == NULL)
    return FALSE;

  zfilepass = NULL;

  as[0].zcmd = zuser;
  as[0].itype = CMDTABTYPE_FN | 2;
  as[0].pvar = (pointer)&zfilepass;
  as[0].ptfn = ticheck_login;

  as[1].zcmd = NULL;

  uprocesscmds ((FILE *) NULL, qmulti, as, (const char *) NULL,
		CMDFLAG_CASESIGNIFICANT | CMDFLAG_BACKSLASH);

  (void) fmulti_close (qmulti);

  fok = zfilepass != NULL && strcmp (zfilepass, zpass) == 0;

  if (zfilepass != NULL)
    {
      bzero (zfilepass, strlen (zfilepass));
      xfree ((pointer) zfilepass);
    }

  if (! fok)
    ulog (LOG_ERROR, "Bad login");

  return fok;

#else /* HAVE_TAYLOR_CONFIG */

  ulog (LOG_ERROR, "Not compiled to accept logins");
  return FALSE;

#endif /* HAVE_TAYLOR_CONFIG */
}

#if HAVE_TAYLOR_CONFIG

/* Process a chat command.  These are done as prefix commands.  We set
   it up such that argv[0] will contain the string "chat" and we must
   look after that point to see what command to execute.  */

static struct schat_info sChat;

static enum tcmdtabret tcchat_fail P((int argc, char **argv,
				      pointer pvar,
				      const char *zerr));

static const struct scmdtab asChatcmds[] =
{
  { "chat", CMDTABTYPE_FULLSTRING, (pointer) &sChat.zchat, NULL },
  { "chat-program", CMDTABTYPE_FULLSTRING, (pointer) &sChat.zprogram,
      NULL },
  { "chat-timeout", CMDTABTYPE_INT, (pointer) &sChat.ctimeout, NULL },
  { "chat-fail", CMDTABTYPE_FN | 2, NULL, tcchat_fail },
  { "chat-seven-bit", CMDTABTYPE_BOOLEAN, (pointer) &sChat.fstrip, NULL },
  { NULL, 0, NULL, NULL }
};

enum tcmdtabret
tprocess_chat_cmd (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  struct schat_info *qchat = (struct schat_info *) pvar;
  char *zchat;
  enum tcmdtabret t;

  zchat = strstr (argv[0], "chat");

#if DEBUG > 0
  if (zchat == NULL)
    ulog (LOG_FATAL, "tprocess_chat_cmd: Can't happen");
#endif

  argv[0] = zchat;

  sChat = *qchat;
  t = tprocess_one_cmd (argc, argv, asChatcmds, zerr,
			CMDFLAG_WARNUNRECOG);
  *qchat = sChat;

  return t;
}

/* Add a new chat failure string.  */

static enum tcmdtabret
tcchat_fail (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  uadd_string (&sChat.zfail, argv[1], ' ');
  return CMDTABRET_FREE;
}

#endif /* HAVE_TAYLOR_CONFIG */

#if HAVE_TAYLOR_CONFIG

/* Add a protocol parameter entry.  The pc parameter points to the
   number of protocol parameter entries, and the pq parameter points
   to the array of protocol parameters.  */

enum tcmdtabret
tadd_proto_param (pc, pq, zerr, cargs, azargs)
     int *pc;
     struct sproto_param **pq;
     const char *zerr;
     int cargs;
     char **azargs;
{
  int i;
  struct sproto_param *q;
  int ientry;
  int icopy;

  if (cargs < 1)
    {
      ulog (LOG_ERROR, "%s: protocol-parameter: Not enough arguments", zerr);
      return CMDTABRET_FREE;
    }
  if (azargs[0][1] != '\0')
    {
      ulog (LOG_ERROR,
	    "%s: protocol-parameter: Protocol names are single characters",
	    zerr);
      return CMDTABRET_FREE;
    }

  q = NULL;
  ientry = 0;

  for (i = 0; i < *pc; i++)
    {
      if ((*pq)[i].bproto == azargs[0][0])
	{
	  q = &(*pq)[i];
	  ientry = q->centries;
	  ++q->centries;
	  q->qentries =
	    ((struct sproto_param_entry *)
	     xrealloc ((pointer) q->qentries,
		       (q->centries * sizeof (struct sproto_param_entry))));
	  break;
	}
    }

  if (i >= *pc)
    {
      ++(*pc);
      *pq = ((struct sproto_param *)
	     xrealloc ((pointer) *pq,
		       (*pc * sizeof (struct sproto_param))));
      q = &(*pq)[*pc - 1];
      q->bproto = azargs[0][0];
      q->centries = 1;
      q->qentries = ((struct sproto_param_entry *)
		     xmalloc (sizeof (struct sproto_param_entry)));
      ientry = 0;
    }

  q->qentries[ientry].cargs = cargs -  1;
  q->qentries[ientry].azargs =
    (char **) xmalloc ((cargs - 1) * sizeof (char *));
  for (icopy = 0; icopy < cargs - 1; icopy++)
    q->qentries[ientry].azargs[icopy] = azargs[icopy + 1];

  return CMDTABRET_CONTINUE;
}

#endif /* HAVE_TAYLOR_CONFIG */

/* Apply some protocol parameters, given the current protocol.  */

void
uapply_proto_params (bproto, qcmds, c, pas)
     int bproto;
     struct scmdtab *qcmds;
     int c;
     struct sproto_param *pas;
{
  int i;
  struct sproto_param *q;

  for (i = 0, q = pas;
       i < c;
       i++, q++)
    {
      if (q->bproto == (char) bproto)
	{
	  char ab[sizeof "g protocol parameters"];
	  struct sproto_param_entry *qentry;
	  int ientry;

	  sprintf (ab, "%c protocol parameters", bproto);
	  q = &pas[i];
	  for (ientry = 0, qentry = &q->qentries[0];
	       ientry < q->centries;
	       ientry++, qentry++)
	    (void) tprocess_one_cmd (qentry->cargs, qentry->azargs,
				     qcmds, ab, CMDFLAG_WARNUNRECOG);
	  return;
	}
    }
}

/* Maintain a list of systems which are permitted to log in using a
   particular login name.  This is the VALIDATE entry from the BNU
   Permissions file.  */

static struct svalidate
{
  struct svalidate *qnext;
  const char *zlogname;
  int cmachines;
  const char *azmachines[1];
} *qIvalidate;

/* Add an entry to the validation list.  This assumes that it does not
   have to copy the login name or the machine names.  It does copy the
   array of machines names.  */

void
uadd_validate (zlogname, cmachines, pazmachines)
     const char *zlogname;
     int cmachines;
     const char **pazmachines;
{
  struct svalidate **pq, *q;

  for (pq = &qIvalidate; *pq != NULL; pq = &(*pq)->qnext)
    {
      if (strcmp ((*pq)->zlogname, zlogname) == 0)
	{
	  *pq = ((struct svalidate *)
		 xrealloc ((pointer) *pq,
			   sizeof (struct svalidate)
			   + (((*pq)->cmachines + cmachines - 1)
			      * sizeof (const char *))));
	  memcpy ((*pq)->azmachines + (*pq)->cmachines, pazmachines,
		  cmachines * sizeof (const char *));
	  (*pq)->cmachines += cmachines;
	  return;
	}
    }

  q = (struct svalidate *) xmalloc (sizeof (struct svalidate)
				    + ((cmachines - 1)
				       * sizeof (const char *)));
  q->qnext = qIvalidate;
  q->zlogname = zlogname;
  memcpy (q->azmachines, pazmachines,
	  cmachines * sizeof (const char *));
  q->cmachines = cmachines;
  qIvalidate = q;
}

/* Check whether a particular login name/machine name is valid.  */

boolean
fcheck_validate (zlogname, zmachine)
     const char *zlogname;
     const char *zmachine;
{
  struct svalidate *q;

  for (q = qIvalidate; q != NULL; q = q->qnext)
    {
      if (strcmp (q->zlogname, zlogname) == 0)
	{
	  int i;

	  for (i = 0; i < q->cmachines; i++)
	    if (strcmp (q->azmachines[i], zmachine) == 0)
	      return TRUE;

	  return FALSE;
	}
    }

  return TRUE;
}

/* The variables which hold the array of timetables.  */

int cTtable;
struct stimetable *pasTtable;

/* Initialize the table of timetables as advertised in the
   documentation.  */

void
uinittimetables ()
{
  pasTtable = (struct stimetable *) xmalloc (3 * sizeof (struct stimetable));
  pasTtable[0].zname = "Evening";
  pasTtable[0].ztime = "Wk1705-0755,Sa,Su";
  pasTtable[1].zname = "Night";
  pasTtable[1].ztime = "Wk2305-0755,Sa,Su2305-1655";
  pasTtable[2].zname = "NonPeak";
  pasTtable[2].ztime = "Wk1805-0655,Sa,Su";
  cTtable = 3;
}

/* Add a new timetable entry.  This assumes it can take control of the
   strings it is passed, so they must not be on the stack and if they
   have been allocated they must not be freed.  */

void
uaddtimetable (zname, ztime)
     const char *zname;
     const char *ztime;
{
  if (pasTtable == NULL)
    uinittimetables ();

  pasTtable = ((struct stimetable *)
	       xrealloc ((pointer) pasTtable,
			 (cTtable + 1) * sizeof (struct stimetable)));
  pasTtable[cTtable].zname = zname;
  pasTtable[cTtable].ztime = ztime;
  ++cTtable;
}
