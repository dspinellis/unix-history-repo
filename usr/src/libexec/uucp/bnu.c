/* bnu.c
   Read BNU configuration files.

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

   $Log: bnu.c,v $
   Revision 1.35  1992/04/06  19:49:49  ian
   Erik Forsberg: support multiple character modem classes

   Revision 1.34  1992/03/15  05:21:12  ian
   Scott Ballantyne: accept "Any" as a device speed

   Revision 1.33  1992/03/12  19:56:10  ian
   Debugging based on types rather than number

   Revision 1.32  1992/03/11  22:06:37  ian
   Marty Shannon: added max-uuxqts command

   Revision 1.31  1992/03/10  21:47:39  ian
   Added protocol command for ports

   Revision 1.30  1992/02/24  20:36:27  ian
   Roberto Biancardi: skip spaces after strtok (NULL, "")

   Revision 1.29  1992/02/24  04:58:47  ian
   Only permit files to be received into directories that are world-writeable

   Revision 1.28  1992/02/24  04:02:45  ian
   Doug Evans: WRITE only applies to remote requests

   Revision 1.27  1992/02/23  19:50:50  ian
   Handle READ and WRITE in Permissions correctly

   Revision 1.26  1992/02/14  16:45:09  ian
   This time for sure

   Revision 1.25  1992/02/14  16:43:07  ian
   Make the last patch better

   Revision 1.24  1992/02/14  16:39:31  ian
   T. William Wells: must xstrdup before calling uadd_validate

   Revision 1.23  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.22  1992/01/30  23:18:59  ian
   Michael Nolan: stupid error in ubadd_perm

   Revision 1.21  1992/01/15  07:06:29  ian
   Set configuration directory in Makefile rather than sysdep.h

   Revision 1.20  1991/12/28  03:49:23  ian
   Added HAVE_MEMFNS and HAVE_BFNS; changed uses of memset to bzero

   Revision 1.19  1991/12/17  23:14:08  ian
   T. William Wells: allow dialer complete and abort to be chat scripts

   Revision 1.18  1991/12/16  16:25:57  ian
   Mike Bernson: ignore lines beginning with whitespace

   Revision 1.17  1991/12/15  03:42:33  ian
   Added tprocess_chat_cmd for all chat commands, and added CMDTABTYPE_PREFIX

   Revision 1.16  1991/12/09  19:20:51  ian
   Arne Ludwig: devices entry can be followed by list of protocols

   Revision 1.15  1991/12/03  03:43:36  ian
   Dave Buck: time strings with grades were not parsed correctly

   Revision 1.14  1991/12/01  19:35:38  ian
   David Nugent: read V2 and BNU files by default even with TAYLOR_CONFIG

   Revision 1.13  1991/12/01  03:10:36  ian
   Niels Baggesen: accept dialers with no substitutions

   Revision 1.12  1991/12/01  03:04:20  ian
   Niels Baggesen: don't free up zline in ubadd_perm; don't even pass it in

   Revision 1.11  1991/12/01  02:44:12  ian
   Niels Baggesen: didn't handle combinations of multiple MACHINE/LOGNAME

   Revision 1.10  1991/12/01  02:31:36  ian
   Made zread and zwrite fields of sperm structure const char *

   Revision 1.9  1991/11/30  23:39:37  ian
   Marty Shannon: allow comments in Sysfiles

   Revision 1.8  1991/11/30  22:39:39  ian
   Marty Shannon: don't initialize an auto array

   Revision 1.7  1991/11/26  01:04:04  ian
   Marty Shannon: initialize ireliable for BNU and V2 configuration files

   Revision 1.6  1991/11/13  20:38:00  ian
   Added TCP port type for connections over TCP

   Revision 1.5  1991/11/11  23:47:24  ian
   Added chat-program to run a program to do a chat script

   Revision 1.4  1991/11/11  16:59:05  ian
   Eliminate fread_port_info, allow NULL pflock arg to ffind_port

   Revision 1.3  1991/11/07  18:26:13  ian
   Chip Salzenberg: can't portably take address of casted value, obviously

   Revision 1.2  1991/09/19  02:22:44  ian
   Chip Salzenberg's patch to allow ";retrytime" at the end of a time string

   Revision 1.1  1991/09/10  19:37:52  ian
   Initial revision

   */

#include "uucp.h"

#if HAVE_BNU_CONFIG

#if USE_RCS_ID
char bnu_rcsid[] = "$Id: bnu.c,v 1.35 1992/04/06 19:49:49 ian Rel $";
#endif

#include <ctype.h>

#include "port.h"
#include "sysdep.h"

/* Whether to use BNU configuration files.  */
boolean fBnu = TRUE;

/* A list of space separated file names to interpret as Systems.  */
char *zBnu_systems;

/* A list of space separated file names to interpret as Dialers.  */
char *zBnu_dialers;

/* A list of space separated file names to interpret as Devices.  */
char *zBnu_devices;

/* Local functions.  */

static void ubadd_perm P((int csystems, struct ssysteminfo *passystems,
			  boolean **paffound));
static void ubadd_perm_alternate P((struct ssysteminfo *q,
				    boolean fmachine,
				    boolean flogname));

/* Read Sysfiles to get the file names to use.  */

void
ubnu_read_sysfiles ()
{
  char ab[sizeof OLDCONFIGLIB + sizeof BNU_SYSFILES - 1];
  FILE *e;
  char *zline;

  zBnu_systems = NULL;
  zBnu_dialers = NULL;
  zBnu_devices = NULL;

  sprintf (ab, "%s%s", OLDCONFIGLIB, BNU_SYSFILES);
  e = fopen (ab, "r");
  if (e != NULL)
    {
      while ((zline = zfgets (e, TRUE)) != NULL)
	{
	  int inl;
	  char *ztok;

	  inl = strlen (zline) - 1;
	  if (zline[inl] == '\n')
	    zline[inl] = '\0';
	  if (isspace (BUCHAR (zline[0])) || zline[0] == '#')
	    {
	      xfree ((pointer) zline);
	      continue;
	    }

	  ztok = strtok (zline, " \t");
	  if (ztok == NULL)
	    {
	      xfree ((pointer) zline);
	      continue;
	    }
	  if (strncmp (ztok, "service=", sizeof "service=" - 1) != 0)
	    {
	      ulog (LOG_ERROR, "Unrecognized Sysfiles line: %s", ztok);
	      xfree ((pointer) zline);
	      continue;
	    }

	  do
	    {
	      int c;

	      c = strcspn (ztok, ":");
	      if (c == sizeof "uucico" - 1
		  && strncmp (ztok, "uucico", sizeof "uucico" - 1) == 0)
		break;
	      ztok += c;
	      if (*ztok == ':')
		++ztok;
	    }
	  while (*ztok != '\0');

	  if (*ztok == '\0')
	    {
	      xfree ((pointer) zline);
	      continue;
	    }

	  while ((ztok = strtok ((char *) NULL, " \t")) != NULL)
	    {
	      char **pz;
	      char *z;
	      boolean fend;

	      if (strncmp (ztok, "systems=", sizeof "systems=" - 1) == 0)
		{
		  pz = &zBnu_systems;
		  ztok += sizeof "systems=" - 1;
		}
	      else if (strncmp (ztok, "dialers=", sizeof "dialers=" - 1) == 0)
		{
		  pz = &zBnu_dialers;
		  ztok += sizeof "dialers=" - 1;
		}
	      else if (strncmp (ztok, "devices=", sizeof "devices=" - 1) == 0)
		{
		  pz = &zBnu_devices;
		  ztok += sizeof "devices=" - 1;
		}
	      else
		{
		  ulog (LOG_ERROR, "Unrecognized Sysfiles command: %s", ztok);
		  continue;
		}

	      /* Stick the configuration file directory in front of each
		 file.  */
	      z = ztok;
	      do
		{
		  int c;

		  c = strcspn (z, ":");
		  fend = z[c] == '\0';
		  z[c] = '\0';

		  /* Looking for a leading '/' is Unix dependent, and
		     should be changed.  */
		  if (*z == '/')
		    uadd_string (pz, z, ' ');
		  else
		    {
		      char *zdir;

		      zdir = (char *) xmalloc (sizeof OLDCONFIGLIB
					       + strlen (z));
		      sprintf (zdir, "%s%s", OLDCONFIGLIB, z);
		      uadd_string (pz, zdir, ' ');
		      xfree ((pointer) zdir);
		    }

		  z += c + 1;
		}
	      while (! fend);
	    }

	  xfree ((pointer) zline);
	}

      (void) fclose (e);
    }

  if (zBnu_systems == NULL)
    {
      zBnu_systems = (char *) xmalloc (sizeof OLDCONFIGLIB
				       + sizeof BNU_SYSTEMS - 1);
      sprintf (zBnu_systems, "%s%s", OLDCONFIGLIB, BNU_SYSTEMS);
    }
  if (zBnu_dialers == NULL)
    {
      zBnu_dialers = (char *) xmalloc (sizeof OLDCONFIGLIB
				       + sizeof BNU_DIALERS - 1);
      sprintf (zBnu_dialers, "%s%s", OLDCONFIGLIB, BNU_DIALERS);
    }
  if (zBnu_devices == NULL)
    {
      zBnu_devices = (char *) xmalloc (sizeof OLDCONFIGLIB
				       + sizeof BNU_DEVICES - 1);
      sprintf (zBnu_devices, "%s%s", OLDCONFIGLIB, BNU_DEVICES);
    }

#if ! HAVE_TAYLOR_CONFIG
  /* If we are NOT reading the new configuration files, then look for
     Maxuuxqts.  It would be more efficient to only read the file in
     uuxqt.c.  Too bad.  */
  {
    char *zmax;

    zmax = (char *) alloca (sizeof OLDCONFIGLIB +
			    sizeof BNU_MAXUUXQTS - 1);
    sprintf (zmax, "%s%s", OLDCONFIGLIB, BNU_MAXUUXQTS);
    e = fopen (zmax, "r");
    if (e != NULL)
      {
	zline = zfgets (e, FALSE);
	if (zline != NULL)
	  {
	    cMaxuuxqts = atoi (zline);
	    if (cMaxuuxqts < 0)
	      cMaxuuxqts = 0;
	    xfree ((pointer) zline);
	  }
	(void) fclose (e);
      }
  }
#endif /* ! HAVE_TAYLOR_CONFIG */
}

/* A little routine to add a grade and a time string to a system.  */

static void ubadd_time P((struct ssysteminfo *q, int bgrade,
			  const char *ztime, int cretry));

static void
ubadd_time (q, bgrade, ztime, cretry)
     struct ssysteminfo *q;
     int bgrade;
     const char *ztime;
     int cretry;
{
  char *zset;

  zset = (char *) alloca (strlen (ztime) + 20);
  if (cretry == 0)
    sprintf (zset, "%c%s", bgrade, ztime);
  else
    sprintf (zset, "%c%s;%d", bgrade, ztime, cretry);
  uadd_string (&q->ztime, zset, ' ');
}

/* These structures are used to read the Permissions file.  */

static struct sperm
{
  char *zlogname;
  char *zmachine;
  boolean frequest;
  enum { SENDFILES_YES, SENDFILES_CALL, SENDFILES_NO } tsendfiles;
  const char *zread;
  const char *zwrite;
  char *znoread;
  char *znowrite;
  boolean fcallback;
  const char *zcommands;
  char *zvalidate;
  char *zmyname;
  char *zpubdir;
} sBperm;

static enum tcmdtabret tbsendfiles P((int argc, char **argv,
				      pointer pvar, const char *zerr));

static struct scmdtab asBperm_cmds[] =
{
  { "LOGNAME", CMDTABTYPE_STRING, (pointer) &sBperm.zlogname, NULL },
  { "MACHINE", CMDTABTYPE_STRING, (pointer) &sBperm.zmachine, NULL },
  { "REQUEST", CMDTABTYPE_BOOLEAN, (pointer) &sBperm.frequest, NULL },
  { "SENDFILES", CMDTABTYPE_FN | 2, NULL, tbsendfiles },
  { "READ", CMDTABTYPE_STRING, (pointer) &sBperm.zread, NULL },
  { "WRITE", CMDTABTYPE_STRING, (pointer) &sBperm.zwrite, NULL },
  { "NOREAD", CMDTABTYPE_STRING, (pointer) &sBperm.znoread, NULL },
  { "NOWRITE", CMDTABTYPE_STRING, (pointer) &sBperm.znowrite, NULL },
  { "CALLBACK", CMDTABTYPE_BOOLEAN, (pointer) &sBperm.fcallback, NULL },
  { "COMMANDS", CMDTABTYPE_STRING, (pointer) &sBperm.zcommands, NULL },
  { "VALIDATE", CMDTABTYPE_STRING, (pointer) &sBperm.zvalidate, NULL },
  { "MYNAME", CMDTABTYPE_STRING, (pointer) &sBperm.zmyname, NULL },
  { "PUBDIR", CMDTABTYPE_STRING, (pointer) &sBperm.zpubdir, NULL },
  { NULL, 0, NULL, NULL }
};

static struct spermlist
{
  struct spermlist *qnext;
  struct sperm sperm;
} *qBothers;

/* Read BNU system information.  This means reading the Systems file
   and the Permissions file.  */

void
ubnu_read_systems (pc, ppas)
     int *pc;
     struct ssysteminfo **ppas;
{
  int calc;
  struct smulti_file *qmulti;
  char *zline;

  *pc = 0;
  *ppas = NULL;
  calc = 0;

  /* Read the Systems file(s) first.  */
  qmulti = qmulti_open (zBnu_systems);
  if (qmulti == NULL)
    return;

  while ((zline = zmulti_gets (qmulti, (boolean *) NULL,
			       (const char **) NULL, TRUE)) != NULL)
    {
      int inl;
      char *ztok;
      int i, cretry;
      struct ssysteminfo *qset;
      char *zsemi, *zslash;

      inl = strlen (zline) - 1;
      if (zline[inl] == '\n')
	zline[inl] = '\0';
      if (isspace (BUCHAR (zline[0])) || zline[0] == '#')
	{
	  xfree ((pointer) zline);
	  continue;
	}

      ztok = strtok (zline, " \t");
      if (ztok == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}

      DEBUG_MESSAGE1 (DEBUG_CONFIG,
		      "ubnu_read_systems: Reading system %s", ztok);

      /* See if we already have information for this system.  */
      for (i = 0; i < *pc; i++)
	if (strcmp ((*ppas)[i].zname, ztok) == 0)
	  break;
      if (i >= *pc)
	{
	  if (*pc >= calc)
	    {
	      calc += 10;
	      *ppas = ((struct ssysteminfo *)
		       xrealloc ((pointer) *ppas,
				 calc * sizeof (struct ssysteminfo)));
	    }
	  qset = &(*ppas)[*pc];
	  (*pc)++;
	}
      else
	{
	  struct ssysteminfo **pq;

	  for (pq = &(*ppas)[i].qalternate;
	       *pq != NULL;
	       pq = &(*pq)->qalternate)
	    ;
	  *pq = (struct ssysteminfo *) xmalloc (sizeof (struct ssysteminfo));
	  qset = *pq;
	}

      uset_system_defaults (qset);
      qset->zname = ztok;

      /* Under BNU, a local request is permitted to write into any
	 directory that is world writeable.  */
      qset->zlocal_receive = "/";

      /* Get the time string.  */
      ztok = strtok ((char *) NULL, " \t");
      if (ztok == NULL)
	continue;

      /* The time string is "time/grade,time/grade;retry".  A missing
	 grade is taken as 'z'.  */

      cretry = 0;
      zsemi = strchr (ztok, ';');
      if (zsemi != NULL)
	{
	  *zsemi = '\0';
	  cretry = atoi (zsemi + 1);
	}

      qset->ztime = NULL;
      while (TRUE)
	{
	  char *zcomma;

	  zcomma = strchr (ztok, ',');
	  if (zcomma != NULL)
	    *zcomma = '\0';

	  zslash = strchr (ztok, '/');
	  if (zslash == NULL || zslash[1] == '\0')
	    ubadd_time (qset, BGRADE_LOW, ztok, cretry);
	  else
	    {
	      *zslash = '\0';
	      ubadd_time (qset, zslash[1], ztok, cretry);
	    }

	  if (zcomma == NULL)
	    break;

	  ztok = zcomma + 1;
	}

      /* Get the devices entry.  */
      qset->zport = strtok ((char *) NULL, " \t");

      /* Get the speed entry.  If it starts with a nondigit, it's a
	 modem class.  We append it to the device name, and do the
	 same thing when reading the Devices file, since we don't need
	 to preserve the ``ACU''.  I've heard that the modem class is
	 permitted at either end, but this code doesn't support that.
	 A range of speeds is also permitted.  */
      if (qset->zport != NULL)
	{

	  /* According to Arne Ludwig, the devices entry can be
	     followed by a comma and a list of protocols.  */
	  ztok = strrchr (qset->zport, ',');
	  if (ztok != NULL && ztok[1] != '\0')
	    {
	      qset->zprotocols = ztok + 1;
	      *ztok = '\0';
	    }

	  ztok = strtok ((char *) NULL, " \t");
	  if (ztok != NULL)
	    {
	      boolean fany;

	      fany = (strcmp (ztok, "Any") == 0
		      || strcmp (ztok, "-") == 0);

	      if (! isdigit (*ztok) && ! fany)
		{
		  int clen;
		  char *zport, *zset;

		  clen = strlen (qset->zport);
		  zport = (char *) xmalloc (clen + strlen (ztok) + 1);
		  strcpy (zport, qset->zport);
		  zset = zport + clen;
		  while (*ztok != '\0' && ! isdigit (*ztok))
		    *zset++ = *ztok++;
		  *zset = '\0';
		  qset->zport = zport;
		}

	      if (fany)
		qset->ibaud = 0;
	      else
		{
		  qset->ibaud = strtol (ztok, &ztok, 10);
		  if (*ztok == '-')
		    qset->ihighbaud = atol (ztok + 1);
		}

	      /* Get the phone number.  */
	      qset->zphone = strtok ((char *) NULL, " \t");

	      /* The rest of the line is the login script.  */
	      if (qset->zphone != NULL)
		{
		  qset->schat.zchat = strtok ((char *) NULL, "");
		  qset->schat.zchat += strspn (qset->schat.zchat, " \t");
		}
	    }
	}
    }

  (void) fmulti_close (qmulti);

  /* Now we have to read the Permissions file.  */

  {
    char abperm[sizeof OLDCONFIGLIB + sizeof BNU_PERMISSIONS - 1];
    const char * const zpubdir = PUBDIR;
    FILE *e;
    boolean *affound;

    sprintf (abperm, "%s%s", OLDCONFIGLIB, BNU_PERMISSIONS);
    e = fopen (abperm, "r");
    if (e == NULL)
      return;

    affound = (boolean *) alloca (*pc * sizeof (boolean));
    bzero (affound, *pc * sizeof (boolean));

    while ((zline = zfgets (e, TRUE)) != NULL)
      {
	int inl;
	char *ztok;
	boolean fany;

	inl = strlen (zline) - 1;
	if (zline[inl] == '\n')
	  zline[inl] = '\0';
	if (isspace (BUCHAR (zline[0])) || zline[0] == '#')
	  {
	    xfree ((pointer) zline);
	    continue;
	  }

	sBperm.zlogname = NULL;
	sBperm.zmachine = NULL;
	sBperm.frequest = FALSE;
	sBperm.tsendfiles = SENDFILES_CALL;
	sBperm.zread = zpubdir;
	sBperm.zwrite = zpubdir;
	sBperm.znoread = NULL;
	sBperm.znowrite = NULL;
	sBperm.fcallback = FALSE;
	sBperm.zcommands = "rnews rmail";
	sBperm.zvalidate = NULL;
	sBperm.zmyname = NULL;
	sBperm.zpubdir = NULL;

	fany = FALSE;

	for (ztok = strtok (zline, " \t");
	     ztok != NULL;
	     ztok = strtok ((char *) NULL, " \t"))
	  {
	    char *zeq;
	    char *azargs[2];

	    fany = TRUE;

	    zeq = strchr (ztok, '=');
	    if (zeq == NULL)
	      {
		ulog (LOG_ERROR, "Bad %s entry: %s", BNU_PERMISSIONS,
		      ztok);
		continue;
	      }
	    *zeq = '\0';

	    azargs[0] = ztok;
	    azargs[1] = zeq + 1;
	    (void) tprocess_one_cmd (2, azargs, asBperm_cmds,
				     BNU_PERMISSIONS, CMDFLAG_WARNUNRECOG);
	  }

	if (! fany)
	  continue;
	
	if (sBperm.zlogname != NULL)
	  {
	    char *zstart;
	    boolean fend;

	    /* Process each LOGNAME separately.  */
	    zstart = sBperm.zlogname;
	    do
	      {
		int c;

		c = strcspn (zstart, ":");
		fend = zstart[c] == '\0';
		zstart[c] = '\0';

		sBperm.zlogname = zstart;
		ubadd_perm (*pc, *ppas, &affound);

		zstart += c + 1;
	      }
	    while (! fend);
	  }
	else if (sBperm.zmachine != NULL)
	  ubadd_perm (*pc, *ppas, &affound);
	else
	  {
	    ulog (LOG_ERROR,
		  "%s: No MACHINE or LOGNAME entry: \"%s\"",
		  BNU_PERMISSIONS, zline);
	    xfree ((pointer) zline);
	  }
      }

    (void) fclose (e);

    /* If there were any MACHINE=OTHER entry, add the permissions for
       each machine that was not specified by name.  */

    while (qBothers != NULL)
      {
	int i;
	struct spermlist *qnext;

	sBperm = qBothers->sperm;
	for (i = 0; i < *pc; i++)
	  {
	    if (! affound[i])
	      {
		sBperm.zmachine = xstrdup ((*ppas)[i].zname);
		ubadd_perm (*pc, *ppas, &affound);
		xfree ((pointer) sBperm.zmachine);
	      }
	  }

	qnext = qBothers->qnext;
	xfree ((pointer) qBothers);
	qBothers = qnext;
      }
  }
}

/* Look up a machine and attach permissions to it.  */

static void
ubadd_perm (csystems, passystems, paffound)
     int csystems;
     struct ssysteminfo *passystems;
     boolean **paffound;
{
  if (sBperm.zmachine != NULL)
    {
      char *zcopy;
      char *ztok;

      if (strcmp (sBperm.zmachine, "OTHER") == 0)
	{
	  struct spermlist *qnew;

	  qnew = (struct spermlist *) xmalloc (sizeof (struct spermlist));
	  qnew->qnext = qBothers;
	  qnew->sperm = sBperm;
	  qBothers = qnew;
	  return;
	}

      zcopy = (char *) alloca (strlen (sBperm.zmachine) + 1);
      strcpy (zcopy, sBperm.zmachine);

      for (ztok = strtok (zcopy, ":");
	   ztok != NULL;
	   ztok = strtok ((char *) NULL, ":"))
	{
	  int i;
	  struct ssysteminfo *q;

	  for (i = 0; i < csystems; i++)
	    if (strcmp (passystems[i].zname, ztok) == 0)
	      break;
	  if (i >= csystems)
	    {
	      /* We just ignore Permissions entries for unknown
		 systems.  */
	      continue;
	    }

	  if ((*paffound)[i])
	    {
	      struct ssysteminfo **pq;

	      /* We have already handled this machine.  Make a copy of
		 the first alternate and set the login name to
		 whatever this entry wants.  Then put the new entry at
		 the end of list of alternates.  This will make
		 whatever we saw first the default.  */
	      q = ((struct ssysteminfo *)
		   xmalloc (sizeof (struct ssysteminfo)));
	      *q = passystems[i];
	      q->qalternate = NULL;
	      for (pq = &passystems[i].qalternate;
		   *pq != NULL;
		   pq = &(*pq)->qalternate)
		;
	      *pq = q;
	      q->zcalled_login = "ANY";
	      ubadd_perm_alternate (q, TRUE, sBperm.zlogname != NULL);
	    }
	  else
	    {
	      (*paffound)[i] = TRUE;
	      for (q = &passystems[i]; q != NULL; q = q->qalternate)
		ubadd_perm_alternate (q, TRUE,
				      sBperm.zlogname != NULL);
	    }
	}
    } 
  else
    {
      int i;

#if DEBUG > 0
      if (sBperm.zlogname == NULL)
	ulog (LOG_FATAL, "bnu.c: ubadd_perm: Can't happen");
#endif

      /* There was a LOGNAME= but no MACHINE=.  We must add an
	 alternate specifying this LOGNAME to all machines.  */

      for (i = 0; i < csystems; i++)
	{
	  struct ssysteminfo *q;

	  if (passystems[i].zcalled_login == NULL)
	    passystems[i].zcalled_login = "ANY";

	  q = (struct ssysteminfo *) xmalloc (sizeof (struct ssysteminfo));
	  *q = passystems[i];
	  q->qalternate = passystems[i].qalternate;
	  passystems[i].qalternate = q;

	  ubadd_perm_alternate (q, FALSE, TRUE);
	}
    }
}

/* Attach permissions to a specific alternate of a machine.  */

static void
ubadd_perm_alternate (q, fmachine, flogname)
     struct ssysteminfo *q;
     boolean fmachine;
     boolean flogname;
{
  char *z, *zset;

  if (flogname)
    {
      if (q->zcalled_login != NULL
	  && strcmp (q->zcalled_login, "ANY") != 0)
	ulog (LOG_ERROR, "%s: Can't handle two login names for one machine",
	      q->zname);
      else
	q->zcalled_login = sBperm.zlogname;
    }
  if (fmachine)
    q->fcall_request = sBperm.frequest;
  if (flogname)
    q->fcalled_request = sBperm.frequest;
  if (flogname)
    {
      q->fcall_transfer = (sBperm.tsendfiles == SENDFILES_CALL
			   || (sBperm.tsendfiles == SENDFILES_YES));
      q->fcalled_transfer = sBperm.tsendfiles == SENDFILES_YES;
    }

  zset = xstrdup (sBperm.zread);
  while ((z = strchr (zset, ':')) != NULL)
    *z = ' ';
  if (sBperm.znoread != NULL)
    {
      char *zstart;
      boolean fend;

      zstart = xstrdup (sBperm.znoread);
      do
	{
	  char *zalloc;
	  int c;

	  c = strcspn (zstart, ":");
	  fend = zstart[c] == '\0';
	  zstart[c] = '\0';

	  zalloc = (char *) xmalloc (c + 2);
	  sprintf (zalloc, "!%s", zstart);
	  uadd_string (&zset, zalloc, ' ');
	  xfree ((pointer) zalloc);

	  zstart += c + 1;
	}
      while (! fend);
    }
  if (fmachine)
    q->zremote_send = zset;
  if (flogname)
    {
      if (fmachine)
	q->zcalled_remote_send = NULL;
      else
	q->zcalled_remote_send = zset;
    }

  zset = xstrdup (sBperm.zwrite);
  while ((z = strchr (zset, ':')) != NULL)
    *z = ' ';
  if (sBperm.znowrite != NULL)
    {
      char *zstart;
      boolean fend;

      zstart = xstrdup (sBperm.znowrite);
      do
	{
	  char *zalloc;
	  int c;

	  c = strcspn (zstart, ":");
	  fend = zstart[c] == '\0';
	  zstart[c] = '\0';

	  zalloc = (char *) xmalloc (c + 2);
	  sprintf (zalloc, "!%s", zstart);
	  uadd_string (&zset, zalloc, ' ');
	  xfree ((pointer) zalloc);

	  zstart += c + 1;
	}
      while (! fend);
    }
  if (fmachine)
    q->zremote_receive = zset;
  if (flogname)
    {
      if (fmachine)
	q->zcalled_remote_receive = NULL;
      else
	q->zcalled_remote_receive = zset;
    }

  if (flogname)
    q->fcallback = sBperm.fcallback;

  if (fmachine && sBperm.zcommands != NULL)
    {
      q->zcmds = sBperm.zcommands;
      while ((z = strchr (q->zcmds, ':')) != NULL)
	*z = ' ';
    }

  if (flogname && sBperm.zvalidate != NULL)
    {
      char *zcopy;
      boolean fend;

      zcopy = xstrdup (sBperm.zvalidate);

      do
	{
	  int c;

	  c = strcspn (zcopy, ":");
	  fend = zcopy[c] == '\0';
	  zcopy[c] = '\0';

	  uadd_validate (sBperm.zlogname, 1, (const char **) &zcopy);

	  zcopy += c + 1;
	}
      while (! fend);
    }

  if (fmachine)
    q->zlocalname = sBperm.zmyname;

  /* This isn't quite right, since the BNU Permissions file can
     specify a different public directory based on whether we are
     calling out or are being called.  */
  if (sBperm.zpubdir != NULL)
    q->zpubdir = sBperm.zpubdir;
}

/* Handle SENDFILES=string, where string can be YES, NO or CALL.  We
   actually only switch off on the first letter.  */

/*ARGSUSED*/
static enum tcmdtabret
tbsendfiles (argc, argv, pvar, zerr)
     int argc;
     char **argv;
     pointer pvar;
     const char *zerr;
{
  switch (argv[1][0])
    {
    case 'C':
    case 'c':
      sBperm.tsendfiles = SENDFILES_CALL;
      break;
    case 'N':
    case 'n':
      sBperm.tsendfiles = SENDFILES_NO;
      break;
    case 'Y':
    case 'y':
      sBperm.tsendfiles = SENDFILES_YES;
      break;
    default:
      ulog (LOG_ERROR, "%s: Unrecognized SENDFILES=%s", zerr, argv[1]);
      break;
    }

  return CMDTABRET_FREE;
}

/* Find a port with a given name and baud rate in the Devices file,
   and lock it.  If found and locked, fill in the structure pointed at
   by qport.  Set *pffound to TRUE if a port was found but could not
   be locked.  */

boolean
fbnu_find_port (zname, ibaud, ihighbaud, qport, pflock, pffound)
     const char *zname;
     long ibaud;
     long ihighbaud;
     struct sport *qport;
     boolean (*pflock) P((struct sport *, boolean fin));
     boolean *pffound;
{
  struct smulti_file *qmulti;
  char *zline;

  qmulti = qmulti_open (zBnu_devices);
  if (qmulti == NULL)
    return FALSE;

  while ((zline = zmulti_gets (qmulti, (boolean *) NULL,
			       (const char **) NULL, TRUE)) != NULL)
    {
      int inl;
      char *ztok, *zportname, *zprotocols, *zdevice, *zdial_device;
      long ilow, ihigh;

      inl = strlen (zline) - 1;
      if (zline[inl] == '\n')
	zline[inl] = '\0';
      if (isspace (BUCHAR (zline[0])) || zline[0] == '#')
	{
	  xfree ((pointer) zline);
	  continue;
	}

      /* An entry in Devices is type device dial-device baud
	 dialer-token pairs.  */

      /* Get the port type.  */
      zportname = strtok (zline, " \t");
      if (zportname == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}

      /* There may be a comma separated list of protocols following
	 the port name.  */
      zprotocols = strchr (zportname, ',');
      if (zprotocols != NULL)
	{
	  *zprotocols = '\0';
	  ++zprotocols;
	}

      /* Get the device name.  */
      zdevice = strtok ((char *) NULL, " \t");
      if (zdevice == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}

      /* Some systems seem to permit ,M at the end of the device name;
	 this means to open the port with O_NDELAY and then allow
	 delays later.  We always do this anyhow, so I just ignore the
	 ,M.  There may be portability problems here.  */
      zdevice[strcspn (zdevice, ",")] = '\0';

      /* Get the dial-device.  A value of ``-'' means none.  */
      zdial_device = strtok ((char *) NULL, " \t");
      if (zdial_device == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}
      if (strcmp (zdial_device, "-") == 0)
	zdial_device = NULL;

      /* Get the speed.  */
      ztok = strtok ((char *) NULL, " \t");
      if (ztok == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}

      /* See whether we match the port name we are looking for.  If
	 the speed starts with a non-digit, it is a modem class (but
	 "-" or "Any" mean any speed).  When reading the Systems file
	 we appended the modem class to the device name, so we must
	 match appropriately.  */
      if (isdigit (*ztok)
	  || strcmp (ztok, "-") == 0
	  || strcmp (ztok, "Any") == 0)
	{
	  if (zname != NULL && strcmp (zname, zportname) != 0)
	    {
	      xfree ((pointer) zline);
	      continue;
	    }
	}
      else
	{
	  char *zclass, *zset;

	  zclass = (char *) alloca (strlen (ztok) + 1);
	  zset = zclass;
	  while (*ztok != '\0' && ! isdigit (*ztok))
	    *zset++ = *ztok++;
	  *zset = '\0';

	  if (zname != NULL)
	    {
	      int clen;

	      clen = strlen (zportname);
	      if (strncmp (zname, zportname, clen) != 0
		  || strlen (zname) <= clen
		  || strcmp (zname + clen, zclass) != 0)
		{
		  xfree ((pointer) zline);
		  continue;
		}
	    }
	}
	  
      ilow = strtol (ztok, &ztok, 10);
      if (ilow == 0)
	ihigh = 38400L;
      else
	{
	  if (*ztok == '-')
	    ihigh = atol (ztok + 1);
	  else
	    ihigh = ilow;
	}

      /* Now we must match the range ibaud to ihighbaud against to
	 range ilow to ihigh.  */
      if (ibaud != 0
	  && (ihighbaud < ilow || ibaud > ihigh))
	{
	  xfree ((pointer) zline);
	  continue;
	}

      /* We have found a matching port.  We now fill in the sport
	 structure so that we can try to lock it.  */
      if (zname != NULL)
	qport->zname = zname;
      else
	qport->zname = zportname;
      qport->zprotocols = zprotocols;
      qport->cproto_params = 0;
      qport->qproto_params = NULL;
      qport->ireliable = 0;
      qport->zlockname = NULL;
      if (strcmp (qport->zname, "Direct") == 0)
	{
	  qport->ttype = PORTTYPE_DIRECT;
	  qport->u.sdirect.zdevice = zdevice;
	  qport->u.sdirect.ibaud = ilow;
#ifdef SYSDEP_DIRECT_INIT
	  SYSDEP_DIRECT_INIT (&qport->u.sdirect.s);
#endif
	}
#if HAVE_TCP
      else if (strcmp (qport->zname, "TCP") == 0)
	{
	  qport->ttype = PORTTYPE_TCP;
	  qport->ireliable = (RELIABLE_ENDTOEND
			      | RELIABLE_RELIABLE
			      | RELIABLE_EIGHT
			      | RELIABLE_SPECIFIED);
	  qport->u.stcp.o = -1;
	  qport->u.stcp.zport = zdevice;
	}
#endif /* HAVE_TCP */
      else
	{
	  qport->ttype = PORTTYPE_MODEM;
	  qport->u.smodem.zdevice = zdevice;
	  qport->u.smodem.zdial_device = zdial_device;
	  if (ilow != ihigh)
	    qport->u.smodem.ibaud = 0;
	  else
	    qport->u.smodem.ibaud = ilow;
	  qport->u.smodem.ilowbaud = ilow;
	  qport->u.smodem.ihighbaud = ihigh;
	  qport->u.smodem.fcarrier = TRUE;
	  qport->u.smodem.zdialer = strtok ((char *) NULL, "");
	  qport->u.smodem.zdialer += strspn (qport->u.smodem.zdialer,
					     " \t");
	  qport->u.smodem.qdialer = NULL;
#ifdef SYSDEP_MODEM_INIT
	  SYSDEP_MODEM_INIT (&qport->u.smodem.s);
#endif
	}

      if (pffound != NULL)
	*pffound = TRUE;

      if (pflock != NULL
	  && ! (*pflock) (qport, FALSE))
	{
	  xfree ((pointer) zline);
	  continue;
	}

      (void) fmulti_close (qmulti);

      return TRUE;
    }

  (void) fmulti_close (qmulti);

  return FALSE;
}

/* Read dialer information from the Dialers file.  */

boolean
fbnu_read_dialer_info (zdialer, qdialer)
     const char *zdialer;
     struct sdialer *qdialer;
{
  struct smulti_file *qmulti;
  char *zline;

  qmulti = qmulti_open (zBnu_dialers);
  if (qmulti == NULL)
    return FALSE;

  while ((zline = zmulti_gets (qmulti, (boolean *) NULL,
			       (const char **) NULL, TRUE)) != NULL)
    {
      int inl;
      char *zname, *zsubs;

      inl = strlen (zline) - 1;
      if (zline[inl] == '\n')
	zline[inl] = '\0';
      if (isspace (BUCHAR (zline[0])) || zline[0] == '#')
	{
	  xfree ((pointer) zline);
	  continue;
	}

      zname = strtok (zline, " \t");
      if (zname == NULL
	  || strcmp (zname, zdialer) != 0)
	{
	  xfree ((pointer) zline);
	  continue;
	}

      /* We found the dialer we want.  Get information from it.  */

      qdialer->zname = zdialer;
      INIT_CHAT (&qdialer->schat);
      qdialer->zdialtone = ",";
      qdialer->zpause = ",";
      qdialer->fcarrier = TRUE;
      qdialer->ccarrier_wait = 60;
      qdialer->fdtr_toggle = FALSE;
      qdialer->fdtr_toggle_wait = FALSE;
      INIT_CHAT (&qdialer->scomplete);
      INIT_CHAT (&qdialer->sabort);
      qdialer->cproto_params = 0;
      qdialer->qproto_params = NULL;
      qdialer->ireliable = 0;

      zsubs = strtok ((char *) NULL, " \t");

      if (zsubs != NULL && strcmp (zsubs, "\"\"") != 0)
	{
	  char bnext;

	  bnext = *zsubs;
	  while (bnext != '\0')
	    {
	      if (zsubs[1] == '\0')
		break;
	      if (bnext == '=') 
		qdialer->zdialtone = zsubs + 1;
	      else if (bnext == '-')
		qdialer->zpause = zsubs + 1;
	      zsubs += 2;
	      bnext = *zsubs;
	      *zsubs = '\0';
	    }
	}

      if (zsubs == NULL)
	qdialer->schat.zchat = NULL;
      else
	{
	  qdialer->schat.zchat = strtok ((char *) NULL, "");
	  qdialer->schat.zchat += strspn (qdialer->schat.zchat, " \t");
	}

      (void) fmulti_close (qmulti);

      return TRUE;
    }

  (void) fmulti_close (qmulti);

  return FALSE;
}

#endif /* HAVE_BNU_CONFIG */
