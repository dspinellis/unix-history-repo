/* v2.c
   Read V2 configuration files.  Much of this file is identical to bnu.c,
   and they really should be partially merged.

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

   $Log: v2.c,v $
   Revision 1.15  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.14  1992/03/10  21:47:39  ian
   Added protocol command for ports

   Revision 1.13  1992/03/04  02:09:36  ian
   Jeff Putsch: infinite loop when parsing time string

   Revision 1.12  1992/02/24  20:36:27  ian
   Roberto Biancardi: skip spaces after strtok (NULL, "")

   Revision 1.11  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.10  1992/01/15  07:06:29  ian
   Set configuration directory in Makefile rather than sysdep.h

   Revision 1.9  1992/01/05  04:30:45  ian
   Set chat script correctly

   Revision 1.8  1991/12/28  03:49:23  ian
   Added HAVE_MEMFNS and HAVE_BFNS; changed uses of memset to bzero

   Revision 1.7  1991/12/01  19:35:38  ian
   David Nugent: read V2 and BNU files by default even with TAYLOR_CONFIG

   Revision 1.6  1991/11/26  01:04:04  ian
   Marty Shannon: initialize ireliable for BNU and V2 configuration files

   Revision 1.5  1991/11/14  03:20:13  ian
   Added seven-bit and reliable commands to help when selecting protocols

   Revision 1.4  1991/11/13  20:38:00  ian
   Added TCP port type for connections over TCP

   Revision 1.3  1991/11/11  16:59:05  ian
   Eliminate fread_port_info, allow NULL pflock arg to ffind_port

   Revision 1.2  1991/09/19  02:22:44  ian
   Chip Salzenberg's patch to allow ";retrytime" at the end of a time string

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if HAVE_V2_CONFIG

#if USE_RCS_ID
char v2_rcsid[] = "$Id: v2.c,v 1.15 1992/03/12 19:54:43 ian Rel $";
#endif

#include <errno.h>

#include "port.h"
#include "sysdep.h"

/* Whether to use V2 configuration files.  */
boolean fV2 = TRUE;

/* A little routine to add a grade and a time string to a system.  */

static void uvadd_time P((struct ssysteminfo *q, int bgrade,
			  const char *ztime, int cretry));

static void
uvadd_time (q, bgrade, ztime, cretry)
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

/* Read V2 system information.  This means reading the L.sys file and
   the USERFILE file.  */

void
uv2_read_systems (pc, ppas)
     int *pc;
     struct ssysteminfo **ppas;
{
  int calc;
  char ab[sizeof OLDCONFIGLIB + sizeof V2_SYSTEMS - 1];
  FILE *e;
  char *zline;

  *pc = 0;
  *ppas = NULL;
  calc = 0;

  /* Read the L.sys file first.  */

  sprintf (ab, "%s%s", OLDCONFIGLIB, V2_SYSTEMS);
  e = fopen (ab, "r");
  if (e == NULL)
    {
      ulog (LOG_ERROR, "fopen (%s): %s", ab, strerror (errno));
      return;
    }

  while ((zline = zfgets (e, FALSE)) != NULL)
    {
      char *ztok;
      int i, cretry;
      struct ssysteminfo *qset;
      char *zsemi, *zslash;

      zline[strcspn (zline, "#\n")] = '\0';
      ztok = strtok (zline, " \t");
      if (ztok == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}

      DEBUG_MESSAGE1 (DEBUG_CONFIG,
		      "uv2_read_systems: Reading system %s", ztok);

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

      /* Get the time string.  */
      ztok = strtok ((char *) NULL, " \t");
      if (ztok == NULL)
	continue;

      /* The format of the time string appears to be time/grade,
	 time/grade;retry.  A missing grade is taken as 'z'.  */

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
	    uvadd_time (qset, BGRADE_LOW, ztok, cretry);
	  else
	    {
	      *zslash = '\0';
	      uvadd_time (qset, zslash[1], ztok, cretry);
	    }

	  if (zcomma == NULL)
	    break;

	  ztok = zcomma + 1;
	}

      /* Get the devices entry.  */
      qset->zport = strtok ((char *) NULL, " \t");

      if (qset->zport != NULL)
	{
	  /* Get the speed entry.  This is the baud rate to use.  */
	  ztok = strtok ((char *) NULL, " \t");

#if HAVE_TCP
	  /* If the port is "TCP", we set up a system specific port.
	     The baud rate becomes the service number and the phone
	     number becomes the address (which is still stored in
	     qsys->zphone).  */
	  if (strcmp (qset->zport, "TCP") == 0)
	    {
	      qset->zport = NULL;
	      qset->qport = ((struct sport *)
			     xmalloc (sizeof (struct sport)));
	      qset->qport->zname = "TCP";
	      qset->qport->ttype = PORTTYPE_TCP;
	      qset->qport->zprotocols = NULL;
	      qset->qport->cproto_params = 0;
	      qset->qport->qproto_params = NULL;
	      qset->qport->ireliable = (RELIABLE_ENDTOEND
					| RELIABLE_RELIABLE
					| RELIABLE_EIGHT
					| RELIABLE_SPECIFIED);
	      qset->qport->zlockname = NULL;
	      qset->qport->u.stcp.o = -1;
	      if (ztok == NULL)
		qset->qport->u.stcp.zport = "uucp";
	      else
		qset->qport->u.stcp.zport = ztok;
	    }
#endif /* HAVE_TCP */

	  if (ztok != NULL)
	    {
	      qset->ibaud = atol (ztok);

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

  (void) fclose (e);

  /* Now read USERFILE and L.cmds to get permissions.  We can't fully
     handle USERFILE since that specifies permissions based on local
     users which we do not support.  */

  {
    char abuserfile[sizeof OLDCONFIGLIB + sizeof V2_USERFILE - 1];

    sprintf (abuserfile, "%s%s", OLDCONFIGLIB, V2_USERFILE);
    e = fopen (abuserfile, "r");
    if (e == NULL)
      ulog (LOG_ERROR, "fopen (%s): %s", abuserfile, strerror (errno));
    else
      {
	boolean *affound;
	boolean fothercallback;
	char *zother;

	affound = (boolean *) alloca (*pc * sizeof (boolean));
	bzero (affound, *pc * sizeof (boolean));

	fothercallback = FALSE;
	zother = NULL;

	while ((zline = zfgets (e, FALSE)) != NULL)
	  {
	    char *ztok, *zcomma, *zfiles, *zset;
	    boolean fcallback;

	    zline[strcspn (zline, "#\n")] = '\0';

	    /* The first field is username,machinename */

	    ztok = strtok (zline, " \t");
	    if (ztok == NULL)
	      {
		xfree ((pointer) zline);
		continue;
	      }

	    zcomma = strchr (ztok, ',');
	    if (zcomma == NULL)
	      {
		xfree ((pointer) zline);
		continue;
	      }

	    *zcomma++ = '\0';

	    /* The rest of the line is the list of directories, expect
	       that if the first directory is "c" we must call the
	       system back.  */
	    zfiles = strtok ((char *) NULL, "");
	    zfiles += strspn (zfiles, " \t");
	    fcallback = FALSE;
	    if (*zfiles == 'c'
		&& (zfiles[1] == '\0'
		    || zfiles[1] == ' '
		    || zfiles[1] == '\t'))
	      {
		fcallback = TRUE;
		++zfiles;
	      }

	    zset = NULL;
	    for (zfiles = strtok (zfiles, " \t");
		 zfiles != NULL;
		 zfiles = strtok ((char *) NULL, " \t"))
	      uadd_string (&zset, zfiles, ' ');

	    if (*ztok == '\0')
	      {
		/* There is no user name.  If this is the first entry
		   with no user name, then this line specifies the
		   directories which may be sent by local request
		   for login names which are not listed otherwise.  */
		if (zother == NULL)
		  {
		    zother = zset;
		    fothercallback = fcallback;
		  }
	      }
	    else if (*zcomma == '\0')
	      {
		int i;

		/* There is no system name.  This entry specifies
		   permissions based on the login name.  We must add
		   an alternate to each system specifying what to do
		   when this login name is used.  */
		for (i = 0; i < *pc; i++)
		  {
		    struct ssysteminfo *q;

		    if ((*ppas)[i].zcalled_login != NULL
			&& strcmp ((*ppas)[i].zcalled_login, "ANY") != 0)
		      continue;

		    (*ppas)[i].zcalled_login = "ANY";

		    q = ((struct ssysteminfo *)
			 xmalloc (sizeof (struct ssysteminfo)));
		    *q = (*ppas)[i];
		    q->qalternate = (*ppas)[i].qalternate;
		    (*ppas)[i].qalternate = q;

		    q->zcalled_login = xstrdup (ztok);
		    q->fcallback = fcallback;
		    q->zremote_send = q->zremote_receive = zset;
		  }
	      }
	    else
	      {
		int i;
		struct ssysteminfo *q;

		/* Both the login name and the machine name were
		   listed; require the machine to be logged in under
		   this name.  This is not fully backward compatible,
		   and perhaps should be changed.  On the other hand,
		   it is more useful.  */

		for (i = 0; i < *pc; i++)
		  if (strcmp ((*ppas)->zname, zcomma) == 0)
		    break;
		if (i >= *pc)
		  {
		    /* We don't warn about systems which don't already
		       exist, since they may appear in sample
		       USERFILES.  */
		    xfree ((pointer) zline);
		    continue;
		  }

		affound[i] = TRUE;

		for (q = &(*ppas)[i]; q != NULL; q = q->qalternate)
		  {
		    q->zcalled_login = xstrdup (ztok);
		    q->zlocal_send = q->zlocal_receive = zset;
		    q->zremote_send = q->zremote_receive = zset;
		    q->fcallback = fcallback;
		  }
	      }

	    xfree ((pointer) zline);
	  }

	(void) fclose (e);

	if (zother != NULL)
	  {
	    int i;

	    for (i = 0; i < *pc; i++)
	      {
		struct ssysteminfo *q;

		if (affound[i])
		  continue;
		q = &(*ppas)[i];
		if (q->zcalled_login != NULL
		    && strcmp (q->zcalled_login, "ANY") != 0)
		  continue;
		q->zlocal_send = q->zlocal_receive = zother;
		q->fcallback = fothercallback;
	      }
	  }
      }
  }

  /* Now we must read L.cmds to determine which commands may be
     executed.  */

  {
    char abcmds[sizeof OLDCONFIGLIB + sizeof V2_CMDS - 1];

    sprintf (abcmds, "%s%s", OLDCONFIGLIB, V2_CMDS);
    e = fopen (abcmds, "r");
    if (e == NULL)
      ulog (LOG_ERROR, "fopen (%s): %s", abcmds, strerror (errno));
    else
      {
	char *zcmds;
	int i;
	struct ssysteminfo *q;

	zline = zfgets (e, FALSE);
	zline[strcspn (zline, "#\n")] = '\0';
	if (strncmp (zline, "PATH=", sizeof "PATH=" - 1) == 0)
	  {
	    char *z;

	    zline += sizeof "PATH=" - 1;
	    while ((z = strchr (zline, ':')) != NULL)
	      *z = ' ';
	    for (i = 0; i < *pc; i++)
	      for (q = &(*ppas)[i]; q != NULL; q = q->qalternate)
		q->zpath = zline;

	    zline = zfgets (e, FALSE);
	  }

	zcmds = NULL;
	while (zline != NULL)
	  {
	    zline[strcspn (zline, "#\n")] = '\0';
	    uadd_string (&zcmds, zline, ' ');
	    xfree ((pointer) zline);
	    zline = zfgets (e, FALSE);
	  }

	(void) fclose (e);

	for (i = 0; i < *pc; i++)
	  for (q = &(*ppas)[i]; q != NULL; q = q->qalternate)
	    q->zcmds = zcmds;
      }
  }
}

/* Find a port with a given name and speed in the file L-devices and
   lock it.  If found and locked, fill in the structure pointed at by
   qport.  Set *pffound to TRUE if a port was found but could not be
   locked.  */

boolean
fv2_find_port (zname, ibaud, ihighbaud, qport, pflock, pffound)
     const char *zname;
     long ibaud;
     long ihighbaud;
     struct sport *qport;
     boolean (*pflock) P((struct sport *, boolean fin));
     boolean *pffound;
{
  char ab[sizeof OLDCONFIGLIB + sizeof V2_DEVICES - 1];
  FILE *e;
  char *zline;

  sprintf (ab, "%s%s", OLDCONFIGLIB, V2_DEVICES);
  e = fopen (ab, "r");
  if (e == NULL)
    {
      ulog (LOG_ERROR, "fopen (%s): %s", ab, strerror (errno));
      return FALSE;
    }

  while ((zline = zfgets (e, FALSE)) != NULL)
    {
      char *ztok, *zdevice, *zdial_device;
      long ilow, ihigh;

      zline[strcspn (zline, "#\n")] = '\0';

      /* An entry in L-devices is type device dial-device baud.  */
      ztok = strtok (zline, " \t");
      if (ztok == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}

      if (zname != NULL
	  && strcmp (zname, ztok) != 0)
	{
	  xfree ((pointer) zline);
	  continue;
	}
      qport->zname = ztok;

      zdevice = strtok ((char *) NULL, " \t");
      if (zdevice == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}

      zdial_device = strtok ((char *) NULL, " \t");
      if (zdial_device == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
	}
      if (strcmp (zdial_device, "-") == 0)
	zdial_device = NULL;

      ztok = strtok ((char *) NULL, " \t");
      if (ztok == NULL)
	{
	  xfree ((pointer) zline);
	  continue;
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

      if (ibaud != 0
	  && (ihighbaud < ilow || ibaud > ihigh))
	{
	  xfree ((pointer) zline);
	  continue;
	}

      /* We have found a matching port.  We now fill in the sport
	 structure so that we can try to lock it.  */
      qport->zprotocols = NULL;
      qport->cproto_params = 0;
      qport->qproto_params = NULL;
      qport->ireliable = 0;
      qport->zlockname =  NULL;
      if (strcmp (qport->zname, "DIR") == 0)
	{
	  qport->ttype = PORTTYPE_DIRECT;
	  qport->u.sdirect.zdevice = zdevice;
	  qport->u.sdirect.ibaud = ilow;
#ifdef SYSDEP_DIRECT_INIT
	  SYSDEP_DIRECT_INIT (&qport->u.sdirect.s);
#endif
	}
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

      (void) fclose (e);

      return TRUE;
    }

  (void) fclose (e);

  return FALSE;
}

#endif /* HAVE_V2_CONFIG */
