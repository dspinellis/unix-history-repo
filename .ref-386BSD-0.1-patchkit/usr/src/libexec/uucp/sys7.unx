/* sys7.unx
   System dependent routines for uustat.

   Copyright (C) 1992 Ian Lance Taylor

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

   $Log: sys7.unx,v $
   Revision 1.11  1992/03/28  04:19:39  ian
   Niels Baggesen, Gerben Wierda: minor patches

   Revision 1.10  1992/03/16  22:22:35  ian
   Adjusted external declarations

   Revision 1.9  1992/03/15  01:54:46  ian
   All execs are now done in isspawn, all waits are done in iswait

   Revision 1.8  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.7  1992/03/11  22:21:19  ian
   Permit uucp user to delete jobs

   Revision 1.6  1992/02/27  19:53:26  ian
   Added some extern definitions

   Revision 1.5  1992/02/24  20:07:43  ian
   John Theus: some systems don't have <fcntl.h>

   Revision 1.4  1992/02/23  16:21:26  ian
   Handle systems on which NULL second argument to utime fails

   Revision 1.3  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.2  1992/02/20  04:40:07  ian
   Make sure only the submitter or the superuser can cancel a request
   
   Revision 1.1  1992/02/20  04:18:59  ian
   Initial revision
   
   */

#include "uucp.h"

#if USE_RCS_ID
char sys7_unx_rcsid[] = "$Id: sys7.unx,v 1.11 1992/03/28 04:19:39 ian Rel $";
#endif

#include <errno.h>

#if USE_STDIO && HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "system.h"
#include "sysdep.h"

#if HAVE_FCNTL_H
#include <fcntl.h>
#else
#if HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#endif

#if HAVE_OPENDIR
#if HAVE_DIRENT_H
#include <dirent.h>
#else /* ! HAVE_DIRENT_H */
#include <sys/dir.h>
#define dirent direct
#endif /* ! HAVE_DIRENT_H */
#endif /* HAVE_OPENDIR */

#ifdef UTIME_NULL_MISSING
#if HAVE_TIME_H
#include <time.h>
#endif
#endif

#if HAVE_UTIME_H
#include <utime.h>
#endif

/* External functions.  */
extern int fclose (), fflush ();
extern int read (), close (), utime (), stat ();
extern uid_t getuid (), geteuid ();

/* Local functions.  */

static boolean fskill_or_rejuv P((const char *zid, boolean fkill));

/* Set file access time to the present.  On most systems this can be a
   macro, but some apparently do not support utime correctly.  */

#ifndef UTIME_NULL_MISSING

#define ussettime(z) utime((z), (struct utimbuf *) NULL)

#else /* defined (UTIME_NULL_MISSING) */

static int
ussettime(z)
     const char *z;
{
  time_t inow;

  inow = time ((time_t *) NULL);

  {
#if HAVE_UTIME_H
    struct utimbuf s;

    s.actime = inow;
    s.modtime = inow;
    return utime (z, &s);
#else
    time_t ai[2];

    ai[0] = inow;
    ai[1] = inow;
    return utime (z, ai);
#endif
  }
}

#endif /* defined (UTIME_NULL_MISSING) */

/* Kill a job, given the jobid.  */

boolean
fsysdep_kill_job (zid)
     const char *zid;
{
  return fskill_or_rejuv (zid, TRUE);
}

/* Rejuvenate a job, given the jobid.  */

boolean
fsysdep_rejuvenate_job (zid)
     const char *zid;
{
  return fskill_or_rejuv (zid, FALSE);
}

/* Kill or rejuvenate a job, given the jobid.  */

static boolean
fskill_or_rejuv (zid, fkill)
     const char *zid;
     boolean fkill;
{
  const char *zfile;
  const char *zsys;
  char *zcopy;
  struct ssysteminfo ssys;
  const struct ssysteminfo *qsys;
  FILE *e;
  boolean fret;
  char *zline;
  int isys;

  zfile = zsjobid_to_file (zid, &zsys);
  zcopy = (char *) alloca (strlen (zfile) + 1);
  strcpy (zcopy, zfile);
  zfile = zcopy;
  zcopy = (char *) alloca (strlen (zsys) + 1);
  strcpy (zcopy, zsys);
  zsys = zcopy;

  if (fread_system_info (zsys, &ssys))
    qsys = &ssys;
  else
    {
      if (! fUnknown_ok)
	{
	  ulog (LOG_ERROR, "%s: Bad job id", zid);
	  return FALSE;
	}
      sUnknown.zname = zsys;
      qsys = &sUnknown;
    }

  e = fopen (zfile, "r");
  if (e == NULL)
    {
      if (errno == ENOENT)
	ulog (LOG_ERROR, "%s: Job not found", zid);
      else
	ulog (LOG_ERROR, "fopen (%s): %s", zfile, strerror (errno));
      return FALSE;
    }

  /* Now we have to read through the file to identify any temporary
     files.  */

  fret = TRUE;
  while ((zline = zfgets (e, FALSE)) != NULL)
    {
      struct scmd s;

      if (! fparse_cmd (zline, &s))
	{
	  ulog (LOG_ERROR, "Bad line in command file %s", zfile);
	  fret = FALSE;
	  xfree ((pointer) zline);
	  continue;
	}

      /* You are only permitted to delete a job if you submitted it or
	 if you are root or uucp.  We check for uucp by seeing if the
	 real user ID and the effective user ID are the same; this
	 works because we should be suid to uucp, so our effective
	 user ID will always be uucp while our real user ID will be
	 whoever ran the program.  */
      if (strcmp (s.zuser, zsysdep_login_name ()) != 0
	  && getuid () != 0
	  && getuid () != geteuid ())
	{
	  ulog (LOG_ERROR, "%s: Not submitted by you", zid);
	  xfree ((pointer) zline);
	  return FALSE;
	}

      if (s.bcmd == 'S')
	{
	  const char *ztemp;

	  ztemp = zsysdep_spool_file_name (qsys, s.ztemp);
	  if (ztemp == NULL)
	    fret = FALSE;
	  else
	    {
	      if (fkill)
		isys = remove (ztemp);
	      else
		isys = ussettime (ztemp);

	      if (isys != 0 && errno != ENOENT)
		{
		  ulog (LOG_ERROR, "%s (%s): %s",
			fkill ? "remove" : "utime", ztemp,
			strerror (errno));
		  fret = FALSE;
		}
	    }
	}

      xfree ((pointer) zline);
    }

  (void) fclose (e);

  if (fkill)
    isys = remove (zfile);
  else
    isys = ussettime (zfile);

  if (isys != 0 && errno != ENOENT)
    {
      ulog (LOG_ERROR, "%s (%s): %s", fkill ? "remove" : "utime",
	    zfile, strerror (errno));
      fret = FALSE;
    }

  return fret;
}

/* Get the time a work job was queued.  */

long
isysdep_work_time (qsys, pseq)
     const struct ssysteminfo *qsys;
     pointer pseq;
{
  return isysdep_file_time (zsjobid_to_file (zsysdep_jobid (qsys, pseq),
					     (const char **) NULL));
}

/* Get the time a file was created (actually, the time it was last
   modified).  */

long
isysdep_file_time (zfile)
     const char *zfile;
{
  struct stat s;

  if (stat (zfile, &s) < 0)
    {
      if (errno != ENOENT)
	ulog (LOG_ERROR, "stat (%s): %s", zfile, strerror (errno));
      return isysdep_time ((long *) NULL);
    }

  return (long) s.st_mtime;
}

/* Get the size in bytes of a file.  */

long
csysdep_size (zfile)
     const char *zfile;
{
  struct stat s;

  if (stat (zfile, &s) < 0)
    {
      if (errno != ENOENT)
	ulog (LOG_ERROR, "stat (%s): %s", zfile, strerror (errno));
      return 0;
    }

  return s.st_size;
}

/* Start getting the status files.  */

boolean
fsysdep_all_status_init (phold)
     pointer *phold;
{
  DIR *qdir;

  qdir = opendir ((char *) ".Status");
  if (qdir == NULL)
    {
      ulog (LOG_ERROR, "opendir (.Status): %s", strerror (errno));
      return FALSE;
    }

  *phold = (pointer) qdir;
  return TRUE;
}

/* Get the next status file.  */

const char *
zsysdep_all_status (phold, pferr, qstat)
     pointer phold;
     boolean *pferr;
     struct sstatus *qstat;
{
  DIR *qdir = (DIR *) phold;
  struct dirent *qentry;

  while (TRUE)
    {
      errno = 0;
      qentry = readdir (qdir);
      if (qentry == NULL)
	{
	  if (errno == 0)
	    *pferr = FALSE;
	  else
	    {
	      ulog (LOG_ERROR, "readdir: %s", strerror (errno));
	      *pferr = TRUE;
	    }
	  return NULL;
	}

      if (qentry->d_name[0] != '.')
	{
	  struct ssysteminfo ssys;

	  /* Hack seriously; fsysdep_get_status only looks at the
	     zname element of the qsys argument, so if we fake that we
	     can read the status file.  This should really be done
	     differently.  */
	  ssys.zname = qentry->d_name;
	  if (fsysdep_get_status (&ssys, qstat))
	    return zscopy (qentry->d_name);

	  /* If fsysdep_get_status fails, it will output an error
	     message.  We just continue with the next entry, so that
	     most of the status files will be displayed.  */
	}
    }
}

/* Finish getting the status file.  */

void
usysdep_all_status_free (phold)
     pointer phold;
{
  DIR *qdir = (DIR *) phold;

  (void) closedir (qdir);
}

/* Get the status of all processes holding lock files.  We do this by
   invoking ps after we've figured out the process entries to use.  */

boolean
fsysdep_lock_status ()
{
  const char *zdir;
  DIR *qdir;
  struct dirent *qentry;
  int calc;
  int *pai;
  int cgot;
  int aidescs[3];
  char *zcopy, *ztok;
  int cargs, iarg;
  char **pazargs;

#ifdef LOCKDIR
  zdir = LOCKDIR;
#else
  zdir = ".";
#endif

  qdir = opendir ((char *) zdir);
  if (qdir == NULL)
    {
      ulog (LOG_ERROR, "opendir (%s): %s", zdir, strerror (errno));
      return FALSE;
    }

  /* We look for entries that start with "LCK.." and ignore everything
     else.  This won't find all possible lock files, but it should
     find all the locks on terminals and systems.  */

  calc = 0;
  pai = NULL;
  cgot = 0;
  while ((qentry = readdir (qdir)) != NULL)
    {
      const char *zname;
      int o;
#if HAVE_V2_LOCKFILES
      int i;
#else
      char ab[12];
#endif
      int cread;
      int ierr;
      int ipid;

      if (strncmp (qentry->d_name, "LCK..", sizeof "LCK.." - 1) != 0)
	continue;

      zname = zsappend (zdir, qentry->d_name);
      o = open (zname, O_RDONLY, 0);
      if (o < 0)
	{
	  if (errno != ENOENT)
	    ulog (LOG_ERROR, "open (%s): %s", zname, strerror (errno));
	  continue;
	}

#if HAVE_V2_LOCKFILES
      cread = read (o, &i, sizeof i);
#else
      cread = read (o, ab, sizeof ab - 1);
#endif

      ierr = errno;
      (void) close (o);

      if (cread < 0)
	{
	  ulog (LOG_ERROR, "read %s: %s", zname, strerror (ierr));
	  continue;
	}

#if HAVE_V2_LOCKFILES
      ipid = i;
#else
      ab[cread] = '\0';
      ipid = atoi (ab);
#endif

      printf ("%s: %d\n", qentry->d_name, ipid);

      if (cgot >= calc)
	{
	  calc += 10;
	  pai = (int *) xrealloc ((pointer) pai, calc * sizeof (int));
	}

      pai[cgot] = ipid;
      ++cgot;
    }

  if (cgot == 0)
    return TRUE;

  aidescs[0] = SPAWN_NULL;
  aidescs[1] = 1;
  aidescs[2] = 2;

  /* Parse PS_PROGRAM into an array of arguments.  */
  zcopy = (char *) alloca (sizeof PS_PROGRAM);
  strcpy (zcopy, PS_PROGRAM);

  cargs = 0;
  for (ztok = strtok (zcopy, " \t");
       ztok != NULL;
       ztok = strtok ((char *) NULL, " \t"))
    ++cargs;

  pazargs = (char **) alloca ((cargs + 1) * sizeof (char *));

  strcpy (zcopy, PS_PROGRAM);
  for (ztok = strtok (zcopy, " \t"), iarg = 0;
       ztok != NULL;
       ztok = strtok ((char *) NULL, " \t"), ++iarg)
    pazargs[iarg] = ztok;
  pazargs[iarg] = NULL;

#if ! HAVE_PS_MULTIPLE
  /* We have to invoke ps multiple times.  */
  {
    int i;
    char *zlast, *zset;

    zlast = pazargs[cargs - 1];
    zset = (char *) alloca (strlen (zlast) + 20);
    for (i = 0; i < cgot; i++)
      {
	pid_t ipid;

	sprintf (zset, "%s%d", zlast, pai[i]);
	pazargs[cargs - 1] = zset;

	ipid = isspawn ((const char **) pazargs, aidescs, FALSE, FALSE,
			(const char *) NULL, FALSE, TRUE,
			(const char *) NULL, (const char *) NULL,
			(const char *) NULL);
	if (ipid < 0)
	  ulog (LOG_ERROR, "isspawn: %s", strerror (errno));
	else
	  (void) iswait ((unsigned long) ipid, PS_PROGRAM);
      }
  }
#else
  {
    char *zlast;
    int i;
    pid_t ipid;

    zlast = (char *) alloca (strlen (pazargs[cargs - 1]) + cgot * 20 + 1);
    strcpy (zlast, pazargs[cargs - 1]);
    for (i = 0; i < cgot; i++)
      {
	char ab[20];

	sprintf (ab, "%d", pai[i]);
	strcat (zlast, ab);
	if (i + 1 < cgot)
	  strcat (zlast, ",");
      }
    pazargs[cargs - 1] = zlast;

    ipid = isspawn ((const char **) pazargs, aidescs, FALSE, FALSE,
		    (const char *) NULL, FALSE, TRUE,
		    (const char *) NULL, (const char *) NULL,
		    (const char *) NULL);
    if (ipid < 0)
      ulog (LOG_ERROR, "isspawn: %s", strerror (errno));
    else
      (void) iswait ((unsigned long) ipid, PS_PROGRAM);
  }
#endif    

  return TRUE;
}

/*
  Local variables:
  mode:c
  End:
  */
