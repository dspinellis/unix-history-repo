/* sys5.unx
   The system dependent routines to read execute files for Unix,
   and to execute programs.  These routines are used by uuxqt.

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

   $Log: sys5.unx,v $
   Revision 1.40  1992/03/17  03:15:40  ian
   Pass command to fsysdep_execute as first element of argument array

   Revision 1.39  1992/03/16  19:44:45  ian
   Cast result of alloca

   Revision 1.38  1992/03/15  01:54:46  ian
   All execs are now done in isspawn, all waits are done in iswait

   Revision 1.37  1992/03/12  19:54:43  ian
   Debugging based on types rather than number

   Revision 1.36  1992/03/11  22:06:37  ian
   Marty Shannon: added max-uuxqts command

   Revision 1.35  1992/03/11  17:04:53  ian
   Jon Zeeff: retry execution later if temporary failure

   Revision 1.34  1992/03/11  02:09:57  ian
   Franc,ois Pinard: retry fork several times before giving up

   Revision 1.33  1992/02/29  01:06:59  ian
   Chip Salzenberg: recheck file permissions before sending

   Revision 1.32  1992/02/27  05:40:54  ian
   T. William Wells: detach from controlling terminal, handle signals safely

   Revision 1.31  1992/02/25  15:58:29  ian
   Bob Denny: don't warn when trying to open a non-directory

   Revision 1.30  1992/02/24  20:07:43  ian
   John Theus: some systems don't have <fcntl.h>

   Revision 1.29  1992/02/24  04:58:47  ian
   Only permit files to be received into directories that are world-writeable

   Revision 1.28  1992/02/23  19:50:50  ian
   Handle READ and WRITE in Permissions correctly

   Revision 1.27  1992/02/23  03:26:51  ian
   Overhaul to use automatic configure shell script

   Revision 1.26  1992/02/18  04:40:06  ian
   Michael Nolan: allow full command path from remote, not just basename

   Revision 1.25  1992/02/09  03:14:48  ian
   Added HAVE_OLD_DIRECTORIES for systems without readdir routines

   Revision 1.24  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.23  1992/01/15  19:42:42  ian
   No longer need to define wait status macros here

   Revision 1.22  1992/01/15  19:40:35  ian
   Mike Park: handle HAVE_UNION_WAIT correctly and completely

   Revision 1.21  1992/01/13  06:11:39  ian
   David Nugent: can't declare open or fcntl

   Revision 1.20  1992/01/04  21:43:24  ian
   Chip Salzenberg: added ALLOW_FILENAME_ARGUMENTS to permit them

   Revision 1.19  1992/01/04  04:12:54  ian
   David J. Fiander: make sure execution arguments are not bad file names

   Revision 1.18  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.17  1991/12/29  00:55:23  ian
   Monty Solomon: added HAVE_UNION_WAIT

   Revision 1.16  1991/12/22  22:14:19  ian
   Monty Solomon: added HAVE_UNISTD_H configuration parameter

   Revision 1.15  1991/12/21  21:34:14  ian
   Moved fsysdep_file_exists from sys5.unx to sys1.unx

   Revision 1.14  1991/12/15  01:58:49  ian
   Oleg Tabarovsky: don't abandon processing because of an opendir error

   Revision 1.13  1991/12/11  04:17:39  ian
   Call fsysdep_make_dirs correctly if chdir (XQTDIR) fails

   Revision 1.12  1991/12/11  03:59:19  ian
   Create directories when necessary; don't just assume they exist

   Revision 1.11  1991/12/07  03:41:44  ian
   David J. Fiander: if execve fails, fall back on /bin/sh

   Revision 1.10  1991/12/01  02:23:12  ian
   Niels Baggesen: don't multiply include <unistd.h>

   Revision 1.9  1991/11/26  01:45:42  ian
   Marty Shannon: configuration option to not include <sys/wait.h>

   Revision 1.8  1991/11/22  06:05:57  ian
   Gregory Gulik: fix wait status macro definitions

   Revision 1.7  1991/11/16  00:35:43  ian
   Case constant arguments to opendir

   Revision 1.6  1991/11/07  20:52:33  ian
   Chip Salzenberg: pass command as single argument to /bin/sh

   Revision 1.5  1991/11/07  19:32:28  ian
   Chip Salzenberg: allow LOCKDIR, and check that locking process exists

   Revision 1.4  1991/09/19  16:15:58  ian
   Chip Salzenberg: configuration option for permitting execution via sh

   Revision 1.3  1991/09/19  15:51:01  ian
   Chip Salzenberg: pass TZ environment variable to execution process

   Revision 1.2  1991/09/19  03:06:04  ian
   Chip Salzenberg: put BNU temporary files in system's directory

   Revision 1.1  1991/09/10  19:45:50  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char sys5_unx_rcsid[] = "$Id: sys5.unx,v 1.40 1992/03/17 03:15:40 ian Rel $";
#endif

#include <ctype.h>
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

/* We need F_OK.  */

#ifndef F_OK
#define F_OK (0)
#endif

/* Get a value for EX_TEMPFAIL.  */

#if HAVE_SYSEXITS_H
#include <sysexits.h>
#endif

#ifndef EX_TEMPFAIL
#define EX_TEMPFAIL 75
#endif

/* External functions.  */
extern int access (), close (), dup2 (), chdir ();
extern void _exit ();

/* Under the V2 or BSD42 spool directory scheme, all execute files are
   in the main spool directory.  Under the BSD43 scheme, they are all
   in the directory X..  Under the BNU scheme, they are in directories
   named after systems.  Under the ULTRIX scheme, they are in X.
   subdirectories of subdirectories of sys.  Under the TAYLOR scheme,
   they are all in the subdirectory X. of a directory named after
   the system.

   This means that for BNU, ULTRIX or TAYLOR, we have to search
   directories of directories.  */

#if SPOOLDIR_V2 | SPOOLDIR_BSD42
#define ZDIR "."
#define SUBDIRS 0
#endif
#if SPOOLDIR_BNU | SPOOLDIR_TAYLOR
#define ZDIR "."
#define SUBDIRS 1
#endif
#if SPOOLDIR_ULTRIX
#define ZDIR "sys"
#define SUBDIRS 1
#endif
#if SPOOLDIR_BSD43
#define ZDIR "X."
#define SUBDIRS 0
#endif

/* Static variables for the execute file scan.  */

static DIR *qSxqt_topdir;
#if ! SUBDIRS
static const char *zSdir;
#else /* SUBDIRS */
static char *zSdir;
static DIR *qSxqt_dir;
static char *zSsystem;
#endif /* SUBDIRS */

/* Initialize the scan for execute files.  The function
   usysdep_get_xqt_free will clear the data out when we are done with
   the system.  This returns FALSE on error.  */

/*ARGSUSED*/
boolean
fsysdep_get_xqt_init ()
{
  usysdep_get_xqt_free ();

  qSxqt_topdir = opendir ((char *) ZDIR);
  if (qSxqt_topdir == NULL)
    {
      if (errno == ENOENT)
	return TRUE;
      ulog (LOG_ERROR, "opendir (%s): %s", ZDIR, strerror (errno));
      return FALSE;
    }

  return TRUE;
}

/* Return the name of the next execute file to read and process.  If
   this returns NULL, *pferr must be checked.  If will be TRUE on
   error, FALSE if there are no more files.  On a successful return
   *pzsystem will be set to the system for which the execute file was
   created.  */

const char *
zsysdep_get_xqt (pzsystem, pferr)
     const char **pzsystem;
     boolean *pferr;
{
  *pferr = FALSE;

  if (qSxqt_topdir == NULL)
    return NULL;

  /* This loop continues until we find a file.  */
  while (TRUE)
    {
      DIR *qdir;
      struct dirent *q;

#if ! SUBDIRS
      zSdir = ZDIR;
      qdir = qSxqt_topdir;
#else /* SUBDIRS */
      /* This loop continues until we find a subdirectory to read.  */
      while (qSxqt_dir == NULL)
	{
	  struct dirent *qtop;
	  char *zset;

	  qtop = readdir (qSxqt_topdir);
	  if (qtop == NULL)
	    {
	      (void) closedir (qSxqt_topdir);
	      qSxqt_topdir = NULL;
	      return NULL;
	    }

	  /* No system name may start with a dot (this is enforced by
	     tisystem in sysinf.c).  This allows us to quickly skip
	     impossible directories.  */
	  if (qtop->d_name[0] == '.')
	    continue;

	  DEBUG_MESSAGE1 (DEBUG_SPOOLDIR,
			  "zsysdep_get_xqt: Found %s in top directory",
			  qtop->d_name);

#if SPOOLDIR_BNU
	  zset = qtop->d_name;
#endif
#if SPOOLDIR_ULTRIX
	  zset = (char *) alloca (strlen (qtop->d_name) + sizeof "sys//X.");
	  sprintf (zset, "sys/%s/X.", qtop->d_name);
#endif
#if SPOOLDIR_TAYLOR
	  zset = (char *) alloca (strlen (qtop->d_name) + sizeof "/X.");
	  sprintf (zset, "%s/X.", qtop->d_name);
#endif

	  xfree ((pointer) zSdir);
	  zSdir = xstrdup (zset);

	  xfree ((pointer) zSsystem);
	  zSsystem = xstrdup (qtop->d_name);

	  qSxqt_dir = opendir (zSdir);

	  if (qSxqt_dir == NULL
	      && errno != ENOTDIR
	      && errno != ENOENT)
	    ulog (LOG_ERROR, "opendir (%s): %s", zSdir, strerror (errno));
	}

      qdir = qSxqt_dir;
#endif /* SUBDIRS */

      q = readdir (qdir);

#if DEBUG > 1
      if (q != NULL)
	DEBUG_MESSAGE2 (DEBUG_SPOOLDIR,
			"zsysdep_get_xqt: Found %s in subdirectory %s",
			q->d_name, zSdir);
#endif

      /* If we've found an execute file, return it.  We have to get
	 the system name, which is easy for BNU or TAYLOR.  For other
	 spool directory schemes, we have to pull it out of the X.
	 file name; this would be insecure, except that zsfind_file
	 clobbers the file name to include the real system name.  */

      if (q != NULL
	  && q->d_name[0] == 'X'
	  && q->d_name[1] == '.')
	{
	  const char *zret;

#if SPOOLDIR_BNU | SPOOLDIR_TAYLOR
	  *pzsystem = zSsystem;
#else
	  {
	    static char *zsys = NULL;
	    static int csys = 0;
	    int clen;

	    clen = strlen (q->d_name) - 7;
	    if (clen + 1 > csys)
	      {
		zsys = (char *) xrealloc ((pointer) zsys, clen + 1);
		csys = clen + 1;
	      }
	    strncpy (zsys, q->d_name + 2, clen);
	    zsys[clen] = '\0';

	    *pzsystem = zsys;
	  }
#endif

	  /* Set *pferr to TRUE in case zsappend returns NULL.  */
	  *pferr = TRUE;

	  zret = zsappend (zSdir, q->d_name);

#if DEBUG > 1
	  if (zret != NULL)
	    DEBUG_MESSAGE2 (DEBUG_SPOOLDIR,
			    "zsysdep_get_xqt: Returning %s (system %s)",
			    zret, *pzsystem);
#endif

	  return zret;
	}
	    
      /* If we've reached the end of the directory, then if we are
	 using subdirectories loop around to read the next one,
	 otherwise we are finished.  */

      if (q == NULL)
	{
	  (void) closedir (qdir);
#if SUBDIRS
	  qSxqt_dir = NULL;
	  continue;
#else
	  qSxqt_topdir = NULL;
	  return NULL;
#endif
	}
    }
}

/* Free up the results of an execute file scan, when we're done with
   this system.  */

/*ARGSUSED*/
void
usysdep_get_xqt_free ()
{
  if (qSxqt_topdir != NULL)
    {
      (void) closedir (qSxqt_topdir);
      qSxqt_topdir = NULL;
    }
#if SUBDIRS
  if (qSxqt_dir != NULL)
    {
      (void) closedir (qSxqt_dir);
      qSxqt_dir = NULL;
    }
  xfree ((pointer) zSdir);
  zSdir = NULL;
  xfree ((pointer) zSsystem);
  zSsystem = NULL;
#endif
}

/* Get the full pathname of the command to execute, given the list of
   permitted commands and the allowed path.  */

const char *
zsysdep_find_command (zcmd, zcmds, zpath, pferr)
     const char *zcmd;
     const char *zcmds;
     const char *zpath;
     boolean *pferr;
{
  char *zcopy, *ztok;

  *pferr = TRUE;

  if (strcmp (zcmds, "ALL") != 0)
    {
      zcopy = (char *) alloca (strlen (zcmds) + 1);
      strcpy (zcopy, zcmds);
      for (ztok = strtok (zcopy, " ");
	   ztok != NULL;
	   ztok = strtok ((char *) NULL, " "))
	{
	  char *zslash;

	  zslash = strrchr (ztok, '/');
	  if (zslash != NULL)
	    ++zslash;
	  else
	    zslash = ztok;
	  if (strcmp (zslash, zcmd) == 0
	      || strcmp (ztok, zcmd) == 0)
	    {
	      if (*ztok == '/')
		{
		  /* Hack to get two arguments for zsappend.  */
		  zslash[-1] = '\0';
		  return zsappend (ztok, zslash);
		}
	      break;
	    }
	}

      /* If we didn't find this command, get out.  */
      if (ztok == NULL)
	{
	  *pferr = FALSE;
	  return NULL;
	}
    }

  /* We didn't find an absolute pathname, so we must look through
     the path.  */
  zcopy = (char *) alloca (strlen (zpath) + 1);
  strcpy (zcopy, zpath);
  for (ztok = strtok (zcopy, " ");
       ztok != NULL;
       ztok = strtok ((char *) NULL, " "))
    {
      const char *zname;

      zname = zsappend (ztok, zcmd);
      if (zname == NULL)
	return NULL;
      if (access (zname, F_OK) == 0)
	return zname;
    }

  *pferr = FALSE;
  return NULL;
}

#if ! ALLOW_FILENAME_ARGUMENTS

/* Check to see whether an argument specifies a file name; if it does,
   make sure that the file may legally be sent and/or received.  For
   Unix, we do not permit any occurrence of "/../" in the name, nor
   may it start with "../".  Otherwise, if it starts with "/" we check
   against the list of permitted files.  */

boolean
fsysdep_xqt_check_file (qsys, zfile)
     const struct ssysteminfo *qsys;
     const char *zfile;
{
  if (strncmp (zfile, "../", sizeof "../" - 1) == 0
      || strstr (zfile, "/../") != NULL
      || (*zfile == '/'
	  && (! fin_directory_list (qsys, zfile, qsys->zremote_send, TRUE,
				    FALSE, (const char *) NULL)
	      || (qsys->zcalled_remote_send != NULL
		  && ! fin_directory_list (qsys, zfile,
					   qsys->zcalled_remote_send,
					   TRUE, FALSE,
					   (const char *) NULL))
	      || ! fin_directory_list (qsys, zfile, qsys->zremote_receive,
				       TRUE, FALSE, (const char *) NULL)
	      || (qsys->zcalled_remote_receive != NULL
		  && ! fin_directory_list (qsys, zfile,
					   qsys->zcalled_remote_receive,
					   TRUE, FALSE,
					   (const char *) NULL)))))
    {
      ulog (LOG_ERROR, "Not permitted to refer to file \"%s\"", zfile);
      return FALSE;
    }

  return TRUE;
}

#endif /* ! ALLOW_FILENAME_ARGUMENTS */

/* Invoke the command specified by an execute file.  */

/*ARGSUSED*/
boolean
fsysdep_execute (qsys, zuser, pazargs, zfullcmd, zinput, zoutput,
		 fshell, pzerror, pftemp)
     const struct ssysteminfo *qsys;
     const char *zuser;
     const char **pazargs;
     const char *zfullcmd;
     const char *zinput;
     const char *zoutput;
     boolean fshell;
     const char **pzerror;
     boolean *pftemp;
{
  int aidescs[3];
  boolean ferr;
  pid_t ipid;
  int ierr;
  int istat;
#if ALLOW_SH_EXECUTION
  const char *azshargs[4];
#endif

  *pzerror = NULL;
  *pftemp = FALSE;

  aidescs[0] = SPAWN_NULL;
  aidescs[1] = SPAWN_NULL;
  aidescs[2] = SPAWN_NULL;

  ferr = FALSE;

  if (zinput != NULL)
    {
      aidescs[0] = open (zinput, O_RDONLY, 0);
      if (aidescs[0] < 0)
	{
	  ulog (LOG_ERROR, "open (%s): %s", zinput, strerror (errno));
	  ferr = TRUE;
	}
    }
  
  if (! ferr && zoutput != NULL)
    {
      aidescs[1] = creat (zoutput, IPRIVATE_FILE_MODE);
      if (aidescs[1] < 0)
	{
	  ulog (LOG_ERROR, "creat (%s): %s", zoutput, strerror (errno));
	  *pftemp = TRUE;
	  ferr = TRUE;
	}
    }

  if (! ferr)
    {
      *pzerror = zstemp_file (qsys);
      aidescs[2] = creat (*pzerror, IPRIVATE_FILE_MODE);
      if (aidescs[2] < 0)
	{
	  if (errno == ENOENT)
	    {
	      if (! fsysdep_make_dirs (*pzerror, FALSE))
		{
		  *pftemp = TRUE;
		  ferr = TRUE;
		}
	      else
		aidescs[2] = creat (*pzerror, IPRIVATE_FILE_MODE);
	    }
	  if (! ferr && aidescs[2] < 0)
	    {
	      ulog (LOG_ERROR, "creat (%s): %s", *pzerror, strerror (errno));
	      *pftemp = TRUE;
	      ferr = TRUE;
	    }
	}
    }

  if (! fsdirectory_exists (XQTDIR))
    {
      char *zcopy;

      /* The fsysdep_make_dirs function needs a trailing slash,
	 because it only works on file names.  */
      zcopy = (char *) alloca (strlen (XQTDIR) + 2);
      sprintf (zcopy, "%s/", XQTDIR);
      if (! fsysdep_make_dirs (zcopy, FALSE))
	ferr = TRUE;
    }

  if (ferr)
    {
      if (aidescs[0] != SPAWN_NULL)
	(void) close (aidescs[0]);
      if (aidescs[1] != SPAWN_NULL)
	(void) close (aidescs[1]);
      if (aidescs[2] != SPAWN_NULL)
	(void) close (aidescs[2]);
      return FALSE;
    }

#if ALLOW_SH_EXECUTION
  if (fshell)
    {
      azshargs[0] = "/bin/sh";
      azshargs[1] = "-c";
      azshargs[2] = zfullcmd;
      azshargs[3] = NULL;
      pazargs = azshargs;
    }
#else
  fshell = FALSE;
#endif

  /* Pass zchdir as XQTDIR, fnosigs as TRUE, fshell as TRUE if we
     aren't already using the shell.  */
  ipid = isspawn (pazargs, aidescs, FALSE, FALSE, XQTDIR, TRUE,
		  ! fshell, qsys->zpath, qsys->zname, zuser);

  ierr = errno;

  if (aidescs[0] != SPAWN_NULL)
    (void) close (aidescs[0]);
  if (aidescs[1] != SPAWN_NULL)
    (void) close (aidescs[1]);
  if (aidescs[2] != SPAWN_NULL)
    (void) close (aidescs[2]);

  if (ipid < 0)
    {
      ulog (LOG_ERROR, "isspawn: %s", strerror (ierr));
      *pftemp = TRUE;
      return FALSE;
    }

  istat = iswait ((unsigned long) ipid, "Execution");

  if (istat == EX_TEMPFAIL)
    *pftemp = TRUE;

  return istat == 0;
}

/* Lock a uuxqt process.  */

int
isysdep_lock_uuxqt (zcmd)
     const char *zcmd;
{
  char ab[14];
  int i;

  if (cMaxuuxqts <= 0)
    i = 0;
  else
    {
      int c;

      c = cMaxuuxqts;
      if (c >= 10000)
	c = 9999;
      for (i = 0; i < c; i++)
	{
	  sprintf (ab, "LCK.XQT.%d", i);
	  if (fsdo_lock (ab, TRUE))
	    break;
	}
      if (i >= c)
	return -1;
    }

  if (zcmd != NULL)
    {
      char abcmd[14];

      sprintf (abcmd, "LXQ.%.9s", zcmd);
      abcmd[strcspn (abcmd, " \t/")] = '\0';
      if (! fsdo_lock (abcmd, TRUE))
	{
	  if (cMaxuuxqts > 0)
	    (void) fsdo_unlock (ab, TRUE);
	  return -1;
	}
    }

  return i;
}

/* Unlock a uuxqt process.  */

boolean
fsysdep_unlock_uuxqt (iseq, zcmd)
     int iseq;
     const char *zcmd;
{
  char ab[14];
  boolean fret;

  fret = TRUE;

  if (cMaxuuxqts > 0)
    {
      sprintf (ab, "LCK.XQT.%d", iseq);
      if (! fsdo_unlock (ab, TRUE))
	fret = FALSE;
    }

  if (zcmd != NULL)
    {
      sprintf (ab, "LXQ.%.9s", zcmd);
      ab[strcspn (ab, " \t/")] = '\0';
      if (! fsdo_unlock (ab, TRUE))
	fret = FALSE;
    }

  return fret;
}

/* See whether a particular uuxqt command is locked (this depends on
   the implementation of fsdo_lock).  */

boolean
fsysdep_uuxqt_locked (zcmd)
     const char *zcmd;
{
  char ab[14];

  sprintf (ab, "LXQ.%.9s", zcmd);
  return access (ab, F_OK) == 0;
}

/* Lock a particular execute file.  */

boolean
fsysdep_lock_uuxqt_file (zfile)
     const char *zfile;
{
  char *zcopy, *z;

  zcopy = (char *) alloca (strlen (zfile) + 1);
  strcpy (zcopy, zfile);

  z = strrchr (zcopy, '/');
  if (z == NULL)
    *zcopy = 'L';
  else
    *(z + 1) = 'L';

  return fsdo_lock (zcopy, TRUE);
}

/* Unlock a particular execute file.  */

boolean
fsysdep_unlock_uuxqt_file (zfile)
     const char *zfile;
{
  char *zcopy, *z;

  zcopy = (char *) alloca (strlen (zfile) + 1);
  strcpy (zcopy, zfile);

  z = strrchr (zcopy, '/');
  if (z == NULL)
    *zcopy = 'L';
  else
    *(z + 1) = 'L';

  return fsdo_unlock (zcopy, TRUE);
}

/* Lock the execute directory.  */

boolean
fsysdep_lock_uuxqt_dir ()
{
  return fsdo_lock ("LCK..XQTDIR", TRUE);
}

/* Unlock the execute directory and clear it out.  */

boolean
fsysdep_unlock_uuxqt_dir ()
{
  DIR *qdir;

  qdir = opendir ((char *) XQTDIR);
  if (qdir != NULL)
    {
      struct dirent *qentry;

      while ((qentry = readdir (qdir)) != NULL)
	{
	  const char *z;

	  if (strcmp (qentry->d_name, ".") == 0
	      || strcmp (qentry->d_name, "..") == 0)
	    continue;
	  z = zsappend (XQTDIR, qentry->d_name);
	  if (z != NULL)
	    (void) remove (z);
	}

      closedir (qdir);
    }

  return fsdo_unlock ("LCK..XQTDIR", TRUE);
}

/*
  Local variables:
  mode:c
  End:
  */
