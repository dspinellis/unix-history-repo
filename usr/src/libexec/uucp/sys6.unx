/* sys6.unx
   System dependent routines to deal with local file names.

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

   $Log: sys6.unx,v $
   Revision 1.9  1992/02/27  19:51:55  ian
   Added extern for stat

   Revision 1.8  1992/02/08  22:33:32  ian
   Only get the current working directory if it's going to be needed

   Revision 1.7  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.6  1992/02/02  20:34:36  ian
   Niels Baggesen: must check user permissions on access to local files

   Revision 1.5  1991/12/22  22:14:19  ian
   Monty Solomon: added HAVE_UNISTD_H configuration parameter

   Revision 1.4  1991/12/14  16:09:07  ian
   Added -l option to uux to link files into the spool directory

   Revision 1.3  1991/12/06  22:50:01  ian
   Franc,ois Pinard: getcwd may legitimately fail in usysdep_initialize

   Revision 1.2  1991/11/13  23:08:40  ian
   Expand remote pathnames in uucp and uux; fix up uux special cases

   Revision 1.1  1991/09/10  19:45:50  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char sys6_unx_rcsid[] = "$Id: sys6.unx,v 1.9 1992/02/27 19:51:55 ian Rel $";
#endif

#include <errno.h>

#if USE_STDIO && HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "system.h"
#include "sysdep.h"

/* We need R_OK.  */
#ifndef R_OK
#define R_OK 4
#endif

/* We need some mode access macros.  */
#ifndef S_IRUSR
#define S_IRUSR (0400)
#define S_IRGRP (0040)
#define S_IROTH (0004)
#endif /* ! defined (S_IRUSR) */

/* External functions.  */
extern int access (), link (), stat ();
extern uid_t getuid (), getgid (), geteuid (), getegid ();

/* See whether running this file through zsysdep_add_cwd would require
   knowing the current working directory.  This is used to avoid
   determining the cwd if it will not be needed.  */

boolean
fsysdep_needs_cwd (zfile)
     const char *zfile;
{
  return *zfile != '/' && *zfile != '~';
}

/* Add the current working directory to a file name, if no directory
   has been specified.  We do ~ expansion here.  */

const char *
zsysdep_add_cwd (zfile, flocal)
     const char *zfile;
     boolean flocal;
{
  if (*zfile == '/')
    return zfile;
  if (*zfile == '~')
    {
      if (flocal)
	return zstilde_expand (&sLocalsys, zfile);
      else
	return zfile;
    }

#if DEBUG > 0
  if (zScwd == NULL)
    ulog (LOG_FATAL, "zsysdep_add_cwd: No cwd");
#endif

  return zsappend (zScwd, zfile);
}

/* Get the base name of a file name.  */

const char *
zsysdep_base_name (zfile)
     const char *zfile;
{
  const char *z;

  z = strrchr (zfile, '/');
  if (z != NULL)
    return z + 1;
  return zfile;
}

/* See if the user has access to a file, to prevent the setuid uucp
   and uux programs handing out unauthorized access.  */

boolean
fsysdep_access (zfile)
     const char *zfile;
{
  if (access (zfile, R_OK) == 0)
    return TRUE;
  ulog (LOG_ERROR, "%s: %s", zfile, strerror (errno));
  return FALSE;
}

/* See if the daemon has access to a file.  This is called if a file
   is not being transferred to the spool directory, since if the
   daemon does not have access the later transfer will fail.  We
   assume that the daemon will have the same euid (or egid) as the one
   we are running under.  If our uid (gid) and euid (egid) are the
   same, we assume that we have access.  Note that is not important
   for security, since the check will be (implicitly) done again when
   the daemon tries to transfer the file.  This routine should work
   whether the UUCP programs are installed setuid or setgid.  */

boolean
fsysdep_daemon_access (zfile)
     const char *zfile;
{
  struct stat s;
  uid_t ieuid, iuid, iegid, igid;
  boolean fok;

  ieuid = geteuid ();
  if (ieuid == 0)
    return TRUE;
  iuid = getuid ();
  iegid = getegid ();
  igid = getgid ();

  /* If our effective uid and gid are the same as our real uid and
     gid, we assume the daemon will have access to the file.  */
  if (ieuid == iuid && iegid == igid)
    return TRUE;

 if (stat (zfile, &s) != 0)
     {
      ulog (LOG_ERROR, "stat (%s): %s", zfile, strerror (errno));
      return FALSE;
    }

  /* If our euid is not our uid, but it is the file's uid, see if the
     owner has read access.  Otherwise, if our egid is not our gid,
     but it is the file's gid, see if the group has read access.
     Otherwise, see if the world has read access.  We know from the
     above check that at least one of our euid and egid are different,
     so that is the only one we want to check.  This check could fail
     if the UUCP programs were both setuid and setgid, but why would
     they be?  */
  if (ieuid != iuid && ieuid == s.st_uid)
    fok = (s.st_mode & S_IRUSR) != 0;
  else if (iegid != igid && iegid == s.st_gid)
    fok = (s.st_mode & S_IRGRP) != 0;
  else
    fok = (s.st_mode & S_IROTH) != 0;

  if (! fok)
    {
      ulog (LOG_ERROR, "%s: cannot be read by daemon", zfile);
      return FALSE;
    }

  return TRUE;
}

/* Link two files.  This in here because it's only called by uux.  */

boolean
fsysdep_link (zfrom, zto, pfworked)
     const char *zfrom;
     const char *zto;
     boolean *pfworked;
{
  if (link (zfrom, zto) == 0)
    {
      *pfworked = TRUE;
      return TRUE;
    }
  *pfworked = FALSE;
  return errno == EXDEV;
}

/*
  Local variables:
  mode:c
  End:
  */
