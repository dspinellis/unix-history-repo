/* eaccess -- check if effective user id can access file
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by David MacKenzie and Torbjorn Granlund. */

#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef _POSIX_VERSION
#include <limits.h>
#ifndef __386BSD__
#ifdef NGROUPS_MAX
#undef NGROUPS_MAX
#endif /* NGROUPS_MAX */
#define NGROUPS_MAX sysconf (_SC_NGROUPS_MAX)
#endif /* __386BSD__ */
#if !defined(sun) && !defined(ultrix) && !defined(__386BSD__)
#define GETGROUPS_T gid_t
#else /* sun or ultrix */
#define GETGROUPS_T int
#endif /* sun or ultrix */
#else /* not _POSIX_VERSION */
uid_t geteuid ();
gid_t getegid ();
#include <sys/param.h>
#if !defined(NGROUPS_MAX) && defined(NGROUPS)
#define NGROUPS_MAX NGROUPS
#endif /* not NGROUPS_MAX and NGROUPS */
#define GETGROUPS_T int
#endif /* POSIX_VERSION */

#include <errno.h>
#ifndef STDC_HEADERS
extern int errno;
#endif

#if defined(EACCES) && !defined(EACCESS)
#define EACCESS EACCES
#endif

#ifndef F_OK
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
#endif

int eaccess_stat ();

/* The user's effective user id. */
static uid_t euid;

/* The user's effective group id. */
static gid_t egid;

#ifdef NGROUPS_MAX
char *xmalloc ();
static int in_group ();

/* Array of group id's that the user is in. */
static GETGROUPS_T *groups = 0;

/* The number of valid elements in `groups'. */
static int ngroups;
#endif

/* Nonzero if the other static variables have valid values. */
static int initialized = 0;

/* Return 0 if the user has permission of type MODE on file PATH;
   otherwise, return -1 and set `errno' to EACCESS.
   Like access, except that it uses the effective user and group
   id's instead of the real ones, and it does not check for read-only
   filesystem, text busy, etc. */

int
eaccess (path, mode)
     char *path;
     int mode;
{
  struct stat stats;

  if (stat (path, &stats))
    return -1;

  return eaccess_stat (&stats, mode);
}

/* Like eaccess, except that a pointer to a filled-in stat structure
   describing the file is provided instead of a filename. */

int
eaccess_stat (statp, mode)
     struct stat *statp;
     int mode;
{
  int granted;

  mode &= (X_OK | W_OK | R_OK);	/* Clear any bogus bits. */

  if (mode == F_OK)
    return 0;			/* The file exists. */

  if (initialized == 0)
    {
      initialized = 1;
      euid = geteuid ();
      egid = getegid ();
#ifdef NGROUPS_MAX
      groups = (GETGROUPS_T *) xmalloc (NGROUPS_MAX * sizeof (GETGROUPS_T));
      ngroups = getgroups (NGROUPS_MAX, groups);
#endif
    }
  
  /* The super-user can read and write any file, and execute any file
     that anyone can execute. */
  if (euid == 0 && ((mode & X_OK) == 0 || (statp->st_mode & 0111)))
    return 0;
  if (euid == statp->st_uid)
    granted = (statp->st_mode & (mode << 6)) >> 6;
  else if (egid == statp->st_gid
#ifdef NGROUPS_MAX
	   || in_group (statp->st_gid)
#endif
	   )
    granted = (statp->st_mode & (mode << 3)) >> 3;
  else
    granted = (statp->st_mode & mode);
  if (granted == mode)
    return 0;
  errno = EACCESS;
  return -1;
}

#ifdef NGROUPS_MAX
static int
in_group (gid)
     GETGROUPS_T gid;
{
  int i;

  for (i = 0; i < ngroups; i++)
    if (gid == groups[i])
      return 1;
  return 0;
}
#endif

#ifdef TEST
main (argc, argv)
     char **argv;
{
  printf ("%d\n", eaccess (argv[1], atoi (argv[2])));
}
#endif
