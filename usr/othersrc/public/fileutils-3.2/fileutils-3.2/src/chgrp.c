/* chgrp -- change group ownership of files
   Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.

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

/* Written by David MacKenzie <djm@gnu.ai.mit.edu>. */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <grp.h>
#include <getopt.h>
#include "system.h"

#ifndef _POSIX_VERSION
struct group *getgrnam ();
void endgrent ();
#endif /* _POSIX_VERSION */

int lstat ();

char *savedir ();
char *xmalloc ();
char *xrealloc ();
int change_file_group ();
int change_dir_group ();
int isnumber ();
void describe_change ();
void error ();
void parse_group ();
void usage ();

/* The name the program was run with. */
char *program_name;

/* If nonzero, change the ownership of directories recursively. */
int recurse;

/* If nonzero, force silence (no error messages). */
int force_silent;

/* If nonzero, describe the files we process. */
int verbose;

/* If nonzero, describe only owners or groups that change. */
int changes_only;

/* The name of the group to which ownership of the files is being given. */
char *groupname;

struct option long_options[] =
{
  {"recursive", 0, 0, 'R'},
  {"show-changes", 0, 0, 'c'},
  {"silent", 0, 0, 'f'},
  {"quiet", 0, 0, 'f'},
  {"verbose", 0, 0, 'v'},
  {0, 0, 0, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int group;
  int errors = 0;
  int optc;

  program_name = argv[0];
  recurse = force_silent = verbose = changes_only = 0;

  while ((optc = getopt_long (argc, argv, "Rcfv", long_options, (int *) 0))
	 != EOF)
    {
      switch (optc)
	{
	case 'R':
	  recurse = 1;
	  break;
	case 'c':
	  verbose = 1;
	  changes_only = 1;
	  break;
	case 'f':
	  force_silent = 1;
	  break;
	case 'v':
	  verbose = 1;
	  break;
	default:
	  usage ();
	}
    }

  if (optind >= argc - 1)
    usage ();

  parse_group (argv[optind++], &group);

  for (; optind < argc; ++optind)
    errors |= change_file_group (argv[optind], group);

  exit (errors);
}

/* Set *G according to NAME. */

void
parse_group (name, g)
     char *name;
     int *g;
{
  struct group *grp;

  groupname = name;
  if (*name == '\0')
    error (1, 0, "can not change to null group");

  grp = getgrnam (name);
  if (grp == NULL)
    {
      if (!isnumber (name))
	error (1, 0, "invalid group `%s'", name);
      *g = atoi (name);
    }
  else
    *g = grp->gr_gid;
  endgrent ();		/* Save a file descriptor. */
}

/* Change the ownership of FILE to GID GROUP.
   If it is a directory and -R is given, recurse.
   Return 0 if successful, 1 if errors occurred. */

int
change_file_group (file, group)
     char *file;
     int group;
{
  struct stat file_stats;
  int errors = 0;

  if (lstat (file, &file_stats))
    {
      if (force_silent == 0)
	error (0, errno, "%s", file);
      return 1;
    }

  if (group != file_stats.st_gid)
    {
      if (verbose)
	describe_change (file, 1);
      if (chown (file, file_stats.st_uid, group))
	{
	  if (force_silent == 0)
	    error (0, errno, "%s", file);
	  errors = 1;
	}
    }
  else if (verbose && changes_only == 0)
    describe_change (file, 0);

  if (recurse && S_ISDIR (file_stats.st_mode))
    errors |= change_dir_group (file, group, &file_stats);
  return errors;
}

/* Recursively change the ownership of the files in directory DIR
   to GID GROUP.
   STATP points to the results of lstat on DIR.
   Return 0 if successful, 1 if errors occurred. */

int
change_dir_group (dir, group, statp)
     char *dir;
     int group;
     struct stat *statp;
{
  char *name_space, *namep;
  char *path;			/* Full path of each entry to process. */
  unsigned dirlength;		/* Length of `dir' and '\0'. */
  unsigned filelength;		/* Length of each pathname to process. */
  unsigned pathlength;		/* Bytes allocated for `path'. */
  int errors = 0;

  errno = 0;
  name_space = savedir (dir, statp->st_size);
  if (name_space == NULL)
    {
      if (errno)
	{
	  if (force_silent == 0)
	    error (0, errno, "%s", dir);
	  return 1;
	}
      else
	error (1, 0, "virtual memory exhausted");
    }

  dirlength = strlen (dir) + 1;	/* + 1 is for the trailing '/'. */
  pathlength = dirlength + 1;
  /* Give `path' a dummy value; it will be reallocated before first use. */
  path = xmalloc (pathlength);
  strcpy (path, dir);
  path[dirlength - 1] = '/';

  for (namep = name_space; *namep; namep += filelength - dirlength)
    {
      filelength = dirlength + strlen (namep) + 1;
      if (filelength > pathlength)
	{
	  pathlength = filelength * 2;
	  path = xrealloc (path, pathlength);
	}
      strcpy (path + dirlength, namep);
      errors |= change_file_group (path, group);
    }
  free (path);
  free (name_space);
  return errors;
}

/* Tell the user the group name to which ownership of FILE
   has been given; if CHANGED is zero, FILE was that group already. */

void
describe_change (file, changed)
     char *file;
     int changed;
{
  if (changed)
    printf ("group of %s changed to %s\n", file, groupname);
  else
    printf ("group of %s retained as %s\n", file, groupname);
}

/* Return nonzero if STR represents an unsigned decimal integer,
   otherwise return 0. */

int
isnumber (str)
     char *str;
{
  for (; *str; str++)
    if (!isdigit (*str))
      return 0;
  return 1;
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-Rcfv] [--recursive] [--show-changes] [--silent] [--quiet]\n\
       [--verbose] group file...\n",
	   program_name);
  exit (1);
}
