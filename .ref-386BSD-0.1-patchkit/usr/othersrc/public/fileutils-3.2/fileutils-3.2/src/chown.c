/* chown -- change user and group ownership of files
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

/* 
              |     		      user
              | unchanged                 explicit
 -------------|-------------------------+-------------------------|
 g unchanged  | ---                     | chown u 		  |
 r            |-------------------------+-------------------------|
 o explicit   | chgrp g or chown .g     | chown u.g		  |
 u            |-------------------------+-------------------------|
 p from passwd| ---      	        | chown u.       	  |
              |-------------------------+-------------------------|

   Written by David MacKenzie <djm@gnu.ai.mit.edu>. */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <getopt.h>
#include "system.h"

#ifndef _POSIX_VERSION
struct passwd *getpwnam ();
struct group *getgrnam ();
struct group *getgrgid ();
void endpwent ();
void endgrent ();
#endif /* _POSIX_VERSION */

int lstat ();

char *savedir ();
char *xmalloc ();
char *xrealloc ();
int change_file_owner ();
int change_dir_owner ();
int isnumber ();
void describe_change ();
void error ();
void parse_owner ();
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

/* The name of the user to which ownership of the files is being given. */
char *username;

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
  int user = -1;		/* New uid; -1 if not to be changed. */
  int group = -1;		/* New gid; -1 if not to be changed. */
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

  parse_owner (argv[optind++], &user, &group);

  for (; optind < argc; ++optind)
    errors |= change_file_owner (argv[optind], user, group);

  exit (errors);
}

/* Set *U and *G according to NAME. */

void
parse_owner (name, u, g)
     char *name;
     int *u, *g;
{
  struct passwd *pwd;
  struct group *grp;
  char *cp;
  int use_login_group = 0;

  /* Check whether group is given. */
  cp = index (name, ':');
  if (cp == NULL)
    cp = index (name, '.');
  if (cp != NULL)
    {
      *cp++ = '\0';
      groupname = cp;
      if (*cp == '\0')
	{
	  if (cp == name + 1)
	    /* Neither user nor group given, just '.'. */
	    error (1, 0, "can not omit both user and group");
	  else
	    use_login_group = 1;
	}
      else
	{
	  grp = getgrnam (cp);
	  if (grp == NULL)
	    {
	      if (!isnumber (cp))
		error (1, 0, "invalid group `%s'", cp);
	      *g = atoi (cp);
	    }
	  else
	    *g = grp->gr_gid;
	  endgrent ();		/* Save a file descriptor. */
	}
    }

  /* Parse user. */
  username = name;
  groupname = 0;
  if (name[0] != '\0')
    {
      pwd = getpwnam (name);
      if (pwd == NULL)
	{
	  if (!isnumber (name))
	    error (1, 0, "invalid user `%s'", name);
	  if (use_login_group)
	    error (1, 0, "cannot get login group for numeric UID");
	  *u = atoi (name);
	}
      else
	{
	  *u = pwd->pw_uid;
	  if (use_login_group)
	    {
	      *g = pwd->pw_gid;
	      grp = getgrgid (pwd->pw_gid);
	      if (grp == NULL)
		{
		  groupname = xmalloc (15);
		  sprintf (groupname, "%u", pwd->pw_gid);
		}
	      else
		groupname = grp->gr_name;
	      endgrent ();
	    }
	}
      endpwent ();
    }
}

/* Change the ownership of FILE to UID USER and GID GROUP.
   If it is a directory and -R is given, recurse.
   Return 0 if successful, 1 if errors occurred. */

int
change_file_owner (file, user, group)
     char *file;
     int user, group;
{
  struct stat file_stats;
  int newuser, newgroup;
  int errors = 0;

  if (lstat (file, &file_stats))
    {
      if (force_silent == 0)
	error (0, errno, "%s", file);
      return 1;
    }

  newuser = user == -1 ? file_stats.st_uid : user;
  newgroup = group == -1 ? file_stats.st_gid : group;
  if (newuser != file_stats.st_uid || newgroup != file_stats.st_gid)
    {
      if (verbose)
	describe_change (file, 1);
      if (chown (file, newuser, newgroup))
	{
	  if (force_silent == 0)
	    error (0, errno, "%s", file);
	  errors = 1;
	}
    }
  else if (verbose && changes_only == 0)
    describe_change (file, 0);

  if (recurse && S_ISDIR (file_stats.st_mode))
    errors |= change_dir_owner (file, user, group, &file_stats);
  return errors;
}

/* Recursively change the ownership of the files in directory DIR
   to UID USER and GID GROUP.
   STATP points to the results of lstat on DIR.
   Return 0 if successful, 1 if errors occurred. */

int
change_dir_owner (dir, user, group, statp)
     char *dir;
     int user, group;
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
      errors |= change_file_owner (path, user, group);
    }
  free (path);
  free (name_space);
  return errors;
}

/* Tell the user the user and group names to which ownership of FILE
   has been given; if CHANGED is zero, FILE had those owners already. */

void
describe_change (file, changed)
     char *file;
     int changed;
{
  if (changed)
    printf ("owner of %s changed to ", file);
  else
    printf ("owner of %s retained as ", file);
  if (groupname)
    printf ("%s.%s\n", username, groupname);
  else
    printf ("%s\n", username);
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
       [--verbose] [user][:.][group] file...\n",
	   program_name);
  exit (1);
}
