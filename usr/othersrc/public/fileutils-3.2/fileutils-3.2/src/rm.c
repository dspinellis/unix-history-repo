/* `rm' file deletion utility for GNU.
   Copyright (C) 1988, 1990, 1991 Free Software Foundation, Inc.

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

/* Written by Paul Rubin, David MacKenzie, and Richard Stallman. */

#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

#ifdef _POSIX_SOURCE
/* POSIX.1 doesn't have inodes, so fake them to avoid lots of ifdefs. */
#define ino_t unsigned long
#define D_INO(dp) 1
#else
#define D_INO(dp) ((dp)->d_ino)
#endif

char *basename ();
char *stpcpy ();
char *xmalloc ();
char *xrealloc ();
int clear_directory ();
int duplicate_entry ();
int eaccess_stat ();
int remove_dir ();
int remove_file ();
int rm ();
int yesno ();
void error ();
void strip_trailing_slashes ();
void usage ();

/* Path of file now being processed; extended as necessary. */
char *pathname;

/* Number of bytes currently allocated for `pathname';
   made larger when necessary, but never smaller.  */
int pnsize;

/* Name this program was run with.  */
char *program_name;

/* If nonzero, display the name of each file removed. */
int verbose;

/* If nonzero, ignore nonexistant files. */
int ignore_missing_files;

/* If nonzero, recursively remove directories. */
int recursive;

/* If nonzero, query the user about whether to remove each file. */
int interactive;

/* If nonzero, remove directories with unlink instead of rmdir, and don't
   require a directory to be empty before trying to unlink it.
   Only works for the super-user. */
int unlink_dirs;

/* If nonzero, stdin is a tty. */
int stdin_tty;

struct option long_opts[] =
{
  {"directory", 0, &unlink_dirs, 1},
  {"force", 0, NULL, 'f'},
  {"interactive", 0, NULL, 'i'},
  {"recursive", 0, &recursive, 1},
  {"verbose", 0, &verbose, 1},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int err = 0;
  int c;

  verbose = ignore_missing_files = recursive = interactive
    = unlink_dirs = 0;
  pnsize = 256;
  pathname = xmalloc (pnsize);
  program_name = argv[0];

  while ((c = getopt_long (argc, argv, "dfirvR", long_opts, (int *) 0)) != EOF)
    {
      switch (c)
	{
	case 0:		/* Long option. */
	  break;
	case 'd':
	  unlink_dirs = 1;
	  break;
	case 'f':
	  interactive = 0;
	  ignore_missing_files = 1;
	  break;
	case 'i':
	  interactive = 1;
	  ignore_missing_files = 0;
	  break;
	case 'r':
	case 'R':
	  recursive = 1;
	  break;
	case 'v':
	  verbose = 1;
	  break;
	default:
	  usage ();
	}
    }

  if (optind == argc)
    usage ();

  stdin_tty = isatty (0);

  for (; optind < argc; optind++)
    {
      int len;

      strip_trailing_slashes (argv[optind]);
      len = strlen (argv[optind]);
      if (len + 1 > pnsize)
	{
	  free (pathname);
	  pnsize = 2 * (len + 1);
	  pathname = xmalloc (pnsize);
	}
      strcpy (pathname, argv[optind]);
      err += rm ();
    }

  exit (err > 0);
}

/* Remove file or directory `pathname' after checking appropriate things.
   Return 0 if `pathname' is removed, 1 if not. */

int
rm ()
{
  struct stat path_stats;
  char *base = basename (pathname);

  if (base[0] == '.' && (base[1] == '\0'
			     || (base[1] == '.' && base[2] == '\0')))
    {
      error (0, 0, "cannot remove `.' or `..'");
      return 1;
    }

  if (lstat (pathname, &path_stats))
    {
      if (errno == ENOENT && ignore_missing_files)
	return 0;
      error (0, errno, "%s", pathname);
      return 1;
    }

  if (S_ISDIR (path_stats.st_mode) && !unlink_dirs)
    return remove_dir (&path_stats);
  else
    return remove_file (&path_stats);
}

/* Query the user if appropriate, and if ok try to remove the
   non-directory `pathname', which STATP contains info about.
   Return 0 if `pathname' is removed, 1 if not. */

int
remove_file (statp)
     struct stat *statp;
{
  if (!ignore_missing_files && (interactive || stdin_tty)
      && eaccess_stat (statp, W_OK))
    {
      fprintf (stderr, "%s: remove %s`%s', overriding mode %04o? ",
	       program_name,
	       S_ISDIR (statp->st_mode) ? "directory " : "",
	       pathname,
	       statp->st_mode & 07777);
      if (!yesno ())
	return 1;
    }
  else if (interactive)
    {
      fprintf (stderr, "%s: remove %s`%s'? ", program_name,
	       S_ISDIR (statp->st_mode) ? "directory " : "",
	       pathname);
      if (!yesno ())
	return 1;
    }

  if (verbose)
    printf ("%s\n", pathname);

  if (unlink (pathname) && (errno != ENOENT || !ignore_missing_files))
    {
      error (0, errno, "%s", pathname);
      return 1;
    }
  return 0;
}

/* If not in recursive mode, print an error message and return 1.
   Otherwise, query the user if appropriate, then try to recursively
   remove directory `pathname', which STATP contains info about.
   Return 0 if `pathname' is removed, 1 if not. */

int
remove_dir (statp)
     struct stat *statp;
{
  int err;

  if (!recursive)
    {
      error (0, 0, "%s: is a directory", pathname);
      return 1;
    }

  if (!ignore_missing_files && (interactive || stdin_tty)
      && eaccess_stat (statp, W_OK))
    {
      fprintf (stderr,
	       "%s: descend directory `%s', overriding mode %04o? ",
	       program_name, pathname, statp->st_mode & 07777);
      if (!yesno ())
	return 1;
    }
  else if (interactive)
    {
      fprintf (stderr, "%s: descend directory `%s'? ",
	       program_name, pathname);
      if (!yesno ())
	return 1;
    }

  if (verbose)
    printf ("%s\n", pathname);

  err = clear_directory (statp);

  if (interactive)
    {
      if (err)
	fprintf (stderr, "%s: remove directory `%s' (might be nonempty)? ",
		 program_name, pathname);
      else
	fprintf (stderr, "%s: remove directory `%s'? ",
		 program_name, pathname);
      if (!yesno ())
	return 1;
    }

  if (rmdir (pathname) && (errno != ENOENT || !ignore_missing_files))
    {
      error (0, errno, "%s", pathname);
      return 1;
    }
  return 0;
}

/* An element in a stack of pointers into `pathname'.
   `pathp' points to where in `pathname' the terminating '\0' goes
   for this level's directory name. */
struct pathstack
{
  struct pathstack *next;
  char *pathp;
  ino_t inum;
};

/* Linked list of pathnames of directories in progress in recursive rm.
   The entries actually contain pointers into `pathname'.
   `pathstack' is the current deepest level. */
static struct pathstack *pathstack = NULL;

/* Read directory `pathname' and remove all of its entries,
   avoiding use of chdir.
   On entry, STATP points to the results of stat on `pathname'.
   Return 0 for success, error count for failure.
   Upon return, `pathname' will have the same contents as before,
   but its address might be different; in that case, `pnsize' will
   be larger, as well. */

int
clear_directory (statp)
     struct stat *statp;
{
  DIR *dirp;
  struct direct *dp;
  char *name_space;		/* Copy of directory's filenames. */
  char *namep;			/* Current entry in `name_space'. */
  unsigned name_size;		/* Bytes allocated for `name_space'. */
  int name_length;		/* Length of filename in `namep' plus '\0'. */
  int pathname_length;		/* Length of `pathname'. */
  ino_t *inode_space;		/* Copy of directory's inodes. */
  ino_t *inodep;		/* Current entry in `inode_space'. */
  unsigned inode_size;		/* Bytes allocated for `inode_space'. */
  int err = 0;			/* Return status. */
  struct pathstack pathframe;	/* New top of stack. */
  struct pathstack *pp;		/* Temporary. */

  name_size = statp->st_size;
  name_space = (char *) xmalloc (name_size);

  inode_size = statp->st_size;
  inode_space = (ino_t *) xmalloc (inode_size);

  do
    {
      namep = name_space;
      inodep = inode_space;

      errno = 0;
      dirp = opendir (pathname);
      if (dirp == NULL)
	{
	  if (errno != ENOENT || !ignore_missing_files)
	    {
	      error (0, errno, "%s", pathname);
	      err = 1;
	    }
	  free (name_space);
	  free (inode_space);
	  return err;
	}

      while ((dp = readdir (dirp)) != NULL)
	{
	  /* Skip "." and ".." (some NFS filesystems' directories lack them). */
	  if (dp->d_name[0] != '.'
	      || (dp->d_name[1] != '\0'
		  && (dp->d_name[1] != '.' || dp->d_name[2] != '\0')))
	    {
	      unsigned size_needed = (namep - name_space) + NLENGTH (dp) + 2;

	      if (size_needed > name_size)
		{
		  char *new_name_space;

		  while (size_needed > name_size)
		    name_size += 1024;

		  new_name_space = xrealloc (name_space, name_size);
		  namep += new_name_space - name_space;
		  name_space = new_name_space;
		}
	      namep = stpcpy (namep, dp->d_name) + 1;

	      if (inodep == inode_space + inode_size)
		{
		  ino_t *new_inode_space;

		  inode_size += 1024;
		  new_inode_space = (ino_t *) xrealloc (inode_space, inode_size);
		  inodep += new_inode_space - inode_space;
		  inode_space = new_inode_space;
		}
	      *inodep++ = D_INO (dp);
	    }
	}
      *namep = '\0';
      if (CLOSEDIR (dirp))
	{
	  error (0, errno, "%s", pathname);
	  err = 1;
	}

      pathname_length = strlen (pathname);

      for (namep = name_space, inodep = inode_space; *namep != '\0';
	   namep += name_length, inodep++)
	{
	  name_length = strlen (namep) + 1;

	  /* Satisfy GNU requirement that filenames can be arbitrarily long. */
	  if (pathname_length + 1 + name_length > pnsize)
	    {
	      char *new_pathname;

	      pnsize = (pathname_length + 1 + name_length) * 2;
	      new_pathname = xrealloc (pathname, pnsize);
	      /* Update the all the pointers in the stack to use the new area. */
	      for (pp = pathstack; pp != NULL; pp = pp->next)
		pp->pathp += new_pathname - pathname;
	      pathname = new_pathname;
	    }

	  /* Add a new frame to the top of the path stack. */
	  pathframe.pathp = pathname + pathname_length;
	  pathframe.inum = *inodep;
	  pathframe.next = pathstack;
	  pathstack = &pathframe;

	  /* Append '/' and the filename to current pathname, take care of the
	     file (which could result in recursive calls), and take the filename
	     back off. */

	  *pathstack->pathp = '/';
	  strcpy (pathstack->pathp + 1, namep);

	  /* If the i-number has already appeared, there's an error. */
	  if (duplicate_entry (pathstack->next, pathstack->inum))
	    err++;
	  else if (rm ())
	    err++;

	  *pathstack->pathp = '\0';
	  pathstack = pathstack->next;	/* Pop the stack. */
	}
    }
  /* Keep trying while there are still files to remove. */
  while (namep > name_space && err == 0);

  free (name_space);
  free (inode_space);
  return err;
}

/* If STACK does not already have an entry with the same i-number as INUM,
   return 0. Otherwise, ask the user whether to continue;
   if yes, return 1, and if no, exit.
   This assumes that no one tries to remove filesystem mount points;
   doing so could cause duplication of i-numbers that would not indicate
   a corrupted file system. */

int
duplicate_entry (stack, inum)
     struct pathstack *stack;
     ino_t inum;
{
#ifndef _POSIX_SOURCE
  struct pathstack *p;

  for (p = stack; p != NULL; p = p->next)
    {
      if (p->inum == inum)
	{
	  fprintf (stderr, "\
%s: WARNING: Circular directory structure.\n\
This almost certainly means that you have a corrupted file system.\n\
NOTIFY YOUR SYSTEM MANAGER.\n\
Cycle detected:\n\
%s\n\
is the same file as\n", program_name, pathname);
	  *p->pathp = '\0';	/* Truncate pathname. */
	  fprintf (stderr, "%s\n", pathname);
	  *p->pathp = '/';	/* Put it back. */
	  if (interactive)
	    {
	      fprintf (stderr, "%s: continue? ", program_name);
	      if (!yesno ())
		exit (1);
	      return 1;
	    }
	  else
	    exit (1);
	}
    }
#endif
  return 0;
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-dfirvR] [--directory] [--force] [--interactive] [--recursive]\n\
       [--verbose] path...\n",
	   program_name);
  exit (1);
}
