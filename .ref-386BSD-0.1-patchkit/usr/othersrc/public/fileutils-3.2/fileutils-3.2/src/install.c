/* install - copy files and set attributes
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

/* Copy files and set their permission modes and, if possible,
   their owner and group.  Used similarly to `cp'; typically
   used in Makefiles to copy programs into their destination
   directories.  It can also be used to create the destination
   directories and any leading directories, and to set the final
   directory's modes.  It refuses to copy files onto themselves.

   Options:
   -g, --group=GROUP
	Set the group ownership of the installed file or directory
	to the group ID of GROUP (default is process's current
	group).  GROUP may also be a numeric group ID.

   -m, --mode=MODE
	Set the permission mode for the installed file or directory
	to MODE, which is an octal number (default is 0755).

   -o, --owner=OWNER
	If run as root, set the ownership of the installed file to
	the user ID of OWNER (default is root).  OWNER may also be
	a numeric user ID.

   -c	No effect.  For compatibility with old Unix versions of install.

   -s, --strip
	Strip the symbol tables from installed files.

   -d, --directory
	Create a directory and its leading directories, if they
	do not already exist.  Set the owner, group and mode
	as given on the command line.  Any leading directories
	that are created are also given those attributes.
	This is different from the SunOS 4.0 install, which gives
	directories that it creates the default attributes.

   David MacKenzie <djm@gnu.ai.mit.edu> */

#include <stdio.h>
#include <getopt.h>
#include <ctype.h>
#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include "system.h"
#include "modechange.h"

#ifdef _POSIX_VERSION
#include <sys/wait.h>
#else /* !_POSIX_VERSION */
struct passwd *getpwnam ();
struct group *getgrnam ();
uid_t getuid ();
gid_t getgid ();
int wait ();
void endpwent ();
void endgrent ();
#endif /* !_POSIX_VERSION */

/* True if C is an ASCII octal digit. */
#define isodigit(c) ((c) >= '0' && c <= '7')

/* Number of bytes of a file to copy at a time. */
#define READ_SIZE (32 * 1024)

char *basename ();
char *xmalloc ();
int change_attributes ();
int copy_file ();
int install_dir ();
int install_file_in_dir ();
int install_file_in_file ();
int isdir ();
int make_path ();
int isnumber ();
void error ();
void get_ids ();
void strip ();
void strip_trailing_slashes ();
void usage ();

/* The name this program was run with, for error messages. */
char *program_name;

/* The user name that will own the files, or NULL to make the owner
   the current user ID. */
char *owner_name;

/* The user ID corresponding to `owner_name'. */
uid_t owner_id;

/* The group name that will own the files, or NULL to make the group
   the current group ID. */
char *group_name;

/* The group ID corresponding to `group_name'. */
gid_t group_id;

/* The permissions to which the files will be set.  The umask has
   no effect. */
int mode;

/* If nonzero, strip executable files after copying them. */
int strip_files;

/* If nonzero, install a directory instead of a regular file. */
int dir_arg;

struct option long_options[] =
{
  {"strip", 0, NULL, 's'},
  {"directory", 0, NULL, 'd'},
  {"group", 1, NULL, 'g'},
  {"mode", 1, NULL, 'm'},
  {"owner", 1, NULL, 'o'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int optc;
  int errors = 0;
  char *symbolic_mode = NULL;

  program_name = argv[0];
  owner_name = NULL;
  group_name = NULL;
  mode = 0755;
  strip_files = 0;
  dir_arg = 0;
  umask (0);

  while ((optc = getopt_long (argc, argv, "csdg:m:o:", long_options,
			      (int *) 0)) != EOF)
    {
      switch (optc)
	{
	case 'c':
	  break;
	case 's':
	  strip_files = 1;
	  break;
	case 'd':
	  dir_arg = 1;
	  break;
	case 'g':
	  group_name = optarg;
	  break;
	case 'm':
	  symbolic_mode = optarg;
	  break;
	case 'o':
	  owner_name = optarg;
	  break;
	default:
	  usage ();
	}
    }

  /* Check for invalid combinations of arguments. */
  if ((dir_arg && strip_files)
      || (optind == argc)
      || (optind == argc - 1 && !dir_arg))
    usage ();

  if (symbolic_mode)
    {
      struct mode_change *change = mode_compile (symbolic_mode, 0);
      if (change == MODE_INVALID)
	error (1, 0, "invalid mode `%s'", symbolic_mode);
      else if (change == MODE_MEMORY_EXHAUSTED)
	error (1, 0, "virtual memory exhausted");
      mode = mode_adjust (0, change);
    }

  get_ids ();

  if (dir_arg)
    {
      for (; optind < argc; ++optind)
	{
	  strip_trailing_slashes (argv[optind]);
	  errors |=
	    make_path (argv[optind], mode, mode, owner_id, group_id, NULL);
	}
    }
  else
    {
      if (optind == argc - 2)
	{
	  strip_trailing_slashes (argv[argc - 2]);
	  strip_trailing_slashes (argv[argc - 1]);
	  if (!isdir (argv[argc - 1]))
	    errors = install_file_in_file (argv[argc - 2], argv[argc - 1]);
	  else
	    errors = install_file_in_dir (argv[argc - 2], argv[argc - 1]);
	}
      else
	{
	  strip_trailing_slashes (argv[argc - 1]);
	  if (!isdir (argv[argc - 1]))
	    usage ();
	  for (; optind < argc - 1; ++optind)
	    {
	      strip_trailing_slashes (argv[optind]);
	      errors |= install_file_in_dir (argv[optind], argv[argc - 1]);
	    }
	}
    }

  exit (errors);
}

/* Copy file FROM onto file TO and give TO the appropriate
   attributes.
   Return 0 if successful, 1 if an error occurs. */

int
install_file_in_file (from, to)
     char *from;
     char *to;
{
  if (copy_file (from, to))
    return 1;
  if (strip_files)
    strip (to);
  return change_attributes (to);
}

/* Copy file FROM into directory TO_DIR, keeping its same name,
   and give the copy the appropriate attributes.
   Return 0 if successful, 1 if not. */

int
install_file_in_dir (from, to_dir)
     char *from;
     char *to_dir;
{
  char *from_base;
  char *to;
  int ret;

  from_base = basename (from);
  to = xmalloc ((unsigned) (strlen (to_dir) + strlen (from_base) + 2));
  sprintf (to, "%s/%s", to_dir, from_base);
  ret = install_file_in_file (from, to);
  free (to);
  return ret;
}

/* A chunk of a file being copied. */
static char buffer[READ_SIZE];

/* Copy file FROM onto file TO, creating TO if necessary.
   Return 0 if the copy is successful, 1 if not. */

int
copy_file (from, to)
     char *from;
     char *to;
{
  int fromfd, tofd;
  int bytes;
  int ret = 0;
  struct stat from_stats, to_stats;

  if (stat (from, &from_stats))
    {
      error (0, errno, "%s", from);
      return 1;
    }
  if (!S_ISREG (from_stats.st_mode))
    {
      error (0, 0, "`%s' is not a regular file", from);
      return 1;
    }
  if (stat (to, &to_stats) == 0)
    {
      if (!S_ISREG (to_stats.st_mode))
	{
	  error (0, 0, "`%s' is not a regular file", to);
	  return 1;
	}
      if (from_stats.st_dev == to_stats.st_dev
	  && from_stats.st_ino == to_stats.st_ino)
	{
	  error (0, 0, "`%s' and `%s' are the same file", from, to);
	  return 1;
	}
      /* If unlink fails, try to proceed anyway.  */
      unlink (to);
    }

  fromfd = open (from, O_RDONLY, 0);
  if (fromfd == -1)
    {
      error (0, errno, "%s", from);
      return 1;
    }

  /* Make sure to open the file in a mode that allows writing. */
  tofd = open (to, O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (tofd == -1)
    {
      error (0, errno, "%s", to);
      close (fromfd);
      return 1;
    }

  while ((bytes = read (fromfd, buffer, READ_SIZE)) > 0)
    if (write (tofd, buffer, bytes) != bytes)
      {
	error (0, errno, "%s", to);
	goto copy_error;
      }

  if (bytes == -1)
    {
      error (0, errno, "%s", from);
      goto copy_error;
    }

  if (close (fromfd) < 0)
    {
      error (0, errno, "%s", from);
      ret = 1;
    }
  if (close (tofd) < 0)
    {
      error (0, errno, "%s", to);
      ret = 1;
    }
  return ret;

 copy_error:
  close (fromfd);
  close (tofd);
  return 1;
}

/* Set the attributes of file or directory PATH.
   Return 0 if successful, 1 if not. */

int
change_attributes (path)
     char *path;
{
  int err = 0;

  /* chown must precede chmod because on some systems,
     chown clears the set[ug]id bits for non-superusers,
     resulting in incorrect permissions.
     On System V, users can give away files with chown and then not
     be able to chmod them.  So don't give files away.  */

  if (chown (path, owner_id, group_id))
    err = errno;
  if (chmod (path, mode))
    err = errno;
  if (err)
    {
      error (0, err, "%s", path);
      return 1;
    }
  return 0;
}

/* Strip the symbol table from the file PATH.
   We could dig the magic number out of the file first to
   determine whether to strip it, but the header files and
   magic numbers vary so much from system to system that making
   it portable would be very difficult.  Not worth the effort. */

void
strip (path)
     char *path;
{
  int pid, status;

  pid = fork ();
  switch (pid)
    {
    case -1:
      error (1, errno, "cannot fork");
      break;
    case 0:			/* Child. */
      execlp ("strip", "strip", path, (char *) NULL);
      error (1, errno, "cannot run strip");
      break;
    default:			/* Parent. */
      /* Parent process. */
      while (pid != wait (&status))	/* Wait for kid to finish. */
	/* Do nothing. */ ;
      break;
    }
}

/* Initialize the user and group ownership of the files to install. */

void
get_ids ()
{
  struct passwd *pw;
  struct group *gr;

  if (owner_name)
    {
      pw = getpwnam (owner_name);
      if (pw == NULL)
	{
	  if (!isnumber (owner_name))
	    error (1, 0, "invalid user `%s'", owner_name);
	  owner_id = atoi (owner_name);
	}
      else
	owner_id = pw->pw_uid;
      endpwent ();
    }
  else
    owner_id = getuid ();

  if (group_name)
    {
      gr = getgrnam (group_name);
      if (gr == NULL)
	{
	  if (!isnumber (group_name))
	    error (1, 0, "invalid group `%s'", group_name);
	  group_id = atoi (group_name);
	}
      else
	group_id = gr->gr_gid;
      endgrent ();
    }
  else
    group_id = getgid ();
}

/* Return nonzero if STR is an ASCII representation of a nonzero
   decimal integer, zero if not. */

int
isnumber (str)
     char *str;
{
  if (*str == 0)
    return 0;
  for (; *str; str++)
    if (!isdigit (*str))
      return 0;
  return 1;
}

void
usage ()
{
   fprintf (stderr, "\
Usage: %s [options] [-s] [--strip] source dest\n\
       %s [options] [-s] [--strip] source... directory\n\
       %s [options] {-d,--directory} directory...\n\
Options:\n\
       [-c] [-g group] [-m mode] [-o owner]\n\
       [--group=group] [--mode=mode] [--owner=owner]\n",
	    program_name, program_name, program_name);
  exit (1);
}
