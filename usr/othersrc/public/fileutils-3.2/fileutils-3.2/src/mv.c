/* mv -- move or rename files
   Copyright (C) 1986, 1989, 1990, 1991 Free Software Foundation, Inc.

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

/* Options:
   -f, --force		Assume a 'y' answer to all questions it would
			normally ask, and not ask the questions.

   -i, --interactive	Require confirmation from the user before
			performing any move that would destroy an
			existing file. 

   -u, --update		Do not move a nondirectory that has an
			existing destination with the same or newer
			modification time.  

   -v, --verbose		List the name of each file as it is moved, and
			the name it is moved to. 

   -b, --backup
   -S, --suffix
   -V, --version-control
			Backup file creation.

   Written by Mike Parker and David MacKenzie */

#ifdef _AIX
 #pragma alloca
#endif
#include <stdio.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"
#include "backupfile.h"

enum backup_type get_version ();
int copy ();
int do_move ();
int eaccess_stat ();
int isdir ();
int movefile ();
int yesno ();
void error ();
void strip_trailing_slashes ();
void usage ();

/* The name this program was run with. */
char *program_name;

/* If nonzero, query the user before overwriting files. */
int interactive;

/* If nonzero, do not query the user before overwriting unwritable
   files. */
int override_mode;

/* If nonzero, do not move a nondirectory that has an existing destination
   with the same or newer modification time. */
int update = 0;

/* If nonzero, list each file as it is moved. */
int verbose;

/* If nonzero, stdin is a tty. */
int stdin_tty;

struct option long_options[] =
{
  {"backup", 0, NULL, 'b'},
  {"force", 0, NULL, 'f'},
  {"interactive", 0, NULL, 'i'},
  {"suffix", 1, NULL, 'S'},
  {"update", 0, &update, 1},
  {"verbose", 0, &verbose, 1},
  {"version-control", 1, NULL, 'V'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int errors;
  int make_backups = 0;
  char *version;

  version = getenv ("SIMPLE_BACKUP_SUFFIX");
  if (version)
    simple_backup_suffix = version;
  version = getenv ("VERSION_CONTROL");
  program_name = argv[0];
  interactive = override_mode = verbose = update = 0;
  errors = 0;

  while ((c = getopt_long (argc, argv, "bfiuvS:V:", long_options, (int *) 0))
	 != EOF)
    {
      switch (c)
	{
	case 0:
	  break;
	case 'b':
	  make_backups = 1;
	  break;
	case 'f':
	  interactive = 0;
	  override_mode = 1;
	  break;
	case 'i':
	  interactive = 1;
	  override_mode = 0;
	  break;
	case 'u':
	  update = 1;
	  break;
	case 'v':
	  verbose = 1;
	  break;
	case 'S':
	  simple_backup_suffix = optarg;
	  break;
	case 'V':
	  version = optarg;
	  break;
	default:
	  usage ();
	}
    }
  if (argc < optind + 2)
    usage ();

  if (make_backups)
    backup_type = get_version (version);

  stdin_tty = isatty (0);

  strip_trailing_slashes (argv[argc - 1]);

  if (argc > optind + 2 && !isdir (argv[argc - 1]))
    error (1, 0, "when moving multiple files, last argument must be a directory");

  /* Move each arg but the last onto the last. */
  for (; optind < argc - 1; ++optind)
    errors |= movefile (argv[optind], argv[argc - 1]);

  exit (errors);
}

/* Move file FROM onto TO.  Handles the case when TO is a directory.
   Return 0 if successful, 1 if an error occurred.  */

int
movefile (from, to)
     char *from;
     char *to;
{
  strip_trailing_slashes (from);
  if (isdir (to))
    {
      /* Target is a directory; build full target filename. */
      char *cp;
      char *newto;

      cp = rindex (from, '/');
      if (cp)
	cp++;
      else
	cp = from;

      newto = (char *) alloca (strlen (to) + 1 + strlen (cp) + 1);
      sprintf (newto, "%s/%s", to, cp);
      return do_move (from, newto);
    }
  else
    return do_move (from, to);
}

struct stat to_stats, from_stats;

/* Move FROM onto TO.  Handles cross-filesystem moves.
   If TO is a directory, FROM must be also.
   Return 0 if successful, 1 if an error occurred.  */

int
do_move (from, to)
     char *from;
     char *to;
{
  char *to_backup = NULL;

  if (lstat (from, &from_stats) != 0)
    {
      error (0, errno, "%s", from);
      return 1;
    }

  if (lstat (to, &to_stats) == 0)
    {
      if (from_stats.st_dev == to_stats.st_dev
	  && from_stats.st_ino == to_stats.st_ino)
	{
	  error (0, 0, "`%s' and `%s' are the same file", from, to);
	  return 1;
	}

      if (S_ISDIR (to_stats.st_mode))
	{
	  error (0, 0, "%s: cannot overwrite directory", to);
	  return 1;
	}

      if (!S_ISDIR (from_stats.st_mode) && update
	  && from_stats.st_mtime <= to_stats.st_mtime)
	return 0;

      if (!override_mode && (interactive || stdin_tty)
	  && eaccess_stat (&to_stats, W_OK))
	{
	  fprintf (stderr, "%s: replace `%s', overriding mode %04o? ",
		   program_name, to, to_stats.st_mode & 07777);
	  if (!yesno ())
	    return 0;
	}
      else if (interactive)
	{
	  fprintf (stderr, "%s: replace `%s'? ", program_name, to);
	  if (!yesno ())
	    return 0;
	}

      if (backup_type != none)
	{
	  char *tmp_backup = find_backup_file_name (to);
	  if (tmp_backup == NULL)
	    error (1, 0, "virtual memory exhausted");
	  to_backup = alloca (strlen (tmp_backup) + 1);
	  strcpy (to_backup, tmp_backup);
	  free (tmp_backup);
	  if (rename (to, to_backup))
	    {
	      if (errno != ENOENT)
		{
		  error (0, errno, "cannot backup `%s'", to);
		  return 1;
		}
	      else
		to_backup = NULL;
	    }
	}
    }
  else if (errno != ENOENT)
    {
      error (0, errno, "%s", to);
      return 1;
    }

  if (verbose)
    printf ("%s -> %s\n", from, to);

  if (rename (from, to) == 0)
    {
      return 0;
    }

  if (errno != EXDEV)
    {
      error (0, errno, "cannot move `%s' to `%s'", from, to);
      goto un_backup;
    }

  /* rename failed on cross-filesystem link.  Copy the file instead. */

  if (copy (from, to))
    goto un_backup;
  
  if (unlink (from))
    {
      error (0, errno, "cannot remove `%s'", from);
      return 1;
    }

  return 0;

 un_backup:
  if (to_backup)
    {
      if (rename (to_backup, to))
	error (0, errno, "cannot un-backup `%s'", to);
    }
  return 1;
}

/* Copy file FROM onto file TO.
   Return 1 if an error occurred, 0 if successful. */

int
copy (from, to)
     char *from, *to;
{
  int ifd;
  int ofd;
  char buf[1024 * 8];
  int len;			/* Number of bytes read into `buf'. */
  
  if (!S_ISREG (from_stats.st_mode))
    {
      error (0, 0, "cannot move `%s' across filesystems: Not a regular file",
	     from);
      return 1;
    }
  
  if (unlink (to) && errno != ENOENT)
    {
      error (0, errno, "cannot remove `%s'", to);
      return 1;
    }

  ifd = open (from, O_RDONLY, 0);
  if (ifd < 0)
    {
      error (0, errno, "%s", from);
      return 1;
    }
  ofd = open (to, O_WRONLY | O_CREAT | O_TRUNC, 0600);
  if (ofd < 0)
    {
      error (0, errno, "%s", to);
      close (ifd);
      return 1;
    }
#ifndef FCHMOD_MISSING
  if (fchmod (ofd, from_stats.st_mode & 0777))
    {
      error (0, errno, "%s", to);
      close (ifd);
      close (ofd);
      unlink (to);
      return 1;
    }
#endif
  
  while ((len = read (ifd, buf, sizeof (buf))) > 0)
    {
      int wrote = 0;
      char *bp = buf;
      
      do
	{
	  wrote = write (ofd, bp, len);
	  if (wrote < 0)
	    {
	      error (0, errno, "%s", to);
	      close (ifd);
	      close (ofd);
	      unlink (to);
	      return 1;
	    }
	  bp += wrote;
	  len -= wrote;
	} while (len > 0);
    }
  if (len < 0)
    {
      error (0, errno, "%s", from);
      close (ifd);
      close (ofd);
      unlink (to);
      return 1;
    }

  if (close (ifd) < 0)
    {
      error (0, errno, "%s", from);
      close (ofd);
      return 1;
    }
  if (close (ofd) < 0)
    {
      error (0, errno, "%s", to);
      return 1;
    }
  
#ifdef FCHMOD_MISSING
  if (chmod (to, from_stats.st_mode & 0777))
    {
      error (0, errno, "%s", to);
      return 1;
    }
#endif

  /* Try to copy the old file's modtime and access time.  */
  {
    struct utimbuf tv;

    tv.actime = from_stats.st_atime;
    tv.modtime = from_stats.st_mtime;
    if (utime (to, &tv))
      {
	error (0, errno, "%s", to);
	return 1;
      }
  }

  if (chown (to, from_stats.st_uid, from_stats.st_gid) < 0 && errno != EPERM)
    error (0, errno, "%s", to);

  return 0;
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [options] source dest\n\
       %s [options] source... directory\n\
Options:\n\
       [-bfiuv] [-S backup-suffix] [-V {numbered,existing,simple}]\n\
       [--backup] [--force] [--interactive] [--update] [--verbose]\n\
       [--suffix=backup-suffix] [--version-control={numbered,existing,simple}]\n",
	   program_name, program_name);
  exit (1);
}
