/* touch -- change modification and access times of files
   Copyright (C) 1987, 1989, 1990, 1991 Free Software Foundation Inc.

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
   -a, --time={atime,access,use}	Change access time only.
   -c, --no-create		Do not create files that do not exist.
   -d, --date=TIME		Specify time and date in various formats.
   -m, --time={mtime,modify}	Change modification time only.
   -r, --file=FILE		Use the time and date of reference file FILE.
   -t TIME			Specify time and date in the form
				`MMDDhhmm[[CC]YY][.ss]'.
   
   If no options are given, -am is the default, using the current time.
   The -r, -t, and -d options are mutually exclusive.  If a file does not
   exist, create it unless -c is given.

   Written by Paul Rubin, Arnold Robbins, Jim Kingdon, David MacKenzie,
   and Randy Smith. */

#include <stdio.h>
#include <ctype.h>
#include <getopt.h>
#include <sys/types.h>
#include "system.h"

#ifdef STDC_HEADERS
#include <time.h>
#else
time_t mktime ();
time_t time ();
#endif

int argmatch ();
int touch ();
time_t get_date ();
time_t posixtime ();
void error ();
void invalid_arg ();
void usage ();

/* Bitmasks for `change_times'. */
#define CH_ATIME 1
#define CH_MTIME 2

/* Which timestamps to change. */
int change_times;

/* (-c) If nonzero, don't create if not already there. */
int no_create;

/* (-d) If nonzero, date supplied on command line in get_date formats. */
int flexible_date;

/* (-r) If nonzero, use times from a reference file. */
int use_ref;

/* (-t) If nonzero, date supplied on command line in POSIX format. */
int posix_date;

/* If nonzero, the only thing we have to do is change both the
   modification and access time to the current time, so we don't
   have to own the file, just be able to read and write it.  */
int amtime_now;

/* New time to use when setting time. */
time_t newtime;

/* File to use for -r. */
char *ref_file;

/* Info about the reference file. */
struct stat ref_stats;

/* The name by which this program was run. */
char *program_name;

struct option longopts[] =
{
  {"time", 1, 0, 130},
  {"no-create", 0, 0, 'c'},
  {"date", 1, 0, 'd'},
  {"file", 1, 0, 'r'},
  {0, 0, 0, 0}
};

/* Valid arguments to the `--time' option. */
char *time_args[] =
{
  "atime", "access", "use", "mtime", "modify", 0
};

/* The bits in `change_times' that those arguments set. */
int time_masks[] =
{
  CH_ATIME, CH_ATIME, CH_ATIME, CH_MTIME, CH_MTIME
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int c, i;
  int date_set = 0;
  int err = 0;

  program_name = argv[0];
  change_times = no_create = use_ref = posix_date = flexible_date = 0;
  newtime = (time_t) -1;

  while ((c = getopt_long (argc, argv, "acd:mr:t:", longopts, (int *) 0))
	 != EOF)
    {
      switch (c)
	{
	case 'a':
	  change_times |= CH_ATIME;
	  break;

	case 'c':
	  no_create++;
	  break;

	case 'd':
	  flexible_date++;
	  newtime = get_date (optarg, NULL);
	  if (newtime == (time_t) -1)
	    error (1, 0, "invalid date format `%s'", optarg);
	  date_set++;
	  break;

	case 'm':
	  change_times |= CH_MTIME;
	  break;

	case 'r':
	  use_ref++;
	  ref_file = optarg;
	  break;

	case 't':
	  posix_date++;
	  newtime = posixtime (optarg);
	  if (newtime == (time_t) -1)
	    error (1, 0, "invalid date format `%s'", optarg);
	  date_set++;
	  break;

	case 130:
	  i = argmatch (optarg, time_args);
	  if (i < 0)
	    {
	      invalid_arg ("time selector", optarg, i);
	      usage ();
	    }
	  change_times |= time_masks[i];
	  break;

	default:
	  usage ();
	}
    }

  if (change_times == 0)
    change_times = CH_ATIME | CH_MTIME;

  if ((use_ref && (posix_date || flexible_date))
      || (posix_date && flexible_date))
    {
      error (0, 0, "cannot specify times from more than one source");
      usage ();
    }

  if (use_ref)
    {
      if (stat (ref_file, &ref_stats))
	error (1, errno, "%s", ref_file);
      date_set++;
    }

  if (!date_set && optind < argc && strcmp (argv[optind - 1], "--"))
    {
      newtime = posixtime (argv[optind]);
      if (newtime != (time_t) -1)
	{
	  optind++;
	  date_set++;
	}
    }
  if (!date_set)
    {
      if ((change_times & (CH_ATIME | CH_MTIME)) == (CH_ATIME | CH_MTIME))
	amtime_now = 1;
      else
	time (&newtime);
    }

  if (optind == argc)
    {
      error (0, 0, "file arguments missing");
      usage ();
    }

  for (; optind < argc; ++optind)
    err += touch (argv[optind]);

  exit (err != 0);
}

/* Update the time of file FILE according to the options given.
   Return 0 if successful, 1 if an error occurs. */

int
touch (file)
     char *file;
{
  int status;
  struct stat sbuf;
  int fd;

  if (stat (file, &sbuf))
    {
      if (errno != ENOENT)
	{
	  error (0, errno, "%s", file);
	  return 1;
	}
      if (no_create)
	return 0;
      fd = creat (file, 0666);
      if (fd == -1)
	{
	  error (0, errno, "%s", file);
	  return 1;
	}
      if (amtime_now)
	{
	  if (close (fd) < 0)
	    {
	      error (0, errno, "%s", file);
	      return 1;
	    }
	  return 0;		/* We've done all we have to. */
	}
      if (fstat (fd, &sbuf))
	{
	  error (0, errno, "%s", file);
	  close (fd);
	  return 1;
	}
      if (close (fd) < 0)
	{
	  error (0, errno, "%s", file);
	  return 1;
	}	
    }

  if (amtime_now)
    {
#if defined (UTIME_NULL_MISSING)
      status = utime_now (file, sbuf.st_size);
#else
      /* Pass NULL to utime so it will not fail if we just have
	 write access to the file, but don't own it.  */
      status = utime (file, NULL);
#endif
    }
  else
    {
      struct utimbuf utb;

      if (use_ref)
	{
	  utb.actime = ref_stats.st_atime;
	  utb.modtime = ref_stats.st_mtime;
	}
      else
	utb.actime = utb.modtime = newtime;

      if (!(change_times & CH_ATIME))
	utb.actime = sbuf.st_atime;

      if (!(change_times & CH_MTIME))
	utb.modtime = sbuf.st_mtime;

      status = utime (file, &utb);
    }
  
  if (status)
    {
      error (0, errno, "%s", file);
      return 1;
    }

  return 0;
}

#if defined (UTIME_NULL_MISSING)
/* Emulate utime (file, NULL) for systems (like 4.3BSD) that do not
   interpret it to set the access and modification times of FILE to
   the current time.  FILESIZE is the correct size of FILE, used to
   make sure empty files are not lengthened to 1 byte.
   Return 0 if successful, -1 if not. */

int
utime_now (file, filesize)
     char *file;
     off_t filesize;
{
  int fd;
  char c;
  int status = 0;

  fd = open (file, O_RDWR, 0666);
  if (fd < 0
      || read (fd, &c, sizeof (char)) < 0
      || lseek (fd, (off_t) 0, SEEK_SET) < 0
      || write (fd, &c, sizeof (char)) < 0
      || ftruncate (fd, filesize) < 0
      || close (fd) < 0)
    status = -1;
  return status;
}
#endif

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-acm] [-r reference-file] [-t MMDDhhmm[[CC]YY][.ss]]\n\
       [-d time] [--time={atime,access,use,mtime,modify}] [--date=time]\n\
       [--file=reference-file] [--no-create] file...\n",
	   program_name);
  exit (1);
}
