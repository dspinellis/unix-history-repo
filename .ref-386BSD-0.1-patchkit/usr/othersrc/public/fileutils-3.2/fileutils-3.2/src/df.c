/* df - summarize free disk space
   Copyright (C) 1991 Free Software Foundation, Inc.

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

/* Usage: df [-aikP] [-t fstype] [--all] [--inodes] [--type fstype]
   [--kilobytes] [--portability] [path...]

   Options:
   -a, --all		List all filesystems, even zero-size ones.
   -i, --inodes		List inode usage information instead of block usage.
   -k, --kilobytes	Print sizes in 1K blocks instead of 512-byte blocks.
   -P, --portability	Use the POSIX output format (one line per filesystem).
   -t, --type fstype	Limit the listing to filesystems of type `fstype'.
			Multiple -t options can be given.
			By default, all filesystem types are listed.

   Written by David MacKenzie <djm@gnu.ai.mit.edu> */

#include <stdio.h>
#include <sys/types.h>
#include <getopt.h>
#include "mountlist.h"
#include "fsusage.h"
#include "system.h"

char *strstr ();
char *xmalloc ();
char *xstrdup ();
int fs_to_list ();
void add_fs_type ();
void error ();
void print_header ();
void show_entry ();
void show_all_entries ();
void show_dev ();
void show_disk ();
void show_point ();
void usage ();

/* If nonzero, show inode information. */
int inode_format;

/* If nonzero, show even filesystems with zero size or
   uninteresting types. */
int show_all_fs;

/* If nonzero, use 1K blocks instead of 512-byte blocks. */
int kilobyte_blocks;

/* If nonzero, use the POSIX output format.  */
int posix_format;

/* Nonzero if errors have occurred. */
int exit_status;

/* Name this program was run with. */
char *program_name;

/* A filesystem type to display. */

struct fs_select
{
  char *fs_name;
  struct fs_select *fs_next;
};

/* Linked list of filesystem types to display.
   If `fs_list' is NULL, list all types.
   This table is generated dynamically from command-line options,
   rather than hardcoding into the program what it thinks are the
   valid filesystem types; let the user specify any filesystem type
   they want to, and if there are any filesystems of that type, they
   will be shown.

   Some filesystem types:
   4.2 4.3 ufs nfs swap ignore io vm */

struct fs_select *fs_list;

/* Linked list of mounted filesystems. */
struct mount_entry *mount_list;

struct option long_options[] =
{
  {"all", 0, &show_all_fs, 1},
  {"inodes", 0, &inode_format, 1},
  {"kilobytes", 0, &kilobyte_blocks, 1},
  {"portability", 0, &posix_format, 1},
  {"type", 1, 0, 't'},
  {NULL, 0, NULL, 0}
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int i;
  struct stat *stats;

  program_name = argv[0];
  fs_list = NULL;
  inode_format = 0;
  show_all_fs = 0;
  kilobyte_blocks = getenv ("POSIXLY_CORRECT") == 0;
  posix_format = 0;
  exit_status = 0;

  while ((i = getopt_long (argc, argv, "aikPt:v", long_options, (int *) 0))
	 != EOF)
    {
      switch (i)
	{
	case 0:			/* Long option. */
	  break;
	case 'a':
	  show_all_fs = 1;
	  break;
	case 'i':
	  inode_format = 1;
	  break;
	case 'k':
	  kilobyte_blocks = 1;
	  break;
	case 'P':
	  posix_format = 1;
	  break;
	case 't':
	  add_fs_type (optarg);
	  break;
	case 'v':		/* For SysV compatibility. */
	  break;
	default:
	  usage ();
	}
    }

  if (optind != argc)
    {
      /* Display explicitly requested empty filesystems. */
      show_all_fs = 1;

      /* stat all the given entries to make sure they get automounted,
	 if necessary, before reading the filesystem table.  */
      stats = (struct stat *)
	xmalloc ((argc - optind) * sizeof (struct stat));
      for (i = optind; i < argc; ++i)
	if (stat (argv[i], &stats[i - optind]))
	  {
	    error (0, errno, "%s", argv[i]);
	    exit_status = 1;
	    argv[i] = NULL;
	  }
    }

  mount_list = read_filesystem_list (fs_list != NULL, show_all_fs);
  if (mount_list == NULL)
    error (1, errno, "cannot read table of mounted filesystems");

  print_header ();

  if (optind == argc)
    show_all_entries ();
  else
    for (i = optind; i < argc; ++i)
      if (argv[i])
	show_entry (argv[i], &stats[i - optind]);

  exit (exit_status);
}

void
print_header ()
{
  if (inode_format)
    printf ("Filesystem             IUsed   IFree  %%IUsed");
  else
    printf ("Filesystem         %s  Used Available Capacity",
	    kilobyte_blocks ? "1024-blocks" : " 512-blocks");
  printf (" Mounted on\n");
}

/* Show all mounted filesystems, except perhaps those that are of
   an unselected type or are empty. */

void
show_all_entries ()
{
  struct mount_entry *me;

  for (me = mount_list; me; me = me->me_next)
    show_dev (me->me_devname, me->me_mountdir, me->me_type);
}

/* Determine what kind of node PATH is and show the disk usage
   for it.  STATP is the results of `stat' on PATH.  */

void
show_entry (path, statp)
     char *path;
     struct stat *statp;
{
  if (S_ISBLK (statp->st_mode) || S_ISCHR (statp->st_mode))
    show_disk (path);
  else
    show_point (path, statp);
}

/* Identify the directory, if any, that device
   DISK is mounted on, and show its disk usage.  */

void
show_disk (disk)
     char *disk;
{
  struct mount_entry *me;

  for (me = mount_list; me; me = me->me_next)
    if (!strcmp (disk, me->me_devname))
      {
	show_dev (me->me_devname, me->me_mountdir, me->me_type);
	return;
      }
  /* No filesystem is mounted on DISK. */
  show_dev (disk, (char *) NULL, (char *) NULL);
}

/* Figure out which device file or directory POINT is mounted on
   and show its disk usage.
   STATP is the results of `stat' on POINT.  */

void
show_point (point, statp)
     char *point;
     struct stat *statp;
{
  struct stat disk_stats;
  struct mount_entry *me;

  for (me = mount_list; me; me = me->me_next)
    {
      if (me->me_dev == (dev_t) -1)
	{
	  if (stat (me->me_mountdir, &disk_stats) == 0)
	    me->me_dev = disk_stats.st_dev;
	  else
	    {
	      error (0, errno, "%s", me->me_mountdir);
	      exit_status = 1;
	      me->me_dev = -2;	/* So we won't try and fail repeatedly. */
	    }
	}

      if (statp->st_dev == me->me_dev)
	{
	  show_dev (me->me_devname, me->me_mountdir, me->me_type);
	  return;
	}
    }
  error (0, 0, "cannot find mount point for %s", point);
  exit_status = 1;
}

/* Display a space listing for the disk device with absolute path DISK.
   If MOUNT_POINT is non-NULL, it is the path of the root of the
   filesystem on DISK.
   If FSTYPE is non-NULL, it is the type of the filesystem on DISK. */

void
show_dev (disk, mount_point, fstype)
     char *disk;
     char *mount_point;
     char *fstype;
{
  struct fs_usage fsu;
  long blocks_used;
  long blocks_percent_used;
  long inodes_used;
  long inodes_percent_used;
  char *stat_file;

  if (!fs_to_list (fstype))
    return;

  /* If MOUNT_POINT is NULL, then the filesystem is not mounted, and this
     program reports on the filesystem that the special file is on.
     It would be better to somehow report on the unmounted filesystem,
     but statfs doesn't work on those on many systems. */
  stat_file = mount_point ? mount_point : disk;

  sync ();
  if (get_fs_usage (stat_file, &fsu))
    {
      error (0, errno, "%s", stat_file);
      exit_status = 1;
      return;
    }

  if (!kilobyte_blocks)
    {
      fsu.fsu_blocks *= 2;
      fsu.fsu_bfree *= 2;
      fsu.fsu_bavail *= 2;
    }

  if (fsu.fsu_blocks == 0)
    {
      if (show_all_fs == 0)
	return;
      blocks_used = fsu.fsu_bavail = blocks_percent_used = 0;
    }
  else
    {
      blocks_used = fsu.fsu_blocks - fsu.fsu_bfree;
      blocks_percent_used = (long)
	(blocks_used * 100.0 / (blocks_used + fsu.fsu_bavail) + 0.5);
    }

  if (fsu.fsu_files == 0)
    {
      inodes_used = fsu.fsu_ffree = inodes_percent_used = 0;
    }
  else
    {
      inodes_used = fsu.fsu_files - fsu.fsu_ffree;
      inodes_percent_used = (long)
	(inodes_used * 100.0 / fsu.fsu_files + 0.5);
    }

  printf ("%-20s", disk);
  if (strlen (disk) > 20 && !posix_format)
    printf ("\n                    ");

  if (inode_format)
    printf (" %7ld %7ld %5ld%%",
	    inodes_used, fsu.fsu_ffree, inodes_percent_used);
  else
    printf (" %7ld %7ld  %7ld  %5ld%% ",
	    fsu.fsu_blocks, blocks_used, fsu.fsu_bavail, blocks_percent_used);

  if (mount_point)
    printf ("  %s", mount_point);
  putchar ('\n');
}

/* Add FSTYPE to the list of filesystem types to display. */

void
add_fs_type (fstype)
     char *fstype;
{
  struct fs_select *fsp;

  fsp = (struct fs_select *) xmalloc (sizeof (struct fs_select));
  fsp->fs_name = fstype;
  fsp->fs_next = fs_list;
  fs_list = fsp;
}

/* If FSTYPE is a type of filesystem that should be listed,
   return nonzero, else zero. */

int
fs_to_list (fstype)
     char *fstype;
{
  struct fs_select *fsp;

  if (fs_list == NULL || fstype == NULL)
    return 1;
  for (fsp = fs_list; fsp; fsp = fsp->fs_next)
    if (!strcmp (fstype, fsp->fs_name))
      return 1;
  return 0;
}

void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-aikPv] [-t fstype] [--all] [--inodes] [--type fstype]\n\
       [--kilobytes] [--portability] [path...]\n",
	   program_name);
  exit (1);
}
