/* cp-aux.c  -- file copying (auxiliary routines)
   Copyright (C) 1989, 1990, 1991 Free Software Foundation.

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
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Written by Torbjorn Granlund, Sweden (tege@sics.se). */

#include <stdio.h>

#include "cp.h"

extern char *program_name;

void
usage (reason)
     char *reason;
{
  if (reason != NULL)
    fprintf (stderr, "%s: %s\n", program_name, reason);

  fprintf (stderr, "\
Usage: %s [options] source dest\n\
       %s [options] source... directory\n\
Options:\n\
       [-abdfilprsuvxPR] [-S backup-suffix] [-V {numbered,existing,simple}]\n\
       [--backup] [--no-dereference] [--force] [--interactive]\n\
       [--one-file-system] [--preserve] [--recursive] [--update] [--verbose]\n\
       [--suffix=backup-suffix] [--version-control={numbered,existing,simple}]\n\
       [--archive] [--path] [--link] [--symbolic-link]\n",
	   program_name, program_name);

  exit (2);
}

int
is_ancestor (sb, ancestors)
     struct stat *sb;
     struct dir_list *ancestors;
{
  while (ancestors != 0)
    {
      if (ancestors->ino == sb->st_ino && ancestors->dev == sb->st_dev)
	return 1;
      ancestors = ancestors->parent;
    }
  return 0;
}
