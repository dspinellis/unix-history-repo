/* Read, sort and compare two directories.  Used for GNU DIFF.
   Copyright (C) 1988, 1989 Free Software Foundation, Inc.

This file is part of GNU DIFF.

GNU DIFF is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU DIFF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU DIFF; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "diff.h"

static int compare_names ();

/* Read the directory named DIRNAME and return a sorted vector
   of filenames for its contents.  NONEX nonzero means this directory is
   known to be nonexistent, so return zero files.  */

struct dirdata
{
  int length;			/* # elements in `files' */
  char **files;			/* Sorted names of files in the dir */
};

static struct dirdata
dir_sort (dirname, nonex)
     char *dirname;
     int nonex;
{
  register DIR *reading;
  register struct direct *next;
  struct dirdata dirdata;

  /* Address of block containing the files that are described.  */
  char **files;

  /* Length of block that `files' points to, measured in files.  */
  int nfiles;

  /* Index of first unused in `files'.  */
  int files_index;

  if (nonex)
    {
      dirdata.length = 0;
      dirdata.files = 0;
      return dirdata;
    }

  /* Open the directory and check for errors.  */
  reading = opendir (dirname);
  if (!reading)
    {
      perror_with_name (dirname);
      dirdata.length = -1;
      return dirdata;
    }

  /* Initialize the table of filenames.  */

  nfiles = 100;
  files = (char **) xmalloc (nfiles * sizeof (char *));
  files_index = 0;

  /* Read the directory entries, and insert the subfiles
     into the `files' table.  */

  while (next = readdir (reading))
    {
      /* Ignore the files `.' and `..' */
      if (next->d_name[0] == '.'
	  && (next->d_name[1] == 0
	      || (next->d_name[1] == '.'
		  && next->d_name[2] == 0)))
	continue;

      if (files_index == nfiles)
	{
	  nfiles *= 2;
	  files
	    = (char **) xrealloc (files, sizeof (char *) * nfiles);
	}
      files[files_index++] = concat (next->d_name, "", "");
    }

  closedir (reading);

  /* Sort the table.  */
  qsort (files, files_index, sizeof (char *), compare_names);

  /* Return a description of location and length of the table.  */
  dirdata.files = files;
  dirdata.length = files_index;

  return dirdata;
}

/* Sort the files now in the table.  */

static int
compare_names (file1, file2)
     char **file1, **file2;
{
  return strcmp (*file1, *file2);
}

/* Compare the contents of two directories named NAME1 and NAME2.
   This is a top-level routine; it does everything necessary for diff
   on two directories.

   NONEX1 nonzero says directory NAME1 doesn't exist, but pretend it is
   empty.  Likewise NONEX2.

   HANDLE_FILE is a caller-provided subroutine called to handle each file.
   It gets five operands: dir and name (rel to original working dir) of file
   in dir 1, dir and name pathname of file in dir 2, and the recursion depth.

   For a file that appears in only one of the dirs, one of the name-args
   to HANDLE_FILE is zero.

   DEPTH is the current depth in recursion.

   Returns the maximum of all the values returned by HANDLE_FILE,
   or 2 if trouble is encountered in opening files.  */

int
diff_dirs (name1, name2, handle_file, depth, nonex1, nonex2)
     char *name1, *name2;
     int (*handle_file) ();
     int nonex1, nonex2;
{
  struct dirdata data1, data2;
  register int i1, i2;
  int val = 0;
  int v1;

  /* Get sorted contents of both dirs.  */
  data1 = dir_sort (name1, nonex1);
  data2 = dir_sort (name2, nonex2);
  if (data1.length == -1 || data2.length == -1)
    {
      if (data1.length >= 0)
	free (data1.files);
      if (data2.length >= 0)
	free (data2.files);
      return 2;
    }

  i1 = 0;
  i2 = 0;

  /* If -Sname was specified, and this is the topmost level of comparison,
     ignore all file names less than the specified starting name.  */

  if (dir_start_file && depth == 0)
    {
      while (i1 < data1.length && strcmp (data1.files[i1], dir_start_file) < 0)
	i1++;
      while (i2 < data2.length && strcmp (data2.files[i2], dir_start_file) < 0)
	i2++;
    }

  /* Loop while files remain in one or both dirs.  */
  while (i1 < data1.length || i2 < data2.length)
    {
      int nameorder;

      /* Compare next name in dir 1 with next name in dir 2.
	 At the end of a dir,
	 pretend the "next name" in that dir is very large.  */

      if (i1 == data1.length)
	nameorder = 1;
      else if (i2 == data2.length)
	nameorder = -1;
      else
	nameorder = strcmp (data1.files[i1], data2.files[i2]);

      if (nameorder == 0)
	{
	  /* We have found a file (or subdir) in common between both dirs.
	     Compare the two files.  */
	  v1 = (*handle_file) (name1, data1.files[i1], name2, data2.files[i2],
			       depth + 1);
	  i1++, i2++;
	}
      if (nameorder < 0)
	{
	  /* Next filename in dir 1 is less; that is a file in dir 1 only.  */
	  v1 = (*handle_file) (name1, data1.files[i1], name2, 0, depth + 1);
	  i1++;
	}
      if (nameorder > 0)
	{
	  /* Next filename in dir 2 is less; that is a file in dir 2 only.  */
	  v1 = (*handle_file) (name1, 0, name2, data2.files[i2], depth + 1);
	  i2++;
	}
      if (v1 > val)
	val = v1;
    }
  if (data1.files)
    free (data1.files);
  if (data2.files)
    free (data2.files);

  return val;
}
