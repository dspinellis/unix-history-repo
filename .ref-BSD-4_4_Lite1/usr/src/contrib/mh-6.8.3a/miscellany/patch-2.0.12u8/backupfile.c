/* backupfile.c -- make Emacs style backup file names
   Copyright (C) 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it without restriction.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  */

/* David MacKenzie <djm@ai.mit.edu>.
   Some algorithms adapted from GNU Emacs. */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include "backupfile.h"
#include "config.h"
char *index ();
char *rindex ();
char *malloc ();

#ifdef DIRENT
#include <dirent.h>
#ifdef direct
#undef direct
#endif
#define direct dirent
#define NLENGTH(direct) (strlen((direct)->d_name))
#else /* !DIRENT */
#define NLENGTH(direct) ((direct)->d_namlen)
#ifdef USG
#ifdef SYSNDIR
#include <sys/ndir.h>
#else /* !SYSNDIR */
#include <ndir.h>
#endif /* !SYSNDIR */
#else /* !USG */
#ifdef SYSDIR
#include <sys/dir.h>
#endif /* SYSDIR */
#endif /* !USG */
#endif /* !DIRENT */

#ifndef isascii
#define ISDIGIT(c) (isdigit ((unsigned char) (c)))
#else
#define ISDIGIT(c) (isascii (c) && isdigit (c))
#endif

#if defined (HAVE_UNISTD_H)
#include <unistd.h>
#endif

#if defined (_POSIX_VERSION)
/* POSIX does not require that the d_ino field be present, and some
   systems do not provide it. */
#define REAL_DIR_ENTRY(dp) 1
#else
#define REAL_DIR_ENTRY(dp) ((dp)->d_ino != 0)
#endif

/* Which type of backup file names are generated. */
enum backup_type backup_type = none;

/* The extension added to file names to produce a simple (as opposed
   to numbered) backup file name. */
char *simple_backup_suffix = "~";

char *basename ();
char *dirname ();
static char *concat ();
char *find_backup_file_name ();
static char *make_version_name ();
static int max_backup_version ();
static int version_number ();

#ifndef NODIR
/* Return the name of the new backup file for file FILE,
   allocated with malloc.  Return 0 if out of memory.
   FILE must not end with a '/' unless it is the root directory.
   Do not call this function if backup_type == none. */

char *
find_backup_file_name (file)
     char *file;
{
  char *dir;
  char *base_versions;
  int highest_backup;

  if (backup_type == simple)
    return concat (file, simple_backup_suffix);
  base_versions = concat (basename (file), ".~");
  if (base_versions == 0)
    return 0;
  dir = dirname (file);
  if (dir == 0)
    {
      free (base_versions);
      return 0;
    }
  highest_backup = max_backup_version (base_versions, dir);
  free (base_versions);
  free (dir);
  if (backup_type == numbered_existing && highest_backup == 0)
    return concat (file, simple_backup_suffix);
  return make_version_name (file, highest_backup + 1);
}

/* Return the number of the highest-numbered backup file for file
   FILE in directory DIR.  If there are no numbered backups
   of FILE in DIR, or an error occurs reading DIR, return 0.
   FILE should already have ".~" appended to it. */

static int
max_backup_version (file, dir)
     char *file, *dir;
{
  DIR *dirp;
  struct direct *dp;
  int highest_version;
  int this_version;
  int file_name_length;
  
  dirp = opendir (dir);
  if (!dirp)
    return 0;
  
  highest_version = 0;
  file_name_length = strlen (file);

  while ((dp = readdir (dirp)) != 0)
    {
      if (!REAL_DIR_ENTRY (dp) || NLENGTH (dp) <= file_name_length)
	continue;
      
      this_version = version_number (file, dp->d_name, file_name_length);
      if (this_version > highest_version)
	highest_version = this_version;
    }
  closedir (dirp);
  return highest_version;
}

/* Return a string, allocated with malloc, containing
   "FILE.~VERSION~".  Return 0 if out of memory. */

static char *
make_version_name (file, version)
     char *file;
     int version;
{
  char *backup_name;

  backup_name = malloc (strlen (file) + 16);
  if (backup_name == 0)
    return 0;
  sprintf (backup_name, "%s.~%d~", file, version);
  return backup_name;
}

/* If BACKUP is a numbered backup of BASE, return its version number;
   otherwise return 0.  BASE_LENGTH is the length of BASE.
   BASE should already have ".~" appended to it. */

static int
version_number (base, backup, base_length)
     char *base;
     char *backup;
     int base_length;
{
  int version;
  char *p;
  
  version = 0;
  if (!strncmp (base, backup, base_length) && ISDIGIT (backup[base_length]))
    {
      for (p = &backup[base_length]; ISDIGIT (*p); ++p)
	version = version * 10 + *p - '0';
      if (p[0] != '~' || p[1])
	version = 0;
    }
  return version;
}

/* Return the newly-allocated concatenation of STR1 and STR2.
   If out of memory, return 0. */

static char *
concat (str1, str2)
     char *str1, *str2;
{
  char *newstr;
  char str1_length = strlen (str1);

  newstr = malloc (str1_length + strlen (str2) + 1);
  if (newstr == 0)
    return 0;
  strcpy (newstr, str1);
  strcpy (newstr + str1_length, str2);
  return newstr;
}

/* Return NAME with any leading path stripped off.  */

char *
basename (name)
     char *name;
{
  char *base;

  base = rindex (name, '/');
  return base ? base + 1 : name;
}

/* Return the leading directories part of PATH,
   allocated with malloc.  If out of memory, return 0.
   Assumes that trailing slashes have already been
   removed.  */

char *
dirname (path)
     char *path;
{
  char *newpath;
  char *slash;
  int length;    /* Length of result, not including NUL. */

  slash = rindex (path, '/');
  if (slash == 0)
	{
	  /* File is in the current directory.  */
	  path = ".";
	  length = 1;
	}
  else
	{
	  /* Remove any trailing slashes from result. */
	  while (slash > path && *slash == '/')
		--slash;

	  length = slash - path + 1;
	}
  newpath = malloc (length + 1);
  if (newpath == 0)
    return 0;
  strncpy (newpath, path, length);
  newpath[length] = 0;
  return newpath;
}

/* If ARG is an unambiguous match for an element of the
   null-terminated array OPTLIST, return the index in OPTLIST
   of the matched element, else -1 if it does not match any element
   or -2 if it is ambiguous (is a prefix of more than one element). */

int
argmatch (arg, optlist)
     char *arg;
     char **optlist;
{
  int i;			/* Temporary index in OPTLIST. */
  int arglen;			/* Length of ARG. */
  int matchind = -1;		/* Index of first nonexact match. */
  int ambiguous = 0;		/* If nonzero, multiple nonexact match(es). */
  
  arglen = strlen (arg);
  
  /* Test all elements for either exact match or abbreviated matches.  */
  for (i = 0; optlist[i]; i++)
    {
      if (!strncmp (optlist[i], arg, arglen))
	{
	  if (strlen (optlist[i]) == arglen)
	    /* Exact match found.  */
	    return i;
	  else if (matchind == -1)
	    /* First nonexact match found.  */
	    matchind = i;
	  else
	    /* Second nonexact match found.  */
	    ambiguous = 1;
	}
    }
  if (ambiguous)
    return -2;
  else
    return matchind;
}

/* Error reporting for argmatch.
   KIND is a description of the type of entity that was being matched.
   VALUE is the invalid value that was given.
   PROBLEM is the return value from argmatch. */

void
invalid_arg (kind, value, problem)
     char *kind;
     char *value;
     int problem;
{
  fprintf (stderr, "patch: ");
  if (problem == -1)
    fprintf (stderr, "invalid");
  else				/* Assume -2. */
    fprintf (stderr, "ambiguous");
  fprintf (stderr, " %s `%s'\n", kind, value);
}

static char *backup_args[] =
{
  "never", "simple", "nil", "existing", "t", "numbered", 0
};

static enum backup_type backup_types[] =
{
  simple, simple, numbered_existing, numbered_existing, numbered, numbered
};

/* Return the type of backup indicated by VERSION.
   Unique abbreviations are accepted. */

enum backup_type
get_version (version)
     char *version;
{
  int i;

  if (version == 0 || *version == 0)
    return numbered_existing;
  i = argmatch (version, backup_args);
  if (i >= 0)
    return backup_types[i];
  invalid_arg ("version control type", version, i);
  exit (1);
}
#endif /* NODIR */
