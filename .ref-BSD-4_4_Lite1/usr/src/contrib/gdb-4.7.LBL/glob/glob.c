/* File-name wildcard pattern matching for GNU.
   Copyright (C) 1985, 1988, 1989, 1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* To whomever it may concern: I have never seen the code which most
   Unix programs use to perform this function.  I wrote this from scratch
   based on specifications for the pattern matching.  --RMS.  */

#if defined (SHELL)
#  include <config.h>
#endif

#if defined (USG) && !defined (Xenix)
#  if !defined (USGr3) && ! defined (USGr4)
#    define USGr3
#endif /* USGr3 */
#endif /* USG && !Xenix */

#include <sys/types.h>

#if defined (_POSIX_VERSION) || defined (USGr3) || defined (USGr4) || defined (DIRENT)
#  include <dirent.h>
#  define direct dirent
#  define	D_NAMLEN(d) strlen((d)->d_name)
#else
#  define D_NAMLEN(d) ((d)->d_namlen)
#  if defined (Xenix)
#    include <sys/ndir.h>
#  else
#    if defined (USG)
#      include "ndir.h"
#     else
#      include <sys/dir.h>
#    endif
#  endif
#endif	/* USGr3 || DIRENT.  */

#if defined (_POSIX_SOURCE)
/* Posix does not require that the d_ino field be present, and some
   systems do not provide it. */
#define REAL_DIR_ENTRY(dp) 1
#else
#define REAL_DIR_ENTRY(dp) (dp->d_ino != 0)
#endif /* _POSIX_SOURCE */


#if defined (NeXT)
#include <string.h>
#else
#if defined (USG)
#if !defined (isc386)
#  include <memory.h>
#endif
#include <string.h>
#if defined (RISC6000)
extern void bcopy ();
#else /* RISC6000 */
#define bcopy(s, d, n) ((void) memcpy ((d), (s), (n)))
#endif /* RISC6000 */
#define rindex	strrchr

#else /* !USG */
#include <strings.h>

extern void bcopy ();
#endif /* !USG */
#endif /* !NeXT */

/* If the opendir () on your system lets you open non-directory files,
   then we consider that not robust.  Define OPENDIR_NOT_ROBUST in the
   SYSDEP_CFLAGS for your machines entry in machines.h. */
#if defined (OPENDIR_NOT_ROBUST)
#if defined (SHELL)
# include "posixstat.h"
#else
# include <sys/stat.h>
#endif /* SHELL */
#endif /* OPENDIR_NOT_ROBUST */

extern char *malloc (), *realloc ();
extern void free ();

#ifndef NULL
#define NULL 0
#endif

/* Global variable which controls whether or not * matches .*.
   Non-zero means don't match .*.  */
int noglob_dot_filenames = 1;


static int glob_match_after_star ();

/* Return nonzero if PATTERN has any special globbing chars in it.  */
int
glob_pattern_p (pattern)
     char *pattern;
{
  register char *p = pattern;
  register char c;
  int	open = 0;

  while ((c = *p++) != '\0')
    switch (c)
      {
      case '?':
      case '*':
	return 1;

      case '[':		/* Only accept an open brace if there is a close */
	open++;		/* brace to match it.  Bracket expressions must be */
	continue;	/* complete, according to Posix.2 */
      case ']':
	if (open)
	  return 1;
	continue;      

      case '\\':
	if (*p++ == '\0')
	  return 0;
      }

  return 0;
}

/* Match the pattern PATTERN against the string TEXT;
   return 1 if it matches, 0 otherwise.

   A match means the entire string TEXT is used up in matching.

   In the pattern string, `*' matches any sequence of characters,
   `?' matches any character, [SET] matches any character in the specified set,
   [!SET] matches any character not in the specified set.

   A set is composed of characters or ranges; a range looks like
   character hyphen character (as in 0-9 or A-Z).
   [0-9a-zA-Z_] is the set of characters allowed in C identifiers.
   Any other character in the pattern must be matched exactly.

   To suppress the special syntactic significance of any of `[]*?!-\',
   and match the character exactly, precede it with a `\'.

   If DOT_SPECIAL is nonzero,
   `*' and `?' do not match `.' at the beginning of TEXT.  */
int
glob_match (pattern, text, dot_special)
     char *pattern, *text;
     int dot_special;
{
  register char *p = pattern, *t = text;
  register char c;

  while ((c = *p++) != '\0')
    switch (c)
      {
      case '?':
	if (*t == '\0' || (dot_special && t == text && *t == '.'))
	  return 0;
	else
	  ++t;
	break;

      case '\\':
	if (*p++ != *t++)
	  return 0;
	break;

      case '*':
	if (dot_special && t == text && *t == '.')
	  return 0;
	return glob_match_after_star (p, t);

      case '[':
	{
	  register char c1 = *t++;
	  int invert;

	  if (!c1)
	    return (0);

	  invert = ((*p == '!') || (*p == '^'));
	  if (invert)
	    p++;

	  c = *p++;
	  while (1)
	    {
	      register char cstart = c, cend = c;

	      if (c == '\\')
		{
		  cstart = *p++;
		  cend = cstart;
		}

	      if (c == '\0')
		return 0;

	      c = *p++;
	      if (c == '-' && *p != ']')
		{
		  cend = *p++;
		  if (cend == '\\')
		    cend = *p++;
		  if (cend == '\0')
		    return 0;
		  c = *p++;
		}
	      if (c1 >= cstart && c1 <= cend)
		goto match;
	      if (c == ']')
		break;
	    }
	  if (!invert)
	    return 0;
	  break;

	match:
	  /* Skip the rest of the [...] construct that already matched.  */
	  while (c != ']')
	    { 
	      if (c == '\0')
		return 0;
	      c = *p++;
	      if (c == '\0')
		return 0;
	      else if (c == '\\')
		++p;
	    }
	  if (invert)
	    return 0;
	  break;
	}

      default:
	if (c != *t++)
	  return 0;
      }

  return *t == '\0';
}

/* Like glob_match, but match PATTERN against any final segment of TEXT.  */

static int
glob_match_after_star (pattern, text)
     char *pattern, *text;
{
  register char *p = pattern, *t = text;
  register char c, c1;

  while ((c = *p++) == '?' || c == '*')
    if (c == '?' && *t++ == '\0')
      return 0;

  if (c == '\0')
    return 1;

  if (c == '\\')
    c1 = *p;
  else
    c1 = c;

  while (1)
    {
      if ((c == '[' || *t == c1) && glob_match (p - 1, t, 0))
	return 1;
      if (*t++ == '\0')
	return 0;
    }
}

/* Return a vector of names of files in directory DIR
   whose names match glob pattern PAT.
   The names are not in any particular order.
   Wildcards at the beginning of PAT do not match an initial period.

   The vector is terminated by an element that is a null pointer.

   To free the space allocated, first free the vector's elements,
   then free the vector.

   Return 0 if cannot get enough memory to hold the pointer
   and the names.

   Return -1 if cannot access directory DIR.
   Look in errno for more information.  */

char **
glob_vector (pat, dir)
     char *pat;
     char *dir;
{
  struct globval
    {
      struct globval *next;
      char *name;
    };

  DIR *d;
  register struct direct *dp;
  struct globval *lastlink;
  register struct globval *nextlink;
  register char *nextname;
  unsigned int count;
  int lose;
  register char **name_vector;
  register unsigned int i;
#if defined (OPENDIR_NOT_ROBUST)
  struct stat finfo;

  if (stat (dir, &finfo) < 0)
    return ((char **)-1);

  if (!S_ISDIR (finfo.st_mode))
    return ((char **)-1);
#endif /* OPENDIR_NOT_ROBUST */

  d = opendir (dir);
  if (d == NULL)
    return (char **) -1;

  lastlink = 0;
  count = 0;
  lose = 0;

  /* Scan the directory, finding all names that match.
     For each name that matches, allocate a struct globval
     on the stack and store the name in it.
     Chain those structs together; lastlink is the front of the chain.  */
  while (1)
    {
#if defined (SHELL)
      /* Make globbing interruptible in the bash shell. */
      extern int interrupt_state;

      if (interrupt_state)
	{
	  closedir (d);
	  lose = 1;
	  goto lost;
	}
#endif /* SHELL */
	  
      dp = readdir (d);
      if (dp == NULL)
	break;

      /* If this directory entry is not to be used, try again. */
      if (!REAL_DIR_ENTRY (dp))
	continue;

      /* If a dot must be explicity matched, check to see if they do. */
      if (noglob_dot_filenames && dp->d_name[0] == '.' && pat[0] != '.')
	continue;

      if (glob_match (pat, dp->d_name, noglob_dot_filenames))
	{
	  nextlink = (struct globval *) alloca (sizeof (struct globval));
	  nextlink->next = lastlink;
	  nextname = (char *) malloc (strlen(dp->d_name) + 1);
	  if (nextname == NULL)
	    {
	      lose = 1;
	      break;
	    }
	  lastlink = nextlink;
	  nextlink->name = nextname;
	  bcopy (dp->d_name, nextname, strlen(dp->d_name) + 1);
	  ++count;
	}
    }
  (void) closedir (d);

  if (!lose)
    {
      name_vector = (char **) malloc ((count + 1) * sizeof (char *));
      lose |= name_vector == NULL;
    }

  /* Have we run out of memory?	 */
 lost:
  if (lose)
    {
      /* Here free the strings we have got.  */
      while (lastlink)
	{
	  free (lastlink->name);
	  lastlink = lastlink->next;
	}
      return NULL;
    }

  /* Copy the name pointers from the linked list into the vector.  */
  for (i = 0; i < count; ++i)
    {
      name_vector[i] = lastlink->name;
      lastlink = lastlink->next;
    }

  name_vector[count] = NULL;
  return name_vector;
}

/* Return a new array which is the concatenation
   of each string in ARRAY to DIR. */

static char **
glob_dir_to_array (dir, array)
     char *dir, **array;
{
  register unsigned int i, l;
  int add_slash;
  char **result;

  l = strlen (dir);
  if (l == 0)
    return array;

  add_slash = dir[l - 1] != '/';

  i = 0;
  while (array[i] != NULL)
    ++i;

  result = (char **) malloc ((i + 1) * sizeof (char *));
  if (result == NULL)
    return NULL;

  for (i = 0; array[i] != NULL; i++)
    {
      result[i] = (char *) malloc (l + (add_slash ? 1 : 0)
				   + strlen (array[i]) + 1);
      if (result[i] == NULL)
	return NULL;
      sprintf (result[i], "%s%s%s", dir, add_slash ? "/" : "", array[i]);
    }
  result[i] = NULL;

  /* Free the input array.  */
  for (i = 0; array[i] != NULL; i++)
    free (array[i]);
  free ((char *) array);

  return result;
}

/* Do globbing on PATHNAME.  Return an array of pathnames that match,
   marking the end of the array with a null-pointer as an element.
   If no pathnames match, then the array is empty (first element is null).
   If there isn't enough memory, then return NULL.
   If a file system error occurs, return -1; `errno' has the error code.  */
char **
glob_filename (pathname)
     char *pathname;
{
  char **result;
  unsigned int result_size;
  char *directory_name, *filename;
  unsigned int directory_len;

  result = (char **) malloc (sizeof (char *));
  result_size = 1;
  if (result == NULL)
    return NULL;

  result[0] = NULL;

  /* Find the filename.  */
  filename = rindex (pathname, '/');
  if (filename == NULL)
    {
      filename = pathname;
      directory_name = "";
      directory_len = 0;
    }
  else
    {
      directory_len = (filename - pathname) + 1;
      directory_name = (char *) alloca (directory_len + 1);

      bcopy (pathname, directory_name, directory_len);
      directory_name[directory_len] = '\0';
      ++filename;
    }

  /* If directory_name contains globbing characters, then we
     have to expand the previous levels.  Just recurse. */
  if (glob_pattern_p (directory_name))
    {
      char **directories;
      register unsigned int i;

      if (directory_name[directory_len - 1] == '/')
	directory_name[directory_len - 1] = '\0';

      directories = glob_filename (directory_name);

      if (directories == NULL)
	goto memory_error;
      else if ((int) directories == -1)
	return (char **) -1;
      else if (*directories == NULL)
	{
	  free ((char *) directories);
	  return (char **) -1;
	}

      /* We have successfully globbed the preceding directory name.
	 For each name in DIRECTORIES, call glob_vector on it and
	 FILENAME.  Concatenate the results together.  */
      for (i = 0; directories[i] != NULL; ++i)
	{
	  char **temp_results = glob_vector (filename, directories[i]);

	  /* Handle error cases. */
	  if (temp_results == NULL)
	    goto memory_error;
	  else if (temp_results == (char **)-1)
	    /* This filename is probably not a directory.  Ignore it.  */
	    ;
	  else
	    {
	      char **array = glob_dir_to_array (directories[i], temp_results);
	      register unsigned int l;

	      l = 0;
	      while (array[l] != NULL)
		++l;

	      result =
		(char **)realloc (result, (result_size + l) * sizeof (char *));

	      if (result == NULL)
		goto memory_error;

	      for (l = 0; array[l] != NULL; ++l)
		result[result_size++ - 1] = array[l];

	      result[result_size - 1] = NULL;

	      /* Note that the elements of ARRAY are not freed.  */
	      free ((char *) array);
	    }
	}
      /* Free the directories.  */
      for (i = 0; directories[i]; i++)
	free (directories[i]);

      free ((char *) directories);

      return result;
    }

  /* If there is only a directory name, return it. */
  if (*filename == '\0')
    {
      result = (char **) realloc ((char *) result, 2 * sizeof (char *));
      if (result == NULL)
	return NULL;
      result[0] = (char *) malloc (directory_len + 1);
      if (result[0] == NULL)
	goto memory_error;
      bcopy (directory_name, result[0], directory_len + 1);
      result[1] = NULL;
      return result;
    }
  else
    {
      /* Otherwise, just return what glob_vector
	 returns appended to the directory name. */
      char **temp_results = glob_vector (filename,
					 (directory_len == 0
					  ? "." : directory_name));

      if (temp_results == NULL || temp_results == (char **)-1)
	return temp_results;

      return (glob_dir_to_array (directory_name, temp_results));
    }

  /* We get to memory error if the program has run out of memory, or
     if this is the shell, and we have been interrupted. */
 memory_error:
  if (result != NULL)
    {
      register unsigned int i;
      for (i = 0; result[i] != NULL; ++i)
	free (result[i]);
      free ((char *) result);
    }
#if defined (SHELL)
  {
    extern int interrupt_state;

    if (interrupt_state)
      throw_to_top_level ();
  }
#endif /* SHELL */
  return NULL;
}

#ifdef TEST

main (argc, argv)
     int argc;
     char **argv;
{
  unsigned int i;

  for (i = 1; i < argc; ++i)
    {
      char **value = glob_filename (argv[i]);
      if (value == NULL)
	puts ("Out of memory.");
      else if ((int) value == -1)
	perror (argv[i]);
      else
	for (i = 0; value[i] != NULL; i++)
	  puts (value[i]);
    }

  exit (0);
}
#endif	/* TEST.  */
