/* general.c -- Stuff that is used by all files. */

/* Copyright (C) 1987,1989 Free Software Foundation, Inc.

This file is part of GNU Bash, the Bourne Again SHell.

Bash is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any later
version.

Bash is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with Bash; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/param.h>
#include "filecntl.h"

#include "shell.h"
#if defined (USG)
#include <string.h>
#else
#include <strings.h>
#endif /* USG */

#if !defined (USG) || defined (HAVE_RESOURCE)
#include <sys/time.h>
#endif

#include <sys/times.h>
#include "maxpath.h"

#ifndef NULL
#define NULL 0x0
#endif

extern int errno;

/* Make the functions index and rindex if they do not exist. */
#if defined (USG) && !defined (sgi) && !defined (DGUX) && !defined (index)
char *
index (s, c)
     char *s;
{
  return ((char *)strchr (s, c));
}
#endif /* USG && !sgi && !DGUX && !index */

#if defined (USG) && !defined (sgi) && !DGUX && !defined (rindex)
char *
rindex (s, c)
     char *s;
{
  return ((char *)strrchr (s, c));
}
#endif /* USG && !sgi && !DUGX && !rindex */

/* **************************************************************** */
/*								    */
/*		   Memory Allocation and Deallocation.		    */
/*								    */
/* **************************************************************** */

char *
xmalloc (size)
     int size;
{
  register char *temp = (char *)malloc (size);

  if (!temp)
    fatal_error ("Out of virtual memory!");

  return (temp);
}

char *
xrealloc (pointer, size)
     register char *pointer;
     int size;
{
  char *temp;

  if (!pointer)
    temp = (char *)xmalloc (size);
  else
    temp = (char *)realloc (pointer, size);

  if (!temp)
    fatal_error ("Out of virtual memory!");

  return (temp);
}


/* **************************************************************** */
/*								    */
/*		     Integer to String Conversion		    */
/*								    */
/* **************************************************************** */

/* Number of characters that can appear in a string representation
   of an integer.  32 is larger than the string rep of 2^^31 - 1. */
#define MAX_INT_LEN 32

/* Integer to string conversion.  This conses the string; the
   caller should free it. */
char *
itos (i)
     int i;
{
  char *buf, *p, *ret;
  int negative = 0;
  unsigned int ui;

  buf = xmalloc (MAX_INT_LEN);

  if (i < 0)
    {
      negative++;
      i = -i;
    }

  ui = (unsigned int) i;

  buf[MAX_INT_LEN - 1] = '\0';
  p = &buf[MAX_INT_LEN - 2];

  do
    *p-- = (ui % 10) + '0';
  while (ui /= 10);

  if (negative)
    *p-- = '-';

  ret = savestring (p + 1);
  free (buf);
  return (ret);
}

/* Return non-zero if all of the characters in STRING are digits. */
int
all_digits (string)
     char *string;
{
  while (*string)
    {
      if (!digit (*string))
	return (0);
      else
	string++;
    }
  return (1);
}

/* A function to unset no-delay mode on a file descriptor.  Used in shell.c
   to unset it on the fd passed as stdin.  Should be called on stdin if
   readline gets an EAGAIN or EWOULDBLOCK when trying to read input. */

#if !defined (O_NDELAY)
#  if defined (FNDELAY)
#    define O_NDELAY FNDELAY
#  endif
#endif /* O_NDELAY */

/* Make sure no-delay mode is not set on file descriptor FD. */
void
unset_nodelay_mode (fd)
     int fd;
{
  int flags, set = 0;

  if ((flags = fcntl (fd, F_GETFL, 0)) < 0)
    return;

#if defined (O_NONBLOCK)
  if (flags & O_NONBLOCK)
    {
      flags &= ~O_NONBLOCK;
      set++;
    }
#endif /* O_NONBLOCK */

#if defined (O_NDELAY)
  if (flags & O_NDELAY)
    {
      flags &= ~O_NDELAY;
      set++;
    }
#endif /* O_NDELAY */

  if (set)
    fcntl (fd, F_SETFL, flags);
}


/* **************************************************************** */
/*								    */
/*			Generic List Functions			    */
/*								    */
/* **************************************************************** */

/* Call FUNCTION on every member of LIST, a generic list. */
void
map_over_list (list, function)
     GENERIC_LIST *list;
     Function *function;
{
  while (list) {
    (*function) (list);
    list = list->next;
  }
}

/* Call FUNCTION on every string in WORDS. */
void
map_over_words (words, function)
     WORD_LIST *words;
     Function *function;
{
  while (words) {
    (*function)(words->word->word);
    words = words->next;
  }
}

/* Reverse the chain of structures in LIST.  Output the new head
   of the chain.  You should always assign the output value of this
   function to something, or you will lose the chain. */
GENERIC_LIST *
reverse_list (list)
     register GENERIC_LIST *list;
{
  register GENERIC_LIST *next, *prev = (GENERIC_LIST *)NULL;

  while (list) {
    next = list->next;
    list->next = prev;
    prev = list;
    list = next;
  }
  return (prev);
}

/* Return the number of elements in LIST, a generic list. */
int
list_length (list)
     register GENERIC_LIST *list;
{
  register int i;

  for (i = 0; list; list = list->next, i++);
  return (i);
}

/* Delete the element of LIST which satisfies the predicate function COMPARER.
   Returns the element that was deleted, so you can dispose of it, or -1 if
   the element wasn't found.  COMPARER is called with the list element and
   then ARG.  Note that LIST contains the address of a variable which points
   to the list.  You might call this function like this:

   SHELL_VAR *elt = delete_element (&variable_list, check_var_has_name, "foo");
   dispose_variable (elt);
*/
GENERIC_LIST *
delete_element (list, comparer, arg)
     GENERIC_LIST **list;
     Function *comparer;
{
  register GENERIC_LIST *prev = (GENERIC_LIST *)NULL;
  register GENERIC_LIST *temp = *list;

  while (temp) {
    if ((*comparer) (temp, arg)) {
      if (prev) prev->next = temp->next;
      else *list = temp->next;
      return (temp);
    }
    prev = temp;
    temp = temp->next;
  }
  return ((GENERIC_LIST *)-1);
}

/* Find NAME in ARRAY.  Return the index of NAME, or -1 if not present.
   ARRAY shoudl be NULL terminated. */
int
find_name_in_list (name, array)
     char *name, *array[];
{
  int i;

  for (i=0; array[i]; i++)
    if (strcmp (name, array[i]) == 0)
      return (i);

  return (-1);
}

/* Return the length of ARRAY, a NULL terminated array of char *. */
int
array_len (array)
     register char **array;
{
  register int i;
  for (i=0; array[i]; i++);
  return (i);
}

/* Free the contents of ARRAY, a NULL terminated array of char *. */
void
free_array (array)
     register char **array;
{
  register int i = 0;

  if (!array) return;

  while (array[i])
    free (array[i++]);
  free (array);
}

/* Append LIST2 to LIST1.  Return the header of the list. */
GENERIC_LIST *
list_append (head, tail)
     GENERIC_LIST *head, *tail;
{
  register GENERIC_LIST *t_head = head;

  if (!t_head)
    return (tail);

  while (t_head->next) t_head = t_head->next;
  t_head->next = tail;
  return (head);
}

/* Some random string stuff. */

/* Remove all leading whitespace from STRING.  This includes
   newlines.  STRING should be terminated with a zero. */
void
strip_leading (string)
     char *string;
{
  char *start = string;

  while (*string && (whitespace (*string) || *string == '\n')) string++;

  if (string != start)
    {
      int len = strlen (string);
      bcopy (string, start, len);
      start[len] = '\0';
    }
}

/* Remove all trailing whitespace from STRING.  This includes
   newlines.  If NEWLINES_ONLY is non-zero, only trailing newlines
   are removed.  STRING should be terminated with a zero. */
void
strip_trailing (string, newlines_only)
     char *string;
     int newlines_only;
{
  int len = strlen (string) - 1;

  while (len >= 0)
    {
      if ((newlines_only && string[len] == '\n') ||
          (!newlines_only && whitespace (string[len])))
        len--;
      else
        break;
    }
  string[len + 1] = '\0';
}


/* Remove the last N directories from PATH.  Do not PATH blank.
   PATH must contain enoung space for MAXPATHLEN characters. */
static void
pathname_backup (path, n)
     char *path;
     int n;
{
  register char *p;

  if (!*path)
    return;

  p = path + (strlen (path) - 1);

  while (n--)
    {
      while (*p == '/' && p != path)
	p--;

      while (*p != '/' && p != path)
	p--;

      *++p = '\0';
    }
}

static char current_path[MAXPATHLEN];

/* Turn STRING (a pathname) into an absolute pathname, assuming that
   DOT_PATH contains the symbolic location of '.'.  This always
   returns a new string, even if STRING was an absolute pathname to
   begin with. */
char *
make_absolute (string, dot_path)
     char *string, *dot_path;
{
  register char *cp;

  if (!dot_path || *string == '/')
    return (savestring (string));

  strcpy (current_path, dot_path);

  if (!current_path[0])
    strcpy (current_path, "./");

  cp = current_path + (strlen (current_path) - 1);

  if (*cp++ != '/')
    *cp++ = '/';

  *cp = '\0';

  while (*string)
    {
      if (*string == '.')
	{
	  if (!string[1])
	    return (savestring (current_path));

	  if (string[1] == '/')
	    {
	      string += 2;
	      continue;
	    }

	  if (string[1] == '.' && (string[2] == '/' || !string[2]))
	    {
	      string += 2;

	      if (*string)
		string++;

	      pathname_backup (current_path, 1);
	      cp = current_path + strlen (current_path);
	      continue;
	    }
	}

      while (*string && *string != '/')
	*cp++ = *string++;

      if (*string)
	*cp++ = *string++;

      *cp = '\0';
    }
  return (savestring (current_path));
}

/* Return 1 if STRING contains an absolute pathname, else 0. */
int
absolute_pathname (string)
     char *string;
{
  if (!string || !*string)
    return (0);

  if (*string == '/')
    return (1);

  if (*string++ == '.')
    {
      if ((!*string) || *string == '/')
	return (1);

      if (*string++ == '.')
	if (!*string || *string == '/')
	  return (1);
    }
  return (0);
}

/* Return 1 if STRING is an absolute program name; it is absolute if it
   contains any slashes.  This is used to decide whether or not to look
   up through $PATH. */
int
absolute_program (string)
     char *string;
{
  return ((char *)index (string, '/') != (char *)NULL);
}

/* Return the `basename' of the pathname in STRING (the stuff after the
   last '/').  If STRING is not a full pathname, simply return it. */
char *
base_pathname (string)
     char *string;
{
  char *p = (char *)rindex (string, '/');

  if (!absolute_pathname (string))
    return (string);

  if (p)
    return (++p);
  else
    return (string);
}

/* Determine if s2 occurs in s1.  If so, return a pointer to the
   match in s1.  The compare is case insensitive. */
char *
strindex (s1, s2)
     register char *s1, *s2;
{
  register int i, l = strlen (s2);
  register int len = strlen (s1);

  for (i = 0; (len - i) >= l; i++)
    if (strnicmp (&s1[i], s2, l) == 0)
      return (s1 + i);
  return ((char *)NULL);
}

#if !defined (to_upper)
#define lowercase_p(c) (((c) > ('a' - 1) && (c) < ('z' + 1)))
#define uppercase_p(c) (((c) > ('A' - 1) && (c) < ('Z' + 1)))
#define pure_alphabetic(c) (lowercase_p(c) || uppercase_p(c))
#define to_upper(c) (lowercase_p(c) ? ((c) - 32) : (c))
#define to_lower(c) (uppercase_p(c) ? ((c) + 32) : (c))
#endif /* to_upper */

/* Compare at most COUNT characters from string1 to string2.  Case
   doesn't matter. */
int
strnicmp (string1, string2, count)
     char *string1, *string2;
{
  register char ch1, ch2;

  while (count) {
    ch1 = *string1++;
    ch2 = *string2++;
    if (to_upper(ch1) == to_upper(ch2))
      count--;
    else break;
  }
  return (count);
}

/* strcmp (), but caseless. */
int
stricmp (string1, string2)
     char *string1, *string2;
{
  register char ch1, ch2;

  while (*string1 && *string2) {
    ch1 = *string1++;
    ch2 = *string2++;
    if (to_upper(ch1) != to_upper(ch2))
      return (1);
  }
  return (*string1 | *string2);
}

/* Return a string corresponding to the error number E.  From
   the ANSI C spec. */
#if defined (strerror)
#undef strerror
#endif

#if !defined (HAVE_STRERROR)
char *
strerror (e)
     int e;
{
  extern int sys_nerr;
  extern char *sys_errlist[];
  static char emsg[40];

  if (e > 0 && e < sys_nerr)
    return (sys_errlist[e]);
  else
    {
      sprintf (emsg, "Unknown error %d", e);
      return (&emsg[0]);
    }
}
#endif /* HAVE_STRERROR */

#if !defined (USG) || defined (HAVE_RESOURCE)
/* Print the contents of a struct timeval * in a standard way. */
void
print_timeval (tvp)
     struct timeval *tvp;
{
  int minutes, seconds_fraction;
  long seconds;

  seconds = tvp->tv_sec;

  seconds_fraction = tvp->tv_usec % 1000000;
  seconds_fraction = (seconds_fraction * 100) / 1000000;

  minutes = seconds / 60;
  seconds %= 60;

  printf ("%0dm%0d.%02ds",  minutes, seconds, seconds_fraction);
}
#endif

/* Print the time defined by a time_t (returned by the `times' and `time'
   system calls) in a standard way.  This is scaled in terms of HZ, which
   is what is returned by the `times' call. */

#if !defined (BrainDeath)
#  if !defined (HZ)
#    if defined (USG)
#      define HZ 100		/* From my Sys V.3.2 manual for times(2) */
#    else
#      define HZ 60		/* HZ is always 60 on BSD systems */
#    endif /* USG */
#  endif /* HZ */

void
print_time_in_hz (t)
  time_t t;
{
  int minutes, seconds_fraction;
  long seconds;

  seconds_fraction = t % HZ;
  seconds_fraction = (seconds_fraction * 100) / HZ;

  seconds = t / HZ;

  minutes = seconds / 60;
  seconds %= 60;

  printf ("%0dm%0d.%02ds",  minutes, seconds, seconds_fraction);
}
#endif /* BrainDeath */

#if !defined (HAVE_DUP2)
/* Replacement for dup2 (), for those systems which either don't have it,
   or supply one with broken behaviour. */
int
dup2 (fd1, fd2)
     int fd1, fd2;
{
  int saved_errno, r;

  /* If FD1 is not a valid file descriptor, then return immediately with
     an error. */
  if (fcntl (fd1, F_GETFL, 0) == -1)
    return (-1);

  if (fd2 < 0 || fd2 >= getdtablesize ())
    {
      errno = EBADF;
      return (-1);
    }

  if (fd1 == fd2)
    return (0);

  saved_errno = errno;

  (void) close (fd2);
  r = fcntl (fd1, F_DUPFD, fd2);

  if (r >= 0)
    errno = saved_errno;
  else
    if (errno == EINVAL)
      errno = EBADF;

  /* Force the new file descriptor to remain open across exec () calls. */
  SET_OPEN_ON_EXEC (fd2);
  return (r);
}
#endif /* !HAVE_DUP2 */

/*
 * Return the total number of available file descriptors.
 *
 * On some systems, like 4.2BSD and its descendents, there is a system call
 * that returns the size of the descriptor table: getdtablesize().  There are
 * lots of ways to emulate this on non-BSD systems.
 *
 * On System V.3, this can be obtained via a call to ulimit:
 *	return (ulimit(4, 0L));
 *
 * On other System V systems, NOFILE is defined in /usr/include/sys/param.h
 * (this is what we assume below), so we can simply use it:
 *	return (NOFILE);
 *
 * On POSIX systems, there are specific functions for retrieving various
 * configuration parameters:
 *	return (sysconf(_SC_OPEN_MAX));
 *
 */

#if defined (USG) || defined (HPUX)
int
getdtablesize ()
{
#  if defined (_POSIX_VERSION) && defined (_SC_OPEN_MAX)
  return (sysconf(_SC_OPEN_MAX));	/* Posix systems use sysconf */
#  else /* ! (_POSIX_VERSION && _SC_OPEN_MAX) */
#    if defined (USGr3)
  return (ulimit (4, 0L));	/* System V.3 systems use ulimit(4, 0L) */
#    else /* !USGr3 */
#      if defined (NOFILE)	/* Other systems use NOFILE */
  return (NOFILE);
#      else /* !NOFILE */
  return (20);			/* XXX - traditional value is 20 */
#      endif /* !NOFILE */
#    endif /* !USGr3 */
#  endif /* ! (_POSIX_VERSION && _SC_OPEN_MAX) */
}
#endif /* USG && !defined USGr4 */

#if defined (USG) && !defined (sgi)

#if !defined (RISC6000)
bcopy(s,d,n) char *d,*s; { memcpy (d, s, n); }
bzero(s,n) char *s; int n; { memset(s, '\0', n); }
#endif /* RISC6000 */

#if !defined (HAVE_GETWD)
char *
getwd (string)
     char *string;
{
  extern char *getcwd ();
  char *result;

  result = getcwd (string, MAXPATHLEN);
  if (result == NULL)
    strcpy (string, "getwd: cannot access parent directories");
  return (result);
}
#endif /* !HAVE_GETWD */

#if !defined (HPUX)
#include <sys/utsname.h>
int
gethostname (name, namelen)
     char *name;
     int namelen;
{
  int i;
  struct utsname ut;

  --namelen;

  uname (&ut);
  i = strlen (ut.nodename) + 1;
  strncpy (name, ut.nodename, i < namelen ? i : namelen);
  name[namelen] = '\0';
  return (0);
}
#endif /* !HPUX */
#endif /* USG && !sgi */

/* A slightly related function.  Get the prettiest name of this
   directory possible. */
static char tdir[MAXPATHLEN];

/* Return a pretty pathname.  If the first part of the pathname is
   the same as $HOME, then replace that with `~'.  */
char *
polite_directory_format (name)
     char *name;
{
  char *home = get_string_value ("HOME");
  int l = home ? strlen (home) : 0;

  if (l > 1 && strncmp (home, name, l) == 0 && (!name[l] || name[l] == '/'))
    {
      strcpy (tdir + 1, name + l);
      tdir[0] = '~';
      return (tdir);
    }
  else
    return (name);
}

#if defined (USG) || (defined (_POSIX_VERSION) && defined (Ultrix))
int
sysv_getc (stream)
     FILE *stream;
{
  int result;
  char c;

  while (1)
    {
      result = read (fileno (stream), &c, sizeof (char));

      if (result == 0)
	return (EOF);

      if (result == sizeof (char))
	return (c);

      if (errno != EINTR)
	return (EOF);
    }
}

/* USG and POSIX systems do not have killpg ().  But we use it in
   jobs.c, nojobs.c and builtins.c. */
#if !defined (_POSIX_VERSION)
#define pid_t int
#endif /* _POSIX_VERSION */

int
killpg (pgrp, sig)
     pid_t pgrp;
     int sig;
{
  int result;

  result = kill (-pgrp, sig);
  return (result);
}
#endif /* USG  || _POSIX_VERSION */

/* **************************************************************** */
/*								    */
/*		    Tilde Initialization and Expansion		    */
/*								    */
/* **************************************************************** */

/* If tilde_expand hasn't been able to expand the text, perhaps it
   is a special shell expansion.  This function is installed as the
   tilde_expansion_failure_hook.  It knows how to expand ~- and ~+. */
static char *
bash_tilde_expand (text)
     char *text;
{
  char *result = (char *)NULL;

  if (strcmp (text, "-") == 0)
    result = get_string_value ("OLDPWD");
  else if (strcmp (text, "+") == 0)
    result = get_string_value ("PWD");

  if (result)
    result = savestring (result);

  return (result);
}

/* Initialize the tilde expander.  In Bash, we handle `~-' and `~+', as
   well as handling special tilde prefixes; `:~" and `=~' are indications
   that we should do tilde expansion. */
void
tilde_initialize ()
{
  extern Function *tilde_expansion_failure_hook;
  extern char **tilde_additional_prefixes, **tilde_additional_suffixes;
  static int times_called = 0;

  /* Tell the tilde expander that we want a crack if it fails. */
  tilde_expansion_failure_hook = (Function *)bash_tilde_expand;

  /* Tell the tilde expander about special strings which start a tilde
     expansion, and the special strings that end one.  Only do this once.
     tilde_initialize () is called from within bashline_reinitialize (). */
  if (times_called == 0)
    {
      tilde_additional_prefixes = (char **)xmalloc (3 * sizeof (char *));
      tilde_additional_prefixes[0] = "=~";
      tilde_additional_prefixes[1] = ":~";
      tilde_additional_prefixes[2] = (char *)NULL;

      tilde_additional_suffixes = (char **)xmalloc (3 * sizeof (char *));
      tilde_additional_suffixes[0] = ":";
      tilde_additional_suffixes[1] = "=~";
      tilde_additional_suffixes[2] = (char *)NULL;
    }
  times_called++;
}
