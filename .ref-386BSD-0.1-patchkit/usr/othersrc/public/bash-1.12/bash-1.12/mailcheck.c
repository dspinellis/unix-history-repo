/* mailcheck.c -- The check is in the mail... */

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
#include <sys/types.h>
#include "posixstat.h"
#include <sys/param.h>
#include "shell.h"

#include "maxpath.h"

#ifndef NOW
#define NOW ((time_t)time ((time_t *)0))
#endif

typedef struct {
  char *name;
  time_t access_time;
  time_t mod_time;
  long file_size;
} FILEINFO;

/* The list of remembered mail files. */
FILEINFO **mailfiles = (FILEINFO **)NULL;

/* Number of mail files that we have. */
int mailfiles_count = 0;

/* The last known time that mail was checked. */
int last_time_mail_checked = 0;

/* Returns non-zero if it is time to check mail. */
time_to_check_mail ()
{
  char *temp = get_string_value ("MAILCHECK");
  time_t now = NOW;
  unsigned seconds = 0;

  if ((!temp || sscanf (temp, "%u", &seconds) == 0) ||
      ((now - last_time_mail_checked < seconds)))
    return (0);

  return (1);
}

/* Okay, we have checked the mail.  Perhaps I should make this function
   go away. */
reset_mail_timer ()
{
  last_time_mail_checked = NOW;
}

/* Locate a file in the list.  Return index of
   entry, or -1 if not found. */
find_mail_file (file)
     char *file;
{
  register int index = 0;

  while (index < mailfiles_count)
    if (strcmp ((mailfiles[index])->name, file) == 0) return index;
    else index++;
  return (-1);
}

/* Add this file to the list of remembered files. */
add_mail_file (file)
     char *file;
{
  struct stat finfo;
  char *full_pathname ();
  char *filename = full_pathname (file);
  int index = find_mail_file (file);

  if (index > -1)
    {
      if (stat (filename, &finfo) == 0)
	{
	  mailfiles[index]->mod_time = finfo.st_mtime;
	  mailfiles[index]->access_time = finfo.st_atime;
	  mailfiles[index]->file_size = (long)finfo.st_size;
	}
      free (filename);
      return;
    }

  mailfiles = (FILEINFO **)
    xrealloc (mailfiles,
	      ((++mailfiles_count) * sizeof (FILEINFO *)));

  mailfiles[mailfiles_count - 1] = (FILEINFO *)xmalloc (sizeof (FILEINFO));
  mailfiles[mailfiles_count - 1]->name = filename;
  if (stat (filename, &finfo) == 0)
    {
      mailfiles[mailfiles_count - 1]->access_time = finfo.st_atime;
      mailfiles[mailfiles_count - 1]->mod_time = finfo.st_mtime;
      mailfiles[mailfiles_count - 1]->file_size = finfo.st_size;
    }
  else
    {
      mailfiles[mailfiles_count - 1]->access_time
	= mailfiles[mailfiles_count - 1]->mod_time = (time_t)-1;
      mailfiles[mailfiles_count - 1]->file_size = (long)-1;
    }
}

/* Reset the existing mail files access and modification times to zero. */
reset_mail_files ()
{
  register int i;

  for (i = 0; i < mailfiles_count; i++)
    {
      mailfiles[i]->access_time = mailfiles[i]->mod_time = 0;
      mailfiles[i]->file_size = (long)0;
    }
}

/* Free the information that we have about the remembered mail files. */
free_mail_files ()
{
  while (mailfiles_count--)
    {
      free (mailfiles[mailfiles_count]->name);
      free (mailfiles[mailfiles_count]);
    }

  if (mailfiles)
    free (mailfiles);

  mailfiles_count = 0;
  mailfiles = (FILEINFO **)NULL;
}

/* Return the full pathname of FILE.  Easy.  Filenames that begin
   with a '/' are returned as themselves.  Other filenames have
   the current working directory prepended.  A new string is
   returned in either case. */
char *
full_pathname (file)
     char *file;
{
  char *tilde_expand (), *disposer;

  if (*file == '~')
    file = tilde_expand (file);
  else
    file = savestring (file);

  if (absolute_pathname (file))
    if (*file == '/')
      return (file);

  disposer = file;

  {
    char *current_dir = (char *)xmalloc (2 + MAXPATHLEN + strlen (file));
    if (!getwd (current_dir))
      {
	report_error (current_dir);
	free (current_dir);
	return ((char *)NULL);
      }
    strcat (current_dir, "/");

    /* Turn /foo/./bar into /foo/bar. */
    if (strncmp (file, "./", 2) == 0)
      file += 2;

    strcat (current_dir, file);
    free (disposer);
    return (current_dir);
  }
}

/* Return non-zero if FILE's mod date has changed. */
file_mod_date_changed (file)
     char *file;
{
  time_t time = (time_t)NULL;
  struct stat finfo;

  int index = find_mail_file (file);
  if (index != -1)
    time = mailfiles[index]->mod_time;

  if (stat (file, &finfo) == 0)
    {
      if (finfo.st_size != 0)
	return (time != finfo.st_mtime);
    }
  return (0);
}

/* Return non-zero if FILE's access date has changed. */
file_access_date_changed (file)
     char *file;
{
  time_t time = (time_t)NULL;
  struct stat finfo;

  int index = find_mail_file (file);
  if (index != -1)
    time = mailfiles[index]->access_time;

  if (stat (file, &finfo) == 0)
    {
      if (finfo.st_size != 0)
	return (time != finfo.st_atime);
    }
  return (0);
}

/* Return non-zero if FILE's size has increased. */
file_has_grown (file)
     char *file;
{
  long size = 0L;
  struct stat finfo;

  int i = find_mail_file (file);
  if (i != -1)
    size = mailfiles[i]->file_size;

  if (stat (file, &finfo) == 0)
    {
      return (finfo.st_size > size);
    }
  return (0);
}

#if defined (USG)
#define DEFAULT_MAIL_PATH "/usr/mail/"
#else
#define DEFAULT_MAIL_PATH "/usr/spool/mail/"
#endif

/* Return the colon separated list of pathnames to check for mail. */
char *
get_mailpaths ()
{
  extern char *current_user_name;
  char *mailpaths;

  mailpaths = get_string_value ("MAILPATH");

  if (!mailpaths)
    mailpaths = get_string_value ("MAIL");

  if (!mailpaths)
    {
      mailpaths = (char *)
	alloca (1 + strlen (DEFAULT_MAIL_PATH) + strlen (current_user_name));

      sprintf (mailpaths, "%s%s", DEFAULT_MAIL_PATH, current_user_name);
    }

  return (savestring (mailpaths));
}

/* Remember the dates of the files specified by MAILPATH, or if there is
   no MAILPATH, by the file specified in MAIL.  If neither exists, use a
   default value, which we randomly concoct from using Unix. */
remember_mail_dates ()
{
  char *mailpaths = get_mailpaths ();
  char *mailfile, *extract_colon_unit ();
  int index = 0;
  
  while (mailfile = extract_colon_unit (mailpaths, &index))
    {
      register int i;
      for (i = 0;
	   mailfile[i] && mailfile[i] != '?' && mailfile[i] != '%';
	   i++);
      mailfile[i] = '\0';
      add_mail_file (mailfile);
    }
}

/* check_mail () is useful for more than just checking mail.  Since it has
   the paranoids dream ability of telling you when someone has read your
   mail, it can just as easily be used to tell you when someones .profile
   file has been read, thus letting one know when someone else has logged
   in.  Pretty good, huh? */

/* Check for mail in some files.  If the modification date of any
   of the files in MAILPATH has changed since we last did a
   remember_mail_dates () then mention that the user has mail.
   Special hack:  If the shell variable MAIL_WARNING is on and the
   mail file has been accessed since the last time we remembered, then
   the message "The mail in <mailfile> has been read" is printed. */
check_mail ()
{
  register int string_index;
  char *extract_colon_unit ();
  char *current_mail_file, *you_have_mail_message;
  char *mailpaths = get_mailpaths ();
  int index = 0;
  char *dollar_underscore;

  dollar_underscore = get_string_value ("_");

  if (dollar_underscore)
    dollar_underscore = savestring (dollar_underscore);

  while ((current_mail_file = extract_colon_unit (mailpaths, &index)))
    {
      char *t;
      int use_user_notification;

      if (!*current_mail_file)
	{
	  free (current_mail_file);
	  continue;
	}

      t = full_pathname (current_mail_file);
      free (current_mail_file);
      current_mail_file = t;

      use_user_notification = 0;
      you_have_mail_message = "You have mail in $_";

      for (string_index = 0; current_mail_file[string_index]; string_index++)
	if (current_mail_file[string_index] == '?'
	    || current_mail_file[string_index] == '%')
	  {
	    current_mail_file[string_index] = '\0';
	    you_have_mail_message = current_mail_file + string_index + 1;
	    use_user_notification++;
	    break;
	  }

      if (file_mod_date_changed (current_mail_file))
	{
	  WORD_LIST *tlist, *expand_string ();
	  char *string_list ();
	  int i, file_is_bigger;
	  bind_variable ("_", current_mail_file);

	  /* Have to compute this before the call to add_mail_file, which
	     resets all the information. */
	  file_is_bigger = file_has_grown (current_mail_file);

	  add_mail_file (current_mail_file);

	  i = find_mail_file (current_mail_file);

	  /* If the user has just run a program which manipulates the
	     mail file, then don't bother explaining that the mail
	     file has been manipulated.  Since some systems don't change
	     the access time to be equal to the modification time when
	     the mail in the file is manipulated, check the size also.  If
	     the file has not grown, continue. */
	  if (i != -1 &&
	      (mailfiles[i]->access_time == mailfiles[i]->mod_time) &&
	      !file_is_bigger)
	    goto next_mail_file;

	  if (!use_user_notification)
	    {
	      if (i != -1 &&
		  (mailfiles[i]->access_time < mailfiles[i]->mod_time) &&
		  file_is_bigger)
		you_have_mail_message = "You have new mail in $_";
	    }

	  if ((tlist = expand_string (you_have_mail_message, 1)))
	    {
	      char *tem = string_list (tlist);
	      you_have_mail_message = (char *)alloca (1 + strlen (tem));
	      strcpy (you_have_mail_message, tem);
	      free (tem);
	    }
	  else
	    you_have_mail_message = "";

	  printf ("%s\n", you_have_mail_message);
	  dispose_words (tlist);
	}

      if (find_variable ("MAIL_WARNING")
	  && file_access_date_changed (current_mail_file))
	{
	  add_mail_file (current_mail_file);
	  printf ("The mail in %s has been read!\n", current_mail_file);
	}
    next_mail_file: 
      free (current_mail_file);
    }
  free (mailpaths);

  if (dollar_underscore)
    {
      bind_variable ("_", dollar_underscore);
      free (dollar_underscore);
    }
  else
    unbind_variable ("_");
}
