/* Copyright (C) 1985 Richard M. Stallman and Dick King

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include "config.h"
#include "lisp.h"
#include "paths.h"
#include "buffer.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <errno.h>
#include <sys/file.h>
#ifdef USG
#include <fcntl.h>
#endif /* USG */

#ifdef CLASH_DETECTION

/* lock_file locks file fn,
   meaning it serves notice on the world that you intend to edit that file.
   This should be done only when about to modify a file-visiting
   buffer previously unmodified.
   Do not (normally) call lock_buffer for a buffer already modified,
   as either the file is already locked, or the user has already
   decided to go ahead without locking.

   When lock_buffer returns, either the lock is locked for us,
   or the user has said to go ahead without locking.

   If the file is locked by someone else, lock_buffer calls
   ask-user-about-lock (a Lisp function) with two arguments,
   the file name and the name of the user who did the locking.
   This function can signal an error, or return t meaning
   take away the lock, or return nil meaning ignore the lock.  */

/* The lock file name is the file name with "/" replaced by "!"
   and put in the Emacs lock directory.  */
/* (ie., /ka/king/junk.tex -> /!/!ka!king!junk.tex). */

void
lock_file (fn)
     register Lisp_Object fn;
{
  register int fd;
  register Lisp_Object attack;
  register char *lfname;
  struct stat s;
  struct passwd *the_pw;
  extern struct passwd *getpwuid ();

  /* Create the name of the lock-file for file fn */
  lfname = (char *) alloca (XSTRING (fn)->size + strlen (PATH_LOCK) + 1);
  fill_in_lock_file_name (lfname, fn);

  /* Try to lock the lock. */
  if (lock_if_free (lfname) <= 0)
    /* Return now if we have locked it, or if lock dir does not exist */
    return;

  /* Else consider breaking the lock */
  the_pw = 0;
  if (lstat (lfname, &s) == 0)
    the_pw = getpwuid (s.st_uid);
  attack = call2 (intern ("ask-user-about-lock"), fn,
		  the_pw == 0 ? Qnil : build_string (the_pw->pw_name));
  if (!NULL (attack))
    /* User says take the lock */
    {
      lock_superlock (lfname);
      lock_file_1 (lfname, O_WRONLY) ;
      unlink (PATH_SUPERLOCK);
      return;
    }
  /* User says ignore the lock */
}

fill_in_lock_file_name (lockfile, fn)
     register char *lockfile;
     register Lisp_Object fn;
{
  register char *p;

  strcpy (lockfile, PATH_LOCK);

  p = lockfile + strlen (lockfile);

  strcpy (p, XSTRING (fn)->data);

  for (; *p; p++)
    {
      if (*p == '/')
	*p = '!';
    }
}

/* Lock the lock file named LFNAME.
   If MODE is O_WRONLY, we do so even if it is already locked.
   If MODE is O_WRONLY | O_EXCL | O_CREAT, we do so only if it is free.
   Return 1 if successful, 0 if not.  */

int
lock_file_1 (lfname, mode)
     int mode; char *lfname; 
{
  register int fd;
  char buf[20];

  if ((fd = open (lfname, mode, 0666)) >= 0)
    {
      fchmod (fd, 0666);
      sprintf (buf, "%d ", getpid ());
      write (fd, buf, strlen (buf));
      close (fd);
      return 1;
    }
  else
    return 0;
}

/* Lock the lock named LFNAME if possible.
   Return 0 in that case.
   Return 1 if lock is really locked by someone else.
   Return -1 if cannot lock for any other reason.  */

int
lock_if_free (lfname)
     register char *lfname; 
{
  register int clasher;
  extern int errno;

  while (lock_file_1 (lfname, O_WRONLY | O_EXCL | O_CREAT) == 0)
    {
      if (errno != EEXIST)
	return -1;
      clasher = current_lock_owner (lfname);
      if (clasher == 0 || (kill (clasher, 0) < 0 && errno == ESRCH))
	{
	  if (unlink (lfname) < 0)
	    return -1;
	  /* If we delete the lock successfully, try again to lock.  */
	}
      else
	return (clasher != getpid ());
     }
  return 0;
}

int
current_lock_owner (lfname)
     char *lfname;
{
  register int fd;
  char buf[20];

  fd = open (lfname, O_RDONLY, 0666);
  if (fd < 0)
    return 0;

  if (read (fd, buf, sizeof buf) <= 0)
    return 0;
  close (fd);
  return atoi (buf);
}

void
unlock_file (fn)
     register Lisp_Object fn;
{
  register char *lfname;

  lfname = (char *) alloca (XSTRING (fn)->size + strlen (PATH_LOCK) + 1);
  fill_in_lock_file_name (lfname, fn);

  lock_superlock (lfname);

  if (current_lock_owner (lfname) == getpid ())
    unlink (lfname);

  unlink (PATH_SUPERLOCK);
}

lock_superlock (lfname)
     char *lfname;
{
  register int i, fd;
  extern int errno;

  for (i = -20; i < 0 && (fd = open (PATH_SUPERLOCK,
				     O_WRONLY | O_EXCL | O_CREAT, 0666)) < 0;
       i++)
    {
      if (errno != EEXIST)
	return;
      sleep (1);
    }
  if (fd >= 0)
    {
      fchmod (fd, 0666);
      write (fd, lfname, strlen (lfname));
      close (fd);
    }
}

void
unlock_all_files ()
{
  register Lisp_Object tail;
  register struct buffer *b;

  for (tail = Vbuffer_alist; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      b = XBUFFER (XCONS (XCONS (tail)->car)->cdr);
      if (!NULL (b->filename)
	  && b->save_modified < b->text.modified)
	unlock_file (b->filename);
    }
}

DEFUN ("lock-buffer", Flock_buffer, Slock_buffer,
  0, 1, 0,
  "Locks FILE, if current buffer is modified.\n\
FILE defaults to current buffer's visited file,\n\
or else nothing is done if current buffer isn't visiting a file.")
  (fn)
     Lisp_Object fn;
{
  if (NULL (fn))
    fn = bf_cur->filename;
  else
    CHECK_STRING (fn, 0);
  if (bf_cur->save_modified < bf_modified
      && !NULL (fn))
    lock_file (fn);
  return Qnil;    
}

DEFUN ("unlock-buffer", Funlock_buffer, Sunlock_buffer,
  0, 0, 0,
 "Unlocks the file visited in the current buffer,\n\
if it should normally be locked.")
  ()
{
  if (bf_cur->save_modified < bf_modified
      && !NULL (bf_cur->filename))
    unlock_file (bf_cur->filename);
  return Qnil;
}

syms_of_filelock ()
{
  defsubr (&Sunlock_buffer);
  defsubr (&Slock_buffer);
}

#endif /* CLASH_DETECTION */
