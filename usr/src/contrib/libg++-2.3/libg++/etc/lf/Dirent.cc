/* Define a portable UNIX directory-entry manipulation interface. 

   This code is heavily based upon Doug Gwyn's public domain directory-access
   routines written in C.  Hacked into C++ conformance for the GNU libg++
   library by Douglas C. Schmidt (schmidt@ics.uci.edu). */

#include <std.h>
#include "Dirent.h"

// error handlers

void verbose_Dirent_error_handler(const char* msg)
{
  perror(msg);
  errno = 0;
}

void quiet_Dirent_error_handler(const char*)
{
  errno = 0;
}

void fatal_Dirent_error_handler(const char* msg)
{
  perror(msg);
  exit(1);
}

one_arg_error_handler_t Dirent_error_handler = verbose_Dirent_error_handler;


one_arg_error_handler_t set_Dirent_error_handler(one_arg_error_handler_t f)
{
  one_arg_error_handler_t old = Dirent_error_handler;
  Dirent_error_handler = f;
  return old;
}

#ifndef __OPTIMIZE__

/* Initialize the directory-entry control block from the given DIRNAME.
   Equivalent to opendir (). */

Dirent::Dirent (char *dirname) 
{
  if ((dirp = ::opendir (dirname)) == 0)
    (*Dirent_error_handler) ("Dirent::Dirent");
}

/* Frees up the dynamically allocated buffer. */

Dirent::~Dirent (void)
{
  ::closedir (dirp);
}

/* Re-initialize the directory-entry control block from the given DIRNAME.
   Equivalent to opendir (). */

void
Dirent::opendir (char *dirname) 
{
  if ((dirp = ::opendir (dirname)) == 0)
    (*Dirent_error_handler) ("Dirent::Dirent");
}

/* Read next entry from a directory stream. */

struct dirent *
Dirent::readdir (void)
{
  return ::readdir (dirp);
}

/* Close a directory stream. */

void
Dirent::closedir (void)
{
  ::closedir (dirp);
}

/* Rewind a directory stream. */

void
Dirent::rewinddir (void)
{
  ::seekdir (dirp, long (0));
}

/* Reposition a directory stream. */

void
Dirent::seekdir (long loc)
{
  ::seekdir (dirp, loc);
}

/* Report directory stream position. */

long
Dirent::telldir (void)
{
  return ::telldir (dirp);
}

#endif // __OPTIMIZE__
