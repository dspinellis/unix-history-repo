#include <stdio.h>
#include <std.h>
#include "Dirent.h"
#include "option.h"
#include "entry.h"
#include "directory.h"

/* Provided in the main driver program. */
extern Option_Handler option;

/* Names used to print out various file classes. */
char *Directory_Handler::class_name[Directory_Handler::MAX_TYPES] =
{
  "Directory", "Regular", "Executable", "Directory Link", "File Link", "Symbolic Link", "Unknown"
};

/* Read each file in the current directory, and enter it into
   the correct file class according to its file type. 
   Then, for each file class, sort the class entries by 
   name and print them to the standard output. */

Directory_Handler::Directory_Handler (void)
{
  Dirent directory (".");
  file_types     file_type;
  
  /* "." always works, since a chdir is performed earlier if a 
     user-specified directory was supplied on the command-line. */
  
  if (!option[HIDDEN])
    {
      /* Skip the first two directory entries ("." and "..").  This
         is fast, but potentially non-portable.  Does this fail
         on anyone's system?  If so, I'll change it (barf). */
      directory.readdir ();
      directory.readdir ();
    }
  
  /* Main loop in program.  Read each directory entry, classify it
    according to its type, then enter it into the appropriate table slot. */
  
  for (struct dirent *dir_buf; dir_buf = directory.readdir (); )
    {
      if (!option[HIDDEN] && *dir_buf->d_name == '.')
        continue;
      
      struct stat stat_buf;
      
#if !defined(S_ISLNK) && defined(S_IFLNK)
#define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)
#endif
#ifndef S_ISLNK
#define lstat stat
#endif
      lstat (dir_buf->d_name, &stat_buf);
      if (S_ISREG(stat_buf.st_mode))
	{
#ifdef S_IEXEC
	  if (stat_buf.st_mode & S_IEXEC)
	      file_type = EXECS;
	  else
#endif
	      file_type = FILES;
        }
#ifdef S_ISLNK
      else if (S_ISLNK(stat_buf.st_mode)) {
          if (option[LINK]) // Either a file or directory link
            {
              stat (dir_buf->d_name, &stat_buf);
              file_type = S_ISDIR(stat_buf.st_mode) ? DLINKS : FLINKS;
            }
          else
            file_type = LINKS;
      }
#endif
      else if (S_ISDIR(stat_buf.st_mode))
	  file_type = DIRS;
      else
          file_type = UNKNOWN_FILE;
      
      file_class[file_type].add_entry (dir_buf->d_name, 
				       strlen (dir_buf->d_name));
    }
}

/* Sort file class entries and print them to stdout. */

void 
Directory_Handler::print (void)
{
  for (file_types i = (file_types) 0; i < MAX_TYPES; i++)
    if (file_class[i].entry_number () > 0)
      {
        file_class[i].sort_entries ();
        file_class[i].print_entries (class_name[i]);
      }
}  
