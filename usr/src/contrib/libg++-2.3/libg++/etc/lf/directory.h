// This may look like C code, but it is really -*- C++ -*-

/* Manipulate all directory entries for all file classes. */
#ifndef directory_h
#define directory_h 1
#include "entry.h"

class Directory_Handler
{
public:
  /* There are five major types of files in the UNIX system. */
  enum file_types
    {
      DIRS,                     /* Subdirectories. */
      FILES,                    /* Regular files. */
      EXECS,                    /* Executable files. */
      DLINKS,                   /* Directory links (if -l option is enabled). */
      FLINKS,                   /* File links (if -l option is enabled). */
      LINKS,                    /* File *and* directory links (if -l option is *not* enabled). */
      UNKNOWN_FILE,		/* E.g. Fifo */
      MAX_TYPES,
    };

       Directory_Handler (void); /* Formats the current directory files. */
  void print (void);             /* Lists the current directory files. */

 private:

/* static */ Entry_Handler file_class[MAX_TYPES]; /* File class array. */
  static char          *class_name[MAX_TYPES]; /* String naem for each file class. */
  
};
#endif
