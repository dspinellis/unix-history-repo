// This may look like C code, but it is really -*- C++ -*-

/* Manipulate directory entries of a particular file class. */
#ifndef entry_h
#define entry_h 1
#include <std.h>
#include <new.h>
#include "screen.h"

/* Defined in sort.cc. */
void sort (char **base_ptr, int total_elems);

class Entry_Handler
{
  
private:
#if ( __GNUG__ > 1) /* Initial number of file entries per class. */
  static const int    default_entries;  
#else
  static const int    default_entries = 50;
#endif
               int    max_entry_length; /* Size of largest filename. */
               int    total_entries;    /* Total number of filenames. */
               int    entries;          /* Current number of filenames. */
               char **buf;              /* Buffer containing filenames for this file class. */
  
public:
                      Entry_Handler (void);                     /* Initialize a new file class. */
  int                 entry_number (void);                      /* Current number of entries. */
  void                add_entry (char *entry_name, int length); /* Add an entry to the class. */
  void                sort_entries (void);                      /* Sort entries by filename. */
  void                print_entries (char *class_name);         /* Print file entries. */
};

/* See comments in the .cc file for the following inline functions */

#ifdef __OPTIMIZE__
inline 
Entry_Handler::Entry_Handler (void)
{
  entries = max_entry_length = 0;
  total_entries = default_entries;
  buf = new char *[default_entries];
}

inline int 
Entry_Handler::entry_number (void)
{
  return entries;
}

inline void 
Entry_Handler::add_entry (char *entry_name, int length)
{
  if (entries >= total_entries)
#ifdef _G_OLD_PLACEMENT
    buf = new {buf, total_entries *= 2} char *;
#else
    buf = new (buf, total_entries *= 2) char *;
#endif
  max_entry_length >?= length;
  buf[entries++] = strcpy (new char[length + 1], entry_name);
}

inline void 
Entry_Handler::sort_entries (void)
{
  sort (buf, entries);
}

#endif // __OPTIMIZE__
#endif
