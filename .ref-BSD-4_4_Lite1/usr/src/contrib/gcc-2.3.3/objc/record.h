/* Implement a vector type that indicates its used size and allocated size.
   Copyright (C) 1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */


#ifndef __record_INCLUDE_GNU
#define __record_INCLUDE_GNU

#include "assert.h"


/* Structure to hold records.  */
struct record
{  
  unsigned int capacity;
  unsigned int used;
  void **records;
};
  

extern void *__objc_xmalloc (unsigned int);
extern void *__objc_xcalloc (unsigned int, unsigned int);
extern void *__objc_xrealloc (void *, unsigned int);

/* Allocate, initialize and return a new record structure.  */

static inline struct record *
record_new ()
{
  struct record *record;

  record = (struct record *) __objc_xcalloc (1, sizeof (struct record));
  record->capacity = 8;
  record ->records
    = (void **) __objc_xcalloc (record->capacity, sizeof (void *));

  return record;
}


/* Delete the record.  */
static inline void
record_delete (struct record *record)
{
  free (record->records);
  free (record);
}


/* Return the number of entries in the record.  */

static inline unsigned int
record_entries (struct record *record)
{
  return record->used;
}


/* Return the capacity of the record.  */

static inline unsigned int
record_capacity (struct record *record)
{
  return record->capacity;
}


/* Store an entry at the specified record location.  */

static inline void
record_store_at (unsigned int i, void *value, struct record *record)
{
  assert (i);
  assert (i <= record_entries (record));

  record->records[i] = value;
}


/* Make a record entry.  Expand the record's size if full.  */

static inline void
record_store (void *value, struct record *record)
{
  ++record->used;
  if (record_entries (record) == record_capacity (record))
    {
      record->capacity *= 2;
      record->records
	= (void **) __objc_xrealloc (record->records,
				     record_capacity (record) * sizeof (void *));
    }
  record_store_at (record_entries (record), value, record);
}


/* Get a value from the record.  */

static inline void *
record_get (unsigned int i, struct record *record)
{
  assert (i);
  assert (i <= record_entries (record));
  return record->records[i];
}

#endif /* not __record_INCLUDE_GNU */

