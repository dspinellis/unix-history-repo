/* Hash.c -- Where hashing for bash is done. */

/* Copyright (C) 1987, 1989, 1991 Free Software Foundation, Inc.

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

/* There appears to be library functions for this stuff, but it seems like
   a lot of overhead, so I just implemented this hashing stuff on my own. */

#include "shell.h"
#include "hash.h"

HASH_TABLE *hashed_filenames;

#define FILENAME_HASH_BUCKETS 107

/* Create the hash table for filenames that we use in the shell. */
initialize_hashed_filenames ()
{
  hashed_filenames = make_hash_table (FILENAME_HASH_BUCKETS);
}

/* Make a new hash table with BUCKETS number of buckets.  Initialize
   each slot in the table to NULL. */
HASH_TABLE *
make_hash_table (buckets)
     int buckets;
{
  HASH_TABLE *new_table = (HASH_TABLE *)xmalloc (sizeof (HASH_TABLE));

  if (!buckets)
    buckets = DEFAULT_HASH_BUCKETS;

  new_table->bucket_array =
    (BUCKET_CONTENTS **)xmalloc (buckets * sizeof (BUCKET_CONTENTS *));
  new_table->nbuckets = buckets;
  new_table->nentries = 0;
  initialize_hash_table (new_table);
  return (new_table);
}

/* Zero the buckets in TABLE. */
initialize_hash_table (table)
     HASH_TABLE *table;
{
  register int i;
  for (i = 0; i < table->nbuckets; i++)
    table->bucket_array[i] = (BUCKET_CONTENTS *)NULL;
}

/* Return the location of the bucket which should contain the data
   for STRING.  TABLE is a pointer to a HASH_TABLE. */
hash_string (string, table)
     char *string;
     HASH_TABLE *table;
{
  register unsigned int i = 0;

  while (*string) i += *string++;
  i %= table->nbuckets;
  return (i);
}

/* Return a pointer to the hashed item, or NULL if the item
   can't be found. */
BUCKET_CONTENTS *
find_hash_item (string, table)
     char *string;
     HASH_TABLE *table;
{
  BUCKET_CONTENTS *list;

  list = table->bucket_array[hash_string (string, table)];

  while (list)
    {
      if (STREQ (list->key, string))
	{
	  list->times_found++;
	  return (list);
	}
      else list = list->next;
    }
  return (BUCKET_CONTENTS *)NULL;
}

/* Remove the item specified by STRING from the hash table TABLE.
   The item removed is returned, so you can free its contents.  If
   the item isn't in this table NULL is returned. */
BUCKET_CONTENTS *
remove_hash_item (string, table)
     char *string;
     HASH_TABLE *table;
{
  int the_bucket = hash_string (string, table);
  BUCKET_CONTENTS *prev = (BUCKET_CONTENTS *)NULL;
  BUCKET_CONTENTS *temp = table->bucket_array[the_bucket];

  while (temp)
    {
      if (strcmp (temp->key, string) == 0)
	{
	  if (prev)
	    prev->next = temp->next;
	  else
	    table->bucket_array[the_bucket] = temp->next;

	  table->nentries--;
	  return (temp);
	}
      prev = temp;
      temp = temp->next;
    }
  return ((BUCKET_CONTENTS *) NULL);
}

/* Create an entry for STRING, in TABLE.  If the entry already
   exists, then return it. */
BUCKET_CONTENTS *
add_hash_item (string, table)
     char *string;
     HASH_TABLE *table;
{
  BUCKET_CONTENTS *item;

  if (!(item = find_hash_item (string, table)))
    {
      int bucket = hash_string (string, table);
      item = table->bucket_array[bucket];

      while (item && item->next) item = item->next;
      if (item)
	{
	  item->next = (BUCKET_CONTENTS *)xmalloc (sizeof (BUCKET_CONTENTS));
	  item = item->next;
	}
      else
	{
	  table->bucket_array[bucket] =
	    (BUCKET_CONTENTS *)xmalloc (sizeof (BUCKET_CONTENTS));
	  item = table->bucket_array[bucket];
	}

      item->data = (char *)NULL;
      item->next = (BUCKET_CONTENTS *)NULL;
      item->key = string;
      table->nentries++;
      item->times_found = 0;
    }

  return (item);
}

/* Return the bucket_contents list of bucket BUCKET in TABLE.  If
   TABLE doesn't have BUCKET buckets, return NULL. */
BUCKET_CONTENTS *
get_hash_bucket (bucket, table)
     int bucket;
     HASH_TABLE *table;
{
  if (bucket < table->nbuckets)
    return (table->bucket_array[bucket]);
  else
    return (BUCKET_CONTENTS *)NULL;
}

#ifdef TEST_HASHING

#undef NULL
#include <stdio.h>

HASH_TABLE *table;
#define NBUCKETS 107

xmalloc (bytes)
     int bytes;
{
  char *result = (char *)malloc (bytes);
  if (!result)
    {
      fprintf (stderr, "Out of memory!");
      abort ();
    }
  return ((int)result);
}

main ()
{
  char string[256];
  int count = 0;
  BUCKET_CONTENTS *tt;

  table = make_hash_table (NBUCKETS);
  
  printf ("Enter some data to be hashed, a word at a time.\n\
Type a blank line when done:\n\n");

  for (;;)
    {
      char *temp_string = savestring (gets (string));
      if (!*string) break;
      tt = add_hash_item (temp_string, table);
      if (tt->times_found)
	{
	  printf ("\nYou have already added that item\n");
	  free (temp_string);
	}
      else
	{
	  count++;
	}
    }
  
  printf ("\nYou have entered %d (%d) items.  The items are:\n\n",
	  table->nentries, count);

  for (count = 0; count < table->nbuckets; count++)
    {
      register BUCKET_CONTENTS *list = get_hash_bucket (count, table);
    
      if (list)
	{
	  printf ("%3d slot: ", count);
	  while (list)
	    {
	      printf ("%s\n	     ", list->key);
	      list = list->next;
	    }
	  printf ("\n");
	}
    }
}

#endif /* TEST_HASHING */

/*
 * Local variables:
 * compile-command: "gcc -g -DTEST_HASHING -o hash hash.c"
 * end:
 */
