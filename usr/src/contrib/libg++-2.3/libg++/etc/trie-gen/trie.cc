/* Generates a minimal-prefix trie for a user-specified set of keywords.

   Copyright (C) 1989 Free Software Foundation, Inc.
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)
   
   This file is part of GNU TRIE-GEN.
   
   GNU TRIE-GEN is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.
   
   GNU TRIE-GEN is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GNU trie-gen; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <std.h>
#include <new.h>
#include "options.h"
#include "trie.h"

/* Insert a new keyword into the data structure, possibly growing the
   keyword table to accomodate the new entry.  Space for STR has already
   been allocated in the caller.  In addition, the MAX_KEY_LEN is updated
   if the current LEN is larger than the previous max.  This information
   is needed later on when dynamically allocating the TRIE table. */
   
void 
Trie::insert (char *str, int len)
{
  if (current_size >= total_size)
    resize (total_size * 2);
  keys[current_size++] = str;
  max_key_len >?= len;
}

/* Grow the KEYS table to allow a maximum of NEW_SIZE entries. 
   Old keys are copied into the new buffer. */

void 
Trie::resize (int new_size)
{
#ifdef _G_OLD_PLACEMENT
  keys = new {keys, total_size = new_size} char *;
#else
  keys = new (keys, total_size = new_size) char *;
#endif
}

/* Write the generated TRIE out as a static table.  Compaction is
   performed if the user requests it, otherwise the table is 
   not compacted at all! (this leads to ridiculously large tables; perhaps
   compaction should be the default?) */

void 
Trie::output (void)
{
  if (current_size <= 0)
    return;
  else
    {
      sort ();

      fputs ("#include <string.h>\n#define MAX_ASCII 128\n\nstatic", stdout);
      if (option[CONST])
        fputs (" const", stdout);
      fputs (" char *const word_list[] = \n{\n  \"\",\n", stdout);

      for (int i = 0; i < current_size; i++) 
        printf ("  \"%s\",\n", keys[i]);

      fflush (stdout);
      fputs ("};\n\n", stdout);
      build (current_size);

      if (option[COMPACT])
        matrix.output ();
      else
        {
          const int MAX_NUMBER = current_size >? max_row;
          int   field_width;
          int   count = MAX_NUMBER;

          for (field_width = 2; (count /= 10) > 0; field_width++)
            ;

          printf ("static const %s trie_array[][MAX_ASCII] = \n{", 
                  MAX_NUMBER < MAX_SIGNED_CHAR ? "char" :
                  MAX_NUMBER < MAX_SIGNED_SHORT ? "short" : "int");

          for (i = 0; i < max_row; i++)
            {
              fputs ("\n  ", stdout);

              for (int j = 0; j < MAX_ASCII; j++)
                printf ("%*d,", field_width, matrix (i, j));
            }
          fputs ("\n};\n", stdout);
        }

      printf ("\n%schar *\nin_word_set (const char *str, int len)\n{\n"
              "  %schar *s = %sstr;\n  int i = 0;\n\n"
              "  while ((i = %s) > 0)\n    ;\n\n"
              "  return i == 0 || strncmp (s = word_list[-i], str, len)"
              " ? 0 : s;\n}\n",
              option[CONST] ? "const " : "",
              option[CONST] ? "const " : "",
              option[CONST] ? "" : "(char*)",
              option[COMPACT] ? "next_state(i, *s++)" : "trie_array[i][*s++]");
    }
}

/* Comparison routine called by qsort. */
int 
Trie::cmp (char **s1, char **s2)
{
  return strcmp (*s1, *s2);
}

/* Sort the keys by lexicographical order. */

void 
Trie::sort (void)
{
  typedef int (*PTF)(void *, void *);
  qsort ((void *) keys, current_size, sizeof *keys, PTF (cmp));
}

/* Generate the trie, using recursive calls if necessary to handle 
   duplicate keyword index positions. */

void 
Trie::build (int hi, int lo, int col)
{
  if (option[FULL])
    {
      int row = max_row - 1;

      /* Process each key in the range lo..hi, possibly calling the function
         recursively when duplicate consecutive characters are found (that's
         why it is important to sort the keywords first!).  Note that calls
         to member function MATRIX build the internal form used to generate 
         the compacted sparse array.  Positive values indicate the next row
         (which really encodes DFA states) to consider; negative values
         are flags that provide (when negated) the correct offset into a generated 
         array of strings. */
      
      for (int i = lo; i < hi; i++)
        if (keys[i][col] == '\0')
          matrix (row, keys[i][col], -i - 1);
        else
          {
            /* Location the end of the duplicate characters in the current column. */
            
            for (lo = i; i < hi - 1 && keys[i][col] == keys[i + 1][col]; i++)
              ;
            
            matrix (row, keys[lo][col], max_row++);
            build (i + 1, lo, col + 1);
          }
    } 
  else
    {
      int row = max_row - 1;

      /* Process each key in the range lo..hi, possibly calling the function
         recursively when duplicate consecutive characters are found (that's
         why it is important to sort the keywords first!).  Note that calls
         to member function MATRIX build the internal form used to generate 
         the compacted sparse array.  Positive values indicate the next row
         (which really encodes DFA states) to consider; negative values
         are flags that provide (when negated) the correct offset into a generated 
         array of strings. */
      
      for (int i = lo; i < hi; i++)
        if (keys[i][col] == '\0' || i == hi - 1 || keys[i][col] != keys[i + 1][col])
          matrix (row, keys[i][col], -i - 1);
        else
          {
            /* Location the end of the duplicate characters in the current column. */
            
            for (lo = i; i < hi - 1 && keys[i][col] == keys[i + 1][col]; i++)
              ;
            
            matrix (row, keys[lo][col], max_row++);
            build (i + 1, lo, col + 1);
          }
    } 
}  
