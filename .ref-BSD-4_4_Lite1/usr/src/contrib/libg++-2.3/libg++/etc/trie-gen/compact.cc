/* Compact a sparse 2-D matrix.  Uses the Tarjan and Yao algorithm
   taken from the article ``Storing a Sparse Table'' in CACM, 1979.

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
#include <builtin.h>
#include <new.h>
#include <std.h>
#include <assert.h>
#include "compact.h"

/* Essentially provides the functionality of `calloc.' */

static inline void *
operator new (size_t elem_size, int size)
{
  void *temp = new char[elem_size * size];
  memset (temp, 0, elem_size * size);
  return temp;
}

/* Essentially combines the functionality of `realloc' and `calloc'. */

static inline void *
operator new (size_t elem_size, void *old_ptr, int old_size, int new_size)
{ 
#ifdef _G_OLD_PLACEMENT
  old_ptr = new {old_ptr, new_size * elem_size} char;
#else
  old_ptr = new (old_ptr, new_size * elem_size) char;
#endif
  memset (old_ptr + old_size * elem_size, 0, (new_size - old_size) * elem_size);
  return old_ptr;
}

/* Initializes the internal form in the case that the user passes
   in a pointer to an already existing 2-dimensional matrix.  Note
   that by declaring the matrix to be a 1-dimensional array we
   can perform the col and row offset calculations ourselves and
   handle matrices with fixed, but arbitrary-sized columns and rows. */

Compact_Matrix::Compact_Matrix (ITEM_TYPE *mat, int rows, int cols):
     matrix (mat), max_rows (rows), total_cols (cols), current_rows (rows)
{
  init (rows);

  for (int i = 0; i < max_rows; i++)
    for (int j = 0; j < total_cols; j++)
      if (matrix[i * total_cols + j] != 0)
        {
          ITEM_TYPE value = matrix[i * total_cols + j];
          total_entries++;
          row_vec[i].count++;
          row_vec[i].col_list = new Column_Node (row_vec[i].col_list, j, value);
        }
}

/* Initializer for the case where we don't have a previously created
   matrix to play with.  DEFAULT_ROWS represents the best first
   approximation as to the number of rows in the matrix.  However, this
   buffer is resized as needed, so the algorithm isn't overly penalized
   for a bad first guess. */

Compact_Matrix::Compact_Matrix (int default_rows): max_rows (default_rows)
{
  current_rows = 0;
  total_cols = 0;
  matrix = 0;

  init (default_rows);
}

void Compact_Matrix::init(int rows)
{
  max_col_count = 0;
  total_entries = 0;
  compressed_len = -1;
  row_offsets = 0;
  checks = 0;
  bucket_vec = 0;
  values = 0;

  row_vec = rows ? new Row_Node[rows] : 0;
}

/* Returns the `matrix[i][j]' item in the sparse 2-dimensional matrix.
   Note that if the matrix is very sparse the number of items in each
   COL_LIST will be very short, hence linear search is not too inefficent. */

ITEM_TYPE 
Compact_Matrix::operator () (int i, int j)
{
  assert (i >= 0 && i < current_rows && j >= 0);
  
  for (Column_Node *col_list = row_vec[i].col_list; col_list; col_list = col_list->next)
    if (col_list->index == j)
      return col_list->value;

  return 0;
}

/* Sets `matrix[i][j]' to VALUE.  ROW_VEC is dynamically resized, if
   necessary.  At the moment the new entry is simply pushed onto the
   linked list of COL_LIST entries.  If there aren't many elements then
   this will not be too inefficient for later retrieval. */

void
Compact_Matrix::operator () (int i, int j, ITEM_TYPE value)
{
  if (i >= max_rows)
    resize ((max_rows * 2) >? (i+1));
  row_vec[i].col_list = new Column_Node (row_vec[i].col_list, j, value);
  total_entries++;
  row_vec[i].count++;
  current_rows >?= i + 1;
}

/* Enlarges the ROW_VEC from CURR_ROWS to NEW_SIZE. */

void 
Compact_Matrix::resize (int new_size)
{
  Row_Node *temp = new Row_Node[max_rows = new_size];
  
  memcpy ((void *) temp, (void *) row_vec, current_rows * sizeof *row_vec);
  delete row_vec;
  row_vec = temp;
}

/* Calls the functions that compact the table and generate the resulting
   lookup scheme. */

void
Compact_Matrix::output (void)
{
  bucket_sort ();
  first_fit_decreasing ();
  output_arrays ();
  output_lookup ();
}

/* Performs a bucket sort so that all rows with the same number of non-null
   entries are treated as part of the same equivalence class.  This
   operation is very fast! */

void 
Compact_Matrix::bucket_sort (void)
{
  for (int i = 0; i < current_rows; i++)
    max_col_count >?= row_vec[i].count;
  
#ifdef _G_OLD_PLACEMENT
  bucket_vec = new {max_col_count + 1} Column_Node *;
#else
  bucket_vec = new (max_col_count + 1) Column_Node *;
#endif

  for (i = 0; i < current_rows; i++)
    bucket_vec[row_vec[i].count] = new Column_Node (bucket_vec[row_vec[i].count], i, 0);
}

/* Useful macros to clarify subsequent code.  They should probably be made
   into member functions... */
#define STARTING_ROW_OFFSET(X) (row_offsets[(X)])
#define LARGEST_COL_VALUE(X) (row_vec[(X)].col_list->index)
#define COL_LIST(X) (row_vec[(X)].col_list)
#define COL_COUNT(X) (row_vec[(X)].count)
#define COL_INDEX(X) ((X)->index)
#define ROW_INDEX(X) ((X)->index)

/* Performs sparse 2-dimensional array compaction suitable for use with the
   `double-offset indexing' (used by Bison and FLEX to compact the size of
   the sparse LR parsing tables and DFA's).  This function implements the
   `first fit decreasing' heuristic described in Tarjan and Yao's paper 
   ``Storing a Sparse Table'' in CACM, 1979. */

void 
Compact_Matrix::first_fit_decreasing (void)
{
  /* Bit-vector and counter that records if a row/col location is already set. */
  int    current_max = current_rows + (total_cols >? MAX_ASCII_RANGE);
  char *already_assigned = (char *) malloc (current_max);
  
  memset (already_assigned, 0, current_max);
#ifdef _G_OLD_PLACEMENT
  row_offsets = new {current_rows} int;
  values      = new {current_max} ITEM_TYPE;
  checks      = new {current_max} int;
#else
  row_offsets = new (current_rows) int;
  values      = new (current_max) ITEM_TYPE;
  checks      = new (current_max) int;
#endif

  for (int row_index = max_col_count; row_index >= 0; row_index--)
    if (bucket_vec[row_index])

      /* Process every row in the `equivalence class' of rows containing the
         same number of non-null column entries. */

      for (Column_Node *rows_list = bucket_vec[row_index]; rows_list; rows_list = rows_list->next)
        {
          int row        = ROW_INDEX (rows_list);
          int row_offset = STARTING_ROW_OFFSET (row);
          
          /* Process every column index in the current row. */

          for (Column_Node *col_list = COL_LIST (row); ;)
            {
              int col = COL_INDEX (col_list);

              /* If we exceed the boundaries it's time to resize various buffers. */
              if (row_offset + col >= current_max)
                {
                  int   new_size = (current_max >? row_offset + col) * 2;
                  already_assigned =
		      (char *) realloc (already_assigned, new_size);

		  memset (already_assigned + current_max, 0,
			  new_size - current_max);
#ifdef _G_OLD_PLACEMENT
                  values = new {values, current_max, new_size} int;
                  checks = new {checks, current_max, new_size} int;
#else
                  values = new (values, current_max, new_size) int;
                  checks = new (checks, current_max, new_size) int;
#endif
                  current_max *= 2;
                }
              if (already_assigned[row_offset + col])
                {
                  /* Efficiency hack to skip over obvious collisions. */

                  while (++row_offset + col < current_max && already_assigned[row_offset + col]) 
                    ;

                  /* Reset col_list and begin again (with new row offset). */
                  col_list = COL_LIST (row);
                }
              else if ((col_list = col_list->next) == 0)
                break;
            }

          /* No more collisions exist.  Record the positions for the next round. */

          for (col_list = COL_LIST (row); col_list; col_list = col_list->next)
            {
              int offset = row_offset + COL_INDEX (col_list);

              already_assigned[offset] = 1;
              compressed_len >?= offset;
              values[offset] = col_list->value;
              checks[offset] = row;
            }

          /* Need to reset this once all is said and done! */
          STARTING_ROW_OFFSET (row) = row_offset;
        }
  free(already_assigned);
}

/* Generates the three arrays that comprise the `double-offset index'
   scheme.  This is a bit messy, since I'm trying to neatly format
   the generated tables and also determine the smallest (in bytes)
   type declarations necessary to represent the elements of the tables. */

void 
Compact_Matrix::output_arrays ()
{
  const int COL_WIDTH = 12;
        int max_number = 0;
        int count;
      
  printf ("#ifndef __STDC__\n#define const\n#endif\n\n");
  printf ("#define YY_LAST %d\n", compressed_len);
      
  for (int i = 0; i < current_rows; i++)
    max_number >?= row_offsets[i];

  count = max_number;
  for (int field_width = 1; (count /= 10) > 0; field_width++)
    ;

  printf ("\nstatic const unsigned %s yy_rows[%d] = \n{\n  ",
          max_number < MAX_UNSIGNED_CHAR ? "char" : max_number < MAX_UNSIGNED_SHORT ? "short" : "int",
          current_rows);
      
  for (i = 0; i < current_rows; )
    printf ("%*d,%s", field_width, row_offsets[i], ++i % COL_WIDTH ? " " : "\n  ");
      
  max_number = 0;

  for (i = 0; i < compressed_len + 1; i++)
    max_number >?= checks[i];

  count = max_number;
  for (field_width = 1; (count /= 10) > 0; field_width++)
    ;

  printf ("\n};\n\nstatic const unsigned %s yy_check[%d] = \n{\n  ",
          max_number < MAX_UNSIGNED_CHAR ? "char" : max_number < MAX_UNSIGNED_SHORT ? "short" : "int",
          compressed_len + 1);
      
  for (i = 0; i < compressed_len + 1; )
    printf ("%*d,%s", field_width, checks[i], ++i % COL_WIDTH ? " " : "\n  ");
      
  max_number = 0;

  for (i = 0; i < compressed_len + 1; i++)
    max_number >?= abs (values[i]);

  count = max_number;
  for (field_width = 2; (count /= 10) > 0; field_width++)
    ;

  printf ("\n};\n\nstatic const %s yy_next[%d] = \n{\n  ",
          max_number < MAX_SIGNED_CHAR ? "char" : max_number < MAX_SIGNED_SHORT ? "short" : "int",
          compressed_len + 1);
      
  for (i = 0; i <= compressed_len; )
    printf ("%*d,%s", field_width, values[i], ++i % COL_WIDTH ? " " : "\n  ");
      
  printf ("\n};\n\n");
}

/* Generates the `double-offset index' function, that provides the
   value stored in the location referenced by parameters ROW and COL. */

void 
Compact_Matrix::output_lookup (void)
{
  printf ("static inline int\nnext_state "
          "(int row, int col)\n{\n  int state_index = yy_rows[row] + col;\n\n"
          "  if (state_index > YY_LAST || yy_check[state_index] != row)\n    "
          "return 0;\n  else\n    return yy_next[state_index];\n}\n");
}

/* Useful debugging routine. */

void
Compact_Matrix::dump_entries (void)
{
  for (int i = 0; i < current_rows; i++)
    if (row_vec[i].col_list)
      {
        Column_Node *temp = row_vec[i].col_list;
      
        fprintf (stderr, "row %d's count = %d, cols = ", i, row_vec[i].count);

        do { fprintf (stderr, "%d ", temp->index); } while (temp = temp->next);

        putc ('\n', stderr);
      }
}

/* Useful debugging routine. */

void 
Compact_Matrix::dump_bucket (void)
{
  for (int i = 0; i < total_entries; i++)
    if (bucket_vec[i])
      {
        Column_Node *temp = bucket_vec[i];
        
        fprintf (stderr, "bucket %d = ", i);

        do { fprintf (stderr, "%d ", temp->index); } while (temp = temp->next);

        putc ('\n', stderr);
      }
}

