/* This may look like C code, but it is really -*- C++ -*- */

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

/* This really should go in the class scope, but alas g++ doesn't like that... */
typedef int ITEM_TYPE;

class Compact_Matrix
{
public:
              Compact_Matrix (ITEM_TYPE *mat, int rows, int cols);
              Compact_Matrix (int default_rows = 0);
  ITEM_TYPE   operator () (int i, int j);
  void        operator () (int i, int j, ITEM_TYPE value);
  void        output (void);

private:

  void init(int rows);

  struct Column_Node
    {
      int            index; /* Actual column index in the current row. */
      ITEM_TYPE      value; /* Value at this index location. */
      Column_Node   *next;  /* Pointer to next column in the row. */

      Column_Node (Column_Node *p, int i, ITEM_TYPE v)
	  : index (i), value (v), next (p) { }
    };

  struct Row_Node
    {
      Column_Node   *col_list; // List of column index values for this row.
      int            count;    // Count of total number of columns in the list.
      Row_Node() { col_list = 0; count = 0; }
    };

  const int     MAX_INT            = ~unsigned (0) >> 1;
  const int     MAX_SIGNED_CHAR    = 127;
  const int     MAX_ASCII_RANGE    = 128;
  const int     MAX_SIGNED_SHORT   = 32767;
  const int     MAX_UNSIGNED_CHAR  = 255;
  const int     MAX_UNSIGNED_SHORT = 65535;

  int           max_col_count;   /* Total number of columns in largest row. */
  int           total_cols;      /* Total number of columns in the matrix (if applicable). */
  int           total_entries;   /* Total number of non-null entries in the matrix. */
  int           compressed_len;  /* Size of the compacted matrix buffer. */
  int          *row_offsets;     /* Dynamic buffer used for double-offset indexing. */
  int          *checks;          /* Dynamic buffer used for double-offset indexing. */
  int           current_rows;    /* Current items in ROW_VEC, at this point. */
  int           max_rows;        /* Maximum size of ROW_VEC, at this point. */
  Row_Node     *row_vec;         /* Dynamic buffer indexed by row number. */
  Column_Node **bucket_vec;      /* Dynamic buffer used to sort by column count. */
  ITEM_TYPE    *values;          /* Dynamic buffer containing non-null matrix values. */
  ITEM_TYPE    *matrix;          /* Pointer to 2-D matrix (if appropriate). */
  
  void          resize (int new_size);
  void          first_fit_decreasing (void);
  void          bucket_sort (void);
  void          output_lookup (void);
  void          output_arrays (void);
  void          dump_entries (void);
  void          dump_bucket (void);
};
