/* Copyright (C) 1990 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* bseq.h */
/* Definitions for Level 2 binary object sequences */

/* Binary object sequence element types */
typedef enum {
  bs_null = 0,
  bs_integer = 1,
  bs_real = 2,
  bs_name = 3,
  bs_boolean = 4,
  bs_string = 5,
  bs_eval_name = 6,
  bs_array = 9,
  bs_mark = 10
} bin_seq_type;
#define bs_executable 128

/* Definition of an object in a binary object sequence */
typedef struct {
  byte tx;			/* type and executable flag */
  byte unused;
  union {
    ushort w;
    byte b[2];
  } size;
  union {
    long w;
    float f;
    byte b[4];
  } value;
} bin_seq_obj;
