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

/* btoken.h */
/* Definitions for Level 2 binary tokens */

/* Binary token types */
typedef enum {
  bt_seq = 128,
    bt_seq_IEEE_msb = 128,	/* binary object sequence, */
				/* IEEE floats, big-endian */
    bt_seq_IEEE_lsb = 129,	/* ditto, little-endian */
    bt_seq_native_msb = 130,	/* ditto, native floats, big-endian */
    bt_seq_native_lsb = 131,	/* ditto, little-endian */
  bt_int32_msb = 132,
  bt_int32_lsb = 133,
  bt_int16_msb = 134,
  bt_int16_lsb = 135,
  bt_int8 = 136,
  bt_fixed = 137,
  bt_float_IEEE_msb = 138,
  bt_float_IEEE_lsb = 139,
  bt_float_native = 140,
  bt_boolean = 141,
  bt_string_256 = 142,
  bt_string_64k_msb = 143,
  bt_string_64k_lsb = 144,
  bt_litname_system = 145,
  bt_execname_system = 146,
  bt_litname_user = 147,
  bt_execname_user = 148,
  bt_num_array = 149
} bt_type;
#define bt_type_min 128
#define bt_type_max 159
