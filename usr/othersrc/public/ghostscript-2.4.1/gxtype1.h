/* Copyright (C) 1990, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gxtype1.h */
/* Private Adobe Type 1 font definitions for GhostScript library */
#include "gstype1.h"

/* Define the charstring command set */
typedef enum {
		c_undef0 = 0,
	c_hstem = 1,
		c_undef2 = 2,
	c_vstem = 3,
	c_vmoveto = 4,
	c_rlineto = 5,
	c_hlineto = 6,
	c_vlineto = 7,
	c_rrcurveto = 8,
	c_closepath = 9,
	c_callsubr = 10,
	c_return = 11,
	c_escape = 12,			/* extends the command set */
	c_hsbw = 13,
	c_endchar = 14,
	c_undoc15 = 15,			/* Undocumented by Adobe, */
					/* but used in some Adobe fonts. */
		c_undef16 = 16,
		c_undef17 = 17,
		c_undef18 = 18,
		c_undef19 = 19,
		c_undef20 = 20,
	c_rmoveto = 21,
	c_hmoveto = 22,
		c_undef23 = 23,
		c_undef24 = 24,
		c_undef25 = 25,
		c_undef26 = 26,
		c_undef27 = 27,
		c_undef28 = 28,
		c_undef29 = 29,
	c_vhcurveto = 30,
	c_hvcurveto = 31
} char_command;
typedef enum {				/* extended commands */
	ce_dotsection = 0,
	ce_vstem3 = 1,
	ce_hstem3 = 2,
	ce_seac = 6,
	ce_sbw = 7,
	ce_div = 12,
	ce_undoc15 = 15,		/* Undocumented by Adobe, */
					/* but used in some Adobe fonts. */
	ce_callothersubr = 16,
	ce_pop = 17,
	ce_setcurrentpoint = 33
} char_extended_command;

/* Define the encoding of numbers */
#define c_num1 32
#define c_value_num1(ch) ((int)(byte)(ch) - 139)
#define c_num2 247
#define c_value_num2(c1,c2)\
  (((int)(byte)((c1) - c_num2) << 8) + (int)(byte)(c2) + 108)
#define c_num3 251
#define c_value_num3(c1,c2)\
  -(((int)(byte)((c1) - c_num3) << 8) + (int)(byte)(c2) + 108)
/* Otherwise, the first byte is 255, followed by a 32-bit 2's complement */
/* number in big-endian order. */
#define c_num4 255

/* Define the encryption parameters */
#define crypt_c1 ((ushort)52845)
#define crypt_c2 ((ushort)22719)
#define encrypt_next(ch, state, chvar)\
  chvar = ((ch) ^ (state >> 8)),\
  state = (chvar + state) * crypt_c1 + crypt_c2
#define decrypt_this(ch, state)\
  ((ch) ^ (state >> 8))
#define decrypt_next(ch, state, chvar)\
  chvar = decrypt_this(ch, state),\
  decrypt_skip_next(ch, state)
#define decrypt_skip_next(ch, state)\
  state = ((ch) + state) * crypt_c1 + crypt_c2
