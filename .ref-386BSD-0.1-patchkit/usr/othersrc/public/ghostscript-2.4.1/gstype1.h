/* Copyright (C) 1990, 1991 Aladdin Enterprises.  All rights reserved.
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

/* gstype1.h */
/* Client interface to Adobe Type 1 font routines for Ghostscript library */

/* Encrypt/decrypt procedures */
typedef ushort crypt_state;
#define crypt_charstring_seed 4330
int gs_type1_encrypt(P4(byte *dest, byte *src, uint len, crypt_state *pstate));
int gs_type1_decrypt(P4(byte *dest, byte *src, uint len, crypt_state *pstate));

/* CharString interpreter */
typedef struct gs_type1_state_s gs_type1_state;
extern const uint gs_type1_state_sizeof;
#ifndef gs_show_enum_s_DEFINED
struct gs_show_enum_s;
#endif
#ifndef gs_type1_data_s_DEFINED
struct gs_type1_data_s;
#endif
int gs_type1_init(P5(gs_type1_state *pis, struct gs_show_enum_s *penum,
		  int charpath_flag, int paint_type,
		  struct gs_type1_data_s *pdata));
/* Continue interpreting a Type 1 CharString. */
/* If str != 0, it is taken as the byte string to interpret. */
/* Return 0 on successful completion, <0 on error, */
/* or >0 when client intervention is required. */
/* The int * argument is where the character is stored for seac, */
/* or the othersubr # for callothersubr. */
#define type1_result_seac 1
#define type1_result_callothersubr 2
int gs_type1_interpret(P3(gs_type1_state *, byte *, int *));
/* Pop a (fixed) number off the internal stack */
int gs_type1_pop(P2(gs_type1_state *, fixed *));
