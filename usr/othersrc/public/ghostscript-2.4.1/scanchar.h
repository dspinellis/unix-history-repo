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

/* scanchar.h */
/* Character scanning table for Ghostscript */

/* An array for fast scanning of names, numbers, and hex strings. */
/*  Indexed by character code (including EOFC and ERRC), it contains: */
/*	0 - max_radix-1 for valid digits, */
/*	ctype_name for other characters valid in names, */
/*	ctype_btoken for characters introducing binary tokens */
/*	  (if the binary token feature is enabled), */
/*	ctype_space for whitespace characters, */
/*	ctype_eof for end-of-file, and */
/*	ctype_other for everything else. */
/* This table is initialized in iscan.c, used in iscan.c and stream.c. */
extern byte scan_char_array[258];
#define scan_char_decoder (&scan_char_array[2])	/* account for EOFC/ERRC */
#define min_radix 2
#define max_radix 36
#define ctype_name 100
#define ctype_btoken 101
#define ctype_space 102
#define ctype_other 103
#define ctype_eof 104
/* Special characters with no \xxx representation */
#define char_NULL 0
#define char_VT 013			/* ^K, vertical tab */
#define char_DOS_EOF 032		/* ^Z */
