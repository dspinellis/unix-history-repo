/* BFD back-end for MIPS Extended-Coff files.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Written by Per Bothner.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "coff/mips.h"
#include "coff/internal.h"
#include "libcoff.h"		/* to allow easier abstraction-breaking */
#include "trad-core.h"

#define BADMAG(x) ECOFFBADMAG(x)

/* Can't read these relocs */
#define RTYPE2HOWTO(a,b) ;
/* Define NO_COFF_SYMBOLS and NO_COFF_LINENOS to avoid coffcode.h
   defining a mess of useless functions.  */
#define	NO_COFF_SYMBOLS
#define	NO_COFF_LINENOS
/* Define MIPS to get MIPS magic numbers and such */
#define MIPS 1
/* Define additional MIPS section types */
#define STYP_OTHER_LOAD	0x98000300
#include "coffcode.h"

/* We do not implement symbols for ecoff. */
#define	coff_get_symtab_upper_bound (PROTO(unsigned int, (*),(bfd *)))bfd_false
#define	coff_get_symtab	 (PROTO(unsigned int, (*), (bfd *, asymbol **)))bfd_0
#define coff_print_symbol \
    (PROTO(void,(*),(bfd *, PTR, asymbol *, enum bfd_print_symbol))) bfd_void
#define	coff_swap_sym_in (PROTO(void,(*),(bfd *,PTR,PTR))) bfd_void
#define	coff_swap_aux_in (PROTO(void,(*),(bfd *,PTR,int,int,PTR))) bfd_void
#define	coff_swap_sym_out (PROTO(unsigned,(*),(bfd *,PTR,PTR))) bfd_void
#define	coff_swap_aux_out (PROTO(unsigned,(*),(bfd *,PTR,int,int,PTR))) bfd_void

/* We do not implement linenos for ecoff. */
#define coff_get_lineno (struct lineno_cache_entry *(*)()) bfd_nullvoidptr
#define	coff_swap_lineno_in (PROTO(void,(*),(bfd *,PTR,PTR))) bfd_void
#define coff_find_nearest_line (PROTO(boolean, (*),(bfd*,asection*,asymbol**,bfd_vma, CONST char**, CONST char**, unsigned int *))) bfd_false
#define coff_swap_lineno_out  (PROTO(unsigned,(*),(bfd *,PTR,PTR))) bfd_void

bfd_target ecoff_little_vec =
{
  "ecoff-littlemips",		/* name */
  bfd_target_coff_flavour,
  false,			/* data byte order is little */
  false,			/* header byte order is little */

  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* sect
							    flags */
  0,				/* leading underscore */
  '/',				/* ar_pad_char */
  15,				/* ar_max_namelen */
  3,				/* minimum alignment power */
  _do_getl64, _do_putl64,	_do_getl32, _do_putl32, _do_getl16, _do_putl16, /* data */
  _do_getl64, _do_putl64,	_do_getl32, _do_putl32, _do_getl16, _do_putl16, /* hdrs */

 {_bfd_dummy_target, coff_object_p, /* bfd_check_format */
   bfd_generic_archive_p, _bfd_dummy_target},
 {bfd_false, coff_mkobject, bfd_false, /* bfd_set_format */
   bfd_false},
 {bfd_false, coff_write_object_contents, bfd_false, bfd_false},
  JUMP_TABLE (coff)
 };

bfd_target ecoff_big_vec =
{
  "ecoff-bigmips",		/* name */
  bfd_target_coff_flavour,
  true,				/* data byte order is big */
  true,				/* header byte order is big */

  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* sect flags */
  0,				/* leading underscore */
  ' ',				/* ar_pad_char */
  16,				/* ar_max_namelen */
  3,				/* minimum alignment power */
  _do_getb64, _do_putb64,	_do_getb32, _do_putb32, _do_getb16, _do_putb16,
  _do_getb64, _do_putb64,	_do_getb32, _do_putb32, _do_getb16, _do_putb16,
 {_bfd_dummy_target, coff_object_p, /* bfd_check_format */
   bfd_generic_archive_p, _bfd_dummy_target},
 {bfd_false, coff_mkobject, bfd_false, /* bfd_set_format */
   bfd_false},
 {bfd_false, coff_write_object_contents, /* bfd_write_contents */
   bfd_false, bfd_false},
  JUMP_TABLE(coff),
  COFF_SWAP_TABLE
 };
