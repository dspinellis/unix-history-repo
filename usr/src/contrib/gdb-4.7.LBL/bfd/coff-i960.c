/* BFD back-end for Intel 960 COFF files.
   Copyright (C) 1990, 1991, 1992 Free Software Foundation, Inc.
   Written by Cygnus Support.

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

#define I960 1
#define BADMAG(x) I960BADMAG(x)

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "obstack.h"
#include "coff/i960.h"
#include "coff/internal.h"
#include "libcoff.h"		/* to allow easier abstraction-breaking */
#define COFF_LONG_FILENAMES

#define CALLS	 0x66003800	/* Template for 'calls' instruction	*/
#define BAL	 0x0b000000	/* Template for 'bal' instruction	*/
#define BAL_MASK 0x00ffffff

static bfd_reloc_status_type 
DEFUN (optcall_callback, (abfd, reloc_entry, symbol_in, data,
			  ignore_input_section, ignore_bfd),
       bfd *abfd AND
       arelent *reloc_entry AND
       asymbol *symbol_in AND
       PTR data AND
       asection *ignore_input_section AND
       bfd *ignore_bfd)
{
  /* This item has already been relocated correctly, but we may be
   * able to patch in yet better code - done by digging out the
   * correct info on this symbol */
  bfd_reloc_status_type result;
  coff_symbol_type *cs = coffsymbol(symbol_in);

  /* So the target symbol has to be of coff type, and the symbol 
     has to have the correct native information within it */
  if ((cs->symbol.the_bfd->xvec->flavour != bfd_target_coff_flavour)
      || (cs->native == (combined_entry_type *)NULL)) {
     /* This is interesting, consider the case where we're outputting */
     /* coff from a mix n match input, linking from coff to a symbol */
     /* defined in a bout file will cause this match to be true. Should */
     /* I complain ? - This will only work if the bout symbol is non */
     /* leaf. */
     result = bfd_reloc_dangerous;
  }
  else  {
    switch (cs->native->u.syment.n_sclass) 
      {
      case C_LEAFSTAT:
      case C_LEAFEXT:
  	/* This is a call to a leaf procedure, replace instruction with a bal
	 to the correct location */
	{
	  union internal_auxent *aux = &((cs->native+2)->u.auxent);
	  int word = bfd_get_32(abfd, (bfd_byte *)data + reloc_entry->address);
	  int olf = (aux->x_bal.x_balntry - cs->native->u.syment.n_value);
	  BFD_ASSERT(cs->native->u.syment.n_numaux==2);
	  /* We replace the original call instruction with a bal to */
	  /* the bal entry point - the offset of which is described in the */
	  /* 2nd auxent of the original symbol. We keep the native sym and */
	  /* auxents untouched, so the delta between the two is the */
	  /* offset of the bal entry point */

	  word = ((word +  olf)  & BAL_MASK) | BAL;
  	  bfd_put_32(abfd, word, (bfd_byte *) data + reloc_entry->address);
  	}
	result = bfd_reloc_ok;
	break;
      case C_SCALL:
	/* This is a call to a system call, replace with a calls to # */
	BFD_ASSERT(0);
	result = bfd_reloc_ok;
	break;
      default:
	result = bfd_reloc_ok;
	break;
      }
  }
  return result;
}

static reloc_howto_type howto_rellong =
  { (unsigned int) R_RELLONG, 0, 2, 32,false, 0, true, true,
      0,"rellong", true, 0xffffffff, 0xffffffff};
static reloc_howto_type howto_iprmed =
  {  R_IPRMED, 0, 2, 24,true,0, true, true,0,"iprmed ", true,
       0x00ffffff, 0x00ffffff};
static reloc_howto_type howto_optcall =
  {  R_OPTCALL, 0,2,24,true,0, true, true, optcall_callback,
       "optcall", true, 0x00ffffff, 0x00ffffff};

static reloc_howto_type *
DEFUN (coff_i960_reloc_type_lookup, (abfd, code),
       bfd *abfd AND
       bfd_reloc_code_real_type code)
{
  switch (code)
    {
    default:
      return 0;
    case BFD_RELOC_I960_CALLJ:
      return &howto_optcall;
    case BFD_RELOC_32:
      return &howto_rellong;
    case BFD_RELOC_24_PCREL:
      return &howto_iprmed;
    }
}

/* The real code is in coffcode.h */

#define RTYPE2HOWTO(cache_ptr, dst) \
{							\
   reloc_howto_type *howto_ptr;				\
   switch ((dst)->r_type) {				\
     case 17: howto_ptr = &howto_rellong; break;	\
     case 25: howto_ptr = &howto_iprmed; break;		\
     case 27: howto_ptr = &howto_optcall; break;	\
     default: howto_ptr = 0; break;			\
     }							\
   cache_ptr->howto = howto_ptr;			\
 }

#include "coffcode.h"

bfd_target icoff_little_vec =
{
  "coff-Intel-little",		/* name */
  bfd_target_coff_flavour,
  false,			/* data byte order is little */
  false,			/* header byte order is little */

  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
  0,				/* leading underscore */
  '/',				/* ar_pad_char */
  15,				/* ar_max_namelen */

  3,				/* minimum alignment power */
  _do_getl64, _do_putl64, _do_getl32, _do_putl32, _do_getl16, _do_putl16, /* data */
  _do_getl64, _do_putl64, _do_getl32, _do_putl32, _do_getl16, _do_putl16, /* hdrs */

 {_bfd_dummy_target, coff_object_p, /* bfd_check_format */
   bfd_generic_archive_p, _bfd_dummy_target},
 {bfd_false, coff_mkobject,	/* bfd_set_format */
   _bfd_generic_mkarchive, bfd_false},
 {bfd_false, coff_write_object_contents, /* bfd_write_contents */
   _bfd_write_archive_contents, bfd_false},
  JUMP_TABLE(coff),
  COFF_SWAP_TABLE,
  coff_i960_reloc_type_lookup,
  coff_make_debug_symbol,
};


bfd_target icoff_big_vec =
{
  "coff-Intel-big",		/* name */
  bfd_target_coff_flavour,
  false,			/* data byte order is little */
  true,				/* header byte order is big */

  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
  0,				/* leading underscore */
  '/',				/* ar_pad_char */
  15,				/* ar_max_namelen */

  3,				/* minimum alignment power */
_do_getl64, _do_putl64,  _do_getl32, _do_putl32, _do_getl16, _do_putl16, /* data */
_do_getb64, _do_putb64,  _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* hdrs */

  {_bfd_dummy_target, coff_object_p, /* bfd_check_format */
     bfd_generic_archive_p, _bfd_dummy_target},
  {bfd_false, coff_mkobject,	/* bfd_set_format */
     _bfd_generic_mkarchive, bfd_false},
  {bfd_false, coff_write_object_contents,	/* bfd_write_contents */
     _bfd_write_archive_contents, bfd_false},
  JUMP_TABLE(coff),
  COFF_SWAP_TABLE,
  coff_i960_reloc_type_lookup,
  coff_make_debug_symbol,
};
