/* BFD back-end for Motorola 88000 COFF "Binary Compatability Standard" files.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
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

#define M88 1		/* Customize various include files */
#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "obstack.h"
#include "coff/m88k.h"
#include "coff/internal.h"
#include "libcoff.h"
#undef HOWTO_PREPARE
/* Provided the symbol, returns the value reffed */
#define HOWTO_PREPARE(relocation, symbol) 	\
  {						\
  if (symbol != (asymbol *)NULL) {		\
    if (symbol->section == &bfd_com_section) {	\
      relocation = 0;				\
    }						\
    else {					\
      relocation = symbol->value;		\
    }						\
  }						\
  if (symbol->section != (asection *)NULL) {	\
    relocation += symbol->section->output_section->vma +	\
      symbol->section->output_offset;		\
  }						\
}			


static bfd_reloc_status_type 
DEFUN(howto_hvrt16,(abfd, reloc_entry, symbol_in, data,
		    ignore_input_section, ignore_bfd),
      bfd *abfd AND
      arelent *reloc_entry AND
      asymbol *symbol_in AND
      PTR data AND
      asection *ignore_input_section AND
      bfd *ignore_bfd)
{
  long relocation = 0;
  bfd_vma addr = reloc_entry->address;
  long x = bfd_get_16(abfd, (bfd_byte *)data + addr);

  HOWTO_PREPARE(relocation, symbol_in);

  x = (x + relocation + reloc_entry->addend) >> 16;

  bfd_put_16(abfd, x, (bfd_byte *)data + addr);
  return bfd_reloc_ok;
}



static reloc_howto_type howto_table[] = 
{
  HOWTO(R_PCR16L,02,1,16,true, 0,false,true,0,"PCR16L",false,0x0000ffff,0x0000ffff,true),
  HOWTO(R_PCR26L,02,2,16,true, 0,false,true,0,"PCR26L",false,0x03ffffff,0x03ffffff,true),
  HOWTO(R_VRT16, 00,1,16,false,0,false,true,0,"VRT16", false,0x0000ffff,0x0000ffff,true),
  HOWTO(R_HVRT16,16,1,16,false,0,false,true,howto_hvrt16,"HVRT16",false,0x0000ffff,0x0000ffff,true),
  HOWTO(R_LVRT16,00,1,16,false,0,false,true,0,"LVRT16",false,0x0000ffff,0x0000ffff,true),
  HOWTO(R_VRT32, 00,2,32,false,0,false,true,0,"VRT32", false,0xffffffff,0xffffffff,true),
};


/* Code to swap in the reloc offset */
#define SWAP_IN_RELOC_OFFSET   bfd_h_get_16
#define SWAP_OUT_RELOC_OFFSET bfd_h_put_16


/* Code to turn an external r_type into a pointer to an entry in the
   above howto table */
#define RTYPE2HOWTO(cache_ptr, dst)					\
	    if ((dst)->r_type >= R_PCR16L && (dst)->r_type <= R_VRT32) {\
		cache_ptr->howto = howto_table + (dst)->r_type - R_PCR16L;\
		cache_ptr->addend += (dst)->r_offset << 16;		\
	    }								\
	    else {							\
		BFD_ASSERT(0);						\
	    }



#define BADMAG(x) MC88BADMAG(x)
#include "coffcode.h"

#undef coff_write_armap

bfd_target m88kbcs_vec =
{
  "coff-m88kbcs",		/* name */
  bfd_target_coff_flavour,
  true,				/* data byte order is big */
  true,				/* header byte order is big */

  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
  0,				/* leading underscore */
  '/',				/* ar_pad_char */
  15,				/* ar_max_namelen */
  3,				/* default alignment power */
  _do_getb64, _do_putb64,  _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* data */
  _do_getb64, _do_putb64,   _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* hdrs */

    {_bfd_dummy_target, coff_object_p, /* bfd_check_format */
       bfd_generic_archive_p, _bfd_dummy_target},
    {bfd_false, coff_mkobject, _bfd_generic_mkarchive, /* bfd_set_format */
       bfd_false},
    {bfd_false, coff_write_object_contents, /* bfd_write_contents */
       _bfd_write_archive_contents, bfd_false},

  JUMP_TABLE(coff),
  COFF_SWAP_TABLE
};
