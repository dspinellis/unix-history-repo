/* BFD library support routines for the Z800n architecture.
   Copyright (C) 1992 Free Software Foundation, Inc.
   Hacked by Steve Chamberlain of Cygnus Support.

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


/* 
Relocations for the Z8K

*/
static bfd_reloc_status_type 
DEFUN(howto16_callback,(abfd, reloc_entry, symbol_in, data,
			ignore_input_section, ignore_bfd),
      bfd *abfd AND
      arelent *reloc_entry AND
      struct symbol_cache_entry *symbol_in AND
      PTR data AND
      asection *ignore_input_section AND
      bfd *ignore_bfd)
{
  long relocation = 0;
  bfd_vma addr = reloc_entry->address;
  long x = bfd_get_16(abfd, (bfd_byte *)data + addr);

  HOWTO_PREPARE(relocation, symbol_in);

  x = (x + relocation + reloc_entry->addend);

  bfd_put_16(abfd, x, (bfd_byte *)data + addr);
  return bfd_reloc_ok;
}


static bfd_reloc_status_type 
DEFUN(howto8_callback,(abfd, reloc_entry, symbol_in, data,
		       ignore_input_section, ignore_bfd),
      bfd *abfd AND
      arelent *reloc_entry AND
      struct symbol_cache_entry *symbol_in AND
      PTR data AND
      asection *ignore_input_section AND
      bfd *ignore_bfd)
{
  long relocation = 0;
  bfd_vma addr = reloc_entry->address;
  long x = bfd_get_8(abfd, (bfd_byte *)data + addr);

  HOWTO_PREPARE(relocation, symbol_in);

  x = (x + relocation + reloc_entry->addend);

  bfd_put_8(abfd, x, (bfd_byte *)data + addr);
  return bfd_reloc_ok;
}


static bfd_reloc_status_type 
DEFUN(howto8_FFnn_callback,(abfd, reloc_entry, symbol_in, data,
			    ignore_input_section, ignore_bfd),
      bfd *abfd AND
      arelent *reloc_entry AND
      struct symbol_cache_entry *symbol_in AND
      PTR data AND
      asection *ignore_input_section AND
      bfd *ignore_bfd)
{
  long relocation = 0;
  bfd_vma addr = reloc_entry->address;

  long x = bfd_get_8(abfd, (bfd_byte *)data + addr);
  abort();
  HOWTO_PREPARE(relocation, symbol_in);

  x = (x + relocation + reloc_entry->addend);

  bfd_put_8(abfd, x, (bfd_byte *)data + addr);
  return bfd_reloc_ok;
}

static bfd_reloc_status_type 
DEFUN(howto8_pcrel_callback,(abfd, reloc_entry, symbol_in, data,
			     ignore_input_section, ignore_bfd),
      bfd *abfd AND
      arelent *reloc_entry AND
      struct symbol_cache_entry *symbol_in AND
      PTR data AND
      asection *ignore_input_section AND
      bfd *ignore_bfd)
{
  long relocation = 0;
  bfd_vma addr = reloc_entry->address;
  long x = bfd_get_8(abfd, (bfd_byte *)data + addr);
  abort();
  HOWTO_PREPARE(relocation, symbol_in);

  x = (x + relocation + reloc_entry->addend);

  bfd_put_8(abfd, x, (bfd_byte *)data + addr);
  return bfd_reloc_ok;
}



static reloc_howto_type howto_16
  = NEWHOWTO(howto16_callback,"abs16",1,false,false);
static reloc_howto_type howto_8
  = NEWHOWTO(howto8_callback,"abs8",0,false,false);

static reloc_howto_type howto_8_FFnn
  = NEWHOWTO(howto8_FFnn_callback,"ff00+abs8",0,false,false);

static reloc_howto_type howto_8_pcrel
  = NEWHOWTO(howto8_pcrel_callback,"pcrel8",0,false,true);


static CONST struct reloc_howto_struct *
DEFUN(local_bfd_reloc_type_lookup,(arch, code),
 CONST struct bfd_arch_info *arch AND
 bfd_reloc_code_real_type code)
{
  switch (code) {
  case BFD_RELOC_16:
    return &howto_16;
  case BFD_RELOC_8_FFnn:
    return &howto_8_FFnn;
  case BFD_RELOC_8:
    return &howto_8;
  case BFD_RELOC_8_PCREL:
    return &howto_8_pcrel;
  }
  return (reloc_howto_type *)NULL;
}

int bfd_default_scan_num_mach();

static boolean 
DEFUN(scan_mach,(info, string),
CONST struct bfd_arch_info *info AND
CONST char *string)
{
  if (strcmp(string,"z8001") == 0 || strcmp(string,"z8k") == 0) 
  {
    return bfd_mach_z8001 == info->mach;
  }
  if (strcmp(string,"z8002") == 0) 
  {
    return bfd_mach_z8002 == info->mach;
  }
  return false;
}


/* This routine is provided two arch_infos and returns whether
   they'd be compatible */

static CONST bfd_arch_info_type *
DEFUN(compatible,(a,b),
      CONST bfd_arch_info_type *a AND
      CONST bfd_arch_info_type *b)
{
  if (a->arch != b->arch || a->mach != b->mach)
   return NULL;
  return a;
}


static bfd_arch_info_type arch_info_struct[] = 
{ 
  { 32, 16, 8, bfd_arch_z8k, bfd_mach_z8002, "z8k", "z8002", 1, false, compatible, scan_mach, 0,},
  { 32, 32, 8, bfd_arch_z8k, bfd_mach_z8001, "z8k", "z8001", 1, true, compatible, scan_mach, 0,},
};



void
DEFUN_VOID(bfd_z8k_arch)
{
  bfd_arch_linkin(&arch_info_struct[0]);
  bfd_arch_linkin(&arch_info_struct[1]);
}


