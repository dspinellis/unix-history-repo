/* BFD back-end for Intel 960 b.out binaries.
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


#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "seclet.h"
#include "bout.h"

#include "aout/stab_gnu.h"
#include "libaout.h"		/* BFD a.out internal data structures */


extern bfd_error_vector_type bfd_error_vector;
PROTO (static boolean, b_out_squirt_out_relocs,(bfd *abfd, asection *section));
PROTO (static bfd_target *, b_out_callback, (bfd *));

PROTO (boolean, aout_32_slurp_symbol_table, (bfd *abfd));
PROTO (void , aout_32_write_syms, ());

/* Swaps the information in an executable header taken from a raw byte
   stream memory image, into the internal exec_header structure.  */

PROTO(void, bout_swap_exec_header_in,
      (bfd *abfd,
      struct external_exec *raw_bytes,
      struct internal_exec *execp));
	 
void
DEFUN(bout_swap_exec_header_in,(abfd, raw_bytes, execp),
      bfd *abfd AND
      struct external_exec *raw_bytes AND
      struct internal_exec *execp)
{
  struct external_exec *bytes = (struct external_exec *)raw_bytes;

  /* Now fill in fields in the execp, from the bytes in the raw data.  */
  execp->a_info   = bfd_h_get_32 (abfd, bytes->e_info);
  execp->a_text   = GET_WORD (abfd, bytes->e_text);
  execp->a_data   = GET_WORD (abfd, bytes->e_data);
  execp->a_bss    = GET_WORD (abfd, bytes->e_bss);
  execp->a_syms   = GET_WORD (abfd, bytes->e_syms);
  execp->a_entry  = GET_WORD (abfd, bytes->e_entry);
  execp->a_trsize = GET_WORD (abfd, bytes->e_trsize);
  execp->a_drsize = GET_WORD (abfd, bytes->e_drsize);
  execp->a_tload  = GET_WORD (abfd, bytes->e_tload);
  execp->a_dload  = GET_WORD (abfd, bytes->e_dload);
  execp->a_talign = bytes->e_talign[0];
  execp->a_dalign = bytes->e_dalign[0];
  execp->a_balign = bytes->e_balign[0];
  execp->a_relaxable = bytes->e_relaxable[0];
}

/* Swaps the information in an internal exec header structure into the
   supplied buffer ready for writing to disk.  */

PROTO(void, bout_swap_exec_header_out,
	  (bfd *abfd,
	   struct internal_exec *execp,
	   struct external_exec *raw_bytes));
void
DEFUN(bout_swap_exec_header_out,(abfd, execp, raw_bytes),
     bfd *abfd AND
     struct internal_exec *execp AND 
     struct external_exec *raw_bytes)
{
  struct external_exec *bytes = (struct external_exec *)raw_bytes;

  /* Now fill in fields in the raw data, from the fields in the exec struct. */
  bfd_h_put_32 (abfd, execp->a_info  , bytes->e_info);
  PUT_WORD (abfd, execp->a_text  , bytes->e_text);
  PUT_WORD (abfd, execp->a_data  , bytes->e_data);
  PUT_WORD (abfd, execp->a_bss   , bytes->e_bss);
  PUT_WORD (abfd, execp->a_syms  , bytes->e_syms);
  PUT_WORD (abfd, execp->a_entry , bytes->e_entry);
  PUT_WORD (abfd, execp->a_trsize, bytes->e_trsize);
  PUT_WORD (abfd, execp->a_drsize, bytes->e_drsize);
  PUT_WORD (abfd, execp->a_tload , bytes->e_tload);
  PUT_WORD (abfd, execp->a_dload , bytes->e_dload);
  bytes->e_talign[0] = execp->a_talign;
  bytes->e_dalign[0] = execp->a_dalign;
  bytes->e_balign[0] = execp->a_balign;
  bytes->e_relaxable[0] = execp->a_relaxable;
}


static bfd_target *
b_out_object_p (abfd)
     bfd *abfd;
{
  struct internal_exec anexec;
  struct external_exec exec_bytes;

  if (bfd_read ((PTR) &exec_bytes, 1, EXEC_BYTES_SIZE, abfd)
      != EXEC_BYTES_SIZE) {
    bfd_error = wrong_format;
    return 0;
  }

  anexec.a_info = bfd_h_get_32 (abfd, exec_bytes.e_info);

  if (N_BADMAG (anexec)) {
    bfd_error = wrong_format;
    return 0;
  }

  bout_swap_exec_header_in (abfd, &exec_bytes, &anexec);
  return aout_32_some_aout_object_p (abfd, &anexec, b_out_callback);
}


/* Finish up the opening of a b.out file for reading.  Fill in all the
   fields that are not handled by common code.  */

static bfd_target *
b_out_callback (abfd)
     bfd *abfd;
{
  struct internal_exec *execp = exec_hdr (abfd);
  unsigned long bss_start;

  /* Architecture and machine type */
  bfd_set_arch_mach(abfd, 
		    bfd_arch_i960, /* B.out only used on i960 */
		    bfd_mach_i960_core /* Default */
		    );

  /* The positions of the string table and symbol table.  */
  obj_str_filepos (abfd) = N_STROFF (*execp);
  obj_sym_filepos (abfd) = N_SYMOFF (*execp);

  /* The alignments of the sections */
  obj_textsec (abfd)->alignment_power = execp->a_talign;
  obj_datasec (abfd)->alignment_power = execp->a_dalign;
  obj_bsssec  (abfd)->alignment_power = execp->a_balign;

  /* The starting addresses of the sections.  */
  obj_textsec (abfd)->vma = execp->a_tload;
  obj_datasec (abfd)->vma = execp->a_dload;

  /* And reload the sizes, since the aout module zaps them */
  obj_textsec (abfd)->_raw_size = execp->a_text;

  bss_start = execp->a_dload + execp->a_data; /* BSS = end of data section */
  obj_bsssec (abfd)->vma = align_power (bss_start, execp->a_balign);

  /* The file positions of the sections */
  obj_textsec (abfd)->filepos = N_TXTOFF(*execp);
  obj_datasec (abfd)->filepos = N_DATOFF(*execp);

  /* The file positions of the relocation info */
  obj_textsec (abfd)->rel_filepos = N_TROFF(*execp);
  obj_datasec (abfd)->rel_filepos =  N_DROFF(*execp);

  adata(abfd).page_size = 1;	/* Not applicable. */
  adata(abfd).segment_size = 1; /* Not applicable. */
  adata(abfd).exec_bytes_size = EXEC_BYTES_SIZE;

  if (execp->a_relaxable)
   abfd->flags |= BFD_IS_RELAXABLE;
  return abfd->xvec;
}

struct bout_data_struct {
    struct aoutdata a;
    struct internal_exec e;
};

static boolean
b_out_mkobject (abfd)
     bfd *abfd;
{
  struct bout_data_struct *rawptr;

  rawptr = (struct bout_data_struct *) bfd_zalloc (abfd, sizeof (struct bout_data_struct));
  if (rawptr == NULL) {
      bfd_error = no_memory;
      return false;
    }

  abfd->tdata.bout_data = rawptr;
  exec_hdr (abfd) = &rawptr->e;

  /* For simplicity's sake we just make all the sections right here. */
  obj_textsec (abfd) = (asection *)NULL;
  obj_datasec (abfd) = (asection *)NULL;
  obj_bsssec (abfd) = (asection *)NULL;

  bfd_make_section (abfd, ".text");
  bfd_make_section (abfd, ".data");
  bfd_make_section (abfd, ".bss");

  return true;
}

static boolean
b_out_write_object_contents (abfd)
     bfd *abfd;
{
  struct external_exec swapped_hdr;

  exec_hdr (abfd)->a_info = BMAGIC;

  exec_hdr (abfd)->a_text = obj_textsec (abfd)->_raw_size;
  exec_hdr (abfd)->a_data = obj_datasec (abfd)->_raw_size;
  exec_hdr (abfd)->a_bss = obj_bsssec (abfd)->_raw_size;
  exec_hdr (abfd)->a_syms = bfd_get_symcount (abfd) * sizeof (struct nlist);
  exec_hdr (abfd)->a_entry = bfd_get_start_address (abfd);
  exec_hdr (abfd)->a_trsize = ((obj_textsec (abfd)->reloc_count) *
                               sizeof (struct relocation_info));
  exec_hdr (abfd)->a_drsize = ((obj_datasec (abfd)->reloc_count) *
                               sizeof (struct relocation_info));

  exec_hdr (abfd)->a_talign = obj_textsec (abfd)->alignment_power;
  exec_hdr (abfd)->a_dalign = obj_datasec (abfd)->alignment_power;
  exec_hdr (abfd)->a_balign = obj_bsssec (abfd)->alignment_power;

  exec_hdr (abfd)->a_tload = obj_textsec (abfd)->vma;
  exec_hdr (abfd)->a_dload = obj_datasec (abfd)->vma;

  bout_swap_exec_header_out (abfd, exec_hdr (abfd), &swapped_hdr);

  bfd_seek (abfd, (file_ptr) 0, SEEK_SET);
  bfd_write ((PTR) &swapped_hdr, 1, EXEC_BYTES_SIZE, abfd);

  /* Now write out reloc info, followed by syms and strings */
  if (bfd_get_symcount (abfd) != 0) 
    {
      bfd_seek (abfd, (file_ptr)(N_SYMOFF(*exec_hdr(abfd))), SEEK_SET);

      aout_32_write_syms (abfd);

      bfd_seek (abfd, (file_ptr)(N_TROFF(*exec_hdr(abfd))), SEEK_SET);

      if (!b_out_squirt_out_relocs (abfd, obj_textsec (abfd))) return false;
      bfd_seek (abfd, (file_ptr)(N_DROFF(*exec_hdr(abfd))), SEEK_SET);

      if (!b_out_squirt_out_relocs (abfd, obj_datasec (abfd))) return false;
    }
  return true;
}

/** Some reloc hackery */

#define CALLS	 0x66003800	/* Template for 'calls' instruction	*/
#define BAL	 0x0b000000	/* Template for 'bal' instruction */
#define BALX	 0x85000000	/* Template for 'balx' instruction	*/
#define BAL_MASK 0x00ffffff
#define CALL     0x09000000
#define PCREL13_MASK 0x1fff
/* Magic to turn callx into calljx */
static bfd_reloc_status_type 
DEFUN (calljx_callback, (abfd, reloc_entry,  src, dst, input_section),
       bfd *abfd AND
       arelent *reloc_entry AND
       PTR src AND
       PTR dst AND

       asection *input_section)
{
  int  word = bfd_get_32(abfd, src);
  asymbol *symbol_in = *(reloc_entry->sym_ptr_ptr);
  aout_symbol_type  *symbol = aout_symbol(symbol_in);

  if (IS_CALLNAME(symbol->other)) 
  {

    aout_symbol_type *balsym = symbol+1;
    int inst = bfd_get_32(abfd, (bfd_byte *) src-4);
    /* The next symbol should be an N_BALNAME */
    BFD_ASSERT(IS_BALNAME(balsym->other));
    inst &= BAL_MASK;
    inst |= BALX;
    bfd_put_32(abfd, inst, (bfd_byte *) dst-4);
    symbol = balsym;
  }

    word += symbol->symbol.section->output_offset +
     symbol->symbol.section->output_section->vma +
      symbol->symbol.value + reloc_entry->addend;

  bfd_put_32(abfd, word, dst);
  return bfd_reloc_ok;
}


/* Magic to turn call into callj */
static bfd_reloc_status_type 
DEFUN (callj_callback, (abfd, reloc_entry,  data, srcidx,dstidx, input_section),
       bfd *abfd AND
       arelent *reloc_entry AND
       PTR data AND
       unsigned int srcidx AND
       unsigned int dstidx AND
       asection *input_section )
{
  int  word = bfd_get_32(abfd, (bfd_byte *) data + srcidx);
  asymbol *symbol_in = *(reloc_entry->sym_ptr_ptr);

  aout_symbol_type  *symbol = aout_symbol(symbol_in);

  if (IS_OTHER(symbol->other)) 
  {
    /* Call to a system procedure - replace code with system
       procedure number */
    word = CALLS | (symbol->other - 1);

  }

  else  if (IS_CALLNAME(symbol->other)) 
  {
    aout_symbol_type *balsym = symbol+1;
    /* The next symbol should be an N_BALNAME */
    BFD_ASSERT(IS_BALNAME(balsym->other));

    /* We are calling a leaf - so replace the call instruction
       with a bal */

    word = BAL |
     (((word & BAL_MASK) +
       balsym->symbol.section->output_offset +
       balsym->symbol.section->output_section->vma+
       balsym->symbol.value + reloc_entry->addend - dstidx -
       ( input_section->output_section->vma + input_section->output_offset))
      & BAL_MASK);


  }
  else 
  {

    word = CALL |
     (((word & BAL_MASK) + 
       symbol->symbol.section->output_offset +
       symbol->symbol.section->output_section->vma+
       symbol->symbol.value + reloc_entry->addend - dstidx -
       ( input_section->output_section->vma + input_section->output_offset))
      & BAL_MASK);
  }
  bfd_put_32(abfd, word, (bfd_byte *) data + dstidx);
  return bfd_reloc_ok;
}

/* type rshift size  bitsize  	pcrel	bitpos  absolute overflow check*/

#define ABS32CODE 0
#define ABS32CODE_SHRUNK 1 
#define PCREL24 2
#define CALLJ 3
#define ABS32 4
#define PCREL13 5
#define ABS32_MAYBE_RELAXABLE 1
#define ABS32_WAS_RELAXABLE 2

#define ALIGNER 10
#define ALIGNDONE 11
static reloc_howto_type howto_reloc_callj =
HOWTO(CALLJ, 0, 2, 24, true, 0, true, true, 0,"callj", true, 0x00ffffff, 0x00ffffff,false);
static  reloc_howto_type howto_reloc_abs32 =
HOWTO(ABS32, 0, 2, 32, false, 0, true, true,0,"abs32", true, 0xffffffff,0xffffffff,false);
static reloc_howto_type howto_reloc_pcrel24 =
HOWTO(PCREL24, 0, 2, 24, true, 0, true, true,0,"pcrel24", true, 0x00ffffff,0x00ffffff,false);

static reloc_howto_type howto_reloc_pcrel13 =
HOWTO(PCREL13, 0, 2, 13, true, 0, true, true,0,"pcrel13", true, 0x00001fff,0x00001fff,false);


static reloc_howto_type howto_reloc_abs32codeshrunk = 
HOWTO(ABS32CODE_SHRUNK, 0, 2, 24, true, 0, true, true, 0,"callx->callj", true, 0x00ffffff, 0x00ffffff,false);

static  reloc_howto_type howto_reloc_abs32code =
HOWTO(ABS32CODE, 0, 2, 32, false, 0, true, true,0,"callx", true, 0xffffffff,0xffffffff,false);

static reloc_howto_type howto_align_table[] = {
  HOWTO (ALIGNER, 0, 0x1, 0, false, 0, false, false, 0, "align16", false, 0, 0, false),
  HOWTO (ALIGNER, 0, 0x3, 0, false, 0, false, false, 0, "align32", false, 0, 0, false),
  HOWTO (ALIGNER, 0, 0x7, 0, false, 0, false, false, 0, "align64", false, 0, 0, false),
  HOWTO (ALIGNER, 0, 0xf, 0, false, 0, false, false, 0, "align128", false, 0, 0, false),
};

static reloc_howto_type howto_done_align_table[] = {
  HOWTO (ALIGNDONE, 0x1, 0x1, 0, false, 0, false, false, 0, "donealign16", false, 0, 0, false),
  HOWTO (ALIGNDONE, 0x3, 0x3, 0, false, 0, false, false, 0, "donealign32", false, 0, 0, false),
  HOWTO (ALIGNDONE, 0x7, 0x7, 0, false, 0, false, false, 0, "donealign64", false, 0, 0, false),
  HOWTO (ALIGNDONE, 0xf, 0xf, 0, false, 0, false, false, 0, "donealign128", false, 0, 0, false),
};

static reloc_howto_type *
b_out_reloc_type_lookup (abfd, code)
     bfd *abfd;
     bfd_reloc_code_real_type code;
{
  switch (code)
    {
    default:
      return 0;
    case BFD_RELOC_I960_CALLJ:
      return &howto_reloc_callj;
    case BFD_RELOC_32:
      return &howto_reloc_abs32;
    case BFD_RELOC_24_PCREL:
      return &howto_reloc_pcrel24;
    }
}

/* Allocate enough room for all the reloc entries, plus pointers to them all */

static boolean
b_out_slurp_reloc_table (abfd, asect, symbols)
     bfd *abfd;
     sec_ptr asect;
     asymbol **symbols;
{
  register struct relocation_info *rptr;
  unsigned int counter ;
  arelent *cache_ptr ;
  int extern_mask, pcrel_mask, callj_mask, length_shift;
  int incode_mask;
  int size_mask;
  bfd_vma prev_addr = 0;
  unsigned int count;
  size_t  reloc_size;
  struct relocation_info *relocs;
  arelent *reloc_cache;

  if (asect->relocation) return true;
  if (!aout_32_slurp_symbol_table (abfd)) return false;

  if (asect == obj_datasec (abfd)) {
    reloc_size = exec_hdr(abfd)->a_drsize;
    goto doit;
  }

  if (asect == obj_textsec (abfd)) {
    reloc_size = exec_hdr(abfd)->a_trsize;
    goto doit;
  }

  bfd_error = invalid_operation;
  return false;

 doit:
  bfd_seek (abfd, (file_ptr)(asect->rel_filepos),  SEEK_SET);
  count = reloc_size / sizeof (struct relocation_info);

  relocs = (struct relocation_info *) bfd_xmalloc (reloc_size);
  if (!relocs) {
    bfd_error = no_memory;
    return false;
  }
  reloc_cache = (arelent *) bfd_xmalloc ((count+1) * sizeof (arelent));
  if (!reloc_cache) {
    free ((char*)relocs);
    bfd_error = no_memory;
    return false;
  }

  if (bfd_read ((PTR) relocs, 1, reloc_size, abfd) != reloc_size) {
    bfd_error = system_call_error;
    free (reloc_cache);
    free (relocs);
    return false;
  }


  
  if (abfd->xvec->header_byteorder_big_p) {
    /* big-endian bit field allocation order */
    pcrel_mask  = 0x80;
    extern_mask = 0x10;
    incode_mask = 0x08;
    callj_mask  = 0x02;
    size_mask =   0x20;
    length_shift = 5;
  } else {
    /* little-endian bit field allocation order */
    pcrel_mask  = 0x01;
    extern_mask = 0x08;
    incode_mask = 0x10;
    callj_mask  = 0x40;
    size_mask   = 0x02;
    length_shift = 1;
  }

  for (rptr = relocs, cache_ptr = reloc_cache, counter = 0;
       counter < count;
       counter++, rptr++, cache_ptr++) 
  {
    unsigned char *raw = (unsigned char *)rptr;
    unsigned int symnum;
    cache_ptr->address = bfd_h_get_32 (abfd, raw + 0);
    cache_ptr->howto = 0;
    if (abfd->xvec->header_byteorder_big_p) 
    {
      symnum = (raw[4] << 16) | (raw[5] << 8) | raw[6];
    } 
    else
    {
      symnum = (raw[6] << 16) | (raw[5] << 8) | raw[4];
    }

    if (raw[7] & extern_mask) 
    {
      /* if this is set then the r_index is a index into the symbol table;
       * if the bit is not set then r_index contains a section map.
       * we either fill in the sym entry with a pointer to the symbol,
       * or point to the correct section
       */
      cache_ptr->sym_ptr_ptr = symbols + symnum;
      cache_ptr->addend = 0;
    } else 
    {
      /* in a.out symbols are relative to the beginning of the
       * file rather than sections ?
       * (look in translate_from_native_sym_flags)
       * the reloc entry addend has added to it the offset into the
       * file of the data, so subtract the base to make the reloc
       * section relative */
      int s;
      {
	/* sign-extend symnum from 24 bits to whatever host uses */
	s = symnum;
	if (s & (1 << 23))
	  s |= (~0) << 24;
      }
      cache_ptr->sym_ptr_ptr = (asymbol **)NULL;
      switch (s)
      {
       case N_TEXT:
       case N_TEXT | N_EXT:
	cache_ptr->sym_ptr_ptr = obj_textsec(abfd)->symbol_ptr_ptr;
	cache_ptr->addend = - obj_textsec(abfd)->vma;
	break;
       case N_DATA:
       case N_DATA | N_EXT:
	cache_ptr->sym_ptr_ptr = obj_datasec(abfd)->symbol_ptr_ptr;
	cache_ptr->addend = - obj_datasec(abfd)->vma;
	break;
       case N_BSS:
       case N_BSS | N_EXT:
	cache_ptr->sym_ptr_ptr = obj_bsssec(abfd)->symbol_ptr_ptr;
	cache_ptr->addend =  - obj_bsssec(abfd)->vma;
	break;
       case N_ABS:
       case N_ABS | N_EXT:
	cache_ptr->sym_ptr_ptr = obj_bsssec(abfd)->symbol_ptr_ptr;
	cache_ptr->addend = 0;
	break;
      case -2: /* .align */
	if (raw[7] & pcrel_mask)
	  {
	    cache_ptr->howto = &howto_align_table[(raw[7] >> length_shift) & 3];
	    cache_ptr->sym_ptr_ptr = &bfd_abs_symbol;
	  }
	else
	  {
	    /* .org? */
	    abort ();
	  }
	cache_ptr->addend = 0;
	break;
       default:
	BFD_ASSERT(0);
	break;
      }
	
    }

    /* the i960 only has a few relocation types:
       abs 32-bit and pcrel 24bit.   except for callj's!  */
    if (cache_ptr->howto != 0)
      ;
    else if (raw[7] & callj_mask)
    {
      cache_ptr->howto = &howto_reloc_callj;
    }
    else if ( raw[7] & pcrel_mask)
    {
      if (raw[7] & size_mask)
       cache_ptr->howto = &howto_reloc_pcrel13;
      else
       cache_ptr->howto = &howto_reloc_pcrel24;
    }
    else 
    {
      if (raw[7] & incode_mask) 
      {
	cache_ptr->howto = &howto_reloc_abs32code;
      }
      else 
      {
	cache_ptr->howto = &howto_reloc_abs32;
      }
    }
    if (cache_ptr->address < prev_addr) 
    {
      /* Ouch! this reloc is out of order, insert into the right place
       */
      arelent tmp;
      arelent *cursor = cache_ptr-1;
      bfd_vma stop = cache_ptr->address;
      tmp  = *cache_ptr;
      while (cursor->address > stop && cursor >= reloc_cache)
      {
	cursor[1] = cursor[0];
	cursor--;
      } 
      cursor[1] = tmp;
    }
    else 
    {
      prev_addr = cache_ptr->address;
    }
  }


  free (relocs);
  asect->relocation = reloc_cache;
  asect->reloc_count = count;


  return true;
}


static boolean
b_out_squirt_out_relocs (abfd, section)
     bfd *abfd;
     asection *section;
{

  arelent **generic;
  int r_extern;
  int r_idx;
  int incode_mask;  
  int len_1;
  unsigned int count = section->reloc_count;
  struct relocation_info *native, *natptr;
  size_t natsize = count * sizeof (struct relocation_info);
  int extern_mask, pcrel_mask,  len_2, callj_mask;
  if (count == 0) return true;
  generic   = section->orelocation;
  native = ((struct relocation_info *) bfd_xmalloc (natsize));
  if (!native) {
    bfd_error = no_memory;
    return false;
  }

  if (abfd->xvec->header_byteorder_big_p) 
  {
    /* Big-endian bit field allocation order */
    pcrel_mask  = 0x80;
    extern_mask = 0x10;
    len_2       = 0x40;
    len_1       = 0x20;
    callj_mask  = 0x02;
    incode_mask = 0x08;
  } 
  else 
  {
    /* Little-endian bit field allocation order */
    pcrel_mask  = 0x01;
    extern_mask = 0x08;
    len_2       = 0x04;
    len_1       = 0x02;
    callj_mask  = 0x40;
    incode_mask = 0x10;
  }

  for (natptr = native; count > 0; --count, ++natptr, ++generic) 
  {
    arelent *g = *generic;
    unsigned char *raw = (unsigned char *)natptr;
    asymbol *sym = *(g->sym_ptr_ptr);
      
    asection *output_section = sym->section->output_section;
    bfd_h_put_32(abfd, g->address, raw);  
    /* Find a type in the output format which matches the input howto - 
     * at the moment we assume input format == output format FIXME!!
     */
    /* FIXME:  Need callj stuff here, and to check the howto entries to
       be sure they are real for this architecture.  */
    if (g->howto== &howto_reloc_callj) 
    {
      raw[7] = callj_mask + pcrel_mask + len_2;
    }
    else if (g->howto == &howto_reloc_pcrel24) 
    {
      raw[7] = pcrel_mask + len_2;
    }
    else if (g->howto == &howto_reloc_pcrel13) 
    {
      raw[7] = pcrel_mask + len_1;
    }
    else if (g->howto == &howto_reloc_abs32code) 
    {
      raw[7] = len_2 + incode_mask;
    }
    else {
      raw[7] = len_2;
    }
    if (output_section == &bfd_com_section 
	|| output_section == &bfd_abs_section
	|| output_section == &bfd_und_section) 
    {

      if (bfd_abs_section.symbol == sym)
      {
	/* Whoops, looked like an abs symbol, but is really an offset
	   from the abs section */
	r_idx = 0;
	r_extern = 0;
       }
      else 
      {
	/* Fill in symbol */

	r_extern = 1;
	r_idx =  stoi((*(g->sym_ptr_ptr))->flags);
      }
    }
    else 
    {
      /* Just an ordinary section */
      r_extern = 0;
      r_idx  = output_section->target_index;      
    }

    if (abfd->xvec->header_byteorder_big_p) {
      raw[4] = (unsigned char) (r_idx >> 16);
      raw[5] = (unsigned char) (r_idx >>  8);
      raw[6] = (unsigned char) (r_idx     );
    } else {
      raw[6] = (unsigned char) (r_idx >> 16);
      raw[5] = (unsigned char) (r_idx>>  8);
      raw[4] = (unsigned char) (r_idx     );
    }  
    if (r_extern)
     raw[7] |= extern_mask; 
  }

  if (bfd_write ((PTR) native, 1, natsize, abfd) != natsize) {
    free((PTR)native);
    return false;
  }
  free ((PTR)native);

  return true;
}

/* This is stupid.  This function should be a boolean predicate */
static unsigned int
b_out_canonicalize_reloc (abfd, section, relptr, symbols)
     bfd *abfd;
     sec_ptr section;
     arelent **relptr;
     asymbol **symbols;
{
  arelent *tblptr = section->relocation;
  unsigned int count = 0;

 if (!(tblptr || b_out_slurp_reloc_table (abfd, section, symbols))) return 0;
  tblptr = section->relocation;
 if (!tblptr) return 0;

  for (; count++ < section->reloc_count;)
    *relptr++ = tblptr++;

  *relptr = 0;

  return section->reloc_count;
}

static unsigned int
b_out_get_reloc_upper_bound (abfd, asect)
     bfd *abfd;
     sec_ptr asect;
{
  if (bfd_get_format (abfd) != bfd_object) {
    bfd_error = invalid_operation;
    return 0;
  }

  if (asect == obj_datasec (abfd))
    return (sizeof (arelent *) *
	    ((exec_hdr(abfd)->a_drsize / sizeof (struct relocation_info))
	     +1));

  if (asect == obj_textsec (abfd))
    return (sizeof (arelent *) *
	    ((exec_hdr(abfd)->a_trsize / sizeof (struct relocation_info))
	     +1));

  bfd_error = invalid_operation;
  return 0;
}

static boolean
b_out_set_section_contents (abfd, section, location, offset, count)
     bfd *abfd;
     sec_ptr section;
     unsigned char *location;
     file_ptr offset;
      int count;
{

  if (abfd->output_has_begun == false) { /* set by bfd.c handler */
    if ((obj_textsec (abfd) == NULL) || (obj_datasec (abfd) == NULL) /*||
        (obj_textsec (abfd)->_cooked_size == 0) || (obj_datasec (abfd)->_cooked_size == 0)*/) {
      bfd_error = invalid_operation;
      return false;
    }

    obj_textsec (abfd)->filepos = sizeof(struct internal_exec);
    obj_datasec(abfd)->filepos = obj_textsec(abfd)->filepos 
                                 +  obj_textsec (abfd)->_raw_size;

  }
  /* regardless, once we know what we're doing, we might as well get going */
  bfd_seek (abfd, section->filepos + offset, SEEK_SET);

  if (count != 0) {
    return (bfd_write ((PTR)location, 1, count, abfd) == count) ?true:false;
  }
  return true;
}

static boolean
b_out_set_arch_mach (abfd, arch, machine)
     bfd *abfd;
     enum bfd_architecture arch;
     unsigned long machine;
{
  bfd_default_set_arch_mach(abfd, arch, machine);

  if (arch == bfd_arch_unknown)	/* Unknown machine arch is OK */
    return true;
  if (arch == bfd_arch_i960)	/* i960 default is OK */
    switch (machine) {
    case bfd_mach_i960_core:
    case bfd_mach_i960_kb_sb:
    case bfd_mach_i960_mc:
    case bfd_mach_i960_xa:
    case bfd_mach_i960_ca:
    case bfd_mach_i960_ka_sa:
    case 0:
      return true;
    default:
      return false;
    }

  return false;
}

static int 
DEFUN(b_out_sizeof_headers,(ignore_abfd, ignore),
      bfd *ignore_abfd AND
      boolean ignore)
{
  return sizeof(struct internal_exec);
}



/************************************************************************/
static bfd_vma 
DEFUN(get_value,(reloc, seclet),
      arelent  *reloc AND
      bfd_seclet_type *seclet)
{
  bfd_vma value;
  asymbol *symbol = *(reloc->sym_ptr_ptr);

  /* A symbol holds a pointer to a section, and an offset from the
     base of the section.  To relocate, we find where the section will
     live in the output and add that in */

  if (symbol->section == &bfd_und_section)
  {
    /* Ouch, this is an undefined symbol.. */
    bfd_error_vector.undefined_symbol(reloc, seclet);
    value = symbol->value;
  }
  else 
  {
    value = symbol->value +
     symbol->section->output_offset +
      symbol->section->output_section->vma;
  }

  /* Add the value contained in the relocation */
  value += (short)((reloc->addend) & 0xffff);
  
  return value;
}

static void
DEFUN(perform_slip,(s, slip, input_section, value),
      asymbol **s AND
      unsigned int slip AND
      asection *input_section AND
      bfd_vma value)
{
  
  /* Find all symbols past this point, and make them know
     what's happened */
  while (*s) 
  {
    asymbol *p = *s;
    if (p->section == input_section) 
    {
      /* This was pointing into this section, so mangle it */
      if (p->value > value)
      {
	p->value -=slip;
      }
    }
    s++;
	
  }    
}
#if 1
/* This routine works out if the thing we want to get to can be
   reached with a 24bit offset instead of a 32 bit one.
   If it can, then it changes the amode */

static int 
DEFUN(abs32code,(input_section, symbols, r, shrink),
      asection *input_section AND
      asymbol **symbols AND
      arelent *r AND
      unsigned int shrink) 
{
  bfd_vma value = get_value(r,0);
  bfd_vma dot = input_section->output_section->vma +  input_section->output_offset + r->address;	
  bfd_vma gap;
  
  /* See if the address we're looking at within 2^23 bytes of where
     we are, if so then we can use a small branch rather than the
     jump we were going to */

  gap = value - (dot - shrink);
  

  if (-1<<23 < (long)gap && (long)gap < 1<<23 )
  { 
    /* Change the reloc type from 32bitcode possible 24, to 24bit
       possible 32 */

    r->howto = &howto_reloc_abs32codeshrunk;
    /* The place to relc moves back by four bytes */
    r->address -=4;
	  
    /* This will be four bytes smaller in the long run */
    shrink += 4 ;
    perform_slip(symbols, 4, input_section, r->address-shrink +4);
  }      
  return shrink;      
}

static int 
DEFUN(aligncode,(input_section, symbols, r, shrink),
      asection *input_section AND
      asymbol **symbols AND
      arelent *r AND
      unsigned int shrink) 
{
  bfd_vma dot = input_section->output_section->vma +  input_section->output_offset + r->address;	
  bfd_vma gap;
  bfd_vma old_end;
  bfd_vma new_end;
  int shrink_delta;
  int size = r->howto->size;

  /* Reduce the size of the alignment so that it's still aligned but
     smaller  - the current size is already the same size as or bigger
     than the alignment required.  */

  /* calculate the first byte following the padding before we optimize */
  old_end = ((dot + size ) & ~size) + size+1;
  /* work out where the new end will be - remember that we're smaller
     than we used to be */
  new_end = ((dot - shrink + size) & ~size);

  /* This is the new end */
  gap = old_end - ((dot + size) & ~size);

  shrink_delta = (old_end - new_end) - shrink;

  if (shrink_delta)
  { 
    /* Change the reloc so that it knows how far to align to */
    r->howto = howto_done_align_table + (r->howto - howto_align_table);

    /* Encode the stuff into the addend - for future use we need to
       know how big the reloc used to be */
    r->addend = old_end ;

    /* This will be N bytes smaller in the long run, adjust all the symbols */
    perform_slip(symbols, shrink_delta, input_section, r->address - shrink );
    shrink += shrink_delta;
  }      
  return shrink;      
}


static boolean 
DEFUN(b_out_relax_section,(abfd, i, symbols),
      bfd *abfd AND
      asection *i AND
      asymbol **symbols)
{
  
  /* Get enough memory to hold the stuff */
  bfd *input_bfd = i->owner;
  asection *input_section = i;
  int shrink = 0 ;
  boolean new = false;
  
  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(input_bfd,
						       input_section);
  arelent **reloc_vector = (arelent **)alloca(reloc_size);

  /* Get the relocs and think about them */
  if (bfd_canonicalize_reloc(input_bfd, 
			     input_section,
			     reloc_vector,
			     symbols))
  {
    arelent **parent;
    for (parent = reloc_vector; *parent; parent++) 
    {
      arelent *r = *parent;
      switch (r->howto->type) {
       case ALIGNER:
	/* An alignment reloc */
	shrink = aligncode(input_section, symbols, r,shrink);
	new=true;
	break;
       case ABS32CODE:
	/* A 32bit reloc in an addressing mode */
	shrink = abs32code(input_section, symbols, r,shrink);
	new=true;
	break;
       case ABS32CODE_SHRUNK:
	shrink+=4;
	break;
      }
    }
  }
  input_section->_cooked_size = input_section->_raw_size - shrink;  

  return new;
}

#endif
static bfd_byte *
DEFUN(b_out_get_relocated_section_contents,(in_abfd, seclet, data),
      bfd *in_abfd AND
      bfd_seclet_type *seclet AND
      bfd_byte *data)

{
  /* Get enough memory to hold the stuff */
  bfd *input_bfd = seclet->u.indirect.section->owner;
  asection *input_section = seclet->u.indirect.section;
  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(input_bfd,
						       input_section);
  arelent **reloc_vector = (arelent **)alloca(reloc_size);
  
  /* read in the section */
  bfd_get_section_contents(input_bfd,
			   input_section,
			   data,
			   0,
			   input_section->_raw_size);
  
  
  if (bfd_canonicalize_reloc(input_bfd, 
			     input_section,
			     reloc_vector,
			     seclet->u.indirect.symbols) )
  {
    arelent **parent = reloc_vector;
    arelent *reloc ;
    


    unsigned int dst_address = 0;
    unsigned int src_address = 0;
    unsigned int run;
    unsigned int idx;
    
    /* Find how long a run we can do */
    while (dst_address < seclet->size) 
    {
      
      reloc = *parent;
      if (reloc) 
      {
	/* Note that the relaxing didn't tie up the addresses in the
	   relocation, so we use the original address to work out the
	   run of non-relocated data */
	run = reloc->address - src_address;
	parent++;
	
      }
      else 
      {
	run = seclet->size - dst_address;
      }
      /* Copy the bytes */
      for (idx = 0; idx < run; idx++)
      {
	data[dst_address++] = data[src_address++];
      }
    
      /* Now do the relocation */
    
      if (reloc) 
      {
	switch (reloc->howto->type) 
	{
	 case ABS32CODE:
	  calljx_callback(in_abfd, reloc, src_address + data, dst_address+data, input_section);
	  src_address+=4;
	  dst_address+=4;
	  break;
	 case ABS32:
	  bfd_put_32(in_abfd, get_value(reloc, seclet), data+dst_address);
	  src_address+=4;
	  dst_address+=4;
	  break;
	 case CALLJ:
	  callj_callback(in_abfd, reloc ,data,src_address,dst_address,input_section);
	  src_address+=4;
	  dst_address+=4;
	  break;
	 case ALIGNDONE:
	  src_address = reloc->addend;
	  dst_address = (dst_address + reloc->howto->size) & ~reloc->howto->size;
	  break;
	 case ABS32CODE_SHRUNK: 
	  /* This used to be a callx, but we've found out that a
	     callj will reach, so do the right thing */
	  callj_callback(in_abfd, reloc,data,src_address+4, dst_address,input_section);

	  dst_address+=4;
	  src_address+=8;
	  break;
	 case PCREL24:
	 {
	   long int word = bfd_get_32(in_abfd, data+src_address);
	   asymbol *symbol = *(reloc->sym_ptr_ptr);
	   word = (word & ~BAL_MASK) |
	    (((word & BAL_MASK) +
	      symbol->section->output_offset +
	      symbol->section->output_section->vma+
	      symbol->value + reloc->addend - dst_address -
	      ( input_section->output_section->vma + input_section->output_offset))
	     & BAL_MASK);

	   bfd_put_32(in_abfd,word,  data+dst_address);
	   dst_address+=4;
	   src_address+=4;

	 }
	  break;

	 case PCREL13:
	 {
	   long int word = bfd_get_32(in_abfd, data+src_address);
	   asymbol *symbol = *(reloc->sym_ptr_ptr);
	   word = (word & ~PCREL13_MASK) |
	    (((word & PCREL13_MASK) +
	      symbol->section->output_offset +
	      symbol->section->output_section->vma+
	      symbol->value + reloc->addend - dst_address -
	      ( input_section->output_section->vma + input_section->output_offset))
	     & PCREL13_MASK);

	   bfd_put_32(in_abfd,word,  data+dst_address);
	   dst_address+=4;
	   src_address+=4;

	 }
	  break;

	 default:

	  abort();
	}
      }    
    }
  }
  return data;
}
/***********************************************************************/

/* Build the transfer vectors for Big and Little-Endian B.OUT files.  */

/* We don't have core files.  */
#define	aout_32_core_file_failing_command _bfd_dummy_core_file_failing_command
#define	aout_32_core_file_failing_signal _bfd_dummy_core_file_failing_signal
#define	aout_32_core_file_matches_executable_p	\
				_bfd_dummy_core_file_matches_executable_p

/* We use BSD-Unix generic archive files.  */
#define	aout_32_openr_next_archived_file	bfd_generic_openr_next_archived_file
#define	aout_32_generic_stat_arch_elt	bfd_generic_stat_arch_elt
#define	aout_32_slurp_armap		bfd_slurp_bsd_armap
#define	aout_32_slurp_extended_name_table	bfd_true
#define	aout_32_write_armap		bsd_write_armap
#define	aout_32_truncate_arname		bfd_bsd_truncate_arname

/* We override these routines from the usual a.out file routines.  */
#define	aout_32_canonicalize_reloc	b_out_canonicalize_reloc
#define	aout_32_get_reloc_upper_bound	b_out_get_reloc_upper_bound
#define	aout_32_set_section_contents	b_out_set_section_contents
#define	aout_32_set_arch_mach		b_out_set_arch_mach
#define	aout_32_sizeof_headers		b_out_sizeof_headers

#define aout_32_bfd_debug_info_start		bfd_void
#define aout_32_bfd_debug_info_end		bfd_void
#define aout_32_bfd_debug_info_accumulate	(PROTO(void,(*),(bfd*, struct sec *))) bfd_void

#define aout_32_bfd_get_relocated_section_contents  b_out_get_relocated_section_contents
#define aout_32_bfd_relax_section                   b_out_relax_section

bfd_target b_out_vec_big_host =
{
  "b.out.big",			/* name */
  bfd_target_aout_flavour,
  false,			/* data byte order is little */
  true,				/* hdr byte order is big */
  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT ),
  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
  '_',				/* symbol leading char */
  ' ',				/* ar_pad_char */
  16,				/* ar_max_namelen */
  2,				/* minumum alignment power */

  _do_getl64, _do_putl64,  _do_getl32, _do_putl32, _do_getl16, _do_putl16, /* data */
  _do_getb64, _do_putb64,  _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* hdrs */
 {_bfd_dummy_target, b_out_object_p, /* bfd_check_format */
   bfd_generic_archive_p, _bfd_dummy_target},
 {bfd_false, b_out_mkobject,	/* bfd_set_format */
   _bfd_generic_mkarchive, bfd_false},
 {bfd_false, b_out_write_object_contents, /* bfd_write_contents */
   _bfd_write_archive_contents, bfd_false},

  JUMP_TABLE(aout_32),
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* COFF stuff?! */
  b_out_reloc_type_lookup,
};


bfd_target b_out_vec_little_host =
{
  "b.out.little",		/* name */
  bfd_target_aout_flavour,
  false,			/* data byte order is little */
  false,			/* header byte order is little */
  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT ),
  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
    '_',			/* symbol leading char */
  ' ',				/* ar_pad_char */
  16,				/* ar_max_namelen */
     2,				/* minum align */
_do_getl64, _do_putl64, _do_getl32, _do_putl32, _do_getl16, _do_putl16, /* data */
_do_getl64, _do_putl64, _do_getl32, _do_putl32, _do_getl16, _do_putl16, /* hdrs */
	 
    {_bfd_dummy_target, b_out_object_p, /* bfd_check_format */
       bfd_generic_archive_p, _bfd_dummy_target},
    {bfd_false, b_out_mkobject,	/* bfd_set_format */
       _bfd_generic_mkarchive, bfd_false},
    {bfd_false, b_out_write_object_contents,	/* bfd_write_contents */
       _bfd_write_archive_contents, bfd_false},
  JUMP_TABLE(aout_32),
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* COFF stuff?! */
  b_out_reloc_type_lookup,
};
