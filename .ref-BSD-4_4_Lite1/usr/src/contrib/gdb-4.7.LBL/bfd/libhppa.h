/* HP PA-RISC SOM object file format:  definitions internal to BFD.
   Copyright (C) 1990-1991 Free Software Foundation, Inc.

   Contributed by the Center for Software Science at the
   University of Utah (pa-gdb-bugs@cs.utah.edu).

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

#define BYTES_IN_WORD 4

struct header;
struct som_exec_auxhdr;
struct subspace_dictionary;



#define FILE_HDR_SIZE sizeof(struct header)
#define AUX_HDR_SIZE sizeof(struct som_exec_auxhdr)

int millicode_start, millicode_end;

struct nlist {
  union {
    char *n_name;
    struct nlist *n_next;
    long n_strx;
  } n_un;
  unsigned char n_type;
  char n_other;
  short n_desc;
  unsigned long n_value;
};

typedef struct hppa_symbol
{
  asymbol symbol;
  short desc;
  char other;
  unsigned char type;
} hppa_symbol_type;

struct hppadata
{
  struct header *file_hdr;
  struct som_exec_auxhdr *aux_hdr;
  hppa_symbol_type *symbols;

  /* For ease, we do this */
  asection *textsec;
  asection *datasec;
  asection *bsssec;

  /* We remember these offsets so that after check_file_format, we have
     no dependencies on the particular format of the exec_hdr.  */
  file_ptr dbx_sym_filepos;
  file_ptr dbx_str_filepos;

  file_ptr hp_sym_filepos;
  file_ptr hp_str_filepos;

  int dbx_sym_count;
  int hp_sym_count;

  unsigned dbx_stringtab_size;
  unsigned hp_stringtab_size;

  /* Size of a symbol table entry in external form */
  unsigned dbx_symbol_entry_size;
  unsigned hp_symbol_entry_size;
};

struct hppa_data_struct {
  struct hppadata a;
};


#define padata(bfd)              ((bfd)->tdata.hppa_data->a)
#define obj_file_hdr(bfd)           (padata(bfd).file_hdr)
#define obj_aux_hdr(bfd)            (padata(bfd).aux_hdr)
#define obj_pa_symbols(bfd)   (padata(bfd).symbols)
#define obj_textsec(bfd)        (padata(bfd).textsec)
#define obj_datasec(bfd)        (padata(bfd).datasec)
#define obj_bsssec(bfd)         (padata(bfd).bsssec)
#define obj_dbx_sym_filepos(bfd)    (padata(bfd).dbx_sym_filepos)
#define obj_dbx_str_filepos(bfd)    (padata(bfd).dbx_str_filepos)
#define obj_hp_sym_filepos(bfd) (padata(bfd).hp_sym_filepos)
#define obj_hp_str_filepos(bfd) (padata(bfd).hp_str_filepos)
#define obj_dbx_sym_count(bfd)   (padata(bfd).dbx_sym_count)
#define obj_hp_sym_count(bfd)   (padata(bfd).hp_sym_count)
#define obj_dbx_stringtab_size(bfd)  (padata(bfd).dbx_stringtab_size)
#define obj_hp_stringtab_size(bfd)  (padata(bfd).hp_stringtab_size)
#define obj_dbx_symbol_entry_size(bfd)  (padata(bfd).dbx_symbol_entry_size)
#define obj_hp_symbol_entry_size(bfd)  (padata(bfd).hp_symbol_entry_size)

/* We take the address of the first element of an asymbol to ensure that the
   macro is only ever applied to an asymbol */
#define hppa_symbol(asymbol) ((hppa_symbol_type *)(&(asymbol)->the_bfd))


/* These are stored in the bfd's tdata */
struct hppa_core_struct 
{
  struct hpuxuser *upage; 
  asection *data_section;
  asection *stack_section;
  asection *reg_section;
};


#define core_upage(bfd) ((bfd)->tdata.hppa_core_data->upage)
#define core_datasec(bfd) ((bfd)->tdata.hppa_core_data->data_section)
#define core_stacksec(bfd) ((bfd)->tdata.hppa_core_data->stack_section)
#define core_regsec(bfd) ((bfd)->tdata.hppa_core_data->reg_section)
