/* bfd back-end for HP PA-RISC SOM objects.
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

#ifdef hp9000s800

#include <sysdep.h>
#include "bfd.h"
#include "libbfd.h"
#include "libhppa.h"

/* #include "aout/hppa.h" */

#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <machine/reg.h>
#ifndef hpux
#include <aout/hppa.h> 
#include <machine/pcb.h>
#include <sys/time.h>
#include <hpux/hpux.h>
#define USRSTACK 0x68FF3000
#else
#include <sys/user.h>           /* After a.out.h  */
#endif
#include <sys/file.h>
#include <errno.h>
 
struct container {
  struct header f;
  struct som_exec_auxhdr e;
};

#undef USIZE
#undef UPAGES

#define USIZE 3
#define UPAGES 7

void
fill_spaces(abfd, file_hdr, dbx_subspace, dbx_strings_subspace)
     bfd *abfd;
     struct header *file_hdr;
     struct subspace_dictionary_record *dbx_subspace, *dbx_strings_subspace;
{
  char *space_strings = (char *) alloca (file_hdr->space_strings_size);
  int i;
  /* for millicode games. */
  struct space_dictionary_record space;
  struct subspace_dictionary_record subspace;
  int index;
  /* indices of subspace entries for $TEXT$ and $GDB_DEBUG$ */
  int text_index = 0, gdb_debug_index = 0;

  /* initialize in case we don't find any dbx symbols. */
  dbx_subspace->subspace_length = dbx_strings_subspace->subspace_length = 0;
  bfd_seek (abfd, file_hdr->space_strings_location, SEEK_SET);
  if (bfd_read ((PTR) space_strings, 1, file_hdr->space_strings_size, abfd) 
      != file_hdr->space_strings_size)
    {
      bfd_error = wrong_format;  /* space strings table corrupted. */
      return;
    }
  bfd_seek (abfd, file_hdr->space_location, SEEK_SET);
  for (i = 0; i < file_hdr->space_total; i++)
    {
      bfd_read ((PTR) &space, 1, sizeof(space), abfd);
      index = (file_hdr->subspace_location +
               (space.subspace_index * sizeof(subspace)));
      if (!strcmp (space_strings + space.name.n_strx, "$TEXT$"))
        text_index = index;
      else if (!strcmp (space_strings + space.name.n_strx, "$GDB_DEBUG$"))
        gdb_debug_index = index;
    }
  /* search out the beginning and end if millicode */
  bfd_seek (abfd, text_index, SEEK_SET);
  for (;;)
    {
      bfd_read ((PTR) &subspace, 1, sizeof(subspace), abfd);
      if (!strcmp (space_strings + subspace.name.n_strx, "$MILLICODE$"))
        {
          millicode_start = subspace.subspace_start;
          millicode_end = (millicode_start + subspace.subspace_length);
          break;
        }
    }
  /* read symbols subspace and strings subspace in possibly arbitrary
     order. */
  bfd_seek (abfd, gdb_debug_index, SEEK_SET);
  bfd_read ((PTR) &subspace, 1, sizeof(struct subspace_dictionary_record), 
	    abfd);
  if (!strcmp (space_strings + subspace.name.n_strx, "$GDB_STRINGS$"))
    {
      *dbx_strings_subspace = subspace;
      bfd_read ((PTR) dbx_subspace, 1, 
		sizeof(struct subspace_dictionary_record), abfd);
    }
  else
    {
      *dbx_subspace = subspace;
      bfd_read ((PTR) dbx_strings_subspace, 1, 
		sizeof(struct subspace_dictionary_record), abfd);
    }
}

bfd_target *
DEFUN(hppa_object_setup,(abfd, file_hdrp, aux_hdrp, dbx_subspace, 
			 dbx_strings_subspace),
      bfd *abfd AND
      struct header *file_hdrp AND
      struct som_exec_auxhdr *aux_hdrp AND
      struct subspace_dictionary_record *dbx_subspace AND
      struct subspace_dictionary_record *dbx_strings_subspace)
{
  struct container *rawptr;
  struct header *f;
  struct hppa_data_struct *rawptr1;

  rawptr = (struct container *) bfd_zalloc (abfd, sizeof (struct container));
  if (rawptr == NULL) {
    bfd_error = no_memory;
    return 0;
  }

  rawptr1 = (struct hppa_data_struct *) bfd_zalloc (abfd, sizeof (struct hppa_data_struct));
  if (rawptr1 == NULL) {
    bfd_error = no_memory;
    return 0;
  }
  
  abfd->tdata.hppa_data = rawptr1;
  obj_file_hdr (abfd) = &rawptr->f;
  obj_aux_hdr (abfd) = &rawptr->e;
  *obj_file_hdr (abfd) = *file_hdrp;
  *obj_aux_hdr (abfd) = *aux_hdrp;

  /* Set the file flags */
  abfd->flags = NO_FLAGS;
  if (file_hdrp->entry_offset)
    abfd->flags |= HAS_RELOC;
  if (file_hdrp->symbol_total)
    abfd->flags |= HAS_LINENO | HAS_DEBUG | HAS_SYMS | HAS_LOCALS;

  bfd_get_start_address (abfd) = aux_hdrp->exec_entry;

  obj_hp_symbol_entry_size (abfd) = sizeof(struct symbol_dictionary_record); 
  obj_dbx_symbol_entry_size (abfd) = 12; 

  obj_pa_symbols (abfd) = (hppa_symbol_type *)NULL;
  obj_hp_sym_count (abfd) = file_hdrp->symbol_total;
  obj_dbx_sym_count (abfd) = dbx_subspace->subspace_length / 
    obj_dbx_symbol_entry_size (abfd);
  bfd_get_symcount (abfd) = obj_hp_sym_count (abfd) + obj_dbx_sym_count (abfd);

  bfd_default_set_arch_mach(abfd, bfd_arch_hppa, 0);

  /* create the sections.  This is raunchy, but bfd_close wants to reclaim
     them */
  obj_textsec (abfd) = (asection *)NULL;
  obj_datasec (abfd) = (asection *)NULL;
  obj_bsssec (abfd) = (asection *)NULL;
  (void)bfd_make_section(abfd, ".text");
  (void)bfd_make_section(abfd, ".data");
  (void)bfd_make_section(abfd, ".bss");

  abfd->sections = obj_textsec (abfd);
  obj_textsec (abfd)->next = obj_datasec (abfd);
  obj_datasec (abfd)->next = obj_bsssec (abfd);

  obj_datasec (abfd)->_raw_size = aux_hdrp->exec_dsize;
  obj_bsssec (abfd)->_raw_size = aux_hdrp->exec_bsize;
  obj_textsec (abfd)->_raw_size = aux_hdrp->exec_tsize;

  obj_textsec (abfd)->flags = (SEC_ALLOC | SEC_LOAD | SEC_HAS_CONTENTS);
  obj_datasec (abfd)->flags = (SEC_ALLOC | SEC_LOAD | SEC_HAS_CONTENTS);
  obj_bsssec (abfd)->flags = SEC_ALLOC;

  /* The virtual memory addresses of the sections */                    
  obj_datasec (abfd)->vma = aux_hdrp->exec_dmem;                          
  obj_bsssec (abfd)->vma = aux_hdrp->exec_bfill;                           
  obj_textsec (abfd)->vma = aux_hdrp->exec_tmem;                          
                                                                        
  /* The file offsets of the sections */                                
  obj_textsec (abfd)->filepos = aux_hdrp->exec_tfile;                      
  obj_datasec (abfd)->filepos = aux_hdrp->exec_dfile;                      
                                                                       
  /* The file offsets of the relocation info */                         
  obj_textsec (abfd)->rel_filepos = 0;                  
  obj_datasec (abfd)->rel_filepos = 0;                  
                                                                        
  /* The file offsets of the string table and symbol table.  */         
  obj_hp_sym_filepos (abfd) = file_hdrp->symbol_location;                  
  obj_hp_str_filepos (abfd) = file_hdrp->symbol_strings_location;           
  obj_dbx_sym_filepos (abfd) = dbx_subspace->file_loc_init_value;  
  obj_dbx_str_filepos (abfd) = dbx_strings_subspace->file_loc_init_value; 
  obj_hp_stringtab_size (abfd) = file_hdrp->symbol_strings_size;
  obj_dbx_stringtab_size (abfd) = dbx_strings_subspace->subspace_length;

  return abfd->xvec;
}

bfd_target *
DEFUN(hppa_object_p,(abfd),
      bfd *abfd)
{
  struct header file_hdr;
  struct som_exec_auxhdr aux_hdr;
  struct subspace_dictionary_record dbx_subspace;
  struct subspace_dictionary_record dbx_strings_subspace;

  if (bfd_read ((PTR) &file_hdr, 1, FILE_HDR_SIZE, abfd) != FILE_HDR_SIZE)
    {
      bfd_error = wrong_format;
      return 0;
    }
  if (bfd_read ((PTR) &aux_hdr, 1, AUX_HDR_SIZE, abfd) != AUX_HDR_SIZE)
    {
      bfd_error = wrong_format;
      return 0;
    }
  
  fill_spaces(abfd, &file_hdr, &dbx_subspace, &dbx_strings_subspace);

  return hppa_object_setup(abfd, &file_hdr, &aux_hdr, &dbx_subspace, &dbx_strings_subspace);
}


static boolean
DEFUN(hppa_mkobject,(abfd),
      bfd *abfd)
{ 
  fprintf (stderr, "hppa_mkobject unimplemented\n");
  fflush (stderr);
  abort ();
  return (false);
}

boolean
DEFUN(hppa_write_object_contents,(abfd),
      bfd *abfd)
{
  fprintf (stderr, "hppa_write_object_contents unimplemented\n");
  fflush (stderr);
  abort ();
  return (false);
}



unsigned int
DEFUN(hppa_get_symtab_upper_bound,(abfd),
      bfd *abfd)
{
  fprintf (stderr, "hppa_get_symtab_upper_bound unimplemented\n");
  fflush (stderr);
  abort ();
  return (0);
}

unsigned int
DEFUN(hppa_get_reloc_upper_bound,(abfd, asect),
      bfd *abfd AND
      sec_ptr asect)
{
  fprintf (stderr, "hppa_get_reloc_upper_bound unimplemented\n");
  fflush (stderr);
  abort ();
  return (0);
}

unsigned int
DEFUN(hppa_canonicalize_reloc,(abfd, section, relptr, symbols),
      bfd *abfd AND
      sec_ptr section AND
      arelent **relptr AND
      asymbol **symbols)
{
  fprintf (stderr, "hppa_canonicalize_reloc unimplemented\n");
  fflush (stderr);
  abort ();
}

extern bfd_target hppa_vec;
unsigned int
DEFUN(hppa_get_symtab,(abfd, location),
      bfd *abfd AND
      asymbol **location)
{
  fprintf (stderr, "hppa_get_symtab unimplemented\n");
  fflush (stderr);
  abort ();
  return (0);
}

asymbol *
DEFUN(hppa_make_empty_symbol,(abfd),
      bfd *abfd)
{
  hppa_symbol_type  *new =
    (hppa_symbol_type *)bfd_zalloc (abfd, sizeof (hppa_symbol_type));
  new->symbol.the_bfd = abfd;

  return &new->symbol;
}


void 
DEFUN(hppa_print_symbol,(ignore_abfd, afile,  symbol, how),
      bfd *ignore_abfd AND
      PTR afile AND
      asymbol *symbol AND
      bfd_print_symbol_type how)
{
  fprintf (stderr, "hppa_print_symbol unimplemented\n");
  fflush (stderr);
  abort ();
}



boolean
DEFUN(hppa_new_section_hook,(abfd, newsect),
      bfd *abfd AND
      asection *newsect)
{
  /* align to double at least */
  newsect->alignment_power = 3;

  if (bfd_get_format (abfd) == bfd_object) {
    if (obj_textsec(abfd) == NULL && !strcmp(newsect->name, ".text")) {
      obj_textsec(abfd)= newsect;
      return true;
    }

    if (obj_datasec(abfd) == NULL && !strcmp(newsect->name, ".data")) {
      obj_datasec(abfd) = newsect;
      return true;
    }

    if (obj_bsssec(abfd) == NULL && !strcmp(newsect->name, ".bss")) {
      obj_bsssec(abfd) = newsect;
      return true;
    }
  }

  /* We allow more than three sections internally */
  return true;
}




boolean
DEFUN(hppa_set_section_contents,(abfd, section, location, offset, count),
      bfd *abfd AND
      sec_ptr section AND
      PTR location AND
      file_ptr offset AND
      bfd_size_type count)
{
  fprintf (stderr, "hppa_set_section_contents unimplimented\n");
  fflush (stderr);
  abort();
  return false;
}


boolean
DEFUN(hppa_set_arch_mach,(abfd, arch, machine),
      bfd *abfd AND
      enum bfd_architecture arch AND
      unsigned long machine)
{
  fprintf (stderr, "hppa_set_arch_mach unimplemented\n");
  fflush (stderr);
  /* Allow any architecture to be supported by the hppa backend */
  return  bfd_default_set_arch_mach(abfd, arch, machine);
}


static boolean
DEFUN (hppa_find_nearest_line,(abfd,
                              section,
                              symbols,
                              offset,
                              filename_ptr,
                              functionname_ptr,
                              line_ptr),
      bfd            *abfd AND
      asection       *section AND
      asymbol       **symbols AND
      bfd_vma         offset AND
      CONST char      **filename_ptr AND
      CONST char       **functionname_ptr AND
      unsigned int   *line_ptr)
{
  fprintf (stderr, "hppa_find_nearest_line unimplemented\n");
  fflush (stderr);
  abort ();
  return (false);
}

static int
DEFUN (hppa_sizeof_headers, (abfd, reloc),
      bfd *abfd AND
      boolean reloc)
{
  fprintf (stderr, "hppa_sizeof_headers unimplemented\n");
  fflush (stderr);
  abort ();
  return (0);
}

#ifdef hpux
#define hppa_core_file_p _bfd_dummy_target
#else
bfd_target *
hppa_core_file_p (abfd)
     bfd *abfd;
{
  int val;
  struct hpuxuser u;
  unsigned int reg_offset, fp_reg_offset;
  /* This struct is just for allocating two things with one zalloc, so
     they will be freed together, without violating alignment constraints. */
  struct core_user {
        struct hppa_core_struct        coredata;
        struct hpuxuser         u;
  } *rawptr;

  val = bfd_read ((void *)&u, 1, sizeof u, abfd);
  if (val != sizeof u)
    return 0;                   /* Too small to be a core file */

  /* Sanity check perhaps??? */
  if (u.u_dsize > 0x1000000)    /* Remember, it's in pages... */
    return 0;
  if (u.u_ssize > 0x1000000)
    return 0;
  /* Check that the size claimed is no greater than the file size. FIXME. */

  /* OK, we believe you.  You're a core file (sure, sure).  */

  /* Allocate both the upage and the struct core_data at once, so
     a single free() will free them both.  */
  rawptr = (struct core_user *)bfd_zalloc (abfd, sizeof (struct core_user));
  if (rawptr == NULL) {
    bfd_error = no_memory;
    return 0;
  }

  abfd->tdata.hppa_core_data = &rawptr->coredata;
  core_upage (abfd) = &rawptr->u;
  *core_upage (abfd) = u;               /* Save that upage! */

  /* Create the sections.  This is raunchy, but bfd_close wants to free
     them separately.  */
  core_stacksec (abfd) = (asection *) zalloc (sizeof (asection));
  if (core_stacksec (abfd) == NULL) {
loser:
    bfd_error = no_memory;
    free ((void *)rawptr);
    return 0;
  }
  core_datasec (abfd) = (asection *) zalloc (sizeof (asection));
  if (core_datasec (abfd) == NULL) {
loser1:
    free ((void *)core_stacksec (abfd));
    goto loser;
  }
  core_regsec (abfd) = (asection *) zalloc (sizeof (asection));
  if (core_regsec (abfd) == NULL) {
loser2:
    free ((void *)core_datasec (abfd));
    goto loser1;
  }


  core_stacksec (abfd)->name = ".stack";
  core_datasec (abfd)->name = ".data";
  core_regsec (abfd)->name = ".reg";

  core_stacksec (abfd)->flags = SEC_ALLOC + SEC_LOAD + SEC_HAS_CONTENTS;
  core_datasec (abfd)->flags = SEC_ALLOC + SEC_LOAD + SEC_HAS_CONTENTS;
  core_regsec (abfd)->flags = SEC_ALLOC + SEC_HAS_CONTENTS;

  core_datasec (abfd)->_raw_size =  NBPG * u.u_dsize;
  core_stacksec (abfd)->_raw_size = NBPG * u.u_ssize;
  core_regsec (abfd)->_raw_size = NBPG * UPAGES;  /* Larger than sizeof struct u */

  core_datasec (abfd)->vma = u.hpuxu_exdata.somexec.a_Dmem;
  core_stacksec (abfd)->vma = USRSTACK; /* from sys/param */
  /* This is tricky.  As the "register section", we give them the entire
     upage and stack.  u.u_ar0 points to where "register 0" is stored.
     There are two tricks with this, though.  One is that the rest of the
     registers might be at positive or negative (or both) displacements
     from *u_ar0.  The other is that u_ar0 is sometimes an absolute address
     in kernel memory, and on other systems it is an offset from the beginning
     of the `struct user'.

     As a practical matter, we don't know where the registers actually are,
     so we have to pass the whole area to GDB.  We encode the value of u_ar0
     by setting the .regs section up so that its virtual memory address
     0 is at the place pointed to by u_ar0 (by setting the vma of the start
     of the section to -u_ar0).  GDB uses this info to locate the regs,
     using minor trickery to get around the offset-or-absolute-addr problem. */
  core_regsec (abfd)->vma = 0 - NBPG * USIZE;  /* -u_ar0  */

  core_datasec (abfd)->filepos = NBPG * UPAGES;
  core_stacksec (abfd)->filepos = (NBPG * UPAGES) + NBPG * u.u_dsize;
  core_regsec (abfd)->filepos = 0;      /* Register segment is the upage */

  /* Align to word at least */
  core_stacksec (abfd)->alignment_power = 2;
  core_datasec (abfd)->alignment_power = 2;
  core_regsec (abfd)->alignment_power = 2;

  abfd->sections = core_stacksec (abfd);
  core_stacksec (abfd)->next = core_datasec (abfd);
  core_datasec (abfd)->next = core_regsec (abfd);
  abfd->section_count = 3;

  return abfd->xvec;
}
#endif

#ifdef hpux
#define hppa_core_file_failing_command (char *(*)())(bfd_nullvoidptr)
#else
char *
hppa_core_file_failing_command (abfd)
     bfd *abfd;
{
#ifndef NO_CORE_COMMAND
  if (*core_upage (abfd)->u_comm)
    return core_upage (abfd)->u_comm;
  else
#endif
    return 0;
}
#endif

/* ARGSUSED */
int
hppa_core_file_failing_signal (ignore_abfd)
     bfd *ignore_abfd;
{
  return -1;            /* FIXME, where is it? */
}

/* ARGSUSED */
boolean
hppa_core_file_matches_executable_p  (core_bfd, exec_bfd)
     bfd *core_bfd, *exec_bfd;
{
  return true;          /* FIXME, We have no way of telling at this point */
}

#define hppa_bfd_debug_info_start        bfd_void
#define hppa_bfd_debug_info_end          bfd_void
#define hppa_bfd_debug_info_accumulate   (PROTO(void,(*),(bfd*, struct sec *))) bfd_void



#define hppa_openr_next_archived_file    bfd_generic_openr_next_archived_file
#define hppa_generic_stat_arch_elt       bfd_generic_stat_arch_elt
#define hppa_slurp_armap                  bfd_false
#define hppa_slurp_extended_name_table    _bfd_slurp_extended_name_table
#define hppa_truncate_arname              (void (*)())bfd_nullvoidptr
#define hppa_write_armap                  0

#define hppa_get_lineno                   (struct lineno_cache_entry *(*)())bfd_nullvoidptr
#define	hppa_close_and_cleanup	           bfd_generic_close_and_cleanup
#define hppa_get_section_contents          bfd_generic_get_section_contents

#define hppa_bfd_get_relocated_section_contents \
 bfd_generic_get_relocated_section_contents
#define hppa_bfd_relax_section bfd_generic_relax_section

/*SUPPRESS 460 */
bfd_target hppa_vec =
{
  "hppa",			/* name */
  bfd_target_hppa_flavour,
  true,				/* target byte order */
  true,				/* target headers byte order */
  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT | D_PAGED),
  (SEC_CODE|SEC_DATA|SEC_ROM|SEC_HAS_CONTENTS
   |SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
  ' ',				/* ar_pad_char */
  16,				/* ar_max_namelen */
    3,				/* minimum alignment */
_do_getb64, _do_putb64, _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* data */
_do_getb64, _do_putb64,  _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* hdrs */

  { _bfd_dummy_target,
     hppa_object_p,		/* bfd_check_format */
     bfd_generic_archive_p,
     hppa_core_file_p,
     },
  {
    bfd_false,
    hppa_mkobject, 
    _bfd_generic_mkarchive,
    bfd_false
    },
  {
    bfd_false,
    hppa_write_object_contents,
    _bfd_write_archive_contents,
    bfd_false,
  },
#undef hppa
  JUMP_TABLE(hppa)
};

#else	/* notdef hp9000s800 */
/* Prevent "empty translation unit" warnings from the idiots at X3J11. */
static char ansi_c_idiots = 69;
#endif	/* hp9000s800 */
