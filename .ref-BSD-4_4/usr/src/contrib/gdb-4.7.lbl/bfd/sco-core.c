/* BFD back-end for SCO System V 3.2 Unix core files.
   This was based on trad-core.c, which was written by John Gilmore of
        Cygnus Support.
   Copyright 1988, 1989, 1991, 1992 Free Software Foundation, Inc.
   Written by Scott Michel, IntelliMED Corporation. scottm%intime@uunet

   NB: This does work with SCO System V 3.2 Unix and ODT. However, it
       may well work with other i386 based *nixen and *nixes.

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

/* To use this file on a particular host, configure the host with these
   parameters in the config/h-HOST file:

	HDEFINES=-DSCO_CORE=1
	HDEPFILES=sco-core.o
 */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "obstack.h"
#include "coff/i386.h"
#include "coff/internal.h"
#include "libcoff.h"

#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>

#include <sys/file.h>
#include <sys/user.h>		/* After a.out.h  */

#include <errno.h>

/* This struct is just for allocating two things with one zalloc, so
   they will be freed together, without violating alignment constraints. */

struct trad_core_struct 
  {
    asection *data_section;
    asection *stack_section;
    asection *reg_section;

    struct user		u;
  } *rawptr;

#define core_upage(bfd) (&((bfd)->tdata.trad_core_data->u))
#define core_datasec(bfd) ((bfd)->tdata.trad_core_data->data_section)
#define core_stacksec(bfd) ((bfd)->tdata.trad_core_data->stack_section)
#define core_regsec(bfd) ((bfd)->tdata.trad_core_data->reg_section)

/* Handle 4.2-style (and perhaps also sysV-style) core dump file.  */

/* ARGSUSED */
bfd_target *
sco_core_file_p (abfd)
     bfd *abfd;

{
  int val;
  struct user u;

  bfd_seek(abfd, (file_ptr) 0, SEEK_SET);
  val = bfd_read ((void *)&u, 1, sizeof u, abfd);
  if (val != sizeof u)
    return 0;			/* Too small to be a core file */

  /* Check that the size claimed is no greater than the file size. FIXME. */

  /* OK, we believe you.  You're a core file (sure, sure).  */

  /* Allocate both the upage and the struct core_data at once, so
     a single free() will free them both.  */
  rawptr = (struct trad_core_struct *)
	   bfd_zalloc (abfd, sizeof (struct trad_core_struct));
  if (rawptr == NULL) {
    bfd_error = no_memory;
    return 0;
  }
  
  abfd->tdata.trad_core_data = rawptr;

  rawptr->u = u; /* Copy the uarea into the tdata part of the bfd */

  /* Create the sections.  This is raunchy, but bfd_close wants to free
     them separately.  */

  core_stacksec(abfd) = (asection *) zalloc (sizeof (asection));
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
    free ((void *)core_datasec (abfd));
    goto loser1;
  }

  core_stacksec (abfd)->name = ".stack";
  core_datasec (abfd)->name = ".data";
  core_regsec (abfd)->name = ".reg";

  core_stacksec (abfd)->flags = SEC_ALLOC + SEC_LOAD + SEC_HAS_CONTENTS;
  core_datasec (abfd)->flags = SEC_ALLOC + SEC_LOAD + SEC_HAS_CONTENTS;
  core_regsec (abfd)->flags = SEC_ALLOC + SEC_HAS_CONTENTS;

  core_datasec (abfd)->_raw_size =  NBPC * u.u_dsize;
  core_stacksec (abfd)->_raw_size = NBPC * u.u_ssize;
  core_regsec (abfd)->_raw_size = NBPC * USIZE; /* Larger than sizeof struct u */

  /* Under SCO, we get at the exact addresses of things: */
  core_datasec (abfd)->vma = u.u_exdata.ux_datorg;
  core_stacksec (abfd)->vma = u.u_sub;

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
  core_regsec (abfd)->vma = 0 - (int) u.u_ar0;

  core_datasec (abfd)->filepos = NBPC * USIZE;
  core_stacksec (abfd)->filepos = (NBPC * USIZE) + NBPC * u.u_dsize;
  core_regsec (abfd)->filepos = 0; /* Register segment is the upage */

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

char *
sco_core_file_failing_command (abfd)
     bfd *abfd;
{
  char *com = abfd->tdata.trad_core_data->u.u_comm;
  if (*com)
    return com;
  else
    return 0;
}

/* ARGSUSED */
int
sco_core_file_failing_signal (abfd)
     bfd *abfd;
{
  int retval = -1;

  if (core_upage(abfd)->u_sysabort != 0)
    retval = core_upage(abfd)->u_sysabort;

  return retval;
}

/* ARGSUSED */
boolean
sco_core_file_matches_executable_p  (core_bfd, exec_bfd)
     bfd *core_bfd, *exec_bfd;
{
  return true;
}

/* No archive file support via this BFD */
#define	sco_openr_next_archived_file	bfd_generic_openr_next_archived_file
#define	sco_generic_stat_arch_elt	bfd_generic_stat_arch_elt
#define	sco_slurp_armap			bfd_false
#define	sco_slurp_extended_name_table	bfd_true
#define	sco_write_armap			(PROTO (boolean, (*),	\
     (bfd *arch, unsigned int elength, struct orl *map, \
      unsigned int orl_count, int stridx))) bfd_false
#define	sco_truncate_arname		bfd_dont_truncate_arname
#define	aout_32_openr_next_archived_file bfd_generic_openr_next_archived_file

#define	sco_close_and_cleanup		bfd_generic_close_and_cleanup
#define	sco_set_section_contents		(PROTO(boolean, (*),	\
         (bfd *abfd, asection *section, PTR data, file_ptr offset,	\
         bfd_size_type count))) bfd_false
#define	sco_get_section_contents		bfd_generic_get_section_contents
#define	sco_new_section_hook		(PROTO (boolean, (*),	\
	(bfd *, sec_ptr))) bfd_true
#define	sco_get_symtab_upper_bound	bfd_0u
#define	sco_get_symtab			(PROTO (unsigned int, (*), \
        (bfd *, struct symbol_cache_entry **))) bfd_0u
#define	sco_get_reloc_upper_bound		(PROTO (unsigned int, (*), \
	(bfd *, sec_ptr))) bfd_0u
#define	sco_canonicalize_reloc		(PROTO (unsigned int, (*), \
	(bfd *, sec_ptr, arelent **, struct symbol_cache_entry**))) bfd_0u
#define	sco_make_empty_symbol		(PROTO (		\
	struct symbol_cache_entry *, (*), (bfd *))) bfd_false
#define	sco_print_symbol			(PROTO (void, (*),	\
	(bfd *, PTR, struct symbol_cache_entry  *,			\
	 bfd_print_symbol_type))) bfd_false
#define	sco_get_lineno			(PROTO (alent *, (*),	\
	(bfd *, struct symbol_cache_entry *))) bfd_nullvoidptr
#define	sco_set_arch_mach			(PROTO (boolean, (*),	\
	(bfd *, enum bfd_architecture, unsigned long))) bfd_false
#define	sco_find_nearest_line		(PROTO (boolean, (*),	\
        (bfd *abfd, struct sec  *section,				\
         struct symbol_cache_entry  **symbols,bfd_vma offset,		\
         CONST char **file, CONST char **func, unsigned int *line))) bfd_false
#define	sco_sizeof_headers		(PROTO (int, (*),	\
	(bfd *, boolean))) bfd_0

#define sco_bfd_debug_info_start		bfd_void
#define sco_bfd_debug_info_end		bfd_void
#define sco_bfd_debug_info_accumulate	(PROTO (void, (*),	\
	(bfd *, struct sec *))) bfd_void
#define sco_bfd_get_relocated_section_contents bfd_generic_get_relocated_section_contents
#define sco_bfd_relax_section bfd_generic_relax_section
/* If somebody calls any byte-swapping routines, shoot them.  */
void
swap_abort()
{
  abort(); /* This way doesn't require any declaration for ANSI to fuck up */
}
#define	NO_GET	((PROTO(bfd_vma, (*), (         bfd_byte *))) swap_abort )
#define	NO_PUT	((PROTO(void,    (*), (bfd_vma, bfd_byte *))) swap_abort )

bfd_target sco_core_vec =
  {
    "sco-core",
    bfd_target_unknown_flavour,
    true,			/* target byte order */
    true,			/* target headers byte order */
  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
    ' ',						   /* ar_pad_char */
    16,							   /* ar_max_namelen */
    3,							   /* minimum alignment power */
    NO_GET, NO_PUT, NO_GET, NO_PUT, NO_GET, NO_PUT, /* data */
    NO_GET, NO_PUT, NO_GET, NO_PUT, NO_GET, NO_PUT, /* hdrs */

    {_bfd_dummy_target, _bfd_dummy_target,
     _bfd_dummy_target, sco_core_file_p},
    {bfd_false, bfd_false,	/* bfd_create_object */
     bfd_false, bfd_false},
    {bfd_false, bfd_false,	/* bfd_write_contents */
     bfd_false, bfd_false},
    
    JUMP_TABLE(sco),
};


