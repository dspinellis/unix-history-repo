/* BFD back-end for a.out.adobe binaries.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Written by Cygnus Support.  Based on bout.c.

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

#include "aout/adobe.h"

#include "aout/stab_gnu.h"
#include "libaout.h"		/* BFD a.out internal data structures */

extern bfd_target a_out_adobe_vec;		/* Forward decl */

PROTO (static bfd_target *, aout_adobe_callback, (bfd *));

PROTO (boolean, aout_32_slurp_symbol_table, (bfd *abfd));
PROTO (void , aout_32_write_syms, ());
PROTO (static void, aout_adobe_write_section, (bfd *abfd, sec_ptr sect));

/* Swaps the information in an executable header taken from a raw byte
   stream memory image, into the internal exec_header structure.  */

PROTO(void, aout_adobe_swap_exec_header_in,
      (bfd *abfd,
      struct external_exec *raw_bytes,
      struct internal_exec *execp));
	 
void
DEFUN(aout_adobe_swap_exec_header_in,(abfd, raw_bytes, execp),
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
}

/* Swaps the information in an internal exec header structure into the
   supplied buffer ready for writing to disk.  */

PROTO(void, aout_adobe_swap_exec_header_out,
	  (bfd *abfd,
	   struct internal_exec *execp,
	   struct external_exec *raw_bytes));
void
DEFUN(aout_adobe_swap_exec_header_out,(abfd, execp, raw_bytes),
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
}


static bfd_target *
aout_adobe_object_p (abfd)
     bfd *abfd;
{
  struct internal_exec anexec;
  struct external_exec exec_bytes;
  char *targ;

  if (bfd_read ((PTR) &exec_bytes, 1, EXEC_BYTES_SIZE, abfd)
      != EXEC_BYTES_SIZE) {
    bfd_error = wrong_format;
    return 0;
  }

  anexec.a_info = bfd_h_get_32 (abfd, exec_bytes.e_info);

  /* Normally we just compare for the magic number.
     However, a bunch of Adobe tools aren't fixed up yet; they generate
     files using ZMAGIC(!).
     If the environment variable GNUTARGET is set to "a.out.adobe", we will
     take just about any a.out file as an Adobe a.out file.  FIXME!  */

  if (N_BADMAG (anexec)) {
    targ = getenv ("GNUTARGET");
    if (targ && strcmp (targ, a_out_adobe_vec.name))
      ;		/* Just continue anyway, if specifically set to this format */
    else
      {
	bfd_error = wrong_format;
	return 0;
      }
  }

  aout_adobe_swap_exec_header_in (abfd, &exec_bytes, &anexec);
  return aout_32_some_aout_object_p (abfd, &anexec, aout_adobe_callback);
}


/* Finish up the opening of a b.out file for reading.  Fill in all the
   fields that are not handled by common code.  */

static bfd_target *
aout_adobe_callback (abfd)
     bfd *abfd;
{
  struct internal_exec *execp = exec_hdr (abfd);
  asection *sect;
  struct external_segdesc ext[1];
  char *section_name;
  char try_again[30];	/* name and number */
  char *newname;
  int trynum;
  flagword flags;

  /* Architecture and machine type -- unknown in this format.  */
  bfd_set_arch_mach(abfd, bfd_arch_unknown, 0);

  /* The positions of the string table and symbol table.  */
  obj_str_filepos (abfd) = N_STROFF (*execp);
  obj_sym_filepos (abfd) = N_SYMOFF (*execp);

  /* Suck up the section information from the file, one section at a time.  */

  for (;;) {
    if (bfd_read ((PTR) ext, 1, sizeof (*ext), abfd) != sizeof (*ext)) {
      bfd_error = wrong_format;
      return 0;
    }
    switch (ext->e_type[0]) {
    case N_TEXT:
      section_name = ".text";
      flags = SEC_CODE | SEC_LOAD | SEC_ALLOC | SEC_HAS_CONTENTS;
      break;

    case N_DATA:
      section_name = ".data";
      flags = SEC_DATA | SEC_LOAD | SEC_ALLOC | SEC_HAS_CONTENTS;
      break;

    case N_BSS:
      section_name = ".bss";
      flags = SEC_DATA | SEC_HAS_CONTENTS;
      break;

    case 0:
      goto no_more_sections;

    default:
      fprintf (stderr, "Unknown section type in a.out.adobe file: %x\n", 
	       ext->e_type[0]);
      goto no_more_sections;
    }

    /* First one is called ".text" or whatever; subsequent ones are
       ".text1", ".text2", ... */

    bfd_error = no_error;
    sect = bfd_make_section (abfd, section_name);
    trynum = 0;
    while (!sect) {
      if (bfd_error != no_error)
	return 0;	/* Some other error -- slide into the sunset */
      sprintf (try_again, "%s%d", section_name, ++trynum);
      sect = bfd_make_section (abfd, try_again);
    }

    /* Fix the name, if it is a sprintf'd name.  */
    if (sect->name == try_again) {
      newname = (char *) bfd_zalloc(abfd, strlen (sect->name));
      if (newname == NULL) {
	bfd_error = no_memory;
	return 0;
      }
      strcpy (newname, sect->name);
      sect->name = newname;
    }

    /* Now set the section's attributes.  */
    bfd_set_section_flags (abfd, sect, flags);
    sect->_raw_size = ((ext->e_size[0] << 8)	/* Assumed big-endian */
		      | ext->e_size[1] << 8)
		      | ext->e_size[2];
    sect->_cooked_size = sect->_raw_size;
    sect->vma = bfd_h_get_32 (abfd, ext->e_virtbase);
    sect->filepos = bfd_h_get_32 (abfd, ext->e_filebase);
    /* FIXME XXX alignment? */

    /* Set relocation information for first section of each type.  */
    if (trynum == 0) switch (ext->e_type[0]) {
    case N_TEXT:
      sect->rel_filepos = N_TRELOFF (*execp);
      sect->reloc_count = execp->a_trsize;
      break;

    case N_DATA:
      sect->rel_filepos = N_DRELOFF (*execp);
      sect->reloc_count = execp->a_drsize;
      break;
    }
  }
no_more_sections:  

  adata(abfd).reloc_entry_size = sizeof (struct reloc_std_external);
  adata(abfd).symbol_entry_size = sizeof (struct external_nlist);
  adata(abfd).page_size = 1; /* Not applicable. */
  adata(abfd).segment_size = 1; /* Not applicable. */
  adata(abfd).exec_bytes_size = EXEC_BYTES_SIZE;

  return abfd->xvec;
}

struct bout_data_struct {
    struct aoutdata a;
    struct internal_exec e;
};

static boolean
aout_adobe_mkobject (abfd)
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

  adata(abfd).reloc_entry_size = sizeof (struct reloc_std_external);
  adata(abfd).symbol_entry_size = sizeof (struct external_nlist);
  adata(abfd).page_size = 1; /* Not applicable. */
  adata(abfd).segment_size = 1; /* Not applicable. */
  adata(abfd).exec_bytes_size = EXEC_BYTES_SIZE;

  return true;
}


static boolean
aout_adobe_write_object_contents (abfd)
     bfd *abfd;
{
  struct external_exec swapped_hdr;
  static struct external_segdesc sentinel[1] = {0};
  asection *sect;

  exec_hdr (abfd)->a_info = ZMAGIC;

  /* Calculate text size as total of text sections, etc. */

  exec_hdr (abfd)->a_text = 0;
  exec_hdr (abfd)->a_data = 0;
  exec_hdr (abfd)->a_bss  = 0;
  exec_hdr (abfd)->a_trsize = 0;
  exec_hdr (abfd)->a_drsize = 0;

  for (sect = abfd->sections; sect; sect = sect->next) {
    if (sect->flags & SEC_CODE)	{
      exec_hdr (abfd)->a_text += sect->_raw_size;
      exec_hdr (abfd)->a_trsize += sect->reloc_count *
                                   sizeof (struct reloc_std_external);
    } else if (sect->flags & SEC_DATA)	{
      exec_hdr (abfd)->a_data += sect->_raw_size;
      exec_hdr (abfd)->a_drsize += sect->reloc_count *
                                   sizeof (struct reloc_std_external);
    } else if (sect->flags & SEC_ALLOC && !(sect->flags & SEC_LOAD)) {
      exec_hdr (abfd)->a_bss  += sect->_raw_size;
    }
  }

  exec_hdr (abfd)->a_syms = bfd_get_symcount (abfd)
    				* sizeof (struct external_nlist);
  exec_hdr (abfd)->a_entry = bfd_get_start_address (abfd);

  aout_adobe_swap_exec_header_out (abfd, exec_hdr (abfd), &swapped_hdr);

  bfd_seek (abfd, (file_ptr) 0, SEEK_SET);
  bfd_write ((PTR) &swapped_hdr, 1, EXEC_BYTES_SIZE, abfd);

  /* Now write out the section information.  Text first, data next, rest
     afterward.  */

  for (sect = abfd->sections; sect; sect = sect->next) {
    if (sect->flags & SEC_CODE)	{
      aout_adobe_write_section (abfd, sect);
    }
  }
  for (sect = abfd->sections; sect; sect = sect->next) {
    if (sect->flags & SEC_DATA)	{
      aout_adobe_write_section (abfd, sect);
    }
  }
  for (sect = abfd->sections; sect; sect = sect->next) {
    if (!(sect->flags & (SEC_CODE|SEC_DATA))) {
      aout_adobe_write_section (abfd, sect);
    }
  }

  /* Write final `sentinel` section header (with type of 0).  */
  bfd_write ((PTR) sentinel, 1, sizeof (*sentinel), abfd);

  /* Now write out reloc info, followed by syms and strings */
  if (bfd_get_symcount (abfd) != 0) 
    {
      bfd_seek (abfd, (file_ptr)(N_SYMOFF(*exec_hdr(abfd))), SEEK_SET);

      aout_32_write_syms (abfd);

      bfd_seek (abfd, (file_ptr)(N_TRELOFF(*exec_hdr(abfd))), SEEK_SET);

      for (sect = abfd->sections; sect; sect = sect->next) {
        if (sect->flags & SEC_CODE)	{
          if (!aout_32_squirt_out_relocs (abfd, sect))
            return false;
        }
      }

      bfd_seek (abfd, (file_ptr)(N_DRELOFF(*exec_hdr(abfd))), SEEK_SET);

      for (sect = abfd->sections; sect; sect = sect->next) {
        if (sect->flags & SEC_DATA)	{
          if (!aout_32_squirt_out_relocs (abfd, sect))
            return false;
        }
      }
    }
  return true;
}

static void
aout_adobe_write_section (abfd, sect)
     bfd *abfd;
     sec_ptr sect;
{
  /* FIXME XXX */
}

static boolean
aout_adobe_set_section_contents (abfd, section, location, offset, count)
     bfd *abfd;
     sec_ptr section;
     unsigned char *location;
     file_ptr offset;
      int count;
{
  file_ptr section_start;
  sec_ptr sect;

  if (abfd->output_has_begun == false) { /* set by bfd.c handler */

    /* Assign file offsets to sections.  Text sections are first, and
       are contiguous.  Then data sections.  Everything else at the end.  */

    section_start = N_TXTOFF (ignore<-->me);

    for (sect = abfd->sections; sect; sect = sect->next) {
      if (sect->flags & SEC_CODE)	{
        sect->filepos = section_start;
	/* FIXME:  Round to alignment */
	section_start += sect->_raw_size;
      }
    }

    for (sect = abfd->sections; sect; sect = sect->next) {
      if (sect->flags & SEC_DATA)	{
        sect->filepos = section_start;
	/* FIXME:  Round to alignment */
	section_start += sect->_raw_size;
      }
    }

    for (sect = abfd->sections; sect; sect = sect->next) {
      if (sect->flags & SEC_HAS_CONTENTS &&
	  !(sect->flags & (SEC_CODE|SEC_DATA)))	{
        sect->filepos = section_start;
	/* FIXME:  Round to alignment */
	section_start += sect->_raw_size;
      }
    }
  }

  /* regardless, once we know what we're doing, we might as well get going */
  bfd_seek (abfd, section->filepos + offset, SEEK_SET);

  if (count != 0) {
    return (bfd_write ((PTR)location, 1, count, abfd) == count) ?true:false;
  }
  return true;
}

static boolean
aout_adobe_set_arch_mach (abfd, arch, machine)
     bfd *abfd;
     enum bfd_architecture arch;
     unsigned long machine;
{
  bfd_default_set_arch_mach(abfd, arch, machine);

  if (arch == bfd_arch_unknown)	/* Unknown machine arch is OK */
    return true;

  return false;
}

static int 
DEFUN(aout_adobe_sizeof_headers,(ignore_abfd, ignore),
      bfd *ignore_abfd AND
      boolean ignore)
{
  return sizeof(struct internal_exec);
}




/* Build the transfer vector for Adobe A.Out files.  */

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
#define	aout_32_set_section_contents	aout_adobe_set_section_contents
#define	aout_32_set_arch_mach		aout_adobe_set_arch_mach
#define	aout_32_sizeof_headers		aout_adobe_sizeof_headers

#define aout_32_bfd_debug_info_start		bfd_void
#define aout_32_bfd_debug_info_end		bfd_void
#define aout_32_bfd_debug_info_accumulate	(PROTO(void,(*),(bfd*, struct sec *))) bfd_void

#define aout_32_bfd_get_relocated_section_contents  bfd_generic_get_relocated_section_contents
#define aout_32_bfd_relax_section                   bfd_generic_relax_section

bfd_target a_out_adobe_vec =
{
  "a.out.adobe",		/* name */
  bfd_target_aout_flavour,
  true,				/* data byte order is unknown (big assumed) */
  true,				/* hdr byte order is big */
  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT ),
  /* section flags */
  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_CODE | SEC_DATA | SEC_RELOC),
  '_',				/*  symbol leading char */
  ' ',				/* ar_pad_char */
  16,				/* ar_max_namelen */
  2,				/* minumum alignment power */

  _do_getb64, _do_putb64,  _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* data */
  _do_getb64, _do_putb64,  _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* hdrs */
 {_bfd_dummy_target, aout_adobe_object_p, /* bfd_check_format */
   bfd_generic_archive_p, _bfd_dummy_target},
 {bfd_false, aout_adobe_mkobject, /* bfd_set_format */
   _bfd_generic_mkarchive, bfd_false},
 {bfd_false, aout_adobe_write_object_contents, /* bfd_write_contents */
   _bfd_write_archive_contents, bfd_false},

  JUMP_TABLE(aout_32)
 };

