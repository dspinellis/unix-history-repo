/* BFD back-end for IBM RS/6000 "XCOFF" files.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Written by Metin G. Ozisik, Mimi Phûông-Thåo Võ, and John Gilmore.
   Archive support from Damon A. Permezel.
   Contributed by IBM Corporation and Cygnus Support.

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

/* This port currently only handles reading object files, except when
   compiled on an RS/6000 host.  -- no archive support, no core files.
   In all cases, it does not support writing.

   FIXMEmgo comments are left from Metin Ozisik's original port.  */

/* Internalcoff.h and coffcode.h modify themselves based on this flag.  */
#define RS6000COFF_C 1

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "obstack.h"
#include "coff/internal.h"
#include "coff/rs6000.h"
#include "libcoff.h"

/* The main body of code is in coffcode.h.  */

/* Can't read rs6000 relocs */
#define RTYPE2HOWTO(a,b)
#include "coffcode.h"

#define	coff_archive_p		bfd_generic_archive_p
#define	coff_mkarchive		_bfd_generic_mkarchive

#ifdef ARCHIVES_PLEASE

/* ------------------------------------------------------------------------ */
/*	Support for archive file stuff..				    */
/*	Stolen from Damon A. Permezel's `bfd' portation.		    */
/* ------------------------------------------------------------------------ */

#undef	coff_openr_next_archived_file
#define coff_openr_next_archived_file	rs6000coff_openr_next_archived_file

#undef	coff_write_armap
#define coff_write_armap		rs6000coff_write_armap

#undef	coff_stat_arch_elt
#define	coff_stat_arch_elt		rs6000coff_stat_arch_elt

#undef	coff_snarf_ar_hdr
#define	coff_snarf_ar_hdr		rs6000coff_snarf_ar_hdr

#undef	coff_mkarchive
#define	coff_mkarchive			rs6000coff_mkarchive

#undef	coff_archive_p
#define	coff_archive_p			rs6000coff_archive_p

#include "/usr/include/ar.h"		/* <ar.h> doesn't do it.	*/


#define arch_hdr(bfd) 		\
	((struct ar_hdr *)	\
	 (((struct areltdata *)((bfd)->arelt_data))->arch_header))


static boolean
rs6000coff_mkarchive (abfd)
     bfd *abfd;
{
	bfd_error = invalid_operation;	/* write not supported	*/
}


/* This functions reads an arch header and returns an areltdata pointer, or
   NULL on error.

   Presumes the file pointer is already in the right place (ie pointing
   to the ar_hdr in the file).   Moves the file pointer; on success it
   should be pointing to the front of the file contents; on failure it
   could have been moved arbitrarily.
*/

struct areltdata *
rs6000coff_snarf_ar_hdr (abfd)
     bfd *abfd;
{
	extern int errno;

	struct {
		struct ar_hdr hdr;
		char namebuf[256];
	} h;
	int size;
	struct areltdata *ared;
	unsigned int namelen = 0;
	char *allocptr;

	size = sizeof (h.hdr);
	if (bfd_read(&h.hdr, 1, size, abfd) != size) {
		bfd_error = no_more_archived_files;
		return NULL;
	}
	size  = atoi(h.hdr.ar_namlen);	/* ar_name[] length	*/
	size += size & 1;

	if (bfd_read(&h.hdr._ar_name.ar_name[2], 1, size, abfd) != size) {
		bfd_error = no_more_archived_files;
		return NULL;
	}

	if (strncmp(h.hdr._ar_name.ar_fmag + size, AIAFMAG, 2)) {
		bfd_error = malformed_archive;
		return NULL;
	}

	h.hdr._ar_name.ar_name[size] = 0;	/* terminate filename	*/

	/*
	 * if the filename is NULL, we're (probably) at the end.
	 */
	if (size == 0) {
		bfd_error = no_more_archived_files;
		return NULL;
	}

	size += sizeof (h.hdr);
	allocptr = bfd_zalloc(abfd, sizeof (*ared) + size);

	if (allocptr == NULL) {
		bfd_error = no_memory;
		return NULL;
	}

	ared = (struct areltdata *) allocptr;

	ared->arch_header = (void *) (allocptr + sizeof (struct areltdata));
	memcpy ((char *) ared->arch_header, &h.hdr, size);
	ared->parsed_size = atoi(h.hdr.ar_size);
	ared->filename    = ((AR_HDR*) ared->arch_header)->_ar_name.ar_name;

	return ared;
}

/* Stolen directly from archive.c, except it calls rs6000coff_snarf_ar_hdr.
   Why wasn't this part of the transfer vector?  */

bfd *
rs6000coff_get_elt_at_filepos (archive, filepos)
     bfd *archive;
     file_ptr filepos;
{
  struct areltdata *new_areldata;
  bfd *n_nfd;

  n_nfd = look_for_bfd_in_cache (archive, filepos);
  if (n_nfd) return n_nfd;

  if (0 != bfd_seek (archive, filepos, SEEK_SET)) {
    bfd_error = system_call_error;
    return NULL;
  }

  if ((new_areldata = rs6000coff_snarf_ar_hdr (archive)) == NULL) return NULL;
  
  n_nfd = _bfd_create_empty_archive_element_shell (archive);
  if (n_nfd == NULL) {
    bfd_release (archive, (PTR)new_areldata);
    return NULL;
  }
  n_nfd->origin = bfd_tell (archive);
  n_nfd->arelt_data = (PTR) new_areldata;
  n_nfd->filename = new_areldata->filename;

  if (add_bfd_to_cache (archive, filepos, n_nfd))
    return n_nfd;

  /* huh? */
  bfd_release (archive, (PTR)n_nfd);
  bfd_release (archive, (PTR)new_areldata);
  return NULL;
}

/*
 * xcoff_openr_next_archived_file -	xcoff has nxt/prv seek addrs.
 */
static bfd *
rs6000coff_openr_next_archived_file(archive, last_file)
  bfd *archive, *last_file; 
{
	file_ptr filestart;

	if (!last_file)
		filestart = bfd_ardata(archive)->first_file_filepos;
	else
		filestart = atol(arch_hdr(last_file)->ar_nxtmem);

	return rs6000coff_get_elt_at_filepos (archive, filestart);
}


static bfd_target *
rs6000coff_archive_p (abfd)
     bfd *abfd;
{
	struct fl_hdr hdr;
	register struct artdata *art;

	if (bfd_read (&hdr, sizeof (hdr), 1, abfd) != sizeof (hdr)) {
		bfd_error = wrong_format;
		return 0;
	}

	if (strncmp(hdr.fl_magic, AIAMAG, SAIAMAG)) {
		bfd_error = wrong_format;
		return 0;
	}

	/*
	 * bfd_ardata() accesses the bfd->tdata field.
	 */
	abfd->tdata.aout_ar_data =
	  (void *) bfd_zalloc(abfd, sizeof (*art) + sizeof (hdr));
	if ((art = bfd_ardata (abfd)) == NULL) {
		bfd_error = no_memory;
		return 0;
	}

	art->first_file_filepos = atoi(hdr.fl_fstmoff);
	*(struct fl_hdr *) (1 + art) = hdr;

	/* Someday...
	 * slurp in the member table, which I think is the armap equivalent.
	xcoff_slurp_armap(abfd);
	 */
  
	return abfd->xvec;
}


static int
rs6000coff_stat_arch_elt(abfd, buf)
  bfd *abfd;
  struct stat *buf;
{
	struct ar_hdr *hdr;
	char *aloser;
  
	if (abfd->arelt_data == NULL) {
		bfd_error = invalid_operation;
		return -1;
	}
    
	hdr = arch_hdr (abfd);

#define foo(arelt, stelt, size)  \
	buf->stelt = strtol (hdr->arelt, &aloser, size); \
		if (aloser == hdr->arelt) return -1;
  
	foo (ar_date, st_mtime, 10);
	foo (ar_uid, st_uid, 10);
	foo (ar_gid, st_gid, 10);
	foo (ar_mode, st_mode, 8);
	foo (ar_size, st_size, 10);
#undef foo

	return 0;
}

static boolean
rs6000coff_write_armap (arch, elength, map, orl_count, stridx)
  bfd *arch;
  unsigned int elength;
  struct orl *map; 
{
	bfd_error = invalid_operation;
	return false;
}
#endif	/* ARCHIVES_PLEASE */


#ifdef COREFILES_PLEASE
extern bfd_target * rs6000coff_core_p ();
extern boolean rs6000coff_get_section_contents ();
extern boolean rs6000coff_core_file_matches_executable_p ();

#undef	coff_core_file_matches_executable_p
#define coff_core_file_matches_executable_p  \
				     rs6000coff_core_file_matches_executable_p

#undef	coff_get_section_contents
#define	coff_get_section_contents	rs6000coff_get_section_contents
#endif

/* The transfer vector that leads the outside world to all of the above. */

bfd_target rs6000coff_vec =
{
  "aixcoff-rs6000",		/* name */
  bfd_target_coff_flavour,	
  true,				/* data byte order is big */
  true,				/* header byte order is big */

  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT),

  (SEC_HAS_CONTENTS | SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
  0,				/* leading char */
  '/',				/* ar_pad_char */
  15,				/* ar_max_namelen??? FIXMEmgo */
  3,				/* default alignment power */

  _do_getb64, _do_putb64, _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* data */
  _do_getb64, _do_putb64, _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* hdrs */

  {_bfd_dummy_target, coff_object_p, 	/* bfd_check_format */
     coff_archive_p,
#ifdef	COREFILES_PLEASE
     rs6000coff_core_p
#else
     _bfd_dummy_target
#endif
       },
  {bfd_false, coff_mkobject, coff_mkarchive, /* bfd_set_format */
     bfd_false},
  {bfd_false, coff_write_object_contents,	/* bfd_write_contents */
     _bfd_write_archive_contents, bfd_false},

  JUMP_TABLE(coff),
  COFF_SWAP_TABLE
};
