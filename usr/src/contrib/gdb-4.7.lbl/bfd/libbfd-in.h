/* libbfd.h -- Declarations used by bfd library *implementation*.
   (This include file is not for users of the library.)
   Copyright 1990, 1991 Free Software Foundation, Inc.
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


/* Align an address upward to a boundary, expressed as a number of bytes.
   E.g. align to an 8-byte boundary with argument of 8.  */
#define BFD_ALIGN(this, boundary) \
  ((( (this) + ((boundary) -1)) & (~((boundary)-1))))

/* If you want to read and write large blocks, you might want to do it
   in quanta of this amount */
#define DEFAULT_BUFFERSIZE 8192

/* Set a tdata field.  Can't use the other macros for this, since they
   do casts, and casting to the left of assignment isn't portable.  */
#define set_tdata(bfd, v) ((bfd)->tdata.any = (PTR) (v))

/* tdata for an archive.  For an input archive, cache
   needs to be free()'d.  For an output archive, symdefs do.  */

struct artdata {
  file_ptr first_file_filepos;
  /* Speed up searching the armap */
  struct ar_cache *cache;
  bfd *archive_head;            /* Only interesting in output routines */
  carsym *symdefs;		/* the symdef entries */
  symindex symdef_count;             /* how many there are */
  char *extended_names;		/* clever intel extension */
};

#define bfd_ardata(bfd) ((bfd)->tdata.aout_ar_data)

/* Goes in bfd's arelt_data slot */
struct areltdata {
  char * arch_header;			     /* it's actually a string */
  unsigned int parsed_size;     /* octets of filesize not including ar_hdr */
  char *filename;			     /* null-terminated */
};

#define arelt_size(bfd) (((struct areltdata *)((bfd)->arelt_data))->parsed_size)

/* FIXME -- a lot of my code allocates a large block and subdivides it.
   This can't always work, because of alignment restrictions.  We should change
   it before it becomes a problem -- Gumby */

PROTO (char *, zalloc, (bfd_size_type size));

/* These routines allocate and free things on the BFD's obstack.  Note
   that realloc can never occur in place.  */

PROTO(PTR, bfd_alloc, (bfd *abfd, bfd_size_type size));
PROTO(PTR, bfd_zalloc,(bfd *abfd, bfd_size_type size));
PROTO(PTR, bfd_realloc,(bfd *abfd, PTR orig, bfd_size_type new));
PROTO(void, bfd_alloc_grow,(bfd *abfd, PTR thing, bfd_size_type size));
PROTO(PTR, bfd_alloc_finish,(bfd *abfd));
PROTO(PTR, bfd_alloc_by_size_t,(bfd *abfd, size_t wanted));

#define bfd_release(x,y) (void) obstack_free(&(x->memory),y)


PROTO (bfd_size_type, bfd_read, (PTR ptr, bfd_size_type size, bfd_size_type nitems, bfd *abfd));
PROTO (bfd_size_type, bfd_write, (CONST PTR ptr, bfd_size_type size, bfd_size_type nitems, bfd *abfd));



PROTO (int, bfd_seek,(bfd* CONST abfd, CONST file_ptr fp , CONST int direction));
PROTO (long, bfd_tell, (bfd *abfd));
PROTO (bfd *, _bfd_create_empty_archive_element_shell, (bfd *obfd));
PROTO (bfd *, look_for_bfd_in_cache, (bfd *arch_bfd, file_ptr index));
PROTO (boolean, _bfd_generic_mkarchive, (bfd *abfd));
PROTO (struct areltdata *, snarf_ar_hdr, (bfd *abfd));
PROTO (bfd_target *, bfd_generic_archive_p, (bfd *abfd));
PROTO (boolean, bfd_slurp_bsd_armap, (bfd *abfd));
PROTO (boolean, bfd_slurp_coff_armap, (bfd *abfd));
PROTO (boolean, _bfd_slurp_extended_name_table, (bfd *abfd));
PROTO (boolean, _bfd_write_archive_contents, (bfd *abfd));
PROTO (bfd *, new_bfd, ());

#define DEFAULT_STRING_SPACE_SIZE 0x2000
PROTO (boolean, bfd_add_to_string_table, (char **table, char *new_string,
					  unsigned int *table_length,
					  char **free_ptr));
PROTO (bfd_vma, _do_getb64, (unsigned char *addr));     
PROTO (bfd_vma, _do_getl64, (unsigned char *addr));     
PROTO (bfd_vma, _do_getb32, (unsigned char *addr));
PROTO (bfd_vma, _do_getl32, (unsigned char *addr));
PROTO (bfd_vma, _do_getb16, (unsigned char *addr));
PROTO (bfd_vma, _do_getl16, (unsigned char *addr));
PROTO (void, _do_putb64, (bfd_vma data, unsigned char *addr));
PROTO (void, _do_putl64, (bfd_vma data, unsigned char *addr));
PROTO (void, _do_putb32, (bfd_vma data, unsigned char *addr));
PROTO (void, _do_putl32, (bfd_vma data, unsigned char *addr));
PROTO (void, _do_putb16, (bfd_vma data, unsigned char *addr));
PROTO (void, _do_putl16, (bfd_vma data, unsigned char *addr));

PROTO (boolean, bfd_false, (bfd *ignore));
PROTO (boolean, bfd_true, (bfd *ignore));
PROTO (PTR, bfd_nullvoidptr, (bfd *ignore));
PROTO (int, bfd_0, (bfd *ignore));
PROTO (unsigned int, bfd_0u, (bfd *ignore));
PROTO (void, bfd_void, (bfd *ignore));

PROTO (bfd *,new_bfd_contained_in,(bfd *));
PROTO (boolean, _bfd_dummy_new_section_hook, (bfd *ignore, asection *newsect));
PROTO (char *, _bfd_dummy_core_file_failing_command, (bfd *abfd));
PROTO (int, _bfd_dummy_core_file_failing_signal, (bfd *abfd));
PROTO (boolean, _bfd_dummy_core_file_matches_executable_p, (bfd *core_bfd,
							    bfd *exec_bfd));
PROTO (bfd_target *, _bfd_dummy_target, (bfd *abfd));

PROTO (void, bfd_dont_truncate_arname, (bfd *abfd, CONST char *filename,
					char *hdr));
PROTO (void, bfd_bsd_truncate_arname, (bfd *abfd, CONST char *filename,
					char *hdr));
PROTO (void, bfd_gnu_truncate_arname, (bfd *abfd, CONST char *filename,
					char *hdr));

PROTO (boolean, bsd_write_armap, (bfd *arch, unsigned int elength,
				  struct orl *map, unsigned int orl_count, int stridx));

PROTO (boolean, coff_write_armap, (bfd *arch, unsigned int elength,
				   struct orl *map, unsigned int orl_count, int stridx));

PROTO (bfd *, bfd_generic_openr_next_archived_file, (bfd *archive,
						     bfd *last_file));

PROTO(int, bfd_generic_stat_arch_elt, (bfd *, struct stat *));

PROTO(boolean, bfd_generic_get_section_contents,
      (bfd *abfd, sec_ptr section, PTR location, file_ptr offset,
       bfd_size_type count));

PROTO(boolean, bfd_generic_set_section_contents,
      (bfd *abfd, sec_ptr section, PTR location, file_ptr offset,
       bfd_size_type count));

/* Macros to tell if bfds are read or write enabled.

   Note that bfds open for read may be scribbled into if the fd passed
   to bfd_fdopenr is actually open both for read and write
   simultaneously.  However an output bfd will never be open for
   read.  Therefore sometimes you want to check bfd_read_p or
   !bfd_read_p, and only sometimes bfd_write_p.
*/

#define bfd_read_p(abfd) ((abfd)->direction == read_direction || (abfd)->direction == both_direction)
#define bfd_write_p(abfd) ((abfd)->direction == write_direction || (abfd)->direction == both_direction)

PROTO (void, bfd_assert,(char*,int));
#define BFD_ASSERT(x) \
{ if (!(x)) bfd_assert(__FILE__,__LINE__); }

#define BFD_FAIL() \
{ bfd_assert(__FILE__,__LINE__); }

PROTO (FILE *, bfd_cache_lookup_worker, (bfd *));

extern bfd *bfd_last_cache;
    
/* Now Steve, what's the story here? */
#ifdef lint
#define itos(x) "l"
#define stoi(x) 1
#else
#define itos(x) ((char*)(x))
#define stoi(x) ((int)(x))
#endif

/* Generic routine for close_and_cleanup is really just bfd_true.  */
#define	bfd_generic_close_and_cleanup	bfd_true

/* And more follows */

