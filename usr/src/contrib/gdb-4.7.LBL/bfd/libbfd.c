/* Assorted BFD support routines, only used internally.
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

/*
SECTION
	libbfd

DESCRIPTION
	This file contains various routines which are used within BFD.
	They are not intended for export, but are documented here for
	completeness.
*/

boolean
DEFUN(_bfd_dummy_new_section_hook,(ignore, ignore_newsect),
      bfd *ignore AND
      asection *ignore_newsect)
{
  return true;
}

boolean
DEFUN(bfd_false ,(ignore),
      bfd *ignore)
{
  return false;
}

boolean
DEFUN(bfd_true,(ignore),
      bfd *ignore)
{
  return true;
}

PTR
DEFUN(bfd_nullvoidptr,(ignore),
      bfd *ignore)
{
  return (PTR)NULL;
}

int 
DEFUN(bfd_0,(ignore),
      bfd *ignore)
{
  return 0;
}

unsigned int 
DEFUN(bfd_0u,(ignore),
      bfd *ignore)
{
   return 0;
}

void 
DEFUN(bfd_void,(ignore),
      bfd *ignore)
{
}

boolean
DEFUN(_bfd_dummy_core_file_matches_executable_p,(ignore_core_bfd, ignore_exec_bfd),
      bfd *ignore_core_bfd AND
      bfd *ignore_exec_bfd)
{
  bfd_error = invalid_operation;
  return false;
}

/* of course you can't initialize a function to be the same as another, grr */

char *
DEFUN(_bfd_dummy_core_file_failing_command,(ignore_abfd),
      bfd *ignore_abfd)
{
  return (char *)NULL;
}

int
DEFUN(_bfd_dummy_core_file_failing_signal,(ignore_abfd),
     bfd *ignore_abfd)
{
  return 0;
}

bfd_target *
DEFUN(_bfd_dummy_target,(ignore_abfd),
     bfd *ignore_abfd)
{
  return 0;
}

/** zalloc -- allocate and clear storage */


#ifndef zalloc
char *
DEFUN(zalloc,(size),
      bfd_size_type size)
{
  char *ptr = (char *) malloc ((int)size);

  if ((ptr != NULL) && (size != 0))
   memset(ptr,0, size);

  return ptr;
}
#endif

/*
INTERNAL_FUNCTION
	bfd_xmalloc

SYNOPSIS
	PTR  bfd_xmalloc( bfd_size_type size);

DESCRIPTION
	Like malloc, but exit if no more memory.

*/

/** There is major inconsistency in how running out of memory is handled.
  Some routines return a NULL, and set bfd_error to no_memory.
  However, obstack routines can't do this ... */


DEFUN(PTR bfd_xmalloc,(size),
      bfd_size_type size)
{
  static CONST char no_memory_message[] = "Virtual memory exhausted!\n";
  PTR ptr;
  if (size == 0) size = 1;
  ptr = (PTR)malloc(size);
  if (!ptr)
    {
      write (2, no_memory_message, sizeof(no_memory_message)-1);
      exit (-1);
    }
  return ptr;
}

/* Some IO code */


/* Note that archive entries don't have streams; they share their parent's.
   This allows someone to play with the iostream behind BFD's back.

   Also, note that the origin pointer points to the beginning of a file's
   contents (0 for non-archive elements).  For archive entries this is the
   first octet in the file, NOT the beginning of the archive header. */

static 
int DEFUN(real_read,(where, a,b, file),
          PTR where AND
          int a AND
          int b AND
          FILE *file)
{
  return fread(where, a,b,file);
}
bfd_size_type
DEFUN(bfd_read,(ptr, size, nitems, abfd),
      PTR ptr AND
      bfd_size_type size AND
      bfd_size_type nitems AND
      bfd *abfd)
{
  return (bfd_size_type)real_read (ptr, 1, (int)(size*nitems), bfd_cache_lookup(abfd));
}

bfd_size_type
DEFUN(bfd_write,(ptr, size, nitems, abfd),
      CONST PTR ptr AND
      bfd_size_type size AND
      bfd_size_type nitems AND
      bfd *abfd)
{
  return fwrite (ptr, 1, (int)(size*nitems), bfd_cache_lookup(abfd));
}

/*
INTERNAL_FUNCTION
	bfd_write_bigendian_4byte_int

SYNOPSIS
	void bfd_write_bigendian_4byte_int(bfd *abfd,  int i);

DESCRIPTION
	Writes a 4 byte integer to the outputing bfd, in big endian
	mode regardless of what else is going on.  This is usefull in
	archives.

*/
void
DEFUN(bfd_write_bigendian_4byte_int,(abfd, i),
      bfd *abfd AND
      int i)
{
  bfd_byte buffer[4];
  _do_putb32(i, buffer);
  bfd_write((PTR)buffer, 4, 1, abfd);
}

int
DEFUN(bfd_seek,(abfd, position, direction),
      bfd * CONST abfd AND
      CONST file_ptr position AND
      CONST int direction)
{
        /* For the time being, a BFD may not seek to it's end.  The
           problem is that we don't easily have a way to recognize
           the end of an element in an archive. */

        BFD_ASSERT(direction == SEEK_SET
                   || direction == SEEK_CUR);
        
        if (direction == SEEK_SET && abfd->my_archive != NULL) 
            {
                    /* This is a set within an archive, so we need to
                       add the base of the object within the archive */
                    return(fseek(bfd_cache_lookup(abfd),
                                 position + abfd->origin,
                                 direction));
            }
        else 
            {
                    return(fseek(bfd_cache_lookup(abfd),  position, direction));
            }   
}

long
DEFUN(bfd_tell,(abfd),
      bfd *abfd)
{
        file_ptr ptr;

        ptr = ftell (bfd_cache_lookup(abfd));

        if (abfd->my_archive)
            ptr -= abfd->origin;
        return ptr;
}

/** Make a string table */

/*>bfd.h<
 Add string to table pointed to by table, at location starting with free_ptr.
   resizes the table if necessary (if it's NULL, creates it, ignoring
   table_length).  Updates free_ptr, table, table_length */

boolean
DEFUN(bfd_add_to_string_table,(table, new_string, table_length, free_ptr),
      char **table AND
      char *new_string AND
      unsigned int *table_length AND
      char **free_ptr)
{
  size_t string_length = strlen (new_string) + 1; /* include null here */
  char *base = *table;
  size_t space_length = *table_length;
  unsigned int offset = (base ? *free_ptr - base : 0);

  if (base == NULL) {
    /* Avoid a useless regrow if we can (but of course we still
       take it next time */
    space_length = (string_length < DEFAULT_STRING_SPACE_SIZE ?
                    DEFAULT_STRING_SPACE_SIZE : string_length+1);
    base = zalloc (space_length);

    if (base == NULL) {
      bfd_error = no_memory;
      return false;
    }
  }

  if ((size_t)(offset + string_length) >= space_length) {
    /* Make sure we will have enough space */
    while ((size_t)(offset + string_length) >= space_length) 
      space_length += space_length/2; /* grow by 50% */

    base = (char *) realloc (base, space_length);
    if (base == NULL) {
      bfd_error = no_memory;
      return false;
    }

  }

  memcpy (base + offset, new_string, string_length);
  *table = base;
  *table_length = space_length;
  *free_ptr = base + offset + string_length;
  
  return true;
}

/** The do-it-yourself (byte) sex-change kit */

/* The middle letter e.g. get<b>short indicates Big or Little endian
   target machine.  It doesn't matter what the byte order of the host
   machine is; these routines work for either.  */

/* FIXME: Should these take a count argument?
   Answer (gnu@cygnus.com):  No, but perhaps they should be inline
                             functions in swap.h #ifdef __GNUC__. 
                             Gprof them later and find out.  */

/*
FUNCTION
	bfd_put_size
FUNCTION
	bfd_get_size

DESCRIPTION
	These macros as used for reading and writing raw data in
	sections; each access (except for bytes) is vectored through
	the target format of the BFD and mangled accordingly. The
	mangling performs any necessary endian translations and
	removes alignment restrictions. 

.#define bfd_put_8(abfd, val, ptr) \
.                (*((char *)ptr) = (char)val)
.#define bfd_get_8(abfd, ptr) \
.                (*((char *)ptr))
.#define bfd_put_16(abfd, val, ptr) \
.                BFD_SEND(abfd, bfd_putx16, (val,ptr))
.#define bfd_get_16(abfd, ptr) \
.                BFD_SEND(abfd, bfd_getx16, (ptr))
.#define bfd_put_32(abfd, val, ptr) \
.                BFD_SEND(abfd, bfd_putx32, (val,ptr))
.#define bfd_get_32(abfd, ptr) \
.                BFD_SEND(abfd, bfd_getx32, (ptr))
.#define bfd_put_64(abfd, val, ptr) \
.                BFD_SEND(abfd, bfd_putx64, (val, ptr))
.#define bfd_get_64(abfd, ptr) \
.                BFD_SEND(abfd, bfd_getx64, (ptr))

*/ 

/*
FUNCTION
	bfd_h_put_size
FUNCTION
	bfd_h_get_size

DESCRIPTION
	These macros have the same function as their <<bfd_get_x>>
	bretherin, except that they are used for removing information
	for the header records of object files. Believe it or not,
	some object files keep their header records in big endian
	order, and their data in little endan order.

.#define bfd_h_put_8(abfd, val, ptr) \
.                (*((char *)ptr) = (char)val)
.#define bfd_h_get_8(abfd, ptr) \
.                (*((char *)ptr))
.#define bfd_h_put_16(abfd, val, ptr) \
.                BFD_SEND(abfd, bfd_h_putx16,(val,ptr))
.#define bfd_h_get_16(abfd, ptr) \
.                BFD_SEND(abfd, bfd_h_getx16,(ptr))
.#define bfd_h_put_32(abfd, val, ptr) \
.                BFD_SEND(abfd, bfd_h_putx32,(val,ptr))
.#define bfd_h_get_32(abfd, ptr) \
.                BFD_SEND(abfd, bfd_h_getx32,(ptr))
.#define bfd_h_put_64(abfd, val, ptr) \
.                BFD_SEND(abfd, bfd_h_putx64,(val, ptr))
.#define bfd_h_get_64(abfd, ptr) \
.                BFD_SEND(abfd, bfd_h_getx64,(ptr))

*/ 

bfd_vma
DEFUN(_do_getb16,(addr),
      register bfd_byte *addr)
{
        return (addr[0] << 8) | addr[1];
}

bfd_vma
DEFUN(_do_getl16,(addr),
      register bfd_byte *addr)
{
        return (addr[1] << 8) | addr[0];
}

void
DEFUN(_do_putb16,(data, addr),
      bfd_vma data AND
      register bfd_byte *addr)
{
        addr[0] = (bfd_byte)(data >> 8);
        addr[1] = (bfd_byte )data;
}

void
DEFUN(_do_putl16,(data, addr),
      bfd_vma data AND              
      register bfd_byte *addr)
{
        addr[0] = (bfd_byte )data;
        addr[1] = (bfd_byte)(data >> 8);
}

bfd_vma
DEFUN(_do_getb32,(addr),
      register bfd_byte *addr)
{
        return ((((addr[0] << 8) | addr[1]) << 8) | addr[2]) << 8 | addr[3];
}

bfd_vma
_do_getl32 (addr)
        register bfd_byte *addr;
{
        return ((((addr[3] << 8) | addr[2]) << 8) | addr[1]) << 8 | addr[0];
}

bfd_vma
DEFUN(_do_getb64,(addr),
      register bfd_byte *addr)
{
#ifdef HOST_64_BIT
  bfd_64_type low, high;

  high= ((((((((addr[0]) << 8) |
              addr[1]) << 8) |
            addr[2]) << 8) |
          addr[3]) );

  low = ((((((((addr[4]) << 8) |
              addr[5]) << 8) |
            addr[6]) << 8) |
          addr[7]));

  return high << 32 | low;
#else
  BFD_FAIL();
  return 0;
#endif

}

bfd_vma
DEFUN(_do_getl64,(addr),
      register bfd_byte *addr)
{

#ifdef HOST_64_BIT
  bfd_64_type low, high;
  high= (((((((addr[7] << 8) |
              addr[6]) << 8) |
            addr[5]) << 8) |
          addr[4]));

  low = (((((((addr[3] << 8) |
              addr[2]) << 8) |
            addr[1]) << 8) |
          addr[0]) );

  return high << 32 | low;
#else
  BFD_FAIL();
  return 0;
#endif

}

void
DEFUN(_do_putb32,(data, addr),
      bfd_vma data AND
      register bfd_byte *addr)
{
        addr[0] = (bfd_byte)(data >> 24);
        addr[1] = (bfd_byte)(data >> 16);
        addr[2] = (bfd_byte)(data >>  8);
        addr[3] = (bfd_byte)data;
}

void
DEFUN(_do_putl32,(data, addr),
      bfd_vma data AND
      register bfd_byte *addr)
{
        addr[0] = (bfd_byte)data;
        addr[1] = (bfd_byte)(data >>  8);
        addr[2] = (bfd_byte)(data >> 16);
        addr[3] = (bfd_byte)(data >> 24);
}
void
DEFUN(_do_putb64,(data, addr),
        bfd_vma data AND
        register bfd_byte *addr)
{
#ifdef HOST_64_BIT
  addr[0] = (bfd_byte)(data >> (7*8));
  addr[1] = (bfd_byte)(data >> (6*8));
  addr[2] = (bfd_byte)(data >> (5*8));
  addr[3] = (bfd_byte)(data >> (4*8));
  addr[4] = (bfd_byte)(data >> (3*8));
  addr[5] = (bfd_byte)(data >> (2*8));
  addr[6] = (bfd_byte)(data >> (1*8));
  addr[7] = (bfd_byte)(data >> (0*8));
#else
  BFD_FAIL();
#endif

}

void
DEFUN(_do_putl64,(data, addr),
      bfd_vma data AND
      register bfd_byte *addr)
{
#ifdef HOST_64_BIT
  addr[7] = (bfd_byte)(data >> (7*8));
  addr[6] = (bfd_byte)(data >> (6*8));
  addr[5] = (bfd_byte)(data >> (5*8));
  addr[4] = (bfd_byte)(data >> (4*8));
  addr[3] = (bfd_byte)(data >> (3*8));
  addr[2] = (bfd_byte)(data >> (2*8));
  addr[1] = (bfd_byte)(data >> (1*8));
  addr[0] = (bfd_byte)(data >> (0*8));
#else
  BFD_FAIL();
#endif

}


/* Default implementation */

boolean
DEFUN(bfd_generic_get_section_contents, (abfd, section, location, offset, count),
      bfd *abfd AND
      sec_ptr section AND
      PTR location AND
      file_ptr offset AND
      bfd_size_type count)
{
    if (count == 0)
        return true;
    if ((bfd_size_type)(offset+count) > section->_raw_size
        || bfd_seek(abfd, (file_ptr)(section->filepos + offset), SEEK_SET) == -1
        || bfd_read(location, (bfd_size_type)1, count, abfd) != count)
        return (false); /* on error */
    return (true);
}

/* This generic function can only be used in implementations where creating
   NEW sections is disallowed.  It is useful in patching existing sections
   in read-write files, though.  See other set_section_contents functions
   to see why it doesn't work for new sections.  */
boolean
DEFUN(bfd_generic_set_section_contents, (abfd, section, location, offset, count),
      bfd *abfd AND
      sec_ptr section AND
      PTR location AND
      file_ptr offset AND
      bfd_size_type count)
{
    if (count == 0)
        return true;
    if ((bfd_size_type)(offset+count) > bfd_get_section_size_after_reloc(section)
        || bfd_seek(abfd, (file_ptr)(section->filepos + offset), SEEK_SET) == -1
        || bfd_write(location, (bfd_size_type)1, count, abfd) != count)
        return (false); /* on error */
    return (true);
}

/*
INTERNAL_FUNCTION
	bfd_log2

DESCRIPTION
	Return the log base 2 of the value supplied, rounded up. eg an
	arg of 1025 would return 11.

SYNOPSIS
	bfd_vma bfd_log2(bfd_vma x);
*/

bfd_vma bfd_log2(x)
bfd_vma x;
{
  bfd_vma  result = 0;
  while ( (bfd_vma)(1<< result) < x)
    result++;
  return result;
}
