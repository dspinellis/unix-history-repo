/* BFD back-end for archive files (libraries).
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Written by Cygnus Support.  Mostly Gumby Henkel-Wallace's fault.

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

/*
@setfilename archive-info
SECTION
	Archives

DESCRIPTION
	Archives are supported in BFD in <<archive.c>>.

	An archive (or library) is just another BFD.  It has a symbol
	table, although there's not much a user program will do with it.

	The big difference between an archive BFD and an ordinary BFD
	is that the archive doesn't have sections.  Instead it has a
	chain of BFDs considered its contents.  These BFDs can be
	manipulated just like any other.  The BFDs contained in an
	archive opened for reading will all be opened for reading; you
	may put either input or output BFDs into an archive opened for
	output; it will be handled correctly when the archive is closed.

	Use <<bfd_openr_next_archived_file>> to step through all
	the contents of an archive opened for input.  It's not
	required that you read the entire archive if you don't want
	to!  Read it until you find what you want.

	Archive contents of output BFDs are chained through the
	<<next>> pointer in a BFD.  The first one is findable through
	the <<archive_head>> slot of the archive.  Set it with
	<<set_archive_head>> (q.v.).  A given BFD may be in only one
	open output archive at a time.

	As expected, the BFD archive code is more general than the
	archive code of any given environment.  BFD archives may
	contain files of different formats (eg a.out and coff) and
	even different architectures.  You may even place archives
	recursively into archives!

	This can cause unexpected confusion, since some archive
	formats are more expressive than others.  For instance intel
	COFF archives can preserve long filenames; Sun a.out archives
	cannot.  If you move a file from the first to the second
	format and back again, the filename may be truncated.
	Likewise, different a.out environments have different
	conventions as to how they truncate filenames, whether they
	preserve directory names in filenames, etc.  When
	interoperating with native tools, be sure your files are
	homogeneous.

	Beware: most of these formats do not react well to the
	presence of spaces in filenames.  We do the best we can, but
	can't always handle this due to restrctions in the format of
	archives.  Many unix utilities are braindead in regards to
	spaces and such in filenames anyway, so this shouldn't be much
	of a restriction.
*/

/* Assumes:
   o - all archive elements start on an even boundary, newline padded;
   o - all arch headers are char *;
   o - all arch headers are the same size (across architectures).
*/

/* Some formats provide a way to cram a long filename into the short
   (16 chars) space provided by a bsd archive.  The trick is: make a
   special "file" in the front of the archive, sort of like the SYMDEF
   entry.  If the filename is too long to fit, put it in the extended
   name table, and use its index as the filename.  To prevent
   confusion prepend the index with a space.  This means you can't
   have filenames that start with a space, but then again, many unix
   utilities can't handle that anyway.

   This scheme unfortunately requires that you stand on your head in
   order to write an archive since you need to put a magic file at the
   front, and need to touch every entry to do so.  C'est la vie.
*/

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "aout/ar.h"
#include "aout/ranlib.h"
#include <errno.h>

#ifndef errno
extern int errno;
#endif

#ifdef GNU960
#define BFD_GNU960_ARMAG(abfd)	(BFD_COFF_FILE_P((abfd)) ? ARMAG : ARMAGB)
#endif

/* We keep a cache of archive filepointers to archive elements to
   speed up searching the archive by filepos.  We only add an entry to
   the cache when we actually read one.  We also don't sort the cache;
   it's generally short enough to search linearly.
   Note that the pointers here point to the front of the ar_hdr, not
   to the front of the contents!
*/
struct ar_cache {
  file_ptr ptr;
  bfd* arelt;
  struct ar_cache *next;
};

#define ar_padchar(abfd) ((abfd)->xvec->ar_pad_char)
#define ar_maxnamelen(abfd) ((abfd)->xvec->ar_max_namelen)

#define arch_hdr(bfd) ((struct ar_hdr *)   \
		       (((struct areltdata *)((bfd)->arelt_data))->arch_header))

boolean
_bfd_generic_mkarchive (abfd)
     bfd *abfd;
{
  abfd->tdata.aout_ar_data = (struct artdata *)bfd_zalloc(abfd, sizeof (struct artdata));

  if (bfd_ardata (abfd) == NULL) {
      bfd_error = no_memory;
      return false;
    }
  bfd_ardata(abfd)->cache = 0;
  return true;
}

/*
FUNCTION
	bfd_get_next_mapent

SYNOPSIS
	symindex bfd_get_next_mapent(bfd *, symindex previous, carsym ** sym);

DESCRIPTION
	This function steps through an archive's symbol table (if it
	has one).  Successively updates <<sym>> with the next symbol's
	information, returning that symbol's (internal) index into the
	symbol table.

	Supply BFD_NO_MORE_SYMBOLS as the <<previous>> entry to get
	the first one; returns BFD_NO_MORE_SYMBOLS when you're already
	got the last one.

	A <<carsym>> is a canonical archive symbol.  The only
	user-visible element is its name, a null-terminated string.
*/

symindex
DEFUN(bfd_get_next_mapent,(abfd, prev, entry),
     bfd *abfd AND
     symindex prev AND
     carsym **entry)
{
  if (!bfd_has_map (abfd)) {
    bfd_error = invalid_operation;
    return BFD_NO_MORE_SYMBOLS;
  }
  
  if (prev == BFD_NO_MORE_SYMBOLS) prev = 0;
  else if (++prev >= bfd_ardata (abfd)->symdef_count)
    return BFD_NO_MORE_SYMBOLS;

  *entry = (bfd_ardata (abfd)->symdefs + prev);
  return prev;
}

/* To be called by backends only */
bfd *
_bfd_create_empty_archive_element_shell (obfd)
     bfd *obfd;
{
  bfd *nbfd;

  nbfd = new_bfd_contained_in(obfd);
  if (nbfd == NULL) {
    bfd_error = no_memory;
    return NULL;
  }
  return nbfd;
}

/*
FUNCTION
	bfd_set_archive_head

SYNOPSIS
	boolean bfd_set_archive_head(bfd *output, bfd *new_head);

DESCRIPTION
	Used whilst processing archives. Sets the head of the chain of
	BFDs contained in an archive to @var{new_head}. 
*/

boolean
DEFUN(bfd_set_archive_head,(output_archive, new_head),
     bfd *output_archive AND 
     bfd *new_head)
{

  output_archive->archive_head = new_head;
  return true;
}

bfd *
look_for_bfd_in_cache (arch_bfd, filepos)
     bfd *arch_bfd;
     file_ptr filepos;
{
  struct ar_cache *current;

  for (current = bfd_ardata (arch_bfd)->cache; current != NULL;
       current = current->next)
    if (current->ptr == filepos) return current->arelt;

  return NULL;
}

/* Kind of stupid to call cons for each one, but we don't do too many */
boolean
add_bfd_to_cache (arch_bfd, filepos, new_elt)
     bfd *arch_bfd, *new_elt;
     file_ptr filepos;
{
  struct ar_cache *new_cache = (struct ar_cache *)
				bfd_zalloc(arch_bfd, sizeof (struct ar_cache));

  if (new_cache == NULL) {
    bfd_error = no_memory;
    return false;
  }

  new_cache->ptr = filepos;
  new_cache->arelt = new_elt;
  new_cache->next = (struct ar_cache *)NULL;
  if (bfd_ardata (arch_bfd)->cache == NULL)
    bfd_ardata (arch_bfd)->cache = new_cache;
  else {
    struct ar_cache *current = bfd_ardata (arch_bfd)->cache;

    for (; current->next != NULL; current = current->next);
    current->next = new_cache;
  }
    
  return true;
}



/* The name begins with space.  Hence the rest of the name is an index into
   the string table. */
char *
get_extended_arelt_filename (arch, name)
     bfd *arch;
     char *name;
{
  unsigned long index = 0;

  /* Should extract string so that I can guarantee not to overflow into
     the next region, but I"m too lazy. */
  errno = 0;
  index = strtol (name, NULL, 10);
  if (errno != 0) {
      bfd_error = malformed_archive;
      return NULL;
    }

  return bfd_ardata (arch)->extended_names + index;
}  

/* This functions reads an arch header and returns an areltdata pointer, or
   NULL on error.

   Presumes the file pointer is already in the right place (ie pointing
   to the ar_hdr in the file).   Moves the file pointer; on success it
   should be pointing to the front of the file contents; on failure it
   could have been moved arbitrarily.
*/

struct areltdata *
snarf_ar_hdr (abfd)
     bfd *abfd;
{
#ifndef errno
  extern int errno;
#endif

    struct ar_hdr hdr;
    char *hdrp = (char *) &hdr;
    unsigned int parsed_size;
    struct areltdata *ared;
    char *filename = NULL;
    unsigned int namelen = 0;
    unsigned int allocsize = sizeof (struct areltdata) + sizeof (struct ar_hdr);
    char *allocptr;

    if (bfd_read ((PTR)hdrp, 1, sizeof (struct ar_hdr), abfd)
	!= sizeof (struct ar_hdr)) {
	bfd_error = no_more_archived_files;
	return NULL;
    }
    if (strncmp ((hdr.ar_fmag), ARFMAG, 2)) {
	bfd_error = malformed_archive;
	return NULL;
    }

    errno = 0;
    parsed_size = strtol (hdr.ar_size, NULL, 10);
    if (errno != 0) {
	bfd_error = malformed_archive;
	return NULL;
    }

    /* extract the filename from the archive - there are two ways to
       specify an extendend name table, either the first char of the
       name is a space, or it's a slash  */
    if ((hdr.ar_name[0] == '/' || hdr.ar_name[0] == ' ') 
	&& bfd_ardata (abfd)->extended_names != NULL) {
	filename = get_extended_arelt_filename (abfd, hdr.ar_name);
	if (filename == NULL) {
	    bfd_error = malformed_archive;
	    return NULL;
	}
    } 
    else 
	{
	    /* We judge the end of the name by looking for a space or a
	       padchar */

	    namelen = 0;

	    while (namelen < (unsigned)ar_maxnamelen(abfd) &&
		   ( hdr.ar_name[namelen] != 0 &&
		    hdr.ar_name[namelen] != ' ' &&
		    hdr.ar_name[namelen] != ar_padchar(abfd))) {
		namelen++;
	    }

	    allocsize += namelen + 1;
	}

    allocptr = bfd_zalloc(abfd, allocsize);
    if (allocptr == NULL) {
	bfd_error = no_memory;
	return NULL;
    }

    ared = (struct areltdata *) allocptr;

    ared->arch_header = allocptr + sizeof (struct areltdata);
    memcpy ((char *) ared->arch_header, (char *) &hdr, sizeof (struct ar_hdr));
    ared->parsed_size = parsed_size;

    if (filename != NULL) ared->filename = filename;
    else {
	ared->filename = allocptr + (sizeof (struct areltdata) +
				     sizeof (struct ar_hdr));
	if (namelen)
	    memcpy (ared->filename, hdr.ar_name, namelen);
	ared->filename[namelen] = '\0';
    }
  
    return ared;
}

/* This is an internal function; it's mainly used when indexing
   through the archive symbol table, but also used to get the next
   element, since it handles the bookkeeping so nicely for us.
*/

bfd *
get_elt_at_filepos (archive, filepos)
     bfd *archive;
     file_ptr filepos;
{
  struct areltdata *new_areldata;
  bfd *n_nfd;

  n_nfd = look_for_bfd_in_cache (archive, filepos);
  if (n_nfd) return n_nfd;

  if (0 > bfd_seek (archive, filepos, SEEK_SET)) {
    bfd_error = system_call_error;
    return NULL;
  }

  if ((new_areldata = snarf_ar_hdr (archive)) == NULL) return NULL;
  
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
FUNCTION
	bfd_get_elt_at_index

SYNOPSIS
	bfd *bfd_get_elt_at_index(bfd * archive, int index);

DESCRIPTION
	Return the bfd which is referenced by the symbol indexed by <<index>>.
	<<index>> should have been returned by <<bfd_get_next_mapent>> (q.v.).

*/
bfd *
DEFUN(bfd_get_elt_at_index,(abfd, index),
     bfd *abfd AND
     int index)
{
  bfd *result =
    get_elt_at_filepos
      (abfd, (bfd_ardata (abfd)->symdefs + index)->file_offset);
  return result;
}

/*
FUNCTION
	bfd_openr_next_archived_file

SYNOPSIS
	bfd* bfd_openr_next_archived_file(bfd *archive, bfd *previous);

DESCRIPTION
	Initially provided a BFD containing an archive and NULL, opens
	an inpout BFD on the first contained element and returns that.
	Subsequent calls to bfd_openr_next_archived_file should pass
	the archive and the previous return value to return a created
	BFD to the next contained element. NULL is returned when there
	are no more.

*/

bfd *
DEFUN(bfd_openr_next_archived_file,(archive, last_file),
     bfd *archive AND  
      bfd*last_file)
{

    if ((bfd_get_format (archive) != bfd_archive) ||
	(archive->direction == write_direction)) {
	    bfd_error = invalid_operation;
	    return NULL;
	}


    return BFD_SEND (archive,
		     openr_next_archived_file,
		     (archive,
		      last_file));

}

bfd *bfd_generic_openr_next_archived_file(archive, last_file)
     bfd *archive;
     bfd *last_file;
{
  file_ptr filestart;

  if (!last_file)
    filestart = bfd_ardata (archive)->first_file_filepos;
  else {
    unsigned int size = arelt_size(last_file);
    /* Pad to an even boundary... */
    filestart = last_file->origin + size + size%2;
  }

  return get_elt_at_filepos (archive, filestart);
}


bfd_target *
bfd_generic_archive_p (abfd)
     bfd *abfd;
{
  char armag[SARMAG+1];

  if (bfd_read ((PTR)armag, 1, SARMAG, abfd) != SARMAG) {
    bfd_error = wrong_format;
    return 0;
  }

#ifdef GNU960
  if (strncmp (armag, BFD_GNU960_ARMAG(abfd), SARMAG)) return 0;
#else
  if (strncmp (armag, ARMAG, SARMAG) &&
      strncmp (armag, ARMAGB, SARMAG)) return 0;
#endif



  /* We are setting bfd_ardata(abfd) here, but since bfd_ardata
     involves a cast, we can't do it as the left operand of assignment. */
  abfd->tdata.aout_ar_data = (struct artdata *) bfd_zalloc(abfd,sizeof (struct artdata));

  if (bfd_ardata (abfd)  == NULL) {
    bfd_error = no_memory;
    return 0;
  }

  bfd_ardata (abfd)->first_file_filepos = SARMAG;
  
  if (!BFD_SEND (abfd, _bfd_slurp_armap, (abfd))) {
    bfd_release(abfd, bfd_ardata (abfd));
    abfd->tdata.aout_ar_data = NULL;
    return 0;
  }

  if (!BFD_SEND (abfd, _bfd_slurp_extended_name_table, (abfd))) {
    bfd_release(abfd, bfd_ardata (abfd));
    abfd->tdata.aout_ar_data = NULL;
    return 0;
  }
  
  return abfd->xvec;
}

/* Returns false on error, true otherwise */
boolean
bfd_slurp_bsd_armap (abfd)
     bfd *abfd;
{

    struct areltdata *mapdata;
    char nextname[17];
    unsigned int counter = 0;
    int *raw_armap, *rbase;
    struct artdata *ardata = bfd_ardata (abfd);
    char *stringbase;

    /* FIXME, if the read fails, this routine quietly returns "true"!!
       It should probably do that if the read gives 0 bytes (empty archive),
       but fail for any other size... */
    if (bfd_read ((PTR)nextname, 1, 16, abfd) == 16) {
	    /* The archive has at least 16 bytes in it */
	    bfd_seek (abfd, (file_ptr) -16, SEEK_CUR);

	    /* This should be using RANLIBMAG, but at least it can be grepped for
	       in this comment.  */
	    if (strncmp (nextname, "__.SYMDEF       ", 16)) {
		    bfd_has_map (abfd) = false;
		    return true;
		}

	    mapdata = snarf_ar_hdr (abfd);
	    if (mapdata == NULL) return false;

	    raw_armap = (int *) bfd_zalloc(abfd,mapdata->parsed_size);
	    if (raw_armap == NULL) {
		    bfd_error = no_memory;
		  byebye:
		    bfd_release (abfd, (PTR)mapdata);
		    return false;
		}

	    if (bfd_read ((PTR)raw_armap, 1, mapdata->parsed_size, abfd) !=
		mapdata->parsed_size) {
		    bfd_error = malformed_archive;
		  byebyebye:
		    bfd_release (abfd, (PTR)raw_armap);
		    goto byebye;
		}

	    ardata->symdef_count = bfd_h_get_32(abfd, (PTR)raw_armap) / sizeof (struct symdef);

	    if (ardata->symdef_count * sizeof (struct symdef)
		> mapdata->parsed_size - sizeof (*raw_armap)) {
	            /* Probably we're using the wrong byte ordering.  */
	            bfd_error = wrong_format;
		    goto byebyebye;
		  }

	    ardata->cache = 0;
	    rbase = raw_armap+1;
	    ardata->symdefs = (carsym *) rbase;
	    stringbase = ((char *) (ardata->symdefs + ardata->symdef_count)) + 4;

	    for (;counter < ardata->symdef_count; counter++) {
		    struct symdef *sym = ((struct symdef *) rbase) + counter;
		    sym->s.name = bfd_h_get_32(abfd, (PTR)(&(sym->s.string_offset))) + stringbase;
		    sym->file_offset = bfd_h_get_32(abfd, (PTR)( &(sym->file_offset)));
		}
  
	    ardata->first_file_filepos = bfd_tell (abfd);
	    /* Pad to an even boundary if you have to */
	    ardata->first_file_filepos += (ardata-> first_file_filepos) %2;
	    /* FIXME, we should provide some way to free raw_ardata when
	       we are done using the strings from it.  For now, it seems
	       to be allocated on an obstack anyway... */
	    bfd_has_map (abfd) = true;
	}
    return true;
}

/* Returns false on error, true otherwise */
boolean
bfd_slurp_coff_armap (abfd)
     bfd *abfd;
{
  struct areltdata *mapdata;
  char nextname;
  int *raw_armap, *rawptr;
  struct artdata *ardata = bfd_ardata (abfd);
  char *stringbase;
  unsigned int stringsize;
  carsym *carsyms;
  int result;
  bfd_vma (*swap)();
  
  result = bfd_read ((PTR)&nextname, 1, 1, abfd);
  bfd_seek (abfd, (file_ptr) -1, SEEK_CUR);

  if (result != 1 || nextname != '/') {
    /* Actually I think this is an error for a COFF archive */
    bfd_has_map (abfd) = false;
    return true;
  }

  mapdata = snarf_ar_hdr (abfd);
  if (mapdata == NULL) return false;

  raw_armap = (int *) bfd_alloc(abfd,mapdata->parsed_size);

  if (raw_armap == NULL) 
  {
    bfd_error = no_memory;
   byebye:
    bfd_release (abfd, (PTR)mapdata);
    return false;
  }

  /* read in the raw map */
  if (bfd_read ((PTR)raw_armap, 1, mapdata->parsed_size, abfd) !=
      mapdata->parsed_size) {
    bfd_error = malformed_archive;
   oops:
    bfd_release (abfd, (PTR)raw_armap);
    goto byebye;
  }

  /* The coff armap must be read sequentially.  So we construct a bsd-style
     one in core all at once, for simplicity. 
     
     It seems that all numeric information in a coff archive is always
     in big endian format, nomatter the host or target. */

  stringsize 
   = mapdata->parsed_size - (4 * (_do_getb32((PTR)raw_armap))) - 4;
  /* Except that some archive formats are broken, and it may be our
     fault - the i960 little endian coff sometimes has big and sometimes
     little, because our tools changed.  Here's a horrible hack to clean
     up the crap
     */
  swap = _do_getb32;
  
  if (stringsize > 0xfffff) 
  {
    /* This looks dangerous, let's do it the other way around */
    stringsize = mapdata->parsed_size - (4 *
					 (_do_getl32((PTR)raw_armap))) - 4;

    swap = _do_getl32;
  }
  

 {
   unsigned int nsymz = swap( (PTR)raw_armap);
   unsigned int carsym_size = (nsymz * sizeof (carsym));
   unsigned int ptrsize = (4 * nsymz);
   unsigned int i;
   ardata->symdefs = (carsym *) bfd_zalloc(abfd,carsym_size + stringsize + 1);
   if (ardata->symdefs == NULL) {
     bfd_error = no_memory;
     goto oops;
   }
   carsyms = ardata->symdefs;

   stringbase = ((char *) ardata->symdefs) + carsym_size;
   memcpy (stringbase, (char*)raw_armap + ptrsize + 4,  stringsize);


   /* OK, build the carsyms */
   for (i = 0; i < nsymz; i++) 
   {
     rawptr = raw_armap + i + 1;
     carsyms->file_offset = swap((PTR)rawptr);
     carsyms->name = stringbase;
     for (; *(stringbase++););
     carsyms++;
   }
   *stringbase = 0;
 }
  ardata->symdef_count = swap((PTR)raw_armap);
  ardata->first_file_filepos = bfd_tell (abfd);
  /* Pad to an even boundary if you have to */
  ardata->first_file_filepos += (ardata->first_file_filepos) %2;

  /* We'd like to release these allocations, but we have allocated stuff
     since then (using the same obstack, if bfd_release is obstack based).
     So they will stick around until the BFD is closed.  */
  /*  bfd_release (abfd, (PTR)raw_armap);
      bfd_release (abfd, (PTR)mapdata);  */
  bfd_has_map (abfd) = true;
  return true;
}

/** Extended name table.

  Normally archives support only 14-character filenames.

  Intel has extended the format: longer names are stored in a special
  element (the first in the archive, or second if there is an armap);
  the name in the ar_hdr is replaced by <space><index into filename
  element>.  Index is the P.R. of an int (decimal).  Data General have
  extended the format by using the prefix // for the special element */

/* Returns false on error, true otherwise */
boolean
_bfd_slurp_extended_name_table (abfd)
     bfd *abfd;
{
  char nextname[17];
  struct areltdata *namedata;

  /* FIXME:  Formatting sucks here, and in case of failure of BFD_READ,
     we probably don't want to return true.  */
  if (bfd_read ((PTR)nextname, 1, 16, abfd) == 16) {

    bfd_seek (abfd, (file_ptr) -16, SEEK_CUR);

    if (strncmp (nextname, "ARFILENAMES/    ", 16) != 0 &&
	strncmp (nextname, "//              ", 16) != 0) 
	{
      bfd_ardata (abfd)->extended_names = NULL;
      return true;
    }

    namedata = snarf_ar_hdr (abfd);
    if (namedata == NULL) return false;
  
    bfd_ardata (abfd)->extended_names = bfd_zalloc(abfd,namedata->parsed_size);
    if (bfd_ardata (abfd)->extended_names == NULL) {
      bfd_error = no_memory;
    byebye:
      bfd_release (abfd, (PTR)namedata);
      return false;
    }

    if (bfd_read ((PTR)bfd_ardata (abfd)->extended_names, 1,
		  namedata->parsed_size, abfd) != namedata->parsed_size) {
      bfd_error = malformed_archive;
      bfd_release (abfd, (PTR)(bfd_ardata (abfd)->extended_names));
      bfd_ardata (abfd)->extended_names = NULL;
      goto byebye;
    }

    /* Since the archive is supposed to be printable if it contains
       text, the entries in the list are newline-padded, not null
       padded. We'll fix that there..  */
      {
	char *temp = bfd_ardata (abfd)->extended_names;
	for (; *temp != '\0'; ++temp)
	  if (*temp == '\n') *temp = '\0';
      }
  
    /* Pad to an even boundary if you have to */
    bfd_ardata (abfd)->first_file_filepos = bfd_tell (abfd);
    bfd_ardata (abfd)->first_file_filepos +=
      (bfd_ardata (abfd)->first_file_filepos) %2;

    /* FIXME, we can't release namedata here because it was allocated
       below extended_names on the obstack... */
    /* bfd_release (abfd, namedata); */
  }
  return true;
}

#ifdef VMS

/* Return a copy of the stuff in the filename between any :]> and a
   semicolon */
static CONST char *
DEFUN(normalize,(file),
      CONST char *file)
{
  CONST char *first;
  CONST char *last;
  char *copy;

  first = file + strlen(file)-1;
  last = first+1;

  while (first != file) 
  {
    if (*first == ';') 
     last = first;
    if (*first == ':' || *first == ']' ||*first == '>') 
    { 
      first++;
      break;
    }
    first --;
  }
  

  copy = malloc(last - first + 1);
  memcpy(copy, first, last-first);
  copy[last-first] = 0;

  return copy;
}

#else
static
CONST char *normalize(file)
CONST char *file;
{
  CONST char *    filename = strrchr(file, '/');

  if (filename != (char *)NULL) {
      filename ++;
    }
  else {
      filename = file;
    }
  return filename;
}
#endif
/* Follows archive_head and produces an extended name table if necessary.
   Returns (in tabloc) a pointer to an extended name table, and in tablen
   the length of the table.  If it makes an entry it clobbers the filename
   so that the element may be written without further massage.
   Returns true if it ran successfully, false if something went wrong.
   A successful return may still involve a zero-length tablen!
   */
boolean
bfd_construct_extended_name_table (abfd, tabloc, tablen)
     bfd *abfd;
     char **tabloc;
     unsigned int *tablen;
{
  unsigned int maxname = abfd->xvec->ar_max_namelen;
  unsigned int total_namelen = 0;
  bfd *current;
  char *strptr;

  *tablen = 0;
  
  /* Figure out how long the table should be */
  for (current = abfd->archive_head; current != NULL; current = current->next){
    unsigned int thislen = strlen (normalize(current->filename));
    if (thislen > maxname) total_namelen += thislen + 1; /* leave room for \n */
  }

  if (total_namelen == 0) return true;

  *tabloc = bfd_zalloc (abfd,total_namelen);
  if (*tabloc == NULL) {
    bfd_error = no_memory;
    return false;
  }

  *tablen = total_namelen;
  strptr = *tabloc;

  for (current = abfd->archive_head; current != NULL; current =
       current->next) {
    CONST char *normal =normalize( current->filename);
    unsigned int thislen = strlen (normal);
    if (thislen > maxname) {
      /* Works for now; may need to be re-engineered if we encounter an oddball
	 archive format and want to generalise this hack. */
      struct ar_hdr *hdr = arch_hdr(current);
      strcpy (strptr, normal);
      strptr[thislen] = '\n';
      hdr->ar_name[0] = ' ';
      /* We know there will always be enough room (one of the few cases
	 where you may safely use sprintf). */
      sprintf ((hdr->ar_name) + 1, "%-d", (unsigned) (strptr - *tabloc));
      /* Kinda Kludgy.   We should just use the returned value of sprintf
	 but not all implementations get this right */
	{
	  char *temp = hdr->ar_name +2; 
	  for (; temp < hdr->ar_name + maxname; temp++)
	    if (*temp == '\0') *temp = ' ';
	}
      strptr += thislen + 1;
    }
  }

  return true;
}

/** A couple of functions for creating ar_hdrs */

/* Takes a filename, returns an arelt_data for it, or NULL if it can't make one.
   The filename must refer to a filename in the filesystem.
   The filename field of the ar_hdr will NOT be initialized
*/

struct areltdata *
DEFUN(bfd_ar_hdr_from_filesystem, (abfd,filename),
      bfd* abfd AND
      CONST char *filename)
{
  struct stat status;
  struct areltdata *ared;
  struct ar_hdr *hdr;
  char *temp, *temp1;


  if (stat (filename, &status) != 0) {
    bfd_error = system_call_error;
    return NULL;
  }

  ared = (struct areltdata *) bfd_zalloc(abfd, sizeof (struct ar_hdr) +
				      sizeof (struct areltdata));
  if (ared == NULL) {
    bfd_error = no_memory;
    return NULL;
  }
  hdr = (struct ar_hdr *) (((char *) ared) + sizeof (struct areltdata));

  /* ar headers are space padded, not null padded! */
  temp = (char *) hdr;
  temp1 = temp + sizeof (struct ar_hdr) - 2;
  for (; temp < temp1; *(temp++) = ' ');
  strncpy (hdr->ar_fmag, ARFMAG, 2);
  
  /* Goddamned sprintf doesn't permit MAXIMUM field lengths */
  sprintf ((hdr->ar_date), "%-12ld", status.st_mtime);
  sprintf ((hdr->ar_uid), "%d", status.st_uid);
  sprintf ((hdr->ar_gid), "%d", status.st_gid);
  sprintf ((hdr->ar_mode), "%-8o", (unsigned) status.st_mode);
  sprintf ((hdr->ar_size), "%-10ld", status.st_size);
  /* Correct for a lossage in sprintf whereby it null-terminates.  I cannot
     understand how these C losers could design such a ramshackle bunch of
     IO operations */
  temp = (char *) hdr;
  temp1 = temp + sizeof (struct ar_hdr) - 2;
  for (; temp < temp1; temp++) {
    if (*temp == '\0') *temp = ' ';
  }
  strncpy (hdr->ar_fmag, ARFMAG, 2);
  ared->parsed_size = status.st_size;
  ared->arch_header = (char *) hdr;

  return ared;
}

/* This is magic required by the "ar" program.  Since it's
    undocumented, it's undocumented.   You may think that it would
    take a strong stomach to write this, and it does, but it takes
    even a stronger stomach to try to code around such a thing!
*/

struct ar_hdr *
DEFUN(bfd_special_undocumented_glue, (abfd, filename),
      bfd *abfd AND
      char *filename)
{
  struct areltdata *ar_elt = bfd_ar_hdr_from_filesystem (abfd, filename);
  if (ar_elt == NULL)
      return NULL;
  return (struct ar_hdr *) ar_elt->arch_header;
}


/* Analogous to stat call */
int
bfd_generic_stat_arch_elt (abfd, buf)
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

  return 0;
}

void
bfd_dont_truncate_arname (abfd, pathname, arhdr)
     bfd *abfd;
     CONST char *pathname;
     char *arhdr;
{
  /* FIXME: This interacts unpleasantly with ar's quick-append option.
     Fortunately ic960 users will never use that option.  Fixing this
     is very hard; fortunately I know how to do it and will do so once
     intel's release is out the door. */
   
  struct ar_hdr *hdr = (struct ar_hdr *) arhdr;
  int length;
  CONST char *filename = normalize(pathname);
  int maxlen = ar_maxnamelen (abfd);

  length = strlen (filename);

  if (length <= maxlen)
    memcpy (hdr->ar_name, filename, length);

  if (length < maxlen) (hdr->ar_name)[length] = ar_padchar (abfd);
  return;

}

void
bfd_bsd_truncate_arname (abfd, pathname, arhdr)
     bfd *abfd;
     CONST char *pathname;
     char *arhdr;
{
  struct ar_hdr *hdr = (struct ar_hdr *) arhdr;
  int length;
  CONST char *filename = strrchr (pathname, '/');
  int maxlen = ar_maxnamelen (abfd);


  if (filename == NULL)
    filename = pathname;
  else
    ++filename;

  length = strlen (filename);

  if (length <= maxlen)
    memcpy (hdr->ar_name, filename, length);
  else {
    /* pathname: meet procrustes */
    memcpy (hdr->ar_name, filename, maxlen);
    length = maxlen;
  }

  if (length < maxlen) (hdr->ar_name)[length] = ar_padchar (abfd);
}

/* Store name into ar header.  Truncates the name to fit.
   1> strip pathname to be just the basename.
   2> if it's short enuf to fit, stuff it in.
   3> If it doesn't end with .o, truncate it to fit
   4> truncate it before the .o, append .o, stuff THAT in.
*/

/* This is what gnu ar does.  It's better but incompatible with the bsd ar. */
void
bfd_gnu_truncate_arname (abfd, pathname, arhdr)
     bfd *abfd;
     CONST char *pathname;
     char *arhdr;
{
  struct ar_hdr *hdr = (struct ar_hdr *) arhdr;
  int length;
  CONST char *filename = strrchr (pathname, '/');
  int maxlen = ar_maxnamelen (abfd);
	
  if (filename == NULL)
    filename = pathname;
  else
    ++filename;

  length = strlen (filename);

  if (length <= maxlen)
    memcpy (hdr->ar_name, filename, length);
  else {			/* pathname: meet procrustes */
    memcpy (hdr->ar_name, filename, maxlen);
    if ((filename[length - 2] == '.') && (filename[length - 1] == 'o')) {
      hdr->ar_name[maxlen - 2] = '.';
      hdr->ar_name[maxlen - 1] = 'o';
    }
    length = maxlen;
  }

  if (length < 16) (hdr->ar_name)[length] = ar_padchar (abfd);
}


PROTO (boolean, compute_and_write_armap, (bfd *arch, unsigned int elength));

/* The BFD is open for write and has its format set to bfd_archive */
boolean
_bfd_write_archive_contents (arch)
     bfd *arch;
{
  bfd *current;
  char *etable = NULL;
  unsigned int elength = 0;
  boolean makemap = bfd_has_map (arch);
  boolean hasobjects = false;	/* if no .o's, don't bother to make a map */
  unsigned int i;

  /* Verify the viability of all entries; if any of them live in the
     filesystem (as opposed to living in an archive open for input)
     then construct a fresh ar_hdr for them.
     */
  for (current = arch->archive_head; current; current = current->next) {
    if (bfd_write_p (current)) {
      bfd_error = invalid_operation;
      return false;
    }
    if (!current->arelt_data) {
      current->arelt_data =
	  (PTR) bfd_ar_hdr_from_filesystem (arch, current->filename);
      if (!current->arelt_data) return false;

      /* Put in the file name */
    
    BFD_SEND (arch, _bfd_truncate_arname,(arch, 
					  current->filename,
					 (char *) arch_hdr(current)));

      
    }

    if (makemap) {		/* don't bother if we won't make a map! */
      if ((bfd_check_format (current, bfd_object))
#if 0				/* FIXME -- these are not set correctly */
	  && ((bfd_get_file_flags (current) & HAS_SYMS))
#endif
	  )
	hasobjects = true;
    }
  }

  if (!bfd_construct_extended_name_table (arch, &etable, &elength))
    return false;

  bfd_seek (arch, (file_ptr) 0, SEEK_SET);
#ifdef GNU960
  bfd_write (BFD_GNU960_ARMAG(arch), 1, SARMAG, arch);
#else
  bfd_write (ARMAG, 1, SARMAG, arch);
#endif

  if (makemap && hasobjects) {

    if (compute_and_write_armap (arch, elength) != true) {
      return false;
    }
  }

  if (elength != 0) {
    struct ar_hdr hdr;

    memset ((char *)(&hdr), 0, sizeof (struct ar_hdr));
    sprintf (&(hdr.ar_name[0]), "ARFILENAMES/");
    sprintf (&(hdr.ar_size[0]), "%-10d", (int) elength);
    hdr.ar_fmag[0] = '`'; hdr.ar_fmag[1] = '\n';
    for (i = 0; i < sizeof (struct ar_hdr); i++)
      if (((char *)(&hdr))[i] == '\0') (((char *)(&hdr))[i]) = ' ';
    bfd_write ((char *)&hdr, 1, sizeof (struct ar_hdr), arch);
    bfd_write (etable, 1, elength, arch);
    if ((elength % 2) == 1) bfd_write ("\n", 1, 1, arch);

  }

  for (current = arch->archive_head; current; current = current->next) {
    char buffer[DEFAULT_BUFFERSIZE];
    unsigned int remaining = arelt_size (current);
    struct ar_hdr *hdr = arch_hdr(current);
    /* write ar header */

    if (bfd_write ((char *)hdr, 1, sizeof(*hdr), arch) != sizeof(*hdr)) {
    syserr:
	bfd_error = system_call_error;
	return false;
      }
    if (bfd_seek (current, (file_ptr) 0, SEEK_SET) != 0) goto syserr;
    while (remaining) 
	{
	  unsigned int amt = DEFAULT_BUFFERSIZE;
	  if (amt > remaining) {
	    amt = remaining;
	  }
	  errno = 0;
	  if (bfd_read (buffer, amt, 1, current) != amt) {
	      if (errno) goto syserr;
	      /* Looks like a truncated archive. */
	      bfd_error = malformed_archive;
	      return false;
	  }
	  if (bfd_write (buffer, amt, 1, arch)   != amt) goto syserr;
	  remaining -= amt;
	}
    if ((arelt_size (current) % 2) == 1) bfd_write ("\n", 1, 1, arch);
  }
return true;
}

/* Note that the namidx for the first symbol is 0 */

boolean
compute_and_write_armap (arch, elength)
     bfd *arch;
     unsigned int elength;
{
    bfd *current;
    file_ptr elt_no = 0;
    struct orl *map;
    int orl_max = 15000;	/* fine initial default */
    int orl_count = 0;
    int stridx = 0;		/* string index */

    /* Dunno if this is the best place for this info... */
    if (elength != 0) elength += sizeof (struct ar_hdr);
    elength += elength %2 ;

    map = (struct orl *) bfd_zalloc (arch,orl_max * sizeof (struct orl));
    if (map == NULL) {
	    bfd_error = no_memory;
	    return false;
	}

    /* Drop all the files called __.SYMDEF, we're going to make our
       own */
    while (arch->archive_head   &&
	   strcmp(arch->archive_head->filename,"__.SYMDEF") == 0) 
    {
	arch->archive_head = arch->archive_head->next;
    }
    /* Map over each element */
    for (current = arch->archive_head;
	 current != (bfd *)NULL;
	 current = current->next, elt_no++) 
    {
	if ((bfd_check_format (current, bfd_object) == true)
	    && ((bfd_get_file_flags (current) & HAS_SYMS))) {
		asymbol **syms;
		unsigned int storage;
		unsigned int symcount;
		unsigned int src_count;

		storage = get_symtab_upper_bound (current);
		if (storage != 0) {

			syms = (asymbol **) bfd_zalloc (arch,storage);
			if (syms == NULL) {
				bfd_error = no_memory; /* FIXME -- memory leak */
				return false;
			    }
			symcount = bfd_canonicalize_symtab (current, syms);


			/* Now map over all the symbols, picking out the ones we want */
			for (src_count = 0; src_count <symcount; src_count++) {
				flagword flags =
				 (syms[src_count])->flags;
				asection  *sec =
				 syms[src_count]->section;
				
				if ((flags & BSF_GLOBAL) ||
				    (flags & BSF_INDIRECT) ||
				    (sec == &bfd_com_section)) {

					/* This symbol will go into the archive header */
					if (orl_count == orl_max) 
					{
					    orl_max *= 2;
					    map = (struct orl *) bfd_realloc (arch, (char *) map,
									      orl_max * sizeof (struct orl));
					}

					(map[orl_count]).name = (char **) &((syms[src_count])->name);
					(map[orl_count]).pos = (file_ptr) current;
					(map[orl_count]).namidx = stridx;

					stridx += strlen ((syms[src_count])->name) + 1;
					++orl_count;
				    }
			    }
		    }
	    }
    }
    /* OK, now we have collected all the data, let's write them out */
    if (!BFD_SEND (arch, write_armap,
		   (arch, elength, map, orl_count, stridx))) {

	    return false;
	}


    return true;
}

boolean
bsd_write_armap (arch, elength, map, orl_count, stridx)
     bfd *arch;
     unsigned int elength;
     struct orl *map;
     unsigned int orl_count;
     int stridx;
{
  int padit = stridx & 1;
  unsigned int ranlibsize = orl_count * sizeof (struct ranlib);
  unsigned int stringsize = stridx + padit;
  /* Include 8 bytes to store ranlibsize and stringsize in output. */
  unsigned int mapsize = ranlibsize + stringsize + 8;
  file_ptr firstreal;
  bfd *current = arch->archive_head;
  bfd *last_elt = current;	/* last element arch seen */
  int temp;
  int count;
  struct ar_hdr hdr;
  struct stat statbuf;
  unsigned int i;

  firstreal = mapsize + elength + sizeof (struct ar_hdr) + SARMAG;

  stat (arch->filename, &statbuf);
  memset ((char *)(&hdr), 0, sizeof (struct ar_hdr));
  sprintf (hdr.ar_name, RANLIBMAG);

  /* write the timestamp of the archive header to be just a little bit
     later than the timestamp of the file, otherwise the linker will
     complain that the index is out of date.
     */

  sprintf (hdr.ar_date, "%ld", statbuf.st_mtime + 60);  
  sprintf (hdr.ar_uid, "%d", getuid());
  sprintf (hdr.ar_gid, "%d", getgid());
  sprintf (hdr.ar_size, "%-10d", (int) mapsize);
  hdr.ar_fmag[0] = '`'; hdr.ar_fmag[1] = '\n';
  for (i = 0; i < sizeof (struct ar_hdr); i++)
   if (((char *)(&hdr))[i] == '\0') (((char *)(&hdr))[i]) = ' ';
  bfd_write ((char *)&hdr, 1, sizeof (struct ar_hdr), arch);
  bfd_h_put_32(arch, ranlibsize, (PTR)&temp);
  bfd_write (&temp, 1, sizeof (temp), arch);
  
  for (count = 0; count < orl_count; count++) {
    struct symdef outs;
    struct symdef *outp = &outs;
    
    if (((bfd *)(map[count]).pos) != last_elt) {
      do {
	firstreal += arelt_size (current) + sizeof (struct ar_hdr);
	firstreal += firstreal % 2;
	current = current->next;
      } while (current != (bfd *)(map[count]).pos);
    }				/* if new archive element */

    last_elt = current;
    bfd_h_put_32(arch, ((map[count]).namidx),(PTR) &outs.s.string_offset);
    bfd_h_put_32(arch, firstreal,(PTR) &outs.file_offset);
    bfd_write ((char *)outp, 1, sizeof (outs), arch);
  }

  /* now write the strings themselves */
  bfd_h_put_32(arch, stringsize, (PTR)&temp);
  bfd_write ((PTR)&temp, 1, sizeof (temp), arch);
  for (count = 0; count < orl_count; count++)
   bfd_write (*((map[count]).name), 1, strlen (*((map[count]).name))+1, arch);

  /* The spec sez this should be a newline.  But in order to be
     bug-compatible for sun's ar we use a null. */
  if (padit)
   bfd_write("\0",1,1,arch);

  return true;
}


/* A coff armap looks like :
 lARMAG
 struct ar_hdr with name = '/' 
 number of symbols
 offset of file for symbol 0
 offset of file for symbol 1

 offset of file for symbol n-1
 symbol name 0
 symbol name 1	
 
 symbol name n-1

*/

boolean
coff_write_armap (arch, elength, map, symbol_count, stridx)
     bfd *arch;
     unsigned int elength;
     struct orl *map;
     unsigned int symbol_count;
     int stridx;
{
    /* The size of the ranlib is the number of exported symbols in the
       archive * the number of bytes in a int, + an int for the count */

    unsigned int ranlibsize = (symbol_count * 4) + 4;
    unsigned int stringsize = stridx;
    unsigned int mapsize = stringsize + ranlibsize;
    file_ptr archive_member_file_ptr;
    bfd *current = arch->archive_head;
    int count;
    struct ar_hdr hdr;
    unsigned int i;
    int padit = mapsize & 1;
  
    if (padit) mapsize ++;

    /* work out where the first object file will go in the archive */
    archive_member_file_ptr =   mapsize + elength + sizeof (struct ar_hdr) + SARMAG;

    memset ((char *)(&hdr), 0, sizeof (struct ar_hdr));
    hdr.ar_name[0] = '/';
    sprintf (hdr.ar_size, "%-10d", (int) mapsize);
    sprintf (hdr.ar_date, "%ld", (long)time (NULL));
    /* This, at least, is what Intel coff sets the values to.: */
    sprintf ((hdr.ar_uid), "%d", 0);
    sprintf ((hdr.ar_gid), "%d", 0);
    sprintf ((hdr.ar_mode), "%-7o",(unsigned ) 0);
    hdr.ar_fmag[0] = '`'; hdr.ar_fmag[1] = '\n';

    for (i = 0; i < sizeof (struct ar_hdr); i++)
     if (((char *)(&hdr))[i] == '\0') (((char *)(&hdr))[i]) = ' ';

    /* Write the ar header for this item and the number of symbols */

  
    bfd_write ((PTR)&hdr, 1, sizeof (struct ar_hdr), arch);

    bfd_write_bigendian_4byte_int(arch, symbol_count);

    /* Two passes, first write the file offsets for each symbol -
       remembering that each offset is on a two byte boundary.  */

    /* Write out the file offset for the file associated with each
       symbol, and remember to keep the offsets padded out.  */

    current = arch->archive_head;
    count = 0;
    while (current != (bfd *)NULL && count < symbol_count) {
	/* For each symbol which is used defined in this object, write out
	   the object file's address in the archive */
    
	while (((bfd *)(map[count]).pos) == current) {
	    bfd_write_bigendian_4byte_int(arch, archive_member_file_ptr);
	    count++;
	}
	/* Add size of this archive entry */
	archive_member_file_ptr += arelt_size (current) + sizeof (struct
								  ar_hdr);
	/* remember aboout the even alignment */
	archive_member_file_ptr += archive_member_file_ptr % 2;
	current = current->next;
    }  



    /* now write the strings themselves */
    for (count = 0; count < symbol_count; count++) {
	bfd_write ((PTR)*((map[count]).name),
		   1,
		   strlen (*((map[count]).name))+1, arch);

    }
    /* The spec sez this should be a newline.  But in order to be
       bug-compatible for arc960 we use a null. */
    if (padit)
     bfd_write("\0",1,1,arch);

    return true;
}
