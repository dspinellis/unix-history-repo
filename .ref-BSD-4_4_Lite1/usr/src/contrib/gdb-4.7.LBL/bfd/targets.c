/* Generic target-file-type support for the BFD library.
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
	Targets

DESCRIPTION
	Each port of BFD to a different machine requries the creation
	of a target back end. All the back end provides to the root
	part of BFD is a structure containing pointers to functions
	which perform certain low level operations on files. BFD
	translates the applications's requests through a pointer into
	calls to the back end routines. 

	When a file is opened with <<bfd_openr>>, its format and
	target are unknown. BFD uses various mechanisms to determine
	how to interpret the file. The operations performed are:

	o First a BFD is created by calling the internal routine
	<<new_bfd>>, then <<bfd_find_target>> is called with the
	target string supplied to <<bfd_openr>> and the new BFD pointer. 

	o If a null target string was provided to <<bfd_find_target>>,
	it looks up the environment variable <<GNUTARGET>> and uses
	that as the target string. 

	o If the target string is still NULL, or the target string is
	<<default>>, then the first item in the target vector is used
	as the target type, and <<target_defaulted>> is set to
	cause <<bfd_check_format>> to loop through all the targets.
	@xref{bfd_target}.  @xref{Formats}.

	o Otherwise, the elements in the target vector are inspected
	one by one, until a match on target name is found. When found,
	that is used. 

	o Otherwise the error <<invalid_target>> is returned to
	<<bfd_openr>>.

	o <<bfd_openr>> attempts to open the file using
	<<bfd_open_file>>, and returns the BFD.

	Once the BFD has been opened and the target selected, the file
	format may be determined. This is done by calling
	<<bfd_check_format>> on the BFD with a suggested format. 
	If <<target_defaulted>> has been set, each possible target
	type is tried to see if it recognizes the specified format.  The
	routine returns <<true>> when the application guesses right.
@menu
@* bfd_target::
@end menu
*/


/*

INODE
	bfd_target,  , Targets, Targets
DOCDD
SUBSECTION
	bfd_target

DESCRIPTION
	This structure contains everything that BFD knows about a
	target. It includes things like its byte order, name, what
	routines to call to do various operations, etc.   

	Every BFD points to a target structure with its <<xvec>>
	member. 

	Shortcut for declaring fields which are prototyped function
	pointers, while avoiding anguish on compilers that don't
	support protos.

.#define SDEF(ret, name, arglist) \
.                PROTO(ret,(*name),arglist)
.#define SDEF_FMT(ret, name, arglist) \
.                PROTO(ret,(*name[bfd_type_end]),arglist)

	These macros are used to dispatch to functions through the
	bfd_target vector. They are used in a number of macros further
	down in @file{bfd.h}, and are also used when calling various
	routines by hand inside the BFD implementation.  The "arglist"
	argument must be parenthesized; it contains all the arguments
	to the called function. 

	They make the documentation (more) unpleasant to read, so if
	someone wants to fix this and not break the above, please do.

.#define BFD_SEND(bfd, message, arglist) \
.               ((*((bfd)->xvec->message)) arglist)

	For operations which index on the BFD format 

.#define BFD_SEND_FMT(bfd, message, arglist) \
.            (((bfd)->xvec->message[(int)((bfd)->format)]) arglist)

	This is the struct which defines the type of BFD this is.  The
	<<xvec>> member of the struct <<bfd>> itself points here.  Each
	module that implements access to a different target under BFD,
	defines one of these.


	FIXME, these names should be rationalised with the names of
	the entry points which call them. Too bad we can't have one
	macro to define them both! 

.typedef struct bfd_target
.{

Identifies the kind of target, eg SunOS4, Ultrix, etc.

.  char *name;

The "flavour" of a back end is a general indication about the contents
of a file.

.  enum target_flavour {
.    bfd_target_unknown_flavour,
.    bfd_target_aout_flavour,
.    bfd_target_coff_flavour,
.    bfd_target_elf_flavour,
.    bfd_target_ieee_flavour,
.    bfd_target_oasys_flavour,
.    bfd_target_tekhex_flavour,
.    bfd_target_srec_flavour,
.    bfd_target_hppa_flavour} flavour;

The order of bytes within the data area of a file.

.  boolean byteorder_big_p;

The order of bytes within the header parts of a file.

.  boolean header_byteorder_big_p;

This is a mask of all the flags which an executable may have set -
from the set <<NO_FLAGS>>, <<HAS_RELOC>>, ...<<D_PAGED>>.

.  flagword object_flags;       

This is a mask of all the flags which a section may have set - from
the set <<SEC_NO_FLAGS>>, <<SEC_ALLOC>>, ...<<SET_NEVER_LOAD>>.

.  flagword section_flags;

The character normally found at the front of a symbol 
(if any), perhaps _.

.  char symbol_leading_char;

The pad character for filenames within an archive header.

.  char ar_pad_char;            

The maximum number of characters in an archive header.

. unsigned short ar_max_namelen;

The minimum alignment restriction for any section.

.  unsigned int align_power_min;

Entries for byte swapping for data. These are different to the other
entry points, since they don't take BFD as first arg.  Certain other handlers
could do the same.

.  SDEF (bfd_vma,      bfd_getx64, (bfd_byte *));
.  SDEF (void,         bfd_putx64, (bfd_vma, bfd_byte *));
.  SDEF (bfd_vma, bfd_getx32, (bfd_byte *));
.  SDEF (void,         bfd_putx32, (bfd_vma, bfd_byte *));
.  SDEF (bfd_vma, bfd_getx16, (bfd_byte *));
.  SDEF (void,         bfd_putx16, (bfd_vma, bfd_byte *));

Byte swapping for the headers

.  SDEF (bfd_vma,   bfd_h_getx64, (bfd_byte *));
.  SDEF (void,          bfd_h_putx64, (bfd_vma, bfd_byte *));
.  SDEF (bfd_vma,  bfd_h_getx32, (bfd_byte *));
.  SDEF (void,          bfd_h_putx32, (bfd_vma, bfd_byte *));
.  SDEF (bfd_vma,  bfd_h_getx16, (bfd_byte *));
.  SDEF (void,          bfd_h_putx16, (bfd_vma, bfd_byte *));

Format dependent routines, these turn into vectors of entry points
within the target vector structure; one for each format to check.

Check the format of a file being read.  Return bfd_target * or zero. 

.  SDEF_FMT (struct bfd_target *, _bfd_check_format, (bfd *));

Set the format of a file being written.  

.  SDEF_FMT (boolean,            _bfd_set_format, (bfd *));

Write cached information into a file being written, at bfd_close. 

.  SDEF_FMT (boolean,            _bfd_write_contents, (bfd *));

The following functions are defined in <<JUMP_TABLE>>. The idea is
that the back end writer of <<foo>> names all the routines
<<foo_>>@var{entry_point}, <<JUMP_TABLE>> will built the entries
in this structure in the right order.

Core file entry points

.  SDEF (char *, _core_file_failing_command, (bfd *));
.  SDEF (int,    _core_file_failing_signal, (bfd *));
.  SDEF (boolean, _core_file_matches_executable_p, (bfd *, bfd *));

Archive entry points

. SDEF (boolean, _bfd_slurp_armap, (bfd *));
. SDEF (boolean, _bfd_slurp_extended_name_table, (bfd *));
. SDEF (void,   _bfd_truncate_arname, (bfd *, CONST char *, char *));
. SDEF (boolean, write_armap, (bfd *arch, 
.                              unsigned int elength,
.                              struct orl *map,
.                              unsigned int orl_count, 
.                              int stridx));

Standard stuff.

.  SDEF (boolean, _close_and_cleanup, (bfd *));
.  SDEF (boolean, _bfd_set_section_contents, (bfd *, sec_ptr, PTR,
.                                            file_ptr, bfd_size_type));
.  SDEF (boolean, _bfd_get_section_contents, (bfd *, sec_ptr, PTR, 
.                                            file_ptr, bfd_size_type));
.  SDEF (boolean, _new_section_hook, (bfd *, sec_ptr));

Symbols and relocations

.  SDEF (unsigned int, _get_symtab_upper_bound, (bfd *));
.  SDEF (unsigned int, _bfd_canonicalize_symtab,
.           (bfd *, struct symbol_cache_entry **));
.  SDEF (unsigned int, _get_reloc_upper_bound, (bfd *, sec_ptr));
.  SDEF (unsigned int, _bfd_canonicalize_reloc, (bfd *, sec_ptr, arelent **,
.                                               struct symbol_cache_entry**));
.  SDEF (struct symbol_cache_entry  *, _bfd_make_empty_symbol, (bfd *));
.  SDEF (void,     _bfd_print_symbol, (bfd *, PTR, struct symbol_cache_entry  *,
.                                      bfd_print_symbol_type));
.#define bfd_print_symbol(b,p,s,e) BFD_SEND(b, _bfd_print_symbol, (b,p,s,e))
.  SDEF (alent *,   _get_lineno, (bfd *, struct symbol_cache_entry  *));
.
.  SDEF (boolean,   _bfd_set_arch_mach, (bfd *, enum bfd_architecture,
.                                       unsigned long));
.
.  SDEF (bfd *,  openr_next_archived_file, (bfd *arch, bfd *prev));
.  SDEF (boolean, _bfd_find_nearest_line,
.        (bfd *abfd, struct sec  *section,
.         struct symbol_cache_entry  **symbols,bfd_vma offset,
.        CONST char **file, CONST char **func, unsigned int *line));
.  SDEF (int,    _bfd_stat_arch_elt, (bfd *, struct stat *));
.
.  SDEF (int,    _bfd_sizeof_headers, (bfd *, boolean));
.
.  SDEF (void, _bfd_debug_info_start, (bfd *));
.  SDEF (void, _bfd_debug_info_end, (bfd *));
.  SDEF (void, _bfd_debug_info_accumulate, (bfd *, struct sec  *));
.  SDEF (bfd_byte *, _bfd_get_relocated_section_contents, (bfd*,struct bfd_seclet_struct *, bfd_byte *data));
.  SDEF (boolean,_bfd_relax_section,(bfd *, struct sec *, struct symbol_cache_entry **));
Special entry points for gdb to swap in coff symbol table parts

.  SDEF(void, _bfd_coff_swap_aux_in,(
.       bfd            *abfd ,
.       PTR             ext,
.       int             type,
.       int             class ,
.       PTR             in));
.
.  SDEF(void, _bfd_coff_swap_sym_in,(
.       bfd            *abfd ,
.       PTR             ext,
.       PTR             in));
.
.  SDEF(void, _bfd_coff_swap_lineno_in,  (
.       bfd            *abfd,
.       PTR            ext,
.       PTR             in));
.

Special entry points for gas to swap coff parts

. SDEF(unsigned int, _bfd_coff_swap_aux_out,(
.       bfd   	*abfd,
.       PTR	in,
.       int    	type,
.       int    	class,
.       PTR    	ext));
.
. SDEF(unsigned int, _bfd_coff_swap_sym_out,(
.      bfd      *abfd,
.      PTR	in,
.      PTR	ext));
.
. SDEF(unsigned int, _bfd_coff_swap_lineno_out,(
.      	bfd   	*abfd,
.      	PTR	in,
.	PTR	ext));
.
. SDEF(unsigned int, _bfd_coff_swap_reloc_out,(
.      	bfd     *abfd,
.     	PTR	src,
.	PTR	dst));
.
. SDEF(unsigned int, _bfd_coff_swap_filehdr_out,(
.      	bfd  	*abfd,
.	PTR 	in,
.	PTR 	out));
.
. SDEF(unsigned int, _bfd_coff_swap_aouthdr_out,(
.      	bfd 	*abfd,
.	PTR 	in,
.	PTR	out));
.
. SDEF(unsigned int, _bfd_coff_swap_scnhdr_out,(
.      	bfd  	*abfd,
.      	PTR	in,
.	PTR	out));
.
. {* See documentation on reloc types.  *}
. SDEF (CONST struct reloc_howto_struct *,
.       reloc_type_lookup,
.       (bfd *abfd, bfd_reloc_code_real_type code));
.
. {* Complete and utter crock, currently used for the assembler
.    when creating COFF files.  *}
. SDEF (asymbol *, _bfd_make_debug_symbol, (
.       bfd *abfd,
.       void *ptr,
.       unsigned long size));

Data for use by back-end routines; e.g., for a.out, includes whether
this particular target maps ZMAGIC files contiguously or with text and
data separated.  Could perhaps also be used to eliminate some of the
above COFF-specific fields.

. PTR backend_data;
.} bfd_target;

*/

/* The default is to define a target_vector containing all the targets.
   By setting MINIMIZE=1 on the "make" command line, the user can change this
   to a vector containing just DEFAULT_VECTOR and any required
   traditional-core-file handler.  (This is to save space in the executables.)
   The config files can also override the default large vector by giving an
   explicit SELECT_VECS macro.  */

#if MINIMIZE && defined(DEFAULT_VECTOR) && !defined(SELECT_VECS)
#ifdef TRAD_CORE
#define SELECT_VECS &DEFAULT_VECTOR,&trad_core_vec
#else
#define SELECT_VECS &DEFAULT_VECTOR
#endif
#endif

/* All known xvecs.  They are listed a second time below, since
   we can't intermix extern's and initializers.  */
extern bfd_target ecoff_little_vec;
extern bfd_target ecoff_big_vec;
extern bfd_target sunos_big_vec;
extern bfd_target demo_64_vec;
extern bfd_target srec_vec;
extern bfd_target tekhex_vec;
extern bfd_target a_out_adobe_vec;
extern bfd_target b_out_vec_little_host;
extern bfd_target b_out_vec_big_host;
extern bfd_target icoff_little_vec;
extern bfd_target icoff_big_vec;
extern bfd_target elf_little_vec;
extern bfd_target elf_big_vec;
extern bfd_target ieee_vec;
extern bfd_target oasys_vec;
extern bfd_target m88kbcs_vec;
extern bfd_target m68kcoff_vec;
extern bfd_target i386coff_vec;
extern bfd_target i386aout_vec;
extern bfd_target i386linux_vec;
extern bfd_target a29kcoff_big_vec;
extern bfd_target trad_core_vec;
extern bfd_target rs6000coff_vec;
extern bfd_target h8300coff_vec;
extern bfd_target z8kcoff_vec;
extern bfd_target we32kcoff_vec;
#ifdef hp9000s800
extern bfd_target hppa_vec;
#endif

#ifdef DEFAULT_VECTOR
extern bfd_target DEFAULT_VECTOR;
#endif

#ifdef SELECT_VECS

bfd_target *target_vector[] = {
	SELECT_VECS,
	0
};

#else

bfd_target *target_vector[] = {

#ifdef DEFAULT_VECTOR
	&DEFAULT_VECTOR,
#endif

	&i386coff_vec,
	&i386aout_vec,
	&ecoff_little_vec,
	&ecoff_big_vec,
	&ieee_vec,
#if 0
	/* We have no oasys tools anymore, so we can't test any of this
	   anymore. If you want to test the stuff yourself, go ahead...
	   steve@cygnus.com
	   Worse, since there is no magic number for archives, there
	   can annoying target mis-matches.  */
	&oasys_vec,
#endif
#if BSD >= 199103
	&sparcbsd_big_vec,
#endif
	&sunos_big_vec,
#ifdef HOST_64_BIT
	&demo_64_vec,	/* Only compiled if host has long-long support */
#endif
	&h8300coff_vec,
	&z8kcoff_vec,
	&m88kbcs_vec,
	&srec_vec,
/*	&tekhex_vec,*/
	&icoff_little_vec,
	&icoff_big_vec,
	&elf_little_vec,
	&elf_big_vec,
	&a_out_adobe_vec,
	&b_out_vec_little_host,
	&b_out_vec_big_host,
	&m68kcoff_vec,
	&a29kcoff_big_vec,
	&rs6000coff_vec,
#ifdef hp9000s800
        &hppa_vec,
#endif
	&we32kcoff_vec,

#ifdef	TRAD_CORE
	&trad_core_vec,
#endif
	NULL, /* end of list marker */
};

#endif

/* default_vector[0] contains either the address of the default vector,
   if there is one, or zero if there isn't.  */

bfd_target *default_vector[] = {
#ifdef DEFAULT_VECTOR
	&DEFAULT_VECTOR,
#endif
	0,
};




/*
FUNCTION
	bfd_find_target

DESCRIPTION
	Returns a pointer to the transfer vector for the object target
	named target_name.  If target_name is NULL, chooses the one in
	the environment variable GNUTARGET; if that is null or not
	defined thenthe first entry in the target list is chosen.
	Passing in the string "default" or setting the environment
	variable to "default" will cause the first entry in the target
	list to be returned, and "target_defaulted" will be set in the
	BFD.  This causes <<bfd_check_format>> to loop over all the
	targets to find the one that matches the file being read.   

SYNOPSIS
	bfd_target *bfd_find_target(CONST char *, bfd *);
*/

bfd_target *
DEFUN(bfd_find_target,(target_name, abfd),
      CONST char *target_name AND
      bfd *abfd)
{
  bfd_target **target;
  extern char *getenv ();
  CONST char *targname = (target_name ? target_name : 
			  (CONST char *) getenv ("GNUTARGET"));

  /* This is safe; the vector cannot be null */
  if (targname == NULL || !strcmp (targname, "default")) {
    abfd->target_defaulted = true;
    return abfd->xvec = target_vector[0];
  }

  abfd->target_defaulted = false;

  for (target = &target_vector[0]; *target != NULL; target++) {
    if (!strcmp (targname, (*target)->name))
      return abfd->xvec = *target;
  }

  bfd_error = invalid_target;
  return NULL;
}


/*
FUNCTION
	bfd_target_list

DESCRIPTION
	This function returns a freshly malloced NULL-terminated
	vector of the names of all the valid BFD targets. Do not
	modify the names 

SYNOPSIS
	CONST char **bfd_target_list(void);

*/

CONST char **
DEFUN_VOID(bfd_target_list)
{
  int vec_length= 0;
#ifdef NATIVE_HPPAHPUX_COMPILER
  /* The native compiler on the HP9000/700 has a bug which causes it
     to loop endlessly when compiling this file.  This avoids it.  */
  volatile
#endif
    bfd_target **target;
  CONST  char **name_list, **name_ptr;

  for (target = &target_vector[0]; *target != NULL; target++)
    vec_length++;

  name_ptr = 
    name_list = (CONST char **) zalloc ((vec_length + 1) * sizeof (char **));

  if (name_list == NULL) {
    bfd_error = no_memory;
    return NULL;
  }

  for (target = &target_vector[0]; *target != NULL; target++)
    *(name_ptr++) = (*target)->name;

  return name_list;
}
