/* BFD semi-generic back-end for a.out binaries.
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

/*
SECTION
	a.out backends


DESCRIPTION

	BFD supports a number of different flavours of a.out format,
	though the major differences are only the sizes of the
	structures on disk, and the shape of the relocation
	information. 

	The support is split into a basic support file @code{aoutx.h}
	and other files which derive functions from the base. One
	derivation file is @code{aoutf1.h} (for a.out flavour 1), and
	adds to the basic a.out functions support for sun3, sun4, 386
	and 29k a.out files, to create a target jump vector for a
	specific target. 

	This information is further split out into more specific files
	for each machine, including @code{sunos.c} for sun3 and sun4,
	@code{newsos3.c} for the Sony NEWS, and @code{demo64.c} for a
	demonstration of a 64 bit a.out format.

	The base file @code{aoutx.h} defines general mechanisms for
	reading and writing records to and from disk, and various
	other methods which BFD requires. It is included by
	@code{aout32.c} and @code{aout64.c} to form the names
	aout_32_swap_exec_header_in, aout_64_swap_exec_header_in, etc.

	As an example, this is what goes on to make the back end for a
	sun4, from aout32.c 

|	#define ARCH_SIZE 32
|	#include "aoutx.h"

	Which exports names:

|	...
|	aout_32_canonicalize_reloc
|	aout_32_find_nearest_line
|	aout_32_get_lineno
|	aout_32_get_reloc_upper_bound
|	...

	from sunos.c

|	#define ARCH 32
|	#define TARGET_NAME "a.out-sunos-big"
|	#define VECNAME    sunos_big_vec
|	#include "aoutf1.h"

	requires all the names from aout32.c, and produces the jump vector

|	sunos_big_vec

	The file host-aout.c is a special case.  It is for a large set
	of hosts that use ``more or less standard'' a.out files, and
	for which cross-debugging is not interesting.  It uses the
	standard 32-bit a.out support routines, but determines the
	file offsets and addresses of the text, data, and BSS
	sections, the machine architecture and machine type, and the
	entry point address, in a host-dependent manner.  Once these
	values have been determined, generic code is used to handle
	the  object file. 

	When porting it to run on a new system, you must supply:

|        HOST_PAGE_SIZE
|        HOST_SEGMENT_SIZE
|        HOST_MACHINE_ARCH       (optional)
|        HOST_MACHINE_MACHINE    (optional)
|        HOST_TEXT_START_ADDR
|        HOST_STACK_END_ADDR

	in the file <<../include/sys/h-XXX.h>> (for your host).  These
	values, plus the structures and macros defined in <<a.out.h>> on
	your host system, will produce a BFD target that will access
	ordinary a.out files on your host. To configure a new machine
	to use <<host-aout.c>., specify: 

|	TDEFAULTS = -DDEFAULT_VECTOR=host_aout_big_vec
|	TDEPFILES= host-aout.o trad-core.o

	in the <<config/mt-XXX>> file, and modify configure.in to use the
	<<mt-XXX>> file (by setting "<<bfd_target=XXX>>") when your
	configuration is selected.

*/

/* Some assumptions:
   * Any BFD with D_PAGED set is ZMAGIC, and vice versa.
     Doesn't matter what the setting of WP_TEXT is on output, but it'll
     get set on input.
   * Any BFD with D_PAGED clear and WP_TEXT set is NMAGIC.
   * Any BFD with both flags clear is OMAGIC.
   (Just want to make these explicit, so the conditions tested in this
   file make sense if you're more familiar with a.out than with BFD.)  */

#define KEEPIT flags
#define KEEPITTYPE int

#include "bfd.h"
#include <sysdep.h>
#include <ansidecl.h>

struct external_exec;
#include "libaout.h"
#include "libbfd.h"
#include "aout/aout64.h"
#include "aout/stab_gnu.h"
#include "aout/ar.h"

extern void (*bfd_error_trap)();

/*
SUBSECTION
	relocations

DESCRIPTION
	The file @code{aoutx.h} caters for both the @emph{standard}
	and @emph{extended} forms of a.out relocation records.

	The standard records are characterised by containing only an
	address, a symbol index and a type field. The extended records
	(used on 29ks and sparcs) also have a full integer for an
	addend. 

*/
#define CTOR_TABLE_RELOC_IDX 2

#define howto_table_ext NAME(aout,ext_howto_table)
#define howto_table_std NAME(aout,std_howto_table)

reloc_howto_type howto_table_ext[] = 
{
  HOWTO(RELOC_8,      0,  0,  	8,  false, 0, true,  true,0,"8",      false, 0,0x000000ff, false),
  HOWTO(RELOC_16,     0,  1, 	16, false, 0, true,  true,0,"16",      false, 0,0x0000ffff, false),
  HOWTO(RELOC_32,     0,  2, 	32, false, 0, true,  true,0,"32",      false, 0,0xffffffff, false),
  HOWTO(RELOC_DISP8,  0,  0, 	8,  true,  0, false, true,0,"DISP8", 	false, 0,0x000000ff, false),
  HOWTO(RELOC_DISP16, 0,  1, 	16, true,  0, false, true,0,"DISP16", 	false, 0,0x0000ffff, false),
  HOWTO(RELOC_DISP32, 0,  2, 	32, true,  0, false, true,0,"DISP32", 	false, 0,0xffffffff, false),
  HOWTO(RELOC_WDISP30,2,  2, 	30, true,  0, false, true,0,"WDISP30", 	false, 0,0x3fffffff, false),
  HOWTO(RELOC_WDISP22,2,  2, 	22, true,  0, false, true,0,"WDISP22", 	false, 0,0x003fffff, false),
  HOWTO(RELOC_HI22,   10, 2, 	22, false, 0, false, true,0,"HI22",	false, 0,0x003fffff, false),
  HOWTO(RELOC_22,      0, 2, 	22, false, 0, false, true,0,"22",       false, 0,0x003fffff, false),
  HOWTO(RELOC_13, 	0, 2, 	13, false, 0, false, true,0,"13",       false, 0,0x00001fff, false),
  HOWTO(RELOC_LO10, 	0, 2, 	10, false, 0, false, true,0,"LO10",     false, 0,0x000003ff, false),
  HOWTO(RELOC_SFA_BASE,0, 2, 	32, false, 0, false, true,0,"SFA_BASE", false, 0,0xffffffff, false),
  HOWTO(RELOC_SFA_OFF13,0,2, 	32, false, 0, false, true,0,"SFA_OFF13",false, 0,0xffffffff, false),
  HOWTO(RELOC_BASE10, 0,  2, 	16, false, 0, false, true,0,"BASE10",   false, 0,0x0000ffff, false),
  HOWTO(RELOC_BASE13, 0,  2,	13, false, 0, false, true,0,"BASE13",   false, 0,0x00001fff, false),
  HOWTO(RELOC_BASE22, 0,  2,	0,  false, 0, false, true,0,"BASE22",   false, 0,0x00000000, false),
  HOWTO(RELOC_PC10,   0,  2,	10, false, 0, false, true,0,"PC10",	false, 0,0x000003ff, false),
  HOWTO(RELOC_PC22,   0,  2,	22, false, 0, false, true,0,"PC22",	false, 0,0x003fffff, false),
  HOWTO(RELOC_JMP_TBL,0,  2,	32, false, 0, false, true,0,"JMP_TBL",	false, 0,0xffffffff, false),
  HOWTO(RELOC_SEGOFF16,0, 2,	0,  false, 0, false, true,0,"SEGOFF16",	false, 0,0x00000000, false),
  HOWTO(RELOC_GLOB_DAT,0, 2,	0,  false, 0, false, true,0,"GLOB_DAT",	false, 0,0x00000000, false),
  HOWTO(RELOC_JMP_SLOT,0, 2,	0,  false, 0, false, true,0,"JMP_SLOT",	false, 0,0x00000000, false),
  HOWTO(RELOC_RELATIVE,0, 2,	0,  false, 0, false,	true,0,"RELATIVE",	false, 0,0x00000000, false),
};

/* Convert standard reloc records to "arelent" format (incl byte swap).  */

reloc_howto_type howto_table_std[] = {
  /* type           rs   size bsz  pcrel bitpos  abs ovrf sf name    part_inpl   readmask  setmask  pcdone */
HOWTO( 0,	       0,  0,  	8,  false, 0, true,  true,0,"8",	true, 0x000000ff,0x000000ff, false),
HOWTO( 1,	       0,  1, 	16, false, 0, true,  true,0,"16",	true, 0x0000ffff,0x0000ffff, false),
HOWTO( 2,	       0,  2, 	32, false, 0, true,  true,0,"32",	true, 0xffffffff,0xffffffff, false),
HOWTO( 3,	       0,  3, 	64, false, 0, true,  true,0,"64",       true, 0xdeaddead,0xdeaddead, false),
HOWTO( 4,	       0,  0, 	8,  true,  0, false, true,0,"DISP8",    true, 0x000000ff,0x000000ff, false),
HOWTO( 5,	       0,  1, 	16, true,  0, false, true,0,"DISP16",   true, 0x0000ffff,0x0000ffff, false),
HOWTO( 6,	       0,  2, 	32, true,  0, false, true,0,"DISP32",   true, 0xffffffff,0xffffffff, false),
HOWTO( 7,	       0,  3, 	64, true,  0, false, true,0,"DISP64",   true, 0xfeedface,0xfeedface, false),
};

CONST struct reloc_howto_struct *
DEFUN(NAME(aout,reloc_type_lookup),(abfd,code),
      bfd *abfd AND
      bfd_reloc_code_real_type code)
{
#define EXT(i,j)	case i: return &howto_table_ext[j]
#define STD(i,j)	case i: return &howto_table_std[j]
  int ext = obj_reloc_entry_size (abfd) == RELOC_EXT_SIZE;
  if (code == BFD_RELOC_CTOR)
    switch (bfd_get_arch_info (abfd)->bits_per_address)
      {
      case 32:
	code = BFD_RELOC_32;
	break;
      }
  if (ext)
    switch (code)
      {
	EXT (BFD_RELOC_32, 2);
	EXT (BFD_RELOC_HI22, 8);
	EXT (BFD_RELOC_LO10, 11);
	EXT (BFD_RELOC_32_PCREL_S2, 6);
      }
  else
    /* std relocs */
    switch (code)
      {
	STD (BFD_RELOC_16, 1);
	STD (BFD_RELOC_32, 2);
	STD (BFD_RELOC_8_PCREL, 4);
	STD (BFD_RELOC_16_PCREL, 5);
	STD (BFD_RELOC_32_PCREL, 6);
      }
  return 0;
}

extern bfd_error_vector_type bfd_error_vector;

/*
SUBSECTION
	Internal Entry Points

DESCRIPTION
	@code{aoutx.h} exports several routines for accessing the
	contents of an a.out file, which are gathered and exported in
	turn by various format specific files (eg sunos.c).

*/

/*
FUNCTION
	 aout_<size>_swap_exec_header_in

DESCRIPTION
	Swaps the information in an executable header taken from a raw
	byte stream memory image, into the internal exec_header
	structure.

EXAMPLE
	void aout_<size>_swap_exec_header_in,
           (bfd *abfd,
            struct external_exec *raw_bytes,
            struct internal_exec *execp);
*/
	 
void
DEFUN(NAME(aout,swap_exec_header_in),(abfd, raw_bytes, execp),
      bfd *abfd AND
      struct external_exec *raw_bytes AND
      struct internal_exec *execp)
{
  struct external_exec *bytes = (struct external_exec *)raw_bytes;

  /* The internal_exec structure has some fields that are unused in this
     configuration (IE for i960), so ensure that all such uninitialized
     fields are zero'd out.  There are places where two of these structs
     are memcmp'd, and thus the contents do matter. */
  memset (execp, 0, sizeof (struct internal_exec));
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

/*
FUNCTION
	aout_<size>_swap_exec_header_out

DESCRIPTION
	Swaps the information in an internal exec header structure
	into the supplied buffer ready for writing to disk.

EXAMPLE
	void aout_<size>_swap_exec_header_out
	  (bfd *abfd,
	   struct internal_exec *execp,
	   struct external_exec *raw_bytes);
*/
void
DEFUN(NAME(aout,swap_exec_header_out),(abfd, execp, raw_bytes),
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



/*
FUNCTION
	aout_<size>_some_aout_object_p

DESCRIPTION
	Some A.OUT variant thinks that the file whose format we're
	checking is an a.out file.  Do some more checking, and set up
	for access if it really is.  Call back to the calling
	environments "finish up" function just before returning, to
	handle any last-minute setup.  

EXAMPLE
	bfd_target *aout_<size>_some_aout_object_p
	 (bfd *abfd,
	  bfd_target *(*callback_to_real_object_p)());
*/
 
bfd_target *
DEFUN(NAME(aout,some_aout_object_p),(abfd, execp, callback_to_real_object_p),
      bfd *abfd AND
      struct internal_exec *execp AND
      bfd_target *(*callback_to_real_object_p) ())
{
  struct aout_data_struct *rawptr, *oldrawptr;
  bfd_target *result;

  rawptr = (struct aout_data_struct  *) bfd_zalloc (abfd, sizeof (struct aout_data_struct ));
  if (rawptr == NULL) {
    bfd_error = no_memory;
    return 0;
  }

  oldrawptr = abfd->tdata.aout_data;
  abfd->tdata.aout_data = rawptr;
  abfd->tdata.aout_data->a.hdr = &rawptr->e;
  *(abfd->tdata.aout_data->a.hdr) = *execp;	/* Copy in the internal_exec struct */
  execp = abfd->tdata.aout_data->a.hdr;

  /* Set the file flags */
  abfd->flags = NO_FLAGS;
  if (execp->a_drsize || execp->a_trsize)
    abfd->flags |= HAS_RELOC;
  /* Setting of EXEC_P has been deferred to the bottom of this function */
  if (execp->a_syms) 
    abfd->flags |= HAS_LINENO | HAS_DEBUG | HAS_SYMS | HAS_LOCALS;

  if (N_MAGIC (*execp) == ZMAGIC)
    {
      abfd->flags |= D_PAGED|WP_TEXT;
      adata(abfd).magic = z_magic;
    }
  else if (N_MAGIC (*execp) == NMAGIC)
    {
      abfd->flags |= WP_TEXT;
      adata(abfd).magic = n_magic;
    }
  else
    adata(abfd).magic = o_magic;

  bfd_get_start_address (abfd) = execp->a_entry;

  obj_aout_symbols (abfd) = (aout_symbol_type *)NULL;
  bfd_get_symcount (abfd) = execp->a_syms / sizeof (struct external_nlist);

  /* The default relocation entry size is that of traditional V7 Unix.  */
  obj_reloc_entry_size (abfd) = RELOC_STD_SIZE;

  /* The default symbol entry size is that of traditional Unix. */
  obj_symbol_entry_size (abfd) = EXTERNAL_NLIST_SIZE;

  /* create the sections.  This is raunchy, but bfd_close wants to reclaim
     them */

  obj_textsec (abfd) = bfd_make_section_old_way (abfd, ".text");
  obj_datasec (abfd) = bfd_make_section_old_way (abfd, ".data");
  obj_bsssec (abfd) = bfd_make_section_old_way (abfd, ".bss");

#if 0
  (void)bfd_make_section (abfd, ".text");
  (void)bfd_make_section (abfd, ".data");
  (void)bfd_make_section (abfd, ".bss");
#endif

  obj_datasec (abfd)->_raw_size = execp->a_data;
  obj_bsssec (abfd)->_raw_size = execp->a_bss;

  obj_textsec (abfd)->flags = (execp->a_trsize != 0 ?
       (SEC_ALLOC | SEC_LOAD | SEC_CODE | SEC_HAS_CONTENTS | SEC_RELOC) :
       (SEC_ALLOC | SEC_LOAD | SEC_CODE | SEC_HAS_CONTENTS));
  obj_datasec (abfd)->flags = (execp->a_drsize != 0 ?
       (SEC_ALLOC | SEC_LOAD | SEC_DATA | SEC_HAS_CONTENTS | SEC_RELOC) :
       (SEC_ALLOC | SEC_LOAD | SEC_DATA | SEC_HAS_CONTENTS));
  obj_bsssec (abfd)->flags = SEC_ALLOC;

#ifdef THIS_IS_ONLY_DOCUMENTATION
  /* The common code can't fill in these things because they depend
     on either the start address of the text segment, the rounding
     up of virtual addersses between segments, or the starting file 
     position of the text segment -- all of which varies among different
     versions of a.out.  */

  /* Call back to the format-dependent code to fill in the rest of the 
     fields and do any further cleanup.  Things that should be filled
     in by the callback:  */

  struct exec *execp = exec_hdr (abfd);

  obj_textsec (abfd)->size = N_TXTSIZE(*execp);
  obj_textsec (abfd)->raw_size = N_TXTSIZE(*execp);
  /* data and bss are already filled in since they're so standard */

  /* The virtual memory addresses of the sections */
  obj_textsec (abfd)->vma = N_TXTADDR(*execp);
  obj_datasec (abfd)->vma = N_DATADDR(*execp);
  obj_bsssec  (abfd)->vma = N_BSSADDR(*execp);

  /* The file offsets of the sections */
  obj_textsec (abfd)->filepos = N_TXTOFF(*execp);
  obj_datasec (abfd)->filepos = N_DATOFF(*execp);

  /* The file offsets of the relocation info */
  obj_textsec (abfd)->rel_filepos = N_TRELOFF(*execp);
  obj_datasec (abfd)->rel_filepos = N_DRELOFF(*execp);

  /* The file offsets of the string table and symbol table.  */
  obj_str_filepos (abfd) = N_STROFF (*execp);
  obj_sym_filepos (abfd) = N_SYMOFF (*execp);

  /* Determine the architecture and machine type of the object file.  */
  switch (N_MACHTYPE (*exec_hdr (abfd))) {
  default:
    abfd->obj_arch = bfd_arch_obscure;
    break;
  }

  adata(abfd)->page_size = PAGE_SIZE;
  adata(abfd)->segment_size = SEGMENT_SIZE;
  adata(abfd)->exec_bytes_size = EXEC_BYTES_SIZE;

  return abfd->xvec;

  /* The architecture is encoded in various ways in various a.out variants,
     or is not encoded at all in some of them.  The relocation size depends
     on the architecture and the a.out variant.  Finally, the return value
     is the bfd_target vector in use.  If an error occurs, return zero and
     set bfd_error to the appropriate error code.
     
     Formats such as b.out, which have additional fields in the a.out
     header, should cope with them in this callback as well.  */
#endif				/* DOCUMENTATION */

  result = (*callback_to_real_object_p)(abfd);

  /* Now that the segment addresses have been worked out, take a better
     guess at whether the file is executable.  If the entry point
     is within the text segment, assume it is.  (This makes files
     executable even if their entry point address is 0, as long as
     their text starts at zero.)  

     At some point we should probably break down and stat the file and
     declare it executable if (one of) its 'x' bits are on...  */
  if ((execp->a_entry >= obj_textsec(abfd)->vma) &&
      (execp->a_entry < obj_textsec(abfd)->vma + obj_textsec(abfd)->_raw_size))
    abfd->flags |= EXEC_P;
  if (result)
    {
      abfd->sections = obj_textsec (abfd);
      obj_textsec (abfd)->next = obj_datasec (abfd);
      obj_datasec (abfd)->next = obj_bsssec (abfd);
    }
  else
    {
      free (rawptr);
      abfd->tdata.aout_data = oldrawptr;
    }
  return result;
}

/*
FUNCTION
	aout_<size>_mkobject

DESCRIPTION
	This routine initializes a BFD for use with a.out files.

EXAMPLE
	boolean aout_<size>_mkobject, (bfd *);
*/

boolean
DEFUN(NAME(aout,mkobject),(abfd),
     bfd *abfd)
{
  struct aout_data_struct  *rawptr;

  bfd_error = system_call_error;

  /* Use an intermediate variable for clarity */
  rawptr = (struct aout_data_struct  *)bfd_zalloc (abfd, sizeof (struct aout_data_struct ));
  
  if (rawptr == NULL) {
    bfd_error = no_memory;
    return false;
  }
  
  abfd->tdata.aout_data = rawptr;
  exec_hdr (abfd) = &(rawptr->e);
  
  /* For simplicity's sake we just make all the sections right here. */
  
  obj_textsec (abfd) = (asection *)NULL;
  obj_datasec (abfd) = (asection *)NULL;
  obj_bsssec (abfd) = (asection *)NULL;
  bfd_make_section (abfd, ".text");
  bfd_make_section (abfd, ".data");
  bfd_make_section (abfd, ".bss");
  bfd_make_section (abfd, BFD_ABS_SECTION_NAME);
  bfd_make_section (abfd, BFD_UND_SECTION_NAME);
  bfd_make_section (abfd, BFD_COM_SECTION_NAME);
  
  return true;
}


/*
FUNCTION
	aout_<size>_machine_type

DESCRIPTION
	Keep track of machine architecture and machine type for
	a.out's. Return the machine_type for a particular
	arch&machine, or M_UNKNOWN if that exact arch&machine can't be
	represented in a.out format. 

	If the architecture is understood, machine type 0 (default)
	should always be understood.  

EXAMPLE
	enum machine_type  aout_<size>_machine_type
	 (enum bfd_architecture arch,
	  unsigned long machine));
*/

enum machine_type
DEFUN(NAME(aout,machine_type),(arch, machine),
      enum bfd_architecture arch AND
      unsigned long machine)
{
  enum machine_type arch_flags;
    
  arch_flags = M_UNKNOWN;
    
  switch (arch) {
  case bfd_arch_sparc:
    if (machine == 0)	arch_flags = M_SPARC;
    break;
      
  case bfd_arch_m68k:
    switch (machine) {
    case 0:		arch_flags = M_68010; break;
    case 68000:		arch_flags = M_UNKNOWN;	break;
    case 68010:		arch_flags = M_68010; break;
    case 68020:		arch_flags = M_68020; break;
    default:		arch_flags = M_UNKNOWN; break;
    }
    break;
      
  case bfd_arch_i386:
    if (machine == 0)	arch_flags = M_386;
    break;
      
  case bfd_arch_a29k:
    if (machine == 0)	arch_flags = M_29K;
    break;
      
  default:
    arch_flags = M_UNKNOWN;
    break;
  }
  return arch_flags;
}


/*
FUNCTION
	aout_<size>_set_arch_mach

DESCRIPTION
	Sets the architecture and the machine of the BFD to those
	values supplied. Verifies that the format can support the
	architecture required.

EXAMPLE
	boolean aout_<size>_set_arch_mach,
	 (bfd *,
	  enum bfd_architecture,
	  unsigned long machine));
*/

boolean
DEFUN(NAME(aout,set_arch_mach),(abfd, arch, machine),
      bfd *abfd AND
      enum bfd_architecture arch AND
      unsigned long machine)
{
  bfd_default_set_arch_mach(abfd, arch, machine);
  if (arch != bfd_arch_unknown &&
      NAME(aout,machine_type) (arch, machine) == M_UNKNOWN)
    return false;		/* We can't represent this type */

  /* Determine the size of a relocation entry */
  switch (arch) {
  case bfd_arch_sparc:
  case bfd_arch_a29k:
    obj_reloc_entry_size (abfd) = RELOC_EXT_SIZE;
    break;
  default:
    obj_reloc_entry_size (abfd) = RELOC_STD_SIZE;
    break;
  }

  return (*aout_backend_info(abfd)->set_sizes) (abfd);
}

boolean
DEFUN (NAME (aout,adjust_sizes_and_vmas), (abfd, text_size, text_end),
       bfd *abfd AND bfd_size_type *text_size AND file_ptr *text_end)
{
  struct internal_exec *execp = exec_hdr (abfd);
  if ((obj_textsec (abfd) == NULL) || (obj_datasec (abfd) == NULL)) 
    {
      bfd_error = invalid_operation;
      return false;
    }
  if (adata(abfd).magic != undecided_magic) return true;
  obj_textsec(abfd)->_raw_size = 	      
    align_power(obj_textsec(abfd)->_raw_size,
		obj_textsec(abfd)->alignment_power);

  *text_size = obj_textsec (abfd)->_raw_size;
  /* Rule (heuristic) for when to pad to a new page.  Note that there
   * are (at least) two ways demand-paged (ZMAGIC) files have been
   * handled.  Most Berkeley-based systems start the text segment at
   * (PAGE_SIZE).  However, newer versions of SUNOS start the text
   * segment right after the exec header; the latter is counted in the
   * text segment size, and is paged in by the kernel with the rest of
   * the text. */

  /* This perhaps isn't the right way to do this, but made it simpler for me
     to understand enough to implement it.  Better would probably be to go
     right from BFD flags to alignment/positioning characteristics.  But the
     old code was sloppy enough about handling the flags, and had enough
     other magic, that it was a little hard for me to understand.  I think
     I understand it better now, but I haven't time to do the cleanup this
     minute.  */
  if (adata(abfd).magic == undecided_magic)
    {
      if (abfd->flags & D_PAGED)
	/* whether or not WP_TEXT is set */
	adata(abfd).magic = z_magic;
      else if (abfd->flags & WP_TEXT)
	adata(abfd).magic = n_magic;
      else
	adata(abfd).magic = o_magic;
    }

#ifdef BFD_AOUT_DEBUG /* requires gcc2 */
#if __GNUC__ >= 2
  fprintf (stderr, "%s text=<%x,%x,%x> data=<%x,%x,%x> bss=<%x,%x,%x>\n",
	   ({ char *str;
	      switch (adata(abfd).magic) {
	      case n_magic: str = "NMAGIC"; break;
	      case o_magic: str = "OMAGIC"; break;
	      case z_magic: str = "ZMAGIC"; break;
	      default: abort ();
	      }
	      str;
	    }),
	   obj_textsec(abfd)->vma, obj_textsec(abfd)->_raw_size, obj_textsec(abfd)->alignment_power,
	   obj_datasec(abfd)->vma, obj_datasec(abfd)->_raw_size, obj_datasec(abfd)->alignment_power,
	   obj_bsssec(abfd)->vma, obj_bsssec(abfd)->_raw_size, obj_bsssec(abfd)->alignment_power);
#endif
#endif

  switch (adata(abfd).magic)
    {
    case o_magic:
      {
	file_ptr pos = adata (abfd).exec_bytes_size;
	bfd_vma vma = 0;
	int pad = 0;

	obj_textsec(abfd)->filepos = pos;
	pos += obj_textsec(abfd)->_raw_size;
	vma += obj_textsec(abfd)->_raw_size;
	if (!obj_datasec(abfd)->user_set_vma)
	  {
#if 0	    /* ?? Does alignment in the file image really matter? */
	    pad = align_power (vma, obj_datasec(abfd)->alignment_power) - vma;
#endif
	    obj_textsec(abfd)->_raw_size += pad;
	    pos += pad;
	    vma += pad;
	    obj_datasec(abfd)->vma = vma;
	  }
	obj_datasec(abfd)->filepos = pos;
	pos += obj_datasec(abfd)->_raw_size;
	vma += obj_datasec(abfd)->_raw_size;
	if (!obj_bsssec(abfd)->user_set_vma)
	  {
#if 0
	    pad = align_power (vma, obj_bsssec(abfd)->alignment_power) - vma;
#endif
	    obj_datasec(abfd)->_raw_size += pad;
	    pos += pad;
	    vma += pad;
	    obj_bsssec(abfd)->vma = vma;
	  }
	obj_bsssec(abfd)->filepos = pos;
	execp->a_text = obj_textsec(abfd)->_raw_size;
	execp->a_data = obj_datasec(abfd)->_raw_size;
	execp->a_bss = obj_bsssec(abfd)->_raw_size;
	N_SET_MAGIC (*execp, OMAGIC);
      }
      break;
    case z_magic:
      {
	bfd_size_type data_pad, text_pad;
	file_ptr text_end;
	CONST struct aout_backend_data *abdp;
	int ztih;
	bfd_vma data_vma;

	abdp = aout_backend_info (abfd);
	ztih = abdp && abdp->text_includes_header;
	obj_textsec(abfd)->filepos = (ztih
				      ? adata(abfd).exec_bytes_size
				      : adata(abfd).page_size);
	if (! obj_textsec(abfd)->user_set_vma)
	  /* ?? Do we really need to check for relocs here?  */
	  obj_textsec(abfd)->vma = ((abfd->flags & HAS_RELOC)
				    ? 0
				    : (ztih
				       ? (abdp->default_text_vma
					  + adata(abfd).exec_bytes_size)
				       : abdp->default_text_vma));
	/* Could take strange alignment of text section into account here?  */

	/* Find start of data.  */
	text_end = obj_textsec(abfd)->filepos + obj_textsec(abfd)->_raw_size;
	text_pad = BFD_ALIGN (text_end, adata(abfd).page_size) - text_end;
	obj_textsec(abfd)->_raw_size += text_pad;
	text_end += text_pad;

	if (!obj_datasec(abfd)->user_set_vma)
	  {
	    bfd_vma vma;
	    vma = obj_textsec(abfd)->vma + obj_textsec(abfd)->_raw_size;
	    obj_datasec(abfd)->vma = BFD_ALIGN (vma, adata(abfd).segment_size);
	  }
	data_vma = obj_datasec(abfd)->vma;
	if (abdp && abdp->zmagic_mapped_contiguous)
	  {
	    text_pad = (obj_datasec(abfd)->vma
			- obj_textsec(abfd)->vma
			- obj_textsec(abfd)->_raw_size);
	    obj_textsec(abfd)->_raw_size += text_pad;
	  }
	obj_datasec(abfd)->filepos = (obj_textsec(abfd)->filepos
				      + obj_textsec(abfd)->_raw_size);

	/* Fix up exec header while we're at it.  */
	execp->a_text = obj_textsec(abfd)->_raw_size;
	if (ztih && (!abdp || (abdp && !abdp->exec_header_not_counted)))
	  execp->a_text += adata(abfd).exec_bytes_size;
	N_SET_MAGIC (*execp, ZMAGIC);
	/* Spec says data section should be rounded up to page boundary.  */
	/* If extra space in page is left after data section, fudge data
	   in the header so that the bss section looks smaller by that
	   amount.  We'll start the bss section there, and lie to the OS.  */
	obj_datasec(abfd)->_raw_size
	  = align_power (obj_datasec(abfd)->_raw_size,
			 obj_bsssec(abfd)->alignment_power);
	execp->a_data = BFD_ALIGN (obj_datasec(abfd)->_raw_size,
				   adata(abfd).page_size);
	data_pad = execp->a_data - obj_datasec(abfd)->_raw_size;
	/* This code is almost surely botched.  It'll only get tested
	   for the case where the application does explicitly set the VMA
	   of the BSS section.  */
	if (obj_bsssec(abfd)->user_set_vma
	    && (obj_bsssec(abfd)->vma
		> BFD_ALIGN (obj_datasec(abfd)->vma
			     + obj_datasec(abfd)->_raw_size,
			     adata(abfd).page_size)))
	  {
	    /* Can't play with squeezing into data pages; fix this code.  */
	    abort ();
	  }
	if (!obj_bsssec(abfd)->user_set_vma)
	  obj_bsssec(abfd)->vma = (obj_datasec(abfd)->vma
				   + obj_datasec(abfd)->_raw_size);
	if (data_pad > obj_bsssec(abfd)->_raw_size)
	  execp->a_bss = 0;
	else
	  execp->a_bss = obj_bsssec(abfd)->_raw_size - data_pad;
      }
      break;
    case n_magic:
      {
	file_ptr pos = adata(abfd).exec_bytes_size;
	bfd_vma vma = 0;
	int pad;

	obj_textsec(abfd)->filepos = pos;
	if (!obj_textsec(abfd)->user_set_vma)
	  obj_textsec(abfd)->vma = vma;
	else
	  vma = obj_textsec(abfd)->vma;
	pos += obj_textsec(abfd)->_raw_size;
	vma += obj_textsec(abfd)->_raw_size;
	obj_datasec(abfd)->filepos = pos;
	if (!obj_datasec(abfd)->user_set_vma)
	  obj_datasec(abfd)->vma = BFD_ALIGN (vma, adata(abfd).segment_size);
	vma = obj_datasec(abfd)->vma;

	/* Since BSS follows data immediately, see if it needs alignment.  */
	vma += obj_datasec(abfd)->_raw_size;
	pad = align_power (vma, obj_bsssec(abfd)->alignment_power) - vma;
	obj_datasec(abfd)->_raw_size += pad;
	pos += obj_datasec(abfd)->_raw_size;

	if (!obj_bsssec(abfd)->user_set_vma)
	  obj_bsssec(abfd)->vma = vma;
	else
	  vma = obj_bsssec(abfd)->vma;
      }
      execp->a_text = obj_textsec(abfd)->_raw_size;
      execp->a_data = obj_datasec(abfd)->_raw_size;
      execp->a_bss = obj_bsssec(abfd)->_raw_size;
      N_SET_MAGIC (*execp, NMAGIC);
      break;
    default:
      abort ();
    }
#ifdef BFD_AOUT_DEBUG
  fprintf (stderr, "       text=<%x,%x,%x> data=<%x,%x,%x> bss=<%x,%x>\n",
	   obj_textsec(abfd)->vma, obj_textsec(abfd)->_raw_size, obj_textsec(abfd)->filepos,
	   obj_datasec(abfd)->vma, obj_datasec(abfd)->_raw_size, obj_datasec(abfd)->filepos,
	   obj_bsssec(abfd)->vma, obj_bsssec(abfd)->_raw_size);
#endif
  return true;
}

/*
FUNCTION
	aout_<size>new_section_hook
  
DESCRIPTION
	Called by the BFD in response to a @code{bfd_make_section}
	request.

EXAMPLE
        boolean aout_<size>_new_section_hook,
	   (bfd *abfd,
	    asection *newsect));
*/
boolean
DEFUN(NAME(aout,new_section_hook),(abfd, newsect),
	bfd *abfd AND
	asection *newsect)
{
  /* align to double at least */
  newsect->alignment_power = bfd_get_arch_info(abfd)->section_align_power;

    
  if (bfd_get_format (abfd) == bfd_object) 
  {
    if (obj_textsec(abfd) == NULL && !strcmp(newsect->name, ".text")) {
	obj_textsec(abfd)= newsect;
	newsect->target_index = N_TEXT | N_EXT;
	return true;
      }
      
    if (obj_datasec(abfd) == NULL && !strcmp(newsect->name, ".data")) {
	obj_datasec(abfd) = newsect;
	newsect->target_index = N_DATA | N_EXT;
	return true;
      }
      
    if (obj_bsssec(abfd) == NULL && !strcmp(newsect->name, ".bss")) {
	obj_bsssec(abfd) = newsect;
	newsect->target_index = N_BSS | N_EXT;
	return true;
      }

  }
    
  /* We allow more than three sections internally */
  return true;
}

boolean
  DEFUN(NAME(aout,set_section_contents),(abfd, section, location, offset, count),
	bfd *abfd AND
	sec_ptr section AND
	PTR location AND
	file_ptr offset AND
	bfd_size_type count)
{
  file_ptr text_end;
  bfd_size_type text_size;

  if (abfd->output_has_begun == false)
      {				/* set by bfd.c handler */
	switch (abfd->direction)
	    {
	    case read_direction:
	    case no_direction:
	      bfd_error = invalid_operation;
	      return false;

	    case write_direction:
	      if (NAME(aout,adjust_sizes_and_vmas) (abfd,
						    &text_size,
						    &text_end) == false)
		return false;
	    case both_direction:
	      break;
	    }
      }

  /* regardless, once we know what we're doing, we might as well get going */
  if (section != obj_bsssec(abfd)) 
      {
	bfd_seek (abfd, section->filepos + offset, SEEK_SET);
    
	if (count) {
	  return (bfd_write ((PTR)location, 1, count, abfd) == count) ?
	    true : false;
	}
	return true;
      }
  return true;
}

/* Classify stabs symbols */

#define sym_in_text_section(sym) \
  (((sym)->type  & (N_ABS | N_TEXT | N_DATA | N_BSS))== N_TEXT)

#define sym_in_data_section(sym) \
  (((sym)->type  & (N_ABS | N_TEXT | N_DATA | N_BSS))== N_DATA)

#define sym_in_bss_section(sym) \
  (((sym)->type  & (N_ABS | N_TEXT | N_DATA | N_BSS))== N_BSS)

/* Symbol is undefined if type is N_UNDF|N_EXT and if it has
  zero in the "value" field.  Nonzeroes there are fortrancommon
  symbols.  */
#define sym_is_undefined(sym) \
  ((sym)->type == (N_UNDF | N_EXT) && (sym)->symbol.value == 0)

/* Symbol is a global definition if N_EXT is on and if it has
  a nonzero type field.  */
#define sym_is_global_defn(sym) \
  (((sym)->type & N_EXT) && (sym)->type & N_TYPE)

/* Symbol is debugger info if any bits outside N_TYPE or N_EXT
  are on.  */
#define sym_is_debugger_info(sym) \
  ((sym)->type & ~(N_EXT | N_TYPE))

#define sym_is_fortrancommon(sym)       \
  (((sym)->type == (N_EXT)) && (sym)->symbol.value != 0)

/* Symbol is absolute if it has N_ABS set */
#define sym_is_absolute(sym) \
  (((sym)->type  & N_TYPE)== N_ABS)


#define sym_is_indirect(sym) \
  (((sym)->type & N_ABS)== N_ABS)

/* Only in their own functions for ease of debugging; when sym flags have
  stabilised these should be inlined into their (single) caller */
  
static void
DEFUN(translate_from_native_sym_flags,(sym_pointer, cache_ptr, abfd),
      struct external_nlist *sym_pointer AND
      aout_symbol_type *cache_ptr AND
      bfd *abfd)
{
  switch (cache_ptr->type & N_TYPE) 
  {
  case N_SETA:
  case N_SETT:
  case N_SETD:
  case N_SETB:
  {
    char *copy = bfd_alloc(abfd, strlen(cache_ptr->symbol.name)+1);
    asection *section ;
    asection *into_section;
      
    arelent_chain *reloc = (arelent_chain *)bfd_alloc(abfd, sizeof(arelent_chain));
    strcpy(copy, cache_ptr->symbol.name);

    /* Make sure that this bfd has a section with the right contructor
       name */
    section = bfd_get_section_by_name (abfd, copy);
    if (!section)
     section = bfd_make_section(abfd,copy);

    /* Build a relocation entry for the constructor */
    switch ( (cache_ptr->type  & N_TYPE) ) 
    {
    case N_SETA:
      into_section = &bfd_abs_section;
      break;
    case N_SETT:
      into_section = (asection *)obj_textsec(abfd);
      break;
    case N_SETD:
      into_section = (asection *)obj_datasec(abfd);
      break;
    case N_SETB:
      into_section = (asection *)obj_bsssec(abfd);
      break;
    default:
      abort();
    }

    /* Build a relocation pointing into the constuctor section
       pointing at the symbol in the set vector specified */

    reloc->relent.addend = cache_ptr->symbol.value;
    cache_ptr->symbol.section =  into_section->symbol->section;
    reloc->relent.sym_ptr_ptr  = into_section->symbol_ptr_ptr;

	  
    /* We modify the symbol to belong to a section depending upon the
       name of the symbol - probably __CTOR__ or __DTOR__ but we don't
       really care, and add to the size of the section to contain a
       pointer to the symbol. Build a reloc entry to relocate to this
       symbol attached to this section.  */
	  
    section->flags = SEC_CONSTRUCTOR;

	  
    section->reloc_count++;
    section->alignment_power = 2;

    reloc->next = section->constructor_chain;
    section->constructor_chain = reloc;
    reloc->relent.address = section->_raw_size;
    section->_raw_size += sizeof(int *);

    reloc->relent.howto = howto_table_ext + CTOR_TABLE_RELOC_IDX;
    cache_ptr->symbol.flags |=  BSF_DEBUGGING  | BSF_CONSTRUCTOR;
  }
    break;
  default:
    if (cache_ptr->type ==  N_WARNING) 
    {
      /* This symbol is the text of a warning message, the next symbol
	 is the symbol to associate the warning with */
      cache_ptr->symbol.flags = BSF_DEBUGGING | BSF_WARNING;
      cache_ptr->symbol.value = (bfd_vma)((cache_ptr+1));
      /* We furgle with the next symbol in place. We don't want it to be undefined, we'll trample the type */
      (sym_pointer+1)->e_type[0] = 0xff;
      break;
    }
    if ((cache_ptr->type | N_EXT) == (N_INDR | N_EXT)) {
	/* Two symbols in a row for an INDR message. The first symbol
	   contains the name we will match, the second symbol contains the
	   name the first name is translated into. It is supplied to us
	   undefined. This is good, since we want to pull in any files which
	   define it */
	cache_ptr->symbol.flags = BSF_DEBUGGING | BSF_INDIRECT;
	cache_ptr->symbol.value = (bfd_vma)((cache_ptr+1));
	cache_ptr->symbol.section = &bfd_und_section;
	break;
      }

      
    if (sym_is_debugger_info (cache_ptr)) {
	cache_ptr->symbol.flags = BSF_DEBUGGING ;
	/* Work out the section correct for this symbol */
	switch (cache_ptr->type & N_TYPE) 
	{
	case N_TEXT:
	case N_FN:
	  cache_ptr->symbol.section = obj_textsec (abfd);
	  cache_ptr->symbol.value -= obj_textsec(abfd)->vma;
	  break;
	case N_DATA:
	  cache_ptr->symbol.value  -= obj_datasec(abfd)->vma;
	  cache_ptr->symbol.section = obj_datasec (abfd);
	  break;
	case N_BSS :
	  cache_ptr->symbol.section = obj_bsssec (abfd);
	  cache_ptr->symbol.value -= obj_bsssec(abfd)->vma;
	  break;
	default:
	case N_ABS:

	  cache_ptr->symbol.section = &bfd_abs_section;
	  break;
	}
      }
    else {

	if (sym_is_fortrancommon (cache_ptr))
	{
	  cache_ptr->symbol.flags = 0;
	  cache_ptr->symbol.section = &bfd_com_section;
	}
	else {


	  }
	  
	/* In a.out, the value of a symbol is always relative to the 
	 * start of the file, if this is a data symbol we'll subtract
	 * the size of the text section to get the section relative
	 * value. If this is a bss symbol (which would be strange)
	 * we'll subtract the size of the previous two sections
	 * to find the section relative address.
	 */
	  
	if (sym_in_text_section (cache_ptr))   {
	    cache_ptr->symbol.value -= obj_textsec(abfd)->vma;
	    cache_ptr->symbol.section = obj_textsec (abfd);
	  }
	else if (sym_in_data_section (cache_ptr)){
	    cache_ptr->symbol.value -= obj_datasec(abfd)->vma;
	    cache_ptr->symbol.section = obj_datasec (abfd);
	  }
	else if (sym_in_bss_section(cache_ptr)) {
	    cache_ptr->symbol.section = obj_bsssec (abfd);
	    cache_ptr->symbol.value -= obj_bsssec(abfd)->vma;
	  }
	else  if (sym_is_undefined (cache_ptr)) {
	    cache_ptr->symbol.flags = 0;
	    cache_ptr->symbol.section = &bfd_und_section;
	  }
	else if (sym_is_absolute(cache_ptr))
	{
	  cache_ptr->symbol.section = &bfd_abs_section;
	}
	    
	if (sym_is_global_defn (cache_ptr)) 
	{
	  cache_ptr->symbol.flags = BSF_GLOBAL | BSF_EXPORT;
	} 
	else 
	{
	  cache_ptr->symbol.flags = BSF_LOCAL;
	}
      }
  }
}



static void
DEFUN(translate_to_native_sym_flags,(sym_pointer, cache_ptr, abfd),
     struct external_nlist *sym_pointer AND
     asymbol *cache_ptr AND
     bfd *abfd)
{
  bfd_vma value = cache_ptr->value;

  /* mask out any existing type bits in case copying from one section
     to another */
  sym_pointer->e_type[0] &= ~N_TYPE;
  
  if (bfd_get_output_section(cache_ptr) == obj_bsssec (abfd)) {
      sym_pointer->e_type[0] |= N_BSS;
    }
  else if (bfd_get_output_section(cache_ptr) == obj_datasec (abfd)) {
      sym_pointer->e_type[0] |= N_DATA;
    }
  else  if (bfd_get_output_section(cache_ptr) == obj_textsec (abfd)) {
      sym_pointer->e_type[0] |= N_TEXT;
    }
  else if (bfd_get_output_section(cache_ptr) == &bfd_abs_section) 
  {
    sym_pointer->e_type[0] |= N_ABS;
  }
  else if (bfd_get_output_section(cache_ptr) == &bfd_und_section) 
  {
    sym_pointer->e_type[0] = (N_UNDF | N_EXT);
  }
  else if (bfd_get_output_section(cache_ptr) == &bfd_com_section) {
      sym_pointer->e_type[0] = (N_UNDF | N_EXT);
    }    
  else {    
      if (cache_ptr->section->output_section) 
      {
	
	bfd_error_vector.nonrepresentable_section(abfd,
						  bfd_get_output_section(cache_ptr)->name);
      }
      else 
      {
	bfd_error_vector.nonrepresentable_section(abfd,
						  cache_ptr->section->name);
	
      }
      
    }
  /* Turn the symbol from section relative to absolute again */
    
  value +=  cache_ptr->section->output_section->vma  + cache_ptr->section->output_offset ;


  if (cache_ptr->flags & (BSF_WARNING)) {
      (sym_pointer+1)->e_type[0] = 1;
    }  
    
  if (cache_ptr->flags & (BSF_GLOBAL | BSF_EXPORT)) {
      sym_pointer->e_type[0] |= N_EXT;
    }
  if (cache_ptr->flags & BSF_DEBUGGING) {
      sym_pointer->e_type [0]= ((aout_symbol_type *)cache_ptr)->type;
    }

  PUT_WORD(abfd, value, sym_pointer->e_value);
}

/* Native-level interface to symbols. */

/* We read the symbols into a buffer, which is discarded when this
function exits.  We read the strings into a buffer large enough to
hold them all plus all the cached symbol entries. */

asymbol *
DEFUN(NAME(aout,make_empty_symbol),(abfd),
      bfd *abfd)
{
  aout_symbol_type  *new =
    (aout_symbol_type *)bfd_zalloc (abfd, sizeof (aout_symbol_type));
  new->symbol.the_bfd = abfd;
    
  return &new->symbol;
}

boolean
DEFUN(NAME(aout,slurp_symbol_table),(abfd),
      bfd *abfd)
{
  bfd_size_type symbol_size;
  bfd_size_type string_size;
  unsigned char string_chars[BYTES_IN_WORD];
  struct external_nlist *syms;
  char *strings;
  aout_symbol_type *cached;
    
  /* If there's no work to be done, don't do any */
  if (obj_aout_symbols (abfd) != (aout_symbol_type *)NULL) return true;
  symbol_size = exec_hdr(abfd)->a_syms;
  if (symbol_size == 0) {
    bfd_error = no_symbols;
    return false;
  }
    
  bfd_seek (abfd, obj_str_filepos (abfd), SEEK_SET);
  if (bfd_read ((PTR)string_chars, BYTES_IN_WORD, 1, abfd) != BYTES_IN_WORD)
    return false;
  string_size = GET_WORD (abfd, string_chars);
    
  strings =(char *) bfd_alloc(abfd, string_size + 1);
  cached = (aout_symbol_type *)
    bfd_zalloc(abfd, (bfd_size_type)(bfd_get_symcount (abfd) * sizeof(aout_symbol_type)));

  /* malloc this, so we can free it if simply. The symbol caching
     might want to allocate onto the bfd's obstack  */
  syms = (struct external_nlist *) bfd_xmalloc(symbol_size);
  bfd_seek (abfd, obj_sym_filepos (abfd), SEEK_SET);
  if (bfd_read ((PTR)syms, 1, symbol_size, abfd) != symbol_size) {
  bailout:
    if (syms) 	free (syms);
    if (cached)	bfd_release (abfd, cached);
    if (strings)bfd_release (abfd, strings);
    return false;
  }
    
  bfd_seek (abfd, obj_str_filepos (abfd), SEEK_SET);
  if (bfd_read ((PTR)strings, 1, string_size, abfd) != string_size) {
    goto bailout;
  }
    
  /* OK, now walk the new symtable, cacheing symbol properties */
    {
      register struct external_nlist *sym_pointer;
      register struct external_nlist *sym_end = syms + bfd_get_symcount (abfd);
      register aout_symbol_type *cache_ptr = cached;
	
      /* Run through table and copy values */
      for (sym_pointer = syms, cache_ptr = cached;
	   sym_pointer < sym_end; sym_pointer++, cache_ptr++) 
	  {
	    bfd_vma x = GET_WORD(abfd, sym_pointer->e_strx);
	    cache_ptr->symbol.the_bfd = abfd;
	    if (x)
	      cache_ptr->symbol.name = x + strings;
	    else
	      cache_ptr->symbol.name = (char *)NULL;
	      
	    cache_ptr->symbol.value = GET_SWORD(abfd,  sym_pointer->e_value);
	    cache_ptr->desc = bfd_h_get_16(abfd, sym_pointer->e_desc);
	    cache_ptr->other = bfd_h_get_8(abfd, sym_pointer->e_other);
	    cache_ptr->type = bfd_h_get_8(abfd,  sym_pointer->e_type);
	    cache_ptr->symbol.udata = 0;
	    translate_from_native_sym_flags (sym_pointer, cache_ptr, abfd);
	  }
    }
    
  obj_aout_symbols (abfd) =  cached;
  free((PTR)syms);
    
  return true;
}


void
DEFUN(NAME(aout,write_syms),(abfd),
      bfd *abfd)
  {
    unsigned int count ;
    asymbol **generic = bfd_get_outsymbols (abfd);
    
    bfd_size_type stindex = BYTES_IN_WORD; /* initial string length */
    
    for (count = 0; count < bfd_get_symcount (abfd); count++) {
      asymbol *g = generic[count];
      struct external_nlist nsp;


      if (g->name) {
	unsigned int length = strlen(g->name) +1;
	PUT_WORD  (abfd, stindex, (unsigned char *)nsp.e_strx);
	stindex += length;
      }
      else 

      {
	PUT_WORD  (abfd, 0, (unsigned char *)nsp.e_strx);
      }
      
      if (g->the_bfd->xvec->flavour == abfd->xvec->flavour) 
	  {
	    bfd_h_put_16(abfd, aout_symbol(g)->desc,  nsp.e_desc);
	    bfd_h_put_8(abfd, aout_symbol(g)->other,  nsp.e_other);
	    bfd_h_put_8(abfd, aout_symbol(g)->type,  nsp.e_type);
	  }
      else
	  {
	    bfd_h_put_16(abfd,0, nsp.e_desc);
	    bfd_h_put_8(abfd, 0,  nsp.e_other);
	    bfd_h_put_8(abfd, 0,  nsp.e_type);
	  }

      translate_to_native_sym_flags (&nsp, g, abfd);

      bfd_write((PTR)&nsp,1,EXTERNAL_NLIST_SIZE, abfd);
    }
    
    /* Now output the strings.  Be sure to put string length into correct
       byte ordering before writing it.  */
      {
	char buffer[BYTES_IN_WORD];
	PUT_WORD  (abfd, stindex, (unsigned char *)buffer);
    
	bfd_write((PTR)buffer, 1, BYTES_IN_WORD, abfd);
      }
    generic = bfd_get_outsymbols(abfd);
    for (count = 0; count < bfd_get_symcount(abfd); count++) 
	{
	  asymbol *g = *(generic++);
	  
	  if (g->name)
	      {
		size_t length = strlen(g->name)+1;
		bfd_write((PTR)g->name, 1, length, abfd);
	      }
	    g->KEEPIT = (KEEPITTYPE) count;
	}
  }



unsigned int
DEFUN(NAME(aout,get_symtab),(abfd, location),
      bfd *abfd AND
      asymbol **location)
{
    unsigned int counter = 0;
    aout_symbol_type *symbase;

    if (!NAME(aout,slurp_symbol_table)(abfd)) return 0;

    for (symbase = obj_aout_symbols(abfd); counter++ < bfd_get_symcount (abfd);)
      *(location++) = (asymbol *)( symbase++);
    *location++ =0;
    return bfd_get_symcount (abfd);
}


/* Standard reloc stuff */
/* Output standard relocation information to a file in target byte order. */

void
DEFUN(NAME(aout,swap_std_reloc_out),(abfd, g, natptr),
      bfd *abfd AND
      arelent *g AND
      struct reloc_std_external *natptr)
{
  int r_index;
  asymbol *sym = *(g->sym_ptr_ptr);
  int r_extern;
  unsigned int r_length;
  int r_pcrel;
  int r_baserel, r_jmptable, r_relative;
  unsigned int r_addend;
  asection *output_section = sym->section->output_section;

  PUT_WORD(abfd, g->address, natptr->r_address);

  r_length = g->howto->size ;	/* Size as a power of two */
  r_pcrel  = (int) g->howto->pc_relative; /* Relative to PC? */
  /* r_baserel, r_jmptable, r_relative???  FIXME-soon */
  r_baserel = 0;
  r_jmptable = 0;
  r_relative = 0;
    
  r_addend = g->addend + (*(g->sym_ptr_ptr))->section->output_section->vma;
    
  /* name was clobbered by aout_write_syms to be symbol index */

  /* If this relocation is relative to a symbol then set the 
     r_index to the symbols index, and the r_extern bit.

     Absolute symbols can come in in two ways, either as an offset
     from the abs section, or as a symbol which has an abs value.
     check for that here
     */
     

  if (output_section == &bfd_com_section 
      || output_section == &bfd_abs_section
      || output_section == &bfd_und_section) 
    {
      if (bfd_abs_section.symbol == sym)
      {
	/* Whoops, looked like an abs symbol, but is really an offset
	   from the abs section */
	r_index = 0;
	r_extern = 0;
       }
      else 
      {
	/* Fill in symbol */
	r_extern = 1;
	r_index =  stoi((*(g->sym_ptr_ptr))->KEEPIT);
     
      }
    }
  else 
    {
      /* Just an ordinary section */
      r_extern = 0;
      r_index  = output_section->target_index;      
    }

  /* now the fun stuff */
  if (abfd->xvec->header_byteorder_big_p != false) {
      natptr->r_index[0] = r_index >> 16;
      natptr->r_index[1] = r_index >> 8;
      natptr->r_index[2] = r_index;
      natptr->r_type[0] =
       (r_extern?    RELOC_STD_BITS_EXTERN_BIG: 0)
	| (r_pcrel?     RELOC_STD_BITS_PCREL_BIG: 0)
	 | (r_baserel?   RELOC_STD_BITS_BASEREL_BIG: 0)
	  | (r_jmptable?  RELOC_STD_BITS_JMPTABLE_BIG: 0)
	   | (r_relative?  RELOC_STD_BITS_RELATIVE_BIG: 0)
	    | (r_length <<  RELOC_STD_BITS_LENGTH_SH_BIG);
    } else {
	natptr->r_index[2] = r_index >> 16;
	natptr->r_index[1] = r_index >> 8;
	natptr->r_index[0] = r_index;
	natptr->r_type[0] =
	 (r_extern?    RELOC_STD_BITS_EXTERN_LITTLE: 0)
	  | (r_pcrel?     RELOC_STD_BITS_PCREL_LITTLE: 0)
	   | (r_baserel?   RELOC_STD_BITS_BASEREL_LITTLE: 0)
	    | (r_jmptable?  RELOC_STD_BITS_JMPTABLE_LITTLE: 0)
	     | (r_relative?  RELOC_STD_BITS_RELATIVE_LITTLE: 0)
	      | (r_length <<  RELOC_STD_BITS_LENGTH_SH_LITTLE);
      }
}


/* Extended stuff */
/* Output extended relocation information to a file in target byte order. */

void
DEFUN(NAME(aout,swap_ext_reloc_out),(abfd, g, natptr),
      bfd *abfd AND
      arelent *g AND
      register struct reloc_ext_external *natptr)
{
  int r_index;
  int r_extern;
  unsigned int r_type;
  unsigned int r_addend;
  asymbol *sym = *(g->sym_ptr_ptr);    
  asection *output_section = sym->section->output_section;
  
  PUT_WORD (abfd, g->address, natptr->r_address);
    
  r_type = (unsigned int) g->howto->type;
    
  r_addend = g->addend + (*(g->sym_ptr_ptr))->section->output_section->vma;


  /* If this relocation is relative to a symbol then set the 
     r_index to the symbols index, and the r_extern bit.

     Absolute symbols can come in in two ways, either as an offset
     from the abs section, or as a symbol which has an abs value.
     check for that here
     */
     
  if (output_section == &bfd_com_section 
      || output_section == &bfd_abs_section
      || output_section == &bfd_und_section) 
  {
    if (bfd_abs_section.symbol == sym)
    {
      /* Whoops, looked like an abs symbol, but is really an offset
	 from the abs section */
      r_index = 0;
      r_extern = 0;
     }
    else 
    {
      r_extern = 1;
      r_index =  stoi((*(g->sym_ptr_ptr))->KEEPIT);
    }
  }
  else 
  {
    /* Just an ordinary section */
    r_extern = 0;
    r_index  = output_section->target_index;      
  }
	 
	 
  /* now the fun stuff */
  if (abfd->xvec->header_byteorder_big_p != false) {
    natptr->r_index[0] = r_index >> 16;
    natptr->r_index[1] = r_index >> 8;
    natptr->r_index[2] = r_index;
    natptr->r_type[0] =
     (r_extern? RELOC_EXT_BITS_EXTERN_BIG: 0)
      | (r_type << RELOC_EXT_BITS_TYPE_SH_BIG);
  } else {
    natptr->r_index[2] = r_index >> 16;
    natptr->r_index[1] = r_index >> 8;
    natptr->r_index[0] = r_index;
    natptr->r_type[0] =
     (r_extern? RELOC_EXT_BITS_EXTERN_LITTLE: 0)
      | (r_type << RELOC_EXT_BITS_TYPE_SH_LITTLE);
  }

  PUT_WORD (abfd, r_addend, natptr->r_addend);
}

/* BFD deals internally with all things based from the section they're
   in. so, something in 10 bytes into a text section  with a base of
   50 would have a symbol (.text+10) and know .text vma was 50. 

   Aout keeps all it's symbols based from zero, so the symbol would
   contain 60. This macro subs the base of each section from the value
   to give the true offset from the section */


#define MOVE_ADDRESS(ad)       						\
  if (r_extern) {							\
   /* undefined symbol */						\
     cache_ptr->sym_ptr_ptr = symbols + r_index;			\
     cache_ptr->addend = ad;						\
     } else {								\
    /* defined, section relative. replace symbol with pointer to    	\
       symbol which points to section  */				\
    switch (r_index) {							\
    case N_TEXT:							\
    case N_TEXT | N_EXT:						\
      cache_ptr->sym_ptr_ptr  = obj_textsec(abfd)->symbol_ptr_ptr;	\
      cache_ptr->addend = ad  - su->textsec->vma;			\
      break;								\
    case N_DATA:							\
    case N_DATA | N_EXT:						\
      cache_ptr->sym_ptr_ptr  = obj_datasec(abfd)->symbol_ptr_ptr;	\
      cache_ptr->addend = ad - su->datasec->vma;			\
      break;								\
    case N_BSS:								\
    case N_BSS | N_EXT:							\
      cache_ptr->sym_ptr_ptr  = obj_bsssec(abfd)->symbol_ptr_ptr;	\
      cache_ptr->addend = ad - su->bsssec->vma;				\
      break;								\
    default:								\
    case N_ABS:								\
    case N_ABS | N_EXT:							\
     cache_ptr->sym_ptr_ptr = bfd_abs_section.symbol_ptr_ptr;	\
      cache_ptr->addend = ad;						\
      break;								\
    }									\
  }     								\

void
DEFUN(NAME(aout,swap_ext_reloc_in), (abfd, bytes, cache_ptr, symbols),
      bfd *abfd AND
      struct reloc_ext_external *bytes AND
      arelent *cache_ptr AND
      asymbol **symbols)
{
  int r_index;
  int r_extern;
  unsigned int r_type;
  struct aoutdata *su = &(abfd->tdata.aout_data->a);

  cache_ptr->address = (GET_SWORD (abfd, bytes->r_address));

  /* now the fun stuff */
  if (abfd->xvec->header_byteorder_big_p != false) {
    r_index =  (bytes->r_index[0] << 16)
	     | (bytes->r_index[1] << 8)
	     |  bytes->r_index[2];
    r_extern = (0 != (bytes->r_type[0] & RELOC_EXT_BITS_EXTERN_BIG));
    r_type   =       (bytes->r_type[0] & RELOC_EXT_BITS_TYPE_BIG)
				      >> RELOC_EXT_BITS_TYPE_SH_BIG;
  } else {
    r_index =  (bytes->r_index[2] << 16)
	     | (bytes->r_index[1] << 8)
	     |  bytes->r_index[0];
    r_extern = (0 != (bytes->r_type[0] & RELOC_EXT_BITS_EXTERN_LITTLE));
    r_type   =       (bytes->r_type[0] & RELOC_EXT_BITS_TYPE_LITTLE)
				      >> RELOC_EXT_BITS_TYPE_SH_LITTLE;
  }

  cache_ptr->howto =  howto_table_ext + r_type;
  MOVE_ADDRESS(GET_SWORD(abfd, bytes->r_addend));
}

void
DEFUN(NAME(aout,swap_std_reloc_in), (abfd, bytes, cache_ptr, symbols),
  bfd *abfd AND
  struct reloc_std_external *bytes AND
  arelent *cache_ptr AND
  asymbol **symbols)
{
  int r_index;
  int r_extern;
  unsigned int r_length;
  int r_pcrel;
  int r_baserel, r_jmptable, r_relative;
  struct aoutdata  *su = &(abfd->tdata.aout_data->a);

  cache_ptr->address = (int32_type)(bfd_h_get_32 (abfd, bytes->r_address));

  /* now the fun stuff */
  if (abfd->xvec->header_byteorder_big_p != false) {
    r_index =  (bytes->r_index[0] << 16)
      | (bytes->r_index[1] << 8)
	|  bytes->r_index[2];
    r_extern  = (0 != (bytes->r_type[0] & RELOC_STD_BITS_EXTERN_BIG));
    r_pcrel   = (0 != (bytes->r_type[0] & RELOC_STD_BITS_PCREL_BIG));
    r_baserel = (0 != (bytes->r_type[0] & RELOC_STD_BITS_BASEREL_BIG));
    r_jmptable= (0 != (bytes->r_type[0] & RELOC_STD_BITS_JMPTABLE_BIG));
    r_relative= (0 != (bytes->r_type[0] & RELOC_STD_BITS_RELATIVE_BIG));
    r_length  =       (bytes->r_type[0] & RELOC_STD_BITS_LENGTH_BIG) 
      			>> RELOC_STD_BITS_LENGTH_SH_BIG;
  } else {
    r_index =  (bytes->r_index[2] << 16)
      | (bytes->r_index[1] << 8)
	|  bytes->r_index[0];
    r_extern  = (0 != (bytes->r_type[0] & RELOC_STD_BITS_EXTERN_LITTLE));
    r_pcrel   = (0 != (bytes->r_type[0] & RELOC_STD_BITS_PCREL_LITTLE));
    r_baserel = (0 != (bytes->r_type[0] & RELOC_STD_BITS_BASEREL_LITTLE));
    r_jmptable= (0 != (bytes->r_type[0] & RELOC_STD_BITS_JMPTABLE_LITTLE));
    r_relative= (0 != (bytes->r_type[0] & RELOC_STD_BITS_RELATIVE_LITTLE));
    r_length  =       (bytes->r_type[0] & RELOC_STD_BITS_LENGTH_LITTLE) 
      			>> RELOC_STD_BITS_LENGTH_SH_LITTLE;
  }

  cache_ptr->howto =  howto_table_std + r_length + 4 * r_pcrel;
  /* FIXME-soon:  Roll baserel, jmptable, relative bits into howto setting */

  MOVE_ADDRESS(0);
}

/* Reloc hackery */

boolean
DEFUN(NAME(aout,slurp_reloc_table),(abfd, asect, symbols),
      bfd *abfd AND
      sec_ptr asect AND
      asymbol **symbols)
{
  unsigned int count;
  bfd_size_type reloc_size;
  PTR relocs;
  arelent *reloc_cache;
  size_t each_size;

  if (asect->relocation) return true;

  if (asect->flags & SEC_CONSTRUCTOR) return true;

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
  bfd_seek (abfd, asect->rel_filepos, SEEK_SET);
  each_size = obj_reloc_entry_size (abfd);

  count = reloc_size / each_size;


  reloc_cache = (arelent *) bfd_zalloc (abfd, (size_t)(count * sizeof
						       (arelent)));
  if (!reloc_cache) {
nomem:
    bfd_error = no_memory;
    return false;
  }

  relocs = (PTR) bfd_alloc (abfd, reloc_size);
  if (!relocs) {
    bfd_release (abfd, reloc_cache);
    goto nomem;
  }

  if (bfd_read (relocs, 1, reloc_size, abfd) != reloc_size) {
    bfd_release (abfd, relocs);
    bfd_release (abfd, reloc_cache);
    bfd_error = system_call_error;
    return false;
  }

  if (each_size == RELOC_EXT_SIZE) {
    register struct reloc_ext_external *rptr = (struct reloc_ext_external *) relocs;
    unsigned int counter = 0;
    arelent *cache_ptr = reloc_cache;

    for (; counter < count; counter++, rptr++, cache_ptr++) {
      NAME(aout,swap_ext_reloc_in)(abfd, rptr, cache_ptr, symbols);
    }
  } else {
    register struct reloc_std_external *rptr = (struct reloc_std_external*) relocs;
    unsigned int counter = 0;
    arelent *cache_ptr = reloc_cache;

    for (; counter < count; counter++, rptr++, cache_ptr++) {
	NAME(aout,swap_std_reloc_in)(abfd, rptr, cache_ptr, symbols);
    }

  }

  bfd_release (abfd,relocs);
  asect->relocation = reloc_cache;
  asect->reloc_count = count;
  return true;
}



/* Write out a relocation section into an object file.  */

boolean
DEFUN(NAME(aout,squirt_out_relocs),(abfd, section),
      bfd *abfd AND
      asection *section)
{
  arelent **generic;
  unsigned char *native, *natptr;
  size_t each_size;

  unsigned int count = section->reloc_count;
  size_t natsize;

  if (count == 0) return true;

  each_size = obj_reloc_entry_size (abfd);
  natsize = each_size * count;
  native = (unsigned char *) bfd_zalloc (abfd, natsize);
  if (!native) {
    bfd_error = no_memory;
    return false;
  }

  generic = section->orelocation;

  if (each_size == RELOC_EXT_SIZE) 
    {
      for (natptr = native;
	   count != 0;
	   --count, natptr += each_size, ++generic)
	NAME(aout,swap_ext_reloc_out) (abfd, *generic, (struct reloc_ext_external *)natptr);
    }
  else 
    {
      for (natptr = native;
	   count != 0;
	   --count, natptr += each_size, ++generic)
	NAME(aout,swap_std_reloc_out)(abfd, *generic, (struct reloc_std_external *)natptr);
    }

  if ( bfd_write ((PTR) native, 1, natsize, abfd) != natsize) {
    bfd_release(abfd, native);
    return false;
  }
  bfd_release (abfd, native);

  return true;
}

/* This is stupid.  This function should be a boolean predicate */
unsigned int
DEFUN(NAME(aout,canonicalize_reloc),(abfd, section, relptr, symbols),
      bfd *abfd AND
      sec_ptr section AND
      arelent **relptr AND
      asymbol **symbols)
{
  arelent *tblptr = section->relocation;
  unsigned int count;

  if (!(tblptr || NAME(aout,slurp_reloc_table)(abfd, section, symbols)))
    return 0;

  if (section->flags & SEC_CONSTRUCTOR) {
    arelent_chain *chain = section->constructor_chain;
    for (count = 0; count < section->reloc_count; count ++) {
      *relptr ++ = &chain->relent;
      chain = chain->next;
    }
  }
  else {
    tblptr = section->relocation;
    if (!tblptr) return 0;

    for (count = 0; count++ < section->reloc_count;) 
      {
	*relptr++ = tblptr++;
      }
  }
  *relptr = 0;

  return section->reloc_count;
}

unsigned int
DEFUN(NAME(aout,get_reloc_upper_bound),(abfd, asect),
     bfd *abfd AND
     sec_ptr asect)
{
  if (bfd_get_format (abfd) != bfd_object) {
    bfd_error = invalid_operation;
    return 0;
  }
  if (asect->flags & SEC_CONSTRUCTOR) {
    return (sizeof (arelent *) * (asect->reloc_count+1));
  }


  if (asect == obj_datasec (abfd))
    return (sizeof (arelent *) *
            ((exec_hdr(abfd)->a_drsize / obj_reloc_entry_size (abfd))
             +1));

  if (asect == obj_textsec (abfd))
    return (sizeof (arelent *) *
            ((exec_hdr(abfd)->a_trsize / obj_reloc_entry_size (abfd))
             +1));

  bfd_error = invalid_operation;
  return 0;
}


 unsigned int
DEFUN(NAME(aout,get_symtab_upper_bound),(abfd),
     bfd *abfd)
{
  if (!NAME(aout,slurp_symbol_table)(abfd)) return 0;

  return (bfd_get_symcount (abfd)+1) * (sizeof (aout_symbol_type *));
}
 alent *
DEFUN(NAME(aout,get_lineno),(ignore_abfd, ignore_symbol),
      bfd *ignore_abfd AND
      asymbol *ignore_symbol)
{
return (alent *)NULL;
}


void 
DEFUN(NAME(aout,print_symbol),(ignore_abfd, afile, symbol, how),
      bfd *ignore_abfd AND
      PTR afile AND
      asymbol *symbol AND
      bfd_print_symbol_type how)
{
  FILE *file = (FILE *)afile;

  switch (how) {
  case bfd_print_symbol_name:
    if (symbol->name)
      fprintf(file,"%s", symbol->name);
    break;
  case bfd_print_symbol_more:
    fprintf(file,"%4x %2x %2x",(unsigned)(aout_symbol(symbol)->desc & 0xffff),
	    (unsigned)(aout_symbol(symbol)->other & 0xff),
	    (unsigned)(aout_symbol(symbol)->type));
    break;
  case bfd_print_symbol_all:
    {
   CONST char *section_name = symbol->section->name;


      bfd_print_symbol_vandf((PTR)file,symbol);

      fprintf(file," %-5s %04x %02x %02x",
	      section_name,
	      (unsigned)(aout_symbol(symbol)->desc & 0xffff),
	      (unsigned)(aout_symbol(symbol)->other & 0xff),
	      (unsigned)(aout_symbol(symbol)->type  & 0xff));
      if (symbol->name)
        fprintf(file," %s", symbol->name);
    }
    break;
  case bfd_print_symbol_nm:
    {
      int section_code = bfd_decode_symclass  (symbol);

      if (section_code == 'U')
	fprintf(file, "        ");
      else
	fprintf_vma(file, symbol->value+symbol->section->vma);
      if (section_code == '?')
	{
	  int type_code = aout_symbol(symbol)->type  & 0xff;
	  char *stab_name = aout_stab_name(type_code);
	  char buf[10];
	  if (stab_name == NULL)
	    {
	      sprintf(buf, "(%d)", type_code);
	      stab_name = buf;
	    }
	  fprintf(file," - %02x %04x %5s",
		  (unsigned)(aout_symbol(symbol)->other & 0xff),
		  (unsigned)(aout_symbol(symbol)->desc & 0xffff),
		  stab_name);
        }
      else
	fprintf(file," %c", section_code);
      if (symbol->name)
        fprintf(file," %s", symbol->name);
    }
    break;
  }
}

/* 
 provided a BFD, a section and an offset into the section, calculate
 and return the name of the source file and the line nearest to the
 wanted location.
*/
 
boolean
DEFUN(NAME(aout,find_nearest_line),(abfd,
				     section,
				     symbols,
				     offset,
				     filename_ptr,
				     functionname_ptr,
				     line_ptr),
      bfd *abfd AND
      asection *section AND
      asymbol **symbols AND
      bfd_vma offset AND
      CONST char **filename_ptr AND
      CONST char **functionname_ptr AND
      unsigned int *line_ptr)
{
  /* Run down the file looking for the filename, function and linenumber */
  asymbol **p;
  static  char buffer[100];
  static  char filename_buffer[200];
  CONST char *directory_name = NULL;
  CONST char *main_file_name = NULL;
  CONST char *current_file_name = NULL;
  CONST char *line_file_name = NULL; /* Value of current_file_name at line number. */
  bfd_vma high_line_vma = ~0;
  bfd_vma low_func_vma = 0;
  asymbol *func = 0;
  *filename_ptr = abfd->filename;
  *functionname_ptr = 0;
  *line_ptr = 0;
  if (symbols != (asymbol **)NULL) {
    for (p = symbols; *p; p++) {
      aout_symbol_type  *q = (aout_symbol_type *)(*p);
    next:
      switch (q->type){
      case N_SO:
	main_file_name = current_file_name = q->symbol.name;
	/* Look ahead to next symbol to check if that too is an N_SO. */
	p++;
	if (*p == NULL)
	  break;
	q = (aout_symbol_type *)(*p);
	if (q->type != (int)N_SO)
	  goto next;

	/* Found a second N_SO  First is directory; second is filename. */
	directory_name = current_file_name;
	main_file_name = current_file_name = q->symbol.name;
	if (obj_textsec(abfd) != section)
	  goto done;
	break;
      case N_SOL:
	current_file_name = q->symbol.name;
	break;

      case N_SLINE:

      case N_DSLINE:
      case N_BSLINE:
	/* We'll keep this if it resolves nearer than the one we have already */
	if (q->symbol.value >= offset &&
	    q->symbol.value < high_line_vma) {
	  *line_ptr = q->desc;
	  high_line_vma = q->symbol.value;
	  line_file_name = current_file_name;
	}
	break;
      case N_FUN:
	{
	  /* We'll keep this if it is nearer than the one we have already */
	  if (q->symbol.value >= low_func_vma &&
	      q->symbol.value <= offset) {
	    low_func_vma = q->symbol.value;
	    func = (asymbol *)q;
	  }
	  if (*line_ptr && func) {
	    CONST char *function = func->name;
	    char *p;
	    strncpy(buffer, function, sizeof(buffer)-1);
	    buffer[sizeof(buffer)-1] = 0;
	    /* Have to remove : stuff */
	    p = strchr(buffer,':');
	    if (p != NULL) { *p = '\0'; }
	    *functionname_ptr = buffer;
	    goto done;

	  }
	}
	break;
      }
    }
  }

 done:
  if (*line_ptr)
    main_file_name = line_file_name;
  if (main_file_name) {
      if (main_file_name[0] == '/' || directory_name == NULL)
	  *filename_ptr = main_file_name;
      else {
	  sprintf(filename_buffer, "%.140s%.50s",
		  directory_name, main_file_name);
	  *filename_ptr = filename_buffer;
      }
  }
  return true;

}

int 
DEFUN(NAME(aout,sizeof_headers),(abfd, execable),
      bfd *abfd AND
      boolean execable)
{
  return adata(abfd).exec_bytes_size;
}
