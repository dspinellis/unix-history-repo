/* Support for the generic parts of most COFF variants, for BFD.
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
Most of this hacked by  Steve Chamberlain,
			sac@cygnus.com 
*/
/*

SECTION
	coff backends

	BFD supports a number of different flavours of coff format.
	The major difference between formats are the sizes and
	alignments of fields in structures on disk, and the occasional
	extra field.  

	Coff in all its varieties is implimented with a few common
	files and a number of implementation specific files. For
	example, The 88k bcs coff format is implemented in the file
	@code{coff-m88k.c}. This file @code{#include}s
	@code{coff-m88k.h} which defines the external structure of the
	coff format for the 88k, and @code{internalcoff.h} which
	defines the internal structure. @code{coff-m88k.c} also
	defines pthe relocations used by the 88k format
	@xref{Relocations}. Then the major portion of coff code is
	included (@code{coffcode.h}) which defines the methods used to
	act upon the types defined in @code{coff-m88k.h} and
	@code{internalcoff.h}.


	The Intel i960 processor version of coff is implemented in
	@code{coff-i960.c}. This file has the same structure as
	@code{coff-m88k.c}, except that it includes @code{coff-i960.h}
	rather than @code{coff-m88k.h}. 

SUBSECTION
	Porting To A New Version of Coff

	The recommended method is to select from the existing
	implimentations the version of coff which is most like the one
	you want to use, for our purposes, we'll say that i386 coff is
	the one you select, and that your coff flavour is called foo.
	Copy the @code{i386coff.c} to @code{foocoff.c}, copy
	@code{../include/i386coff.h} to @code{../include/foocoff.h}
	and add the lines to @code{targets.c} and @code{Makefile.in}
	so that your new back end is used. Alter the shapes of the
	structures in @code{../include/foocoff.h} so that they match
	what you need. You will probably also have to add
	@code{#ifdef}s to the code in @code{internalcoff.h} and
	@code{coffcode.h} if your version of coff is too wild. 

	You can verify that your new BFD backend works quite simply by
	building @code{objdump} from the @code{binutils} directory,
	and making sure that its version of what's going on at your
	host systems idea (assuming it has the pretty standard coff
	dump utility (usually called @code{att-dump} or just
	@code{dump})) are the same.  Then clean up your code, and send
	what you've done to Cygnus. Then your stuff will be in the
	next release, and you won't have to keep integrating it.

SUBSECTION
	How The Coff Backend Works

SUBSUBSECTION
	Bit Twiddling

	Each flavour of coff supported in BFD has its own header file
	descibing the external layout of the structures. There is also
	an internal description of the coff layout (in
	@code{internalcoff.h}) file (@code{}). A major function of the
	coff backend is swapping the bytes and twiddling the bits to
	translate the external form of the structures into the normal
	internal form. This is all performed in the
	@code{bfd_swap}_@i{thing}_@i{direction} routines. Some
	elements are different sizes between different versions of
	coff, it is the duty of the coff version specific include file
	to override the definitions of various packing routines in
	@code{coffcode.h}. Eg the size of line number entry in coff is
	sometimes 16 bits, and sometimes 32 bits. @code{#define}ing
	@code{PUT_LNSZ_LNNO} and @code{GET_LNSZ_LNNO} will select the
	correct one. No doubt, some day someone will find a version of
	coff which has a varying field size not catered for at the
	moment. To port BFD, that person will have to add more @code{#defines}.  
	Three of the bit twiddling routines are exported to
	@code{gdb}; @code{coff_swap_aux_in}, @code{coff_swap_sym_in}
	and @code{coff_swap_linno_in}. @code{GDB} reads the symbol
	table on its own, but uses BFD to fix things up.  More of the
	bit twiddlers are exported for @code{gas};
	@code{coff_swap_aux_out}, @code{coff_swap_sym_out},
	@code{coff_swap_lineno_out}, @code{coff_swap_reloc_out},
	@code{coff_swap_filehdr_out}, @code{coff_swap_aouthdr_out},
	@code{coff_swap_scnhdr_out}. @code{Gas} currently keeps track
	of all the symbol table and reloc drudgery itself, thereby
	saving the internal BFD overhead, but uses BFD to swap things
	on the way out, making cross ports much safer.  This also
	allows BFD (and thus the linker) to use the same header files
	as @code{gas}, which makes one avenue to disaster disappear.

SUBSUBSECTION
	Symbol Reading

	The simple canonical form for symbols used by BFD is not rich
	enough to keep all the information available in a coff symbol
	table. The back end gets around this by keeping the original
	symbol table around, "behind the scenes". 

	When a symbol table is requested (through a call to
	@code{bfd_canonicalize_symtab}, a request gets through to
	@code{get_normalized_symtab}. This reads the symbol table from
	the coff file and swaps all the structures inside into the
	internal form. It also fixes up all the pointers in the table
	(represented in the file by offsets from the first symbol in
	the table) into physical pointers to elements in the new
	internal table. This involves some work since the meanings of
	fields changes depending upon context; a field that is a
	pointer to another structure in the symbol table at one moment
	may be the size in bytes of a structure in the next.  Another
	pass is made over the table. All symbols which mark file names
	(<<C_FILE>> symbols) are modified so that the internal
	string points to the value in the auxent (the real filename)
	rather than the normal text associated with the symbol
	(@code{".file"}). 

	At this time the symbol names are moved around. Coff stores
	all symbols less than nine characters long physically
	within the symbol table, longer strings are kept at the end of
	the file in the string 	table. This pass moves all strings
	into memory, and replaces them with pointers to the strings.


	The symbol table is massaged once again, this time to create
	the canonical table used by the BFD application. Each symbol
	is inspected in turn, and a decision made (using the
	@code{sclass} field) about the various flags to set in the
	@code{asymbol} @xref{Symbols}. The generated canonical table
	shares strings with the hidden internal symbol table. 

	Any linenumbers are read from the coff file too, and attached
	to the symbols which own the functions the linenumbers belong to. 

SUBSUBSECTION
	Symbol Writing

	Writing a symbol to a coff file which didn't come from a coff
	file will lose any debugging information. The @code{asymbol}
	structure remembers the BFD from which was born, and on output
	the back end makes sure that the same destination target as
	source target is present.

	When the symbols have come from a coff file then all the
	debugging information is preserved.

	Symbol tables are provided for writing to the back end in a
	vector of pointers to pointers. This allows applications like
	the linker to accumulate and output large symbol tables
	without having to do too much byte copying.

	This function runs through the provided symbol table and
	patches each symbol marked as a file place holder
	(@code{C_FILE}) to point to the next file place holder in the
	list. It also marks each @code{offset} field in the list with
	the offset from the first symbol of the current symbol.

	Another function of this procedure is to turn the canonical
	value form of BFD into the form used by coff. Internally, BFD
	expects symbol values to be offsets from a section base; so a
	symbol physically at 0x120, but in a section starting at
	0x100, would have the value 0x20. Coff expects symbols to
	contain their final value, so symbols have their values
	changed at this point to reflect their sum with their owning
	section. Note that this transformation uses the
	<<output_section>> field of the @code{asymbol}'s
	@code{asection} @xref{Sections}. 

	o coff_mangle_symbols

	This routine runs though the provided symbol table and uses
	the offsets generated by the previous pass and the pointers
	generated when the symbol table was read in to create the
	structured hierachy required by coff. It changes each pointer
	to a symbol to an index into the symbol table of the symbol
	being referenced. 

	o coff_write_symbols

	This routine runs through the symbol table and patches up the
	symbols from their internal form into the coff way, calls the
	bit twiddlers and writes out the tabel to the file. 

*/

/*
INTERNAL_DEFINITION
	coff_symbol_type

DESCRIPTION
	The hidden information for an asymbol is described in a
	coff_ptr_struct, which is typedefed to a combined_entry_type

CODE_FRAGMENT
.
.typedef struct coff_ptr_struct 
.{
.
.       {* Remembers the offset from the first symbol in the file for
.          this symbol. Generated by coff_renumber_symbols. *}
.unsigned int offset;
.
.       {* Should the tag field of this symbol be renumbered.
.          Created by coff_pointerize_aux. *}
.char fix_tag;
.
.       {* Should the endidx field of this symbol be renumbered.
.          Created by coff_pointerize_aux. *}
.char fix_end;
.
.       {* The container for the symbol structure as read and translated
.           from the file. *}
.
.union {
.   union internal_auxent auxent;
.   struct internal_syment syment;
. } u;
.} combined_entry_type;
.
.
.{* Each canonical asymbol really looks like this: *}
.
.typedef struct coff_symbol_struct
.{
.   {* The actual symbol which the rest of BFD works with *}
.asymbol symbol;
.
.   {* A pointer to the hidden information for this symbol *}
.combined_entry_type *native;
.
.   {* A pointer to the linenumber information for this symbol *}
.struct lineno_cache_entry *lineno;
.
.   {* Have the line numbers been relocated yet ? *}
.boolean done_lineno;
.} coff_symbol_type;


*/

#include "seclet.h"
extern bfd_error_vector_type bfd_error_vector;




#define PUTWORD bfd_h_put_32
#define PUTHALF bfd_h_put_16
#define	PUTBYTE bfd_h_put_8

#ifndef GET_FCN_LNNOPTR
#define GET_FCN_LNNOPTR(abfd, ext)  bfd_h_get_32(abfd, (bfd_byte *) ext->x_sym.x_fcnary.x_fcn.x_lnnoptr)
#endif

#ifndef GET_FCN_ENDNDX
#define GET_FCN_ENDNDX(abfd, ext)  bfd_h_get_32(abfd, (bfd_byte *) ext->x_sym.x_fcnary.x_fcn.x_endndx)
#endif

#ifndef PUT_FCN_LNNOPTR
#define PUT_FCN_LNNOPTR(abfd, in, ext)  PUTWORD(abfd,  in, (bfd_byte *) ext->x_sym.x_fcnary.x_fcn.x_lnnoptr)
#endif
#ifndef PUT_FCN_ENDNDX
#define PUT_FCN_ENDNDX(abfd, in, ext) PUTWORD(abfd, in, (bfd_byte *) ext->x_sym.x_fcnary.x_fcn.x_endndx)
#endif
#ifndef GET_LNSZ_LNNO
#define GET_LNSZ_LNNO(abfd, ext) bfd_h_get_16(abfd, (bfd_byte *) ext->x_sym.x_misc.x_lnsz.x_lnno)
#endif
#ifndef GET_LNSZ_SIZE
#define GET_LNSZ_SIZE(abfd, ext) bfd_h_get_16(abfd, (bfd_byte *) ext->x_sym.x_misc.x_lnsz.x_size)
#endif
#ifndef PUT_LNSZ_LNNO
#define PUT_LNSZ_LNNO(abfd, in, ext) bfd_h_put_16(abfd, in, (bfd_byte *)ext->x_sym.x_misc.x_lnsz.x_lnno)
#endif
#ifndef PUT_LNSZ_SIZE
#define PUT_LNSZ_SIZE(abfd, in, ext) bfd_h_put_16(abfd, in, (bfd_byte*) ext->x_sym.x_misc.x_lnsz.x_size)
#endif
#ifndef GET_SCN_SCNLEN
#define GET_SCN_SCNLEN(abfd,  ext) bfd_h_get_32(abfd, (bfd_byte *) ext->x_scn.x_scnlen)
#endif
#ifndef GET_SCN_NRELOC
#define GET_SCN_NRELOC(abfd,  ext) bfd_h_get_16(abfd, (bfd_byte *)ext->x_scn.x_nreloc)
#endif
#ifndef GET_SCN_NLINNO
#define GET_SCN_NLINNO(abfd, ext)  bfd_h_get_16(abfd, (bfd_byte *)ext->x_scn.x_nlinno)
#endif
#ifndef PUT_SCN_SCNLEN
#define PUT_SCN_SCNLEN(abfd,in, ext) bfd_h_put_32(abfd, in, (bfd_byte *) ext->x_scn.x_scnlen)
#endif
#ifndef PUT_SCN_NRELOC
#define PUT_SCN_NRELOC(abfd,in, ext) bfd_h_put_16(abfd, in, (bfd_byte *)ext->x_scn.x_nreloc)
#endif
#ifndef PUT_SCN_NLINNO
#define PUT_SCN_NLINNO(abfd,in, ext)  bfd_h_put_16(abfd,in, (bfd_byte  *) ext->x_scn.x_nlinno)
#endif
#ifndef GET_LINENO_LNNO
#define GET_LINENO_LNNO(abfd, ext) bfd_h_get_16(abfd, (bfd_byte *) (ext->l_lnno));
#endif
#ifndef PUT_LINENO_LNNO
#define PUT_LINENO_LNNO(abfd,val, ext) bfd_h_put_16(abfd,val,  (bfd_byte *) (ext->l_lnno));
#endif


/* void warning(); */

/*
 * Return a word with STYP_* (scnhdr.s_flags) flags set to represent the
 * incoming SEC_* flags.  The inverse of this function is styp_to_sec_flags().
 * NOTE: If you add to/change this routine, you should mirror the changes
 * 	in styp_to_sec_flags().
 */
static long
DEFUN(sec_to_styp_flags, (sec_name, sec_flags),
	CONST char *		sec_name	AND
	flagword	sec_flags)
{
    long styp_flags = 0;

    if (!strcmp(sec_name, _TEXT)) {
	return((long)STYP_TEXT);
    } else if (!strcmp(sec_name, _DATA)) {
	return((long)STYP_DATA);
    } else if (!strcmp(sec_name, _BSS)) {
	return((long)STYP_BSS);
#ifdef _COMMENT
    } else if (!strcmp(sec_name, _COMMENT)) {
        return((long)STYP_INFO);
#endif /* _COMMENT */
    }

/* Try and figure out what it should be */
   if (sec_flags & SEC_CODE) styp_flags = STYP_TEXT;
   if (sec_flags & SEC_DATA) styp_flags = STYP_DATA;
   else if (sec_flags & SEC_READONLY)
#ifdef STYP_LIT	/* 29k readonly text/data section */
   	styp_flags = STYP_LIT;
#else
   	styp_flags = STYP_TEXT;
#endif	/* STYP_LIT */
   else if (sec_flags & SEC_LOAD) styp_flags = STYP_TEXT;

   if (styp_flags == 0) styp_flags = STYP_BSS;

   return(styp_flags);
}
/*
 * Return a word with SEC_* flags set to represent the incoming
 * STYP_* flags (from scnhdr.s_flags).   The inverse of this
 * function is sec_to_styp_flags().
 * NOTE: If you add to/change this routine, you should mirror the changes
 *      in sec_to_styp_flags().
 */
static flagword
DEFUN(styp_to_sec_flags, (styp_flags),
	long	styp_flags)
{
  flagword	sec_flags=0;

  if ((styp_flags & STYP_TEXT) || (styp_flags & STYP_DATA)) 
  {
    sec_flags = SEC_LOAD | SEC_ALLOC;
  }
  else if (styp_flags & STYP_BSS) 
  {
    sec_flags = SEC_ALLOC;
  }
  else if (styp_flags & STYP_INFO) 
  {
    sec_flags = SEC_NEVER_LOAD;
  }
  else
  {
    sec_flags = SEC_ALLOC | SEC_LOAD;
  }
#ifdef STYP_LIT			/* A29k readonly text/data section type */
  if ((styp_flags & STYP_LIT) == STYP_LIT)
  {
    sec_flags = (SEC_LOAD | SEC_ALLOC | SEC_READONLY);
  }
#endif				/* STYP_LIT */
#ifdef STYP_OTHER_LOAD		/* Other loaded sections */
  if (styp_flags & STYP_OTHER_LOAD)
  {
    sec_flags = (SEC_LOAD | SEC_ALLOC);
  }
#endif				/* STYP_SDATA */

  return(sec_flags);
}

#define	get_index(symbol)	((int) (symbol)->udata)
#define	set_index(symbol, idx)	((symbol)->udata =(PTR) (idx))

/*  **********************************************************************
Here are all the routines for swapping the structures seen in the
outside world into the internal forms.
*/


static void
DEFUN(bfd_swap_reloc_in,(abfd, reloc_src, reloc_dst),
      bfd            *abfd AND
      RELOC *reloc_src AND
      struct internal_reloc *reloc_dst)
{
  reloc_dst->r_vaddr = bfd_h_get_32(abfd, (bfd_byte *)reloc_src->r_vaddr);
  reloc_dst->r_symndx = bfd_h_get_32(abfd, (bfd_byte *) reloc_src->r_symndx);

#ifdef RS6000COFF_C
  reloc_dst->r_type = bfd_h_get_8(abfd, reloc_src->r_type);
  reloc_dst->r_size = bfd_h_get_8(abfd, reloc_src->r_size);
#else
  reloc_dst->r_type = bfd_h_get_16(abfd, (bfd_byte *) reloc_src->r_type);
#endif

#ifdef SWAP_IN_RELOC_OFFSET
  reloc_dst->r_offset = SWAP_IN_RELOC_OFFSET(abfd,
					     (bfd_byte *) reloc_src->r_offset);
#endif
}


static unsigned int
DEFUN(coff_swap_reloc_out,(abfd, src, dst),
      bfd       *abfd AND
      PTR	src AND
      PTR	dst)
{
  struct internal_reloc *reloc_src = (struct internal_reloc *)src;
  struct external_reloc *reloc_dst = (struct external_reloc *)dst;
  bfd_h_put_32(abfd, reloc_src->r_vaddr, (bfd_byte *) reloc_dst->r_vaddr);
  bfd_h_put_32(abfd, reloc_src->r_symndx, (bfd_byte *) reloc_dst->r_symndx);
  bfd_h_put_16(abfd, reloc_src->r_type, (bfd_byte *)
	       reloc_dst->r_type);

#ifdef SWAP_OUT_RELOC_OFFSET
  SWAP_OUT_RELOC_OFFSET(abfd,
			reloc_src->r_offset,
			(bfd_byte *) reloc_dst->r_offset);
#endif
#ifdef SWAP_OUT_RELOC_EXTRA
  SWAP_OUT_RELOC_EXTRA(abfd,reloc_src, reloc_dst);
#endif

  return sizeof(struct external_reloc);
}

static void
DEFUN(bfd_swap_filehdr_in,(abfd, filehdr_src, filehdr_dst),
      bfd            *abfd AND
      FILHDR         *filehdr_src AND
      struct internal_filehdr *filehdr_dst)
{
  filehdr_dst->f_magic = bfd_h_get_16(abfd, (bfd_byte *) filehdr_src->f_magic);
  filehdr_dst->f_nscns = bfd_h_get_16(abfd, (bfd_byte *)filehdr_src-> f_nscns);
  filehdr_dst->f_timdat = bfd_h_get_32(abfd, (bfd_byte *)filehdr_src-> f_timdat);
  filehdr_dst->f_symptr = bfd_h_get_32(abfd, (bfd_byte *)filehdr_src-> f_symptr);
  filehdr_dst->f_nsyms = bfd_h_get_32(abfd, (bfd_byte *)filehdr_src-> f_nsyms);
  filehdr_dst->f_opthdr = bfd_h_get_16(abfd, (bfd_byte *)filehdr_src-> f_opthdr);
  filehdr_dst->f_flags = bfd_h_get_16(abfd, (bfd_byte *)filehdr_src-> f_flags);
}

static  unsigned int
DEFUN(coff_swap_filehdr_out,(abfd, in, out),
      bfd       *abfd AND
      PTR	in AND
      PTR	out)
{
  struct internal_filehdr *filehdr_in = (struct internal_filehdr *)in;
  FILHDR *filehdr_out = (FILHDR *)out;
  bfd_h_put_16(abfd, filehdr_in->f_magic, (bfd_byte *) filehdr_out->f_magic);
  bfd_h_put_16(abfd, filehdr_in->f_nscns, (bfd_byte *) filehdr_out->f_nscns);
  bfd_h_put_32(abfd, filehdr_in->f_timdat, (bfd_byte *) filehdr_out->f_timdat);
  bfd_h_put_32(abfd, filehdr_in->f_symptr, (bfd_byte *) filehdr_out->f_symptr);
  bfd_h_put_32(abfd, filehdr_in->f_nsyms, (bfd_byte *) filehdr_out->f_nsyms);
  bfd_h_put_16(abfd, filehdr_in->f_opthdr, (bfd_byte *) filehdr_out->f_opthdr);
  bfd_h_put_16(abfd, filehdr_in->f_flags, (bfd_byte *) filehdr_out->f_flags);
  return sizeof(FILHDR);
}


#ifndef NO_COFF_SYMBOLS

static void
DEFUN(coff_swap_sym_in,(abfd, ext1, in1),
      bfd            *abfd AND
      PTR ext1 AND
      PTR in1)
{
  SYMENT *ext = (SYMENT *)ext1;
  struct internal_syment      *in = (struct internal_syment *)in1;

  if( ext->e.e_name[0] == 0) {
    in->_n._n_n._n_zeroes = 0;
    in->_n._n_n._n_offset = bfd_h_get_32(abfd, (bfd_byte *) ext->e.e.e_offset);
  }
  else {
#if SYMNMLEN != E_SYMNMLEN
   -> Error, we need to cope with truncating or extending SYMNMLEN!;
#else
    memcpy(in->_n._n_name, ext->e.e_name, SYMNMLEN);
#endif
  }
  in->n_value = bfd_h_get_32(abfd, (bfd_byte *) ext->e_value);
  in->n_scnum = bfd_h_get_16(abfd, (bfd_byte *) ext->e_scnum);
  if (sizeof(ext->e_type) == 2){
    in->n_type = bfd_h_get_16(abfd, (bfd_byte *) ext->e_type);
  }
  else {
    in->n_type = bfd_h_get_32(abfd, (bfd_byte *) ext->e_type);
  }
  in->n_sclass = bfd_h_get_8(abfd, ext->e_sclass);
  in->n_numaux = bfd_h_get_8(abfd, ext->e_numaux);
}

static unsigned int
DEFUN(coff_swap_sym_out,(abfd, inp, extp),
      bfd       *abfd AND
      PTR	inp AND
      PTR	extp)
{
  struct internal_syment *in = (struct internal_syment *)inp;
  SYMENT *ext =(SYMENT *)extp;
  if(in->_n._n_name[0] == 0) {
    bfd_h_put_32(abfd, 0, (bfd_byte *) ext->e.e.e_zeroes);
    bfd_h_put_32(abfd, in->_n._n_n._n_offset, (bfd_byte *)  ext->e.e.e_offset);
  }
  else {
#if SYMNMLEN != E_SYMNMLEN
    -> Error, we need to cope with truncating or extending SYMNMLEN!;
#else
    memcpy(ext->e.e_name, in->_n._n_name, SYMNMLEN);
#endif
  }
  bfd_h_put_32(abfd,  in->n_value , (bfd_byte *) ext->e_value);
  bfd_h_put_16(abfd,  in->n_scnum , (bfd_byte *) ext->e_scnum);
  if (sizeof(ext->e_type) == 2)
      {
	bfd_h_put_16(abfd,  in->n_type , (bfd_byte *) ext->e_type);
      }
  else
      {
	bfd_h_put_32(abfd,  in->n_type , (bfd_byte *) ext->e_type);
      }
  bfd_h_put_8(abfd,  in->n_sclass , ext->e_sclass);
  bfd_h_put_8(abfd,  in->n_numaux , ext->e_numaux);
  return sizeof(SYMENT);
}

static void
DEFUN(coff_swap_aux_in,(abfd, ext1, type, class, in1),
      bfd            *abfd AND
      PTR 	      ext1 AND
      int             type AND
      int             class AND
      PTR 	      in1)
{
  AUXENT    *ext = (AUXENT *)ext1;
  union internal_auxent *in = (union internal_auxent *)in1;

  switch (class) {
    case C_FILE:
      if (ext->x_file.x_fname[0] == 0) {
	  in->x_file.x_n.x_zeroes = 0;
	  in->x_file.x_n.x_offset = 
	   bfd_h_get_32(abfd, (bfd_byte *) ext->x_file.x_n.x_offset);
	} else {
#if FILNMLEN != E_FILNMLEN
	    -> Error, we need to cope with truncating or extending FILNMLEN!;
#else
	    memcpy (in->x_file.x_fname, ext->x_file.x_fname, FILNMLEN);
#endif
	  }
      break;

      /* RS/6000 "csect" auxents */
#ifdef RS6000COFF_C
    case C_EXT:
    case C_HIDEXT:
      in->x_csect.x_scnlen   = bfd_h_get_32 (abfd, (bfd_byte *) ext->x_csect.x_scnlen);
      in->x_csect.x_parmhash = bfd_h_get_32 (abfd, (bfd_byte *) ext->x_csect.x_parmhash);
      in->x_csect.x_snhash   = bfd_h_get_16 (abfd, (bfd_byte *) ext->x_csect.x_snhash);
      /* We don't have to hack bitfields in x_smtyp because it's defined by
	 shifts-and-ands, which are equivalent on all byte orders.  */
      in->x_csect.x_smtyp    = bfd_h_get_8  (abfd, (bfd_byte *) ext->x_csect.x_smtyp);
      in->x_csect.x_smclas   = bfd_h_get_8  (abfd, (bfd_byte *) ext->x_csect.x_smclas);
      in->x_csect.x_stab     = bfd_h_get_32 (abfd, (bfd_byte *) ext->x_csect.x_stab);
      in->x_csect.x_snstab   = bfd_h_get_16 (abfd, (bfd_byte *) ext->x_csect.x_snstab);
      break;
#endif

    case C_STAT:
#ifdef C_LEAFSTAT
    case C_LEAFSTAT:
#endif
    case C_HIDDEN:
      if (type == T_NULL) {
	  in->x_scn.x_scnlen = GET_SCN_SCNLEN(abfd, ext);
	  in->x_scn.x_nreloc = GET_SCN_NRELOC(abfd, ext);
	  in->x_scn.x_nlinno = GET_SCN_NLINNO(abfd, ext);
	  break;
	}
    default:
      in->x_sym.x_tagndx.l = bfd_h_get_32(abfd, (bfd_byte *) ext->x_sym.x_tagndx);
#ifndef NO_TVNDX
      in->x_sym.x_tvndx = bfd_h_get_16(abfd, (bfd_byte *) ext->x_sym.x_tvndx);
#endif

      if (ISARY(type) || class == C_BLOCK) {
#if DIMNUM != E_DIMNUM
	  -> Error, we need to cope with truncating or extending DIMNUM!;
#else
	  in->x_sym.x_fcnary.x_ary.x_dimen[0] = bfd_h_get_16(abfd, (bfd_byte *) ext->x_sym.x_fcnary.x_ary.x_dimen[0]);
	  in->x_sym.x_fcnary.x_ary.x_dimen[1] = bfd_h_get_16(abfd, (bfd_byte *) ext->x_sym.x_fcnary.x_ary.x_dimen[1]);
	  in->x_sym.x_fcnary.x_ary.x_dimen[2] = bfd_h_get_16(abfd, (bfd_byte *) ext->x_sym.x_fcnary.x_ary.x_dimen[2]);
	  in->x_sym.x_fcnary.x_ary.x_dimen[3] = bfd_h_get_16(abfd, (bfd_byte *) ext->x_sym.x_fcnary.x_ary.x_dimen[3]);
#endif
	}
      in->x_sym.x_fcnary.x_fcn.x_lnnoptr = GET_FCN_LNNOPTR(abfd, ext);
      in->x_sym.x_fcnary.x_fcn.x_endndx.l = GET_FCN_ENDNDX(abfd, ext);

      if (ISFCN(type)) {
	  in->x_sym.x_misc.x_fsize = bfd_h_get_32(abfd, (bfd_byte *) ext->x_sym.x_misc.x_fsize);
	}
      else {
	  in->x_sym.x_misc.x_lnsz.x_lnno = GET_LNSZ_LNNO(abfd, ext);
	  in->x_sym.x_misc.x_lnsz.x_size = GET_LNSZ_SIZE(abfd, ext);
	}
    }
}

static unsigned int
DEFUN(coff_swap_aux_out,(abfd, inp, type, class, extp),
  bfd   *abfd AND
  PTR 	inp AND
  int   type AND
  int   class AND
  PTR	extp)
{
  union internal_auxent *in = (union internal_auxent *)inp;
  AUXENT *ext = (AUXENT *)extp;
  switch (class) {
  case C_FILE:
    if (in->x_file.x_fname[0] == 0) {
      PUTWORD(abfd, 0, (bfd_byte *) ext->x_file.x_n.x_zeroes);
      PUTWORD(abfd,
	      in->x_file.x_n.x_offset,
	      (bfd_byte *) ext->x_file.x_n.x_offset);
    }
    else {
#if FILNMLEN != E_FILNMLEN
      -> Error, we need to cope with truncating or extending FILNMLEN!;
#else
      memcpy (ext->x_file.x_fname, in->x_file.x_fname, FILNMLEN);
#endif
    }
    break;

#ifdef RS6000COFF_C
  /* RS/6000 "csect" auxents */
  case C_EXT:
  case C_HIDEXT:
    PUTWORD (abfd, in->x_csect.x_scnlen,	ext->x_csect.x_scnlen);
    PUTWORD (abfd, in->x_csect.x_parmhash,	ext->x_csect.x_parmhash);
    PUTHALF (abfd, in->x_csect.x_snhash,	ext->x_csect.x_snhash);
    /* We don't have to hack bitfields in x_smtyp because it's defined by
       shifts-and-ands, which are equivalent on all byte orders.  */
    PUTBYTE (abfd, in->x_csect.x_smtyp,		ext->x_csect.x_smtyp);
    PUTBYTE (abfd, in->x_csect.x_smclas,	ext->x_csect.x_smclas);
    PUTWORD (abfd, in->x_csect.x_stab,		ext->x_csect.x_stab);
    PUTHALF (abfd, in->x_csect.x_snstab,	ext->x_csect.x_snstab);
    break;
#endif

  case C_STAT:
#ifdef C_LEAFSTAT
  case C_LEAFSTAT:
#endif
  case C_HIDDEN:
    if (type == T_NULL) {
      PUT_SCN_SCNLEN(abfd, in->x_scn.x_scnlen, ext);
      PUT_SCN_NRELOC(abfd, in->x_scn.x_nreloc, ext);
      PUT_SCN_NLINNO(abfd, in->x_scn.x_nlinno, ext);
      break;
    }
  default:
    PUTWORD(abfd, in->x_sym.x_tagndx.l, (bfd_byte *) ext->x_sym.x_tagndx);
#ifndef NO_TVNDX
    bfd_h_put_16(abfd, in->x_sym.x_tvndx , (bfd_byte *) ext->x_sym.x_tvndx);
#endif

    if (ISFCN(type)) {
      PUTWORD(abfd, in->x_sym.x_misc.x_fsize, (bfd_byte *)  ext->x_sym.x_misc.x_fsize);
      PUT_FCN_LNNOPTR(abfd,  in->x_sym.x_fcnary.x_fcn.x_lnnoptr, ext);
      PUT_FCN_ENDNDX(abfd,  in->x_sym.x_fcnary.x_fcn.x_endndx.l, ext);
    }
    else {

      if (ISARY(type) || class == C_BLOCK) {
#if DIMNUM != E_DIMNUM
	-> Error, we need to cope with truncating or extending DIMNUM!;
#else
	bfd_h_put_16(abfd, in->x_sym.x_fcnary.x_ary.x_dimen[0], (bfd_byte *)ext->x_sym.x_fcnary.x_ary.x_dimen[0]);
	bfd_h_put_16(abfd, in->x_sym.x_fcnary.x_ary.x_dimen[1], (bfd_byte *)ext->x_sym.x_fcnary.x_ary.x_dimen[1]);
	bfd_h_put_16(abfd, in->x_sym.x_fcnary.x_ary.x_dimen[2], (bfd_byte *)ext->x_sym.x_fcnary.x_ary.x_dimen[2]);
	bfd_h_put_16(abfd, in->x_sym.x_fcnary.x_ary.x_dimen[3], (bfd_byte *)ext->x_sym.x_fcnary.x_ary.x_dimen[3]);
#endif
      }
      PUT_LNSZ_LNNO(abfd, in->x_sym.x_misc.x_lnsz.x_lnno, ext);
      PUT_LNSZ_SIZE(abfd, in->x_sym.x_misc.x_lnsz.x_size, ext);

      PUT_FCN_LNNOPTR(abfd,  in->x_sym.x_fcnary.x_fcn.x_lnnoptr, ext);
      PUT_FCN_ENDNDX(abfd,  in->x_sym.x_fcnary.x_fcn.x_endndx.l, ext);


    }
  }
return sizeof(AUXENT);
}

#endif /* NO_COFF_SYMBOLS */

#ifndef NO_COFF_LINENOS

static void
DEFUN(coff_swap_lineno_in,(abfd, ext1, in1),
      bfd            *abfd AND
      PTR ext1 AND
      PTR in1)
{
  LINENO *ext = (LINENO *)ext1;
  struct internal_lineno      *in = (struct internal_lineno *)in1;

  in->l_addr.l_symndx = bfd_h_get_32(abfd, (bfd_byte *) ext->l_addr.l_symndx);
  in->l_lnno = GET_LINENO_LNNO(abfd, ext);
}

static unsigned int
DEFUN(coff_swap_lineno_out,(abfd, inp, outp),
      bfd       *abfd AND
      PTR	inp AND
      PTR	outp)
{
  struct internal_lineno *in = (struct internal_lineno *)inp;
  struct external_lineno *ext = (struct external_lineno *)outp;
  PUTWORD(abfd, in->l_addr.l_symndx, (bfd_byte *)
	  ext->l_addr.l_symndx);

  PUT_LINENO_LNNO (abfd, in->l_lnno, ext);
  return sizeof(struct external_lineno);
}

#endif /* NO_COFF_LINENOS */


static void
DEFUN(bfd_swap_aouthdr_in,(abfd, aouthdr_ext1, aouthdr_int1),
      bfd            *abfd AND
      PTR aouthdr_ext1 AND
      PTR aouthdr_int1)
{
  AOUTHDR        *aouthdr_ext = (AOUTHDR *) aouthdr_ext1;
  struct internal_aouthdr *aouthdr_int = (struct internal_aouthdr *)aouthdr_int1;

  aouthdr_int->magic = bfd_h_get_16(abfd, (bfd_byte *) aouthdr_ext->magic);
  aouthdr_int->vstamp = bfd_h_get_16(abfd, (bfd_byte *) aouthdr_ext->vstamp);
  aouthdr_int->tsize = bfd_h_get_32(abfd, (bfd_byte *) aouthdr_ext->tsize);
  aouthdr_int->dsize = bfd_h_get_32(abfd, (bfd_byte *) aouthdr_ext->dsize);
  aouthdr_int->bsize = bfd_h_get_32(abfd, (bfd_byte *) aouthdr_ext->bsize);
  aouthdr_int->entry = bfd_h_get_32(abfd, (bfd_byte *) aouthdr_ext->entry);
  aouthdr_int->text_start = bfd_h_get_32(abfd, (bfd_byte *) aouthdr_ext->text_start);
  aouthdr_int->data_start = bfd_h_get_32(abfd, (bfd_byte *) aouthdr_ext->data_start);
#ifdef I960
  aouthdr_int->tagentries = bfd_h_get_32(abfd, (bfd_byte *) aouthdr_ext->tagentries);
#endif

#ifdef RS6000COFF_C
  aouthdr_int->o_toc = bfd_h_get_32(abfd, aouthdr_ext->o_toc);
  aouthdr_int->o_snentry = bfd_h_get_16(abfd, aouthdr_ext->o_snentry);
  aouthdr_int->o_sntext = bfd_h_get_16(abfd, aouthdr_ext->o_sntext);
  aouthdr_int->o_sndata = bfd_h_get_16(abfd, aouthdr_ext->o_sndata);
  aouthdr_int->o_sntoc = bfd_h_get_16(abfd, aouthdr_ext->o_sntoc);
  aouthdr_int->o_snloader = bfd_h_get_16(abfd, aouthdr_ext->o_snloader);
  aouthdr_int->o_snbss = bfd_h_get_16(abfd, aouthdr_ext->o_snbss);
  aouthdr_int->o_algntext = bfd_h_get_16(abfd, aouthdr_ext->o_algntext);
  aouthdr_int->o_algndata = bfd_h_get_16(abfd, aouthdr_ext->o_algndata);
  aouthdr_int->o_modtype = bfd_h_get_16(abfd, aouthdr_ext->o_modtype);
  aouthdr_int->o_maxstack = bfd_h_get_32(abfd, aouthdr_ext->o_maxstack);
#endif
}

static unsigned int
DEFUN(coff_swap_aouthdr_out,(abfd, in, out),
      bfd       *abfd AND
      PTR	in AND
      PTR	out)
{
  struct internal_aouthdr *aouthdr_in = (struct internal_aouthdr *)in;
  AOUTHDR *aouthdr_out = (AOUTHDR *)out;
  bfd_h_put_16(abfd, aouthdr_in->magic, (bfd_byte *) aouthdr_out->magic);
  bfd_h_put_16(abfd, aouthdr_in->vstamp, (bfd_byte *) aouthdr_out->vstamp);
  bfd_h_put_32(abfd, aouthdr_in->tsize, (bfd_byte *) aouthdr_out->tsize);
  bfd_h_put_32(abfd, aouthdr_in->dsize, (bfd_byte *) aouthdr_out->dsize);
  bfd_h_put_32(abfd, aouthdr_in->bsize, (bfd_byte *) aouthdr_out->bsize);
  bfd_h_put_32(abfd, aouthdr_in->entry, (bfd_byte *) aouthdr_out->entry);
  bfd_h_put_32(abfd, aouthdr_in->text_start,
	       (bfd_byte *) aouthdr_out->text_start);
  bfd_h_put_32(abfd, aouthdr_in->data_start, (bfd_byte *) aouthdr_out->data_start);
#ifdef I960
  bfd_h_put_32(abfd, aouthdr_in->tagentries, (bfd_byte *) aouthdr_out->tagentries);
#endif
  return sizeof(AOUTHDR);
}

static void
DEFUN(coff_swap_scnhdr_in,(abfd, scnhdr_ext, scnhdr_int),
      bfd            *abfd AND
      SCNHDR         *scnhdr_ext AND
      struct internal_scnhdr *scnhdr_int)
{
  memcpy(scnhdr_int->s_name, scnhdr_ext->s_name, sizeof(scnhdr_int->s_name));
  scnhdr_int->s_vaddr = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_vaddr);
  scnhdr_int->s_paddr = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_paddr);
  scnhdr_int->s_size = bfd_h_get_32(abfd, (bfd_byte *)  scnhdr_ext->s_size);

  scnhdr_int->s_scnptr = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_scnptr);
  scnhdr_int->s_relptr = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_relptr);
  scnhdr_int->s_lnnoptr = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_lnnoptr);
  scnhdr_int->s_flags = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_flags);
#if defined(M88)
  scnhdr_int->s_nreloc = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_nreloc);
  scnhdr_int->s_nlnno = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_nlnno);
#else
  scnhdr_int->s_nreloc = bfd_h_get_16(abfd, (bfd_byte *) scnhdr_ext->s_nreloc);
  scnhdr_int->s_nlnno = bfd_h_get_16(abfd, (bfd_byte *) scnhdr_ext->s_nlnno);
#endif
#ifdef I960
  scnhdr_int->s_align = bfd_h_get_32(abfd, (bfd_byte *) scnhdr_ext->s_align);
#endif
}

static unsigned int
DEFUN(coff_swap_scnhdr_out,(abfd, in, out),
      bfd       *abfd AND
      PTR	in AND
      PTR	out)
{
  struct internal_scnhdr *scnhdr_int = (struct internal_scnhdr *)in;
  SCNHDR *scnhdr_ext = (SCNHDR *)out;
  memcpy(scnhdr_ext->s_name, scnhdr_int->s_name, sizeof(scnhdr_int->s_name));
  PUTWORD(abfd, scnhdr_int->s_vaddr, (bfd_byte *) scnhdr_ext->s_vaddr);
  PUTWORD(abfd, scnhdr_int->s_paddr, (bfd_byte *) scnhdr_ext->s_paddr);
  PUTWORD(abfd, scnhdr_int->s_size, (bfd_byte *) scnhdr_ext->s_size);
  PUTWORD(abfd, scnhdr_int->s_scnptr, (bfd_byte *) scnhdr_ext->s_scnptr);
  PUTWORD(abfd, scnhdr_int->s_relptr, (bfd_byte *) scnhdr_ext->s_relptr);
  PUTWORD(abfd, scnhdr_int->s_lnnoptr, (bfd_byte *) scnhdr_ext->s_lnnoptr);
  PUTWORD(abfd, scnhdr_int->s_flags, (bfd_byte *) scnhdr_ext->s_flags);
#if defined(M88)
  PUTWORD(abfd, scnhdr_int->s_nlnno, (bfd_byte *) scnhdr_ext->s_nlnno);
  PUTWORD(abfd, scnhdr_int->s_nreloc, (bfd_byte *) scnhdr_ext->s_nreloc);
#else
  PUTHALF(abfd, scnhdr_int->s_nlnno, (bfd_byte *) scnhdr_ext->s_nlnno);
  PUTHALF(abfd, scnhdr_int->s_nreloc, (bfd_byte *) scnhdr_ext->s_nreloc);
#endif

#if defined(I960)
  PUTWORD(abfd, scnhdr_int->s_align, (bfd_byte *) scnhdr_ext->s_align);
#endif
  return sizeof(SCNHDR);
}


/*
   initialize a section structure with information peculiar to this
   particular implementation of coff
*/

static          boolean
DEFUN(coff_new_section_hook,(abfd, section),
      bfd            *abfd AND
      asection       *section)
{
  section->alignment_power = abfd->xvec->align_power_min;
  /* Allocate aux records for section symbols, to store size and
     related info.

     @@ Shouldn't use constant multiplier here!  */
  coffsymbol (section->symbol)->native =
    (combined_entry_type *) bfd_zalloc (abfd,
					sizeof (combined_entry_type) * 10);
  return true;
}

static asection bfd_debug_section = { "*DEBUG*" };

/* Take a section header read from a coff file (in HOST byte order),
   and make a BFD "section" out of it.  */
static          boolean
DEFUN(make_a_section_from_file,(abfd, hdr, target_index),
      bfd            *abfd AND
      struct internal_scnhdr  *hdr AND
      unsigned int target_index)
{
  asection       *return_section;
  char *name;
    
  /* Assorted wastage to null-terminate the name, thanks AT&T! */
  name = bfd_alloc(abfd, sizeof (hdr->s_name)+1);
  if (name == NULL) {
      bfd_error = no_memory;
      return false;
    }
  strncpy(name, (char *) &hdr->s_name[0], sizeof (hdr->s_name));
  name[sizeof (hdr->s_name)] = 0;

  return_section = bfd_make_section(abfd, name);
  if (return_section == NULL)
   return false;

  /* s_paddr is presumed to be = to s_vaddr */

  return_section->vma = hdr->s_vaddr;
  return_section->_raw_size = hdr->s_size;
  return_section->filepos = hdr->s_scnptr;
  return_section->rel_filepos =  hdr->s_relptr;
  return_section->reloc_count = hdr->s_nreloc;
#ifdef I960

  /* FIXME, use a temp var rather than alignment_power */
  return_section->alignment_power = hdr->s_align;
{
  unsigned int    i;
  for (i = 0; i < 32; i++) {
      if ((1 << i) >= (int) (return_section->alignment_power)) {
	  return_section->alignment_power = i;
	  break;
	}
    }
}

#endif
return_section->line_filepos =  hdr->s_lnnoptr;
  /*
    return_section->linesize =   hdr->s_nlnno * sizeof (struct lineno);
    */

  return_section->lineno_count = hdr->s_nlnno;
  return_section->userdata = NULL;
  return_section->next = (asection *) NULL;
  return_section->flags = styp_to_sec_flags(hdr->s_flags);

  return_section->target_index = target_index;

  if (hdr->s_nreloc != 0)
   return_section->flags |= SEC_RELOC;
  /* FIXME: should this check 'hdr->s_size > 0' */
  if (hdr->s_scnptr != 0)
   return_section->flags |= SEC_HAS_CONTENTS;
  return true;
}
static          boolean
DEFUN(coff_mkobject,(abfd),
      bfd            *abfd)
{
 abfd->tdata.coff_obj_data = (struct coff_tdata *)bfd_zalloc (abfd,sizeof(coff_data_type));
  if (abfd->tdata.coff_obj_data == 0){
    bfd_error = no_memory;
    return false;
  }
  coff_data(abfd)->relocbase = 0;
/*  make_abs_section(abfd);*/
  return true;
}

static
bfd_target     *
DEFUN(coff_real_object_p,(abfd, nscns, internal_f, internal_a),
    bfd            *abfd AND
    unsigned        nscns AND
  struct internal_filehdr *internal_f AND
  struct internal_aouthdr *internal_a)
{
  coff_data_type *coff;
  enum bfd_architecture arch;
  long machine;
  size_t          readsize;	/* length of file_info */
  SCNHDR *external_sections;

  /* Build a play area */
  if (coff_mkobject(abfd) != true)
    return 0;

  coff = coff_data(abfd);


  external_sections = (SCNHDR *)bfd_alloc(abfd, readsize = (nscns * SCNHSZ));

  if (bfd_read((PTR)external_sections, 1, readsize, abfd) != readsize) {
    goto fail;
  }


  /* Now copy data as required; construct all asections etc */
  coff->symbol_index_slew = 0;
  coff->relocbase =0;
  coff->raw_syment_count = 0;
  coff->raw_linenos = 0;
  coff->raw_syments = 0;
  coff->sym_filepos =0;
  coff->flags = internal_f->f_flags;
  if (nscns != 0) {
    unsigned int    i;
    for (i = 0; i < nscns; i++) {
      struct internal_scnhdr tmp;
      coff_swap_scnhdr_in(abfd, external_sections + i, &tmp);
      make_a_section_from_file(abfd,&tmp, i+1);
    }
  }

/*  make_abs_section(abfd);*/
  
  /* Determine the machine architecture and type.  */
machine = 0;
  switch (internal_f->f_magic) {
#ifdef I386MAGIC
  case I386MAGIC:
    arch = bfd_arch_i386;
    machine = 0;
    break;
#endif

#ifdef A29K_MAGIC_BIG
  case  A29K_MAGIC_BIG:
  case  A29K_MAGIC_LITTLE:
    arch = bfd_arch_a29k;
    machine = 0;
    break;
#endif

#ifdef MIPS
  case  MIPS_MAGIC_1:
  case  MIPS_MAGIC_2:
  case  MIPS_MAGIC_3:
    arch = bfd_arch_mips;
    machine = 0;
    break;
#endif

#ifdef MC68MAGIC
  case MC68MAGIC:
  case M68MAGIC:
    arch = bfd_arch_m68k;
    machine = 68020;
    break;
#endif
#ifdef MC88MAGIC
  case MC88MAGIC:
  case MC88DMAGIC:
  case MC88OMAGIC:
    arch = bfd_arch_m88k;
    machine = 88100;
    break;
#endif
#ifdef Z8KMAGIC
   case Z8KMAGIC:
    arch = bfd_arch_z8k;
    switch (internal_f->f_flags & F_MACHMASK) 
{
     case F_Z8001:
      machine = bfd_mach_z8001;
      break;
     case F_Z8002:
      machine = bfd_mach_z8002;
      break;
     default:
      goto fail;
    }
    break;
#endif
#ifdef I960
#ifdef I960ROMAGIC
  case I960ROMAGIC:
  case I960RWMAGIC:
    arch = bfd_arch_i960;
    switch (F_I960TYPE & internal_f->f_flags)
	{
	default:
	case F_I960CORE:
	  machine = bfd_mach_i960_core;
	  break;
	case F_I960KB:
	  machine = bfd_mach_i960_kb_sb;
	  break;
	case  F_I960MC:
	  machine = bfd_mach_i960_mc;
	  break;
	case F_I960XA:
	  machine = bfd_mach_i960_xa;
	  break;
	case F_I960CA:
	  machine = bfd_mach_i960_ca;
	  break;
	case F_I960KA:
	  machine = bfd_mach_i960_ka_sa;
	  break;
	}
    break;
#endif
#endif

#ifdef U802ROMAGIC
  case U802ROMAGIC:
  case U802WRMAGIC:
  case U802TOCMAGIC:
    arch = bfd_arch_rs6000;
    machine = 6000;
    break;
#endif

#ifdef WE32KMAGIC
  case WE32KMAGIC:
    arch = bfd_arch_we32k;
    machine = 0;
    break;
#endif

#ifdef H8300MAGIC
  case H8300MAGIC:
    arch = bfd_arch_h8300;
    machine = 0;
    break;
#endif

  default:			/* Unreadable input file type */
 arch = bfd_arch_obscure;
    break;
  }

  bfd_default_set_arch_mach(abfd, arch, machine);
  if (!(internal_f->f_flags & F_RELFLG))
    abfd->flags |= HAS_RELOC;
  if ((internal_f->f_flags & F_EXEC))
    abfd->flags |= EXEC_P;
  if (!(internal_f->f_flags & F_LNNO))
    abfd->flags |= HAS_LINENO;
  if (!(internal_f->f_flags & F_LSYMS))
    abfd->flags |= HAS_LOCALS;


  bfd_get_symcount(abfd) = internal_f->f_nsyms;
  if (internal_f->f_nsyms)
    abfd->flags |= HAS_SYMS;

  coff->sym_filepos = internal_f->f_symptr;

  /* These members communicate important constants about the symbol table
    to GDB's symbol-reading code.  These `constants' unfortunately vary
      from coff implementation to implementation...  */
#ifndef NO_COFF_SYMBOLS
  coff->local_n_btmask = N_BTMASK;
  coff->local_n_btshft = N_BTSHFT;
  coff->local_n_tmask  = N_TMASK;
  coff->local_n_tshift = N_TSHIFT;
  coff->local_symesz   = SYMESZ;
  coff->local_auxesz   = AUXESZ;
  coff->local_linesz   = LINESZ;
#endif

  coff->symbols = (coff_symbol_type *) NULL;
  bfd_get_start_address(abfd) = internal_f->f_opthdr ? internal_a->entry : 0;

  return abfd->xvec;
 fail:
  bfd_release(abfd, coff);
  return (bfd_target *)NULL;
}

static bfd_target *
DEFUN(coff_object_p,(abfd),
      bfd            *abfd)
{
  int   nscns;
  FILHDR filehdr;
  AOUTHDR opthdr;
  struct internal_filehdr internal_f;
  struct internal_aouthdr internal_a;

  bfd_error = system_call_error;

  /* figure out how much to read */
  if (bfd_read((PTR) &filehdr, 1, FILHSZ, abfd) != FILHSZ)
    return 0;

  bfd_swap_filehdr_in(abfd, &filehdr, &internal_f);

  if (BADMAG(internal_f)) {
    bfd_error = wrong_format;
    return 0;
  }
  nscns =internal_f.f_nscns;

  if (internal_f.f_opthdr) {
    if (bfd_read((PTR) &opthdr, 1,AOUTSZ, abfd) != AOUTSZ) {
      return 0;
    }
    bfd_swap_aouthdr_in(abfd, (char *)&opthdr, (char *)&internal_a);
  }

  /* Seek past the opt hdr stuff */
  bfd_seek(abfd, (file_ptr) (internal_f.f_opthdr + FILHSZ), SEEK_SET);

  /* if the optional header is NULL or not the correct size then
     quit; the only difference I can see between m88k dgux headers (MC88DMAGIC)
     and Intel 960 readwrite headers (I960WRMAGIC) is that the
     optional header is of a different size.

     But the mips keeps extra stuff in it's opthdr, so dont check
     when doing that
     */

#if defined(M88) || defined(I960)
  if (internal_f.f_opthdr != 0 && AOUTSZ != internal_f.f_opthdr)
    return (bfd_target *)NULL;
#endif

  return coff_real_object_p(abfd, nscns, &internal_f, &internal_a);
}



#ifndef NO_COFF_LINENOS

static void
DEFUN(coff_count_linenumbers,(abfd),
      bfd            *abfd)
{
  unsigned int    limit = bfd_get_symcount(abfd);
  unsigned int    i;
  asymbol       **p;
    {
      asection       *s = abfd->sections->output_section;
      while (s) {
	BFD_ASSERT(s->lineno_count == 0);
	s = s->next;
      }
    }


  for (p = abfd->outsymbols, i = 0; i < limit; i++, p++) {
    asymbol        *q_maybe = *p;
    if (q_maybe->the_bfd->xvec->flavour == bfd_target_coff_flavour) {
      coff_symbol_type *q = coffsymbol(q_maybe);
      if (q->lineno) {
	/*
	  This symbol has a linenumber, increment the owning
	  section's linenumber count
	  */
	alent          *l = q->lineno;
	q->symbol.section->output_section->lineno_count++;
	l++;
	while (l->line_number) {
	  q->symbol.section->output_section->lineno_count++;
	  l++;
	}
      }
    }
  }
}

#endif /* NO_COFF_LINENOS */

#ifndef NO_COFF_SYMBOLS

/*
  Takes a bfd and a symbol, returns a pointer to the coff specific area
  of the symbol if there is one.
  */
static coff_symbol_type *
DEFUN(coff_symbol_from,(ignore_abfd, symbol),
      bfd            *ignore_abfd AND
      asymbol        *symbol)
{
  if (symbol->the_bfd->xvec->flavour != bfd_target_coff_flavour)
    return (coff_symbol_type *)NULL;

  if (symbol->the_bfd->tdata.coff_obj_data == (coff_data_type*)NULL)
    return (coff_symbol_type *)NULL;

  return  (coff_symbol_type *) symbol;
}



static void
DEFUN(fixup_symbol_value,(coff_symbol_ptr, syment),
coff_symbol_type *coff_symbol_ptr AND
struct internal_syment *syment)
{

  /* Normalize the symbol flags */
  if (coff_symbol_ptr->symbol.section == &bfd_com_section) {
    /* a common symbol is undefined with a value */
    syment->n_scnum = N_UNDEF;
    syment->n_value = coff_symbol_ptr->symbol.value;
  }
  else if (coff_symbol_ptr->symbol.flags & BSF_DEBUGGING) {
    syment->n_value = coff_symbol_ptr->symbol.value;
  }
  else if (coff_symbol_ptr->symbol.section == & bfd_und_section) {
    syment->n_scnum = N_UNDEF;
    syment->n_value = 0;
  }
  else {
    if (coff_symbol_ptr->symbol.section) {
      syment->n_scnum	 =
       coff_symbol_ptr->symbol.section->output_section->target_index;

      syment->n_value =
       coff_symbol_ptr->symbol.value +
	coff_symbol_ptr->symbol.section->output_offset +
	 coff_symbol_ptr->symbol.section->output_section->vma;
    }
    else {
	BFD_ASSERT(0);
      /* This can happen, but I don't know why yet (steve@cygnus.com) */
      syment->n_scnum = N_ABS;
      syment->n_value = coff_symbol_ptr->symbol.value;
    }
  }
}

/* run through all the symbols in the symbol table and work out what
   their indexes into the symbol table will be when output

 Coff requires that each C_FILE symbol points to the next one in the
 chain, and that the last one points to the first external symbol. We
 do that here too.

*/
static void
DEFUN(coff_renumber_symbols,(bfd_ptr),
      bfd *bfd_ptr)
{
  unsigned int symbol_count = bfd_get_symcount(bfd_ptr);
  asymbol **symbol_ptr_ptr = bfd_ptr->outsymbols;
  unsigned int native_index = 0;
  struct internal_syment *last_file = (struct internal_syment *)NULL;
  unsigned int symbol_index;

  /* COFF demands that undefined symbols come after all other symbols.
     Since we don't need to impose this extra knowledge on all our client
     programs, deal with that here.  Sort the symbol table; just move the
     undefined symbols to the end, leaving the rest alone.  */
  /* @@ Do we have some condition we could test for, so we don't always
     have to do this?  I don't think relocatability is quite right, but
     I'm not certain.  [raeburn:19920508.1711EST]  */
  {
    asymbol **newsyms;
    int i;

    newsyms = (asymbol **) bfd_alloc_by_size_t (bfd_ptr,
						sizeof (asymbol *)
						* (symbol_count + 1));
    bfd_ptr->outsymbols = newsyms;
    for (i = 0; i < symbol_count; i++)
      if (symbol_ptr_ptr[i]->section != &bfd_und_section)
	*newsyms++ = symbol_ptr_ptr[i];
    for (i = 0; i < symbol_count; i++)
      if (symbol_ptr_ptr[i]->section == &bfd_und_section)
	*newsyms++ = symbol_ptr_ptr[i];
    *newsyms = (asymbol *) NULL;
    symbol_ptr_ptr = bfd_ptr->outsymbols;
  }

  for (symbol_index = 0; symbol_index < symbol_count; symbol_index++)
      {
	coff_symbol_type *coff_symbol_ptr = coff_symbol_from(bfd_ptr, symbol_ptr_ptr[symbol_index]);
	if (coff_symbol_ptr && coff_symbol_ptr->native) {
	  combined_entry_type *s = coff_symbol_ptr->native;
	  int i;

	  if (s->u.syment.n_sclass == C_FILE)
	      {
		if (last_file != (struct internal_syment *)NULL) {
		  last_file->n_value = native_index;
		}
		last_file = &(s->u.syment);
	      }
	  else {

	    /* Modify the symbol values according to their section and
	       type */

	    fixup_symbol_value(coff_symbol_ptr, &(s->u.syment));
	  }
	  for (i = 0; i < s->u.syment.n_numaux + 1; i++) {
	    s[i].offset = native_index ++;
	  }
	}
	else {
	  native_index++;
	}
      }
  obj_conv_table_size (bfd_ptr) = native_index;
}


/*
 Run thorough the symbol table again, and fix it so that all pointers to
 entries are changed to the entries' index in the output symbol table.

*/
static void
DEFUN(coff_mangle_symbols,(bfd_ptr),
      bfd *bfd_ptr)
{
  unsigned int symbol_count = bfd_get_symcount(bfd_ptr);
  asymbol **symbol_ptr_ptr = bfd_ptr->outsymbols;
  unsigned int symbol_index;

  for (symbol_index = 0; symbol_index < symbol_count; symbol_index++)
      {
	coff_symbol_type *coff_symbol_ptr =
	  coff_symbol_from(bfd_ptr, symbol_ptr_ptr[symbol_index]);

	if (coff_symbol_ptr && coff_symbol_ptr->native) {
	  int i;
	  combined_entry_type *s = coff_symbol_ptr->native;

	  for (i = 0; i < s->u.syment.n_numaux ; i++) {
	    combined_entry_type *a = s + i + 1;
	    if (a->fix_tag) {
	      a->u.auxent.x_sym.x_tagndx.l =
		a->u.auxent.x_sym.x_tagndx.p->offset;
	      a->fix_tag = 0;
	    }
	    if (a->fix_end) {
	      a->u.auxent.x_sym.x_fcnary.x_fcn.x_endndx.l =
		a->u.auxent.x_sym.x_fcnary.x_fcn.x_endndx.p->offset;
	      a->fix_end = 0;
	      
	    }

	  }
	}
      }
}

static int string_size;
static void
DEFUN(coff_fix_symbol_name,(ignore_abfd, symbol, native),
  bfd *ignore_abfd AND
  asymbol *symbol AND
  combined_entry_type *native)
{
  unsigned int    name_length;
  union internal_auxent *auxent;
  char *  name = ( char *)(symbol->name);

  if (name == (char *) NULL) {
    /* coff symbols always have names, so we'll make one up */
    symbol->name = "strange";
    name = (char *)symbol->name;
  }
  name_length = strlen(name);

  if (native->u.syment.n_sclass == C_FILE) {
    strncpy(native->u.syment._n._n_name, ".file", SYMNMLEN);
    auxent = &(native+1)->u.auxent;

#ifdef COFF_LONG_FILENAMES
    if (name_length <= FILNMLEN) {
      strncpy(auxent->x_file.x_fname, name, FILNMLEN);
    }
    else {
      auxent->x_file.x_n.x_offset = string_size + 4;
      auxent->x_file.x_n.x_zeroes = 0;
      string_size += name_length + 1;
    }
#else
    strncpy(auxent->x_file.x_fname, name, FILNMLEN);
    if (name_length > FILNMLEN) {
      name[FILNMLEN] = '\0';
    }
#endif
  }
  else
      {				/* NOT A C_FILE SYMBOL */
	if (name_length <= SYMNMLEN) {
	  /* This name will fit into the symbol neatly */
	  strncpy(native->u.syment._n._n_name, symbol->name, SYMNMLEN);
	}
	else {
	  native->u.syment._n._n_n._n_offset =  string_size + 4;
	  native->u.syment._n._n_n._n_zeroes = 0;
	  string_size += name_length + 1;
	}
      }
}



static unsigned int
DEFUN(coff_write_symbol,(abfd, symbol, native, written),
bfd *abfd AND
asymbol *symbol AND
combined_entry_type *native AND
unsigned int written)
{
  unsigned int    numaux = native->u.syment.n_numaux;
  int             type = native->u.syment.n_type;
  int             class =  native->u.syment.n_sclass;
  SYMENT buf;
  unsigned int j;

  /* @@ bfd_debug_section isn't accessible outside this file, but we know
     that C_FILE symbols belong there.  So move them.  */
  if (native->u.syment.n_sclass == C_FILE)
    symbol->section = &bfd_debug_section;

  if (symbol->section == &bfd_abs_section) 
  {
    native->u.syment.n_scnum = N_ABS;
  }
  else if (symbol->section == &bfd_debug_section) 
  {
    native->u.syment.n_scnum = N_DEBUG;
  }
  else if (symbol->section == &bfd_und_section)   
  {
    native->u.syment.n_scnum = N_UNDEF;
  }
  else 
  {
    native->u.syment.n_scnum =
     symbol->section->output_section->target_index;
  }
  
  
  coff_fix_symbol_name(abfd, symbol, native);

  coff_swap_sym_out(abfd, &native->u.syment, &buf);
  bfd_write((PTR)& buf, 1, SYMESZ, abfd);
  for (j = 0; j < native->u.syment.n_numaux;  j++)
  {
    AUXENT buf1;
    memset((PTR)&buf, 0, AUXESZ);
    coff_swap_aux_out(abfd,
		      &( (native + j + 1)->u.auxent), type, class, &buf1);
    bfd_write((PTR) (&buf1), 1, AUXESZ, abfd);
  }
  /*
    Reuse somewhere in the symbol to keep the index
    */
  set_index(symbol, written);
  return   written + 1 + numaux;
}


static unsigned int
DEFUN(coff_write_alien_symbol,(abfd, symbol, written),
      bfd *abfd AND
      asymbol *symbol AND
      unsigned int written)
{
  /*
    This symbol has been created by the loader, or come from a non
    coff format. It  has no native element to inherit, make our
    own
    */
  combined_entry_type *native;
  combined_entry_type dummy;
  native = &dummy;
  native->u.syment.n_type =  T_NULL;
#ifdef I960
  native->u.syment.n_flags =  0;
#endif
  if (symbol->section == &bfd_und_section) 
  {
      native->u.syment.n_scnum =  N_UNDEF;
      native->u.syment.n_value =  symbol->value;
    }
  else if (symbol->section == &bfd_com_section) 
  {
      native->u.syment.n_scnum =  N_UNDEF;
      native->u.syment.n_value =  symbol->value;

  }
  
  else if (symbol->flags & BSF_DEBUGGING) {
      /*
	remove name so it doesn't take up any space
	*/
      symbol->name = "";
    }
  else {
      native->u.syment.n_scnum  =   symbol->section->output_section->target_index;
      native->u.syment.n_value =   symbol->value +
       symbol->section->output_section->vma +
	symbol->section->output_offset;
#ifdef I960
      /* Copy the any flags from the the file hdr into the symbol  */
    {
      coff_symbol_type *c = coff_symbol_from(abfd, symbol);
      if (c != (coff_symbol_type *)NULL) {
	  native->u.syment.n_flags =   c->symbol.the_bfd->flags;
	}
    }
#endif
    }

#ifdef HASPAD1
  native->u.syment.pad1[0] = 0;
  native->u.syment.pad1[0] = 0;
#endif

  native->u.syment.n_type =  0;
  if (symbol->flags & BSF_LOCAL)
   native->u.syment.n_sclass =  C_STAT;
  else
   native->u.syment.n_sclass =  C_EXT;
  native->u.syment.n_numaux =  0;

  return   coff_write_symbol(abfd, symbol, native, written);
}

static unsigned int
DEFUN(coff_write_native_symbol,(abfd, symbol,   written),
bfd *abfd AND
coff_symbol_type *symbol AND
unsigned int written)
{
  /*
    Does this symbol have an ascociated line number - if so then
    make it remember this symbol index. Also tag the auxent of
    this symbol to point to the right place in the lineno table
    */
  combined_entry_type *native = symbol->native;

  alent          *lineno = symbol->lineno;

  if (lineno && !symbol->done_lineno) {
    unsigned int    count = 0;
    lineno[count].u.offset = written;
    if (native->u.syment.n_numaux) {
      union internal_auxent  *a = &((native+1)->u.auxent);

      a->x_sym.x_fcnary.x_fcn.x_lnnoptr =
	symbol->symbol.section->output_section->moving_line_filepos;
    }
    /*
      And count and relocate all other linenumbers
      */

    count++;
    while (lineno[count].line_number) {
#if 0
/* 13 april 92. sac 
I've been told this, but still need proof:
> The second bug is also in `bfd/coffcode.h'.  This bug causes the linker to screw
> up the pc-relocations for all the line numbers in COFF code.  This bug isn't
> only specific to A29K implementations, but affects all systems using COFF
> format binaries.  Note that in COFF object files, the line number core offsets
> output by the assembler are relative to the start of each procedure, not
> to the start of the .text section.  This patch relocates the line numbers
> relative to the `native->u.syment.n_value' instead of the section virtual
> address.  modular!olson@cs.arizona.edu (Jon Olson)
*/
       lineno[count].u.offset += native->u.syment.n_value;

#else
      lineno[count].u.offset +=
	symbol->symbol.section->output_section->vma +
	  symbol->symbol.section->output_offset;
#endif
      count++;
    }
    symbol->done_lineno = true;
    
    symbol->symbol.section->output_section->moving_line_filepos +=
      count * LINESZ;
  }
  return coff_write_symbol(abfd, &( symbol->symbol), native,written);
}

static void
DEFUN(coff_write_symbols,(abfd),
      bfd            *abfd)
{
  unsigned int    i;
  unsigned int    limit = bfd_get_symcount(abfd);
  unsigned int    written = 0;

  asymbol       **p;

  string_size = 0;


  /* Seek to the right place */
  bfd_seek(abfd, obj_sym_filepos(abfd), SEEK_SET);

  /* Output all the symbols we have */

  written = 0;
  for (p = abfd->outsymbols, i = 0; i < limit; i++, p++)
      {
	asymbol        *symbol = *p;
	coff_symbol_type *c_symbol = coff_symbol_from(abfd, symbol);

	if (c_symbol == (coff_symbol_type *) NULL ||
	    c_symbol->native == (combined_entry_type *)NULL)
	    {
	      written = coff_write_alien_symbol(abfd, symbol, written);
	    }
	else
	    {
	      written = coff_write_native_symbol(abfd, c_symbol, written);
	    }

      }

  bfd_get_symcount(abfd) = written;

  /* Now write out strings */

  if (string_size != 0)
   {
     unsigned int    size = string_size + 4;
     bfd_byte buffer[4];

     bfd_h_put_32(abfd, size, buffer);
     bfd_write((PTR) buffer, 1, sizeof(buffer), abfd);
     for (p = abfd->outsymbols, i = 0;
	  i < limit;
	  i++, p++)
	 {
	   asymbol        *q = *p;
	   size_t          name_length = strlen(q->name);
	   int maxlen;
	   coff_symbol_type*	   c_symbol = coff_symbol_from(abfd, q);
	   maxlen = ((c_symbol != NULL && c_symbol->native != NULL) &&
		     (c_symbol->native->u.syment.n_sclass == C_FILE)) ?
	     FILNMLEN : SYMNMLEN;

	   if (name_length > maxlen) {
	     bfd_write((PTR) (q->name), 1, name_length + 1, abfd);
	   }
	 }
   }
  else {
    /* We would normally not write anything here, but we'll write
       out 4 so that any stupid coff reader which tries to read
       the string table even when there isn't one won't croak.
       */

    uint32e_type size = 4;
    size =  size;
    bfd_write((PTR)&size, 1, sizeof(size), abfd);

  }
}

/*
SUBSUBSECTION
	Writing Relocations

	To write relocations, all the back end does is step though the
	canonical relocation table, and create an
	@code{internal_reloc}. The symbol index to use is removed from
	the @code{offset} field in the symbol table supplied, the
	address comes directly from the sum of the section base
	address and the relocation offset and the type is dug directly
	from the howto field.  Then the @code{internal_reloc} is
	swapped into the shape of an @code{external_reloc} and written
	out to disk. 

*/

static void
DEFUN(coff_write_relocs,(abfd),
      bfd            *abfd)
{
  asection       *s;
  for (s = abfd->sections; s != (asection *) NULL; s = s->next) {
    unsigned int    i;
    struct external_reloc dst;

    arelent       **p = s->orelocation;
    bfd_seek(abfd, s->rel_filepos, SEEK_SET);
    for (i = 0; i < s->reloc_count; i++) {
      struct internal_reloc    n;
      arelent        *q = p[i];
      memset((PTR)&n, 0, sizeof(n));

      n.r_vaddr = q->address + s->vma;
      /* The 29k const/consth reloc pair is a real kludge - the consth
	 part doesn't have a symbol - it has an offset. So rebuilt
	 that here */
#ifdef R_IHCONST                       
      if (q->howto->type == R_IHCONST)
	n.r_symndx = q->addend;
      else
#endif

      if (q->sym_ptr_ptr) {
	n.r_symndx = get_index((*(q->sym_ptr_ptr)));
	/* Take notice if the symbol reloc points to a symbol we don't have
	   in our symbol table.  What should we do for this??  */
	if (n.r_symndx > obj_conv_table_size (abfd))
	  abort ();
      }
#ifdef SELECT_RELOC
      /* Work out reloc type from what is required */
      SELECT_RELOC(n.r_type, q->howto);
#else
      n.r_type = q->howto->type;
#endif
      coff_swap_reloc_out(abfd, &n, &dst);
      bfd_write((PTR) &dst, 1, RELSZ, abfd);
    }
  }
}
#endif /* NO_COFF_SYMBOLS */

#ifndef NO_COFF_LINENOS

static void
DEFUN(coff_write_linenumbers,(abfd),
      bfd            *abfd)
{
  asection       *s;
  for (s = abfd->sections; s != (asection *) NULL; s = s->next) {
    if (s->lineno_count) {
      asymbol       **q = abfd->outsymbols;
      bfd_seek(abfd, s->line_filepos, SEEK_SET);
      /* Find all the linenumbers in this section */
      while (*q) {
	asymbol        *p = *q;
	alent          *l = BFD_SEND(p->the_bfd, _get_lineno, (p->the_bfd, p));
	if (l) {
	  /* Found a linenumber entry, output */
	  struct internal_lineno  out;
	  LINENO buff;
	  memset( (PTR)&out, 0, sizeof(out));
	  out.l_lnno = 0;
	  out.l_addr.l_symndx = l->u.offset;
	  coff_swap_lineno_out(abfd, &out, &buff);
	  bfd_write((PTR) &buff, 1, LINESZ, abfd);
	  l++;
	  while (l->line_number) {
	    out.l_lnno = l->line_number;
	    out.l_addr.l_symndx = l->u.offset;
	    coff_swap_lineno_out(abfd, &out, &buff);
	    bfd_write((PTR) &buff, 1, LINESZ, abfd);
	    l++;
	  }
	}
	q++;
      }
    }
  }
}

static alent   *
DEFUN(coff_get_lineno,(ignore_abfd, symbol),
      bfd            *ignore_abfd AND
      asymbol        *symbol)
{
  return coffsymbol(symbol)->lineno;
}

#endif /* NO_COFF_LINENOS */

static asymbol *
coff_make_empty_symbol(abfd)
bfd            *abfd;
{
  coff_symbol_type *new = (coff_symbol_type *) bfd_alloc(abfd, sizeof(coff_symbol_type));
  if (new == NULL) {
    bfd_error = no_memory;
    return (NULL);
  }				/* on error */
  new->symbol.section = 0;
  new->native = 0;
  new->lineno = (alent *) NULL;
  new->done_lineno = false;
  new->symbol.the_bfd = abfd;
  return &new->symbol;
}

#ifndef NO_COFF_SYMBOLS

static asymbol *
DEFUN (coff_make_debug_symbol, (abfd, ptr, sz),
       bfd *abfd AND
       PTR ptr AND
       unsigned long sz)
{
  coff_symbol_type *new = (coff_symbol_type *) bfd_alloc(abfd, sizeof(coff_symbol_type));
  if (new == NULL) {
    bfd_error = no_memory;
    return (NULL);
  }				/* on error */
  /* @@ This shouldn't be using a constant multiplier.  */
  new->native = (combined_entry_type *) bfd_zalloc (abfd, sizeof (combined_entry_type) * 10);
  new->symbol.section = &bfd_debug_section;
  new->lineno = (alent *) NULL;
  new->done_lineno = false;
  new->symbol.the_bfd = abfd;
  return &new->symbol;
}

static void
DEFUN(coff_print_symbol,(ignore_abfd, filep, symbol, how),
      bfd            *ignore_abfd AND
      PTR           filep AND
      asymbol        *symbol AND
      bfd_print_symbol_type how)
{
  FILE *file = (FILE *)filep;
  switch (how) {
    case bfd_print_symbol_name:
      fprintf(file, "%s", symbol->name);
      break;
    case bfd_print_symbol_more:
      fprintf(file, "coff %lx %lx", (unsigned long) coffsymbol(symbol)->native,
	      (unsigned long) coffsymbol(symbol)->lineno);
      break;
    case bfd_print_symbol_nm:

    {
      CONST char *section_name = symbol->section->name;
      bfd_print_symbol_vandf((PTR) file, symbol);

	
      fprintf(file, " %-5s %s %s %s",
	      section_name,
	      coffsymbol(symbol)->native ? "n" : "g",
	      coffsymbol(symbol)->lineno ? "l" : " ",
	      symbol->name);
    }


      break;
    case bfd_print_symbol_all:
      /* Print out the symbols in a reasonable way */
    {
      CONST char *section_name = symbol->section->name;


      if (coffsymbol(symbol)->native) 
      {
	unsigned int aux;
	combined_entry_type *combined = coffsymbol(symbol)->native;
	combined_entry_type *root = obj_raw_syments(ignore_abfd);
	
	fprintf(file,"[%3d]",
		combined - root);
	

	fprintf(file, "(sc %2d)(fl%4x)(ty%3x)(sc%3d) nx(%d) %08x %s",
		combined->u.syment.n_scnum,
		combined->u.syment.n_flags,
		combined->u.syment.n_type,
		combined->u.syment.n_sclass,
		combined->u.syment.n_numaux,
		combined->u.syment.n_value,
		symbol->name
		);
	for (aux = 0; aux < combined->u.syment.n_numaux; aux++) 
	{
	  fprintf(file,"\n");
	  switch (combined->u.syment.n_sclass) {
	    case C_FILE:
	      fprintf(file, "File ");
	      break;
	    default:
	      fprintf(file, "AUX lnno %x size %x tagndx %x",
		      combined[aux+1].u.auxent.x_sym.x_misc.x_lnsz.x_lnno,
		      combined[aux+1].u.auxent.x_sym.x_misc.x_lnsz.x_size,
		      combined[aux+1].u.auxent.x_sym.x_tagndx.l);
	      break;
    
	    }

	}
	
      {
	struct lineno_cache_entry *l = coffsymbol(symbol)->lineno;
	if (l) 
	{
	  printf("\n%s :", l->u.sym->name);
	  l++;
	  while (l->line_number) 
	  {
	    printf("\n%4d : %x", 
		   l->line_number,
		   l->u.offset);
	    l++;
	    
	  }
	}
      }

    

      } 

      else {
	  bfd_print_symbol_vandf((PTR) file, symbol);
	  fprintf(file, " %-5s %s %s %s",
		  section_name,
		  coffsymbol(symbol)->native ? "n" : "g",
		  coffsymbol(symbol)->lineno ? "l" : " ",
		  symbol->name);
	}

    }
	
    }
}

#endif /* NO_COFF_SYMBOLS */

/* Set flags and magic number of a coff file from architecture and machine
   type.  Result is true if we can represent the arch&type, false if not.  */

static          boolean
DEFUN(coff_set_flags,(abfd, magicp, flagsp),
      bfd            *abfd AND
      unsigned       *magicp AND
      unsigned short *flagsp)
{
  switch (bfd_get_arch(abfd)) {
#ifdef Z8KMAGIC
   case bfd_arch_z8k:
    *magicp = Z8KMAGIC;
    switch (bfd_get_mach(abfd)) 
    {
     case bfd_mach_z8001:
      *flagsp = F_Z8001;
      break;
     case bfd_mach_z8002:
      *flagsp = F_Z8002;
      break;
     default:
      return false;
    }
    return true;
#endif
#ifdef I960ROMAGIC

    case bfd_arch_i960:

      {
	unsigned        flags;
	*magicp = I960ROMAGIC;
	/*
	  ((bfd_get_file_flags(abfd) & WP_TEXT) ? I960ROMAGIC :
	  I960RWMAGIC);   FIXME???
	  */
	switch (bfd_get_mach(abfd)) {
	case bfd_mach_i960_core:
	  flags = F_I960CORE;
	  break;
	case bfd_mach_i960_kb_sb:
	  flags = F_I960KB;
	  break;
	case bfd_mach_i960_mc:
	  flags = F_I960MC;
	  break;
	case bfd_mach_i960_xa:
	  flags = F_I960XA;
	  break;
	case bfd_mach_i960_ca:
	  flags = F_I960CA;
	  break;
	case bfd_mach_i960_ka_sa:
	  flags = F_I960KA;
	  break;
	default:
	  return false;
	}
	*flagsp = flags;
	return true;
      }
    break;
#endif
#ifdef MIPS
  case bfd_arch_mips:
    *magicp = MIPS_MAGIC_2;
    return true;
    break;
#endif
#ifdef I386MAGIC
  case bfd_arch_i386:
    *magicp = I386MAGIC;
    return true;
    break;
#endif
#ifdef MC68MAGIC
  case bfd_arch_m68k:
    *magicp = MC68MAGIC;
    return true;
    break;
#endif

#ifdef MC88MAGIC
    case bfd_arch_m88k:
      *magicp = MC88OMAGIC;
      return true;
      break;
#endif
#ifdef H8300MAGIC
    case bfd_arch_h8300:
      *magicp = H8300MAGIC;
      return true;
      break;
#endif
#ifdef A29K_MAGIC_BIG
    case bfd_arch_a29k:
      if (abfd->xvec->byteorder_big_p)
       *magicp = A29K_MAGIC_BIG;
      else
       *magicp = A29K_MAGIC_LITTLE;
      return true;
      break;
#endif

#ifdef WE32KMAGIC
  case bfd_arch_we32k:
    *magicp = WE32KMAGIC;
    return true;
    break;
#endif

#ifdef U802TOCMAGIC
  case bfd_arch_rs6000:
    *magicp = U802TOCMAGIC;
    return true;
    break;
#endif

  default:			/* Unknown architecture */
    /* return false;  -- fall through to "return false" below, to avoid
       "statement never reached" errors on the one below. */
    break;
  }

  return false;
}


static          boolean
DEFUN(coff_set_arch_mach,(abfd, arch, machine),
      bfd            *abfd AND
      enum bfd_architecture arch AND
      unsigned long   machine)
{
  unsigned        dummy1;
  unsigned     short dummy2;
  bfd_default_set_arch_mach(abfd, arch, machine);

  if (arch != bfd_arch_unknown &&
      coff_set_flags(abfd, &dummy1, &dummy2) != true)
    return false;		/* We can't represent this type */
  return true;			/* We're easy ... */
}


/* Calculate the file position for each section. */

static void
DEFUN(coff_compute_section_file_positions,(abfd),
      bfd            *abfd)
{
  asection       *current;
  asection	*previous = (asection *)NULL;
  file_ptr        sofar = FILHSZ;
  file_ptr	old_sofar;
  if (bfd_get_start_address(abfd)) 
  {
    /*  A start address may have been added to the original file. In this
	case it will need an optional header to record it.  */
    abfd->flags |= EXEC_P;
  }

  if (abfd->flags & EXEC_P)
   sofar += AOUTSZ;

  sofar += abfd->section_count * SCNHSZ;
  for (current = abfd->sections;
       current != (asection *)NULL;
       current = current->next) {

      /* Only deal with sections which have contents */
      if (!(current->flags & SEC_HAS_CONTENTS))
       continue;

      /* Align the sections in the file to the same boundary on
	 which they are aligned in virtual memory.  I960 doesn't
	 do this (FIXME) so we can stay in sync with Intel.  960
	 doesn't yet page from files... */
#ifndef I960
    {
      /* make sure this section is aligned on the right boundary - by
	 padding the previous section up if necessary */

      old_sofar= sofar;
      sofar = BFD_ALIGN(sofar, 1 << current->alignment_power);
      if (previous != (asection *)NULL) {
	  previous->_raw_size += sofar - old_sofar;
	}
    }

#endif
      /* FIXME, in demand paged files, the low order bits of the file
	 offset must match the low order bits of the virtual address.
	 "Low order" is apparently implementation defined.  Add code
	 here to round sofar up to match the virtual address.  */

      current->filepos = sofar;

      sofar += current->_raw_size;
#ifndef I960
      /* make sure that this section is of the right size too */
      old_sofar =  sofar;
      sofar = BFD_ALIGN(sofar, 1 << current->alignment_power);
      current->_raw_size += sofar - old_sofar ;
#endif

      previous = current;
    }
  obj_relocbase(abfd) = sofar;
}

#ifndef NO_COFF_SYMBOLS
static asymbol *
coff_section_symbol (abfd, name)
     bfd *abfd;
     char *name;
{
  asection *sec = bfd_make_section_old_way (abfd, name);
  asymbol *sym;
  combined_entry_type *csym;

  sym = sec->symbol;
  if (coff_symbol_from (abfd, sym))
    csym = coff_symbol_from (abfd, sym)->native;
  else
    csym = 0;
  /* Make sure back-end COFF stuff is there.  */
  if (csym == 0)
    {
      struct foo {
	coff_symbol_type sym;
	/* @@FIXME This shouldn't use a fixed size!!  */
	combined_entry_type e[10];
      };
      struct foo *f;
      f = (struct foo *) bfd_alloc_by_size_t (abfd, sizeof (*f));
      memset ((char *) f, 0, sizeof (*f));
      coff_symbol_from (abfd, sym)->native = csym = f->e;
    }
  csym[0].u.syment.n_sclass = C_STAT;
  csym[0].u.syment.n_numaux = 1;
/*  SF_SET_STATICS (sym);	@@ ??? */
  if (sec)
    {
      csym[1].u.auxent.x_scn.x_scnlen = sec->_raw_size;
      csym[1].u.auxent.x_scn.x_nreloc = sec->reloc_count;
      csym[1].u.auxent.x_scn.x_nlinno = sec->lineno_count;
    }
  else
    {
      csym[1].u.auxent.x_scn.x_scnlen = 0;
      csym[1].u.auxent.x_scn.x_nreloc = 0;
      csym[1].u.auxent.x_scn.x_nlinno = 0;
    }
  return sym;
}

/* If .file, .text, .data, .bss symbols are missing, add them.  */
/* @@ Should we only be adding missing symbols, or overriding the aux
   values for existing section symbols?  */
static void
coff_add_missing_symbols (abfd)
     bfd *abfd;
{
  unsigned int nsyms = bfd_get_symcount (abfd);
  asymbol **sympp = abfd->outsymbols;
  asymbol **sympp2;
  unsigned int i;
  int need_text = 1, need_data = 1, need_bss = 1, need_file = 1;

  for (i = 0; i < nsyms; i++)
    {
      coff_symbol_type *csym = coff_symbol_from (abfd, sympp[i]);
      CONST char *name;
      if (csym) 
      {
	/* only do this if there is a coff representation of the input
	   symbol */
	if (csym->native && csym->native->u.syment.n_sclass == C_FILE)
	{
	  need_file = 0;
	  continue;
	}
	name = csym->symbol.name;
	if (!name)
	 continue;
	if (!strcmp (name, _TEXT))
	 need_text = 0;
	else if (!strcmp (name, _DATA))
	 need_data = 0;
	else if (!strcmp (name, _BSS))
	 need_bss = 0;
      }
    }
  /* Now i == bfd_get_symcount (abfd).  */
  /* @@ For now, don't deal with .file symbol.  */
  need_file = 0;

  if (!need_text && !need_data && !need_bss && !need_file)
    return;
  nsyms += need_text + need_data + need_bss + need_file;
  sympp2 = (asymbol**) bfd_alloc_by_size_t (abfd, nsyms * sizeof (asymbol *));
  memcpy (sympp2, sympp, i * sizeof (asymbol *));
  if (need_file)
    {
      /* @@ Generate fake .file symbol, in sympp2[i], and increment i.  */
      abort ();
    }
  if (need_text)
    sympp2[i++] = coff_section_symbol (abfd, _TEXT);
  if (need_data)
    sympp2[i++] = coff_section_symbol (abfd, _DATA);
  if (need_bss)
    sympp2[i++] = coff_section_symbol (abfd, _BSS);
  BFD_ASSERT (i == nsyms);
  bfd_set_symtab (abfd, sympp2, nsyms);
}
#endif /* NO_COFF_SYMBOLS */

/* SUPPRESS 558 */
/* SUPPRESS 529 */
static          boolean
DEFUN(coff_write_object_contents,(abfd),
      bfd            *abfd)
{
  asection       *current;
  unsigned int count;
  
  boolean         hasrelocs = false;
  boolean         haslinno = false;
  file_ptr        reloc_base;
  file_ptr        lineno_base;
  file_ptr        sym_base;
  file_ptr        scn_base;
  file_ptr        data_base;
  unsigned long   reloc_size = 0;
  unsigned long   lnno_size = 0;
  asection       *text_sec = NULL;
  asection       *data_sec = NULL;
  asection       *bss_sec = NULL;

  struct internal_filehdr internal_f;
  struct internal_aouthdr internal_a;


  bfd_error = system_call_error;
  /* Number the output sections, starting from one on the first section
     with a name which doesn't start with a *.
     @@ The code doesn't make this check.  Is it supposed to be done,
     or isn't it??  */
  count = 1;
  for (current = abfd->sections; current != (asection *)NULL; 
       current = current->next) 
  {
      current->target_index = count;
      count++;
  }

  if(abfd->output_has_begun == false) {
      coff_compute_section_file_positions(abfd);
    }

  if (abfd->sections != (asection *)NULL) {
      scn_base = abfd->sections->filepos;
    }
  else {
      scn_base = 0;
    }
  if (bfd_seek(abfd, scn_base, SEEK_SET) != 0)
   return false;
  reloc_base = obj_relocbase(abfd);

  /* Make a pass through the symbol table to count line number entries and
     put them into the correct asections */

#ifndef NO_COFF_LINENOS
  coff_count_linenumbers(abfd);
#endif
  data_base = scn_base;

  /* Work out the size of the reloc and linno areas */

  for (current = abfd->sections; current != NULL; current =
       current->next) 
  {
    /* We give section headers to +ve indexes */
    if (current->target_index > 0) 
    {
	
      reloc_size += current->reloc_count * RELSZ;
#ifndef NO_COFF_LINENOS
      lnno_size += current->lineno_count * LINESZ;
#endif
      data_base += SCNHSZ;
    }
      
  }

  lineno_base = reloc_base + reloc_size;
  sym_base = lineno_base + lnno_size;

  /* Indicate in each section->line_filepos its actual file address */
  for (current = abfd->sections; current != NULL; current =
       current->next) 
  {
    if (current->target_index > 0) 
    {
	
      if (current->lineno_count) {
	  current->line_filepos = lineno_base;
	  current->moving_line_filepos = lineno_base;
#ifndef NO_COFF_LINENOS
	  lineno_base += current->lineno_count * LINESZ;
#endif
	}
      else {
	  current->line_filepos = 0;
	}
      if (current->reloc_count) {
	  current->rel_filepos = reloc_base;
	  reloc_base += current->reloc_count * RELSZ;
	}
      else {
	  current->rel_filepos = 0;
	}
    }
  }    
      


  /* Write section headers to the file.  */
  internal_f.f_nscns = 0;
  bfd_seek(abfd,
	   (file_ptr) ((abfd->flags & EXEC_P) ?
		       (FILHSZ + AOUTSZ) : FILHSZ),
	   SEEK_SET);

{
#if 0
  unsigned int    pad = abfd->flags & D_PAGED ? data_base : 0;
#endif
  unsigned int    pad = 0;

  for (current = abfd->sections;
       current != NULL;
       current = current->next) {
      struct internal_scnhdr section;
      if (current->target_index > 0) 
      {
	internal_f.f_nscns ++;
	strncpy(&(section.s_name[0]), current->name, 8);
	section.s_vaddr = current->vma + pad;
	section.s_paddr = current->vma + pad;
	section.s_size = current->_raw_size - pad;
	/*
	  If this section has no size or is unloadable then the scnptr
	  will be 0 too
	  */
	if (current->_raw_size - pad == 0 ||
	    (current->flags & SEC_LOAD) == 0) {
	    section.s_scnptr = 0;
	  }
	else {
	    section.s_scnptr = current->filepos;
	  }
	section.s_relptr = current->rel_filepos;
	section.s_lnnoptr = current->line_filepos;
	section.s_nreloc = current->reloc_count;
	section.s_nlnno = current->lineno_count;
	if (current->reloc_count != 0)
	 hasrelocs = true;
	if (current->lineno_count != 0)
	 haslinno = true;

	section.s_flags = sec_to_styp_flags(current->name,current->flags);

	if (!strcmp(current->name, _TEXT)) {
	    text_sec = current;
	  } else if (!strcmp(current->name, _DATA)) {
	      data_sec = current;
	    } else if (!strcmp(current->name, _BSS)) {
		bss_sec = current;
	      }

#ifdef I960
	section.s_align = (current->alignment_power
			   ? 1 << current->alignment_power
			   : 0);

#endif
      {
	SCNHDR          buff;

	coff_swap_scnhdr_out(abfd, &section, &buff);
	bfd_write((PTR) (&buff), 1, SCNHSZ, abfd);

      }

	pad = 0;
      }
    }	
}


  /* OK, now set up the filehdr... */

  /* Don't include the internal abs section in the section count */

  /*
    We will NOT put a fucking timestamp in the header here. Every time you
    put it back, I will come in and take it out again. I'm sorry. This
    field does not belong here.  We fill it with a 0 so it compares the
    same but is not a reasonable time. -- gnu@cygnus.com
    */
  /*
    Well, I like it, and now we have *customers* who have requested it,
    so I'm conditionally compiling it in.

    sac@cygnus.com
    */
#ifndef NOCOFF_TIMESTAMP
  internal_f.f_timdat = time(0);
#else
  internal_f.f_timdat = 0;
#endif

  if (bfd_get_symcount(abfd) != 0)
   internal_f.f_symptr = sym_base;
  else
   internal_f.f_symptr = 0;

  internal_f.f_flags = 0;

  if (abfd->flags & EXEC_P)
   internal_f.f_opthdr = AOUTSZ;
  else
   internal_f.f_opthdr = 0;

  if (!hasrelocs)
   internal_f.f_flags |= F_RELFLG;
  if (!haslinno)
   internal_f.f_flags |= F_LNNO;
  if (0 == bfd_get_symcount(abfd))
   internal_f.f_flags |= F_LSYMS;
  if (abfd->flags & EXEC_P)
   internal_f.f_flags |= F_EXEC;

  if (!abfd->xvec->byteorder_big_p)
   internal_f.f_flags |= F_AR32WR;
  else 
  internal_f.f_flags |= F_AR32W;

  /*
    FIXME, should do something about the other byte orders and
    architectures.
    */

  /* Set up architecture-dependent stuff */

{ unsigned int   magic = 0;
  unsigned short    flags = 0;
  coff_set_flags(abfd, &magic, &flags);
  internal_f.f_magic = magic;
  internal_f.f_flags |= flags;
  /* ...and the "opt"hdr... */

#ifdef A29K
# ifdef ULTRA3			/* NYU's machine */
  /* FIXME: This is a bogus check.  I really want to see if there
   * is a .shbss or a .shdata section, if so then set the magic
   * number to indicate a shared data executable.
   */
  if (internal_f.f_nscns >= 7)
   internal_a.magic = SHMAGIC; 	/* Shared magic */
  else
# endif				/* ULTRA3 */
   internal_a.magic = NMAGIC; 	/* Assume separate i/d */
#define __A_MAGIC_SET__
#endif				/* A29K */
#ifdef I960
  internal_a.magic = (magic == I960ROMAGIC ? NMAGIC : OMAGIC);
#define __A_MAGIC_SET__
#endif				/* I960 */
#if M88
#define __A_MAGIC_SET__
  internal_a.magic = PAGEMAGICBCS;
#endif				/* M88 */

#if M68 || MIPS || WE32K
#define __A_MAGIC_SET__
  /* Never was anything here for the 68k */
#endif				/* M68 || MIPS || WE32K */

#if I386
# define __A_MAGIC_SET__
  internal_a.magic = ZMAGIC;
#endif /* I386 */

#if RS6000COFF_C
#define __A_MAGIC_SET__
  internal_a.magic = (abfd->flags & D_PAGED)? RS6K_AOUTHDR_ZMAGIC:
  (abfd->flags & WP_TEXT)? RS6K_AOUTHDR_NMAGIC:
  RS6K_AOUTHDR_OMAGIC;
#endif

#ifndef __A_MAGIC_SET__
# include "Your aouthdr magic number is not being set!"
#else
# undef __A_MAGIC_SET__
#endif
}
  /* Now should write relocs, strings, syms */
  obj_sym_filepos(abfd) = sym_base;

#ifndef NO_COFF_SYMBOLS
  if (bfd_get_symcount(abfd) != 0) {
      coff_add_missing_symbols (abfd);
      coff_renumber_symbols(abfd);
      coff_mangle_symbols(abfd);
      coff_write_symbols(abfd);
      coff_write_linenumbers(abfd);
      coff_write_relocs(abfd);
    }
#endif				/* NO_COFF_SYMBOLS */
  if (text_sec) {
      internal_a.tsize = bfd_get_section_size_before_reloc(text_sec);
      internal_a.text_start = internal_a.tsize ? text_sec->vma : 0;
    }
  if (data_sec) {
      internal_a.dsize = bfd_get_section_size_before_reloc(data_sec);
      internal_a.data_start = internal_a.dsize ? data_sec->vma      : 0;
    }
  if (bss_sec) {
      internal_a.bsize = bfd_get_section_size_before_reloc(bss_sec);
    }

  internal_a.entry = bfd_get_start_address(abfd);
  internal_f.f_nsyms =  bfd_get_symcount(abfd);

  /* now write them */
  if (bfd_seek(abfd, (file_ptr) 0, SEEK_SET) != 0)
   return false;
{
  FILHDR buff;
  coff_swap_filehdr_out(abfd, (PTR)&internal_f, (PTR)&buff);
  bfd_write((PTR) &buff, 1, FILHSZ, abfd);
}
  if (abfd->flags & EXEC_P) {
      AOUTHDR buff;
      coff_swap_aouthdr_out(abfd, (PTR)&internal_a, (PTR)&buff);
      bfd_write((PTR) &buff, 1, AOUTSZ, abfd);
    }
  return true;
}

#ifndef NO_COFF_SYMBOLS

/*
this function transforms the offsets into the symbol table into
pointers to syments.
*/


static void
DEFUN(coff_pointerize_aux,(ignore_abfd, table_base, type, class, auxent),
bfd *ignore_abfd AND
combined_entry_type *table_base AND
int type AND
int class AND
combined_entry_type *auxent)
{
  /* Don't bother if this is a file or a section */
  if (class == C_STAT && type == T_NULL) return;
  if (class == C_FILE) return;

  /* Otherwise patch up */
  if (ISFCN(type) || ISTAG(class) || class == C_BLOCK) {
      auxent->u.auxent.x_sym.x_fcnary.x_fcn.x_endndx.p = table_base +
       auxent->u.auxent.x_sym.x_fcnary.x_fcn.x_endndx.l;
      auxent->fix_end = 1;
    }
  if (auxent->u.auxent.x_sym.x_tagndx.l != 0) {
      auxent->u.auxent.x_sym.x_tagndx.p =
       table_base +  auxent->u.auxent.x_sym.x_tagndx.l;
      auxent->fix_tag = 1;
    }
}

#endif /* NO_COFF_SYMBOLS */

static          boolean
DEFUN(coff_set_section_contents,(abfd, section, location, offset, count),
      bfd            *abfd AND
      sec_ptr         section AND
      PTR             location AND
      file_ptr        offset AND
      bfd_size_type   count)
{
    if (abfd->output_has_begun == false)	/* set by bfd.c handler */
	coff_compute_section_file_positions(abfd);

    bfd_seek(abfd, (file_ptr) (section->filepos + offset), SEEK_SET);

    if (count != 0) {
	return (bfd_write(location, 1, count, abfd) == count) ? true : false;
    }
    return true;
}
#if 0
static          boolean
coff_close_and_cleanup(abfd)
    bfd            *abfd;
{
  if (!bfd_read_p(abfd))
    switch (abfd->format) {
    case bfd_archive:
      if (!_bfd_write_archive_contents(abfd))
	return false;
      break;
    case bfd_object:
      if (!coff_write_object_contents(abfd))
	return false;
      break;
    default:
      bfd_error = invalid_operation;
      return false;
    }

  /* We depend on bfd_close to free all the memory on the obstack.  */
  /* FIXME if bfd_release is not using obstacks! */
  return true;
}

#endif
static PTR
buy_and_read(abfd, where, seek_direction, size)
    bfd            *abfd;
    file_ptr        where;
    int             seek_direction;
    size_t          size;
{
    PTR             area = (PTR) bfd_alloc(abfd, size);
    if (!area) {
	bfd_error = no_memory;
	return (NULL);
    }
    bfd_seek(abfd, where, seek_direction);
    if (bfd_read(area, 1, size, abfd) != size) {
	bfd_error = system_call_error;
	return (NULL);
    }				/* on error */
    return (area);
}				/* buy_and_read() */


#ifndef NO_COFF_SYMBOLS

static char *
DEFUN(build_string_table,(abfd),
bfd *abfd)
{
  char string_table_size_buffer[4];
  unsigned int string_table_size;
  char *string_table;

  /* At this point we should be "seek"'d to the end of the
     symbols === the symbol table size.  */
  if (bfd_read((char *) string_table_size_buffer,
	       sizeof(string_table_size_buffer),
	       1, abfd) != sizeof(string_table_size)) {
    bfd_error = system_call_error;
    return (NULL);
  }				/* on error */

  string_table_size = bfd_h_get_32(abfd, (bfd_byte *) string_table_size_buffer);

  if ((string_table = (PTR) bfd_alloc(abfd, string_table_size -= 4)) == NULL) {
    bfd_error = no_memory;
    return (NULL);
  }				/* on mallocation error */
  if (bfd_read(string_table, string_table_size, 1, abfd) != string_table_size) {
    bfd_error = system_call_error;
    return (NULL);
  }
  return string_table;
}

/* Allocate space for the ".debug" section, and read it.
   We did not read the debug section until now, because
   we didn't want to go to the trouble until someone needed it. */

static char *
DEFUN(build_debug_section,(abfd),
	bfd *abfd)
{
  char *debug_section;
  long position;

  asection *sect = bfd_get_section_by_name (abfd, ".debug");

  if (!sect) {
     bfd_error = no_debug_section;
     return NULL;
  }

  debug_section = (PTR) bfd_alloc (abfd,
				   bfd_get_section_size_before_reloc (sect));
  if (debug_section == NULL) {
    bfd_error = no_memory;
    return NULL;
  }

  /* Seek to the beginning of the `.debug' section and read it. 
     Save the current position first; it is needed by our caller.
     Then read debug section and reset the file pointer.  */

  position = bfd_tell (abfd);
  bfd_seek (abfd, sect->filepos, SEEK_SET);
  if (bfd_read (debug_section, 
		bfd_get_section_size_before_reloc (sect), 1, abfd)
      != bfd_get_section_size_before_reloc(sect)) {
    bfd_error = system_call_error;
    return NULL;
  }
  bfd_seek (abfd, position, SEEK_SET);
  return debug_section;
}


/* Return a pointer to a malloc'd copy of 'name'.  'name' may not be
 \0-terminated, but will not exceed 'maxlen' characters.  The copy *will*
 be \0-terminated.  */
static char *
DEFUN(copy_name,(abfd, name, maxlen),
      bfd *abfd AND
      char *name AND
      int maxlen)
{
  int  len;
  char *newname;

  for (len = 0; len < maxlen; ++len) {
    if (name[len] == '\0') {
      break;
    }
  }

  if ((newname = (PTR) bfd_alloc(abfd, len+1)) == NULL) {
    bfd_error = no_memory;
    return (NULL);
  }
  strncpy(newname, name, len);
  newname[len] = '\0';
  return newname;
}


/* Read a symbol table into freshly bfd_allocated memory, swap it, and
   knit the symbol names into a normalized form.  By normalized here I
   mean that all symbols have an n_offset pointer that points to a null-
   terminated string.  */

#ifndef SYMNAME_IN_DEBUG
#define	SYMNAME_IN_DEBUG(x)  0
#endif

static combined_entry_type *
DEFUN(get_normalized_symtab,(abfd),
bfd            *abfd)
{
  combined_entry_type          *internal;
  combined_entry_type          *internal_ptr;
  combined_entry_type          *symbol_ptr;
  combined_entry_type         *internal_end;
  SYMENT *raw;
  SYMENT *raw_src;
  SYMENT *raw_end;
  char           *string_table = NULL;
  char		 *debug_section = NULL;
  unsigned long   size;

  unsigned int raw_size;
  if (obj_raw_syments(abfd) != (combined_entry_type *)NULL) {
      return obj_raw_syments(abfd);
    }
  if ((size = bfd_get_symcount(abfd) * sizeof(combined_entry_type)) == 0) {
      bfd_error = no_symbols;
      return (NULL);
    }

  internal = (combined_entry_type *)bfd_alloc(abfd, size);
  internal_end = internal + bfd_get_symcount(abfd);

  raw_size =      bfd_get_symcount(abfd) * SYMESZ;
  raw = (SYMENT *)bfd_alloc(abfd,raw_size);

  if (bfd_seek(abfd, obj_sym_filepos(abfd), SEEK_SET) == -1
      || bfd_read((PTR)raw, raw_size, 1, abfd) != raw_size) {
      bfd_error = system_call_error;
      return (NULL);
    }
  /* mark the end of the symbols */
  raw_end = raw + bfd_get_symcount(abfd);
  /*
    FIXME SOMEDAY.  A string table size of zero is very weird, but
    probably possible.  If one shows up, it will probably kill us.
    */

  /* Swap all the raw entries */
  for (raw_src = raw, internal_ptr = internal;
       raw_src < raw_end;
       raw_src++, internal_ptr++) {

      unsigned int i;
      coff_swap_sym_in(abfd, (PTR)raw_src, (PTR)&internal_ptr->u.syment);
      internal_ptr->fix_tag = 0;
      internal_ptr->fix_end = 0;
      symbol_ptr = internal_ptr;
    
      for (i = 0;
	   i < symbol_ptr->u.syment.n_numaux;
	   i++) 
      {
	internal_ptr++;
	raw_src++;
      
	internal_ptr->fix_tag = 0;
	internal_ptr->fix_end = 0;
	coff_swap_aux_in(abfd, (char *)(raw_src),
			 symbol_ptr->u.syment.n_type,
			 symbol_ptr->u.syment.n_sclass,
			 &(internal_ptr->u.auxent));
	/* Remember that bal entries arn't pointerized */
	if (i != 1 || symbol_ptr->u.syment.n_sclass != C_LEAFPROC)
	{
	  
	coff_pointerize_aux(abfd,
			    internal,
			    symbol_ptr->u.syment.n_type,
			    symbol_ptr->u.syment.n_sclass,
			    internal_ptr);
      }
	
      }
    }

  /* Free all the raw stuff */
  bfd_release(abfd, raw);

  for (internal_ptr = internal; internal_ptr < internal_end;
       internal_ptr ++)
  {
    if (internal_ptr->u.syment.n_sclass == C_FILE) {
	/* make a file symbol point to the name in the auxent, since
	   the text ".file" is redundant */
	if ((internal_ptr+1)->u.auxent.x_file.x_n.x_zeroes == 0) {
	    /* the filename is a long one, point into the string table */
	    if (string_table == NULL) {
		string_table = build_string_table(abfd);
	      }

	    internal_ptr->u.syment._n._n_n._n_offset =
	     (int) (string_table - 4 +
		    (internal_ptr+1)->u.auxent.x_file.x_n.x_offset);
	  }
	else {
	    /* ordinary short filename, put into memory anyway */
	    internal_ptr->u.syment._n._n_n._n_offset = (int)
	     copy_name(abfd, (internal_ptr+1)->u.auxent.x_file.x_fname,
		       FILNMLEN);
	  }
      }
    else {
	if (internal_ptr->u.syment._n._n_n._n_zeroes != 0) {
	    /* This is a "short" name.  Make it long.  */
	    unsigned long   i = 0;
	    char           *newstring = NULL;

	    /* find the length of this string without walking into memory
	       that isn't ours.  */
	    for (i = 0; i < 8; ++i) {
		if (internal_ptr->u.syment._n._n_name[i] == '\0') {
		    break;
		  }		/* if end of string */
	      }			/* possible lengths of this string. */

	    if ((newstring = (PTR) bfd_alloc(abfd, ++i)) == NULL) {
		bfd_error = no_memory;
		return (NULL);
	      }			/* on error */
	    memset(newstring, 0, i);
	    strncpy(newstring, internal_ptr->u.syment._n._n_name, i-1);
	    internal_ptr->u.syment._n._n_n._n_offset =  (int) newstring;
	    internal_ptr->u.syment._n._n_n._n_zeroes = 0;
	  }
	else if (!SYMNAME_IN_DEBUG(&internal_ptr->u.syment)) {
	    /* Long name already.  Point symbol at the string in the table.  */
	    if (string_table == NULL) {
		string_table = build_string_table(abfd);
	      }
	    internal_ptr->u.syment._n._n_n._n_offset = (int)
	     (string_table - 4 + internal_ptr->u.syment._n._n_n._n_offset);
	  }
	else {
	    /* Long name in debug section.  Very similar.  */
	    if (debug_section == NULL) {
		debug_section = build_debug_section(abfd);
	      }
	    internal_ptr->u.syment._n._n_n._n_offset = (int)
	     (debug_section + internal_ptr->u.syment._n._n_n._n_offset);
	  }
      }
    internal_ptr += internal_ptr->u.syment.n_numaux;
  }

  obj_raw_syments(abfd) = internal;

  return (internal);
}				/* get_normalized_symtab() */

#endif /* NO_COFF_SYMBOLS */

static
struct sec *
DEFUN(section_from_bfd_index,(abfd, index),
      bfd            *abfd AND
      int             index)
{
  struct sec *answer = abfd->sections;

  if (index == N_ABS) 
  {
    return &bfd_abs_section;
  }
  if (index == N_UNDEF)
  {
    return &bfd_und_section;
  }
  if(index == N_DEBUG)
  {
    return &bfd_debug_section;
    
  }
  
  while (answer) {
      if (answer->target_index == index)
       return answer;
      answer = answer->next;
    }
  BFD_ASSERT(0);
  return &bfd_und_section;	/* For gcc -W and lint.  Never executed. */
}

#ifndef NO_COFF_LINENOS

/*
SUBSUBSECTION 
	Reading Linenumbers

	Creating the linenumber table is done by reading in the entire
	coff linenumber table, and creating another table for internal use.

	A coff line number table is structured so that each function
	is marked as having a line number of 0. Each line within the
	function is an offset from the first line in the function. The
	base of the line number information for the table is stored in
	the symbol associated with the function. 

	The information is copied from the external to the internal
	table, and each symbol which marks a function is marked by
	pointing its...

	How does this work ?

*/

static boolean
coff_slurp_line_table(abfd, asect)
bfd            *abfd;
asection       *asect;
  {
    LINENO  *native_lineno;
    alent          *lineno_cache;

    BFD_ASSERT(asect->lineno == (alent *) NULL);

    native_lineno = (LINENO *) buy_and_read(abfd,
					    asect->line_filepos,
					    SEEK_SET,
					    (size_t) (LINESZ *
						      asect->lineno_count));
    lineno_cache =
      (alent *) bfd_alloc(abfd, (size_t) ((asect->lineno_count + 1) * sizeof(alent)));
    if (lineno_cache == NULL) {
      bfd_error = no_memory;
      return false;
    } else {
      unsigned int    counter = 0;
      alent          *cache_ptr = lineno_cache;
      LINENO  *src = native_lineno;

      while (counter < asect->lineno_count) {
	struct internal_lineno dst;
	coff_swap_lineno_in(abfd, src, &dst);
	cache_ptr->line_number = dst.l_lnno;

	if (cache_ptr->line_number == 0) {
	  coff_symbol_type *sym =
	    (coff_symbol_type *) (dst.l_addr.l_symndx
				  + obj_raw_syments(abfd))->u.syment._n._n_n._n_zeroes;
	  cache_ptr->u.sym = (asymbol *) sym;
	  sym->lineno = cache_ptr;
	}
	else {
	  cache_ptr->u.offset = dst.l_addr.l_paddr
	    - bfd_section_vma(abfd, asect);
	}				/* If no linenumber expect a symbol index */

	cache_ptr++;
	src++;
	counter++;
      }
      cache_ptr->line_number = 0;

    }
    asect->lineno = lineno_cache;
    /* FIXME, free native_lineno here, or use alloca or something. */
    return true;
  }				/* coff_slurp_line_table() */

#endif /* NO_COFF_LINENOS */

#ifndef NO_COFF_LINENOS

static          boolean
DEFUN(coff_slurp_symbol_table,(abfd),
      bfd            *abfd)
{
  combined_entry_type         *native_symbols;
  coff_symbol_type *cached_area;
  unsigned int   *table_ptr;

  unsigned int    number_of_symbols = 0;
  if (obj_symbols(abfd))
    return true;
  bfd_seek(abfd, obj_sym_filepos(abfd), SEEK_SET);

  /* Read in the symbol table */
  if ((native_symbols = get_normalized_symtab(abfd)) == NULL) {
    return (false);
  }				/* on error */

  /* Allocate enough room for all the symbols in cached form */
  cached_area =
    (coff_symbol_type *)
      bfd_alloc(abfd, (size_t) (bfd_get_symcount(abfd) * sizeof(coff_symbol_type)));

  if (cached_area == NULL) {
    bfd_error = no_memory;
    return false;
  }				/* on error */
  table_ptr =
    (unsigned int *)
      bfd_alloc(abfd, (size_t) (bfd_get_symcount(abfd) * sizeof(unsigned int)));

  if (table_ptr == NULL) {
    bfd_error = no_memory;
    return false;
  } 
  else 
  {
    coff_symbol_type *dst = cached_area;
    unsigned int    last_native_index = bfd_get_symcount(abfd);
    unsigned int    this_index = 0;
    while (this_index < last_native_index) {
      combined_entry_type         *src = native_symbols + this_index;
      table_ptr[this_index] = number_of_symbols;
      dst->symbol.the_bfd = abfd;

      dst->symbol.name = (char *)(src->u.syment._n._n_n._n_offset);
      /*
	We use the native name field to point to the cached field
	*/
      src->u.syment._n._n_n._n_zeroes = (int) dst;
      dst->symbol.section = section_from_bfd_index(abfd,
						   src->u.syment.n_scnum);
      dst->symbol.flags = 0;
      dst->done_lineno = false;
      
      switch (src->u.syment.n_sclass) {
#ifdef I960
      case C_LEAFEXT:
#if 0
	dst->symbol.value = src->u.syment.n_value - dst->symbol.section->vma;
	dst->symbol.flags = BSF_EXPORT | BSF_GLOBAL;
	dst->symbol.flags |= BSF_NOT_AT_END;
#endif
	/* Fall through to next case */

#endif

      case C_EXT:
#ifdef RS6000COFF_C
      case C_HIDEXT:
#endif
	if ((src->u.syment.n_scnum) == 0) {
	  if ((src->u.syment.n_value) == 0) {
	    dst->symbol.section = &bfd_und_section;
	    dst->symbol.value= 0;
	  }
	  else {
	    dst->symbol.section = &bfd_com_section;
	    dst->symbol.value = (src->u.syment.n_value);
	  }
	}
	else {
	  /*
	    Base the value as an index from the base of the
	    section
	    */

	    dst->symbol.flags = BSF_EXPORT | BSF_GLOBAL;
	    dst->symbol.value = src->u.syment.n_value - dst->symbol.section->vma;

	  if (ISFCN((src->u.syment.n_type))) {
	    /*
	      A function ext does not go at the end of a file
	      */
	    dst->symbol.flags |= BSF_NOT_AT_END;
	  }
	}


	break;

      case C_STAT:		/* static			 */
#ifdef I960
      case C_LEAFSTAT:		/* static leaf procedure        */
#endif
      case C_LABEL:		/* label			 */
	if (src->u.syment.n_scnum == -2)
	  dst->symbol.flags = BSF_DEBUGGING;
	else
	  dst->symbol.flags = BSF_LOCAL;
	/*
	  Base the value as an index from the base of the section, if
	  there is one
	  */
	if (dst->symbol.section)
	  dst->symbol.value = (src->u.syment.n_value) -
	    dst->symbol.section->vma;
	else
	  dst->symbol.value = (src->u.syment.n_value) ;
	break;

      case C_MOS:		/* member of structure	 */
      case C_EOS:		/* end of structure		 */
#ifdef NOTDEF	/* C_AUTOARG has the same value */
#ifdef C_GLBLREG
      case C_GLBLREG:		/* A29k-specific storage class */
#endif
#endif
      case C_REGPARM:		/* register parameter		 */
      case C_REG:		/* register variable		 */
#ifdef C_AUTOARG
      case C_AUTOARG:		/* 960-specific storage class */
#endif
      case C_TPDEF:		/* type definition		 */
      case C_ARG:
      case C_AUTO:		/* automatic variable */
      case C_FIELD:		/* bit field */
      case C_ENTAG:		/* enumeration tag		 */
      case C_MOE:		/* member of enumeration	 */
      case C_MOU:		/* member of union		 */
      case C_UNTAG:		/* union tag			 */
	dst->symbol.flags = BSF_DEBUGGING;
	dst->symbol.value = (src->u.syment.n_value);
	break;

      case C_FILE:		/* file name			 */
      case C_STRTAG:		/* structure tag		 */
#ifdef RS6000COFF_C
      case C_BINCL:		/* beginning of include file     */
      case C_EINCL:		/* ending of include file        */
      case C_GSYM:
      case C_LSYM:
      case C_PSYM:
      case C_RSYM:
      case C_RPSYM:
      case C_STSYM:
      case C_DECL:
      case C_ENTRY:
      case C_FUN:
      case C_BSTAT:
      case C_ESTAT:
#endif
	dst->symbol.flags = BSF_DEBUGGING;
	dst->symbol.value = (src->u.syment.n_value);
	break;

      case C_BLOCK:		/* ".bb" or ".eb"		 */
      case C_FCN:		/* ".bf" or ".ef"		 */
      case C_EFCN:		/* physical end of function	 */
	dst->symbol.flags = BSF_LOCAL;
	/*
	  Base the value as an index from the base of the section
	  */
	dst->symbol.value = (src->u.syment.n_value) - dst->symbol.section->vma;
	break;

      case C_NULL:
      case C_EXTDEF:		/* external definition		 */
      case C_ULABEL:		/* undefined label		 */
      case C_USTATIC:		/* undefined static		 */
      case C_LINE:		/* line # reformatted as symbol table entry */
      case C_ALIAS:		/* duplicate tag		 */
      case C_HIDDEN:		/* ext symbol in dmert public lib */
      default:

	fprintf(stderr,"Unrecognized storage class %d\n",
				src->u.syment.n_sclass);
/*	abort();*/
	dst->symbol.flags = BSF_DEBUGGING;
	dst->symbol.value = (src->u.syment.n_value);
	break;
      }

/*      BFD_ASSERT(dst->symbol.flags != 0);*/

      dst->native = src;

      dst->symbol.udata = 0;
      dst->lineno = (alent *) NULL;
      this_index += (src->u.syment.n_numaux) + 1;
      dst++;
      number_of_symbols++;
    }				/* walk the native symtab */
  }				/* bfdize the native symtab */

  obj_symbols(abfd) = cached_area;
  obj_raw_syments(abfd) = native_symbols;

  obj_conv_table_size (abfd) = bfd_get_symcount (abfd);
  bfd_get_symcount(abfd) = number_of_symbols;
  obj_convert(abfd) = table_ptr;
  /* Slurp the line tables for each section too */
    {
      asection       *p;
      p = abfd->sections;
      while (p) {
	coff_slurp_line_table(abfd, p);
	p = p->next;
      }
    }
  return true;
}				/* coff_slurp_symbol_table() */

static unsigned int
coff_get_symtab_upper_bound(abfd)
bfd            *abfd;
  {
    if (!coff_slurp_symbol_table(abfd))
      return 0;

    return (bfd_get_symcount(abfd) + 1) * (sizeof(coff_symbol_type *));
  }


static unsigned int
DEFUN(coff_get_symtab, (abfd, alocation),
      bfd            *abfd AND
      asymbol       **alocation)
{
    unsigned int    counter = 0;
    coff_symbol_type *symbase;
    coff_symbol_type **location = (coff_symbol_type **) (alocation);
    if (!coff_slurp_symbol_table(abfd))
     return 0;

    symbase = obj_symbols(abfd);
    while (counter <  bfd_get_symcount(abfd))
    {
	/* This nasty code looks at the symbol to decide whether or
	   not it is descibes a constructor/destructor entry point. It
	   is structured this way to (hopefully) speed non matches */
#if 0	
	if (0 && symbase->symbol.name[9] == '$') 
	{
	    bfd_constructor_entry(abfd, 
				 (asymbol **)location,
				  symbase->symbol.name[10] == 'I' ?
				  "CTOR" : "DTOR");
	}
#endif
	*(location++) = symbase++;
	counter++;
    }
    *location++ = 0;
    return bfd_get_symcount(abfd);
}

#endif /* NO_COFF_SYMBOLS */

static unsigned int
coff_get_reloc_upper_bound(abfd, asect)
bfd            *abfd;
sec_ptr         asect;
  {
    if (bfd_get_format(abfd) != bfd_object) {
      bfd_error = invalid_operation;
      return 0;
    }
    return (asect->reloc_count + 1) * sizeof(arelent *);
  }

/*
SUBSUBSECTION 
	Reading Relocations

	Coff relocations are easily transformed into the internal BFD form
	(@code{arelent}).

	Reading a coff relocation table is done in the following stages:

	o The entire coff relocation table is read into memory.

	o Each relocation is processed in turn, first it is swapped from the
	external to the internal form.

	o The symbol referenced in the relocation's symbol index is
	turned intoa pointer into the canonical symbol table. Note
	that this table is the same as the one returned by a call to
	@code{bfd_canonicalize_symtab}. The back end will call the
	routine and save the result if a canonicalization hasn't been done.

	o The reloc index is turned into a pointer to a howto
	structure, in a back end specific way. For instance, the 386
	and 960 use the @code{r_type} to directly produce an index
	into a howto table vector; the 88k subtracts a number from the
	@code{r_type} field and creates an addend field.


*/

#ifndef CALC_ADDEND
#define CALC_ADDEND(abfd, ptr, reloc, cache_ptr) 	\
	    if (ptr && ptr->the_bfd == abfd		\
		&& ((ptr->flags & BSF_OLD_COMMON)== 0))	\
	    {						\
		cache_ptr->addend = -(ptr->section->vma + ptr->value);	\
	    }						\
	    else {					\
		cache_ptr->addend = 0;			\
	    }			
#endif

static          boolean
DEFUN(coff_slurp_reloc_table,(abfd, asect, symbols),
      bfd            *abfd AND
      sec_ptr         asect AND
      asymbol       **symbols)
{
  RELOC   *native_relocs;
  arelent        *reloc_cache;
  arelent        *cache_ptr;

  unsigned int idx;
  
  if (asect->relocation)
   return true;
  if (asect->reloc_count == 0)
   return true;
  if (asect->flags & SEC_CONSTRUCTOR)
   return true;
#ifndef NO_COFF_SYMBOLS
  if (!coff_slurp_symbol_table(abfd))
   return false;
#endif
  native_relocs =
   (RELOC *) buy_and_read(abfd,
			  asect->rel_filepos,
			  SEEK_SET,
			  (size_t) (RELSZ *
				    asect->reloc_count));
  reloc_cache = (arelent *)
   bfd_alloc(abfd, (size_t) (asect->reloc_count * sizeof(arelent)));

  if (reloc_cache == NULL) {
      bfd_error = no_memory;
      return false;
    } 

  
  for (idx = 0; idx < asect->reloc_count; idx ++) 
  {
#ifdef RELOC_PROCESSING
      struct internal_reloc dst;
      struct external_reloc  *src;

      cache_ptr = reloc_cache + idx;
      src = native_relocs + idx;
      bfd_swap_reloc_in(abfd, src, &dst);

      RELOC_PROCESSING(cache_ptr, &dst, symbols, abfd, asect);
#else
      struct internal_reloc dst;
      asymbol        *ptr;
      struct external_reloc  *src;

      cache_ptr = reloc_cache + idx;
      src = native_relocs + idx;

      bfd_swap_reloc_in(abfd, src, &dst);


      cache_ptr->address = dst.r_vaddr;

      if (dst.r_symndx != -1) 
	{
	  /* @@ Should never be greater than count of symbols!  */
	  if (dst.r_symndx >= obj_conv_table_size (abfd))
	    abort ();
	  cache_ptr->sym_ptr_ptr = symbols + obj_convert(abfd)[dst.r_symndx];
	  ptr = *(cache_ptr->sym_ptr_ptr);
	}
      else 
	{
	  cache_ptr->sym_ptr_ptr= bfd_abs_section.symbol_ptr_ptr;
	  ptr = 0;
	}

      /*
	The symbols definitions that we have read in have been
	relocated as if their sections started at 0. But the offsets
	refering to the symbols in the raw data have not been
	modified, so we have to have a negative addend to compensate.
	
	Note that symbols which used to be common must be left alone */

      /* Calculate any reloc addend by looking at the symbol */
      CALC_ADDEND(abfd, ptr, dst, cache_ptr);

      cache_ptr->address -= asect->vma;
/* !!     cache_ptr->section = (asection *) NULL;*/

      /* Fill in the cache_ptr->howto field from dst.r_type */
      RTYPE2HOWTO(cache_ptr, &dst);
#endif

  }

  asect->relocation = reloc_cache;
  return true;
}


/* This is stupid.  This function should be a boolean predicate */
static unsigned int
DEFUN(coff_canonicalize_reloc, (abfd, section, relptr, symbols),
bfd            *abfd AND
sec_ptr         section AND
arelent       **relptr AND
asymbol       **symbols)
{
  arelent        *tblptr = section->relocation;
  unsigned int    count = 0;


  if (section->flags & SEC_CONSTRUCTOR) 
  {
    /* this section has relocs made up by us, they are not in the
       file, so take them out of their chain and place them into
       the data area provided */
    arelent_chain *chain = section->constructor_chain;
    for (count = 0; count < section->reloc_count; count ++) 
    {
      *relptr ++ = &chain->relent;
      chain = chain->next;
    }

  }
  else 
  { 
    coff_slurp_reloc_table(abfd, section, symbols);


    tblptr = section->relocation;
    if (!tblptr)
     return 0;

    for (; count++ < section->reloc_count;)
     *relptr++ = tblptr++;


  }
  *relptr = 0;
  return section->reloc_count;
}

#ifndef NO_COFF_SYMBOLS

/*
provided a BFD, a section and an offset into the section, calculate and
return the name of the source file and the line nearest to the wanted
location.
*/

static          boolean
DEFUN(coff_find_nearest_line,(abfd,
			      section,
			      ignore_symbols,
			      offset,
			      filename_ptr,
			      functionname_ptr,
			      line_ptr),
      bfd            *abfd AND
      asection       *section AND
      asymbol       **ignore_symbols AND
      bfd_vma         offset AND
      CONST char      **filename_ptr AND
      CONST char       **functionname_ptr AND
      unsigned int   *line_ptr)
{
  static bfd     *cache_abfd;
  static asection *cache_section;
  static bfd_vma  cache_offset;
  static unsigned int cache_i;
  static alent   *cache_l;

  unsigned int    i = 0;
  coff_data_type *cof = coff_data(abfd);
  /* Run through the raw syments if available */
  combined_entry_type *p;
  alent          *l;
  unsigned int    line_base = 0;


  *filename_ptr = 0;
  *functionname_ptr = 0;
  *line_ptr = 0;

  /* Don't try and find line numbers in a non coff file */
  if (abfd->xvec->flavour != bfd_target_coff_flavour)
    return false;

  if (cof == NULL)
    return false;

  p = cof->raw_syments;

  for (i = 0; i < cof->raw_syment_count; i++) {
    if (p->u.syment.n_sclass == C_FILE) {
      /* File name has been moved into symbol */
      *filename_ptr = (char *) p->u.syment._n._n_n._n_offset;
      break;
    }
    p += 1 +  p->u.syment.n_numaux;
  }
  /* Now wander though the raw linenumbers of the section */
  /*
    If this is the same BFD as we were previously called with and this is
    the same section, and the offset we want is further down then we can
    prime the lookup loop
    */
  if (abfd == cache_abfd &&
      section == cache_section &&
      offset >= cache_offset) {
    i = cache_i;
    l = cache_l;
  }
  else {
    i = 0;
    l = section->lineno;
  }

  for (; i < section->lineno_count; i++) {
    if (l->line_number == 0) {
      /* Get the symbol this line number points at */
      coff_symbol_type *coff = (coff_symbol_type *) (l->u.sym);
      *functionname_ptr = coff->symbol.name;
      if (coff->native) {
	combined_entry_type  *s = coff->native;
	s = s + 1 + s->u.syment.n_numaux;
	/*
	  S should now point to the .bf of the function
	  */
	if (s->u.syment.n_numaux) {
	  /*
	    The linenumber is stored in the auxent
	    */
	  union internal_auxent   *a = &((s + 1)->u.auxent);
	  line_base = a->x_sym.x_misc.x_lnsz.x_lnno;
	}
      }
    }
    else {
      if (l->u.offset > offset)
	break;
      *line_ptr = l->line_number + line_base + 1;
    }
    l++;
  }

  cache_abfd = abfd;
  cache_section = section;
  cache_offset = offset;
  cache_i = i;
  cache_l = l;

  return true;
}

#ifdef GNU960
file_ptr
coff_sym_filepos(abfd)
bfd *abfd;
  {
    return obj_sym_filepos(abfd);
  }
#endif

#endif /* NO_COFF_SYMBOLS */


static int
DEFUN(coff_sizeof_headers,(abfd, reloc),
      bfd *abfd AND
      boolean reloc)
{
    size_t size;

    if (reloc == false) {
	size = FILHSZ + AOUTSZ;
    }
    else {
	size = FILHSZ;
    }

    size +=  abfd->section_count * SCNHSZ;
    return size;
}

static bfd_vma 
DEFUN(get_value,(reloc, seclet),
      arelent  *reloc AND
      bfd_seclet_type *seclet)
{
  bfd_vma value;
  asymbol *symbol = *(reloc->sym_ptr_ptr);
  /* A symbol holds a pointer to a section, and an offset from the
     base of the section.  To relocate, we find where the section will
     live in the output and add that in */

  if (symbol->section == &bfd_und_section)
  {
    /* Ouch, this is an undefined symbol.. */
    bfd_error_vector.undefined_symbol(reloc, seclet);
    value = symbol->value;
  }
  else 
  {
    value = symbol->value +
     symbol->section->output_offset +
      symbol->section->output_section->vma;
  }
  
  
  /* Add the value contained in the relocation */
  value += (short)((reloc->addend) & 0xffff);
  
  return value;
}

static void
DEFUN(perform_slip,(s, slip, input_section, value),
      asymbol **s AND
      unsigned int slip AND
      asection *input_section AND
      bfd_vma value)
{
  
  /* Find all symbols past this point, and make them know
     what's happened */
  while (*s) 
  {
    asymbol *p = *s;
    if (p->section == input_section) 
    {
      /* This was pointing into this section, so mangle it */
      if (p->value > value)
      {
	p->value -= slip;
      }
    }
    s++;
	
  }    
}
static int 
DEFUN(movb1,(input_section, symbols, r, shrink),
      asection *input_section AND
      asymbol **symbols AND
      arelent *r AND
      unsigned int shrink) 
{
  bfd_vma value = get_value(r,0);
	
  if (value >= 0xff00)
  { 

    /* Change the reloc type from 16bit, possible 8 to 8bit
       possible 16 */
    r->howto = r->howto + 1;	  
    /* The place to relc moves back by one */
    r->address -=1;
	  
    /* This will be two bytes smaller in the long run */
    shrink +=2 ;
    perform_slip(symbols, 2, input_section, r->address - shrink +1);

	  
  }      
  return shrink;      
}

static int 
DEFUN(jmp1,(input_section, symbols, r, shrink),
      asection *input_section AND
      asymbol **symbols AND
      arelent *r AND
      unsigned int shrink) 
{

  
  bfd_vma value = get_value(r, 0);
	
  bfd_vma dot = input_section->output_section->vma +
   input_section->output_offset + r->address;	
  bfd_vma gap;
  
  /* See if the address we're looking at within 127 bytes of where
     we are, if so then we can use a small branch rather than the
     jump we were going to */

  gap = value - (dot - shrink);
  

  if (-120 < (long)gap && (long)gap < 120 )
  { 

    /* Change the reloc type from 16bit, possible 8 to 8bit
       possible 16 */
    r->howto = r->howto + 1;	  
    /* The place to relc moves back by one */
    r->address -=1;
	  
    /* This will be two bytes smaller in the long run */
    shrink +=2 ;
    perform_slip(symbols, 2, input_section, r->address-shrink +1);

	  
  }      
  return shrink;      
}

static boolean 
DEFUN(bfd_coff_relax_section,(abfd, i, symbols),
      bfd *abfd AND
      asection *i AND
      asymbol **symbols)
{
  
  /* Get enough memory to hold the stuff */
  bfd *input_bfd = i->owner;
  asection *input_section = i;
  int shrink = 0 ;
  boolean new = false;
  
  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(input_bfd,
						       input_section);
  arelent **reloc_vector = (arelent **)bfd_xmalloc(reloc_size);

  /* Get the relocs and think about them */
  if (bfd_canonicalize_reloc(input_bfd, 
			     input_section,
			     reloc_vector,
			     symbols))
  {
    arelent **parent;
    for (parent = reloc_vector; *parent; parent++) 
    {
      arelent *r = *parent;
      switch (r->howto->type) {
	case R_MOVB2:
	case R_JMP2:
	  
	  shrink+=2;
	  break;
	  
	case R_MOVB1:
	  shrink = movb1(input_section, symbols, r, shrink);
	  new = true;
	  
	  break;
	case R_JMP1:
	  shrink = jmp1(input_section, symbols, r, shrink);
	  new = true;
	  
	  break;
	}
    }

  }
  input_section->_cooked_size -= shrink;  
  free((char *)reloc_vector);
  return new;
}

static bfd_byte *
DEFUN(bfd_coff_get_relocated_section_contents,(in_abfd, seclet, data),
      bfd *in_abfd AND
      bfd_seclet_type *seclet AND
      bfd_byte *data)

{
  /* Get enough memory to hold the stuff */
  bfd *input_bfd = seclet->u.indirect.section->owner;
  asection *input_section = seclet->u.indirect.section;
  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(input_bfd,
						       input_section);
  arelent **reloc_vector = (arelent **)bfd_xmalloc(reloc_size);
  
  /* read in the section */
  bfd_get_section_contents(input_bfd,
			   input_section,
			   data,
			   0,
			   input_section->_raw_size);
  
  
  if (bfd_canonicalize_reloc(input_bfd, 
			     input_section,
			     reloc_vector,
			     seclet->u.indirect.symbols) )
  {
    arelent **parent = reloc_vector;
    arelent *reloc ;
    


    unsigned int dst_address = 0;
    unsigned int src_address = 0;
    unsigned int run;
    unsigned int idx;
    
    /* Find how long a run we can do */
    while (dst_address < seclet->size) 
    {
      
      reloc = *parent;
      if (reloc) 
      {
	/* Note that the relaxing didn't tie up the addresses in the
	   relocation, so we use the original address to work out the
	   run of non-relocated data */
	run = reloc->address - src_address;
	parent++;
	
      }
      else 
      {
	run = seclet->size - dst_address;
      }
      /* Copy the bytes */
      for (idx = 0; idx < run; idx++)
      {
	data[dst_address++] = data[src_address++];
      }
    
      /* Now do the relocation */
    
      if (reloc) 
      {
	switch (reloc->howto->type) 
	{
	case R_JMP2:
	  /* Speciial relaxed type */
	{
	  bfd_vma dot = seclet->offset + dst_address + seclet->u.indirect.section->output_section->vma;	  
	  int   gap = get_value(reloc,seclet)-dot-1;
	  if ((gap & ~0xff  ) != 0 &&((gap & 0xff00)!= 0xff00)) abort();

	  bfd_put_8(in_abfd,gap,   data+dst_address);

	  switch (data[dst_address-1]) 
	  {
	    
	  case 0x5e:
	    /* jsr -> bsr */
	    bfd_put_8(in_abfd, 0x55, data+dst_address-1);
	    break;
	  case 0x5a:	 
	    /* jmp ->bra */
	    bfd_put_8(in_abfd, 0x40, data+dst_address-1);
	    break;
	  
	  default:
	    abort();
	  
	  }
	
	

	     
	  dst_address++;
	  src_address+=3;
  
	  break;
	}

	     
	case R_MOVB2:
	  /* Special relaxed type, there will be a gap between where we
	     get stuff from and where we put stuff to now 
	     
	     for a mov.b @aa:16 -> mov.b @aa:8
	     opcode 0x6a 0x0y offset
	     ->     0x2y off
	     */
	  if (data[dst_address-1] != 0x6a)
	   abort();
	  switch (data[src_address] & 0xf0) 
	  {
	  case 0x00:
	    /* Src is memory */
	    data[dst_address-1] = (data[src_address] & 0xf) | 0x20;
	    break;
	  case 0x80:
	    /* Src is reg */
	    data[dst_address-1] = (data[src_address] & 0xf) | 0x30;
	    break;
	  default:
	    abort();
	  }
	
	  /* the offset must fit ! after all, what was all the relaxing
	     about ? */

	  bfd_put_8(in_abfd, get_value(reloc, seclet), data + dst_address);

	  /* Note the magic - src goes up by two bytes, but dst by only
	     one */
	  dst_address+=1;
	  src_address+=3;
	
	  break;
	  /* PCrel 8 bits */
	case R_PCRBYTE:	  
	{
	  bfd_vma dot = seclet->offset + dst_address + seclet->u.indirect.section->output_section->vma;	  
	  int   gap = get_value(reloc,seclet)-dot;
	  if (gap > 127 || gap < -128) 
	  {
	    bfd_error_vector.reloc_value_truncated(reloc, seclet);
	  }
	  
	  bfd_put_8(in_abfd,gap,   data+dst_address);
	  dst_address++;
	  src_address++;
  
	  break;
	}

	case R_RELBYTE:
	{
	  unsigned  int gap =get_value(reloc,seclet);
	  if (gap > 0xff && gap < ~0xff)
	  {
	    bfd_error_vector.reloc_value_truncated(reloc, seclet);
	  }
	  
	  bfd_put_8(in_abfd, gap, data+dst_address);
	  dst_address+=1;
	  src_address+=1;


	}
	  break; 
	case R_JMP1:
	  /* A relword which would have like to have been a pcrel */
	case R_MOVB1:	
	  /* A relword which would like to have been modified but
	     didn't make it */
	case R_RELWORD:
	  bfd_put_16(in_abfd, get_value(reloc,seclet), data+dst_address);
	  dst_address+=2;
	  src_address+=2;
	  break;
#ifdef EXTRA_CASES
	EXTRA_CASES
#else

	default:
	  abort();
#endif
	}
      }    
    }
  }
  free((char *)reloc_vector);
  return data;
  
}


#define coff_core_file_failing_command	_bfd_dummy_core_file_failing_command
#define coff_core_file_failing_signal	_bfd_dummy_core_file_failing_signal
#define coff_core_file_matches_executable_p	_bfd_dummy_core_file_matches_executable_p
#define coff_slurp_armap		bfd_slurp_coff_armap
#define coff_slurp_extended_name_table	_bfd_slurp_extended_name_table
#define coff_truncate_arname		bfd_dont_truncate_arname
#define coff_openr_next_archived_file	bfd_generic_openr_next_archived_file
#define coff_generic_stat_arch_elt	bfd_generic_stat_arch_elt
#define	coff_get_section_contents	bfd_generic_get_section_contents
#define	coff_close_and_cleanup		bfd_generic_close_and_cleanup

#define coff_bfd_debug_info_start		bfd_void
#define coff_bfd_debug_info_end		bfd_void
#define coff_bfd_debug_info_accumulate	(PROTO(void,(*),(bfd*, struct sec *))) bfd_void
#define coff_bfd_get_relocated_section_contents  bfd_generic_get_relocated_section_contents
#define coff_bfd_relax_section bfd_generic_relax_section

