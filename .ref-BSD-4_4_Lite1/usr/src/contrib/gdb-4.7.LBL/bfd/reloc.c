/* BFD support for handling relocation entries.
   Copyright (C) 1990-1991 Free Software Foundation, Inc.
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
	Relocations

	BFD maintains relocations in much the same was as it maintains
	symbols; they are left alone until required, then read in
	en-mass and traslated into an internal form. There is a common
	routine <<bfd_perform_relocation>> which acts upon the
	canonical form to to the actual fixup.

	Note that relocations are maintained on a per section basis,
	whilst symbols are maintained on a per BFD basis.

	All a back end has to do to fit the BFD interface is to create
	as many <<struct reloc_cache_entry>> as there are relocations
	in a particuar section, and fill in the right bits:

@menu
@* typedef arelent::
@* howto manager::
@end menu

*/
#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "seclet.h"
/*
DOCDD
INODE
	typedef arelent, howto manager, Relocations, Relocations

SUBSECTION
	typedef arelent

	This is the structure of a relocation entry:

CODE_FRAGMENT
.
.typedef enum bfd_reloc_status 
.{
.       {* No errors detected *}
.  bfd_reloc_ok,
.
.       {* The relocation was performed, but there was an overflow. *}
.  bfd_reloc_overflow,
.
.       {* The address to relocate was not within the section supplied*}
.  bfd_reloc_outofrange,
.
.       {* Used by special functions *}
.  bfd_reloc_continue,
.
.       {* Unused *}
.  bfd_reloc_notsupported,
.
.       {* Unsupported relocation size requested.  *}
.  bfd_reloc_other,
.
.       {* The symbol to relocate against was undefined.*}
.  bfd_reloc_undefined,
.
.       {* The relocation was performed, but may not be ok - presently
.          generated only when linking i960 coff files with i960 b.out
.          symbols. *}
.  bfd_reloc_dangerous
. }
. bfd_reloc_status_type;
.
.
.typedef struct reloc_cache_entry 
.{
.       {* A pointer into the canonical table of pointers  *}
.  struct symbol_cache_entry **sym_ptr_ptr;
.
.       {* offset in section *}
.  rawdata_offset address;
.
.       {* addend for relocation value *}
.  bfd_vma addend;    
.
.       {* Pointer to how to perform the required relocation *}
.  CONST struct reloc_howto_struct *howto;
.
.} arelent;

*/

/*
DESCRIPTION

        Here is a description of each of the fields within a relent:

        o sym_ptr_ptr

        The symbol table pointer points to a pointer to the symbol
        associated with the relocation request. This would naturally
        be the pointer into the table returned by the back end's
        get_symtab action. @xref{Symbols}. The symbol is referenced
        through a pointer to a pointer so that tools like the linker
        can fix up all the symbols of the same name by modifying only
        one pointer. The relocation routine looks in the symbol and
        uses the base of the section the symbol is attached to and the
        value of the symbol as the initial relocation offset. If the
        symbol pointer is zero, then the section provided is looked up.

        o address

        The address field gives the offset in bytes from the base of
        the section data which owns the relocation record to the first
        byte of relocatable information. The actual data relocated
        will be relative to this point - for example, a relocation
        type which modifies the bottom two bytes of a four byte word
        would not touch the first byte pointed to in a big endian
        world.
	
	o addend

	The addend is a value provided by the back end to be added (!)
	to the relocation offset. Its interpretation is dependent upon
	the howto. For example, on the 68k the code:


|        char foo[];
|        main()
|                {
|                return foo[0x12345678];
|                }

        Could be compiled into:

|        linkw fp,#-4
|        moveb @@#12345678,d0
|        extbl d0
|        unlk fp
|        rts


        This could create a reloc pointing to foo, but leave the
        offset in the data (something like)


|RELOCATION RECORDS FOR [.text]:
|offset   type      value 
|00000006 32        _foo
|
|00000000 4e56 fffc          ; linkw fp,#-4
|00000004 1039 1234 5678     ; moveb @@#12345678,d0
|0000000a 49c0               ; extbl d0
|0000000c 4e5e               ; unlk fp
|0000000e 4e75               ; rts


        Using coff and an 88k, some instructions don't have enough
        space in them to represent the full address range, and
        pointers have to be loaded in two parts. So you'd get something like:


|        or.u     r13,r0,hi16(_foo+0x12345678)
|        ld.b     r2,r13,lo16(_foo+0x12345678)
|        jmp      r1


        This whould create two relocs, both pointing to _foo, and with
        0x12340000 in their addend field. The data would consist of:


|RELOCATION RECORDS FOR [.text]:
|offset   type      value 
|00000002 HVRT16    _foo+0x12340000
|00000006 LVRT16    _foo+0x12340000

|00000000 5da05678           ; or.u r13,r0,0x5678
|00000004 1c4d5678           ; ld.b r2,r13,0x5678
|00000008 f400c001           ; jmp r1


        The relocation routine digs out the value from the data, adds
        it to the addend to get the original offset and then adds the
        value of _foo. Note that all 32 bits have to be kept around
        somewhere, to cope with carry from bit 15 to bit 16.

        On further example is the sparc and the a.out format. The
        sparc has a similar problem to the 88k, in that some
        instructions don't have room for an entire offset, but on the
        sparc the parts are created odd sized lumps. The designers of
        the a.out format chose not to use the data within the section
        for storing part of the offset; all the offset is kept within
        the reloc. Any thing in the data should be ignored. 

|        save %sp,-112,%sp
|        sethi %hi(_foo+0x12345678),%g2
|        ldsb [%g2+%lo(_foo+0x12345678)],%i0
|        ret
|        restore

        Both relocs contains a pointer to foo, and the offsets would
        contain junk.


|RELOCATION RECORDS FOR [.text]:
|offset   type      value 
|00000004 HI22      _foo+0x12345678
|00000008 LO10      _foo+0x12345678

|00000000 9de3bf90     ; save %sp,-112,%sp
|00000004 05000000     ; sethi %hi(_foo+0),%g2
|00000008 f048a000     ; ldsb [%g2+%lo(_foo+0)],%i0
|0000000c 81c7e008     ; ret
|00000010 81e80000     ; restore


        o howto 

        The howto field can be imagined as a
        relocation instruction. It is a pointer to a struct which
        contains information on what to do with all the other
        information in the reloc record and data section. A back end
        would normally have a relocation instruction set and turn
        relocations into pointers to the correct structure on input -
        but it would be possible to create each howto field on demand.
        
*/


/*
SUBSUBSECTION 
        <<reloc_howto_type>>

        The <<reloc_howto_type>> is a structure which contains all the
        information that BFD needs to know to tie up a back end's data.

CODE_FRAGMENT
.struct symbol_cache_entry;		{* Forward declaration *}
.
.typedef CONST struct reloc_howto_struct 
.{ 
.       {*  The type field has mainly a documetary use - the back end can
.           to what it wants with it, though the normally the back end's
.           external idea of what a reloc number would be would be stored
.           in this field. For example, the a PC relative word relocation
.           in a coff environment would have the type 023 - because that's
.           what the outside world calls a R_PCRWORD reloc. *}
.  unsigned int type;
.
.       {*  The value the final relocation is shifted right by. This drops
.           unwanted data from the relocation.  *}
.  unsigned int rightshift;
.
.       {*  The size of the item to be relocated - 0, is one byte, 1 is 2
.           bytes, 3 is four bytes.  A -ve value indicates that the
.	    result is to be subtracted from the data*}
.  int size;
.
.       {*  Now obsolete *}
.  unsigned int bitsize;
.
.       {*  Notes that the relocation is relative to the location in the
.           data section of the addend. The relocation function will
.           subtract from the relocation value the address of the location
.           being relocated. *}
.  boolean pc_relative;
.
.       {*  Now obsolete *}
.  unsigned int bitpos;
.
.       {*  Now obsolete *}
.  boolean absolute;
.
.       {* Causes the relocation routine to return an error if overflow
.          is detected when relocating. *}
.  boolean complain_on_overflow;
.
.       {* If this field is non null, then the supplied function is
.          called rather than the normal function. This allows really
.          strange relocation methods to be accomodated (eg, i960 callj
.          instructions). *}
.  bfd_reloc_status_type EXFUN ((*special_function), 
.					    (bfd *abfd,
.					     arelent *reloc_entry,
.                                            struct symbol_cache_entry *symbol,
.                                            PTR data,
.                                            asection *input_section, 
.                                            bfd *output_bfd     ));
.
.       {* The textual name of the relocation type. *}
.  char *name;
.
.       {* When performing a partial link, some formats must modify the
.          relocations rather than the data - this flag signals this.*}
.  boolean partial_inplace;
.
.       {* The src_mask is used to select what parts of the read in data
.          are to be used in the relocation sum. Eg, if this was an 8 bit
.          bit of data which we read and relocated, this would be
.          0x000000ff. When we have relocs which have an addend, such as
.          sun4 extended relocs, the value in the offset part of a
.          relocating field is garbage so we never use it. In this case
.          the mask would be 0x00000000. *}
.  bfd_word src_mask;
.
.       {* The dst_mask is what parts of the instruction are replaced
.          into the instruction. In most cases src_mask == dst_mask,
.          except in the above special case, where dst_mask would be
.          0x000000ff, and src_mask would be 0x00000000.   *}
.  bfd_word dst_mask;           
.
.       {* When some formats create PC relative instructions, they leave
.          the value of the pc of the place being relocated in the offset
.          slot of the instruction, so that a PC relative relocation can
.          be made just by adding in an ordinary offset (eg sun3 a.out).
.          Some formats leave the displacement part of an instruction
.          empty (eg m88k bcs), this flag signals the fact.*}
.  boolean pcrel_offset;
.
.} reloc_howto_type;

*/

/*
FUNCTION
	the HOWTO macro

DESCRIPTION
	The HOWTO define is horrible and will go away.


.#define HOWTO(C, R,S,B, P, BI, ABS, O, SF, NAME, INPLACE, MASKSRC, MASKDST, PC) \
.  {(unsigned)C,R,S,B, P, BI, ABS,O,SF,NAME,INPLACE,MASKSRC,MASKDST,PC}

DESCRIPTION
	And will be replaced with the totally magic way. But for the
	moment, we are compatible, so do it this way..


.#define NEWHOWTO( FUNCTION, NAME,SIZE,REL,IN) HOWTO(0,0,SIZE,0,REL,0,false,false,FUNCTION, NAME,false,0,0,IN)
.
DESCRIPTION
	Helper routine to turn a symbol into a relocation value.

.#define HOWTO_PREPARE(relocation, symbol)      \
.  {                                            \
.  if (symbol != (asymbol *)NULL) {             \
.    if (symbol->section == &bfd_com_section) { \
.      relocation = 0;                          \
.    }                                          \
.    else {                                     \
.      relocation = symbol->value;              \
.    }                                          \
.  }                                            \
.}                      

*/

/*
TYPEDEF
	reloc_chain

DESCRIPTION

	How relocs are tied together

.typedef unsigned char bfd_byte;
.
.typedef struct relent_chain {
.  arelent relent;
.  struct   relent_chain *next;
.} arelent_chain;

*/



/*
FUNCTION 
	bfd_perform_relocation

SYNOPSIS
	bfd_reloc_status_type
                bfd_perform_relocation
                        (bfd * abfd,
                        arelent *reloc_entry,
                        PTR data,
                        asection *input_section,
                        bfd *output_bfd);

DESCRIPTION
	If an output_bfd is supplied to this function the generated
	image will be relocatable, the relocations are copied to the
	output file after they have been changed to reflect the new
	state of the world. There are two ways of reflecting the
	results of partial linkage in an output file; by modifying the
	output data in place, and by modifying the relocation record.
	Some native formats (eg basic a.out and basic coff) have no
	way of specifying an addend in the relocation type, so the
	addend has to go in the output data.  This is no big deal
	since in these formats the output data slot will always be big
	enough for the addend. Complex reloc types with addends were
	invented to solve just this problem.

*/


bfd_reloc_status_type
DEFUN(bfd_perform_relocation,(abfd,
                              reloc_entry,
                              data,
                              input_section,
                              output_bfd),
      bfd *abfd AND
      arelent *reloc_entry AND
      PTR data AND
      asection *input_section AND
      bfd *output_bfd)
{
  bfd_vma relocation;
  bfd_reloc_status_type flag = bfd_reloc_ok;
  bfd_vma addr = reloc_entry->address ;
  bfd_vma output_base = 0;
  reloc_howto_type *howto = reloc_entry->howto;
  asection *reloc_target_output_section ;

  asymbol *symbol;

  symbol = *( reloc_entry->sym_ptr_ptr);
  if ((symbol->section == &bfd_abs_section) 
      && output_bfd != (bfd *)NULL) 
  {
    reloc_entry->address += input_section->output_offset;
       
    return bfd_reloc_ok;
       
  }

  if ((symbol->section == &bfd_und_section) && output_bfd == (bfd *)NULL) {
    flag = bfd_reloc_undefined;
  }

  if (howto->special_function) {
    bfd_reloc_status_type cont;
    cont = howto->special_function(abfd,
				   reloc_entry,
				   symbol,
				   data,
				   input_section,
				   output_bfd);
    if (cont != bfd_reloc_continue) return cont;
  }

  /* 
    Work out which section the relocation is targetted at and the
    initial relocation command value.
    */


  if (symbol->section == &bfd_com_section) {
    relocation = 0;
  }
  else {
    relocation = symbol->value;
  }


  reloc_target_output_section = symbol->section->output_section;

  if (output_bfd && howto->partial_inplace==false) {
    output_base = 0;
  }
  else {
    output_base = reloc_target_output_section->vma;

  }

  relocation += output_base +   symbol->section->output_offset;
  

  relocation += reloc_entry->addend ;


  if(reloc_entry->address > input_section->_cooked_size)
  {
    return bfd_reloc_outofrange;
  }
          

  if (howto->pc_relative == true)
  {
    /*
      Anything which started out as pc relative should end up that
      way too. 
      
      There are two ways we can see a pcrel instruction. Sometimes
      the pcrel displacement has been partially calculated, it
      includes the distance from the start of the section to the
      instruction in it (eg sun3), and sometimes the field is
      totally blank - eg m88kbcs.
      */

        
    relocation -= 
     input_section->output_section->vma + input_section->output_offset;

    if (howto->pcrel_offset == true) {
      relocation -= reloc_entry->address;
    }

  }

  if (output_bfd!= (bfd *)NULL) {
    if ( howto->partial_inplace == false)  {
      /*
	This is a partial relocation, and we want to apply the relocation
	to the reloc entry rather than the raw data. Modify the reloc
	inplace to reflect what we now know.
	*/
      reloc_entry->addend = relocation  ;
      reloc_entry->address +=  input_section->output_offset;
      return flag;
    }
    else 
    {
      /* This is a partial relocation, but inplace, so modify the
	 reloc record a bit. 
	 
	 If we've relocated with a symbol with a section, change
	 into a ref to  the section belonging to the symbol
	 */
      reloc_entry->addend = relocation  ;
      reloc_entry->address +=  input_section->output_offset;


    }
  }
  else 
  {
    
    reloc_entry->addend = 0;
  }
  


  /* 
    Either we are relocating all the way, or we don't want to apply
    the relocation to the reloc entry (probably because there isn't
    any room in the output format to describe addends to relocs)
    */
  relocation >>= howto->rightshift;

  /* Shift everything up to where it's going to be used */
   
  relocation <<= howto->bitpos;

  /* Wait for the day when all have the mask in them */

  /* What we do:
     i instruction to be left alone
     o offset within instruction
     r relocation offset to apply
     S src mask
     D dst mask
     N ~dst mask
     A part 1
     B part 2
     R result
     
     Do this:
     i i i i i o o o o o        from bfd_get<size>
     and           S S S S S    to get the size offset we want
     +   r r r r r r r r r r  to get the final value to place
     and           D D D D D  to chop to right size
     -----------------------
     A A A A A 
     And this:
     ...   i i i i i o o o o o  from bfd_get<size>
     and   N N N N N            get instruction
     -----------------------
     ...   B B B B B
     
     And then:       
     B B B B B       
     or              A A A A A     
     -----------------------
     R R R R R R R R R R        put into bfd_put<size>
     */

#define DOIT(x) \
  x = ( (x & ~howto->dst_mask) | (((x & howto->src_mask) +  relocation) & howto->dst_mask))

   switch (howto->size)
   {
    case 0:
    {
      char x = bfd_get_8(abfd, (char *)data + addr);
      DOIT(x);
      bfd_put_8(abfd,x, (unsigned char *) data + addr);
    }
     break;

    case 1:
    { 
      short x = bfd_get_16(abfd, (bfd_byte *)data + addr);
      DOIT(x);
      bfd_put_16(abfd, x,   (unsigned char *)data + addr);
    }
     break;
    case 2:
    {
      long  x = bfd_get_32(abfd, (bfd_byte *) data + addr);
      DOIT(x);
      bfd_put_32(abfd,x,    (bfd_byte *)data + addr);
    }      
     break;
    case -2:
    {
      long  x = bfd_get_32(abfd, (bfd_byte *) data + addr);
      relocation = -relocation;
      DOIT(x);
      bfd_put_32(abfd,x,    (bfd_byte *)data + addr);
    }      
     break;

    case 3:

     /* Do nothing */
     break;
    default:
     return bfd_reloc_other;
   }

  return flag;
}



/*
DOCDD
INODE
	howto manager,  , typedef arelent, Relocations

SECTION
	The howto manager 

	When an application wants to create a relocation, but doesn't
	know what the target machine might call it, it can find out by
	using this bit of code.

*/

/*
TYPEDEF
	bfd_reloc_code_type

DESCRIPTION
	The insides of a reloc code

CODE_FRAGMENT
.
.typedef enum bfd_reloc_code_real 
.
.{
.       {* 16 bits wide, simple reloc *}
.  BFD_RELOC_16,        
.
.       {* 8 bits wide, but used to form an address like 0xffnn *}
.  BFD_RELOC_8_FFnn,
.
.       {* 8 bits wide, simple *}
.  BFD_RELOC_8,
.
.       {* 8 bits wide, pc relative *}
.  BFD_RELOC_8_PCREL,
.
.       {* The type of reloc used to build a contructor table - at the
.          moment probably a 32 bit wide abs address, but the cpu can
.          choose. *}
.
.  BFD_RELOC_CTOR,
.
.       {* 32 bits wide, simple reloc *}
.  BFD_RELOC_32,
.	{* 32 bits, PC-relative *}
.  BFD_RELOC_32_PCREL,
.
.	{* High 22 bits of 32-bit value; simple reloc.  *}
.  BFD_RELOC_HI22,
.	{* Low 10 bits.  *}
.  BFD_RELOC_LO10,
.
.	{* Reloc types used for i960/b.out.  *}
.  BFD_RELOC_24_PCREL,
.  BFD_RELOC_I960_CALLJ,
.
.  BFD_RELOC_16_PCREL,
.	{* 32-bit pc-relative, shifted right 2 bits (i.e., 30-bit
.	   word displacement, e.g. for SPARC) *}
.  BFD_RELOC_32_PCREL_S2,
.
.  {* now for the sparc/elf codes *}
.  BFD_RELOC_NONE,		{* actually used *}
.  BFD_RELOC_SPARC_WDISP22,
.  BFD_RELOC_SPARC22,
.  BFD_RELOC_SPARC13,
.  BFD_RELOC_SPARC_BASE13,
.  BFD_RELOC_SPARC_GOT10,
.  BFD_RELOC_SPARC_GOT13,
.  BFD_RELOC_SPARC_GOT22,
.  BFD_RELOC_SPARC_PC10,
.  BFD_RELOC_SPARC_PC22,
.  BFD_RELOC_SPARC_WPLT30,
.  BFD_RELOC_SPARC_COPY,
.  BFD_RELOC_SPARC_GLOB_DAT,
.  BFD_RELOC_SPARC_JMP_SLOT,
.  BFD_RELOC_SPARC_RELATIVE,
.  BFD_RELOC_SPARC_UA32,
.
.  {* this one is a.out specific? *}
.  BFD_RELOC_SPARC_BASE22,
.
.  {* this must be the highest numeric value *}
.  BFD_RELOC_UNUSED
. } bfd_reloc_code_real_type;
*/



/*
SECTION
	bfd_reloc_type_lookup

SYNOPSIS
	CONST struct reloc_howto_struct *
	bfd_reloc_type_lookup (bfd *abfd, bfd_reloc_code_real_type code);

DESCRIPTION
	This routine returns a pointer to a howto struct which when
	invoked, will perform the supplied relocation on data from the
	architecture noted.

*/


CONST struct reloc_howto_struct *
DEFUN(bfd_reloc_type_lookup,(abfd, code),
      bfd *abfd AND
      bfd_reloc_code_real_type code)
{
  return BFD_SEND (abfd, reloc_type_lookup, (abfd, code));
}

static reloc_howto_type bfd_howto_32 =
 HOWTO(0, 00,2,32,false,0,false,true,0,"VRT32", false,0xffffffff,0xffffffff,true);


/*
INTERNAL_FUNCTION
	bfd_default_reloc_type_lookup

SYNOPSIS
	CONST struct reloc_howto_struct *bfd_default_reloc_type_lookup
	(CONST struct bfd_arch_info *,
         bfd_reloc_code_real_type  code);

DESCRIPTION
	Provides a default relocation lookuperer for any architectue 


*/
CONST struct reloc_howto_struct *
DEFUN(bfd_default_reloc_type_lookup,(arch,  code),
     CONST struct bfd_arch_info *arch AND
      bfd_reloc_code_real_type  code)
{
    switch (code) 
    {
       case BFD_RELOC_CTOR:
	/* The type of reloc used in a ctor, which will be as wide as the
	   address - so either a 64, 32, or 16 bitter.. */
	switch (arch->bits_per_address) {
	   case 64:
	    BFD_FAIL();
	   case 32:
	    return &bfd_howto_32;
	   case 16:
	    BFD_FAIL();
	   default:
	    BFD_FAIL();
	}
       default:
	BFD_FAIL();
    }
return (CONST struct reloc_howto_struct *)NULL;
}


/*
INTERNAL_FUNCTION
	bfd_generic_relax_section

SYNOPSIS
	boolean bfd_generic_relax_section
	 (bfd *abfd,
	  asection *section,
	  asymbol **symbols);

DESCRIPTION
	Provides default handling for relaxing for back ends which
	don't do relaxing -- i.e., does nothing.
*/

boolean
DEFUN(bfd_generic_relax_section,(abfd, section, symbols),
      bfd *abfd AND
      asection *section AND
      asymbol **symbols)
{
  
  return false;
  
}

		
/*
INTERNAL_FUNCTION
	bfd_generic_get_relocated_section_contents

SYNOPSIS
	bfd_byte *
	   bfd_generic_get_relocated_section_contents(bfd *abfd,
	     struct bfd_seclet_struct  *seclet,
	     bfd_byte *data)

DESCRIPTION
	Provides default handling of relocation effort for back ends
	which can't be bothered to do it efficiently.

*/

bfd_byte *
DEFUN(bfd_generic_get_relocated_section_contents,(abfd, seclet, data),
      bfd *abfd AND
      struct bfd_seclet_struct *seclet AND
      bfd_byte *data)
{
  extern bfd_error_vector_type bfd_error_vector;

  /* Get enough memory to hold the stuff */
  bfd *input_bfd = seclet->u.indirect.section->owner;
  asection *input_section = seclet->u.indirect.section;



  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(input_bfd,
						       input_section);
  arelent **reloc_vector = (arelent **) alloca(reloc_size);
  
  /* read in the section */
  bfd_get_section_contents(input_bfd,
			   input_section,
			   data,
			   0,
			   input_section->_raw_size);
  
/* We're not relaxing the section, so just copy the size info */
  input_section->_cooked_size = input_section->_raw_size;
  input_section->reloc_done = true;
  

  if (bfd_canonicalize_reloc(input_bfd, 
			     input_section,
			     reloc_vector,
			     seclet->u.indirect.symbols) )
  {
    arelent **parent;
    for (parent = reloc_vector;  * parent != (arelent *)NULL;
	 parent++) 
    { 
      bfd_reloc_status_type r=
       bfd_perform_relocation(input_bfd,
			      *parent,
			      data,
			      input_section, 0);
      

      if (r != bfd_reloc_ok) 
      {
	switch (r)
	{
	case bfd_reloc_undefined:
	  bfd_error_vector.undefined_symbol(*parent, seclet);
	  break;
	case bfd_reloc_dangerous: 
	  bfd_error_vector.reloc_dangerous(*parent, seclet);
	  break;
	case bfd_reloc_outofrange:
	case bfd_reloc_overflow:
	  bfd_error_vector.reloc_value_truncated(*parent, seclet);
	  break;
	default:
	  abort();
	  break;
	}

      }
    }    
  }


  return data;

  
}

