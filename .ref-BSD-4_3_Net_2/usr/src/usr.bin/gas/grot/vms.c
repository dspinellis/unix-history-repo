/* vms.c -- Write out a VAX/VMS object file
   Copyright (C) 1987, 1988 Free Software Foundation, Inc.

This file is part of GAS, the GNU Assembler.

GAS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GAS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GAS; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by David L. Kashtan */
#include <ctype.h>
#include <stdio.h>
#include "as.h"
#include "md.h"
#include "subsegs.h"
#include "obstack.h"
#include "struc-symbol.h"
#include "write.h"
#include "symbols.h"

#ifdef	VMS		/* THIS MODULE IS FOR VMS ONLY */

#include <stab.h>
#include "objrecdef.h"			/* Define VMS Object record lang. */
#include <vms/fabdef.h>			/* Define File Access Block	  */
#include <vms/namdef.h>			/* Define NAM Block		  */
#include <vms/xabdef.h>			/* Define XAB			  */
#include <vms/xabdatdef.h>		/* Define Date XAB		  */
#include <vms/xabfhcdef.h>		/* Define File Header XAB	  */

/*
 *	Version string of the compiler that produced the code we are
 *	assembling.  (And this assembler, if we do not have compiler info.)
 */
extern char version_string[];
char *compiler_version_string;

extern char *myname;
static symbolS *Entry_Point_Symbol = 0;	/* Pointer to "_main"	 */

/*
 *	We augment the "gas" symbol structure with this
 */
struct VMS_Symbol {
	struct VMS_Symbol *Next;
	struct symbol *Symbol;
	int Size;
	int Psect_Index;
	int Psect_Offset;
	};
struct VMS_Symbol *VMS_Symbols = 0;

/* we need this to keep track of the various input files, so that we can
 * give the debugger the correct source line 
 */

struct input_file{
	struct input_file* next;
	struct input_file* same_file_fpnt;
	int file_number;
	int max_line;
	int min_line;
	int offset;
	char flag;
	char * name;
	symbolS * spnt;
	};

static struct input_file * file_root = (struct input_file*)NULL;

struct input_file * find_file(symbolS *);


/*
 *	If the procedure "main()" exists we have to add the instruction
 *	"jsb c$main_args" at the beginning to be compatible with VAX-11 "C".
 */
VMS_Check_For_Main()
{
  register symbolS *symbolP;
#ifdef	HACK_DEC_C_STARTUP	/* JF */
  register struct frchain *frchainP;
  register fragS *fragP;
  register fragS **prev_fragPP;
  register struct fix *fixP;
  register fragS *New_Frag;
  int i;
#endif	HACK_DEC_C_STARTUP

  symbolP = (struct symbol *)symbol_find("_main");
  if (symbolP && (symbolP->sy_nlist.n_type == (N_TEXT | N_EXT))) {
#ifdef	HACK_DEC_C_STARTUP
	if( !flagseen['+']) {
#endif
		/*
		 *	Remember the entry point symbol
		 */
		Entry_Point_Symbol = symbolP;
#ifdef HACK_DEC_C_STARTUP
	} else {
		/*
		 *	Scan all the fragment chains for the one with "_main"
		 *	(Actually we know the fragment from the symbol, but we need
		 *	 the previous fragment so we can change its pointer)
		 */
		frchainP = frchain_root;
		while(frchainP) {
			/*
			 *	Scan all the fragments in this chain, remembering
			 *	the "previous fragment"
			 */
			prev_fragPP = &frchainP->frch_root;
			fragP = frchainP->frch_root;
			while(fragP && (fragP != frchainP->frch_last)) {
				/*
				 *	Is this the fragment?
				 */
				if (fragP == symbolP->sy_frag) {
					/*
					 *	Yes: Modify the fragment by replacing
					 *	     it with a new fragment.
					 */
					New_Frag = (fragS *)
						xmalloc(sizeof(*New_Frag) +
								fragP->fr_fix +
								fragP->fr_var +
								5);
					/*
					 *	The fragments are the same except
					 *	that the "fixed" area is larger
					 */
					*New_Frag = *fragP;
					New_Frag->fr_fix += 6;
					/*
					 *	Copy the literal data opening a hole
					 *	2 bytes after "_main" (i.e. just after
					 *	the entry mask).  Into which we place
					 *	the JSB instruction.
					 */
					New_Frag->fr_literal[0] = fragP->fr_literal[0];
					New_Frag->fr_literal[1] = fragP->fr_literal[1];
					New_Frag->fr_literal[2] = 0x16; /* Jsb */
					New_Frag->fr_literal[3] = 0xef;
					New_Frag->fr_literal[4] = 0;
					New_Frag->fr_literal[5] = 0;
					New_Frag->fr_literal[6] = 0;
					New_Frag->fr_literal[7] = 0;
					for(i = 2; i < fragP->fr_fix + fragP->fr_var; i++)
						New_Frag->fr_literal[i+6] =
							fragP->fr_literal[i];
					/*
					 *	Now replace the old fragment with the
					 *	newly generated one.
					 */
					*prev_fragPP = New_Frag;
					/*
					 *	Remember the entry point symbol
					 */
					Entry_Point_Symbol = symbolP;
					/*
					 *	Scan the text area fixup structures
					 *	as offsets in the fragment may have
					 *	changed
					 */
					for(fixP = text_fix_root; fixP; fixP = fixP->fx_next) {
						/*
						 *	Look for references to this
						 *	fragment.
						 */
						if (fixP->fx_frag == fragP) {
							/*
							 *	Change the fragment
							 *	pointer
							 */
							fixP->fx_frag = New_Frag;
							/*
							 *	If the offset is after
							 *	the entry mask we need
							 *	to account for the JSB
							 *	instruction we just
							 *	inserted.
							 */
							if (fixP->fx_where >= 2)
								fixP->fx_where += 6;
						}
					}
					/*
					 *	Scan the symbols as offsets in the
					 *	fragment may have changed
					 */
					for(symbolP = symbol_rootP;
					    symbolP;
					    symbolP = symbolP->sy_next) {
						/*
						 *	Look for references to this
						 *	fragment.
						 */
						if (symbolP->sy_frag == fragP) {
							/*
							 *	Change the fragment
							 *	pointer
							 */
							symbolP->sy_frag = New_Frag;
							/*
							 *	If the offset is after
							 *	the entry mask we need
							 *	to account for the JSB
							 *	instruction we just
							 *	inserted.
							 */
							if (symbolP->sy_nlist.n_value >= 2)
								symbolP->sy_nlist.n_value += 6;
						}
					}
					/*
					 *	Make a symbol reference to
					 *	"_c$main_args" so we can get
					 *	its address inserted into the
					 *	JSB instruction.
					 */
					symbolP = (symbolS *)xmalloc(sizeof(*symbolP));
					symbolP->sy_nlist.n_un.n_name = "_c$main_args";
					symbolP->sy_nlist.n_type = N_UNDF;
					symbolP->sy_nlist.n_other = 0;
					symbolP->sy_nlist.n_desc = 0;
					symbolP->sy_nlist.n_value = 0;
					symbolP->sy_name_offset = 0;
					symbolP->sy_number = 0;
					symbolP->sy_frag = New_Frag;
					symbolP->sy_forward = 0;
					symbolP->sy_next = symbol_rootP;
					symbol_rootP = symbolP;
					/*
					 *	Generate a text fixup structure
					 *	to get "_c$main_args" stored into the
					 *	JSB instruction.
					 */
					fixP = (struct fix *)xmalloc(sizeof(*fixP));
					fixP->fx_frag = New_Frag;
					fixP->fx_where = 4;
					fixP->fx_addsy = symbolP;
					fixP->fx_subsy = 0;
					fixP->fx_offset = 0;
					fixP->fx_size = sizeof(long);
					fixP->fx_pcrel = 1;
					fixP->fx_next = text_fix_root;
					text_fix_root = fixP;
					/*
					 *	Now make sure we exit from the loop
					 */
					frchainP = 0;
					break;
				}
				/*
				 *	Try the next fragment
				 */
				prev_fragPP = &fragP->fr_next;
				fragP = fragP->fr_next;
			}
			/*
			 *	Try the next fragment chain
			 */
			if (frchainP) frchainP=frchainP->frch_next;
		}
	}
#endif	/* HACK_DEC_C_STARTUP */
  }
}

/*
 *	Write a VAX/VMS object file (everything else has been done!)
 */
VMS_write_object_file(text_siz, data_siz, text_frag_root, data_frag_root)
unsigned text_siz;
unsigned data_siz;
struct frag *text_frag_root;
struct frag *data_frag_root;
{
   register fragS *		fragP;
   register symbolS *		symbolP;
   register symbolS *		sp;
   register struct fix *	fixP;
   register struct VMS_Symbol *	vsp;
   int Local_Initialized_Data_Size = 0;
   int Psect_Number = 0;		/* Psect Index Number */
   int Text_Psect = -1;			/* Text Psect Index   */
   int Data_Psect = -2;			/* Data Psect Index   JF: Was -1 */
   int Bss_Psect = -3;			/* Bss Psect Index    JF: Was -1 */

   /*
    *	Create the VMS object file
    */
   Create_VMS_Object_File();
   /*
    *	Write the module header records
    */
   Write_VMS_MHD_Records();

    /*
     *	Generate the VMS object file records
     *	1st GSD then TIR records
     */

	/*******       Global Symbol Dictionary       *******/
	/*
	 *	Define the Text Psect
	 */
	if (text_siz > 0) {
		Text_Psect = Psect_Number++;
		VMS_Psect_Spec("$code",text_siz,"TEXT");
	}
	/*
	 *	Define the BSS Psect
	 */
	if (local_bss_counter > 0) {
		Bss_Psect = Psect_Number++;
		VMS_Psect_Spec("$uninitialized_data",local_bss_counter,"DATA");
	}
	/*
	 *	Now scan the symbols and emit the appropriate GSD records
	 */
	for (sp = symbol_rootP; sp; sp = sp->sy_next) {
		/*
		 *	Dispatch on symbol type
		 */
		switch(sp->sy_type) {
			/*
			 *	Global uninitialized data
			 */
			case N_UNDF | N_EXT:
				/*
				 *	Make a VMS data symbol entry
				 */
				vsp = (struct VMS_Symbol *)
					xmalloc(sizeof(*vsp));
				vsp->Symbol = sp;
				vsp->Size = sp->sy_nlist.n_value;
				vsp->Psect_Index = Psect_Number++;
				vsp->Psect_Offset = 0;
				vsp->Next = VMS_Symbols;
				VMS_Symbols = vsp;
				sp->sy_number = (int)vsp;
				/*
				 *	Make the psect for this data
				 */
				if(sp->sy_nlist.n_other)
					VMS_Psect_Spec(sp->sy_nlist.n_un.n_name,
					       vsp->Size,
					       "CONST");
				else
					VMS_Psect_Spec(sp->sy_nlist.n_un.n_name,
					       vsp->Size,
					       "COMMON");
#ifdef	NOT_VAX_11_C_COMPATIBLE
				/*
				 *	Place a global symbol at the
				 *	beginning of the Psect
				 */
				VMS_Global_Symbol_Spec(sp->sy_nlist.n_un.n_name,
						       vsp->Psect_Index,
						       0,
						       1);
#endif	NOT_VAX_11_C_COMPATIBLE
				break;
			/*
			 *	Local uninitialized data
			 */
			case N_BSS:
				/*
				 *	Make a VMS data symbol entry
				 */
				vsp = (struct VMS_Symbol *)
					xmalloc(sizeof(*vsp));
				vsp->Symbol = sp;
				vsp->Size = 0;
				vsp->Psect_Index = Bss_Psect;
				vsp->Psect_Offset =
					sp->sy_nlist.n_value -
						bss_address_frag . fr_address;
				vsp->Next = VMS_Symbols;
				VMS_Symbols = vsp;
				sp->sy_number = (int)vsp;
				break;
			/*
			 *	Global initialized data
			 */
			case N_DATA | N_EXT:
				/*
				 *	Make a VMS data symbol entry
				 */
				vsp = (struct VMS_Symbol *)
					xmalloc(sizeof(*vsp));
				vsp->Symbol = sp;
				vsp->Size = VMS_Initialized_Data_Size(sp,
							text_siz + data_siz);
				vsp->Psect_Index = Psect_Number++;
				vsp->Psect_Offset = 0;
				vsp->Next = VMS_Symbols;
				VMS_Symbols = vsp;
				sp->sy_number = (int)vsp;
				/*
				 *	Make its psect
				 */
				if(sp->sy_nlist.n_other)
					VMS_Psect_Spec(sp->sy_nlist.n_un.n_name,
					       vsp->Size,
					       "CONST");
				else
					VMS_Psect_Spec(sp->sy_nlist.n_un.n_name,
					       vsp->Size,
					       "COMMON");
#ifdef	NOT_VAX_11_C_COMPATIBLE
				/*
				 *	Place a global symbol at the
				 *	beginning of the Psect
				 */
				VMS_Global_Symbol_Spec(sp->sy_nlist.n_un.n_name,
						       vsp->Psect_Index,
						       0,
						       1);
#endif	NOT_VAX_11_C_COMPATIBLE
				break;
			/*
			 *	Local initialized data
			 */
			case N_DATA:
				/*
				 *	Make a VMS data symbol entry
				 */
				vsp = (struct VMS_Symbol *)
					xmalloc(sizeof(*vsp));
				vsp->Symbol = sp;
				vsp->Size = 
					VMS_Initialized_Data_Size(sp,
							  text_siz + data_siz);
				vsp->Psect_Index = Data_Psect;
				vsp->Psect_Offset =
					Local_Initialized_Data_Size;
				Local_Initialized_Data_Size += vsp->Size;
				vsp->Next = VMS_Symbols;
				VMS_Symbols = vsp;
				sp->sy_number = (int)vsp;
				break;
			/*
			 *	Global Text definition
			 */
			case N_TEXT | N_EXT: {
				unsigned short Entry_Mask;

				/*
				 *	Get the entry mask
				 */
				fragP = sp->sy_frag;
				Entry_Mask = (fragP->fr_literal[0] & 0xff) +
						((fragP->fr_literal[1] & 0xff)
							<< 8);
				/*
				 *	Define the Procedure entry pt.
				 */
				VMS_Procedure_Entry_Pt(sp->sy_nlist.n_un.n_name,
						       Text_Psect,
						       sp->sy_nlist.n_value,
						       Entry_Mask);
				break;
			}
			/*
			 *	Local Text definition
			 */
			case N_TEXT:
				/*
				 *	Make a VMS data symbol entry
				 */
				 if(Text_Psect != -1) {
					vsp = (struct VMS_Symbol *)
						xmalloc(sizeof(*vsp));
					vsp->Symbol = sp;
					vsp->Size = 0;
					vsp->Psect_Index = Text_Psect;
					vsp->Psect_Offset = sp->sy_nlist.n_value;
					vsp->Next = VMS_Symbols;
					VMS_Symbols = vsp;
					sp->sy_number = (int)vsp;
				 }
				break;
			/*
			 *	Global Reference
			 */
			case N_UNDF:
				/*
				 *	Make a GSD global symbol reference
				 *	record.
				 */
				VMS_Global_Symbol_Spec(sp->sy_nlist.n_un.n_name,
						       0,
						       0,
						       0);
				break;
			/*
			 *	Anything else
			 */
			default:
				/*
				 *	Ignore STAB symbols
				 *	Including .stabs emitted by g++
				 */
				if ((sp->sy_type & N_STAB) != 0 || sp->sy_nlist.n_type==22)
					break;
				/*
				 *	Error
				 */
				if(sp->sy_nlist.n_type !=22)
					printf(" ERROR, unknown type (%d)\n",
						sp->sy_nlist.n_type);
				break;
		}
	}
	/*
	 *	Define the Data Psect
	 */
	if ((data_siz > 0) && (Local_Initialized_Data_Size > 0)) {
		/*
		 *	Do it
		 */
		Data_Psect = Psect_Number++;
		VMS_Psect_Spec("$data",
				Local_Initialized_Data_Size,
				"DATA");
		/*
		 *	Scan the VMS symbols and fill in the data psect
		 */
		for (vsp = VMS_Symbols; vsp; vsp = vsp->Next) {
			/*
			 *	Only look for undefined psects
			 */
			if (vsp->Psect_Index < 0)  {
				/*
				 *	And only initialized data
				 */
				if (vsp->Symbol->sy_nlist.n_type == N_DATA)
					vsp->Psect_Index = Data_Psect;
			}
		}
	}

	/*******  Text Information and Relocation Records  *******/
	/*
	 *	Write the text segment data
	 */
	if (text_siz > 0) {
		/*
		 *	Scan the text fragments
		 */
		for(fragP = text_frag_root; fragP; fragP = fragP->fr_next) {
			/*
			 *	Stop if we get to the data fragments
			 */
			if (fragP == data_frag_root) break;
			/*
			 *	Ignore fragments with no data
			 */
			if ((fragP->fr_fix == 0) && (fragP->fr_var == 0))
				continue;
			/*
			 *	Go the the appropriate offset in the
			 *	Text Psect.
			 */
			VMS_Set_Psect(Text_Psect,fragP->fr_address,OBJ$C_TIR);
			/*
			 *	Store the "fixed" part
			 */
			if (fragP->fr_fix)
				VMS_Store_Immediate_Data(fragP->fr_literal,
							 fragP->fr_fix,
							 OBJ$C_TIR);
			/*
			 *	Store the "variable" part
			 */
			if (fragP->fr_var && fragP->fr_offset)
				VMS_Store_Repeated_Data(fragP->fr_offset,
							fragP->fr_literal+
							     fragP->fr_fix,
							fragP->fr_var,
							OBJ$C_TIR);
		}
		/*
		 *	Now we go through the text segment fixups and
		 *	generate TIR records to fix up addresses within
		 *	the Text Psect
		 */
		for(fixP = text_fix_root; fixP; fixP = fixP->fx_next) {
			/*
			 *	We DO handle the case of "Symbol - Symbol" as
			 *	long as it is in the same segment.
			 */
			if (fixP->fx_subsy && fixP->fx_addsy) {
				int i;

				/*
				 *	They need to be in the same segment
				 */
				if (fixP->fx_subsy->sy_type !=
					    	fixP->fx_addsy->sy_type)
					error("Fixup data addsy and subsy didn't have the same type");
				/*
				 *	And they need to be in one that we
				 *	can check the psect on
				 */
				if (((fixP->fx_addsy->sy_type & ~N_EXT) != N_DATA) &&
				    ((fixP->fx_addsy->sy_type & ~N_EXT) != N_TEXT))
					error("Fixup data addsy and subsy didn't have an appropriate type");
				/*
				 *	This had better not be PC relative!
				 */
				if (fixP->fx_pcrel)
					error("Fixup data was erroneously \"pcrel\"");
				/*
				 *	Subtract their values to get the
				 *	difference.
				 */
				i = fixP->fx_addsy->sy_value -
						fixP->fx_subsy->sy_value;
				/*
				 *	Now generate the fixup object records
				 *	Set the psect and store the data
				 */
				VMS_Set_Psect(Text_Psect,
					      fixP->fx_where +
					         fixP->fx_frag->fr_address,
					      OBJ$C_TIR);
				VMS_Store_Immediate_Data(&i,
							 fixP->fx_size,
							 OBJ$C_TIR);
				/*
				 *	Done
				 */
				continue;
			}
			/*
			 *	Size will HAVE to be "long"
			 */
			if (fixP->fx_size != sizeof(long))
				error("Fixup datum was not a longword");
			/*
			 *	Symbol must be "added" (if it is ever
			 *				subtracted we can
			 *				fix this assumption)
			 */
			if (fixP->fx_addsy == 0)
				error("Fixup datum was not \"fixP->fx_addsy\"");
			/*
			 *	Store the symbol value in a PIC fashion
			 */
			VMS_Store_PIC_Symbol_Reference(fixP->fx_addsy,
						       fixP->fx_offset,
						       fixP->fx_pcrel,
						       Text_Psect,
						       fixP->fx_where +
						          fixP->fx_frag->fr_address,
						       OBJ$C_TIR);
			/*
			 *	Check for indirect address reference,
			 *	which has to be fixed up (as the linker
			 *	will screw it up with TIR$C_STO_PICR).
			 */
			if (fixP->fx_pcrel)
				VMS_Fix_Indirect_Reference(Text_Psect,
							   fixP->fx_where +
								fixP->fx_frag->fr_address,
							   fixP->fx_frag,
							   text_frag_root);
		}
	}
	/*
	 *	Store the Data segment:
	 *
	 *	Since this is REALLY hard to do any other way,
	 *	we actually manufacture the data segment and
	 *	the store the appropriate values out of it.
	 */
	if (data_siz > 0) {
		char *Data_Segment;

		/*
		 *	Allocate the data segment
		 */
		Data_Segment = (char *)xmalloc(data_siz);
		/*
		 *	Run through the data fragments, filling in the segment
		 */
		for(fragP = data_frag_root; fragP; fragP = fragP->fr_next) {
			register long int		count;
			register char	 *		fill_literal;
			register long int		fill_size;
			int i;

			i = fragP->fr_address - text_siz;
			if (fragP->fr_fix)
				bcopy(fragP->fr_literal,
				      Data_Segment + i,
				      fragP->fr_fix);
			i += fragP->fr_fix;

			fill_literal= fragP -> fr_literal + fragP -> fr_fix;
			fill_size   = fragP -> fr_var;
			for (count = fragP -> fr_offset;  count;  count --) {
				if (fill_size)
					bcopy(fill_literal,
					      Data_Segment + i,
					      fill_size);
				i += fill_size;
			}
		}
		/*
		 *	Now we can run through all the data symbols
		 *	and store the data
		 */
		for(vsp = VMS_Symbols; vsp; vsp = vsp->Next) {
			/*
			 *	Ignore anything other than data symbols
			 */
			if ((vsp->Symbol->sy_nlist.n_type & ~N_EXT) != N_DATA)
				continue;
			/*
			 *	Set the Psect + Offset
			 */
			VMS_Set_Psect(vsp->Psect_Index,
				      vsp->Psect_Offset,
				      OBJ$C_TIR);
			/*
			 *	Store the data
			 */
			VMS_Store_Immediate_Data(Data_Segment +
							vsp->Symbol->sy_nlist.n_value -
								text_siz,
						 vsp->Size,
						 OBJ$C_TIR);
		}
		/*
		 *	Now we go through the data segment fixups and
		 *	generate TIR records to fix up addresses within
		 *	the Data Psects
		 */
		for(fixP = data_fix_root; fixP; fixP = fixP->fx_next) {
			/*
			 *	Find the symbol for the containing datum
			 */
			for(vsp = VMS_Symbols; vsp; vsp = vsp->Next) {
				/*
				 *	Only bother with Data symbols
				 */
				sp = vsp->Symbol;
				if ((sp->sy_nlist.n_type & ~N_EXT) != N_DATA)
					continue;
				/*
				 *	Ignore symbol if After fixup
				 */
				if (sp->sy_nlist.n_value >
					(fixP->fx_where +
						fixP->fx_frag->fr_address))
					continue;
				/*
				 *	See if the datum is here
				 */
				if ((sp->sy_nlist.n_value + vsp->Size) <=
					(fixP->fx_where +
						fixP->fx_frag->fr_address))
					continue;
				/*
				 *	We DO handle the case of "Symbol - Symbol" as
				 *	long as it is in the same segment.
				 */
				if (fixP->fx_subsy && fixP->fx_addsy) {
					int i;

					/*
					 *	They need to be in the same segment
					 */
					if (fixP->fx_subsy->sy_type !=
						    	fixP->fx_addsy->sy_type)
						error("Fixup data addsy and subsy didn't have the same type");
					/*
					 *	And they need to be in one that we
					 *	can check the psect on
					 */
					if (((fixP->fx_addsy->sy_type & ~N_EXT) != N_DATA) &&
					    ((fixP->fx_addsy->sy_type & ~N_EXT) != N_TEXT))
						error("Fixup data addsy and subsy didn't have an appropriate type");
					/*
					 *	This had better not be PC relative!
					 */
					if (fixP->fx_pcrel)
						error("Fixup data was erroneously \"pcrel\"");
					/*
					 *	Subtract their values to get the
					 *	difference.
					 */
					i = fixP->fx_addsy->sy_value -
							fixP->fx_subsy->sy_value;
					/*
					 *	Now generate the fixup object records
					 *	Set the psect and store the data
					 */
					VMS_Set_Psect(vsp->Psect_Index,
						      fixP->fx_frag->fr_address +
						        fixP->fx_where -
							  vsp->Symbol->sy_value +
							    vsp->Psect_Offset,
						      OBJ$C_TIR);
					VMS_Store_Immediate_Data(&i,
								 fixP->fx_size,
								 OBJ$C_TIR);
					/*
					 *	Done
					 */
					break;
				}
				/*
				 *	Size will HAVE to be "long"
				 */
				if (fixP->fx_size != sizeof(long))
					error("Fixup datum was not a longword");
				/*
				 *	Symbol must be "added" (if it is ever
				 *				subtracted we can
				 *				fix this assumption)
				 */
				if (fixP->fx_addsy == 0)
					error("Fixup datum was not \"fixP->fx_addsy\"");
				/*
				 *	Store the symbol value in a PIC fashion
				 */
				VMS_Store_PIC_Symbol_Reference(
					fixP->fx_addsy,
					fixP->fx_offset,
					fixP->fx_pcrel,
					vsp->Psect_Index,
					fixP->fx_frag->fr_address +
						fixP->fx_where -
						  vsp->Symbol->sy_value +
						     vsp->Psect_Offset,
					OBJ$C_TIR);
				/*
				 *	Done
				 */
				break;
			}
			
		}
	}

	/*
	 *	Write the Traceback Begin Module record
	 */
	VMS_TBT_Module_Begin();
	/*
	 *	Scan the symbols and write out the routines
	 *	(this makes the assumption that symbols are in
	 *	 order of ascending text segment offset)
	 */
	{
	   struct symbol *Current_Routine = 0;
	   int Current_Line_Number = 0;
	   int Current_Offset = -1;
	   struct input_file * Current_File;

/* Output debugging info for global variables and static variables that are not
 * specific to one routine. We also need to examine all stabs directives, to
 * find the definitions to all of the advanced data types, and this is done by
 * VMS_LSYM_Parse.  This needs to be done before any definitions are output to
 * the object file, since there can be forward references in the stabs
 * directives. When through with parsing, the text of the stabs directive
 * is altered, with the definitions removed, so that later passes will see
 * directives as they would be written if the type were already defined.
 *
 * We also look for files and include files, and make a list of them.  We
 * examine the source file numbers to establish the actual lines that code was
 * generated from, and then generate offsets.
 */
	VMS_LSYM_Parse();
	   for(symbolP = symbol_rootP; symbolP; symbolP = symbolP->sy_next) {
		/*
		 *	Deal with STAB symbols
		 */
		if ((symbolP->sy_nlist.n_type & N_STAB) != 0) {
		  /*
		   *	Dispatch on STAB type
		   */
		  switch((unsigned char)symbolP->sy_nlist.n_type) {
		    case N_SLINE:
			if(symbolP->sy_nlist.n_desc > Current_File->max_line)
			   Current_File->max_line = symbolP->sy_nlist.n_desc;
			if(symbolP->sy_nlist.n_desc < Current_File->min_line)
			   Current_File->min_line = symbolP->sy_nlist.n_desc;
			break;			
		    case N_SO:
			Current_File =find_file(symbolP);
			Current_File->flag = 1;
			Current_File->min_line = 1;
				break;
		    case N_SOL:
			Current_File = find_file(symbolP);
				break;
		    case N_GSYM:
				VMS_GSYM_Parse(symbolP,Text_Psect);
				break;
		    case N_LCSYM:
				VMS_LCSYM_Parse(symbolP,Text_Psect);
				break;
		    case N_FUN:		/* For static constant symbols */
		    case N_STSYM:
				VMS_STSYM_Parse(symbolP,Text_Psect);
				break;
		  }
		}
	   }

	/* now we take a quick sweep through the files and assign offsets
	to each one.  This will essentially be the starting line number to the
       debugger for each file.  Output the info for the debugger to specify the
       files, and then tell it how many lines to use */
	{
	int File_Number = 0;
	int Debugger_Offset = 0;
	int file_available;
	Current_File = file_root;
	for(Current_File = file_root; Current_File; Current_File = Current_File->next){
	  if(Current_File == (struct input_file*) NULL) break;
	  if(Current_File->max_line == 0) continue;
	  if((strncmp(Current_File->name,"GNU_GXX_INCLUDE:",16) == 0) &&
		!flagseen['D']) continue;
	  if((strncmp(Current_File->name,"GNU_CC_INCLUDE:",15) == 0) &&
		!flagseen['D']) continue;
/* show a few extra lines at the start of the region selected */
	  if(Current_File->min_line > 2) Current_File->min_line -= 2;
	  Current_File->offset = Debugger_Offset - Current_File->min_line + 1;
	  Debugger_Offset += Current_File->max_line - Current_File->min_line + 1;
	  if(Current_File->same_file_fpnt != (struct input_file *) NULL)
	   Current_File->file_number =Current_File->same_file_fpnt->file_number;
	  else {
	    Current_File->file_number = ++File_Number;
	    file_available = VMS_TBT_Source_File(Current_File->name,
		Current_File->file_number);
	    if(!file_available) {Current_File->file_number = 0;
				File_Number--;
				continue;};
	  };
	  VMS_TBT_Source_Lines(Current_File->file_number,
			Current_File->min_line,
			Current_File->max_line-Current_File->min_line+1);
	  }; /* for */
	};	/* scope */
	Current_File = (struct input_file*) NULL;

	   for(symbolP = symbol_rootP; symbolP; symbolP = symbolP->sy_next) {
		/*
		 *	Deal with text symbols
		 */
		if ((symbolP->sy_nlist.n_type & ~N_EXT) == N_TEXT) {
			/*
			 *	Ignore symbols starting with "L",
			 *	as they are local symbols
			 */
			if (symbolP->sy_nlist.n_un.n_name[0] == 'L') continue;
			/*
			 *	If there is a routine start defined,
			 *	terminate it.
			 */
			if (Current_Routine) {
				/*
				 *	End the routine
				 */
				VMS_TBT_Routine_End(text_siz,Current_Routine);
			}
			/*
			 *	Store the routine begin traceback info
			 */
			if(Text_Psect != -1) {
				VMS_TBT_Routine_Begin(symbolP,Text_Psect);
				Current_Routine = symbolP;
			}
/* Output local symbols, i.e. all symbols that are associated with a specific
 * routine.  We output them now so the debugger recognizes them as local to
 * this routine.
 */
	{ symbolS * symbolP1;
	  char* pnt;
	  char* pnt1;
	   for(symbolP1 = Current_Routine; symbolP1; symbolP1 = symbolP1->sy_next) {
		if ((symbolP1->sy_nlist.n_type & N_STAB) == 0) continue;
		if (symbolP1->sy_nlist.n_type != N_FUN) continue;
		pnt=symbolP->sy_nlist.n_un.n_name;
		pnt1=symbolP1->sy_nlist.n_un.n_name;
		if(*pnt++ != '_') continue;
		while(*pnt++ == *pnt1++) {};
		if((*(--pnt) == '\0') && (*(--pnt1) == ':')) break;
		};
	  if(symbolP1 != (symbolS *) NULL)
		VMS_DBG_Define_Routine(symbolP1,Current_Routine,Text_Psect);
	} /* local symbol block */
			/*
			 *	Done
			 */
			continue;
		}
		/*
		 *	Deal with STAB symbols
		 */
		if ((symbolP->sy_nlist.n_type & N_STAB) != 0) {
		  /*
		   *	Dispatch on STAB type
		   */
		  switch((unsigned char)symbolP->sy_nlist.n_type) {
			/*
			 *	Line number
			 */
			case N_SLINE:
				/* Offset the line into the correct portion
				 * of the file */
				if(Current_File->file_number == 0) break;
				/* Sometimes the same offset gets several source
				 * lines assigned to it.
				 * We should be selective about which lines
				 * we allow, we should prefer lines that are
				 * in the main source file when debugging
				 * inline functions. */
				if((Current_File->file_number != 1) &&
					symbolP->sy_nlist.n_value == 
					Current_Offset) break;
				/* calculate actual debugger source line */
				symbolP->sy_nlist.n_desc 
					+= Current_File->offset;
				/*
				 *	If this is the 1st N_SLINE, setup
				 *	PC/Line correlation.  Otherwise
				 *	do the delta PC/Line.  If the offset
				 *	for the line number is not +ve we need
				 *	to do another PC/Line correlation
				 *	setup
				 */
				if (Current_Offset == -1) {
				   VMS_TBT_Line_PC_Correlation(
						symbolP->sy_nlist.n_desc,
						symbolP->sy_nlist.n_value,
						Text_Psect,
						0);
				} else {
				   if ((symbolP->sy_nlist.n_desc -
						Current_Line_Number) <= 0) {
					/*
					 *	Line delta is not +ve, we
					 *	need to close the line and
					 *	start a new PC/Line
					 *	correlation.
					 */
					VMS_TBT_Line_PC_Correlation(0,
						symbolP->sy_nlist.n_value -
							Current_Offset,
						0,
						-1);
					VMS_TBT_Line_PC_Correlation(
						symbolP->sy_nlist.n_desc,
						symbolP->sy_nlist.n_value,
						Text_Psect,
						0);
				   } else {
					/*
					 *	Line delta is +ve, all is well
					 */
					VMS_TBT_Line_PC_Correlation(
						symbolP->sy_nlist.n_desc -
							Current_Line_Number,
						symbolP->sy_nlist.n_value -
							Current_Offset,
						0,
						1);
				   }
				}
				/*
				 *	Update the current line/PC
				 */
				Current_Line_Number = symbolP->sy_nlist.n_desc;
				Current_Offset = symbolP->sy_nlist.n_value;
				/*
				 *	Done
				 */
				break;
			/*
			 *	Source file
			 */
			case N_SO:
				/*
				 *	Remember that we had a source file
				 *	and emit the source file debugger
				 *	record
				 */
				Current_File =
				    find_file(symbolP);
				break;
/* We need to make sure that we are really in the actual source file when
 * we compute the maximum line number.  Otherwise the debugger gets really
 * confused */
			case N_SOL:
				Current_File =
				    find_file(symbolP);
				break;
		  }
		}
	   }
	   /*
	    *	If there is a routine start defined,
	    *	terminate it (and the line numbers)
	    */
	   if (Current_Routine) {
		/*
		 *	Terminate the line numbers
		 */
		VMS_TBT_Line_PC_Correlation(0,
					    text_siz - Current_Routine->sy_nlist.n_value,
					    0,
					    -1);
		/*
		 *	Terminate the routine
		 */
		VMS_TBT_Routine_End(text_siz,Current_Routine);
	   }
	}
	/*
	 *	Write the Traceback End Module TBT record
	 */
	VMS_TBT_Module_End();

	/*
	 *	Write the End Of Module record
	 */
	if (Entry_Point_Symbol == 0)
		Write_VMS_EOM_Record(-1,0);
	else
		Write_VMS_EOM_Record(Text_Psect,
				     Entry_Point_Symbol->sy_nlist.n_value);

   /*
    *	All done, close the object file
    */
   Close_VMS_Object_File();
}


	/****** VMS OBJECT FILE HACKING ROUTINES *******/


/*
 *	Global data (Object records limited to 512 bytes by VAX-11 "C" runtime)
 */
static int VMS_Object_File_FD;		/* File Descriptor for object file */
static char Object_Record_Buffer[512];	/* Buffer for object file records  */
static int Object_Record_Offset;	/* Offset to end of data	   */
static int Current_Object_Record_Type;	/* Type of record in above	   */

/*
 *	Macros for placing data into the object record buffer
 */
#define	PUT_LONG(val)	*((long *)(Object_Record_Buffer + \
				  Object_Record_Offset)) = val; \
			Object_Record_Offset += sizeof(long)

#define	PUT_SHORT(val)	*((short *)(Object_Record_Buffer + \
				  Object_Record_Offset)) = val; \
			Object_Record_Offset += sizeof(short)

#define	PUT_CHAR(val)	Object_Record_Buffer[Object_Record_Offset++] = val

#define	PUT_COUNTED_STRING(cp) {\
			register char *p = cp; \
			PUT_CHAR(strlen(p)); \
			while(*p) PUT_CHAR(*p++);}

/*
 *	Macro for determining if a Name has psect attributes attached
 *	to it.
 */
#define	PSECT_ATTRIBUTES_STRING		"$$PsectAttributes_"
#define	PSECT_ATTRIBUTES_STRING_LENGTH	18

#define	HAS_PSECT_ATTRIBUTES(Name) \
		(strncmp((Name[0] == '_' ? Name + 1 : Name), \
		 PSECT_ATTRIBUTES_STRING, \
		 PSECT_ATTRIBUTES_STRING_LENGTH) == 0)


/*
 *	Create the VMS object file
 */
Create_VMS_Object_File()
{
#ifdef	eunice
	VMS_Object_File_FD = creat(out_file_name, 0777, "var");
#else	eunice
	VMS_Object_File_FD = creat(out_file_name, 0, "rfm=var");
#endif	eunice
	/*
	 *	Deal with errors
	 */
	if (VMS_Object_File_FD < 0) {
		char Error_Line[256];

		sprintf(Error_Line,"Couldn't create VMS object file \"%s\"",
				out_file_name);
		error(Error_Line);
	}
	/*
	 *	Initialize object file hacking variables
	 */
	Object_Record_Offset = 0;
	Current_Object_Record_Type = -1;
}


/*
 *	Declare a particular type of object file record
 */
Set_VMS_Object_File_Record(Type)
int Type;
{
	/*
	 *	If the type matches, we are done
	 */
	if (Type == Current_Object_Record_Type) return;
	/*
	 *	Otherwise: flush the buffer
	 */
	Flush_VMS_Object_Record_Buffer();
	/*
	 *	Set the new type
	 */
	Current_Object_Record_Type = Type;
}


/*
 *	Flush the object record buffer to the object file
 */
Flush_VMS_Object_Record_Buffer()
{
	int i;

	/*
	 *	If the buffer is empty, we are done
	 */
	if (Object_Record_Offset == 0) return;
	/*
	 *	Write the data to the file
	 */
	i= write(VMS_Object_File_FD,
		 Object_Record_Buffer,
		 Object_Record_Offset);
	if (i != Object_Record_Offset)
		error("I/O error writing VMS object file");
	/*
	 *	The buffer is now empty
	 */
	Object_Record_Offset = 0;
}


/*
 *	Close the VMS Object file
 */
Close_VMS_Object_File()
{
	close(VMS_Object_File_FD);
}


/*
 *	Write the MHD (Module Header) records
 */
Write_VMS_MHD_Records()
{
	register char *cp,*cp1;
	register int i;
	struct {int Size; char *Ptr;} Descriptor;
	char Module_Name[256];
	char Now[17];

	/*
	 *	We are writing a module header record
	 */
	Set_VMS_Object_File_Record(OBJ$C_HDR);
	/*
	 *	***************************
	 *	*MAIN MODULE HEADER RECORD*
	 *	***************************
	 *
	 *	Store record type and header type
	 */
	PUT_CHAR(OBJ$C_HDR);
	PUT_CHAR(MHD$C_MHD);
	/*
	 *	Structure level is 0
	 */
	PUT_CHAR(OBJ$C_STRLVL);
	/*
	 *	Maximum record size is size of the object record buffer
	 */
	PUT_SHORT(sizeof(Object_Record_Buffer));
	/*
	 *	Get module name (the FILENAME part of the object file)
	 */
	cp = out_file_name;
	cp1 = Module_Name;
	while(*cp) {
		if ((*cp == ']') || (*cp == '>') ||
		    (*cp == ':') || (*cp == '/')) {
			cp1 = Module_Name;
			cp++;
			continue;
		}
		*cp1++ = islower(*cp) ? toupper(*cp++) : *cp++;
	}
	*cp1 = 0;
	/*
	 *	Limit it to 31 characters and store in the object record
	 */
	while(--cp1 >= Module_Name)
		if (*cp1 == '.') *cp1 = 0;
	if (strlen(Module_Name) > 31) {
 		if(flagseen['+'])
			printf("%s: Module name truncated: %s\n", myname, Module_Name);
		Module_Name[31] = 0;
	}
	PUT_COUNTED_STRING(Module_Name);
	/*
	 *	Module Version is "V1.0"
	 */
	PUT_COUNTED_STRING("V1.0");
	/*
	 *	Creation time is "now" (17 chars of time string)
	 */
	Descriptor.Size = 17;
	Descriptor.Ptr = Now;
	sys$asctim(0,&Descriptor,0,0);
	for(i = 0; i < 17; i++) PUT_CHAR(Now[i]);
	/*
	 *	Patch time is "never" (17 zeros)
	 */
	for(i = 0; i < 17; i++) PUT_CHAR(0);
	/*
	 *	Flush the record
	 */
	Flush_VMS_Object_Record_Buffer();
	/*
	 *	*************************
	 *	*LANGUAGE PROCESSOR NAME*
	 *	*************************
	 *
	 *	Store record type and header type
	 */
	PUT_CHAR(OBJ$C_HDR);
	PUT_CHAR(MHD$C_LNM);
	/*
	 *	Store language processor name and version
	 *	(not a counted string!)
	 */
	cp = compiler_version_string;
	if (cp == 0) {
		cp ="GNU AS  V";
		while(*cp) PUT_CHAR(*cp++);
		cp = strchr(&version_string,'.');		
		while(*cp != ' ') cp--; cp++;
		};
	while(*cp >= 32) PUT_CHAR(*cp++);
	/*
	 *	Flush the record
	 */
	Flush_VMS_Object_Record_Buffer();
}


/*
 *	Write the EOM (End Of Module) record
 */
Write_VMS_EOM_Record(Psect, Offset)
int Psect;
int Offset;
{
	/*
	 *	We are writing an end-of-module record
	 */
	Set_VMS_Object_File_Record(OBJ$C_EOM);
	/*
	 *	Store record Type
	 */
	PUT_CHAR(OBJ$C_EOM);
	/*
	 *	Store the error severity (0)
	 */
	PUT_CHAR(0);
	/*
	 *	Store the entry point, if it exists
	 */
	if (Psect >= 0) {
		/*
		 *	Store the entry point Psect
		 */
		PUT_CHAR(Psect);
		/*
		 *	Store the entry point Psect offset
		 */
		PUT_LONG(Offset);
	}
	/*
	 *	Flush the record
	 */
	Flush_VMS_Object_Record_Buffer();
}


/* this hash routine borrowed from GNU-EMACS, and strengthened slightly  ERY*/

static int
hash_string (ptr)
     unsigned char *ptr;
{
  register unsigned char *p = ptr;
  register unsigned char *end = p + strlen(ptr);
  register unsigned char c;
  register int hash = 0;

  while (p != end)
    {
      c = *p++;
      hash = ((hash<<3) + (hash<<15) + (hash>>28) + c);
    }
  return hash;
}

/*
 *	Generate a Case-Hacked VMS symbol name (limited to 31 chars)
 */
VMS_Case_Hack_Symbol(In,Out)
register char *In;
register char *Out;
{
	long int init = 0;
	long int result;
	char *pnt;
	char *new_name;
	char *old_name;
	register int i;
	int destructor = 0;	/*hack to allow for case sens in a destructor*/
	int truncate = 0;
	int Case_Hack_Bits = 0;
	int Saw_Dollar = 0;
	static char Hex_Table[16] =
	 {'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};

	/*
	 *	Kill any leading "_"
	 */
	if (*In == '_') In++;

	new_name=Out;	/* save this for later*/

	if((In[0]=='_')&&(In[1]=='$')&&(In[2]=='_'))
						 destructor=1;

	/* We may need to truncate the symbol, save the hash for later*/
	if(strlen(In)>23)  result = hash_string(In);
	/*
	 *	Is there a Psect Attribute to skip??
	 */
	if (HAS_PSECT_ATTRIBUTES(In)) {
		/*
		 *	Yes: Skip it
		 */
		In += PSECT_ATTRIBUTES_STRING_LENGTH;
		while(*In) {
			if ((In[0] == '$') && (In[1] == '$')) {
				In += 2;
				break;
			}
			In++;
		}
	}

	old_name=In;
/*	if(strlen(In) > 31 && flagseen['+'])
		printf("%s: Symbol name truncated: %s\n",myname,In);*/
	/*
	 *	Do the case conversion
	 */
	i = 23;		/* Maximum of 23 chars */
	while(*In && (--i >= 0)) {
		Case_Hack_Bits <<= 1;
		if (*In == '$') Saw_Dollar = 1;
		if ((destructor==1)&&(i==21)) Saw_Dollar = 0;
		if (isupper(*In)) {
			*Out++ = *In++;
			Case_Hack_Bits |= 1;
		} else {
			*Out++ = islower(*In) ? toupper(*In++) : *In++;
		}
	}
	/*
	 *	If we saw a dollar sign, we don't do case hacking
	 */
	if(flagseen['h'] || Saw_Dollar)
		Case_Hack_Bits = 0;

	/*
	 *	If we have more than 23 characters and everything is lowercase
	 *	we can insert the full 31 characters
	 */
	if (*In) {
		/*
		 *	We  have more than 23 characters
		 * If we must add the case hack, then we have truncated the str
		 */
		pnt=Out;
		truncate=1;
		if (Case_Hack_Bits == 0) {
			/*
			 *	And so far they are all lower case:
			 *		Check up to 8 more characters
			 *		and ensure that they are lowercase
			 */
			if(flagseen['h'])
				i=8;
			else
				for(i = 0; (In[i] != 0) && (i < 8); i++)
					if (isupper(In[i]) && !Saw_Dollar)
						break;
			if(In[i]==0)
				truncate=0;

			if ((i >= 8) || (In[i] == 0)) {
				/*
				 *	They are:  Copy up to 31 characters
				 *			to the output string
				 */
				i = 8;
				while((--i >= 0) && (*In))
					*Out++ = islower(*In) ?
							toupper(*In++) :
							*In++;
			}
		}
	}
	/*
	 *	If there were any uppercase characters in the name we
	 *	take on the case hacking string
	 */

	/* Old behavior for regular GNU-C compiler */
	if (!flagseen['+'])
		truncate=0;
	if ((Case_Hack_Bits != 0)||(truncate==1)) {
		if(truncate==0) {
			*Out++ = '_';
			for(i = 0; i < 6; i++) {
				*Out++ = Hex_Table[Case_Hack_Bits & 0xf];
				Case_Hack_Bits >>= 4;
			}
			*Out++ = 'X';
		} else {
			Out=pnt;	/*Cut back to 23 characters maximum */
			*Out++ = '_';
			for( i=0; i < 7; i++) {
				init = result & 0x01f;
				if (init < 10)
 					*Out++='0'+init;
				else
					*Out++ = 'A'+init-10;	
				result = result >> 5;
			}
		}
	} /*Case Hack */
	/*
	 *	Done
	 */
	*Out = 0;
	if( truncate==1 && flagseen['+'] && flagseen['H'])
		printf("%s: Symbol %s replaced by %s\n",myname,old_name,new_name);
}


/*
 *	Scan a symbol name for a psect attribute specification
 */
VMS_Modify_Psect_Attributes(Name, Attribute_Pointer)
char *Name;
int *Attribute_Pointer;
{
	register int i;
	register char *cp;
	int Negate;
	static struct {
		char *Name;
		int   Value;
		} Attributes[] = {
		{"PIC",		GPS$M_PIC},
		{"LIB",		GPS$M_LIB},
		{"OVR",		GPS$M_OVR},
		{"REL",		GPS$M_REL},
		{"GBL",		GPS$M_GBL},
		{"SHR",		GPS$M_SHR},
		{"EXE",		GPS$M_EXE},
		{"RD",		GPS$M_RD},
		{"WRT",		GPS$M_WRT},
		{"VEC",		GPS$M_VEC},
		{0,		0}};

	/*
	 *	Kill leading "_"
	 */
	if (*Name == '_') Name++;
	/*
	 *	Check for a PSECT attribute list
	 */
	if (!HAS_PSECT_ATTRIBUTES(Name)) return;	/* If not, return */
	/*
	 *	Skip the attribute list indicator
	 */
	Name += PSECT_ATTRIBUTES_STRING_LENGTH;
	/*
	 *	Process the attributes ("_" separated, "$" terminated)
	 */
	while(*Name != '$') {
		/*
		 *	Assume not negating
		 */
		Negate = 0;
		/*
		 *	Check for "NO"
		 */
		if ((Name[0] == 'N') && (Name[1] == 'O')) {
			/*
			 *	We are negating (and skip the NO)
			 */
			Negate = 1;
			Name += 2;
		}
		/*
		 *	Find the token delimiter
		 */
		cp = Name;
		while(*cp && (*cp != '_') && (*cp != '$')) cp++;
		/*
		 *	Look for the token in the attribute list
		 */
		for(i = 0; Attributes[i].Name; i++) {
			/*
			 *	If the strings match, set/clear the attr.
			 */
			if (strncmp(Name, Attributes[i].Name, cp - Name) == 0) {
				/*
				 *	Set or clear
				 */
				if (Negate)
					*Attribute_Pointer &=
						~Attributes[i].Value;
				else
					*Attribute_Pointer |=
						 Attributes[i].Value;
				/*
				 *	Done
				 */
				break;
			}
		}
		/*
		 *	Now skip the attribute
		 */
		Name = cp;
		if (*Name == '_') Name++;
	}
	/*
	 *	Done
	 */
	return;
}


/*
 *	Define a psect
 */
VMS_Psect_Spec(Name, Size, Type)
char *Name;
int Size;
char *Type;
{
	char Local[32];
	int Psect_Attributes;

	/*
	 *	We are writing a GSD record
	 */
	Set_VMS_Object_File_Record(OBJ$C_GSD);
	/*
	 *	If the buffer is empty we must insert the GSD record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_GSD);
	/*
	 *	We are writing a PSECT definition subrecord
	 */
	PUT_CHAR(GSD$C_PSC);
	/*
	 *	Psects are always LONGWORD aligned
	 */
	PUT_CHAR(2);
	/*
	 *	Generate the appropriate PSECT flags given the PSECT type
	 */
	if (strcmp(Type,"COMMON") == 0) {
		/*
		 *	Common block psects are:  PIC,OVR,REL,GBL,SHR,RD,WRT
		 */
		Psect_Attributes = (GPS$M_PIC|GPS$M_OVR|GPS$M_REL|GPS$M_GBL|
					  GPS$M_SHR|GPS$M_RD|GPS$M_WRT);
	} else if (strcmp(Type,"CONST") == 0) {
		/*
		 *	Common block psects are:  PIC,OVR,REL,GBL,SHR,RD
		 */
		Psect_Attributes = (GPS$M_PIC|GPS$M_OVR|GPS$M_REL|GPS$M_GBL|
					  GPS$M_SHR|GPS$M_RD);
	} else if (strcmp(Type,"DATA") == 0) {
		/*
		 *	The Data psects are PIC,REL,RD,WRT
		 */
		Psect_Attributes =
				(GPS$M_PIC|GPS$M_REL|GPS$M_RD|GPS$M_WRT);
	} else if (strcmp(Type,"TEXT") == 0) {
		/*
		 *	The Text psects are PIC,REL,SHR,EXE,RD
		 */
		Psect_Attributes =
				(GPS$M_PIC|GPS$M_REL|GPS$M_SHR|
							  GPS$M_EXE|GPS$M_RD);
	} else {
		/*
		 *	Error: Unknown psect type
		 */
		error("Unknown VMS psect type");
	}
	/*
	 *	Modify the psect attributes according to any attribute string
	 */
	if (HAS_PSECT_ATTRIBUTES(Name))
		VMS_Modify_Psect_Attributes(Name,&Psect_Attributes);
	/*
	 *	Specify the psect attributes
	 */
	PUT_SHORT(Psect_Attributes);
	/*
	 *	Specify the allocation
	 */
	PUT_LONG(Size);
	/*
	 *	Finally, the psect name
	 */
	VMS_Case_Hack_Symbol(Name,Local);
	PUT_COUNTED_STRING(Local);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}


/*
 *	Define a global symbol
 */
VMS_Global_Symbol_Spec(Name, Psect_Number, Psect_Offset, Defined)
char *Name;
int Psect_Number;
int Psect_Offset;
{
	char Local[32];

	/*
	 *	We are writing a GSD record
	 */
	Set_VMS_Object_File_Record(OBJ$C_GSD);
	/*
	 *	If the buffer is empty we must insert the GSD record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_GSD);
	/*
	 *	We are writing a Global symbol definition subrecord
	 */
	if (Psect_Number <= 255) {
		PUT_CHAR(GSD$C_SYM);
	} else {
		PUT_CHAR(GSD$C_SYMW);
	}
	/*
	 *	Data type is undefined
	 */
	PUT_CHAR(0);
	/*
	 *	Switch on Definition/Reference
	 */
	if (Defined) {
		/*
		 *	Definition:
		 *	Flags = "RELOCATABLE" and "DEFINED"
		 */
		PUT_SHORT(GSY$M_DEF|GSY$M_REL);
		/*
		 *	Psect Number
		 */
		if (Psect_Number <= 255) {
			PUT_CHAR(Psect_Number);
		} else {
			PUT_SHORT(Psect_Number);
		}
		/*
		 *	Offset
		 */
		PUT_LONG(Psect_Offset);
	} else {
		/*
		 *	Reference:
		 *	Flags = "RELOCATABLE"
		 */
		PUT_SHORT(GSY$M_REL);
	}
	/*
	 *	Finally, the global symbol name
	 */
	VMS_Case_Hack_Symbol(Name,Local);
	PUT_COUNTED_STRING(Local);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}


/*
 *	Define a procedure entry pt/mask
 */
VMS_Procedure_Entry_Pt(Name, Psect_Number, Psect_Offset, Entry_Mask)
char *Name;
int Psect_Number;
int Psect_Offset;
int Entry_Mask;
{
	char Local[32];

	/*
	 *	We are writing a GSD record
	 */
	Set_VMS_Object_File_Record(OBJ$C_GSD);
	/*
	 *	If the buffer is empty we must insert the GSD record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_GSD);
	/*
	 *	We are writing a Procedure Entry Pt/Mask subrecord
	 */
	if (Psect_Number <= 255) {
		PUT_CHAR(GSD$C_EPM);
	} else {
		PUT_CHAR(GSD$C_EPMW);
	}
	/*
	 *	Data type is undefined
	 */
	PUT_CHAR(0);
	/*
	 *	Flags = "RELOCATABLE" and "DEFINED"
	 */
	PUT_SHORT(GSY$M_DEF|GSY$M_REL);
	/*
	 *	Psect Number
	 */
	if (Psect_Number <= 255) {
		PUT_CHAR(Psect_Number);
	} else {
		PUT_SHORT(Psect_Number);
	}
	/*
	 *	Offset
	 */
	PUT_LONG(Psect_Offset);
	/*
	 *	Entry mask
	 */
	PUT_SHORT(Entry_Mask);
	/*
	 *	Finally, the global symbol name
	 */
	VMS_Case_Hack_Symbol(Name,Local);
	PUT_COUNTED_STRING(Local);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}


/*
 *	Set the current location counter to a particular Psect and Offset
 */
VMS_Set_Psect(Psect_Index, Offset, Record_Type)
int Psect_Index;
int Offset;
int Record_Type;
{
	/*
	 *	We are writing a "Record_Type" record
	 */
	Set_VMS_Object_File_Record(Record_Type);
	/*
	 *	If the buffer is empty we must insert the record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(Record_Type);
	/*
	 *	Stack the Psect base + Longword Offset
	 */
	if (Psect_Index < 255) {
		PUT_CHAR(TIR$C_STA_PL);
		PUT_CHAR(Psect_Index);
	} else {
		PUT_CHAR(TIR$C_STA_WPL);
		PUT_SHORT(Psect_Index);
	}
	PUT_LONG(Offset);
	/*
	 *	Set relocation base
	 */
	PUT_CHAR(TIR$C_CTL_SETRB);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}

/*
 *	Make a data reference
 */
VMS_Set_Data(Psect_Index, Offset, Record_Type,Force)
int Psect_Index;
int Offset;
int Record_Type;
int Force;
{
	/*
	 *	We are writing a "Record_Type" record
	 */
	Set_VMS_Object_File_Record(Record_Type);
	/*
	 *	If the buffer is empty we must insert the record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(Record_Type);
	/*
	 *	Stack the Psect base + Longword Offset
	 */
	if(Force==1){
	   if(Psect_Index>127){
		PUT_CHAR(TIR$C_STA_WPL);
		PUT_SHORT(Psect_Index);
	  	PUT_LONG(Offset);}
	    else {
		PUT_CHAR(TIR$C_STA_PL);
		PUT_CHAR(Psect_Index);
	  	PUT_LONG(Offset);}
	} else {if(Offset>32767){
		PUT_CHAR(TIR$C_STA_WPL);
		PUT_SHORT(Psect_Index);
	  	PUT_LONG(Offset);}
	    else if(Offset>127){
		PUT_CHAR(TIR$C_STA_WPW);
		PUT_SHORT(Psect_Index);
	  	PUT_SHORT(Offset);}
	    else{
		PUT_CHAR(TIR$C_STA_WPB);
		PUT_SHORT(Psect_Index);
		PUT_CHAR(Offset);};};
	/*
	 *	Set relocation base
	 */
	PUT_CHAR(TIR$C_STO_PIDR);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}

/*
 *	Make a debugger reference to a struct, union or enum.
 */
VMS_Store_Struct(int Struct_Index)
{
	/*
	 *	We are writing a "OBJ$C_DBG" record
	 */
	Set_VMS_Object_File_Record(OBJ$C_DBG);
	/*
	 *	If the buffer is empty we must insert the record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_DBG);
	PUT_CHAR(TIR$C_STA_UW);
	PUT_SHORT(Struct_Index);
	PUT_CHAR(TIR$C_CTL_STKDL);
	PUT_CHAR(TIR$C_STO_L);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}

/*
 *	Make a debugger reference to partially define a struct, union or enum.
 */
VMS_Def_Struct(int Struct_Index)
{
	/*
	 *	We are writing a "OBJ$C_DBG" record
	 */
	Set_VMS_Object_File_Record(OBJ$C_DBG);
	/*
	 *	If the buffer is empty we must insert the record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_DBG);
	PUT_CHAR(TIR$C_STA_UW);
	PUT_SHORT(Struct_Index);
	PUT_CHAR(TIR$C_CTL_DFLOC);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}

VMS_Set_Struct(int Struct_Index)
{/* see previous functions for comments */
	Set_VMS_Object_File_Record(OBJ$C_DBG);
	if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_DBG);
	PUT_CHAR(TIR$C_STA_UW);
	PUT_SHORT(Struct_Index);
	PUT_CHAR(TIR$C_CTL_STLOC);
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}

/*
 *	Store immediate data in current Psect
 */
VMS_Store_Immediate_Data(Pointer, Size, Record_Type)
register char *Pointer;
int Size;
int Record_Type;
{
	register int i;

	/*
	 *	We are writing a "Record_Type" record
	 */
	Set_VMS_Object_File_Record(Record_Type);
	/*
	 *	We can only store 128 bytes at a time
	 */
	while(Size > 0) {
		/*
		 *	Store a maximum of 128 bytes
		 */
		i = (Size > 128) ? 128 : Size;
		Size -= i;
		/*
		 *	If we cannot accommodate this record, flush the
		 *	buffer.
		 */
		if ((Object_Record_Offset + i + 1) >=
					sizeof(Object_Record_Buffer))
			Flush_VMS_Object_Record_Buffer();
		/*
		 *	If the buffer is empty we must insert record type
		 */
		if (Object_Record_Offset == 0) PUT_CHAR(Record_Type);
		/*
		 *	Store the count
		 */
		PUT_CHAR(-i & 0xff);
		/*
		 *	Store the data
		 */
		while(--i >= 0) PUT_CHAR(*Pointer++);
		/*
		 *	Flush the buffer if it is more than 75% full
		 */
		if (Object_Record_Offset >
			(sizeof(Object_Record_Buffer)*3/4))
				Flush_VMS_Object_Record_Buffer();
	}
}


/*
 *	Store repeated immediate data in current Psect
 */
VMS_Store_Repeated_Data(Repeat_Count,Pointer, Size, Record_Type)
int Repeat_Count;
register char *Pointer;
int Size;
int Record_Type;
{

	/*
	 *	Ignore zero bytes/words/longwords
	 */
	if ((Size == sizeof(char)) && (*Pointer == 0)) return;
	if ((Size == sizeof(short)) && (*(short *)Pointer == 0)) return;
	if ((Size == sizeof(long)) && (*(long *)Pointer == 0)) return;
	/*
	 *	If the data is too big for a TIR$C_STO_RIVB sub-record
	 *	then we do it manually
	 */
	if (Size > 255) {
		while(--Repeat_Count >= 0)
			VMS_Store_Immediate_Data(Pointer,Size,Record_Type);
		return;
	}
	/*
	 *	We are writing a "Record_Type" record
	 */
	Set_VMS_Object_File_Record(Record_Type);
	/*
	 *	If the buffer is empty we must insert record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(Record_Type);
	/*
	 *	Stack the repeat count
	 */
	PUT_CHAR(TIR$C_STA_LW);
	PUT_LONG(Repeat_Count);
	/*
	 *	And now the command and its data
	 */
	PUT_CHAR(TIR$C_STO_RIVB);
	PUT_CHAR(Size);
	while(--Size >= 0) PUT_CHAR(*Pointer++);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}


/*
 *	Store a Position Independent Reference
 */
VMS_Store_PIC_Symbol_Reference(Symbol, Offset, PC_Relative,
					Psect, Psect_Offset, Record_Type)
struct symbol *Symbol;
int Offset;
int PC_Relative;
int Psect;
int Psect_Offset;
int Record_Type;
{
	register struct VMS_Symbol *vsp =
		(struct VMS_Symbol *)(Symbol->sy_number);
	char Local[32];

	/*
	 *	We are writing a "Record_Type" record
	 */
	Set_VMS_Object_File_Record(Record_Type);
	/*
	 *	If the buffer is empty we must insert record type
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(Record_Type);
	/*
	 *	Set to the appropriate offset in the Psect
	 */
	if (PC_Relative) {
		/*
		 *	For a Code reference we need to fix the operand
		 *	specifier as well (so back up 1 byte)
		 */
		VMS_Set_Psect(Psect, Psect_Offset - 1, Record_Type);
	} else {
		/*
		 *	For a Data reference we just store HERE
		 */
		VMS_Set_Psect(Psect, Psect_Offset, Record_Type);
	}
	/*
	 *	Make sure we are still generating a "Record Type" record
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(Record_Type);
	/*
	 *	Dispatch on symbol type (so we can stack its value)
	 */
	switch(Symbol->sy_nlist.n_type) {
		/*
		 *	Global symbol
		 */
#ifdef	NOT_VAX_11_C_COMPATIBLE
		case N_UNDF | N_EXT:
		case N_DATA | N_EXT:
#endif	NOT_VAX_11_C_COMPATIBLE
		case N_UNDF:
		case N_TEXT | N_EXT:
			/*
			 *	Get the symbol name (case hacked)
			 */
			VMS_Case_Hack_Symbol(Symbol->sy_nlist.n_un.n_name,Local);
			/*
			 *	Stack the global symbol value
			 */
			PUT_CHAR(TIR$C_STA_GBL);
			PUT_COUNTED_STRING(Local);
			if (Offset) {
				/*
				 *	Stack the longword offset
				 */
				PUT_CHAR(TIR$C_STA_LW);
				PUT_LONG(Offset);
				/*
				 *	Add the two, leaving the result on the stack
				 */
				PUT_CHAR(TIR$C_OPR_ADD);
			}
			break;
		/*
		 *	Uninitialized local data
		 */
		case N_BSS:
			/*
			 *	Stack the Psect (+offset)
			 */
			if (vsp->Psect_Index < 255) {
				PUT_CHAR(TIR$C_STA_PL);
				PUT_CHAR(vsp->Psect_Index);
			} else {
				PUT_CHAR(TIR$C_STA_WPL);
				PUT_SHORT(vsp->Psect_Index);
			}
			PUT_LONG(vsp->Psect_Offset + Offset);
			break;
		/*
		 *	Local text
		 */
		case N_TEXT:
			/*
			 *	Stack the Psect (+offset)
			 */
			if (vsp->Psect_Index < 255) {
				PUT_CHAR(TIR$C_STA_PL);
				PUT_CHAR(vsp->Psect_Index);
			} else {
				PUT_CHAR(TIR$C_STA_WPL);
				PUT_SHORT(vsp->Psect_Index);
			}
			PUT_LONG(Symbol->sy_nlist.n_value);
			break;
		/*
		 *	Initialized local or global data
		 */
		case N_DATA:
#ifndef	NOT_VAX_11_C_COMPATIBLE
		case N_UNDF | N_EXT:
		case N_DATA | N_EXT:
#endif	NOT_VAX_11_C_COMPATIBLE
			/*
			 *	Stack the Psect (+offset)
			 */
			if (vsp->Psect_Index < 255) {
				PUT_CHAR(TIR$C_STA_PL);
				PUT_CHAR(vsp->Psect_Index);
			} else {
				PUT_CHAR(TIR$C_STA_WPL);
				PUT_SHORT(vsp->Psect_Index);
			}
			PUT_LONG(vsp->Psect_Offset + Offset);
			break;
	}
	/*
	 *	Store either a code or data reference
	 */
	PUT_CHAR(PC_Relative ? TIR$C_STO_PICR : TIR$C_STO_PIDR);
	/*
	 *	Flush the buffer if it is more than 75% full
	 */
	if (Object_Record_Offset >
		(sizeof(Object_Record_Buffer)*3/4))
			Flush_VMS_Object_Record_Buffer();
}


/*
 *	Check in the text area for an indirect pc-relative reference
 *	and fix it up with addressing mode 0xff [PC indirect]
 *
 *	THIS SHOULD BE REPLACED BY THE USE OF TIR$C_STO_PIRR IN THE
 *	PIC CODE GENERATING FIXUP ROUTINE.
 */
VMS_Fix_Indirect_Reference(Text_Psect, Offset, fragP, text_frag_root)
int Text_Psect;
int Offset;
register fragS *fragP;
struct frag *text_frag_root;
{
	/*
	 *	The addressing mode byte is 1 byte before the address
	 */
	Offset--;
	/*
	 *	Is it in THIS frag??
	 */
	if ((Offset < fragP->fr_address) ||
	    (Offset >= (fragP->fr_address + fragP->fr_fix))) {
		/*
		 *	We need to search for the fragment containing this
		 *	Offset
		 */
		for(fragP = text_frag_root; fragP; fragP = fragP->fr_next) {
			if ((Offset >= fragP->fr_address) &&
			    (Offset < (fragP->fr_address + fragP->fr_fix)))
				break;
		}
		/*
		 *	If we couldn't find the frag, things are BAD!!
		 */
		if (fragP == 0)
			error("Couldn't find fixup fragment when checking for indirect reference");
	}
	/*
	 *	Check for indirect PC relative addressing mode
	 */
	if (fragP->fr_literal[Offset - fragP->fr_address] == (char)0xff) {
		static char Address_Mode = 0xff;

		/*
		 *	Yes: Store the indirect mode back into the image
		 *	     to fix up the damage done by STO_PICR
		 */
		VMS_Set_Psect(Text_Psect,Offset,OBJ$C_TIR);
		VMS_Store_Immediate_Data(&Address_Mode,1,OBJ$C_TIR);
	}
}


/*
 *	Write the Traceback Module Begin record
 */
VMS_TBT_Module_Begin()
{
	register char *cp,*cp1;
	int Size;
	char Module_Name[256];
	char Local[256];

	/*
	 *	Get module name (the FILENAME part of the object file)
	 */
	cp = out_file_name;
	cp1 = Module_Name;
	while(*cp) {
		if ((*cp == ']') || (*cp == '>') ||
		    (*cp == ':') || (*cp == '/')) {
			cp1 = Module_Name;
			cp++;
			continue;
		}
		*cp1++ = islower(*cp) ? toupper(*cp++) : *cp++;
	}
	*cp1 = 0;
	/*
	 *	Limit it to 31 characters
	 */
	while(--cp1 >= Module_Name)
		if (*cp1 == '.') *cp1 = 0;
	if (strlen(Module_Name) > 31) {
		if(flagseen['+'])
			printf("%s: Module name truncated: %s\n",myname, Module_Name);
		Module_Name[31] = 0;
	}
	/*
	 *	Arrange to store the data locally (leave room for size byte)
	 */
	cp = Local+1;
	/*
	 *	Begin module
	 */
	*cp++ = DST$C_MODBEG;
	/*
	 *	Unused
	 */
	*cp++ = 0;
	/*
	 *	Language type == "C"
	 */
	*(long *)cp = DST$C_C;
	cp += sizeof(long);
	/*
	 *	Store the module name
	 */
	*cp++ = strlen(Module_Name);
	cp1 = Module_Name;
	while(*cp1) *cp++ = *cp1++;
	/*
	 *	Now we can store the record size
	 */
	Size = (cp - Local);
	Local[0] = Size-1;
	/*
	 *	Put it into the object record
	 */
	VMS_Store_Immediate_Data(Local, Size, OBJ$C_TBT);
}


/*
 *	Write the Traceback Module End record
*/
VMS_TBT_Module_End()
{
	char Local[2];

	/*
	 *	End module
	 */
	Local[0] = 1;
	Local[1] = DST$C_MODEND;
	/*
	 *	Put it into the object record
	 */
	VMS_Store_Immediate_Data(Local, 2, OBJ$C_TBT);
}


/*
 *	Write the Traceback Routine Begin record
 */
VMS_TBT_Routine_Begin(symbolP, Psect)
struct symbol *symbolP;
int Psect;
{
	register char *cp,*cp1;
	char *Name;
	int Offset;
	int Size;
	char Local[512];

	/*
	 *	Strip the leading "_" from the name
	 */
	Name = symbolP->sy_nlist.n_un.n_name;
	if (*Name == '_') Name++;
	/*
	 *	Get the text psect offset
	 */
	Offset = symbolP->sy_nlist.n_value;
	/*
	 *	Calculate the record size
	 */
	Size = 1+1+4+1+strlen(Name);
	/*
	 *	Record Size
	 */
	Local[0] = Size;
	/*
	 *	Begin Routine
	 */
	Local[1] = DST$C_RTNBEG;
	/*
	 *	Uses CallS/CallG
	 */
	Local[2] = 0;
	/*
	 *	Store the data so far
	 */
	VMS_Store_Immediate_Data(Local, 3, OBJ$C_TBT);
	/*
	 *	Make sure we are still generating a OBJ$C_TBT record
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_TBT);
	/*
	 *	Now get the symbol address
	 */
	PUT_CHAR(TIR$C_STA_WPL);
	PUT_SHORT(Psect);
	PUT_LONG(Offset);
	/*
	 *	Store the data reference
	 */
	PUT_CHAR(TIR$C_STO_PIDR);
	/*
	 *	Store the counted string as data
	 */
	cp = Local;
	cp1 = Name;
	Size = strlen(cp1) + 1;
	*cp++ = Size - 1;
	while(*cp1) *cp++ = *cp1++;
	VMS_Store_Immediate_Data(Local, Size, OBJ$C_TBT);
}


/*
 *	Write the Traceback Routine End record
 * 	We *must* search the symbol table to find the next routine, since
 * 	the assember has a way of reassembling the symbol table OUT OF ORDER
 * 	Thus the next routine in the symbol list is not necessarily the 
 *	next one in memory.  For debugging to work correctly we must know the
 *	size of the routine.
 */
VMS_TBT_Routine_End(Max_Size,sp)
int Max_Size;
symbolS *sp;
{
	symbolS *symbolP;
	int Size = 0x7fffffff;
	char Local[16];


	for(symbolP = symbol_rootP; symbolP; symbolP = symbolP->sy_next) {
	  if ((symbolP->sy_nlist.n_type & ~N_EXT) == N_TEXT) {
	    if (symbolP->sy_nlist.n_un.n_name[0] == 'L') continue;
	    if((symbolP->sy_nlist.n_value > sp->sy_nlist.n_value) &&
		(symbolP->sy_nlist.n_value < Size ))
		Size = symbolP->sy_nlist.n_value;
	/* check if gcc_compiled. has size of zero */
	    if((symbolP->sy_nlist.n_value == sp->sy_nlist.n_value) &&
		sp != symbolP &&
		!strcmp(sp->sy_nlist.n_un.n_name,"gcc_compiled."))
		Size = symbolP->sy_nlist.n_value;

		};
	};
	if(Size == 0x7fffffff) Size = Max_Size;
	Size -= sp->sy_nlist.n_value; /* and get the size of the routine */
	/*
	 *	Record Size
	 */
	Local[0] = 6;
	/*
	 *	End of Routine
	 */
	Local[1] = DST$C_RTNEND;
	/*
	 *	Unused
	 */
	Local[2] = 0;
	/*
	 *	Size of routine
	 */
	*((long *)(Local+3)) = Size;
	/*
	 *	Store the record
	 */
	VMS_Store_Immediate_Data(Local,7, OBJ$C_TBT);
}
/*
 *	Write the Traceback Block End record
 */
VMS_TBT_Block_Begin(symbolP, Psect, Name)
struct symbol *symbolP;
int Psect;
char* Name;
{
	register char *cp,*cp1;
	int Offset;
	int Size;
	char Local[512];
	/*
	 *	Begin block
	 */
	Size = 1+1+4+1+strlen(Name);
	/*
	 *	Record Size
	 */
	Local[0] = Size;
	/*
	 *	Begin Block - We simulate with a phony routine
	 */
	Local[1] = DST$C_BLKBEG;
	/*
	 *	Uses CallS/CallG
	 */
	Local[2] = 0;
	/*
	 *	Store the data so far
	 */
	VMS_Store_Immediate_Data(Local, 3, OBJ$C_DBG);
	/*
	 *	Make sure we are still generating a OBJ$C_DBG record
	 */
	if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_DBG);
	/*
	 *	Now get the symbol address
	 */
	PUT_CHAR(TIR$C_STA_WPL);
	PUT_SHORT(Psect);
	/*
	 *	Get the text psect offset
	 */
	Offset = symbolP->sy_nlist.n_value;
	PUT_LONG(Offset);
	/*
	 *	Store the data reference
	 */
	PUT_CHAR(TIR$C_STO_PIDR);
	/*
	 *	Store the counted string as data
	 */
	cp = Local;
	cp1 = Name;
	Size = strlen(cp1) + 1;
	*cp++ = Size - 1;
	while(*cp1) *cp++ = *cp1++;
	VMS_Store_Immediate_Data(Local, Size, OBJ$C_DBG);
}


/*
 *	Write the Traceback Block End record
 */
VMS_TBT_Block_End(int Size)
{
	char Local[16];

	/*
	 *	End block - simulate with a phony end routine
	 */
	Local[0] = 6;
	Local[1] = DST$C_BLKEND;
	*((long *)(Local+3)) = Size;
	/*
	 *	Unused
	 */
	Local[2] = 0;
	VMS_Store_Immediate_Data(Local,7, OBJ$C_DBG);
}



/*
 *	Write a Line number / PC correlation record
 */
VMS_TBT_Line_PC_Correlation(Line_Number, Offset, Psect, Do_Delta)
int Line_Number;
int Offset;
int Psect;
int Do_Delta;
{
	register char *cp;
	char Local[64];

	/*
*	If not delta, set our PC/Line number correlation
*/
	if (Do_Delta == 0) {
		/*
		 *	Size
		 */
		Local[0] = 1+1+2+1+4;
		/*
		 *	Line Number/PC correlation
		 */
		Local[1] = DST$C_LINE_NUM;
		/*
		 *	Set Line number
		 */
		Local[2] = DST$C_SET_LINE_NUM;
		*((unsigned short *)(Local+3)) = Line_Number-1;
		/*
		 *	Set PC
		 */
		Local[5] = DST$C_SET_ABS_PC;
		VMS_Store_Immediate_Data(Local, 6, OBJ$C_TBT);
		/*
		 *	Make sure we are still generating a OBJ$C_TBT record
		 */
		if (Object_Record_Offset == 0) PUT_CHAR(OBJ$C_TBT);
		if (Psect < 255) {
			PUT_CHAR(TIR$C_STA_PL);
			PUT_CHAR(Psect);
		} else {
			PUT_CHAR(TIR$C_STA_WPL);
			PUT_SHORT(Psect);
		}
		PUT_LONG(Offset);
		PUT_CHAR(TIR$C_STO_PIDR);
		/*
		 *	Do a PC offset of 0 to register the line number
		 */
		Local[0] = 2;
		Local[1] = DST$C_LINE_NUM;
		Local[2] = 0;	/* Increment PC by 0 and register line # */
		VMS_Store_Immediate_Data(Local, 3, OBJ$C_TBT);
	} else {
		/*
		 *	If Delta is negative, terminate the line numbers
		 */
		if (Do_Delta < 0) {
			Local[0] = 1+1+4;
			Local[1] = DST$C_LINE_NUM;
			Local[2] = DST$C_TERM_L;
			*((long *)(Local+3)) = Offset;
			VMS_Store_Immediate_Data(Local, 7, OBJ$C_TBT);
			/*
			 *	Done
			 */
			return;
		}
		/*
		 *	Do a PC/Line delta
		 */
		cp = Local+1;
		*cp++ = DST$C_LINE_NUM;
		if (Line_Number > 1) {
			/*
			 *	We need to increment the line number
			 */
			if (Line_Number-1 <= 255) {
				*cp++ = DST$C_INCR_LINUM;
				*cp++ = Line_Number-1;
			} else {
				*cp++ = DST$C_INCR_LINUM_W;
				*(short *)cp = Line_Number-1;
				cp += sizeof(short);
			}
		}
		/*
		 *	Increment the PC
		 */
		if (Offset <= 128) {
			*cp++ = -Offset;
		} else {
			if (Offset < 0x10000) {
				*cp++ = DST$C_DELTA_PC_W;
				*(short *)cp = Offset;
				cp += sizeof(short);
			} else {
				*cp++ = DST$C_DELTA_PC_L;
				*(long *)cp = Offset;
				cp += sizeof(long);
			}
		}
		Local[0] = cp - (Local+1);
		VMS_Store_Immediate_Data(Local,cp - Local, OBJ$C_TBT);
	}
}


/*
 *	Describe a source file to the debugger
 */
VMS_TBT_Source_File(Filename, ID_Number)
char *Filename;
int ID_Number;
{
	register char *cp,*cp1;
	int Status,i;
	char Local[512];
	static struct FAB Fab;
	static struct NAM Nam;
	static struct XABDAT Date_Xab;
	static struct XABFHC File_Header_Xab;
	char Es_String[255],Rs_String[255];

	/*
	 *	Setup the Fab
	 */
	Fab.fab$b_bid = FAB$C_BID;
	Fab.fab$b_bln = sizeof(Fab);
	Fab.fab$l_nam = (&Nam);
	Fab.fab$l_xab = (struct XAB *)&Date_Xab;
	/*
	 *	Setup the Nam block so we can find out the FULL name
	 *	of the source file.
	 */
	Nam.nam$b_bid = NAM$C_BID;
	Nam.nam$b_bln = sizeof(Nam);
	Nam.nam$l_rsa = Rs_String;
	Nam.nam$b_rss = sizeof(Rs_String);
	Nam.nam$l_esa = Es_String;
	Nam.nam$b_ess = sizeof(Es_String);
	/*
	 *	Setup the Date and File Header Xabs
	 */
	Date_Xab.xab$b_cod = XAB$C_DAT;
	Date_Xab.xab$b_bln = sizeof(Date_Xab);
	Date_Xab.xab$l_nxt = (char *)&File_Header_Xab;
	File_Header_Xab.xab$b_cod = XAB$C_FHC;
	File_Header_Xab.xab$b_bln = sizeof(File_Header_Xab);
/* ((struct XAB *)&Date_Xab)->xab$b_cod = XAB$C_DAT;				*/
/* ((struct XAB *)&Date_Xab)->xab$b_bln = sizeof(Date_Xab);			*/
/* ((struct XAB *)&Date_Xab)->xab$l_nxt = (struct XAB *)&File_Header_Xab;	*/
/* ((struct XAB *)&File_Header_Xab)->xab$b_cod = XAB$C_FHC;			*/
/* ((struct XAB *)&File_Header_Xab)->xab$b_bln = sizeof(File_Header_Xab);	*/
	/*
	 *	Get the file information
	 */
	Fab.fab$l_fna = Filename;
	Fab.fab$b_fns = strlen(Filename);
	Status = sys$open(&Fab);
	if (!(Status & 1)) {
		printf("gas: Couldn't find source file \"%s\", Error = %%X%x\n",
				Filename, Status);
		return(0);
	}
	sys$close(&Fab);
	/*
	 *	Calculate the size of the resultant string
	 */
	i = Nam.nam$b_rsl;
	/*
	 *	Size of record
	 */
	Local[0] = 1+1+1+1+1+2+8+4+2+1+1+i+1;
	/*
	 *	Source declaration
	 */
	Local[1] = DST$C_SOURCE;
	/*
	 *	Make formfeeds count as source records
	 */
	Local[2] = DST$C_SRC_FORMFEED;
	/*
	 *	Declare source file
	 */
	Local[3] = DST$C_SRC_DECLFILE;	
	Local[4] = 1+2+8+4+2+1+1+i+1;
	cp = Local+5;
	/*
	 *	Flags
	 */
	*cp++ = 0;
	/*
	 *	File ID
	 */
	*(short *)cp = ID_Number;
	cp += sizeof(short);
	/*
	 *	Creation Date
	 */
	*(long *)cp = ((long *) &Date_Xab.xab$q_cdt)[0];
	cp += sizeof(long);
	*(long *)cp = ((long *) &Date_Xab.xab$q_cdt)[1];
	cp += sizeof(long);
	/*
	 *	End of file block
	 */
	*(long *)cp = File_Header_Xab.xab$l_ebk;
	cp += sizeof(long);
	/*
	 *	First free byte
	 */
	*(short *)cp = File_Header_Xab.xab$w_ffb;
	cp += sizeof(short);
	/*
	 *	Record format
	 */
	*cp++ = File_Header_Xab.xab$b_rfo;
	/*
	 *	Filename
	 */
	*cp++ = i;
	cp1 = Rs_String;
	while(--i >= 0) *cp++ = *cp1++;
	/*
	 *	Library module name (none)
	 */
	*cp++ = 0;
	/*
	 *	Done
	 */
	VMS_Store_Immediate_Data(Local,cp - Local, OBJ$C_TBT);
}


/*
 *	Give the number of source lines to the debugger
 */
VMS_TBT_Source_Lines(ID_Number,Starting_Line_Number,Number_Of_Lines)
int ID_Number;
int Starting_Line_Number;
int Number_Of_Lines;
{
	char *cp,*cp1;
	char Local[16];

	/*
	 *	Size of record
	 */
	Local[0] = 1+1+2+1+4+1+2;
	/*
	 *	Source declaration
	 */
	Local[1] = DST$C_SOURCE;
	/*
	 *	Set Source File
	 */
	cp = Local+2;
	*cp++ = DST$C_SRC_SETFILE;
	/*
	 *	File ID Number
	 */
	*(short *)cp = ID_Number;
	cp += sizeof(short);
	/*
	 *	Set record number
	 */
	*cp++ = DST$C_SRC_SETREC_L;
	*(long *)cp = Starting_Line_Number;
	cp += sizeof(long);
	/*
	 *	Define lines
	 */
	*cp++ = DST$C_SRC_DEFLINES_W;
	*(short *)cp = Number_Of_Lines;
	cp += sizeof(short);
	/*
	 *	Done
	 */
	VMS_Store_Immediate_Data(Local, cp-Local, OBJ$C_TBT);
}


/*
 *	Given the pointer to a symbol we calculate how big the data at the
 *	symbol is.  We do this by looking for the next symbol (local or
 *	global) which will indicate the start of another datum.
 */
int VMS_Initialized_Data_Size(sp, End_Of_Data)
register struct symbol *sp;
int End_Of_Data;
{
	register struct symbol *sp1,*Next_Symbol;

	/*
	 *	Find the next symbol
	 *	it delimits this datum
	 */
	Next_Symbol = 0;
	for (sp1 = symbol_rootP; sp1; sp1 = sp1->sy_next) {
		/*
		 *	The data type must match
		 */
		if ((sp1->sy_nlist.n_type & ~N_EXT) != N_DATA) continue;
		/*
		 *	The symbol must be AFTER this symbol
		 */
		if (sp1->sy_nlist.n_value <= sp->sy_nlist.n_value) continue;
		/*
		 *	We ignore THIS symbol
		 */
		if (sp1 == sp) continue;
		/*
		 *	If there is already a candidate selected for the
		 *	next symbol, see if we are a better candidate
		 */
		if (Next_Symbol) {
			/*
			 *	We are a better candidate if we are "closer"
			 *	to the symbol
			 */
			if (sp1->sy_nlist.n_value >
					Next_Symbol->sy_nlist.n_value)
				continue;
			/*
			 *	Win:  Make this the candidate
			 */
			Next_Symbol = sp1;
		} else {
			/*
			 *	This is the 1st candidate
			 */
			Next_Symbol = sp1;
		}
	}
	/*
	 *	Calculate its size
	 */
	return(Next_Symbol ?
		(Next_Symbol->sy_nlist.n_value -
				sp->sy_nlist.n_value) :
		(End_Of_Data - sp->sy_nlist.n_value));
}



/* this routine locates a file in the list of files.  If an entry does not
 * exist, one is created.  For include files, a new entry is always created
 * such that inline functions can be properly debugged */
struct input_file *
find_file(sp)
symbolS * sp;
{
	struct input_file * same_file;
	struct input_file * fpnt;
	same_file = (struct input_file*) NULL;
	for(fpnt = file_root; fpnt; fpnt = fpnt->next){
	  if(fpnt == (struct input_file*) NULL) break;
	  if(fpnt->spnt == sp) return fpnt;
	};
	for(fpnt = file_root; fpnt; fpnt = fpnt->next){
	  if(fpnt == (struct input_file*) NULL) break;
	  if (strcmp(sp->sy_nlist.n_un.n_name,fpnt->name) == 0){
		if(fpnt->flag == 1)return fpnt;
		same_file = fpnt;
		break;
		};
	};
	fpnt = (struct input_file*) malloc(sizeof(struct input_file));
	if(file_root == (struct input_file*) NULL) file_root = fpnt;
	else {
		struct input_file * fpnt1;
		for(fpnt1 = file_root; fpnt1->next; fpnt1 = fpnt1->next);
		fpnt1->next = fpnt;
	};
	fpnt->next = (struct input_file*) NULL;
	fpnt->name = sp->sy_nlist.n_un.n_name;
	fpnt->min_line = 0x7fffffff;
	fpnt->max_line = 0;
	fpnt->offset = 0;
	fpnt->flag = 0;
	fpnt->file_number = 0;
	fpnt->spnt = sp;
	fpnt->same_file_fpnt = same_file;
	return fpnt;
}


/*
 *	This is a hacked _doprnt() for VAX-11 "C".  It understands that
 *	it is ONLY called by as_fatal(Format, Args) with a pointer to the
 *	"Args" argument.  From this we can make it all work right!
 */
#ifndef	eunice
_doprnt(Format, a, f)
char *Format;
FILE *f;
char **a;
{
	int Nargs = ((int *)a)[-2];	/* This understands as_fatal() */

	switch(Nargs) {
		default: fprintf(f,"_doprnt error on \"%s\"!!",Format); break;
		case 1:  fprintf(f,Format); break;
		case 2:  fprintf(f,Format,a[0]); break;
		case 3:  fprintf(f,Format,a[0],a[1]); break;
		case 4:  fprintf(f,Format,a[0],a[1],a[2]); break;
		case 5:  fprintf(f,Format,a[0],a[1],a[2],a[3]); break;
		case 6:  fprintf(f,Format,a[0],a[1],a[2],a[3],a[4]); break;
		case 7:  fprintf(f,Format,a[0],a[1],a[2],a[3],a[4],a[5]); break;
		case 8:  fprintf(f,Format,a[0],a[1],a[2],a[3],a[4],a[5],a[6]); break;
		case 9:  fprintf(f,Format,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7]); break;
		case 10: fprintf(f,Format,a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8]); break;
	}
}

#endif	/* eunice */

#endif	/* VMS */
