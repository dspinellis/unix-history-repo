/* Print instructions for the Motorola 88000, for GDB and GNU Binutils.
   Copyright 1986, 1987, 1988, 1989, 1990, 1991 Free Software Foundation, Inc.
   Contributed by Data General Corporation, November 1989.
   Partially derived from an earlier printcmd.c.

This file is part of GDB and the GNU Binutils.

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

#include "defs.h"
#include "opcode/m88k.h"
#include "symtab.h"

void sprint_address ();

INSTAB  *hashtable[HASHVAL] = {0};

/*
*		Disassemble an M88000 Instruction
*
*
*       This module decodes the first instruction in inbuf.  It uses the pc
*	to display pc-relative displacements.  It writes the disassembled
*	instruction in outbuf.
*
*			Revision History
*
*       Revision 1.0    11/08/85        Creation date by Motorola
*			05/11/89	R. Trawick adapted to GDB interface.
*/
#define MAXLEN 20

print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
  unsigned char buffer[MAXLEN];
  /* should be expanded if disassembler prints symbol names */
  char outbuf[100];
  int n;

  /* Instruction addresses may have low two bits set. Clear them.	*/
  memaddr&= 0xfffffffc;
  read_memory (memaddr, buffer, MAXLEN);

  n = m88kdis ((int)memaddr, buffer, outbuf);

  fputs (outbuf, stream);

  return (n);
}

/*
 * disassemble the first instruction in 'inbuf'.
 * 'pc' should be the address of this instruction, it will
 *   be used to print the target address if this is a relative jump or call
 * 'outbuf' gets filled in with the disassembled instruction.  It should
 *   be long enough to hold the longest disassembled instruction.
 *   100 bytes is certainly enough, unless symbol printing is added later
 * The function returns the length of this instruction in bytes.
 */

int m88kdis( pc, inbuf, outbuf )

    int		pc;
    int	        *inbuf;
    char	*outbuf;

{   static		ihashtab_initialized = 0;
    int			instruction;
    unsigned int	opcode;
    INSTAB		*entry_ptr;
    int	        	opmask;
    int			class;

    instruction= *inbuf;

    if (!ihashtab_initialized) {
	init_disasm();
    }

    /* create a the appropriate mask to isolate the opcode */
    opmask= DEFMASK;
    class= instruction & DEFMASK;
    if ((class >= SFU0) && (class <= SFU7)) {
	if (instruction < SFU1) {
	    opmask= CTRLMASK;
	} else {
	    opmask= SFUMASK;
	}
    } else if (class == RRR) {
	opmask= RRRMASK;
    } else if (class == RRI10) {
	opmask= RRI10MASK;
    }

    /* isolate the opcode */
    opcode= instruction & opmask;

    /* search the hash table with the isolated opcode */
    for (entry_ptr= hashtable[ opcode % HASHVAL ];
	 (entry_ptr != NULL) && (entry_ptr->opcode != opcode);
	 entry_ptr= entry_ptr->next) {
    }

    if (entry_ptr == NULL) {
	sprintf( outbuf, "word\t%08x", instruction );
    } else {
	sprintf( outbuf, "%s\t", entry_ptr->mnemonic );
	sprintop( &outbuf[strlen(outbuf)], &(entry_ptr->op1), instruction, pc, 1 );
	sprintop( &outbuf[strlen(outbuf)], &(entry_ptr->op2), instruction, pc, 0 );
	sprintop( &outbuf[strlen(outbuf)], &(entry_ptr->op3), instruction, pc, 0 );
    }


    return 4;
}


/*
*                      Decode an Operand of an Instruction
*
*			Functional Description
*
*       This module formats and writes an operand of an instruction to buf
*       based on the operand specification.  When the first flag is set this
*       is the first operand of an instruction.  Undefined operand types
*       cause a <dis error> message.
*
*			Parameters
*	char	*buf		buffer where the operand may be printed
*       OPSPEC  *opptr          Pointer to an operand specification
*       UINT    inst            Instruction from which operand is extracted
*	UINT    pc		PC of instruction; used for pc-relative disp.
*       int     first           Flag which if nonzero indicates the first
*                               operand of an instruction
*
*			Output
*
*       The operand specified is extracted from the instruction and is
*       written to buf in the format specified. The operand is preceded
*       by a comma if it is not the first operand of an instruction and it
*       is not a register indirect form.  Registers are preceded by 'r' and
*       hex values by '0x'.
*
*			Revision History
*
*       Revision 1.0    11/08/85        Creation date
*/

sprintop( buf, opptr, inst, pc, first )

   char   *buf;
   OPSPEC *opptr;
   UINT   inst;
   int	  pc;
   int    first;

{  int	  extracted_field;
   char	  *cond_mask_sym;
   char	  cond_mask_sym_buf[6];

   if (opptr->width == 0)
      return;

   switch(opptr->type) {
      case CRREG:
		       if (!first)
			   *buf++= ',';
		       sprintf( buf, "cr%d", UEXT(inst,opptr->offset,opptr->width));
		       break;

      case FCRREG:
		       if (!first)
			   *buf++= ',';
		       sprintf( buf, "fcr%d", UEXT(inst,opptr->offset,opptr->width));
		       break;

      case REGSC:
		       sprintf( buf, "[r%d]", UEXT(inst,opptr->offset,opptr->width));
		       break;

      case REG:
		       if (!first)
			   *buf++= ',';
		       sprintf( buf, "r%d", UEXT(inst,opptr->offset,opptr->width));
		       break;

      case HEX:
		        if (!first)
			   *buf++= ',';
		        extracted_field= UEXT(inst, opptr->offset, opptr->width);
		        if (extracted_field == 0) {
			    sprintf( buf, "0" );
		        } else {
			    sprintf( buf, "0x%02x", extracted_field );
		        }
		        break;

      case CONDMASK:
		        if (!first)
			   *buf++= ',';
		        extracted_field= UEXT(inst, opptr->offset, opptr->width);
		        switch (extracted_field & 0x0f) {
			  case 0x1:	cond_mask_sym= "gt0";
					break;
			  case 0x2:	cond_mask_sym= "eq0";
					break;
			  case 0x3:	cond_mask_sym= "ge0";
					break;
			  case 0xc:	cond_mask_sym= "lt0";
					break;
			  case 0xd:	cond_mask_sym= "ne0";
					break;
			  case 0xe:	cond_mask_sym= "le0";
					break;
			  default:	cond_mask_sym= cond_mask_sym_buf;
					sprintf( cond_mask_sym_buf,
						 "%x",
						 extracted_field );
					break;
			}
			strcpy( buf, cond_mask_sym );
			break;
			
      case PCREL:
		        if (!first)
			   *buf++= ',';
			sprint_address( pc + 4*(SEXT(inst,opptr->offset,opptr->width)),
					buf );
		        break;

      case CONT:
		       sprintf( buf,
				"%d,r%d",
				UEXT(inst,opptr->offset,5),
			        UEXT(inst,(opptr->offset)+5,5) );
		       break;

      case BF:
		       if (!first)
			   *buf++= ',';
		       sprintf( buf,
				"%d<%d>",
				UEXT(inst,(opptr->offset)+5,5),
			        UEXT(inst,opptr->offset,5));
		       break;

      default:
		       sprintf( buf, "<dis error: %08x>", inst );
    }

}

/*
*                 Initialize the Disassembler Instruction Table
*
*       Initialize the hash table and instruction table for the disassembler.
*       This should be called once before the first call to disasm().
*
*			Parameters
*
*			Output
*
*       If the debug option is selected, certain statistics about the hashing
*       distribution are written to stdout.
*
*			Revision History
*
*       Revision 1.0    11/08/85        Creation date
*/

init_disasm()
{
   int i,size;

   for (i=0 ; i < HASHVAL ; i++)
      hashtable[i] = NULL;

   for (i=0, size =  sizeof(instructions) / sizeof(INSTAB) ; i < size ;
       install(&instructions[i++]));

}

/*
*       Insert an instruction into the disassembler table by hashing the
*       opcode and inserting it into the linked list for that hash value.
*
*			Parameters
*
*       INSTAB *instptr         Pointer to the entry in the instruction table
*                               to be installed
*
*       Revision 1.0    11/08/85        Creation date
*			05/11/89	R. TRAWICK ADAPTED FROM MOTOROLA
*/

install(instptr)
   INSTAB *instptr;
{
   UINT i;

   i = (instptr->opcode) % HASHVAL;
   instptr->next = hashtable[i];
   hashtable[i] = instptr;
}


/* adapted from print_address in printcmd by R. Trawick 5/15/89.  The two should
   be combined.
 */

void sprint_address (addr, buffer)

     CORE_ADDR  addr;
     char	*buffer;

{
	struct minimal_symbol *msymbol;
	struct symbol	*fs;
	char		*name;
	int		name_location;

	sprintf ( buffer, "0x%x", addr);

	fs = find_pc_function (addr);

	if (!fs) {
	    msymbol = lookup_minimal_symbol_by_pc (addr);

	    if (msymbol == NULL) return;  /* If nothing comes through, don't
					     print anything symbolic */

	    name = msymbol -> name;
	    name_location = msymbol -> address;
	} else {
	    name = fs->name;
	    name_location = BLOCK_START (SYMBOL_BLOCK_VALUE (fs));
	}

	if (addr - name_location)
	    sprintf (buffer, " <%s+%d>", name, addr - name_location);
	else
	    sprintf (buffer, " <%s>", name);
}
