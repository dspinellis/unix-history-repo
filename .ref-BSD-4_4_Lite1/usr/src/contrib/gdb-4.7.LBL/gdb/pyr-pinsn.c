/* Print Pyramid Technology 90x instructions for GDB, the GNU Debugger.
   Copyright 1988, 1989, 1991, 1992 Free Software Foundation, Inc.

This file is part of GDB, the GNU debugger.

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
#include "symtab.h"
#include "opcode/pyr.h"
#include "gdbcore.h"


/*  A couple of functions used for debugging frame-handling on
    Pyramids. (The Pyramid-dependent handling of register values for
    windowed registers is known to be buggy.)

    When debugging, these functions can supplant the normal definitions of some
    of the macros in tm-pyramid.h  The quantity of information produced
    when these functions are used makes the gdb  unusable as a
    debugger for user programs.  */
    
extern unsigned pyr_saved_pc(), pyr_frame_chain();

CORE_ADDR pyr_frame_chain(frame)
    CORE_ADDR frame;
{
    int foo=frame - CONTROL_STACK_FRAME_SIZE;
    /* printf ("...following chain from %x: got %x\n", frame, foo);*/
    return foo;
}

CORE_ADDR pyr_saved_pc(frame)
    CORE_ADDR frame;
{
    int foo=0;
    foo = read_memory_integer (((CORE_ADDR)(frame))+60, 4);
    printf ("..reading pc from frame 0x%0x+%d regs: got %0x\n",
	    frame, 60/4, foo);
    return foo;
}

/* Pyramid instructions are never longer than this many bytes.  */
#define MAXLEN 24

/* Number of elements in the opcode table.  */
/*const*/ static int nopcodes = (sizeof (pyr_opcodes) / sizeof( pyr_opcodes[0]));
#define NOPCODES (nopcodes)

/* Let's be byte-independent so we can use this as a cross-assembler.  */

#define NEXTLONG(p)  \
  (p += 4, (((((p[-4] << 8) + p[-3]) << 8) + p[-2]) << 8) + p[-1])

/* Print one instruction at address MEMADDR in debugged memory,
   on STREAM.  Returns length of the instruction, in bytes.  */

int
print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
  unsigned char buffer[MAXLEN];
  register int i, nargs, insn_size =4;
  register unsigned char *p;
  register char *d;
  register int insn_opcode, operand_mode;
  register int index_multiplier, index_reg_regno, op_1_regno, op_2_regno ;
  long insn;			/* first word of the insn, not broken down. */
  pyr_insn_format insn_decode;	/* the same, broken out into op{code,erands} */
  long extra_1, extra_2;

  read_memory (memaddr, buffer, MAXLEN);
  insn_decode = *((pyr_insn_format *) buffer);
  insn = * ((int *) buffer);
  insn_opcode = insn_decode.operator;
  operand_mode = insn_decode.mode;
  index_multiplier = insn_decode.index_scale;
  index_reg_regno = insn_decode.index_reg;
  op_1_regno = insn_decode.operand_1;
  op_2_regno = insn_decode.operand_2;
  
  
  if (*((int *)buffer) == 0x0) {
    /* "halt" looks just like an invalid "jump" to the insn decoder,
       so is dealt with as a special case */
    fprintf (stream, "halt");
    return (4);
  }

  for (i = 0; i < NOPCODES; i++)
	  if (pyr_opcodes[i].datum.code == insn_opcode)
		  break;

  if (i == NOPCODES)
	  /* FIXME: Handle unrecognised instructions better.  */
	  fprintf (stream, "???\t#%08x\t(op=%x mode =%x)",
		   insn, insn_decode.operator, insn_decode.mode);
  else
    {
      /* Print the mnemonic for the instruction.  Pyramid insn operands
         are so regular that we can deal with almost all of them
         separately.
	 Unconditional branches are an exception: they are encoded as
	 conditional branches (branch if false condition, I think)
	 with no condition specified. The average user will not be
	 aware of this. To maintain their illusion that an
	 unconditional branch insn exists, we will have to FIXME to
	 treat the insn mnemnonic of all branch instructions here as a
	 special case: check the operands of branch insn and print an
	 appropriate mnemonic. */ 

      fprintf (stream, "%s\t", pyr_opcodes[i].name);

    /* Print the operands of the insn (as specified in
       insn.operand_mode). 
       Branch operands of branches are a special case: they are a word
       offset, not a byte offset. */
  
    if (insn_decode.operator == 0x01 || insn_decode.operator == 0x02) {
      register int bit_codes=(insn >> 16)&0xf;
      register int i;
      register int displacement = (insn & 0x0000ffff) << 2;

      static char cc_bit_names[] = "cvzn";	/* z,n,c,v: strange order? */

      /* Is bfc and no bits specified an unconditional branch?*/
      for (i=0;i<4;i++) {
	if ((bit_codes) & 0x1)
		fputc (cc_bit_names[i], stream);
	bit_codes >>= 1;
      }

      fprintf (stream, ",%0x",
	       displacement + memaddr);
      return (insn_size);
    }

      switch (operand_mode) {
      case 0:
	fprintf (stream, "%s,%s",
		 reg_names [op_1_regno],
		 reg_names [op_2_regno]);
	break;
	    
      case 1:
	fprintf (stream, " 0x%0x,%s",
		 op_1_regno,
		 reg_names [op_2_regno]);
	break;
	
      case 2:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	fprintf (stream, " $0x%0x,%s",
		 extra_1,
		 reg_names [op_2_regno]);
	break;
      case 3:
	fprintf (stream, " (%s),%s",
		 reg_names [op_1_regno],
		 reg_names [op_2_regno]);
	break;
	
      case 4:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	fprintf (stream, " 0x%0x(%s),%s",
		 extra_1,
		 reg_names [op_1_regno],
		 reg_names [op_2_regno]);
	break;
	
	/* S1 destination mode */
      case 5:
	fprintf (stream,
		 ((index_reg_regno) ? "%s,(%s)[%s*%1d]" : "%s,(%s)"),
		 reg_names [op_1_regno],
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
	
      case 6:
	fprintf (stream,
		 ((index_reg_regno) ? " $%#0x,(%s)[%s*%1d]"
		  : " $%#0x,(%s)"),
		 op_1_regno,
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
	
      case 7:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	fprintf (stream,
		 ((index_reg_regno) ? " $%#0x,(%s)[%s*%1d]"
		  : " $%#0x,(%s)"),
		 extra_1,
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
	
      case 8:
	fprintf (stream,
		 ((index_reg_regno) ? " (%s),(%s)[%s*%1d]" : " (%s),(%s)"),
		 reg_names [op_1_regno],
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
	
      case 9:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	fprintf (stream,
		 ((index_reg_regno)
		  ? "%#0x(%s),(%s)[%s*%1d]"
		  : "%#0x(%s),(%s)"),
		 extra_1,
		 reg_names [op_1_regno],
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
	
	/* S2 destination mode */
      case 10:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	fprintf (stream,
		 ((index_reg_regno) ? "%s,%#0x(%s)[%s*%1d]" : "%s,%#0x(%s)"),
		 reg_names [op_1_regno],
		 extra_1,
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
      case 11:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	fprintf (stream,
		 ((index_reg_regno) ?
		  " $%#0x,%#0x(%s)[%s*%1d]" : " $%#0x,%#0x(%s)"),
		 op_1_regno,
		 extra_1,
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
      case 12:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	read_memory (memaddr+8, buffer, MAXLEN);
	insn_size += 4;
	extra_2 = * ((int *) buffer);
	fprintf (stream,
		 ((index_reg_regno) ?
		  " $%#0x,%#0x(%s)[%s*%1d]" : " $%#0x,%#0x(%s)"),
		 extra_1,
		 extra_2,
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
	
      case 13:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	fprintf (stream,
		 ((index_reg_regno)
		  ? " (%s),%#0x(%s)[%s*%1d]" 
		  : " (%s),%#0x(%s)"),
		 reg_names [op_1_regno],
		 extra_1,
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
      case 14:
	read_memory (memaddr+4, buffer, MAXLEN);
	insn_size += 4;
	extra_1 = * ((int *) buffer);
	read_memory (memaddr+8, buffer, MAXLEN);
	insn_size += 4;
	extra_2 = * ((int *) buffer);
	fprintf (stream,
		 ((index_reg_regno) ? "%#0x(%s),%#0x(%s)[%s*%1d]"
		  : "%#0x(%s),%#0x(%s) "),
		 extra_1,
		 reg_names [op_1_regno],
		 extra_2,
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	break;
	
      default:
	fprintf (stream,
		 ((index_reg_regno) ? "%s,%s [%s*%1d]" : "%s,%s"),
		 reg_names [op_1_regno],
		 reg_names [op_2_regno],
		 reg_names [index_reg_regno],
		 index_multiplier);
	fprintf (stream,
		 "\t\t# unknown mode in %08x",
		 insn);
	break;
      } /* switch */
    }
  
  {
    return insn_size;
  }
  abort ();
}
