/* Instruction printing code for the AMD 29000
   Copyright (C) 1990 Free Software Foundation, Inc.
   Contributed by Cygnus Support.  Written by Jim Kingdon.

This file is part of GDB.

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
#include "target.h"
#include "opcode/a29k.h"

/* Print a symbolic representation of a general-purpose
   register number NUM on STREAM.
   NUM is a number as found in the instruction, not as found in
   debugging symbols; it must be in the range 0-255.  */
static void
print_general (num, stream)
     int num;
     FILE *stream;
{
  if (num < 128)
    fprintf_filtered (stream, "gr%d", num);
  else
    fprintf_filtered (stream, "lr%d", num - 128);
}

/* Like print_general but a special-purpose register.
   
   The mnemonics used by the AMD assembler are not quite the same
   as the ones in the User's Manual.  We use the ones that the
   assembler uses.  */
static void
print_special (num, stream)
     int num;
     FILE *stream;
{
  /* Register names of registers 0-SPEC0_NUM-1.  */
  static char *spec0_names[] = {
    "vab", "ops", "cps", "cfg", "cha", "chd", "chc", "rbp", "tmc", "tmr",
    "pc0", "pc1", "pc2", "mmu", "lru"
    };
#define SPEC0_NUM ((sizeof spec0_names) / (sizeof spec0_names[0]))

  /* Register names of registers 128-128+SPEC128_NUM-1.  */
  static char *spec128_names[] = {
    "ipc", "ipa", "ipb", "q", "alu", "bp", "fc", "cr"
    };
#define SPEC128_NUM ((sizeof spec128_names) / (sizeof spec128_names[0]))

  /* Register names of registers 160-160+SPEC160_NUM-1.  */
  static char *spec160_names[] = {
    "fpe", "inte", "fps", "sr163", "exop"
    };
#define SPEC160_NUM ((sizeof spec160_names) / (sizeof spec160_names[0]))

  if (num < SPEC0_NUM)
    fprintf_filtered (stream, spec0_names[num]);
  else if (num >= 128 && num < 128 + SPEC128_NUM)
    fprintf_filtered (stream, spec128_names[num-128]);
  else if (num >= 160 && num < 160 + SPEC160_NUM)
    fprintf_filtered (stream, spec160_names[num-160]);
  else
    fprintf_filtered (stream, "sr%d", num);
}

/* Is an instruction with OPCODE a delayed branch?  */
static int
is_delayed_branch (opcode)
     int opcode;
{
  return (opcode == 0xa8 || opcode == 0xa9 || opcode == 0xa0 || opcode == 0xa1
	  || opcode == 0xa4 || opcode == 0xa5
	  || opcode == 0xb4 || opcode == 0xb5
	  || opcode == 0xc4 || opcode == 0xc0
	  || opcode == 0xac || opcode == 0xad
	  || opcode == 0xcc);
}

/* Now find the four bytes of INSN and put them in *INSN{0,8,16,24}.
   Note that the amd can be set up as either
   big or little-endian (the tm file says which) and we can't assume
   the host machine is the same.  */
static void
find_bytes (insn, insn0, insn8, insn16, insn24)
     char *insn;
     unsigned char *insn0;
     unsigned char *insn8;
     unsigned char *insn16;
     unsigned char *insn24;
{
#if TARGET_BYTE_ORDER == BIG_ENDIAN
  *insn24 = insn[0];
  *insn16 = insn[1];
  *insn8  = insn[2];
  *insn0  = insn[3];
#else /* Little-endian.  */
  *insn24 = insn[3];
  *insn16 = insn[2];
  *insn8 = insn[1];
  *insn0 = insn[0];
#endif /* Little-endian.  */
}

/* Print one instruction from MEMADDR on STREAM.
   Return the size of the instruction (always 4 on am29k).  */
int
print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
  /* The raw instruction.  */
  char insn[4];

  /* The four bytes of the instruction.  */
  unsigned char insn24, insn16, insn8, insn0;

  struct a29k_opcode *opcode;

  read_memory (memaddr, &insn[0], 4);

  find_bytes (insn, &insn0, &insn8, &insn16, &insn24);

  /* Handle the nop (aseq 0x40,gr1,gr1) specially */
  if ((insn24==0x70) && (insn16==0x40) && (insn8==0x01) && (insn0==0x01)) {
    fprintf_filtered (stream,"nop");
    return 4;
  }

  /* The opcode is always in insn24.  */
  for (opcode = &a29k_opcodes[0];
       opcode < &a29k_opcodes[num_opcodes];
       ++opcode)
    {
      if ((insn24<<24) == opcode->opcode)
	{
	  char *s;
	  
	  fprintf_filtered (stream, "%s ", opcode->name);
	  for (s = opcode->args; *s != '\0'; ++s)
	    {
	      switch (*s)
		{
		case 'a':
		  print_general (insn8, stream);
		  break;
		  
		case 'b':
		  print_general (insn0, stream);
		  break;

		case 'c':
		  print_general (insn16, stream);
		  break;

		case 'i':
		  fprintf_filtered (stream, "%d", insn0);
		  break;

		case 'x':
		  fprintf_filtered (stream, "%d", (insn16 << 8) + insn0);
		  break;

		case 'h':
		  fprintf_filtered (stream, "0x%x",
				    (insn16 << 24) + (insn0 << 16));
		  break;

		case 'X':
		  fprintf_filtered (stream, "%d",
				    ((insn16 << 8) + insn0) | 0xffff0000);
		  break;

		case 'P':
		  /* This output looks just like absolute addressing, but
		     maybe that's OK (it's what the GDB 68k and EBMON
		     29k disassemblers do).  */
		  /* All the shifting is to sign-extend it.  p*/
		  print_address
		    (memaddr +
		     (((int)((insn16 << 10) + (insn0 << 2)) << 14) >> 14),
		     stream);
		  break;

		case 'A':
		  print_address ((insn16 << 10) + (insn0 << 2), stream);
		  break;

		case 'e':
		  fprintf_filtered (stream, "%d", insn16 >> 7);
		  break;

		case 'n':
		  fprintf_filtered (stream, "0x%x", insn16 & 0x7f);
		  break;

		case 'v':
		  fprintf_filtered (stream, "0x%x", insn16);
		  break;

		case 's':
		  print_special (insn8, stream);
		  break;

		case 'u':
		  fprintf_filtered (stream, "%d", insn0 >> 7);
		  break;

		case 'r':
		  fprintf_filtered (stream, "%d", (insn0 >> 4) & 7);
		  break;

		case 'd':
		  fprintf_filtered (stream, "%d", (insn0 >> 2) & 3);
		  break;

		case 'f':
		  fprintf_filtered (stream, "%d", insn0 & 3);
		  break;

		case 'F':
		  fprintf_filtered (stream, "%d", (insn16 >> 2) & 15);
		  break;

		case 'C':
		  fprintf_filtered (stream, "%d", insn16 & 3);
		  break;

		default:
		  fprintf_filtered (stream, "%c", *s);
		}
	    }

	  /* Now we look for a const,consth pair of instructions,
	     in which case we try to print the symbolic address.  */
	  if (insn24 == 2)  /* consth */
	    {
	      int errcode;
	      char prev_insn[4];
	      unsigned char prev_insn0, prev_insn8, prev_insn16, prev_insn24;
	      
	      errcode = target_read_memory (memaddr - 4,
					    &prev_insn[0],
					    4);
	      if (errcode == 0)
		{
		  /* If it is a delayed branch, we need to look at the
		     instruction before the delayed brach to handle
		     things like
		     
		     const _foo
		     call _printf
		     consth _foo
		     */
		  find_bytes (prev_insn, &prev_insn0, &prev_insn8,
			      &prev_insn16, &prev_insn24);
		  if (is_delayed_branch (prev_insn24))
		    {
		      errcode = target_read_memory
			(memaddr - 8, &prev_insn[0], 4);
		      find_bytes (prev_insn, &prev_insn0, &prev_insn8,
				  &prev_insn16, &prev_insn24);
		    }
		}
		  
	      /* If there was a problem reading memory, then assume
		 the previous instruction was not const.  */
	      if (errcode == 0)
		{
		  /* Is it const to the same register?  */
		  if (prev_insn24 == 3
		      && prev_insn8 == insn8)
		    {
		      fprintf_filtered (stream, "\t; ");
		      print_address (((insn16 << 24) + (insn0 << 16)
				      + (prev_insn16 << 8) + (prev_insn0)),
				     stream);
		    }
		}
	    }

	  return 4;
	}
    }
  fprintf_filtered (stream, ".word 0x%8x",
		    (insn24 << 24) + (insn16 << 16) + (insn8 << 8) + insn0);
  return 4;
}
