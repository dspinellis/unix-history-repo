/* Print mips instructions for GDB, the GNU debugger.
   Copyright 1989, 1991, 1992 Free Software Foundation, Inc.
   Contributed by Nobuyuki Hikichi(hikichi@sra.co.jp).

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
#include "symtab.h"
#include "opcode/mips.h"

/* Mips instructions are never longer than this many bytes.  */
#define MAXLEN 4

/* Number of elements in the opcode table.  */
#define NOPCODES (sizeof mips_opcodes / sizeof mips_opcodes[0])

#define MKLONG(p)  *(unsigned long*)p

/* subroutine */
static unsigned char *
print_insn_arg (d, l, stream, pc)
     char *d;
     register unsigned long int *l;
     FILE *stream;
     CORE_ADDR pc;
{
  switch (*d)
    {
    case ',':
    case '(':
    case ')':
      fputc (*d, stream);
      break;

    case 's':
      fprintf (stream, "$%s", reg_names[((struct op_i_fmt *) l)->rs]);
      break;

    case 't':
      fprintf (stream, "$%s", reg_names[((struct op_i_fmt *) l)->rt]);
      break;

    case 'i':
      fprintf (stream, "%d", ((struct op_i_fmt *) l)->immediate);
      break;

    case 'j': /* same as i, but sign-extended */
      fprintf (stream, "%d", ((struct op_b_fmt *) l)->delta);
      break;

    case 'a':
      print_address ((pc & 0xF0000000) | (((struct op_j_fmt *)l)->target << 2),
		     stream);
      break;

    case 'b':
      print_address ((((struct op_b_fmt *) l)->delta << 2) + pc + 4, stream);
      break;

    case 'd':
      fprintf (stream, "$%s", reg_names[((struct op_r_fmt *) l)->rd]);
      break;

    case 'h':
      fprintf (stream, "0x%x", ((struct op_r_fmt *) l)->shamt);
      break;

    case 'B':
      fprintf (stream, "0x%x", ((struct op_brk_fmt *) l)->code);
      break;

    case 'S':
      fprintf (stream, "$f%d", ((struct fop_r_fmt *) l)->fs);
      break;

    case 'T':
      fprintf (stream, "$f%d", ((struct fop_r_fmt *) l)->ft);
      break;

    case 'D':
      fprintf (stream, "$f%d", ((struct fop_r_fmt *) l)->fd);
      break;

    default:
      fprintf (stream, "# internal error, undefined modifier(%c)", *d);
      break;
    }
}

/* Print the mips instruction at address MEMADDR in debugged memory,
   on STREAM.  Returns length of the instruction, in bytes, which
   is always 4.  */

int
print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
  unsigned char buffer[MAXLEN];
  register int i;
  register char *d;
  unsigned long int l;

  read_memory (memaddr, buffer, MAXLEN);
  SWAP_TARGET_AND_HOST (buffer, MAXLEN);

  for (i = 0; i < NOPCODES; i++)
    {
      register unsigned int opcode = mips_opcodes[i].opcode;
      register unsigned int match = mips_opcodes[i].match;
      if ((*(unsigned int*)buffer & match) == opcode)
	break;
    }

  l = MKLONG (buffer);
  /* Handle undefined instructions.  */
  if (i == NOPCODES)
    {
      fprintf (stream, "0x%x",l);
      return 4;
    }

  fprintf (stream, "%s", mips_opcodes[i].name);

  if (!(d = mips_opcodes[i].args))
    return 4;

  fputc (' ', stream);

  while (*d)
    print_insn_arg (d++, &l, stream, memaddr);

  return 4;
}
