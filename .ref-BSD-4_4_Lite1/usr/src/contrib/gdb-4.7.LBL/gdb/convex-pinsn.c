/* Print Convex instructions for GDB, the GNU debugger.
   Copyright 1989, 1991 Free Software Foundation, Inc.

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

/* reg (fmt_field, inst_field) --
   the {first,second,third} operand of instruction as fmt_field = [ijk]
   gets the value of the field from the [ijk] position of the instruction */

#define reg(a,b) ((char (*)[3])(op[fmt->a]))[inst.f0.b]

/* lit (fmt_field) -- field [ijk] is a literal (PSW, VL, eg) */

#define lit(i) op[fmt->i]

/* aj[j] -- name for A register j */

#define aj ((char (*)[3])(op[A]))

union inst {
    struct {
	unsigned   : 7;
	unsigned i : 3;
	unsigned j : 3;
	unsigned k : 3;
	unsigned   : 16;
	unsigned   : 32;
    } f0;
    struct {
	unsigned   : 8;
	unsigned indir : 1;
	unsigned len : 1;
	unsigned j : 3;
	unsigned k : 3;
	unsigned   : 16;
	unsigned   : 32;
    } f1;
    unsigned char byte[8];
    unsigned short half[4];
    char signed_byte[8];
    short signed_half[4];
};

struct opform {
    int mask;			/* opcode mask */
    int shift;			/* opcode align */
    struct formstr *formstr[3];	/* ST, E0, E1 */
};

struct formstr {
    unsigned lop:8, rop:5;	/* opcode */
    unsigned fmt:5;		/* inst format */
    unsigned i:5, j:5, k:2;	/* operand formats */
};

#include "convx-opcode.h"

unsigned char formdecode [] = {
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
    4,4,4,4,4,4,4,4,5,5,5,5,6,6,7,8,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
};

struct opform opdecode[] = {
    0x7e00, 9, format0, e0_format0, e1_format0,
    0x3f00, 8, format1, e0_format1, e1_format1,
    0x1fc0, 6, format2, e0_format2, e1_format2,
    0x0fc0, 6, format3, e0_format3, e1_format3,
    0x0700, 8, format4, e0_format4, e1_format4,
    0x03c0, 6, format5, e0_format5, e1_format5,
    0x01f8, 3, format6, e0_format6, e1_format6,
    0x00f8, 3, format7, e0_format7, e1_format7,
    0x0000, 0, formatx, formatx, formatx,
    0x0f80, 7, formatx, formatx, formatx,
    0x0f80, 7, formatx, formatx, formatx,
};

/* Print the instruction at address MEMADDR in debugged memory,
   on STREAM.  Returns length of the instruction, in bytes.  */

int
print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
  union inst inst;
  struct formstr *fmt;
  register int format, op1, pfx;
  int l;

  read_memory (memaddr, &inst, sizeof inst);

  /* Remove and note prefix, if present */
    
  pfx = inst.half[0];
  if ((pfx & 0xfff0) == 0x7ef0)
    {
      pfx = ((pfx >> 3) & 1) + 1;
      *(long long *) &inst = *(long long *) &inst.half[1];
    }
  else pfx = 0;

  /* Split opcode into format.op1 and look up in appropriate table */

  format = formdecode[inst.byte[0]];
  op1 = (inst.half[0] & opdecode[format].mask) >> opdecode[format].shift;
  if (format == 9)
    {
      if (pfx)
	fmt = formatx;
      else if (inst.f1.j == 0)
	fmt = &format1a[op1];
      else if (inst.f1.j == 1)
	fmt = &format1b[op1];
      else
	fmt = formatx;
    }
  else
    fmt = &opdecode[format].formstr[pfx][op1];

  /* Print it */

  if (fmt->fmt == xxx)
    {
      /* noninstruction */
      fprintf (stream, "0x%04x", pfx ? pfx : inst.half[0]);
      return 2;
    }

  if (pfx)
    pfx = 2;

  fprintf (stream, "%s%s%s", lop[fmt->lop], rop[fmt->rop],
	   &"        "[strlen(lop[fmt->lop]) + strlen(rop[fmt->rop])]);

  switch (fmt->fmt)
    {
    case rrr:			/* three register */
      fprintf (stream, "%s,%s,%s", reg(i,i), reg(j,j), reg(k,k));
      return pfx + 2;

    case rr:			/* two register */
      fprintf (stream, "%s,%s", reg(i,j), reg(j,k));
      return pfx + 2;

    case rxr:			/* two register, reversed i and j fields */
      fprintf (stream, "%s,%s", reg(i,k), reg(j,j));
      return pfx + 2;

    case r:			/* one register */
      fprintf (stream, "%s", reg(i,k));
      return pfx + 2;

    case nops:			/* no operands */
      return pfx + 2;

    case nr:			/* short immediate, one register */
      fprintf (stream, "#%d,%s", inst.f0.j, reg(i,k));
      return pfx + 2;

    case pcrel:			/* pc relative */
      print_address (memaddr + 2 * inst.signed_byte[1], stream);
      return pfx + 2;

    case lr:			/* literal, one register */
      fprintf (stream, "%s,%s", lit(i), reg(j,k));
      return pfx + 2;

    case rxl:			/* one register, literal */
      fprintf (stream, "%s,%s", reg(i,k), lit(j));
      return pfx + 2;

    case rlr:			/* register, literal, register */
      fprintf (stream, "%s,%s,%s", reg(i,j), lit(j), reg(k,k));
      return pfx + 2;

    case rrl:			/* register, register, literal */
      fprintf (stream, "%s,%s,%s", reg(i,j), reg(j,k), lit(k));
      return pfx + 2;

    case iml:			/* immediate, literal */
      if (inst.f1.len)
	{
	  fprintf (stream, "#%#x,%s",
		   (inst.signed_half[1] << 16) + inst.half[2], lit(i));
	  return pfx + 6;
	}
      else
	{
	  fprintf (stream, "#%d,%s", inst.signed_half[1], lit(i));
	  return pfx + 4;
	}

    case imr:			/* immediate, register */
      if (inst.f1.len)
	{
	  fprintf (stream, "#%#x,%s",
		   (inst.signed_half[1] << 16) + inst.half[2], reg(i,k));
	  return pfx + 6;
	}
      else
	{
	  fprintf (stream, "#%d,%s", inst.signed_half[1], reg(i,k));
	  return pfx + 4;
	}

    case a1r:			/* memory, register */
      l = print_effa (inst, stream);
      fprintf (stream, ",%s", reg(i,k));
      return pfx + l;

    case a1l:			/* memory, literal  */
      l = print_effa (inst, stream);
      fprintf (stream, ",%s", lit(i));
      return pfx + l;

    case a2r:			/* register, memory */
      fprintf (stream, "%s,", reg(i,k));
      return pfx + print_effa (inst, stream);

    case a2l:			/* literal, memory */
      fprintf (stream, "%s,", lit(i));
      return pfx + print_effa (inst, stream);

    case a3:			/* memory */
      return pfx + print_effa (inst, stream);

    case a4:			/* system call */
      l = 29; goto a4a5;
    case a5:			/* trap */
      l = 27;
    a4a5:
      if (inst.f1.len)
	{
	  unsigned int m = (inst.signed_half[1] << 16) + inst.half[2];
	  fprintf (stream, "#%d,#%d", m >> l, m & (-1 >> (32-l)));
	  return pfx + 6;
	}
      else
	{
	  unsigned int m = inst.signed_half[1];
	  fprintf (stream, "#%d,#%d", m >> l, m & (-1 >> (32-l)));
	  return pfx + 4;
	}
    }
}


/* print effective address @nnn(aj), return instruction length */

int print_effa (inst, stream)
     union inst inst;
     FILE *stream;
{
  int n, l;

  if (inst.f1.len)
    {
      n = (inst.signed_half[1] << 16) + inst.half[2];
      l = 6;
    }
  else
    {
      n = inst.signed_half[1];
      l = 4;
    }
	
  if (inst.f1.indir)
    printf ("@");

  if (!inst.f1.j)
    {
      print_address (n, stream);
      return l;
    }

  fprintf (stream, (n & 0xf0000000) == 0x80000000 ? "%#x(%s)" : "%d(%s)",
	   n, aj[inst.f1.j]);

  return l;
}
