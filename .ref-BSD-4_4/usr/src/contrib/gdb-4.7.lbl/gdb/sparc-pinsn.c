/* Print SPARC instructions for GDB, the GNU Debugger.
   Copyright 1989, 1991, 1992 Free Software Foundation, Inc.

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
#include "opcode/sparc.h"
#include "gdbcore.h"
#include <string.h>
#include "target.h"

#define	freg_names	(&reg_names[4 * 8])

union sparc_insn
  {
    unsigned long int code;
    struct
      {
	unsigned int anop:2;
#define	op	ldst.anop
	unsigned int anrd:5;
#define	rd	ldst.anrd
	unsigned int op3:6;
	unsigned int anrs1:5;
#define	rs1	ldst.anrs1
	unsigned int i:1;
	unsigned int anasi:8;
#define	asi	ldst.anasi
	unsigned int anrs2:5;
#define	rs2	ldst.anrs2
#define	shcnt	rs2
      } ldst;
    struct
      {
	unsigned int anop:2, anrd:5, op3:6, anrs1:5, i:1;
	unsigned int IMM13:13;
#define	imm13	IMM13.IMM13
      } IMM13;
    struct
      {
	unsigned int anop:2;
	unsigned int a:1;
	unsigned int cond:4;
	unsigned int op2:3;
	unsigned int DISP22:22;
#define	disp22	branch.DISP22
      } branch;
#define	imm22	disp22
    struct
      {
	unsigned int anop:2;
	unsigned int adisp30:30;
#define	disp30	call.adisp30
      } call;
  };

/* Nonzero if INSN is the opcode for a delayed branch.  */
static int
is_delayed_branch (insn)
     union sparc_insn insn;
{
  unsigned int i;

  for (i = 0; i < NUMOPCODES; ++i)
    {
      const struct sparc_opcode *opcode = &sparc_opcodes[i];
      if ((opcode->match & insn.code) == opcode->match
	  && (opcode->lose & insn.code) == 0)
	return (opcode->flags & F_DELAYED);
    }
  return 0;
}

static int opcodes_sorted = 0;
extern void qsort ();

/* Print one instruction from MEMADDR on STREAM.

   We suffix the instruction with a comment that gives the absolute
   address involved, as well as its symbolic form, if the instruction
   is preceded by a findable `sethi' and it either adds an immediate
   displacement to that register, or it is an `add' or `or' instruction
   on that register.  */
int
print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
  union sparc_insn insn;

  register unsigned int i;

  if (!opcodes_sorted)
    {
      static int compare_opcodes ();
      qsort ((char *) sparc_opcodes, NUMOPCODES,
	     sizeof (sparc_opcodes[0]), compare_opcodes);
      opcodes_sorted = 1;
    }

  read_memory (memaddr, (char *) &insn, sizeof (insn));

  for (i = 0; i < NUMOPCODES; ++i)
    {
      const struct sparc_opcode *opcode = &sparc_opcodes[i];
      if ((opcode->match & insn.code) == opcode->match
	  && (opcode->lose & insn.code) == 0)
	{
	  /* Nonzero means that we have found an instruction which has
	     the effect of adding or or'ing the imm13 field to rs1.  */
	  int imm_added_to_rs1 = 0;

	  /* Nonzero means that we have found a plus sign in the args
	     field of the opcode table.  */
	  int found_plus = 0;
	  
	  /* Do we have an `add' or `or' instruction where rs1 is the same
	     as rsd, and which has the i bit set?  */
	  if ((opcode->match == 0x80102000 || opcode->match == 0x80002000)
	  /*			  (or)				 (add)  */
	      && insn.rs1 == insn.rd)
	    imm_added_to_rs1 = 1;

	  if (insn.rs1 != insn.rd
	      && strchr (opcode->args, 'r') != 0)
	      /* Can't do simple format if source and dest are different.  */
	      continue;

	  fputs_filtered (opcode->name, stream);

	  {
	    register const char *s;

	    if (opcode->args[0] != ',')
	      fputs_filtered (" ", stream);
	    for (s = opcode->args; *s != '\0'; ++s)
	      {
		if (*s == ',')
		  {
		    fputs_filtered (",", stream);
		    ++s;
		    if (*s == 'a')
		      {
			fputs_filtered ("a", stream);
			++s;
		      }
		    fputs_filtered (" ", stream);
		  }

		switch (*s)
		  {
		  case '+':
		    found_plus = 1;

		    /* note fall-through */
		  default:
		    fprintf_filtered (stream, "%c", *s);
		    break;

		  case '#':
		    fputs_filtered ("0", stream);
		    break;

#define	reg(n)	fprintf_filtered (stream, "%%%s", reg_names[n])
		  case '1':
		  case 'r':
		    reg (insn.rs1);
		    break;

		  case '2':
		    reg (insn.rs2);
		    break;

		  case 'd':
		    reg (insn.rd);
		    break;
#undef	reg

#define	freg(n)	fprintf_filtered (stream, "%%%s", freg_names[n])
		  case 'e':
		  case 'v':	/* double/even */
		  case 'V':	/* quad/multiple of 4 */
		    freg (insn.rs1);
		    break;

		  case 'f':
		  case 'B':	/* double/even */
		  case 'R':	/* quad/multiple of 4 */
		    freg (insn.rs2);
		    break;

		  case 'g':
		  case 'H':	/* double/even */
		  case 'J':	/* quad/multiple of 4 */
		    freg (insn.rd);
		    break;
#undef	freg

#define	creg(n)	fprintf_filtered (stream, "%%c%u", (unsigned int) (n))
		  case 'b':
		    creg (insn.rs1);
		    break;

		  case 'c':
		    creg (insn.rs2);
		    break;

		  case 'D':
		    creg (insn.rd);
		    break;
#undef	creg

		  case 'h':
		    fprintf_filtered (stream, "%%hi(%#x)",
				      (int) insn.imm22 << 10);
		    break;

		  case 'i':
		    {
		      /* We cannot trust the compiler to sign-extend
			 when extracting the bitfield, hence the shifts.  */
		      int imm = ((int) insn.imm13 << 19) >> 19;

		      /* Check to see whether we have a 1+i, and take
			 note of that fact.

			 FIXME: No longer true/relavant ???
			 Note: because of the way we sort the table,
			 we will be matching 1+i rather than i+1,
			 so it is OK to assume that i is after +,
			 not before it.  */
		      if (found_plus)
			imm_added_to_rs1 = 1;
		      
		      if (imm <= 9)
			fprintf_filtered (stream, "%d", imm);
		      else
			fprintf_filtered (stream, "%#x", imm);
		    }
		    break;

		  case 'L':
		    print_address ((CORE_ADDR) memaddr + insn.disp30 * 4,
				   stream);
		    break;

		  case 'l':
		    if ((insn.code >> 22) == 0)
		      /* Special case for `unimp'.  Don't try to turn
			 it's operand into a function offset.  */
		      fprintf_filtered (stream, "%#x",
					(int) (((int) insn.disp22 << 10) >> 10));
		    else
		      /* We cannot trust the compiler to sign-extend
			 when extracting the bitfield, hence the shifts.  */
		      print_address ((CORE_ADDR)
				     (memaddr
				      + (((int) insn.disp22 << 10) >> 10) * 4),
				     stream);
		    break;

		  case 'A':
		    fprintf_filtered (stream, "(%d)", (int) insn.asi);
		    break;

		  case 'C':
		    fputs_filtered ("%csr", stream);
		    break;

		  case 'F':
		    fputs_filtered ("%fsr", stream);
		    break;

		  case 'p':
		    fputs_filtered ("%psr", stream);
		    break;

		  case 'q':
		    fputs_filtered ("%fq", stream);
		    break;

		  case 'Q':
		    fputs_filtered ("%cq", stream);
		    break;

		  case 't':
		    fputs_filtered ("%tbr", stream);
		    break;

		  case 'w':
		    fputs_filtered ("%wim", stream);
		    break;

		  case 'y':
		    fputs_filtered ("%y", stream);
		    break;
		  }
	      }
	  }

	  /* If we are adding or or'ing something to rs1, then
	     check to see whether the previous instruction was
	     a sethi to the same register as in the sethi.
	     If so, attempt to print the result of the add or
	     or (in this context add and or do the same thing)
	     and its symbolic value.  */
	  if (imm_added_to_rs1)
	    {
	      union sparc_insn prev_insn;
	      int errcode;

	      errcode = target_read_memory (memaddr - 4,
				     (char *)&prev_insn, sizeof (prev_insn));

	      if (errcode == 0)
		{
		  /* If it is a delayed branch, we need to look at the
		     instruction before the delayed branch.  This handles
		     sequences such as

		     sethi %o1, %hi(_foo), %o1
		     call _printf
		     or %o1, %lo(_foo), %o1
		     */

		  if (is_delayed_branch (prev_insn))
		    errcode = target_read_memory
		      (memaddr - 8, (char *)&prev_insn, sizeof (prev_insn));
		}

	      /* If there was a problem reading memory, then assume
		 the previous instruction was not sethi.  */
	      if (errcode == 0)
		{
		  /* Is it sethi to the same register?  */
		  if ((prev_insn.code & 0xc1c00000) == 0x01000000
		      && prev_insn.rd == insn.rs1)
		    {
		      fprintf_filtered (stream, "\t! ");
		      /* We cannot trust the compiler to sign-extend
			 when extracting the bitfield, hence the shifts.  */
		      print_address (((int) prev_insn.imm22 << 10)
				     | (insn.imm13 << 19) >> 19, stream);
		    }
		}
	    }

	  return sizeof (insn);
	}
    }

  printf_filtered ("%#8x", insn.code);
  return sizeof (insn);
}

/* Compare opcodes A and B.  */

static int
compare_opcodes (a, b)
     char *a, *b;
{
  struct sparc_opcode *op0 = (struct sparc_opcode *) a;
  struct sparc_opcode *op1 = (struct sparc_opcode *) b;
  unsigned long int match0 = op0->match, match1 = op1->match;
  unsigned long int lose0 = op0->lose, lose1 = op1->lose;
  register unsigned int i;

  /* If a bit is set in both match and lose, there is something
     wrong with the opcode table.  */
  if (match0 & lose0)
    {
      fprintf (stderr, "Internal error:  bad sparc-opcode.h: \"%s\", %#.8lx, %#.8lx\n",
	       op0->name, match0, lose0);
      op0->lose &= ~op0->match;
      lose0 = op0->lose;
    }

  if (match1 & lose1)
    {
      fprintf (stderr, "Internal error: bad sparc-opcode.h: \"%s\", %#.8lx, %#.8lx\n",
	       op1->name, match1, lose1);
      op1->lose &= ~op1->match;
      lose1 = op1->lose;
    }

  /* Because the bits that are variable in one opcode are constant in
     another, it is important to order the opcodes in the right order.  */
  for (i = 0; i < 32; ++i)
    {
      unsigned long int x = 1 << i;
      int x0 = (match0 & x) != 0;
      int x1 = (match1 & x) != 0;

      if (x0 != x1)
	return x1 - x0;
    }

  for (i = 0; i < 32; ++i)
    {
      unsigned long int x = 1 << i;
      int x0 = (lose0 & x) != 0;
      int x1 = (lose1 & x) != 0;

      if (x0 != x1)
	return x1 - x0;
    }

  /* They are functionally equal.  So as long as the opcode table is
     valid, we can put whichever one first we want, on aesthetic grounds.  */

  /* Our first aesthetic ground is that aliases defer to real insns.  */
  {
    int alias_diff = (op0->flags & F_ALIAS) - (op1->flags & F_ALIAS);
    if (alias_diff != 0)
      /* Put the one that isn't an alias first.  */
      return alias_diff;
  }

  /* Except for aliases, two "identical" instructions had
     better have the same opcode.  This is a sanity check on the table.  */
  i = strcmp (op0->name, op1->name);
  if (i)
      if (op0->flags & F_ALIAS) /* If they're both aliases, be arbitrary. */
	  return i;
      else
	  fprintf (stderr,
		   "Internal error: bad sparc-opcode.h: \"%s\" == \"%s\"\n",
		   op0->name, op1->name);

  /* Fewer arguments are preferred.  */
  {
    int length_diff = strlen (op0->args) - strlen (op1->args);
    if (length_diff != 0)
      /* Put the one with fewer arguments first.  */
      return length_diff;
  }

  /* Put 1+i before i+1.  */
  {
    char *p0 = (char *) strchr(op0->args, '+');
    char *p1 = (char *) strchr(op1->args, '+');

    if (p0 && p1)
      {
	/* There is a plus in both operands.  Note that a plus
	   sign cannot be the first character in args,
	   so the following [-1]'s are valid.  */
	if (p0[-1] == 'i' && p1[1] == 'i')
	  /* op0 is i+1 and op1 is 1+i, so op1 goes first.  */
	  return 1;
	if (p0[1] == 'i' && p1[-1] == 'i')
	  /* op0 is 1+i and op1 is i+1, so op0 goes first.  */
	  return -1;
      }
  }

  /* They are, as far as we can tell, identical.
     Since qsort may have rearranged the table partially, there is
     no way to tell which one was first in the opcode table as
     written, so just say there are equal.  */
  return 0;
}
