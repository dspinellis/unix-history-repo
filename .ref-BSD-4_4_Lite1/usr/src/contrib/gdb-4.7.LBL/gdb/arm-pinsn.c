/* Print Acorn Risc Machine instructions for GDB, the GNU debugger.
   Copyright 1986, 1989, 1991, 1992 Free Software Foundation, Inc.

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
#include <ctype.h>
#include <assert.h>

#include "symtab.h"
#include "opcode/arm.h"

static char *shift_names[] = {
    "lsl", "lsr", "asr", "ror",
};

static char *cond_names[] = {
	"eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
	"hi", "ls", "ge", "lt", "gt", "le", "", "nv"
};

static char float_precision[] = "sdep";
static char float_rounding[] = " pmz";
static float float_immed[] = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 0.5, 10.0 };

static void print_ldr_str_offset();
static void print_ldc_stc_offset();
static long immediate_value();

/* Print the ARM instruction at address MEMADDR in debugged memory,
   on STREAM.  Returns length of the instruction, in bytes.  */

int
print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
    unsigned long ins;
    register struct opcode *op;
    register char *p;
    register int i, c;
    int s, e, val;

    ins = read_memory_integer(memaddr, 4);
    for (i = 0, op = opcodes; i < N_OPCODES; i++, op++)
	if ((ins & op->mask) == op->value) break;
    assert(i != N_OPCODES);
    
    for (p = op->assembler; *p;) {
	c = *p++;
	if (c == '%') {
	    s = e = 0;
	    while (isdigit(*p))
		s = s*10 + (*p++ - '0');
	    if (*p == '-') {
		p++;
		while (isdigit(*p))
		    e = e*10 + (*p++ - '0');
	    } else
		e = s;
	    assert(s >= 0 && s <= 31 && e >= 0 && e <= 31);
	    val = (ins >> s) & ((1 << (e + 1 - s)) - 1);
	    switch (*p++) {
	    case '%' :
		putc('%', stream);
		break;
	    case 'd' :
		fprintf(stream, "%d", val);
		break;
	    case 'x' :
		fprintf(stream, "%x", val);
		break;
	    case 'r' :
		assert(val >= 0 && val <= 15);
		fprintf(stream, "%s", reg_names[val]);
		break;
	    case 'c' :
		fprintf(stream, "%s", cond_names[ins >> 28]);
		break;
	    case '\'' :
		assert(*p);
		c = *p++;
		if (val)
		    putc(c, stream);
		break;
	    case '`' :
		assert(*p);
		c = *p++;
		if (!val)
		    putc(c, stream);
		break;
	    case '?' :
		assert(*p);
		c = *p++;
		assert(*p);
		if (val)
		    p++;
		else
		    c = *p++;
		putc(c, stream);
		break;
	    case 'p' :
		if (((ins >> 12) & 0xf) == 0xf)
		    putc('p', stream);
		break;
	    case 'o' :
		if (ins & (1<<25)) {
		    int immed = immediate_value(ins & 0xfff);
		    fprintf (stream, "#%d (0x%x)", immed, immed);
		} else {
		    int operand2 = ins & 0xfff;
		    /* in operand2 :
		       bits 0-3 are the base register
		       bits 5-6 are the shift (0=lsl, 1=lsr, 2=asr, 3=ror)
		       if bit 4 is zero then bits 7-11 are an immediate shift count
		       else bit 7 must be zero and bits 8-11 are the register
		       to be used as a shift count.
		       Note: no shift at all is encoded as "reg lsl #0" */
		    fprintf (stream, "%s", reg_names[operand2 & 0xf]);
		    if (operand2 & 0xff0) {
			/* ror #0 is really rrx (rotate right extend) */
			if ((operand2 & 0xff0) == 0x060)
			    fprintf (stream, ", rrx");
			else {
			    fprintf (stream, ", %s ",
				     shift_names[(operand2 >> 5) & 3]);
			    if (operand2 & (1<<4)) /* register shift */
				fprintf (stream, "%s",
					 reg_names[operand2 >> 8]);
			    else	/* immediate shift */
				fprintf (stream, "#%d",
					 operand2 >> 7);
			}
		    }
		}
		break;
	    case 'a' :
		fprintf (stream, "[%s", reg_names[(ins >> 16) & 0xf]);
		if (ins & (1<<24)) {
		    fprintf (stream, ", ");
		    print_ldr_str_offset (ins, stream);
		    putc (']', stream);
		    if (ins & (1<<21)) putc('!', stream);
		    /* If it is a pc relative load, then it is probably
		       a constant so print it */
		    if (((ins >> 16) & 0xf) == 15 &&
			(ins & (1<<25)) == 0 &&
			(ins & (1<<20))) {
			int addr = memaddr + 8 +
			    (ins & 0xfff) * ((ins & (1<<23)) ? 1 : -1);
			fprintf (stream, " (contents=");
			print_address (read_memory_integer(addr, 4), stream);
			fprintf (stream, ")");
		    }
		} else {
		    fprintf (stream, "]," );
		    print_ldr_str_offset (ins, stream);
		}
		break;
	    case 'b' :
		print_address (memaddr + 8 + (((int)ins << 8) >> 6), stream);
		break;
	    case 'A' :
		fprintf (stream, "[%s", reg_names[(ins >> 16) & 0xf]);
		if (ins & (1<<24)) {
		    fprintf (stream, ", ");
		    print_ldc_stc_offset (ins, stream);
		    putc(']', stream);
		    if (ins & (1<<21))
			putc('!', stream);
		} else {
		    fprintf (stream, "], ");
		    print_ldc_stc_offset (ins, stream);
		}
		break;
	    case 'm' :
		{
		    int regnum, first = 1;
		    putc('{', stream);
		    for (regnum = 0; regnum < 16; regnum++)
			if (ins & (1<<regnum)) {
			    if (!first)
				putc (',', stream);
			    first = 0;
			    fprintf (stream, "%s", reg_names[regnum]);
			}
		    putc('}', stream);
		}
		break;
	    case 'P' :
		val = ((ins >> 18) & 2) | ((ins >> 7) & 1);
		putc(float_precision[val], stream);
		break;
	    case 'Q' :
		val = ((ins >> 21) & 2) | ((ins >> 15) & 1);
		putc(float_precision[val], stream);
		break;
	    case 'R' :
		val = ((ins >> 5) & 3);
		if (val) putc(float_rounding[val], stream);
		break;
	    case 'f' :
		assert(val >= 0 && val <= 15);
		if (val > 7)
		    fprintf (stream, "#%3.1f", float_immed[val - 8]);
		else
		    fprintf (stream, "f%d", val);
		break;
	    default:
		abort();
	    }
	} else
	    putc(c, stream);
    }
    return 4;
}

static long
immediate_value(operand)
int operand;
{
    int val = operand & 0xff;
    int shift = 2*(operand >> 8);
    /* immediate value is (val ror shift) */
    return (val >> shift) | (val << (32 - shift));
}

static void
print_ldr_str_offset(ins, stream)
unsigned long ins;
FILE *stream;
{
    if ((ins & (1<<25)) == 0)
	fprintf (stream, "#%d",
		 (ins & 0xfff) * ((ins & (1<<23)) ? 1 : -1));
    else {
	fprintf (stream, "%s%s", reg_names[ins & 0xf],
		 (ins & (1<<23)) ? "" : "-");
	if (ins & 0xff0)
	    fprintf (stream, ", %s #%d",
		     shift_names[(ins >> 5) & 3],
		     (ins >> 7) & 0x1f);
    }
}

static void
print_ldc_stc_offset(ins, stream)
unsigned long ins;
FILE *stream;
{
    fprintf (stream, "#%d",
	     4 * (ins & 0xff) * ((ins & (1<<23)) ? 1 : -1));
}
