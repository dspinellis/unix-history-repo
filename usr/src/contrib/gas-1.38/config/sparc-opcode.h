/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1993 by Chris Torek at Lawrence Berkeley Laboratory.
 *
 *	@(#)sparc-opcode.h	5.2 (Berkeley) 4/12/93
 */

/* Table of opcodes for the sparc.
   Copyright (C) 1989 Free Software Foundation, Inc.

This file is part of GAS, the GNU Assembler, and GDB, the GNU disassembler.

GAS/GDB is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GAS/GDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GAS or GDB; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#if !defined(__STDC__) && !defined(const)
#define const
#endif

/*
 * Structure of an opcode table entry.
 */
struct sparc_opcode
{
    const char *name;
    unsigned long int match;	/* Bits that must be set.  */
    unsigned long int lose;	/* Bits that must not be set.  */
    const char *args;
    /* Nonzero if this is a delayed branch instruction.  */
    char delayed;
};

/*
   All sparc opcodes are 32 bits, except for the `set' instruction (really
   a macro), which is 64 bits.  It is handled as a special case.

   The match component is a mask saying which bits must match a
   particular opcode in order for an instruction to be an instance
   of that opcode.

   The args component is a string containing one character
   for each operand of the instruction.

Kinds of operands:
   #    Number used by optimizer.  It is ignored.
   1    rs1 register.
   2    rs2 register.
   d    rd register.
   e    frs1 floating point register.
   f    frs2 floating point register.
   g    frsd floating point register.
   b    crs1 coprocessor register
   c    crs2 coprocessor register
   D    crsd coprocessor register
   h    22 high bits.
   i    13 bit Immediate.
   l    22 bit PC relative immediate.
   L    30 bit PC relative immediate.
   a    Annul.  The annul bit is set.
   A    Alternate address space.  Stored as 8 bits.
   C    Coprocessor state register.
   F    floating point state register.
   p    Processor state register.
   q    Floating point queue.
   r    Single register that is both rs1 and rsd.
   R	Single register that is both rs2 and rsd.
   Q    Coprocessor queue.
   S    Special case.
   t    Trap base register.
   w    Window invalid mask register.
   y    Y register.

*/

/* The order of the opcodes in this table is significant:
   
   * The assembler requires that all instances of the same mnemonic must be
   consecutive.  If they aren't, the assembler will bomb at runtime.

   * The disassembler should not care about the order of the opcodes.  */

static struct sparc_opcode sparc_opcodes[] =
{

{ "ldd",        0xc1980000, 0x0060201f, "[1],D", 0 }, /* ldd [rs1+%g0],d */
{ "ldd",        0xc1982000, 0x00601fff, "[1],D", 0 }, /* ldd [rs1+0],d */
{ "ldd",        0xc1982000, 0x00600000, "[1+i],D", 0 },
{ "ldd",        0xc1982000, 0x00600000, "[i+1],D", 0 },
{ "ldd",        0xc1980000, 0x00602000, "[1+2],D", 0 },
{ "ldd",        0xc1180000, 0x00e0201f, "[1],g", 0 }, /* ldd [rs1+%g0],d */
{ "ldd",        0xc1182000, 0x00e01fff, "[1],g", 0 }, /* ldd [rs1+0],d */
{ "ldd",        0xc1182000, 0x00e00000, "[1+i],g", 0 },
{ "ldd",        0xc1182000, 0x00e00000, "[i+1],g", 0 },
{ "ldd",        0xc1180000, 0x00e02000, "[1+2],g", 0 },
{ "ldd",	0xc0180000, 0x01e0201f, "[1],d", 0 }, /* ldd [rs1+%g0],d */
{ "ldd",        0xc0182000, 0x01e01fff, "[1],d", 0 }, /* ldd [rs1+0],d */
{ "ldd",	0xc0182000, 0x01e00000, "[1+i],d", 0 },
{ "ldd",	0xc0182000, 0x01e00000, "[i+1],d", 0 },
{ "ldd",	0xc0180000, 0x01e02000, "[1+2],d", 0 },
{ "ld",         0xc1880000, 0x0070201f, "[1],C", 0 }, /* ld [rs1+%g0],d */
{ "ld",         0xc1882000, 0x00701fff, "[1],C", 0 }, /* ld [rs1+0],d */
{ "ld",         0xc1882000, 0x00700000, "[1+i],C", 0 },
{ "ld",         0xc1882000, 0x00700000, "[i+1],C", 0 },
{ "ld",         0xc1880000, 0x00702000, "[1+2],C", 0 },
{ "ld",         0xc1800000, 0x0078201f, "[1],D", 0 }, /* ld [rs1+%g0],d */
{ "ld",         0xc1802000, 0x00781fff, "[1],D", 0 }, /* ld [rs1+0],d */
{ "ld",         0xc1802000, 0x00780000, "[1+i],D", 0 },
{ "ld",         0xc1802000, 0x00780000, "[i+1],D", 0 },
{ "ld",         0xc1800000, 0x00782000, "[1+2],D", 0 },
{ "ld",         0xc1080000, 0x00f0201f, "[1],F", 0 }, /* ld [rs1+%g0],d */
{ "ld",         0xc1082000, 0x00f01fff, "[1],F", 0 }, /* ld [rs1+0],d */
{ "ld",         0xc1082000, 0x00f00000, "[1+i],F", 0 },
{ "ld",         0xc1082000, 0x00f00000, "[i+1],F", 0 },
{ "ld",         0xc1080000, 0x00f02000, "[1+2],F", 0 },
{ "ld",         0xc1000000, 0x00f8201f, "[1],g", 0 }, /* ld [rs1+%g0],d */
{ "ld",         0xc1002000, 0x00f81fff, "[1],g", 0 }, /* ld [rs1+0],d */
{ "ld",         0xc1002000, 0x00f80000, "[1+i],g", 0 },
{ "ld",         0xc1002000, 0x00f80000, "[i+1],g", 0 },
{ "ld",         0xc1000000, 0x00f82000, "[1+2],g", 0 },
{ "ld",	        0xc0000000, 0x01f8201f, "[1],d", 0 }, /* ld [rs1+%g0],d */
{ "ld",		0xc0002000, 0x01f81fff, "[1],d", 0 }, /* ld [rs1+0],d */
{ "ld",		0xc0002000, 0x01f80000, "[1+i],d", 0 },
{ "ld",		0xc0002000, 0x01f80000, "[i+1],d", 0 },
{ "ld",		0xc0000000, 0x01f82000, "[1+2],d", 0 },
{ "ldstuba",    0xc0d80000, 0x0100201f, "[1]A,d", 0 }, /* ldstuba [rs1+%g0],d */
{ "ldstuba",	0xc0d80000, 0x01002000, "[1+2]A,d", 0 },
{ "ldsha",      0xc0d00000, 0x0128201f, "[1]A,d", 0 }, /* ldsha [rs1+%g0],d */
{ "ldsha",	0xc0d00000, 0x01282000, "[1+2]A,d", 0 },
{ "ldsba",      0xc0c80000, 0x0130201f, "[1]A,d", 0 }, /* ldsba [rs1+%g0],d */
{ "ldsba",	0xc0c80000, 0x01302000, "[1+2]A,d", 0 },
{ "ldda",       0xc0980000, 0x0160201f, "[1]A,d", 0 }, /* ldda [rs1+%g0],d */
{ "ldda",	0xc0980000, 0x01602000, "[1+2]A,d", 0 },
{ "lduha",      0xc0900000, 0x0168201f, "[1]A,d", 0 }, /* lduha [rs1+%g0],d */
{ "lduha",	0xc0900000, 0x01682000, "[1+2]A,d", 0 },
{ "ldstub",     0xc0680000, 0x0190201f, "[1],d", 0 }, /* ldstub [rs1+%g0],d */
{ "ldstub",	0xc0682000, 0x01900000, "[1+i],d", 0 },
{ "ldstub",	0xc0682000, 0x01900000, "[i+1],d", 0 },
{ "ldstub",	0xc0680000, 0x01902000, "[1+2],d", 0 },
{ "lda",        0xc0800000, 0x0178201f, "[1]A,d", 0 }, /* lda [rs1+%g0],d */
{ "lda",	0xc0800000, 0x01782000, "[1+2]A,d", 0 },
{ "ldsh",       0xc0500000, 0x0000000d, "[1],d", 0 }, /* ldsh [rs1+%g0],d */
{ "ldsh",       0xc0502000, 0x01a81fff, "[1],d", 0 }, /* ldsh [rs1+0],d */
{ "ldsh",	0xc0502000, 0x01a80000, "[1+i],d", 0 },
{ "ldsh",	0xc0502000, 0x01a80000, "[i+1],d", 0 },
{ "ldsh",	0xc0500000, 0x01a82000, "[1+2],d", 0 },
{ "ldsb",       0xc0480000, 0x01b0201f, "[1],d", 0 }, /* ldsb [rs1+%g0],d */
{ "ldsb",	0xc0482000, 0x01b01fff, "[1],d", 0 }, /* ldsb [rs1+0],d */
{ "ldsb",	0xc0482000, 0x01b00000, "[1+i],d", 0 },
{ "ldsb",	0xc0482000, 0x01b00000, "[i+1],d", 0 },
{ "ldsb",	0xc0480000, 0x01b02000, "[1+2],d", 0 },
{ "ldub",       0xc0080000, 0x01f0201f, "[1],d", 0 }, /* ldub [rs1+%g0],d */
{ "ldub",       0xc0082000, 0x01f01fff, "[1],d", 0 }, /* ldub [rs1+0],d */
{ "ldub",	0xc0082000, 0x01f00000, "[1+i],d", 0 },
{ "ldub",	0xc0082000, 0x01f00000, "[i+1],d", 0 },
{ "ldub",	0xc0080000, 0x01f02000, "[1+2],d", 0 },
{ "lduba",      0xc0880000, 0x0170201f, "[1]A,d", 0 }, /* lduba [rs1+%g0],d */
{ "lduba",	0xc0880000, 0x01702000, "[1+2]A,d", 0 },
{ "lduh",	0xc0102000, 0x01e80000, "[1+i],d", 0 },
{ "lduh",	0xc0102000, 0x01e80000, "[i+1],d", 0 },
{ "lduh",	0xc0100000, 0x01e8201f, "[1],d", 0 }, /* lduh [rs1+%g0],d */
{ "lduh",	0xc0102000, 0x01e81fff, "[1],d", 0 }, /* lduh [rs1+0],d */
{ "lduh",	0xc0100000, 0x01e82000, "[1+2],d", 0 },

{ "st",	        0xc0200000, 0x01d8201f, "d,[1]", 0 }, /* st d,[rs1+%g0] */
{ "st",	        0xc0202000, 0x01d81fff, "d,[1]", 0 }, /* st d,[rs1+0] */
{ "st",		0xc0202000, 0x01d80000, "d,[1+i]", 0 },
{ "st",		0xc0202000, 0x01d80000, "d,[i+1]", 0 },
{ "st",		0xc0200000, 0x01d82000, "d,[1+2]", 0 },
{ "st",		0xc1200000, 0x00d8201f, "g,[1]", 0 }, /* st d[rs1+%g0] */
{ "st",		0xc1202000, 0x00d81fff, "g,[1]", 0 }, /* st d,[rs1+0] */
{ "st",		0xc1202000, 0x00d80000, "g,[1+i]", 0 },
{ "st",		0xc1202000, 0x00d80000, "g,[i+1]", 0 },
{ "st",		0xc1200000, 0x00d82000, "g,[1+2]", 0 },
{ "st",		0xc1280000, 0x00c0d01f, "F,[1]", 0 }, /* st d,[rs1+%g0] */
{ "st",		0xc1282000, 0x00c0dfff, "F,[1]", 0 }, /* st d,[rs1+0] */
{ "st",		0xc1282000, 0x00c0d000, "F,[1+i]", 0 },
{ "st",		0xc1282000, 0x00c0d000, "F,[i+1]", 0 },
{ "st",		0xc1280000, 0x00c0d000, "F,[1+2]", 0 },
{ "st",		0xc1a00000, 0x0058201f, "D,[1]", 0 }, /* st d,[rs1+%g0] */
{ "st",		0xc1a02000, 0x00581fff, "D,[1]", 0 }, /* st d,[rs1+0] */
{ "st",		0xc1a02000, 0x00580000, "D,[1+i]", 0 },
{ "st",		0xc1a02000, 0x00580000, "D,[i+1]", 0 },
{ "st",		0xc1a00000, 0x00582000, "D,[1+2]", 0 },
{ "st",		0xc1a80000, 0x0050201f, "C,[1]", 0 }, /* st d,[rs1+%g0] */
{ "st",		0xc1a82000, 0x00501fff, "C,[1]", 0 }, /* st d,[rs1+0] */
{ "st",		0xc1a82000, 0x00500000, "C,[1+i]", 0 },
{ "st",		0xc1a82000, 0x00500000, "C,[i+1]", 0 },
{ "st",		0xc1a80000, 0x00502000, "C,[1+2]", 0 },
{ "sta",        0xc0a00000, 0x0108201f, "d,[1]A", 0 }, /* sta d,[rs1+%g0] */
{ "sta",	0xc0a00000, 0x01082000, "d,[1+2]A", 0 },

{ "stb",        0xc0280000, 0x01d0201f, "d,[1]", 0 }, /* stb d,[rs1+%g0] */
{ "stb",	0xc0282000, 0x01d01fff, "d,[1]", 0 }, /* stb d,[rs1+0] */
{ "stb",	0xc0282000, 0x01d00000, "d,[1+i]", 0 },
{ "stb",	0xc0282000, 0x01d00000, "d,[i+1]", 0 },
{ "stb",	0xc0280000, 0x01d02000, "d,[1+2]", 0 },
{ "stba",       0xc0a80000, 0x01002000, "d,[1+2]A", 0 },
{ "stba",	0xc0a80000, 0x0100201f, "d,[1]A", 0 }, /* stba d,[rs1+%g0] */

{ "std",        0xc0380000, 0x01c0201f, "d,[1]", 0 }, /* std d,[rs1+%g0] */
{ "std",	0xc0382000, 0x01c01fff, "d,[1]", 0 }, /* std d,[rs1+0] */
{ "std",	0xc0382000, 0x01c00000, "d,[1+i]", 0 },
{ "std",	0xc0382000, 0x01c00000, "d,[i+1]", 0 },
{ "std",	0xc0380000, 0x01c02000, "d,[1+2]", 0 },
{ "std",	0xc1380000, 0x00c0201f, "g,[1]", 0 }, /* std d,[rs1+%g0] */
{ "std",        0xc1382000, 0x00c01fff, "g,[1]", 0 }, /* std d,[rs1+0] */
{ "std",	0xc1382000, 0x00c00000, "g,[1+i]", 0 },
{ "std",	0xc1382000, 0x00c00000, "g,[i+1]", 0 },
{ "std",	0xc1380000, 0x00c02000, "g,[1+2]", 0 },
{ "std",        0xc1300000, 0x00c8201f, "q,[1]", 0 }, /* std d,[rs1+%g0] */
{ "std",	0xc1302000, 0x00c81fff, "q,[1]", 0 }, /* std d,[rs1+0] */
{ "std",	0xc1302000, 0x00c80000, "q,[1+i]", 0 },
{ "std",	0xc1302000, 0x00c80000, "q,[i+1]", 0 },
{ "std",	0xc1300000, 0x00c82000, "q,[1+2]", 0 },
{ "std",	0xc1b80000, 0x0040201f, "D,[1]", 0 }, /* std d,[rs1+%g0] */
{ "std",	0xc1b82000, 0x00401fff, "D,[1]", 0 }, /* std d,[rs1+0] */
{ "std",	0xc1b82000, 0x00400000, "D,[1+i]", 0 },
{ "std",	0xc1b82000, 0x00400000, "D,[i+1]", 0 },
{ "std",	0xc1b80000, 0x00402000, "D,[1+2]", 0 },
{ "std",	0xc1b00000, 0x0048201f, "Q,[1]", 0 }, /* std d,[rs1+%g0] */
{ "std",	0xc1b02000, 0x00481fff, "Q,[1]", 0 }, /* std d,[rs1+0] */
{ "std",	0xc1b02000, 0x00480000, "Q,[1+i]", 0 },
{ "std",	0xc1b02000, 0x00480000, "Q,[i+1]", 0 },
{ "std",	0xc1b00000, 0x00482000, "Q,[1+2]", 0 },
{ "stda",       0xc0b80000, 0x01402000, "d,[1+2]A", 0 },
{ "stda",	0xc0b80000, 0x0140201f, "d,[1]A", 0 }, /* stda d,[rs1+%g0] */

{ "sth",        0xc0300000, 0x01c8201f, "d,[1]", 0 }, /* sth d,[rs1+%g0] */
{ "sth",	0xc0302000, 0x01c81fff, "d,[1]", 0 }, /* sth d,[rs1+0] */
{ "sth",	0xc0300000, 0x01c82000, "d,[1+2]", 0 },
{ "sth",	0xc0302000, 0x01c80000, "d,[1+i]", 0 },
{ "sth",	0xc0302000, 0x01c80000, "d,[i+1]", 0 },
{ "stha",       0xc0b00000, 0x0148201f, "d,[1]A", 0 }, /* stha d,[rs1+%g0] */
{ "stha",	0xc0b00000, 0x01482000, "d,[1+2]A", 0 },

{ "swap",       0xc0780000, 0x0180201f, "[1],d", 0 }, /* swap [rs1+%g0],d */
{ "swap",       0xc0782000, 0x01801fff, "[1],d", 0 }, /* swap [rs1+0],d */
{ "swap",       0xc0782000, 0x01800000, "[1+i],d", 0 },
{ "swap",       0xc0782000, 0x01800000, "[i+1],d", 0 },
{ "swap",       0xc0780000, 0x01802000, "[1+2],d", 0 },
{ "swapa",      0xc0f80000, 0x01002000, "[1+2]A,d", 0 },
{ "swapa",      0xc0f80000, 0x0100201f, "[1]A,d", 0 }, /* swapa [rs1+%g0],d */

{ "restore",    0x81e80000, 0x7e17e01f, "", 0 }, /* restore %g0,%g0,%g0 */
{ "restore",    0x81e82000, 0x7e14dfff, "", 0 }, /* restore %g0,0,%g0 */
{ "restore",	0x81e82000, 0x00000000, "1,i,d", 0 },
{ "restore",	0x81e80000, 0x00000000, "1,2,d", 0 },
{ "rett",       0x81c82000, 0x40300000, "1+i", 1 },
{ "rett",       0x81c82000, 0x40300000, "i+1", 1 },
{ "rett",	0x81c80000, 0x40302000, "1+2", 1 },
{ "rett",	0x81c82000, 0x40300000, "1", 1},
{ "save",       0x81e02000, 0x40180000, "1,i,d", 0 },
{ "save",	0x81e00000, 0x40180000, "1,2,d", 0 },

{ "ret",	0x81c7e008, 0x00001ff7, "", 1 }, /* jmpl %i7+8,%g0 */
{ "retl",       0x81c3e008, 0x00001ff7, "", 1 }, /* jmpl %o7+8,%g0 */

{ "jmpl",       0x81c00000, 0x4038201f, "1,d", 1 }, /* jmpl rs1+%g0,d */
{ "jmpl",	0x81c02000, 0x4037c000, "i,d", 1 }, /* jmpl %g0+i,d */
{ "jmpl",	0x81c02000, 0x40380000, "1+i,d", 1 },
{ "jmpl",	0x81c02000, 0x40380000, "i+1,d", 1 },
{ "jmpl",	0x81c00000, 0x40382000, "1+2,d", 1 },
{ "wr",         0x81982000, 0x40600000, "1,i,t", 0 },
{ "wr",         0x81980000, 0x40600000, "1,2,t", 0 },
{ "wr",         0x81902000, 0x40680000, "1,i,w", 0 },
{ "wr",         0x81900000, 0x40680000, "1,2,w", 0 },
{ "wr",         0x81882000, 0x40700000, "1,i,p", 0 },
{ "wr",         0x81880000, 0x40700000, "1,2,p", 0 },
{ "wr",         0x81802000, 0x40780000, "1,i,y", 0 },
{ "wr",         0x81800000, 0x40780000, "1,2,y", 0 },

{ "rd", 	0x81580000, 0x40a00000, "t,d", 0 },
{ "rd", 	0x81500000, 0x40a80000, "w,d", 0 },
{ "rd", 	0x81480000, 0x40b00000, "p,d", 0 },
{ "rd",         0x81400000, 0x40b80000, "y,d", 0 },

{ "sra",	0x81382000, 0x00000000, "1,i,d", 0 },
{ "sra",	0x81380000, 0x00000000, "1,2,d", 0 },
{ "srl",        0x81302000, 0x40c80000, "1,i,d", 0 },
{ "srl",	0x81300000, 0x40c80000, "1,2,d", 0 },
{ "sll",        0x81282000, 0x40d00000, "1,i,d", 0 },
{ "sll",	0x81280000, 0x40d00000, "1,2,d", 0 },

{ "mulscc",     0x81202000, 0x40d80000, "1,i,d", 0 },
{ "mulscc",	0x81200000, 0x40d80000, "1,2,d", 0 },

{ "clr",        0x80100000, 0x4e87e01f, "d", 0 }, /* or %g0,%g0,d */
{ "clr",        0x80102000, 0x41efdfff, "d", 0 }, /* or %g0,0,d   */

{ "orncc",      0x80b02000, 0x04048000, "1,i,d", 0 },
{ "orncc",      0x80b02000, 0x04048000, "i,1,d", 0 },
{ "orncc",	0x80b00000, 0x04048000, "1,2,d", 0 },

{ "tst",        0x80900000, 0x7f6fe000, "2", 0 }, /* orcc %g0, rs2, %g0 */
{ "tst",        0x80900000, 0x7f68201f, "1", 0 }, /* orcc rs1, %g0, %g0 */
{ "tst",        0x80902000, 0x7f681fff, "1", 0 }, /* orcc rs1, 0, %g0 */
  
{ "orcc",       0x80902000, 0x41680000, "1,i,d", 0 },
{ "orcc",	0x80902000, 0x41680000, "i,1,d", 0 },
{ "orcc",	0x80900000, 0x41680000, "1,2,d", 0 },
{ "orn",        0x80302000, 0x41c80000, "1,i,d", 0 },
{ "orn",	0x80302000, 0x41c80000, "i,1,d", 0 },
{ "orn",	0x80300000, 0x41c80000, "1,2,d", 0 },

{ "mov",        0x81800000, 0x4078201f, "1,y", 0 }, /* wr rs1,%g0,%y */
{ "mov",        0x81802000, 0x40781fff, "1,y", 0 }, /* wr rs1,0,%y */
{ "mov",        0x81400000, 0x40b80000, "y,d", 0 }, /* rd %y,d */
{ "mov",        0x81980000, 0x4060201f, "1,t", 0 }, /* wr rs1,%g0,%tbr */
{ "mov",        0x81982000, 0x40601fff, "1,t", 0 }, /* wr rs1,0,%tbr */
{ "mov",        0x81580000, 0x40a00000, "t,d", 0 }, /* rd %tbr,d */
{ "mov",        0x81900000, 0x4068201f, "1,w", 0 }, /* wr rs1,%g0,%wim */
{ "mov",        0x81902000, 0x40681fff, "1,w", 0 }, /* wr rs1,0,%wim */
{ "mov",        0x81500000, 0x40a80000, "w,d", 0 }, /* rd %wim,d */
{ "mov",        0x81880000, 0x4070201f, "1,p", 0 }, /* wr rs1,%g0,%psr */
{ "mov",        0x81882000, 0x40701fff, "1,p", 0 }, /* wr rs1,0,%psr */
{ "mov",        0x81480000, 0x40b00000, "p,d", 0 }, /* rd %psr,d */

{ "mov",        0x80102000, 0x41efc000, "i,d", 0 }, /* or %g0,i,d   */
{ "mov",        0x80100000, 0x41efe000, "2,d", 0 }, /* or %g0,rs2,d */

{ "or",	        0x80102000, 0x40800000, "1,i,d", 0 },
{ "or",		0x80102000, 0x40800000, "i,1,d", 0 },
{ "or",		0x80100000, 0x40800000, "1,2,d", 0 },

{ "bset",	0x80100000, 0x40800000, "2,r", 0 },	/* or rsd,rs2,rsd */
{ "bset",	0x80102000, 0x40800000, "i,r", 0 },	/* or rsd,i,rsd */

{ "andncc",     0x80a82000, 0x41500000, "1,i,d", 0 },
{ "andncc",	0x80a82000, 0x41500000, "i,1,d", 0 },
{ "andncc",	0x80a80000, 0x41500000, "1,2,d", 0 },
{ "andn",       0x80282000, 0x41d00000, "1,i,d", 0 },
{ "andn",	0x80282000, 0x41d00000, "i,1,d", 0 },
{ "andn",	0x80280000, 0x41d00000, "1,2,d", 0 },

{ "bclr",	0x80280000, 0x41d00000, "2,r", 0 },	/* andn rsd,rs2,rsd */
{ "bclr",	0x80282000, 0x41d00000, "i,r", 0 },	/* andn rsd,i,rsd */

{ "cmp",        0x80a02000, 0x7d580000, "1,i", 0 },     /* subcc rs1,i,%g0 */
{ "cmp",	0x80a00000, 0x7d580000, "1,2", 0 },     /* subcc rs1,rs2,%g0 */

{ "deccc",	0x80a02001, 0x41581ffe, "r", 0 },	/* subcc rs1,1,rsd */
{ "deccc",	0x80a02000, 0x41581fff, "i,r", 0 },	/* subcc rs1,i,rsd */
{ "dec",	0x80202001, 0x41d81ffe, "r", 0 },	/* sub rs1,1,rsd */
{ "dec",	0x80202000, 0x41d81fff, "i,r", 0 },	/* etc */

{ "neg",	0x80200000, 0x41d87800, "2,d", 0 },	/* sub %g0,rs2,rsd */
{ "neg",	0x80200000, 0x41d87800, "R", 0 },	/* same, but rsd=rs2 */

{ "subcc",      0x80a02000, 0x41580000, "1,i,d", 0 },
{ "subcc",	0x80a00000, 0x41580000, "1,2,d", 0 },
{ "sub",	0x80202000, 0x41d80000, "1,i,d", 0 },
{ "sub",	0x80200000, 0x41d80000, "1,2,d", 0 },
{ "subx",	0x80602000, 0x41980000, "1,i,d", 0 },
{ "subx",	0x80600000, 0x41980000, "1,2,d", 0 },
{ "subxcc",     0x80e02000, 0x41180000, "1,i,d", 0 },
{ "subxcc",	0x80e00000, 0x41180000, "1,2,d", 0 },

{ "andcc",      0x80882000, 0x41700000, "1,i,d", 0 },
{ "andcc",	0x80882000, 0x41700000, "i,1,d", 0 },
{ "andcc",	0x80880000, 0x41700000, "1,2,d", 0 },
{ "and",        0x80082000, 0x41f00000, "1,i,d", 0 },
{ "and",	0x80082000, 0x41f00000, "i,1,d", 0 },
{ "and",	0x80080000, 0x41f00000, "1,2,d", 0 },

{ "btst",	0x80880000, 0x41700000, "1,2", 0 },	/* andcc rs1,rs2,%g0 */
{ "btst",	0x80882000, 0x41700000, "i,1", 0 },	/* andcc rs1,i,%g0 */

{ "inccc",	0x80802001, 0x41781ffe, "r", 0 },	/* addcc rs1,1,rsd */
{ "inccc",	0x80802000, 0x41781fff, "i,r", 0 },	/* addcc rs1,i,rsd */
{ "inc",	0x80002001, 0x41f81ffe, "r", 0 },       /* add rs1,1,rsd */
{ "inc",	0x80002000, 0x41f81fff, "i,r", 0 },	/* add rs1,i,rsd */

{ "addxcc",     0x80c02000, 0x41380000, "1,i,d", 0 },
{ "addxcc",     0x80c02000, 0x41380000, "i,1,d", 0 },
{ "addxcc",     0x80c00000, 0x41380000, "1,2,d", 0 },
{ "addcc",      0x80802000, 0x41780000, "1,i,d", 0 },
{ "addcc",      0x80802000, 0x41780000, "i,1,d", 0 },
{ "addcc",      0x80800000, 0x41780000, "1,2,d", 0 },
{ "addx",       0x80402000, 0x41b80000, "1,i,d", 0 },
{ "addx",	0x80402000, 0x41b80000, "i,1,d", 0 },
{ "addx",	0x80400000, 0x41b80000, "1,2,d", 0 },
{ "add",        0x80002000, 0x41f80000, "1,i,d", 0 },
{ "add",	0x80002000, 0x41f80000, "i,1,d", 0 },
{ "add",	0x80000000, 0x41f80000, "1,2,d", 0 },

{ "call",       0x9fc00000, 0x4038201f, "1", 1 }, /* jmpl rs1+%g0, %o7 */
{ "call",	0x9fc00000, 0x4038201f, "1,#", 1 },
{ "call",	0x40000000, 0x80000000, "L", 1 },
{ "call",	0x40000000, 0x80000000, "L,#", 1 },

{ "bvc",        0x3e800000, 0xc1400000, ",al", 1 },
{ "bvc",	0x1e800000, 0xc1400000, "l", 1 },
{ "bvs",	0x2e800000, 0xc1400000, ",al", 1 },
{ "bvs",	0x0e800000, 0xc1400000, "l", 1 },
{ "bpos",	0x3c800000, 0xc1400000, ",al", 1 },
{ "bpos",	0x1c800000, 0xc1400000, "l", 1 },
{ "bneg",	0x2c800000, 0xc1400000, ",al", 1 },
{ "bneg",	0x0c800000, 0xc1400000, "l", 1 },
{ "bcc",	0x3a800000, 0xc1400000, ",al", 1 },
{ "bcc",	0x1a800000, 0xc1400000, "l", 1 },
{ "bcs",	0x2a800000, 0xc1400000, ",al", 1 },
{ "bcs",	0x0a800000, 0xc1400000, "l", 1 },
{ "blu",	0x2a800000, 0xc1400000, ",al", 1 },
{ "blu",	0x0a800000, 0xc1400000, "l", 1 }, /* same as bcs */
{ "bgeu",	0x3a800000, 0xc1400000, ",al", 1 },
{ "bgeu",	0x1a800000, 0xc1400000, "l", 1 }, /* same as bcc */
{ "bgu",	0x38800000, 0xc1400000, ",al", 1 },
{ "bgu",	0x18800000, 0xc1400000, "l", 1 },
{ "bleu",	0x28800000, 0xc1400000, ",al", 1 },
{ "bleu",	0x08800000, 0xc1400000, "l", 1 },
{ "bge",	0x36800000, 0xc1400000, ",al", 1 },
{ "bge",	0x16800000, 0xc1400000, "l", 1 },
{ "bl",		0x26800000, 0xc1400000, ",al", 1 },
{ "bl",		0x06800000, 0xc1400000, "l", 1 },
{ "bg",		0x34800000, 0xc1400000, ",al", 1 },
{ "bg",		0x14800000, 0xc1400000, "l", 1 },
{ "ble",	0x24800000, 0xc1400000, ",al", 1 },
{ "ble",	0x04800000, 0xc1400000, "l", 1 },
{ "be",		0x22800000, 0xc1400000, ",al", 1 },
{ "be",		0x02800000, 0xc1400000, "l", 1 },
{ "bz",		0x22800000, 0xc1400000, ",al", 1 },
{ "bz",		0x02800000, 0xc1400000, "l", 1 },
{ "bne",	0x32800000, 0xc1400000, ",al", 1 },
{ "bne",	0x12800000, 0xc1400000, "l", 1 },
{ "bnz",	0x32800000, 0xc1400000, ",al", 1 },
{ "bnz",	0x12800000, 0xc1400000, "l", 1 },
{ "b",		0x30800000, 0xc1400000, ",al", 1 },
{ "b",		0x10800000, 0xc1400000, "l", 1 },
{ "ba",		0x30800000, 0xc1400000, ",al", 1 },
{ "ba",		0x10800000, 0xc1400000, "l", 1 },
{ "bn", 	0x20800000, 0xc1400000, ",al", 1 },
{ "bn",		0x00800000, 0xc1400000, "l", 1 },

{ "jmp",        0x81c00000, 0x7e38201f, "1", 1 }, /* jmpl rs1+%g0,%g0 */
{ "jmp",        0x81c02000, 0x7e3fc000, "i", 1 }, /* jmpl %g0+i,%g0 */
{ "jmp",        0x81c00000, 0x7e382000, "1+2", 1 }, /* jmpl rs1+rs2,%g0 */
{ "jmp",        0x81c02000, 0x7e380000, "1+i", 1 }, /* jmpl rs1+i,%g0 */
{ "jmp",        0x81c02000, 0x7e380000, "i+1", 1 }, /* jmpl i+rs1,%g0 */

{ "nop",	0x01000000, 0xfe3fffff, "", 0 }, /* sethi 0, %g0 */

{ "set",        0x01000000, 0xc0c00000, "Sh,d", 0 },

{ "sethi",      0x01000000, 0xc0c00000, "h,d", 0 },

{ "taddcctv",   0x81102000, 0x40e00000, "1,i,d", 0 },
{ "taddcctv",   0x81100000, 0x40e00000, "1,2,d", 0 },
{ "taddcc",     0x81002000, 0x40f80000, "1,i,d", 0 },
{ "taddcc",     0x81000000, 0x40f80000, "1,2,d", 0 },

{ "tvc",	0x9fd02000, 0x402fc000, "i", 0 }, /* tvc %g0+i */
{ "tvc",        0x9fd02000, 0x40280000, "1+i", 0 },
{ "tvc",	0x9fd00000, 0x40282000, "1+2", 0 },
{ "tvc",        0x9fd00000, 0x4028201f, "1", 0 }, /* tvc rs1+%g0 */
{ "tpos",	0x9dd02000, 0x402fc000, "i", 0 }, /* tpos %g0+i */
{ "tpos",	0x9dd02000, 0x40280000, "1+i", 0 },
{ "tpos",	0x9dd00000, 0x40282000, "1+2", 0 },
{ "tpos",       0x9dd00000, 0x4028201f, "1", 0 }, /* tpos rs1+%g0 */
{ "tcc",        0x9bd02000, 0x402fc000, "i", 0 }, /* tcc %g0+i */
{ "tcc",	0x9bd02000, 0x40280000, "1+i", 0 },
{ "tcc",	0x9bd00000, 0x40282000, "1+2", 0 },
{ "tcc",        0x9bd00000, 0x4028201f, "1", 0 }, /* tcc rs1+%g0 */
{ "tgu",	0x99d02000, 0x402fc000, "i", 0 }, /* tgu %g0+i */
{ "tgu",	0x99d02000, 0x40280000, "1+i", 0 },
{ "tgu",	0x99d00000, 0x40282000, "1+2", 0 },
{ "tgu",        0x99d00000, 0x4028201f, "1", 0 }, /* tgu rs1+%g0 */
{ "tge",	0x97d02000, 0x402fc000, "i", 0 }, /* tge %g0+i */
{ "tge",	0x97d02000, 0x40280000, "1+i", 0 },
{ "tge",	0x97d00000, 0x40282000, "1+2", 0 },
{ "tge",        0x97d00000, 0x4028201f, "1", 0 }, /* tge rs1+%g0 */
{ "tg",		0x95d02000, 0x402fc000, "i", 0 }, /* tg %g0+i */
{ "tg",		0x95d02000, 0x40280000, "1+i", 0 },
{ "tg",		0x95d00000, 0x40282000, "1+2", 0 },
{ "tg",         0x95d00000, 0x4028201f, "1", 0 }, /* tg rs1+%g0 */
{ "tne",        0x93d02000, 0x402fc000, "i", 0 }, /* tne %g0+i */
{ "tne",	0x93d02000, 0x40280000, "1+i", 0 },
{ "tne",	0x93d00000, 0x40282000, "1+2", 0 },
{ "tne",        0x93d00000, 0x4028201f, "1", 0 }, /* tne rs1+%g0 */
{ "tnz",        0x93d02000, 0x402fc000, "i", 0 }, /* tne %g0+i */
{ "tnz",	0x93d02000, 0x40280000, "1+i", 0 },
{ "tnz",	0x93d00000, 0x40282000, "1+2", 0 },
{ "tnz",        0x93d00000, 0x4028201f, "1", 0 }, /* tne rs1+%g0 */
{ "tleu",       0x8bd02000, 0x502fc000, "i", 0 }, /* tleu %g0+i */
{ "tleu",	0x8bd02000, 0x50280000, "1+i", 0 },
{ "tleu",	0x8bd00000, 0x50282000, "1+2", 0 },
{ "tleu",       0x8bd00000, 0x5028201f, "1", 0 }, /* tleu rs1+%g0 */
{ "ta",	        0x91d02000, 0x402fc000, "i", 0 }, /* ta %g0+i */
{ "ta",		0x91d02000, 0x402d0000, "1+i", 0 },
{ "ta",		0x91d00000, 0x40282000, "1+2", 0 },
{ "ta",         0x91d00000, 0x4028201f, "1", 0 }, /* ta rs1+%g0 */
{ "t",	        0x91d02000, 0x402fc000, "i", 0 }, /* ta %g0+i */
{ "t",		0x91d02000, 0x402d0000, "1+i", 0 },
{ "t",		0x91d00000, 0x40282000, "1+2", 0 },
{ "t",		0x91d00000, 0x4028201f, "1", 0 }, /* ta rs1+%g0 */
{ "tvs",	0x8fd02000, 0x502fc000, "i", 0 }, /* tvs %g0+i */
{ "tvs",	0x8fd02000, 0x50280000, "1+i", 0 },
{ "tvs",	0x8fd00000, 0x50282000, "1+2", 0 },
{ "tvs",        0x8fd00000, 0x5028201f, "1", 0 }, /* tvs rs1+%g0 */
{ "tneg",	0x8dd02000, 0x502fc000, "i", 0 }, /* tneg %g0+i */
{ "tneg",	0x8dd02000, 0x50280000, "1+i", 0 },
{ "tneg",	0x8dd00000, 0x50282000, "1+2", 0 },
{ "tneg",	0x8dd00000, 0x5028201f, "1", 0 }, /* tneg rs1+%g0 */
{ "tcs",        0x8bd02000, 0x502fc000, "i", 0 }, /* tcs %g0+i */
{ "tcs",	0x8bd02000, 0x50280000, "1+i", 0 },
{ "tcs",	0x8bd00000, 0x50282000, "1+2", 0 },
{ "tcs",	0x8bd00000, 0x5028201f, "1", 0 }, /* tcs rs1+%g0 */
{ "tl",		0x87d02000, 0x502fc000, "i", 0 }, /* tl %g0+i */
{ "tl",		0x87d02000, 0x50280000, "1+i", 0 },
{ "tl",		0x87d00000, 0x50282000, "1+2", 0 },
{ "tl",		0x87d00000, 0x5028201f, "1", 0 }, /* tl rs1+%g0 */
{ "tle",	0x85d02000, 0x502fc000, "i", 0 }, /* tle %g0+i */
{ "tle",	0x85d02000, 0x50280000, "1+i", 0 },
{ "tle",	0x85d00000, 0x50282000, "1+2", 0 },
{ "tle",	0x85d00000, 0x5028201f, "1", 0 }, /* tle rs1+%g0 */
{ "te",	        0x83d02000, 0x502fc000, "i", 0 }, /* te %g0+i */
{ "te",		0x83d02000, 0x50280000, "1+i", 0 },
{ "te",		0x83d00000, 0x50282000, "1+2", 0 },
{ "te",         0x83d00000, 0x5028201f, "1", 0 }, /* te rs1+%g0 */
{ "tz",	        0x83d02000, 0x502fc000, "i", 0 }, /* te %g0+i */
{ "tz",		0x83d02000, 0x50280000, "1+i", 0 },
{ "tz",		0x83d00000, 0x50282000, "1+2", 0 },
{ "tz",         0x83d00000, 0x5028201f, "1", 0 }, /* te rs1+%g0 */
{ "tn",		0x81d02000, 0x502fc000, "i", 0 }, /* tn %g0+i */
{ "tn",	        0x81d02000, 0x50280000, "1+i", 0 },
{ "tn",		0x81d00000, 0x50282000, "1+2", 0 },
{ "tn",         0x81d00000, 0x5028201f, "1", 0 }, /* tn rs1+%g0 */

{ "tsubcc",     0x81080000, 0x40f00000, "1,2,d", 0 },
{ "tsubcc",     0x81082000, 0x40f00000, "1,i,d", 0 },
{ "tsubcctv",   0x80580000, 0x40a00000, "1,2,d", 0 },
{ "tsubcctv",   0x80582000, 0x40a00000, "1,i,d", 0 },

{ "unimp",      0x00000000, 0x00000000, "l", 0 },

{ "iflush",     0x81d80000, 0x40202000, "1+2", 0 },
{ "iflush",     0x81d82000, 0x40200000, "1+i", 0 },

{ "xnorcc",     0x80b80000, 0x41400000, "1,2,d", 0 },
{ "xnorcc",	0x80b82000, 0x41400000, "1,i,d", 0 },
{ "xnorcc",	0x80b82000, 0x41400000, "i,1,d", 0 },
{ "xorcc",      0x80980000, 0x41600000, "1,2,d", 0 },
{ "xorcc",	0x80982000, 0x41600000, "1,i,d", 0 },
{ "xorcc",	0x80982000, 0x41600000, "i,1,d", 0 },
{ "xnor",       0x80380000, 0x41c00000, "1,2,d", 0 },
{ "xnor",	0x80382000, 0x41c00000, "1,i,d", 0 },
{ "xnor",	0x80382000, 0x41c00000, "i,1,d", 0 },
{ "xor",        0x80180000, 0x41e00000, "1,2,d", 0 },
{ "xor",	0x80182000, 0x41e00000, "1,i,d", 0 },
{ "xor",	0x80182000, 0x41e00000, "i,1,d", 0 },

{ "btog",	0x80180000, 0x41e00000, "2,r", 0 },	/* xor rsd,rs2,rsd */
{ "btog",	0x80182000, 0x41e00000, "i,r", 0 },	/* xor rsd,i,rsd */

{ "not",	0x80380000, 0x41c00000, "1,d", 0 },	/* xnor rs1,%g0,rsd */
{ "not",	0x80380000, 0x41c00000, "r", 0 },	/* xnor rs1,%g0,rsd */

{ "fpop1",      0x81a00000, 0x40580000, "[1+2],d", 0 },
{ "fpop2",      0x81a80000, 0x40500000, "[1+2],d", 0 },

{ "fb",         0x31800000, 0xc0400000, ",al", 1 },
{ "fb",         0x11800000, 0xc0400000, "l", 1 },
{ "fba",        0x31800000, 0xc0400000, ",al", 1 },
{ "fba",        0x11800000, 0xc0400000, "l", 1 },
{ "fbn",        0x21800000, 0xc0400000, ",al", 1 },
{ "fbn",        0x01800000, 0xc0400000, "l", 1 },
{ "fbu",        0x2f800000, 0xc0400000, ",al", 1 },
{ "fbu",        0x0f800000, 0xc0400000, "l", 1 },
{ "fbg",        0x2d800000, 0xc0400000, ",al", 1 },
{ "fbg",        0x0d800000, 0xc0400000, "l", 1 },
{ "fbug",       0x2b800000, 0xc0400000, ",al", 1 },
{ "fbug",       0x0b800000, 0xc0400000, "l", 1 },
{ "fbl",        0x29800000, 0xc0400000, ",al", 1 },
{ "fbl",        0x09800000, 0xc0400000, "l", 1 },
{ "fbul",       0x27800000, 0xc0400000, ",al", 1 },
{ "fbul",       0x07800000, 0xc0400000, "l", 1 },
{ "fblg",       0x25800000, 0xc0400000, ",al", 1 },
{ "fblg",       0x05800000, 0xc0400000, "l", 1 },
{ "fbne",       0x23800000, 0xc0400000, ",al", 1 },
{ "fbne",       0x03800000, 0xc0400000, "l", 1 },
{ "fbe",        0x33800000, 0xc0400000, ",al", 1 },
{ "fbe",        0x13800000, 0xc0400000, "l", 1 },
{ "fbue",       0x35800000, 0xc0400000, ",al", 1 },
{ "fbue",       0x15800000, 0xc0400000, "l", 1 },
{ "fbge",       0x37800000, 0xc0400000, ",al", 1 },
{ "fbge",       0x17800000, 0xc0400000, "l", 1 },
{ "fbuge",      0x39800000, 0xc0400000, ",al", 1 },
{ "fbuge",      0x19800000, 0xc0400000, "l", 1 },
{ "fble",       0x3b800000, 0xc0400000, ",al", 1 },
{ "fble",       0x1b800000, 0xc0400000, "l", 1 },
{ "fbule",      0x3d800000, 0xc0400000, ",al", 1 },
{ "fbule",      0x1d800000, 0xc0400000, "l", 1 },
{ "fbo",        0x3f800000, 0xc0400000, ",al", 1 },
{ "fbo",        0x1f800000, 0xc0400000, "l", 1 },

{ "cba",        0x31c00000, 0xce000000, ",al", 1 },
{ "cba",        0x11c00000, 0xce000000, "l", 1 },
{ "cbn",        0x21c00000, 0xde000000, ",al", 1 },
{ "cbn",        0x01c00000, 0xde000000, "l", 1 },
{ "cb3",        0x2fc00000, 0xc0000000, ",al", 1 },
{ "cb3",        0x0fc00000, 0xc0000000, "l", 1 },
{ "cb2",        0x2dc00000, 0xc0000000, ",al", 1 },
{ "cb2",        0x0dc00000, 0xc0000000, "l", 1 },
{ "cb23",       0x2bc00000, 0xc0000000, ",al", 1 },
{ "cb23",       0x0bc00000, 0xc0000000, "l", 1 },
{ "cb1",        0x29c00000, 0xc0000000, ",al", 1 },
{ "cb1",        0x09c00000, 0xc0000000, "l", 1 },
{ "cb13",       0x27c00000, 0xc0000000, ",al", 1 },
{ "cb13",       0x07c00000, 0xc0000000, "l", 1 },
{ "cb12",       0x25c00000, 0xc0000000, ",al", 1 },
{ "cb12",       0x05c00000, 0xc0000000, "l", 1 },
{ "cb123",      0x23c00000, 0xc0000000, ",al", 1 },
{ "cb123",      0x03c00000, 0xc0000000, "l", 1 },
{ "cb0",        0x33c00000, 0xc0000000, ",al", 1 },
{ "cb0",        0x13c00000, 0xc0000000, "l", 1 },
{ "cb03",       0x35c00000, 0xc0000000, ",al", 1 },
{ "cb03",       0x15c00000, 0xc0000000, "l", 1 },
{ "cb02",       0x37c00000, 0xc0000000, ",al", 1 },
{ "cb02",       0x17c00000, 0xc0000000, "l", 1 },
{ "cb023",      0x39c00000, 0xc0000000, ",al", 1 },
{ "cb023",      0x19c00000, 0xc0000000, "l", 1 },
{ "cb013",      0x3dc00000, 0xc0000000, ",al", 1 },
{ "cb013",      0x1dc00000, 0xc0000000, "l", 1 },
{ "cb012",      0x3fc00000, 0xc0000000, ",al", 1 },
{ "cb012",      0x1fc00000, 0xc0000000, "l", 1 },

{ "fstoi",      0x81a01a20, 0x400025c0, "f,g", 0 },
{ "fdtoi",      0x81a01a40, 0x400025a0, "f,g", 0 },
{ "fxtoi",      0x81a01a60, 0x40002580, "f,g", 0 },

{ "fitox",      0x81a01980, 0x40002660, "f,g", 0 },
{ "fitod",      0x81a01900, 0x400026e0, "f,g", 0 },
{ "fitos",      0x81a01880, 0x40002660, "f,g", 0 },

{ "fstod",      0x81a01920, 0x400026c0, "f,g", 0 },
{ "fstox",      0x81a019a0, 0x40002640, "f,g", 0 },
{ "fdtos",      0x81a018c0, 0x40002720, "f,g", 0 },
{ "fdtox",      0x81a019c0, 0x40002620, "f,g", 0 },
{ "fxtos",      0x81a018e0, 0x40002700, "f,g", 0 },
{ "fxtod",      0x81a01960, 0x40002680, "f,g", 0 },

{ "fdivx",      0x81a009e0, 0x40083600, "e,f,g", 0 },
{ "fdivd",      0x81a009c0, 0x40003620, "e,f,g", 0 },
{ "fdivs",      0x81a009a0, 0x40003640, "e,f,g", 0 },
{ "fmuls",      0x81a00920, 0x400036c0, "e,f,g", 0 },
{ "fmuld",      0x81a00940, 0x400036a0, "e,f,g", 0 },
{ "fmulx",      0x81a00960, 0x40003680, "e,f,g", 0 },

{ "fsqrts",     0x81a00520, 0x40003ac0, "f,g", 0 },
{ "fsqrtd",     0x81a00540, 0x40003aa8, "f,g", 0 },
{ "fsqrtx",     0x81a00560, 0x40003a80, "f,g", 0 },

{ "fabss",      0x81a00120, 0x40003ec0, "f,g", 0 },
{ "fnegs",      0x81a000a0, 0x40003f40, "f,g", 0 },
{ "fmovs",      0x81a00020, 0x40003fc0, "f,g", 0 },

{ "fsubx",      0x81a008e0, 0x40003700, "e,f,g", 0 },
{ "fsubd",      0x81a008c0, 0x40003720, "e,f,g", 0 },
{ "fsubs",      0x81a008a0, 0x40003740, "e,f,g", 0 },
{ "faddx",      0x81a00860, 0x40003780, "e,f,g", 0 },
{ "faddd",      0x81a00840, 0x400037a0, "e,f,g", 0 },
{ "fadds",      0x81a00820, 0x400037c0, "e,f,g", 0 },

{ "fcmpex",     0x81a80ae0, 0x40003500, "e,f", 0 },
{ "fcmped",     0x81a80ac0, 0x40003520, "e,f", 0 },
{ "fcmpes",     0x81a80aa0, 0x40003540, "e,f", 0 },
{ "fcmpx",      0x81a80a60, 0x40003580, "e,f", 0 },
{ "fcmpd",      0x81a80a40, 0x400035a0, "e,f", 0 },
{ "fcmps",      0x81a80a20, 0x400035c0, "e,f", 0 },

{ "cpop1",      0x81b00000, 0x40480000, "[1+2],d", 0 },
{ "cpop2",      0x81b80000, 0x40400000, "[1+2],d", 0 },
};

#define NUMOPCODES ((sizeof sparc_opcodes)/(sizeof sparc_opcodes[0]))

