
/* Definitions for opcode table for the sparc.
	Copyright 1989, 1991, 1992 Free Software Foundation, Inc.

This file is part of GAS, the GNU Assembler, GDB, the GNU debugger, and
the GNU Binutils.


GAS/GDB is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GAS/GDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GAS or GDB; see the file COPYING.	If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.	*/

/* The SPARC opcode table (and other related data) is defined in
   the BFD library in opc-sparc.c.  If you change anything here, make
   sure you fix up that file, and vice versa.  */

 /* FIXME-someday: perhaps the ,a's and such should be embedded in the
    instruction's name rather than the args.  This would make gas faster, pinsn
    slower, but would mess up some macros a bit.  xoxorich. */

#if !defined(__STDC__) && !defined(const)
#define const
#endif

#define sparc_architecture	bfd_sparc_architecture
#define architecture_pname	bfd_sparc_architecture_pname
#define sparc_opcode		bfd_sparc_opcode
#define sparc_opcodes		bfd_sparc_opcodes

/*
 * Structure of an opcode table entry.
 * This enumerator must parallel the architecture_pname array
 * in bfd/opc-sparc.c.
 */
enum sparc_architecture {
	v6 = 0,
	v7,
	v8,
	sparclite,
};

extern const char *architecture_pname[];



struct sparc_opcode {
	const char *name;
	unsigned long match;	/* Bits that must be set. */
	unsigned long lose;	/* Bits that must not be set. */
	const char *args;
 /* This was called "delayed" in versions before the flags. */
	char flags;
	enum sparc_architecture architecture;
};

#define	F_DELAYED	1	/* Delayed branch */
#define	F_ALIAS		2	/* Alias for a "real" instruction */

/*

All sparc opcodes are 32 bits, except for the `set' instruction (really a
macro), which is 64 bits. It is handled as a special case.

The match component is a mask saying which bits must match a particular
opcode in order for an instruction to be an instance of that opcode.

The args component is a string containing one character for each operand of the
instruction.

Kinds of operands:
	#	Number used by optimizer.	It is ignored.
	1	rs1 register.
	2	rs2 register.
	d	rd register.
	e	frs1 floating point register.
	v	frs1 floating point register (double/even).
	V	frs1 floating point register (quad/multiple of 4).
	f	frs2 floating point register.
	B	frs2 floating point register (double/even).
	R	frs2 floating point register (quad/multiple of 4).
	g	frsd floating point register.
	H	frsd floating point register (double/even).
	J	frsd floating point register (quad/multiple of 4).
	b	crs1 coprocessor register
	c	crs2 coprocessor register
	D	crsd coprocessor register
	m	alternate space register (asr) in rd
	M	alternate space register (asr) in rs1
	h	22 high bits.
	i	13 bit Immediate.
	n	22 bit immediate.
	l	22 bit PC relative immediate.
	L	30 bit PC relative immediate.
	a	Annul.	The annul bit is set.
	A	Alternate address space. Stored as 8 bits.
	C	Coprocessor state register.
	F	floating point state register.
	p	Processor state register.
	q	Floating point queue.
	r	Single register that is both rs1 and rsd.
	Q	Coprocessor queue.
	S	Special case.
	t	Trap base register.
	w	Window invalid mask register.
	y	Y register.

The following chars are unused: (note: ,[] are used as punctuation)
[uxOUXY3450]

*/

#define OP2(x)		(((x)&0x7) << 22) /* op2 field of format2 insns */
#define OP3(x)		(((x)&0x3f) << 19) /* op3 field of format3 insns */
#define OP(x)		((unsigned)((x)&0x3) << 30) /* op field of all insns */
#define OPF(x)		(((x)&0x1ff) << 5) /* opf field of float insns */
#define F3F(x, y, z)	(OP(x) | OP3(y) | OPF(z)) /* format3 float insns */
#define F3I(x)		(((x)&0x1) << 13) /* immediate field of format 3 insns */
#define F2(x, y)	(OP(x) | OP2(y)) /* format 2 insns */
#define F3(x, y, z)	(OP(x) | OP3(y) | F3I(z)) /* format3 insns */
#define F1(x)		(OP(x))
#define DISP30(x)	((x)&0x3fffffff)
#define ASI(x)		(((x)&0xff) << 5) /* asi field of format3 insns */
#define RS2(x)		((x)&0x1f) /* rs2 field */
#define SIMM13(x)	((x)&0x1fff) /* simm13 field */
#define RD(x)		(((x)&0x1f) << 25) /* destination register field */
#define RS1(x)		(((x)&0x1f) << 14) /* rs1 field */
#define ASI_RS2(x)	(SIMM13(x))

#define ANNUL	(1<<29)
#define	IMMED	F3I(1)
#define RD_G0	RD(~0)
#define	RS1_G0	RS1(~0)
#define	RS2_G0	RS2(~0)

extern struct sparc_opcode sparc_opcodes[];
extern const int bfd_sparc_num_opcodes;

#define NUMOPCODES bfd_sparc_num_opcodes

/*
 * Local Variables:
 * fill-column: 131
 * comment-column: 0
 * End:
 */

/* end of sparc.h */
