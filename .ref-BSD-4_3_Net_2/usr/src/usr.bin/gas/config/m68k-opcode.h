/* Opcode table for m68000/m68020 and m68881.
   Copyright (C) 1989, Free Software Foundation.

This file is part of GDB, the GNU Debugger and GAS, the GNU Assembler.

Both GDB and GAS are free software; you can redistribute and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GDB and GAS are distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GDB or GAS; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */
   

struct m68k_opcode
{
  char *name;
  unsigned long opcode;
  unsigned long  match;
  char *args;
};

/* We store four bytes of opcode for all opcodes because that
   is the most any of them need.  The actual length of an instruction
   is always at least 2 bytes, and is as much longer as necessary to
   hold the operands it has.

   The match component is a mask saying which bits must match
   particular opcode in order for an instruction to be an instance
   of that opcode.

   The args component is a string containing two characters
   for each operand of the instruction.  The first specifies
   the kind of operand; the second, the place it is stored.  */

/* Kinds of operands:
   D  data register only.  Stored as 3 bits.
   A  address register only.  Stored as 3 bits.
   R  either kind of register.  Stored as 4 bits.
   F  floating point coprocessor register only.   Stored as 3 bits.
   O  an offset (or width): immediate data 0-31 or data register.
      Stored as 6 bits in special format for BF... insns.
   +  autoincrement only.  Stored as 3 bits (number of the address register).
   -  autodecrement only.  Stored as 3 bits (number of the address register).
   Q  quick immediate data.  Stored as 3 bits.
      This matches an immediate operand only when value is in range 1 .. 8.
   M  moveq immediate data.  Stored as 8 bits.
      This matches an immediate operand only when value is in range -128..127
   T  trap vector immediate data.  Stored as 4 bits.

   k  K-factor for fmove.p instruction.   Stored as a 7-bit constant or
      a three bit register offset, depending on the field type.

   #  immediate data.  Stored in special places (b, w or l)
      which say how many bits to store.
   ^  immediate data for floating point instructions.   Special places
      are offset by 2 bytes from '#'...
   B  pc-relative address, converted to an offset
      that is treated as immediate data.
   d  displacement and register.  Stores the register as 3 bits
      and stores the displacement in the entire second word.

   C  the CCR.  No need to store it; this is just for filtering validity.
   S  the SR.  No need to store, just as with CCR.
   U  the USP.  No need to store, just as with CCR.

   I  Coprocessor ID.   Not printed if 1.   The Coprocessor ID is always
      extracted from the 'd' field of word one, which means that an extended
      coprocessor opcode can be skipped using the 'i' place, if needed.

   s  System Control register for the floating point coprocessor.
   S  List of system control registers for floating point coprocessor.

   J  Misc register for movec instruction, stored in 'j' format.
	Possible values:
	000	SFC	Source Function Code reg
	001	DFC	Data Function Code reg
	002	CACR	Cache Control Register
	800	USP	User Stack Pointer
	801	VBR	Vector Base reg
	802	CAAR	Cache Address Register
	803	MSP	Master Stack Pointer
	804	ISP	Interrupt Stack Pointer

    L  Register list of the type d0-d7/a0-a7 etc.
       (New!  Improved!  Can also hold fp0-fp7, as well!)
       The assembler tries to see if the registers match the insn by
       looking at where the insn wants them stored.

    l  Register list like L, but with all the bits reversed.
       Used for going the other way. . .

 They are all stored as 6 bits using an address mode and a register number;
 they differ in which addressing modes they match.

   *  all					(modes 0-6,7.*)
   ~  alterable memory				(modes 2-6,7.0,7.1)(not 0,1,7.~)
   %  alterable					(modes 0-6,7.0,7.1)(not 7.~)
   ;  data					(modes 0,2-6,7.*)(not 1)
   @  data, but not immediate			(modes 0,2-6,7.? ? ?)(not 1,7.?)  This may really be ;, the 68020 book says it is
   !  control					(modes 2,5,6,7.*-)(not 0,1,3,4,7.4)
   &  alterable control				(modes 2,5,6,7.0,7.1)(not 0,1,7.? ? ?)
   $  alterable data				(modes 0,2-6,7.0,7.1)(not 1,7.~)
   ?  alterable control, or data register	(modes 0,2,5,6,7.0,7.1)(not 1,3,4,7.~)
   /  control, or data register			(modes 0,2,5,6,7.0,7.1,7.2,7.3)(not 1,3,4,7.4)
*/

/* JF: for the 68851 */
/*
   I didn't use much imagination in choosing the 
   following codes, so many of them aren't very
   mnemonic. -rab

   P  pmmu register
	Possible values:
	000	TC	Translation Control reg
	100	CAL	Current Access Level
	101	VAL	Validate Access Level
	110	SCC	Stack Change Control
	111	AC	Access Control

   W  wide pmmu registers
	Possible values:
	001	DRP	Dma Root Pointer
	010	SRP	Supervisor Root Pointer
	011	CRP	Cpu Root Pointer

   f	function code register
	0	SFC
	1	DFC

   V	VAL register only

   X	BADx, BACx
	100	BAD	Breakpoint Acknowledge Data
	101	BAC	Breakpoint Acknowledge Control

   Y	PSR
   Z	PCSR

   |	memory 		(modes 2-6, 7.*)

*/

/* Places to put an operand, for non-general operands:
   s  source, low bits of first word.
   d  dest, shifted 9 in first word
   1  second word, shifted 12
   2  second word, shifted 6
   3  second word, shifted 0
   4  third word, shifted 12
   5  third word, shifted 6
   6  third word, shifted 0
   7  second word, shifted 7
   8  second word, shifted 10
   D  store in both place 1 and place 3; for divul and divsl.
   b  second word, low byte
   w  second word (entire)
   l  second and third word (entire)
   g  branch offset for bra and similar instructions.
      The place to store depends on the magnitude of offset.
   t  store in both place 7 and place 8; for floating point operations
   c  branch offset for cpBcc operations.
      The place to store is word two if bit six of word one is zero,
      and words two and three if bit six of word one is one.
   i  Increment by two, to skip over coprocessor extended operands.   Only
      works with the 'I' format.
   k  Dynamic K-factor field.   Bits 6-4 of word 2, used as a register number.
      Also used for dynamic fmovem instruction.
   C  floating point coprocessor constant - 7 bits.  Also used for static
      K-factors...
   j  Movec register #, stored in 12 low bits of second word.

 Places to put operand, for general operands:
   d  destination, shifted 6 bits in first word
   b  source, at low bit of first word, and immediate uses one byte
   w  source, at low bit of first word, and immediate uses two bytes
   l  source, at low bit of first word, and immediate uses four bytes
   s  source, at low bit of first word.
      Used sometimes in contexts where immediate is not allowed anyway.
   f  single precision float, low bit of 1st word, immediate uses 4 bytes
   F  double precision float, low bit of 1st word, immediate uses 8 bytes
   x  extended precision float, low bit of 1st word, immediate uses 12 bytes
   p  packed float, low bit of 1st word, immediate uses 12 bytes
*/

#define one(x) ((x) << 16)
#define two(x, y) (((x) << 16) + y)

/*
	*** DANGER WILL ROBINSON ***

   The assembler requires that all instances of the same mnemonic must be
   consecutive.  If they aren't, the assembler will bomb at runtime
 */
struct m68k_opcode m68k_opcodes[] =
{
{"abcd",	one(0140400),		one(0170770),		"DsDd"},
{"abcd",	one(0140410),		one(0170770),		"-s-d"},

		/* Add instructions */
{"addal",	one(0150700),		one(0170700),		"*lAd"},
{"addaw",	one(0150300),		one(0170700),		"*wAd"},
{"addib",	one(0003000),		one(0177700),		"#b$b"},
{"addil",	one(0003200),		one(0177700),		"#l$l"},
{"addiw",	one(0003100),		one(0177700),		"#w$w"},
{"addqb",	one(0050000),		one(0170700),		"Qd$b"},
{"addql",	one(0050200),		one(0170700),		"Qd%l"},
{"addqw",	one(0050100),		one(0170700),		"Qd%w"},

{"addb",	one(0050000),		one(0170700),		"Qd$b"},	/* addq written as add */
{"addb",	one(0003000),		one(0177700),		"#b$b"},	/* addi written as add */
{"addb",	one(0150000),		one(0170700),		";bDd"},	/* addb <ea>,	Dd */
{"addb",	one(0150400),		one(0170700),		"Dd~b"},	/* addb Dd,	<ea> */

{"addw",	one(0050100),		one(0170700),		"Qd%w"},	/* addq written as add */
{"addw",	one(0003100),		one(0177700),		"#w$w"},	/* addi written as add */
{"addw",	one(0150300),		one(0170700),		"*wAd"},	/* adda written as add */
{"addw",	one(0150100),		one(0170700),		"*wDd"},	/* addw <ea>,	Dd */
{"addw",	one(0150500),		one(0170700),		"Dd~w"},	/* addw Dd,	<ea> */

{"addl",	one(0050200),		one(0170700),		"Qd%l"},	/* addq written as add */
{"addl",	one(0003200),		one(0177700),		"#l$l"},	/* addi written as add */
{"addl",	one(0150700),		one(0170700),		"*lAd"},	/* adda written as add */
{"addl",	one(0150200),		one(0170700),		"*lDd"},	/* addl <ea>,	Dd */
{"addl",	one(0150600),		one(0170700),		"Dd~l"},	/* addl Dd,	<ea> */

{"addxb",	one(0150400),		one(0170770),		"DsDd"},
{"addxb",	one(0150410),		one(0170770),		"-s-d"},
{"addxl",	one(0150600),		one(0170770),		"DsDd"},
{"addxl",	one(0150610),		one(0170770),		"-s-d"},
{"addxw",	one(0150500),		one(0170770),		"DsDd"},
{"addxw",	one(0150510),		one(0170770),		"-s-d"},

{"andib",	one(0001000),		one(0177700),		"#b$b"},
{"andib",	one(0001074),		one(0177777),		"#bCb"},	/* andi to ccr */
{"andiw",	one(0001100),		one(0177700),		"#w$w"},
{"andiw",	one(0001174),		one(0177777),		"#wSw"},	/* andi to sr */
{"andil",	one(0001200),		one(0177700),		"#l$l"},

{"andb",	one(0001000),		one(0177700),		"#b$b"},	/* andi written as or */
{"andb",	one(0001074),		one(0177777),		"#bCb"},	/* andi to ccr */
{"andb",	one(0140000),		one(0170700),		";bDd"},	/* memory to register */
{"andb",	one(0140400),		one(0170700),		"Dd~b"},	/* register to memory */
{"andw",	one(0001100),		one(0177700),		"#w$w"},	/* andi written as or */
{"andw",	one(0001174),		one(0177777),		"#wSw"},	/* andi to sr */
{"andw",	one(0140100),		one(0170700),		";wDd"},	/* memory to register */
{"andw",	one(0140500),		one(0170700),		"Dd~w"},	/* register to memory */
{"andl",	one(0001200),		one(0177700),		"#l$l"},	/* andi written as or */
{"andl",	one(0140200),		one(0170700),		";lDd"},	/* memory to register */
{"andl",	one(0140600),		one(0170700),		"Dd~l"},	/* register to memory */

{"aslb",	one(0160400),		one(0170770),		"QdDs"},
{"aslb",	one(0160440),		one(0170770),		"DdDs"},
{"asll",	one(0160600),		one(0170770),		"QdDs"},
{"asll",	one(0160640),		one(0170770),		"DdDs"},
{"aslw",	one(0160500),		one(0170770),		"QdDs"},
{"aslw",	one(0160540),		one(0170770),		"DdDs"},
{"aslw",	one(0160700),		one(0177700),		"~s"},	/* Shift memory */
{"asrb",	one(0160000),		one(0170770),		"QdDs"},
{"asrb",	one(0160040),		one(0170770),		"DdDs"},
{"asrl",	one(0160200),		one(0170770),		"QdDs"},
{"asrl",	one(0160240),		one(0170770),		"DdDs"},
{"asrw",	one(0160100),		one(0170770),		"QdDs"},
{"asrw",	one(0160140),		one(0170770),		"DdDs"},
{"asrw",	one(0160300),		one(0177700),		"~s"},	/* Shift memory */

{"bhi",		one(0061000),		one(0177400),		"Bg"},
{"bls",		one(0061400),		one(0177400),		"Bg"},
{"bcc",		one(0062000),		one(0177400),		"Bg"},
{"bcs",		one(0062400),		one(0177400),		"Bg"},
{"bne",		one(0063000),		one(0177400),		"Bg"},
{"beq",		one(0063400),		one(0177400),		"Bg"},
{"bvc",		one(0064000),		one(0177400),		"Bg"},
{"bvs",		one(0064400),		one(0177400),		"Bg"},
{"bpl",		one(0065000),		one(0177400),		"Bg"},
{"bmi",		one(0065400),		one(0177400),		"Bg"},
{"bge",		one(0066000),		one(0177400),		"Bg"},
{"blt",		one(0066400),		one(0177400),		"Bg"},
{"bgt",		one(0067000),		one(0177400),		"Bg"},
{"ble",		one(0067400),		one(0177400),		"Bg"},

{"bchg",	one(0000500),		one(0170700),		"Dd$s"},
{"bchg",	one(0004100),		one(0177700),		"#b$s"},
{"bclr",	one(0000600),		one(0170700),		"Dd$s"},
{"bclr",	one(0004200),		one(0177700),		"#b$s"},
{"bfchg",	two(0165300, 0),	two(0177700, 0170000),	"?sO2O3"},
{"bfclr",	two(0166300, 0),	two(0177700, 0170000),	"?sO2O3"},
{"bfexts",	two(0165700, 0),	two(0177700, 0100000),	"/sO2O3D1"},
{"bfextu",	two(0164700, 0),	two(0177700, 0100000),	"/sO2O3D1"},
{"bfffo",	two(0166700, 0),	two(0177700, 0100000),	"/sO2O3D1"},
{"bfins",	two(0167700, 0),	two(0177700, 0100000),	"D1?sO2O3"},
{"bfset",	two(0167300, 0),	two(0177700, 0170000),	"?sO2O3"},
{"bftst",	two(0164300, 0),	two(0177700, 0170000),	"/sO2O3"},
{"bset",	one(0000700),		one(0170700),		"Dd$s"},
{"bset",	one(0004300),		one(0177700),		"#b$s"},
{"btst",	one(0000400),		one(0170700),		"Dd@s"},
{"btst",	one(0004000),		one(0177700),		"#b@s"},

{"bkpt",	one(0044110),		one(0177770),		"Qs"},
{"bra",		one(0060000),		one(0177400),		"Bg"},
{"bras",	one(0060000),		one(0177400),		"Bw"},
{"bsr",		one(0060400),		one(0177400),		"Bg"},
{"bsrs",	one(0060400),		one(0177400),		"Bw"},

{"callm",	one(0003300),		one(0177700),		"#b!s"},
{"cas2l",	two(0007374, 0),	two(0177777, 0107070),	"D3D6D2D5R1R4"}, /* JF FOO this is really a 3 word ins */
{"cas2w",	two(0006374, 0),	two(0177777, 0107070),	"D3D6D2D5R1R4"}, /* JF ditto */
{"casb",	two(0005300, 0),	two(0177700, 0177070),	"D3D2~s"},
{"casl",	two(0007300, 0),	two(0177700, 0177070),	"D3D2~s"},
{"casw",	two(0006300, 0),	two(0177700, 0177070),	"D3D2~s"},

/*  {"chk",	one(0040600),		one(0170700),		";wDd"}, JF FOO this looks wrong */
{"chk2b",	two(0000300, 0004000),	two(0177700, 07777),	"!sR1"},
{"chk2l",	two(0002300, 0004000),	two(0177700, 07777),	"!sR1"},
{"chk2w",	two(0001300, 0004000),	two(0177700, 07777),	"!sR1"},
{"chkl",	one(0040400),		one(0170700),		";lDd"},
{"chkw",	one(0040600),		one(0170700),		";wDd"},
{"clrb",	one(0041000),		one(0177700),		"$s"},
{"clrl",	one(0041200),		one(0177700),		"$s"},
{"clrw",	one(0041100),		one(0177700),		"$s"},

{"cmp2b",	two(0000300, 0),	two(0177700, 07777),	"!sR1"},
{"cmp2l",	two(0002300, 0),	two(0177700, 07777),	"!sR1"},
{"cmp2w",	two(0001300, 0),	two(0177700, 07777),	"!sR1"},
{"cmpal",	one(0130700),		one(0170700),		"*lAd"},
{"cmpaw",	one(0130300),		one(0170700),		"*wAd"},
{"cmpib",	one(0006000),		one(0177700),		"#b;b"},
{"cmpil",	one(0006200),		one(0177700),		"#l;l"},
{"cmpiw",	one(0006100),		one(0177700),		"#w;w"},
{"cmpb",	one(0006000),		one(0177700),		"#b;b"},	/* cmpi written as cmp */
{"cmpb",	one(0130000),		one(0170700),		";bDd"},
{"cmpw",	one(0006100),		one(0177700),		"#w;w"},
{"cmpw",	one(0130100),		one(0170700),		"*wDd"},
{"cmpw",	one(0130300),		one(0170700),		"*wAd"},	/* cmpa written as cmp */
{"cmpl",	one(0006200),		one(0177700),		"#l;l"},
{"cmpl",	one(0130200),		one(0170700),		"*lDd"},
{"cmpl",	one(0130700),		one(0170700),		"*lAd"},
{"cmpmb",	one(0130410),		one(0170770),		"+s+d"},
{"cmpml",	one(0130610),		one(0170770),		"+s+d"},
{"cmpmw",	one(0130510),		one(0170770),		"+s+d"},

{"dbcc",	one(0052310),		one(0177770),		"DsBw"},
{"dbcs",	one(0052710),		one(0177770),		"DsBw"},
{"dbeq",	one(0053710),		one(0177770),		"DsBw"},
{"dbf",		one(0050710),		one(0177770),		"DsBw"},
{"dbge",	one(0056310),		one(0177770),		"DsBw"},
{"dbgt",	one(0057310),		one(0177770),		"DsBw"},
{"dbhi",	one(0051310),		one(0177770),		"DsBw"},
{"dble",	one(0057710),		one(0177770),		"DsBw"},
{"dbls",	one(0051710),		one(0177770),		"DsBw"},
{"dblt",	one(0056710),		one(0177770),		"DsBw"},
{"dbmi",	one(0055710),		one(0177770),		"DsBw"},
{"dbne",	one(0053310),		one(0177770),		"DsBw"},
{"dbpl",	one(0055310),		one(0177770),		"DsBw"},
{"dbra",	one(0050710),		one(0177770),		"DsBw"},
{"dbt",		one(0050310),		one(0177770),		"DsBw"},
{"dbvc",	one(0054310),		one(0177770),		"DsBw"},
{"dbvs",	one(0054710),		one(0177770),		"DsBw"},

{"divsl",	two(0046100, 0006000),	two(0177700, 0107770),	";lD3D1"},
{"divsl",	two(0046100, 0004000),	two(0177700, 0107770),	";lDD"},
{"divsll",	two(0046100, 0004000),	two(0177700, 0107770),	";lD3D1"},
{"divsw",	one(0100700),		one(0170700),		";wDd"},
{"divs",	one(0100700),		one(0170700),		";wDd"},
{"divul",	two(0046100, 0002000),	two(0177700, 0107770),	";lD3D1"},
{"divul",	two(0046100, 0000000),	two(0177700, 0107770),	";lDD"},
{"divull",	two(0046100, 0000000),	two(0177700, 0107770),	";lD3D1"},
{"divuw",	one(0100300),		one(0170700),		";wDd"},
{"divu",	one(0100300),		one(0170700),		";wDd"},
{"eorb",	one(0005000),		one(0177700),		"#b$s"},	/* eori written as or */
{"eorb",	one(0005074),		one(0177777),		"#bCs"},	/* eori to ccr */
{"eorb",	one(0130400),		one(0170700),		"Dd$s"},	/* register to memory */
{"eorib",	one(0005000),		one(0177700),		"#b$s"},
{"eorib",	one(0005074),		one(0177777),		"#bCs"},	/* eori to ccr */
{"eoril",	one(0005200),		one(0177700),		"#l$s"},
{"eoriw",	one(0005100),		one(0177700),		"#w$s"},
{"eoriw",	one(0005174),		one(0177777),		"#wSs"},	/* eori to sr */
{"eorl",	one(0005200),		one(0177700),		"#l$s"},
{"eorl",	one(0130600),		one(0170700),		"Dd$s"},
{"eorw",	one(0005100),		one(0177700),		"#w$s"},
{"eorw",	one(0005174),		one(0177777),		"#wSs"},	/* eori to sr */
{"eorw",	one(0130500),		one(0170700),		"Dd$s"},

{"exg",		one(0140500),		one(0170770),		"DdDs"},
{"exg",		one(0140510),		one(0170770),		"AdAs"},
{"exg",		one(0140610),		one(0170770),		"DdAs"},
{"exg",		one(0140610),		one(0170770),		"AsDd"},

{"extw",	one(0044200),		one(0177770),		"Ds"},
{"extl",	one(0044300),		one(0177770),		"Ds"},
{"extbl",	one(0044700),		one(0177770),		"Ds"},
{"extb.l",	one(0044700),		one(0177770),		"Ds"},	/* Not sure we should support this one*/

{"illegal",	one(0045374),		one(0177777),		""},
{"jmp",		one(0047300),		one(0177700),		"!s"},
{"jsr",		one(0047200),		one(0177700),		"!s"},
{"lea",		one(0040700),		one(0170700),		"!sAd"},
{"linkw",	one(0047120),		one(0177770),		"As#w"},
{"linkl",	one(0044010),		one(0177770),		"As#l"},
{"link",	one(0047120),		one(0177770),		"As#w"},
{"link",	one(0044010),		one(0177770),		"As#l"},

{"lslb",	one(0160410),		one(0170770),		"QdDs"},	/* lsrb #Q,	Ds */
{"lslb",	one(0160450),		one(0170770),		"DdDs"},	/* lsrb Dd,	Ds */
{"lslw",	one(0160510),		one(0170770),		"QdDs"},	/* lsrb #Q,	Ds */
{"lslw",	one(0160550),		one(0170770),		"DdDs"},	/* lsrb Dd,	Ds */
{"lslw",	one(0161700),		one(0177700),		"~s"},	/* Shift memory */
{"lsll",	one(0160610),		one(0170770),		"QdDs"},	/* lsrb #Q,	Ds */
{"lsll",	one(0160650),		one(0170770),		"DdDs"},	/* lsrb Dd,	Ds */

{"lsrb",	one(0160010),		one(0170770),		"QdDs"} /* lsrb #Q,	Ds */,
{"lsrb",	one(0160050),		one(0170770),		"DdDs"},	/* lsrb Dd,	Ds */
{"lsrl",	one(0160210),		one(0170770),		"QdDs"},	/* lsrb #Q,	Ds */
{"lsrl",	one(0160250),		one(0170770),		"DdDs"},	/* lsrb #Q,	Ds */
{"lsrw",	one(0160110),		one(0170770),		"QdDs"},	/* lsrb #Q,	Ds */
{"lsrw",	one(0160150),		one(0170770),		"DdDs"},	/* lsrb #Q,	Ds */
{"lsrw",	one(0161300),		one(0177700),		"~s"},	/* Shift memory */

{"moveal",	one(0020100),		one(0170700),		"*lAd"},
{"moveaw",	one(0030100),		one(0170700),		"*wAd"},
{"moveb",	one(0010000),		one(0170000),		";b$d"},	/* move */
{"movel",	one(0070000),		one(0170400),		"MsDd"},	/* moveq written as move */
{"movel",	one(0020000),		one(0170000),		"*l$d"},
{"movel",	one(0020100),		one(0170700),		"*lAd"},
{"movel",	one(0047140),		one(0177770),		"AsUd"},	/* move to USP */
{"movel",	one(0047150),		one(0177770),		"UdAs"},	/* move from USP */

{"movec",	one(0047173),		one(0177777),		"R1Jj"},
{"movec",	one(0047173),		one(0177777),		"R1#j"},
{"movec",	one(0047172),		one(0177777),		"JjR1"},
{"movec",	one(0047172),		one(0177777),		"#jR1"},

/* JF added these next four for the assembler */
{"moveml",	one(0044300),		one(0177700),		"Lw&s"},	/* movem reg to mem. */
{"moveml",	one(0044340),		one(0177770),		"lw-s"},	/* movem reg to autodecrement. */
{"moveml",	one(0046300),		one(0177700),		"!sLw"},	/* movem mem to reg. */
{"moveml",	one(0046330),		one(0177770),		"+sLw"},	/* movem autoinc to reg. */

{"moveml",	one(0044300),		one(0177700),		"#w&s"},	/* movem reg to mem. */
{"moveml",	one(0044340),		one(0177770),		"#w-s"},	/* movem reg to autodecrement. */
{"moveml",	one(0046300),		one(0177700),		"!s#w"},	/* movem mem to reg. */
{"moveml",	one(0046330),		one(0177770),		"+s#w"},	/* movem autoinc to reg. */

/* JF added these next four for the assembler */
{"movemw",	one(0044200),		one(0177700),		"Lw&s"},	/* movem reg to mem. */
{"movemw",	one(0044240),		one(0177770),		"lw-s"},	/* movem reg to autodecrement. */
{"movemw",	one(0046200),		one(0177700),		"!sLw"},	/* movem mem to reg. */
{"movemw",	one(0046230),		one(0177770),		"+sLw"},	/* movem autoinc to reg. */

{"movemw",	one(0044200),		one(0177700),		"#w&s"},	/* movem reg to mem. */
{"movemw",	one(0044240),		one(0177770),		"#w-s"},	/* movem reg to autodecrement. */
{"movemw",	one(0046200),		one(0177700),		"!s#w"},	/* movem mem to reg. */
{"movemw",	one(0046230),		one(0177770),		"+s#w"},	/* movem autoinc to reg. */

{"movepl",	one(0000510),		one(0170770),		"dsDd"},	/* memory to register */
{"movepl",	one(0000710),		one(0170770),		"Ddds"},	/* register to memory */
{"movepw",	one(0000410),		one(0170770),		"dsDd"},	/* memory to register */
{"movepw",	one(0000610),		one(0170770),		"Ddds"},	/* register to memory */
{"moveq",	one(0070000),		one(0170400),		"MsDd"},
{"movew",	one(0030000),		one(0170000),		"*w$d"},
{"movew",	one(0030100),		one(0170700),		"*wAd"},	/* movea,	written as move */
{"movew",	one(0040300),		one(0177700),		"Ss$s"},	/* Move from sr */
{"movew",	one(0041300),		one(0177700),		"Cs$s"},	/* Move from ccr */
{"movew",	one(0042300),		one(0177700),		";wCd"},	/* move to ccr */
{"movew",	one(0043300),		one(0177700),		";wSd"},	/* move to sr */

{"movesb",	two(0007000, 0),	two(0177700, 07777),	"~sR1"},	 /* moves from memory */
{"movesb",	two(0007000, 04000),	two(0177700, 07777),	"R1~s"},	 /* moves to memory */
{"movesl",	two(0007200, 0),	two(0177700, 07777),	"~sR1"},	 /* moves from memory */
{"movesl",	two(0007200, 04000),	two(0177700, 07777),	"R1~s"},	 /* moves to memory */
{"movesw",	two(0007100, 0),	two(0177700, 07777),	"~sR1"},	 /* moves from memory */
{"movesw",	two(0007100, 04000),	two(0177700, 07777),	"R1~s"},	 /* moves to memory */

{"mulsl",	two(0046000, 004000),	two(0177700, 0107770),	";lD1"},
{"mulsl",	two(0046000, 006000),	two(0177700, 0107770),	";lD3D1"},
{"mulsw",	one(0140700),		one(0170700),		";wDd"},
{"muls",	one(0140700),		one(0170700),		";wDd"},
{"mulul",	two(0046000, 000000),	two(0177700, 0107770),	";lD1"},
{"mulul",	two(0046000, 002000),	two(0177700, 0107770),	";lD3D1"},
{"muluw",	one(0140300),		one(0170700),		";wDd"},
{"mulu",	one(0140300),		one(0170700),		";wDd"},
{"nbcd",	one(0044000),		one(0177700),		"$s"},
{"negb",	one(0042000),		one(0177700),		"$s"},
{"negl",	one(0042200),		one(0177700),		"$s"},
{"negw",	one(0042100),		one(0177700),		"$s"},
{"negxb",	one(0040000),		one(0177700),		"$s"},
{"negxl",	one(0040200),		one(0177700),		"$s"},
{"negxw",	one(0040100),		one(0177700),		"$s"},
{"nop",		one(0047161),		one(0177777),		""},
{"notb",	one(0043000),		one(0177700),		"$s"},
{"notl",	one(0043200),		one(0177700),		"$s"},
{"notw",	one(0043100),		one(0177700),		"$s"},

{"orb",		one(0000000),		one(0177700),		"#b$s"},	/* ori written as or */
{"orb",		one(0000074),		one(0177777),		"#bCs"},	/* ori to ccr */
{"orb",		one(0100000),		one(0170700),		";bDd"},	/* memory to register */
{"orb",		one(0100400),		one(0170700),		"Dd~s"},	/* register to memory */
{"orib",	one(0000000),		one(0177700),		"#b$s"},
{"orib",	one(0000074),		one(0177777),		"#bCs"},	/* ori to ccr */
{"oril",	one(0000200),		one(0177700),		"#l$s"},
{"oriw",	one(0000100),		one(0177700),		"#w$s"},
{"oriw",	one(0000174),		one(0177777),		"#wSs"},	/* ori to sr */
{"orl",		one(0000200),		one(0177700),		"#l$s"},
{"orl",		one(0100200),		one(0170700),		";lDd"},	/* memory to register */
{"orl",		one(0100600),		one(0170700),		"Dd~s"},	/* register to memory */
{"orw",		one(0000100),		one(0177700),		"#w$s"},
{"orw",		one(0000174),		one(0177777),		"#wSs"},	/* ori to sr */
{"orw",		one(0100100),		one(0170700),		";wDd"},	/* memory to register */
{"orw",		one(0100500),		one(0170700),		"Dd~s"},	/* register to memory */

{"pack",	one(0100500),		one(0170770),		"DsDd#w"},	/* pack Ds,	Dd,	#w */
{"pack",	one(0100510),		one(0170770),		"-s-d#w"},	/* pack -(As),	-(Ad),	#w */
{"pea",		one(0044100),		one(0177700),		"!s"},
{"reset",	one(0047160),		one(0177777),		""},

{"rolb",	one(0160430),		one(0170770),		"QdDs"},	/* rorb #Q,	Ds */
{"rolb",	one(0160470),		one(0170770),		"DdDs"},	/* rorb Dd,	Ds */
{"roll",	one(0160630),		one(0170770),		"QdDs"},	/* rorb #Q,	Ds */
{"roll",	one(0160670),		one(0170770),		"DdDs"},	/* rorb Dd,	Ds */
{"rolw",	one(0160530),		one(0170770),		"QdDs"},	/* rorb #Q,	Ds */
{"rolw",	one(0160570),		one(0170770),		"DdDs"},	/* rorb Dd,	Ds */
{"rolw",	one(0163700),		one(0177700),		"~s"},	/* Rotate memory */
{"rorb",	one(0160030),		one(0170770),		"QdDs"},	/* rorb #Q,	Ds */
{"rorb",	one(0160070),		one(0170770),		"DdDs"},	/* rorb Dd,	Ds */
{"rorl",	one(0160230),		one(0170770),		"QdDs"},	/* rorb #Q,	Ds */
{"rorl",	one(0160270),		one(0170770),		"DdDs"},	/* rorb Dd,	Ds */
{"rorw",	one(0160130),		one(0170770),		"QdDs"},	/* rorb #Q,	Ds */
{"rorw",	one(0160170),		one(0170770),		"DdDs"},	/* rorb Dd,	Ds */
{"rorw",	one(0163300),		one(0177700),		"~s"},	/* Rotate memory */

{"roxlb",	one(0160420),		one(0170770),		"QdDs"},	/* roxrb #Q,	Ds */
{"roxlb",	one(0160460),		one(0170770),		"DdDs"},	/* roxrb Dd,	Ds */
{"roxll",	one(0160620),		one(0170770),		"QdDs"},	/* roxrb #Q,	Ds */
{"roxll",	one(0160660),		one(0170770),		"DdDs"},	/* roxrb Dd,	Ds */
{"roxlw",	one(0160520),		one(0170770),		"QdDs"},	/* roxrb #Q,	Ds */
{"roxlw",	one(0160560),		one(0170770),		"DdDs"},	/* roxrb Dd,	Ds */
{"roxlw",	one(0162700),		one(0177700),		"~s"},	/* Rotate memory */
{"roxrb",	one(0160020),		one(0170770),		"QdDs"},	/* roxrb #Q,	Ds */
{"roxrb",	one(0160060),		one(0170770),		"DdDs"},	/* roxrb Dd,	Ds */
{"roxrl",	one(0160220),		one(0170770),		"QdDs"},	/* roxrb #Q,	Ds */
{"roxrl",	one(0160260),		one(0170770),		"DdDs"},	/* roxrb Dd,	Ds */
{"roxrw",	one(0160120),		one(0170770),		"QdDs"},	/* roxrb #Q,	Ds */
{"roxrw",	one(0160160),		one(0170770),		"DdDs"},	/* roxrb Dd,	Ds */
{"roxrw",	one(0162300),		one(0177700),		"~s"},	/* Rotate memory */

{"rtd",		one(0047164),		one(0177777),		"#w"},
{"rte",		one(0047163),		one(0177777),		""},
{"rtm",		one(0003300),		one(0177760),		"Rs"},
{"rtr",		one(0047167),		one(0177777),		""},
{"rts",		one(0047165),		one(0177777),		""},

{"scc",		one(0052300),		one(0177700),		"$s"},
{"scs",		one(0052700),		one(0177700),		"$s"},
{"seq",		one(0053700),		one(0177700),		"$s"},
{"sf",		one(0050700),		one(0177700),		"$s"},
{"sge",		one(0056300),		one(0177700),		"$s"},
{"sgt",		one(0057300),		one(0177700),		"$s"},
{"shi",		one(0051300),		one(0177700),		"$s"},
{"sle",		one(0057700),		one(0177700),		"$s"},
{"sls",		one(0051700),		one(0177700),		"$s"},
{"slt",		one(0056700),		one(0177700),		"$s"},
{"smi",		one(0055700),		one(0177700),		"$s"},
{"sne",		one(0053300),		one(0177700),		"$s"},
{"spl",		one(0055300),		one(0177700),		"$s"},
{"st",		one(0050300),		one(0177700),		"$s"},
{"svc",		one(0054300),		one(0177700),		"$s"},
{"svs",		one(0054700),		one(0177700),		"$s"},

{"sbcd",	one(0100400),		one(0170770),		"DsDd"},
{"sbcd",	one(0100410),		one(0170770),		"-s-d"},
{"stop",	one(0047162),		one(0177777),		"#w"},

{"subal",	one(0110700),		one(0170700),		"*lAd"},
{"subaw",	one(0110300),		one(0170700),		"*wAd"},
{"subb",	one(0050400),		one(0170700),		"Qd%s"},	/* subq written as sub */
{"subb",	one(0002000),		one(0177700),		"#b$s"},	/* subi written as sub */
{"subb",	one(0110000),		one(0170700),		";bDd"},	/* subb ? ?,	Dd */
{"subb",	one(0110400),		one(0170700),		"Dd~s"},	/* subb Dd,	? ? */
{"subib",	one(0002000),		one(0177700),		"#b$s"},
{"subil",	one(0002200),		one(0177700),		"#l$s"},
{"subiw",	one(0002100),		one(0177700),		"#w$s"},
{"subl",	one(0050600),		one(0170700),		"Qd%s"},
{"subl",	one(0002200),		one(0177700),		"#l$s"},
{"subl",	one(0110700),		one(0170700),		"*lAd"},
{"subl",	one(0110200),		one(0170700),		"*lDd"},
{"subl",	one(0110600),		one(0170700),		"Dd~s"},
{"subqb",	one(0050400),		one(0170700),		"Qd%s"},
{"subql",	one(0050600),		one(0170700),		"Qd%s"},
{"subqw",	one(0050500),		one(0170700),		"Qd%s"},
{"subw",	one(0050500),		one(0170700),		"Qd%s"},
{"subw",	one(0002100),		one(0177700),		"#w$s"},
{"subw",	one(0110100),		one(0170700),		"*wDd"},
{"subw",	one(0110300),		one(0170700),		"*wAd"},	/* suba written as sub */
{"subw",	one(0110500),		one(0170700),		"Dd~s"},
{"subxb",	one(0110400),		one(0170770),		"DsDd"},	/* subxb Ds,	Dd */
{"subxb",	one(0110410),		one(0170770),		"-s-d"},	/* subxb -(As),	-(Ad) */
{"subxl",	one(0110600),		one(0170770),		"DsDd"},
{"subxl",	one(0110610),		one(0170770),		"-s-d"},
{"subxw",	one(0110500),		one(0170770),		"DsDd"},
{"subxw",	one(0110510),		one(0170770),		"-s-d"},

{"swap",	one(0044100),		one(0177770),		"Ds"},
	
{"tas",		one(0045300),		one(0177700),		"$s"},
{"trap",	one(0047100),		one(0177760),		"Ts"},

{"trapcc",	one(0052374),		one(0177777),		""},
{"trapcs",	one(0052774),		one(0177777),		""},
{"trapeq",	one(0053774),		one(0177777),		""},
{"trapf",	one(0050774),		one(0177777),		""},
{"trapge",	one(0056374),		one(0177777),		""},
{"trapgt",	one(0057374),		one(0177777),		""},
{"traphi",	one(0051374),		one(0177777),		""},
{"traple",	one(0057774),		one(0177777),		""},
{"trapls",	one(0051774),		one(0177777),		""},
{"traplt",	one(0056774),		one(0177777),		""},
{"trapmi",	one(0055774),		one(0177777),		""},
{"trapne",	one(0053374),		one(0177777),		""},
{"trappl",	one(0055374),		one(0177777),		""},
{"trapt",	one(0050374),		one(0177777),		""},
{"trapvc",	one(0054374),		one(0177777),		""},
{"trapvs",	one(0054774),		one(0177777),		""},

{"trapcc.w",	one(0052372),		one(0177777),		""},
{"trapcs.w",	one(0052772),		one(0177777),		""},
{"trapeq.w",	one(0053772),		one(0177777),		""},
{"trapf.w",	one(0050772),		one(0177777),		""},
{"trapge.w",	one(0056372),		one(0177777),		""},
{"trapgt.w",	one(0057372),		one(0177777),		""},
{"traphi.w",	one(0051372),		one(0177777),		""},
{"traple.w",	one(0057772),		one(0177777),		""},
{"trapls.w",	one(0051772),		one(0177777),		""},
{"traplt.w",	one(0056772),		one(0177777),		""},
{"trapmi.w",	one(0055772),		one(0177777),		""},
{"trapne.w",	one(0053372),		one(0177777),		""},
{"trappl.w",	one(0055372),		one(0177777),		""},
{"trapt.w",	one(0050372),		one(0177777),		""},
{"trapvc.w",	one(0054372),		one(0177777),		""},
{"trapvs.w",	one(0054772),		one(0177777),		""},

{"trapcc.l",	one(0052373),		one(0177777),		""},
{"trapcs.l",	one(0052773),		one(0177777),		""},
{"trapeq.l",	one(0053773),		one(0177777),		""},
{"trapf.l",	one(0050773),		one(0177777),		""},
{"trapge.l",	one(0056373),		one(0177777),		""},
{"trapgt.l",	one(0057373),		one(0177777),		""},
{"traphi.l",	one(0051373),		one(0177777),		""},
{"traple.l",	one(0057773),		one(0177777),		""},
{"trapls.l",	one(0051773),		one(0177777),		""},
{"traplt.l",	one(0056773),		one(0177777),		""},
{"trapmi.l",	one(0055773),		one(0177777),		""},
{"trapne.l",	one(0053373),		one(0177777),		""},
{"trappl.l",	one(0055373),		one(0177777),		""},
{"trapt.l",	one(0050373),		one(0177777),		""},
{"trapvc.l",	one(0054373),		one(0177777),		""},
{"trapvs.l",	one(0054773),		one(0177777),		""},

{"trapv",	one(0047166),		one(0177777),		""},

{"tstb",	one(0045000),		one(0177700),		";b"},
{"tstw",	one(0045100),		one(0177700),		"*w"},
{"tstl",	one(0045200),		one(0177700),		"*l"},

{"unlk",	one(0047130),		one(0177770),		"As"},
{"unpk",	one(0100600),		one(0170770),		"DsDd#w"},
{"unpk",	one(0100610),		one(0170770),		"-s-d#w"},
	/* JF floating pt stuff moved down here */

{"fabsb",	two(0xF000, 0x5818),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fabsd",	two(0xF000, 0x5418),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fabsl",	two(0xF000, 0x4018),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fabsp",	two(0xF000, 0x4C18),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fabss",	two(0xF000, 0x4418),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fabsw",	two(0xF000, 0x5018),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fabsx",	two(0xF000, 0x0018),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fabsx",	two(0xF000, 0x4818),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fabsx",	two(0xF000, 0x0018),	two(0xF1C0, 0xE07F),	"IiFt"},

{"facosb",	two(0xF000, 0x581C),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"facosd",	two(0xF000, 0x541C),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"facosl",	two(0xF000, 0x401C),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"facosp",	two(0xF000, 0x4C1C),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"facoss",	two(0xF000, 0x441C),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"facosw",	two(0xF000, 0x501C),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"facosx",	two(0xF000, 0x001C),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"facosx",	two(0xF000, 0x481C),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"facosx",	two(0xF000, 0x001C),	two(0xF1C0, 0xE07F),	"IiFt"},

{"faddb",	two(0xF000, 0x5822),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"faddd",	two(0xF000, 0x5422),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"faddl",	two(0xF000, 0x4022),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"faddp",	two(0xF000, 0x4C22),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fadds",	two(0xF000, 0x4422),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"faddw",	two(0xF000, 0x5022),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"faddx",	two(0xF000, 0x0022),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"faddx",	two(0xF000, 0x4822),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
/* {"faddx",	two(0xF000, 0x0022),	two(0xF1C0, 0xE07F),	"IiFt"}, JF removed */

{"fasinb",	two(0xF000, 0x580C),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fasind",	two(0xF000, 0x540C),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fasinl",	two(0xF000, 0x400C),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fasinp",	two(0xF000, 0x4C0C),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fasins",	two(0xF000, 0x440C),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fasinw",	two(0xF000, 0x500C),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fasinx",	two(0xF000, 0x000C),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fasinx",	two(0xF000, 0x480C),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fasinx",	two(0xF000, 0x000C),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fatanb",	two(0xF000, 0x580A),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fatand",	two(0xF000, 0x540A),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fatanl",	two(0xF000, 0x400A),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fatanp",	two(0xF000, 0x4C0A),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fatans",	two(0xF000, 0x440A),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fatanw",	two(0xF000, 0x500A),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fatanx",	two(0xF000, 0x000A),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fatanx",	two(0xF000, 0x480A),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fatanx",	two(0xF000, 0x000A),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fatanhb",	two(0xF000, 0x580D),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fatanhd",	two(0xF000, 0x540D),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fatanhl",	two(0xF000, 0x400D),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fatanhp",	two(0xF000, 0x4C0D),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fatanhs",	two(0xF000, 0x440D),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fatanhw",	two(0xF000, 0x500D),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fatanhx",	two(0xF000, 0x000D),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fatanhx",	two(0xF000, 0x480D),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fatanhx",	two(0xF000, 0x000D),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fbeq",	one(0xF081),		one(0xF1BF),		"IdBc"},
{"fbf",		one(0xF080),		one(0xF1BF),		"IdBc"},
{"fbge",	one(0xF093),		one(0xF1BF),		"IdBc"},
{"fbgl",	one(0xF096),		one(0xF1BF),		"IdBc"},
{"fbgle",	one(0xF097),		one(0xF1BF),		"IdBc"},
{"fbgt",	one(0xF092),		one(0xF1BF),		"IdBc"},
{"fble",	one(0xF095),		one(0xF1BF),		"IdBc"},
{"fblt",	one(0xF094),		one(0xF1BF),		"IdBc"},
{"fbne",	one(0xF08E),		one(0xF1BF),		"IdBc"},
{"fbnge",	one(0xF09C),		one(0xF1BF),		"IdBc"},
{"fbngl",	one(0xF099),		one(0xF1BF),		"IdBc"},
{"fbngle",	one(0xF098),		one(0xF1BF),		"IdBc"},
{"fbngt",	one(0xF09D),		one(0xF1BF),		"IdBc"},
{"fbnle",	one(0xF09A),		one(0xF1BF),		"IdBc"},
{"fbnlt",	one(0xF09B),		one(0xF1BF),		"IdBc"},
{"fboge",	one(0xF083),		one(0xF1BF),		"IdBc"},
{"fbogl",	one(0xF086),		one(0xF1BF),		"IdBc"},
{"fbogt",	one(0xF082),		one(0xF1BF),		"IdBc"},
{"fbole",	one(0xF085),		one(0xF1BF),		"IdBc"},
{"fbolt",	one(0xF084),		one(0xF1BF),		"IdBc"},
{"fbor",	one(0xF087),		one(0xF1BF),		"IdBc"},
{"fbseq",	one(0xF091),		one(0xF1BF),		"IdBc"},
{"fbsf",	one(0xF090),		one(0xF1BF),		"IdBc"},
{"fbsne",	one(0xF09E),		one(0xF1BF),		"IdBc"},
{"fbst",	one(0xF09F),		one(0xF1BF),		"IdBc"},
{"fbt",		one(0xF08F),		one(0xF1BF),		"IdBc"},
{"fbueq",	one(0xF089),		one(0xF1BF),		"IdBc"},
{"fbuge",	one(0xF08B),		one(0xF1BF),		"IdBc"},
{"fbugt",	one(0xF08A),		one(0xF1BF),		"IdBc"},
{"fbule",	one(0xF08D),		one(0xF1BF),		"IdBc"},
{"fbult",	one(0xF08C),		one(0xF1BF),		"IdBc"},
{"fbun",	one(0xF088),		one(0xF1BF),		"IdBc"},

{"fcmpb",	two(0xF000, 0x5838),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fcmpd",	two(0xF000, 0x5438),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fcmpl",	two(0xF000, 0x4038),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fcmpp",	two(0xF000, 0x4C38),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fcmps",	two(0xF000, 0x4438),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fcmpw",	two(0xF000, 0x5038),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fcmpx",	two(0xF000, 0x0038),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fcmpx",	two(0xF000, 0x4838),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
/* {"fcmpx",	two(0xF000, 0x0038),	two(0xF1C0, 0xE07F),	"IiFt"}, JF removed */

{"fcosb",	two(0xF000, 0x581D),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fcosd",	two(0xF000, 0x541D),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fcosl",	two(0xF000, 0x401D),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fcosp",	two(0xF000, 0x4C1D),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fcoss",	two(0xF000, 0x441D),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fcosw",	two(0xF000, 0x501D),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fcosx",	two(0xF000, 0x001D),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fcosx",	two(0xF000, 0x481D),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fcosx",	two(0xF000, 0x001D),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fcoshb",	two(0xF000, 0x5819),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fcoshd",	two(0xF000, 0x5419),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fcoshl",	two(0xF000, 0x4019),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fcoshp",	two(0xF000, 0x4C19),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fcoshs",	two(0xF000, 0x4419),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fcoshw",	two(0xF000, 0x5019),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fcoshx",	two(0xF000, 0x0019),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fcoshx",	two(0xF000, 0x4819),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fcoshx",	two(0xF000, 0x0019),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fdbeq",	two(0xF048, 0x0001),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbf",	two(0xF048, 0x0000),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbge",	two(0xF048, 0x0013),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbgl",	two(0xF048, 0x0016),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbgle",	two(0xF048, 0x0017),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbgt",	two(0xF048, 0x0012),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdble",	two(0xF048, 0x0015),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdblt",	two(0xF048, 0x0014),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbne",	two(0xF048, 0x000E),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbnge",	two(0xF048, 0x001C),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbngl",	two(0xF048, 0x0019),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbngle",	two(0xF048, 0x0018),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbngt",	two(0xF048, 0x001D),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbnle",	two(0xF048, 0x001A),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbnlt",	two(0xF048, 0x001B),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdboge",	two(0xF048, 0x0003),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbogl",	two(0xF048, 0x0006),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbogt",	two(0xF048, 0x0002),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbole",	two(0xF048, 0x0005),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbolt",	two(0xF048, 0x0004),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbor",	two(0xF048, 0x0007),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbseq",	two(0xF048, 0x0011),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbsf",	two(0xF048, 0x0010),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbsne",	two(0xF048, 0x001E),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbst",	two(0xF048, 0x001F),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbt",	two(0xF048, 0x000F),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbueq",	two(0xF048, 0x0009),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbuge",	two(0xF048, 0x000B),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbugt",	two(0xF048, 0x000A),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbule",	two(0xF048, 0x000D),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbult",	two(0xF048, 0x000C),	two(0xF1F8, 0xFFFF),	"IiDsBw"},
{"fdbun",	two(0xF048, 0x0008),	two(0xF1F8, 0xFFFF),	"IiDsBw"},

{"fdivb",	two(0xF000, 0x5820),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fdivd",	two(0xF000, 0x5420),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fdivl",	two(0xF000, 0x4020),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fdivp",	two(0xF000, 0x4C20),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fdivs",	two(0xF000, 0x4420),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fdivw",	two(0xF000, 0x5020),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fdivx",	two(0xF000, 0x0020),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fdivx",	two(0xF000, 0x4820),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
/* {"fdivx",	two(0xF000, 0x0020),	two(0xF1C0, 0xE07F),	"IiFt"}, JF */

{"fetoxb",	two(0xF000, 0x5810),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fetoxd",	two(0xF000, 0x5410),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fetoxl",	two(0xF000, 0x4010),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fetoxp",	two(0xF000, 0x4C10),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fetoxs",	two(0xF000, 0x4410),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fetoxw",	two(0xF000, 0x5010),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fetoxx",	two(0xF000, 0x0010),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fetoxx",	two(0xF000, 0x4810),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fetoxx",	two(0xF000, 0x0010),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fetoxm1b",	two(0xF000, 0x5808),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fetoxm1d",	two(0xF000, 0x5408),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fetoxm1l",	two(0xF000, 0x4008),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fetoxm1p",	two(0xF000, 0x4C08),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fetoxm1s",	two(0xF000, 0x4408),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fetoxm1w",	two(0xF000, 0x5008),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fetoxm1x",	two(0xF000, 0x0008),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fetoxm1x",	two(0xF000, 0x4808),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fetoxm1x",	two(0xF000, 0x0008),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fgetexpb",	two(0xF000, 0x581E),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fgetexpd",	two(0xF000, 0x541E),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fgetexpl",	two(0xF000, 0x401E),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fgetexpp",	two(0xF000, 0x4C1E),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fgetexps",	two(0xF000, 0x441E),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fgetexpw",	two(0xF000, 0x501E),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fgetexpx",	two(0xF000, 0x001E),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fgetexpx",	two(0xF000, 0x481E),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fgetexpx",	two(0xF000, 0x001E),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fgetmanb",	two(0xF000, 0x581F),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fgetmand",	two(0xF000, 0x541F),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fgetmanl",	two(0xF000, 0x401F),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fgetmanp",	two(0xF000, 0x4C1F),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fgetmans",	two(0xF000, 0x441F),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fgetmanw",	two(0xF000, 0x501F),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fgetmanx",	two(0xF000, 0x001F),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fgetmanx",	two(0xF000, 0x481F),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fgetmanx",	two(0xF000, 0x001F),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fintb",	two(0xF000, 0x5801),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fintd",	two(0xF000, 0x5401),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fintl",	two(0xF000, 0x4001),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fintp",	two(0xF000, 0x4C01),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fints",	two(0xF000, 0x4401),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fintw",	two(0xF000, 0x5001),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fintx",	two(0xF000, 0x0001),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fintx",	two(0xF000, 0x4801),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fintx",	two(0xF000, 0x0001),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fintrzb",	two(0xF000, 0x5803),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fintrzd",	two(0xF000, 0x5403),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fintrzl",	two(0xF000, 0x4003),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fintrzp",	two(0xF000, 0x4C03),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fintrzs",	two(0xF000, 0x4403),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fintrzw",	two(0xF000, 0x5003),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fintrzx",	two(0xF000, 0x0003),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fintrzx",	two(0xF000, 0x4803),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fintrzx",	two(0xF000, 0x0003),	two(0xF1C0, 0xE07F),	"IiFt"},

{"flog10b",	two(0xF000, 0x5815),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"flog10d",	two(0xF000, 0x5415),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"flog10l",	two(0xF000, 0x4015),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"flog10p",	two(0xF000, 0x4C15),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"flog10s",	two(0xF000, 0x4415),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"flog10w",	two(0xF000, 0x5015),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"flog10x",	two(0xF000, 0x0015),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"flog10x",	two(0xF000, 0x4815),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"flog10x",	two(0xF000, 0x0015),	two(0xF1C0, 0xE07F),	"IiFt"},

{"flog2b",	two(0xF000, 0x5816),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"flog2d",	two(0xF000, 0x5416),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"flog2l",	two(0xF000, 0x4016),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"flog2p",	two(0xF000, 0x4C16),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"flog2s",	two(0xF000, 0x4416),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"flog2w",	two(0xF000, 0x5016),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"flog2x",	two(0xF000, 0x0016),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"flog2x",	two(0xF000, 0x4816),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"flog2x",	two(0xF000, 0x0016),	two(0xF1C0, 0xE07F),	"IiFt"},

{"flognb",	two(0xF000, 0x5814),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"flognd",	two(0xF000, 0x5414),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"flognl",	two(0xF000, 0x4014),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"flognp",	two(0xF000, 0x4C14),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"flogns",	two(0xF000, 0x4414),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"flognw",	two(0xF000, 0x5014),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"flognx",	two(0xF000, 0x0014),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"flognx",	two(0xF000, 0x4814),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"flognx",	two(0xF000, 0x0014),	two(0xF1C0, 0xE07F),	"IiFt"},

{"flognp1b",	two(0xF000, 0x5806),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"flognp1d",	two(0xF000, 0x5406),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"flognp1l",	two(0xF000, 0x4006),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"flognp1p",	two(0xF000, 0x4C06),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"flognp1s",	two(0xF000, 0x4406),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"flognp1w",	two(0xF000, 0x5006),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"flognp1x",	two(0xF000, 0x0006),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"flognp1x",	two(0xF000, 0x4806),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"flognp1x",	two(0xF000, 0x0006),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fmodb",	two(0xF000, 0x5821),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fmodd",	two(0xF000, 0x5421),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fmodl",	two(0xF000, 0x4021),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fmodp",	two(0xF000, 0x4C21),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fmods",	two(0xF000, 0x4421),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fmodw",	two(0xF000, 0x5021),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fmodx",	two(0xF000, 0x0021),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fmodx",	two(0xF000, 0x4821),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
/* {"fmodx",	two(0xF000, 0x0021),	two(0xF1C0, 0xE07F),	"IiFt"}, JF */

{"fmoveb",	two(0xF000, 0x5800),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},		/* fmove from <ea> to fp<n> */
{"fmoveb",	two(0xF000, 0x7800),	two(0xF1C0, 0xFC7F),	"IiF7@b"},		/* fmove from fp<n> to <ea> */
{"fmoved",	two(0xF000, 0x5400),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},		/* fmove from <ea> to fp<n> */
{"fmoved",	two(0xF000, 0x7400),	two(0xF1C0, 0xFC7F),	"IiF7@F"},		/* fmove from fp<n> to <ea> */
{"fmovel",	two(0xF000, 0x4000),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},		/* fmove from <ea> to fp<n> */
{"fmovel",	two(0xF000, 0x6000),	two(0xF1C0, 0xFC7F),	"IiF7@l"},		/* fmove from fp<n> to <ea> */
/* Warning:  The addressing modes on these are probably not right:
   esp, Areg direct is only allowed for FPI */
		/* fmove.l from/to system control registers: */
{"fmovel",	two(0xF000, 0xA000),	two(0xF1C0, 0xE3FF),	"Iis8@s"},
{"fmovel",	two(0xF000, 0x8000),	two(0xF1C0, 0xE3FF),	"Ii*ls8"},

/* {"fmovel",	two(0xF000, 0xA000),	two(0xF1C0, 0xE3FF),	"Iis8@s"},
{"fmovel",	two(0xF000, 0x8000),	two(0xF2C0, 0xE3FF),	"Ii*ss8"}, */

{"fmovep",	two(0xF000, 0x4C00),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},		/* fmove from <ea> to fp<n> */
{"fmovep",	two(0xF000, 0x6C00),	two(0xF1C0, 0xFC00),	"IiF7@pkC"},		/* fmove.p with k-factors: */
{"fmovep",	two(0xF000, 0x7C00),	two(0xF1C0, 0xFC0F),	"IiF7@pDk"},		/* fmove.p with k-factors: */

{"fmoves",	two(0xF000, 0x4400),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},		/* fmove from <ea> to fp<n> */
{"fmoves",	two(0xF000, 0x6400),	two(0xF1C0, 0xFC7F),	"IiF7@f"},		/* fmove from fp<n> to <ea> */
{"fmovew",	two(0xF000, 0x5000),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},		/* fmove from <ea> to fp<n> */
{"fmovew",	two(0xF000, 0x7000),	two(0xF1C0, 0xFC7F),	"IiF7@w"},		/* fmove from fp<n> to <ea> */
{"fmovex",	two(0xF000, 0x0000),	two(0xF1C0, 0xE07F),	"IiF8F7"},		/* fmove from <ea> to fp<n> */
{"fmovex",	two(0xF000, 0x4800),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},		/* fmove from <ea> to fp<n> */
{"fmovex",	two(0xF000, 0x6800),	two(0xF1C0, 0xFC7F),	"IiF7@x"},		/* fmove from fp<n> to <ea> */
/* JF removed {"fmovex",	two(0xF000, 0x0000),	two(0xF1C0, 0xE07F),	"IiFt"},		/ * fmove from <ea> to fp<n> */

{"fmovecrx",	two(0xF000, 0x5C00),	two(0xF1FF, 0xFC00),	"Ii#CF7"},		/* fmovecr.x #ccc,	FPn */
{"fmovecr",	two(0xF000, 0x5C00),	two(0xF1FF, 0xFC00),	"Ii#CF7"},

/* Other fmovemx.  */
{"fmovemx",	two(0xF020, 0xE000),	two(0xF1F8, 0xFF00),	"IdL3-s"},		/* fmovem.x to autodecrement,	static and dynamic */
{"fmovemx",	two(0xF020, 0xE000),	two(0xF1F8, 0xFF00),	"Id#3-s"},		/* fmovem.x to autodecrement,	static and dynamic */

{"fmovemx",	two(0xF020, 0xE800),	two(0xF1F8, 0xFF8F),	"IiDk-s"},		/* fmovem.x to autodecrement,	static and dynamic */

{"fmovemx",	two(0xF000, 0xF000),	two(0xF1C0, 0xFF00),	"Id#3&s"},		/* fmovem.x to control,	static and dynamic: */
{"fmovemx",	two(0xF000, 0xF800),	two(0xF1C0, 0xFF8F),	"IiDk&s"},		/* fmovem.x to control,	static and dynamic: */
{"fmovemx",	two(0xF000, 0xD000),	two(0xF1C0, 0xFF00),	"Id&s#3"},		/* fmovem.x from control,	static and dynamic: */
{"fmovemx",	two(0xF000, 0xD800),	two(0xF1C0, 0xFF8F),	"Ii&sDk"},		/* fmovem.x from control,	static and dynamic: */
{"fmovemx",	two(0xF000, 0xF000),	two(0xF1C0, 0xFF00),	"Idl3&s"},		/* fmovem.x to control,	static and dynamic: */
{"fmovemx",	two(0xF000, 0xD000),	two(0xF1C0, 0xFF00),	"Id&sl3"},		/* fmovem.x from control,	static and dynamic: */

{"fmovemx",	two(0xF018, 0xD000),	two(0xF1F8, 0xFF00),	"Id+sl3"},		/* fmovem.x from autoincrement,	static and dynamic: */
{"fmovemx",	two(0xF018, 0xD000),	two(0xF1F8, 0xFF00),	"Id+s#3"},		/* fmovem.x from autoincrement,	static and dynamic: */
{"fmovemx",	two(0xF018, 0xD800),	two(0xF1F8, 0xFF8F),	"Ii+sDk"},		/* fmovem.x from autoincrement,	static and dynamic: */

{"fmoveml",	two(0xF000, 0xA000),	two(0xF1C0, 0xE3FF),	"IiL8@s"},
{"fmoveml",	two(0xF000, 0xA000),	two(0xF1C0, 0xE3FF),	"Ii#8@s"},
{"fmoveml",	two(0xF000, 0xA000),	two(0xF1C0, 0xE3FF),	"Iis8@s"},

{"fmoveml",	two(0xF000, 0x8000),	two(0xF2C0, 0xE3FF),	"Ii*sL8"},
{"fmoveml",	two(0xF000, 0x8000),	two(0xF1C0, 0xE3FF),	"Ii*s#8"},
{"fmoveml",	two(0xF000, 0x8000),	two(0xF1C0, 0xE3FF),	"Ii*ss8"},

/* fmovemx with register lists */
{"fmovem",	two(0xF020, 0xE000),	two(0xF1F8, 0xFF00),	"IdL3-s"},		/* fmovem.x to autodecrement,	static and dynamic */
{"fmovem",	two(0xF000, 0xF000),	two(0xF1C0, 0xFF00),	"Idl3&s"},		/* fmovem.x to control,	static and dynamic: */
{"fmovem",	two(0xF018, 0xD000),	two(0xF1F8, 0xFF00),	"Id+sl3"},		/* fmovem.x from autoincrement,	static and dynamic: */
{"fmovem",	two(0xF000, 0xD000),	two(0xF1C0, 0xFF00),	"Id&sl3"},		/* fmovem.x from control,	static and dynamic: */

	/* Alternate mnemonics for GNU as and GNU CC */
{"fmovem",	two(0xF020, 0xE000),	two(0xF1F8, 0xFF00),	"Id#3-s"},		/* fmovem.x to autodecrement,	static and dynamic */
{"fmovem",	two(0xF020, 0xE800),	two(0xF1F8, 0xFF8F),	"IiDk-s"},		/* fmovem.x to autodecrement,	static and dynamic */

{"fmovem",	two(0xF000, 0xF000),	two(0xF1C0, 0xFF00),	"Id#3&s"},		/* fmovem.x to control,	static and dynamic: */
{"fmovem",	two(0xF000, 0xF800),	two(0xF1C0, 0xFF8F),	"IiDk&s"},		/* fmovem.x to control,	static and dynamic: */

{"fmovem",	two(0xF018, 0xD000),	two(0xF1F8, 0xFF00),	"Id+s#3"},		/* fmovem.x from autoincrement,	static and dynamic: */
{"fmovem",	two(0xF018, 0xD800),	two(0xF1F8, 0xFF8F),	"Ii+sDk"},		/* fmovem.x from autoincrement,	static and dynamic: */
  
{"fmovem",	two(0xF000, 0xD000),	two(0xF1C0, 0xFF00),	"Id&s#3"},		/* fmovem.x from control,	static and dynamic: */
{"fmovem",	two(0xF000, 0xD800),	two(0xF1C0, 0xFF8F),	"Ii&sDk"},		/* fmovem.x from control,	static and dynamic: */

/* fmoveml a FP-control register */
{"fmovem",	two(0xF000, 0xA000),	two(0xF1C0, 0xE3FF),	"Iis8@s"},
{"fmovem",	two(0xF000, 0x8000),	two(0xF1C0, 0xE3FF),	"Ii*ss8"},

/* fmoveml a FP-control reglist */
{"fmovem",	two(0xF000, 0xA000),	two(0xF1C0, 0xE3FF),	"IiL8@s"},
{"fmovem",	two(0xF000, 0x8000),	two(0xF2C0, 0xE3FF),	"Ii*sL8"},

{"fmulb",	two(0xF000, 0x5823),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fmuld",	two(0xF000, 0x5423),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fmull",	two(0xF000, 0x4023),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fmulp",	two(0xF000, 0x4C23),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fmuls",	two(0xF000, 0x4423),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fmulw",	two(0xF000, 0x5023),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fmulx",	two(0xF000, 0x0023),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fmulx",	two(0xF000, 0x4823),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
/* {"fmulx",	two(0xF000, 0x0023),	two(0xF1C0, 0xE07F),	"IiFt"}, JF */

{"fnegb",	two(0xF000, 0x581A),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fnegd",	two(0xF000, 0x541A),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fnegl",	two(0xF000, 0x401A),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fnegp",	two(0xF000, 0x4C1A),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fnegs",	two(0xF000, 0x441A),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fnegw",	two(0xF000, 0x501A),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fnegx",	two(0xF000, 0x001A),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fnegx",	two(0xF000, 0x481A),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fnegx",	two(0xF000, 0x001A),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fnop",	two(0xF280, 0x0000),	two(0xFFFF, 0xFFFF),	"Ii"},

{"fremb",	two(0xF000, 0x5825),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fremd",	two(0xF000, 0x5425),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"freml",	two(0xF000, 0x4025),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fremp",	two(0xF000, 0x4C25),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"frems",	two(0xF000, 0x4425),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fremw",	two(0xF000, 0x5025),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fremx",	two(0xF000, 0x0025),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fremx",	two(0xF000, 0x4825),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
/* {"fremx",	two(0xF000, 0x0025),	two(0xF1C0, 0xE07F),	"IiFt"}, JF */

{"frestore",	one(0xF140),		one(0xF1C0),		"Id&s"},
{"frestore",	one(0xF158),		one(0xF1F8),		"Id+s"},
{"fsave",	one(0xF100),		one(0xF1C0),		"Id&s"},
{"fsave",	one(0xF120),		one(0xF1F8),		"Id-s"},

{"fsincosb",	two(0xF000, 0x5830),	two(0xF1C0, 0xFC78),	"Ii;bF3F7"},
{"fsincosd",	two(0xF000, 0x5430),	two(0xF1C0, 0xFC78),	"Ii;FF3F7"},
{"fsincosl",	two(0xF000, 0x4030),	two(0xF1C0, 0xFC78),	"Ii;lF3F7"},
{"fsincosp",	two(0xF000, 0x4C30),	two(0xF1C0, 0xFC78),	"Ii;pF3F7"},
{"fsincoss",	two(0xF000, 0x4430),	two(0xF1C0, 0xFC78),	"Ii;fF3F7"},
{"fsincosw",	two(0xF000, 0x5030),	two(0xF1C0, 0xFC78),	"Ii;wF3F7"},
{"fsincosx",	two(0xF000, 0x0030),	two(0xF1C0, 0xE078),	"IiF8F3F7"},
{"fsincosx",	two(0xF000, 0x4830),	two(0xF1C0, 0xFC78),	"Ii;xF3F7"},

{"fscaleb",	two(0xF000, 0x5826),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fscaled",	two(0xF000, 0x5426),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fscalel",	two(0xF000, 0x4026),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fscalep",	two(0xF000, 0x4C26),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fscales",	two(0xF000, 0x4426),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fscalew",	two(0xF000, 0x5026),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fscalex",	two(0xF000, 0x0026),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fscalex",	two(0xF000, 0x4826),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
/* {"fscalex",	two(0xF000, 0x0026),	two(0xF1C0, 0xE07F),	"IiFt"}, JF */

/* $ is necessary to prevent the assembler from using PC-relative.
   If @ were used, "label: fseq label" could produce "ftrapeq",
   because "label" became "pc@label".  */
{"fseq",	two(0xF040, 0x0001),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsf",		two(0xF040, 0x0000),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsge",	two(0xF040, 0x0013),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsgl",	two(0xF040, 0x0016),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsgle",	two(0xF040, 0x0017),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsgt",	two(0xF040, 0x0012),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsle",	two(0xF040, 0x0015),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fslt",	two(0xF040, 0x0014),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsne",	two(0xF040, 0x000E),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsnge",	two(0xF040, 0x001C),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsngl",	two(0xF040, 0x0019),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsngle",	two(0xF040, 0x0018),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsngt",	two(0xF040, 0x001D),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsnle",	two(0xF040, 0x001A),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsnlt",	two(0xF040, 0x001B),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsoge",	two(0xF040, 0x0003),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsogl",	two(0xF040, 0x0006),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsogt",	two(0xF040, 0x0002),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsole",	two(0xF040, 0x0005),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsolt",	two(0xF040, 0x0004),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsor",	two(0xF040, 0x0007),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsseq",	two(0xF040, 0x0011),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fssf",	two(0xF040, 0x0010),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fssne",	two(0xF040, 0x001E),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsst",	two(0xF040, 0x001F),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fst",		two(0xF040, 0x000F),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsueq",	two(0xF040, 0x0009),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsuge",	two(0xF040, 0x000B),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsugt",	two(0xF040, 0x000A),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsule",	two(0xF040, 0x000D),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsult",	two(0xF040, 0x000C),	two(0xF1C0, 0xFFFF),	"Ii$s"},
{"fsun",	two(0xF040, 0x0008),	two(0xF1C0, 0xFFFF),	"Ii$s"},

{"fsgldivb",	two(0xF000, 0x5824),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fsgldivd",	two(0xF000, 0x5424),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fsgldivl",	two(0xF000, 0x4024),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fsgldivp",	two(0xF000, 0x4C24),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fsgldivs",	two(0xF000, 0x4424),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fsgldivw",	two(0xF000, 0x5024),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fsgldivx",	two(0xF000, 0x0024),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fsgldivx",	two(0xF000, 0x4824),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fsgldivx",	two(0xF000, 0x0024),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fsglmulb",	two(0xF000, 0x5827),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fsglmuld",	two(0xF000, 0x5427),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fsglmull",	two(0xF000, 0x4027),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fsglmulp",	two(0xF000, 0x4C27),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fsglmuls",	two(0xF000, 0x4427),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fsglmulw",	two(0xF000, 0x5027),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fsglmulx",	two(0xF000, 0x0027),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fsglmulx",	two(0xF000, 0x4827),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fsglmulx",	two(0xF000, 0x0027),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fsinb",	two(0xF000, 0x580E),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fsind",	two(0xF000, 0x540E),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fsinl",	two(0xF000, 0x400E),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fsinp",	two(0xF000, 0x4C0E),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fsins",	two(0xF000, 0x440E),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fsinw",	two(0xF000, 0x500E),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fsinx",	two(0xF000, 0x000E),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fsinx",	two(0xF000, 0x480E),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fsinx",	two(0xF000, 0x000E),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fsinhb",	two(0xF000, 0x5802),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fsinhd",	two(0xF000, 0x5402),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fsinhl",	two(0xF000, 0x4002),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fsinhp",	two(0xF000, 0x4C02),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fsinhs",	two(0xF000, 0x4402),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fsinhw",	two(0xF000, 0x5002),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fsinhx",	two(0xF000, 0x0002),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fsinhx",	two(0xF000, 0x4802),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fsinhx",	two(0xF000, 0x0002),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fsqrtb",	two(0xF000, 0x5804),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fsqrtd",	two(0xF000, 0x5404),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fsqrtl",	two(0xF000, 0x4004),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fsqrtp",	two(0xF000, 0x4C04),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fsqrts",	two(0xF000, 0x4404),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fsqrtw",	two(0xF000, 0x5004),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fsqrtx",	two(0xF000, 0x0004),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fsqrtx",	two(0xF000, 0x4804),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fsqrtx",	two(0xF000, 0x0004),	two(0xF1C0, 0xE07F),	"IiFt"},

{"fsubb",	two(0xF000, 0x5828),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"fsubd",	two(0xF000, 0x5428),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"fsubl",	two(0xF000, 0x4028),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"fsubp",	two(0xF000, 0x4C28),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"fsubs",	two(0xF000, 0x4428),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"fsubw",	two(0xF000, 0x5028),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"fsubx",	two(0xF000, 0x0028),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"fsubx",	two(0xF000, 0x4828),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"fsubx",	two(0xF000, 0x0028),	two(0xF1C0, 0xE07F),	"IiFt"},

{"ftanb",	two(0xF000, 0x580F),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"ftand",	two(0xF000, 0x540F),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"ftanl",	two(0xF000, 0x400F),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"ftanp",	two(0xF000, 0x4C0F),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"ftans",	two(0xF000, 0x440F),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"ftanw",	two(0xF000, 0x500F),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"ftanx",	two(0xF000, 0x000F),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"ftanx",	two(0xF000, 0x480F),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"ftanx",	two(0xF000, 0x000F),	two(0xF1C0, 0xE07F),	"IiFt"},

{"ftanhb",	two(0xF000, 0x5809),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"ftanhd",	two(0xF000, 0x5409),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"ftanhl",	two(0xF000, 0x4009),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"ftanhp",	two(0xF000, 0x4C09),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"ftanhs",	two(0xF000, 0x4409),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"ftanhw",	two(0xF000, 0x5009),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"ftanhx",	two(0xF000, 0x0009),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"ftanhx",	two(0xF000, 0x4809),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"ftanhx",	two(0xF000, 0x0009),	two(0xF1C0, 0xE07F),	"IiFt"},

{"ftentoxb",	two(0xF000, 0x5812),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"ftentoxd",	two(0xF000, 0x5412),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"ftentoxl",	two(0xF000, 0x4012),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"ftentoxp",	two(0xF000, 0x4C12),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"ftentoxs",	two(0xF000, 0x4412),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"ftentoxw",	two(0xF000, 0x5012),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"ftentoxx",	two(0xF000, 0x0012),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"ftentoxx",	two(0xF000, 0x4812),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"ftentoxx",	two(0xF000, 0x0012),	two(0xF1C0, 0xE07F),	"IiFt"},

{"ftrapeq",	two(0xF07C, 0x0001),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapf",	two(0xF07C, 0x0000),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapge",	two(0xF07C, 0x0013),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapgl",	two(0xF07C, 0x0016),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapgle",	two(0xF07C, 0x0017),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapgt",	two(0xF07C, 0x0012),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftraple",	two(0xF07C, 0x0015),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftraplt",	two(0xF07C, 0x0014),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapne",	two(0xF07C, 0x000E),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapnge",	two(0xF07C, 0x001C),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapngl",	two(0xF07C, 0x0019),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapngle",	two(0xF07C, 0x0018),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapngt",	two(0xF07C, 0x001D),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapnle",	two(0xF07C, 0x001A),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapnlt",	two(0xF07C, 0x001B),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapoge",	two(0xF07C, 0x0003),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapogl",	two(0xF07C, 0x0006),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapogt",	two(0xF07C, 0x0002),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapole",	two(0xF07C, 0x0005),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapolt",	two(0xF07C, 0x0004),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapor",	two(0xF07C, 0x0007),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapseq",	two(0xF07C, 0x0011),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapsf",	two(0xF07C, 0x0010),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapsne",	two(0xF07C, 0x001E),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapst",	two(0xF07C, 0x001F),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapt",	two(0xF07C, 0x000F),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapueq",	two(0xF07C, 0x0009),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapuge",	two(0xF07C, 0x000B),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapugt",	two(0xF07C, 0x000A),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapule",	two(0xF07C, 0x000D),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapult",	two(0xF07C, 0x000C),	two(0xF1FF, 0xFFFF),	"Ii"},
{"ftrapun",	two(0xF07C, 0x0008),	two(0xF1FF, 0xFFFF),	"Ii"},
        
{"ftrapeqw",	two(0xF07A, 0x0001),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapfw",	two(0xF07A, 0x0000),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapgew",	two(0xF07A, 0x0013),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapglw",	two(0xF07A, 0x0016),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapglew",	two(0xF07A, 0x0017),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapgtw",	two(0xF07A, 0x0012),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftraplew",	two(0xF07A, 0x0015),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapltw",	two(0xF07A, 0x0014),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapnew",	two(0xF07A, 0x000E),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapngew",	two(0xF07A, 0x001C),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapnglw",	two(0xF07A, 0x0019),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapnglew",	two(0xF07A, 0x0018),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapngtw",	two(0xF07A, 0x001D),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapnlew",	two(0xF07A, 0x001A),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapnltw",	two(0xF07A, 0x001B),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapogew",	two(0xF07A, 0x0003),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapoglw",	two(0xF07A, 0x0006),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapogtw",	two(0xF07A, 0x0002),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapolew",	two(0xF07A, 0x0005),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapoltw",	two(0xF07A, 0x0004),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftraporw",	two(0xF07A, 0x0007),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapseqw",	two(0xF07A, 0x0011),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapsfw",	two(0xF07A, 0x0010),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapsnew",	two(0xF07A, 0x001E),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapstw",	two(0xF07A, 0x001F),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftraptw",	two(0xF07A, 0x000F),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapueqw",	two(0xF07A, 0x0009),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapugew",	two(0xF07A, 0x000B),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapugtw",	two(0xF07A, 0x000A),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapulew",	two(0xF07A, 0x000D),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapultw",	two(0xF07A, 0x000C),	two(0xF1FF, 0xFFFF),	"Ii^w"},
{"ftrapunw",	two(0xF07A, 0x0008),	two(0xF1FF, 0xFFFF),	"Ii^w"},

{"ftrapeql",	two(0xF07B, 0x0001),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapfl",	two(0xF07B, 0x0000),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapgel",	two(0xF07B, 0x0013),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapgll",	two(0xF07B, 0x0016),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapglel",	two(0xF07B, 0x0017),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapgtl",	two(0xF07B, 0x0012),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftraplel",	two(0xF07B, 0x0015),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapltl",	two(0xF07B, 0x0014),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapnel",	two(0xF07B, 0x000E),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapngel",	two(0xF07B, 0x001C),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapngll",	two(0xF07B, 0x0019),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapnglel",	two(0xF07B, 0x0018),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapngtl",	two(0xF07B, 0x001D),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapnlel",	two(0xF07B, 0x001A),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapnltl",	two(0xF07B, 0x001B),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapogel",	two(0xF07B, 0x0003),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapogll",	two(0xF07B, 0x0006),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapogtl",	two(0xF07B, 0x0002),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapolel",	two(0xF07B, 0x0005),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapoltl",	two(0xF07B, 0x0004),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftraporl",	two(0xF07B, 0x0007),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapseql",	two(0xF07B, 0x0011),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapsfl",	two(0xF07B, 0x0010),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapsnel",	two(0xF07B, 0x001E),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapstl",	two(0xF07B, 0x001F),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftraptl",	two(0xF07B, 0x000F),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapueql",	two(0xF07B, 0x0009),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapugel",	two(0xF07B, 0x000B),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapugtl",	two(0xF07B, 0x000A),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapulel",	two(0xF07B, 0x000D),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapultl",	two(0xF07B, 0x000C),	two(0xF1FF, 0xFFFF),	"Ii^l"},
{"ftrapunl",	two(0xF07B, 0x0008),	two(0xF1FF, 0xFFFF),	"Ii^l"},

{"ftstb",	two(0xF000, 0x583A),	two(0xF1C0, 0xFC7F),	"Ii;b"},
{"ftstd",	two(0xF000, 0x543A),	two(0xF1C0, 0xFC7F),	"Ii;F"},
{"ftstl",	two(0xF000, 0x403A),	two(0xF1C0, 0xFC7F),	"Ii;l"},
{"ftstp",	two(0xF000, 0x4C3A),	two(0xF1C0, 0xFC7F),	"Ii;p"},
{"ftsts",	two(0xF000, 0x443A),	two(0xF1C0, 0xFC7F),	"Ii;f"},
{"ftstw",	two(0xF000, 0x503A),	two(0xF1C0, 0xFC7F),	"Ii;w"},
{"ftstx",	two(0xF000, 0x003A),	two(0xF1C0, 0xE07F),	"IiF8"},
{"ftstx",	two(0xF000, 0x483A),	two(0xF1C0, 0xFC7F),	"Ii;x"},

{"ftwotoxb",	two(0xF000, 0x5811),	two(0xF1C0, 0xFC7F),	"Ii;bF7"},
{"ftwotoxd",	two(0xF000, 0x5411),	two(0xF1C0, 0xFC7F),	"Ii;FF7"},
{"ftwotoxl",	two(0xF000, 0x4011),	two(0xF1C0, 0xFC7F),	"Ii;lF7"},
{"ftwotoxp",	two(0xF000, 0x4C11),	two(0xF1C0, 0xFC7F),	"Ii;pF7"},
{"ftwotoxs",	two(0xF000, 0x4411),	two(0xF1C0, 0xFC7F),	"Ii;fF7"},
{"ftwotoxw",	two(0xF000, 0x5011),	two(0xF1C0, 0xFC7F),	"Ii;wF7"},
{"ftwotoxx",	two(0xF000, 0x0011),	two(0xF1C0, 0xE07F),	"IiF8F7"},
{"ftwotoxx",	two(0xF000, 0x4811),	two(0xF1C0, 0xFC7F),	"Ii;xF7"},
{"ftwotoxx",	two(0xF000, 0x0011),	two(0xF1C0, 0xE07F),	"IiFt"},


{"fjeq",	one(0xF081),		one(0xF1FF),		"IdBc"},
{"fjf",		one(0xF080),		one(0xF1FF),		"IdBc"},
{"fjge",	one(0xF093),		one(0xF1FF),		"IdBc"},
{"fjgl",	one(0xF096),		one(0xF1FF),		"IdBc"},
{"fjgle",	one(0xF097),		one(0xF1FF),		"IdBc"},
{"fjgt",	one(0xF092),		one(0xF1FF),		"IdBc"},
{"fjle",	one(0xF095),		one(0xF1FF),		"IdBc"},
{"fjlt",	one(0xF094),		one(0xF1FF),		"IdBc"},
{"fjne",	one(0xF08E),		one(0xF1FF),		"IdBc"},
{"fjnge",	one(0xF09C),		one(0xF1FF),		"IdBc"},
{"fjngl",	one(0xF099),		one(0xF1FF),		"IdBc"},
{"fjngle",	one(0xF098),		one(0xF1FF),		"IdBc"},
{"fjngt",	one(0xF09D),		one(0xF1FF),		"IdBc"},
{"fjnle",	one(0xF09A),		one(0xF1FF),		"IdBc"},
{"fjnlt",	one(0xF09B),		one(0xF1FF),		"IdBc"},
{"fjoge",	one(0xF083),		one(0xF1FF),		"IdBc"},
{"fjogl",	one(0xF086),		one(0xF1FF),		"IdBc"},
{"fjogt",	one(0xF082),		one(0xF1FF),		"IdBc"},
{"fjole",	one(0xF085),		one(0xF1FF),		"IdBc"},
{"fjolt",	one(0xF084),		one(0xF1FF),		"IdBc"},
{"fjor",	one(0xF087),		one(0xF1FF),		"IdBc"},
{"fjseq",	one(0xF091),		one(0xF1FF),		"IdBc"},
{"fjsf",	one(0xF090),		one(0xF1FF),		"IdBc"},
{"fjsne",	one(0xF09E),		one(0xF1FF),		"IdBc"},
{"fjst",	one(0xF09F),		one(0xF1FF),		"IdBc"},
{"fjt",		one(0xF08F),		one(0xF1FF),		"IdBc"},
{"fjueq",	one(0xF089),		one(0xF1FF),		"IdBc"},
{"fjuge",	one(0xF08B),		one(0xF1FF),		"IdBc"},
{"fjugt",	one(0xF08A),		one(0xF1FF),		"IdBc"},
{"fjule",	one(0xF08D),		one(0xF1FF),		"IdBc"},
{"fjult",	one(0xF08C),		one(0xF1FF),		"IdBc"},
{"fjun",	one(0xF088),		one(0xF1FF),		"IdBc"},

/* The assembler will ignore attempts to force a short offset */

{"bhis",	one(0061000),		one(0177400),		"Bg"},
{"blss",	one(0061400),		one(0177400),		"Bg"},
{"bccs",	one(0062000),		one(0177400),		"Bg"},
{"bcss",	one(0062400),		one(0177400),		"Bg"},
{"bnes",	one(0063000),		one(0177400),		"Bg"},
{"beqs",	one(0063400),		one(0177400),		"Bg"},
{"bvcs",	one(0064000),		one(0177400),		"Bg"},
{"bvss",	one(0064400),		one(0177400),		"Bg"},
{"bpls",	one(0065000),		one(0177400),		"Bg"},
{"bmis",	one(0065400),		one(0177400),		"Bg"},
{"bges",	one(0066000),		one(0177400),		"Bg"},
{"blts",	one(0066400),		one(0177400),		"Bg"},
{"bgts",	one(0067000),		one(0177400),		"Bg"},
{"bles",	one(0067400),		one(0177400),		"Bg"},

/* Alternate mnemonics for SUN */

{"jbsr",	one(0060400),		one(0177400),		"Bg"},
{"jbsr",	one(0047200),		one(0177700),		"!s"},
{"jra",		one(0060000),		one(0177400),		"Bg"},
{"jra",		one(0047300),		one(0177700),		"!s"},
  
{"jhi",		one(0061000),		one(0177400),		"Bg"},
{"jls",		one(0061400),		one(0177400),		"Bg"},
{"jcc",		one(0062000),		one(0177400),		"Bg"},
{"jcs",		one(0062400),		one(0177400),		"Bg"},
{"jne",		one(0063000),		one(0177400),		"Bg"},
{"jeq",		one(0063400),		one(0177400),		"Bg"},
{"jvc",		one(0064000),		one(0177400),		"Bg"},
{"jvs",		one(0064400),		one(0177400),		"Bg"},
{"jpl",		one(0065000),		one(0177400),		"Bg"},
{"jmi",		one(0065400),		one(0177400),		"Bg"},
{"jge",		one(0066000),		one(0177400),		"Bg"},
{"jlt",		one(0066400),		one(0177400),		"Bg"},
{"jgt",		one(0067000),		one(0177400),		"Bg"},
{"jle",		one(0067400),		one(0177400),		"Bg"},

/* Short offsets are ignored */

{"jbsrs",	one(0060400),		one(0177400),		"Bg"},
{"jras",	one(0060000),		one(0177400),		"Bg"},
{"jhis",	one(0061000),		one(0177400),		"Bg"},
{"jlss",	one(0061400),		one(0177400),		"Bg"},
{"jccs",	one(0062000),		one(0177400),		"Bg"},
{"jcss",	one(0062400),		one(0177400),		"Bg"},
{"jnes",	one(0063000),		one(0177400),		"Bg"},
{"jeqs",	one(0063400),		one(0177400),		"Bg"},
{"jvcs",	one(0064000),		one(0177400),		"Bg"},
{"jvss",	one(0064400),		one(0177400),		"Bg"},
{"jpls",	one(0065000),		one(0177400),		"Bg"},
{"jmis",	one(0065400),		one(0177400),		"Bg"},
{"jges",	one(0066000),		one(0177400),		"Bg"},
{"jlts",	one(0066400),		one(0177400),		"Bg"},
{"jgts",	one(0067000),		one(0177400),		"Bg"},
{"jles",	one(0067400),		one(0177400),		"Bg"},

{"movql",	one(0070000),		one(0170400),		"MsDd"},
{"moveql",	one(0070000),		one(0170400),		"MsDd"},
{"moval",	one(0020100),		one(0170700),		"*lAd"},
{"movaw",	one(0030100),		one(0170700),		"*wAd"},
{"movb",	one(0010000),		one(0170000),		";b$d"},	/* mov */
{"movl",	one(0070000),		one(0170400),		"MsDd"},	/* movq written as mov */
{"movl",	one(0020000),		one(0170000),		"*l$d"},
{"movl",	one(0020100),		one(0170700),		"*lAd"},
{"movl",	one(0047140),		one(0177770),		"AsUd"},	/* mov to USP */
{"movl",	one(0047150),		one(0177770),		"UdAs"},	/* mov from USP */
{"movc",	one(0047173),		one(0177777),		"R1Jj"},
{"movc",	one(0047173),		one(0177777),		"R1#j"},
{"movc",	one(0047172),		one(0177777),		"JjR1"},
{"movc",	one(0047172),		one(0177777),		"#jR1"},
{"movml",	one(0044300),		one(0177700),		"#w&s"},	/* movm reg to mem. */
{"movml",	one(0044340),		one(0177770),		"#w-s"},	/* movm reg to autodecrement. */
{"movml",	one(0046300),		one(0177700),		"!s#w"},	/* movm mem to reg. */
{"movml",	one(0046330),		one(0177770),		"+s#w"},	/* movm autoinc to reg. */
{"movml",	one(0044300),		one(0177700),		"Lw&s"},	/* movm reg to mem. */
{"movml",	one(0044340),		one(0177770),		"lw-s"},	/* movm reg to autodecrement. */
{"movml",	one(0046300),		one(0177700),		"!sLw"},	/* movm mem to reg. */
{"movml",	one(0046330),		one(0177770),		"+sLw"},	/* movm autoinc to reg. */
{"movmw",	one(0044200),		one(0177700),		"#w&s"},	/* movm reg to mem. */
{"movmw",	one(0044240),		one(0177770),		"#w-s"},	/* movm reg to autodecrement. */
{"movmw",	one(0046200),		one(0177700),		"!s#w"},	/* movm mem to reg. */
{"movmw",	one(0046230),		one(0177770),		"+s#w"},	/* movm autoinc to reg. */
{"movmw",	one(0044200),		one(0177700),		"Lw&s"},	/* movm reg to mem. */
{"movmw",	one(0044240),		one(0177770),		"lw-s"},	/* movm reg to autodecrement. */
{"movmw",	one(0046200),		one(0177700),		"!sLw"},	/* movm mem to reg. */
{"movmw",	one(0046230),		one(0177770),		"+sLw"},	/* movm autoinc to reg. */
{"movpl",	one(0000510),		one(0170770),		"dsDd"},	/* memory to register */
{"movpl",	one(0000710),		one(0170770),		"Ddds"},	/* register to memory */
{"movpw",	one(0000410),		one(0170770),		"dsDd"},	/* memory to register */
{"movpw",	one(0000610),		one(0170770),		"Ddds"},	/* register to memory */
{"movq",	one(0070000),		one(0170400),		"MsDd"},
{"movw",	one(0030000),		one(0170000),		"*w$d"},
{"movw",	one(0030100),		one(0170700),		"*wAd"},	/* mova,	written as mov */
{"movw",	one(0040300),		one(0177700),		"Ss$s"},	/* Move from sr */
{"movw",	one(0041300),		one(0177700),		"Cs$s"},	/* Move from ccr */
{"movw",	one(0042300),		one(0177700),		";wCd"},	/* mov to ccr */
{"movw",	one(0043300),		one(0177700),		";wSd"},	/* mov to sr */

{"movsb",	two(0007000, 0),	two(0177700, 07777),	"~sR1"},
{"movsb",	two(0007000, 04000),	two(0177700, 07777),	"R1~s"},
{"movsl",	two(0007200, 0),	two(0177700, 07777),	"~sR1"},
{"movsl",	two(0007200, 04000),	two(0177700, 07777),	"R1~s"},
{"movsw",	two(0007100, 0),	two(0177700, 07777),	"~sR1"},
{"movsw",	two(0007100, 04000),	two(0177700, 07777),	"R1~s"},

#ifdef m68851
 /* name */	/* opcode */		/* match */		/* args */

{"pbac",	one(0xf0c7),		one(0xffbf),		"Bc"},
{"pbacw",	one(0xf087),		one(0xffbf),		"Bc"},
{"pbas",	one(0xf0c6),		one(0xffbf),		"Bc"},
{"pbasw",	one(0xf086),		one(0xffbf),		"Bc"},
{"pbbc",	one(0xf0c1),		one(0xffbf),		"Bc"},
{"pbbcw",	one(0xf081),		one(0xffbf),		"Bc"},
{"pbbs",	one(0xf0c0),		one(0xffbf),		"Bc"},
{"pbbsw",	one(0xf080),		one(0xffbf),		"Bc"},
{"pbcc",	one(0xf0cf),		one(0xffbf),		"Bc"},
{"pbccw",	one(0xf08f),		one(0xffbf),		"Bc"},
{"pbcs",	one(0xf0ce),		one(0xffbf),		"Bc"},
{"pbcsw",	one(0xf08e),		one(0xffbf),		"Bc"},
{"pbgc",	one(0xf0cd),		one(0xffbf),		"Bc"},
{"pbgcw",	one(0xf08d),		one(0xffbf),		"Bc"},
{"pbgs",	one(0xf0cc),		one(0xffbf),		"Bc"},
{"pbgsw",	one(0xf08c),		one(0xffbf),		"Bc"},
{"pbic",	one(0xf0cb),		one(0xffbf),		"Bc"},
{"pbicw",	one(0xf08b),		one(0xffbf),		"Bc"},
{"pbis",	one(0xf0ca),		one(0xffbf),		"Bc"},
{"pbisw",	one(0xf08a),		one(0xffbf),		"Bc"},
{"pblc",	one(0xf0c3),		one(0xffbf),		"Bc"},
{"pblcw",	one(0xf083),		one(0xffbf),		"Bc"},
{"pbls",	one(0xf0c2),		one(0xffbf),		"Bc"},
{"pblsw",	one(0xf082),		one(0xffbf),		"Bc"},
{"pbsc",	one(0xf0c5),		one(0xffbf),		"Bc"},
{"pbscw",	one(0xf085),		one(0xffbf),		"Bc"},
{"pbss",	one(0xf0c4),		one(0xffbf),		"Bc"},
{"pbssw",	one(0xf084),		one(0xffbf),		"Bc"},
{"pbwc",	one(0xf0c9),		one(0xffbf),		"Bc"},
{"pbwcw",	one(0xf089),		one(0xffbf),		"Bc"},
{"pbws",	one(0xf0c8),		one(0xffbf),		"Bc"},
{"pbwsw",	one(0xf088),		one(0xffbf),		"Bc"},


{"pdbac",	two(0xf048, 0x0007),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbas",	two(0xf048, 0x0006),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbbc",	two(0xf048, 0x0001),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbbs",	two(0xf048, 0x0000),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbcc",	two(0xf048, 0x000f),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbcs",	two(0xf048, 0x000e),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbgc",	two(0xf048, 0x000d),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbgs",	two(0xf048, 0x000c),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbic",	two(0xf048, 0x000b),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbis",	two(0xf048, 0x000a),	two(0xfff8, 0xffff),	"DsBw"},
{"pdblc",	two(0xf048, 0x0003),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbls",	two(0xf048, 0x0002),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbsc",	two(0xf048, 0x0005),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbss",	two(0xf048, 0x0004),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbwc",	two(0xf048, 0x0009),	two(0xfff8, 0xffff),	"DsBw"},
{"pdbws",	two(0xf048, 0x0008),	two(0xfff8, 0xffff),	"DsBw"},

{"pflusha",	two(0xf000, 0x2400),	two(0xffff, 0xffff),	"" },

{"pflush",	two(0xf000, 0x3010),	two(0xffc0, 0xfe10),	"T3T9" },
{"pflush",	two(0xf000, 0x3810),	two(0xffc0, 0xfe10),	"T3T9&s" },
{"pflush",	two(0xf000, 0x3008),	two(0xffc0, 0xfe18),	"D3T9" },
{"pflush",	two(0xf000, 0x3808),	two(0xffc0, 0xfe18),	"D3T9&s" },
{"pflush",	two(0xf000, 0x3000),	two(0xffc0, 0xfe1e),	"f3T9" },
{"pflush",	two(0xf000, 0x3800),	two(0xffc0, 0xfe1e),	"f3T9&s" },

{"pflushs",	two(0xf000, 0x3410),	two(0xfff8, 0xfe10),	"T3T9" },
{"pflushs",	two(0xf000, 0x3c10),	two(0xfff8, 0xfe00),	"T3T9&s" },
{"pflushs",	two(0xf000, 0x3408),	two(0xfff8, 0xfe18),	"D3T9" },
{"pflushs",	two(0xf000, 0x3c08),	two(0xfff8, 0xfe18),	"D3T9&s" },
{"pflushs",	two(0xf000, 0x3400),	two(0xfff8, 0xfe1e),	"f3T9" },
{"pflushs",	two(0xf000, 0x3c00),	two(0xfff8, 0xfe1e),	"f3T9&s"},

{"pflushr",	two(0xf000, 0xa000),	two(0xffc0, 0xffff),	"|s" },

{"ploadr",	two(0xf000, 0x2210),	two(0xffc0, 0xfff0),	"T3&s" },
{"ploadr",	two(0xf000, 0x2208),	two(0xffc0, 0xfff8),	"D3&s" },
{"ploadr",	two(0xf000, 0x2200),	two(0xffc0, 0xfffe),	"f3&s" },
{"ploadw",	two(0xf000, 0x2010),	two(0xffc0, 0xfff0),	"T3&s" },
{"ploadw",	two(0xf000, 0x2008),	two(0xffc0, 0xfff8),	"D3&s" },
{"ploadw",	two(0xf000, 0x2000),	two(0xffc0, 0xfffe),	"f3&s" },

/* TC, CRP, DRP, SRP, CAL, VAL, SCC, AC */
{"pmove",	two(0xf000, 0x4000),	two(0xffc0, 0xe3ff),	"*sP8" },
{"pmove",	two(0xf000, 0x4200),	two(0xffc0, 0xe3ff),	"P8%s" },
{"pmove",	two(0xf000, 0x4000),	two(0xffc0, 0xe3ff),	"|sW8" },
{"pmove",	two(0xf000, 0x4200),	two(0xffc0, 0xe3ff),	"W8~s" },

/* BADx, BACx */
{"pmove",	two(0xf000, 0x6200),	two(0xffc0, 0xe3e3),	"*sX3" },
{"pmove",	two(0xf000, 0x6000),	two(0xffc0, 0xe3e3),	"X3%s" },

/* PSR, PCSR */
/* {"pmove",	two(0xf000, 0x6100),	two(oxffc0, oxffff),	"*sZ8" }, */
{"pmove",	two(0xf000, 0x6000),	two(0xffc0, 0xffff),	"*sY8" },
{"pmove",	two(0xf000, 0x6200),	two(0xffc0, 0xffff),	"Y8%s" },
{"pmove",	two(0xf000, 0x6600),	two(0xffc0, 0xffff),	"Z8%s" },

{"prestore",	one(0xf140),		one(0xffc0),		"&s"},
{"prestore",	one(0xf158),		one(0xfff8),		"+s"},
{"psave",	one(0xf100),		one(0xffc0),		"&s"},
{"psave",	one(0xf100),		one(0xffc0),		"+s"},

{"psac",	two(0xf040, 0x0007),	two(0xffc0, 0xffff),	"@s"},
{"psas",	two(0xf040, 0x0006),	two(0xffc0, 0xffff),	"@s"},
{"psbc",	two(0xf040, 0x0001),	two(0xffc0, 0xffff),	"@s"},
{"psbs",	two(0xf040, 0x0000),	two(0xffc0, 0xffff),	"@s"},
{"pscc",	two(0xf040, 0x000f),	two(0xffc0, 0xffff),	"@s"},
{"pscs",	two(0xf040, 0x000e),	two(0xffc0, 0xffff),	"@s"},
{"psgc",	two(0xf040, 0x000d),	two(0xffc0, 0xffff),	"@s"},
{"psgs",	two(0xf040, 0x000c),	two(0xffc0, 0xffff),	"@s"},
{"psic",	two(0xf040, 0x000b),	two(0xffc0, 0xffff),	"@s"},
{"psis",	two(0xf040, 0x000a),	two(0xffc0, 0xffff),	"@s"},
{"pslc",	two(0xf040, 0x0003),	two(0xffc0, 0xffff),	"@s"},
{"psls",	two(0xf040, 0x0002),	two(0xffc0, 0xffff),	"@s"},
{"pssc",	two(0xf040, 0x0005),	two(0xffc0, 0xffff),	"@s"},
{"psss",	two(0xf040, 0x0004),	two(0xffc0, 0xffff),	"@s"},
{"pswc",	two(0xf040, 0x0009),	two(0xffc0, 0xffff),	"@s"},
{"psws",	two(0xf040, 0x0008),	two(0xffc0, 0xffff),	"@s"},

{"ptestr",	two(0xf000, 0x8210),	two(0xffc0, 0xe3f0),	"T3&sQ8" },
{"ptestr",	two(0xf000, 0x8310),	two(0xffc0, 0xe310),	"T3&sQ8A9" },
{"ptestr",	two(0xf000, 0x8208),	two(0xffc0, 0xe3f8),	"D3&sQ8" },
{"ptestr",	two(0xf000, 0x8308),	two(0xffc0, 0xe318),	"D3&sQ8A9" },
{"ptestr",	two(0xf000, 0x8200),	two(0xffc0, 0xe3fe),	"f3&sQ8" },
{"ptestr",	two(0xf000, 0x8300),	two(0xffc0, 0xe31e),	"f3&sQ8A9" },

{"ptestw",	two(0xf000, 0x8010),	two(0xffc0, 0xe3f0),	"T3&sQ8" },
{"ptestw",	two(0xf000, 0x8110),	two(0xffc0, 0xe310),	"T3&sQ8A9" },
{"ptestw",	two(0xf000, 0x8008),	two(0xffc0, 0xe3f8),	"D3&sQ8" },
{"ptestw",	two(0xf000, 0x8108),	two(0xffc0, 0xe318),	"D3&sQ8A9" },
{"ptestw",	two(0xf000, 0x8000),	two(0xffc0, 0xe3fe),	"f3&sQ8" },
{"ptestw",	two(0xf000, 0x8100),	two(0xffc0, 0xe31e),	"f3&sQ8A9" },

{"ptrapacw",	two(0xf07a, 0x0007),	two(0xffff, 0xffff),	"#w"},
{"ptrapacl",	two(0xf07b, 0x0007),	two(0xffff, 0xffff),	"#l"},
{"ptrapac",	two(0xf07c, 0x0007),	two(0xffff, 0xffff),	""},

{"ptrapasw",	two(0xf07a, 0x0006),	two(0xffff, 0xffff),	"#w"},
{"ptrapasl",	two(0xf07b, 0x0006),	two(0xffff, 0xffff),	"#l"},
{"ptrapas",	two(0xf07c, 0x0006),	two(0xffff, 0xffff),	""},

{"ptrapbcw",	two(0xf07a, 0x0001),	two(0xffff, 0xffff),	"#w"},
{"ptrapbcl",	two(0xf07b, 0x0001),	two(0xffff, 0xffff),	"#l"},
{"ptrapbc",	two(0xf07c, 0x0001),	two(0xffff, 0xffff),	""},

{"ptrapbsw",	two(0xf07a, 0x0000),	two(0xffff, 0xffff),	"#w"},
{"ptrapbsl",	two(0xf07b, 0x0000),	two(0xffff, 0xffff),	"#l"},
{"ptrapbs",	two(0xf07c, 0x0000),	two(0xffff, 0xffff),	""},

{"ptrapccw",	two(0xf07a, 0x000f),	two(0xffff, 0xffff),	"#w"},
{"ptrapccl",	two(0xf07b, 0x000f),	two(0xffff, 0xffff),	"#l"},
{"ptrapcc",	two(0xf07c, 0x000f),	two(0xffff, 0xffff),	""},

{"ptrapcsw",	two(0xf07a, 0x000e),	two(0xffff, 0xffff),	"#w"},
{"ptrapcsl",	two(0xf07b, 0x000e),	two(0xffff, 0xffff),	"#l"},
{"ptrapcs",	two(0xf07c, 0x000e),	two(0xffff, 0xffff),	""},

{"ptrapgcw",	two(0xf07a, 0x000d),	two(0xffff, 0xffff),	"#w"},
{"ptrapgcl",	two(0xf07b, 0x000d),	two(0xffff, 0xffff),	"#l"},
{"ptrapgc",	two(0xf07c, 0x000d),	two(0xffff, 0xffff),	""},

{"ptrapgsw",	two(0xf07a, 0x000c),	two(0xffff, 0xffff),	"#w"},
{"ptrapgsl",	two(0xf07b, 0x000c),	two(0xffff, 0xffff),	"#l"},
{"ptrapgs",	two(0xf07c, 0x000c),	two(0xffff, 0xffff),	""},

{"ptrapicw",	two(0xf07a, 0x000b),	two(0xffff, 0xffff),	"#w"},
{"ptrapicl",	two(0xf07b, 0x000b),	two(0xffff, 0xffff),	"#l"},
{"ptrapic",	two(0xf07c, 0x000b),	two(0xffff, 0xffff),	""},

{"ptrapisw",	two(0xf07a, 0x000a),	two(0xffff, 0xffff),	"#w"},
{"ptrapisl",	two(0xf07b, 0x000a),	two(0xffff, 0xffff),	"#l"},
{"ptrapis",	two(0xf07c, 0x000a),	two(0xffff, 0xffff),	""},

{"ptraplcw",	two(0xf07a, 0x0003),	two(0xffff, 0xffff),	"#w"},
{"ptraplcl",	two(0xf07b, 0x0003),	two(0xffff, 0xffff),	"#l"},
{"ptraplc",	two(0xf07c, 0x0003),	two(0xffff, 0xffff),	""},

{"ptraplsw",	two(0xf07a, 0x0002),	two(0xffff, 0xffff),	"#w"},
{"ptraplsl",	two(0xf07b, 0x0002),	two(0xffff, 0xffff),	"#l"},
{"ptrapls",	two(0xf07c, 0x0002),	two(0xffff, 0xffff),	""},

{"ptrapscw",	two(0xf07a, 0x0005),	two(0xffff, 0xffff),	"#w"},
{"ptrapscl",	two(0xf07b, 0x0005),	two(0xffff, 0xffff),	"#l"},
{"ptrapsc",	two(0xf07c, 0x0005),	two(0xffff, 0xffff),	""},

{"ptrapssw",	two(0xf07a, 0x0004),	two(0xffff, 0xffff),	"#w"},
{"ptrapssl",	two(0xf07b, 0x0004),	two(0xffff, 0xffff),	"#l"},
{"ptrapss",	two(0xf07c, 0x0004),	two(0xffff, 0xffff),	""},

{"ptrapwcw",	two(0xf07a, 0x0009),	two(0xffff, 0xffff),	"#w"},
{"ptrapwcl",	two(0xf07b, 0x0009),	two(0xffff, 0xffff),	"#l"},
{"ptrapwc",	two(0xf07c, 0x0009),	two(0xffff, 0xffff),	""},

{"ptrapwsw",	two(0xf07a, 0x0008),	two(0xffff, 0xffff),	"#w"},
{"ptrapwsl",	two(0xf07b, 0x0008),	two(0xffff, 0xffff),	"#l"},
{"ptrapws",	two(0xf07c, 0x0008),	two(0xffff, 0xffff),	""},

{"pvalid",	two(0xf000, 0x2800),	two(0xffc0, 0xffff),	"Vs&s"},
{"pvalid",	two(0xf000, 0x2c00),	two(0xffc0, 0xfff8),	"A3&s" },

#endif /* m68851 */

};

int numopcodes=sizeof(m68k_opcodes)/sizeof(m68k_opcodes[0]);

struct m68k_opcode *endop = m68k_opcodes+sizeof(m68k_opcodes)/sizeof(m68k_opcodes[0]);
