
/* Table of opcodes for the sparc.
	Copyright 1989, 1991, 1992 Free Software Foundation, Inc.

This file is part of the BFD library.

BFD is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BFD is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.	*/

/* FIXME-someday: perhaps the ,a's and such should be embedded in the
   instruction's name rather than the args.  This would make gas faster, pinsn
   slower, but would mess up some macros a bit.  xoxorich. */


#include <stdio.h>
#include "ansidecl.h"
#define BFD_EMIT_TABLE
#include "opcode/sparc.h"

const char *architecture_pname[] = {
	"v6",
	"v7",
	"v8",
	"sparclite",
	NULL,
};


#define COND(x)		(((x)&0xf)<<25)

#define CONDA	(COND(0x8))
#define CONDCC	(COND(0xd))
#define CONDCS	(COND(0x5))
#define CONDE	(COND(0x1))
#define CONDG	(COND(0xa))
#define CONDGE	(COND(0xb))
#define CONDGU	(COND(0xc))
#define CONDL	(COND(0x3))
#define CONDLE	(COND(0x2))
#define CONDLEU	(COND(0x4))
#define CONDN	(COND(0x0))
#define CONDNE	(COND(0x9))
#define CONDNEG	(COND(0x6))
#define CONDPOS	(COND(0xe))
#define CONDVC	(COND(0xf))
#define CONDVS	(COND(0x7))

#define CONDNZ	CONDNE
#define CONDZ	CONDE
#define CONDGEU	CONDCC
#define CONDLU	CONDCS

#define FCONDA		(COND(0x8))
#define FCONDE		(COND(0x9))
#define FCONDG		(COND(0x6))
#define FCONDGE		(COND(0xb))
#define FCONDL		(COND(0x4))
#define FCONDLE		(COND(0xd))
#define FCONDLG		(COND(0x2))
#define FCONDN		(COND(0x0))
#define FCONDNE		(COND(0x1))
#define FCONDO		(COND(0xf))
#define FCONDU		(COND(0x7))
#define FCONDUE		(COND(0xa))
#define FCONDUG		(COND(0x5))
#define FCONDUGE	(COND(0xc))
#define FCONDUL		(COND(0x3))
#define FCONDULE	(COND(0xe))

#define FCONDNZ	FCONDNE
#define FCONDZ	FCONDE


/* The order of the opcodes in the table is significant:
	
	* The assembler requires that all instances of the same mnemonic must
	be consecutive.	If they aren't, the assembler will bomb at runtime.

	* The disassembler should not care about the order of the opcodes.

*/

struct sparc_opcode sparc_opcodes[] = {

{ "ld",	F3(3, 0x00, 0), F3(~3, ~0x00, ~0),		"[1+2],d", 0, v6 },
{ "ld",	F3(3, 0x00, 0), F3(~3, ~0x00, ~0)|RS2_G0,	"[1],d", 0, v6 }, /* ld [rs1+%g0],d */
{ "ld",	F3(3, 0x00, 1), F3(~3, ~0x00, ~1),		"[1+i],d", 0, v6 },
{ "ld",	F3(3, 0x00, 1), F3(~3, ~0x00, ~1),		"[i+1],d", 0, v6 },
{ "ld",	F3(3, 0x00, 1), F3(~3, ~0x00, ~1)|RS1_G0,	"[i],d", 0, v6 },
{ "ld",	F3(3, 0x00, 1), F3(~3, ~0x00, ~1)|SIMM13(~0),	"[1],d", 0, v6 }, /* ld [rs1+0],d */
{ "ld",	F3(3, 0x20, 0), F3(~3, ~0x20, ~0),		"[1+2],g", 0, v6 },
{ "ld",	F3(3, 0x20, 0), F3(~3, ~0x20, ~0)|RS2_G0,	"[1],g", 0, v6 }, /* ld [rs1+%g0],d */
{ "ld",	F3(3, 0x20, 1), F3(~3, ~0x20, ~1),		"[1+i],g", 0, v6 },
{ "ld",	F3(3, 0x20, 1), F3(~3, ~0x20, ~1),		"[i+1],g", 0, v6 },
{ "ld",	F3(3, 0x20, 1), F3(~3, ~0x20, ~1)|RS1_G0,	"[i],g", 0, v6 },
{ "ld",	F3(3, 0x20, 1), F3(~3, ~0x20, ~1)|SIMM13(~0),	"[1],g", 0, v6 }, /* ld [rs1+0],d */

{ "ld",	F3(3, 0x21, 0), F3(~3, ~0x21, ~0)|RD(~0),	"[1+2],F", 0, v6 },
{ "ld",	F3(3, 0x21, 0), F3(~3, ~0x21, ~0)|RS2_G0|RD(~0),"[1],F", 0, v6 }, /* ld [rs1+%g0],d */
{ "ld",	F3(3, 0x21, 1), F3(~3, ~0x21, ~1)|RD(~0),	"[1+i],F", 0, v6 },
{ "ld",	F3(3, 0x21, 1), F3(~3, ~0x21, ~1)|RD(~0),	"[i+1],F", 0, v6 },
{ "ld",	F3(3, 0x21, 1), F3(~3, ~0x21, ~1)|RS1_G0|RD(~0),"[i],F", 0, v6 },
{ "ld",	F3(3, 0x21, 1), F3(~3, ~0x21, ~1)|SIMM13(~0)|RD(~0),"[1],F", 0, v6 }, /* ld [rs1+0],d */

{ "ld",	F3(3, 0x30, 0), F3(~3, ~0x30, ~0),		"[1+2],D", F_ALIAS, v6 },
{ "ld",	F3(3, 0x30, 0), F3(~3, ~0x30, ~0)|RS2_G0,	"[1],D", F_ALIAS, v6 }, /* ld [rs1+%g0],d */
{ "ld",	F3(3, 0x30, 1), F3(~3, ~0x30, ~1),		"[1+i],D", F_ALIAS, v6 },
{ "ld",	F3(3, 0x30, 1), F3(~3, ~0x30, ~1),		"[i+1],D", F_ALIAS, v6 },
{ "ld",	F3(3, 0x30, 1), F3(~3, ~0x30, ~1)|RS1_G0,	"[i],D", F_ALIAS, v6 },
{ "ld",	F3(3, 0x30, 1), F3(~3, ~0x30, ~1)|SIMM13(~0),	"[1],D", F_ALIAS, v6 }, /* ld [rs1+0],d */
{ "ld",	F3(3, 0x31, 0), F3(~3, ~0x31, ~0),		"[1+2],C", 0, v6 },
{ "ld",	F3(3, 0x31, 0), F3(~3, ~0x31, ~0)|RS2_G0,	"[1],C", 0, v6 }, /* ld [rs1+%g0],d */
{ "ld",	F3(3, 0x31, 1), F3(~3, ~0x31, ~1),		"[1+i],C", 0, v6 },
{ "ld",	F3(3, 0x31, 1), F3(~3, ~0x31, ~1),		"[i+1],C", 0, v6 },
{ "ld",	F3(3, 0x31, 1), F3(~3, ~0x31, ~1)|RS1_G0,	"[i],C", 0, v6 },
{ "ld",	F3(3, 0x31, 1), F3(~3, ~0x31, ~1)|SIMM13(~0),	"[1],C", 0, v6 }, /* ld [rs1+0],d */


{ "ldd",	F3(3, 0x03, 0), F3(~3, ~0x03, ~0)|ASI(~0),	"[1+2],d", 0, v6 },
{ "ldd",	F3(3, 0x03, 0), F3(~3, ~0x03, ~0)|ASI_RS2(~0),	"[1],d", 0, v6 }, /* ldd [rs1+%g0],d */
{ "ldd",	F3(3, 0x03, 1), F3(~3, ~0x03, ~1),		"[1+i],d", 0, v6 },
{ "ldd",	F3(3, 0x03, 1), F3(~3, ~0x03, ~1),		"[i+1],d", 0, v6 },
{ "ldd",	F3(3, 0x03, 1), F3(~3, ~0x03, ~1)|RS1_G0,	"[i],d", 0, v6 },
{ "ldd",	F3(3, 0x03, 1), F3(~3, ~0x03, ~1)|SIMM13(~0),	"[1],d", 0, v6 }, /* ldd [rs1+0],d */
{ "ldd",	F3(3, 0x23, 0), F3(~3, ~0x23, ~0)|ASI(~0),	"[1+2],H", 0, v6 },
{ "ldd",	F3(3, 0x23, 0), F3(~3, ~0x23, ~0)|ASI_RS2(~0),	"[1],H", 0, v6 }, /* ldd [rs1+%g0],d */
{ "ldd",	F3(3, 0x23, 1), F3(~3, ~0x23, ~1),		"[1+i],H", 0, v6 },
{ "ldd",	F3(3, 0x23, 1), F3(~3, ~0x23, ~1),		"[i+1],H", 0, v6 },
{ "ldd",	F3(3, 0x23, 1), F3(~3, ~0x23, ~1)|RS1_G0,	"[i],H", 0, v6 },
{ "ldd",	F3(3, 0x23, 1), F3(~3, ~0x23, ~1)|SIMM13(~0),	"[1],H", 0, v6 }, /* ldd [rs1+0],d */
{ "ldd",	F3(3, 0x33, 0), F3(~3, ~0x33, ~0)|ASI(~0),	"[1+2],D", F_ALIAS, v6 },
{ "ldd",	F3(3, 0x33, 0), F3(~3, ~0x33, ~0)|ASI_RS2(~0),	"[1],D", F_ALIAS, v6 }, /* ldd [rs1+%g0],d */
{ "ldd",	F3(3, 0x33, 1), F3(~3, ~0x33, ~1),		"[1+i],D", F_ALIAS, v6 },
{ "ldd",	F3(3, 0x33, 1), F3(~3, ~0x33, ~1),		"[i+1],D", F_ALIAS, v6 },
{ "ldd",	F3(3, 0x33, 1), F3(~3, ~0x33, ~1)|RS1_G0,	"[i],D", F_ALIAS, v6 },
{ "ldd",	F3(3, 0x33, 1), F3(~3, ~0x33, ~1)|SIMM13(~0),	"[1],D", F_ALIAS, v6 }, /* ldd [rs1+0],d */


{ "ldsb",	F3(3, 0x09, 0), F3(~3, ~0x09, ~0)|ASI(~0),	"[1+2],d", 0, v6 },
{ "ldsb",	F3(3, 0x09, 0), F3(~3, ~0x09, ~0)|ASI_RS2(~0),	"[1],d", 0, v6 }, /* ldsb [rs1+%g0],d */
{ "ldsb",	F3(3, 0x09, 1), F3(~3, ~0x09, ~1),		"[1+i],d", 0, v6 },
{ "ldsb",	F3(3, 0x09, 1), F3(~3, ~0x09, ~1),		"[i+1],d", 0, v6 },
{ "ldsb",	F3(3, 0x09, 1), F3(~3, ~0x09, ~1)|RS1_G0,	"[i],d", 0, v6 },
{ "ldsb",	F3(3, 0x09, 1), F3(~3, ~0x09, ~1)|SIMM13(~0),	"[1],d", 0, v6 }, /* ldsb [rs1+0],d */

{ "ldsh",	F3(3, 0x0a, 0), F3(~3, ~0x0a, ~0)|ASI_RS2(~0),	"[1],d", 0, v6 }, /* ldsh [rs1+%g0],d */
{ "ldsh",	F3(3, 0x0a, 0), F3(~3, ~0x0a, ~0)|ASI(~0),	"[1+2],d", 0, v6 },
{ "ldsh",	F3(3, 0x0a, 1), F3(~3, ~0x0a, ~1),		"[1+i],d", 0, v6 },
{ "ldsh",	F3(3, 0x0a, 1), F3(~3, ~0x0a, ~1),		"[i+1],d", 0, v6 },
{ "ldsh",	F3(3, 0x0a, 1), F3(~3, ~0x0a, ~1)|RS1_G0,	"[i],d", 0, v6 },
{ "ldsh",	F3(3, 0x0a, 1), F3(~3, ~0x0a, ~1)|SIMM13(~0),	"[1],d", 0, v6 }, /* ldsh [rs1+0],d */

{ "ldstub",	F3(3, 0x0d, 0), F3(~3, ~0x0d, ~0)|ASI(~0),	"[1+2],d", 0, v6 },
{ "ldstub",	F3(3, 0x0d, 0), F3(~3, ~0x0d, ~0)|ASI_RS2(~0),	"[1],d", 0, v6 }, /* ldstub [rs1+%g0],d */
{ "ldstub",	F3(3, 0x0d, 1), F3(~3, ~0x0d, ~1),		"[1+i],d", 0, v6 },
{ "ldstub",	F3(3, 0x0d, 1), F3(~3, ~0x0d, ~1),		"[i+1],d", 0, v6 },
{ "ldstub",	F3(3, 0x0d, 1), F3(~3, ~0x0d, ~1)|RS1_G0,	"[i],d", 0, v6 },
{ "ldstub",	F3(3, 0x0d, 1), F3(~3, ~0x0d, ~1)|SIMM13(~0),	"[1],d", 0, v6 }, /* ldstub [rs1+0],d */


{ "ldub",	F3(3, 0x01, 0), F3(~3, ~0x01, ~0)|ASI(~0),	"[1+2],d", 0, v6 },
{ "ldub",	F3(3, 0x01, 0), F3(~3, ~0x01, ~0)|ASI_RS2(~0),	"[1],d", 0, v6 }, /* ldub [rs1+%g0],d */
{ "ldub",	F3(3, 0x01, 1), F3(~3, ~0x01, ~1),		"[1+i],d", 0, v6 },
{ "ldub",	F3(3, 0x01, 1), F3(~3, ~0x01, ~1),		"[i+1],d", 0, v6 },
{ "ldub",	F3(3, 0x01, 1), F3(~3, ~0x01, ~1)|RS1_G0,	"[i],d", 0, v6 },
{ "ldub",	F3(3, 0x01, 1), F3(~3, ~0x01, ~1)|SIMM13(~0),	"[1],d", 0, v6 }, /* ldub [rs1+0],d */

{ "lduh",	F3(3, 0x02, 0), F3(~3, ~0x02, ~0)|ASI(~0),	"[1+2],d", 0, v6 },
{ "lduh",	F3(3, 0x02, 0), F3(~3, ~0x02, ~0)|ASI_RS2(~0),	"[1],d", 0, v6 }, /* lduh [rs1+%g0],d */
{ "lduh",	F3(3, 0x02, 1), F3(~3, ~0x02, ~1),		"[1+i],d", 0, v6 },
{ "lduh",	F3(3, 0x02, 1), F3(~3, ~0x02, ~1),		"[i+1],d", 0, v6 },
{ "lduh",	F3(3, 0x02, 1), F3(~3, ~0x02, ~1)|RS1_G0,	"[i],d", 0, v6 },
{ "lduh",	F3(3, 0x02, 1), F3(~3, ~0x02, ~1)|SIMM13(~0),	"[1],d", 0, v6 }, /* lduh [rs1+0],d */



{ "lda",	F3(3, 0x10, 0), F3(~3, ~0x10, ~0),		"[1+2]A,d", 0, v6 },
{ "lda",	F3(3, 0x10, 0), F3(~3, ~0x10, ~0)|RS2_G0,	"[1]A,d", 0, v6 }, /* lda [rs1+%g0],d */

{ "ldda",	F3(3, 0x13, 0), F3(~3, ~0x13, ~0),		"[1+2]A,d", 0, v6 },
{ "ldda",	F3(3, 0x13, 0), F3(~3, ~0x13, ~0)|RS2_G0,	"[1]A,d", 0, v6 }, /* ldda [rs1+%g0],d */



{ "ldsba",	F3(3, 0x19, 0), F3(~3, ~0x19, ~0),		"[1+2]A,d", 0, v6 },
{ "ldsba",	F3(3, 0x19, 0), F3(~3, ~0x19, ~0)|RS2_G0,	"[1]A,d", 0, v6 }, /* ldsba [rs1+%g0],d */

{ "ldsha",	F3(3, 0x1a, 0), F3(~3, ~0x1a, ~0),		"[1+2]A,d", 0, v6 },
{ "ldsha",	F3(3, 0x1a, 0), F3(~3, ~0x1a, ~0)|RS2_G0,	"[1]A,d", 0, v6 }, /* ldsha [rs1+%g0],d */

{ "ldstuba",	F3(3, 0x1d, 0), F3(~3, ~0x1d, ~0),		"[1+2]A,d", 0, v6 },
{ "ldstuba",	F3(3, 0x1d, 0), F3(~3, ~0x1d, ~0)|RS2_G0,	"[1]A,d", 0, v6 }, /* ldstuba [rs1+%g0],d */


{ "lduba",	F3(3, 0x11, 0), F3(~3, ~0x11, ~0),		"[1+2]A,d", 0, v6 },
{ "lduba",	F3(3, 0x11, 0), F3(~3, ~0x11, ~0)|RS2_G0,	"[1]A,d", 0, v6 }, /* lduba [rs1+%g0],d */

{ "lduha",	F3(3, 0x12, 0), F3(~3, ~0x12, ~0),		"[1+2]A,d", 0, v6 },
{ "lduha",	F3(3, 0x12, 0), F3(~3, ~0x12, ~0)|RS2_G0,	"[1]A,d", 0, v6 }, /* lduha [rs1+%g0],d */



{ "st",	F3(3, 0x04, 0), F3(~3, ~0x04, ~0)|ASI(~0),		"d,[1+2]", 0, v6 },
{ "st",	F3(3, 0x04, 0), F3(~3, ~0x04, ~0)|ASI_RS2(~0),		"d,[1]", 0, v6 }, /* st d,[rs1+%g0] */
{ "st",	F3(3, 0x04, 1), F3(~3, ~0x04, ~1),			"d,[1+i]", 0, v6 },
{ "st",	F3(3, 0x04, 1), F3(~3, ~0x04, ~1),			"d,[i+1]", 0, v6 },
{ "st",	F3(3, 0x04, 1), F3(~3, ~0x04, ~1)|RS1_G0,		"d,[i]", 0, v6 },
{ "st",	F3(3, 0x04, 1), F3(~3, ~0x04, ~1)|SIMM13(~0),		"d,[1]", 0, v6 }, /* st d,[rs1+0] */
{ "st",	F3(3, 0x24, 0), F3(~3, ~0x24, ~0)|ASI(~0),		"g,[1+2]", 0, v6 },
{ "st",	F3(3, 0x24, 0), F3(~3, ~0x24, ~0)|ASI_RS2(~0),		"g,[1]", 0, v6 }, /* st d[rs1+%g0] */
{ "st",	F3(3, 0x24, 1), F3(~3, ~0x24, ~1),			"g,[1+i]", 0, v6 },
{ "st",	F3(3, 0x24, 1), F3(~3, ~0x24, ~1),			"g,[i+1]", 0, v6 },
{ "st",	F3(3, 0x24, 1), F3(~3, ~0x24, ~1)|RS1_G0,		"g,[i]", 0, v6 },
{ "st",	F3(3, 0x24, 1), F3(~3, ~0x24, ~1)|SIMM13(~0),		"g,[1]", 0, v6 }, /* st d,[rs1+0] */
{ "st",	F3(3, 0x34, 0), F3(~3, ~0x34, ~0)|ASI(~0),		"D,[1+2]", F_ALIAS, v6 },
{ "st",	F3(3, 0x34, 0), F3(~3, ~0x34, ~0)|ASI_RS2(~0),		"D,[1]", F_ALIAS, v6 }, /* st d,[rs1+%g0] */
{ "st",	F3(3, 0x34, 1), F3(~3, ~0x34, ~1),			"D,[1+i]", F_ALIAS, v6 },
{ "st",	F3(3, 0x34, 1), F3(~3, ~0x34, ~1),			"D,[i+1]", F_ALIAS, v6 },
{ "st",	F3(3, 0x34, 1), F3(~3, ~0x34, ~1)|RS1_G0,		"D,[i]", F_ALIAS, v6 },
{ "st",	F3(3, 0x34, 1), F3(~3, ~0x34, ~1)|SIMM13(~0),		"D,[1]", F_ALIAS, v6 }, /* st d,[rs1+0] */
{ "st",	F3(3, 0x35, 0), F3(~3, ~0x35, ~0)|ASI(~0),		"C,[1+2]", 0, v6 },
{ "st",	F3(3, 0x35, 0), F3(~3, ~0x35, ~0)|ASI_RS2(~0),		"C,[1]", 0, v6 }, /* st d,[rs1+%g0] */
{ "st",	F3(3, 0x35, 1), F3(~3, ~0x35, ~1),			"C,[1+i]", 0, v6 },
{ "st",	F3(3, 0x35, 1), F3(~3, ~0x35, ~1),			"C,[i+1]", 0, v6 },
{ "st",	F3(3, 0x35, 1), F3(~3, ~0x35, ~1)|RS1_G0,		"C,[i]", 0, v6 },
{ "st",	F3(3, 0x35, 1), F3(~3, ~0x35, ~1)|SIMM13(~0),		"C,[1]", 0, v6 }, /* st d,[rs1+0] */

{ "st",	F3(3, 0x25, 0), F3(~3, ~0x25, ~0)|RD_G0|ASI(~0),	"F,[1+2]", 0, v6 },
{ "st",	F3(3, 0x25, 0), F3(~3, ~0x25, ~0)|RD_G0|ASI_RS2(~0),	"F,[1]", 0, v6 }, /* st d,[rs1+%g0] */
{ "st",	F3(3, 0x25, 1), F3(~3, ~0x25, ~1)|RD_G0,		"F,[1+i]", 0, v6 },
{ "st",	F3(3, 0x25, 1), F3(~3, ~0x25, ~1)|RD_G0,		"F,[i+1]", 0, v6 },
{ "st",	F3(3, 0x25, 1), F3(~3, ~0x25, ~1)|RD_G0|RS1_G0,		"F,[i]", 0, v6 },
{ "st",	F3(3, 0x25, 1), F3(~3, ~0x25, ~1)|RD_G0|SIMM13(~0),	"F,[1]", 0, v6 }, /* st d,[rs1+0] */




{ "sta",	F3(3, 0x14, 0), F3(~3, ~0x14, ~0),		"d,[1+2]A", 0, v6 },
{ "sta",	F3(3, 0x14, 0), F3(~3, ~0x14, ~0)|RS2(~0),	"d,[1]A", 0, v6 }, /* sta d,[rs1+%g0] */





{ "stb",	F3(3, 0x05, 0), F3(~3, ~0x05, ~0)|ASI(~0),	"d,[1+2]", 0, v6 },
{ "stb",	F3(3, 0x05, 0), F3(~3, ~0x05, ~0)|ASI_RS2(~0),	"d,[1]", 0, v6 }, /* stb d,[rs1+%g0] */
{ "stb",	F3(3, 0x05, 1), F3(~3, ~0x05, ~1),		"d,[1+i]", 0, v6 },
{ "stb",	F3(3, 0x05, 1), F3(~3, ~0x05, ~1),		"d,[i+1]", 0, v6 },
{ "stb",	F3(3, 0x05, 1), F3(~3, ~0x05, ~1)|RS1_G0,	"d,[i]", 0, v6 },
{ "stb",	F3(3, 0x05, 1), F3(~3, ~0x05, ~1)|SIMM13(~0),	"d,[1]", 0, v6 }, /* stb d,[rs1+0] */



{ "stba",	F3(3, 0x15, 0), F3(~3, ~0x15, ~0),		"d,[1+2]A", 0, v6 },
{ "stba",	F3(3, 0x15, 0), F3(~3, ~0x15, ~0)|RS2(~0),	"d,[1]A", 0, v6 }, /* stba d,[rs1+%g0] */



{ "std",	F3(3, 0x07, 0), F3(~3, ~0x07, ~0)|ASI(~0),	"d,[1+2]", 0, v6 },
{ "std",	F3(3, 0x07, 0), F3(~3, ~0x07, ~0)|ASI_RS2(~0),	"d,[1]", 0, v6 }, /* std d,[rs1+%g0] */
{ "std",	F3(3, 0x07, 1), F3(~3, ~0x07, ~1),		"d,[1+i]", 0, v6 },
{ "std",	F3(3, 0x07, 1), F3(~3, ~0x07, ~1),		"d,[i+1]", 0, v6 },
{ "std",	F3(3, 0x07, 1), F3(~3, ~0x07, ~1)|RS1_G0,	"d,[i]", 0, v6 },
{ "std",	F3(3, 0x07, 1), F3(~3, ~0x07, ~1)|SIMM13(~0),	"d,[1]", 0, v6 }, /* std d,[rs1+0] */
{ "std",	F3(3, 0x26, 0), F3(~3, ~0x26, ~0)|ASI(~0),	"q,[1+2]", F_ALIAS, v6 },
{ "std",	F3(3, 0x26, 0), F3(~3, ~0x26, ~0)|ASI_RS2(~0),	"q,[1]", F_ALIAS, v6 }, /* std d,[rs1+%g0] */
{ "std",	F3(3, 0x26, 1), F3(~3, ~0x26, ~1),		"q,[1+i]", F_ALIAS, v6 },
{ "std",	F3(3, 0x26, 1), F3(~3, ~0x26, ~1),		"q,[i+1]", F_ALIAS, v6 },
{ "std",	F3(3, 0x26, 1), F3(~3, ~0x26, ~1)|RS1_G0,	"q,[i]", F_ALIAS, v6 },
{ "std",	F3(3, 0x26, 1), F3(~3, ~0x26, ~1)|SIMM13(~0),	"q,[1]", F_ALIAS, v6 }, /* std d,[rs1+0] */
{ "std",	F3(3, 0x27, 0), F3(~3, ~0x27, ~0)|ASI(~0),	"H,[1+2]", 0, v6 },
{ "std",	F3(3, 0x27, 0), F3(~3, ~0x27, ~0)|ASI_RS2(~0),	"H,[1]", 0, v6 }, /* std d,[rs1+%g0] */
{ "std",	F3(3, 0x27, 1), F3(~3, ~0x27, ~1),		"H,[1+i]", 0, v6 },
{ "std",	F3(3, 0x27, 1), F3(~3, ~0x27, ~1),		"H,[i+1]", 0, v6 },
{ "std",	F3(3, 0x27, 1), F3(~3, ~0x27, ~1)|RS1_G0,	"H,[i]", 0, v6 },
{ "std",	F3(3, 0x27, 1), F3(~3, ~0x27, ~1)|SIMM13(~0),	"H,[1]", 0, v6 }, /* std d,[rs1+0] */
{ "std",	F3(3, 0x36, 0), F3(~3, ~0x36, ~0)|ASI(~0),	"Q,[1+2]", F_ALIAS, v6 },
{ "std",	F3(3, 0x36, 0), F3(~3, ~0x36, ~0)|ASI_RS2(~0),	"Q,[1]", F_ALIAS, v6 }, /* std d,[rs1+%g0] */
{ "std",	F3(3, 0x36, 1), F3(~3, ~0x36, ~1),		"Q,[1+i]", F_ALIAS, v6 },
{ "std",	F3(3, 0x36, 1), F3(~3, ~0x36, ~1),		"Q,[i+1]", F_ALIAS, v6 },
{ "std",	F3(3, 0x36, 1), F3(~3, ~0x36, ~1)|RS1_G0,	"Q,[i]", F_ALIAS, v6 },
{ "std",	F3(3, 0x36, 1), F3(~3, ~0x36, ~1)|SIMM13(~0),	"Q,[1]", F_ALIAS, v6 }, /* std d,[rs1+0] */
{ "std",	F3(3, 0x37, 0), F3(~3, ~0x37, ~0)|ASI(~0),	"D,[1+2]", F_ALIAS, v6 },
{ "std",	F3(3, 0x37, 0), F3(~3, ~0x37, ~0)|ASI_RS2(~0),	"D,[1]", F_ALIAS, v6 }, /* std d,[rs1+%g0] */
{ "std",	F3(3, 0x37, 1), F3(~3, ~0x37, ~1),		"D,[1+i]", F_ALIAS, v6 },
{ "std",	F3(3, 0x37, 1), F3(~3, ~0x37, ~1),		"D,[i+1]", F_ALIAS, v6 },
{ "std",	F3(3, 0x37, 1), F3(~3, ~0x37, ~1)|RS1_G0,	"D,[i]", F_ALIAS, v6 },
{ "std",	F3(3, 0x37, 1), F3(~3, ~0x37, ~1)|SIMM13(~0),	"D,[1]", F_ALIAS, v6 }, /* std d,[rs1+0] */

{ "stda",	F3(3, 0x17, 0), F3(~3, ~0x17, ~0),		"d,[1+2]A", 0, v6 },
{ "stda",	F3(3, 0x17, 0), F3(~3, ~0x17, ~0)|RS2(~0),	"d,[1]A", 0, v6 }, /* stda d,[rs1+%g0] */

{ "sth",	F3(3, 0x06, 0), F3(~3, ~0x06, ~0)|ASI(~0),	"d,[1+2]", 0, v6 },
{ "sth",	F3(3, 0x06, 0), F3(~3, ~0x06, ~0)|ASI_RS2(~0),	"d,[1]", 0, v6 }, /* sth d,[rs1+%g0] */
{ "sth",	F3(3, 0x06, 1), F3(~3, ~0x06, ~1),		"d,[1+i]", 0, v6 },
{ "sth",	F3(3, 0x06, 1), F3(~3, ~0x06, ~1),		"d,[i+1]", 0, v6 },
{ "sth",	F3(3, 0x06, 1), F3(~3, ~0x06, ~1)|RS1_G0,	"d,[i]", 0, v6 },
{ "sth",	F3(3, 0x06, 1), F3(~3, ~0x06, ~1)|SIMM13(~0),	"d,[1]", 0, v6 }, /* sth d,[+] */



{ "stha",	F3(3, 0x16, 0), F3(~3, ~0x16, ~0),		"d,[1+2]A", 0, v6 },
{ "stha",	F3(3, 0x16, 0), F3(~3, ~0x16, ~0)|RS2(~0),	"d,[1]A", 0, v6 }, /* stha ,[+%] */








{ "swap",	F3(3, 0x0f, 0), F3(~3, ~0x0f, ~0)|ASI(~0),	"[1+2],d", 0, v7 },
{ "swap",	F3(3, 0x0f, 0), F3(~3, ~0x0f, ~0)|ASI_RS2(~0),	"[1],d", 0, v7 }, /* swap [rs1+%g0],d */
{ "swap",	F3(3, 0x0f, 1), F3(~3, ~0x0f, ~1),		"[1+i],d", 0, v7 },
{ "swap",	F3(3, 0x0f, 1), F3(~3, ~0x0f, ~1),		"[i+1],d", 0, v7 },
{ "swap",	F3(3, 0x0f, 1), F3(~3, ~0x0f, ~1)|RS1_G0,	"[i],d", 0, v7 },
{ "swap",	F3(3, 0x0f, 1), F3(~3, ~0x0f, ~1)|SIMM13(~0),	"[1],d", 0, v7 }, /* swap [rs1+0],d */

{ "swapa",	F3(3, 0x1f, 0), F3(~3, ~0x1f, ~0),		"[1+2]A,d", 0, v7 },
{ "swapa",	F3(3, 0x1f, 0), F3(~3, ~0x1f, ~0)|RS2(~0),	"[1]A,d", 0, v7 }, /* swapa [rs1+%g0],d */

{ "restore",	F3(2, 0x3d, 0), F3(~2, ~0x3d, ~0)|ASI(~0),			"1,2,d", 0, v6 },
{ "restore",	F3(2, 0x3d, 0), F3(~2, ~0x3d, ~0)|RD_G0|RS1_G0|ASI_RS2(~0),	"", 0, v6 }, /* restore %g0,%g0,%g0 */
{ "restore",	F3(2, 0x3d, 1), F3(~2, ~0x3d, ~1),				"1,i,d", 0, v6 },
{ "restore",	F3(2, 0x3d, 1), F3(~2, ~0x3d, ~1)|RD_G0|RS1_G0|SIMM13(~0),	"", 0, v6 }, /* restore %g0,0,%g0 */

{ "rett",	F3(2, 0x39, 0), F3(~2, ~0x39, ~0)|RD_G0|ASI(~0),	"1+2", F_DELAYED, v6 }, /* rett rs1+rs2 */
{ "rett",	F3(2, 0x39, 0), F3(~2, ~0x39, ~0)|RD_G0|ASI_RS2(~0),	"1", F_DELAYED, v6 },	/* rett rs1,%g0 */
{ "rett",	F3(2, 0x39, 1), F3(~2, ~0x39, ~1)|RD_G0,	"1+i", F_DELAYED, v6 }, /* rett rs1+X */
{ "rett",	F3(2, 0x39, 1), F3(~2, ~0x39, ~1)|RD_G0,	"i+1", F_DELAYED, v6 }, /* rett X+rs1 */
{ "rett",	F3(2, 0x39, 1), F3(~2, ~0x39, ~1)|RD_G0|RS1_G0,"i", F_DELAYED, v6 }, /* rett X+rs1 */
{ "rett",	F3(2, 0x39, 1), F3(~2, ~0x39, ~1)|RD_G0|RS1_G0,	"i", F_DELAYED, v6 },	/* rett X */
{ "rett",	F3(2, 0x39, 1), F3(~2, ~0x39, ~1)|RD_G0|SIMM13(~0),	"1", F_DELAYED, v6 },	/* rett rs1+0 */

{ "save",	F3(2, 0x3c, 0), F3(~2, ~0x3c, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "save",	F3(2, 0x3c, 1), F3(~2, ~0x3c, ~1),		"1,i,d", 0, v6 },
{ "save",	0x81e00000,	~0x81e00000,	"", F_ALIAS, v6 },

{ "ret",  F3(2, 0x38, 1)|RS1(0x1f)|SIMM13(8), F3(~2, ~0x38, ~1)|SIMM13(~8),	       "", F_DELAYED, v6 }, /* jmpl %i7+8,%g0 */
{ "retl", F3(2, 0x38, 1)|RS1(0x0f)|SIMM13(8), F3(~2, ~0x38, ~1)|RS1(~0x0f)|SIMM13(~8), "", F_DELAYED, v6 }, /* jmpl %o7+8,%g0 */

{ "jmpl",	F3(2, 0x38, 0), F3(~2, ~0x38, ~0)|ASI(~0),	"1+2,d", F_DELAYED, v6 },
{ "jmpl",	F3(2, 0x38, 0), F3(~2, ~0x38, ~0)|ASI_RS2(~0),	"1,d", F_DELAYED, v6 }, /* jmpl rs1+%g0,d */
{ "jmpl",	F3(2, 0x38, 1), F3(~2, ~0x38, ~1)|SIMM13(~0),	"1,d", F_DELAYED, v6 }, /* jmpl rs1+0,d */
{ "jmpl",	F3(2, 0x38, 1), F3(~2, ~0x38, ~1)|RS1_G0,	"i,d", F_DELAYED, v6 }, /* jmpl %g0+i,d */
{ "jmpl",	F3(2, 0x38, 1), F3(~2, ~0x38, ~1),		"1+i,d", F_DELAYED, v6 },
{ "jmpl",	F3(2, 0x38, 1), F3(~2, ~0x38, ~1),		"i+1,d", F_DELAYED, v6 },


{ "flush",	F3(2, 0x3b, 0), F3(~2, ~0x3b, ~0)|ASI(~0),	"1+2", 0, v8 },
{ "flush",	F3(2, 0x3b, 0), F3(~2, ~0x3b, ~0)|ASI_RS2(~0),	"1", 0, v8 }, /* flush rs1+%g0 */
{ "flush",	F3(2, 0x3b, 1), F3(~2, ~0x3b, ~1)|SIMM13(~0),	"1", 0, v8 }, /* flush rs1+0 */
{ "flush",	F3(2, 0x3b, 1), F3(~2, ~0x3b, ~1)|RS1_G0,	"i", 0, v8 }, /* flush %g0+i */
{ "flush",	F3(2, 0x3b, 1), F3(~2, ~0x3b, ~1),		"1+i", 0, v8 },
{ "flush",	F3(2, 0x3b, 1), F3(~2, ~0x3b, ~1),		"i+1", 0, v8 },

/* IFLUSH was renamed to FLUSH in v8.  */
{ "iflush",	F3(2, 0x3b, 0), F3(~2, ~0x3b, ~0)|ASI(~0),	"1+2", F_ALIAS, v6 },
{ "iflush",	F3(2, 0x3b, 0), F3(~2, ~0x3b, ~0)|ASI_RS2(~0),	"1", F_ALIAS, v6 }, /* flush rs1+%g0 */
{ "iflush",	F3(2, 0x3b, 1), F3(~2, ~0x3b, ~1)|SIMM13(~0),	"1", F_ALIAS, v6 }, /* flush rs1+0 */
{ "iflush",	F3(2, 0x3b, 1), F3(~2, ~0x3b, ~1)|RS1_G0,	"i", F_ALIAS, v6 },
{ "iflush",	F3(2, 0x3b, 1), F3(~2, ~0x3b, ~1),		"1+i", F_ALIAS, v6 },
{ "iflush",	F3(2, 0x3b, 1), F3(~2, ~0x3b, ~1),		"i+1", F_ALIAS, v6 },



{ "stbar",	F3(2, 0x28, 0)|RS1(0xf), F3(~2, ~0x28, ~0)|RD_G0|RS1(~0xf)|SIMM13(~0),	"", 0, v8 },


 /* The 1<<12 is a long story.  It is necessary.  For more info, please contact rich@cygnus.com */
{ "sll",	F3(2, 0x25, 0), F3(~2, ~0x25, ~0)|(1<<12)|ASI(~0),	"1,2,d", 0, v6 },
{ "sll",	F3(2, 0x25, 1), F3(~2, ~0x25, ~1)|(1<<12),		"1,i,d", 0, v6 },
{ "sra",	F3(2, 0x27, 0), F3(~2, ~0x27, ~0)|(1<<12)|ASI(~0),	"1,2,d", 0, v6 },
{ "sra",	F3(2, 0x27, 1), F3(~2, ~0x27, ~1)|(1<<12),		"1,i,d", 0, v6 },
{ "srl",	F3(2, 0x26, 0), F3(~2, ~0x26, ~0)|(1<<12)|ASI(~0),	"1,2,d", 0, v6 },
{ "srl",	F3(2, 0x26, 1), F3(~2, ~0x26, ~1)|(1<<12),		"1,i,d", 0, v6 },


{ "mulscc",	F3(2, 0x24, 0), F3(~2, ~0x24, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "mulscc",	F3(2, 0x24, 1), F3(~2, ~0x24, ~1),		"1,i,d", 0, v6 },

{ "divscc",	F3(2, 0x1d, 0), F3(~2, ~0x1d, ~0)|ASI(~0),	"1,2,d", 0, sparclite },
{ "divscc",	F3(2, 0x1d, 1), F3(~2, ~0x1d, ~1),		"1,i,d", 0, sparclite },

{ "scan",	F3(2, 0x2c, 0), F3(~2, ~0x2c, ~0)|ASI(~0),	"1,2,d", 0, sparclite },
{ "scan",	F3(2, 0x2c, 1), F3(~2, ~0x2c, ~1),		"1,i,d", 0, sparclite },


{ "clr",	F3(2, 0x02, 0), F3(~2, ~0x02, ~0)|RD_G0|RS1_G0|ASI_RS2(~0),	"d", F_ALIAS, v6 }, /* or %g0,%g0,d */
{ "clr",	F3(2, 0x02, 1), F3(~2, ~0x02, ~1)|RS1_G0|SIMM13(~0),		"d", F_ALIAS, v6 }, /* or %g0,0,d	*/
{ "clr",	F3(3, 0x04, 0), F3(~3, ~0x04, ~0)|RD_G0|ASI(~0),		"[1+2]", F_ALIAS, v6 },
{ "clr",	F3(3, 0x04, 0), F3(~3, ~0x04, ~0)|RD_G0|ASI_RS2(~0),		"[1]", F_ALIAS, v6 }, /* st %g0,[rs1+%g0] */
{ "clr",	F3(3, 0x04, 1), F3(~3, ~0x04, ~1)|RD_G0,			"[1+i]", F_ALIAS, v6 },
{ "clr",	F3(3, 0x04, 1), F3(~3, ~0x04, ~1)|RD_G0,			"[i+1]", F_ALIAS, v6 },
{ "clr",	F3(3, 0x04, 1), F3(~3, ~0x04, ~1)|RD_G0|RS1_G0,		"[i]", F_ALIAS, v6 },
{ "clr",	F3(3, 0x04, 1), F3(~3, ~0x04, ~1)|RD_G0|SIMM13(~0),		"[1]", F_ALIAS, v6 }, /* st %g0,[rs1+0] */

{ "clrb",	F3(3, 0x05, 0), F3(~3, ~0x05, ~0)|RD_G0|ASI(~0),	"[1+2]", F_ALIAS, v6 },
{ "clrb",	F3(3, 0x05, 0), F3(~3, ~0x05, ~0)|RD_G0|ASI_RS2(~0),	"[1]", F_ALIAS, v6 }, /* stb %g0,[rs1+%g0] */
{ "clrb",	F3(3, 0x05, 1), F3(~3, ~0x05, ~1)|RD_G0,		"[1+i]", F_ALIAS, v6 },
{ "clrb",	F3(3, 0x05, 1), F3(~3, ~0x05, ~1)|RD_G0,		"[i+1]", F_ALIAS, v6 },
{ "clrb",	F3(3, 0x05, 1), F3(~3, ~0x05, ~1)|RD_G0|RS1_G0,	"[i]", F_ALIAS, v6 },
{ "clrb",	F3(3, 0x05, 1), F3(~3, ~0x05, ~1)|RD_G0|SIMM13(~0),	"[1]", F_ALIAS, v6 }, /* clrb [rs1+0],d */

{ "clrh",	F3(3, 0x06, 0), F3(~3, ~0x06, ~0)|RD_G0|ASI(~0),	"[1+2]", F_ALIAS, v6 },
{ "clrh",	F3(3, 0x06, 0), F3(~3, ~0x06, ~0)|RD_G0|ASI_RS2(~0),	"[1]", F_ALIAS, v6 }, /* sth %g0,[rs1+%g0] */
{ "clrh",	F3(3, 0x06, 1), F3(~3, ~0x06, ~1)|RD_G0,		"[1+i]", F_ALIAS, v6 },
{ "clrh",	F3(3, 0x06, 1), F3(~3, ~0x06, ~1)|RD_G0,		"[i+1]", F_ALIAS, v6 },
{ "clrh",	F3(3, 0x06, 1), F3(~3, ~0x06, ~1)|RD_G0|RS1_G0,	"[i]", F_ALIAS, v6 },
{ "clrh",	F3(3, 0x06, 1), F3(~3, ~0x06, ~1)|RD_G0|SIMM13(~0),	"[1]", F_ALIAS, v6 }, /* clrb [rs1+0],d */

{ "orcc",	F3(2, 0x12, 0), F3(~2, ~0x12, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "orcc",	F3(2, 0x12, 1), F3(~2, ~0x12, ~1),		"1,i,d", 0, v6 },
{ "orcc",	F3(2, 0x12, 1), F3(~2, ~0x12, ~1),		"i,1,d", 0, v6 },

/* This is not a commutative instruction.  */
{ "orncc",	F3(2, 0x16, 0), F3(~2, ~0x16, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "orncc",	F3(2, 0x16, 1), F3(~2, ~0x16, ~1),		"1,i,d", 0, v6 },

/* This is not a commutative instruction.  */
{ "orn",	F3(2, 0x06, 0), F3(~2, ~0x06, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "orn",	F3(2, 0x06, 1), F3(~2, ~0x06, ~1),		"1,i,d", 0, v6 },

{ "tst",	F3(2, 0x12, 0), F3(~2, ~0x12, ~0)|RD_G0|ASI_RS2(~0),	"1", 0, v6 }, /* orcc rs1, %g0, %g0 */
{ "tst",	F3(2, 0x12, 0), F3(~2, ~0x12, ~0)|RD_G0|RS1_G0|ASI(~0),	"2", 0, v6 }, /* orcc %g0, rs2, %g0 */
{ "tst",	F3(2, 0x12, 1), F3(~2, ~0x12, ~1)|RD_G0|SIMM13(~0),	"1", 0, v6 }, /* orcc rs1, 0, %g0 */

{ "wr",	F3(2, 0x30, 0),		F3(~2, ~0x30, ~0)|ASI(~0),		"1,2,m", 0, v8 }, /* wr r,r,%asrX */
{ "wr",	F3(2, 0x30, 0),		F3(~2, ~0x30, ~0)|RD_G0|ASI(~0),	"1,2,y", 0, v6 }, /* wr r,r,%y */
{ "wr",	F3(2, 0x30, 1),		F3(~2, ~0x30, ~1),			"1,i,m", 0, v8 }, /* wr r,i,%asrX */
{ "wr",	F3(2, 0x30, 1),		F3(~2, ~0x30, ~1)|RD_G0,		"1,i,y", 0, v6 }, /* wr r,i,%y */
{ "wr",	F3(2, 0x31, 0),		F3(~2, ~0x31, ~0)|RD_G0|ASI(~0),	"1,2,p", 0, v6 }, /* wr r,r,%psr */
{ "wr",	F3(2, 0x31, 1),		F3(~2, ~0x31, ~1)|RD_G0,		"1,i,p", 0, v6 }, /* wr r,i,%psr */
{ "wr",	F3(2, 0x32, 0),		F3(~2, ~0x32, ~0)|RD_G0|ASI(~0),	"1,2,w", 0, v6 }, /* wr r,r,%wim */
{ "wr",	F3(2, 0x32, 1),		F3(~2, ~0x32, ~1)|RD_G0,		"1,i,w", 0, v6 }, /* wr r,i,%wim */
{ "wr",	F3(2, 0x33, 0),		F3(~2, ~0x33, ~0)|RD_G0|ASI(~0),	"1,2,t", 0, v6 }, /* wr r,r,%tbr */
{ "wr",	F3(2, 0x33, 1),		F3(~2, ~0x33, ~1)|RD_G0,		"1,i,t", 0, v6 }, /* wr r,i,%tbr */


{ "rd",	F3(2, 0x28, 0),			F3(~2, ~0x28, ~0)|SIMM13(~0),		"M,d", 0, v8 }, /* rd %asr1,r */
{ "rd",	F3(2, 0x28, 0),			F3(~2, ~0x28, ~0)|RS1_G0|SIMM13(~0),	"y,d", 0, v6 }, /* rd %y,r */
{ "rd",	F3(2, 0x29, 0),			F3(~2, ~0x29, ~0)|RS1_G0|SIMM13(~0),	"p,d", 0, v6 }, /* rd %psr,r */
{ "rd",	F3(2, 0x2a, 0),			F3(~2, ~0x2a, ~0)|RS1_G0|SIMM13(~0),	"w,d", 0, v6 }, /* rd %wim,r */
{ "rd",	F3(2, 0x2b, 0),			F3(~2, ~0x2b, ~0)|RS1_G0|SIMM13(~0),	"t,d", 0, v6 }, /* rd %tbr,r */



{ "mov",	F3(2, 0x30, 0),		F3(~2, ~0x30, ~0)|ASI(~0),		"1,2,m", F_ALIAS, v8 }, /* wr r,r,%asrX */
{ "mov",	F3(2, 0x30, 0),		F3(~2, ~0x30, ~0)|RD_G0|ASI(~0),	"1,2,y", F_ALIAS, v6 }, /* wr r,r,%y */
{ "mov",	F3(2, 0x30, 1),		F3(~2, ~0x30, ~1),			"1,i,m", F_ALIAS, v8 }, /* wr r,i,%asrX */
{ "mov",	F3(2, 0x30, 1),		F3(~2, ~0x30, ~1)|RD_G0,		"1,i,y", F_ALIAS, v6 }, /* wr r,i,%y */
{ "mov",	F3(2, 0x31, 0),		F3(~2, ~0x31, ~0)|RD_G0|ASI(~0),	"1,2,p", F_ALIAS, v6 }, /* wr r,r,%psr */
{ "mov",	F3(2, 0x31, 1),		F3(~2, ~0x31, ~1)|RD_G0,		"1,i,p", F_ALIAS, v6 }, /* wr r,i,%psr */
{ "mov",	F3(2, 0x32, 0),		F3(~2, ~0x32, ~0)|RD_G0|ASI(~0),	"1,2,w", F_ALIAS, v6 }, /* wr r,r,%wim */
{ "mov",	F3(2, 0x32, 1),		F3(~2, ~0x32, ~1)|RD_G0,		"1,i,w", F_ALIAS, v6 }, /* wr r,i,%wim */
{ "mov",	F3(2, 0x33, 0),		F3(~2, ~0x33, ~0)|RD_G0|ASI(~0),	"1,2,t", F_ALIAS, v6 }, /* wr r,r,%tbr */
{ "mov",	F3(2, 0x33, 1),		F3(~2, ~0x33, ~1)|RD_G0,		"1,i,t", F_ALIAS, v6 }, /* wr r,i,%tbr */

{ "mov",	F3(2, 0x28, 0),		 F3(~2, ~0x28, ~0)|SIMM13(~0),			"M,d", F_ALIAS, v8 }, /* rd %asr1,r */
{ "mov",	F3(2, 0x28, 0),		 F3(~2, ~0x28, ~0)|RS1_G0|SIMM13(~0),		"y,d", F_ALIAS, v6 }, /* rd %y,r */
{ "mov",	F3(2, 0x29, 0),		 F3(~2, ~0x29, ~0)|RS1_G0|SIMM13(~0),		"p,d", F_ALIAS, v6 }, /* rd %psr,r */
{ "mov",	F3(2, 0x2a, 0),		 F3(~2, ~0x2a, ~0)|RS1_G0|SIMM13(~0),		"w,d", F_ALIAS, v6 }, /* rd %wim,r */
{ "mov",	F3(2, 0x2b, 0),		 F3(~2, ~0x2b, ~0)|RS1_G0|SIMM13(~0),		"t,d", F_ALIAS, v6 }, /* rd %tbr,r */

{ "mov",	F3(2, 0x30, 0), F3(~2, ~0x30, ~0)|ASI_RS2(~0),	"1,y", F_ALIAS, v6 }, /* wr rs1,%g0,%y */
{ "mov",	F3(2, 0x30, 1), F3(~2, ~0x30, ~1),		"i,y", F_ALIAS, v6 },
{ "mov",	F3(2, 0x30, 1), F3(~2, ~0x30, ~1)|SIMM13(~0),	"1,y", F_ALIAS, v6 }, /* wr rs1,0,%y */
{ "mov",	F3(2, 0x31, 0), F3(~2, ~0x31, ~0)|ASI_RS2(~0),	"1,p", F_ALIAS, v6 }, /* wr rs1,%g0,%psr */
{ "mov",	F3(2, 0x31, 1), F3(~2, ~0x31, ~1),		"i,p", F_ALIAS, v6 },
{ "mov",	F3(2, 0x31, 1), F3(~2, ~0x31, ~1)|SIMM13(~0),	"1,p", F_ALIAS, v6 }, /* wr rs1,0,%psr */
{ "mov",	F3(2, 0x32, 0), F3(~2, ~0x32, ~0)|ASI_RS2(~0),	"1,w", F_ALIAS, v6 }, /* wr rs1,%g0,%wim */
{ "mov",	F3(2, 0x32, 1), F3(~2, ~0x32, ~1),		"i,w", F_ALIAS, v6 },
{ "mov",	F3(2, 0x32, 1), F3(~2, ~0x32, ~1)|SIMM13(~0),	"1,w", F_ALIAS, v6 }, /* wr rs1,0,%wim */
{ "mov",	F3(2, 0x33, 0), F3(~2, ~0x33, ~0)|ASI_RS2(~0),	"1,t", F_ALIAS, v6 }, /* wr rs1,%g0,%tbr */
{ "mov",	F3(2, 0x33, 1), F3(~2, ~0x33, ~1),		"i,t", F_ALIAS, v6 },
{ "mov",	F3(2, 0x33, 1), F3(~2, ~0x33, ~1)|SIMM13(~0),	"1,t", F_ALIAS, v6 }, /* wr rs1,0,%tbr */

{ "mov",	F3(2, 0x02, 0), F3(~2, ~0x02, ~0)|RS1_G0|ASI(~0),	"2,d", 0, v6 }, /* or %g0,rs2,d */
{ "mov",	F3(2, 0x02, 1), F3(~2, ~0x02, ~1)|RS1_G0,		"i,d", 0, v6 }, /* or %g0,i,d	*/
{ "mov",        F3(2, 0x02, 0), F3(~2, ~0x02, ~0)|ASI_RS2(~0),		"1,d", 0, v6 }, /* or rs1,%g0,d   */
{ "mov",        F3(2, 0x02, 1), F3(~2, ~0x02, ~1)|SIMM13(~0),		"1,d", 0, v6 }, /* or rs1,0,d */

{ "or",	F3(2, 0x02, 0), F3(~2, ~0x02, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "or",	F3(2, 0x02, 1), F3(~2, ~0x02, ~1),		"1,i,d", 0, v6 },
{ "or",	F3(2, 0x02, 1), F3(~2, ~0x02, ~1),		"i,1,d", 0, v6 },

{ "bset",	F3(2, 0x02, 0), F3(~2, ~0x02, ~0)|ASI(~0),	"2,r", F_ALIAS, v6 },	/* or rd,rs2,rd */
{ "bset",	F3(2, 0x02, 1), F3(~2, ~0x02, ~1),		"i,r", F_ALIAS, v6 },	/* or rd,i,rd */

/* This is not a commutative instruction.  */
{ "andn",	F3(2, 0x05, 0), F3(~2, ~0x05, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "andn",	F3(2, 0x05, 1), F3(~2, ~0x05, ~1),		"1,i,d", 0, v6 },

/* This is not a commutative instruction.  */
{ "andncc",	F3(2, 0x15, 0), F3(~2, ~0x15, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "andncc",	F3(2, 0x15, 1), F3(~2, ~0x15, ~1),		"1,i,d", 0, v6 },

{ "bclr",	F3(2, 0x05, 0), F3(~2, ~0x05, ~0)|ASI(~0),	"2,r", F_ALIAS, v6 },	/* andn rd,rs2,rd */
{ "bclr",	F3(2, 0x05, 1), F3(~2, ~0x05, ~1),		"i,r", F_ALIAS, v6 },	/* andn rd,i,rd */

{ "cmp",	F3(2, 0x14, 0), F3(~2, ~0x14, ~0)|RD_G0|ASI(~0),	"1,2", 0, v6 },	/* subcc rs1,rs2,%g0 */
{ "cmp",	F3(2, 0x14, 1), F3(~2, ~0x14, ~1)|RD_G0,		"1,i", 0, v6 },	/* subcc rs1,i,%g0 */

{ "sub",	F3(2, 0x04, 0), F3(~2, ~0x04, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "sub",	F3(2, 0x04, 1), F3(~2, ~0x04, ~1),		"1,i,d", 0, v6 },

{ "subcc",	F3(2, 0x14, 0), F3(~2, ~0x14, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "subcc",	F3(2, 0x14, 1), F3(~2, ~0x14, ~1),		"1,i,d", 0, v6 },

{ "subx",	F3(2, 0x0c, 0), F3(~2, ~0x0c, ~0)|ASI(~0),	"1,2,d", F_ALIAS, v6 },
{ "subx",	F3(2, 0x0c, 1), F3(~2, ~0x0c, ~1),		"1,i,d", F_ALIAS, v6 },

{ "subxcc",	F3(2, 0x1c, 0), F3(~2, ~0x1c, ~0)|ASI(~0),	"1,2,d", F_ALIAS, v6 },
{ "subxcc",	F3(2, 0x1c, 1), F3(~2, ~0x1c, ~1),		"1,i,d", F_ALIAS, v6 },

{ "and",	F3(2, 0x01, 0), F3(~2, ~0x01, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "and",	F3(2, 0x01, 1), F3(~2, ~0x01, ~1),		"1,i,d", 0, v6 },
{ "and",	F3(2, 0x01, 1), F3(~2, ~0x01, ~1),		"i,1,d", 0, v6 },

{ "andcc",	F3(2, 0x11, 0), F3(~2, ~0x11, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "andcc",	F3(2, 0x11, 1), F3(~2, ~0x11, ~1),		"1,i,d", 0, v6 },
{ "andcc",	F3(2, 0x11, 1), F3(~2, ~0x11, ~1),		"i,1,d", 0, v6 },

{ "dec",	F3(2, 0x04, 1)|SIMM13(0x1), F3(~2, ~0x04, ~1)|SIMM13(~0x0001), "r", F_ALIAS, v6 },	/* sub rd,1,rd */
{ "dec",	F3(2, 0x04, 1),		    F3(~2, ~0x04, ~1),		       "i,r", F_ALIAS, v8 },	/* sub rd,imm,rd */
{ "deccc",	F3(2, 0x14, 1)|SIMM13(0x1), F3(~2, ~0x14, ~1)|SIMM13(~0x0001), "r", F_ALIAS, v6 },	/* subcc rd,1,rd */
{ "deccc",	F3(2, 0x14, 1),		    F3(~2, ~0x14, ~1),		       "i,r", F_ALIAS, v8 },	/* subcc rd,imm,rd */
{ "inc",	F3(2, 0x00, 1)|SIMM13(0x1), F3(~2, ~0x00, ~1)|SIMM13(~0x0001), "r", F_ALIAS, v6 },	/* add rd,1,rd */
{ "inc",	F3(2, 0x00, 1),		    F3(~2, ~0x00, ~1),		       "i,r", F_ALIAS, v8 },	/* add rd,imm,rd */
{ "inccc",	F3(2, 0x10, 1)|SIMM13(0x1), F3(~2, ~0x10, ~1)|SIMM13(~0x0001), "r", F_ALIAS, v6 },	/* addcc rd,1,rd */
{ "inccc",	F3(2, 0x10, 1),		    F3(~2, ~0x10, ~1),		       "i,r", F_ALIAS, v8 },	/* addcc rd,imm,rd */

{ "btst",	F3(2, 0x11, 0), F3(~2, ~0x11, ~0)|RD_G0|ASI(~0), "1,2", F_ALIAS, v6 },	/* andcc rs1,rs2,%g0 */
{ "btst",	F3(2, 0x11, 1), F3(~2, ~0x11, ~1)|RD_G0, "i,1", F_ALIAS, v6 },	/* andcc rs1,i,%g0 */

{ "neg",	F3(2, 0x04, 0), F3(~2, ~0x04, ~0)|RS1_G0|ASI(~0), "2,d", F_ALIAS, v6 }, /* sub %g0,rs2,rd */
{ "neg",	F3(2, 0x04, 0), F3(~2, ~0x04, ~0)|RS1_G0|ASI(~0), "r", F_ALIAS, v6 }, /* sub %g0,rd,rd */

{ "add",	F3(2, 0x00, 0), F3(~2, ~0x00, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "add",	F3(2, 0x00, 1), F3(~2, ~0x00, ~1),		"1,i,d", 0, v6 },
{ "add",	F3(2, 0x00, 1), F3(~2, ~0x00, ~1),		"i,1,d", 0, v6 },
{ "addcc",	F3(2, 0x10, 0), F3(~2, ~0x10, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "addcc",	F3(2, 0x10, 1), F3(~2, ~0x10, ~1),		"1,i,d", 0, v6 },
{ "addcc",	F3(2, 0x10, 1), F3(~2, ~0x10, ~1),		"i,1,d", 0, v6 },
{ "addx",	F3(2, 0x08, 0), F3(~2, ~0x08, ~0)|ASI(~0),	"1,2,d", F_ALIAS, v6 },
{ "addx",	F3(2, 0x08, 1), F3(~2, ~0x08, ~1),		"1,i,d", F_ALIAS, v6 },
{ "addx",	F3(2, 0x08, 1), F3(~2, ~0x08, ~1),		"i,1,d", F_ALIAS, v6 },
{ "addxcc",	F3(2, 0x18, 0), F3(~2, ~0x18, ~0)|ASI(~0),	"1,2,d", F_ALIAS, v6 },
{ "addxcc",	F3(2, 0x18, 1), F3(~2, ~0x18, ~1),		"1,i,d", F_ALIAS, v6 },
{ "addxcc",	F3(2, 0x18, 1), F3(~2, ~0x18, ~1),		"i,1,d", F_ALIAS, v6 },

{ "smul",	F3(2, 0x0b, 0), F3(~2, ~0x0b, ~0)|ASI(~0),	"1,2,d", 0, v8 },
{ "smul",	F3(2, 0x0b, 1), F3(~2, ~0x0b, ~1),		"1,i,d", 0, v8 },
{ "smul",	F3(2, 0x0b, 1), F3(~2, ~0x0b, ~1),		"i,1,d", 0, v8 },
{ "smulcc",	F3(2, 0x1b, 0), F3(~2, ~0x1b, ~0)|ASI(~0),	"1,2,d", 0, v8 },
{ "smulcc",	F3(2, 0x1b, 1), F3(~2, ~0x1b, ~1),		"1,i,d", 0, v8 },
{ "smulcc",	F3(2, 0x1b, 1), F3(~2, ~0x1b, ~1),		"i,1,d", 0, v8 },
{ "umul",	F3(2, 0x0a, 0), F3(~2, ~0x0a, ~0)|ASI(~0),	"1,2,d", 0, v8 },
{ "umul",	F3(2, 0x0a, 1), F3(~2, ~0x0a, ~1),		"1,i,d", 0, v8 },
{ "umul",	F3(2, 0x0a, 1), F3(~2, ~0x0a, ~1),		"i,1,d", 0, v8 },
{ "umulcc",	F3(2, 0x1a, 0), F3(~2, ~0x1a, ~0)|ASI(~0),	"1,2,d", 0, v8 },
{ "umulcc",	F3(2, 0x1a, 1), F3(~2, ~0x1a, ~1),		"1,i,d", 0, v8 },
{ "umulcc",	F3(2, 0x1a, 1), F3(~2, ~0x1a, ~1),		"i,1,d", 0, v8 },
{ "sdiv",	F3(2, 0x0f, 0), F3(~2, ~0x0f, ~0)|ASI(~0),	"1,2,d", 0, v8 },
{ "sdiv",	F3(2, 0x0f, 1), F3(~2, ~0x0f, ~1),		"1,i,d", 0, v8 },
{ "sdiv",	F3(2, 0x0f, 1), F3(~2, ~0x0f, ~1),		"i,1,d", 0, v8 },
{ "sdivcc",	F3(2, 0x1f, 0), F3(~2, ~0x1f, ~0)|ASI(~0),	"1,2,d", 0, v8 },
{ "sdivcc",	F3(2, 0x1f, 1), F3(~2, ~0x1f, ~1),		"1,i,d", 0, v8 },
{ "sdivcc",	F3(2, 0x1f, 1), F3(~2, ~0x1f, ~1),		"i,1,d", 0, v8 },
{ "udiv",	F3(2, 0x0e, 0), F3(~2, ~0x0e, ~0)|ASI(~0),	"1,2,d", 0, v8 },
{ "udiv",	F3(2, 0x0e, 1), F3(~2, ~0x0e, ~1),		"1,i,d", 0, v8 },
{ "udiv",	F3(2, 0x0e, 1), F3(~2, ~0x0e, ~1),		"i,1,d", 0, v8 },
{ "udivcc",	F3(2, 0x1e, 0), F3(~2, ~0x1e, ~0)|ASI(~0),	"1,2,d", 0, v8 },
{ "udivcc",	F3(2, 0x1e, 1), F3(~2, ~0x1e, ~1),		"1,i,d", 0, v8 },
{ "udivcc",	F3(2, 0x1e, 1), F3(~2, ~0x1e, ~1),		"i,1,d", 0, v8 },


{ "call",	F1(0x1), F1(~0x1), "L", F_DELAYED, v6 },
{ "call",	F1(0x1), F1(~0x1), "L,#", F_DELAYED, v6 },
{ "call",	F3(2, 0x38, 0)|RD(0xf), F3(~2, ~0x38, ~0)|RD(~0xf)|ASI_RS2(~0), "1", F_DELAYED, v6 }, /* jmpl rs1+%g0, %o7 */
{ "call",	F3(2, 0x38, 0)|RD(0xf), F3(~2, ~0x38, ~0)|RD(~0xf)|ASI_RS2(~0), "1,#", F_DELAYED, v6 },

/* Conditional instructions.

   Because this part of the table was such a mess earlier, I have
   macrofied it so that all the branches and traps are generated from
   a single-line description of each condition value.  John Gilmore. */

/* Define branches -- one annulled, one without, etc. */
#define br(opcode, mask, lose, flags) \
 { opcode, (mask)|ANNUL, (lose),       ",a l",   (flags), v6 }, \
 { opcode, (mask)      , (lose)|ANNUL, "l",     (flags), v6 }


/* Define four traps: reg+reg, reg + immediate, immediate alone, reg alone. */
#define tr(opcode, mask, lose, flags) \
 { opcode, (mask)|IMMED, (lose)|RS1_G0,		"i",     (flags), v6 }, /* %g0 + imm */ \
 { opcode, (mask)|IMMED, (lose),		"1+i",   (flags), v6 }, /* rs1 + imm */ \
 { opcode, (mask), IMMED|(lose),		"1+2",   (flags), v6 }, /* rs1 + rs2 */ \
 { opcode, (mask), IMMED|(lose)|RS2_G0,		"1",     (flags), v6 } /* rs1 + %g0 */


/* Define both branches and traps based on condition mask */
#define cond(bop, top, mask, flags) \
  br(bop,  F2(0, 2)|(mask), F2(~0, ~2)|((~mask)&COND(~0)), F_DELAYED|(flags)), \
  tr(top,  F3(2, 0x3a, 0)|(mask), F3(~2, ~0x3a, 0)|((~mask)&COND(~0)), (flags))

/* Define all the conditions, all the branches, all the traps.  */

cond ("b",	"t",    CONDA, 0),
cond ("ba",	"ta",   CONDA, F_ALIAS), /* for nothing */
cond ("bcc",	"tcc",  CONDCC, 0),
cond ("bcs",	"tcs",  CONDCS, 0),
cond ("be",	"te",   CONDE, 0),
cond ("bg",	"tg",   CONDG, 0),
cond ("bgt",	"tgt",   CONDG, F_ALIAS),
cond ("bge",	"tge",  CONDGE, 0),
cond ("bgeu",	"tgeu", CONDGEU, F_ALIAS), /* for cc */
cond ("bgu",	"tgu",  CONDGU, 0),
cond ("bl",	"tl",   CONDL, 0),
cond ("blt",	"tlt",   CONDL, F_ALIAS),
cond ("ble",	"tle",  CONDLE, 0),
cond ("bleu",	"tleu", CONDLEU, 0),
cond ("blu",	"tlu",  CONDLU, F_ALIAS), /* for cs */
cond ("bn",	"tn",   CONDN, 0),
cond ("bne",	"tne",  CONDNE, 0),
cond ("bneg",	"tneg", CONDNEG, 0),
cond ("bnz",	"tnz",  CONDNZ, F_ALIAS), /* for ne */
cond ("bpos",	"tpos", CONDPOS, 0),
cond ("bvc",	"tvc",  CONDVC, 0),
cond ("bvs",	"tvs",  CONDVS, 0),
cond ("bz",	"tz",   CONDZ, F_ALIAS), /* for e */

#undef cond
#undef br
#undef tr















#define brfc(opcode, mask, lose, flags) \
 { opcode, (mask), ANNUL|(lose), "l",    flags|F_DELAYED, v6 }, \
 { opcode, (mask)|ANNUL, (lose), ",a l", flags|F_DELAYED, v6 }



#define condfc(fop, cop, mask, flags) \
  brfc(fop, F2(0, 6)|COND(mask), F2(~0, ~6)|COND(~(mask)), flags), \
  brfc(cop, F2(0, 7)|COND(mask), F2(~0, ~7)|COND(~(mask)), flags)

#define condf(fop, mask, flags) \
  brfc(fop, F2(0, 6)|COND(mask), F2(~0, ~6)|COND(~(mask)), flags)

condfc("fb",	"cb",	 0x8, 0),
condfc("fba",	"cba",	 0x8, F_ALIAS),
condfc("fbe",	"cb0",	 0x9, 0),
condf("fbz",		 0x9, F_ALIAS),
condfc("fbg",	"cb2",	 0x6, 0),
condfc("fbge",	"cb02",	 0xb, 0),
condfc("fbl",	"cb1",	 0x4, 0),
condfc("fble",	"cb01",	 0xd, 0),
condfc("fblg",	"cb12",	 0x2, 0),
condfc("fbn",	"cbn",	 0x0, 0),
condfc("fbne",	"cb123", 0x1, 0),
condf("fbnz",		 0x1, F_ALIAS),
condfc("fbo",	"cb012", 0xf, 0),
condfc("fbu",	"cb3",	 0x7, 0),
condfc("fbue",	"cb03",	 0xa, 0),
condfc("fbug",	"cb23",	 0x5, 0),
condfc("fbuge",	"cb023", 0xc, 0),
condfc("fbul",	"cb13",	 0x3, 0),
condfc("fbule",	"cb013", 0xe, 0),

#undef condfc
#undef brfc

{ "jmp",	F3(2, 0x38, 0), F3(~2, ~0x38, ~0)|RD_G0|ASI(~0),	"1+2", F_DELAYED, v6 }, /* jmpl rs1+rs2,%g0 */
{ "jmp",	F3(2, 0x38, 0), F3(~2, ~0x38, ~0)|RD_G0|ASI_RS2(~0),	"1", F_DELAYED, v6 }, /* jmpl rs1+%g0,%g0 */
{ "jmp",	F3(2, 0x38, 1), F3(~2, ~0x38, ~1)|RD_G0,		"1+i", F_DELAYED, v6 }, /* jmpl rs1+i,%g0 */
{ "jmp",	F3(2, 0x38, 1), F3(~2, ~0x38, ~1)|RD_G0,		"i+1", F_DELAYED, v6 }, /* jmpl i+rs1,%g0 */
{ "jmp",	F3(2, 0x38, 1), F3(~2, ~0x38, ~1)|RD_G0|RS1_G0,		"i", F_DELAYED, v6 }, /* jmpl %g0+i,%g0 */
{ "jmp",	F3(2, 0x38, 1), F3(~2, ~0x38, ~1)|RD_G0|SIMM13(~0),	"1", F_DELAYED, v6 }, /* jmpl rs1+0,%g0 */

{ "nop",	F2(0, 4), 0xfeffffff, "", 0, v6 }, /* sethi 0, %g0 */

{ "set",	F2(0x0, 0x4), F2(~0x0, ~0x4), "Sh,d", F_ALIAS, v6 },

{ "sethi",	F2(0x0, 0x4), F2(~0x0, ~0x4), "h,d", 0, v6 },

{ "taddcc",	F3(2, 0x20, 0), F3(~2, ~0x20, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "taddcc",	F3(2, 0x20, 1), F3(~2, ~0x20, ~1),		"1,i,d", 0, v6 },
{ "taddcc",	F3(2, 0x20, 1), F3(~2, ~0x20, ~1),		"i,1,d", 0, v6 },
{ "taddcctv",	F3(2, 0x22, 0), F3(~2, ~0x22, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "taddcctv",	F3(2, 0x22, 1), F3(~2, ~0x22, ~1),		"1,i,d", 0, v6 },
{ "taddcctv",	F3(2, 0x22, 1), F3(~2, ~0x22, ~1),		"i,1,d", 0, v6 },

{ "tsubcc",	F3(2, 0x21, 0), F3(~2, ~0x21, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "tsubcc",	F3(2, 0x21, 1), F3(~2, ~0x21, ~1),		"1,i,d", 0, v6 },
{ "tsubcctv",	F3(2, 0x23, 0), F3(~2, ~0x23, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "tsubcctv",	F3(2, 0x23, 1), F3(~2, ~0x23, ~1),		"1,i,d", 0, v6 },

{ "unimp",	F2(0x0, 0x0), 0xffc00000, "n", F_ALIAS, v6 },

/* This *is* a commutative instruction.  */
{ "xnor",	F3(2, 0x07, 0), F3(~2, ~0x07, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "xnor",	F3(2, 0x07, 1), F3(~2, ~0x07, ~1),		"1,i,d", 0, v6 },
{ "xnor",	F3(2, 0x07, 1), F3(~2, ~0x07, ~1),		"i,1,d", 0, v6 },
/* This *is* a commutative instruction.  */
{ "xnorcc",	F3(2, 0x17, 0), F3(~2, ~0x17, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "xnorcc",	F3(2, 0x17, 1), F3(~2, ~0x17, ~1),		"1,i,d", 0, v6 },
{ "xnorcc",	F3(2, 0x17, 1), F3(~2, ~0x17, ~1),		"i,1,d", 0, v6 },
{ "xor",	F3(2, 0x03, 0), F3(~2, ~0x03, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "xor",	F3(2, 0x03, 1), F3(~2, ~0x03, ~1),		"1,i,d", 0, v6 },
{ "xor",	F3(2, 0x03, 1), F3(~2, ~0x03, ~1),		"i,1,d", 0, v6 },
{ "xorcc",	F3(2, 0x13, 0), F3(~2, ~0x13, ~0)|ASI(~0),	"1,2,d", 0, v6 },
{ "xorcc",	F3(2, 0x13, 1), F3(~2, ~0x13, ~1),		"1,i,d", 0, v6 },
{ "xorcc",	F3(2, 0x13, 1), F3(~2, ~0x13, ~1),		"i,1,d", 0, v6 },

{ "not",	F3(2, 0x07, 0), F3(~2, ~0x07, ~0)|ASI(~0), "1,d", F_ALIAS, v6 }, /* xnor rs1,%0,rd */
{ "not",	F3(2, 0x07, 0), F3(~2, ~0x07, ~0)|ASI(~0), "r", F_ALIAS, v6 }, /* xnor rd,%0,rd */

{ "btog",	F3(2, 0x03, 0), F3(~2, ~0x03, ~0)|ASI(~0),	"2,r", F_ALIAS, v6 }, /* xor rd,rs2,rd */
{ "btog",	F3(2, 0x03, 1), F3(~2, ~0x03, ~1),		"i,r", F_ALIAS, v6 }, /* xor rd,i,rd */

/* FPop1 and FPop2 are not instructions.  Don't accept them.  */

{ "fdtoi",	F3F(2, 0x34, 0x0d2), F3F(~2, ~0x34, ~0x0d2)|RS1_G0, "B,g", 0, v6 },
{ "fstoi",	F3F(2, 0x34, 0x0d1), F3F(~2, ~0x34, ~0x0d1)|RS1_G0, "f,g", 0, v6 },
{ "fqtoi",	F3F(2, 0x34, 0x0d3), F3F(~2, ~0x34, ~0x0d3)|RS1_G0, "R,g", 0, v8 },


{ "fitod",	F3F(2, 0x34, 0x0c8), F3F(~2, ~0x34, ~0x0c8)|RS1_G0, "f,H", 0, v6 },
{ "fitos",	F3F(2, 0x34, 0x0c4), F3F(~2, ~0x34, ~0x0c4)|RS1_G0, "f,g", 0, v6 },
{ "fitoq",	F3F(2, 0x34, 0x0cc), F3F(~2, ~0x34, ~0x0cc)|RS1_G0, "f,J", 0, v8 },


{ "fdtoq",	F3F(2, 0x34, 0x0ce), F3F(~2, ~0x34, ~0x0ce)|RS1_G0, "B,J", 0, v8 },
{ "fdtos",	F3F(2, 0x34, 0x0c6), F3F(~2, ~0x34, ~0x0c6)|RS1_G0, "B,g", 0, v6 },
{ "fqtod",	F3F(2, 0x34, 0x0cb), F3F(~2, ~0x34, ~0x0cb)|RS1_G0, "R,H", 0, v8 },
{ "fqtos",	F3F(2, 0x34, 0x0c7), F3F(~2, ~0x34, ~0x0c7)|RS1_G0, "R,g", 0, v8 },
{ "fstod",	F3F(2, 0x34, 0x0c9), F3F(~2, ~0x34, ~0x0c9)|RS1_G0, "f,H", 0, v6 },
{ "fstoq",	F3F(2, 0x34, 0x0cd), F3F(~2, ~0x34, ~0x0cd)|RS1_G0, "f,J", 0, v8 },

{ "fdivd",	F3F(2, 0x34, 0x04e), F3F(~2, ~0x34, ~0x04e), "v,B,H", 0, v6 },
{ "fdivq",	F3F(2, 0x34, 0x04f), F3F(~2, ~0x34, ~0x04f), "V,R,J", 0, v8 },
{ "fdivs",	F3F(2, 0x34, 0x04d), F3F(~2, ~0x34, ~0x04d), "e,f,g", 0, v6 },
{ "fmuld",	F3F(2, 0x34, 0x04a), F3F(~2, ~0x34, ~0x04a), "v,B,H", 0, v6 },
{ "fmulq",	F3F(2, 0x34, 0x04b), F3F(~2, ~0x34, ~0x04b), "V,R,J", 0, v8 },
{ "fmuls",	F3F(2, 0x34, 0x049), F3F(~2, ~0x34, ~0x049), "e,f,g", 0, v6 },

{ "fdmulq",	F3F(2, 0x34, 0x06e), F3F(~2, ~0x34, ~0x06e), "v,B,J", 0, v8 },
{ "fsmuld",	F3F(2, 0x34, 0x069), F3F(~2, ~0x34, ~0x069), "e,f,H", 0, v8 },

{ "fsqrtd",	F3F(2, 0x34, 0x02a), F3F(~2, ~0x34, ~0x02a)|RS1_G0, "B,H", 0, v7 },
{ "fsqrtq",	F3F(2, 0x34, 0x02b), F3F(~2, ~0x34, ~0x02b)|RS1_G0, "R,J", 0, v8 },
{ "fsqrts",	F3F(2, 0x34, 0x029), F3F(~2, ~0x34, ~0x029)|RS1_G0, "f,g", 0, v7 },

{ "fabss",	F3F(2, 0x34, 0x009), F3F(~2, ~0x34, ~0x009)|RS1_G0, "f,g", 0, v6 },
{ "fmovs",	F3F(2, 0x34, 0x001), F3F(~2, ~0x34, ~0x001)|RS1_G0, "f,g", 0, v6 },
{ "fnegs",	F3F(2, 0x34, 0x005), F3F(~2, ~0x34, ~0x005)|RS1_G0, "f,g", 0, v6 },

{ "faddd",	F3F(2, 0x34, 0x042), F3F(~2, ~0x34, ~0x042), "v,B,H", 0, v6 },
{ "faddq",	F3F(2, 0x34, 0x043), F3F(~2, ~0x34, ~0x043), "V,R,J", 0, v8 },
{ "fadds",	F3F(2, 0x34, 0x041), F3F(~2, ~0x34, ~0x041), "e,f,g", 0, v6 },
{ "fsubd",	F3F(2, 0x34, 0x046), F3F(~2, ~0x34, ~0x046), "v,B,H", 0, v6 },
{ "fsubq",	F3F(2, 0x34, 0x047), F3F(~2, ~0x34, ~0x047), "V,R,J", 0, v8 },
{ "fsubs",	F3F(2, 0x34, 0x045), F3F(~2, ~0x34, ~0x045), "e,f,g", 0, v6 },

#define CMPFCC(x)	(((x)&0x3)<<25)

{ "fcmpd",	          F3F(2, 0x35, 0x052),            F3F(~2, ~0x35, ~0x052)|RD_G0,  "v,B",   0, v6 },
{ "fcmped",	          F3F(2, 0x35, 0x056),            F3F(~2, ~0x35, ~0x056)|RD_G0,  "v,B",   0, v6 },
{ "fcmpq",	          F3F(2, 0x34, 0x053),            F3F(~2, ~0x34, ~0x053)|RD_G0,	 "V,R", 0, v8 },
{ "fcmpeq",	          F3F(2, 0x34, 0x057),            F3F(~2, ~0x34, ~0x057)|RD_G0,	 "V,R", 0, v8 },
{ "fcmps",	          F3F(2, 0x35, 0x051),            F3F(~2, ~0x35, ~0x051)|RD_G0, "e,f",   0, v6 },
{ "fcmpes",	          F3F(2, 0x35, 0x055),            F3F(~2, ~0x35, ~0x055)|RD_G0, "e,f",   0, v6 },

{ "cpop1",	F3(2, 0x36, 0), F3(~2, ~0x36, ~1), "[1+2],d", F_ALIAS, v6 },
{ "cpop2",	F3(2, 0x37, 0), F3(~2, ~0x37, ~1), "[1+2],d", F_ALIAS, v6 },
    

};

const int bfd_sparc_num_opcodes = ((sizeof sparc_opcodes)/(sizeof sparc_opcodes[0]));
