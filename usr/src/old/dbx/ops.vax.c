/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ops.vax.c	5.6 (Berkeley) %G%";
#endif /* not lint */

/*
 * Machine operators.
 */

#include "defs.h"
#include "ops.h"

#ifndef public
typedef unsigned char VaxOpcode;

#define O_HALT 0x00		/* halt */
#define O_NOP 0x01		/* no operation */
#define O_REI 0x02		/* return from exception or interrupt */
#define O_BPT 0x03		/* break point fault */
#define O_RET 0x04		/* return from called procedure */
#define O_RSB 0x05		/* return from subroutine */
#define O_LDPCTX 0x06		/* load process context */
#define O_SVPCTX 0x07		/* save process context */
#define O_CVTPS 0x08		/* convert packed to leading separate numeric */
#define O_CVTSP 0x09		/* convert leading separate numeric to packed */
#define O_INDEX 0x0A		/* compute index */
#define O_CRC 0x0B		/* calculate cyclic redundancy check */
#define O_PROBER 0x0C		/* probe read access */
#define O_PROBEW 0x0D		/* probe write access */
#define O_INSQUE 0x0E		/* insert into queue */
#define O_REMQUE 0x0F		/* remove from queue */
#define O_BSBB 0x10		/* branch to subroutine with byte disp */
#define O_BRB 0x11		/* branch with byte disp */
#define O_BNEQ 0x12		/* branch on not equal (also BNEQU) */
#define O_BEQL 0x13		/* branch on equal (also BEQLU */
#define O_BGTR 0x14		/* branch on greater */
#define O_BLEQ 0x15		/* branch on less or equal */
#define O_JSB 0x16		/* jump to subroutine */
#define O_JMP 0x17		/* jump */
#define O_BGEQ 0x18		/* branch on greater or equal */
#define O_BLSS 0x19		/* branch on less */
#define O_BGTRU 0x1A		/* branch on greater unsigned */
#define O_BLEQU 0x1B		/* branch on less of equal unsigned */
#define O_BVC 0x1C		/* branch on overflow clear */
#define O_BVS 0x1D		/* branch on overflow set */
#define O_BCC 0x1E		/* branch on carry clear (also BGEQU) */
#define O_BCS 0x1F		/* branch on carry set (also BLSSU) */
#define O_ADDP4 0x20		/* add packed 4 operand */
#define O_ADDP6 0x21		/* add packed 6 operand */
#define O_SUBP4 0x22		/* subtract packed 4 operand */
#define O_SUBP6 0x23		/* subtract packed 6 operand */
#define O_CVTPT 0x24		/* convert packed to trailing numeric */
#define O_MULP 0x25		/* multiply packed */
#define O_CVTTP 0x26		/* convert trailing numeric to packed */
#define O_DIVP 0x27		/* divide packed */
#define O_MOVC3 0x28		/* move character 3 operand */
#define O_CMPC3 0x29		/* compare character 3 operand */
#define O_SCANC 0x2A		/* scan for character */
#define O_SPANC 0x2B		/* span characters */
#define O_MOVC5 0x2C		/* move character 5 operand */
#define O_CMPC5 0x2D		/* compare character 5 operand */
#define O_MOVTC 0x2E		/* move translated characters */
#define O_MOVTUC 0x2F		/* move translated until character */
#define O_BSBW 0x30		/* branch to subroutine with word disp */
#define O_BRW 0x31		/* branch with word disp */
#define O_CVTWL 0x32		/* convert word to long */
#define O_CVTWB 0x33		/* convert word to byte */
#define O_MOVP 0x34		/* move packed */
#define O_CMPP3 0x35		/* compare packed 3 operand */
#define O_CVTPL 0x36		/* convert packed to long */
#define O_CMPP4 0x37		/* compare packed 4 operand */
#define O_EDITPC 0x38		/* edit packed to character */
#define O_MATCHC 0x39		/* match characters */
#define O_LOCC 0x3A		/* locate characters */
#define O_SKPC 0x3B		/* skip character */
#define O_MOVZWL 0x3C		/* move zero-extended word to long */
#define O_ACBW 0x3D		/* add compare and branch word */
#define O_MOVAW 0x3E		/* move address of word */
#define O_PUSHAW 0x3F		/* push address of word */
#define O_ADDF2 0x40		/* add floating 2 operand */
#define O_ADDF3 0x41		/* add floating 3 operand */
#define O_SUBF2 0x42		/* subtract floating 2 operand */
#define O_SUBF3 0x43		/* subtract floating 3 operand */
#define O_MULF2 0x44		/* multiply floating 2 operand */
#define O_MULF3 0x45		/* multiply floating 3 operand */
#define O_DIVF2 0x46		/* divide floating 2 operand */
#define O_DIVF3 0x47		/* divide floating 3 operand */
#define O_CVTFB 0x48		/* convert float to byte */
#define O_CVTFW 0x49		/* convert float to word */
#define O_CVTFL 0x4A		/* convert float to long */
#define O_CVTRFL 0x4B		/* convert rounded float to long */
#define O_CVTBF 0x4C		/* convert byte to float */
#define O_CVTWF 0x4D		/* convert word to float */
#define O_CVTLF 0x4E		/* convert long to float */
#define O_ACBF 0x4F		/* add compare and branch floating */
#define O_MOVF 0x50		/* move float */
#define O_CMPF 0x51		/* compare floating */
#define O_MNEGF 0x52		/* move negated floating */
#define O_TSTF 0x53		/* test float */
#define O_EMODF 0x54		/* extended modulus floating */
#define O_POLYF 0x55		/* evaluate polynomial floating */
#define O_CVTFD 0x56		/* convert float to double */
#define O_DUMMY57 0x57		/* RESERVED to DIGITAL */
#define O_ADAWI 0x58		/* add aligned word interlocked */
#define O_DUMMY59 0x59		/* RESERVED to DIGITAL */
#define O_DUMMY5a 0x5A		/* RESERVED to DIGITAL */
#define O_DUMMY5b 0x5B		/* RESERVED to DIGITAL */
#define O_INSQHI 0x5C		/* insert into queue head, interlocked */
#define O_INSQTI 0x5D		/* insert into queue tail, interlocked */
#define O_REMQHI 0x5E		/* remove from queue head, interlocked */
#define O_REMQTI 0x5F		/* remove from queue tail, interlocked */
#define O_ADDD2 0x60		/* add double 2 operand */
#define O_ADDD3 0x61		/* add double 3 operand */
#define O_SUBD2 0x62		/* subtract double 2 operand */
#define O_SUBD3 0x63		/* subtrace double 3 operand */
#define O_MULD2 0x64		/* multiply double 2 operand */
#define O_MULD3 0x65		/* multiply double 3 operand */
#define O_DIVD2 0x66		/* divide double 2 operand */
#define O_DIVD3 0x67		/* divide double 3 operand */
#define O_CVTDB 0x68		/* convert double to byte */
#define O_CVTDW 0x69		/* convert double to word */
#define O_CVTDL 0x6A		/* convert double to long */
#define O_CVTRDL 0x6B		/* convert rounded double to long */
#define O_CVTBD 0x6C		/* convert byte to double */
#define O_CVTWD 0x6D		/* convert word to double */
#define O_CVTLD 0x6E		/* convert long to double */
#define O_ACBD 0x6F		/* add compare and branch double */
#define O_MOVD 0x70		/* move double */
#define O_CMPD 0x71		/* compare double */
#define O_MNEGD 0x72		/* move negated double */
#define O_TSTD 0x73		/* test double */
#define O_EMODD 0x74		/* extended modulus double */
#define O_POLYD 0x75		/* evaluate polynomial double */
#define O_CVTDF 0x76		/* convert double to float */
#define O_DUMMY77 0x77		/* RESERVED to DIGITAL */
#define O_ASHL 0x78		/* arithmetic shift long */
#define O_ASHQ 0x79		/* arithmetic shift quad */
#define O_EMUL 0x7A		/* extended multiply */
#define O_EDIV 0x7B		/* extended divide */
#define O_CLRQ 0x7C		/* clear quad (also CLRD) */
#define O_MOVQ 0x7D		/* move quad */
#define O_MOVAQ 0x7E		/* move address of quad (also MOVAD) */
#define O_PUSHAQ 0x7F		/* push address of quad (also PUSHAD) */
#define O_ADDB2 0x80		/* add byte 2 operand */
#define O_ADDB3 0x81		/* add byte 3 operand */
#define O_SUBB2 0x82		/* subtract byte 2 operand */
#define O_SUBB3 0x83		/* subtract byte 3 operand */
#define O_MULB2 0x84		/* multiply byte 2 operand */
#define O_MULB3 0x85		/* multiply byte 3 operand */
#define O_DIVB2 0x86		/* divide byte 2 operand */
#define O_DIVB3 0x87		/* divide byte 3 operand */
#define O_BISB2 0x88		/* bit set byte 2 operand */
#define O_BISB3 0x89		/* bit set byte 3 operand */
#define O_BICB2 0x8A		/* bit clear byte 2 operand */
#define O_BICB3 0x8B		/* bit clear byte 3 operand */
#define O_XORB2 0x8C		/* exclusive or byte 2 operand */
#define O_XORB3 0x8D		/* exclusive or byte 3 operand */
#define O_MNEGB 0x8E		/* move negated byte */
#define O_CASEB 0x8F		/* case byte */
#define O_MOVB 0x90		/* move byte */
#define O_CMPB 0x91		/* compare byte */
#define O_MCOMB 0x92		/* move complemented byte */
#define O_BITB 0x93		/* bit test byte */
#define O_CLRB 0x94		/* clear byte */
#define O_TSTB 0x95		/* test byte */
#define O_INCB 0x96		/* increment byte */
#define O_DECB 0x97		/* decrement byte */
#define O_CVTBL 0x98		/* convert byte to long */
#define O_CVTBW 0x99		/* convert byte to word */
#define O_MOVZBL 0x9A		/* move zero-extended byte to long */
#define O_MOVZBW 0x9B		/* move zero-extended byte to word */
#define O_ROTL 0x9C		/* rotate long */
#define O_ACBB 0x9D		/* add compare and branch byte */
#define O_MOVAB 0x9E		/* move address of byte */
#define O_PUSHAB 0x9F		/* push address of byte */
#define O_ADDW2 0xA0		/* add word 2 operand */
#define O_ADDW3 0xA1		/* add word 3 operand */
#define O_SUBW2 0xA2		/* subtract word 2 operand */
#define O_SUBW3 0xA3		/* subtract word 3 operand */
#define O_MULW2 0xA4		/* multiply word 2 operand */
#define O_MULW3 0xA5		/* multiply word 3 operand */
#define O_DIVW2 0xA6		/* divide word 2 operand */
#define O_DIVW3 0xA7		/* divide word 3 operand */
#define O_BISW2 0xA8		/* bit set word 2 operand */
#define O_BISW3 0xA9		/* bit set word 3 operand */
#define O_BICW2 0xAA		/* bit clear word 2 operand */
#define O_BICW3 0xAB		/* bit clear word 3 operand */
#define O_XORW2 0xAC		/* exclusive or word 2 operand */
#define O_XORW3 0xAD		/* exclusive or word 3 operand */
#define O_MNEGW 0xAE		/* move negated word */
#define O_CASEW 0xAF		/* case word */
#define O_MOVW 0xB0		/* move word */
#define O_CMPW 0xB1		/* compare word */
#define O_MCOMW 0xB2		/* move complemented word */
#define O_BITW 0xB3		/* bit test word */
#define O_CLRW 0xB4		/* clear word */
#define O_TSTW 0xB5		/* test word */
#define O_INCW 0xB6		/* increment word */
#define O_DECW 0xB7		/* decrement word */
#define O_BISPSW 0xB8		/* bit set processor status word */
#define O_BICPSW 0xB9		/* bit clear processor status word */
#define O_POPR 0xBA		/* pop register */
#define O_PUSHR 0xBB		/* push register */
#define O_CHMK 0xBC		/* change mode to kernel */
#define O_CHME 0xBD		/* change mode to executive */
#define O_CHMS 0xBE		/* change mode to supervisor */
#define O_CHMU 0xBF		/* change mode to user */
#define O_ADDL2 0xC0		/* add long 2 operand */
#define O_ADDL3 0xC1		/* add long 3 operand */
#define O_SUBL2 0xC2		/* subtract long 2 operand */
#define O_SUBL3 0xC3		/* subtract long 3 operand */
#define O_MULL2 0xC4		/* multiply long 2 operand */
#define O_MULL3 0xC5		/* multiply long 3 operand */
#define O_DIVL2 0xC6		/* divide long 2 operand */
#define O_DIVL3 0xC7		/* divide long 3 operand */
#define O_BISL2 0xC8		/* bit set long 2 operand */
#define O_BISL3 0xC9		/* bit set long 3 operand */
#define O_BICL2 0xCA		/* bit clear long 2 operand */
#define O_BICL3 0xCB		/* bit clear long 3 operand */
#define O_XORL2 0xCC		/* exclusive or long 2 operand */
#define O_XORL3 0xCD		/* exclusive or long 3 operand */
#define O_MNEGL 0xCE		/* move negated long */
#define O_CASEL 0xCF		/* case long */
#define O_MOVL 0xD0		/* move long */
#define O_CMPL 0xD1		/* compare long */
#define O_MCOML 0xD2		/* move complemented long */
#define O_BITL 0xD3		/* bit test long */
#define O_CLRL 0xD4		/* clear long (also CLRF) */
#define O_TSTL 0xD5		/* test long */
#define O_INCL 0xD6		/* increment long */
#define O_DECL 0xD7		/* decrement long */
#define O_ADWC 0xD8		/* add with carry */
#define O_SBWC 0xD9		/* subtrace with carry */
#define O_MTPR 0xDA		/* move to processor register */
#define O_MFPR 0xDB		/* move from processor register */
#define O_MOVPSL 0xDC		/* move processor status longword */
#define O_PUSHL 0xDD		/* push long */
#define O_MOVAL 0xDE		/* move address of long (also MOVAF) */
#define O_PUSHAL 0xDF		/* push address of long (also PUSHAF) */
#define O_BBS 0xE0		/* branch on bit set */
#define O_BBC 0xE1		/* branch on bit clear */
#define O_BBSS 0xE2		/* branch on bit set and set */
#define O_BBCS 0xE3		/* branch on bit clear and set */
#define O_BBSC 0xE4		/* branch on bit set and clear */
#define O_BBCC 0xE5		/* branch on bit clear and clear */
#define O_BBSSI 0xE6		/* branch on bit set and set interlocked */
#define O_BBCCI 0xE7		/* branch on bit clear and clear interlocked */
#define O_BLBS 0xE8		/* branch on low bit set */
#define O_BLBC 0xE9		/* branch on low bit clear */
#define O_FFS 0xEA		/* find first set bit */
#define O_FFC 0xEB		/* find first clear bit */
#define O_CMPV 0xEC		/* compare field */
#define O_CMPZV 0xED		/* compare zero-extended field */
#define O_EXTV 0xEE		/* extract field */
#define O_EXTZV 0xEF		/* extract zero-extended field */
#define O_INSV 0xF0		/* insert field */
#define O_ACBL 0xF1		/* add compare and branch long */
#define O_AOBLSS 0xF2		/* add one and branch on less */
#define O_AOBLEQ 0xF3		/* add one and branch on less or equal */
#define O_SOBGEQ 0xF4		/* subtract one and branch on gtr or equal */
#define O_SOBGTR 0xF5		/* subtract one and branch on greater */
#define O_CVTLB 0xF6		/* convert long to byte */
#define O_CVTLW 0xF7		/* convert long to word */
#define O_ASHP 0xF8		/* arithmetic shift and round packed */
#define O_CVTLP 0xF9		/* convert long to packed */
#define O_CALLG 0xFA		/* call with general argument list */
#define O_CALLS 0xFB		/* call with stack */
#define O_XFC 0xFC		/* extended function call */
#define O_ESCD 0xFD		/* ESCD to DIGITAL */
#define O_ESCE 0xFE		/* ESCE to DIGITAL */
#define O_ESCF 0xFF		/* ESCF to DIGITAL */

/*
 * Addressing modes.
 */

#define LITSHORT    0x0	/* short literals */
#define LITUPTO31   0x1
#define LITUPTO47   0x2
#define LITUPTO63   0x3
#define INDEX       0x4 /* i[r] */
#define REG	    0x5 /* r */
#define REGDEF      0x6 /* (r) */
#define AUTODEC     0x7 /* -(r) */
#define AUTOINC     0x8 /* (r)+ */
#define AUTOINCDEF  0x9 /* *(r)+ */
#define BYTEDISP    0xA /* BD(r) */
#define BYTEDISPDEF 0xB /* *BD(r) */
#define WORDDISP    0xC /* WD(r) */
#define WORDDISPDEF 0xD /* *WD(r) */
#define LONGDISP    0xE /* LD(r) */
#define LONGDISPDEF 0xF /* *LD(r) */

#define is_branch_disp(arg) ((arg & ACCB) != 0)
#define typelen(arg)        (arg & 0xF)
#define regnm(mode)	    (mode & 0xF)
#define addrmode(mode)      (mode >> 4)

/*
 * Operator information structure.
 */

typedef struct {
    char *iname;
    unsigned char format;
    unsigned char val;
    char numargs;
    char argtype[6];
} Optab;

#ifndef ASINSTRS
#define ASINSTRS "../../bin/as/as.vax/instrs.h"
#endif

#ifndef ADBINSTRS
#define ADBINSTRS "../../bin/adb/adb.vax/instrs.adb"
#endif

#define INSTTAB
#include ASINSTRS

#endif

#define OP(name,eopcode,popdcode,nargs,a1,a2,a3,a4,a5,a6) {name,eopcode,popdcode,nargs,a1,a2,a3,a4,a5,a6}

public Optab optab[] = {
#include ADBINSTRS
0};

/*
 * Register names.
 */

public String regname[] = {
    "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
    "r8", "r9", "r10","r11","ap", "fp", "sp", "pc"
};

/*
 * Floating point immediate operands.
 */

public String fltimm[] = {
    "0.5", "0.5625", "0.625", "0.6875", "0.75", "0.8125", "0.875", "0.9375",
    "1.0", "1.125", "1.25", "1.375", "1.5", "1.625", "1.75", "1.875",
    "2.0", "2.25", "2.5", "2.75", "3.0", "3.25", "3.5", "3.75",
    "4.0", "4.5", "5.0", "5.5", "6.0", "6.5", "7.0", "7.5",
    "8.0", "9.0", "10.0", "11.0", "12.0", "13.0", "14.0", "15.0",
    "16.0", "18.0", "20.0", "22.0", "24.0", "26.0", "28.0", "30.0",
    "32.0", "36.0", "40.0", "44.0", "48.0", "52.0", "56.0", "60.0",
    "64.0", "72.0", "80.0", "88.0", "96.0", "104.0", "112.0", "120.0"
};
