/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)operand.h	5.2 (Berkeley) %G%
 */

/*
 * operand.h  --  definitions useful to VAX operand decoding
 */

#define	opmode(op)	(op & 0xf0)
#define	opregn(op)	(op & 0xf)

/*
 * operand modes
 */
#define	LITERAL0	0x00
#define	LITERAL1	0x10
#define	LITERAL2	0x20
#define	LITERAL3	0x30
#define	INDEXED		0x40
#define	REGISTER	0x50
#define	REGDEFERED	0x60
#define	AUTODEC		0x70
#define	AUTOINC		0x80
#define	AUTOINCDEF	0x90
#define	BYTEDISP	0xa0
#define	BYTEDISPDEF	0xb0
#define	WORDDISP	0xc0
#define	WORDDISPDEF	0xd0
#define	LONGDISP	0xe0
#define	LONGDISPDEF	0xf0
/*
 * Modes where R is PC
 */
#define	IMMEDIATE	0x8f
#define	ABSOLUTE	0x9f
#define	BYTEREL		0xaf
#define	BYTERELDEF	0xbf
#define	WORDREL		0xcf
#define	WORDRELDEF	0xdf
#define	LONGREL		0xef
#define	LONGRELDEF	0xff

/*
 * register definitions
 */
#define	R0	0
#define	R1	1
#define	R2	2
#define	R3	3
#define	R4	4
#define	R5	5
#define	R6	6
#define	R7	7
#define	R8	8
#define	R9	9
#define	R10	10
#define	R11	11
#define	AP	12
#define	FP	13
#define	SP	14
#define	PC	15
