/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)format.h	5.1 (Berkeley) 6/7/85
 */

/*
 * format parser definitions
 */

struct syl
{
	short op,p1,p2,rpcnt;
};

/*	do NOT change this defines or add new ones without
 *	changing the value of the following define for OP_TYPE_TAB.
 *	change format.h both in the compiler and libI77 simultaneously.
 */


#define RET	1
#define REVERT 	2
#define GOTO 	3
#define X 	4
#define SLASH 	5
#define STACK 	6
#define I 	7
#define ED 	8
#define NED 	9
#define IM 	10
#define APOS 	11
#define H 	12
#define TL 	13
#define TR 	14
#define T 	15
#define COLON 	16
#define S 	17
#define SP 	18
#define SS 	19
#define P 	20
#define BNZ 	21
#define B 	22
#define F 	23
#define E 	24
#define EE 	25
#define D 	26
#define DE	27		/*** NOT STANDARD FORTRAN ***/
#define G 	28
#define GE 	29
#define L 	30
#define A 	31
#define AW	32
#define R	33		/*** NOT STANDARD FORTRAN ***/
#define DOLAR	34		/*** NOT STANDARD FORTRAN ***/
#define SU	35		/*** NOT STANDARD FORTRAN ***/

#define LAST_TERM SU

/* OP_TYPE_TAB is used in dofio.c .
	  Each value corresponds to a value above, and must be
	  ED for editing terms: I,IM,F,E,EE,D,DE,G,GE,L,A,AW
	  NED for nonediting terms which change the I/O stream:
			X,SLASH,APOS,H,TL,TR,T
	  and just the value of the term for all others.

	  E.g. SP is defined above as 17, so the element 17 of
	  OP_TYPE_TAB (counting from zero) is SP since SP does not
	  read or write data;
	  IM is defined as 10 so the element 10 of OP_TYPE_TAB
	  is ED since IM edits data from the i/o list.
 */
#define OP_TYPE_TAB {0, RET, REVERT, GOTO, NED, NED, STACK, ED, ED, NED, \
			ED, NED, NED, NED, NED, NED, COLON, S, SP, SS, P, \
			BNZ, B, ED, ED, ED, ED, ED, ED, ED, ED, ED, ED, \
			R, DOLAR, SU }

#define	FMTUNKN	-1
#define FMTOK	1
#define FMTERR	0

#define FMT_COMP 0x101		/* indicates pre-compiled formats */

extern struct syl *syl_ptr;
extern short pc;
