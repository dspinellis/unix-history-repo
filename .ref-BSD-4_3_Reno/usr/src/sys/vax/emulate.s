/*
 * Copyright (c) 1986, 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mt. Xinu.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)emulate.s	7.5 (Berkeley) 6/28/90
 */

#if VAX630 || VAX650
/*
 * String instruction emulation - MicroVAX only.  These routines are called
 * from locore.s when an "emulate" fault occurs on the MicroVAX.  They are
 * called with the stack set up as follows:
 *
 *	  (sp):	Return address of trap handler
 *	 4(sp):	Instruction Opcode	(also holds PSL result from emulator)
 *	 8(sp):	Instruction PC
 *	12(sp):	Operand 1
 *	16(sp):	Operand 2
 *	20(sp):	Operand 3
 *	24(sp):	Operand 4
 *	28(sp):	Operand 5
 *	32(sp):	Operand 6
 *	36(sp):	old Register 11
 *	40(sp):	old Register 10
 *	44(sp):	Return PC
 *	48(sp):	Return PSL
 *	52(sp): TOS before instruction
 *
 * R11 and r10 are available for use.  If any routine needs to use r9-r1
 * they need to save them first (unless those registers are SUPPOSED to be
 * messed with by the "instruction").  These routines leave their results
 * in registers 0-5 explicitly, as needed, and use the macros defined below
 * to link up with calling routine.
 */

#define return		rsb
#define savepsl		movpsl	4(sp)
#define setpsl(reg)	movl	reg,4(sp)
#define overflowpsl	movl	$2,4(sp)
#define arg1		12(sp)
#define arg2		16(sp)
#define arg3		20(sp)
#define arg4		24(sp)
#define arg5		28(sp)
#define arg6		32(sp)
#define argub(num,reg)	movzbl	8+4*num(sp),reg
#define arguw(num,reg)	movzwl	8+4*num(sp),reg
#define argul(num,reg)	movl	8+4*num(sp),reg
#define argb(num,reg)	cvtbl	8+4*num(sp),reg
#define argw(num,reg)	cvtwl	8+4*num(sp),reg
#define argl(num,reg)	movl	8+4*num(sp),reg
#define toarg(reg,num)	movl	reg,8+4*num(sp)


	.text
	.align	1
	.globl	_EMcrc
_EMcrc:
	argl(1,r11)		# (1) table address == r11
	argl(2,r0)		# (2) initial crc == r0
	argl(4,r3)		# (4) source address == r3
	arguw(3,r2)		# (3) source length == r2
	jeql	Lcrc_out
Lcrc_loop:
	xorb2	(r3)+,r0
	extzv	$0,$4,r0,r10
	extzv	$4,$28,r0,r1
	xorl3	r1,(r11)[r10],r0
	extzv	$0,$4,r0,r10
	extzv	$4,$28,r0,r1
	xorl3	r1,(r11)[r10],r0
	sobgtr	r2,Lcrc_loop
	tstl	r0
Lcrc_out:
	savepsl
	clrl	r1
	return


	.align	1
	.globl	_EMmovtc
_EMmovtc:
	arguw(1,r0)		# (1) source length == r0
	argl(2,r1)		# (2) source address == r1
	argub(3,r11)		# (3) fill character == r11
	argl(4,r3)		# (4) table address == r3
	argl(6,r5)		# (6) destination address == r5
	arguw(5,r4)		# (5) destination length == r4
	jeql	Lmovtc_out
Lmovtc_loop:
	tstl	r0
	jeql	Lmovtc_2loop
	movzbl	(r1)+,r2
	movb	(r3)[r2],(r5)+
	decl	r0
	sobgtr	r4,Lmovtc_loop
	jbr	Lmovtc_out
Lmovtc_2loop:
	movb	r11,(r5)+
	sobgtr	r4,Lmovtc_2loop
Lmovtc_out:
	cmpl	r4,r0
	savepsl
	clrl	r2
	return


	.align	1
	.globl	_EMmovtuc
_EMmovtuc:
	arguw(1,r0)		# (1) source length == r0
	argl(2,r1)		# (2) source address == r1
	argub(3,r11)		# (3) escape character == r11
	argl(4,r3)		# (4) table address == r3
	argl(6,r5)		# (6) destination address == r5
	arguw(5,r4)		# (5) destination length == r4
	jeql	Lmovtuc_out
Lmovtuc_loop:
	tstl	r0
	jeql	Lmovtuc_out
	movzbl	(r1),r2
	movzbl	(r3)[r2],r2
	cmpl	r2,r11
	jeql	Lmovtuc_out
	movzbl	(r1)+,r2
	movb	(r3)[r2],(r5)+
	decl	r0
	sobgtr	r4,Lmovtuc_loop
Lmovtuc_out:
	cmpl	r4,r0
	savepsl
	clrl	r2
	return


	.align	1
	.globl	_EMmatchc
_EMmatchc:
	argl(2,r10)		# (2) substring address == r10
	arguw(3,r2)		# (3) source length == r2
	argl(4,r3)		# (4) source address == r3
	arguw(1,r11)		# (1) substring length == r11
	jeql	Lmatchc_out	# temp source address == r1
	addl2	r10,r11		# temp substring address == r0
	tstl	r2
	jeql	Lmatchc_out
Lmatchc_loop:
	cmpb	(r10),(r3)
	jneq	Lmatchc_fail
	movl	r3,r1
	movl	r10,r0
Lmatchc_2loop:
	cmpl	r0,r11
	jeql	Lmatchc_succ
	cmpb	(r0)+,(r1)+
	jeql	Lmatchc_2loop
Lmatchc_fail:
	incl	r3
	sobgtr	r2,Lmatchc_loop
	movl	r10,r1
	subl3	r10,r11,r0
	jbr	Lmatchc_out
Lmatchc_succ:	
	movl	r1,r3
	movl	r11,r1
	clrl	r0
Lmatchc_out:
	savepsl
	return


	.align	1
	.globl	_EMspanc
_EMspanc:
	argl(2,r1)		# (2) string address == r1
	argub(4,r2)		# (4) character-mask == r2
	argl(3,r3)		# (3) table address == r3
	arguw(1,r0)		# (1) string length == r0
	jeql	Lspanc_out
Lspanc_loop:
	movzbl	(r1),r11
	mcomb	(r3)[r11],r11
	bicb3	r11,r2,r11
	jeql	Lspanc_out
	incl	r1
	sobgtr	r0,Lspanc_loop
Lspanc_out:
	savepsl
	clrl	r2
	return


	.align	1
	.globl	_EMscanc
_EMscanc:
	argl(2,r1)		# (2) string address == r1
	argub(4,r2)		# (4) character-mask == r2
	argl(3,r3)		# (3) table address == r3
	arguw(1,r0)		# (1) string length == r0
	jeql	Lscanc_out
Lscanc_loop:
	movzbl	(r1),r11
	mcomb	(r3)[r11],r11
	bicb3	r11,r2,r11
	jneq	Lscanc_out
	incl	r1
	sobgtr	r0,Lscanc_loop
Lscanc_out:
	savepsl
	clrl	r2
	return


	.align	1
	.globl	_EMskpc
_EMskpc:
	argub(1,r11)		# (1) character == r11
	argl(3,r1)		# (3) string address == r1
	arguw(2,r0)		# (2) string length == r0
	jeql	Lskpc_out	# forget zero length strings
Lskpc_loop:
	cmpb	(r1),r11
	jneq	Lskpc_out
	incl	r1
	sobgtr	r0,Lskpc_loop
Lskpc_out:
	tstl	r0		# be sure of condition codes
	savepsl
	return


	.align	1
	.globl	_EMlocc
_EMlocc:
	argub(1,r11)		# (1) character == r11
	argl(3,r1)		# (3) string address == r1
	arguw(2,r0)		# (2) string length == r0
	jeql	Lskpc_out	# forget zero length strings
Llocc_loop:
	cmpb	(r1),r11
	jeql	Llocc_out
	incl	r1
	sobgtr  r0,Llocc_loop
Llocc_out:
	tstl	r0		# be sure of condition codes
	savepsl
	return


	.align	1
	.globl	_EMcmpc3
_EMcmpc3:
	argl(2,r1)		# (2) string1 address == r1
	argl(3,r3)		# (3) string2 address == r3
	arguw(1,r0)		# (1) strings' length == r0
	jeql	Lcmpc3_out
Lcmpc3_loop:
	cmpb	(r1),(r3)
	jneq	Lcmpc3_out
	incl	r1
	incl	r3
	sobgtr	r0,Lcmpc3_loop
Lcmpc3_out:
	savepsl
	movl	r0,r2
	return


	.align	1
	.globl	_EMcmpc5
_EMcmpc5:
	argl(2,r1)		# (2) string1 address == r1
	argub(3,r11)		# (1) fill character == r11
	arguw(4,r2)		# (1) string2 length == r2
	argl(5,r3)		# (3) string2 address == r3
	arguw(1,r0)		# (1) string1 length == r0
	jeql	Lcmpc5_str2
Lcmpc5_loop:
	tstl	r2
	jeql	Lcmpc5_str1loop
	cmpb	(r1),(r3)
	jneq	Lcmpc5_out
	incl	r1
	incl	r3
	decl	r2
	sobgtr	r0,Lcmpc5_loop
Lcmpc5_str2:
	tstl	r2
	jeql	Lcmpc5_out
Lcmpc5_str2loop:
	cmpb	r11,(r3)
	jneq	Lcmpc5_out
	incl	r3
	sobgtr	r2,Lcmpc5_str2loop
	jbr	Lcmpc5_out
Lcmpc5_str1loop:
	cmpb	(r1),r11
	jneq	Lcmpc5_out
	incl	r1
	sobgtr	r0,Lcmpc5_str1loop
Lcmpc5_out:
	savepsl
	return


/*
 * Packed Decimal string operations
 */

#define POSITIVE	$12
#define NEGATIVE	$13
#define NEGATIVEalt	$11


	.align	1
	.globl	_EMaddp4
_EMaddp4:
	toarg(r9,6)		# save register r9 in arg6 spot
	arguw(1,r11)		# (1) source length == r11
	argl(2,r10)		# (2) source address == r10
	arguw(3,r9)		# (3) destination length == r9
	argl(4,r3)		# (4) destination address == r3
			# arg4 will be needed later
			# arg5 holds destination address of LSNibble temporarily
	ashl	$-1,r11,r11
	addl2	r11,r10		# source address of LSNibble
	incl	r11		# source length is in bytes
	ashl	$-1,r9,r9
	addl2	r9,r3		# r3 = destination address of LSNibble
	incl	r9		# destination length is in bytes
	toarg(r3,5)		#    stored in arg5 spot
	extzv	$0,$4,(r3),r2	# set standard +/- indicators in destination
	cmpl	r2,NEGATIVE
	jeql	L112
	cmpl	r2,NEGATIVEalt
	jeql	L111
	insv	POSITIVE,$0,$4,(r3)
	jbr	L112
L111:
	insv	NEGATIVE,$0,$4,(r3)
L112:
	extzv	$0,$4,(r10),r2	# r2 = standard +/- of source
	cmpl	r2,NEGATIVE
	jeql	L114
	cmpl	r2,NEGATIVEalt
	jeql	L113
	movl	POSITIVE,r2
	jbr	L114
L113:
	movl	NEGATIVE,r2
L114:
	cmpl	r11,r9		# if source is longer than destination
	jleq	L115
	movl	r9,r11		#	set source length == destination length
L115:
	extzv	$4,$4,(r3),r9	# r9 = LSDigit of destination
	extzv	$4,$4,(r10),r1	# r1 = LSDigit of source
	extzv	$0,$4,(r3),r0
	cmpl	r0,r2		# if signs of operands are not equal
	jeql	Laddp4_same	#	do a subtraction
	clrl	r2		# r2 is non-zero if result is non-zero
	subl2	r1,r9		# r9 = "addition" of operands' high nibble
	jbr	L119		# jump into addition loop
Laddp4_diff_loop:
	decl	r3
	extzv	$0,$4,(r3),r0
	addl2	r0,r1		# r1 = carry + next (low) nibble of source
	decl	r10
	extzv	$0,$4,(r10),r0
	subl2	r0,r1		# r1 -= next (low) nibble of destination
	jgeq	L121		# if negative result
	mnegl	$1,r9		#	r9 == carry = -1
	addl2	$10,r1		#	r1 == result += 10
	jbr	L122		# else
L121:
	clrl	r9		#	r9 == carry = 0
L122:
	insv	r1,$0,$4,(r3)	# store result low nibble
	bisl2	r1,r2
	extzv	$4,$4,(r3),r0
	addl2	r0,r9		# r9 = carry + next (high) nibble of source
	extzv	$4,$4,(r10),r0
	subl2	r0,r9		# r9 -= next (high) nibble of destination
L119:
	jgeq	L117		# if negative result
	mnegl	$1,r1		#	r1 == carry = -1
	addl2	$10,r9		#	r9 == result += 10
	jbr	L118		# else
L117:
	clrl	r1		#	r1 == carry = 0
L118:
	insv	r9,$4,$4,(r3)	# store result high nibble
	bisl2	r9,r2		# r2 is non-zero if result is non-zero
	decl	r11		# while (--source length)
	jneq	Laddp4_diff_loop
	argl(4,r10)		# r10 = address of destination MSNibble
	jbr	Laddp4_diff_carry
Laddp4_diff_carlop:
	decl	r3
	extzv	$0,$4,(r3),r0
	addl2	r0,r1		# r1 == carry += next (low) nibble
	jgeq	L127		# if less than zero
	movl	r1,r9		#	r9 == carry (must be -1)
	movl	$9,r1		#	r1 == result = 9
	jbr	L128
L127:				# else
	clrl	r9		#	r9 == carry = 0
L128:
	insv	r1,$0,$4,(r3)	# store result
	bisl2	r1,r2
	extzv	$4,$4,(r3),r0
	addl2	r0,r9		# r9 == carry += next (high) nibble
	jgeq	L129		# if less than zero
	movl	r9,r1		# r1 == carry (must be -1)
	movl	$9,r9		# r9 == result = 9
	jbr	L130
L129:
	clrl	r1
L130:
	insv	r9,$4,$4,(r3)	# store result
	bisl2	r9,r2
Laddp4_diff_carry:
	cmpl	r3,r10
	jneq	Laddp4_diff_carlop
	tstl	r1		#	if carry out of MSN then fix up result
	jeql	Laddp4_add_done
	argl(5,r3)		# r3 == address of LSN of destination
	extzv	$0,$4,(r3),r0
	cmpl	r0,NEGATIVE	# switch sign of result
	jneq	L132
	insv	POSITIVE,$0,$4,(r3)
	jbr	L133
L132:
	insv	NEGATIVE,$0,$4,(r3)
L133:
	extzv	$4,$4,(r3),r0	# normalize result (carry out of MSN into LSN)
	subl3	r0,$10,r9	# r9 = 10 - destination LSNibble
	jbr	L134
L137:
	movl	$9,r1
Laddp4_diff_norm:
	insv	r9,$4,$4,(r3)
	cmpl	r3,r10		# while (not at MSNibble)
	jeql	Laddp4_add_done
	decl	r3
	extzv	$0,$4,(r3),r0	# low nibble = (9 + carry) - low nibble
	subl2	r0,r1
	cmpl	r1,$9
	jleq	L135
	clrl	r1
	movl	$10,r9
	jbr	L136
L135:
	movl	$9,r9
L136:
	insv	r1,$0,$4,(r3)
	extzv	$4,$4,(r3),r0	# high nibble = (9 + carry) - high nibble
	subl2	r0,r9
L134:
	cmpl	r9,$9
	jleq	L137
	clrl	r9
	movl	$10,r1
	jbr	Laddp4_diff_norm

Laddp4_same:			# operands are of the same sign
	clrl	r2
	addl2	r1,r9
	jbr	L139
Laddp4_same_loop:
	decl	r3
	extzv	$0,$4,(r3),r0
	addl2	r0,r1		# r1 == carry += next (low) nibble of dest
	decl	r10
	extzv	$0,$4,(r10),r0
	addl2	r0,r1		# r1 += next (low) nibble of source
	cmpl	r1,$9		# if result > 9
	jleq	L141
	movl	$1,r9		#	r9 == carry = 1
	subl2	$10,r1		#	r1 == result -= 10
	jbr	L142
L141:				# else
	clrl	r9		#	r9 == carry = 0
L142:
	insv	r1,$0,$4,(r3)	# store result
	bisl2	r1,r2
	extzv	$4,$4,(r10),r0
	addl2	r0,r9		# ditto for high nibble
	extzv	$4,$4,(r3),r0
	addl2	r0,r9
L139:
	cmpl	r9,$9
	jleq	L143
	movl	$1,r1
	subl2	$10,r9
	jbr	L144
L143:
	clrl	r1
L144:
	insv	r9,$4,$4,(r3)
	bisl2	r9,r2
	sobgtr	r11,Laddp4_same_loop	# while (--source length)
	argl(4,r10)		# r10 = destination address of MSNibble
	jbr	Laddp4_same_carry
Laddp4_same_cloop:
	decl	r3
	extzv	$0,$4,(r3),r0	# propagate carry up to MSNibble of destination
	addl2	r0,r1
	cmpl	r1,$10
	jneq	L147
	movl	$1,r9
	clrl	r1
	jbr	L148
L147:
	clrl	r9
L148:
	insv	r1,$0,$4,(r3)
	bisl2	r1,r2
	extzv	$4,$4,(r3),r0
	addl2	r0,r9
	cmpl	r9,$10
	jneq	L149
	movl	$1,r1
	clrl	r9
	jbr	L150
L149:
	clrl	r1
L150:
	insv	r9,$4,$4,(r3)
	bisl2	r9,r2
Laddp4_same_carry:
	cmpl	r3,r10
	jneq	Laddp4_same_cloop

Laddp4_add_done:
	argl(5,r3)		# r3 = destination address of LSNibble
	tstl	r2		# if zero result
	jneq	L151
	savepsl			#	remember that for condition codes
	insv	POSITIVE,$0,$4,(r3) #	make sure sign of result is positive
	jbr	Laddp4_out
L151:				# else
	extzv	$0,$4,(r3),r0
	cmpl	r0,NEGATIVE	#	if result is negative
	jneq	Laddp4_out
	mnegl	r2,r2		#		remember THAT in Cond Codes
	savepsl
Laddp4_out:
	argl(4,r3)
	argl(2,r1)
	clrl	r0
	clrl	r2
	argl(6,r9)		# restore r9 from stack
	return


	.align	1
	.globl	_EMmovp
_EMmovp:
	arguw(1,r11)		# (1) string length == r11
	argl(2,r10)		# (1) source address == r10
	argl(3,r3)		# (1) destination address == r3
			# we will need arg2 and arg3 later
	clrl	r2		# r2 == non-zero if source is non-zero
	ashl	$-1,r11,r11	# length is number of bytes, not nibbles
	jeql	Lmovp_zlen
Lmovp_copy:
	bisb2	(r10),r2	# keep track of non-zero source
	movb	(r10)+,(r3)+	# move two nibbles
	sobgtr	r11,Lmovp_copy	# loop for length of source
Lmovp_zlen:
	extzv	$4,$4,(r10),r0	# look at least significant nibble
	bisl2	r0,r2
	extzv	$0,$4,(r10),r0	# check sign nibble
	cmpl	r0,NEGATIVEalt
	jeql	Lmovp_neg
	cmpl	r0,NEGATIVE
	jneq	Lmovp_pos
Lmovp_neg:			# source was negative
	mnegl	r2,r2
Lmovp_pos:
	tstl	r2		# set condition codes
	savepsl
	jeql	Lmovp_zero
	movb	(r10),(r3)	# move last byte if non-zero result
	jbr	Lmovp_out
Lmovp_zero:
	movb	POSITIVE,(r3)	#	otherwise, make result zero and positive
Lmovp_out:
	clrl	r0
	argl(2,r1)
	clrl	r2
	argl(3,r3)
	return


/*
 *	Definitions for Editpc instruction
 *
 *  Here are the commands and their corresponding hex values:
 *
 *	EPend		0x00
 *	EPend_float	0x01
 *	EPclear_signif	0x02
 *	EPset_signif	0x03
 *	EPstore_sign	0x04
 *	EPload_fill	0x40
 *	EPload_sign	0x41
 *	EPload_plus	0x42
 *	EPload_minus	0x43
 *	EPinsert	0x44
 *	EPblank_zero	0x45
 *	EPreplace_sign	0x46
 *	EPadjust_input	0x47
 *	EPfill		0x80
 *	EPmove		0x90
 *	EPfloat		0xa0
 *
 *
 *  r4 is carved up as follows:
 *
 *	------------------------------------------- 
 *     |                                   N Z V C |
 *	-------------------------------------------
 *
 *	fill character is stuffed into arg5 space
 *	sign character is stuffed into arg6 space
 */

#define SIGNIFBIT	$0
#define setsignif	bisl2	$1,r4
#define clsignif	bicl2	$1,r4
#define OVERFLOWBIT	$1
#define setoverflow	bisl2	$2,r4
#define cloverflow	bicl2	$2,r4
#define ZEROBIT		$2
#define setzero		bisl2	$4,r4
#define clzero		bicl2	$4,r4
#define NEGATIVEBIT	$3
#define setnegative	bisl2	$8,r4
#define clnegative	bicl2	$8,r4
#define putfill		movb	arg5,(r5)+
#define setfill(reg)	movb	reg,arg5
#define putsign		movb	arg6,(r5)+
#define setsign(reg)	movb	reg,arg6


	.align	1
	.globl	_EMeditpc
_EMeditpc:
	arguw(1,r11)		# (1) source length == r11
	argl(2,r10)		# (2) source address == r10
	argl(3,r3)		# (3) pattern address == r3
	argl(4,r5)		# (4) destination address == r5
			# we will need arg1 and arg2 later
			# arg5 and arg6 are used for fill and sign - r0 is free
	setfill($32)		# fill character is ' '
	setsign($32)		# sign character is ' '
	clrl	r4		# clear flags
	ashl	$-1,r11,r11	# source length / 2
	addl3	r11,r10,r2
	extzv	$4,$4,(r2),r1	# r1 == least significant nibble of source
L169:
	cmpl	r2,r10
	jeql	L170
	tstb	-(r2)		# loop over source packed decimal number
	jeql	L169
	incl	r1		# r1 is non-zero if source is non-zero
L170:
	addl3	r11,r10,r2
	tstl	r1
	jeql	L172		# source is zero - set flags
	extzv	$0,$4,(r2),r11
	cmpl	r11,NEGATIVEalt
	jeql	L9998		# source is negative - set sign and flags
	cmpl	r11,NEGATIVE
	jneq	L175
L9998:
	setnegative
	setsign($45)		# sign character is '-'
	jbr	L175
L172:
	setzero
L175:
	arguw(1,r2)		# (1) source length == r2
Ledit_case:
	movzbl	(r3)+,r11	# get next edit command (pattern)
	cmpl	r11,$128
	jlss	L180
	extzv	$0,$4,r11,r1	# command has a "count" arg - into r1
	ashl	$-4,r11,r11	# and shift over
L180:
	jbc	$6,r11,L181	# "shift" those commands > 64 to 16 and up
	subl2	$48,r11
L181:
	caseb	r11,$0,$0x18	# "do" the command
				# r11 is available for use, r1 has "count" in it
Lcaseb_label:
	.word	Le_end - Lcaseb_label		# 00
	.word	Le_end_float - Lcaseb_label	# 01
	.word	Le_clear_signif - Lcaseb_label	# 02
	.word	Le_set_signif - Lcaseb_label	# 03
	.word	Le_store_sign - Lcaseb_label	# 04
	.word	Le_end - Lcaseb_label		# 05
	.word	Le_end - Lcaseb_label		# 06
	.word	Le_end - Lcaseb_label		# 07
	.word	Le_fill - Lcaseb_label		# 80
	.word	Le_move - Lcaseb_label		# 90
	.word	Le_float - Lcaseb_label		# a0
	.word	Le_end - Lcaseb_label		# b0
	.word	Le_end - Lcaseb_label		# c0
	.word	Le_end - Lcaseb_label		# d0
	.word	Le_end - Lcaseb_label		# e0
	.word	Le_end - Lcaseb_label		# f0
	.word	Le_load_fill - Lcaseb_label	# 40
	.word	Le_load_sign - Lcaseb_label	# 41
	.word	Le_load_plus - Lcaseb_label	# 42
	.word	Le_load_minus - Lcaseb_label	# 43
	.word	Le_insert - Lcaseb_label	# 44
	.word	Le_blank_zero - Lcaseb_label	# 45
	.word	Le_replace_sign - Lcaseb_label	# 46
	.word	Le_adjust_input - Lcaseb_label	# 47
Le_end:
	arguw(1,r0)
	argl(2,r1)
	clrl	r2
	decl	r3
	setpsl(r4)
	clrl	r4
	return

Le_end_float:
	jbs	SIGNIFBIT,r4,Ledit_case	# if significance not set
	putsign				# drop in the sign
					# fall into...
Le_set_signif:
	setsignif
	jbr	Ledit_case

Le_clear_signif:
	clsignif
	jbr	Ledit_case

Le_store_sign:
	putsign
	jbr	Ledit_case

Le_load_fill:
	setfill((r3)+)
	jbr	Ledit_case

Le_load_plus:
	jbs	NEGATIVEBIT,r4,Lpattern_inc	# if non-negative
					# fall into...
Le_load_sign:
	setsign((r3)+)
	jbr	Ledit_case

Le_load_minus:
	jbs	NEGATIVEBIT,r4,Le_load_sign	# if negative load the sign
	incl	r3			# else increment pattern
	jbr	Ledit_case

Le_insert:
	jbc	SIGNIFBIT,r4,L196	# if significance set, put next byte
	movb	(r3)+,(r5)+
	jbr	Ledit_case
L196:					# else put in fill character
	putfill
					# and throw away character in pattern
Le_replace_sign:			# we don't do anything with
Lpattern_inc:				# replace sign `cause we don't
	incl	r3			# get negative zero
	jbr	Ledit_case

Le_blank_zero:
	jbc	ZEROBIT,r4,Lpattern_inc	# if zero
	movzbl	(r3)+,r11		# next byte is a count
	jeql	Ledit_case
	subl2	r11,r5			# to back up over output and replace
L200:
	putfill				# with fill character
	sobgtr	r11,L200
	jbr	Ledit_case

Le_adjust_input:
	movzbl	(r3)+,r0		# get count of nibbles from pattern
	subl3	r2,r0,r11
	jgeq	Ledit_case		# if length of source is > this number
L204:					# discard digits in source
	jlbc	r2,L206			# use low bit of length to choose nibble
	bitb	$0xf0,(r10)		# high nibble
	jeql	L208
	setsignif			# set significance and overflow if
	setoverflow			#    wasted digit is non-zero
	jbr	L208
L206:
	bitb	$0xf,(r10)		# low nibble
	jeql	L209
	setsignif
	setoverflow
L209:
	incl	r10			# increment to next byte
L208:
	decl	r2			# decrement source length
	incl	r11			# continue `till we're out of excess
	jlss	L204
	jbr	Ledit_case

Le_fill:
	tstl	r1			# put (count in r1) fill characters
	jeql	Ledit_case
Le_fill_loop:
	putfill
	sobgtr	r1,Le_fill_loop
	jbr	Ledit_case

Le_move:
	tstl	r1			# move (count in r1) characters
	jeql	Ledit_case		# from source to destination
L214:
	jlbc	r2,L215			# read a nibble
	extzv	$4,$4,(r10),r11
	jbr	L216
L215:
	extzv	$0,$4,(r10),r11
	incl	r10
L216:
	decl	r2			# source length CAN go negative here...
	tstl	r11
	jeql	L218			# if non-zero
	setsignif			# set significance
L218:
	jbc	SIGNIFBIT,r4,L219	# if significance set
	addb3	$48,r11,(r5)+		# put '0' + digit into destination
	jbr	L220
L219:					# else put fill character
	putfill
L220:
	sobgtr	r1,L214
	jbr	Ledit_case

Le_float:				# move with floating sign character
	tstl	r1
	jeql	Ledit_case
L221:
	jlbc	r2,L222
	extzv	$4,$4,(r10),r11
	jbr	L223
L222:
	extzv	$0,$4,(r10),r11
	incl	r10
L223:
	decl	r2			# source length CAN go negative here...
	tstl	r11
	jeql	L225
	jbs	SIGNIFBIT,r4,L226
	putsign
L226:
	setsignif
L225:
	jbc	SIGNIFBIT,r4,L227
	addb3	$48,r11,(r5)+
	jbr	L228
L227:
	putfill
L228:
	sobgtr	r1,L221
	jbr	Ledit_case


	.align	1
	.globl	_EMashp
_EMashp:
	argb(1,r11)		# (1) scale (number to shift) == r11
	arguw(2,r10)		# (2) source length == r10
	argl(3,r1)		# (3) source address == r1
	argub(4,r2)		# (4) rounding factor == r2
	arguw(5,r3)		# (5) destination length == r3
	toarg(r6,3)	# arg3 holds register 6 from caller
	argl(6,r6)		# (6) destination address == r6
			# we need arg6 for later
			# arg1 is used for temporary storage
			# arg2 holds "even or odd" destination length
			# arg4 is used as general storage
			# arg5 is used as general storage
	ashl	$-1,r3,r0	# destination length is number of bytes
	addl2	r0,r6		# destination address == least sig nibble
	toarg(r6,1)		# save in arg1 spot for later
	ashl	$-1,r10,r0
	addl2	r0,r1		# source address == least sig nibble
	extzv	$0,$4,(r1),r0	# determine sign of source
	cmpl	r0,NEGATIVEalt
	jeql	Lashp_neg
	cmpl	r0,NEGATIVE
	jeql	Lashp_neg
	movb	POSITIVE,(r6)
	jbr	L245
Lashp_neg:
	movb	NEGATIVE,(r6)
L245:
	clrl	arg2		# arg2 is 1 if dstlen is even, 0 if odd
	blbs	r3,L246
	incl	arg2
	bisl2	$1,r3		# r3<0> counts digits going into destination
L246:				#	and is flip-flop for which nibble to
	tstl	r11		#	write in destination (1 = high, 0 = low)
	jgeq	Lashp_left	#	(it must start out odd)
	addl2	r11,r10		# scale is negative (right shift)
	jgeq	Lashp_right
	clrl	r10		# test for shifting whole number out
	jbr	Lashp_setround
Lashp_right:
	divl3	$2,r11,r0
	addl2	r0,r1		# source address == MSNibble to be shifted off
	jlbc	r11,L249
	extzv	$4,$4,(r1),r0
	addl2	r0,r2		# round = last nibble to be shifted off + round
	jbr	Lashp_setround
L249:
	extzv	$0,$4,(r1),r0
	addl2	r0,r2		# round = last nibble to be shifted off + round
Lashp_setround:			# r11<0> now is flip-flop for which nibble to
	incl	r11		#    read from source (1 == high, 0 == low)
	cmpl	r2,$9		# set rounding factor to one if nibble shifted
	jleq	Lashp_noround	#    off + round argument was 10 or greater
	movl	$1,r2
	jbr	Lashp_shift
Lashp_zloop:
	jlbs	r3,L257		# don't need to clear high nibble twice
	clrb	-(r6)		# clear low (and high) nib of next byte in dest
L257:
	sobgtr	r3,L258		# move to next nibble in destination, but
	incl	r3		#	don't go beyond the end.
L258:
	decl	r11
Lashp_left:			# while scale is positive
	jneq	Lashp_zloop
	incl	r11		# r11<0> is flip-plop ... (incl sets it to one)
Lashp_noround:
	clrl	r2		# no more rounding
Lashp_shift:
	clrl	arg4		# arg4 will be used for result condition codes
	tstl	r10
	jeql	Lashp_round
Lashp_shloop:
	jlbc	r11,L260
	extzv	$4,$4,(r1),r0
	jbr	L261
L260:
	decl	r1
	extzv	$0,$4,(r1),r0
L261:
	incl	r11		# flip the source nibble flip/flop
	addl2	r0,r2		# round += next nibble
	cmpl	r2,$10		# if round == 10
	jneq	L262
	clrl	arg5		#	then result = 0 and round = 1
	movl	$1,r2
	jbr	L263
L262:				# else
	movl	r2,arg5		#	store result and round = 0
	clrl	r2
L263:
	bisl2	arg5,arg4	# remember if result was nonzero in arg4
	decl	r3		# move to next nibble early to check
	cmpl	r3,arg2		# if we've moved passed destination limits
	jgeq	Lashp_noovfl	#	test the result for possible overflow
	movl	arg2,r3		#	ignore zero nibbles
	tstl	arg5		#	if the nibble was non-zero, overflow
	jeql	L265
	jbr	Lashp_overfl
Lashp_noovfl:			# else
	jlbs	r3,L264
	insv	arg5,$4,$4,(r6)	# put the result into destination (high or low)
	jbr	L265
L264:
	movb	arg5,-(r6)
L265:
	sobgtr	r10,Lashp_shloop	# loop for length of source

Lashp_round:
	tstl	r2		# take care of round out of high nibble
	jeql	Lashp_zeroround
	decl	r3
	cmpl	r3,arg2		# if we've moved passed destination limits
	jlss	Lashp_overfl	#	then overflow
	jlbs	r3,L266
	insv	arg5,$4,$4,(r6)	# put the round into destination (high or low)
	jbr	Lashp_zeroround
L266:
	movb	arg5,-(r6)

Lashp_zeroround:
	argl(1,r10)		# r10 = address of destination LSNibble
	argl(6,r3)		# r3 = address of destination MSNibble
	movl	arg4,r11	# r11 = non-zero if destination == non-zero
	savepsl
	jbr	L267
Lashp_zerofill:
	clrb	-(r6)		# fill up MSNs of destination with zeros
L267:
	cmpl	r3,r6
	jneq	Lashp_zerofill
	extzv	$0,$4,(r10),r0	# test for negative result
	cmpl	r0,NEGATIVE
	jneq	Lashp_out
	mnegl	r11,r11
	savepsl
	jneq	Lashp_out	# turn -0 into 0
	insv	POSITIVE,$0,$4,(r10)
Lashp_out:
	clrl	r0
	argl(3,r6)		# restore r6 from stack
	return
Lashp_overfl:			#    do overflow
	clrl	r2
	overflowpsl
	jbr	Lashp_out


	.align	1
	.globl	_EMcvtlp
_EMcvtlp:
	arguw(2,r10)		# (2) destination length == r10
	argl(3,r3)		# (3) destination address == r3
	ashl	$-1,r10,r10
	addl2	r10,r3		# destination address points to Least Sig byte
	incl	r10		# length is # of bytes, not nibbles
	argl(1,r11)		# (1) source == r11
	savepsl
	jgeq	Lcvtlp_pos
	movb	NEGATIVE,(r3)	# source is negative
	divl3	$10,r11,r0
	mull3	$10,r0,r1
	subl3	r11,r1,r2	# r2 = source mod 10
	mnegl	r0,r11		# source = -(source / 10)
	jbr	Lcvtlp_cvt
Lcvtlp_pos:
	movb	POSITIVE,(r3)	# source is non-negative
	divl3	$10,r11,r0
	mull3	$10,r0,r1
	subl3	r1,r11,r2	# r2 = source mod 10
	movl	r0,r11		# source = source / 10
Lcvtlp_cvt:
	insv	r2,$4,$4,(r3)	# store least significant digit
	tstl	r11
	jeql	Lcvtlp_zloop
Lcvtlp_loop:			# while source is non-zero
	decl	r10		#   and for length of destination ...
	jeql	Lcvtlp_over
	divl3	$10,r11,r1	# r1 = source / 10
	mull3	$10,r1,r0
	subl2	r0,r11		# source = source mod 10
	movb	r11,-(r3)	# store low "nibble" in next significant byte
	divl3	$10,r1,r11	# source = r1 / 10
	mull3	$10,r11,r0
	subl2	r0,r1		# r1 = source mod 10
	insv	r1,$4,$4,(r3)	# store high nibble
	tstl	r11
	jneq	Lcvtlp_loop	# quit if source becomes zero
Lcvtlp_zloop:			# fill any remaining bytes with zeros
	decl	r10
	jeql	Lcvtlp_out
	clrb	-(r3)
	jbr	Lcvtlp_zloop
Lcvtlp_over:
	overflowpsl
Lcvtlp_out:
	clrl	r1		# r0 is already zero
	clrl	r2
	return


	.align	1
	.globl	_EMcvtpl
_EMcvtpl:
	arguw(1,r11)		# (1) source length == r11
	argl(2,r10)		# (2) source address == r10
	clrl	r3		# r3 == destination
	movl	r10,r1		# r1 set up now for return
	ashl	$-1,r11,r11	# source length is number of bytes
	jeql	Lcvtpl_zero
Lcvtpl_loop:			# for source length
	mull2	$10,r3		# destination *= 10
	extzv	$4,$4,(r10),r0
	addl2	r0,r3		# destination += high nibble
	mull2	$10,r3		# destination *= 10
	extzv	$0,$4,(r10),r0
	addl2	r0,r3		# destination += low nibble
	incl	r10
	sobgtr	r11,Lcvtpl_loop
Lcvtpl_zero:			# least significant byte
	mull2	$10,r3
	extzv	$4,$4,(r10),r0
	addl2	r0,r3		# dest = 10 * dest + high nibble
	savepsl
	extzv	$0,$4,(r10),r2	# test sign nibble
	cmpl	r2,NEGATIVE
	jeql	Lcvtpl_neg
	cmpl	r2,NEGATIVEalt
	jneq	Lcvtpl_out
Lcvtpl_neg:			# source was negative - negate destination
	mnegl	r3,r3
	savepsl
Lcvtpl_out:
	toarg(r3,3)
	clrl	r0
	clrl	r2
	clrl	r3
	return


	.align	1
	.globl	_EMcvtps
_EMcvtps:
	return


	.align	1
	.globl	_EMcvtsp
_EMcvtsp:
	return


	.align	1
	.globl	_EMaddp6
_EMaddp6:
	return


	.align	1
	.globl	_EMsubp4
_EMsubp4:
	return


	.align	1
	.globl	_EMsubp6
_EMsubp6:
	return


	.align	1
	.globl	_EMcvtpt
_EMcvtpt:
	return


	.align	1
	.globl	_EMmulp
_EMmulp:
	return


	.align	1
	.globl	_EMcvttp
_EMcvttp:
	return


	.align	1
	.globl	_EMdivp
_EMdivp:
	return


	.align	1
	.globl	_EMcmpp3
_EMcmpp3:
	return


	.align	1
	.globl	_EMcmpp4
_EMcmpp4:
	return


#endif UVAXII


#ifdef notdef
/*
 * Emulation OpCode jump table:
 *	ONLY GOES FROM 0xf8 (-8) TO 0x3B (59)
 */
#define EMUTABLE	0x43
#define NOEMULATE	.long noemulate
#define	EMULATE(a)	.long _EM/**/a
	.globl	_emJUMPtable
_emJUMPtable:
/* f8 */	EMULATE(ashp);	EMULATE(cvtlp);	NOEMULATE;	NOEMULATE
/* fc */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 00 */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 04 */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 08 */	EMULATE(cvtps);	EMULATE(cvtsp);	NOEMULATE;	EMULATE(crc)
/* 0c */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 10 */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 14 */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 18 */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 1c */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 20 */	EMULATE(addp4);	EMULATE(addp6);	EMULATE(subp4);	EMULATE(subp6)
/* 24 */	EMULATE(cvtpt);	EMULATE(mulp);	EMULATE(cvttp);	EMULATE(divp)
/* 28 */	NOEMULATE;	EMULATE(cmpc3);	EMULATE(scanc);	EMULATE(spanc)
/* 2c */	NOEMULATE;	EMULATE(cmpc5);	EMULATE(movtc);	EMULATE(movtuc)
/* 30 */	NOEMULATE;	NOEMULATE;	NOEMULATE;	NOEMULATE
/* 34 */	EMULATE(movp);	EMULATE(cmpp3);	EMULATE(cvtpl);	EMULATE(cmpp4)
/* 38 */	EMULATE(editpc); EMULATE(matchc); EMULATE(locc); EMULATE(skpc)

/*
 * The following is called with the stack set up as follows:
 *
 *	  (sp):	Opcode
 *	 4(sp):	Instruction PC
 *	 8(sp):	Operand 1
 *	12(sp):	Operand 2
 *	16(sp):	Operand 3
 *	20(sp):	Operand 4
 *	24(sp):	Operand 5
 *	28(sp):	Operand 6
 *	32(sp):	Operand 7 (unused)
 *	36(sp):	Operand 8 (unused)
 *	40(sp):	Return PC
 *	44(sp):	Return PSL
 *	48(sp): TOS before instruction
 *
 * Each individual routine is called with the stack set up as follows:
 *
 *	  (sp):	Return address of trap handler
 *	 4(sp):	Opcode (will get return PSL)
 *	 8(sp):	Instruction PC
 *	12(sp):	Operand 1
 *	16(sp):	Operand 2
 *	20(sp):	Operand 3
 *	24(sp):	Operand 4
 *	28(sp):	Operand 5
 *	32(sp):	Operand 6
 *	36(sp):	saved register 11
 *	40(sp):	saved register 10
 *	44(sp):	Return PC
 *	48(sp):	Return PSL
 *	52(sp): TOS before instruction
 */

SCBVEC(emulate):
	movl	r11,32(sp)		# save register r11 in unused operand
	movl	r10,36(sp)		# save register r10 in unused operand
	cvtbl	(sp),r10		# get opcode
	addl2	$8,r10			# shift negative opcodes
	subl3	r10,$EMUTABLE,r11	# forget it if opcode is out of range
	bcs	noemulate
	movl	_emJUMPtable[r10],r10	# call appropriate emulation routine
	jsb	(r10)		# routines put return values into regs 0-5
	movl	32(sp),r11		# restore register r11
	movl	36(sp),r10		# restore register r10
	insv	(sp),$0,$4,44(sp)	# and condition codes in Opcode spot
	addl2	$40,sp			# adjust stack for return
	rei
noemulate:
	addl2	$48,sp			# adjust stack for
	.word	0xffff			# "reserved instruction fault"
SCBVEC(emulateFPD):
	.word	0xffff			# "reserved instruction fault"
#endif
