/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)srt0.c	7.6 (Berkeley) %G%
 */

#include "../vax/mtpr.h"
#define	LOCORE
#include "../vax/cpu.h"

/*
 * Startup code for standalone system
 * Non-relocating version -- for programs which are loaded by boot
 * Relocating version for boot*
 */

	.globl	_end
	.globl	_edata
	.globl	_main
	.globl	__rtt
	.globl	_configure
	.globl	_cpu
	.globl	_openfirst
	.globl	_boothowto
	.globl	_bootdev

	.set	HIGH,31		# mask for total disable

entry:	.globl	entry
	nop; nop			# .word	0x0101
	mtpr	$HIGH,$IPL		# just in case

#ifdef REL
	# we need to do special stuff on microvaxen
	mfpr	$SID,r0
	cmpzv	$24,$8,r0,$VAX_630
	beql	1f
	cmpzv	$24,$8,r0,$VAX_650
	bneq	2f

	/*
	 * Were we booted by VMB?  If so, r11 is not boothowto,
	 * but rather the address of the `Extended RPB' (see KA630
	 * User's Manual, pp 3-21).  These tests were devised by
	 * richl@tektronix, 11/10/87.
	 */
1:
	cmpl	(r11),r11		# if boothowto, r11 will be small
	bneq	2f			# and these will not fault
	cmpl	4(r11),$0
	bneq	2f
	cmpl	8(r11),$-1
	bneq	2f
	tstl	0xc(r11)
	bneq	2f

	/*
	 * Booted by VMB: get flags from extended rpb.
	 * We can only guess at the boot device (here ra(0,0)).
	 */
	movl	0x30(r11),r11
	movl	$9,r10			# device = ra(0,0)
2:
	movl	$RELOC,sp
#else
	movl	$RELOC-0x2400,sp
#endif
start:
#ifndef REL
	/*
	 * Clear bss segment
	 */
	movl	aedata,r0
clr:
	clrl	(r0)+
	cmpl	r0,sp
	jlss	clr
#else
	/*
	 * `entry' below generates a pc-relative reference to the
	 * code, so this works no matter where we are now.
	 * Clear bss segment *after* moving text and data.
	 */
	movc3	aedata,entry,(sp)
dclr:
	clrl	(r3)+
	cmpl	r3,$_end
	jlss	dclr
/* this loop shouldn't be necessary, but is when booting from an ra81 */
xclr:	
	clrl	(r3)+
	cmpl	r3,$0x100000
	jlss	xclr
	jmp	*abegin
begin:
#endif
	movl	r11,_boothowto
	movl	r10,_bootdev
again:
	mtpr	$0,$SCBB
	calls	$0,_configure
	movl	$1,_openfirst
	calls	$0,_main
#ifdef REL
	jmp	again
#else
	ret
#endif

	.data
#ifdef REL
abegin:	.long	begin
aedata:	.long	_edata-RELOC
#else
aedata:	.long	_edata
#endif
_bootdev:	.long	0
_boothowto:	.long	0
	.text

__rtt:
	.word	0x0
#ifdef	REL
	halt
#else
	jmp	start
#endif

	.globl	_badaddr
_badaddr:
	.word	0
	movl	$1,r0
	movl	4(ap),r3
	movl	8(ap),r4
	movl	$4,r2
	movab	9f,(r2)
	bbc	$0,r4,1f; tstb	(r3)
1:	bbc	$1,r4,1f; tstw	(r3)
1:	bbc	$2,r4,1f; tstl	(r3)
1:	clrl	r0			# made it w/o machine checks
2:	movl	$4,r2
	clrl	(r2)
	ret
	.align	2
9:
	casel	_cpu,$1,$VAX_MAX
0:
	.word	8f-0b		# 1 is 780
	.word	5f-0b		# 2 is 750
	.word	5f-0b		# 3 is 730
	.word	6f-0b		# 4 is 8600
	.word	5f-0b		# 5 is 8200
	.word	1f-0b		# 6 is 8800
	.word	1f-0b		# 7 is 610
	.word	5f-0b		# 8 is 630
	.word	1f-0b		# 9 is ???
	.word	1f-0b		# 10 is 650
5:
	mtpr	$0xf,$MCESR
	brb	1f
6:
	mtpr	$0,$EHSR
	brb	1f
8:
	mtpr	$0,$SBIFS
1:
	addl2	(sp)+,sp		# discard mchchk trash
	movab	2b,(sp)
	rei

/*
 * Short assembly versions of strcmp, strcpy, and strlen
 * that do not use special instructions.
 */
	.globl	_strcmp
_strcmp:
	.word	0
	movq	4(ap),r0
0:	cmpb	(r0),(r1)+
	bneq	1f
	tstb	(r0)+
	bneq	0b
	clrl	r0
	ret
1:	cvtbl	(r0),r0
	cvtbl	-(r1),r1
	subl2	r1,r0
	ret

	.globl	_strcpy
_strcpy:
	.word	0
	movq	4(ap),r0
0:	movb	(r1)+,(r0)+
	bneq	0b
	movl	4(ap),r0
	ret

	.globl	_strlen
_strlen:
	.word	0
	movl	4(ap),r0
0:	tstb	(r0)+
	bneq	0b
	decl	r0
	subl2	4(ap),r0
	ret
