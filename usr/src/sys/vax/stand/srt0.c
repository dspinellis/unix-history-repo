/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)srt0.c	7.3 (Berkeley) %G%
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
	.word	0x0
	mtpr	$HIGH,$IPL		# just in case
#ifdef REL
	movl	$RELOC,sp
#else
	movl	$RELOC-0x2400,sp
#endif
start:
	movl	aedata,r0
clr:
	clrl	(r0)+
	cmpl	r0,sp
	jlss	clr
#ifdef REL
	movc3	aedata,*$0,(sp)
/*
 * Reclear bss segment separately from text and data
 * since movc3 can't move more than 64K bytes
 */
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
