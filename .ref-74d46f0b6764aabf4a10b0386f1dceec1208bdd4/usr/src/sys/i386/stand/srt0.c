/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)srt0.c	7.1 (Berkeley) %G%
 */

/*
 * Startup code for standalone system
 * Non-relocating version -- for programs which are loaded by boot
 * Relocating version for boot
 */

	.globl	_end
	.globl	_edata
	.globl	_main
	.globl	__rtt
	.globl	_exit
	.globl	_howto
	.globl	_bootdev
	.globl	_unit


entry:	.globl	entry
	cli				# no interrupts
#ifdef REL
	movl	$RELOC,%esp
#else
	movl	%esp,savearea
	movl	%ebp,savearea+4
	movl	$RELOC-0x2400,%esp
#endif
	movl	%esp,%ecx
start:
	movl	$_edata,%eax
#ifndef foo
#else
	subl	%eax,%ecx
1:
	movl	$0,(%eax)
	addl	$4,%eax
	loopnz	1f
#endif
#ifdef REL
	# movl	$entry-RELOC,%esi	# from beginning of ram
	movl	$0,%esi
	movl	$entry,%edi		# to relocated area
	# movl	$_edata-RELOC,%ecx	# this much
	movl	$64*1024,%ecx
	cld
	rep
	movsb
	.globl	begin
	# jmp	*$begin	-- does not work, why!?
	pushl	$begin
	ret
begin:
#endif
	# movl	%esi,_howto
	# movl	%edi,_bootdev
	# movl	%ebx,_unit
1:
	# calls	$0,_configure
	movl	$1,_openfirst
	pushl	$0
	popf
	call	_main
#ifdef REL
	jmp	1b
#else
	jmp	1f
#endif

	.data
_openfirst:	.long	0
_bootdev:	.long	0
_howto:		.long	0
_unit:		.long	0
savearea:	.long	0,0	# sp & bp to return to
	.text
	.globl _getchar
	.globl _wait

__rtt:
	call	_getchar
	pushl	$1000000
	call	_wait
	popl	%eax
	movl	$-7,%eax
	jmp	1f
_exit:
	call	_getchar
	pushl	$1000000
	call	_wait
	popl	%eax
	movl	4(sp),%eax
1:
#ifdef	REL
	movw	$0x1234,%ax
	movw	%ax,0x472	# warm boot
	movl	$0,%esp		# segment violation
	ret
	# jump	PA_Monitor		# jump to startup code in ROM
#else
	movl	savearea,%esp
	movl	savearea+4,%ebp
	ret
#endif
	.globl _setregs
_setregs:
	movl	_howto,%esi
	movl	_bootdev,%edi
	movl	_unit,%ebx
	ret

	.globl	_inb
_inb:	movl	4(%esp),%edx
	subl	%eax,%eax	# clr eax
	nop
	inb	%dx,%al
	nop
	ret

	.globl	_outb
_outb:	movl	4(%esp),%edx
	movl	8(%esp),%eax
	nop
	outb	%al,%dx
	nop
	ret

	.globl ___udivsi3
___udivsi3:
	movl 4(%esp),%eax
	xorl %edx,%edx
	divl 8(%esp)
	ret

	.globl ___divsi3
___divsi3:
	movl 4(%esp),%eax
	xorl %edx,%edx
	cltd
	idivl 8(%esp)
	ret

	#
	# bzero (base,cnt)
	#

	.globl _bzero
_bzero:
	pushl	%edi
	movl	8(%esp),%edi
	movl	12(%esp),%ecx
	movb	$0x00,%al
	cld
	rep
	stosb
	popl	%edi
	ret

	#
	# bcopy (src,dst,cnt)
	# NOTE: does not (yet) handle overlapped copies
	#

	.globl	_bcopy
_bcopy:
	pushl	%esi
	pushl	%edi
	movl	12(%esp),%esi
	movl	16(%esp),%edi
	movl	20(%esp),%ecx
	cld
	rep
	movsb
	popl	%edi
	popl	%esi
	ret

	# insw(port,addr,cnt)
	.globl	_insw
_insw:
	pushl	%edi
	movw	8(%esp),%dx
	movl	12(%esp),%edi
	movl	16(%esp),%ecx
	cld
	nop
	.byte 0x66,0xf2,0x6d	# rep insw
	nop
	movl	%edi,%eax
	popl	%edi
	ret

	# outsw(port,addr,cnt)
	.globl	_outsw
_outsw:
	pushl	%esi
	movw	8(%esp),%dx
	movl	12(%esp),%esi
	movl	16(%esp),%ecx
	cld
	nop
	.byte 0x66,0xf2,0x6f	# rep outsw
	nop
	movl	%esi,%eax
	popl	%esi
	ret

