/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)icu.s	5.2 (Berkeley) %G%
 */

/*
 * AT/386
 * Vector interrupt control section
 * W. Jolitz	8/89
 */

	.data
	.globl	_imen
	.globl	_cpl
_cpl:	.long	0xffff			# current priority level (all off)
_imen:	.long	0xffff			# interrupt mask enable (all off)
	.globl	_ttymask
_ttymask:	.long	IRQ1+IRQ3+IRQ4
	.globl	_biomask
_biomask:	.long	IRQ14+IRQ6
	.text

	.globl	_iml0			# masks off all interrupts
	.globl	_splhigh
	.globl	_splclock
	.globl	_spl6
_iml0:
_spl6:
_splhigh:
_splclock:
	cli				# disable interrupts
	movw	$0xffff,%ax		# set new priority level
	movw	%ax,%dx
	# orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml1			# mask off all but irq0
	.globl	_spltty			# block clists
_iml1:
_spltty:
	cli				# disable interrupts
	# movw	$0xfffe,%ax		# set new priority level
	movw	_cpl,%ax
	orw	_ttymask,%ax
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml8			# mask off all but irq0-1
_iml8:
	cli				# disable interrupts
	movw	$0xfffc,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml9			# mask off all but irq0-1,8
	.globl	_iml2			# alias for PC hardware level 2
	.globl	_splimp
	.globl	_splnet
_iml9:
_iml2:
_splimp:
_splnet:
	cli				# disable interrupts
	# movw	$0xfef8,%ax		# set new priority level
	movw	$0xffff,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml10			# mask off all but irq0-1,8-9
_iml10:
	cli				# disable interrupts
	movw	$0xfcf8,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml11			# mask off all but irq0-1,8-10
_iml11:
	cli				# disable interrupts
	movw	$0xf8f8,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml12			# mask off all but irq0-1,8-11
_iml12:
	cli				# disable interrupts
	movw	$0xf0f8,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml13			# mask off all but irq0-1,8-12
_iml13:
	cli				# disable interrupts
	movw	$0xe0f8,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml14			# mask off all but irq0-1,8-13
	.globl	_splbio	
_iml14:
_splbio:
	cli				# disable interrupts
	movw	_cpl,%ax
	orw	_biomask,%ax
	# movw	$0xffff,%ax		# set new priority level XXX
	# movw	$0xc0f8,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml15			# mask off all but irq0-1,8-14
_iml15:
	cli				# disable interrupts
	movw	$0x80f8,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml3			# mask off all but irq0-1,8-15
_iml3:
	cli				# disable interrupts
	movw	$0x00f8,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml4			# mask off all but irq0-1,8-15,3
_iml4:
	cli				# disable interrupts
	movw	$0x00f0,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml5			# mask off all but irq0-1,8-15,3-4
_iml5:
	cli				# disable interrupts
	movw	$0x00e0,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml6			# mask off all but irq0-1,8-15,3-5
_iml6:
	cli				# disable interrupts
	movw	$0x00c0,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_iml7,_splsoftclock	# mask off all but irq0-1,8-15,3-7
_iml7:
_splsoftclock:
	cli				# disable interrupts
	movw	$0x0080,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl _imlnone			# masks off no interrupts
	.globl _splnone
	.globl _spl0
_imlnone:
_splnone:
_spl0:
	cli				# disable interrupts
	movw	$0,%ax			# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl _splx
_splx:
	cli				# disable interrupts
	movw	4(%esp),%ax		# new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$0x21		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$0xA1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret
