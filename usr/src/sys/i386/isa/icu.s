/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)icu.s	5.4 (Berkeley) %G%
 */

/*
 * AT/386
 * Vector interrupt control section
 * Copyright (C) 1989,90 W. Jolitz
 */

	.data
	.globl	_imen
	.globl	_cpl
_cpl:	.long	0xffff			# current priority level (all off)
_imen:	.long	0xffff			# interrupt mask enable (all off)
	.globl	_highmask
_highmask:	.long	0xffff
	.globl	_ttymask
_ttymask:	.long	0
	.globl	_biomask
_biomask:	.long	0
	.globl	_netmask
_netmask:	.long	0

	.text
/*
 * Handle return from interrupt after device handler finishes
 */
doreti:
	cli
	popl	%ebx			# remove intr number
	popl	%eax			# get previous priority
	# now interrupt frame is a trap frame!
	movw	%ax,%cx
	movw	%ax,_cpl
	orw	_imen,%ax
	NOP
	outb	%al,$ IO_ICU1+1		# re-enable intr?
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP

	andw	$0xffff,%cx	
	cmpw	$0,%cx			# returning to zero?
	je	1f

	pop	%es			# nope, going to non-zero level
	pop	%ds
	popa
	addl	$8,%esp
	iret

1:	cmpl	$0,_netisr		# check for softint s/traps
	jne	1f

	pop	%es			# none, going back to base pri
	pop	%ds
	popa
	addl	$8,%esp
	iret
	
#include "../net/netisr.h"

1:

#define DONET(s, c)	; \
	.globl	c ;  \
	movl	$ s ,%eax ; 	\
	btrl	%eax,_netisr ;  \
	jnb	1f ; \
	call	c ; \
1:

	call	_splnet
	pushl	%eax

	DONET(NETISR_RAW,_rawintr)
#ifdef INET
	DONET(NETISR_IP,_ipintr)
#endif
#ifdef IMP
	DONET(NETISR_IMP,_impintr)
#endif
#ifdef NS
	DONET(NETISR_NS,_nsintr)
#endif

	popl	%eax
	movw	%ax,_cpl
	orw	_imen,%ax
	NOP
	outb	%al,$ IO_ICU1+1		# re-enable intr?
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP

	# btrl	$ NETISR_SCLK,_netisr
	movl	$ NETISR_SCLK,%eax	# stupid assembler, as usual
	btrl	%eax,_netisr
	jnb	1f
	# back to an interrupt frame for a moment
	call	_splsoftclock
	pushl	%eax
	pushl	$0xff	# dummy intr
	call	_softclock
	popl	%eax
	call	_splx
	popl	%eax

	jmp	2f

	/* 1:	btrl	$NETISR_AST,_netisr*/
1:
	cmpw	$0x1f,13*4(%esp)	# to user?
	jne	2f			# nope, leave
	movl	$ NETISR_AST,%eax	# stupid assembler, as usual
	btrl	%eax,_netisr
	jnb	2f
	call	_trap

2:	pop	%es
	pop	%ds
	popal
	addl	$8,%esp
	iret

/*
 * Interrupt priority mechanism
 *
 * Two flavors	-- imlXX masks relative to ISA noemenclature (for PC compat sw)
 *		-- splXX masks with group mechanism for BSD purposes
 */

	.globl	_splhigh
	.globl	_splclock
_splhigh:
_splclock:
	cli				# disable interrupts
	movw	$0xffff,%ax		# set new priority level
	movw	%ax,%dx
	# orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_spltty			# block clists
_spltty:
	cli				# disable interrupts
	movw	_cpl,%ax
	orw	_ttymask,%ax
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_splimp
	.globl	_splnet
_splimp:
_splnet:
	cli				# disable interrupts
	movw	_cpl,%ax
	orw	_netmask,%ax
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_splbio	
_splbio:
	cli				# disable interrupts
	movw	_cpl,%ax
	orw	_biomask,%ax
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_splsoftclock
_splsoftclock:
	cli				# disable interrupts
	movw	_cpl,%ax
	orw	$0x8000,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl _splnone
	.globl _spl0
_splnone:
_spl0:
	cli				# disable interrupts
	pushl	_cpl			# save old priority
	movw	_cpl,%ax
	orw	_netmask,%ax		# mask off those network devices
	movw	%ax,_cpl		# set new priority level
	orw	_imen,%ax		# mask off those not enabled yet
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP
	sti				# enable interrupts

	DONET(NETISR_RAW,_rawintr)
#ifdef INET
	DONET(NETISR_IP,_ipintr)
#endif
	cli				# disable interrupts
	popl	_cpl			# save old priority
	movw	$0,%ax			# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	cmpw	$0,%dx
	je	_spl0			# going to "zero level" is special

	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

#ifdef notyet
	.globl	_iml8			# mask off all but irq0-1
_iml8:
	cli				# disable interrupts
	movw	$0xfffc,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	NOP
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	NOP
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

#endif	notyet

	/* hardware interrupt catcher (IDT 32 - 47) */
	.globl	_isa_strayintr

IDTVEC(intr0)
	INTR(0) ; call	_isa_strayintr ; INTREXT1

IDTVEC(intr1)
	INTR(1) ; call	_isa_strayintr ; INTREXT1

IDTVEC(intr2)
	INTR(2) ; call	_isa_strayintr ; INTREXT1

IDTVEC(intr3)
	INTR(3) ; call	_isa_strayintr ; INTREXT1

IDTVEC(intr4)
	INTR(4) ; call	_isa_strayintr ; INTREXT1

IDTVEC(intr5)
	INTR(5) ; call	_isa_strayintr ; INTREXT1

IDTVEC(intr6)
	INTR(6) ; call	_isa_strayintr ; INTREXT1

IDTVEC(intr7)
	INTR(7) ; call	_isa_strayintr ; INTREXT1


IDTVEC(intr8)
	INTR(8) ; call	_isa_strayintr ; INTREXT2

IDTVEC(intr9)
	INTR(9) ; call	_isa_strayintr ; INTREXT2

IDTVEC(intr10)
	INTR(10) ; call	_isa_strayintr ; INTREXT2

IDTVEC(intr11)
	INTR(11) ; call	_isa_strayintr ; INTREXT2

IDTVEC(intr12)
	INTR(12) ; call	_isa_strayintr ; INTREXT2

IDTVEC(intr13)
	INTR(13) ; call	_isa_strayintr ; INTREXT2

IDTVEC(intr14)
	INTR(14) ; call	_isa_strayintr ; INTREXT2

IDTVEC(intr15)
	INTR(15) ; call	_isa_strayintr ; INTREXT2

