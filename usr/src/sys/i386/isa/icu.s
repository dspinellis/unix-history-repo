/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)icu.s	7.4 (Berkeley) %G%
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
	.globl	_isa_intr
_isa_intr:	.space	16*4

	.text
/*
 * Handle return from interrupt after device handler finishes
 */
doreti:
	cli
	popl	%ebx			# remove intr number
	nop
	popl	%eax			# get previous priority
	nop
	# now interrupt frame is a trap frame!
	movw	%ax,%cx
	movw	%ax,_cpl
	orw	_imen,%ax
	outb	%al,$ IO_ICU1+1		# re-enable intr?
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1

	# andw	$0xffff,%cx	
	cmpw	$0,%cx			# returning to zero?
	je	1f

2:	popl	%es			# nope, going to non-zero level
	nop
	popl	%ds
	nop
	popal
	nop
	addl	$8,%esp
	iret

1:	cmpl	$0,_netisr		# check for softint s/traps
	je	2b

#include "../net/netisr.h"

1:

#define DONET(s, c)	; \
	.globl	c ;  \
	btrl	$ s ,_netisr ;  \
	jnb	1f ; \
	call	c ; \
1:

	call	_splnet
	pushl	%eax


	DONET(NETISR_RAW,_rawintr)
#ifdef INET
	DONET(NETISR_IP,_ipintr)
	DONET(NETISR_ARP,_arpintr)
#endif
#ifdef IMP
	DONET(NETISR_IMP,_impintr)
#endif
#ifdef NS
	DONET(NETISR_NS,_nsintr)
#endif
#ifdef ISO
	DONET(NETISR_ISO,_clnlintr)
#endif
#ifdef CCITT
	DONET(NETISR_CCITT,_hdintr)
#endif

	/* restore interrupt state, but don't turn them on just yet */
	cli
	popl	%eax
	nop
	movw	%ax,_cpl
	orw	_imen,%ax
	outb	%al,$ IO_ICU1+1		# re-enable intr?
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1

	btrl	$ NETISR_SCLK,_netisr
	jnb	1f
	# back to an interrupt frame for a moment
	call	_splsoftclock
	pushl	%eax
	pushl	$0xff	# dummy intr
	call	_softclock
	popl	%eax
	nop
	call	_splx
	popl	%eax
	nop

	jmp	2f

1:
	cmpw	$0x1f,13*4(%esp)	# to user?
	jne	2f			# nope, leave
	btrl	$ NETISR_AST ,_netisr
	jnb	2f
	call	_trap

2:	pop	%es
	nop
	pop	%ds
	nop
	popal
	nop
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	sti				# enable interrupts

	DONET(NETISR_RAW,_rawintr)
#ifdef INET
	DONET(NETISR_IP,_ipintr)
	DONET(NETISR_ARP,_arpintr)
#endif
#ifdef IMP
	DONET(NETISR_IMP,_impintr)
#endif
#ifdef NS
	DONET(NETISR_NS,_nsintr)
#endif
#ifdef ISO
	DONET(NETISR_ISO,_clnlintr)
#endif
#ifdef CCITT
	DONET(NETISR_CCITT,_hdintr)
#endif

	cli				# disable interrupts
	popl	_cpl			# save old priority
	nop

	movw	$0,%ax			# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	outb	%al,$ IO_ICU1+1		/* update icu's */
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
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
	outb	%al,$ IO_ICU1+1		/* update icu's */
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	/* hardware interrupt catcher (IDT 32 - 47) */
	.globl	_isa_strayintr

IDTVEC(intr0)
	INTR(0, _highmask, 0) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr1)
	INTR(1, _highmask, 1) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr2)
	INTR(2, _highmask, 2) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr3)
	INTR(3, _highmask, 3) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr4)
	INTR(4, _highmask, 4) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr5)
	INTR(5, _highmask, 5) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr6)
	INTR(6, _highmask, 6) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr7)
	INTR(7, _highmask, 7) ; call	_isa_strayintr ; INTREXIT1


IDTVEC(intr8)
	INTR(8, _highmask, 8) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr9)
	INTR(9, _highmask, 9) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr10)
	INTR(10, _highmask, 10) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr11)
	INTR(11, _highmask, 11) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr12)
	INTR(12, _highmask, 12) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr13)
	INTR(13, _highmask, 13) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr14)
	INTR(14, _highmask, 14) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr15)
	INTR(15, _highmask, 15) ; call	_isa_strayintr ; INTREXIT2

