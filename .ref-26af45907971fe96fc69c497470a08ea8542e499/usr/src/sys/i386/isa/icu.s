/*-
 * Copyright (C) 1989,90 W. Jolitz
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)icu.s	7.7 (Berkeley) %G%
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
#include <net/netisr.h>

#define DONET(s, c)	; \
	.globl	c ;  \
	btrl	$ s ,_netisr ;  \
	jnb	1f ; \
	call	c ; \
1:

/*
 * Handle return from interrupt after device handler finishes
 *
 * register usage:
 *
 * %ebx is cpl we are going back to
 * %esi is 0 if returning to kernel mode
 *
 * Note that these registers will be preserved though C calls,
 * such as the network interrupt routines.
 */
doreti:
	cli
	popl	%ebx			# flush unit number
	popl	%ebx			# get previous priority
	# now interrupt frame is a trap frame!

	/* compensate for drivers that return with non-zero cpl */
	movl	0x34(%esp), %esi /* cs */
	andl	$3, %esi
	jz	1f

return_to_user_mode: /* entry point from trap and syscall return */

	/* return cs is for user mode: force 0 cpl */
	xorl	%ebx,%ebx
1:

	/* like splx(%ebx), except without special 0 handling */
	cli
	movl	%ebx, %eax
	movw	%ax,_cpl
	orw	_imen,%ax
	outb	%al, $ IO_ICU1+1
	movb	%ah, %al
	outb	%al, $ IO_ICU2+1

	/* return immediately if previous cpl was non-zero */
	cmpw	$0, %bx
	jnz	just_return

	/* do network stuff, if requested, even if returning to kernel mode */
	cmpl	$0,_netisr
	jne	donet

	/* if (returning to user mode && astpending), go back to trap
	 * (check astpending first since it is more likely to be false)
	 */
	cmpl	$0,_astpending
	je	just_return

	testl	%esi, %esi
	jz	just_return

	/* we need to go back to trap */
	popl	%es
	popl	%ds
	popal
	addl	$8,%esp

	pushl	$0
	TRAP (T_ASTFLT)
	/* this doesn't return here ... instead it goes though
	 * calltrap in locore.s
	 */

donet:
	/* like splnet(), except we know the current pri is 0 */
	cli
	movw _netmask, %ax
	movw %ax,_cpl
	orw _imen,%ax
	outb %al, $ IO_ICU1+1
	movb %ah, %al
	outb %al, $ IO_ICU2+1
	sti

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

	btrl	$ NETISR_SCLK,_netisr
	jnb	return_to_user_mode

	/* like splsoftclock */
	cli
	movw $0x8000, %ax
	movw %ax,_cpl
	orw _imen,%ax
	outb %al, $ IO_ICU1+1
	movb %ah, %al
	outb %al, $ IO_ICU2+1
	sti

	# back to an interrupt frame for a moment
	pushl	%eax
	pushl	$0xff	# dummy intr
	call	_softclock
	leal	8(%esp), %esp
	jmp	return_to_user_mode

just_return:
	pop	%es
	pop	%ds
	popa
	leal	8(%esp),%esp
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
	INTR1(0, _highmask, 0) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr1)
	INTR1(1, _highmask, 1) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr2)
	INTR1(2, _highmask, 2) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr3)
	INTR1(3, _highmask, 3) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr4)
	INTR1(4, _highmask, 4) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr5)
	INTR1(5, _highmask, 5) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr6)
	INTR1(6, _highmask, 6) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr7)
	INTR1(7, _highmask, 7) ; call	_isa_strayintr ; INTREXIT1


IDTVEC(intr8)
	INTR2(8, _highmask, 8) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr9)
	INTR2(9, _highmask, 9) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr10)
	INTR2(10, _highmask, 10) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr11)
	INTR2(11, _highmask, 11) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr12)
	INTR2(12, _highmask, 12) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr13)
	INTR2(13, _highmask, 13) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr14)
	INTR2(14, _highmask, 14) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr15)
	INTR2(15, _highmask, 15) ; call	_isa_strayintr ; INTREXIT2

