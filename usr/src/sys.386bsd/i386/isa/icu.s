/*-
 * Copyright (c) 1989, 1990 William F. Jolitz.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)icu.s	7.2 (Berkeley) 5/21/91
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         1       00064
 * --------------------         -----   ----------------------
 * 
 * 28 Nov 92	Frank MacLachlan	Aligned addresses and data
 *					on 32bit boundaries.
 */

/*
 * AT/386
 * Vector interrupt control section
 */

	.data
	ALIGN32
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
	ALIGN32
doreti:
	cli
	popl	%ebx			# remove intr number
	NOP
	popl	%eax			# get previous priority
	# now interrupt frame is a trap frame!
	movw	%ax,%cx
	movw	%ax,_cpl
	orw	_imen,%ax
	outb	%al,$ IO_ICU1+1		# re-enable intr?
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP

	cmpw	$0x1f,13*4(%esp)	# to user?
	je	1f			# nope, leave
	andw	$0xffff,%cx	
	cmpw	$0,%cx			# returning to zero?
	je	1f

	pop	%es			# nope, going to non-zero level
	pop	%ds
	popa
	addl	$8,%esp
	iret

	ALIGN32
1:	cmpl	$0,_netisr		# check for softint s/traps
	jne	1f
	cmpl	$0,_want_resched
	jne	1f

	pop	%es			# none, going back to base pri
	pop	%ds
	popa
	addl	$8,%esp
	iret
	
#include "../net/netisr.h"

	ALIGN32
1:

#define DONET(s, c)	; \
	.globl	c ;  \
	btrl	$ s ,_netisr ;  \
	jnb	1f ; \
	call	c ; \
1:

	call	_splnet

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

#ifdef notdef
	NOP
	popl	%eax
	movw	%ax,_cpl
	orw	_imen,%ax
	outb	%al,$ IO_ICU1+1		# re-enable intr?
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
#else
	call	_spl0
#endif

	btrl	$ NETISR_SCLK,_netisr
	jnb	1f
	# back to an interrupt frame for a moment
	call	_splsoftclock
	pushl	$0xff	# dummy intr
	call	_softclock
	popl	%eax
	call	_spl0

	# jmp	2f

1:
	cmpw	$0x1f,13*4(%esp)	# to user?
	jne	2f			# nope, leave
	cmpl	$0,_want_resched
	je	2f
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
	ALIGN32
_splhigh:
_splclock:
	cli				# disable interrupts
	NOP
	movw	$0xffff,%ax		# set new priority level
	movw	%ax,%dx
	# orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_spltty			# block clists
	ALIGN32
_spltty:
	cli				# disable interrupts
	NOP
	movw	_cpl,%ax
	orw	_ttymask,%ax
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_splimp
	.globl	_splnet
	ALIGN32
_splimp:
_splnet:
	cli				# disable interrupts
	NOP
	movw	_cpl,%ax
	orw	_netmask,%ax
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_splbio	
	ALIGN32
_splbio:
	cli				# disable interrupts
	NOP
	movw	_cpl,%ax
	orw	_biomask,%ax
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl	_splsoftclock
	ALIGN32
_splsoftclock:
	cli				# disable interrupts
	NOP
	movw	_cpl,%ax
	orw	$0x8000,%ax		# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl _splnone
	.globl _spl0
	ALIGN32
_splnone:
_spl0:
	cli				# disable interrupts
	NOP
	pushl	_cpl			# save old priority
	movw	_cpl,%ax
	orw	_netmask,%ax		# mask off those network devices
	movw	%ax,_cpl		# set new priority level
	orw	_imen,%ax		# mask off those not enabled yet
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
	sti				# enable interrupts

	DONET(NETISR_RAW,_rawintr)
#ifdef INET
	DONET(NETISR_IP,_ipintr)
#endif
	cli				# disable interrupts
	popl	_cpl			# save old priority
	NOP
	movw	$0,%ax			# set new priority level
	movw	%ax,%dx
	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	.globl _splx
	ALIGN32
_splx:
	cli				# disable interrupts
	NOP
	movw	4(%esp),%ax		# new priority level
	movw	%ax,%dx
	cmpw	$0,%dx
	je	_spl0			# going to "zero level" is special

	orw	_imen,%ax		# mask off those not enabled yet
	movw	%ax,%cx
	outb	%al,$ IO_ICU1+1		/* update icu's */
	NOP
	movb	%ah,%al
	outb	%al,$ IO_ICU2+1
	NOP
	movzwl	_cpl,%eax		# return old priority
	movw	%dx,_cpl		# set new priority level
	sti				# enable interrupts
	ret

	/* hardware interrupt catcher (IDT 32 - 47) */
	.globl	_isa_strayintr

IDTVEC(intr0)
	INTRSTRAY(0, _highmask, 0) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr1)
	INTRSTRAY(1, _highmask, 1) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr2)
	INTRSTRAY(2, _highmask, 2) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr3)
	INTRSTRAY(3, _highmask, 3) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr4)
	INTRSTRAY(4, _highmask, 4) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr5)
	INTRSTRAY(5, _highmask, 5) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr6)
	INTRSTRAY(6, _highmask, 6) ; call	_isa_strayintr ; INTREXIT1

IDTVEC(intr7)
	INTRSTRAY(7, _highmask, 7) ; call	_isa_strayintr ; INTREXIT1


IDTVEC(intr8)
	INTRSTRAY(8, _highmask, 8) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr9)
	INTRSTRAY(9, _highmask, 9) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr10)
	INTRSTRAY(10, _highmask, 10) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr11)
	INTRSTRAY(11, _highmask, 11) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr12)
	INTRSTRAY(12, _highmask, 12) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr13)
	INTRSTRAY(13, _highmask, 13) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr14)
	INTRSTRAY(14, _highmask, 14) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intr15)
	INTRSTRAY(15, _highmask, 15) ; call	_isa_strayintr ; INTREXIT2

IDTVEC(intrdefault)
	INTRSTRAY(255, _highmask, 255) ; call	_isa_strayintr ; INTREXIT2
