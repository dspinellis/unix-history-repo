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
 * CURRENT PATCH LEVEL:         4       00158
 * --------------------         -----   ----------------------
 * 
 * 28 Nov 92	Frank MacLachlan	Aligned addresses and data
 *					on 32bit boundaries.
 * 24 Mar 93	Rodney W. Grimes	Added interrupt counters for vmstat
 *					also stray and false intr counters added
 * 20 Apr 93	Bruce Evans		New npx-0.5 code
 * 25 Apr 93	Bruce Evans		Support new interrupt code (intr-0.1)
 *		Rodney W. Grimes	Reimplement above patches..
 * 17 May 93	Rodney W. Grimes	Redid the interrupt counter stuff
 *					moved the counters to vectors.s so
 *					they are next to the name tables.
 */

/*
 * AT/386
 * Vector interrupt control section
 */

/*
 * XXX - this file is now misnamed.  All spls are now soft and the only thing
 * related to the hardware icu is that the bit numbering is the same in the
 * soft priority masks as in the hard ones.
 */

#define	HIGHMASK	0xffff
#define	SOFTCLOCKMASK	0x8000

	.data
	.globl	_cpl
_cpl:	.long	0xffff			# current priority (all off)
	.globl	_imen
_imen:	.long	0xffff			# interrupt mask enable (all off)
#	.globl	_highmask
_highmask:	.long	HIGHMASK
	.globl	_ttymask
_ttymask:	.long	0
	.globl	_biomask
_biomask:	.long	0
	.globl	_netmask
_netmask:	.long	0
	.globl	_ipending
_ipending:	.long	0

#define	GENSPL(name, mask, event) \
	.globl	_spl/**/name ; \
	ALIGN_TEXT ; \
_spl/**/name: ; \
	COUNT_EVENT(_intrcnt_spl, event) ; \
	movl	_cpl,%eax ; \
	movl	%eax,%edx ; \
	orl	mask,%edx ; \
	movl	%edx,_cpl ; \
	SHOW_CPL ; \
	ret

#define	FASTSPL(mask) \
	movl	mask,_cpl ; \
	SHOW_CPL

#define	FASTSPL_VARMASK(varmask) \
	movl	varmask,%eax ; \
	movl	%eax,_cpl ; \
	SHOW_CPL

	.text

#undef BUILD_FAST_VECTOR
#define	BUILD_FAST_VECTOR	BUILD_VECTOR

#undef BUILD_VECTOR
#define BUILD_VECTOR(name, unit, irq_num, id_num, mask, handler, \
		     icu_num, icu_enables, reg) \
	testb	$IRQ_BIT(irq_num),%reg ; \
	jne	unpend_v/**/id_num

	ALIGN_TEXT
unpend_v:
	COUNT_EVENT(_intrcnt_spl, 0)
	BUILD_VECTORS

/*
 * XXX - we have already tested that the ipending bit is set, but we didn't
 * disable interrupts, so if any interrupt occurs while we are test-and-
 * resetting, then the doreti routine for the new interrupt is guaranteed to
 * unpend the pending interrupt.  So use btrl to test the bit again.  We
 * avoided using btrl in the chain of tests since it is slow (8 cycles to
 * memory on 386's and 486's, while testing a register takes 2 cyles on 386's
 * and 1 on 486's, and and-immediate to memory takes 7 cycles on 386's and 3
 * on 486's).  We avoided bsf because it is slow and doesn't handle the
 * strange priority order.  A reordered bsf can be done faster using table
 * lookup but it is still slower than our dumb-looking linear search for the
 * first 8 or so (configured) interrupts.  Binary search of 1-16 items would
 * have too many slow branches taken.  Perhaps this code is not excecuted
 * enough to be worth so much attention!
 *
 * TODO: get rid of slow btrl's and btsl's, and slower bsf's elsewhere.
 *       Remove kludges for gas once not handling immediate-mode btrl's.
 */

#undef BUILD_FAST_VECTOR
#define BUILD_FAST_VECTOR(name, unit, irq_num, id_num, mask, handler, \
			  icu_num, icu_enables, reg) \
	ALIGN_TEXT ; \
unpend_v/**/id_num: ; \
	btrl	$irq_num,_ipending ; \
	jnc	unpend_v_confirmation_failed ; \
	SHOW_IPENDING ; \
	pushl	$unit ; \
	call	_soft/**/name ; \
	addl	$4,%esp ; \
	jmp	unpend_v_confirmation_failed

#undef BUILD_VECTOR
#define BUILD_VECTOR(name, unit, irq_num, id_num, mask, handler, \
		     icu_num, icu_enables, reg) \
	ALIGN_TEXT ; \
unpend_v/**/id_num: ; \
	btrl	$irq_num,_ipending ; \
	jnc	unpend_v_confirmation_failed ; \
	SHOW_IPENDING ; \
	jmp	Vretry/**/id_num

	BUILD_VECTORS

/*
 * Unconfigured interrupt or no longer pending interrupt.
 *
 * XXX - unconfigured interrupts "can't happen", except possibly for
 * strayintr's 7 and 15 when they are not configured.  If they happen,
 * ipending will be checked forever.
 */

	ALIGN_TEXT
unpend_v_confirmation_failed:
	movl	_cpl,%eax
	movl	%eax,%edx
	notl	%eax
	andl	_ipending,%eax
	je	none_to_unpend
	jmp	unpend_v

/*
 * Handle return from interrupt after device handler finishes
 */
	ALIGN_TEXT
doreti:
	COUNT_EVENT(_intrcnt_spl, 1)
	addl	$4,%esp			# discard unit arg
	popl	%eax			# get previous priority
/*
 * Now interrupt frame is a trap frame!
 *
 * XXX - setting up the interrupt frame to be almost a stack frame is mostly
 * a waste of time.
 */
	movl	%eax,_cpl
	SHOW_CPL
	movl	%eax,%edx
	notl	%eax
	andl	_ipending,%eax
	jne	unpend_v
none_to_unpend:
	testl	%edx,%edx		# returning to zero priority?
	jne	1f			# nope, going to non-zero priority
	movl	_netisr,%eax
	testl	%eax,%eax		# check for softint s/traps
	jne	2f			# there are some
	jmp	test_resched		# XXX - schedule jumps better
	COUNT_EVENT(_intrcnt_spl, 2)			# XXX

	ALIGN_TEXT			# XXX
1:					# XXX
	COUNT_EVENT(_intrcnt_spl, 3)
	popl	%es
	popl	%ds
	popal
	addl	$8,%esp
	iret

#include "../net/netisr.h"

#define DONET(s, c, event) ; \
	.globl	c ; \
	btrl	$s,_netisr ; \
	jnc	1f ; \
	COUNT_EVENT(_intrcnt_spl, event) ; \
	call	c ; \
1:

	ALIGN_TEXT
2:
	COUNT_EVENT(_intrcnt_spl, 4)
/*
 * XXX - might need extra locking while testing reg copy of netisr, but
 * interrupt routines setting it would not cause any new problems (since we
 * don't loop, fresh bits will not be processed until the next doreti or spl0).
 */
	testl	$~((1 << NETISR_SCLK) | (1 << NETISR_AST)),%eax
	je	test_ASTs		# no net stuff, just temporary AST's
	FASTSPL_VARMASK(_netmask)
	DONET(NETISR_RAW, _rawintr, 5)
#ifdef INET
	DONET(NETISR_IP, _ipintr, 6)
#endif
#ifdef IMP
	DONET(NETISR_IMP, _impintr, 7)
#endif
#ifdef NS
	DONET(NETISR_NS, _nsintr, 8)
#endif
	FASTSPL($0)
test_ASTs:
	btrl	$NETISR_SCLK,_netisr
	jnc	test_resched
	COUNT_EVENT(_intrcnt_spl, 9)
	FASTSPL($SOFTCLOCKMASK)
/*
 * Back to an interrupt frame for a moment.
 */
	pushl	$0			# previous cpl (probably not used)
	pushl	$0x7f			# dummy unit number
	call	_softclock
	addl	$8,%esp			# discard dummies
	FASTSPL($0)
test_resched:
#ifdef notused1
	btrl	$NETISR_AST,_netisr
	jnc	2f
#endif
#ifdef notused2
	cmpl	$0,_want_resched
	je	2f
#endif
	cmpl	$0,_astpending		# XXX - put it back in netisr to
	je	2f			# reduce the number of tests
	testb	$SEL_RPL_MASK,TRAPF_CS_OFF(%esp)
					# to non-kernel (i.e., user)?
	je	2f			# nope, leave
	COUNT_EVENT(_intrcnt_spl, 10)
	movl	$0,_astpending
	call	_trap
2:
	COUNT_EVENT(_intrcnt_spl, 11)
	popl	%es
	popl	%ds
	popal
	addl	$8,%esp
	iret

/*
 * Interrupt priority mechanism
 *	-- soft splXX masks with group mechanism (cpl)
 *	-- h/w masks for currently active or unused interrupts (imen)
 *	-- ipending = active interrupts currently masked by cpl
 */

	GENSPL(bio, _biomask, 12)
	GENSPL(clock, $HIGHMASK, 13)	/* splclock == splhigh ex for count */
	GENSPL(high, $HIGHMASK, 14)
	GENSPL(imp, _netmask, 15)	/* splimp == splnet except for count */
	GENSPL(net, _netmask, 16)
	GENSPL(softclock, $SOFTCLOCKMASK, 17)
	GENSPL(tty, _ttymask, 18)

	.globl _splnone
	.globl _spl0
	ALIGN_TEXT
_splnone:
_spl0:
	COUNT_EVENT(_intrcnt_spl, 19)
in_spl0:
	movl	_cpl,%eax
	pushl	%eax			# save old priority
	testl	$(1 << NETISR_RAW) | (1 << NETISR_IP),_netisr
	je	over_net_stuff_for_spl0
	movl	_netmask,%eax		# mask off those network devices
	movl	%eax,_cpl		# set new priority
	SHOW_CPL
/*
 * XXX - what about other net intrs?
 */
	DONET(NETISR_RAW, _rawintr, 20)
#ifdef INET
	DONET(NETISR_IP, _ipintr, 21)
#endif
over_net_stuff_for_spl0:
	movl	$0,_cpl			# set new priority
	SHOW_CPL
	movl	_ipending,%eax
	testl	%eax,%eax
	jne	unpend_V
	popl	%eax			# return old priority
	ret

	.globl _splx
	ALIGN_TEXT
_splx:
	COUNT_EVENT(_intrcnt_spl, 22)
	movl	4(%esp),%eax		# new priority
	testl	%eax,%eax
	je	in_spl0			# going to "zero level" is special
	COUNT_EVENT(_intrcnt_spl, 23)
	movl	_cpl,%edx		# save old priority
	movl	%eax,_cpl		# set new priority
	SHOW_CPL
	notl	%eax
	andl	_ipending,%eax
	jne	unpend_V_result_edx
	movl	%edx,%eax		# return old priority
	ret

#undef BUILD_FAST_VECTOR
#define BUILD_FAST_VECTOR	BUILD_VECTOR

#undef BUILD_VECTOR
#define BUILD_VECTOR(name, unit, irq_num, id_num, mask, handler, \
		     icu_num, icu_enables, reg) \
	testb	$IRQ_BIT(irq_num),%reg ; \
	jne	unpend_V/**/id_num

	ALIGN_TEXT
unpend_V_result_edx:
	pushl	%edx
unpend_V:
	COUNT_EVENT(_intrcnt_spl, 24)
	BUILD_VECTORS

	ALIGN_TEXT
unpend_V_confirmation_failed:
	movl	_cpl,%eax
	notl	%eax
	andl	_ipending,%eax
	jne	unpend_V
	popl	%eax
	ret

#undef BUILD_FAST_VECTOR
#define BUILD_FAST_VECTOR(name, unit, irq_num, id_num, mask, handler, \
			  icu_num, icu_enables, reg) \
	ALIGN_TEXT ; \
unpend_V/**/id_num: ; \
	btrl	$irq_num,_ipending ; \
	jnc	unpend_V_confirmation_failed ; \
	SHOW_IPENDING ; \
	pushl	$unit ; \
	call	_soft/**/name ; \
	addl	$4,%esp ; \
	jmp	unpend_V_confirmation_failed

#undef BUILD_VECTOR
#define BUILD_VECTOR(name, unit, irq_num, id_num, mask, handler, \
		     icu_num, icu_enables, reg) \
	ALIGN_TEXT ; \
unpend_V/**/id_num: ; \
	btrl	$irq_num,_ipending ; \
	jnc	unpend_V_confirmation_failed ; \
	SHOW_IPENDING ; \
	int	$ICU_OFFSET + irq_num ; \
	popl	%eax ; \
	ret

	BUILD_VECTORS
