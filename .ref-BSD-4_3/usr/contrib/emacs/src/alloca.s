/* `alloca' standard 4.2 subroutine for 68000's and 16000's and pyramids.
   Also has _setjmp and _longjmp for pyramids.
   Also has hack_sky for sun 2.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


/* Both 68000 systems I have run this on have had broken versions of alloca.
   Also, I am told that non-berkeley systems do not have it at all.
   So replace whatever system-provided alloca there may be
   on all 68000 systems.  */

#include "config.h"

#ifndef HAVE_ALLOCA  /* define this to use system's alloca */

#ifdef hp9000s200
	data
	text
	globl	_alloca
_alloca	
	move.l	(sp)+,a0	; pop return addr from top of stack
	move.l	(sp)+,d0	; pop size in bytes from top of stack
	add.l	#ROUND,d0	; round size up to long word
	and.l	#MASK,d0	; mask out lower two bits of size
	sub.l	d0,sp		; allocate by moving stack pointer
	tst.b	PROBE(sp)	; stack probe to allocate pages
	move.l	sp,d0		; return pointer
	add.l	#-4,sp		; new top of stack
	jmp	(a0)		; not a normal return
MASK	equ	-4		; Longword alignment
ROUND	equ	3		; ditto
PROBE	equ	-128		; safety buffer for C compiler scratch
	data

#else
#ifdef m68k			/* SGS assembler totally different */
	file	"alloca.s"
	global	alloca
alloca:
	mov.l	(%sp)+,%a1	# pop return addr from top of stack
	mov.l	(%sp)+,%d0	# pop size in bytes from top of stack
	add.l	&R%1,%d0	# round size up to long word
	and.l	&-4,%d0		# mask out lower two bits of size
	sub.l	%d0,%sp		# allocate by moving stack pointer
	tst.b	P%1(%sp)	# stack probe to allocate pages
	mov.l	%sp,%a0		# return pointer as pointer
	mov.l	%sp,%d0		# return pointer as int to avoid disaster
	add.l	&-4,%sp		# new top of stack
	jmp	(%a1)		# not a normal return
	set	S%1,64		# safety factor for C compiler scratch
	set	R%1,3+S%1	# add to size for rounding
	set	P%1,-132	# probe this far below current top of stack

#else /* not m68k */

#ifdef m68000

/* Some systems want the _, some do not.  Win with both kinds.  */
.globl	_alloca
_alloca:
.globl	alloca
alloca:
	movl	sp@+,a0
	movl	a7,d0
	subl	sp@,d0
	andl	#~3,d0
	movl	d0,sp
	tstb	sp@(0)		/* Make stack pages exist  */
				/* Needed on certain systems
				   that lack true demand paging */
	addql	#4,d0
	jmp	a0@

#endif /* m68000 */
#endif /* not m68k */
#endif /* not hp9000s200 */

#ifdef ns16000

	.text
	.align	2
/* Some systems want the _, some do not.  Win with both kinds.  */
.globl	_alloca
_alloca:
.globl	alloca
alloca:

/* Two different assembler syntaxes are used for the same code
	on different systems.  */

#ifdef sequent
#define IM
#define REGISTER(x) x
#else
#define IM $
#define REGISTER(x) 0(x)
#endif

/*
 * The ns16000 is a little more difficult, need to copy regs.
 * Also the code assumes direct linkage call sequence (no mod table crap).
 * We have to copy registers, and therefore waste 32 bytes.
 *
 * Stack layout:
 * new	sp ->	junk	
 *	 	registers (copy)
 *	r0 ->	new data		
 *		 | 	  (orig retval)
 *		 |	  (orig arg)
 * old  sp ->	regs	  (orig)
 *		local data
 *	fp ->	old fp
 */

	movd	tos,r1		/*  pop return addr */
	negd	tos,r0		/*  pop amount to allocate */
	sprd	sp,r2
	addd	r2,r0
	bicb	IM/**/3,r0	/*  4-byte align */
	lprd	sp,r0
	adjspb	IM/**/36	/*  space for regs, +4 for caller to pop */
	movmd	0(r2),4(sp),IM/**/4	/*  copy regs */
	movmd	0x10(r2),0x14(sp),IM/**/4
	jump	REGISTER(r1)	/* funky return */
#endif /* ns16000 */

#ifdef pyramid

.globl _alloca

_alloca: addw $3,pr0	# add 3 (dec) to first argument
	bicw $3,pr0	# then clear its last 2 bits
	subw pr0,sp	# subtract from SP the val in PR0
	movw sp,pr0	# ret. current SP
	ret

.globl __longjmp
.globl _longjmp
.globl __setjmp
.globl _setjmp

__longjmp: jump _longjmp
__setjmp:  jump _setjmp

#endif /* pyramid */

#ifdef ATT3B5

	.align 4
	.globl alloca

alloca:
	movw %ap, %r8
	subw2 $9*4, %r8
	movw 0(%r8), %r1    /* pc */
	movw 4(%r8), %fp
	movw 8(%r8), %sp
	addw2 %r0, %sp /* make room */
	movw %sp, %r0 /* return value */
	jmp (%r1) /* continue... */

#endif /* ATT3B5 */

/*
  A Hack to reinitialize the floating point table in C or Fortran
  code.
     There are no arguments to the call
 
 	David Robinson
 	Jet Propulsion Laboratory
	ia-sun2!david@cit-vax.arpa
 
  WARNING: Very SUN dependant Only tested under SUN 2.0
*/
#ifdef sun2
.globl _hack_sky

_hack_sky:
	moveml  #0xC0C0,sp@-	|save registers <d0,d1,a0,a1>
	clrl	__skybase	|clear out old sky status
	jsr     __skyinit	|initialize sky board
	tstl    d0		|check if successful
	lea	floatflavor,a1  
	beqs    hack1		
	lea     a1@(0x7e),a0 	|Sky board exists
	bras    hack2
hack1:	lea     a1@(0x12),a0	|No Sky board
hack2:	movw    #0x1a,d0
	lea     fvflti,a1	|load destination
hack3:	movl    a0@+,a1@+	|copy table
	dbra    d0,hack3
	moveml  sp@+,#0x0303	|restore registers
	rts
#endif /* sun2 */

#endif /* not HAVE_ALLOCA */
