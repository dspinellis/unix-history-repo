/* `alloca' standard 4.2 subroutine for 68000's and 16000's and others.
   Also has _setjmp and _longjmp for pyramids.
   Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

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

#define NOT_C_CODE
/* #include "config.h" */

#ifndef HAVE_ALLOCA  /* define this to use system's alloca */

#ifndef hp9000s300
#ifndef m68k
#ifndef mc68k
#ifndef m68000
#ifndef WICAT
#ifndef ns32000
#ifndef ns16000
#ifndef sequent
#ifndef pyramid
#ifndef ATT3B5
#ifndef XENIX
you
lose!!
#endif /* XENIX */
#endif /* ATT3B5 */
#endif /* pyramid */
#endif /* sequent */
#endif /* ns16000 */
#endif /* ns32000 */
#endif /* WICAT */
#endif /* m68000 */
#endif /* mc68k */
#endif /* m68k */
#endif /* hp9000s300 */


#ifdef hp9000s300
#ifdef OLD_HP_ASSEMBLER
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
#else /* new hp assembler syntax */
/*
  The new compiler does "move.m <registers> (%sp)" to save registers,
    so we must copy the saved registers when we mung the sp.
  The old compiler did "move.m <register> <offset>(%a6)", which
    gave us no trouble
 */
	text
	set	PROBE,-128	# safety for C frame temporaries
	set	MAXREG,22	# d2-d7, a2-a5, fp2-fp7 may have been saved
	global	_alloca
_alloca:
	mov.l	(%sp)+,%a0	# return addess
	mov.l	(%sp)+,%d0	# number of bytes to allocate
	mov.l	%sp,%a1		# save old sp for register copy
	mov.l	%sp,%d1		# compute new sp
	sub.l	%d0,%d1		# space requested
	and.l	&-4,%d1		# round down to longword
	sub.l	&MAXREG*4,%d1	# space for saving registers
	mov.l	%d1,%sp		# save new value of sp
	tst.b	PROBE(%sp)	# create pages (sigh)
	mov.l	%a2,%d1		# save reg a2
	mov.l	%sp,%a2
	move.w	&MAXREG-1,%d0
copy_regs_loop:			/* save caller's saved registers */
	mov.l	(%a1)+,(%a2)+
	dbra	%d0,copy_regs_loop
	mov.l	%a2,%d0		# return value
	mov.l	%d1,%a2		# restore a2
	add.l	&-4,%sp		# adjust tos
	jmp	(%a0)		# rts
#endif /* new hp assembler */
#else
#if defined(m68k) || defined(mc68k)	/* SGS assembler totally different */
	file	"alloca.s"
	global	alloca
alloca:
#ifdef MOTOROLA_DELTA
/* slightly modified version of alloca to motorola sysV/68 pcc - based
   compiler. 
   this compiler saves used regfisters relative to %sp instead of %fp.
   alright, just make new copy of saved register set whenever we allocate
   new space from stack..
   this is true at last until SVR3V5.1 . bug has reported to Motorola. */
	set	MAXREG,10	# max no of registers to save (d2-d7, a2-a5)
        mov.l   (%sp)+,%a1	# pop return addr from top of stack
        mov.l   (%sp)+,%d0	# pop size in bytes from top of stack
	mov.l	%sp,%a0		# save stack pointer for register copy
        addq.l  &3,%d0		# round size up to long word
        andi.l  &-4,%d0		# mask out lower two bits of size
	mov.l	%sp,%d1		# compute new value of sp to d1
        sub.l	%d0,%d1		# pseudo-allocate by moving stack pointer
	sub.l	&MAXREG*4,%d1	# allocate more space for saved regs.
	mov.l	%d1,%sp		# actual alloaction.
	move.w	&MAXREG-1,%d0	# d0 counts saved regs.
	mov.l	%a2,%d1		# preserve a2.
	mov.l	%sp,%a2		# make pointer to new reg save area.
copy_regs_loop: 		# copy stuff from old save area.
	mov.l	(%a0)+,(%a2)+	# save saved register
	dbra	%d0,copy_regs_loop
        mov.l   %a2,%a0		# now a2 is start of allocated space.
	mov.l	%a2,%d0		# return it in both a0 and d0 to play safe.
	mov.l	%d1,%a2		# restore a2.
        subq.l  &4,%sp		# new top of stack
        jmp     (%a1)		# far below normal return
#else /* not MOTOROLA_DELTA */
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
#endif /* not MOTOROLA_DELTA */

#else /* not m68k && not mc68k */

#ifdef m68000

#ifdef WICAT
/*
 * Registers are saved after the corresponding link so we have to explicitly
 * move them to the top of the stack where they are expected to be.
 * Since we do not know how many registers were saved in the calling function
 * we must assume the maximum possible (d2-d7,a2-a5).  Hence, we end up
 * wasting some space on the stack.
 *
 * The large probe (tst.b) attempts to make up for the fact that we have
 * potentially used up the space that the caller probed for its own needs.
 */
	.procss m0
	.config "68000 1"
	.module	_alloca
MAXREG:	.const	10
	.sect	text
	.global	_alloca
_alloca:
	move.l	(sp)+,a1	; pop return address
	move.l	(sp)+,d0	; pop allocation size
	move.l	sp,d1		; get current SP value
	sub.l	d0,d1		; adjust to reflect required size...
	sub.l	#MAXREG*4,d1	; ...and space needed for registers
	and.l	#-4,d1		; backup to longword boundry
	move.l	sp,a0		; save old SP value for register copy
	move.l	d1,sp		; set the new SP value
	tst.b	-4096(sp)	; grab an extra page (to cover caller)
	move.l	a2,d1		; save callers register
	move.l	sp,a2
	move.w	#MAXREG-1,d0	; # of longwords to copy
loop:	move.l	(a0)+,(a2)+	; copy registers...
	dbra	d0,loop		; ...til there are no more
	move.l	a2,d0		; end of register area is addr for new space
	move.l	d1,a2		; restore saved a2.
	addq.l	#4,sp		; caller will increment sp by 4 after return.
	move.l	d0,a0		; return value in both a0 and d0.
	jmp	(a1)
	.end	_alloca
#else

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

#endif /* not WICAT */
#endif /* m68000 */
#endif /* not m68k */
#endif /* not hp9000s300 */

#if defined (ns16000) || defined (ns32000)

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
#ifdef NS5   /* ns SysV assembler */
#define IM $
#define REGISTER(x) x
#else
#define IM $
#define REGISTER(x) 0(x)
#endif
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
#endif /* ns16000 or ns32000 */

#ifdef pyramid

.globl _alloca

_alloca: addw $3,pr0	# add 3 (dec) to first argument
	bicw $3,pr0	# then clear its last 2 bits
	subw pr0,sp	# subtract from SP the val in PR0
	andw $-32,sp	# keep sp aligned on multiple of 32.
	movw sp,pr0	# ret. current SP
	ret

#ifdef PYRAMID_OLD /* This isn't needed in system version 4.  */
.globl __longjmp
.globl _longjmp
.globl __setjmp
.globl _setjmp

__longjmp: jump _longjmp
__setjmp:  jump _setjmp
#endif

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

#ifdef XENIX

.386

_TEXT segment dword use32 public 'CODE'
assume   cs:_TEXT

;-------------------------------------------------------------------------

public _alloca
_alloca proc near

	pop	ecx		; return address
	pop	eax		; amount to alloc
	add	eax,3		; round it to 32-bit boundary
	and	al,11111100B	;
	mov	edx,esp		; current sp in edx
	sub	edx,eax		; lower the stack
	xchg	esp,edx		; start of allocation in esp, old sp in edx
	mov	eax,esp		; return ptr to base in eax
	push	[edx+8]		; save poss. stored reg. values (esi,edi,ebx)
	push	[edx+4]		;  on lowered stack
	push	[edx]		;
	sub	esp,4		; allow for 'add esp, 4'
	jmp	ecx		; jump to return address

_alloca endp

_TEXT	ends

end

#endif /* XENIX */

#endif /* not HAVE_ALLOCA */
