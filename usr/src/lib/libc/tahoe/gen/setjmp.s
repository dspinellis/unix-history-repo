#ifdef LIBC_SCCS
	.asciz	"@(#)setjmp.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/*
 * C library -- setjmp, longjmp
 *
 *	longjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	setjmp(a)
 * by restoring registers from the stack,
 * previous signal mask, and doing a return.
 *
 * BUG: always restores onsigstack state to 0
 */

#include "DEFS.h"

ENTRY(setjmp, 0)
	pushl	$0
	callf	$8,_sigblock		# get signal mask
	movl	r0,r1
	movl	4(fp),r0
	movl	(fp),(r0)		# save frame pointer of caller
	movl	-8(fp),4(r0)		# save pc of caller
	movl	r1,8(r0)		# save signal mask
	clrl	12(r0)			# XXX (should be onsigstack) XXX
	clrl	r0
	ret

ENTRY(longjmp, 0)
	movl	8(fp),r0		# return(v)
	movl	4(fp),r1		# fetch buffer
	tstl	(r1)
	beql	botch
loop:
	cmpl	(r1),(fp)
	beql	done
	blssu	botch
	movl	$loop,-8(fp)
	ret				# pop another frame

done:
	cmpb	*-8(fp),reiins		# returning to an "rei"?
	bneq	1f
	movab	3f,-8(fp)		# do return w/ psl-pc pop
	brw	2f
1:
	movab	4f,-8(fp)		# do standard return
2:
	ret				# unwind stack before signals enabled
3:
	addl2	$8,sp			# compensate for PSL-PC push
4:
	pushal	(sp)			# old stack pointer
	pushl	8(r1)			# old signal mask
	pushl	12(r1)			# old onsigstack
	pushal	(sp)			# pointer to sigcontext
	kcall	$139			# restore previous signal context
	jmp	*4(r1)			# done, return....

botch:
	callf	$4,_longjmperror
	halt

	.data
reiins:	rei
