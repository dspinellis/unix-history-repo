#include "../h/config.h"

#ifdef VAX
/*
 * lsusp - Suspend or return from a limited expression.
 * Decrements the limit counter; if it becomes zero, lsusp
 * exits the current expression frame.  If not, lsusp
 * suspends from the current expression frame.
 */

.globl	_esusp

.globl	_lsusp
_lsusp:
	.word	r(2)|r(9)|r(10)|r(11)

	decl	8(r11)		# decrement the limit counter
	jneq	1f		# branch if still > 0

   /* Return from the limited expression. */

	movl	16(sp),r0	# pop return pc
	movq	8(ap),4(r11)	# copy expression value
	movl	-4(r11),r10	# exit expression frame
	movl	r11,sp
	movl	(sp)+,r11
	movl	8(fp),ap
	movl	12(fp),fp
	jmp	(r0)

/* Suspend from the limited expression.
 * Duplicates the most recent generator frame outside the
 * current expression frame.  Lsusp does not return directly.
 * The expression is reactivated when an alternative is needed;
 * the return actually comes from efail.
 */

/* This code is exactly the same as esusp.s except for the line marked ***
 * The difference is due to the extra descriptor below the expression frame
 * marker that holds the limit counter.
 */

.globl	_efail			# signal failure in an expression

.globl	_boundary		# Icon/C boundary address
.globl	_file			# current file name
.globl  _k_level		# value of &level
.globl	_line			# current line number

1:
	movl	fp,_boundary	#  create Icon/C boundary
	pushl	fp

   /* Compute addresses of new generator frame. */

	movl	sp,r10		# r10 <- pointer to new generator frame
	pushl	_k_level        # save &level
	pushl	_line		# save current line number
	pushl	_file		#   and file name
	addl3   $12,r11,r0	# *** r0 <- pointer to top of region to copy
	movl	-4(r11),r2	# r2 <- generator frame pointer from caller
	jneq	1f		#   use saved gfp - 12 if non-zero,
	subl3	$8,(r11),r2 	#   else use saved efp - 8
	jmp	2f
1:
	subl2	$12,r2

   /* Copy surrounding expression frame. */

2:
	subl2   r0,r2		# r2 <- byte count of expression frame to copy
	subl2	r2,sp		# adjust stack pointer
	movc3   r2,(r0),(sp)    # copy expression frame

   /* Copy value of suspending expression. */

	movq	8(ap),-(sp)    	# push return value

   /* Return to code; reactivation will go directly to efail. */

	movl	16(fp),r1	# r1 <- return pc
	movl	$_efail,16(fp)	# fix resumption pc to propagate failure
#ifdef INT
	movl	-12(ap),r9	# restore old ipc.  I'm not sure that
				# this is needed as r9 isn't used
				# in the preceding code.
#endif INT
	movl	8(fp),ap  	# restore old registers,
	movl	12(fp),fp
	movl	(r11),r11	#   and exit suspending expression frame
	clrl	_boundary	# returning to Icon code
	jmp	(r1)		# this really suspends

#endif VAX

#ifdef PDP11
/ lsusp - Suspend or return from a limited expression.
/ Decrements the limit counter; if it becomes zero, lsusp
/ exits the current expression frame.  If not, lsusp
/ suspends from the current expression frame.

.globl	_esusp

.globl	_lsusp
_lsusp:
	dec	4(r4)		/ decrement the limit counter
	bne	1f		/ branch if still > 0

/ Return from the limited expression.

	mov	(sp)+,r0	/ pop return pc
	tst	(sp)+		/ skip nargs
	mov	(sp)+,2(r4)	/ copy expression value
	mov	(sp)+,4(r4)
	mov	-2(r4),r3	/ exit expression frame
	mov	r4,sp
	mov	(sp)+,r4
	jmp	(r0)		/ return

/ Suspend from the limited expression.
/ Duplicates the most recent generator frame outside the
/ current expression frame.  Lsusp does not return directly.
/ The expression is reactivated when an alternative is needed;
/ the return actually comes from efail.

/ Register usage:
/   r0:    pointer to top of stack region to be copied,
/	     which is just above the procedure descriptor (arg0) of the
/	     suspending procedure
/   r2:	   old generator frame pointer, indexed down to r0 during copy
/   r3:    new generator frame pointer
/   r4:    suspending expression frame pointer
/   r5:    current procedure frame pointer

/ This code is exactly the same as esusp.s except for the line marked ***
/ The difference is due to the extra descriptor below the expression frame
/ marker that holds the limit counter.

.globl	_efail			/ signal failure in an expression

.globl	_boundary		/ Icon/C boundary address
.globl	_file			/ current file name
.globl  _k_level		/ value of &level
.globl	_line			/ current line number

1:
	mov	r5,-(sp)	/ create new procedure frame
	mov	sp,r5
	mov	r4,-(sp)    	/ save registers
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	r5,-(sp)	/ create Icon/C boundary
	mov	r5,_boundary

/ Calculate addresses of new generator frame.

	mov	sp,r3		/ r3 <- pointer to new generator frame
	mov	_k_level,-(sp)	/ save &level
	mov	_line,-(sp)	/ save current line number
	mov	_file,-(sp)	/   and file name
	mov	r4,r0		/ r0 <- pointer to top of region to be copied
	add	$6,r0		/	(= r4 + 6) ***
	mov	-2(r4),r2	/ r2 <- generator frame pointer from caller
	bne	1f		/   use saved gfp - 6 if non-zero,
	mov	(r4),r2  	/   else use saved efp - 4
	cmp	-(r2),-(r2)
	br	2f
1:
	sub	$6,r2
	br	2f

/ Copy surrounding expression frame.

1:
	mov	-(r2),-(sp)
2:
	cmp	r2,r0		/ stop at end of frame
	bhi	1b

/ Copy value of suspending expression.

	mov	8.(r5),-(sp)	/ push return value
	mov	6(r5),-(sp)

/ Return to code; reactivation will go directly to efail.

	mov	2(r5),r1	/ r1 <- return pc
	mov	$_efail,2(r5)	/ fix reactivation pc to propagate failure
#ifdef INT
	mov	-6(r5),r2
#endif INT
	mov	(r5),r5		/ restore old registers,
	mov	(r4),r4		/   and exit suspending expression frame
	clr	_boundary	/ returning to Icon code
	jmp	(r1)		/ this really suspends
#endif PDP11
