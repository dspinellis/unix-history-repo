#include "../h/config.h"

#ifdef VAX
/*
 * esusp - Suspend from an expression.
 * Duplicates the most recent generator frame outside the
 * current expression frame.  Esusp does not return directly.
 * The expression is reactivated when an alternative is needed;
 * the return actually comes from efail.
 */

.globl	_efail			# signal failure in an expression

.globl	_boundary		# Icon/C boundary address
.globl	_line			# current line number
.globl	_file			# current file name
.globl  _k_level		# value of &level

.globl  _esusp

_esusp:
	.word   r(2)|r(9)|r(10)|r(11) #  create new generator frame
	movl	fp,_boundary	#  create Icon/C boundary
	pushl	fp

   /* Compute addresses of new generator frame. */

	movl	sp,r10		# r10 <- pointer to new generator frame
	pushl	_k_level        # save &level
	pushl	_line		# save current line number
	pushl	_file		#   and file name
	addl3   $4,r11,r0	# r0 <- pointer to top of region to be copied
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
	movl	8(fp),ap  	# restore old registers,
	movl	12(fp),fp
	movl	(r11),r11	#   and exit suspending expression frame
	clrl	_boundary	# returning to Icon code
	jmp	(r1)		# this really suspends

#endif VAX

#ifdef PDP11
/ esusp - Suspend from an expression.
/ Duplicates the most recent generator frame outside the
/ current expression frame.  Esusp does not return directly.
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

.globl	_efail			/ signal failure in an expression

.globl	_boundary		/ Icon/C boundary address
.globl	_file			/ current file name
.globl  _k_level		/ value of &level
.globl	_line			/ current line number

.globl  _esusp
_esusp:
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
	tst	(r0)+		/	(= r4 + 2)
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
