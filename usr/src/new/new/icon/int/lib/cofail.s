/* cofail(coexpr,value) - suspend current coexpression and activate
 * activator with failure, without changing activator's activator.
 *
 * NOTE:  this code is highly dependent on stack frame layout.
 *
 * Outline:
 *    create procedure frame
 *    save sp and boundary in current coexpression stack header
 *    change current stack to coexpr
 *    get sp and boundary from new coexpression stack header
 *    return value in new stack
 */
#ifdef VAX
.globl	_boundary		# Icon/C boundary
.globl	_current		# current coexpression
.globl	_file			# current file name
.globl	_line			# current line number

.globl	_deref			# dereference
.globl	_fail			# signal failure

.globl	_cofail
_cofail:
	.word	r(2)|r(3)|r(4)|r(5)|r(9)|r(10)|r(11)
	calls	$0,_setbound
	subl2	$8,sp		# make room on stack for line and file
	movl	_line,-4(fp)	# and put them in the frame
	movl	_file,-8(fp)
	movl	_current+4,r2	# r2 points at current stack header
	movl	sp,16(r2)	# save the stack pointer,
	movl	ap,20(r2)	#  address pointer,
	movl	_boundary,24(r2)#  and boundary
	movl	8(r2),r3	# r3 points to activator
	movl	r3,_current+4	# make new stack header current
	movl	16(r3),sp	# get new sp,
	movl	20(r3),ap	#  ap,
	movl	24(r3),fp	#  fp,
	movl	fp,_boundary	#  and boundary
	movl	-4(fp),_line	# restore line number
	movl	-8(fp),_file	#  and file name
	calls	$0,_fail	# "fail in the new stack"
#endif VAX
#ifdef PDP11
/ cofail(coexpr,value) - suspend current coexpression and activate
/ activator with failure, without changing activator's activator.

/ NOTE:  this code is highly dependent on stack frame layout.

/ Outline:
/    create procedure frame
/    save sp and boundary in current coexpression stack header
/    change current stack to coexpr
/    get sp and boundary from new coexpression stack header
/    return value in new stack

/ Register usage:
/    r2:  pointer to current coexpression stack header
/    r3:  pointer to new coexpression stack header
/    r5:  procedure frame pointer

.globl	_boundary		/ Icon/C boundary
.globl	_current		/ current coexpression
.globl	_file			/ current file name
.globl	_line			/ current line number

.globl	csv			/ save registers
.globl  cret                    / return as from C
.globl	_deref			/ dereference
.globl	_fail			/ signal failure

.globl	_cofail
_cofail:
	jsr	r5,csv		/ create procedure frame
	mov	_line,(sp)	/ save current line number
	mov	_file,-(sp)	/   and file name
	mov	_current+2,r2	/ r2 <- pointer to current stack header
	mov	sp,8.(r2)	/ save sp
	mov	_boundary,12.(r2)  / save boundary
        mov     4(r2),r3        / r3 <- pointer to activator
	mov	r3,_current+2	/ make new stack header current
	mov	8.(r3),sp	/ get new sp
	mov	12.(r3),r5	/ get new r5 and
	mov	r5,_boundary	/   new boundary
	mov	-8.(r5),_line	/ restore line number
	mov	-10.(r5),_file	/   and file name
	jsr     pc,_fail        / fail in new stack
#endif PDP11
