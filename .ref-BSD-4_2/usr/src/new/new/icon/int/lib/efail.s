#include "../h/config.h"

#ifdef VAX
/* * efail - resume newest inactive generator within current
 * expression frame.  If there are none, exit the expression
 * frame and take the failure branch (stored in the expression
 * frame marker).
 */

#ifdef INT
.globl	_interp			# interpreter loop -- whm
.globl	_efail_unwind		# Unwind one stack frame and then do efail
#endif INT
.globl	_atrace			# trace generator reactivations

.globl	_boundary		# Icon/C boundary address
.globl	_line			# current line number
.globl	_file			# current file name
.globl  _k_level		# value of &level
.globl	_k_trace		# value of &trace

.globl  _efail		# Handle expression failure

#ifdef INT
_efail_unwind:
	.word	0
	movl	$_efail,16(fp)	# plug the old pc and fake a return
	ret			#  to clear the stack
#endif INT
_efail:
	tstl	r10		# test for inactive generators,
	jeql	1f		#   branch if none

/* Reactivate newest dormant generator.	*/

	movl	(r10),_boundary # restore Icon/C boundary address
	movl	fp,r0		# save frame pointer for testing
	movl	r10,fp          # restore previous procedure frame pointer
	tstl	(fp)+

/* Trace resumption if &trace is set and if generator is an Icon procedure. */

	tstl	_k_trace
	jeql	2f
	cmpl	_boundary,fp	# if boundary == fp, then
	jneq	2f		#   generator is an Icon procedure
	movl	12(fp),r11	# r11 <- address of procedure frame
	cmpl	r11,r0		# if hidden procedure frame is same as current,
	jeql	2f		#   then generator is control regime
	movl	8(fp),r4	# r4 <- old ap
	ashl	$3,4(r4),r2	# r2 <- byte offest of arg0 from old ap
	addl2	r4,r2   	# r2 <- address of arg0	(procedure block desc.)
	addl2   $8,r2		#     (ap + 8 + 8*nargs)
	pushl	4(r2)		# push address of procedure block
	calls	$1,_atrace	# do the tracing

/* Restore &level, line number, and file name, and return to generator. */

2:
	movl	-(r10),_k_level
	movl	-(r10),_line
	movl	-(r10),_file

	cmpl	_boundary,fp	# see if returning to Icon from C
        bneq	3f
	clrl    _boundary	# and clear boundary if so.
3:
   	ret			# return to generator

/* Exit expression frame and signal failure again. */

1:
#ifdef INT
 	movl	-8(r11),r9	# r9 <- failure label
#endif INT
#ifdef CMP
	movl	-8(r11),r0	# r0 <- failure label
#endif CMP
	movl	-4(r11),r10	# exit current expression frame
	movl	r11,sp		
	movl	(sp)+,r11
#ifdef INT
 	tstl	r9		# is failure label zero?
#endif INT
#ifdef CMP
	tstl	r0		# is failure label zero?
#endif CMP
	jeql	_efail		#   yes, pass failure to outer expression
#ifdef INT
 	jmp	_interp		# resume interp. at failure label
#endif INT
#ifdef CMP
	jmp	(r0)		#   no, take failure branch
#endif CMP

#endif VAX

#ifdef PDP11
/ efail - reactivate newest dormant generator within current
/ expression frame.  If there are none, exit the expression
/ frame and take the failure branch (stored in the expression
/ frame marker).

.globl	cret			/ return from C calling sequence
.globl	_atrace			/ trace generator reactivations
#ifdef INT
.globl	_interp			/ interpreter loop
#endif INT
.globl	_boundary		/ Icon/C boundary address
.globl	_file			/ current file name
.globl  _k_level		/ value of &level
.globl	_k_trace		/ value of &trace
.globl	_line			/ current line number

.globl  _efail
_efail:
	tst	r3		/ test for dormant generators,
	beq	1f		/   branch if none

/ Reactivate newest dormant generator.

	mov	(r3),_boundary 	/ restore Icon/C boundary address
	mov	r5,r0		/ save procedure frame pointer
	mov	r3,r5		/ restore procedure frame pointer
	add	$8.,r5

/ Trace reactivation if &trace is set and if generator is an Icon procedure.

	tst	_k_trace
	beq	2f
	cmp	_boundary,r5	/ if boundary == r5, then
	bne	2f		/   generator is an Icon procedure
	mov	(r5),r4		/ r4 <- address of procedure frame
	cmp	r4,r0		/ if hidden procedure frame is same as current,
	beq	2f		/   then generator is control regime
	mov	4(r4),r2	/ r2 <- nargs * 4
	asl	r2
	asl	r2
	add	r4,r2		/ push address of procedure block,
	mov	8.(r2),-(sp)	/   which is pointer field of arg0
	jsr	pc,_atrace
	
/ Restore &level, line number, and file name, and return to generator.

2:
	mov	-(r3),_k_level
	mov	-(r3),_line
	mov	-(r3),_file
	jmp	cret

/ Exit expression frame and signal failure again.

1:
#ifdef INT
	mov	-4(r4),r2	/ get failure label
#endif INT
#ifdef CMP
	mov	-4(r4),r0	/ get failure label
#endif CMP
	mov	-2(r4),r3	/ exit current expression frame
	mov	r4,sp		
	mov	(sp)+,r4
#ifdef INT
	tst	r2		/ is failure label zero?
#endif INT
#ifdef CMP
	tst	r0		/ is failure label zero?
#endif CMP
	beq	_efail		/   yes, pass failure to outer expression
#ifdef INT
	jmp	_interp		/   no, resume interpreting at failure label
#endif INT
#ifdef CMP
	jmp	(r0)		/   no, take failure branch
#endif CMP

#endif PDP11
