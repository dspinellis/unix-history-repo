#include "../h/config.h"

/*
 * efail - handles the failure of an expression.  efail is used
 *  by a number of routines.  Its task to resume the newest
 *  inactive generator in the current expression frame.  If
 *  no such generator exists, the expression frame is exited
 *  and execution continues at the point indicated in the
 *  expression marker.  If the marker has a 0 address for
 *  the point to continue at, efail is called to fail again.
 */

Global(_interp)		/* interpreter loop */
Global(_atrace)		/* trace generator reactivations */
Global(_boundary)	/* Icon/C boundary address */
Global(_line)		/* current line number */
Global(_file)		/* current file name */
Global(_k_level)	/* value of &level */
Global(_k_trace)	/* value of &trace */

Global(_efail)
/*
 * Note that efail is jumped to.
 */
#ifdef VAX
_efail:
	tstl	gfp		# gfp points to the most recent generator
				#  frame.  If it's 0, there is no inactive
	jeql	nogen		#  generator in this expression frame and
				#  the frame is exited.

/*
 * There is an inactive generator in this expression frame. It
 *  must be reactivated.
 */
	movl	(gfp),_boundary	# Restore boundary address from that
				#  stored in the generator frame.
	movl	fp,r0		# Save fp value for testing later.
				# The fp must be restored.  gfp points
	movl	gfp,fp		#  at the word before the former top
	tstl	(fp)+		#  of stack.  The fp is loaded from
				#  gfp and then incremented by 4.
/*
 * Trace resumption if &trace is set and if generator is an Icon procedure.
 */
	tstl	_k_trace	# If &trace is 0,
	jeql	tracedone	#  no tracing.
	cmpl	_boundary,fp	# If the boundary is not the same as the
	jneq	tracedone	#  fp, the generator is not an Icon procedure.
	movl	12(fp),efp	# Point efp at saved fp in generator frame.
				#  If efp is the same as the fp was upon
	cmpl	efp,r0		#  entry to efail, then generator is 
	jeql	tracedone	#  control regime
				#
				# Otherwise, an Icon procedure is to be
				#  resumed.  atrace handles the tracing,
				#  and takes the address of the procedure
				#  block as it's only argument.  arg0 is
				#  is the descriptor for the block.  It is
				#  located using:
				#   &arg0 = (ap + 8 + 8*nargs)
	movl	8(fp),r4	# r4 = old ap
	ashl	$3,4(r4),r2	# r2 = nargs * 8
	addl2	r4,r2   	# ... + old ap
	addl2	$8,r2		# ... + 8
	pushl	4(r2)		# r2 points to descriptor, push address
				#  residing in second word.
	calls	$1,_atrace	# atrace(&arg0)
/*
 * Resume the generator.
 */
tracedone:
	movl	-(gfp),_k_level	# Restore &level
	movl	-(gfp),_line	# Restore _line and
	movl	-(gfp),_file	#  _file from the generator frame.

	cmpl	_boundary,fp	# If the boundary is the same as the
	bneq	f3		#  fp, the return if from Icon to C
	clrl	_boundary	#  and the boundary is cleared.
f3:
   	ret			# All set, return.  This return
				#  will sweep off the generator frame
				#  on the top of the stack and resume
				#  execution after point where the
				#  suspension that created this generator
				#  was performed.

/* 
 * There are no inactive generators in the expression frame, thus
 *  the expression fails.  If the failure label in the expression
 *  frame marker is not 0, execution resumes at the indicated address.
 *  Otherwise, failure in the surrounding expression is signaled by
 *  looping back to efail.
 */
nogen:
 	movl	-8(efp),ipc	# Point ipc at failure label from expression
				#  frame.  Note that this is always a
				#  ucode address.
	movl	-4(efp),gfp	# Restore old generator frame pointer from
				#  expression frame marker.
	movl	efp,sp		# Move sp back to point at expression frame
				#  marker.
	movl	(sp)+,efp	# Restore old expression frame pointer and
				#  move sp up to previous word.  Moving
				#  the sp up effectively removes the
				#  expression marker from the stack.
 	tstl	ipc		# If the failure label in the expression
				#  frame is 0, this expression fails
	jeql	_efail		#  by branching back to efail.
 	jmp	_interp		# Otherwise, execution continues. (at
				#  address in ipc)

#endif VAX

#ifdef PORT
DummyFcn(_efail)
#endif PORT

#ifdef PDP11
/ efail - reactivate newest inactive generator within current
/ expression frame.  If there are none, exit the expression
/ frame and take the failure branch (stored in the expression
/ frame marker).
Global(cret)
_efail:
	tst	r3		/ test for inactive generators,
	beq	1f		/   branch if none

/ Reactivate newest inactive generator.

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
	mov	-4(r4),r2	/ get failure label
	mov	-2(r4),r3	/ exit current expression frame
	mov	r4,sp		
	mov	(sp)+,r4
	tst	r2		/ is failure label zero?
	beq	_efail		/   yes, pass failure to outer expression
	jmp	_interp		/   no, resume interpreting at failure label

#endif PDP11
