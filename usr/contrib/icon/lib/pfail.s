#include "../h/config.h"
/*
 * pfail - returns from an Icon procedure with failure.
 *  The current procedure frame is exited and efail causes
 *  the failure of the enclosing expression.  A tracing
 *  message is produced if &trace is non-zero.
 */
Global(_k_level)
Global(_k_trace)
Global(_efail)
Global(_ftrace)
Global(_pfail)
Global(_line)
Global(_file)

#ifdef VAX
_pfail:
	decl	_k_level	# A procedure is being exited, so
				#  decrement &level.
	tstl	_k_trace	# If &trace is zero,
	beql	dofail		#  no tracing.
				#
				# Otherwise, a trace message is
				#  produced by calling ftrace with
				#  the address of the block of the
				#  failing procedure.
				# &arg0 = (nargs * 8) + 8 + ap
	ashl	$3,4(ap),r0	# r0 = nargs * 8
	addl2	$8,r0		#  + 8
	addl3	r0,ap,r0	#  + ap
				# r0 points to descriptor for procedure
	pushl	4(r0)		#  and second word is address of block
	calls	$1,_ftrace	# ftrace(&arg0)

dofail:
	movl	-4(fp),_line	# Restore _line,
	movl	-8(fp),_file	#  _file,
	movl	-4(ap),efp	#  expression frame pointer,
	movl	-8(ap),gfp	#  generator frame pointer,
	movl	-12(ap),ipc	#  interpreter pc,
	movl	8(fp),ap	#  ap,
	movl	12(fp),fp	#  and fp from failing procedure frame
	jmp	_efail		# Cause failure in expression by
				#  branching to efail
#endif VAX

#ifdef PORT
DummyFcn(_pfail)
#endif PORT
#ifdef PDP11
/ pfail - return from an Icon procedure with failure.
/ Exit the current procedure frame, and branch to _efail.
Global(_boundary)

_pfail:
	dec	_k_level	/ decrement &level

/ Print trace message if &trace is set.

	tst	_k_trace
	beq	1f
	mov	4(r5),r0	/ calculate address of arg0
	asl	r0		/   r0 <- r5 + 4*nargs + 6
	asl	r0
	add	r5,r0
	add	$6,r0
	mov	2(r0),-(sp)	/ push address of failing procedure block
	mov	$1,_boundary	/ prevent csv from setting boundary
	jsr	pc,_ftrace
	clr	_boundary
	tst	(sp)+		/ pop address of procedure block

/ Exit the procedure frame, decrement &level, and jump to efail.

1:
	mov	r5,r0
	mov	-(r0),r4
	mov	-(r0),r3
	mov	-(r0),r2
	mov	-(r0),_line
	mov	-(r0),_file
	mov	r5,sp
	mov	(sp)+,r5
	jmp	_efail		/ pass failure to expression frame
#endif PDP11
