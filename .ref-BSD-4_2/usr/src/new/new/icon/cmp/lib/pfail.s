#include "../h/config.h"
#ifdef VAX
/*
 * pfail - return from an Icon procedure with failure.
 * Exit the current procedure frame, and branch to _efail.
 */

.globl	_k_level		# &level
.globl	_k_trace		# &trace

.globl	_efail			# signal failure in an expression frame
.globl	_ftrace			# print trace message

.globl	_pfail

_pfail:
#ifdef INT
	.word	0
	movl	$unwound,16(fp)	# unwind the stack--if we're lucky
	ret			# should go right to unwound:
unwound:
#endif INT
	decl	_k_level	# decrement &level

   /* Print trace message if &trace is set. */

	tstl	_k_trace
	beql	1f

	ashl	$3,4(ap),r0	# r0 <- address of arg0
	addl2	$8,r0
	addl3	r0,ap,r0
	pushl	4(r0)		# push address of failing procedure block
	pushl   $0   		# ftrace needs nargs
	calls   $2,_ftrace

   /* Exit the procedure frame, decrement &level, and jump to efail. */

1:
	movl	-4(fp),_line
	movl	-8(fp),_file
	movl    -4(ap),r11
	movl	-8(ap),r10
	movl	-12(ap),r9
	movl	8(fp),ap
	movl	12(fp),fp
	jmp	_efail		# pass failure to expression frame
#endif VAX
#ifdef PDP11
/ pfail - return from an Icon procedure with failure.
/ Exit the current procedure frame, and branch to _efail.

.globl	_file			/ current file name
.globl	_k_level		/ &level
.globl	_k_trace		/ &trace
.globl	_line			/ current line number

.globl	_efail			/ signal failure in an expression frame
.globl	_ftrace			/ print trace message

.globl	_pfail
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
	clr	-(sp)		/ ftrace needs nargs
	jsr	pc,_ftrace
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
