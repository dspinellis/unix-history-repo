# Copyright (c) 1985, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)tan.s	8.1 (Berkeley) %G%
#
	.data
	.align	2
_sccsid:
.asciz	"@(#)tan.s	1.1 (Berkeley) 8/21/85; 8.1 (ucb.elefunt) %G%"

#  This is the implementation of Peter Tang's double precision  
#  tangent for the VAX using Bob Corbett's argument reduction.
#  
#  Notes:
#       under 1,024,000 random arguments testing on [0,2*pi] 
#       tan() observed maximum error = 2.15 ulps
#
# double tan(arg)
# double arg;
# method: true range reduction to [-pi/4,pi/4], P. Tang  &  B. Corbett
# S. McDonald, April 4,  1985
#
	.globl	_tan
	.text
	.align	1

_tan:	.word	0xffc		# save r2-r11
	movq	4(ap),r0
	bicw3	$0x807f,r0,r2
	beql	1f		# if x is zero or reserved operand then return x
#
# Save the PSL's IV & FU bits on the stack.
#
	movpsl	r2
	bicw3	$0xff9f,r2,-(sp)
#
#  Clear the IV & FU bits.
#
	bicpsw	$0x0060
	jsb	libm$argred
#
#  At this point,
#	   r0  contains the quadrant number, 0, 1, 2, or 3;
#	r2/r1  contains the reduced argument as a D-format number;
#  	   r3  contains a F-format extension to the reduced argument;
#
#  Save  r3/r0  so that we can call cosine after calling sine.
#
	movq	r2,-(sp)
	movq	r0,-(sp)
#
#  Call sine.  r4 = 0  implies sine.
#
	movl	$0,r4
	jsb	libm$sincos
#
#  Save  sin(x)  in  r11/r10 .
#
	movd	r0,r10
#
#  Call cosine.  r4 = 1  implies cosine.
#
	movq	(sp)+,r0
	movq	(sp)+,r2
	movl	$1,r4
	jsb	libm$sincos
	divd3	r0,r10,r0
	bispsw	(sp)+
1:	ret
