# @(#)modf.s	4.2 (Berkeley) 12/21/80
#
#	double modf (value, iptr)
#		double value;
#		double *iptr;
#
#	Modf returns the fractional part of "value", and stores the integer
#	part indirectly through "iptr".
#
#	Entry point
#
	.text
	.align	1
	.globl	_modf
_modf:	.word	0x0000		# We use r0-r2, but do not save them
#
#	This instruction does the following:
#
#		Extend the long floating value at 4(ap) with 0, and
#		multiply it by 1.0.  Store the integer part of the result
#		in r2, and the fractional part of the result in r0-r1.
#
	emodd	4(ap),$0,$0f1.0,r2,r0	# How did you like
					# THAT, sports fans?
	jvs	iovfl			# Jump if integer overflow
	cvtld	r2,*12(ap)		# Else store integer result
	ret				#    and return
#
#	Integer overflow occurred...develop the integer part by
#	subtracting the fractional part from the argument.
#
iovfl:	subd3	r0,4(ap),*12(ap)	# Develop integer part
	ret
