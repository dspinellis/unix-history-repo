From Prof. Kahan at UC at Berkeley
# 
# Copyright (c) 1985 Regents of the University of California.
# 
# Use and reproduction of this software are granted  in  accordance  with
# the terms and conditions specified in  the  Berkeley  Software  License
# Agreement (in particular, this entails acknowledgement of the programs'
# source, and inclusion of this notice) with the additional understanding
# that  all  recipients  should regard themselves as participants  in  an
# ongoing  research  project and hence should  feel  obligated  to report
# their  experiences (good or bad) with these elementary function  codes,
# using "sendbug 4bsd-bugs@BERKELEY", to the authors.
#

# @(#)cabs.s	1.2 (Berkeley) %G%

# double precision complex absolute value
# CABS by W. Kahan, 9/7/80.
# Revised for reserved operands by E. LeBlanc, 8/18/82
# argument for complex absolute value by reference, *4(ap)
# argument for cabs and hypot (C fcns) by value, 4(ap)
# output is in r0:r1 (error less than 0.86 ulps)

	.text
	.align	1
	.globl  _cabs
	.globl  _hypot
	.globl	_z_abs
	.globl	libm$cdabs_r6
	.globl	libm$dsqrt_r5

#	entry for c functions cabs and hypot
_cabs:
_hypot:
	.word	0x807c		# save r2-r6, enable floating overflow
	movq	4(ap),r0	# r0:1 = x
	movq	12(ap),r2	# r2:3 = y
	jmp	cabs2
#	entry for Fortran use, call by:   d = abs(z)
_z_abs:
	.word	0x807c		# save r2-r6, enable floating overflow
	movl	4(ap),r2	# indirect addressing is necessary here
	movq	(r2)+,r0	# r0:1 = x
	movq	(r2),r2		# r2:3 = y

cabs2:
	bicw3	$0x7f,r0,r4	# r4 has signed biased exp of x
	cmpw	$0x8000,r4
	jeql	return		# x is a reserved operand, so return it
	bicw3	$0x7f,r2,r5	# r5 has signed biased exp of y
	cmpw	$0x8000,r5
	jneq	cont		# y isn't a reserved operand
	movq	r2,r0		# return y if it's reserved
	ret

cont:
	bsbb	regs_set	# r0:1 = dsqrt(x^2+y^2)/2^r6
	addw2	r6,r0		# unscaled cdabs in r0:1
	jvc	return		# unless it overflows
	subw2	$0x80,r0	# halve r0 to get meaningful overflow
	addd2	r0,r0		# overflow; r0 is half of true abs value
return:
	ret

libm$cdabs_r6:			# ENTRY POINT for cdsqrt
				# calculates a scaled (factor in r6)
				# complex absolute value

	movq	(r4)+,r0	# r0:r1 = x via indirect addressing
	movq	(r4),r2		# r2:r3 = y via indirect addressing

	bicw3	$0x7f,r0,r5	# r5 has signed biased exp of x
	cmpw	$0x8000,r5
	jeql	cdreserved	# x is a reserved operand
	bicw3	$0x7f,r2,r5	# r5 has signed biased exp of y
	cmpw	$0x8000,r5
	jneq	regs_set	# y isn't a reserved operand either?

cdreserved:
	movl	*4(ap),r4	# r4 -> (u,v), if x or y is reserved
	movq	r0,(r4)+	# copy u and v as is and return
	movq	r2,(r4)		# (again addressing is indirect)
	ret

regs_set:
	bicw2	$0x8000,r0	# r0:r1 = dabs(x)
	bicw2	$0x8000,r2	# r2:r3 = dabs(y)
	cmpw	r0,r2
	jgeq	ordered
	movq	r0,r4
	movq	r2,r0
	movq	r4,r2		# force y's exp <= x's exp
ordered:
	bicw3	$0x7f,r0,r6	# r6 = exponent(x) + bias(129)
	jeql	retsb		# if x = y = 0 then cdabs(x,y) = 0
	subw2	$0x4780,r6	# r6 = exponent(x) - 14
	subw2	r6,r0		# 2^14 <= scaled x < 2^15
	bitw	$0xff80,r2
	jeql	retsb		# if y = 0 return dabs(x)
	subw2	r6,r2
	cmpw	$0x3780,r2	# if scaled y < 2^-18
	jgtr	retsb		#   return dabs(x)
	emodd	r0,$0,r0,r4,r0	# r4 + r0:1 = scaled x^2
	emodd	r2,$0,r2,r5,r2	# r5 + r2:3 = scaled y^2
	addd2	r2,r0
	addl2	r5,r4
	cvtld	r4,r2
	addd2	r2,r0		# r0:1 = scaled x^2 + y^2
	jmp	libm$dsqrt_r5	# r0:1 = dsqrt(x^2+y^2)/2^r6
retsb:
	rsb			# error < 0.86 ulp
