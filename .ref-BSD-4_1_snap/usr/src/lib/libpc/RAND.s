#
# Copyright (c) 1979 Regents of the University of California
#
# sccsid[] = "@(#)RAND.s 1.1 1/16/81";
#
# calculate (1103515245*seed) mod 2^31-1
#
	.globl	_RAND
_RAND:
	.word	0
	emul	4(ap),$1103515245,$0,r0	# a * seed into r1,r0 quadword
	ediv	$0x7fffffff,r0,r1,r0	# get quotient into r1, remainder in r0
	ret
