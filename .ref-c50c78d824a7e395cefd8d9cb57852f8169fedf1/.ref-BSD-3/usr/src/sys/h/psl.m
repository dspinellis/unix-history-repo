#
#	psl.m	2.1	1/5/80
#
#
#	VAX program status longword
#

	.set	PSL_C,0x1		# carry bit
	.set	PSL_V,0x2		# overflow bit
	.set	PSL_Z,0x4		# zero bit
	.set	PSL_N,0x8		# negative bit
	.set	PSL_T,0x10		# trace enable bit
	.set	PSL_IV,0x20		# integer overflow enable bit
	.set	PSL_FU,0x40		# floating point underflow enable bit
	.set	PSL_DV,0x80		# decimal overflow enable bit
	.set	PSL_IPL,0x1f0000	# interrupt priority level
	.set	PSL_PRVMOD,0xc00000	# previous mode
	.set	PSL_CURMOD,0x3000000	# current mode
	.set	PSL_IS,0x4000000	# interrupt stack
	.set	PSL_FPD,0x8000000	# first part done
	.set	PSL_TP,0x40000000	# trace pending
	.set	PSL_CM,0x80000000	# compatibility mode

