#
#	mba.m	2.1	1/5/80
#
#
#	VAX massbus adapter registers
#

	.set	MBA0_CSR,0x80048000		# virtual address of mba0
	.set	MBA1_CSR,0x8004a000		# ditto mba1

	.set	MBA_CSR,0	# configuration and status register
	.set	MBA_CR,4	# control register
	.set	MBA_SR,8	# status register
	.set	MBA_VAR,12	# virtual address register
	.set	MBA_BCR,16	# byte count register
	.set	MBA_ERB,0x400	# ???
	.set	MBA_AS,0x410	# attention summary

