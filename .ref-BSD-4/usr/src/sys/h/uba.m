#
#	uba.m	4.1	11/9/80
#
#
#	VAX unibus adapter registers
#

	.set	UBA,0x80060000		# virtual address of unibus registers
	.set	UBA0,0x80060000
	.set	UBR_OFF,0x30		# UBA offset to BRRVR regs
	.set	CFGFLT,0xfc000000	# SBI fault bits in UBA config reg
	.set	UCN_OFF,0		# UBA offset to config reg
	.set	UST_OFF,8		# UBA offset to status reg
	.set	UFMER_OFF,0x10		# UBA offset to failed map register
	.set	UFUBAR_OFF,0x14		# UBA offset to failed UB addr reg

	.set	UBA_SR,UBA + 0x8
	.set	UBA_BR4,UBA + 0x30	# virtual address of intr vect registers

