#
#	VAX unibus adapter registers
#

	.set	UBA,0x80024000		# virtual address of unibus registers
	.set	UBA0,0x80024000
	.set	UBR_OFF,0x30  #  UBA  offset to BRRVR regs
	.set	CFGFLT,0xfc000000  #  SBI fault bits in UBA config reg
	.set	UCN_OFF,0  #  UBA offset to config reg
	.set	UST_OFF,8  #  UBA  offset to status reg

	.set	UBA_SR,UBA + 0x8
	.set	UBA_BR4,UBA + 0x30	# virtual address of interrupt vector registers

