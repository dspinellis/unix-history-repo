#
#	mtpr.m	2.1	1/5/80
#
#
#	VAX processor register numbers
#
	.set	KSP,0		# kernel stack pointer
	.set	ESP,1		# exec stack pointer
	.set	SSP,2		# supervisor stack pointer
	.set	USP,3		# user stack pointer
	.set	ISP,4		# interrupt stack pointer
	.set	P0BR,8		# p0 base register
	.set	P0LR,9		# p0 length register
	.set	P1BR,10		# p1 base register
	.set	P1LR,11		# p1 length register
	.set	SBR,12		# system segment base register
	.set	SLR,13		# system segment length register
	.set	PCBB,16		# process control block base
	.set	SCBB,17		# system control block base
	.set	IPL,18		# interrupt priority level
	.set	ASTLVL,19	# async. system trap level
	.set	SIRR,20		# software interrupt request
	.set	SISR,21		# software interrupt summary
	.set	ICCS,24		# interval clock control
	.set	NICR,25		# next interval count
	.set	ICR,26		# interval count
	.set	TODR,27		# time of year (day)
	.set	RXCS,32		# console receiver control and status
	.set	RXDB,33		# console receiver data buffer
	.set	TXCS,34		# console transmitter control and status
	.set	TXDB,35		# console transmitter data buffer
	.set	MAPEN,56	# memory management enable
	.set	TBIA,57		# translation buffer invalidate all
	.set	TBIS,58		# translation buffer invalidate single
	.set	PMR,61		# performance monitor enable
	.set	SID,62		# system identification
#
#	VAX-11/780 specific registers
#
	.set	ACCS,40		# accelerator control and status
	.set	ACCR,41		# accelerator maintenance
	.set	WCSA,44		# WCS address
	.set	WCSD,45		# WCS data
	.set	SBIFS,48	# SBI fault and status
	.set	SBIS,49		# SBI silo
	.set	SBISC,50	# SBI silo comparator
	.set	SBIMT,51	# SBI maintenance
	.set	SBIER,52	# SBI error register
	.set	SBITA,53	# SBI timeout address
	.set	SBIQC,54	# SBI quadword clear
	.set	MBRK,60		# micro-program breakpoint
