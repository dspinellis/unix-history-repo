#
#	pcb.m	2.1	1/5/80
#
#
#	VAX process control block
#

	.set	PCB_KSP,0	# kernel stack pointer
	.set	PCB_ESP,4	# exec stack pointer
	.set	PCB_SSP,8	# supervisor stack pointer
	.set	PCB_USP,12	# user stack pointer
	.set	PCB_R0,16
	.set	PCB_R1,20
	.set	PCB_R2,24
	.set	PCB_R3,28
	.set	PCB_R4,32
	.set	PCB_R5,36
	.set	PCB_R6,40
	.set	PCB_R7,44
	.set	PCB_R8,48
	.set	PCB_R9,52
	.set	PCB_R10,56
	.set	PCB_R11,60
	.set	PCB_R12,64
	.set	PCB_R13,68
	.set	PCB_PC,72	# program counter
	.set	PCB_PSL,76	# program status longword
	.set	PCB_P0BR,80	# seg 0 base register
	.set	PCB_P0LR,84	# seg 0 length register and astlevel
	.set	PCB_P1BR,88	# seg 1 base register
	.set	PCB_P1LR,92	# seg 1 length register and pme

#
#	software pcb (extension)
#

	.set	PCB_SZPT,96	# number of pages of user page table
	.set	PCB_CMAP2,100

