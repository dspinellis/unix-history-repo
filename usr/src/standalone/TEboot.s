#  Stand-alone code to boot TE16 mag tape - read in
#    1st block from tape to core loc 0 and halt
	.set	MBA1,0x20012000
	.set	M_var,MBA1+12  #  MBA 1 virt addr reg
	.set	M_bc,MBA1+16  #  MBA 1 byte count reg
	.set	M_map,MBA1+0x800  #  MBA 1 1st map reg
	.set	M_cr,MBA1+4  #  MBA 1 control reg
	.set	MBAinit,1  #  MBA init bit
#
	.set	TM02,MBA1+0x400  #  TM02 reg's are 1st set of
#			external reg's on MBA 1
	.set	TM_cs1,TM02+0  #  TM02 control reg
	.set	TM_tc,TM02+36  #  TM02 tape control reg
	.set	TM_fc,TM02+0x14
#
	.set	GO,1  #  go bit
	.set	REDF,070  # read fwd function code
	.set	TCHAR,011700  #  drive 0, odd parity, PDP11,
#				800 BPI  NRZI, abort on error - for
#				tape control reg
#
#
	movl	$MBAinit,*$M_cr  #  initialize MBA
	movl	$TCHAR,*$TM_tc  #  set drive characteristics
	clrl	*$M_var  #  virt addr reg = map reg no. + byte offset
	movl	$0x80000000,*$M_map  #  map reg = valid bit + mem page no.
	movl	$-512,*$M_bc  #  byte count = 512
	movl	$REDF+GO,*$TM_cs1  #  read 1 block
	halt
