	.set	HIGH,31		# mask for total disable
	.set	MCKVEC,4	# offset into Scbbase of machine check vector

# Trap vectors and C interface for Vax

#
#
#
# System control block
#

	.set	INTSTK,1	# handle this interrupt on the interrupt stack
	.set	HALT,3		# halt if this interrupt occurs
	.align	9
	.globl	Scbbase
Scbbase:
	.long	Xrandom + HALT	# unused
	.long	Xmachcheck + HALT	# machine check interrupt
	.long	Xkspnotval + HALT	# kernal stack not valid
	.long	Xpowfail + HALT	# power fail
	.long	Xprivinflt	# privileged instruction 
	.long	Xxfcflt		# xfc instruction 
	.long	Xresopflt	# reserved operand 
	.long	Xresadflt	# reserved addressing 
	.long	Xprotflt	# protection 
	.long	Xsegflt		# segmentation 
	.long	Xtracep		# trace pending
	.long	Xbptflt		# bpt instruction
	.long	Xcompatflt	# compatibility mode fault
	.long	Xarithtrap	# arithmetic trap
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xsyscall	# chmk
	.long	Xchme+HALT		# chme
	.long	Xchms+HALT		# chms
	.long	Xchmu+HALT		# chmu
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# software level 1
	.long	Xrandom + HALT	# software level 2 (asts)
	.long	Xresched	# reschedule nudge
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
ubabase:
	.long	Xclockint + INTSTK	# clock
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xconsrint + INTSTK	# console receiver 
	.long	Xconsxint + INTSTK	# console transmitter

#	I/O vectors

# IPL 14
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xua0int + INTSTK	# UBA 0 br4
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused

# IPL 15
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xua0int + INTSTK	# UBA 0 br5
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xmba0int + INTSTK	# mass bus adapter 0
	.long	Xmba1int + INTSTK	# mass bus adapter 1
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused

# IPL 16
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xua0int + INTSTK	# UBA 0 br6
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused

# IPL 17
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused
	.long	Xrandom + HALT	# unused


#  0x200
# Produce a core image dump on mag tape
	.globl	doadump
doadump:
	movl	sp,dumpstack	# save stack pointer
	movab	dumpstack,sp	# reinit stack
	mfpr	$PCBB,-(sp)	# save u-area pointer
	mfpr	$MAPEN,-(sp)	# save value
	mfpr	$IPL,-(sp)	# ...
	mtpr	$0,$MAPEN		# turn off memory mapping
	mtpr	$HIGH,$IPL		# disable interrupts
	pushr	$0x3fff			# save regs 0 - 13
	calls	$0,_dump	# produce dump
	halt

	.data
	.align	2
	.globl	dumpstack
	.space	58*4		# seperate stack for tape dumps
dumpstack: 
	.space	4
	.text


# I/O interrupt vector routines
#

# Catch random or unexpected interrupts

	.align	2
Xrandom:
Xmachcheck:
Xkspnotval:
Xpowfail:
Xchme:
Xchms:
Xchmu:
	halt

# Massbus 0 adapter interrupts

	.align	2
Xmba0int:
	pushr	$0x3f		# save r0 - r5
	movab	MBA0_CSR,r0	# point at mba regs
	movl	MBA_AS(r0),r1		# get attn summary bits
	cvtwl	r1,-(sp)		# push attn summary as arg
	pushl	MBA_SR(r0)	# pass sr as argument
	mnegl	$1,MBA_SR(r0)	# clear attention bit
	calls	$2,_hpintr	# call rp06 interrupt dispatcher
	brw 	int_ret		# merge with common interrupt code


# Massbus 1 adapter interrupts

	.align	2
Xmba1int:
	pushr	$0x3f
	movab	MBA1_CSR,r0
	pushl	MBA_AS(r0)
	mnegl	$1,MBA_AS(r0)
	pushl	MBA_SR(r0)		# pass cr as argument_
	mnegl	$1,MBA_SR(r0)	# clear attention bit
	calls	$2,_htintr	# call te16 interrupt dispatcher
	brw 	int_ret		# return from interrupt


# Unibus adapter interrupts

	.align	2
Xua0int:	#  UBA 0 interrupts
	pushr	$0x3f  #  save regs 0-5
	pushab	UBA0  # save UBA base addr
	brb	Xuacom  #  jump to common code
Xua1int :	#  UBA 1 interrupts
Xuacom :	#  common code for UBA interrupts
	popr	$1		# UBA base addr
	mfpr	$IPL,r2		# get br level
	movl	UBR_OFF-20*4(r0)[r2],r3	# get unibus device vector
	bleq	ubasrv  # branch if zero vector or UBA service required
#  normal UBA interrupt point - device on a UBA has generated an
#    interrupt - r3 holds interrupt vector
ubanorm :
	movl	_UNIvec(r3),r1  #  get interrupt service routine address
#			and controller code from UNIBUS vector area
	extzv	$27,$4,r1,-(sp)  #  controller code is in 4 most
#				significant bits-1 of ISR addr
	bicl2	$0x78000000,r1  #  clear code
	calls	$1,(r1)  #  call ISR
	brw	int_ret  #  go to common interrupt return
#  here for zero or negative UBA interrupt vector.
#  negative vector -> UBA requires service
ubasrv :
	beql	ubapass
#
#  UBA service required
#  The following 'printf' calls should probably be replaced
#    with calls to an error logger and/or some corrective action.
	bitl	$CFGFLT,UCN_OFF(r0)  #  any SBI faults ?
	beql	UBAflt
	pushr	$0xf  #  save regs 0-3
	pushab	SBImsg
	calls	$1,_printf
	popr	$0xf
	halt
#
#  no SBI fault bits set in UBA config reg - must be
#    some error bits set in UBA status reg
UBAflt :
	movl	UST_OFF(r0),r2  #  UBA status reg
	pushr	$0xf  #  save regs 0-3
	pushr	r2
	pushab	UBAmsg
	calls	$2,_printf
	popr	$0xf
	movl	r2,UST_OFF(r0)  #  clear error bits
	bicl2	$0x80000000,r1  #  clear neg bit in vector
	bneq	ubanorm  #  branch if normal UBA interrupt to process
	brb 	int_ret		# restore regs and return
#
#  zero interrupt vector - print message & continue
ubapass :
	pushr	$0xf
	pushab	ZERmsg
	calls	$1,_printf
	popr	$0xf
	brb	int_ret


# Console receiver interrupt

	.align	2
Xconsrint:
	pushr	$0x3f		# save registers 0 - 5
	calls	$0,_consrint
	brb 	int_ret		# merge


# Console transmit interrupt
	.align	2
Xconsxint:
	pushr	$0x3f		# save registers 0 - 5
	calls	$0,_consxint
	brb 	int_ret


# Clock interrupt
	.align	2
Xclockint:
	pushr	$0x3f		# save regs 0 - 5
	pushl	4+6*4(sp)	# push psl
	pushl	4+6*4(sp)	# push pc
	calls	$2,_clock
	brb 	int_ret


#
# Common code for interrupts
# At this point, the interrupt stack looks like:
#
#		 ___________
#		|    r0     | :isp
#		|-----------|
#		|    ...    |
#		|-----------|
#		|    r5     |
#		|-----------|
#		|    pc     |
#		|-----------|
#		|    psl    |
#		|___________|


int_ret:
	bbssi	$0,idleflag,int_r0		# set idle escape flag (no wait instr)
int_r0:
	popr	$0x3f		# restore regs 0 - 5
	bitl	$PSL_CURMOD,4(sp)	# interrupt from user mode?
	beql	int_r1		# no, from kernal, just rei
	tstb	_runrun		# should we reschedule?
	beql	int_r1		# no, just rei
#
# If here, interrupt from user mode, and time to reschedule.
#	to do this, we set a software level 3 interrupt to
#	change to kernal mode, switch stacks,
#	and format kernal stack for a `qswitch' trap to force
#	a reschedule.
#
	mtpr	$0x18,$IPL	# make sure int won't happen now
	mtpr	$3,$SIRR	# request level 3 software interrupt
int_r1:
	rei 			# return to interrupted process

#
# Trap and fault vector routines
#

#	Reschedule trap (Software level 3 interrupt)
	.align	2
Xresched:
	movpsl	-(sp)	# get ps
	bicl2	$PSL_IPL,(sp)	# lower ipl
	pushab	resc1		# push pc
	rei			# lower ipl, jump to resc1

resc1:
	pushl	$0			# dummy code
	pushl	$RESCHED	# type
	brb 	alltraps	# merge


#	privileged instruction fault
	.align	2
Xprivinflt:
	pushl	$0		# push dummy code
	pushl	$PRIVINFLT	# push type
	brb 	alltraps	# merge



#	xfc instruction fault
	.align	2
Xxfcflt:
	pushl	$0		# push dummy code value
	pushl	$XFCFLT		# push type value
	brb 	alltraps	# merge


#	reserved operand fault
	.align	2
Xresopflt:
	pushl	$0		# push dummy code value
	pushl	$RESOPFLT	# push type value
	brb 	alltraps	# merge


#	reserved addressing mode fault
	.align	2
Xresadflt:
	pushl	$0		# push dummy code value
	pushl	$RESADFLT	# push type value
	brb 	alltraps	# merge with common code


#	bpt instruction fault
	.align	2
Xbptflt:
	pushl	$0		# push dummy code value
	pushl	$BPTFLT		# push type value
	brb 	alltraps	# merge with common code

#	Compatibility mode fault
	.align	2
Xcompatflt:
	pushl	$COMPATFLT	# push type value
	brb 	alltraps	#merge with common code


#	Trace trap

	.align	2
Xtracep:
	pushl	$0		# push dummy code value
	pushl	$TRCTRAP	# push type value
	brb 	alltraps	# go do it

#	Arithmitic trap
	.align	2
Xarithtrap:
	pushl	$ARITHTRAP	# push type value
	brb 	alltraps	# merge with common code


#	Protection  fault

	.align	2
Xprotflt:
	blbs	(sp),Xsegflt		# check for pt length violation
	addl2	$4,sp		# pop fault param word 
	pushl	$PROTFLT
	brb 	alltraps

#	Segmentation fault

	.align	2
Xsegflt:
	addl2	$4,sp
	pushl	$SEGFLT
	brb 	alltraps




#
# CHMK trap (syscall trap)
#	on entry, kernal stack:
#
#		 ___________ 
#		|    code   | :ksp
#		|-----------|
#		|    pc     |
#		|-----------|
#		|    psl    |
#		|___________|
#
#
#
#	stack (parameters) at calls to _trap:
#
#		 ___________ 
#		|    ap     | :ksp
#		|-----------|
#		|    r0     |
#		|-----------|
#		|    ...    |
#		|-----------|
#		|    r13    |
#		|-----------|
#		|    usp    |
#		|-----------|
#		|    type   |
#		|-----------|
#		|    code   |
#		|-----------|
#		|    pc     |
#		|-----------|
#		|    psl    |
#		|___________|
#

	.align	2
Xsyscall:
	pushl	$SYSCALL	# push type value
alltraps:
	movq	8(sp),_u+PCB_PC	# save pc
	bitl	$PSL_CURMOD,12(sp)	# from user mode?
	beql	sysc1		# no
# Prepare arguments to _trap, note that type has already been pushed
	mfpr	$USP,-(sp)	# get usp
	pushr	$0x3fff		# registers 0 - 13
	pushl	ap		# ptr to syscall parameters
#
# Call _trap with wrong number of arguments
#   so args not popped by ret
#
	calls	$1,_trap
# Restore
	popr	$0x3fff		# restore regs 0 - 13
	mtpr	(sp)+,$USP	# restore usp
	addl2	$8,sp		# pop type, code
#
	bitl	$PSL_CURMOD,4(sp)		# are we returning to user mode?
	beql	sysc2		# no
# Return
	rei

sysc1:
	movab	emsg1,eptr	# set message pointer
	brb 	err_print	# print message and halt

sysc2:
	movab	emsg2,eptr	# pointer to error message
	brb 	err_print	# print msg and halt

#
# err_print
#	print message on console and die
#	message pointed to by eptr, terminated by zero byte.
#

err_print:
	mtpr	$HIGH,$IPL	# disable all interrupts
	mtpr	$0,$TXCS	# disable interrupts on console transmitter
eloop1:
	mfpr	$TXCS,ewk1	# get transmitter status
	bbc 	$TXCS_BRDY,ewk1,eloop1	# loop if not ready to transmit
	tstb	*eptr		# end of message?
	beql	eout		# yes, out of loop
	movzbl	*eptr,ewk1	# get byte of message
	incl	eptr		# bump pointer
	mtpr	ewk1,$TXDB	# give byte to transmitter
	brb 	eloop1		# loop

eout:
	halt

	.data
eptr:	.long	0
ewk1:	.long	0
	.text



#
# Initialization

#
#	IPL == 1F
#	MAPEN == off
#	SCBB, PCBB not set
#	SBR, SLR not set
#	ISP, KSP not set
#
	.globl	start
start:
	.word	0x0000
	mtpr	$HIGH,$IPL		# no interrupts yet
	mtpr	$Scbbase,$SCBB	# set SCBB
	mtpr	$_Sysmap,$SBR	# set SBR
	mtpr	$Syssize,$SLR	# set SLR
	mtpr	$_Sysmap,$P0BR	# set temp P0BR
	mtpr	$Syssize,$P0LR	# set temp P0LR
	movl	$_intstack+2048,sp	# set ISP
# initialize i/o adatpers
	movl	$1,PHYSMBA0+4	# init & interrupt enable
	movl	$4,PHYSMBA0+4	# init & interrupt enable
	movl	$1,PHYSMBA1+4	# init  interrupt enable
	movl	$4,PHYSMBA1+4	# init  interrupt enable
	movl	$1,PHYSUBA+4	# init  interrupt enable
	movl	$0x78,PHYSUBA+4	# init  interrupt enable

	movl	Scbbase+MCKVEC,r5	# save machine check entry
	movab	startint+INTSTK,Scbbase+MCKVEC	# set new vector address
#
# will now see how much memory there really is
#	in 64kb chunks, save number of bytes in r7
#
	mtpr	$HIGH-1,$IPL	# allow machine check interrupts
	clrl	r7
startlp:
	tstl	(r7)		# this chunk really there?
	acbl	$8096*1024-1,$64*1024,r7,startlp	# loop till machine check
	brb 	startint	# full load of memory, avoid .align
	.align	2
startint:
	mtpr	$0,$SBIFS		# clear sbi fault status
	movl	r5,Scbbase+MCKVEC	# restore machine check vector
	movl	$_intstack+2048,sp	# reset interrupt stack pointer

#	clear memory starting with unitialized data (kernal)
	movab	_edata,r6
	movab	_end+8096,r5	# clear uninitialized data and some slop
strtclr:
	clrq	(r6)
	acbl	r5,$8,r6,strtclr

#
#	initialize system page table
#
	movab	_etext+511,r1	# end of kernal text segment
	bbcc	$31,r1,strt1	# turn off high order bit
strt1:
	ashl	$-9,r1,r1	# last page of kernal text
	clrl	r2		# point at first kernal text page
strtlp1:
	bisl3	$PG_V|PG_KR,r2,_Sysmap[r2]	# initialize page table entry
	aoblss	r1,r2,strtlp1	# fill text entries
	movab	_end+511,r1	# end of kernal data segment
	bbcc	$31,r1,strt2	# turn off high order bit
strt2:
	ashl	$-9,r1,r1	# last page of kernal data
strtlp2:
	bisl3	$PG_V|PG_KW,r2,_Sysmap[r2]	# fill data entries
	aoblss	r1,r2,strtlp2

#	init i/o space page table entries
	movl	$PHYSUBA/512,r1		# page frame number for uba
	movab	_Sysmap+uba_offset,r2	# page table address
	movab	15(r1),r3	# last pt entry
strtlp3:
	bisl3	$PG_V|PG_KW,r1,(r2)+	# init pt entry
	aobleq	r3,r1,strtlp3
	movl	$PHYSUMEM/512,r1
	movab	_Sysmap+umem_offset,r2	# page table address
	movab	15(r1),r3		# limit
strtlp4:
	bisl3	$PG_V|PG_KW,r1,(r2)+
	aobleq	r3,r1,strtlp4
	movl	$PHYSMBA0/512,r1
	movab	_Sysmap+mba0_offset,r2
	movab	15(r1),r3
strtlp5:
	bisl3	$PG_V|PG_KW,r1,(r2)+
	aobleq	r3,r1,strtlp5
	movl	$PHYSMBA1/512,r1
	movab	_Sysmap+mba1_offset,r2
	movab	15(r1),r3
strtlp6:
	bisl3	$PG_V|PG_KW,r1,(r2)+
	aobleq	r3,r1,strtlp6

	mtpr	$1,$TBIA	# invalidate all trans buffer entries
	mtpr	$1,$MAPEN	# turn on memory mapping
	jmp 	*$startmap	# put system virtual address in pc

startmap:
# set maxmem = btoc(r7)
	ashl	$-9,r7,_maxmem
	movl	_maxmem,_physmem
	movl	_maxmem,_freemem

#
# Setup context for proc[0] == Scheduler
#
# address first page past _end
#	this will be u area for proc[0].
#	initialize u area page table entries.
#	initialize (slightly) the pcb.

	movab	_end+511,r6
	bicl2	$0x1ff,r6	# make page boundary

# set up u area page table
	bbcc	$31,r6,strt3
strt3:
	ashl	$-9,r6,r3		# r3 = btoc(r6)
	bisl3	$PG_V|PG_KW,r3,_Sysmap+u_ptoffset	# init first u pt entry
	movab	_u,r1		# point at _u area
	mtpr	r1,$TBIS
	movab	usize*512(r1),PCB_KSP(r1)	# init ksp
	mnegl	$1,PCB_ESP(r1)		# invalidate esp
	mnegl	$1,PCB_SSP(r1)		# invalidate ssp
	movl	$0x80000000,PCB_USP(r1)		# set user sp
	movab	_u+usize*512,PCB_P0BR(r1)	# p0 page table pointer
	clrl	PCB_P0LR(r1)		# size zero page table
	movb	$4,PCB_P0LR+3(r1)	# disable ast
	movab	_u+(usize+1)*512-0x800000,PCB_P1BR(r1)		# p1 page table pointer
	movl	$0x200000,PCB_P1LR(r1)		# invalid p1 p t length
	movl	$1,PCB_SZPT(r1)		# init number pages usr page table
	addl3	$usize,r3,PCB_SZPT+4(r1)	# store into u.u_ptable[0]
	movl	$1,r2
strt5:
	incl	r3
	bisl3	$PG_V|PG_KW,r3,_Sysmap+u_ptoffset[r2]
	addl2	$512,r1
	mtpr	r1,$TBIS
	aoblss	$usize,r2,strt5

	mtpr	r6,$PCBB		# first pcb

	ldpctx			# set regs, p0br, p0lr, p1br, p1lr, 
#				astlvl, ksp, and change to kernal mode
	addl2	$8,sp		# pop dummy pc, psl
	mtpr	$0,$IPL		# enable interrupts
	movab	_end+511,r0	# calculate firstaddr
	bbcc	$31,r0,strt4
strt4:
	ashl	$-9,r0,-(sp)	# convert to clicks and stack
	calls	$1,_main	# startup, fork off /etc/init
#
# proc[1] == /etc/init now running here.
# execute code at location 0, in user mode.
#
	pushl	$PSL_CURMOD|PSL_PRVMOD	# psl, user mode, ipl= 0
	pushl	$0		# pc, $location 0
	rei 			# do /etc/init


#
# Primitives
#

_display:	.globl	_display
_savfp:	.globl	_savfp
_restfp:	.globl	_restfp
	.word	0x0000
	ret

_addupc:	.globl	_addupc
	.word	0x0000
	movl	8(ap),r2	# &u.u_prof
	subl3	8(r2),4(ap),r0	# corrected pc
	blss	addret
	extzv	$1,$31,r0,r0	# logical right shift
	extzv	$1,$31,12(r2),r1	# ditto for scale
	mull2	r1,r0
	ashl	$-14,r0,r0
	incl	r0
	bicb2	$1,r0
	cmpl	r0,4(r2)	# length
	bgequ	addret
	addl2	(r2),r0		# base
	probew	$3,$2,(r0)
	beql	adderr
	addw2	12(ap),(r0)
addret:
	ret
adderr:
	clrl	12(r2)
	ret


_fubyte:	.globl	_fubyte
_fuibyte:.globl	_fuibyte
	.word	0x0000
	prober	$3,$1,*4(ap)	# byte accessible ?
	beql	eret			# no
	movzbl	*4(ap),r0
	ret

_subyte:	.globl	_subyte
_suibyte:.globl	_suibyte
	.word	0x0000
	probew	$3,$1,*4(ap)	# byte accessible ?
	beql	eret			# no
	movb	8(ap),*4(ap)
	clrl	r0
	ret

_fuword:	.globl	_fuword
_fuiword:.globl	_fuiword
	.word	0x0000
	prober	$3,$4,*4(ap)
	beql	eret
	movl	*4(ap),r0
	ret

_suword:	.globl	_suword
_suiword:.globl	_suiword
	.word	0x0000
	probew	$3,$4,*4(ap)
	beql	eret
	movl	8(ap),*4(ap)
	clrl	r0
	ret
eret:
	mnegl	$1,r0			# error return
	ret

_copyin:	.globl	_copyin
_copyiin:.globl	_copyiin
	.word	0x0000
	movl	12(ap),r0	# copy length
	movl	4(ap),r1	# copy user address
	cmpl	$512,r0		# probing one page or less ?
	bgeq	cishort		# yes
ciloop:
	prober	$3,$512,(r1)	# bytes accessible ?
	beql	eret		# no
	addl2	$512,r1		# incr user address ptr
	acbl	$513,$-512,r0,ciloop	# reduce count and loop
cishort:
	prober	$3,r0,(r1)	# bytes accessible ?
	beql	eret		# no
	movc3	12(ap),*4(ap),*8(ap)
	clrl	r0
	ret

_copyout: .globl	_copyout
_copyiout:.globl	_copyiout
	.word	0x0000
	movl	12(ap),r0	# get count
	movl	8(ap),r1	# get user address
	cmpl	$512,r0		# can do in one probew?
	bgeq	coshort		# yes
coloop:
	probew	$3,$512,(r1)	# bytes accessible?
	beql	eret		# no 
	addl2	$512,r1		# increment user address
	acbl	$513,$-512,r0,coloop	# reduce count and loop
coshort:
	probew	$3,r0,(r1)	# bytes accessible?
	beql	eret		# no
	movc3	12(ap),*4(ap),*8(ap)
	clrl	r0
	ret

_idle:	.globl	_idle
	.word	0x0000
	mtpr	$0,$IPL		# enable interrupts
waitloc:	blbc	idleflag,waitloc	# loop until interrupt
ewaitloc:	bbcci	$0,idleflag,idle1	# clear idle escape flag
idle1:
	ret
	.data
	.globl	_waitloc
	.globl	_ewaitloc
	.align	2
_waitloc:	.long	waitloc
_ewaitloc:	.long	ewaitloc
idleflag:	.long	0
	.text



# save reg's and ret loc into save area - return 0
	.globl	_save
_save :		# save(save_area)
	.word	0x0
	mtpr	$HIGH,$IPL
	movl	4(ap),r0  #  save area addr
	movab	2*4(ap),sp  #  restore stack to val before 'save' call
	movl	8(fp),ap  #  restore ap	"	"	"
	movl	16(fp),r1  #  restore pc	"	"	"
	movl	12(fp),fp  #  restore fp	"	"	"
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	ap,(r0)+  #  ap & fp
	movl	sp,(r0)+
	movl	r1,(r0)+  #  ret loc of call to 'save'
	movpsl	-(sp)
	pushl	r1
	svpctx	# save reg's -> PCB
	movpsl	-(sp)  # set up for return
	bicl2	$PSL_IS|PSL_IPL,(sp)  #  undo SVPCTX
	pushl	r1  #  ret loc
	clrl	r0  #  return val
	rei
#
#
#  switch to another process's '_u' area - return val 1
	.globl	_resume
_resume :		#  resume(proc_addr,save_addr)
	.word	0x0
	mtpr	$HIGH,$IPL  # inhibit interrupts
	movl	8(ap),retloc
#  map u-area
	clrl	r0
	movab	_u,r1
	movl	4(ap),r2
res1:	cvtwl	(r2)[r0],r3	# get u click number from proc
	bisl3	$PG_V|PG_KW,r3,_Umap[r0]
	mtpr	r1,$TBIS
	addl2	$512,r1
	aoblss	$usize,r0,res1
#  map user page tables
	clrl	r0
	movl	_u+PCB_SZPT,r3
	jeql	res3		# no user page tables
res2:	bisl3	$PG_V|PG_KW,_u+PCB_SZPT+4[r0],_Umap+4*usize[r0]
	mtpr	r1,$TBIS
	addl2	$512,r1
	aoblss	r3,r0,res2
res3:
	movl	_u,sp		# KSP from u-area
	ashl	$9,_Umap,r5	# pcb address
	mtpr	r5,$PCBB
	ldpctx
	addl2	$8,sp		# clear ps,pc from stack
	movl	retloc,r1  #  'ssav' or 'qsav' addr
	movq	(r1)+,r6
	movq	(r1)+,r8
	movq	(r1)+,r10
	movq	(r1)+,ap
	movl	(r1)+,sp
	movl	$1,r0  # return val
	mtpr	$0,$IPL
	jmp	*(r1)+  #  return to caller at 'save' address
#
	.data
	.align	2
retloc:	.space	1*4
	.text




# disable interrupts
_spl1:	.globl	_spl1
	.word	0x0000
	mfpr	$IPL,r0		# get IPL value
	mtpr	$2,$IPL		# disable RESCHED & AST interrupts
	ret

_spl4:	.globl	_spl4
	.word	0x0000
	mfpr	$IPL,r0
	mtpr	$0x14,$IPL		# disable bus level 4 interrupts
	ret

_spl5:	.globl	_spl5
	.word	0x0000
	mfpr	$IPL,r0
	mtpr	$0x15,$IPL		# disable bus level 5 interrupts
	ret

_spl6:	.globl	_spl6
_spl7:	.globl	_spl7
	.word	0x0000
	mfpr	$IPL,r0
	mtpr	$0x18,$IPL		# disable bus level 7 and clock interrupts
	ret

# enable interrupts
_spl0:	.globl	_spl0
	.word	0x0000
	mfpr	$IPL,r0
	mtpr	$0,$IPL
	ret

# restore interrupt state
_splx:	.globl	_splx
	.word	0x0000
	mfpr	$IPL,r0
	mtpr	4(ap),$IPL
	ret

#
# Copy 1 relocation unit (512 bytes)
# from one physical address to another
_copyseg: .globl	_copyseg
	.word	0x0000
	mfpr	$IPL,r0		# get current pri level
	mtpr	$HIGH,$IPL	# turn off interrupts
	bisl3	$PG_V|PG_KR,4(ap),_Sysmap+CMAP1
	bisl3	$PG_V|PG_KW,8(ap),_Sysmap+CMAP2
	mtpr	$CADDR1,$TBIS	# invalidate entry for copy 
	mtpr	$CADDR2,$TBIS
	movc3	$512,CADDR1,CADDR2
	mtpr	r0,$IPL	# restore pri level
	ret

# zero out physical memory
# specified in relocation units (512 bytes)
_clearseg: .globl	_clearseg
	.word	0x0000
	mfpr	$IPL,r0		# get current pri level
	mtpr	$HIGH,$IPL	# extreme pri level
	bisl3	$PG_V|PG_KW,4(ap),_Sysmap+CMAP1
	mtpr	$CADDR1,$TBIS
	movc5	$0,(r0),$0,$512,CADDR1
	mtpr	r0,$IPL		# restore pri level
	ret

# Check address
# given virtual address, byte count, and rw flag
#  returns 0 on no access
_useracc:	.globl	_useracc
	.word	0x0000
	movl	4(ap),r0		# get va
	movl	8(ap),r1		# count
	tstl	12(ap)		# test for read access ?
	bneq	userar	# yes
	cmpl	$512,r1	# can we do it in one probe ?
	bgeq	uaw2		# yes
uaw1:
	probew	$3,$512,(r0)
	beql	uaerr		# no access
	addl2	$512,r0
	acbl	$513,$-512,r1,uaw1
uaw2:
	probew	$3,r1,(r0)
	beql	uaerr
	movl	$1,r0
	ret

userar:
	cmpl	$512,r1
	bgeq	uar2
uar1:
	prober	$3,$512,(r0)
	beql	uaerr
	addl2	$512,r0
	acbl	$513,$-512,r1,uar1
uar2:
	prober	$3,r1,(r0)
	beql	uaerr
	movl	$1,r0
	ret

uaerr:
	clrl	r0
	ret

#	kernacc
#		check for kernal access privileges
#	Quiz: Why doesn't probe[rw] work?
#

	.globl	_kernacc
_kernacc:
	.word	0x0000

	movl	4(ap),r0	# virtual address
	bbcc	$31,r0,kacc1
	mfpr	$SBR,r2	# address and length of page table (system)
	mfpr	$SLR,r3
	brb	kacc2
kacc1:
	bbsc	$30,r0,kacc3
	mfpr	$P0BR,r2	# user P0
	mfpr	$P0LR,r3
	brb	kacc2
kacc3:
	mfpr	$P1BR,r2	# user P1 (stack)
	mfpr	$P1LR,r3
kacc2:
	addl3	8(ap),r0,r1	# ending virtual address
	ashl	$-9,r0,r0	# page number
	ashl	$-9,r1,r1
	bbc	$30,4(ap),kacc6
	cmpl	r0,r3		# user stack
	blss	kacerr		# address too low
	brb	kacc4
kacc6:	cmpl	r1,r3		# compare last page to P0LR or SLR
	bgeq	kacerr		# address too high
kacc4:	
	movl	(r2)[r0],r1
	bbc	$31,r1,kacerr	# valid bit is off
	cmpzv	$27,$4,r1,$1	# check protection code
	bleq	kacerr		# no access allowed
	tstb	12(ap)
	bneq	kacc5		# only check read access
	cmpzv	$27,$2,r1,$3	# check low 2 bits of prot code
	beql	kacerr		# no write access
kacc5:
	aobleq	r1,r0,kacc4	# next page
	movl	$1,r0		# no errors
	ret
kacerr:
	clrl	r0		# error
	ret

#
#  unsigned int divide :
#		(int) i = udiv( (int)dvdnd , (int) divis)
#
#  unsigned int remainder :
#		(int) j = urem( (int)dvdnd , (int) divis)
#
	.text
	.align	1
	.globl	_udiv
	.globl	_urem
#
_udiv :
	.word	0  #  no reg save
	movl	4(ap),r0  #  dividend
	clrl	r1
	ediv	8(ap),r0,r0,r1  #  quotient in r0
	ret
#
	.align 	1
_urem :
	.word	0
	movl	4(ap),r0
	clrl	r1
	ediv	8(ap),r0,r1,r0  #  remainder in r0
	ret

# define user area virtual address
	.set	physpages,1024
	.set	kernsize,256	 # number of page table entries allocated to kernal
	.globl	_u
	.set	usize,4		# size of user area, in pages
	.set	_u,0x80000000 + kernsize*512
	.globl	_mbautl
	.set	_mbautl,_u+128*512
	.globl	_swaputl
	.set	_swaputl,_mbautl+16*512
	.globl	_swap2utl
	.set	_swap2utl,_swaputl+16*512
	.globl	_forkutl
	.set	_forkutl,_swap2utl+16*512
	.globl	_xswaputl
	.set	_xswaputl,_forkutl+16*512
	.globl	_xallutl
	.set	_xallutl,_xswaputl+16*512
	.globl	_xccdutl
	.set	_xccdutl,_xallutl+16*512
	.globl	_xswap2utl
	.set	_xswap2utl,_xccdutl+16*512
	.set	u_ptoffset,256*4	# offset into _Sysmap of ptentries of _u
	.set	CMAP1,u_ptoffset+16*4	# offset into _sysmap of 1st seg copy entry
	.set	CMAP2,CMAP1+4	# ditto 2ed entry
	.set	CADDR1,_u+16*512	# virtual address of 1st copy segment
	.set	CADDR2,CADDR1+512	# ditto second segment
	.set	PHYSUBA,0x20006000	# real address of uba
	.set	PHYSMBA0,0x20010000	# real addr of mba 0
	.set	PHYSMBA1,0x20012000	# real addre of mba1
	.set	PHYSUMEM,0x2013e000		# real address of unibus memory
	.set	uba_offset,u_ptoffset+32*4	# offset in Sysmap of uba entries
	.set	umem_offset,uba_offset+16*4	# ... unibus device registers
	.set	mba0_offset,umem_offset+16*4	# ... massbus 0
	.set	mba1_offset,mba0_offset+16*4	# ... massbus 1
	.set	mba2_offset,mba1_offset+16*4	# ... massbus 2
	.set	mba3_offset,mba2_offset+16*4	# ... massbus 3



#
# Error messages
#
	.data

emsg1:
	.byte	0xa,0xa,0xa,0xd,0x54,0x52,0x41,0x50,0x20
	.byte	0x46,0x52,0x4f,0x4d,0x20
	.byte	0x4b,0x45,0x52,0x4e,0x41,0x4c,0x20
	.byte	0x4d,0x4f,0x44,0x45,0xa,0xa,0xd,0x0

emsg2:
	.byte	0xa,0xa,0xa,0xd,0x54,0x52,0x41,0x50,0x20
	.byte	0x52,0x45,0x54,0x55,0x52,0x4e,0x20
	.byte	0x44,0x4f,0x20
	.byte	0x4b,0x45,0x52,0x4e,0x41,0x4c,0x20
	.byte	0x4d,0x4f,0x44,0x45,0xa,0xa,0xd,0x0

SBImsg :
	.byte	'S,'B,'I,' ,'f,'a,'u,'l,'t,' ,012,0
 
UBAmsg :
	.byte	'U,'B,'A,' ,'e,'r,'r,'o,'r,' ,'%,'x,012,0
 
ZERmsg :
	.byte	'Z,'e,'r,'o,' ,'V,'e,'c,'t,'o,'r,012,0
 
#
#	_Sysmap:
#		system page table
#
#	structure:
#		2 pages of page table entries
#			reserved for kernal text and data.
#		additional page table entries
#			used in mapping the u area (16 entries),
#			utility entries (16 entries),
#			unibus adapter (16 entries),
#			unibus device memory (16 entries),
#			massbus adapter 0 (16 entries),
#			massbus adapter 1 (16 entries),
#			massbus adapter 2 (16 entries),
#			massbus adapter 3 (16 entries).
#			used for phys & swap I/O
#
#

	.align	2
	.globl	_Sysmap
	.globl	_Umap
	.globl	_Tempmap
	.globl	_Mbamap
	.globl	_Swapmap
	.globl	_Swap2map
	.globl	_Forkmap
	.globl	_Xswapmap
	.globl	_Xallmap
	.globl	_Xccdmap
	.globl	_Xswap2map
_Sysmap:
	.space	2*128*4		# 2 pages of page table entries for kernal
_Umap:
	.space	16*4		# u-area
_Tempmap:
	.space	16*4*7		# utility area & others
_Mbamap:
	.space	16*4		# map for phys & swap I/O
_Swapmap:
	.space	16*4		# swap utility area
_Swap2map:
	.space	16*4		# second swap area
_Forkmap:
	.space	16*4		# fork area
_Xswapmap:
	.space	16*4		# xswap area
_Xallmap:
	.space	16*4		# xalloc area
_Xccdmap:
	.space	16*4		# xccdec area
_Xswap2map:
	.space	16*4		# xswap2 area
	.set	Syssize,4*128		# number pt entries in sys page table
	.globl	_mmap
	.set	_mmap,_Sysmap+CMAP2+4
	.globl	_vmmap
	.set	_vmmap,CADDR2+512
