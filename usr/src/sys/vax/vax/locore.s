#
# Machine Language Assist for UC Berkeley Virtual Vax/Unix
#
#	locore.s		3.4	%H%
#

	.set	HIGH,31		# mask for total disable
	.set	MCKVEC,4	# offset into Scbbase of machine check vector
	.set	NBPG,512
	.set	PGSHIFT,9

	.set	CLSIZE,2
	.set	BSIZE,NBPG*CLSIZE
	.set	NBUF,48
	.set	UPAGES,6	# size of user area, in pages

# ====================================
# Trap vectors and C interface for Vax
# ====================================

#
# System control block
#

	.set	INTSTK,1	# handle this interrupt on the interrupt stack
	.set	HALT,3		# halt if this interrupt occurs
	.align	PGSHIFT
	.globl	Scbbase
Scbbase:
	.long	Xstray + INTSTK		# unused
	.long	Xmachcheck + HALT	# machine check interrupt
	.long	Xkspnotval + HALT	# kernel stack not valid
	.long	Xpowfail + HALT		# power fail
	.long	Xprivinflt		# privileged instruction 
	.long	Xxfcflt			# xfc instruction 
	.long	Xresopflt		# reserved operand 
	.long	Xresadflt		# reserved addressing 
	.long	Xprotflt		# protection and pt length violation
	.long	Xtransflt		# address translation not valid fault 
	.long	Xtracep			# trace pending
	.long	Xbptflt			# bpt instruction
	.long	Xcompatflt		# compatibility mode fault
	.long	Xarithtrap		# arithmetic trap
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xsyscall		# chmk
	.long	Xchme+HALT		# chme
	.long	Xchms+HALT		# chms
	.long	Xchmu+HALT		# chmu
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# software level 1
	.long	Xstray + INTSTK		# software level 2 (asts)
	.long	Xresched		# reschedule nudge
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
ubabase:
	.long	Xclockint		# clock
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xcnrint + INTSTK	# console receiver 
	.long	Xcnxint + INTSTK	# console transmitter

#
# I/O vectors
#

# IPL 14
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xua0int + INTSTK	# UBA 0 br4
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused

# IPL 15
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xua0int + INTSTK	# UBA 0 br5
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xmba0int + INTSTK	# mass bus adapter 0
	.long	Xmba1int + INTSTK	# mass bus adapter 1
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused

# IPL 16
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xua0int + INTSTK	# UBA 0 br6
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused

# IPL 17
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused
	.long	Xstray + INTSTK		# unused

# 0x200

# =====================================
# Produce a core image dump on mag tape
# =====================================
	.globl	doadump
doadump:
	movl	sp,dumpstack		# save stack pointer
	movab	dumpstack,sp		# reinit stack
	mfpr	$PCBB,-(sp)		# save u-area pointer
	mfpr	$MAPEN,-(sp)		# save value
	mfpr	$IPL,-(sp)		# ...
	mtpr	$0,$MAPEN		# turn off memory mapping
	mtpr	$HIGH,$IPL		# disable interrupts
	pushr	$0x3fff			# save regs 0 - 13
	calls	$0,_dump		# produce dump
	halt

	.data
	.align	2
	.globl	dumpstack
	.space	58*4			# separate stack for tape dumps
dumpstack: 
	.space	4
	.text

#
# Debugging print switches given here so they won't move around
#
	.data
	.align	2
	.globl	_printsw
_printsw:
	.space	4
	.globl	_coresw
_coresw:
	.space	4
	.text

# =============================
# I/O interrupt vector routines
# =============================

#
# Physical i/o addresses
#
	.set	PHYSMCR,0x20002000	# memory controller register
	.set	PHYSUBA,0x20006000	# uba 0
	.set	PHYSMBA0,0x20010000	# mba 0
	.set	PHYSMBA1,0x20012000	# mba 1
	.set	PHYSUMEM,0x2013e000	# unibus memory

#
# Catch random or unexpected interrupts
#
	.align	2
Xrandom:
Xmachcheck:
Xkspnotval:
Xpowfail:
Xchme:
Xchms:
Xchmu:
	halt

	.align	2
Xstray:
	pushr	$0x3f
	pushab	straym
	calls	$1,_printf
	popr	$0x3f
	rei

#
# Massbus 0 adapter interrupts
#
	.align	2
Xmba0int:
	pushr	$0x3f			# save r0 - r5
	movab	MBA0_CSR,r0		# point at mba regs
	movl	MBA_AS(r0),r1		# get attn summary bits
	cvtwl	r1,-(sp)		# push attn summary as arg
	pushl	MBA_SR(r0)		# pass sr as argument
	mnegl	$1,MBA_SR(r0)		# clear attention bit
	calls	$2,_hpintr		# call rp06 interrupt dispatcher
	brw 	int_ret			# merge with common interrupt code

#
# Massbus 1 adapter interrupts
#
	.align	2
Xmba1int:
	pushr	$0x3f
	movab	MBA1_CSR,r0
	pushl	MBA_AS(r0)
	mnegl	$1,MBA_AS(r0)
	pushl	MBA_SR(r0)		# pass sr as argument
	mnegl	$1,MBA_SR(r0)		# clear attention bit
	calls	$2,_htintr		# call te16 interrupt dispatcher
	brw 	int_ret			# return from interrupt

#
# Unibus adapter interrupts
#
	.align	2
Xua0int:
	pushr	$0x3f  			# save regs 0-5
	mfpr	$IPL,r2			# get br level
	movl	UBA0+UBR_OFF-20*4[r2],r3# get unibus device vector
	bleq	ubasrv  		# branch if zero vector
					# ... or UBA service required

#
# Normal UBA interrupt point - device on a UBA has generated an
# interrupt - r3 holds interrupt vector.  Get the service routine
# address and controller code from the UNIBUS vector area
# and service the interrupt.
#
ubanorm:
	movl	_UNIvec(r3),r1 
	extzv	$27,$4,r1,r0  		# controller code is in 4 most
					# significant bits-1 of ISR addr
	bicl2	$0x78000000,r1		# clear code
	jlbc	r1,ubanpdma		# no pseudo dma here
	jmp 	-1(r1)			# branch to pseudo dma rtn
ubanpdma:
	pushl	r0			# controller code
	calls	$1,(r1)  		# call ISR
	brw	int_ret			# go to common interrupt return

#
# Come here for zero or negative UBA interrupt vector.
# Negative vector -> UBA requires service.
#
ubasrv:
	beql	ubapass
#
# UBA service required.
# The following 'printf' calls should probably be replaced
# with calls to an error logger and/or some corrective action.
#
	bitl	$CFGFLT,UBA0+UCN_OFF  	# any SBI faults ?
	beql	UBAflt
	pushr	$0xf  			# save regs 0-3
	pushab	SBImsg
	calls	$1,_printf
	popr	$0xf
	halt
#
# No SBI fault bits set in UBA config reg - must be
# some error bits set in UBA status reg.
#
UBAflt:
	movl	UBA0+UST_OFF,r2  	# UBA status reg
	pushr	$0xf  			# save regs 0-3
	mfpr	$IPL,-(sp)
	mtpr	$HIGH,$IPL
	pushl	UBA0+UFUBAR_OFF
	pushl	UBA0+UFMER_OFF
	pushl	r2
	pushab	UBAmsg
	calls	$4,_printf
	mtpr	(sp)+,$IPL
	popr	$0xf
	movl	r2,UBA0+UST_OFF		# clear error bits
	bicl2	$0x80000000,r3  	# clear neg bit in vector
	jneq	ubanorm  		# branch if normal UBA interrupt
					# to process
	brw 	int_ret			# restore regs and return
#
# Zero interrupt vector - count 'em
#
ubapass:
	incl	_zvcnt
	brw 	int_ret
	.data
	.globl	_zvcnt
_zvcnt:	.space	4
	.text

#
# DZ pseudo dma routine:
#	r0 - controller number
#
	.align	1
	.globl	_dzdma
_dzdma:
#	bisw2	$4,*_draddr	# leading edge for dr11-c
	mull2	$8*20,r0
	movab	_dzpdma(r0),r3		# pdma structure base
					# for this controller
dzploop:
	movl	r3,r0	
	movl	(r0)+,r1		# device register address
	movzbl	1(r1),r2		# get line number
	bitb	$0x80,r2		# TRDY on?
	beql	dzprei			# no	
	bicb2	$0xf8,r2		# clear garbage bits
	mull2	$20,r2
	addl2	r2,r0			# point at line's pdma structure
	movl	(r0)+,r2		# p_mem
	cmpl	r2,(r0)+		# p_mem < p_end ?
	bgequ	dzpcall			# no, go call dzxint
	movb	(r2)+,6(r1)		# dztbuf = *p_mem++
	movl	r2,-8(r0)
	brb 	dzploop			# check for another line
dzprei:
#	bicw2	$4,*_draddr	# trailing edge for dr11-c
	popr	$0x3f
	incl	_cnt+V_PDMA
	rei

dzpcall:
	pushl	(r0)			# push tty address
	calls	$1,_dzxint		# call interrupt rtn
	brb 	dzploop			# check for another line

#
# Console receiver interrupt
#
	.align	2
Xcnrint:
	pushr	$0x3f			# save registers 0 - 5
	calls	$0,_cnrint
	brb 	int_ret			# merge

#
# Console transmit interrupt
#
	.align	2
Xcnxint:
	pushr	$0x3f			# save registers 0 - 5
	calls	$0,_cnxint
	brb 	int_ret

#
# Clock interrupt
#
	.align	2
Xclockint:
	pushr	$0x3f			# save regs 0 - 5
	pushl	4+6*4(sp)		# push psl
	pushl	4+6*4(sp)		# push pc
	calls	$2,_clock
	brb 	int_ret

#
# Common code for interrupts.
# At this point, the interrupt stack looks like:
#
#	r0	<- isp
#	...
#	r5
#	pc
#	psl
#

int_ret:
	incl	_cnt+V_INTR
#	bbssi	$0,idleflag,int_r0	# escape from idle() if old switch code
#int_r0:
	popr	$0x3f			# restore regs 0 - 5
	bitl	$PSL_CURMOD,4(sp)	# interrupt from user mode?
	beql	int_r1			# no, from kernel, just rei
	tstb	_runrun			# should we reschedule?
	beql	int_r1			# no, just rei
#
# If here, interrupt from user mode, and time to reschedule.
# To do this, we set a software level 3 interrupt to
# change to kernel mode, switch stacks, and format
# kernel stack for a `qswitch' trap to force a reschedule.
#
	mtpr	$0x18,$IPL
	mtpr	$3,$SIRR		# request level 1 software interrupt
int_r1:
	rei 				# return to interrupted process

# ==================================
# User area virtual addresses
# ==================================

	.globl	_u
	.set	_u,0x80000000 - UPAGES*NBPG

	.data
	.globl	_Sysmap
_Sysmap:
	.space	6*NBPG
UBA0map:
	.space	16*4
	.globl	_umbabeg
	.set	_umbabeg,((UBA0map-_Sysmap)/4)*512+0x80000000
UMEMmap:
	.space	16*4
MBA0map:
	.space	16*4
MBA1map:
	.space	16*4
umend:
	.globl	_umbaend
	.set	_umbaend,((umend-_Sysmap)/4)*512+0x80000000

	.globl	_Usrptmap
_Usrptmap:
	.space	8*NBPG
	.globl	_usrpt
	.set	_usrpt,((_Usrptmap-_Sysmap)/4)*NBPG+0x80000000

	.globl	_Forkmap
_Forkmap:
	.space	4*UPAGES
	.globl	_forkutl
	.set	_forkutl,((_Forkmap-_Sysmap)/4)*NBPG+0x80000000

	.globl	_Xswapmap
_Xswapmap:
	.space	4*UPAGES
	.globl	_xswaputl
	.set	_xswaputl,((_Xswapmap-_Sysmap)/4)*NBPG+0x80000000

	.globl	_Xswap2map
_Xswap2map:
	.space	4*UPAGES
	.globl	_xswap2utl
	.set	_xswap2utl,((_Xswap2map-_Sysmap)/4)*NBPG+0x80000000

	.globl	_Swapmap
_Swapmap:
	.space	4*UPAGES
	.globl	_swaputl
	.set	_swaputl,((_Swapmap-_Sysmap)/4)*NBPG+0x80000000

	.globl	_Pushmap
_Pushmap:
	.space	4*UPAGES
	.globl	_pushutl
	.set	_pushutl,((_Pushmap-_Sysmap)/4)*NBPG+0x80000000

	.globl	_Vfmap
_Vfmap:
	.space	4*UPAGES
	.globl	_vfutl
	.set	_vfutl,((_Vfmap-_Sysmap)/4)*NBPG+0x80000000

CMAP1:
	.space	4
	.set	CADDR1,((CMAP1-_Sysmap)/4)*NBPG+0x80000000
CMAP2:
	.space	4
	.set	CADDR2,((CMAP2-_Sysmap)/4)*NBPG+0x80000000

	.globl	_mcrmap
_mcrmap:
	.space	4
	.globl	_mcr
	.set	_mcr,((_mcrmap-_Sysmap)/4)*NBPG+0x80000000

	.globl	_mmap
_mmap:
	.space	4
	.globl	_vmmap
	.set	_vmmap,((_mmap-_Sysmap)/4)*NBPG+0x80000000

eSysmap:
	.set	Syssize,(eSysmap-_Sysmap)/4
	.text

# ==============================
# Trap and fault vector routines
# ==============================

#
# Reschedule trap (Software level 3 interrupt)
#
	.align	2
Xresched:
	mtpr	$0,$IPL			# lower ipl
	pushl	$0			# dummy code
	pushl	$RESCHED		# type
	bitl	$PSL_CURMOD,12(sp)
	bneq	alltraps
	addl2	$8,sp
	mtpr	$HIGH,$IPL
	rei

#
# Privileged instruction fault
#
	.align	2
Xprivinflt:
	pushl	$0			# push dummy code
	pushl	$PRIVINFLT		# push type
	brw 	alltraps		# merge

#
# Xfc instruction fault
#
	.align	2
Xxfcflt:
	pushl	$0			# push dummy code value
	pushl	$XFCFLT			# push type value
	brw 	alltraps		# merge

#
# Reserved operand fault
#
	.align	2
Xresopflt:
	pushl	$0			# push dummy code value
	pushl	$RESOPFLT		# push type value
	brw 	alltraps		# merge

#
# Reserved addressing mode fault
#
	.align	2
Xresadflt:
	pushl	$0			# push dummy code value
	pushl	$RESADFLT		# push type value
	brw 	alltraps		# merge with common code

#
# Bpt instruction fault
#
	.align	2
Xbptflt:
	pushl	$0			# push dummy code value
	pushl	$BPTFLT			# push type value
	brw 	alltraps		# merge with common code

#
# Compatibility mode fault
#
	.align	2
Xcompatflt:
	pushl	$COMPATFLT		# push type value
	brw 	alltraps		# merge with common code

#
# Trace trap
#
	.align	2
Xtracep:
	pushl	$0			# push dummy code value
	pushl	$TRCTRAP		# push type value
	brw 	alltraps		# go do it

#
# Arithmetic trap
#
	.align	2
Xarithtrap:
	pushl	$ARITHTRAP		# push type value
	brw 	alltraps		# merge with common code

#
# Protection and segmentation fault
#
	.align	2
Xprotflt:
	blbs	(sp),segflt		# check for pt length violation
	addl2	$4,sp			# pop fault param word 
	pushl	$PROTFLT
	brw 	alltraps

#
# Segmentation fault
#
segflt:
	addl2	$4,sp
	pushl	$SEGFLT
	brb 	alltraps

#
# Translation Not Valid Fault
#
	.align  2
Xtransflt:
	bbs	$1,(sp),tableflt	# check for page table fault
	addl2	$4,sp			# pop fault parameter word
	pushl	$PAGEFLT		# push type value
	brb	alltraps

#
# Page table fault
#
tableflt: 
	addl2	$4,sp			# pop fault parameter word
	pushl	$TABLEFLT		# push type value
	brb	alltraps

#
# all traps but syscalls...
#
alltraps:
	mfpr	$USP,-(sp)		# get usp
	calls	$0,_trap		# $0 so ret wont pop args
	incl	_cnt+V_TRAP
	mtpr	(sp)+,$USP		# restore usp
	addl2	$8,sp			# pop type, code
	mtpr	$HIGH,$IPL		# make sure we are not going to
					# a higher IPL
	rei

#
# CHMK trap (syscall trap)
#
# Kernel stack on entry:
#
#	code	<- ksp
#	pc
#	psl
#
#
# Stack (parameters) at calls to _trap or _syscall
#
#	usp	<- ksp
#	type
#	code
#	pc
#	psl
#

	.align	2
Xsyscall:
	pushl	$SYSCALL		# push type value
	mfpr	$USP,-(sp)		# get usp
	calls	$0,_syscall		# $0 so ret wont pop args
	incl	_cnt+V_SYSCALL
	mtpr	(sp)+,$USP		# restore usp
	addl2	$8,sp			# pop type, code
	mtpr	$HIGH,$IPL		# make sure we are not going to
					# a higher IPL
	rei

# ==============
# Initialization
# ==============
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
	mtpr	$Scbbase,$SCBB		# set SCBB
	mtpr	$_Sysmap,$SBR		# set SBR
	mtpr	$Syssize,$SLR		# set SLR
	mtpr	$_Sysmap,$P0BR		# set temp P0BR
	mtpr	$Syssize,$P0LR		# set temp P0LR
	movl	$_intstack+2048,sp	# set ISP
#
# Initialize I/O adapters.
#
	movl	$1,PHYSMBA0+4		# init & interrupt enable
	movl	$4,PHYSMBA0+4		# init & interrupt enable
	movl	$1,PHYSMBA1+4		# init & interrupt enable
	movl	$4,PHYSMBA1+4		# init & interrupt enable
	movl	$1,PHYSUBA+4		# init & interrupt enable
	movl	$0x78,PHYSUBA+4		# init & interrupt enable

	movl	Scbbase+MCKVEC,r5	# save machine check entry
	movab	startint+INTSTK,Scbbase+MCKVEC	# set new vector address
#
# Will now see how much memory there really is
# in 64kb chunks.  Save number of bytes in r7.
#
	mtpr	$HIGH-1,$IPL		# allow machine check interrupts
	clrl	r7
startlp:
	tstl	(r7)			# this chunk really there?
	acbl	$8096*1024-1,$64*1024,r7,startlp	# loop till mach check
	brb 	startint		# full load of memory

	.align	2
startint:
	mtpr	$0,$SBIFS		# clear sbi fault status
	movl	r5,Scbbase+MCKVEC	# restore machine check vector
	movl	$_intstack+2048,sp	# reset interrupt stack pointer
#
# calculate size of cmap[] based on available memory, and allocate space for it
#
	movab	_end,r5
	movl	r5,_cmap
	bbss	$31,_cmap,cm0
cm0:
	subl3	r5,r7,r1
	divl2	$(NBPG*CLSIZE)+CMSIZE,r1
	mull2	$CMSIZE,r1
	addl3	_cmap,r1,_ecmap
#
# Clear memory starting with kernel bss, and extra pages for
# proc 0 u. and proc 0 paget.
#
	movab	_edata,r6
	movab	_ecmap,r5		# clear to end of cmap[]
	addl2	$(UPAGES*NBPG)+NBPG+NBPG,r5
strtclr:
	clrq	(r6)
	acbl	r5,$8,r6,strtclr

#
# Finagle _trap and _syscall to save r0-r11 so
# that it won't be necessary to pushr/popr what
# the (already time consuming) calls is prepared to do.
# The fact that this is done is well known (e.g. in the definition
# of the stack offsets of the registers in ../h/reg.h)
# 
	bisw2	$0x0fff,_trap		# so _trap saves r0-r11
	bisw2	$0x0fff,_syscall	# so _syscall saves r0-r11

#
# Initialize system page table
#
	movab	_etext+NBPG-1,r1	# end of kernel text segment
	bbcc	$31,r1,strt1		# turn off high order bit
strt1:
	ashl	$-9,r1,r1		# last page of kernel text
	clrl	r2			# point at first kernel text page
strtlp1:
	bisl3	$PG_V|PG_KR,r2,_Sysmap[r2]	# initialize page table entry
	aoblss	r1,r2,strtlp1		# fill text entries
	addl3	_ecmap,$NBPG-1,r1	# end of cmap[]
	bbcc	$31,r1,strt2		# turn off high order bit
strt2:
	ashl	$-9,r1,r1		# last page of kernel data
strtlp2:
	bisl3	$PG_V|PG_KW,r2,_Sysmap[r2]	# fill data entries
	aoblss	r1,r2,strtlp2
#
# initialize memory controller mapping
#
	movl	$PHYSMCR/NBPG,r1
	movab	_mcrmap,r2
	bisl3	$PG_V|PG_KW,r1,(r2)
#
# Initialize I/O space page table entries
#
	movl	$PHYSUBA/NBPG,r1	# page frame number for uba
	movab	UBA0map,r2		# page table address
	movab	15(r1),r3		# last pt entry
strtlp3:
	bisl3	$PG_V|PG_KW,r1,(r2)+	# init pt entry
	aobleq	r3,r1,strtlp3
	movl	$PHYSUMEM/NBPG,r1
	movab	UMEMmap,r2		# page table address
	movab	15(r1),r3		# limit
strtlp4:
	bisl3	$PG_V|PG_KW,r1,(r2)+
	aobleq	r3,r1,strtlp4
	movl	$PHYSMBA0/NBPG,r1
	movab	MBA0map,r2
	movab	15(r1),r3
strtlp5:
	bisl3	$PG_V|PG_KW,r1,(r2)+
	aobleq	r3,r1,strtlp5
	movl	$PHYSMBA1/NBPG,r1
	movab	MBA1map,r2
	movab	15(r1),r3
strtlp6:
	bisl3	$PG_V|PG_KW,r1,(r2)+
	aobleq	r3,r1,strtlp6

	mtpr	$1,$TBIA		# invalidate all trans buffer entries
	mtpr	$1,$MAPEN		# turn on memory mapping
	jmp 	*$startmap		# put system virtual address in pc
#
# Now we move forward, virtually.
#
startmap:
	ashl	$-9,r7,_maxmem		# set maxmem = btoc(r7)
	movl	_maxmem,_physmem
	movl	_maxmem,_freemem

#
# Setup context for proc[0] == Scheduler
#
# First page: paget for proc[0]
# Next UPAGES: _u for proc[0]
# Initialize (slightly) the pcb.
#
	addl3	_ecmap,$NBPG-1,r6
	bicl2	$NBPG-1,r6		# make page boundary
#
# set up u area page table
#
	bbcc	$31,r6,strt3
strt3:
	ashl	$-9,r6,r3			# r3 = btoc(r6)
	bisl3	$PG_V|PG_KW,r3,_Usrptmap	# init first upt entry
	movab	_usrpt,r0
	mtpr	r0,$TBIS
	mtpr	r0,$P0BR
	mtpr	$0,$P0LR
	movab	NBPG(r0),r0
	movl	$0x200000-UPAGES,r1
	mtpr	r1,$P1LR
	mnegl	r1,r1
	moval	-4*UPAGES(r0)[r1],r2
	mtpr	r2,$P1BR
	movl	$UPAGES,r2
	movab	_u+NBPG*UPAGES,r1
	jbr	strt3b
strt3a:
	incl	r3
	moval	-NBPG(r1),r1
	bisl3	$PG_V|PG_URKW,r3,-(r0)
	mtpr	r1,$TBIS
strt3b:
	sobgeq	r2,strt3a

	movab	UPAGES*NBPG(r1),PCB_KSP(r1)	# init ksp
	mnegl	$1,PCB_ESP(r1)			# invalidate esp
	mnegl	$1,PCB_SSP(r1)			# invalidate ssp
	movl	r1,PCB_USP(r1)			# set user sp
	mfpr	$P0BR,PCB_P0BR(r1)
	mfpr	$P0LR,PCB_P0LR(r1)
	movb	$4,PCB_P0LR+3(r1)		# disable ast
	mfpr	$P1BR,PCB_P1BR(r1)
	mfpr	$P1LR,PCB_P1LR(r1)
	movl	$CLSIZE,PCB_SZPT(r1)		# init u.u_pcb.pcb_szpt

	movab	strt3c,PCB_PC(r1)		# initial pc
	clrl	PCB_PSL(r1)			# mode(k,k), ipl=0
	ashl	$9,r3,r3
	mtpr	r3,$PCBB			# first pcbb
#
# set regs, p0br, p0lr, p1br, p1lr
# astlvl, ksp and change to kernel mode
#
	ldpctx
	rei

#
# put signal trampoline code in u. area
#
strt3c:
	movab	_u,r0
	movc3	$12,sigcode,PCB_SIGC(r0)

	addl3	_ecmap,$NBPG-1,r0		# calculate firstaddr
	bbcc	$31,r0,strt4
strt4:
	ashl	$-9,r0,-(sp)			# convert to clicks and stack
	calls	$1,_main			# startup, fork off /etc/init.vm
#
# proc[1] == /etc/init now running here.
# execute code at location 0, in user mode.
#
	pushl	$PSL_CURMOD|PSL_PRVMOD		# psl, user mode, ipl = 0
	pushl	$0				# pc, $location 0
	rei 					# do /etc/init.vm

#
# signal trampoline code
# it is known that this code takes exactly 12 bytes
# in ../h/pcb.h and in the movc3 above
#
sigcode:
	calls	$3,1(pc)
	rei
	.word	0x7f
	callg	(ap),*12(ap)			# registers 0-6 (6==sp/compat)
	ret

# ==========
# Primitives
# ==========

_addupc:	.globl	_addupc
	.word	0x0000
	movl	8(ap),r2		# &u.u_prof
	subl3	8(r2),4(ap),r0		# corrected pc
	blss	addret
	extzv	$1,$31,r0,r0		# logical right shift
	extzv	$1,$31,12(r2),r1	# ditto for scale
	emul	r1,r0,$0,r0
	ashq	$-14,r0,r0
	tstl	r1
	bneq	addret
	incl	r0
	bicb2	$1,r0
	blss	addret
	cmpl	r0,4(r2)		# length
	bgequ	addret
	addl2	(r2),r0			# base
	probew	$3,$2,(r0)
	beql	adderr
	addw2	12(ap),(r0)
addret:
	ret
adderr:
	clrl	12(r2)
	ret

_Copyin:	.globl	_Copyin		# <<<massaged for jsb by asm.sed>>>
	movl	12(sp),r0		# copy length
	blss	ersb
	movl	4(sp),r1		# copy user address
	cmpl	$NBPG,r0		# probing one page or less ?
	bgeq	cishort			# yes
ciloop:
	prober	$3,$NBPG,(r1)		# bytes accessible ?
	beql	ersb			# no
	addl2	$NBPG,r1		# incr user address ptr
	acbl	$NBPG+1,$-NBPG,r0,ciloop	# reduce count and loop
cishort:
	prober	$3,r0,(r1)		# bytes accessible ?
	beql	ersb			# no
	movc3	12(sp),*4(sp),*8(sp)
	clrl	r0
	rsb

ersb:
	mnegl	$1,r0
	rsb

_Copyout: 	.globl	_Copyout	# <<<massaged for jsb by asm.sed >>>
	movl	12(sp),r0		# get count
	blss	ersb
	movl	8(sp),r1		# get user address
	cmpl	$NBPG,r0		# can do in one probew?
	bgeq	coshort			# yes
coloop:
	probew	$3,$NBPG,(r1)		# bytes accessible?
	beql	ersb			# no 
	addl2	$NBPG,r1		# increment user address
	acbl	$NBPG+1,$-NBPG,r0,coloop	# reduce count and loop
coshort:
	probew	$3,r0,(r1)		# bytes accessible?
	beql	ersb			# no
	movc3	12(sp),*4(sp),*8(sp)
	clrl	r0
	rsb

#
# non-local goto's
#
	.globl	_Setjmp
_Setjmp:
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	r12,(r0)+
	addl3	$4,sp,(r0)+
	movl	(sp),(r0)
	clrl	r0
	rsb

	.globl	_Longjmp
_Longjmp:
	movq	(r0)+,r6
	movq	(r0)+,r8
	movq	(r0)+,r10
	movq	(r0)+,r12
	movl	(r0)+,r1
	cmpl	r1,sp				# must be a pop
	bgequ	lj2
	pushab	lj1
	calls	$1,_panic
lj2:
	movl	r1,sp
	jmp	*(r0)				# ``rsb''

lj1:	.asciz	"longjmp"

	.globl	_whichqs
	.globl	_qs
	.globl	_cnt

	.globl	_noproc
	.comm	_noproc,4
	.globl	_runrun
	.comm	_runrun,4

#
# The following primitives use the fancy VAX instructions
# much like VMS does.  _whichqs tells which of the 32 queues _qs
# have processes in them.  Setrq puts processes into queues, Remrq
# removes them from queues.  The running process is on no queue,
# other processes are on a queue related to p->p_pri, divided by 4
# actually to shrink the 0-127 range of priorities into the 32 available
# queues.
#

#
# Setrq(p), using fancy VAX instructions.
#
# Call should be made at spl6(), and p->p_stat should be SRUN
#
	.globl	_Setrq		# <<<massaged to jsb by "asm.sed">>>
_Setrq:
	tstl	P_RLINK(r0)		## firewall: p->p_rlink must be 0
	beql	set1			##
	pushab	set3			##
	calls	$1,_panic		##
set1:
	movzbl	P_PRI(r0),r1		# put on queue which is p->p_pri / 4
	ashl	$-2,r1,r1
	movaq	_qs[r1],r2
	insque	(r0),*4(r2)		# at end of queue
	bbss	r1,_whichqs,set2	# mark queue non-empty
set2:
	rsb

set3:	.asciz	"setrq"

#
# Remrq(p), using fancy VAX instructions
#
# Call should be made at spl6().
#
	.globl	_Remrq		# <<<massaged to jsb by "asm.sed">>>
_Remrq:
	movzbl	P_PRI(r0),r1
	ashl	$-2,r1,r1
	bbsc	r1,_whichqs,rem1
	pushab	rem3			# it wasn't recorded to be on its q
	calls	$1,_panic
rem1:
	remque	(r0),r2
	beql	rem2
	bbss	r1,_whichqs,rem2
rem2:
	clrl	P_RLINK(r0)		## for firewall checking
	rsb

rem3:	.asciz	"remrq"

sw0:	.asciz	"swtch"
#
# Swtch(), using fancy VAX instructions
#
	.globl	_Swtch
_Swtch:				# <<<massaged to jsb by "asm.sed">>>
	movl	$1,_noproc
	clrl	_runrun
sw1:	ffs	$0,$32,_whichqs,r0	# look for non-empty queue
	bneq	sw1a
	mtpr	$0,$IPL			# must allow interrupts here
	brw	sw1			# this is an idle loop!
sw1a:	mtpr	$0x18,$IPL		# lock out all so _whichqs==_qs
	bbcc	r0,_whichqs,sw1		# proc moved via lbolt interrupt
	movaq	_qs[r0],r1
	remque	*(r1),r2		# r2 = p = highest pri process
	bvc	sw2			# make sure something was there
sw1b:	pushab	sw0
	calls	$1,_panic
sw2:	beql	sw3
	insv	$1,r0,$1,_whichqs	# still more procs in this queue
sw3:
	clrl	_noproc
	tstl	P_WCHAN(r2)		## firewalls
	bneq	sw1b			##
	movzbl	P_STAT(r2),r3		##
	cmpl	$SRUN,r3		##
	bneq	sw1b			##
	clrl	P_RLINK(r2)		##
	ashl	$PGSHIFT,*P_ADDR(r2),r0	# r0 = pcbb(p)
#	mfpr	$PCBB,r1		# resume of current proc is easy
#	cmpl	r0,r1
#	beql	res0
	incl	_cnt+V_SWTCH
# fall into...

#
# Resume(pf)
#
	.globl	_Resume		# <<<massaged to jsb by "asm.sed">>>
_Resume:
	mtpr	$0x18,$IPL			# no interrupts, please
	movl	CMAP2,_u+PCB_CMAP2	# yech
	svpctx
	mtpr	r0,$PCBB
	ldpctx
	movl	_u+PCB_CMAP2,CMAP2	# yech
res0:
	tstl	_u+PCB_SSWAP
	beql	res1
	movl	_u+PCB_SSWAP,r0
	clrl	_u+PCB_SSWAP
	movab	_Longjmp,(sp)
	movl	$PSL_PRVMOD,4(sp)		# ``cheating'' (jfr)
res1:
	rei

#
# {fu,su},{byte,word}, all massaged by asm.sed to jsb's
#
	.globl	_Fuword
_Fuword:
	prober	$3,$4,(r0)
	beql	fserr
	movl	(r0),r0
	rsb
fserr:
	mnegl	$1,r0
	rsb

	.globl	_Fubyte
_Fubyte:
	prober	$3,$1,(r0)
	beql	fserr
	movzbl	(r0),r0
	rsb

	.globl	_Suword
_Suword:
	probew	$3,$4,(r0)
	beql	fserr
	movl	r1,(r0)
	clrl	r0
	rsb

	.globl	_Subyte
_Subyte:
	probew	$3,$1,(r0)
	beql	fserr
	movb	r1,(r0)
	clrl	r0
	rsb

#
# Copy 1 relocation unit (NBPG bytes)
# from user virtual address to physical address
#
_copyseg: 	.globl	_copyseg
	.word	0x0000
	mfpr	$IPL,r0		# get current pri level
	mtpr	$HIGH,$IPL	# turn off interrupts
	bisl3	$PG_V|PG_KW,8(ap),CMAP2
	mtpr	$CADDR2,$TBIS	# invalidate entry for copy 
	movc3	$NBPG,*4(ap),CADDR2
	bicl3	$PG_V|PG_M|PG_KW,CMAP2,r1
	cmpl	r1,8(ap)
	beql	okcseg
badcseg:
	halt
	jmp	badcseg
okcseg:
	mtpr	r0,$IPL		# restore pri level
	ret

#
# zero out physical memory
# specified in relocation units (NBPG bytes)
#
_clearseg: 	.globl	_clearseg
	.word	0x0000
	mfpr	$IPL,r0		# get current pri level
	mtpr	$HIGH,$IPL	# extreme pri level
	bisl3	$PG_V|PG_KW,4(ap),CMAP1
	mtpr	$CADDR1,$TBIS
	movc5	$0,(sp),$0,$NBPG,CADDR1
	mtpr	r0,$IPL		# restore pri level
	ret

#
# Check address.
# Given virtual address, byte count, and rw flag
# returns 0 on no access.
#
_useracc:	.globl	_useracc
	.word	0x0000
	movl	4(ap),r0		# get va
	movl	8(ap),r1		# count
	tstl	12(ap)			# test for read access ?
	bneq	userar			# yes
	cmpl	$NBPG,r1			# can we do it in one probe ?
	bgeq	uaw2			# yes
uaw1:
	probew	$3,$NBPG,(r0)
	beql	uaerr			# no access
	addl2	$NBPG,r0
	acbl	$NBPG+1,$-NBPG,r1,uaw1
uaw2:
	probew	$3,r1,(r0)
	beql	uaerr
	movl	$1,r0
	ret

userar:
	cmpl	$NBPG,r1
	bgeq	uar2
uar1:
	prober	$3,$NBPG,(r0)
	beql	uaerr
	addl2	$NBPG,r0
	acbl	$NBPG+1,$-NBPG,r1,uar1
uar2:
	prober	$3,r1,(r0)
	beql	uaerr
	movl	$1,r0
	ret
uaerr:
	clrl	r0
	ret

#
# kernacc - check for kernel access privileges
#
# We can't use the probe instruction directly because
# it ors together current and previous mode.
#
	.globl	_kernacc
_kernacc:
	.word	0x0000
	movl	4(ap),r0	# virtual address
	bbcc	$31,r0,kacc1
	mfpr	$SBR,r2		# address and length of page table (system)
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
	bbs	$31,4(ap),kacc6
	bbc	$30,4(ap),kacc6
	cmpl	r0,r3		# user stack
	blss	kacerr		# address too low
	brb	kacc4
kacc6:
	cmpl	r1,r3		# compare last page to P0LR or SLR
	bgeq	kacerr		# address too high
kacc4:	
	movl	(r2)[r0],r3
	bbc	$31,4(ap),kacc4a
	bbc	$31,r3,kacerr	# valid bit is off
kacc4a:
	cmpzv	$27,$4,r3,$1	# check protection code
	bleq	kacerr		# no access allowed
	tstb	12(ap)
	bneq	kacc5		# only check read access
	cmpzv	$27,$2,r3,$3	# check low 2 bits of prot code
	beql	kacerr		# no write access
kacc5:
	aobleq	r1,r0,kacc4	# next page
	movl	$1,r0		# no errors
	ret
kacerr:
	clrl	r0		# error
	ret

#
# unsigned int divide:
#	(int) i = udiv( (int)dvdnd , (int) divis)
#
#  unsigned int remainder:
#	(int) j = urem( (int)dvdnd , (int) divis)
#
	.text
	.align	1
	.globl	_udiv
_udiv :
	.word	0  			# no reg save
	movl	4(ap),r0  		# dividend
	clrl	r1
	ediv	8(ap),r0,r0,r1  	# quotient in r0
	ret

#	.globl	_urem
#	.align 	1
#_urem:
#	.word	0
#	movl	4(ap),r0
#	clrl	r1
#	ediv	8(ap),r0,r1,r0  	#  remainder in r0
#	ret

# ==============
# Error messages
# ==============

	.data
SBImsg: .asciz	"SBI fault\n"
UBAmsg: .asciz	"UBA error UBASR %X, FMER %X, FUBAR %X\n"
straym: .asciz	"Stray Interrupt\n"

#
# Junk.
#

# these should be memall'ed
	.data
	.globl _buffers
	.align  PGSHIFT
_buffers:	.space NBUF*BSIZE

#
# This is needed when running old-style switch code.
# Be sure to enable setting of idleflag in interrupt code above also.
#
#_idle:	.globl	_idle
#	.word	0x0000
#	mtpr	$0,$IPL			# enable interrupts
#waitloc:
#	blbc	idleflag,waitloc	# loop until interrupt
#ewaitloc:
#	bbcci	$0,idleflag,idle1	# clear idle escape flag
#idle1:
#	ret
#	.data
#	.globl	_waitloc
#	.globl	_ewaitloc
#	.align	2
#_waitloc:	.long	waitloc
#_ewaitloc:	.long	ewaitloc
#idleflag:	.long	0
#	.text

