#
# Machine Language Assist for UC Berkeley Virtual Vax/Unix
#
#	locore.s	2.2	1/15/80
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
	.long	Xrandom + HALT		# unused
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
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xsyscall		# chmk
	.long	Xchme+HALT		# chme
	.long	Xchms+HALT		# chms
	.long	Xchmu+HALT		# chmu
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# software level 1
	.long	Xrandom + HALT		# software level 2 (asts)
	.long	Xresched		# reschedule nudge
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
ubabase:
	.long	Xclockint		# clock
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xcnrint + INTSTK	# console receiver 
	.long	Xcnxint + INTSTK	# console transmitter

#
# I/O vectors
#

# IPL 14
	.long	X14stray + INTSTK	# stray!
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xua0int + INTSTK	# UBA 0 br4
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused

# IPL 15
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xua0int + INTSTK	# UBA 0 br5
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xmba0int + INTSTK	# mass bus adapter 0
	.long	Xmba1int + INTSTK	# mass bus adapter 1
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused

# IPL 16
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xua0int + INTSTK	# UBA 0 br6
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused

# IPL 17
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused
	.long	Xrandom + HALT		# unused

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

X14stray:
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
# Zero interrupt vector - print message & continue
#
# WE GET SO MANY OF THESE THAT WE ONLY COUNT THEM
#
ubapass:
#	pushr	$0xf
#	pushab	ZERmsg
#	calls	$1,_printf
#	popr	$0xf
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
	bbssi	$0,idleflag,int_r0	# set idle escape flag (no wait instr)
int_r0:
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
	mtpr	$3,$SIRR		# request level 1 software interrupt
int_r1:
	rei 				# return to interrupted process

# ==================================
# User area virtual addresses
# ==================================

	.set	kernsize,512	 	# number of pte's allocated to kernel
	.globl	_u
	.set	_u,0x80000000 + kernsize*NBPG
	.globl	_utilutl
	.set	_utilutl,_u+16*NBPG
	.globl	_umbabeg
	.set	_umbabeg,_utilutl+16*NBPG
	.globl	_umbaend
	.set	_umbaend,_umbabeg+6*16*NBPG
	.globl	_swaputl
	.set	_swaputl,_u+128*NBPG
	.globl	_forkutl
	.set	_forkutl,_swaputl+UPAGES*NBPG
	.globl	_xswaputl
	.set	_xswaputl,_forkutl+UPAGES*NBPG
	.globl	_xswap2utl
	.set	_xswap2utl,_xswaputl+UPAGES*NBPG
	.globl	_pushutl
	.set	_pushutl,_xswap2utl+UPAGES*NBPG
	.globl	_vfutl
	.set	_vfutl,_pushutl+UPAGES*NBPG
	.globl 	_usrpt
	.set	_usrpt,_vfutl+UPAGES*NBPG
	.set	u_ptoffset,kernsize*4	# offset in _Sysmap of pte's of _u
	.set	CMAP1,u_ptoffset+16*4	# offset in _sysmap of 1st copyseg entry
	.set	CMAP2,CMAP1+4		# ... of 2nd copyseg entry
	.set	CADDR1,_u+16*NBPG	# virtual address of 1st copy segment
	.set	CADDR2,CADDR1+NBPG	# ... of second copy segment


#
# offsets in Sysmap
#
	.set	uba_offset,u_ptoffset+32*4	# ... of uba entries
	.set	umem_offset,uba_offset+16*4	# ... of unibus device registers
	.set	mba0_offset,umem_offset+16*4	# ... of massbus 0
	.set	mba1_offset,mba0_offset+16*4	# ... of massbus 1
	.set	mba2_offset,mba1_offset+16*4	# ... of massbus 2
	.set	mba3_offset,mba2_offset+16*4	# ... of massbus 3

# ==========================
# Sysmap - system page table
# ==========================
#
#	structure:
#		4 pages of page table entries
#			reserved for kernel text and data.
#		1 page to map u area and other utilities
#			used in mapping the u area (16 entries),
#			utility entries (16 entries),
#			unibus adapter (16 entries),
#			unibus device memory (16 entries),
#			massbus adapter 0 (16 entries),
#			massbus adapter 1 (16 entries),
#			massbus adapter 2 (16 entries),
#			massbus adapter 3 (16 entries).
#			used for phys & swap I/O
#		6*4 entries for various other utilities
#		4 pages to map user page tables of resident processes
#

	.data
	.align	2
	.globl	_Sysmap
	.globl	_Umap
	.globl	_Tempmap
	.globl	_Swapmap
	.globl	_Forkmap
	.globl	_Xswapmap
	.globl	_Xswap2map
	.globl	_Pushmap
	.globl	_Vfmap
	.globl	_Usrptmap
_Sysmap:
	.space	4*128*4			# four pages of pte's for kernel
_Umap:
	.space	16*4			# u-area (could be just 6*4)
_Tempmap:
	.space	16*4*7			# utility area & others
_Swapmap:
	.space	UPAGES*4		# swap utility area
_Forkmap:
	.space	UPAGES*4		# fork area
_Xswapmap:
	.space	UPAGES*4		# xswap area
_Xswap2map:
	.space	UPAGES*4		# xswap2 area
_Pushmap:
	.space	UPAGES*4		# pageout daemon utility
_Vfmap:
	.space	UPAGES*4		# vfork utility
_Usrptmap:
	.space	8*128*4			# user page table area

	.set	Syssize,13*128+6*UPAGES	# number pt entries in sys page table
	.globl	_mmap
	.set	_mmap,_Sysmap+CMAP2+4
	.globl	_vmmap
	.set	_vmmap,CADDR2+NBPG
	.globl	_mcrmap
	.set	_mcrmap,_Sysmap+CMAP2+8
	.globl	_mcr
	.set	_mcr,CADDR2+(2*NBPG)
	.set	mcr_offset,CMAP2+8
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
	brw 	alltraps		# merge

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
	bbs	$31,4(sp),alltraps 	# is faulting address in kernel
	brb	usrtraps		# page faults in user space
					# even in kernel mode are ok
#
# Page table fault
#
tableflt: 
	addl2	$4,sp			# pop fault parameter word
	pushl	$TABLEFLT		# push type value
	cmpl	_end,4(sp)		# cannot tolerate sysmap faults
	bgequ	alltraps
	brb	usrtraps		# a table fault on a user page
					# table in kernel mode is ok

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
# Stack (parameters) at calls to _trap:
#
#	ap	<- ksp
#	r0
#	...
#	r13
#	usp
#	type
#	code
#	pc
#	psl
#
	.align	2
Xsyscall:
	pushl	$SYSCALL		# push type value
alltraps:
	bitl	$PSL_CURMOD,12(sp)	# from user mode?
	beql	sysc1			# no
#
# Prepare arguments to _trap.  Note that type has already been pushed.
#
usrtraps:
	mfpr	$USP,-(sp)		# get usp
	pushr	$0x3fff			# registers 0 - 13
	pushl	ap			# ptr to syscall parameters
#
# Call _trap with wrong number of arguments
# so args not popped by ret.
#
	calls	$1,_trap
#
# Restore
#
	popr	$0x3fff			# restore regs 0 - 13
	mtpr	(sp)+,$USP		# restore usp
ignresch:
	addl2	$8,sp			# pop type, code
	mtpr	$HIGH,$IPL		# make sure we are not going to
					# a higher IPL
	rei

#
# ``Trap from kernel mode'' -- one special case here is to ignore
#	RESCHED traps which happen in the kernel... these happen only
#	when a clock interrupt occured before the RESCHED AST could
#	be delivered, and the clock interrupt lowered the ipl, usually
#	because the addupc() in clock page faulted because the running
#	process was profiling.  In this case we are context switching
#	away from the current process anyways, so ignoring the RESCHED
#	trap is the right thing to do.
#
sysc1:
	cmpl	(sp),$RESCHED		# spurious RESCHED traps from kernel
	beql	ignresch		# ... can be ignored
	movab	emsg1,eptr		# set message pointer
	brb 	err_print		# print message and halt

#
# err_print
#	print message on console and die
#	message pointed to by eptr, terminated by zero byte.
#
err_print:
	mtpr	$HIGH,$IPL		# disable all interrupts
	mtpr	$0,$TXCS		# dis. interrupts on cons. xmtr.
eloop1:
	mfpr	$TXCS,ewk1		# get transmitter status
	bbc 	$TXCS_BRDY,ewk1,eloop1	# loop if not ready to transmit
	tstb	*eptr			# end of message?
	beql	eout			# yes, out of loop
	movzbl	*eptr,ewk1		# get byte of message
	incl	eptr			# bump pointer
	mtpr	ewk1,$TXDB		# give byte to transmitter
	brb 	eloop1			# loop
eout:
	halt

	.data
eptr:	.long	0
ewk1:	.long	0
	.text

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
# Clear memory starting with kernel bss
#
	movab	_edata,r6
	movab	_ecmap,r5		# clear to end of cmap[]
strtclr:
	clrq	(r6)
	acbl	r5,$8,r6,strtclr

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
	.set	PHYSMCR,0x20002000
	movl	$PHYSMCR/NBPG,r1
	movab	_Sysmap+mcr_offset,r2
	bisl3	$PG_V|PG_KW,r1,(r2)
#
# Initialize I/O space page table entries
#
	movl	$PHYSUBA/NBPG,r1	# page frame number for uba
	movab	_Sysmap+uba_offset,r2	# page table address
	movab	15(r1),r3		# last pt entry
strtlp3:
	bisl3	$PG_V|PG_KW,r1,(r2)+	# init pt entry
	aobleq	r3,r1,strtlp3
	movl	$PHYSUMEM/NBPG,r1
	movab	_Sysmap+umem_offset,r2	# page table address
	movab	15(r1),r3		# limit
strtlp4:
	bisl3	$PG_V|PG_KW,r1,(r2)+
	aobleq	r3,r1,strtlp4
	movl	$PHYSMBA0/NBPG,r1
	movab	_Sysmap+mba0_offset,r2
	movab	15(r1),r3
strtlp5:
	bisl3	$PG_V|PG_KW,r1,(r2)+
	aobleq	r3,r1,strtlp5
	movl	$PHYSMBA1/NBPG,r1
	movab	_Sysmap+mba1_offset,r2
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
# Address first page past _ecmap.
# This will be u area for proc[0].
# Initialize u area page table entries.
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
	bisl3	$PG_V|PG_KW,r3,_Sysmap+u_ptoffset	# init first upt entry
	movab	_u,r1				# point at _u area
	mtpr	r1,$TBIS
	movab	UPAGES*NBPG(r1),PCB_KSP(r1)	# init ksp
	mnegl	$1,PCB_ESP(r1)			# invalidate esp
	mnegl	$1,PCB_SSP(r1)			# invalidate ssp
	movl	$0x80000000,PCB_USP(r1)		# set user sp
# The shape of the following computation is historical...
# P0BR and P1BR should CLSIZE*NBPG bytes apart,
# otherwise their values are irrelevant.
	movab	_u+UPAGES*NBPG,PCB_P0BR(r1)	# p0 page table pointer
	clrl	PCB_P0LR(r1)			# size zero page table
	movb	$4,PCB_P0LR+3(r1)		# disable ast
	movab	_u+(UPAGES+CLSIZE)*NBPG-0x800000,PCB_P1BR(r1)	# p1 pt pointer
	movl	$0x200000,PCB_P1LR(r1)		# invalid p1 pt length
	movl	$CLSIZE,PCB_SZPT(r1)		# init u.u_pcb.pcb_szpt
	movl	$1,r2
strt5:
	incl	r3
	bisl3	$PG_V|PG_KW,r3,_Sysmap+u_ptoffset[r2]
	addl2	$NBPG,r1
	mtpr	r1,$TBIS
	aoblss	$UPAGES,r2,strt5

	mtpr	r6,$PCBB			# first pcb
#
# set regs, p0br, p0lr, p1br, p1lr, 
# astlvl, ksp, and change to kernel mode
#
	ldpctx		
	addl2	$8,sp				# pop dummy pc, psl
	mtpr	$0,$IPL				# enable interrupts
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

_fubyte:	.globl	_fubyte
_fuibyte:	.globl	_fuibyte
	.word	0x0000
	prober	$3,$1,*4(ap)		# byte accessible ?
	beql	eret			# no
	movzbl	*4(ap),r0
	ret

_subyte:	.globl	_subyte
_suibyte:	.globl	_suibyte
	.word	0x0000
	probew	$3,$1,*4(ap)		# byte accessible ?
	beql	eret			# no
	movb	8(ap),*4(ap)
	clrl	r0
	ret

_fuword:	.globl	_fuword
_fuiword:	.globl	_fuiword
	.word	0x0000
	prober	$3,$4,*4(ap)
	beql	eret
	movl	*4(ap),r0
	ret

_suword:	.globl	_suword
_suiword:	.globl	_suiword
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
	.word	0x0000
	movl	12(ap),r0		# copy length
	blss	eret
	movl	4(ap),r1		# copy user address
	cmpl	$NBPG,r0		# probing one page or less ?
	bgeq	cishort			# yes
ciloop:
	prober	$3,$NBPG,(r1)		# bytes accessible ?
	beql	eret			# no
	addl2	$NBPG,r1		# incr user address ptr
	acbl	$NBPG+1,$-NBPG,r0,ciloop	# reduce count and loop
cishort:
	prober	$3,r0,(r1)		# bytes accessible ?
	beql	eret			# no
	movc3	12(ap),*4(ap),*8(ap)
	clrl	r0
	ret

_copyout: 	.globl	_copyout
	.word	0x0000
	movl	12(ap),r0		# get count
	blss	eret
	movl	8(ap),r1		# get user address
	cmpl	$NBPG,r0		# can do in one probew?
	bgeq	coshort			# yes
coloop:
	probew	$3,$NBPG,(r1)		# bytes accessible?
	beql	eret			# no 
	addl2	$NBPG,r1		# increment user address
	acbl	$NBPG+1,$-NBPG,r0,coloop	# reduce count and loop
coshort:
	probew	$3,r0,(r1)		# bytes accessible?
	beql	eret			# no
	movc3	12(ap),*4(ap),*8(ap)
	clrl	r0
	ret

_idle:	.globl	_idle
	.word	0x0000
	mtpr	$0,$IPL			# enable interrupts
waitloc:
	blbc	idleflag,waitloc	# loop until interrupt
ewaitloc:
	bbcci	$0,idleflag,idle1	# clear idle escape flag
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

#
# save(save_area)
#
# Save reg's and ret loc into save area - return 0.
# The contents of CMAP2 will be saved in the soft pcb extension.
#
	.globl	_save
_save:
	.word	0x0
	mtpr	$HIGH,$IPL
	movl	_Sysmap+CMAP2,_u+PCB_CMAP2	# save copyseg mapping
	movl	4(ap),r0  		# save area addr
	movab	3*4(ap),sp  		# restore stack to val before call
	movl	8(fp),ap  		# restore ap	"	"	"
	movl	16(fp),r1  		# restore pc	"	"	"
	movl	12(fp),fp  		# restore fp	"	"	"
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	ap,(r0)+  		# ap & fp
	movl	sp,(r0)+
	movl	r1,(r0)+  		# ret loc of call to 'save'
	movpsl	-(sp)
	pushl	r1
	svpctx				# save reg's -> PCB
	movpsl	-(sp)  			# set up for return
	bicl2	$PSL_IS|PSL_IPL,(sp)	# undo SVPCTX
	pushl	r1  			# ret loc
	clrl	r0  			# return val
	rei

#
# resume(proc_addr, save_addr)
#
# Switch to another process's '_u' area - return val 1
# restores CMAP2 contents from from software pcb
#
	.globl	_resume
_resume :	
	.word	0x0
	mtpr	$HIGH,$IPL  		# inhibit interrupts
	movl	8(ap),retloc
# map u-area
	clrl	r0
	movab	_u,r1
	movl	4(ap),r2
res1:	cvtwl	(r2)[r0],r3		# get u click number from proc
	bisl3	$PG_V|PG_KW,r3,_Umap[r0]
	mtpr	r1,$TBIS
	addl2	$NBPG,r1
	aoblss	$UPAGES,r0,res1

# restore CMAP2 contents for copyseg
	movl	_u+PCB_CMAP2,_Sysmap+CMAP2
	mtpr	$CADDR2,$TBIS

	movl	_u,sp			# KSP from u-area
	ashl	$9,_Umap,r5		# pcb address
	mtpr	r5,$PCBB
	ldpctx
	addl2	$8,sp			# clear ps,pc from stack

	movl	retloc,r1  		# 'ssav' or 'qsav' addr
	movq	(r1)+,r6
	movq	(r1)+,r8
	movq	(r1)+,r10
	movq	(r1)+,ap
	movl	(r1)+,sp
	movl	$1,r0  			# return val
	mtpr	$0,$IPL
	jmp	*(r1)+  		# return to caller at 'save' address

	.data
	.align	2
retloc:	.space	1*4
	.text

#
# Disable interrupts
#
_spl1:	.globl	_spl1
	.word	0x0000
	mfpr	$IPL,r0			# get IPL value
	mtpr	$2,$IPL			# disable RESCHED & AST interrupts
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
	mtpr	$0x18,$IPL		# disable bus level 7 and clock ints
	ret

#
# enable interrupts
#
_spl0:	.globl	_spl0
	.word	0x0000
	mfpr	$IPL,r0
	mtpr	$0,$IPL
	ret

#
# restore interrupt state
#
_splx:	.globl	_splx
	.word	0x0000
	mfpr	$IPL,r0
	mtpr	4(ap),$IPL
	ret

#
# Copy 1 relocation unit (NBPG bytes)
# from user virtual address to physical address
#
_copyseg: 	.globl	_copyseg
	.word	0x0000
	mfpr	$IPL,r0		# get current pri level
	mtpr	$HIGH,$IPL	# turn off interrupts
	bisl3	$PG_V|PG_KW,8(ap),_Sysmap+CMAP2
	mtpr	$CADDR2,$TBIS	# invalidate entry for copy 
	movc3	$NBPG,*4(ap),CADDR2
	bicl3	$PG_V|PG_M|PG_KW,_Sysmap+CMAP2,r1
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
	bisl3	$PG_V|PG_KW,4(ap),_Sysmap+CMAP1
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
emsg1:	.asciz	"\nTrap from kernel mode\n\r"
SBImsg: .asciz	"SBI fault\n"
UBAmsg: .asciz	"UBA error UBASR %X, FMER %X, FUBAR %X\n"
ZERmsg: .asciz	"Zero vector\n"
straym: .asciz	"Stray interrupt\n"

# these should be memall'ed
	.data
	.globl _buffers
	.align  PGSHIFT
_buffers:	.space NBUF*BSIZE
