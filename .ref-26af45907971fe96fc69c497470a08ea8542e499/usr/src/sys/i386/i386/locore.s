/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)locore.s	7.10 (Berkeley) %G%
 */

#include "assym.s"
#include <machine/psl.h>
#include <machine/pte.h>

#include <sys/errno.h>

#include <machine/trap.h>

#include "npx.h"

/*
 * Note: This version greatly munged to avoid various assembler errors
 * that may be fixed in newer versions of gas. Perhaps newer versions
 * will have more pleasant appearance.
 */

	.set	IDXSHIFT,10
	.set	SYSTEM,0xFE000000	# virtual address of system start
	/*note: gas copys sign bit (e.g. arithmetic >>), can't do SYSTEM>>22! */
	.set	SYSPDROFF,0x3F8		# Page dir index of System Base

/* IBM "compatible" nop - sensitive macro on "fast" 386 machines */
#define	NOP	;

/*
 * PTmap is recursive pagemap at top of virtual address space.
 * Within PTmap, the page directory can be found (third indirection).
 */
	.set	PDRPDROFF,0x3F7		# Page dir index of Page dir
	.globl	_PTmap, _PTD, _PTDpde
	.set	_PTmap,0xFDC00000
	.set	_PTD,0xFDFF7000
	.set	_PTDpde,0xFDFF7000+4*PDRPDROFF

/*
 * APTmap, APTD is the alternate recursive pagemap.
 * It's used when modifying another process's page tables.
 */
	.set	APDRPDROFF,0x3FE		# Page dir index of Page dir
	.globl	_APTmap, _APTD, _APTDpde
	.set	_APTmap,0xFF800000
	.set	_APTD,0xFFBFE000
	.set	_APTDpde,0xFDFF7000+4*APDRPDROFF

/*
 * Access to each processes kernel stack is via a region of
 * per-process address space (at the beginning), immediatly above
 * the user process stack.
 */
	.set	_kstack, USRSTACK
	.globl	_kstack
	.set	PPDROFF,0x3F6
	.set	PPTEOFF,0x400-UPAGES	# 0x3FE

#define	ENTRY(name) \
	.globl _/**/name; _/**/name:
#define	ALTENTRY(name) \
	.globl _/**/name; _/**/name:

/*
 * Initialization
 */
	.data
	.globl	_cpu,_cold,_boothowto,_bootdev,_cyloffset,_atdevbase,_atdevphys
_cpu:	.long	0		# are we 386, 386sx, or 486
_cold:	.long	1		# cold till we are not
_atdevbase:	.long	0	# location of start of iomem in virtual
_atdevphys:	.long	0	# location of device mapping ptes (phys)

	.globl	_IdlePTD, _KPTphys
_IdlePTD:	.long	0
_KPTphys:	.long	0

pcb:
	.space 8192
tmpstk:
pcb2:
	.space 8192
tmpstk2:
	.text
	.globl	start
 #start:
	.set start,0
	movw	$0x1234,%ax
	movw	%ax,0x472	# warm boot
	jmp	1f
	.space	0x500		# skip over warm boot shit

	/* enable a20! yecchh!! - move this to bootstrap? */
1:	inb	$0x64,%al
	andb	$2,%al
	jnz	1b
	movb	$0xd1,%al
	NOP
	outb	%al,$0x64
	NOP
1:	inb	$0x64,%al
	andb	$2,%al
	jnz	1b
	movb	$0xdf,%al
	NOP
	outb	%al,$0x60

	/*
	 * pass parameters on stack (howto, bootdev, unit, cyloffset)
	 * note: 0(%esp) is return address of boot
	 * ( if we want to hold onto /boot, it's physical %esp up to _end)
	 */

 1:	movl	4(%esp),%eax
	movl	%eax,_boothowto-SYSTEM
	movl	8(%esp),%eax
	movl	%eax,_bootdev-SYSTEM
	movl	12(%esp),%eax
	movl	%eax, _cyloffset-SYSTEM

	/* count up memory */

	xorl	%edx,%edx		# start with base memory at 0x0
	#movl	$ 0xA0000/NBPG,%ecx	# look every 4K up to 640K
	movl	$ 0xA0,%ecx		# look every 4K up to 640K
1:	movl	0(%edx),%ebx		# save location to check
	movl	$0xa55a5aa5,0(%edx)	# write test pattern

	inb	$0x84,%al		# flush write buffer
	/* flush stupid cache here! (with bcopy (0,0,512*1024) ) */

	cmpl	$0xa55a5aa5,0(%edx)	# does not check yet for rollover
	jne	2f
	movl	%ebx,0(%edx)		# restore memory
	addl	$ NBPG,%edx
	loop	1b

	movl	$0x100000,%edx		# next, talley remaining memory
	#movl	$((0xFFF000-0x100000)/NBPG),%ecx
	movl	$(0xFFF-0x100),%ecx
1:	movl	0(%edx),%ebx		# save location to check
	movl	$0xa55a5aa5,0(%edx)	# write test pattern
	cmpl	$0xa55a5aa5,0(%edx)	# does not check yet for rollover
	jne	2f
	movl	%ebx,0(%edx)		# restore memory
	addl	$ NBPG,%edx
	loop	1b
2:	shrl	$12,%edx
	movl	%edx,_Maxmem-SYSTEM

/* find end of kernel image */
	movl	$_end-SYSTEM,%ecx
	addl	$ NBPG-1,%ecx
	andl	$~(NBPG-1),%ecx
	movl	%ecx,%esi

/* clear bss and memory for bootstrap pagetables. */
	movl	$_edata-SYSTEM,%edi
	subl	%edi,%ecx
	addl	$(UPAGES+5)*NBPG,%ecx
/*
 * Virtual address space of kernel:
 *
 *	text | data | bss | page dir | proc0 kernel stack | usr stk map | Sysmap
 *			     0               1       2       3             4
 */
	xorl	%eax,%eax	# pattern
	cld
	rep
	stosb

	movl	%esi,_IdlePTD-SYSTEM /*physical address of Idle Address space */
	movl	$ tmpstk-SYSTEM,%esp	# bootstrap stack end location

#define	fillkpt		\
1:	movl	%eax,0(%ebx)	; \
	addl	$ NBPG,%eax	; /* increment physical address */ \
	addl	$4,%ebx		; /* next pte */ \
	loop	1b		;

/*
 * Map Kernel
 * N.B. don't bother with making kernel text RO, as 386
 * ignores R/W AND U/S bits on kernel access (only v works) !
 *
 * First step - build page tables
 */
	movl	%esi,%ecx		# this much memory,
	shrl	$ PGSHIFT,%ecx		# for this many pte s
	addl	$ UPAGES+4,%ecx		# including our early context
	movl	$ PG_V,%eax		#  having these bits set,
	lea	(4*NBPG)(%esi),%ebx	#   physical address of KPT in proc 0,
	movl	%ebx,_KPTphys-SYSTEM	#    in the kernel page table,
	fillkpt

/* map I/O memory map */

	movl	$0x100-0xa0,%ecx	# for this many pte s,
	movl	$(0xa0000|PG_V),%eax	#  having these bits set, (perhaps URW?)
	movl	%ebx,_atdevphys-SYSTEM	#   remember phys addr of ptes
	fillkpt

 /* map proc 0's kernel stack into user page table page */

	movl	$ UPAGES,%ecx		# for this many pte s,
	lea	(1*NBPG)(%esi),%eax	# physical address in proc 0
	lea	(SYSTEM)(%eax),%edx
	movl	%edx,_proc0paddr-SYSTEM  # remember VA for 0th process init
	orl	$ PG_V|PG_URKW,%eax	#  having these bits set,
	lea	(3*NBPG)(%esi),%ebx	# physical address of stack pt in proc 0
	addl	$(PPTEOFF*4),%ebx
	fillkpt

/*
 * Construct a page table directory
 * (of page directory elements - pde's)
 */
	/* install a pde for temporary double map of bottom of VA */
	lea	(4*NBPG)(%esi),%eax	# physical address of kernel page table
	orl	$ PG_V,%eax		# pde entry is valid
	movl	%eax,(%esi)		# which is where temp maps!

	/* kernel pde's */
	movl	$ 3,%ecx		# for this many pde s,
	lea	(SYSPDROFF*4)(%esi), %ebx	# offset of pde for kernel
	fillkpt

	/* install a pde recursively mapping page directory as a page table! */
	movl	%esi,%eax		# phys address of ptd in proc 0
	orl	$ PG_V,%eax		# pde entry is valid
	movl	%eax, PDRPDROFF*4(%esi)	# which is where PTmap maps!

	/* install a pde to map kernel stack for proc 0 */
	lea	(3*NBPG)(%esi),%eax	# physical address of pt in proc 0
	orl	$ PG_V,%eax		# pde entry is valid
	movl	%eax,PPDROFF*4(%esi)	# which is where kernel stack maps!

	/* load base of page directory, and enable mapping */
	movl	%esi,%eax		# phys address of ptd in proc 0
 	orl	$ I386_CR3PAT,%eax
	movl	%eax,%cr3		# load ptd addr into mmu
	movl	%cr0,%eax		# get control word
	orl	$0x80000001,%eax	# and let s page!
	movl	%eax,%cr0		# NOW!

	pushl	$begin				# jump to high mem!
	ret

begin: /* now running relocated at SYSTEM where the system is linked to run */

	.globl _Crtat
	movl	_Crtat,%eax
	subl	$0xfe0a0000,%eax
	movl	_atdevphys,%edx	# get pte PA
	subl	_KPTphys,%edx	# remove base of ptes, now have phys offset
	shll	$ PGSHIFT-2,%edx  # corresponding to virt offset
	addl	$ SYSTEM,%edx	# add virtual base
	movl	%edx, _atdevbase
	addl	%eax,%edx
	movl	%edx,_Crtat

	/* set up bootstrap stack */
	movl	$ _kstack+UPAGES*NBPG-4*12,%esp	# bootstrap stack end location
	xorl	%eax,%eax		# mark end of frames
	movl	%eax,%ebp
	movl	_proc0paddr, %eax
	movl	%esi, PCB_CR3(%eax)

	lea	7*NBPG(%esi),%esi	# skip past stack.
	pushl	%esi
	
	call	_init386		# wire 386 chip for unix operation
	popl	%esi
	
	movl	$0,_PTD
	call 	_main

	.globl	__ucodesel,__udatasel
	movzwl	__ucodesel,%eax
	movzwl	__udatasel,%ecx
	# build outer stack frame
	pushl	%ecx		# user ss
	pushl	$ USRSTACK	# user esp
	pushl	%eax		# user cs
	pushl	$0		# user ip
	movw	%cx,%ds
	movw	%cx,%es
	# movw	%ax,%fs		# double map cs to fs
	# movw	%cx,%gs		# and ds to gs
	lret	# goto user!

	pushl	$lretmsg1	/* "should never get here!" */
	call	_panic
lretmsg1:
	.asciz	"lret: toinit\n"

	.globl	__exit
__exit:
	call _reset_cpu
	/* NOTREACHED */

	.set	exec,59
	.set	exit,1
	.globl	_icode
	.globl	_szicode

#define	LCALL(x,y)	.byte 0x9a ; .long y; .word x
/*
 * Icode is copied out to process 1 to exec /etc/init.
 * If the exec fails, process 1 exits.
 */
_icode:
	pushl	$0 /* environment */

	# pushl	$argv-_icode	# gas fucks up again
	movl	$argv,%eax
	subl	$_icode,%eax
	pushl	%eax

	# pushl	$init-_icode
	movl	$init,%eax
	subl	$_icode,%eax
	pushl	%eax
	pushl	%eax	# dummy out rta

	movl	%esp,%ebp
	movl	$exec,%eax
	LCALL(0x7,0x0)
	pushl	%eax
	movl	$exit,%eax
	pushl	%eax	# dummy out rta
	LCALL(0x7,0x0)

init:
	.asciz	"/sbin/init"
	.align	2
argv:
	.long	init+6-_icode		# argv[0] = "init" ("/sbin/init" + 6)
	.long	eicode-_icode		# argv[1] follows icode after copyout
	.long	0
eicode:

_szicode:
	.long	_szicode-_icode

	.globl	_sigcode,_szsigcode
_sigcode:
	movl	12(%esp),%eax	# unsure if call will dec stack 1st
	call	%eax
	xorl	%eax,%eax	# smaller movl $103,%eax
	movb	$103,%al	# sigreturn()
	LCALL(0x7,0)		# enter kernel with args on stack
	hlt			# never gets here

_szsigcode:
	.long	_szsigcode-_sigcode

	.globl ___udivsi3
___udivsi3:
	movl 4(%esp),%eax
	xorl %edx,%edx
	divl 8(%esp)
	ret

	.globl ___divsi3
___divsi3:
	movl 4(%esp),%eax
	xorl %edx,%edx
	cltd
	idivl 8(%esp)
	ret

	.globl	_inb
_inb:	movl	4(%esp),%edx
	# inb	$0x84,%al	# Compaq SystemPro 
	subl	%eax,%eax	# clr eax
	NOP
	inb	%dx,%al
	NOP
	ret


	.globl	_rtcin
_rtcin:	movl	4(%esp),%eax
	outb	%al,$0x70
	subl	%eax,%eax	# clr eax
	inb	$0x71,%al	# Compaq SystemPro 
	ret

	.globl	_outb
_outb:	movl	4(%esp),%edx
	movl	8(%esp),%eax
	NOP
	outb	%al,%dx
	# inb	$0x84,%al
	NOP
	ret

	#
	# bzero (base,cnt)
	#

	.globl _bzero
	.globl _blkclr
_bzero:
_blkclr:
	pushl	%edi
	movl	8(%esp),%edi
	movl	12(%esp),%ecx
	xorl	%eax,%eax
	shrl	$2,%ecx	
	cld
	rep
	stosl
	movl	12(%esp),%ecx
	andl	$3,%ecx
	rep
	stosb
	popl	%edi
	ret

	#
	# fillw (pat,base,cnt)
	#

	.globl _fillw
_fillw:
	pushl	%edi
	movl	8(%esp),%eax
	movl	12(%esp),%edi
	movl	16(%esp),%ecx
	cld
	rep
	stosw
	popl	%edi
	ret

	#
	# bcopy (src,dst,cnt)
	# NOTE: does not (yet) handle overlapped copies
	#

	.globl	_bcopy
_bcopy:
	pushl	%esi
	pushl	%edi
	movl	12(%esp),%esi
	movl	16(%esp),%edi
	movl	20(%esp),%ecx
	shrl	$2,%ecx	
	cld
	rep
	movsl
	movl	20(%esp),%ecx
	andl	$3,%ecx
	rep
	movsb
	popl	%edi
	popl	%esi
	xorl	%eax,%eax
	ret

	#
	# ovbcopy (src,dst,cnt)
	# NOTE: does not (yet) work doing words at a time
	#

	.globl	_ovbcopy
_ovbcopy:
	pushl	%esi
	pushl	%edi
	movl	12(%esp),%esi
	movl	16(%esp),%edi
	movl	20(%esp),%ecx
	addl	%ecx,%esi	/* copy from end to beginning */
	addl	%ecx,%edi
	decl	%esi
	decl	%edi
	std			/* decrementing as we go */
	rep
	movsb
	popl	%edi
	popl	%esi
	xorl	%eax,%eax
	cld
	ret

	.globl	_copyin
_copyin:
	movl	_curpcb,%eax
	movl	$cpyflt,PCB_ONFAULT(%eax) # in case we page/protection violate
	pushl	%esi
	pushl	%edi
	movl	12(%esp),%esi
	movl	16(%esp),%edi
	movl	20(%esp),%ecx

	/* if dest >= USRSTACK, return error */
	cmpl	$ USRSTACK, %esi
	jae	cpyflt

	/* if USRSTACK-dest < len, return error */
	movl	$ USRSTACK, %eax
	subl	%esi, %eax
	cmpl	%ecx, %eax
	jb	cpyflt

	shrl	$2,%ecx
	cld
	rep
	movsl
	movl	20(%esp),%ecx
	andl	$3,%ecx
	rep
	movsb
	popl	%edi
	popl	%esi
	xorl	%eax,%eax
	movl	_curpcb,%edx
	movl	%eax,PCB_ONFAULT(%edx)
	ret

cpyflt: popl	%edi
	popl	%esi
	movl	_curpcb,%edx
	movl	$0,PCB_ONFAULT(%edx)
	movl	$ EFAULT,%eax
	ret

	# insb(port,addr,cnt)
	.globl	_insb
_insb:
	pushl	%edi
	movw	8(%esp),%dx
	movl	12(%esp),%edi
	movl	16(%esp),%ecx
	cld
	NOP
	rep
	insb
	NOP
	movl	%edi,%eax
	popl	%edi
	ret

	# insw(port,addr,cnt)
	.globl	_insw
_insw:
	pushl	%edi
	movw	8(%esp),%dx
	movl	12(%esp),%edi
	movl	16(%esp),%ecx
	cld
	NOP
	.byte 0x66,0xf2,0x6d	# rep insw
	NOP
	movl	%edi,%eax
	popl	%edi
	ret

	# outsw(port,addr,cnt)
	.globl	_outsw
_outsw:
	pushl	%esi
	movw	8(%esp),%dx
	movl	12(%esp),%esi
	movl	16(%esp),%ecx
	cld
	NOP
	.byte 0x66,0xf2,0x6f	# rep outsw
	NOP
	movl	%esi,%eax
	popl	%esi
	ret

	# lgdt(*gdt, ngdt)
	.globl	_lgdt
	# .globl	_gdt
xxx:	.word 31
	.long 0
_lgdt:
	movl	4(%esp),%eax
	movl	%eax,xxx+2
	movl	8(%esp),%eax
	movw	%ax,xxx
	lgdt	xxx
	jmp	1f
	NOP
1:	movw	$0x10,%ax
	movw	%ax,%ds
	movw	%ax,%es
	movw	%ax,%ss
	movl	0(%esp),%eax
	pushl	%eax
	movl	$8,4(%esp)
	lret

	# lidt(*idt, nidt)
	.globl	_lidt
yyy:	.word	255
	.long	0 
_lidt:
	movl	4(%esp),%eax
	movl	%eax,yyy+2
	movl	8(%esp),%eax
	movw	%ax,yyy
	lidt	yyy
	ret

	# lldt(sel)
	.globl	_lldt
_lldt:
	movl	4(%esp),%eax
	lldt	%eax
	ret

	# ltr(sel)
	.globl	_ltr
_ltr:
	movl	4(%esp),%eax
	ltr	%eax
	ret

	# lcr3(cr3)
	.globl	_lcr3
	.globl	_load_cr3
_load_cr3:
_lcr3:
	inb	$0x84,%al	# check wristwatch
	movl	4(%esp),%eax
 	orl	$ I386_CR3PAT,%eax

	movl	$tmpstk2,%edx
	movl	(%edx),%ecx	# touch stack, fault if not there
	movl	%ecx,(%edx)
	movl	%esp,%ecx
	movl	%edx,%esp

	movl	%eax,%cr3
	inb	$0x84,%al	# check wristwatch

	movl	(%ecx),%edx	# touch stack, fault if not there
	movl	%edx,(%ecx)
	movl	%ecx,%esp
	ret

	# tlbflush()
	.globl	_tlbflush
_tlbflush:
	inb	$0x84,%al	# check wristwatch
	movl	%cr3,%eax
 	orl	$ I386_CR3PAT,%eax

	movl	$tmpstk2,%edx
	movl	(%edx),%ecx	# touch stack, fault if not there
	movl	%ecx,(%edx)
	movl	%esp,%ecx
	movl	%edx,%esp

	movl	%eax,%cr3
	inb	$0x84,%al	# check wristwatch

	movl	(%ecx),%edx	# touch stack, fault if not there
	movl	%edx,(%ecx)
	movl	%ecx,%esp
	ret

	# lcr0(cr0)
	.globl	_lcr0,_load_cr0
_lcr0:
_load_cr0:
	movl	4(%esp),%eax
	movl	%eax,%cr0
	ret

	# rcr0()
	.globl	_rcr0
_rcr0:
	movl	%cr0,%eax
	ret

	# rcr2()
	.globl	_rcr2
_rcr2:
	movl	%cr2,%eax
	ret

	# rcr3()
	.globl	_rcr3
	.globl	__cr3
__cr3:
_rcr3:
	movl	%cr3,%eax
	ret

	# ssdtosd(*ssdp,*sdp)
	.globl	_ssdtosd
_ssdtosd:
	pushl	%ebx
	movl	8(%esp),%ecx
	movl	8(%ecx),%ebx
	shll	$16,%ebx
	movl	(%ecx),%edx
	roll	$16,%edx
	movb	%dh,%bl
	movb	%dl,%bh
	rorl	$8,%ebx
	movl	4(%ecx),%eax
	movw	%ax,%dx
	andl	$0xf0000,%eax
	orl	%eax,%ebx
	movl	12(%esp),%ecx
	movl	%edx,(%ecx)
	movl	%ebx,4(%ecx)
	popl	%ebx
	ret

/*
 * {fu,su},{byte,word}
 */
ALTENTRY(fuiword)
ENTRY(fuword)
	movl	_curpcb,%ecx
	movl	$fusufault,PCB_ONFAULT(%ecx)
	movl	4(%esp),%edx

	cmpl	$ USRSTACK - 3, %edx
	jae	fusufault

	# .byte	0x65		# use gs
	movl	0(%edx),%eax
	movl	$0,PCB_ONFAULT(%ecx)
	ret
	
ALTENTRY(fuibyte)
ENTRY(fubyte)
	movl	_curpcb,%ecx
	movl	$fusufault,PCB_ONFAULT(%ecx) #in case we page/protection violate
	movl	4(%esp),%edx

	cmpl	$ USRSTACK, %edx
	jae	fusufault

	# .byte	0x65		# use gs
	movzbl	0(%edx),%eax
	movl	$0,PCB_ONFAULT(%ecx)
	ret
	
fusufault:
	movl	_curpcb,%ecx
	xorl	%eax,%eax
	movl	%eax,PCB_ONFAULT(%ecx) #in case we page/protection violate
	decl	%eax
	ret


/*
 * There is a little bit of duplicated code so we can avoid flushing the
 * prefetch queue in the common case.
 */

ALTENTRY(suiword)
ENTRY(suword)
	movl 4(%esp), %eax /* address */

	cmpl $ USRSTACK, %eax
	jae return_minus_one

	/* check if the destination crosses a page boundary */
	movl %eax, %ecx
	andl $ NBPG - 1, %ecx
	cmpl $ NBPG - 4, %ecx
	ja suword_breakup

	/* make sure the page table is present */
	movl %eax, %ecx
	shrl $ PDRSHIFT, %ecx
	movl _PTD(,%ecx,4), %ecx
	andl $ PG_V, %ecx
	jnz suword_fault

	/* now make sure the page is present with user write permission */
	movl %eax, %ecx
	shrl $ PGSHIFT, %ecx
	movl _PTmap(,%ecx,4), %ecx
	andl $ PG_V | PG_UW, %ecx
	cmpl $ PG_V | PG_UW, %ecx
	jnz suword_fault

	movl 8(%esp), %ecx
	movl %ecx, (%eax)
	xorl %eax, %eax
	ret

suword_fault:
	/* 
	 * this is a slow case anyway, so build a frame to make the debugger
	 * more useful
	 */
	pushl %ebp
	movl %esp, %ebp

	pushl %eax
	call _user_write_fault

	leave

	cmpl $0, %eax
	jnz return_minus_one

	movl 4(%esp), %eax
	movl 8(%esp), %ecx
	movl %ecx, (%eax)
	xorl %eax, %eax
	ret


suword_breakup:
	/* crosses page boundary ... do each byte separately */
	pushl %ebp
	movl %esp, %ebp
	pushl %esi
	pushl %edi
	pushl %ebx

	movl %eax, %edi
	movl 12(%ebp), %ebx
	movl $4, %esi

suword_breakup_loop:
	pushl %ebx
	pushl %edi
	call _subyte
	addl $8, %esp

	cmpl $-1, %eax
	jz suword_breakup_error

	incl %edi
	shrl $8, %ebx
	decl %esi
	jnz suword_breakup_loop

	xorl %eax, %eax
	popl %ebx
	popl %edi
	popl %esi
	leave
	ret

suword_breakup_error:
	movl $-1, %eax
	popl %ebx
	popl %edi
	popl %esi
	leave
	ret

ALTENTRY(suibyte)
ENTRY(subyte)
	movl 4(%esp), %eax /* address */

	cmpl $ USRSTACK, %eax
	jae return_minus_one

	/* make sure the page table is present */
	movl %eax, %ecx
	shrl $ PDRSHIFT, %ecx
	movl _PTD(,%ecx,4), %ecx
	andl $ PG_V, %ecx
	jnz subyte_fault

	/* now make sure the page is present with user write permission */
	movl %eax, %ecx
	shrl $ PGSHIFT, %ecx
	movl _PTmap(,%ecx,4), %ecx
	andl $ PG_V | PG_UW, %ecx
	cmpl $ PG_V | PG_UW, %ecx
	jnz subyte_fault

	movl 8(%esp), %ecx
	movb %cl, (%eax)
	xorl %eax, %eax
	ret

subyte_fault:
	pushl %ebp
	movl %esp, %ebp

	pushl %eax
	call _user_write_fault

	leave

	cmpl $0, %eax
	jnz return_minus_one

	movl 4(%esp), %eax
	movl 8(%esp), %ecx
	movb %cl, (%eax)
	xorl %eax, %eax
	ret

return_minus_one:
	movl $-1, %eax
	ret

	ENTRY(setjmp)
	movl	4(%esp),%eax
	movl	%ebx, 0(%eax)		# save ebx
	movl	%esp, 4(%eax)		# save esp
	movl	%ebp, 8(%eax)		# save ebp
	movl	%esi,12(%eax)		# save esi
	movl	%edi,16(%eax)		# save edi
	movl	(%esp),%edx		# get rta
	movl	%edx,20(%eax)		# save eip
	xorl	%eax,%eax		# return (0);
	ret

#ifdef notdef
	ENTRY(longjmp)
	movl	4(%esp),%eax
	movl	 0(%eax),%ebx		# restore ebx
	movl	 4(%eax),%esp		# restore esp
	movl	 8(%eax),%ebp		# restore ebp
	movl	12(%eax),%esi		# restore esi
	movl	16(%eax),%edi		# restore edi
	movl	20(%eax),%edx		# get rta
	movl	%edx,(%esp)		# put in return frame
	xorl	%eax,%eax		# return (1);
	incl	%eax
	ret
#endif
/*
 * The following primitives manipulate the run queues.
 * _whichqs tells which of the 32 queues _qs
 * have processes in them.  Setrq puts processes into queues, Remrq
 * removes them from queues.  The running process is on no queue,
 * other processes are on a queue related to p->p_pri, divided by 4
 * actually to shrink the 0-127 range of priorities into the 32 available
 * queues.
 */

	.globl	_whichqs,_qs,_cnt,_panic
	.comm	_noproc,4
	.comm	_runrun,4

/*
 * Setrq(p)
 *
 * Call should be made at spl6(), and p->p_stat should be SRUN
 */
ENTRY(setrq)
	movl	4(%esp),%eax
	cmpl	$0,P_RLINK(%eax)	# should not be on q already
	je	set1
	pushl	$set2
	call	_panic
set1:
	movzbl	P_PRI(%eax),%edx
	shrl	$2,%edx
	btsl	%edx,_whichqs		# set q full bit
	shll	$3,%edx
	addl	$_qs,%edx		# locate q hdr
	movl	%edx,P_LINK(%eax)	# link process on tail of q
	movl	P_RLINK(%edx),%ecx
	movl	%ecx,P_RLINK(%eax)
	movl	%eax,P_RLINK(%edx)
	movl	%eax,P_LINK(%ecx)
	ret

set2:	.asciz	"setrq"

/*
 * Remrq(p)
 *
 * Call should be made at spl6().
 */
ENTRY(remrq)
	movl	4(%esp),%eax
	movzbl	P_PRI(%eax),%edx
	shrl	$2,%edx
	btrl	%edx,_whichqs		# clear full bit, panic if clear already
	jb	rem1
	pushl	$rem3
	call	_panic
rem1:
	pushl	%edx
	movl	P_LINK(%eax),%ecx	# unlink process
	movl	P_RLINK(%eax),%edx
	movl	%edx,P_RLINK(%ecx)
	movl	P_RLINK(%eax),%ecx
	movl	P_LINK(%eax),%edx
	movl	%edx,P_LINK(%ecx)
	popl	%edx
	movl	$_qs,%ecx
	shll	$3,%edx
	addl	%edx,%ecx
	cmpl	P_LINK(%ecx),%ecx	# q still has something?
	je	rem2
	shrl	$3,%edx			# yes, set bit as still full
	btsl	%edx,_whichqs
rem2:
	movl	$0,P_RLINK(%eax)	# zap reverse link to indicate off list
	ret

rem3:	.asciz	"remrq"
sw0:	.asciz	"swtch"

/*
 * When no processes are on the runq, Swtch branches to idle
 * to wait for something to come ready.
 */
	.globl	Idle
Idle:
idle:
	call	_spl0
	cmpl	$0,_whichqs
	jne	sw1
	hlt		# wait for interrupt
	jmp	idle

badsw:
	pushl	$sw0
	call	_panic
	/*NOTREACHED*/

/*
 * Swtch()
 */
ENTRY(swtch)

	incl	_cnt+V_SWTCH

	/* switch to new process. first, save context as needed */

	movl	_curproc,%ecx
	movl	P_ADDR(%ecx),%ecx


	movl	(%esp),%eax		# Hardware registers
	movl	%eax, PCB_EIP(%ecx)
	movl	%ebx, PCB_EBX(%ecx)
	movl	%esp, PCB_ESP(%ecx)
	movl	%ebp, PCB_EBP(%ecx)
	movl	%esi, PCB_ESI(%ecx)
	movl	%edi, PCB_EDI(%ecx)

#if 0 && NNPX > 0
	movb	PCB_FLAGS(%ecx),%al
	/* have we used fp, and need a save? */
	andb	$ FP_WASUSED|FP_NEEDSSAVE,%al
	cmpb	$ FP_WASUSED|FP_NEEDSSAVE,%al
	jne	1f
	movl	%cr0,%eax		/* insure fp is enabled */
	andb 	$0xfb,%al
	movl	%eax,%cr0
	fnsave	PCB_SAVEFPU(%ecx)
	orb 	$4,%al			/* disable it */
	movl	%eax,%cr0
	movb	PCB_FLAGS(%ecx),%al
	xorb	$ FP_NEEDSSAVE,%al	/* save processed */
	movb	%al,PCB_FLAGS(%ecx)
1:
#endif

	movl	_CMAP2,%eax		# save temporary map PTE
	movl	%eax,PCB_CMAP2(%ecx)	# in our context

	movw	_cpl, %ax
	movw	%ax, PCB_IML(%ecx)	# save ipl

	movl	$tmpstk2,%edx
	movl	(%edx),%eax	# touch stack, fault if not there
	movl	%eax,(%edx)
	movl	%edx,%esp
	movl	$pcb2,_curpcb

	/* save is done, now choose a new process or idle */
sw1:
	cli				# XXX?
	movl	_whichqs,%edi
2:
	bsfl	%edi,%eax		# find a full q
	jz	idle			# if none, idle
	# XX update whichqs?
swfnd:
	btrl	%eax,%edi		# clear q full status
	jnb	2b		# if it was clear, look for another
	movl	%eax,%ebx		# save which one we are using

	shll	$3,%eax
	addl	$_qs,%eax		# select q
	movl	%eax,%esi

#ifdef	DIAGNOSTIC
	cmpl	P_LINK(%eax),%eax # linked to self? (e.g. not on list)
	je	badsw			# not possible
#endif

	movl	P_LINK(%eax),%ecx	# unlink from front of process q
	movl	P_LINK(%ecx),%edx
	movl	%edx,P_LINK(%eax)
	movl	P_RLINK(%ecx),%eax
	movl	%eax,P_RLINK(%edx)

	cmpl	P_LINK(%ecx),%esi	# q empty
	je	3f
	btsl	%ebx,%edi		# nope, set to indicate full
3:
	movl	%edi,_whichqs		# update q status

	movl	$0,%eax
	movl	%ecx,_curproc
	movl	%eax,_want_resched

#ifdef	DIAGNOSTIC
	cmpl	%eax,P_WCHAN(%ecx)
	jne	badsw
	cmpb	$ SRUN,P_STAT(%ecx)
	jne	badsw
#endif

	movl	%eax,P_RLINK(%ecx) /* isolate process to run */
	movl	P_ADDR(%ecx),%edx
	movl	%edx,_curpcb
	inb	$0x84,%al	# flush write buffers
	movl	PCB_CR3(%edx),%ebx

	/* switch address space */
	cli
 	orl	$ I386_CR3PAT,%ebx
	movl	%ebx,%cr3	# context switch address space

	jmp	7f
	nop
 7:	inb	$0x84,%al	# flush write buffers
	movl	PCB_ESP(%edx), %ecx
	movl	(%ecx),%eax	# touch stack, fault if not there
	movl	%eax,(%ecx)
	movl	%ecx,%esp

	/* restore context */
	movl	PCB_EBX(%edx), %ebx
	movl	PCB_ESP(%edx), %esp
	movl	PCB_EBP(%edx), %ebp
	movl	PCB_ESI(%edx), %esi
	movl	PCB_EDI(%edx), %edi
	movl	PCB_EIP(%edx), %eax
	movl	%eax, (%esp)

#if NNPX > 0
#ifdef notdef
	movb	PCB_FLAGS(%edx),%al
	/* if fp could be used, a dna trap will do a restore */
	testb	$ FP_WASUSED,%al
	je	1f
	orb	$ FP_NEEDSRESTORE,PCB_FLAGS(%ecx)
1:
#endif
	movl	%cr0,%eax
	orb 	$4,%al			/* disable it */
	movl	%eax,%cr0
#endif

	movl	PCB_CMAP2(%edx),%eax	# get temporary map
	movl	%eax,_CMAP2		# reload temporary map PTE

	pushl	PCB_IML(%edx)
	call	_splx
	popl	%eax

	movl	%edx,%eax		# return (1);
	ret

/*
 * struct proc *swtch_to_inactive(p) ; struct proc *p;
 *
 * At exit of a process, move off the address space of the
 * process and onto a "safe" one. Then, on a temporary stack
 * return and run code that disposes of the old state.
 * Since this code requires a parameter from the "old" stack,
 * pass it back as a return value.
 */
ENTRY(swtch_to_inactive)

	movl	$tmpstk2-4,%ecx		# temporary stack, compensated for call
	movl	(%ecx),%eax		# touch stack, fault if not there
	movl	%eax,(%ecx)

	popl	%edx			# old pc
	popl	%eax			# arg, our return value
	inb	$0x84,%al	# flush write buffers

	movl	%ecx,%esp

	movl	_IdlePTD,%ecx

	movl	%ecx,%cr3		# good bye address space
	inb	$0x84,%al	# flush write buffers

 #write buffer?
	movl	$pcb2,_curpcb
	jmp	%edx			# return, execute remainder of cleanup

/*
 * savectx(pcb, altreturn)
 * Update pcb, saving current processor state and arranging
 * for alternate return ala longjmp in swtch if altreturn is true.
 */
ENTRY(savectx)
	movl	4(%esp), %ecx
	movw	_cpl, %ax
	movw	%ax,  PCB_IML(%ecx)
	movl	(%esp), %eax	
	movl	%eax, PCB_EIP(%ecx)
	movl	%ebx, PCB_EBX(%ecx)
	movl	%esp, PCB_ESP(%ecx)
	movl	%ebp, PCB_EBP(%ecx)
	movl	%esi, PCB_ESI(%ecx)
	movl	%edi, PCB_EDI(%ecx)
#if 0 && NNPX > 0
	/* have we ever used fp, and need to save? */
	testb	$ FP_WASUSED, PCB_FLAGS(%ecx)
	je	1f
	movl	%cr0, %edx
	andb 	$0xfb, %dl
	movl	%edx, %cr0
	fnsave	PCB_SAVEFPU(%ecx)
	orb 	$4, %edx
	movl	%edx, %cr0
1:
#endif
	movl	_CMAP2, %edx		# save temporary map PTE
	movl	%edx, PCB_CMAP2(%ecx)	# in our context

	cmpl	$0, 8(%esp)
	je	1f
	movl	%esp, %edx		# relocate current sp relative to pcb
	subl	$_kstack, %edx		#   (sp is relative to kstack):
	addl	%edx, %ecx		#   pcb += sp - kstack;
	movl	%eax, (%ecx)		# write return pc at (relocated) sp@
	# this mess deals with replicating register state gcc hides
	movl	12(%esp),%eax
	movl	%eax,12(%ecx)
	movl	16(%esp),%eax
	movl	%eax,16(%ecx)
	movl	20(%esp),%eax
	movl	%eax,20(%ecx)
	movl	24(%esp),%eax
	movl	%eax,24(%ecx)
1:
	xorl	%eax, %eax		# return 0
	ret

	.globl	_mvesp
_mvesp:	movl	%esp,%eax
	ret

/*
 * update profiling information for the user
 * addupc(pc, up, ticks) struct uprof *up;
 */

ENTRY(addupc)
	movl	4(%esp),%eax		/* pc */
	movl	8(%esp),%ecx		/* up */

	/* does sampled pc fall within bottom of profiling window? */
	subl	PR_OFF(%ecx),%eax 	/* pc -= up->pr_off; */
	jl	1f 			/* if (pc < 0) return; */

	/* construct scaled index */
	shrl	$1,%eax			/* reduce pc to a short index */
	mull	PR_SCALE(%ecx)		/* pc*up->pr_scale */
	shrdl	$15,%edx,%eax 		/* praddr >> 15 */
	cmpl	$0,%edx			/* if overflow, ignore */
	jne	1f
	andb	$0xfe,%al		/* praddr &= ~1 */

	/* within profiling buffer? if so, compute address */
	cmpl	%eax,PR_SIZE(%ecx)	/* if (praddr > up->pr_size) return; */
	jg	1f
	addl	PR_BASE(%ecx),%eax	/* praddr += up->pr_base; */

	/* tally ticks to selected counter */
	movl	_curpcb,%ecx
	movl	$proffault,PCB_ONFAULT(%ecx) #in case we page/protection violate
	movl	12(%esp),%edx		/* ticks */
	addw	%dx,(%eax)
	movl	$0,PCB_ONFAULT(%ecx)
1:	ret

proffault:
	/* disable profiling if we get a fault */
	movl	$0,PR_SCALE(%ecx) /*	up->pr_scale = 0; */
	movl	_curpcb,%ecx
	movl	$0,PCB_ONFAULT(%ecx)
	ret

.data
	.globl	_cyloffset, _curpcb
_cyloffset:	.long	0
	.globl	_proc0paddr
_proc0paddr:	.long	0
LF:	.asciz "swtch %x"

.text
	.globl _astoff
_astoff:
	movl	$0,_astpending
	ret

#define	IDTVEC(name)	.align 4; .globl _X/**/name; _X/**/name:
#define	PANIC(msg)	xorl %eax,%eax; movl %eax,_waittime; pushl 1f; \
			call _panic; 1: .asciz msg
#define	PRINTF(n,msg)	pushal ; pushl 1f; call _printf; MSG(msg) ; \
			 popl %eax ; popal
#define	MSG(msg)	.data; 1: .asciz msg; .text

	.text

/*
 * Trap and fault vector routines
 */ 
#define	TRAP(a)		pushl $a ; jmp alltraps
#ifdef KGDB
#define	BPTTRAP(a)	pushl $a ; jmp bpttraps
#else
#define	BPTTRAP(a)	TRAP(a)
#endif

IDTVEC(div)
	pushl $0; TRAP(T_DIVIDE)
IDTVEC(dbg)
	pushl $0; BPTTRAP(T_TRCTRAP)
IDTVEC(nmi)
	pushl $0; TRAP(T_NMI)
IDTVEC(bpt)
	pushl $0; BPTTRAP(T_BPTFLT)
IDTVEC(ofl)
	pushl $0; TRAP(T_OFLOW)
IDTVEC(bnd)
	pushl $0; TRAP(T_BOUND)
IDTVEC(ill)
	pushl $0; TRAP(T_PRIVINFLT)
IDTVEC(dna)
	pushl $0; TRAP(T_DNA)
IDTVEC(dble)
	TRAP(T_DOUBLEFLT)
	/*PANIC("Double Fault");*/
IDTVEC(fpusegm)
	pushl $0; TRAP(T_FPOPFLT)
IDTVEC(tss)
	TRAP(T_TSSFLT)
	/*PANIC("TSS not valid");*/
IDTVEC(missing)
	TRAP(T_SEGNPFLT)
IDTVEC(stk)
	TRAP(T_STKFLT)
IDTVEC(prot)
	TRAP(T_PROTFLT)
IDTVEC(page)
	TRAP(T_PAGEFLT)
IDTVEC(rsvd)
	pushl $0; TRAP(T_RESERVED)
IDTVEC(fpu)
	pushl $0; TRAP(T_ARITHTRAP)
	/* 17 - 31 reserved for future exp */
IDTVEC(rsvd0)
	pushl $0; TRAP(17)
IDTVEC(rsvd1)
	pushl $0; TRAP(18)
IDTVEC(rsvd2)
	pushl $0; TRAP(19)
IDTVEC(rsvd3)
	pushl $0; TRAP(20)
IDTVEC(rsvd4)
	pushl $0; TRAP(21)
IDTVEC(rsvd5)
	pushl $0; TRAP(22)
IDTVEC(rsvd6)
	pushl $0; TRAP(23)
IDTVEC(rsvd7)
	pushl $0; TRAP(24)
IDTVEC(rsvd8)
	pushl $0; TRAP(25)
IDTVEC(rsvd9)
	pushl $0; TRAP(26)
IDTVEC(rsvd10)
	pushl $0; TRAP(27)
IDTVEC(rsvd11)
	pushl $0; TRAP(28)
IDTVEC(rsvd12)
	pushl $0; TRAP(29)
IDTVEC(rsvd13)
	pushl $0; TRAP(30)
IDTVEC(rsvd14)
	pushl $0; TRAP(31)

alltraps:
	pushal
	push %ds
	push %es
	movw	$0x10,%ax
	movw	%ax,%ds
	movw	%ax,%es
calltrap:
	incl	_cnt+V_TRAP
	call	_trap

	cli

	/* this value may also be used in return_to_user_mode */
	movl	0x34(%esp),%esi /* previous cs */
	andl	$3,%esi
	jz	trap_return

	cmpl	$0,_astpending
	jnz	do_astflt

	cmpw	$0,_cpl
	jnz	return_to_user_mode /* in icu.s */

trap_return:
	pop %es
	pop %ds
	popal
	nop
	addl	$8,%esp			# pop type, code
	iret

do_astflt:
	/* pop off the old trap frame, then create a new one that
	 * will give trap() another chance
	 */
	pop	%es
	pop	%ds
	popa
	addl	$8,%esp

	pushl	$0
	TRAP (T_ASTFLT)
	/* NORETURN */
		


#ifdef KGDB
/*
 * This code checks for a kgdb trap, then falls through
 * to the regular trap code.
 */
bpttraps:
	pushal
	push	%es
	push	%ds
	movw	$0x10,%ax
	movw	%ax,%ds
	movw	%ax,%es
	movzwl	52(%esp),%eax
	test	$3,%eax	
	jne	calltrap
	call	_kgdb_trap_glue		
	jmp	calltrap
#endif

/*
 * Call gate entry for syscall
 */

IDTVEC(syscall)
	pushfl	# only for stupid carry bit and more stupid wait3 cc kludge
	pushal	# only need eax,ecx,edx - trap resaves others
	movw	$0x10,%ax	# switch to kernel segments
	movw	%ax,%ds
	movw	%ax,%es
	call	_syscall

	cli

	cmpl	$0,_astpending
	jnz	syscall_ast

	cmpw	$0,_cpl
	jnz	syscall_fix_cpl

	movw	__udatasel,%ax	# switch back to user segments
	movw	%ax,%ds
	movw	%ax,%es
	popal
	nop
	popfl
	lret

syscall_ast:
	movw	__udatasel,%ax
	movw	%ax,%ds
	movw	%ax,%es
	popal

	/* convert to trap frame
	 * stack is now ss, sp, cs,    ip, flags
	 * we want      ss, sp, flags, cs, ip   
 	 * offsets      16  12  8      4   0    
	 */
	xchgl	%eax,8(%esp) /* now eax has cs */
	xchgl	%eax,4(%esp) /* now eax has ip */
	xchgl	%eax,0(%esp) /* now eax has flags */
	xchgl	%eax,8(%esp) /* now eax has its original value */
	pushl $0
	TRAP (T_ASTFLT)
	/* NORETURN */

syscall_fix_cpl:
	movw	__udatasel,%ax
	movw	%ax,%ds
	movw	%ax,%es
	popal

	/* convert to trap frame
	 * stack is now ss, sp, cs,    ip, flags
	 * we want      ss, sp, flags, cs, ip   
 	 * offsets      16  12  8      4   0    
	 */
	xchgl	%eax,8(%esp) /* now eax has cs */
	xchgl	%eax,4(%esp) /* now eax has ip */
	xchgl	%eax,0(%esp) /* now eax has flags */
	xchgl	%eax,8(%esp) /* now eax has its original value */
	
	pushl $0
	pushl $ T_ASTFLT

	pushal
	nop
	push %ds
	push %es
	movw	$0x10,%ax
	movw	%ax,%ds
	movw	%ax,%es

	movl	$1, %esi /* non-zero to indicate return to user mode */
	jmp	return_to_user_mode /* in icu.s */

	
ENTRY(htonl)
ENTRY(ntohl)
	movl	4(%esp),%eax
	xchgb	%al,%ah
	roll	$16,%eax
	xchgb	%al,%ah
	ret

ENTRY(htons)
ENTRY(ntohs)
	movzwl	4(%esp),%eax
	xchgb	%al,%ah
	ret

/* DELAY(n)  delay about n microseconds */
ENTRY(DELAY)
	movl 4(%esp), %ecx
	incl %ecx /* make DELAY(0) go through the loop just once */

	/* 
	 * 0x80 is the manufacturing test port, which should be safe to
	 * write to on any motherboard.  The output instruction will
	 * be executed at bus speed, rather than processor speed, so
	 * it will be about 750ns on any ISA or EISA machine.
	 */
1:
	outb %al, $0x80
	loop 1b
	ret	

#include "vector.s"
#include <i386/isa/icu.s>
