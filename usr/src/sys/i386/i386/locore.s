/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)locore.s	5.7 (Berkeley) %G%
 */

/*
 * locore.s:	4BSD machine support for the Intel 386
 *		Preliminary version
 *		Written by William F. Jolitz, 386BSD Project
 */

#include "machine/psl.h"
#include "machine/pte.h"

#include "errno.h"
#include "cmap.h"

#include "machine/trap.h"

/*
 * Note: This version greatly munged to avoid various assembler errors
 * that may be fixed in newer versions of gas. Perhaps newer versions
 * will have more pleasant appearance.
 */

	.set	IDXSHIFT,10
	.set	SYSTEM,0xFE000000	# virtual address of system start
	/*note: gas copys sign bit (e.g. arithmetic >>), can't do SYSTEM>>22! */
	.set	SYSPDROFF,0x3F8		# Page dir

	.set	IOPHYSmem,0xa0000

/* IBM "compatible" nop - sensitive macro on "fast" 386 machines */
#define	NOP	jmp 7f ; nop ; 7: jmp 7f ; nop ; 7:

/*
 * User structure is UPAGES at top of user space.
 */
	.set	_u,0xFDFFE000
	.globl	_u
	.set	UPDROFF,0x3F7
	.set	UPTEOFF,0x3FE

#define	ENTRY(name) \
	.globl _/**/name; _/**/name:
#define	ALTENTRY(name) \
	.globl _/**/name; _/**/name:

/*
 * System page table
 * Mbmap and Usrptmap are enlarged by CLSIZE entries
 * as they are managed by resource maps starting with index 1 or CLSIZE.
 */ 
#define	SYSMAP(mname, vname, npte)		\
_/**/mname:	.globl	_/**/mname;		\
	.space	(npte)*4;			\
	.set	_/**/vname,ptes*NBPG+SYSTEM;	\
	.globl	_/**/vname;			\
	.set	ptes,ptes + npte
#define	ZSYSMAP(mname, vname, npte)		\
_/**/mname:	.globl	_/**/mname;		\
	.set	_/**/vname,ptes*NBPG+SYSTEM;	\
	.globl	_/**/vname;

	.data
	# assumed to start at data mod 4096
	.set	ptes,0
	SYSMAP(Sysmap,Sysbase,SYSPTSIZE)
	SYSMAP(Forkmap,forkutl,UPAGES)
	SYSMAP(Xswapmap,xswaputl,UPAGES)
	SYSMAP(Xswap2map,xswap2utl,UPAGES)
	SYSMAP(Swapmap,swaputl,UPAGES)
	SYSMAP(Pushmap,pushutl,UPAGES)
	SYSMAP(Vfmap,vfutl,UPAGES)
	SYSMAP(CMAP1,CADDR1,1)
	SYSMAP(CMAP2,CADDR2,1)
	SYSMAP(mmap,vmmap,1)
	SYSMAP(alignmap,alignutl,1)	/* XXX */
	SYSMAP(msgbufmap,msgbuf,MSGBUFPTECNT)
	/* SYSMAP(EMCmap,EMCbase,1) */
	SYSMAP(Npxmap,npxutl,UPAGES)
	SYSMAP(Swtchmap,Swtchbase,UPAGES)
	.set mbxxx,(NMBCLUSTERS*MCLBYTES)
	.set mbyyy,(mbxxx>>PGSHIFT)
	.set mbpgs,(mbyyy+CLSIZE)
	SYSMAP(Mbmap,mbutl,mbpgs)
	/*
	 * XXX: NEED way to compute kmem size from maxusers,
	 * device complement
	 */
	SYSMAP(kmempt,kmembase,300*CLSIZE)
#ifdef	GPROF
	SYSMAP(profmap,profbase,600*CLSIZE)
#endif
	.set	atmemsz,0x100000-0xa0000
	.set	atpgs,(atmemsz>>PGSHIFT)
	SYSMAP(ATDevmem,atdevbase,atpgs)
/*#define USRIOSIZE 30*/
	SYSMAP(Usriomap,usrio,USRIOSIZE+CLSIZE) /* for PHYSIO */
	ZSYSMAP(ekmempt,kmemlimit,0)
	SYSMAP(Usrptmap,usrpt,USRPTSIZE+CLSIZE)

eSysmap:
	# .set	_Syssize,(eSysmap-_Sysmap)/4
	.set	_Syssize,ptes
	.globl	_Syssize

	/* align on next page boundary */
	# . = . + NBPG - 1 & -NBPG	/* align to page boundry-does not work*/
	# .space (PGSIZE - ((eSysmap-_Sysmap) % PGSIZE)) % PGSIZE
	.set sz,(4*ptes)%NBPG
	# .set rptes,(ptes)%1024
	# .set rptes,1024-rptes
	# .set ptes,ptes+rptes
	.set Npdes,10
	# .space (NBPG - sz)

/*
 * Initialization
 */
	.data
	.globl	_cpu, _boothowto, _bootdev, _cyloffset, _Maxmem
_cpu:	.long	0		# are we 386, 386sx, or 486
	.text
	.globl	start
start:				# This is assumed to be location zero!
	movw	$0x1234,%ax
	movw	%ax,0x472	# warm boot
	jmp	1f
	.space	0x500		# skip over warm boot shit

	/* enable a20! yecchh!! */
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

	/* pass parameters on stack (howto, bootdev, unit, cyloffset) */

	movl	4(%esp),%eax
	movl	%eax,_boothowto-SYSTEM
	movl	8(%esp),%eax
	movl	%eax,_bootdev-SYSTEM
	movl	12(%esp),%eax
	movl	%eax, _cyloffset-SYSTEM

	/* count up memory */

	xorl	%eax,%eax		# start with base memory at 0x0
	#movl	$ 0xA0000/NBPG,%ecx	# look every 4K up to 640K
	movl	$ 0xA0,%ecx		# look every 4K up to 640K
1:	movl	0(%eax),%ebx		# save location to check
	movl	$0xa55a5aa5,0(%eax)	# write test pattern
	cmpl	$0xa55a5aa5,0(%eax)	# does not check yet for rollover
	jne	2f
	movl	%ebx,0(%eax)		# restore memory
	addl	$ NBPG,%eax
	loop	1b
2:	shrl	$12,%eax
	movl	%eax,_Maxmem-SYSTEM

	movl	$0x100000,%eax		# next, talley remaining memory
	#movl	$((0xFFF000-0x100000)/NBPG),%ecx
	movl	$(0xFFF-0x100),%ecx
1:	movl	0(%eax),%ebx		# save location to check
	movl	$0xa55a5aa5,0(%eax)	# write test pattern
	cmpl	$0xa55a5aa5,0(%eax)	# does not check yet for rollover
	jne	2f
	movl	%ebx,0(%eax)		# restore memory
	addl	$ NBPG,%eax
	loop	1b
2:	shrl	$12,%eax
	movl	%eax,_Maxmem-SYSTEM

/* clear memory. */
	movl	$_edata-SYSTEM,%edi
	movl	$_end-SYSTEM,%ecx
	addl	$ NBPG-1,%ecx
	andl	$~(NBPG-1),%ecx
	movl	%ecx,%esi
	subl	%edi,%ecx
	addl	$(UPAGES*NBPG)+NBPG+NBPG+NBPG,%ecx
	#	txt+data+proc zero pt+u.
	# any other junk?
	# addl	$ NBPG-1,%ecx
	# andl	$~(NBPG-1),%ecx
	# shrl	$2,%ecx	# convert to long word count
	xorl	%eax,%eax	# pattern
	cld
	rep
	stosb

#ifdef notdef
	/* pass parameters on stack (howto, bootdev, unit, cyloffset) */

	movl	4(%esp),%eax
	movl	%eax,_boothowto-SYSTEM
	movl	8(%esp),%eax
	movl	%eax,_bootdev-SYSTEM
	movl	12(%esp),%eax
	movl	%eax, _cyloffset-SYSTEM


	movl	$0x36000,%edi
	movl	$0x68000,%ecx
	xorl	%eax,%eax	# pattern
	cld
	rep
	stosb

#endif
	movl	$0x100000,%edi
	movl	$0x200000,%ecx
	xorl	%eax,%eax	# pattern
	cld
	rep
	stosb
/*
 * Map Kernel
 * N.B. don't bother with making kernel text RO, as 386
 * ignores R/W AND U/S bits on kernel access (only v works) !
 *
 * First step - build page tables
 */
	movl	%esi,%ecx		# this much memory,
	shrl	$ PGSHIFT,%ecx		# for this many pte s
	movl	$ PG_V,%eax		#  having these bits set,
	movl	$_Sysmap-SYSTEM,%ebx	#   in the kernel page table,
					#    fill in kernel page table.
1:	movl	%eax,0(%ebx)
	addl	$ NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b

/* temporary double map  virt == real */

	movl	$1024,%ecx		# for this many pte s,
	movl	$ PG_V,%eax		#  having these bits set,
	movl	$_Sysmap+4096-SYSTEM,%ebx	#   in the temporary page table,
					#    fill in kernel page table.
1:	movl	%eax,0(%ebx)
	addl	$ NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b

/* map I/O memory map */

	movl	$atpgs,%ecx		# for this many pte s,
	movl	$(IOPHYSmem|PG_V),%eax	#  having these bits set, (perhaps URW?)
	movl	$_ATDevmem-SYSTEM,%ebx	#   in the temporary page table,
					#    fill in kernel page table.
1:	movl	%eax,0(%ebx)
	addl	$ NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b

/* map proc 0's page table (P1 region) */

	movl	$_Usrptmap-SYSTEM,%ebx	# get pt map address
	lea	(0*NBPG)(%esi),%eax	# physical address of pt in proc 0
	orl	$ PG_V,%eax		#  having these bits set,
	movl	%eax,0(%ebx)

 /* map proc 0's _u */

	movl	$ UPAGES,%ecx		# for this many pte s,
	lea	(2*NBPG)(%esi),%eax	# physical address of _u in proc 0
	orl	$ PG_V|PG_URKW,%eax	#  having these bits set,
	lea	(0*NBPG)(%esi),%ebx	# physical address of stack pt in proc 0
	addl	$(UPTEOFF*4),%ebx
					#    fill in proc 0 stack page table.
1:	movl	%eax,0(%ebx)
	addl	$ NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b

 /*# map proc 0's page directory*/
	lea	(1*NBPG)(%esi),%eax	# physical address of ptd in proc 0
	movl	%eax,%edi		# remember ptd physical address
#ifdef dubious
	orl	$ PG_V|PG_URKW,%eax	#  having these bits set,
	lea	(0*NBPG)(%esi),%ebx	# physical address of stack pt in proc 0
	addl	$(UPTEOFF*4),%ebx
	addl	$(UPAGES*4),%ebx
	movl	%eax,0(%ebx)
#endif

/*
 * Construct a page table directory
 * (of page directory elements - pde's)
 */
					/* kernel pde's */
	movl	$_Sysmap-SYSTEM,%eax	# physical address of kernel page table
	orl	$ PG_V,%eax		# pde entry is valid
	movl	$ Npdes,%ecx		# for this many pde s,
	movl	%edi,%ebx		# phys address of ptd in proc 0
	addl	$(SYSPDROFF*4), %ebx	# offset of pde for kernel
1:	movl	%eax,0(%ebx)
	addl	$ NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pde
	loop	1b
					# install a pde for temporary double map
	movl	$_Sysmap+4096-SYSTEM,%eax	# physical address of temp page table
	# movl	$_Sysmap-SYSTEM,%eax	# physical address of temp page table
	orl	$ PG_V,%eax		# pde entry is valid
	movl	%edi,%ebx		# phys address of ptd in proc 0
	movl	%eax,0(%ebx)			# which is where temp maps!
					# install a pde to map _u for proc 0
	lea	(0*NBPG)(%esi),%eax	# physical address of pt in proc 0
	orl	$ PG_V,%eax		# pde entry is valid
	movl	%edi,%ebx		# phys address of ptd in proc 0
	addl	$(UPDROFF*4), %ebx	# offset of pde for kernel
	movl	%eax,0(%ebx)		# which is where _u maps!

	movl	%edi,%eax		# phys address of ptd in proc 0
 	orl	$ I386_CR3PAT,%eax
	movl	%eax,%cr3		# load ptd addr into mmu
	movl	%cr0,%eax		# get control word
	orl	$0x80000001,%eax	# and let s page!
	movl	%eax,%cr0		# NOW!

	pushl	$begin				# jump to high mem!
	ret		# jmp $begin does not work
begin:
	movl	$_Sysbase,%eax		# kernel stack just below system
	movl	%eax,%esp
	xorl	%eax,%eax		# mark end of frames
	movl	%eax,%ebp
	
	movl	_Crtat,%eax		# initialize Crt video ram address
	subl	$ IOPHYSmem,%eax
	addl	$_atdevbase,%eax
	movl	%eax,_Crtat

	call	_init386		# wire 386 chip for unix operation

/* initialize (slightly) the pcb */
	movl	$_u,%eax		# proc0 u-area
	movl	$_usrpt,%ecx
	movl	%ecx,PCB_P0BR(%eax)	# p0br: SVA of text/data user PT
	xorl	%ecx,%ecx
	movl	%ecx,PCB_P0LR(%eax)	# p0lr: 0 (doesn t really exist)
	movl	$_usrpt+NBPG,%ecx	# addr of end of PT
	subl	$ P1PAGES*4,%ecx		# backwards size of P1 region
	movl	%ecx,PCB_P1BR(%eax)	# p1br: P1PAGES from end of PT
	movl	$ P1PAGES-UPAGES,PCB_P1LR(%eax)	# p1lr: vax style
	movl	$ CLSIZE,PCB_SZPT(%eax)	# page table size
	movl	%edi,PCB_CR3(%eax)
	pushl	%edi	# cr3
	movl	%esi,%eax
	addl	$(UPAGES*NBPG)+NBPG+NBPG+NBPG,%eax
	shrl	$ PGSHIFT,%eax
	pushl	%eax	# firstaddr

	pushl	$20		# install signal trampoline code
	pushl	$_u+PCB_SIGC
	pushl	$sigcode
	call	_bcopy
	addl	$12,%esp

	call 	_main

	.globl	__ucodesel,__udatasel
	movzwl	__ucodesel,%eax
	movzwl	__udatasel,%ecx
	# build outer stack frame
	pushl	%ecx		# user ss
	pushl	$_u	# user esp
	pushl	%eax	# user cs
	pushl	$0	# user ip
	movw	%cx,%ds
	movw	%cx,%es
	movw	%ax,%fs		# double map cs to fs
	movw	%cx,%gs		# and ds to gs
	lret	# goto user!

	.globl	__exit
__exit:
	call _reset_cpu
	lidt	xaxa		# invalidate interrupt descriptor
	movl	$0,%esp		# hardware "freeze" fault
	ret
xaxa:	.long	0,0

	.set	exec,11
	.set	exit,1
	.globl	_icode
	.globl	_initflags
	.globl	_szicode
/* gas fucks up offset -- */
#define	LCALL(x,y)	.byte 0x9a ; .long y; .word x
/*
 * Icode is copied out to process 1 to exec /etc/init.
 * If the exec fails, process 1 exits.
 */
_icode:
	# pushl	$argv-_icode
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

init:	.asciz	"/sbin/init"
	.align	2
_initflags:
	.long	0
argv:	.long	init-_icode
	.long	_initflags-_icode
	.long	0
_szicode:
	.long	_szicode-_icode
sigcode:
	movl	12(%esp),%eax	# unsure if call will dec stack 1st
	call	%eax
	xorl	%eax,%eax	# smaller movl $103,%eax
	movb	$103,%al	# sigreturn()
	LCALL(0x7,0)		# enter kernel with args on stack
	hlt			# never gets here


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
	subl	%eax,%eax	# clr eax
	NOP
	inb	%dx,%al
	NOP
	ret

	.globl	_outb
_outb:	movl	4(%esp),%edx
	movl	8(%esp),%eax
	NOP
	outb	%al,%dx
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
	# xorl	%eax,%eax
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

	.globl	_copyout
_copyout:
	movl	$cpyflt,_nofault	# in case we page/protection violate
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
	movl	%eax,_nofault
	ret

	.globl	_copyin
_copyin:
	movl	$cpyflt,_nofault	# in case we page/protection violate
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
	movl	%eax,_nofault
	ret

cpyflt: popl	%edi
	popl	%esi
	xorl	%eax,%eax
	movl	%eax,_nofault
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
	movl	4(%esp),%eax
 	orl	$ I386_CR3PAT,%eax
	movl	%eax,%cr3
	movl	%cr3,%eax
	ret

	# lcr0(cr0)
	.globl	_lcr0
_lcr0:
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
	movl	$fusufault,_nofault	# in case we page/protection violate
	movl	4(%esp),%edx
	.byte	0x65		# use gs
	movl	0(%edx),%eax
	xorl	%edx,%edx
	movl	%edx,_nofault
	ret
	
ENTRY(fusword)
	movl	$fusufault,_nofault	# in case we page/protection violate
	movl	4(%esp),%edx
	.byte	0x65		# use gs
	movzwl	0(%edx),%eax
	xorl	%edx,%edx
	movl	%edx,_nofault
	ret
	
ALTENTRY(fuibyte)
ENTRY(fubyte)
	movl	$fusufault,_nofault	# in case we page/protection violate
	movl	4(%esp),%edx
	.byte	0x65		# use gs
	movzbl	0(%edx),%eax
	xorl	%edx,%edx
	movl	%edx,_nofault
	ret
	
fusufault:
	xorl	%eax,%eax
	movl	%eax,_nofault
	decl	%eax
	ret

ALTENTRY(suiword)
ENTRY(suword)
	movl	$fusufault,_nofault	# in case we page/protection violate
	movl	4(%esp),%edx
	movl	8(%esp),%eax
	.byte	0x65		# use gs
	movl	%eax,0(%edx)
	xorl	%eax,%eax
	movl	%eax,_nofault
	ret
	
ENTRY(susword)
	movl	$fusufault,_nofault	# in case we page/protection violate
	movl	4(%esp),%edx
	movl	8(%esp),%eax
	.byte	0x65		# use gs
	movw	%ax,0(%edx)
	xorl	%eax,%eax
	movl	%eax,_nofault
	ret

ALTENTRY(suibyte)
ENTRY(subyte)
	movl	$fusufault,_nofault	# in case we page/protection violate
	movl	4(%esp),%edx
	movl	8(%esp),%eax
	.byte	0x65		# use gs
	movb	%eax,0(%edx)
	xorl	%eax,%eax
	movl	%eax,_nofault
	ret

	ALTENTRY(savectx)
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
sw01:	.asciz	"swtch1"
sw02:	.asciz	"swtch2"

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
	movw	_cpl, %ax
	movw	%ax, _u+PCB_IML
	movl	$1,%eax
	movl	%eax,_noproc
	incl	_cnt+V_SWTCH
sw1:
	cli
	bsfl	_whichqs,%eax	# find a full q
	jz	idle		# if none, idle
swfnd:
	btrl	%eax,_whichqs	# clear q full status
	jnb	sw1		# if it was clear, look for another
	pushl	%eax		# save which one we are using
	shll	$3,%eax
	addl	$_qs,%eax	# select q
	pushl	%eax

	cmpl	P_LINK(%eax),%eax	# linked to self? (e.g. not on list)
	je	badsw		# not possible
	movl	P_LINK(%eax),%ecx	# unlink from front of process q
	movl	P_LINK(%ecx),%edx
	movl	%edx,P_LINK(%eax)
	movl	P_RLINK(%ecx),%eax
	movl	%eax,P_RLINK(%edx)

	popl	%eax
	popl	%edx
	cmpl	P_LINK(%ecx),%eax	# q empty
	je	sw2
	btsl	%edx,_whichqs		# nope, indicate full
sw2:
	movl	$0,%eax
	movl	%eax,_noproc
	movl	%eax,_runrun
	cmpl	$0,P_WCHAN(%ecx)
	jne	badsw
	cmpb	$ SRUN,P_STAT(%ecx)
	jne	badsw
	movl	%eax,P_RLINK(%ecx)

	movl	P_ADDR(%ecx),%edx
	movl	(%edx),%eax
	movl	%eax,_Swtchmap
	# movl	4(%edx),%eax
	# movl	%eax,_Swtchmap+4
	# movl	%cr3,%eax
	# movl	%eax,%cr3
	movl	_Swtchbase+PCB_CR3,%edx

/* switch to new process. first, save context as needed */

	movl	$_u,%ecx

	movl	(%esp),%eax		# Hardware registers
	movl	%eax, PCB_EIP(%ecx)
	movl	%ebx, PCB_EBX(%ecx)
	movl	%esp, PCB_ESP(%ecx)
	movl	%ebp, PCB_EBP(%ecx)
	movl	%esi, PCB_ESI(%ecx)
	movl	%edi, PCB_EDI(%ecx)

	fsave	PCB_SAVEFPU(%ecx)

	movl	_CMAP2,%eax		# save temporary map PTE
	movl	%eax,PCB_CMAP2(%ecx)	# in our context

 	orl	$ I386_CR3PAT,%edx
	movl	%edx,%cr3	# context switch

	movl	$_u,%ecx

/* restore context */
	movl	PCB_EBX(%ecx), %ebx
	movl	PCB_ESP(%ecx), %esp
	movl	PCB_EBP(%ecx), %ebp
	movl	PCB_ESI(%ecx), %esi
	movl	PCB_EDI(%ecx), %edi
	movl	PCB_EIP(%ecx), %eax
	movl	%eax, (%esp)

	frstor	PCB_SAVEFPU(%ecx)

	movl	PCB_CMAP2(%ecx),%eax	# get temporary map
	movl	%eax,_CMAP2		# reload temporary map PTE
#ifdef FPUNOTYET
#endif
	cmpl	$0,PCB_SSWAP(%ecx)	# do an alternate return?
	jne	res3			# yes, go reload regs

	# pushl	PCB_IML(%ecx)
	# call	_splx
	# popl	%eax
	call _spl0
	movl	$0,%eax
	ret

res3:
	xorl	%eax,%eax		# inline restore context
	xchgl	PCB_SSWAP(%ecx),%eax	# addr of saved context, clear it

	movl	 0(%eax),%ebx		# restore ebx
	movl	 4(%eax),%esp		# restore esp
	movl	 8(%eax),%ebp		# restore ebp
	movl	12(%eax),%esi		# restore esi
	movl	16(%eax),%edi		# restore edi
	movl	20(%eax),%edx		# get rta
	movl	%edx,(%esp)		# put in return frame

	# call	_spl0
	pushl	_u+PCB_IML
	call	_splx
	popl	%eax

	xorl	%eax,%eax		# return (1);
	incl	%eax
	ret

/*
 * Resume(p_addr)
 * current just used to fillout u. tss so fork can fake a return to swtch
 * [ all thats really needed is esp and eip ]
 */
ENTRY(resume)
	# movl	4(%esp),%ecx
	movl	$_u,%ecx
	movw	_cpl, %ax
	movw	%ax,  PCB_IML(%ecx)
	movl	(%esp),%eax	
	movl	%eax, PCB_EIP(%ecx)
	movl	%ebx, PCB_EBX(%ecx)
	movl	%esp, PCB_ESP(%ecx)
	movl	%ebp, PCB_EBP(%ecx)
	movl	%esi, PCB_ESI(%ecx)
	movl	%edi, PCB_EDI(%ecx)
#ifdef FPUNOTYET
#endif
	fsave	PCB_SAVEFPU(%ecx)
	movl	$0,%eax
	ret


.data
	.globl	_cyloffset
_cyloffset:	.long	0
	.globl	_nofault
_nofault:	.long	0
.text
 # To be done:
	.globl _addupc
	.globl _astoff
	.globl _doadump
	.globl _inittodr
	.globl _physaddr
_addupc:
	.byte 0xcc
_astoff:
	ret
_doadump:
	.byte 0xcc
_physaddr:
	.byte 0xcc

#define	IDTVEC(name)	.align 4; .globl _X/**/name; _X/**/name:
/*#define	PANIC(msg)	xorl %eax,%eax; movl %eax,_waittime; pushl 1f; \
			call _panic; 1: .asciz msg*/
#define	PRINTF(n,msg)	pushal ; pushl 1f; call _printf; MSG(msg) ; popl %eax ; popal
#define	MSG(msg)	.data; 1: .asciz msg; .text

	.text

/*
 * Trap and fault vector routines
 */ 
#define	TRAP(a)	pushl $ a; jmp alltraps

IDTVEC(div)
	pushl $0; TRAP(T_DIVIDE)
IDTVEC(dbg)
	pushl $0; TRAP(T_TRCTRAP)
IDTVEC(nmi)
	pushl $0; TRAP(T_NMI)
IDTVEC(bpt)
	pushl $0; TRAP(T_BPTFLT)
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
	incl	_cnt+V_TRAP
	call	_trap

#ifdef junk
	cmpl	$0xfc000000,12*4(%esp)	# is it a user pc
	ja	1f 
	cmpw	$0x1f,13*4(%esp)	# is it a user cs
	je	1f 
	.data	; lx: .asciz "t user cs %x?" ; .text
2:
	movl	13*4(%esp),%eax
	pushl	%eax
	pushl	$lx
	call	_pg
	popl %eax ; popl %eax
	jmp	2b
1:
#endif junk

	pop %es
	pop %ds
	popal
	addl	$8,%esp			# pop type, code
	iret

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

#ifdef notdef
	cmpw	$0x1f,10*4(%esp)	# is user cs what it should be?
	jz	1f
	.data	; lz: .asciz "s user cs %x?" ; .text
2:
	movl	10*4(%esp),%eax
	pushl	%eax
	pushl	$lz
	call	_pg
	jmp	2b
1:
#endif

	movw	__udatasel,%ax	# switch back to user segments
	movw	%ax,%ds
	movw	%ax,%es
	popal
	popfl
	lret			# back we go, we hope!

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
