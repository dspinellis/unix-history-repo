/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)locore.s	1.1 (Berkeley) %G%
 */

#include "psl.h"
#include "pte.h"

#include "errno.h"
#include "cmap.h"

#include "../i386/trap.h"
/*#include "cpu.h"*/
/*#include "clock.h"*/


	.set	IDXSHIFT,10
	.set	SYSTEM,0xFE000000	# virtual address of system start
	/*note: gas copys sign bit (e.g. arithmetic >>), can't do SYSTEM>>22! */
	.set	SYSPDROFF,0x3F8		# Page dir

	.set	IOPHYSmem,0xa0000

/*
 * User structure is UPAGES at top of user space.
 */
	.set	_u,0xFDFFE000
	.globl	_u
	.set	UPDROFF,0x3F7
	.set	UPTEOFF,0x3FE

#ifdef badidea
/*
 * I/O Memory Map is 0xfffa000-0xffffffff (virtual == real)
 */
	.set	_IOmembase,0xFFFA0000
	.globl	_IOmembase
	.set	IOPDROFF,0x3FF
	.set	IOPTEOFF,0x3A0

#endif

/*
 * System page table
 * Mbmap and Usrptmap are enlarged by CLSIZE entries
 * as they are managed by resource maps starting with index 1 or CLSIZE.
 */ 
#define	SYSMAP(mname, vname, npte)		\
_##mname:	.globl	_##mname;		\
	.space	(npte)*4;			\
	.set	_##vname,ptes*4096+SYSTEM;	\
	.globl	_##vname;			\
	.set	ptes,ptes + npte
#define	ZSYSMAP(mname, vname, npte)		\
_##mname:	.globl	_##mname;		\
	.set	_##vname,ptes*4096+SYSTEM;	\
	.globl	_##vname;

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
	.set Npdes,1
	.space (NBPG - sz)

	# SYSMAP(Tmap,tmap,1024)
_tMap:
	.space	NBPG
_PDR:
	# SYSMAP(PDR,pdr,1024)
	.space	NBPG

/*
 * Initialization
 */
	.data
	.globl	_cpu
_cpu:	.long	0	# are we 386, 386sx, or 486
	.text
	.globl	start
start:
#ifdef notyet
	# XXX pass parameters on stack
/* count up memory */
	xorl	%eax,%eax		# start with base memory at 0x0
	movl	$(0xA0000/NBPG),%ecx	# look every 4K up to 640K
1:	movl	0(%eax),%ebx		# save location to check
	movl	$0xa55a5aa5,0(%eax)	# write test pattern
	cmpl	$0xa55a5aa5,0(%eax)	# does not check yet for rollover
	jne	2f
	movl	%ebx,0(%eax)		# restore memory
	addl	$NBPG,%eax
	loop	1b
2:	movl	%eax,_basemem-SYSTEM

	movl	$0x100000,%eax		# next, talley remaining memory
	movl	$((0xFA0000-0x100000)/NBPG),%ecx
1:	movl	0(%eax),%ebx		# save location to check
	movl	$0xa55a5aa5,0(%eax)	# write test pattern
	cmpl	$0xa55a5aa5,0(%eax)	# does not check yet for rollover
	jne	2f
	movl	%ebx,0(%eax)		# restore memory
	addl	$NBPG,%eax
	loop	1b
2:	movl	%eax,_abovemem-SYSTEM

#endif notyet
/* clear memory. is this redundant ? */
	movl	$_edata-SYSTEM,%edi
	movl	$_end-SYSTEM,%ecx
	addl	$NBPG-1,%ecx
	andl	$~(NBPG-1),%ecx
	movl	%ecx,%esi
	subl	%edi,%ecx
	addl	$(UPAGES*NBPG)+NBPG+NBPG+NBPG,%ecx
	#	txt+data+proc zero pt+u.
	# any other junk?
	addl	$NBPG-1,%ecx
	andl	$~(NBPG-1),%ecx
	# shrl	$2,%ecx	# convert to long word count
	xorl	%eax,%eax	# pattern
	cld
	rep
	stosb

/*
 * Map Kernel
 * N.B. don't bother with making kernel text RO, as 386
 * ignores R/W bit on kernel access!
 */
	# movl	$_Syssize,%ecx		# for this many pte s,
	movl	%esi,%ecx		# this much memory,
	shrl	$PGSHIFT,%ecx		# for this many pte s
	movl	$PG_V,%eax		#  having these bits set,
	movl	$_Sysmap-SYSTEM,%ebx	#   in the kernel page table,
					#    fill in kernel page table.
1:	movl	%eax,0(%ebx)
	addl	$NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b

/* temporary double map  virt == real */

	movl	$1024,%ecx		# for this many pte s,
	movl	$PG_V,%eax		#  having these bits set,
	movl	$_tMap-SYSTEM,%ebx	#   in the temporary page table,
					#    fill in kernel page table.
1:	movl	%eax,0(%ebx)
	addl	$NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b

#ifdef badidea
/* map I/O memory virt == real */

	movl	$(1024-IOPTEOFF),%ecx	# for this many pte s,
	movl	$(_IOmembase|PG_V),%eax	#  having these bits set, (perhaps URW?)
	movl	$_IOMap-SYSTEM,%ebx	#   in the temporary page table,
	addl	$(IOPTEOFF*4),%ebx
					#    fill in kernel page table.
1:	movl	%eax,0(%ebx)
	addl	$NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b
#endif

/* map I/O memory map */

	movl	$atpgs,%ecx		# for this many pte s,
	movl	$(IOPHYSmem|PG_V),%eax	#  having these bits set, (perhaps URW?)
	movl	$_ATDevmem-SYSTEM,%ebx	#   in the temporary page table,
					#    fill in kernel page table.
1:	movl	%eax,0(%ebx)
	addl	$NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b

/*# map proc 0's page table*/
	movl	$_Usrptmap-SYSTEM,%ebx	# get pt map address
	lea	(0*NBPG)(%esi),%eax	# physical address of pt in proc 0
	orl	$PG_V,%eax		#  having these bits set,
	movl	%eax,0(%ebx)

 /*# map proc 0's _u*/
	movl	$UPAGES,%ecx		# for this many pte s,
	lea	(1*NBPG)(%esi),%eax	# physical address of _u in proc 0
	orl	$PG_V|PG_URKW,%eax	#  having these bits set,
	lea	(0*NBPG)(%esi),%ebx	# physical address of stack pt in proc 0
	addl	$(UPTEOFF*4),%ebx
					#    fill in proc 0 stack page table.
1:	movl	%eax,0(%ebx)
	addl	$NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pte
	loop	1b

/*
 * Construct a page table directory
 * (of page directory elements - pde's)
 */
					/* kernel pde's */
	movl	$_Sysmap-SYSTEM,%eax	# physical address of kernel page table
	orl	$PG_V,%eax		# pde entry is valid
	movl	$Npdes,%ecx		# for this many pde s,
	movl	$_PDR-SYSTEM,%ebx		# address of start of ptd
	# lea	(2*NBPG)(%esi),%ebx	# address of ptd in proc 0 pt
	addl	$(SYSPDROFF*4), %ebx	# offset of pde for kernel
1:	movl	%eax,0(%ebx)
	addl	$NBPG,%eax			# increment physical address
	addl	$4,%ebx				# next pde
	loop	1b
					# install a pde for temporary double map
	movl	$_tMap-SYSTEM,%eax	# physical address of temp page table
	orl	$PG_V,%eax		# pde entry is valid
	movl	$_PDR-SYSTEM,%ebx		# address of start of ptd
	# lea	(2*NBPG)(%esi),%ebx	# address of ptd in proc 0 pt
	movl	%eax,0(%ebx)			# which is where temp maps!
#ifdef badidea
					# install a pde for IO memory
	movl	$_IOMap-SYSTEM,%eax	# physical address of temp page table
	orl	$PG_V,%eax		# pde entry is valid
	movl	$_PDR-SYSTEM,%ebx		# address of start of ptd
	# lea	(2*NBPG)(%esi),%ebx	# address of ptd in proc 0 pt
	addl	$(IOPDROFF*4), %ebx	# offset of pde for kernel
	movl	%eax,0(%ebx)			# which is where temp maps!
#endif
					# install a pde to map _u for proc 0
	lea	(0*NBPG)(%esi),%eax	# physical address of pt in proc 0
	orl	$PG_V,%eax		# pde entry is valid
	movl	$_PDR-SYSTEM,%ebx		# address of start of ptd
	# lea	(2*NBPG)(%esi),%ebx	# address of ptd in proc 0 pt
	addl	$(UPDROFF*4), %ebx	# offset of pde for kernel
	movl	%eax,0(%ebx)		# which is where _u maps!

	# movl	%eax,_PDR-SYSTEM+(1024-16-1)*4
	# movl	$UDOT,%eax
	# shrl	$PGSHIFT+IDXSHF,%eax
	# shll	$2,%eax
	# addl	$_PDR-SYSTEM,%eax
	# orl	$PG_V,%eax

	movl	$_PDR-SYSTEM,%eax		# address of start of ptd
	# lea	(2*NBPG)(%esi),%eax	# address o ptd in proc 0 pt
	movl	%eax,%cr3			# load ptd addr into mmu
	movl	$0x80000001,%eax		# and let s page!
	movl	%eax,%cr0			# NOW!

	pushl	$begin				# jump to high mem!
	ret
begin:
	movl	$_u+UPAGES*NBPG-4,%eax
	movl	%eax,%esp
	movl	%eax,%ebp
	movl	_Crtat,%eax
	subl	$IOPHYSmem,%eax
	addl	$_atdevbase,%eax
	movl	%eax,_Crtat
	# call	_init386
	call 	_main
	movw	$0x1234,%ax
	movw	%ax,0x472	# warm boot
	lidt	xaxa
	movl	$0,%esp		# segment violation
	ret
xaxa:	.long	0,0

#ifdef	newway

#define	P(x)	(x-SYSTEM)
#define	PTE(b,o,p) \
		# build a pte pointing to physical p; leave it at loc b+o \	
		movl	p,%eax \
		andl	$0xfffff000,%eax \
		orl	$PG_V,%eax \
		movl	%eax,b(,o,4)

#define	PDE(d, v, p) \
		# build a pde at virtual addr v, pointing to physical pte p \
		movl	v,%edx			\
		andl	$0xffc00000,%edx	\
		shrl	$PGSHIFT+IDXSHFT,%edx	\
		PTE(d, %edx, p)

/* Initialize Sysmap */

	movl	$Syssize,%ecx	# this many pte s
	xorl	%ebx,%ebx	# starting at physical 0
	xorl	%edx,%edx	# starting at virtual XX000000
1:
	PTE(P(SysMap,%edx,%ebx)
	incl	%edx
	addl	$PGSIZE,%edx
	loop 1b

/* Initialize Proc 0 page table  map */
/* Initialize Udot map */

	movl	$UPAGES,%ecx	# this many pte s
	movl	$P(_end),%ebx
	addl	$PGSIZE-1,%ebx
	andl	$PGMASK,%ebx
	xorl	%ebx,%ebx	# starting at physical 0
	movl	$_u,%edx	# starting at virtual _u
1:
	PTE(P(SysMap,%edx,%ebx)
	incl	%edx
	addl	$PGSIZE,%edx
	loop 1b

/* Initialize page table directory */
	zero all entries
	PTD(P(_ptd), 0, P(SysMap))	# bottom double mapped to system
	PTD(P(_ptd), SYSTEM, P(SysMap))	# system location
	PTD(P(_ptd), _u, P(_end))	# udot&ptd

9:
/* clear memory from kernel bss and pages for proc 0 u. and page table */
	lea	_edata-SYSTEM,r6
	lea	_end-SYSTEM,r5
	bisl3	$SYSTEM,r5,r9			# convert to virtual address
	addl2	$NBPG-1,r9			# roundup to next page
	addl2	$(UPAGES*NBPG)+NBPG+NBPG,r5
1:	clrq	(r6); acbl r5,$8,r6,1b
/* initialize system page table: uba vectors and int stack writeable */
	clrl	r2
	movab	eintstack,r1; bbcc $31,r1,1f;
1:	bisl3	$PG_V|PG_KW,r2,_Sysmap[r2]; aoblss r1,r2,1b
/* make kernel text space read-only */
	movab	_etext+NBPG-1,r1; bbcc $31,r1,1f;
1:	bisl3	$PG_V|PG_URKR,r2,_Sysmap[r2]; aoblss r1,r2,1b
/* make kernel data, bss, read-write */
	bicl3	$SYSTEM,r9,r1; ashl $-PGSHIFT,r1,r1
1:	bisl3	$PG_V|PG_KW,r2,_Sysmap[r2]; aoblss r1,r2,1b
/* now go to mapped mode */
	mtpr	$0,$TBIA; mtpr $1,$MAPEN; jmp *$0f; 0:
/* init mem sizes */
	ashl	$-PGSHIFT,r7,_maxmem
	movl	_maxmem,_physmem
	movl	_maxmem,_freemem
/* setup context for proc[0] == Scheduler */
	bicl3	$SYSTEM|(NBPG-1),r9,r6	# make phys, page boundary
/* setup page table for proc[0] */
	ashl	$-PGSHIFT,r6,r3			# r3 = btoc(r6)
	bisl3	$PG_V|PG_KW,r3,_Usrptmap	# init first upt entry
	incl	r3
	movab	_usrpt,r0
	mtpr	r0,$TBIS
/* init p0br, p0lr */
	mtpr	r0,$P0BR
	mtpr	$0,$P0LR
/* double map the kernel into the virtual user addresses of phys mem */
	mtpr	$_Sysmap,$P0BR
	mtpr	$_Syssize,$P0LR
/* init p1br, p1lr */
	movab	NBPG(r0),r0
	movl	$0x200000-UPAGES,r1
	mtpr	r1,$P1LR
	mnegl	r1,r1
	moval	-4*UPAGES(r0)[r1],r2
	mtpr	r2,$P1BR
/* setup mapping for UPAGES of _u */
	movl	$UPAGES,r2; movab _u+NBPG*UPAGES,r1; addl2 $UPAGES,r3; jbr 2f
1:	decl	r3
	moval	-NBPG(r1),r1;
	bisl3	$PG_V|PG_URKW,r3,-(r0)
	mtpr	r1,$TBIS
2:	sobgeq	r2,1b
/* initialize (slightly) the pcb */
	movab	UPAGES*NBPG(r1),PCB_KSP(r1)
	mnegl	$1,PCB_ESP(r1)
	mnegl	$1,PCB_SSP(r1)
	movl	r1,PCB_USP(r1)
	mfpr	$P0BR,PCB_P0BR(r1)
	mfpr	$P0LR,PCB_P0LR(r1)
	movb	$4,PCB_P0LR+3(r1)		# disable ast
	mfpr	$P1BR,PCB_P1BR(r1)
	mfpr	$P1LR,PCB_P1LR(r1)
	movl	$CLSIZE,PCB_SZPT(r1)		# init u.u_pcb.pcb_szpt
	movl	r9,PCB_R9(r1)
	movl	r10,PCB_R10(r1)
	movl	r11,PCB_R11(r1)
	movab	1f,PCB_PC(r1)			# initial pc
	clrl	PCB_PSL(r1)			# mode(k,k), ipl=0
	ashl	$PGSHIFT,r3,r3
	mtpr	r3,$PCBB			# first pcbb
/* set regs, p0br, p0lr, p1br, p1lr, astlvl, ksp and change to kernel mode */
	ldpctx
	rei
/* put signal trampoline code in u. area */
1:	movab	_u,r0
	movc3	$19,sigcode,PCB_SIGC(r0)
/* save boot device in global _bootdev */
	movl	r10,_bootdev
/* save reboot flags in global _boothowto */
	movl	r11,_boothowto
#ifdef KADB
/* save end of symbol & string table in global _bootesym */
	subl3	$NBPG-1,r9,_bootesym
#endif
/* calculate firstaddr, and call main() */
	bicl3	$SYSTEM,r9,r0; ashl $-PGSHIFT,r0,-(sp)
	addl2	$UPAGES+1,(sp); calls $1,_main
/* proc[1] == /etc/init now running here; run icode */
	pushl	$PSL_CURMOD|PSL_PRVMOD; pushl $0; rei

/* signal trampoline code: it is known that this code takes exactly 19 bytes */
/* in ../vax/pcb.h and in the movc3 above */
sigcode:
	calls	$4,8(pc)	# params pushed by sendsig
	movl	sp,ap		# calls frame built by sendsig
	chmk	$103		# cleanup mask and onsigstack
	halt			# sigreturn() does not return!
	.word	0x3f		# registers 0-5
	callg	(ap),*16(ap)	# call the signal handler
	ret			# return to code above

	.set	exec,11
	.set	exit,1
	.globl	_icode
	.globl	_initflags
	.globl	_szicode
/*
 * Icode is copied out to process 1 to exec /etc/init.
 * If the exec fails, process 1 exits.
 */
_icode:
	pushab	b`argv-l0(pc)
l0:	pushab	b`init-l1(pc)
l1:	pushl	$2
	movl	sp,ap
	chmk	$exec
	pushl	r0
	chmk	$exit

init:	.asciz	"/sbin/init"
	.align	2
_initflags:
	.long	0
argv:	.long	init+6-_icode
	.long	_initflags-_icode
	.long	0
_szicode:
	.long	_szicode-_icode

/*
 * Primitives
 */ 

#ifdef GPROF
#define	ENTRY(name, regs) \
	.globl _##name; .align 1; _##name: .word regs; jsb mcount
#else
#define	ENTRY(name, regs) \
	.globl _##name; .align 1; _##name: .word regs
#endif GPROF
#define R0 0x01
#define R1 0x02
#define R2 0x04
#define R3 0x08
#define R4 0x10
#define R5 0x20
#define R6 0x40

/*
 * badaddr(addr, len)
 *	see if access addr with a len type instruction causes a machine check
 *	len is length of access (1=byte, 2=short, 4=long)
 */
	.globl	_badaddr
_badaddr:
	.word	0
	movl	$1,r0
	mfpr	$IPL,r1
	mtpr	$HIGH,$IPL
	movl	4(ap),r3
	movl	8(ap),r4
	movab	2f,nofault		# jump to 2f on machcheck
	bbc	$0,r4,1f; tstb	(r3)
1:	bbc	$1,r4,1f; tstw	(r3)
1:	bbc	$2,r4,1f; tstl	(r3)
1:	clrl	r0			# made it w/o machine checks
2:	clrl	nofault
	mtpr	r1,$IPL
	ret

/*
 * update profiling information for the user
 * addupc(pc, &u.u_prof, ticks)
 */
ENTRY(addupc, 0)
	movl	8(ap),r2		# &u.u_prof
	subl3	8(r2),4(ap),r0		# corrected pc
	blss	9f
	extzv	$1,$31,r0,r0		# logical right shift
	extzv	$1,$31,12(r2),r1	# ditto for scale
	emul	r1,r0,$0,r0
	ashq	$-14,r0,r0
	tstl	r1
	bneq	9f
	bicl2	$1,r0
	cmpl	r0,4(r2)		# length
	bgequ	9f
	addl2	(r2),r0			# base
	probew	$3,$2,(r0)
	beql	8f
	addw2	12(ap),(r0)
9:
	ret
8:
	clrl	12(r2)
	ret

/*
 * Copy a null terminated string from the user address space into
 * the kernel address space.
 *
 * copyinstr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copyinstr, R6)
	movl	12(ap),r6		# r6 = max length
	jlss	8f
	movl	4(ap),r1		# r1 = user address
	bicl3	$~(NBPG*CLSIZE-1),r1,r2	# r2 = bytes on first page
	subl3	r2,$NBPG*CLSIZE,r2
	movl	8(ap),r3		# r3 = kernel address
1:
	cmpl	r6,r2			# r2 = min(bytes on page, length left);
	jgeq	2f
	movl	r6,r2
2:
	prober	$3,r2,(r1)		# bytes accessible?
	jeql	8f
	subl2	r2,r6			# update bytes left count
#ifdef NOSUBSINST
	# fake the locc instr. for processors that don t have it
	movl	r2,r0
6:
	tstb	(r1)+
	jeql	5f
	sobgtr	r0,6b
	jbr	7f
5:
	decl	r1
	jbr	3f
7:
#else
	locc	$0,r2,(r1)		# null byte found?
	jneq	3f
#endif
	subl2	r2,r1			# back up pointer updated by `locc 
	movc3	r2,(r1),(r3)		# copy in next piece
	movl	$(NBPG*CLSIZE),r2	# check next page
	tstl	r6			# run out of space?
	jneq	1b
	movl	$ENOENT,r0		# set error code and return
	jbr	9f
3:
	tstl	16(ap)			# return length?
	beql	4f
	subl3	r6,12(ap),r6		# actual len = maxlen - unused pages
	subl2	r0,r6			#	- unused on this page
	addl3	$1,r6,*16(ap)		#	+ the null byte
4:
	subl2	r0,r2			# r2 = number of bytes to move
	subl2	r2,r1			# back up pointer updated by `locc 
	incl	r2			# copy null byte as well
	movc3	r2,(r1),(r3)		# copy in last piece
	clrl	r0			# redundant
	ret
8:
	movl	$EFAULT,r0
9:
	tstl	16(ap)
	beql	1f
	subl3	r6,12(ap),*16(ap)
1:
	ret

/*
 * Copy a null terminated string from the kernel
 * address space to the user address space.
 *
 * copyoutstr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copyoutstr, R6)
	movl	12(ap),r6		# r6 = max length
	jlss	8b
	movl	4(ap),r1		# r1 = kernel address
	movl	8(ap),r3		# r3 = user address
	bicl3	$~(NBPG*CLSIZE-1),r3,r2	# r2 = bytes on first page
	subl3	r2,$NBPG*CLSIZE,r2
1:
	cmpl	r6,r2			# r2 = min(bytes on page, length left);
	jgeq	2f
	movl	r6,r2
2:
	probew	$3,r2,(r3)		# bytes accessible?
	jeql	8b
	subl2	r2,r6			# update bytes left count
#ifdef NOSUBSINST
	# fake the locc instr. for processors that don t have it
	movl	r2,r0
6:
	tstb	(r1)+
	jeql	5f
	sobgtr	r0,6b
	jbr	7f
5:
	decl	r1
	jbr	3b
7:
#else
	locc	$0,r2,(r1)		# null byte found?
	jneq	3b
#endif
	subl2	r2,r1			# back up pointer updated by `locc 
	movc3	r2,(r1),(r3)		# copy in next piece
	movl	$(NBPG*CLSIZE),r2	# check next page
	tstl	r6			# run out of space?
	jneq	1b
	movl	$ENOENT,r0		# set error code and return
	jbr	9b

/*
 * Copy a null terminated string from one point to another in
 * the kernel address space.
 *
 * copystr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copystr, R6)
	movl	12(ap),r6		# r6 = max length
	jlss	8b
	movl	4(ap),r1		# r1 = src address
	movl	8(ap),r3		# r3 = dest address
1:
	movzwl	$65535,r2		# r2 = bytes in first chunk
	cmpl	r6,r2			# r2 = min(bytes in chunk, length left);
	jgeq	2f
	movl	r6,r2
2:
	subl2	r2,r6			# update bytes left count
#ifdef NOSUBSINST
	# fake the locc instr. for processors that don t have it
	movl	r2,r0
6:
	tstb	(r1)+
	jeql	5f
	sobgtr	r0,6b
	jbr	7f
5:
	decl	r1
	jbr	3b
7:
#else
	locc	$0,r2,(r1)		# null byte found?
	jneq	3b
#endif
	subl2	r2,r1			# back up pointer updated by `locc 
	movc3	r2,(r1),(r3)		# copy in next piece
	tstl	r6			# run out of space?
	jneq	1b
	movl	$ENOENT,r0		# set error code and return
	jbr	9b

/* 
 * Copy specified amount of data from user space into the kernel
 * Copyin(from, to, len)
 *	r1 == from (user source address)
 *	r3 == to (kernel destination address)
 *	r5 == length
 */
	.align	1
JSBENTRY(Copyin, R1|R3|R5)
	cmpl	r5,$(NBPG*CLSIZE)	# probing one page or less ?
	bgtru	1f			# no
	prober	$3,r5,(r1)		# bytes accessible ?
	beql	ersb			# no
	movc3	r5,(r1),(r3)
/*	clrl	r0			# redundant */
	rsb
1:
	blss	ersb			# negative length?
	pushl	r6			# r6 = length
	movl	r5,r6
	bicl3	$~(NBPG*CLSIZE-1),r1,r0	# r0 = bytes on first page
	subl3	r0,$(NBPG*CLSIZE),r0
	addl2	$(NBPG*CLSIZE),r0	# plus one additional full page
	jbr	2f

ciloop:
	movc3	r0,(r1),(r3)
	movl	$(2*NBPG*CLSIZE),r0	# next amount to move
2:
	cmpl	r0,r6
	bleq	3f
	movl	r6,r0
3:
	prober	$3,r0,(r1)		# bytes accessible ?
	beql	ersb1			# no
	subl2	r0,r6			# last move?
	bneq	ciloop			# no

	movc3	r0,(r1),(r3)
/*	clrl	r0			# redundant */
	movl	(sp)+,r6		# restore r6
	rsb

ersb1:
	movl	(sp)+,r6		# restore r6
ersb:
	movl	$EFAULT,r0
	rsb

/* 
 * Copy specified amount of data from kernel to the user space
 * Copyout(from, to, len)
 *	r1 == from (kernel source address)
 *	r3 == to (user destination address)
 *	r5 == length
 */
	.align	1
JSBENTRY(Copyout, R1|R3|R5)
	cmpl	r5,$(NBPG*CLSIZE)	# moving one page or less ?
	bgtru	1f			# no
	probew	$3,r5,(r3)		# bytes writeable?
	beql	ersb			# no
	movc3	r5,(r1),(r3)
/*	clrl	r0			# redundant */
	rsb
1:
	blss	ersb			# negative length?
	pushl	r6			# r6 = length
	movl	r5,r6
	bicl3	$~(NBPG*CLSIZE-1),r3,r0	# r0 = bytes on first page
	subl3	r0,$(NBPG*CLSIZE),r0
	addl2	$(NBPG*CLSIZE),r0	# plus one additional full page
	jbr	2f

coloop:
	movc3	r0,(r1),(r3)
	movl	$(2*NBPG*CLSIZE),r0	# next amount to move
2:
	cmpl	r0,r6
	bleq	3f
	movl	r6,r0
3:
	probew	$3,r0,(r3)		# bytes writeable?
	beql	ersb1			# no
	subl2	r0,r6			# last move?
	bneq	coloop			# no

	movc3	r0,(r1),(r3)
/*	clrl	r0			# redundant */
	movl	(sp)+,r6		# restore r6
	rsb

/*
 * non-local goto s
 */
#ifdef notdef		/* this is now expanded completely inline */
	.align	1
JSBENTRY(Setjmp, R0)
	movl	fp,(r0)+	# current stack frame
	movl	(sp),(r0)	# resuming pc
	clrl	r0
	rsb
#endif

#define PCLOC 16	/* location of pc in calls frame */
#define APLOC 8		/* location of ap,fp in calls frame */
	.align	1
JSBENTRY(Longjmp, R0)
	movl	(r0)+,newfp	# must save parameters in memory as all
	movl	(r0),newpc	# registers may be clobbered.
1:
	cmpl	fp,newfp	# are we there yet?
	bgequ	2f		# yes
	moval	1b,PCLOC(fp)	# redirect return pc to us!
	ret			# pop next frame
2:
	beql	3f		# did we miss our frame?
	pushab	4f		# yep ?!?
	calls	$1,_panic
3:
	movl	newpc,r0	# all done, just return to the `setjmp 
	jmp	(r0)		# rsb

	.data
newpc:	.space	4
newfp:	.space	4
4:	.asciz	"longjmp"
	.text
/*
 * setjmp that saves all registers as the call frame may not
 * be available to recover them in the usual mannor by longjmp.
 * Called before swapping out the u. area, restored by resume()
 * below.
 */
ENTRY(savectx, 0)
	movl	4(ap),r0
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	APLOC(fp),(r0)+	# save ap, fp
	addl3	$8,ap,(r0)+	# save sp
	movl	PCLOC(fp),(r0)	# save pc
	clrl	r0
	ret

#ifdef KADB
/*
 * C library -- reset, setexit
 *
 *	reset(x)
 * will generate a "return" from
 * the last call to
 *	setexit()
 * by restoring r6 - r12, ap, fp
 * and doing a return.
 * The returned value is x; on the original
 * call the returned value is 0.
 */
ENTRY(setexit, 0)
	movab	setsav,r0
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	8(fp),(r0)+		# ap, fp
	movab	4(ap),(r0)+		# sp
	movl	16(fp),(r0)		# pc
	clrl	r0
	ret

ENTRY(reset, 0)
	movl	4(ap),r0	# returned value
	movab	setsav,r1
	movq	(r1)+,r6
	movq	(r1)+,r8
	movq	(r1)+,r10
	movq	(r1)+,r12
	movl	(r1)+,sp
	jmp 	*(r1)

	.data
	.align  2
setsav:	.space	10*4
	.text
#endif

	.globl	_whichqs
	.globl	_qs
	.globl	_cnt

	.globl	_noproc
	.comm	_noproc,4
	.globl	_runrun
	.comm	_runrun,4

/*
 * The following primitives use the fancy VAX instructions
 * much like VMS does.  _whichqs tells which of the 32 queues _qs
 * have processes in them.  Setrq puts processes into queues, Remrq
 * removes them from queues.  The running process is on no queue,
 * other processes are on a queue related to p->p_pri, divided by 4
 * actually to shrink the 0-127 range of priorities into the 32 available
 * queues.
 */

/*
 * Setrq(p), using fancy VAX instructions.
 *
 * Call should be made at splclock(), and p->p_stat should be SRUN
 */
	.align	1
JSBENTRY(Setrq, R0)
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

/*
 * Remrq(p), using fancy VAX instructions
 *
 * Call should be made at splclock().
 */
	.align	1
JSBENTRY(Remrq, R0)
	movzbl	P_PRI(r0),r1
	ashl	$-2,r1,r1
	bbsc	r1,_whichqs,rem1
	pushab	rem3			# it wasn t recorded to be on its q
	calls	$1,_panic
rem1:
	remque	(r0),r2
	beql	rem2
	bbss	r1,_whichqs,rem2
rem2:
	clrl	P_RLINK(r0)		## for firewall checking
	rsb

rem3:	.asciz	"remrq"

/*
 * Masterpaddr is the p->p_addr of the running process on the master
 * processor.  When a multiprocessor system, the slave processors will have
 * an array of slavepaddr s.
 */
	.globl	_masterpaddr
	.data
_masterpaddr:
	.long	0

	.text
sw0:	.asciz	"swtch"

/*
 * When no processes are on the runq, Swtch branches to idle
 * to wait for something to come ready.
 */
	.globl	Idle
Idle: idle:
	movl	$1,_noproc
	mtpr	$0,$IPL			# must allow interrupts here
1:
	tstl	_whichqs		# look for non-empty queue
	bneq	sw1
	brb	1b

badsw:	pushab	sw0
	calls	$1,_panic
	/*NOTREACHED*/

/*
 * Swtch(), using fancy VAX instructions
 */
	.align	1
JSBENTRY(Swtch, 0)
	incl	_cnt+V_SWTCH
sw1:	ffs	$0,$32,_whichqs,r0	# look for non-empty queue
	beql	idle			# if none, idle
	mtpr	$0x18,$IPL		# lock out all so _whichqs==_qs
	bbcc	r0,_whichqs,sw1		# proc moved via interrupt
	movaq	_qs[r0],r1
	remque	*(r1),r2		# r2 = p = highest pri process
	bvs	badsw			# make sure something was there
	beql	sw2
	insv	$1,r0,$1,_whichqs	# still more procs in this queue
sw2:
	clrl	_noproc
	clrl	_runrun
#ifdef notdef
	tstl	P_WCHAN(r2)		## firewalls
	bneq	badsw			##
	cmpb	P_STAT(r2),$SRUN	##
	bneq	badsw			##
#endif
	clrl	P_RLINK(r2)		##
	movl	*P_ADDR(r2),r0
#ifdef notdef
	cmpl	r0,_masterpaddr		# resume of current proc is easy
	beql	res0
#endif
	movl	r0,_masterpaddr
	ashl	$PGSHIFT,r0,r0		# r0 = pcbb(p)
/* fall into... */

/*
 * Resume(pf)
 */
JSBENTRY(Resume, R0)
	mtpr	$HIGH,$IPL			# no interrupts, please
	movl	_CMAP2,_u+PCB_CMAP2	# yech
	svpctx
	mtpr	r0,$PCBB
	ldpctx
	movl	_u+PCB_CMAP2,_CMAP2	# yech
	mtpr	$_CADDR2,$TBIS
res0:
	tstl	_u+PCB_SSWAP
	bneq	res1
	rei
res1:
	movl	_u+PCB_SSWAP,r0			# longjmp to saved context
	clrl	_u+PCB_SSWAP
	movq	(r0)+,r6			# restore r6, r7
	movq	(r0)+,r8			# restore r8, r9
	movq	(r0)+,r10			# restore r10, r11
	movq	(r0)+,r12			# restore ap, fp
	movl	(r0)+,r1			# saved sp
	cmpl	r1,sp				# must be a pop
	bgequ	1f
	pushab	2f
	calls	$1,_panic
	/* NOTREACHED */
1:
	movl	r1,sp				# restore sp
	pushl	$PSL_PRVMOD			# return psl
	pushl	(r0)				# address to return to
	rei

2:	.asciz	"ldctx"

/*
 * {fu,su},{byte,word}, all massaged by asm.sed to jsb s
 */
	.align	1
JSBENTRY(Fuword, R0)
	prober	$3,$4,(r0)
	beql	fserr
	movl	(r0),r0
	rsb
fserr:
	mnegl	$1,r0
	rsb

	.align	1
JSBENTRY(Fubyte, R0)
	prober	$3,$1,(r0)
	beql	fserr
	movzbl	(r0),r0
	rsb

	.align	1
JSBENTRY(Suword, R0|R1)
	probew	$3,$4,(r0)
	beql	fserr
	movl	r1,(r0)
	clrl	r0
	rsb

	.align	1
JSBENTRY(Subyte, R0|R1)
	probew	$3,$1,(r0)
	beql	fserr
	movb	r1,(r0)
	clrl	r0
	rsb

/*
 * Copy 1 relocation unit (NBPG bytes)
 * from user virtual address to physical address
 */
ENTRY(copyseg, 0)
	bisl3	$PG_V|PG_KW,8(ap),_CMAP2
	mtpr	$_CADDR2,$TBIS	# invalidate entry for copy 
	movc3	$NBPG,*4(ap),_CADDR2
	ret

/*
 * zero out physical memory
 * specified in relocation units (NBPG bytes)
 */
ENTRY(clearseg, 0)
	bisl3	$PG_V|PG_KW,4(ap),_CMAP1
	mtpr	$_CADDR1,$TBIS
	movc5	$0,(sp),$0,$NBPG,_CADDR1
	ret

/*
 * Check address.
 * Given virtual address, byte count, and rw flag
 * returns 0 on no access.
 */
ENTRY(useracc, 0)
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

/*
 * kernacc - check for kernel access privileges
 *
 * We can t use the probe instruction directly because
 * it ors together current and previous mode.
 */
 ENTRY(kernacc, 0)
	movl	4(ap),r0	# virtual address
	bbcc	$31,r0,kacc1
	bbs	$30,r0,kacerr
	mfpr	$SBR,r2		# address and length of page table (system)
	bbss	$31,r2,0f; 0:
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
	addl2	$NBPG-1,r1
	ashl	$-PGSHIFT,r0,r0
	ashl	$-PGSHIFT,r1,r1
	bbs	$31,4(ap),kacc6
	bbc	$30,4(ap),kacc6
	cmpl	r0,r3		# user stack
	blss	kacerr		# address too low
	brb	kacc4
kacc6:
	cmpl	r1,r3		# compare last page to P0LR or SLR
	bgtr	kacerr		# address too high
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
	aoblss	r1,r0,kacc4	# next page
	movl	$1,r0		# no errors
	ret
kacerr:
	clrl	r0		# error
	ret
/*
 * Extracted and unrolled most common case of pagein (hopefully):
 *	resident and not on free list (reclaim of page is purely
 *	for the purpose of simulating a reference bit)
 *
 * Built in constants:
 *	CLSIZE of 2, any bit fields in pte s
 */
	.text
	.globl	Fastreclaim
Fastreclaim:
	PUSHR
#ifdef GPROF
	movl	fp,-(sp)
	movab	12(sp),fp
	jsb	mcount
	movl	(sp)+,fp
#endif GPROF
	extzv	$9,$23,28(sp),r3	# virtual address
	bicl2	$1,r3			# v = clbase(btop(virtaddr)); 
	movl	_u+U_PROCP,r5		# p = u.u_procp 
					# from vtopte(p, v) ...
	movl	$1,r2			# type = CTEXT;
	cmpl	r3,P_TSIZE(r5)
	jlssu	1f			# if (isatsv(p, v)) {
	addl3	P_TSIZE(r5),P_DSIZE(r5),r0
	cmpl	r3,r0
	jgequ	2f
	clrl	r2			#	type = !CTEXT;
1:
	ashl	$2,r3,r4
	addl2	P_P0BR(r5),r4		#	tptopte(p, vtotp(p, v));
	jbr	3f
2:
	cvtwl	P_SZPT(r5),r4		# } else (isassv(p, v)) {
	ashl	$7,r4,r4
	subl2	$0x400000,r4
	addl2	r3,r4
	ashl	$2,r4,r4
	addl2	P_P0BR(r5),r4		#	sptopte(p, vtosp(p, v));
	clrl	r2			# 	type = !CTEXT;
3:					# }
	bitb	$0x82,3(r4)
	beql	2f			# if (pte->pg_v || pte->pg_fod)
	POPR; rsb			#	let pagein handle it
2:
	bicl3	$0xffe00000,(r4),r0
	jneq	2f			# if (pte->pg_pfnum == 0)
	POPR; rsb			# 	let pagein handle it 
2:
	subl2	_firstfree,r0
	ashl	$-1,r0,r0	
	incl	r0			# pgtocm(pte->pg_pfnum) 
	mull2	$SZ_CMAP,r0
	addl2	_cmap,r0		# &cmap[pgtocm(pte->pg_pfnum)] 
	tstl	r2
	jeql	2f			# if (type == CTEXT &&
	jbc	$C_INTRANS,(r0),2f	#     c_intrans)
	POPR; rsb			# 	let pagein handle it
2:
	jbc	$C_FREE,(r0),2f		# if (c_free)
	POPR; rsb			# 	let pagein handle it 
2:
	bisb2	$0x80,3(r4)		# pte->pg_v = 1;
	jbc	$26,4(r4),2f		# if (anycl(pte, pg_m) 
	bisb2	$0x04,3(r4)		#	pte->pg_m = 1;
2:
	bicw3	$0x7f,2(r4),r0
	bicw3	$0xff80,6(r4),r1
	bisw3	r0,r1,6(r4)		# distcl(pte);
	ashl	$PGSHIFT,r3,r0
	mtpr	r0,$TBIS
	addl2	$NBPG,r0
	mtpr	r0,$TBIS		# tbiscl(v); 
	tstl	r2
	jeql	2f			# if (type == CTEXT) 
	movl	P_TEXTP(r5),r0
	movl	X_CADDR(r0),r5		# for (p = p->p_textp->x_caddr; p; ) {
	jeql	2f
	ashl	$2,r3,r3
3:
	addl3	P_P0BR(r5),r3,r0	#	tpte = tptopte(p, tp);
	bisb2	$1,P_FLAG+3(r5)		#	p->p_flag |= SPTECHG;
	movl	(r4),(r0)+		#	for (i = 0; i < CLSIZE; i++)
	movl	4(r4),(r0)		#		tpte[i] = pte[i];
	movl	P_XLINK(r5),r5		#	p = p->p_xlink;
	jneq	3b			# }
2:					# collect a few statistics...
	incl	_u+U_RU+RU_MINFLT	# u.u_ru.ru_minflt++;
	moval	_cnt,r0
	incl	V_FAULTS(r0)		# cnt.v_faults++; 
	incl	V_PGREC(r0)		# cnt.v_pgrec++;
	incl	V_FASTPGREC(r0)		# cnt.v_fastpgrec++;
	incl	V_TRAP(r0)		# cnt.v_trap++;
	POPR
	addl2	$8,sp			# pop pc, code
	mtpr	$HIGH,$IPL		## dont go to a higher IPL (GROT)
	rei
#endif newway
