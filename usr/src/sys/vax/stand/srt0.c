/*	srt0.c	4.4	%G%	*/

/*
 * Startup code for standalone system
 * Non-relocating version -- for programs which are loaded by boot
 */

	.globl	_edata
	.globl	_main
	.globl	__rtt

#if VAX==780
	.set	PHYSUBA,0x20006000	# uba 0
#endif

	.set	HIGH,31		# mask for total disable

entry:	.globl	entry
	.word	0x0
	mtpr	$HIGH,$IPL		# just in case
	mtpr	$_Scbbase,$SCBB
	movl	$RELOC-0x2400,sp
	mtpr	$RELOC-0x2000,$ISP	/* space for interrupts
					/* (in case we are not using that
					/* stack already)
					*/
#if VAX==780
	movl	$1,PHYSUBA+4		# init
ubic:
	movl	*$PHYSUBA,r0		# while ((up->uba_cnfgr & UBIC) == 0)
	bitl	$0x10000,r0		# 	continue;
	jeql	ubic
#endif
start:
	movab	_edata,r0
clr:
	clrl	(r0)+
	cmpl	r0,sp
	jlss	clr
	calls	$0,_main
	jmp	start

__rtt:
	.word	0x0
	jmp	start
