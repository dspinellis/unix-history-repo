/*	srt0.c	4.3	%G%	*/

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

	.word	0x0
	mtpr	$HIGH,$IPL		# just in case
	movl	$RELOC-0x2000,sp
#if VAX==780
	movl	$1,PHYSUBA+4		# init
ubic:
	movl	*$PHYSUBA,r0		# while ((up->uba_cnfgr & UBIC) == 0)
	bitl	$0x10000,r0		# 	continue;
	jeql	ubic
#endif
	movab	_edata,r0
clr:
	clrl	(r0)+
	cmpl	r0,sp
	jlss	clr
start:
	calls	$0,_main
	jmp	start

__rtt:
	.word	0x0
	jmp	start
