#	relsrt0.s	4.2	11/9/80

# Startup code for standalone system
# Self-relocating version used by boot itself.

	.globl	_main
	.globl	__rtt
	.globl	_end
	.globl	_edata

	.set	PHYSUBA,0x20006000	# uba 0

	.set	HIGH,31		# mask for total disable

	.word	0x0
	mtpr	$HIGH,$IPL		# just in case
	movl	$RELOC,sp
	movl	$1,*$PHYSUBA+4		# init
ubic:
	movl	*$PHYSUBA,r0		# while ((up->uba_cnfgr & UBIC) == 0)
	bitl	$0x10000,r0		# 	continue;
	jeql	ubic
	movl	aend,r0
clr:
	clrl	(r0)+
	cmpl	r0,sp
	jlss	clr
	movc3	aend,*$0,(sp)		# relocate
	jmp	*astart
start:
	calls	$0,_main
	movl	$RELOC,sp
	jmp	start

__rtt:
	.word	0x0
	movl	$RELOC,sp
	jmp	*$start

	.data
astart:	.long	start
aend:	.long	_end-RELOC
