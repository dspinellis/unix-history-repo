# Startup code for standalone system
# Non-relocating version -- for programs which are loaded by boot

	.globl	_end
	.globl	_main
	.globl	__rtt
	.globl	_edata

	.set	PHYSUBA,0x20006000	# uba 0
	.set	PHYSMBA0,0x20010000	# mba 0
	.set	PHYSMBA1,0x20012000	# mba 1
	.set	PHYSUMEM,0x2013e000	# unibus memory

	.set	HIGH,31		# mask for total disable

	.word	0x0
	mtpr	$HIGH,$IPL		# just in case
	movl	$1,PHYSMBA0+4		# init
	movl	$1,PHYSMBA1+4		# init
	movl	$1,PHYSUBA+4		# init
	subl3	$_edata,$RELOC-0x2000,r0
	movc5	$0,(sp),$0,r0,_edata
start:
	movl	$RELOC-0x2000,sp
	calls	$0,_main
	jmp	start

__rtt:
	.word	0x0
	jmp	start
