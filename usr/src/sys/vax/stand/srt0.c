#	srt0.c	1.4	%G%

# Startup code for standalone system
# Non-relocating version -- for programs which are loaded by boot

	.globl	_end
	.globl	_main
	.globl	__rtt
	.globl	_edata

	.set	PHYSUBA,0x20006000	# uba 0
	.set	PHYSUMEM,0x2013e000	# unibus memory

	.set	HIGH,31		# mask for total disable

	.word	0x0
	mtpr	$HIGH,$IPL		# just in case
	movl	$1,PHYSUBA+4		# init
	movab	_end,r0
clr:
	clrl	(r0)+
	cmpl	r0,sp
	jlss	clr
start:
	movl	$RELOC-0x2000,sp
	calls	$0,_main
	jmp	start

__rtt:
	.word	0x0
	jmp	start
