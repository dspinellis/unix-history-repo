	.globl	_bar
_bar:	.word 0x17
	.ascii "ascii"
	.asciz "asciz"
	movl	r0, r0
	jbr	_foo
	ret
_foo:	jbr	L17
	ret
L17:	rsb
