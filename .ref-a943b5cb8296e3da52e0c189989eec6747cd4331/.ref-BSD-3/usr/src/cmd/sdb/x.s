LL0:
	.data
	.text
	.align	1
	.globl	_main
_main:
	.word	.R1
	jbr 	L13
L14:
	pushl	$123456
	calls	$1,_foo
	ret
	.set	.R1,0x0
L13:
	jbr 	L14
	.data
	.text
	.align	1
	.globl	_foo
_foo:
	.word	.R2
	jbr 	L18
L19:
	movl	$37,r11
	movl	$38,r10
	movl	$39,r9
	calls	$0,_bar
	ret
	.set	.R2,0xe00
L18:
	jbr 	L19
	.data
	.text
	.align	1
	.globl	_bar
_bar:
	.word	.R3
	jbr 	L23
L24:
	movl	$2,r11
	movl	$3,r10
	movl	$4,r9
	calls	$0,_abort
	ret
	.set	.R3,0xe00
L23:
	jbr 	L24
	.data
