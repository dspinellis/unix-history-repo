	.globl	_Xrkintr0
	.align	2
_Xrkintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_rkintr
	popr	$0x3f
	rei

	.globl	_Xlpintr0
	.align	2
_Xlpintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_lpintr
	popr	$0x3f
	rei

	.globl	_Xtmintr0
	.align	2
_Xtmintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_tmintr
	popr	$0x3f
	rei

	.globl	_Xupintr0
	.align	2
_Xupintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_upintr
	popr	$0x3f
	rei

	.globl	_Xudintr0
	.align	2
_Xudintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_udintr
	popr	$0x3f
	rei

	.globl	_Xdnintr0
	.align	2
_Xdnintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dnintr
	popr	$0x3f
	rei

	.globl	_Xdhrint0
	.align	2
_Xdhrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dhrint
	popr	$0x3f
	rei

	.globl	_Xdhxint0
	.align	2
_Xdhxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dhxint
	popr	$0x3f
	rei

	.globl	_Xdhrint1
	.align	2
_Xdhrint1:
	pushr	$0x3f
	pushl	$1
	calls	$1,_dhrint
	popr	$0x3f
	rei

	.globl	_Xdhxint1
	.align	2
_Xdhxint1:
	pushr	$0x3f
	pushl	$1
	calls	$1,_dhxint
	popr	$0x3f
	rei

	.globl	_Xdzrint0
	.align	2
_Xdzrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dzrint
	popr	$0x3f
	rei

	.globl	_Xdzxint0
	.align	2
_Xdzxint0:
	pushr	$0x3f
	movl	$0,r0
	jbr	_dzdma

	.globl	_Xdzrint1
	.align	2
_Xdzrint1:
	pushr	$0x3f
	pushl	$1
	calls	$1,_dzrint
	popr	$0x3f
	rei

	.globl	_Xdzxint1
	.align	2
_Xdzxint1:
	pushr	$0x3f
	movl	$1,r0
	jbr	_dzdma

	.globl	_Xdzrint2
	.align	2
_Xdzrint2:
	pushr	$0x3f
	pushl	$2
	calls	$1,_dzrint
	popr	$0x3f
	rei

	.globl	_Xdzxint2
	.align	2
_Xdzxint2:
	pushr	$0x3f
	movl	$2,r0
	jbr	_dzdma

	.globl	_Xdzrint3
	.align	2
_Xdzrint3:
	pushr	$0x3f
	pushl	$3
	calls	$1,_dzrint
	popr	$0x3f
	rei

	.globl	_Xdzxint3
	.align	2
_Xdzxint3:
	pushr	$0x3f
	movl	$3,r0
	jbr	_dzdma

	.globl	_Xdzrint4
	.align	2
_Xdzrint4:
	pushr	$0x3f
	pushl	$4
	calls	$1,_dzrint
	popr	$0x3f
	rei

	.globl	_Xdzxint4
	.align	2
_Xdzxint4:
	pushr	$0x3f
	movl	$4,r0
	jbr	_dzdma

	.globl	_Xdzrint5
	.align	2
_Xdzrint5:
	pushr	$0x3f
	pushl	$5
	calls	$1,_dzrint
	popr	$0x3f
	rei

	.globl	_Xdzxint5
	.align	2
_Xdzxint5:
	pushr	$0x3f
	movl	$5,r0
	jbr	_dzdma

	.globl	_Xdzrint6
	.align	2
_Xdzrint6:
	pushr	$0x3f
	pushl	$6
	calls	$1,_dzrint
	popr	$0x3f
	rei

	.globl	_Xdzxint6
	.align	2
_Xdzxint6:
	pushr	$0x3f
	movl	$6,r0
	jbr	_dzdma

	.globl	_Xdzrint7
	.align	2
_Xdzrint7:
	pushr	$0x3f
	pushl	$7
	calls	$1,_dzrint
	popr	$0x3f
	rei

	.globl	_Xdzxint7
	.align	2
_Xdzxint7:
	pushr	$0x3f
	movl	$7,r0
	jbr	_dzdma

	.globl	_Xtsintr0
	.align	2
_Xtsintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_tsintr
	popr	$0x3f
	rei

