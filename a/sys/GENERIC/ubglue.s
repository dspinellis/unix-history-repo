	.globl	_Xrkintr0
	.align	2
_Xrkintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_rkintr
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

	.globl	_Xutintr0
	.align	2
_Xutintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_utintr
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

	.globl	_Xidcintr0
	.align	2
_Xidcintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_idcintr
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

	.globl	_Xdmintr0
	.align	2
_Xdmintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmintr
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
	jmp	_dzdma

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
	jmp	_dzdma

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
	jmp	_dzdma

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
	jmp	_dzdma

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
	jmp	_dzdma

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
	jmp	_dzdma

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
	jmp	_dzdma

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
	jmp	_dzdma

	.globl	_Xtsintr0
	.align	2
_Xtsintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_tsintr
	popr	$0x3f
	rei

	.globl	_Xdmfsrint0
	.align	2
_Xdmfsrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmfsrint
	popr	$0x3f
	rei

	.globl	_Xdmfsxint0
	.align	2
_Xdmfsxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmfsxint
	popr	$0x3f
	rei

	.globl	_Xdmfdaint0
	.align	2
_Xdmfdaint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmfdaint
	popr	$0x3f
	rei

	.globl	_Xdmfdbint0
	.align	2
_Xdmfdbint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmfdbint
	popr	$0x3f
	rei

	.globl	_Xdmfrint0
	.align	2
_Xdmfrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmfrint
	popr	$0x3f
	rei

	.globl	_Xdmfxint0
	.align	2
_Xdmfxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmfxint
	popr	$0x3f
	rei

	.globl	_Xdmflint0
	.align	2
_Xdmflint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmflint
	popr	$0x3f
	rei

	.globl	_Xikintr0
	.align	2
_Xikintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_ikintr
	popr	$0x3f
	rei

	.globl	_Xenxint0
	.align	2
_Xenxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_enxint
	popr	$0x3f
	rei

	.globl	_Xenrint0
	.align	2
_Xenrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_enrint
	popr	$0x3f
	rei

	.globl	_Xencollide0
	.align	2
_Xencollide0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_encollide
	popr	$0x3f
	rei

	.globl	_Xecrint0
	.align	2
_Xecrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_ecrint
	popr	$0x3f
	rei

	.globl	_Xeccollide0
	.align	2
_Xeccollide0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_eccollide
	popr	$0x3f
	rei

	.globl	_Xecxint0
	.align	2
_Xecxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_ecxint
	popr	$0x3f
	rei

	.globl	_Xilrint0
	.align	2
_Xilrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_ilrint
	popr	$0x3f
	rei

	.globl	_Xilcint0
	.align	2
_Xilcint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_ilcint
	popr	$0x3f
	rei

	.globl	_Xaccrint0
	.align	2
_Xaccrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_accrint
	popr	$0x3f
	rei

	.globl	_Xaccxint0
	.align	2
_Xaccxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_accxint
	popr	$0x3f
	rei

	.globl	_Xunintr0
	.align	2
_Xunintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_unintr
	popr	$0x3f
	rei

	.globl	_Xdmcrint0
	.align	2
_Xdmcrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmcrint
	popr	$0x3f
	rei

	.globl	_Xdmcxint0
	.align	2
_Xdmcxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_dmcxint
	popr	$0x3f
	rei

	.globl	_Xvvrint0
	.align	2
_Xvvrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_vvrint
	popr	$0x3f
	rei

	.globl	_Xvvxint0
	.align	2
_Xvvxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_vvxint
	popr	$0x3f
	rei

	.globl	_Xhyint0
	.align	2
_Xhyint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_hyint
	popr	$0x3f
	rei

	.globl	_Xcssxint0
	.align	2
_Xcssxint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_cssxint
	popr	$0x3f
	rei

	.globl	_Xcssrint0
	.align	2
_Xcssrint0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_cssrint
	popr	$0x3f
	rei

	.globl	_Xvaintr0
	.align	2
_Xvaintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_vaintr
	popr	$0x3f
	rei

	.globl	_Xvpintr0
	.align	2
_Xvpintr0:
	pushr	$0x3f
	pushl	$0
	calls	$1,_vpintr
	popr	$0x3f
	rei

